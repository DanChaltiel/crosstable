


# Error and warning handling ----------------------------------------------


#' @importFrom rlang env env_has inform is_installed
#' @importFrom checkmate assert_string
#' @author tidyselect (https://github.com/r-lib/tidyselect/blob/2fab83639982d37fd94914210f771ab9cbd36b4b/R/utils.R#L281)
#' @keywords internal
#' @noRd
warning_once = function(msg, id=msg) {
    assert_string(id)
    verbosity = getOption("tidyselect_verbosity", FALSE)
    if(verbosity=="silent" || (env_has(inform_env, id) && verbosity!="verbose")) {
        return(invisible(NULL))
    }
    inform_env[[id]] = TRUE
    x = "This message is displayed once per session."
    if(is_installed("crayon") && crayon::has_color())
        x = crayon::silver(x)
    warn(paste(msg, x, sep = "\n"))
}
inform_env = rlang::env()


#' @author https://stackoverflow.com/a/20578779/3888000
#' @keywords internal
#' @noRd
tryCatch2 = function(expr){
    errors <- list()
    warnings <- list()
    rtn = withCallingHandlers(
        tryCatch(expr,
                 error=function(e) {errors <<- c(errors, conditionMessage(e)); return("error")}),
        warning=function(m){
            warnings <<- c(warnings, conditionMessage(m))
            invokeRestart("muffleWarning")
        }
    )
    # attr(rtn, "msg") = unique(msg)
    attr(rtn, "errors") = unique(unlist(errors))
    attr(rtn, "warnings") = unique(unlist(warnings))
    rtn
}




# Function handling --------------------------------------------------------


#' Used for defaulting S3 methods to loaded function
#' @importFrom utils getAnywhere
#' @keywords internal
#' @noRd
get_defined_function = function(name) {
    # https://stackoverflow.com/a/60988796/3888000
    matches = getAnywhere(name)
    # Filter out invisible objects and duplicates
    objs = matches$objs[matches$visible & !matches$dups]
    # Filter out non-function objects
    funs = objs[vapply(objs, is.function, logical(1L))]
    # Filter out function defined in own package.
    envs = lapply(funs, environment)
    funs = funs[! vapply(envs, identical, logical(1L), topenv())]
    if(length(funs)>1) warning("There are multiple '", name,"' functions loaded. If this causes any trouble, fill an issue on crosstable's github page.")
    unlist(funs[1L])
    # unlist(funs[[1L]])
}



#' Clean functions names to character
#'
#' @param funs named vector of functions
#' @keywords internal
#' @noRd
clear_funs = function(funs){
    if (!is.character(funs)) {
        nomf = names(funs)
        funs = as.character(as.list(substitute(funs, caller_env())))
        funs = funs[funs != "c" & funs != "list"]
        names(funs) = nomf
    }
    funs
}



#' Concatenate functions
#'
#' @param ... functions
#' @author David Hajage
#' @keywords internal
#' @noRd
#' @importFrom  stringr str_squish
funs2fun = function(...) {
    fnames = as.character(match.call()[-1])
    fs = list(...)
    fnames2 = names(fs)
    
    if (!is.null(fnames2)) {
        fnames[fnames2 != ""] = fnames2[fnames2 != ""]
    }
    
    n = length(fs)
    function(x, ...) {
        results = NULL
        args = list(...)
        namesargs = names(args)
        for (i in 1:n) {
            func = match.fun(fs[[i]])
            forms = formals(func) # Pour min et max (et les autres
            # primitives), il faudrait mettre
            # 'formals(args(func))'. Le probleme est
            # que min et max retourne le minimum de
            # tout ce qui n'est pas 'na.rm', donc si
            # je met un autre argument (genre probs =
            # 1/3), min et max prennent en compte sa
            # valeur, d'ou surprises... Je prefere
            # laisser comme ca.
            namesforms = names(forms)
            if (all(namesforms != "...")) {
                finalargs = c(list(x = x), args[namesargs %in% namesforms])
            } else {
                finalargs = c(list(x = x), args)
            }
            tmp = do.call(func, finalargs)
            names(tmp) = str_squish(paste(fnames[i], names(tmp)))
            results = c(results, as.list(tmp))
        }
        data.frame(results, check.names = FALSE)
    }
}



# Class checking ----------------------------------------------------------



#' test
#'
#' @param x x
#' @author David Hajage
#' @keywords internal
#' @noRd
is.character.or.factor = function(x) {
    is.character(x) | is.factor(x)
}

#' test
#'
#' @param x x
#' @author David Hajage
#' @keywords internal
#' @noRd
#' @importFrom  survival is.Surv
is.numeric.and.not.surv = function(x) {
    is.numeric(x) & !is.Surv(x)
}

#' test
#'
#' @param x x
#' @keywords internal
#' @noRd
is.date = function(x){
    inherits(x, "Date") || inherits(x, "POSIXt") ||
        inherits(x, "POSIXct") || inherits(x, "POSIXlt")
}



# Misc --------------------------------------------------------------------


#' Computes the standard deviation of a date/datetime with the appropriate unit
#'
#' @param x a Date or Posix time
#' @param date_unit one of `c("auto", "seconds", "minutes", "hours", "days", "months", "years")`. `"auto"` will set it to the highest time unit, but it can differ in grouped analysis.
#'
#' @keywords internal
#' @noRd
#' @importFrom checkmate assert
#'
#' @examples
#' x_date = as.Date(mtcars2$hp , origin="2010-01-01") %>% set_label("Date")
#' x_posix = as.POSIXct(mtcars2$qsec*3600*24 , origin="2010-01-01") %>% set_label("Date+time")
#' sd_date(x_date)
#' sd_date(as.POSIXct(x_date))
#' sd_date(x_date, date_unit="days")
#' sd_date(x_posix)
sd_date = function(x, date_unit=c("auto", "seconds", "minutes", "hours", "days", "months", "years")){
    assert(is.date(x))
    unit=match.arg(date_unit)
    if(inherits(x, "Date")) x = as.numeric(x) * 3600 * 24 #days to seconds
    x_sd = sd(x, na.rm=TRUE)
    limits = c("seconds"=-Inf, "minutes"=60, "hours"=3600,
               "days"=3600*24, "months"=3600*24*365/12, "years"=3600*24*365)
    if(unit=="auto"){
        lim = limits[x_sd>limits][length(limits[x_sd>limits])]
    } else {
        lim = limits[unit][length(limits[unit])]
    }
    rtn =sd(as.numeric(x)/lim, na.rm=TRUE)
    
    list(value=rtn, unit=names(lim))
}


#' Confidence interval of a numeric vector
#'
#' Not an S3 method, which might have conflicted with [stats::confint].
#'
#' @param object a vector, numeric or equivalent (date, logical...)
#' @param level the confidence level required
#'
#' @return the vector \[conf_inf, conf_sup\]
#'
#' @export
#'
#' @examples
#' confint_numeric(iris$Sepal.Length)
#' confint_numeric(mtcars2$hp_date)
#' confint_numeric(mtcars2$hp_date, level=0.99)
confint_numeric = function(object, level=0.95){
    a = (1-level)/2
    ua = qnorm(1-a)
    .mean = mean(object, na.rm=TRUE)
    .sd = sd(object, na.rm=TRUE)
    rtn = .mean+c(-1,1)*ua*.sd
    nm = format_fixed(c(a, 1-a)*100, 1)
    names(rtn) = paste(nm, "%")
    rtn
}


#' Return the number of non NA observations
#'
#' @export
#' @param x a vector
#' @author David Hajage
N = function(x) {
    sum(!is.na(x))
}

#' Return the number of NA observations
#'
#' @export
#' @param x a vector
#' @author David Hajage
na = function(x) {
    sum(is.na(x))
}
