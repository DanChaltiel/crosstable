utils::globalVariables(".")


# Error and warning handling ----------------------------------------------


#' @author https://stackoverflow.com/a/20578779/3888000
#' @keywords internal
#' @noRd
tryCatch2 = function(expr){
    errors = list()
    warnings = list()
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
    if(length(funs)>1) warn("There are multiple '", name,"' functions loaded. If this causes any trouble, fill an issue on crosstable's github page.") # nocov
    unlist(funs[1L])
    # unlist(funs[[1L]])
}



#' @keywords internal
#' @importFrom rlang as_function is_formula caller_env warn abort
#' @importFrom purrr map map_dbl pmap_chr
#' @importFrom glue glue glue_collapse
#' @importFrom stringr str_subset
#' @noRd
parse_funs = function(funs){
    funs = c(funs)
    if(is.null(names(funs))) names(funs)=NA
    if(length(funs)>1) {
        fun_call = as.character(as.list(substitute(funs, caller_env())))
        fun_call = fun_call[fun_call != "c" & fun_call != "list"]
    } else {
        fun_call = deparse(substitute(funs, caller_env()))
    }
    
    x=list(funs, names(funs), fun_call) 
    if(map_dbl(x, length) %>% .[.>0] %>% unique() %>% length() != 1){
        abort(c("Problem with fun_call. This should never happen. Is `funs` syntax correct?", 
                i=glue("lengths: funs={length(funs)}, names(funs)={length(names(funs))}, fun_call={length(fun_call)}"))) #nocov
    }
    
    names(funs) = pmap_chr(x, ~{
        .f = ..1; .name = ..2; .call = ..3
        target_name = NULL 
        
        if(!is.null(.name) && !is.na(.name) && .name!=""){
            target_name=.name
        } else {
            if(is_formula(.f)){
                target_name = format(.f)
                warn(c("Anonymous lambda-functions should be named.", 
                       i=paste0("Instead of: funs=", target_name), 
                       i=paste0('Write: funs=c("Some calculation"=', target_name)))
            } else if(grepl("function *\\(", .call)){
                .call2 = deparse(substitute(.f))
                fargs = names(formals(.f)) %>% glue_collapse(", ")
                fbody = str_subset(.call2[-1], "[}{]", negate = TRUE) %>% str_squish()
                if(length(fbody)>1) fbody = paste0(fbody[1], "...")
                target_name = paste0("function(", fargs ,"){", fbody, "}")
                warn(c("Anonymous functions should be named.", 
                       i=paste0("Instead of: funs=", target_name), 
                       i=paste0('Write: funs=c("Some calculation"=', target_name)))
            } else{
                target_name = .call
            }
        }
        target_name
    })
    
    map(funs, as_function)
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

#' paste all classes (minus "labelled")
#'
#' @param x x
#' @keywords internal
#' @noRd
paste_classes = function(x){
    paste(class(remove_labels(x)), collapse=", ")
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
#' @param B if >0, the number of bootstraps
#'
#' @return the vector \[conf_inf, conf_sup\]
#'
#' @export
#'
#' @examples
#' confint_numeric(iris$Sepal.Length)
#' confint_numeric(mtcars2$hp_date)
#' confint_numeric(mtcars2$hp_date, level=0.99)
confint_numeric = function(object, level=0.95, B=0){
    a = (1-level)/2
    ua = qnorm(1-a)
    n = length(object)
    .mean = mean(object, na.rm=TRUE)
    if(B>0){
        boot.samples = matrix(sample(object, size = B * n, replace = TRUE), B, n)
        boot.statistics = apply(boot.samples, 1, mean, na.rm=TRUE)
        se = sd(boot.statistics, na.rm=TRUE)
    } else {
        se = sd(object, na.rm=TRUE)/sqrt(n)
    }
    rtn = .mean+c(-1,1)*ua*se
    nm = format_fixed(c(a, 1-a)*100, 1)
    names(rtn) = paste(nm, "%")
    rtn
}

# x = iris$Sepal.Length
# x[5:20]=NA
# confint_numeric(x) - confint_numeric(x, B=10)
# confint_numeric(x) - confint_numeric(x, B=100)
# confint_numeric(x) - confint_numeric(x, B=1000)
# confint_numeric(x) - confint_numeric(x, B=10000)
# x = rnorm(1500, mean = 0, sd = 1)
# confint_numeric(x) - confint_numeric(x, B=10)
# confint_numeric(x) - confint_numeric(x, B=100)
# confint_numeric(x) - confint_numeric(x, B=1000)
# confint_numeric(x) - confint_numeric(x, B=10000)
# t.test(x)$conf.int






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


#' Small improvement around stringr::str_wrap in case there is no whitespace
#'
#' @param x character vector of strings to reformat
#' @param width target line width
#' @param ... passed on to stringr::str_wrap
#'
#' @keywords internal
#' @importFrom stringr str_wrap str_replace_all
#'
#' @examples
#' \dontrun{
#' set.seed(0)
#' x=sample(iris$Species, 10)
#' x %>% paste(collapse="") %>% str_wrap2(20) %>% cat
#' x %>% paste(collapse=" ") %>% str_wrap2(20) %>% cat
#' }
str_wrap2 = function(x, width, ...){
    ifelse(str_detect(x, " "),
           str_wrap(x, width, ...),
           str_replace_all(x, paste0("(.{",width,"})"), "\\1\n"))
}


# Check silencing ---------------------------------------------------------

utils::globalVariables("where")
