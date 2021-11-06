utils::globalVariables(".")


# Error and warning handling ----------------------------------------------

#' @source https://stackoverflow.com/a/20578779/3888000 
#' @keywords internal
#' @noRd
#' @examples 
#' foo=function(){log(-1);warn("outch");message("foo");message("bar");stop("END");1}
#' x=tryCatch2(foo())
#' x
#' attributes(x)
tryCatch2 = function(expr){
    errors = list()
    warnings = list()
    messages = list()
    rtn = withCallingHandlers(
        tryCatch(
            expr,
            error=function(e) {
                errors <<- c(errors, list(e))
                return("error")
            }
        ),
        warning=function(w){
            warnings <<- c(warnings, list(w))
            invokeRestart("muffleWarning")
        },
        message=function(m){
            messages <<- c(messages, list(m))
            invokeRestart("muffleMessage")
        }
    )
    attr(rtn, "errors") = unique(map_chr(errors, conditionMessage))
    attr(rtn, "warnings") = unique(map_chr(warnings, conditionMessage))
    attr(rtn, "messages") = unique(map_chr(messages, conditionMessage))
    
    x = c(errors, warnings, messages) %>% unique()
    attr(rtn, "overview") = tibble(
        type=map_chr(x, ~ifelse(inherits(.x, "error"), "Error", 
                                ifelse(inherits(.x, "warning"), "Warning", "Message"))),
        class=map_chr(x, ~class(.x) %>% glue_collapse("/")),
        message=map_chr(x, ~conditionMessage(.x))
    )
    
    rtn
}

#' @keywords internal
#' @noRd
condition_overview = function(expr){
    tryCatch2(expr) %>% attr("overview")
}

#' @importFrom glue glue
#' @importFrom rlang abort
#' @importFrom stringr str_ends
#' @keywords internal
#' @noRd
assert_is_installed = function(pkg, fun) {
    if(!str_ends(fun, "()")) fun=paste0(fun, "()")
    if(!requireNamespace(pkg, quietly=TRUE)) {
        abort(glue('Package "{pkg}" is needed for function {fun} to work. Please install it.'),
              class="missing_package_error") # nocov
    }
    invisible(pkg)
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
}



#' @importFrom rlang as_function is_formula caller_env warn abort
#' @importFrom purrr map map_dbl pmap_chr
#' @importFrom glue glue glue_collapse
#' @importFrom stringr str_subset
#' @keywords internal
#' @noRd
parse_funs = function(funs){
    # browser()
    # fun_quo=enquo(funs)
    funs = c(funs)
    if(is.null(names(funs))) names(funs)=NA
    caller = caller_env()
    
    #TODO if(!is.list(funs) funs=list(funs))
    
    # fun_call = as.character(as.list(substitute(funs)))
    # fun_call = fun_call[fun_call != "c" & fun_call != "list"]
    # fun_call2 = as.character(as.list(substitute(funs, caller_env())))
    # fun_call2 = fun_call[fun_call != "c" & fun_call != "list"]
    
    if(length(funs)>1) {
        fun_call = as.character(as.list(substitute(funs, caller_env())))
        fun_call = fun_call[fun_call != "c" & fun_call != "list"]
    } else {
        fun_call = deparse(substitute(funs, caller_env()))
    }
    # print(fun_call)
    # print(deparse(substitute(funs, caller_env())))
    # print(deparse(substitute(funs)))
    # print(deparse(substitute(list(funs), caller_env())))
    # browser()
    if(length(fun_call)!=length(funs)){
        fun_call = as.character(as.list(substitute(funs, caller_env())))
        fun_call = fun_call[fun_call != "c" & fun_call != "list"]
        # x=list(funs, names(funs), fun_call) 
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
                       i=paste0('Write: funs=c("Some calculation"=', target_name)),
                     class="crosstable_unnamed_lambda_warning")
            } else if(grepl("function *\\(", .call)){
                .call2 = deparse(substitute(.f))
                fargs = names(formals(.f)) %>% glue_collapse(", ")
                fbody = str_subset(.call2[-1], "[}{]", negate = TRUE) %>% str_squish()
                if(length(fbody)>1) fbody = paste0(fbody[1], "...")
                target_name = paste0("function(", fargs ,"){", fbody, "}")
                warn(c("Anonymous functions should be named.", 
                       i=paste0("Instead of: funs=", target_name), 
                       i=paste0('Write: funs=c("Some calculation"=', target_name)),
                     class="crosstable_unnamed_anonymous_warning")
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

#' paste all names and first classes (minus "labelled")
#'
#' @param x a dataframe
#' @keywords internal
#' @noRd
paste_nameclasses = function(x){
    glue("{name} ({class})", name=names(x), 
         class=map_chr(x, ~class(remove_label(.x))[1]))
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
#' @author Dan Chaltiel
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

#' Confidence interval of a vector of proportion
#'
#' @param p the proportion
#' @param n the sample size
#' @param i either -1 or +1
#' @param level the confidence level required
#' @source binom:::binom.confint
#' @source https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
#' @note validé avec PropCIs::scoreci
#' @keywords internal
#' @noRd
confint_proportion = function(p, n, 
                              method=c("wilson", "asymptotic"),
                              level=0.95){
    a = 1-level
    method = match.arg(method)
    z = qnorm(1-a/2)
    # browser()
    if(method=="wilson"){
        z2 = z * z
        p1 = p + 0.5 * z2/n
        p2 = z * sqrt((p * (1 - p) + 0.25 * z2/n)/n)
        p3 = 1 + z2/n
        lcl = (p1 - p2)/p3
        ucl = (p1 + p2)/p3
        rtn = data.frame(inf=lcl, sup=ucl)
        rtn2 = purrr::map2_dfr(n*p, n, ~{
            PropCIs::scoreci(.x, .y, conf.level=0.95)$conf.int %>% 
                set_names(c("inf", "sup"))
        })
        
    } else if(method=="asymptotic"){
        p1 = z*sqrt(p*(1-p)/n)
        rtn = data.frame(inf=p-p1, sup=p+p1)
    } 
    return(rtn)
}

#' Return the number of non NA observations
#'
#' @export
#' @return integer, number of non NA observations
#' @param x a vector
#' @author David Hajage
N = function(x) {
    sum(!is.na(x))
}

#' Return the number of NA observations
#'
#' @export
#' @return integer, number of NA observations
#' @param x a vector
#' @author David Hajage
na = function(x) {
    sum(is.na(x))
}


#' Small improvement around [stringr::str_wrap()] in case there is no whitespace
#'
#' @param x character vector of strings to reformat
#' @param width target line width
#' @param ... passed on to [stringr::str_wrap()]
#' @return A character vector of re-wrapped strings
#'
#' @keywords internal
#' @noRd
#' @importFrom stringr str_detect str_wrap str_replace_all
#'
#' @examples
#' set.seed(0)
#' x=sample(iris$Species, 10)
#' x %>% paste(collapse="") %>% crosstable:::str_wrap2(20) %>% cat
#' x %>% paste(collapse=" ") %>% crosstable:::str_wrap2(20) %>% cat
str_wrap2 = function(x, width, ...){
    ifelse(str_detect(x, " "),
           str_wrap(x, width, ...),
           str_replace_all(x, paste0("(.{",width,"})"), "\\1\n"))
}


# dplyr -------------------------------------------------------------------

#' @source https://github.com/tidyverse/dplyr/issues/5563#issuecomment-721769342
across_unpack <- function(...) {
    out <- across(...)
    tidyr::unpack(out, names(out), names_sep = "_")
}

# Check silencing ---------------------------------------------------------

utils::globalVariables("where")
