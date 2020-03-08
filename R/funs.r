


#' Format numbers with same number of decimals, including trailing zeros
#'
#' @param x a numeric vector to format
#' @param digits number of decimals
#' @param zero_digits number of significant digits for values rounded to 0 (set to NULL to keep the original 0 value)
#' @param only_round util option, to simply return rounded value instead of formatted
#'
#' @return a character vector of formatted numbers
#' @author Dan Chaltiel
#' @export
#'
#' @examples
#' x = c(1, 1.2, 12.78749, pi, 0.00000012)
#' format_fixed(x, digits=3)
#' format_fixed(x, digits=3, zero_digits=2)
#' format_fixed(x, digits=3, zero_digits=NULL)
#' x = iris$Sepal.Length/10000
#' x %>% 
#' sd(na.rm=na.rm) %>% 
#'   format_fixed(dig=3, zero_digits=2, only_round=T)
format_fixed = function(x, digits, zero_digits=1, only_round=FALSE){
  stopifnot(is.numeric(x), 
            is.numeric(digits), 
            is.logical(only_round), 
            is.null(zero_digits)||is.numeric(zero_digits)
            )
  if(only_round) {
    return(round(x,digits))
  } else {
    rtn = formatC(x, format='f', digits=digits)
    if(!is.null(zero_digits) && !is.na(zero_digits)){
      rtn = ifelse(as.numeric(rtn)==0, signif(x, digits=zero_digits), rtn)
    }
    return(rtn)
  }
}


# summary functions --------------------------------------------------------


#' Summary functions
#' 
#' Summary functions to use with \code{\link{cross}} or anywhere else.
#'
#' @section Fixed format:
#' The use of the \code{fixed_format} argument allows to have trailing zeros after rounded values.
#' In the case when the output of rounded values is zero, the use of the \code{zero_digits} argument allows to keep some significant digits for this specific case only. 
#' 
#' @param x a numeric vector
#' @param na.rm \code{TRUE} as default
#' @param dig number of digits
#' @param fixed_format use \code{\link{format_fixed}} instead of round (default)
#' @param zero_digits is fixed_format==TRUE, number of significant digits for values rounded to 0 (set to NULL to keep the original 0 value)
#' 
#' 
#' @examples 
#' moystd(iris$Sepal.Length, dig=3)
#' minmax(iris$Sepal.Length, dig=3)
#' mediqr(iris$Sepal.Length, dig=3)
#' nna(iris$Sepal.Length, dig=3)
#' 
#' x = iris$Sepal.Length/10000 #closer to zero 
#' 
#' moystd(x, dig=3, fixed_format=T)
#' moystd(x, dig=3, fixed_format=T, zero_digits=NULL)
#' options("biostat2_fixed_format"=T)
#' moystd(x, dig=3, zero_digits=2)
#' options("biostat2_fixed_format"=NULL)
#'
#' @author Dan Chaltiel
#' @author David Hajage
#' 
#' @seealso \code{\link{format_fixed}}
#' 
#' @name biostats2SummaryFunctions
NULL


#' @describeIn biostats2SummaryFunctions returns mean and std error
#' @export
moystd = function(x, na.rm = TRUE, dig = 2, 
                  fixed_format=getOption("biostat2_fixed_format", FALSE), zero_digits=1) {
  moy = x %>% 
    mean(na.rm=na.rm) %>% 
    format_fixed(digits=dig, zero_digits=zero_digits, only_round=!fixed_format)
  std = x %>% 
    sd(na.rm=na.rm) %>% 
    format_fixed(digits=dig, zero_digits=zero_digits, only_round=!fixed_format)
  paste0(moy, " (", std, ")")
}

#' @describeIn biostats2SummaryFunctions returns median and IQR
#' @export
mediqr = function(x, na.rm = TRUE, dig = 2, 
                   fixed_format=getOption("biostat2_fixed_format", FALSE), zero_digits=1) {
  med = x %>% 
    median(na.rm=na.rm) %>% 
    format_fixed(digits=dig, zero_digits=zero_digits, only_round=!fixed_format)
  iqr = x %>% 
    quantile(probs=c(0.25, 0.75), na.rm=na.rm) %>% 
    format_fixed(digits=dig, zero_digits=zero_digits, only_round=!fixed_format)
  paste0(med, " [", iqr[1], ";", iqr[2], "]")
}

#' @describeIn biostats2SummaryFunctions returns minimum and maximum
#' @export
minmax = function(x, na.rm = TRUE, dig = 2) {
  mi = ifelse(!all(is.na(x)), round(min(x, na.rm = na.rm), dig), NA)
  ma = ifelse(!all(is.na(x)), round(max(x, na.rm = na.rm), dig), NA)
  paste(mi, "/", ma)
}

#' @describeIn biostats2SummaryFunctions returns  number of observations and number of missing values
#' @export
nna = function(x) {
  paste0(n(x), " (", na(x), ")")
}


#' Summarize a numeric vector
#'
#' @param x a numeric vector
#' @param dig number of digits
#' @keywords internal
#' @section Note: 
#' Function \code{mysummary} is kept for compatibility with old codes. It produces the same exact object than \code{cross_summary}, which should be preferred.
cross_summary = function(x, dig=2) {
  return(c("Min / Max" = minmax(x, dig=dig), "Med [IQR]" = mediqr(x, dig=dig), 
           "Moy (std)" = moystd(x, dig=dig), "N (NA)" = nna(x)))
}
#' @rdname cross_summary
#' @deprecated
mysummary=cross_summary

# Utils functions ---------------------------------------------------------



#' Remove blancks at the begining and the end
#'
#' @param x x
#' @author David Hajage
#' @keywords internal
trim = function (x) {
  x = sub("^ +", "", x)
  x = sub(" +$", "", x)
  x
}


#' Concatenate functions
#'
#' @param ... functions
#' @author David Hajage
#' @keywords internal
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
      names(tmp) = trim(paste(fnames[i], names(tmp)))
      results = c(results, as.list(tmp))
    }
    data.frame(results, check.names = FALSE)
  }
}