
# Formatting --------------------------------------------------------------


#' Format numbers with the exact same number of decimals, including trailing zeros
#'
#' @param x a numeric vector to format
#' @param digits number of decimals
#' @param zero_digits number of significant digits for values rounded to 0 (can be set to NULL to keep the original 0 value)
#' @param date_format if `x` is a vector of Date or POSIXt, the format to apply (see [strptime] for formats)
#' @param percent if TRUE, format the values as percentages
#' @param only_round if TRUE, `format_fixed` simply returns the rounded value. Can be set globally with `options("crosstable_only_round"=TRUE)`.
#' @param ... unused
#'
#' @return a character vector of formatted numbers
#' @author Dan Chaltiel
#' @importFrom checkmate assert_numeric assert_logical assert
#' @export
#'
#' @examples
#' x = c(1, 1.2, 12.78749, pi, 0.00000012)
#' format_fixed(x, digits=3) #default zero_digits=1
#' format_fixed(x, digits=3, zero_digits=2)
#' format_fixed(x, digits=3, zero_digits=NULL)
#' 
#' x_sd = sd(iris$Sepal.Length/10000, na.rm=TRUE)
#' format_fixed(x_sd, dig=6)
#' format_fixed(x_sd, dig=3, zero_digits=2) #default only_round=FALSE
#' format_fixed(x_sd, dig=3, zero_digits=2, only_round=TRUE)
#' options("crosstable_only_round"=TRUE)
#' format_fixed(x_sd, dig=3, zero_digits=2) #override default
#' options("crosstable_only_round"=NULL)
#' 
#' x2 = mtcars$mpg/max(mtcars$mpg)
#' x2 = c(0.01, 0.1001, 0.500005, 0.00000012)
#' format_fixed(x2, percent=TRUE, dig=6)
format_fixed = function(x, digits=1, zero_digits=1, date_format=NULL, 
                        percent=FALSE, 
                        only_round=getOption("crosstable_only_round", FALSE), ...){
  assert_numeric(x)
  assert_numeric(digits)
  assert_logical(percent)
  assert_logical(only_round)
  assert(is.null(zero_digits)||is.na(zero_digits)||is.numeric(zero_digits))
  if(is.date(x)){
    if(!is.null(date_format)) 
      return(format(x, date_format))
    else 
      return(x)
  } else  {
    if(percent) x=x*100
    if(only_round) return(round(x, digits))
    rtn = ifelse(is.na(x), NA_character_, formatC(x, format='f', digits=digits))
    if(!is.null(zero_digits) && !is.na(zero_digits)){
      rtn = ifelse(as.numeric(rtn)==0, signif(x, digits=zero_digits), rtn)
    }
    if(percent) rtn=paste0(rtn, "%")
    return(rtn)
  }
}



#' Format p values (alternative to [format.pval()])
#'
#' @param p p values
#' @param digits number of digits
#' @return formatted p values
#' @seealso [format.pval()], https://stackoverflow.com/a/23018806/3888000
#' @author David Hajage
#' @export
plim = function(p, digits = 4) {
  pround = round(p, digits)
  lim = 10^(-digits)
  ptxt = vector("character", length(p))
  ptxt[pround <  lim] = paste("<", "0.", paste(rep("0", digits - 1), collapse = ""), "1", sep = "")
  ptxt[pround >= lim] = formatC(pround[pround >= lim], format = "f", digits = digits)
  return(ptxt)
}


# Summary functions --------------------------------------------------------


#' Summary functions
#' 
#' Summary functions to use with [crosstable()] or anywhere else.
#'
#' @section Fixed format:
#' These functions use [format_fixed()] which allows to have trailing zeros after rounded values.
#' In the case when the output of rounded values is zero, the use of the \code{zero_digits} argument allows to keep some significant digits for this specific case only. 
#' 
#' @param x a numeric vector
#' @param na.rm \code{TRUE} as default
#' @param dig number of digits
#' @param ... params to pass on to [format_fixed()]:
#' \itemize{
#'  \item `zero_digits` (default=`1`): the number of significant digits for values rounded to 0 (set to NULL to keep the original 0 value)
#'  \item `only_round` (default=`FALSE`): use [round()] instead of [format_fixed()]
#' }
#' 
#' @return a character vector
#' 
#' 
#' @examples 
#' meansd(iris$Sepal.Length, dig=3)
#' meanCI(iris$Sepal.Length)
#' minmax(iris$Sepal.Length, dig=3)
#' mediqr(iris$Sepal.Length, dig=3)
#' nna(iris$Sepal.Length)
#' 
#' #arguments for format_fixed
#' x = iris$Sepal.Length/10000 #closer to zero 
#' 
#' meansd(x, dig=3)
#' meansd(x, dig=3, zero_digits=NULL) #or NA
#' meansd(x, dig=3, only_round=TRUE)
#' options("crosstable_only_round"=TRUE)
#' meansd(x, dig=3, zero_digits=2)
#' options("crosstable_only_round"=NULL)
#' meanCI(mtcars2$x_date)
#' 
#' #dates
#' x = as.POSIXct(mtcars$qsec*3600*24 , origin="2010-01-01")
#' meansd(x)
#' minmax(x, date_format="%d/%m/%Y")
#'
#' @author Dan Chaltiel, David Hajage
#' 
#' @seealso [format_fixed()]
#' 
#' @name summaryFunctions
NULL



#' @describeIn summaryFunctions returns mean and std error
#' @importFrom stats sd
#' @aliases moystd
#' @author Dan Chaltiel, David Hajage
#' @export
meansd = function(x, na.rm = TRUE, dig = 2, ...) {
  moy = mean(x, na.rm=na.rm) %>% 
    format_fixed(digits=dig, ...)
  if(is.date(x)){
    if("date_unit" %in% names(list(...))) 
      date_unit=list(...)$date_unit 
    else 
      date_unit="auto"
    std = sd_date(x, date_unit)
    std = std$value %>% 
      format_fixed(digits=dig, ...) %>% 
      paste(std$unit)
  } else {
    std = sd(x, na.rm=na.rm) %>% 
      format_fixed(digits=dig, ...)
  }
  paste0(moy, " (", std, ")")
}

#' @export
moystd=function(...){
  deprecate_warn("0.1.4", "moystd()", "meansd()")
  meansd(...)
}



#' @describeIn summaryFunctions returns mean and confidence interval
#' @param level the confidence level required
#' @param format a sugar argument. If FALSE, the function returns a list instead of a formatted string
#' @author Dan Chaltiel, David Hajage
#' @export
meanCI = function(x, na.rm = TRUE, dig = 2, level=0.95, format=TRUE, ...) {
  .mean = mean(x, na.rm=na.rm) %>% 
    format_fixed(digits=dig, ...)
  conf = confint_numeric(x, level=level) %>% 
    format_fixed(digits=dig, ...)
  if(!format) return(list(mean=.mean, conf_low=conf[1], conf_high=conf[2]))
  paste0(.mean, " [", conf[1], ";", conf[2],  "]")
}






#' @describeIn summaryFunctions returns median and IQR
#' @author Dan Chaltiel, David Hajage
#' @export
#' @importFrom stats median quantile
mediqr = function(x, na.rm = TRUE, dig = 2, format=TRUE, ...) {
  if(is.date(x)) type=1 else type=7
  med = x %>% 
    median(na.rm=na.rm) %>% 
    format_fixed(digits=dig, ...)
  iqr = x %>% 
    quantile(probs=c(0.25, 0.75), na.rm=na.rm, type=type) %>% 
    format_fixed(digits=dig, ...)
  if(!format) return(list(med=med, iqr_low=iqr[1], iqr_high=iqr[2]))
  paste0(med, " [", iqr[1], ";", iqr[2], "]")
}

#' @describeIn summaryFunctions returns minimum and maximum
#' @author Dan Chaltiel, David Hajage
#' @export
minmax = function(x, na.rm = TRUE, dig = 2, ...) {
  if(all(is.na(x))){
    mi=ma=NA
  } else {
    mi = format_fixed(min(x, na.rm = na.rm), digits=dig, ...)
    ma = format_fixed(max(x, na.rm = na.rm), digits=dig, ...)
  }
  if(is.date(x)){
    paste(mi, "-", ma)
  } else {
    paste(mi, "/", ma)
  }
}

#' @describeIn summaryFunctions returns  number of observations and number of missing values
#' @author Dan Chaltiel, David Hajage
#' @export
nna = function(x) {
  paste0(N(x), " (", na(x), ")")
}


#' Summarize a numeric vector
#' 
#' Summarize a numeric vector with min, max, mean, sd, median, IQR, n and missings.
#'
#' @param x a numeric vector
#' @param dig number of digits
#' @param ... params to pass on to [format_fixed()]: `zero_digits` and `only_round`
#' 
#' @return a list of named functions
#'
#' @author Dan Chaltiel, David Hajage
#' @export
#' @examples 
#' cross_summary(iris$Sepal.Length)
#' cross_summary(iris$Petal.Width, dig=3)
#' cross_summary(mtcars2$hp_date)
#' cross_summary(mtcars2$qsec_posix, date_format="%d/%m %H:%M")
cross_summary = function(x, dig=1, ...) {
  return(c("Min / Max" = minmax(x, dig=dig, ...), "Med [IQR]" = mediqr(x, dig=dig, ...), 
           "Mean (std)" = meansd(x, dig=dig, ...), "N (NA)" = nna(x)))
}

