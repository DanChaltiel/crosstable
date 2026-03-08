
# Formatting --------------------------------------------------------------


#' Format values for display
#'

#' Format numeric values for display in tables and inline text.
#' `format_fixed()` supports fixed or scientific notation, percentages,
#' dates, and small-value formatting through `epsilon` and `zero_digits`.
#'
#' @param x A numeric vector to format. Can also be a `Date`, `POSIXt`, or `Period`.
#' @param digits number of decimals places in decimal notation or number of
#'   significant digits in scientific notation.
#' @param scientific Order of magnitude beyond which numbers are printed in
#'   scientific notation. For example, `scientific = 4` prints values of order
#'   `10^±4` or larger in scientific notation. Can also be `TRUE` (always)
#'   or `FALSE` (never).
#' Can be set globally with `crosstable_options(format_scientific = ...)`.
#' @param zero_digits Number of significant digits to use for non-zero values
#'   that would otherwise round to `0`. Use `NULL` to disable this behavior.
#'   Can be set globally with `crosstable_options(zero_digits = ...)`.
#' @param percent If `TRUE`, values are formatted as percentages.
#' @param date_format A format string passed to [format()] for `Date` and
#'   `POSIXt` vectors. See [strptime] for formats. Can be set globally
#'   with `crosstable_options(date_format = ...)`
#' @param epsilon Values smaller than `epsilon` are displayed as `"< [epsilon]"`.
#'   Can be set globally with `crosstable_options(format_epsilon = ...)`.
#' @param is_period Whether `x` is a period (a numeric value of seconds).
#'   Mainly for internal use.
#' @param only_round Deprecated, use `zero_digits=NULL` instead.
#' @param ... Not used.
#'
#' @return A character vector of formatted numbers
#' @author Dan Chaltiel
#' @importFrom checkmate assert assert_logical assert_numeric
#' @importFrom glue glue
#' @importFrom rlang check_installed
#' @export
#'
#' @examples
#' # Basic formatting
#' x = c(1, 1.2, 99.999, pi, 0.00000012)
#' format_fixed(x, digits = 2)
#'
#' # Prevent small values from rounding to zero
#' x = c(1.1, 0.1, 0.008280661)
#' format_fixed(x, digits = 1, zero_digits = 1)
#'
#' # Control when scientific notation is used
#' x = c(0.11e-04, 0.001, 0.01, 0.1, 1, 10)
#' format_fixed(x, scientific = 2, digits = 2)
#'
#' # Force scientific or fixed notation
#' x = c(0.5, 0.01, 1e-07)
#' format_fixed(x, scientific = TRUE, digits = 1)
#' format_fixed(x, scientific = FALSE, digits = 4)
#'
#' # Percent formatting
#' x = c(0.5, 0.1001, 0.01)
#' format_fixed(x, percent = TRUE, digits = 1)
#'
#' # Threshold display for very small values
#' x = c(0.5, 0.1, 0.01)
#' format_fixed(x, epsilon = 0.05)
format_fixed = function(x, digits=1, ..., scientific=4,
                        zero_digits=1, percent=FALSE,
                        date_format=NULL, epsilon=NULL, is_period=FALSE,
                        only_round="deprecated"){
  assert_numeric(x, null.ok=TRUE)
  assert_numeric(digits, len=1)
  assert_numeric(epsilon, len=1, null.ok=TRUE, any.missing=FALSE)
  assert_numeric(zero_digits, len=1, null.ok=TRUE, any.missing=FALSE)
  assert_logical(percent, len=1)
  assert_logical(is_period, len=1)
  assert_scalar(scientific)
  if(length(x)==0) return(NULL)
  if(missing(zero_digits)) zero_digits = getOption("crosstable_zero_digits", zero_digits)
  if(missing(date_format)) date_format = getOption("crosstable_date_format", date_format)
  if(missing(scientific)) scientific = getOption("crosstable_format_scientific", scientific)
  if(missing(epsilon)) epsilon = getOption("crosstable_format_epsilon", epsilon)
  #TODO optionize()

  if(is_period){
    check_installed("lubridate", reason="to use `format_fixed(is_period=TRUE)`")
    d = structure(round(x), class="difftime", units="secs")
    return(format(lubridate::as.period(d)))
  }

  if(inherits(x, "Period")){
    return(x)
  }

  if(is.date(x)){
    if(is.null(date_format)) date_format=""
    return(format(x, format=date_format))
  }

  if(percent) x = x*100
  x_stable = x + sign(x) * .Machine$double.eps * 10 #avoid floating-point tie errors

  x_round = round(x_stable, digits=digits)
  rtn_dec = format(x_round, nsmall=digits, scientific=FALSE, trim=TRUE)
  x_signif = signif(x_stable, digits=digits)
  rtn_sci = .format_sci(x_signif, digits=digits)

  if(is.logical(scientific)){
    sci = rep(scientific, length(x))
  } else {
    assert_numeric(scientific, null.ok=FALSE, any.missing=FALSE, lower=1)
    expo = floor(log10(abs(x)))
    sci = x!=0 & abs(expo) >= scientific
    sci = replace_na(sci, FALSE)
  }

  rtn = ifelse(sci & x_round!=0, rtn_sci, rtn_dec)

  if(!is.null(zero_digits) && !is.na(zero_digits)){
    x_signif = signif(x, digits=zero_digits)
    rtn_zero_dec = format(x_signif, digits=zero_digits, scientific=FALSE, trim=TRUE)
    rtn_zero_sci = .format_sci(x, digits=max(zero_digits, digits))
    rtn_zero = ifelse(sci, rtn_zero_sci, rtn_zero_dec)
    rtn = ifelse(x_round==0, rtn_zero, rtn)
  }

  rtn = ifelse(x==0, "0", rtn)

  if(percent){
    rtn[x == 100] = "100"
    rtn=paste0(rtn, "%")
  }

  if(!is.null(epsilon) && !is.na(epsilon)){
    rtn = ifelse(x<epsilon, paste0("<", epsilon), rtn)
  }

  is_special = is.infinite(x) | is.na(x) | is.nan(x)
  rtn = ifelse(is_special, x, rtn)

  rtn
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

#' Remove missing values
#'
#' @param x a vector
#'
#' @return the same vector without missing values
#' @export
narm = function(x){
  x[!is.na(x)]
}

#' @noRd
#' @importFrom stringr str_pad
#' @examples
#' x = c(1e-4, 1e-3, 1e-2, 1e-1, 1, 10, 5881747999, 152e150)
#' .format_sci(x, 1)
#' .format_sci(x, 2)
.format_sci = function(x, digits){
  expo = floor(log10(abs(x)))
  mant = x / 10^expo
  mant = round(mant, digits - 1)
  mant_chr = format(mant, scientific = FALSE, trim = TRUE, nsmall = max(digits - 1, 0))
  expo_abs = str_pad(abs(expo), width = 2, pad = "0")
  expo_chr = ifelse(expo >= 0, "+", "-")
  paste0(mant_chr, "e", expo_chr, expo_abs)
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
  is_period = !is.null(attr(x, "is_period"))
  moy = mean(x, na.rm=na.rm) %>%
    format_fixed(digits=dig, is_period=is_period, ...)
  if(is.date(x)){
    if("date_unit" %in% names(list(...)))
      date_unit=list(...)$date_unit
    else
      date_unit="auto"
    std = sd_date(x, date_unit)
    std = std$value %>%
      format_fixed(digits=dig, is_period=is_period, ...) %>%
      paste(std$unit)
  } else {
    std = sd(x, na.rm=na.rm) %>%
      format_fixed(digits=dig, is_period=is_period, ...)
  }
  paste0(moy, " (", std, ")")
}

#' @export
#' @importFrom lifecycle deprecate_warn
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
  is_period = !is.null(attr(x, "is_period"))
  .mean = mean(x, na.rm=na.rm) %>%
    format_fixed(digits=dig, is_period=is_period, ...)
  conf = confint_numeric(x, level=level) %>%
    format_fixed(digits=dig, is_period=is_period, ...)
  if(!format) return(list(mean=.mean, conf_low=conf[1], conf_high=conf[2]))
  paste0(.mean, " [", conf[1], ";", conf[2],  "]")
}






#' @describeIn summaryFunctions returns median and IQR
#' @author Dan Chaltiel, David Hajage
#' @export
#' @importFrom stats median quantile
mediqr = function(x, na.rm = TRUE, dig = 2, format=TRUE, ...) {
  if(is.date(x)) type=1 else type=7
  is_period = !is.null(attr(x, "is_period"))
  med = x %>%
    median(na.rm=na.rm) %>%
    format_fixed(digits=dig, is_period=is_period, ...)
  iqr = x %>%
    quantile(probs=c(0.25, 0.75), na.rm=na.rm, type=type) %>%
    format_fixed(digits=dig, is_period=is_period, ...)
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
    is_period = !is.null(attr(x, "is_period"))
    mi = format_fixed(min(x, na.rm = na.rm), digits=dig, is_period=is_period, ...)
    ma = format_fixed(max(x, na.rm = na.rm), digits=dig, is_period=is_period, ...)
  }
  if(is.date(x)){
    paste(mi, "-", ma)
  } else {
    paste(mi, "/", ma)
  }
}

#' @describeIn summaryFunctions returns number of observations and number of missing values
#' @author Dan Chaltiel, David Hajage
#' @export
nna = function(x) {
  paste0(N(x), " (", na(x), ")")
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
  c("Min / Max" = minmax(x, dig=dig, ...), "Med [IQR]" = mediqr(x, dig=dig, ...),
    "Mean (std)" = meansd(x, dig=dig, ...), "N (NA)" = nna(x))
}
