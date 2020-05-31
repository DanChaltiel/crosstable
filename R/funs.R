


#' Format numbers with the exact same number of decimals, including trailing zeros
#'
#' @param x a numeric vector to format
#' @param digits number of decimals
#' @param zero_digits number of significant digits for values rounded to 0 (can be set to NULL to keep the original 0 value)
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
format_fixed = function(x, digits=1, zero_digits=1, only_round=getOption("crosstable_only_round", FALSE), ...){
  assert_numeric(x)
  assert_numeric(digits)
  assert_logical(only_round)
  assert(is.null(zero_digits)||is.na(zero_digits)||is.numeric(zero_digits))
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


#' Import labels from a dataset
#'
#' @param .tbl the data.frame to labellize
#' @param data_label a data.frame from which to import labels
#' @param name_from in `data_label`, which column to get the variable name
#' @param label_from in `data_label`, which column to get the variable label
#' @param verbose if TRUE, displays a warning if a variable name is not found in `data_label`
#'
#' @export
#' @importFrom expss var_lab var_lab<-
#' @importFrom glue glue
#'
#' @examples
#' iris_label = tibble::tibble(name=c("Sepal.Length", "Sepal.Width",
#'                                    "Petal.Length", "Petal.Width", "Species"),
#'                             label=c("Length of Sepals", "Width of Sepals",
#'                                     "Length of Petals", "Width of Petals", "Specie name"))
#' iris %>% 
#'   import_labels(iris_label) %>% 
#'   crosstable
import_labels = function(.tbl, data_label, name_from = "name", label_from = "label", 
                         verbose=TRUE){
  data_label = as.data.frame(data_label)
  for(i in 1:nrow(data_label)){
    name = data_label[i, name_from]
    label = data_label[i, label_from]
    if(is.null(.tbl[[name]]) && verbose){
      warning(glue("Cannot import label, variable '{name}' not found"))
    } else {
      var_lab(.tbl[name]) = label
    }
  }
  .tbl
}


# summary functions --------------------------------------------------------


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
#' 
#' @examples 
#' moystd(iris$Sepal.Length, dig=3)
#' minmax(iris$Sepal.Length, dig=3)
#' mediqr(iris$Sepal.Length, dig=3)
#' nna(iris$Sepal.Length)
#' 
#' x = iris$Sepal.Length/10000 #closer to zero 
#' 
#' moystd(x, dig=3)
#' moystd(x, dig=3, zero_digits=NULL) #or NA
#' moystd(x, dig=3, only_round=TRUE)
#' options("crosstable_only_round"=TRUE)
#' moystd(x, dig=3, zero_digits=2)
#' options("crosstable_only_round"=NULL)
#'
#' @author Dan Chaltiel
#' @author David Hajage
#' 
#' @seealso [format_fixed()]
#' 
#' @name biostats2SummaryFunctions
NULL


#' @describeIn biostats2SummaryFunctions returns mean and std error
#' @importFrom stats sd
#' @export
moystd = function(x, na.rm = TRUE, dig = 2, ...) {
  moy = x %>% 
    mean(na.rm=na.rm) %>% 
    format_fixed(digits=dig, ...)
  std = x %>% 
    sd(na.rm=na.rm) %>% 
    format_fixed(digits=dig, ...)
  paste0(moy, " (", std, ")")
}


#' @describeIn biostats2SummaryFunctions returns median and IQR
#' @importFrom stats median quantile
#' @export
mediqr = function(x, na.rm = TRUE, dig = 2, ...) {
  med = x %>% 
    median(na.rm=na.rm) %>% 
    format_fixed(digits=dig, ...)
  iqr = x %>% 
    quantile(probs=c(0.25, 0.75), na.rm=na.rm) %>% 
    format_fixed(digits=dig, ...)
  paste0(med, " [", iqr[1], ";", iqr[2], "]")
}

#' @describeIn biostats2SummaryFunctions returns minimum and maximum
#' @export
minmax = function(x, na.rm = TRUE, dig = 2, ...) {
  mi = ifelse(!all(is.na(x)), format_fixed(min(x, na.rm = na.rm), digits=dig, ...), NA)
  ma = ifelse(!all(is.na(x)), format_fixed(max(x, na.rm = na.rm), digits=dig, ...), NA)
  paste(mi, "/", ma)
}

#' @describeIn biostats2SummaryFunctions returns  number of observations and number of missing values
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
#' @export
#' @examples 
#' cross_summary(iris$Sepal.Length)
#' cross_summary(iris$Petal.Width, dig=3)
cross_summary = function(x, dig=1, ...) {
  return(c("Min / Max" = minmax(x, dig=dig, ...), "Med [IQR]" = mediqr(x, dig=dig, ...), 
           "Mean (std)" = moystd(x, dig=dig, ...), "N (NA)" = nna(x)))
}


