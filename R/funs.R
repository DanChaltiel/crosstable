
# Formatting --------------------------------------------------------------


#' Format numbers with the exact same number of decimals, including trailing zeros
#'
#' @param x a numeric vector to format
#' @param digits number of decimals
#' @param zero_digits number of significant digits for values rounded to 0 (can be set to NULL to keep the original 0 value)
#' @param date_format if `x` is a vector of Date or POSIXt, the format to apply (see [strptime] for formats)
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
format_fixed = function(x, digits=1, zero_digits=1, date_format=NULL, only_round=getOption("crosstable_only_round", FALSE), ...){
  assert_numeric(x)
  assert_numeric(digits)
  assert_logical(only_round)
  assert(is.null(zero_digits)||is.na(zero_digits)||is.numeric(zero_digits))
  if(is.date(x)){
    if(!is.null(date_format)) 
      return(format(x, date_format))
    else 
      return(x)
  } else if(only_round) {
    return(round(x,digits))
  } else {
    rtn = ifelse(is.na(x), NA_character_, formatC(x, format='f', digits=digits))
    # rtn = formatC(x, format='f', digits=digits)
    if(!is.null(zero_digits) && !is.na(zero_digits)){
      rtn = ifelse(as.numeric(rtn)==0, signif(x, digits=zero_digits), rtn)
    }
    return(rtn)
  }
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
#' 
#' @examples 
#' moystd(iris$Sepal.Length, dig=3)
#' minmax(iris$Sepal.Length, dig=3)
#' mediqr(iris$Sepal.Length, dig=3)
#' nna(iris$Sepal.Length)
#' 
#' #arguments for format_fixed
#' x = iris$Sepal.Length/10000 #closer to zero 
#' 
#' moystd(x, dig=3)
#' moystd(x, dig=3, zero_digits=NULL) #or NA
#' moystd(x, dig=3, only_round=TRUE)
#' options("crosstable_only_round"=TRUE)
#' moystd(x, dig=3, zero_digits=2)
#' options("crosstable_only_round"=NULL)
#' 
#' #dates
#' x = as.POSIXct(mtcars$qsec*3600*24 , origin="2010-01-01")
#' moystd(x)
#' minmax(x, date_format="%d/%m/%Y")
#'
#' @author Dan Chaltiel
#' @author David Hajage
#' 
#' @seealso [format_fixed()]
#' 
#' @name summaryFunctions
NULL



#' @describeIn summaryFunctions returns mean and std error
#' @importFrom stats sd
#' @aliases moystd
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
#' @export
#' @examples 
#' meanCI(iris$Sepal.Length)
#' meanCI(mtcars2$x_date)
meanCI = function(x, na.rm = TRUE, dig = 2, level=0.95, ...) {
  .mean = mean(x, na.rm=na.rm) %>% 
    format_fixed(digits=dig, ...)
  conf = confint_numeric(x, level=level) %>% 
    format_fixed(digits=dig, ...)
  paste0(.mean, " [", conf[1], ";", conf[2],  "]")
}






#' @describeIn summaryFunctions returns median and IQR
#' @importFrom stats median quantile
#' @export
mediqr = function(x, na.rm = TRUE, dig = 2, ...) {
  if(is.date(x)) type=1 else type=7
  med = x %>% 
    median(na.rm=na.rm) %>% 
    format_fixed(digits=dig, ...)
  iqr = x %>% 
    quantile(probs=c(0.25, 0.75), na.rm=na.rm, type=type) %>% 
    format_fixed(digits=dig, ...)
  paste0(med, " [", iqr[1], ";", iqr[2], "]")
}

#' @describeIn summaryFunctions returns minimum and maximum
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
#' cross_summary(mtcars2$hp_date)
#' cross_summary(mtcars2$qsec_posix, date_format="%d/%m %H:%M")
cross_summary = function(x, dig=1, ...) {
  return(c("Min / Max" = minmax(x, dig=dig, ...), "Med [IQR]" = mediqr(x, dig=dig, ...), 
           "Mean (std)" = meansd(x, dig=dig, ...), "N (NA)" = nna(x)))
}


# Labels ------------------------------------------------------------------


#' Get label if wanted and available, or default (name) otherwise
#'
#' @param x labelled object
#' @param default value returned if there is no label. Default to `names(x)`.
#'
#' @export
#' @seealso [set_label], [import_labels], [remove_label], [Hmisc::label], [expss::var_lab]
#' @examples 
#' #vectors
#' get_label(mtcars2$mpg)
#' get_label(mtcars$mpg)
#' get_label(mtcars$mpg, default="foo")
#' get_label(list(bar=mtcars$mpg)) #default to names
#' 
#' #data.frames
#' get_label(mtcars2["mpg"])
#' get_label(mtcars["mpg"]) #default to names
#' get_label(mtcars["mpg"], default="bar")
get_label = function(x, default=names(x)){
  if(is.list(x)){
    lab = sapply(x, attr, which="label", exact=TRUE, simplify=FALSE)
    lab = unlist(lab)
  } else {
    lab = attr(x, "label", exact=TRUE)
  }
  if(is_null(lab)) return(default)
  lab
}


#' Set the "label" attribute of an object
#'
#' @param x object to labelise
#' @param value value of the label
#'
#' @importFrom checkmate assert_string
#' @export
#' @seealso [get_label], [import_labels], [remove_label]
#' @examples 
#' library(dplyr)
#' mtcars %>% 
#'    mutate(mpg2=set_label(mpg, "Miles per gallon")) %>% 
#'    crosstable(mpg, mpg2)
set_label = function(x, value){
  if(is.null(value) || is.na(value)) return(x)
  assert_string(value)
  if(is.list(x)){
    for (each in seq_along(x)) 
      x[[each]] = set_label(x[[each]], value)
    return(x)
  }
  attr(x, "label") = value
  if (!"labelled" %in% class(x)) {
    class(x) = c("labelled", class(x))
  }
  return(x)
}



#' Remove all label attributes.
#'
#' Use `remove_labels()` to remove the label from an object or to recursively remove all the labels from a collection of objects (such as a list or a data.frame). \cr \cr This can be useful with functions reacting badly to labelled objects.
#' 
#' @param x object to unlabel
#'
#' @export
#' @rdname remove_labels
#' @aliases remove_label
#' @seealso [get_label], [set_label], [import_labels], [expss::unlab]
#' @examples 
#' mtcars2 %>% remove_labels %>% crosstable(1:2) #no labels
#' mtcars2$hp %>% remove_labels %>% get_label #numeric
remove_labels = function(x){
  if (is.null(x)) 
    return(x)
  if (is.list(x)) {
    for (each in seq_along(x)) 
      x[[each]] = remove_label(x[[each]])
    return(x)
  }
  attr(x, "label") = NULL
  class(x) = setdiff(class(x), c("labelled"))
  x
}

#' @rdname remove_labels
#' @aliases remove_labels
#' @param x object to unlabel
#' @usage NULL
#' @export
remove_label = remove_labels


#' Import labels from a dataset
#' @description `import_labels` imports labels from a data.frame (`data_label`) to another one (`.tbl`).
#'
#' @param .tbl the data.frame to labellize
#' @param data_label a data.frame from which to import labels
#' @param name_from in `data_label`, which column to get the variable name
#' @param label_from in `data_label`, which column to get the variable label
#' @param verbose if TRUE, displays a warning if a variable name is not found in `data_label`
#'
#' @export
#' @importFrom glue glue
#'
#' @seealso [get_label], [set_label], [remove_label]
#' @examples
#' #import the labels from a data.frame to another
#' iris_label = data.frame(
#'   name=c("Sepal.Length", "Sepal.Width",
#'          "Petal.Length", "Petal.Width", "Species"),
#'   label=c("Length of Sepals", "Width of Sepals",
#'           "Length of Petals", "Width of Petals", "Specie name")
#' )
#' iris %>% 
#'   import_labels(iris_label) %>% 
#'   crosstable
#'   
import_labels = function(.tbl, data_label = get_last_save(), 
                         name_from = "name", label_from = "label", 
                         verbose=TRUE){
  data_label = as.data.frame(data_label)
  for(i in 1:nrow(data_label)){
    name = as.character(data_label[i, name_from])
    label = as.character(data_label[i, label_from])
    if(!is.null(.tbl[[name]])){
      .tbl[name] = set_label(.tbl[name], label)
    } else if(verbose){
      warning(glue("Cannot import label, variable '{name}' not found"))
    }
  }
  .tbl
}

#' @rdname import_labels
#' @description `save_labels` saves the labels from a data.frame in a temporary variable that can be retrieve by `import_labels`.
#' @export
#' @examples 
#' #save the labels, use some dplyr label-removing function, then retrieve the labels
#' library(dplyr)
#' mtcars2 %>%
#'   save_labels() %>% 
#'   transmute(disp=as.numeric(disp)+1) %>%
#'   import_labels(verbose=FALSE) %>% #
#'   crosstable(disp)
save_labels = function(.tbl){
  labels_env$last_save = tibble(
    name=names(.tbl),
    label=get_label(.tbl)[.data$name]
  )
  .tbl
}

labels_env = rlang::new_environment()
# ls(envir=labels_env)

#' @keywords internal
#' @noRd
get_last_save = function(){
  labels_env$last_save
}
