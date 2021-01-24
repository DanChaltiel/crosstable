
#' Get label if wanted and available, or default (name) otherwise
#'
#' @param x labelled object. If `x` is a list/data.frame, `get_label()` will return the labels of all children recursively
#' @param default value returned if there is no label. Default to `names(x)`.
#' @param object if `x` is a list/data.frame, `object=TRUE` will force getting the labels of the object instead of the children
#'
#' @export
#' @importFrom purrr map map2
#' @seealso [set_label()], [import_labels()], [remove_label()], [Hmisc::label()], [expss::var_lab()]
#' @examples 
#' xx=mtcars2 %>% 
#'   set_label("The mtcars2 dataset", object=TRUE)
#' xx$cyl=remove_label(xx$cyl)
#' 
#' #vectors
#' get_label(xx$mpg) #label="Miles/(US) gallon"
#' get_label(xx$cyl) #default to NULL (as names(xx$cyl)==NULL)
#' get_label(xx$cyl, default="Default value")
#' 
#' #data.frames
#' get_label(xx)
#' get_label(xx, object=TRUE)
#' data.frame(name=names(xx), label=get_label(xx, default=NA)) #cyl is NA
#' 
#' #lists
#' get_label(list(xx$cyl, xx$mpg))
#' get_label(list(foo=xx$cyl, bar=xx$mpg))
#' get_label(list(foo=xx$cyl, bar=xx$mpg), default="Default value")
get_label = function(x, default=names(x), object=FALSE){
    if(is.list(x) && !object){
        # browser()
        if(is.null(default)) default=rep(NA, length(x))
        if(length(default)>1 && length(x)!=length(default)) warning("prout")
        lab = x %>% 
            map(get_label) %>% 
            map2(default, ~{if(is.null(.x)) .y else .x}) %>%
            unlist()
    } else {
        lab = attr(x, "label", exact=TRUE)
        if(is_null(lab)) lab=default
    }
    # stop("TODO")
    lab
}



#' Set the "label" attribute of an object
#'
#' @param x object to labelise. If `x` is a list/data.frame, `set_label()` will return the labels of all children recursively
#' @param value value of the label
#' @param object if `x` is a list/data.frame, `object=TRUE` will force setting the labels of the object instead of the children
#'
#' @importFrom checkmate assert_string
#' @export
#' @seealso [get_label()], [import_labels()], [remove_label()]
#' @examples 
#' library(dplyr)
#' mtcars %>% 
#'    mutate(mpg2=set_label(mpg, "Miles per gallon")) %>% 
#'    crosstable(mpg, mpg2)
set_label = function(x, value, object=FALSE){
    if(is.null(value) || is.na(value)) return(x)
    assert_string(value)
    if(is.list(x) && !object){
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