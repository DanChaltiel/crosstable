
#' Turns a `crosstable` object into a formatted `gt` table
#'
#' @param data the result of [crosstable()]
#' @param show_test_name in the `test` column, show the test name
#' @param by_header a string to override the `by` header
#' @param keep_id whether to keep the `.id` column
#' @param generic_labels names of the crosstable default columns 
#' @param ... unused
#'
#' @author Dan Chaltiel
#' @describeIn as_gt Turns a `crosstable` object into a formatted `gt` table.
#' @family as_gt methods
#' @seealso [as_flextable.crosstable()]
#' 
#' @importFrom checkmate expect_class vname
#' @importFrom stringr str_remove
#' @importFrom dplyr %>% mutate across everything any_of lag select
#' @importFrom glue glue
#' @importFrom gt gt tab_spanner
#' @export
#'
#' @examples
#' xx = mtcars2 %>% select(1:9)
#' crosstable(xx) %>% as_gt
#' crosstable(xx, by=am) %>% as_gt
#' crosstable(xx, by=am, test=T, total=T, effect=T) %>% as_gt(keep_id=TRUE)
as_gt.crosstable = function(data, show_test_name = TRUE, 
                            by_header = NULL, keep_id = FALSE,
                            generic_labels=list(id = ".id", variable = "variable", value = "value", 
                                                total="Total", label = "label", test = "test", 
                                                effect="effect"), 
                            ...) {
    
    expect_class(data, "crosstable", label=vname(data))
    if (inherits(data, "compacted_crosstable")) {
        stop("`as_gt` is not implemented for compacted crosstable yet.")
    }
    
    # has_effect = attr(data, "has_effect")
    # has_total = attr(data, "has_total")
    # has_label = attr(data, "has_label")
    by_label = attr(data, "by_label")
    by_levels = attr(data, "by_levels") %>% replace_na("NA")
    by = attr(data, "by")
    has_by =  !is.null(by)
    if(has_by && is.null(by_label)) by_label=by
    showNA = attr(data, "showNA")
    if(showNA=="always") by_levels=c(by_levels, "NA")
    
    has_test = attr(data, "has_test")
    test=generic_labels$test
    if (has_test && !is.null(data[[test]]) && !show_test_name) {
        data[[test]] = str_remove(data[[test]], "\\n\\(.*\\)")
    }
    
    if(keep_id) {
        group_glue="{label} (`{.id}`)"
    } else {
        group_glue="{label}"
    }
    
    rtn = data %>% 
        mutate(
            across(everything(), replace_na, replace="NA"), 
            across(any_of(c("test", "effect")), 
                   ~ifelse(is.na(lag(.x)) | .x!=lag(.x), .x, "")), 
            groupname=glue(group_glue)) %>% 
        select(-.id, -label) %>% 
        gt(groupname_col="groupname", rowname_col="variable")
    
    if(has_by){
        if(!is.null(by_header)) by_label=by_header
        rtn = rtn %>%
            tab_spanner(label=by_label, columns=any_of(by_levels))
    }
    
    rtn
}


#' Generic method
#' 
#' @param data 
#' @param ... 
#'
#' @name as_gt 
#' @rdname as_gt 
#' @family as_gt methods
#' @seealso [gt::gt()]
#' @export
as_gt = function(data, ...){
    if (!requireNamespace("gt", quietly = TRUE)) {
        stop("Package \"gt\" is obviously needed for this function to work. Please install it.",
             call. = FALSE)
    }
    UseMethod("as_gt")
}


#' @importFrom gt gt
#' @family as_gt methods
#' @export
as_gt.default = function(data, ...){
    gt(data, ...)
}


