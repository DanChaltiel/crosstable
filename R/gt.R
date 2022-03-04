#' Converts a `crosstable` object into a formatted `gt` table.
#' 
#' @param x the result of [crosstable()]
#' @param show_test_name in the `test` column, show the test name
#' @param by_header a string to override the `by` header
#' @param keep_id whether to keep the `.id` column
#' @param generic_labels names of the crosstable default columns 
#' @param ... unused
#' 
#' @return a formatted `gt` table
#'
#' @author Dan Chaltiel
#' @family as_gt methods
#' @describeIn as_gt For crosstables
#' @seealso [as_flextable.crosstable()]
#' 
#' @author Dan Chaltiel
#' @export
#' @importFrom checkmate assert_class vname
#' @importFrom stringr str_remove
#' @importFrom dplyr %>% mutate across everything any_of lag select
#' @importFrom glue glue
#'
#' @examples
#' xx = mtcars2 %>% dplyr::select(2:10)
#' crosstable(xx) %>% as_gt
#' crosstable(xx, by=am) %>% as_gt
#' crosstable(xx, by=cyl, test=TRUE, total=TRUE, effect=TRUE) %>% 
#'    as_gt(keep_id=TRUE, show_test_name=FALSE, by_header="Cylinders")
as_gt.crosstable = function(x, show_test_name = TRUE, 
                            by_header = NULL, keep_id = FALSE,
                            generic_labels=list(id = ".id", variable = "variable", value = "value", 
                                                total="Total", label = "label", test = "test", 
                                                effect="effect"), 
                            ...) {
    
    assert_class(x, "crosstable", .var.name=vname(x))
    if (inherits(x, "compacted_crosstable")) {
        abort("`as_gt` is not implemented for compacted crosstables yet.",
              class="gt_compact_not_implemented_error")
    }
    if (inherits(x, "crosstable_multiby")) {
        abort("`as_gt` is not implemented for multi-by crosstables yet.",
              class="gt_multiby_not_implemented_error")
    }
    
    by_label = attr(x, "by_label")
    by_levels = attr(x, "by_levels") %>% unlist() %>% unname() %>% replace_na("NA")
    by = attr(x, "by")
    has_by =  !is.null(by)
    if(has_by && is.null(by_label)) by_label=by
    showNA = attr(x, "showNA")
    if(showNA=="always") by_levels=c(by_levels, "NA")
    
    has_test = attr(x, "has_test")
    generic_labels = get_generic_labels(generic_labels)
    test=generic_labels$test
    if (has_test && !is.null(x[[test]]) && !show_test_name) {
        x[[test]] = str_remove(x[[test]], "\\n\\(.*\\)")
    }
    
    if(keep_id) {
        group_glue="{label} (`{.id}`)"
    } else {
        group_glue="{label}"
    }
    
    rtn = x %>% 
        mutate(
            across(everything(), replace_na, replace="NA"), 
            across(any_of(c("test", "effect")), 
                   ~ifelse(is.na(lag(.x)) | .x!=lag(.x), .x, "")), 
            groupname=glue(group_glue)) %>% 
        select(-.data$.id, -.data$label) %>% 
        gt::gt(groupname_col="groupname", rowname_col="variable")
    
    if(has_by){
        if(!is.null(by_header)) by_label=by_header
        rtn = rtn %>%
            gt::tab_spanner(label=by_label, columns=any_of(by_levels))
    }
    
    rtn
}


#' Method to convert an object to a `gt` table
#'
#' @param x object to be converted
#' @param ... arguments for custom methods
#'
#' @family as_gt methods
#' @seealso [gt::gt()]
#' @author Dan Chaltiel
#' @export
as_gt = function(x, ...){
    assert_is_installed("gt", "as_gt()")
    UseMethod("as_gt")
}


#' Default method to convert an object to a `gt` table
#'
#' @param x object to be converted
#' @param ... arguments for custom methods
#' 
#' @describeIn as_gt default function
#'
#' @family as_gt methods
#' @author Dan Chaltiel
#' @export
as_gt.default = function(x, ...){
    gt::gt(data=x, ...)
}



