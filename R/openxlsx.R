
#' Converts a `crosstable` object into a formatted, savable `openxlsx` workbook.
#' 
#' @param x the result of [crosstable()]
#' @param show_test_name in the `test` column, show the test name
#' @param by_header a string to override the `by` header
#' @param keep_id whether to keep the `.id` column
#' @param generic_labels names of the crosstable default columns 
#' @param ... unused
#' 
#' @return an `openxlsx` workbook containing the crosstable
#'
#' @author Dan Chaltiel
#' 
#' @importFrom checkmate assert_class vname
#' @importFrom stringr str_remove
#' @importFrom purrr walk
#' @importFrom tidyr replace_na
#' @importFrom dplyr %>% mutate across everything any_of lag lead select
#' @importFrom glue glue
#' @export
#'
#' @examples
#' library(openxlsx)
#' target = tempfile(fileext=".xlsx")
#' x=crosstable(mtcars2, c(mpg, vs, gear), total=TRUE, test=TRUE)
#' x %>% 
#'     as_workbook(keep_id=TRUE) %>% 
#'     saveWorkbook(file=target, overwrite = TRUE)
#' #browseURL(target)
as_workbook = function(x, show_test_name = TRUE, 
                       by_header = NULL, keep_id = FALSE,
                       generic_labels=list(id = ".id", variable = "variable", value = "value",
                                           total="Total", label = "label", test = "test",
                                           effect="effect"),
                       ...){
    
    assert_class(x, "crosstable", .var.name=vname(x))
    if (inherits(x, "compacted_crosstable")) {
        abort("`as_workbook()` is not implemented for compacted crosstable yet.",
              class="compact_not_implemented_error")
    }
    
    border1 = fp_border(color = "black", style = "solid", width = 1)
    border2 = fp_border(color = "black", style = "solid", width = 1.5)
    labs.names = setdiff(names(x), generic_labels)
    
    has_test = attr(x, "has_test")
    has_effect = attr(x, "has_effect")
    has_total = attr(x, "has_total")
    has_label = attr(x, "has_label")
    by_label = attr(x, "by_label")
    by_levels = attr(x, "by_levels") %>% replace_na("NA")
    by = attr(x, "by")
    has_by =  !is.null(by)
    if(has_by && is.null(by_label)) by_label=by
    showNA = attr(x, "showNA")
    if(showNA=="always") by_levels=c(by_levels, "NA")
    
    test=generic_labels$test
    id=generic_labels$id
    label=generic_labels$label
    
    if (has_test && !is.null(x[[test]]) && !show_test_name) {
        x[[test]] = str_remove(x[[test]], "\\n\\(.*\\)")
    }
    
    rtn = x %>% mutate(across(everything(), replace_na, replace="NA"))
    sep.rows = which(rtn[[id]] != lead(rtn[[id]]))
    
    if(keep_id) {
        body_merge = setdiff(names(rtn), 
                             c(generic_labels[c("variable", "value", "total")], by_levels))
    } else {
        rtn = rtn %>% select(-any_of(id))
        body_merge = setdiff(names(rtn), 
                             c(generic_labels[c("variable", "value", "total", "id")], by_levels))
    }
    
    col_right = ncol(rtn)+1
    
    wb <- openxlsx::createWorkbook("Creator of workbook")
    openxlsx::addWorksheet(wb, sheetName="crosstable")
    
    
    default <- openxlsx::createStyle(valign = "center")
    header <- openxlsx::createStyle(halign = "center", textDecoration = "bold")
    border1 <- openxlsx::createStyle(border = "top", borderStyle="medium")
    border2 <- openxlsx::createStyle(border = "top", borderStyle="thin")
    
    if(has_by) {
        if(!is.null(by_header)) by_label=by_header
        byname = if(has_label) by_label else by
        by_cols = which(names(rtn) %in% by_levels)
        not_by_cols = which(!names(rtn) %in% by_levels)
        rtn = rbind(names(rtn), rtn)
        names(rtn)[by_cols] = byname
        rh = 4 #row_header
    } else {
        rh = 3
    }
    
    openxlsx::writeData(wb, sheet="crosstable", x=rtn, startRow=2, startCol=2)
    
    if(has_by) {
        openxlsx::mergeCells(wb, sheet="crosstable", cols=by_cols+1, rows=2)
        openxlsx::addStyle(wb, sheet="crosstable", style = border2, rows = 3, cols = by_cols+1,
                 gridExpand = TRUE, stack=TRUE)
        walk(not_by_cols+1, ~{
            openxlsx::mergeCells(wb, sheet="crosstable", cols=.x, rows=2:3)
        })
    }
    
    border_rows = c(rh, sep.rows+rh, nrow(x)+rh)
    merge_rows_intervals = Map(c, border_rows[-length(border_rows)], border_rows[-1] - 1)
    merge_rows_intervals %>% walk(function(i){#rows
        merge_cols = which(names(rtn) %in% body_merge)
        walk(merge_cols+1, function(j){#cols
            openxlsx::mergeCells(wb, sheet="crosstable", cols=j, rows=i[1]:i[2])
        })
    })
    
    openxlsx::showGridLines(wb, sheet="crosstable", showGridLines = FALSE)
    openxlsx::addStyle(wb, sheet="crosstable", style = default, rows = 1:(nrow(rtn)+rh-1), cols = 1:col_right, 
             gridExpand = TRUE, stack=TRUE)
    openxlsx::addStyle(wb, sheet="crosstable", style = header, rows = 2:(rh-1), cols = 2:col_right, 
             gridExpand = TRUE, stack=TRUE)
    openxlsx::addStyle(wb, sheet="crosstable", style = border1, rows = c(2,rh,nrow(x)+rh), cols = 2:col_right, 
             gridExpand = TRUE, stack=TRUE)
    openxlsx::addStyle(wb, sheet="crosstable", style = border2, rows = sep.rows+rh, cols = 2:col_right, 
             gridExpand = TRUE, stack=TRUE)

    openxlsx::setColWidths(wb, sheet="crosstable", cols = 1:col_right, widths = "auto")
    wb
}

