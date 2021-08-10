
#' Converts a `crosstable` object into a formatted, savable `openxlsx` workbook.
#' 
#' @param x the result of [crosstable()] or a list of crosstables
#' @param show_test_name in the `test` column, show the test name
#' @param by_header a string to override the `by` header
#' @param keep_id whether to keep the `.id` column
#' @param generic_labels names of the crosstable default columns 
#' @param ... unused
#' 
#' @return an `openxlsx` workbook containing the crosstable(s)
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom checkmate assert_class vname
#' @importFrom stringr str_remove
#' @importFrom purrr walk
#' @importFrom tidyr replace_na
#' @importFrom dplyr %>% mutate across everything any_of lag lead select
#' @importFrom glue glue
#'
#' @examples
#' library(openxlsx)
#' target = tempfile(fileext=".xlsx")
#' 
#' x=crosstable(mtcars2, c(mpg, vs, gear), total=TRUE, test=TRUE)
#' as_workbook(x, keep_id=TRUE) %>% 
#'     saveWorkbook(file=target)
#' if(interactive()) browseURL(target)
#' 
#' x2=list(iris=crosstable(iris2), crosstable(mtcars2))
#' as_workbook(x2, keep_id=TRUE) %>% 
#'     saveWorkbook(file=target)
#' if(interactive()) browseURL(target)
as_workbook = function(x, show_test_name=TRUE, 
                       by_header=NULL, keep_id=FALSE,
                       generic_labels=list(id = ".id", variable = "variable", value = "value",
                                           total="Total", label = "label", test = "test",
                                           effect="effect"),
                       ...){
    assert_is_installed("openxlsx", "as_workbook()")
    
    if(!inherits(x, "list")){
        x=list("crosstable"=x)
    }
    wb = openxlsx::createWorkbook()
    unnamed = 1
    
    for(i in seq_along(x)) {
        .x=x[[i]]
        .name=names(x[i])
        if(.name==""){
            .name = paste0("noname", unnamed)
            unnamed = unnamed + 1
        }
        assert_class(.x, "crosstable", .var.name=vname(ct))
        if(inherits(.x, "compacted_crosstable")) {
            warn(c("`as_workbook()` is not implemented for compacted crosstable yet.",
                   i=glue("Sheet: {.name}")),
                  class="compact_not_implemented_error")
            next
        }
        wb = addToWorksheet(wb, .x, .name, show_test_name, by_header, keep_id, generic_labels)
    }
    wb
}


#' @keywords internal
#' @noRd
addToWorksheet = function(wb, ct, sheetname, show_test_name = TRUE, 
                          by_header = NULL, keep_id = FALSE,
                          generic_labels=list(id = ".id", variable = "variable", value = "value",
                                              total="Total", label = "label", test = "test",
                                              effect="effect"),
                          ...){
    has_test = attr(ct, "has_test")
    has_label = attr(ct, "has_label")
    by_label = attr(ct, "by_label")
    by_levels = attr(ct, "by_levels") %>% replace_na("NA") %>% unlist()
    multiple_by = length(attr(ct, "by_levels"))>1
    
    by = attr(ct, "by")
    has_by =  !is.null(by)
    if(has_by && is.null(by_label)) by_label=by
    showNA = attr(ct, "showNA")
    if(showNA=="always") by_levels=c(by_levels, "NA")
    
    test=generic_labels$test
    id=generic_labels$id
    label=generic_labels$label
    
    if (has_test && !is.null(ct[[test]]) && !show_test_name) {
        ct[[test]] = str_remove(ct[[test]], "\\n\\(.*\\)")
    }
    
    rtn = ct %>% mutate(across(everything(), replace_na, replace="NA"))
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
    
    openxlsx::addWorksheet(wb, sheetName=sheetname)
    
    
    default <- openxlsx::createStyle(valign = "center")
    header <- openxlsx::createStyle(halign = "center", textDecoration = "bold")
    border1 <- openxlsx::createStyle(border = "top", borderStyle="medium")
    border2 <- openxlsx::createStyle(border = "top", borderStyle="thin")
    
    if(has_by && !multiple_by) {
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
    openxlsx::writeData(wb, sheet=sheetname, x=rtn, startRow=2, startCol=2)
    
    if(has_by && !multiple_by) {
        openxlsx::mergeCells(wb, sheet=sheetname, cols=by_cols+1, rows=2)
        openxlsx::addStyle(wb, sheet=sheetname, style = border2, rows = 3, cols = by_cols+1,
                           gridExpand = TRUE, stack=TRUE)
        walk(not_by_cols+1, ~{
            openxlsx::mergeCells(wb, sheet=sheetname, cols=.x, rows=2:3)
        })
    }
    
    border_rows = c(rh, sep.rows+rh, nrow(ct)+rh)
    merge_rows_intervals = Map(c, border_rows[-length(border_rows)], border_rows[-1] - 1)
    merge_rows_intervals %>% walk(function(i){#rows
        merge_cols = which(names(rtn) %in% body_merge)
        walk(merge_cols+1, function(j){#cols
            openxlsx::mergeCells(wb, sheet=sheetname, cols=j, rows=i[1]:i[2])
        })
    })
    
    openxlsx::showGridLines(wb, sheet=sheetname, showGridLines = FALSE)
    openxlsx::addStyle(wb, sheet=sheetname, style = default, rows = 1:(nrow(rtn)+rh-1), cols = 1:col_right, 
                       gridExpand = TRUE, stack=TRUE)
    openxlsx::addStyle(wb, sheet=sheetname, style = header, rows = 2:(rh-1), cols = 2:col_right, 
                       gridExpand = TRUE, stack=TRUE)
    openxlsx::addStyle(wb, sheet=sheetname, style = border1, rows = c(2,rh,nrow(ct)+rh), cols = 2:col_right, 
                       gridExpand = TRUE, stack=TRUE)
    openxlsx::addStyle(wb, sheet=sheetname, style = border2, rows = sep.rows+rh, cols = 2:col_right, 
                       gridExpand = TRUE, stack=TRUE)
    
    openxlsx::setColWidths(wb, sheet=sheetname, cols = 1:col_right, widths = "auto")
    wb
}

