

#' Turns a `crosstable` object into a formatted `flextable`
#'
#' @param x the result of [crosstable()]
#' @param autofit whether to use [flextable::autofit()] on the table
#' @param compact whether to compact the table
#' @param show_test_name in the `test` column, show the test name
#' @param by_header a string to override the `by` header
#' @param keep_id whether to keep the `.id` column
#' @param generic_labels names of the crosstable default columns 
#' @param ... unused
#'
#' @author Dan Chaltiel
#' @aliases ctf cross_to_flextable
#' @describeIn as_flextable Turns a `crosstable` object into a formatted `flextable`.
#' @seealso [crosstable()], [flextable::flextable()], [as_gt.crosstable()]
#' 
#' @importFrom dplyr select lead sym %>%
#' @importFrom stringr str_replace str_replace_all str_remove
#' @importFrom flextable flextable autofit add_header_row merge_v merge_h bold align hline_top hline_bottom border_inner_h hline fix_border_issues as_flextable
#' @importFrom officer fp_border
#' @importFrom checkmate expect_class vname
#' @importFrom tibble as_tibble
#' @importFrom tidyr replace_na
#' @export
#'
#' @examples 
#' #Crosstables
#' library(crosstable)
#' library(dplyr)
#' crosstable(mtcars2) %>% as_flextable()
#' crosstable(mtcars2, by=vs, test = TRUE) %>% as_flextable()
#' crosstable(esoph, by=tobgp, test = TRUE) %>% as_flextable()
#' crosstable(iris, by=Species, test = TRUE) %>% as_flextable(autofit=FALSE)
#' crosstable(iris, by=Species, test = TRUE) %>% as_flextable(compact=TRUE)
#' crosstable(iris) %>% as_flextable(compact=TRUE, autofit=TRUE)
#' 
#' #Renaming (because why not?)
#' crosstable(iris, by=Species, total="both", test=TRUE, effect=TRUE) %>%
#'    rename(ID=.id, math=variable, Tot=Total, lab=label, pval=test, fx=effect) %>%
#'    as_flextable(by_header = "The specie", 
#'                 generic_labels=list(id = "ID", variable = "math", total="Tot", 
#'                                     label = "lab", test = "pval", effect="fx"))
as_flextable.crosstable = function(x, autofit = TRUE, compact = FALSE, show_test_name = TRUE, 
                                   by_header = NULL, keep_id = FALSE,
                                   generic_labels=list(id = ".id", variable = "variable", value = "value", 
                                                       total="Total", label = "label", test = "test", 
                                                       effect="effect"), 
                                   ...) {
    expect_class(x, "crosstable", label=vname(x))
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
    if (compact && !inherits(x, "compacted_crosstable")) {
        x = compact.crosstable(x)
    }
    
    rtn = x %>% mutate_all(replace_na, replace="NA")
    
    if(inherits(x, "compacted_crosstable")) {
        rows = attr(x, "title_rows")
        title_rows=which(rows)+(0:(sum(rows)-1))
        rtn = rtn %>% 
            flextable %>% 
            border(title_rows, border.top = fp_border()) %>%
            bold(title_rows) %>% 
            align(title_rows, align="left")
    } else {
        sep.rows = which(rtn[[id]] != lead(rtn[[id]]))
        if(keep_id) {
            cols = rtn %>% names()
            body_merge = setdiff(names(rtn), 
                                 c(generic_labels[c("variable", "value", "total")], by_levels))
        } else {
            cols = rtn %>% select(-any_of(id)) %>% names()
            body_merge = setdiff(names(rtn), 
                                 c(generic_labels[c("variable", "value", "total", "id")], by_levels))
        }
        rtn = rtn  %>% 
            flextable(col_keys=cols) %>% 
            hline(i=sep.rows, border=border1) %>% 
            merge_v(j=id, target=body_merge, part = "body")
    }
    
    if(has_by) {
        byname = if(has_label) by_label else by
        header_values = x %>% names
        header_values = ifelse(header_values %in% by_levels, byname, header_values) %>% unique
        if(!keep_id) header_values = setdiff(header_values, id)
        header_colwidths = ifelse(header_values==byname, sum(names(x) %in% labs.names), 1)
        if(!is.null(by_header))
            header_values = header_values %>% str_replace(byname, by_header)
        rtn = rtn %>% 
            add_header_row(values = header_values, colwidths = header_colwidths) %>% 
            merge_v(part = "head")
    }
    
    
    rtn = rtn %>% 
        bold(part = "head") %>% 
        align(align = "left", part = "all") %>% 
        align(i = 1, align = "center", part = "head") %>% 
        hline_top(border = border2, part = "head") %>% 
        hline_bottom(border = border2, part = "head") %>% 
        border_inner_h(border = border2, part = "head") %>% 
        fix_border_issues
    
    if (autofit) {
        rtn = autofit(rtn)
    }
    return(rtn)
}



#' @usage NULL
#' @importFrom lifecycle deprecate_warn
#' @export
cross_to_flextable = function(...){
    deprecate_warn("0.1.0", "cross_to_flextable()", "as_flextable()")
    as_flextable.crosstable(...)
}
#' @usage NULL
#' @export
ctf = cross_to_flextable

#' @name as_flextable 
#' @rdname as_flextable 
#' @importFrom flextable as_flextable
#' @export
flextable::as_flextable
