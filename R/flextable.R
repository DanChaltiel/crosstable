

#' Turns a `crosstable` object into a `flextable`
#'
#' @param x the result of [crosstable()]
#' @param compact whether to compact the table
#' @param auto.fit whether to use \code{flextable::autofit} on the table
#' @param show.test.name in the `test` column, show the test name
#' @param generic.labels names of the crosstable default columns 
#' @param by_header a string to override the `by` header
#' @param keep_id whether to keep the `.id` column
#' @param ... unused
#'
#' @author Dan Chaltiel
#' @aliases ctf cross_to_flextable
#' @describeIn as_flextable Turns a `crosstable` object into a formatted `flextable`.\cr You can also use the alias `cross_to_flextable` or its shortcut `ctf()` which has the same arguments.
#' @seealso [crosstable()], [flextable::flextable()]
#' 
#' @importFrom dplyr select lead sym %>% setdiff
#' @importFrom stringr str_replace str_replace_all str_remove
#' @importFrom flextable flextable autofit add_header_row merge_v merge_h bold align hline_top hline_bottom border_inner_h hline fix_border_issues as_flextable
#' @importFrom officer fp_border
#' @export
#'
#' @examples 
#' #Crosstables
#' library(crosstable)
#' library(dplyr)
#' crosstable(mtcars2) %>% as_flextable()
#' crosstable(mtcars2, by=vs, test = TRUE) %>% as_flextable()
#' crosstable(esoph, by=tobgp, test = TRUE) %>% as_flextable()
#' crosstable(iris, by=Species, test = TRUE) %>% as_flextable(auto.fit=FALSE)
#' crosstable(iris, by=Species, test = TRUE) %>% as_flextable(compact=TRUE)
#' crosstable(iris) %>% as_flextable(compact=TRUE, auto.fit=TRUE)
#' 
#' #Renaming
#' crosstable(iris, by=Species, total="both", test=TRUE, effect=TRUE) %>%
#'    rename(ID=.id, math=variable, Tot=Total, lab=label, pval=test, fx=effect) %>%
#'    as_flextable(by_header = "The specie", 
#'                       generic.labels=list(id = "ID", variable = "math", total="Tot", 
#'                                           label = "lab", test = "pval", effect="fx"))
as_flextable.crosstable = function(x, auto.fit = TRUE, compact = FALSE, show.test.name = TRUE, 
                              by_header = NULL, keep_id = FALSE,
                              generic.labels=list(id = ".id", variable = "variable", value = "value", 
                                                  total="Total", label = "label", test = "test", effect="effect"), ...) {
    stopifnot(is.data.frame(x)) 
    # browser()
    border1 = fp_border(color = "black", style = "solid", width = 1)
    border2 = fp_border(color = "black", style = "solid", width = 1.5)
    labs.names = setdiff(names(x), generic.labels)
    
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
    
    test=generic.labels$test
    id=generic.labels$id
    label=generic.labels$label
    
    if (has_test && !is.null(x[[test]]) && !show.test.name) {
        x[[test]] = str_remove(x[[test]], "\\n\\(.*\\)")
    }
    if (compact && !inherits(x, "compacted_crosstable")) {
        x = compact.crosstable(x)
    }
    
    rtn = x %>% mutate_all(replace_na, "NA")
    
    if(inherits(x, "compacted_crosstable")) {
        rows = attr(x, "title_rows")
        title_rows=which(rows)+(0:(sum(rows)-1))
        rtn = rtn %>% 
            flextable %>% 
            border(title_rows, border.top = fp_border()) %>%
            bold(title_rows) %>% 
            align(title_rows, align="left")
    } else {
        if(!keep_id) rtn = rtn %>% select(-any_of(id))
        body_merge = setdiff(names(rtn), c(generic.labels[2:4],by_levels))
        sep.rows = which(rtn[[label]] != lead(rtn[[label]]))
        rtn = rtn %>% 
            flextable %>% 
            hline(i=sep.rows, border=border1) %>% 
            merge_v(j = label, target=body_merge, part = "body")
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
    
    if (auto.fit) {
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
#' @aliases as_flextable.crosstable
#' @export
flextable::as_flextable


