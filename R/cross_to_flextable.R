



#' Crosstables output 
#' 
#' [cross_to_flextable()] turns the result of [crosstable()] into a formatted `flextable` object. \cr
#' You can also use the shortcut `ctf()` which has the same arguments.
#'
#' @param crosstable the result of \code{crosstable} function
#' @param compact whether to compact the table
#' @param auto.fit whether to \code{flextable::autofit} the table
#' @param show.test.name in the `test` column, show the test name
#' @param generic.labels names of the crosstable default columns 
#' @param by_header a string to override the `by` header
#' @param keep_id whether to keep the `.id` column
#'
#' @author Dan Chaltiel
#' @aliases ctf cross_to_flextable
#' @seealso [crosstable()], [flextable::flextable()]
#' 
#' @importFrom dplyr select lead sym %>% setdiff
#' @importFrom methods is
#' @importFrom stringr str_replace str_replace_all str_remove
#' @importFrom flextable flextable autofit add_header_row merge_v merge_h bold align hline_top hline_bottom border_inner_h hline fix_border_issues
#' @importFrom officer fp_border
#' @export
#'
#' @examples 
#' library(dplyr)
#' crosstable(mtcars2) %>% cross_to_flextable()
#' crosstable(mtcars2, by=vs, test = TRUE) %>% cross_to_flextable()
#' crosstable(esoph, by=tobgp, test = TRUE) %>% cross_to_flextable()
#' crosstable(iris, by=Species, test = TRUE) %>% cross_to_flextable(auto.fit=FALSE)
#' crosstable(iris, by=Species, test = TRUE) %>% cross_to_flextable(compact=TRUE)
#' crosstable(iris) %>% cross_to_flextable(compact=TRUE, auto.fit=TRUE)
#' 
#' #Renaming
#' crosstable(iris, by=Species, total="both", test=TRUE, effect=TRUE) %>%
#'    rename(ID=.id, math=variable, Tot=Total, lab=label, pval=test, fx=effect) %>%
#'    cross_to_flextable(by_header = "The specie", 
#'                       generic.labels=list(id = "ID", variable = "math", total="Tot", 
#'                                           label = "lab", test = "pval", effect="fx"))
cross_to_flextable = function(crosstable, auto.fit = TRUE, compact = FALSE, show.test.name = TRUE, 
                              by_header = NULL, keep_id = FALSE,
                              generic.labels=list(id = ".id", variable = "variable", value = "value", 
                                                  total="Total", label = "label", test = "test", effect="effect")) {
    stopifnot(is.data.frame(crosstable)) 
    # browser()
    border1 = fp_border(color = "black", style = "solid", width = 1)
    border2 = fp_border(color = "black", style = "solid", width = 1.5)
    labs.names = setdiff(names(crosstable), generic.labels)
    
    has_test = attr(crosstable, "has_test")
    has_effect = attr(crosstable, "has_effect")
    has_total = attr(crosstable, "has_total")
    has_label = attr(crosstable, "has_label")
    by_label = attr(crosstable, "by_label")
    by_levels = attr(crosstable, "by_levels") %>% replace_na("NA")
    by = attr(crosstable, "by")
    has_by =  !is.null(by)
    if(has_by && by_label=="") by_label=by
    showNA = attr(crosstable, "showNA")
    if(showNA=="always") by_levels=c(by_levels, "NA")
    
    test=generic.labels$test
    id=generic.labels$id
    label=generic.labels$label
    
    if (has_test && !is.null(crosstable[[test]]) && !show.test.name) {
        crosstable[[test]] = str_remove(crosstable[[test]], "\\n\\(.*\\)")
    }
    if (compact && !is(crosstable, "compacted_crosstable")) {
        crosstable = compact.crosstable(crosstable)
    }
    
    rtn = crosstable %>% mutate_all(replace_na, "NA")
    
    if(is(crosstable, "compacted_crosstable")) {
        rows = attr(crosstable, "title_rows")
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
        header_values = crosstable %>% names
        header_values = ifelse(header_values %in% by_levels, byname, header_values) %>% unique
        if(!keep_id) header_values = setdiff(header_values, id)
        header_colwidths = ifelse(header_values==byname, sum(names(crosstable) %in% labs.names), 1)
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



#' @rdname cross_to_flextable
#' @aliases ctf cross_to_flextable
#' @usage NULL
#' @export
ctf = cross_to_flextable
