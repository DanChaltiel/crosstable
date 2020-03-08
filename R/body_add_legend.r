#' Add a table legend
#'
#' @param x a docx object
#' @param legend the table legend
#' @param legend_style may depend on the docx template
#' @param style the legend style (strnog, italic...)
#' @param seqfield to figure this out, in a docx file, insert a table legend, right click on the inserted number and select "Toggle Field Codes". This argument should be the value of the field, with extra escaping.
#'
#' @export
body_add_table_legend = function(x, legend, legend_style="table title", style="strong", 
                                 seqfield="SEQ Table \\* Arabic"){
    x %>% 
        body_add_par(value=legend, style=legend_style) %>% 
        slip_in_text(str=": ", style=style, pos="before") %>% 
        slip_in_seqfield(str=seqfield, 
                         style=style, pos="before") %>% 
        slip_in_text(str="Table ", style=style,
                     pos="before") %>%
        identity
}