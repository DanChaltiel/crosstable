

#' Turns a `crosstable` object into a formatted `flextable`
#'
#' @param x the result of [crosstable()].
#' @param keep_id whether to keep the `.id` column. 
#' @param by_header a string to override the `by` header.
#' @param autofit whether to use [flextable::autofit()] on the table.
#' @param compact whether to compact the table. If `TRUE`, see [compact.crosstable()] to see how to use `keep_id`.
#' @param show_test_name in the `test` column, show the test name.
#' @param fontsizes font sizes as a list of keys `c(body, subheaders, header)`. If set through arguments instead of options, all 3 names should be specified.
#' @param padding_v vertical padding (body).
#' @param remove_header_keys if `TRUE` and `x` has several `by` strata, header will only display values.
#' @param header_show_n show the size (`N=xx`) of each terminal `by` strata in their header.
#' @param generic_labels names of the crosstable default columns. Useful for translation for instance. 
#' @param ... unused.
#' 
#' @return a flextable.
#'
#' @author Dan Chaltiel
#' @aliases ctf cross_to_flextable to_flextable
#' @describeIn as_flextable Turns a `crosstable` object into a formatted `flextable`.
#' @seealso [crosstable()], [flextable::flextable()], [as_gt.crosstable()]
#' 
#' @importFrom dplyr select lead sym %>%
#' @importFrom stringr str_replace str_replace_all str_remove
#' @importFrom flextable flextable autofit add_header_row set_header_labels merge_v merge_h bold align hline_top hline_bottom border_inner_h hline fix_border_issues padding as_flextable fontsize vline vline_left vline_right set_header_df
#' @importFrom officer fp_border
#' @importFrom checkmate assert_class vname
#' @importFrom tibble as_tibble
#' @importFrom tidyr replace_na separate
#' @export
#'
#' @examples 
#' #Crosstables
#' library(crosstable)
#' library(dplyr)
#' crosstable_options(crosstable_fontsize_header=14, 
#'                    crosstable_fontsize_subheaders=10, 
#'                    crosstable_fontsize_body=8)
#' crosstable(iris) %>% as_flextable()
#' crosstable(mtcars2, by=c(am, vs)) %>% as_flextable()
#' crosstable(mtcars2, cols=c(mpg, cyl), by=am, effect=TRUE) %>% 
#'    as_flextable(keep_id=TRUE, autofit=FALSE)
#' crosstable(mtcars2, cols=c(mpg, cyl), by=am, effect=TRUE) %>% 
#'    as_flextable(compact=TRUE)
#' 
#' #Renaming (because why not?)
#' crosstable(mtcars2, by=vs, total="both", test=TRUE, effect=TRUE) %>%
#'    rename(ID=.id, math=variable, Tot=Total, lab=label, pval=test, fx=effect) %>%
#'    as_flextable(by_header = "Engine shape", 
#'                 generic_labels=list(id = "ID", variable = "math", total="Tot", 
#'                                     label = "lab", test = "pval", effect="fx"))
as_flextable.crosstable = function(x, keep_id=FALSE, by_header=NULL, 
                                   autofit=TRUE, compact=FALSE, 
                                   show_test_name=TRUE, 
                                   fontsizes=list(body=11, subheaders=11, header=11), 
                                   padding_v=NULL, remove_header_keys=FALSE,
                                   header_show_n=FALSE, 
                                   generic_labels=list(id=".id", variable="variable", value="value", 
                                                       total="Total", label="label", test="test", 
                                                       effect="effect"), 
                                   ...) {
    assert_class(x, "crosstable", .var.name=vname(x))
    
    if(missing(keep_id)) keep_id = getOption("crosstable_keep_id", keep_id)
    if(missing(autofit)) autofit = getOption('crosstable_autofit', autofit)
    if(missing(compact)) compact = getOption('crosstable_compact', compact)
    if(missing(show_test_name)) show_test_name = getOption('crosstable_show_test_name', show_test_name)
    if(missing(padding_v)) padding_v = getOption('crosstable_padding_v', padding_v)
    if(missing(remove_header_keys)) remove_header_keys = getOption('crosstable_remove_header_keys',
                                                                   remove_header_keys)
    if(missing(header_show_n)) header_show_n = getOption('crosstable_header_show_n', header_show_n)
    if(missing(generic_labels)) generic_labels = getOption('crosstable_generic_labels', generic_labels)
    if(missing(fontsizes)) fontsizes = list(
        body=getOption('crosstable_fontsize_body', fontsizes$body),
        subheaders=getOption('crosstable_fontsize_subheaders', fontsizes$subheaders),
        header=getOption('crosstable_fontsize_header', fontsizes$header)
    )
    
    border1 = fp_border(color = "black", style = "solid", width = 1)
    border2 = fp_border(color = "black", style = "solid", width = 1.5)
    labs.names = setdiff(names(x), generic_labels)
    
    has_test = attr(x, "has_test")
    has_effect = attr(x, "has_effect")
    has_total = attr(x, "has_total")
    has_label = attr(x, "has_label")
    by_label = attr(x, "by_label")
    by_table = attr(x, "by_table")
    showNA = attr(x, "showNA")
    by_levels = attr(x, "by_levels") %>% map(~{
        if(showNA=="always") .x=unique(c(.x, NA))
        replace_na(.x, replace="NA")
    })
    by = attr(x, "by")
    has_by =  !is.null(by)
    if(has_by && is.null(by_label)) by_label=by
    # if(showNA=="always") by_levels=c(by_levels, "NA")
    
    
    test=generic_labels$test
    id=generic_labels$id
    label=generic_labels$label
    
    if (has_test && !is.null(x[[test]]) && !show_test_name) {
        x[[test]] = str_remove(x[[test]], "\\n\\(.*\\)")
    }
    if (compact && !inherits(x, "compacted_crosstable")) {
        x = compact.crosstable(x, keep_id=keep_id)
    }
    
    rtn = replace(x, is.na(x), "NA")
    if(header_show_n){
        #TODO faire ça en multiby !
        col_names_n = imap_chr(c(by_table), ~glue("{.y} (n={.x})")) 
        # rtn = rtn %>% rename_with(~ifelse(.x %in% names(col_names_n), col_names_n[.x], .x))
    }
    
    if(inherits(x, "compacted_crosstable")) {
        # if(length(by_levels)>1) abort("Cannot compact a crosstable with multiple `by`.")
        rows = attr(x, "title_rows")
        title_rows = which(rows)+(0:(sum(rows)-1))
        padded_rows = 1:nrow(rtn)
        padded_rows = padded_rows[!padded_rows %in% title_rows]
        header_labels = names(rtn) %>% recode(!!generic_labels$variable:="")
        hl = set_names("", generic_labels$variable)
        rtn = rtn %>% 
            flextable() %>% 
            set_header_labels(values=hl) %>% 
            fontsize(size=fontsizes$body) %>%
            fontsize(i=title_rows, size=fontsizes$subheaders) %>%
            border(title_rows, border.top = fp_border()) %>%
            bold(title_rows, j=1:2) %>% 
            align(title_rows, align="left") %>% 
            padding(i=padded_rows, j=1, padding.left=getOption('crosstable_compact_padding', 25))
    } else {
        sep.rows = which(rtn[[id]] != lead(rtn[[id]]))
        body_merge = intersect(names(rtn), generic_labels[c("label", "test", "effect", "id")]) %>%
            unlist()
        
        if(keep_id) {
            cols = rtn %>% names()
        } else {
            cols = rtn %>% select(-any_of(id)) %>% names()
            body_merge = body_merge[body_merge!=id]
        }
        
        rtn = rtn %>% 
            mutate(
                !!id:=str_wrap2(.data[[id]], width = getOption("crosstable_wrap_id", 70))
            )  %>% 
            flextable(col_keys=cols) %>% 
            fontsize(size=fontsizes$body) %>%
            hline(i=sep.rows, border=border1) %>% 
            merge_v(j=id, target=body_merge, part = "body")
    }
    
    if(length(by_levels)==1) {
        by_levels2 = unlist(by_levels)
        byname = if(has_label) by_label else by
        
        header_mapping = tibble(
            col_keys = names(x), 
            .col_2 = ifelse(names(x) %in% by_levels2, byname, names(x)), 
            .col_1 = col_keys
        )
        
        if(!is.null(by_header)){
            header_mapping = header_mapping %>% 
                mutate(.col_2=str_replace(.col_2,byname, by_header))
        }
        
        if(header_show_n){
            header_mapping = header_mapping %>% 
                mutate(
                    n = by_table[col_keys], 
                    .col_1 = ifelse(!is.na(n), glue("{.col_1} (n={n})"), .col_1)
                ) %>% 
                select(-n)
        }
        rtn = rtn %>% 
            set_header_df(header_mapping, key = "col_keys") %>%
            merge_h(part = "head")
        
        
    } else if(length(by_levels)>1) {
        header_mapping = tibble(col_keys = names(x)) %>% 
            separate(col_keys, into=paste0(".col_", 1:length(by_levels)), 
                     sep=" & ", remove=FALSE, fill="right") %>% 
            select(col_keys, rev(names(.))) %>% 
            mutate(across(starts_with(".col_"), ~ifelse(is.na(.x), col_keys, .x)))
        
        if(remove_header_keys){ 
            header_mapping = header_mapping %>% 
                mutate(across(starts_with(".col_"), ~str_remove(.x, "^.*=")))
        }
        if(header_show_n){
            header_mapping = header_mapping %>% 
                mutate(
                    n = by_table[col_keys], 
                    .col_1 = ifelse(!is.na(n), glue("{.col_1} (n={n})"), .col_1)
                ) %>% 
                select(-n)
        }
        border_left_first = sum(rtn$header$col_keys %in% generic_labels[c("label", "variable", "id")])
        border_separations = header_mapping %>% 
            filter(col_keys %in% rtn$header$col_keys) %>%
            pull(-2) %>% {which(!. %in% generic_labels & .!=lead(.))}
        borders_j = c(border_left_first+1, border_separations+1)
        
        rtn = rtn %>% 
            set_header_df(header_mapping, key = "col_keys") %>%
            merge_h(i=seq.int(length(by_levels)-1), part = "head") %>%
            border(j=borders_j, border.left=border1, part="all") %>%
            vline_left(border=border1) %>% 
            vline_right(border=border1)
    }
    
    rtn = rtn %>%  
        merge_v(part = "head") %>% 
        bold(part = "head") %>% 
        align(align = "left", part = "all") %>% 
        align(align="center", part="head") %>% 
        fontsize(part="head", size=fontsizes$header) %>%
        hline_top(border = border2, part = "head") %>% 
        hline_bottom(border = border2, part = "head") %>% 
        border_inner_h(border = border1, part = "head") %>%
        fix_border_issues()
    if(length(padding_v)!=0)
        rtn = padding(rtn, padding.top=padding_v, padding.bottom=padding_v, part="body")
    
    if (autofit) {
        rtn = autofit(rtn)
    }
    return(rtn)
}

#' @usage NULL
#' @export
#' @rdname as_flextable
to_flextable = as_flextable.crosstable


#' @usage NULL
#' @export
#' @importFrom lifecycle deprecate_warn
#' @rdname as_flextable
cross_to_flextable = function(...){
    deprecate_warn("0.1.0", "cross_to_flextable()", "as_flextable()")# nocov
    as_flextable.crosstable(...)# nocov
}
#' @usage NULL
#' @export
#' @rdname as_flextable
ctf = cross_to_flextable

#' @name as_flextable 
#' @rdname as_flextable 
#' @export
#' @importFrom flextable as_flextable
flextable::as_flextable

#' @usage NULL
#' @export
#' @rdname as_flextable
af = as_flextable.crosstable



#' Open a `crosstable` in a temporary document
#' 
#' This eases copy-pasting
#'
#' @param x a crosstable
#' @param docx if true, peek as a `docx`, else, peek as `xlsx`
#' @param ... passed on to `as_flextable.crosstable()` or to `as_workbook()`
#' 
#' @return Nothing, called for its side effects
#'
#' @author Dan Chaltiel
#' @export
peek = function(x, docx=getOption("crosstable_peek_docx", TRUE), ...) {
    if(docx){
        x=as_flextable.crosstable(x, ...)
        print(x, preview="docx")
    } else {
        x=as_workbook(x, ...)
        filename = tempfile(fileext=".xlsx")
        openxlsx::saveWorkbook(x, file=filename)
        if(interactive()) browseURL(filename)
    }
    invisible()
}
