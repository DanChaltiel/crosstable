

#' @importFrom dplyr select mutate mutate_all everything .data
#' @keywords internal
#' @noRd
cross_categorical=function(data_x, data_y, showNA, total, label, percent_digits, percent_pattern,
                           test, test_args, effect, effect_args){
    
    stopifnot(ncol(data_x)==1 && (is.null(data_y) || ncol(data_y)==1))
    stopifnot(is.character.or.factor(data_x[[1]]))
    
    if(label){
        x_name = get_label(data_x)
        y_name = get_label(data_y)
    } else {
        x_name = names(data_x)
        y_name = names(data_y)
    }
    
    if(is.null(data_y)){
        rtn=summarize_categorical_single(data_x, 
                                         percent_pattern=percent_pattern, showNA=showNA, 
                                         total=total, digits=percent_digits)
    } else if(is.character.or.factor(data_y[[1]])){
        rtn=summarize_categorical_by(data_x[[1]], data_y[[1]], 
                                     percent_pattern=percent_pattern, showNA=showNA, 
                                     total=total, digits=percent_digits, 
                                     test=test, test_args=test_args, 
                                     effect=effect, effect_args=effect_args)
    } else {
        return(NULL)
    }
    
    rtn = rtn %>% 
        mutate(.id=names(data_x), label=x_name) %>% 
        select(.data$.id, .data$label, everything()) %>% 
        mutate_all(as.character)
    
    rtn
}


#' @importFrom checkmate assert_numeric assert_character
#' @importFrom stringr str_starts
#' @importFrom glue glue
#' @importFrom dplyr mutate mutate_at mutate_all vars select .data
#' @keywords internal
#' @noRd
summarize_categorical_single = function(x, showNA, total, digits, percent_pattern){
    tbd = table(x, useNA = "no") %>%
        as.data.frame(stringsAsFactors=FALSE) %>%
        select(x=1, n=2) #needed for an odd bug on fedora-devel
    
    any_p = get_glue_vars(percent_pattern) %>% str_starts("p") %>% any()
    pattern = if(!any_p) "{n}" else "{n} ({p})"
    
    rtn = tbd %>% 
        mutate(
            p=.data$n/sum(.data$n), 
            p=format_fixed(.data$p, digits=digits, percent=TRUE),
            value=glue(pattern)
        ) %>% 
        select(variable="x", value="value")
    .showNA = showNA=="always" || showNA=="ifany" && (anyNA(x))
    if(.showNA){
        rtn = rbind(rtn, data.frame(variable="NA", value=sum(is.na(x))))
    }
    
    if (2 %in% total){
        value = glue("{sum(table(x, useNA='always'))} ({format_fixed(100, digits=digits)}%)")
        if(!any_p) value = sum(table(x, useNA='always'))
        rtn = rbind(rtn, data.frame(variable="Total", value=value))
    }
    
    rtn %>% mutate_all(as.character)
}



#' @importFrom dplyr mutate mutate_at mutate_all transmute vars starts_with left_join pull .data
#' @importFrom purrr map reduce safely
#' @importFrom tidyr unite pivot_wider
#' @importFrom glue glue
#' @keywords internal
#' @noRd
summarize_categorical_by = function(x, by, 
                                    percent_pattern, margin, 
                                    showNA, total, digits, 
                                    test, test_args, effect, effect_args){
    dummy = safely(glue)(percent_pattern, n=1, p_cell=1, p_row=1, p_col=1)
    if(!is.null(dummy$error)){
        abort(c("`percent_pattern` should only consider variables {n}, {p_cell}, {p_row}, and {p_col}", 
                i=glue('percent_pattern: "{percent_pattern}"'), x=dummy$error$message))
    }
    
    nn = table(x, by, useNA=showNA)
    .tbl = as.data.frame(nn, responseName="Freq", stringsAsFactors=FALSE)
    
    table_n = as.data.frame(nn, responseName="n", stringsAsFactors=FALSE)
    table_p_cell = getTable(x, by, type="p_cell")
    table_p_row =  getTable(x, by, type="p_row")
    table_p_col =  getTable(x, by, type="p_col")
    
    rtn = reduce(list(table_n, table_p_cell, table_p_row, table_p_col),
                 left_join, by=c("x", "by")) %>%
        mutate(
            across(starts_with("p"), ~format_fixed(.x, digits=digits, percent=TRUE)),
            value=ifelse(is.na(x)|is.na(by), .data$n, glue(percent_pattern))
        ) %>%
        transmute(variable=replace_na(x, "NA"), by=.data$by, value=.data$value) %>%
        pivot_wider(names_from="by", values_from = "value")
    
    
    
    if(2 %in% total){
        mt = margin.table(nn, margin=2) %>% as.numeric()
        line = mt
        any_p = get_glue_vars(percent_pattern) %>% str_starts("p") %>% any()
        if(any_p){
            mt2 = margin.table(table(x, by, useNA="no"), margin=2) %>% as.numeric()
            pct = format_fixed(100*prop.table(mt2), digits) %>% paste0("%")
            length(pct) = length(mt) #expands with NA
            line = paste0(mt,ifelse(is.na(pct), "", glue(" ({pct})")))
        }
        rtn=rbind(rtn, c("Total", line))
    }
    
    .effect=.test=.total=NULL
    if(1 %in% total){
        .total = summarize_categorical_single(x=x, showNA=showNA, total=total, 
                                              digits=digits, percent_pattern=percent_pattern)$value
    }
    if(effect) {
        e = effect_args$effect_tabular(x, by, effect_args$conf_level)
        .effect = effect_args$effect_display(e, digits = effect_args$digits)
    }
    if(test) {
        .test = test_args$test_display(test_args$test_tabular(x, by), digits = test_args$plim, 
                                       method = test_args$show_method)
    }
    rtn %>%
        mutate(Total=.total, effect=.effect, test=.test) %>% 
        mutate_all(as.character)
}



# Utils -------------------------------------------------------------------

#' @importFrom rlang as_function
#' @keywords internal
#' @noRd
getTable = function(x, by, type=c("n", "p_cell", "p_row", "p_col")){
    fun = switch(type,
                 n=identity,
                 p_cell=as_function(~.x/sum(.x, na.rm=TRUE)),
                 p_row=as_function(~prop.table(.x, margin=1)),
                 p_col=as_function(~prop.table(.x, margin=2))
    )
    table(x, by, useNA="no") %>% fun() %>%
        as.data.frame(responseName=type, stringsAsFactors=FALSE)
}

#' @importFrom stringr str_match_all
#' @keywords internal
#' @noRd
get_glue_vars = function(.x){
    str_match_all(.x, "\\{(.*?)\\}")[[1]][,2]
}


#' @importFrom purrr map
#' @importFrom rlang abort warn
#' @importFrom glue glue glue_collapse
#' @keywords internal
#' @noRd
#' @examples 
#' get_percent_pattern(margin=TRUE)
#' get_percent_pattern(margin=1)
#' get_percent_pattern(margin=c(1,0,2))
#' get_percent_pattern(margin=1:2)
#' get_percent_pattern(margin=2:1)
#' get_percent_pattern(margin="row")
#' get_percent_pattern(margin=c("row","cells","column"))
#' get_percent_pattern(margin=c("row", "rows","cells")) #warn
#' get_percent_pattern(margin=c("row","cells", "rows","column")) #warn
#' get_percent_pattern(margin=c("foobar", "rows","cells")) #error
get_percent_pattern = function(margin){
    
    if(length(margin)==1){
        if(margin %in% list(-1, "none")){
            return("{n}")
        } else if(isTRUE(margin)){
            return("{n} ({p_row} / {p_col})")
        } else if(margin=="all"){
            return("{n} ({p_cell} / {p_row} / {p_col})")
        }
    }
    
    marginopts = list(p_row = c(1, "row", "rows"),
                      p_col = c(2, "col", "cols", "column", "columns"),
                      p_cell = c(0, "cell", "cells"))
    unexpected = margin[!margin %in% unlist(marginopts)]
    if(length(unexpected)>0){
        abort(c(glue("Unexpected margin values: `{paste(unexpected, collapse='`, `')}`."), 
                i='Margins should be in c("row", "column", "cell", "none", "all")'), 
              class="XXX") #TODO implement this ERROR
    }
    x = marginopts %>% 
        map(~{ #not map_dbl :-( # https://github.com/tidyverse/purrr/issues/841
            rtn = na.omit(match(.x, margin))
            if(length(rtn)>1){ 
                a = glue_collapse(margin[rtn], "`, `")
                warn(glue("Duplicated margins: `{a}`"), class="XXX") #TODO TODO!
            }
            rtn[1]
        }) %>% 
        unlist() %>% sort() %>% names()
    x = glue_collapse(glue("{{{x}}}"), " / ")
    return(glue("{{n}} ({x})"))
}
