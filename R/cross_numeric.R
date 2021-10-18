
#' @keywords internal
#' @importFrom glue glue
#' @importFrom rlang :=
#' @importFrom dplyr rename select everything .data
#' @importFrom checkmate assert assert_numeric assert_character
#' @noRd
cross_numeric = function(data_x, data_y, funs, funs_arg, showNA, total,
                         label, cor_digits, cor_method,  test, test_args, effect, effect_args) {
    assert(ncol(data_x)==1)
    assert(is.null(data_y) || ncol(data_y)==1)
    assert_numeric(data_x[[1]])
    
    if(label){
        x_name = get_label(data_x)
        y_name = get_label(data_y)
    } else {
        x_name = names(data_x)
        y_name = names(data_y)
    }

    if(is.null(data_y)){
        rtn = summarize_numeric_single(data_x[[1]], funs, funs_arg)
    } else if(is.character.or.factor(data_y[[1]])) {
        rtn = summarize_numeric_factor(data_x[[1]], data_y[[1]], funs, funs_arg, showNA, total, 
                                       cor_digits, cor_method,  test, test_args, 
                                       effect, effect_args)
    } else if(!is.date(data_x[[1]]) && is.numeric.and.not.surv(data_y[[1]])){
        rtn = summarize_numeric_numeric(data_x[[1]], data_y[[1]], method=cor_method, 
                                        digits=cor_digits, test=test, test_args) %>% 
            rename(!!y_name:=.data$value)
    } else {
        return(NULL)
    }
    
    rtn = rtn %>% 
        mutate(.id=names(data_x), label=x_name) %>% 
        select(.data$.id, .data$label, everything()) %>% 
        mutate_all(as.character)
        
    rtn
}


#' Summarize numeric variables
#' @importFrom checkmate assert_numeric assert_character
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr rename mutate_if
#' @keywords internal
#' @noRd
summarize_numeric_single = function(x, funs, funs_arg){
    imap_dfr(funs, ~{
        v = do.call(.x, c(list(x), funs_arg))
        if(length(v)<2){
            variable = .y
        } else {
            variable=names(v)
            if(.y!=" "){
                variable = paste(.y, variable)
            }
        }
        if(length(variable)!=length(v)){
            abort("Summary functions should return a single value", 
                  class="crosstable_summary_not_scalar")
        }
        
        data.frame(variable=variable, value=v) %>% 
            mutate_if(is.numeric, format_fixed, !!!funs_arg)
    })
}


#' Summarize numeric by categorical
#' @importFrom checkmate assert_numeric assert_character assert_scalar
#' @importFrom tibble tibble
#' @importFrom dplyr group_by mutate ungroup mutate_at vars arrange filter .data
#' @importFrom tidyr nest unnest pivot_wider replace_na
#' @importFrom purrr map imap reduce
#' @importFrom forcats fct_explicit_na
#' @keywords internal
#' @noRd
summarize_numeric_factor = function(x, by, funs, funs_arg, showNA, total, 
                                    cor_digits, cor_method, test, test_args, 
                                    effect, effect_args){
    assert_numeric(x)
    assert_scalar(showNA)
    .=NULL #mute the R CMD Check note
    .na=.effect=.test=.total=NULL
    .showNA = showNA == "always" | (showNA == "ifany" && anyNA(by))
    
    if(effect){
        .effect = effect_args$effect_display(effect_args$effect_summarize(x, by, effect_args$conf_level), 
                                             digits = cor_digits)
    }
    if(test){
        .test = test_args$test_display(test_args$test_summarize(x, by), digits = test_args$plim, 
                                       method = test_args$show_method)
    }
    if(identical(total, 1) | identical(total, 1:2) | identical(total, TRUE)){
        .total = summarize_numeric_single(x, funs=funs, funs_arg=funs_arg)[["value"]]
    }
    
    if(.showNA==TRUE) {
        if(!anyNA(by)) .na="no NA"
        by_filter=TRUE
        by = fct_explicit_na(by, "NA")
    } else{
        by_filter = !is.na(by) 
    }
    
    by(x[by_filter], by[by_filter], summarize_numeric_single, funs=funs, funs_arg=funs_arg) %>% 
        imap_dfr(~{
            if(is.null(.x)) .x=summarize_numeric_single(numeric(0), funs=funs, funs_arg=funs_arg)
            mutate(.x, by=.y, .before=1)
        }) %>%
        pivot_wider(names_from = "by") %>%
        {if(!is.null(.na)){mutate(.,"NA"=.na)} else .} %>%
        mutate(Total=.total, effect=.effect, test=.test, 
               across(everything(), as.character)) %>%
        ungroup()
}



#' Summarize numeric by numeric (correlation)
#' @importFrom checkmate assert_numeric assert_string assert_count assert_logical assert_list
#' @importFrom tibble tibble
#' @importFrom dplyr mutate .data
#' @importFrom stats cor.test
#' @importFrom glue glue
#' @keywords internal
#' @noRd
summarize_numeric_numeric = function(x, by, method, digits, test, test_args){
    assert_numeric(x)
    assert_numeric(by)
    assert_string(method)
    assert_count(digits)
    assert_logical(test)
    assert_list(test_args)
    
    ct=test_args$test_correlation(x, by, method=method)
    
    cor=round(ct$estimate, digits=digits)
    if(!is.null(ct$conf.int)){
        ci=round(ct$conf.int, digits=digits)
        value=glue("{cor} \n95%CI [{ci[1]};{ci[2]}]")
    } else {
        value=glue("{cor}")
    }
    
    if(test) .test=test_args$test_display(ct) else .test=NULL
    
    tibble(variable=method, value=as.character(value)) %>% mutate(test=.test)
}

