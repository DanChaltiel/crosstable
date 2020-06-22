
#' Summarize numeric variables
#' @importFrom checkmate assert_numeric assert_character
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr rename mutate_if
#' @keywords internal
#' @noRd
summarize_numeric_single = function(x,funs,funs_arg){
    assert_numeric(x)
    assert_character(funs)
    
    fun = do.call(funs2fun, as.list(funs))
    rtn = do.call(fun, c(list(x=x), funs_arg)) %>% 
        mutate_if(is.numeric, format_fixed, !!!funs_arg)
    data.frame(value=t(rtn)) %>% rownames_to_column("variable")
}




#' Summarize numeric by categorial
#' @importFrom checkmate assert_numeric assert_character assert_scalar
#' @importFrom tibble tibble
#' @importFrom forcats fct_explicit_na
#' @importFrom dplyr group_by mutate ungroup mutate_at vars arrange filter .data
#' @importFrom tidyr nest unnest pivot_wider replace_na
#' @importFrom purrr map imap reduce
#' @keywords internal
#' @noRd
summarize_numeric_factor = function(x, by, funs, funs_arg, showNA, total, 
                                    cor_digits, cor_method, test, test_args, 
                                    effect, effect_args){
    assert_numeric(x)
    assert_character(funs)
    assert_scalar(showNA)
    .=NULL #mute the R CMD Check note
    .na=.effect=.test=.total=NULL
    if(effect) 
        .effect = effect_args$show_effect(effect_args$effect_summarize(x, by, effect_args$conf_level), 
                                          digits = cor_digits)
    if(test) 
        .test = test_args$display_test(test_args$test_summarize(x, by), digits = test_args$plim, 
                                    method = test_args$show_method)
    if (identical(total, 1) | identical(total, 1:2) | identical(total, TRUE))
        .total = summarize_numeric_single(x, funs=funs, funs_arg=funs_arg)[["value"]]
    
    .showNA = showNA == "always" | (showNA == "ifany" && anyNA(by))
    if(showNA == "always" && !anyNA(by)){
        .na="no NA"
    }
    
    if(.showNA==FALSE) by = by[!is.na(by)]
    by = fct_explicit_na(by, "NA")
    rtn = unique(by) %>% sort() %>% set_names() %>% 
        map(~summarize_numeric_single(x[by==.x], funs=funs, funs_arg=funs_arg)) %>% 
        imap(~rename(.x, !!.y:=value)) %>% 
        reduce(left_join, by="variable") %>%
        {if(!"NA" %in% names(.)){mutate(.,"NA"=.na)} else .} %>%
        mutate(Total=.total, effect=.effect, test=.test) %>%
        mutate_all(as.character)
    
    rtn
}




#' Summarize numeric by numeric (correlation)
#' @importFrom checkmate assert_numeric assert_character assert_string assert_count assert_logical assert_list
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
    
    if(test) .test=test_args$display_test(ct) else .test=NULL
    
    tibble(variable=method, value=as.character(value)) %>% mutate(test=.test)
}

