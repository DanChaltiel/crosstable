
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
    # browser()
    data.frame(value=t(rtn)) %>% rownames_to_column("variable")
}




#' Summarize numeric by categorial
#' @importFrom checkmate assert_numeric assert_character assert_scalar
#' @importFrom tibble tibble
#' @importFrom dplyr group_by mutate ungroup mutate_at vars arrange filter .data
#' @importFrom tidyr nest unnest pivot_wider replace_na
#' @importFrom purrr map
#' @keywords internal
#' @noRd
summarize_numeric_factor = function(x, by, funs, funs_arg, showNA, total, 
                                    cor_digits, cor_method,  test, test_args, 
                                    effect, effect_args){
    assert_numeric(x)
    assert_character(funs)
    assert_scalar(showNA)
    .=NULL #mute the R CMD Check note
    .na=.effect=.test=.total=NULL
    if(effect) 
        .effect = effect_args$show.effect(effect_args$effect.summarize(x, by, effect_args$conf.level), 
                                          digits = cor_digits)
    if(test) 
        .test = test_args$show.test(test_args$test.summarize(x, by), digits = test_args$plim, 
                                    method = test_args$show.method)
    if (identical(total, 1) | identical(total, 1:2) | identical(total, TRUE))
        .total = summarize_numeric_single(x, funs=funs, funs_arg=funs_arg)[["value"]]
    
    .showNA = showNA == "always" | (showNA == "ifany" && anyNA(by))
    if(showNA == "always" && !anyNA(by)){
        .na="no NA"
    }
    
    rtn = tibble(x, by) %>% 
        group_by(by) %>% 
        nest() %>% 
        filter(.showNA | by!="no") %>%
        mutate(data=map(.data$data, ~summarize_numeric_single(x=.x[[1]], funs=funs, funs_arg=funs_arg))) %>%
        unnest(cols=c(.data$data)) %>% 
        arrange(by) %>% 
        ungroup() %>% mutate_at(vars(by), unlab) %>% #bug in pivotwider (tidyr/issues/831)
        pivot_wider(names_from="by", values_from = "value") %>% 
        {if(!"NA" %in% names(.)){mutate(.,"NA"=.na)} else .} %>%
        mutate(Total=.total, effect=.effect, test=.test) %>%
        mutate_all(as.character)
    
    rtn
}







#' Summarize numeric by numeric (correlation)
#' @importFrom checkmate assert_numeric assert_character
#' @importFrom tibble tibble
#' @importFrom dplyr mutate .data
#' @importFrom stats cor.test
#' @importFrom glue glue
#' @keywords internal
#' @noRd
summarize_numeric_numeric = function(x, by, method, digits, test, test_args){
    assert_numeric(x)
    assert_numeric(by)
    
    ct=cor.test(x, by, method = method)
    
    .test=NULL
    if(test) .test=paste0(plim(ct$p.value, test_args$plim), "\n(",ct$method, ")")
    
    value=glue("{cor} \n95%CI [{ci[1]};{ci[2]}]", 
               cor=round(ct$estimate, digits=digits), ci=round(ct$conf.int, digits=digits))
    
    tibble(variable=method, value=as.character(value)) %>% mutate(test=.test)
}



