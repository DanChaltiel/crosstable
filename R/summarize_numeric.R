
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
            v=data.frame(variable=.y, value=v)
        } else {
            if(is.null(names(v))){
                names(v) = paste0("fun", seq_along(v))
                i=glue_collapse(names(v), "', '", last="', and '")
                warn(glue("`funs` member {.y} had unnamed functions. They have been named '{i}' in the crosstable."))
            } else if("" %in% names(v)){
                newnames = paste0("fun", seq_along(names(v)[names(v)==""]))
                i=glue_collapse(newnames, "', '", last="', and '")
                names(v)[names(v)==""] = newnames
                warn(glue("`funs` member {.y} had unnamed functions. They have been named '{i}' in the crosstable."))
            }
            v2 = data.frame(variable=names(v), value=v)
            
            
            v = data.frame(variable=names(v), value=v)
            if(.y!=" "){
                v$variable = paste(.y, v$variable)
            }
        }
        mutate_if(v, is.numeric, format_fixed, !!!funs_arg)
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
    if(effect) 
        .effect = effect_args$show_effect(effect_args$effect_summarize(x, by, effect_args$conf_level), 
                                          digits = cor_digits)
    if(test) 
        .test = test_args$display_test(test_args$test_summarize(x, by), digits = test_args$plim, 
                                       method = test_args$show_method)
    if (identical(total, 1) | identical(total, 1:2) | identical(total, TRUE))
        .total = summarize_numeric_single(x, funs=funs, funs_arg=funs_arg)[["value"]]
    
    .showNA = showNA == "always" | (showNA == "ifany" && anyNA(by))
    
    
    if(.showNA==TRUE) {
        if(!anyNA(by)) .na="no NA"
        by_filter=TRUE
        by = fct_explicit_na(by, "NA")
    } else{
        by_filter = !is.na(by) 
    }
    
    tibble(x, by) %>%
        filter(by_filter) %>%
        group_by(by) %>%
        summarise(x=list(summarize_numeric_single(x, funs=funs, funs_arg=funs_arg))) %>%
        unnest(cols="x") %>%
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
    
    if(test) .test=test_args$display_test(ct) else .test=NULL
    
    tibble(variable=method, value=as.character(value)) %>% mutate(test=.test)
}

