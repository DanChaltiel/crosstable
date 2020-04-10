
#' @importFrom checkmate assert_numeric assert_character
#' @importFrom glue glue
#' @importFrom dplyr mutate mutate_at vars select .data
#' @keywords internal
#' @noRd
summarize_categorical_single = function(x, showNA, total, digits, margin){
    if(identical(margin,-1)){
        rtn = table(x, useNA = "no") %>% 
            as.data.frame() %>% 
            select(variable=.data$x, value=.data$Freq)
    } else {
        rtn = table(x, useNA = "no") %>% 
            as.data.frame() %>% 
            mutate(p=100*.data$Freq/sum(.data$Freq)) %>% 
            mutate_at(vars("p"), format_fixed, digits=digits) %>% 
            mutate(value=glue("{Freq} ({p}%)")) %>% 
            select(variable=.data$x, value=.data$value)
    }
    
    .showNA = showNA=="always" || showNA=="ifany" && (anyNA(x))
    if(.showNA){
        rtn = rbind(rtn, data.frame(variable="NA", value=sum(is.na(x))))
    }
    
    if (2 %in% total && identical(margin,-1)) {
        rtn = rbind(rtn, data.frame(variable="Total", value=sum(table(x, useNA='always'))))
    } else if (2 %in% total && !identical(margin,-1)) {
        value = glue("{sum(table(x, useNA='always'))} ({format_fixed(100, digits=digits)}%)")
        rtn = rbind(rtn, data.frame(variable="Total", value=value))
    }
    
    rtn %>% mutate_all(as.character)
}




#' @importFrom dplyr mutate mutate_at mutate_all transmute vars starts_with left_join pull .data
#' @importFrom purrr map reduce    
#' @importFrom tidyr unite pivot_wider
#' @importFrom glue glue
#' @keywords internal
#' @noRd
summarize_categorical_by = function(x, by, margin, showNA, total, digits, 
                                    test, test_args, effect, effect_args){
    
    nn = table(x, by, useNA = showNA)
    .tbl = nn %>% as.data.frame(responseName="Freq", stringsAsFactors=FALSE)
    
    if(identical(margin,-1)){
        rtn = .tbl %>% 
            transmute(variable=replace_na(x, "NA"), by=.data$by, Freq=.data$Freq) %>% 
            pivot_wider(names_from="by", values_from = "Freq")
    } else {
        .ptbl = margin %>% 
            map(~{
                if(.x==0)
                    .ptbl=table(x, by, useNA="no") %>% {./sum(.)}
                else
                    .ptbl=table(x, by, useNA="no") %>% prop.table(margin=.x)
                .ptbl %>% as.data.frame(responseName=paste0("p",.x), stringsAsFactors=FALSE) %>%
                    mutate_at(vars(starts_with("p")), ~format_fixed(100*., digits=digits)) %>%
                    mutate_at(vars(starts_with("p")), ~paste0(.,"%"))
            }) %>% reduce(left_join, by=c("x", "by")) %>% 
            unite(col="p", starts_with("p"), sep=" / ")
        rtn = .tbl %>% 
            left_join(.ptbl, by=c("x", "by")) %>% 
            mutate(value=ifelse(is.na(x)|is.na(by), .data$Freq, glue("{Freq} ({p})"))) %>% 
            transmute(variable=replace_na(x, "NA"), by=.data$by, value=.data$value) %>% 
            pivot_wider(names_from="by", values_from = "value")
    }
    
    
    if(2 %in% total && identical(margin,-1)){
        mt=margin.table(nn, margin=2) %>% as.numeric
        rtn=rbind(rtn, c("Total", mt))
    } else if(2 %in% total && !identical(margin,-1)){
        mt=margin.table(nn, margin=2) %>% as.numeric
        mt2=margin.table(table(x, by, useNA="no"), margin=2) %>% as.numeric
        pct=format_fixed(100*prop.table(mt2), digits) %>% paste0("%")
        length(pct)=length(mt) #expands with NA
        line=paste0(mt,ifelse(is.na(pct), "", glue(" ({pct})")))
        rtn=rbind(rtn, c("Total", line))
    }
    
    .effect=.test=.total=NULL
    if(1 %in% total){
        .total=summarize_categorical_single(x, showNA, total, digits, margin) %>% pull(.data$value)
    }
    
    if(effect) 
        .effect = effect_args$show.effect(effect_args$effect.tabular(x, by, effect_args$conf.level), 
                                          digits = effect_args$digits)
    if(test) 
        .test = test_args$show.test(test_args$test.tabular(x, by), digits = test_args$plim, 
                                    method = test_args$show.method)
    rtn %>%
        mutate(Total=.total, effect=.effect, test=.test) %>% 
        mutate_all(as.character)
}
