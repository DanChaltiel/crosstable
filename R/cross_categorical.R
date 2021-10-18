
#' @importFrom dplyr select mutate mutate_all everything .data
#' @keywords internal
#' @noRd
cross_categorical=function(data_x, data_y, showNA, total, label, percent_digits, margin,
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
        rtn=summarize_categorical_single(data_x, showNA=showNA, total=total, 
                                         digits=percent_digits, margin=margin)
    } else if(is.character.or.factor(data_y[[1]])){
        rtn=summarize_categorical_by(data_x[[1]], data_y[[1]], margin=margin, showNA=showNA, 
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
#' @importFrom glue glue
#' @importFrom dplyr mutate mutate_at vars select .data
#' @keywords internal
#' @noRd
summarize_categorical_single = function(x, showNA, total, digits, margin){
    utils::str(x)
    print(class(x))
    print(margin)
    tb=table(x, useNA = "no")
    print(tb)
    tbd = as.data.frame(tb, responseName="Freq", stringsAsFactors=FALSE)
    print(tbd)
    print(class(tbd))
    print(dimnames(tbd))
    print(tbd$x)
    print(tbd$Freq)
    select(tbd, variable="x", value="Freq") %>% print()
    if(identical(margin,-1)){
        rtn = table(x, useNA = "no") %>% 
            as.data.frame(responseName="Freq", stringsAsFactors=FALSE) %>% 
            select(variable="x", value="Freq")
    } else {
        rtn = table(x, useNA = "no") %>% 
            as.data.frame(responseName="Freq", stringsAsFactors=FALSE) %>% 
            mutate(p=100*.data$Freq/sum(.data$Freq)) %>% 
            mutate_at(vars("p"), format_fixed, digits=digits) %>% 
            mutate(value=glue("{Freq} ({p}%)")) %>% 
            select(variable="x", value="value")
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
    nn = table(x, by, useNA=showNA)
    .tbl = as.data.frame(nn, responseName="Freq", stringsAsFactors=FALSE)
    if(identical(margin,-1)){
        rtn = .tbl %>% 
            transmute(variable=replace_na(x, "NA"), by=.data$by, Freq=.data$Freq) %>% 
            pivot_wider(names_from="by", values_from = "Freq")
    } else {
        if(length(unique(by))==1) margin=2
        
        .ptbl = margin %>% 
            map(~{
                if(.x==0)
                    tmp=table(x, by, useNA="no") %>% {./sum(.)}
                else
                    tmp=table(x, by, useNA="no") %>% prop.table(margin=.x)
                tmp %>% 
                    as.data.frame(responseName=paste0("p",.x), stringsAsFactors=FALSE) %>%
                    mutate(across(starts_with("p"), 
                                  ~format_fixed(100*., digits=digits) %>% paste0("%")))
            }) %>% 
            reduce(left_join, by=c("x", "by")) %>% 
            unite(col="p", starts_with("p"), sep=" / ")
        rtn = .tbl %>% 
            left_join(.ptbl, by=c("x", "by")) %>%
            mutate(value=ifelse(is.na(x)|is.na(by), .data$Freq, glue("{Freq} ({p})"))) %>%
            transmute(variable=replace_na(x, "NA"), by=.data$by, value=.data$value) %>%
            pivot_wider(names_from="by", values_from = "value")
    }
    
    
    if(2 %in% total){
        mt=margin.table(nn, margin=2) %>% as.numeric()
        if(identical(margin,-1)){
            line=mt
        } else{
            mt2=margin.table(table(x, by, useNA="no"), margin=2) %>% as.numeric()
            pct=format_fixed(100*prop.table(mt2), digits) %>% paste0("%")
            length(pct)=length(mt) #expands with NA
            line=paste0(mt,ifelse(is.na(pct), "", glue(" ({pct})")))
        }
        rtn=rbind(rtn, c("Total", line))
    }
    
    
    .effect=.test=.total=NULL
    if(1 %in% total){
        .total=summarize_categorical_single(x, showNA, total, digits, margin)$value
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
