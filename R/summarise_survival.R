
#' @importFrom dplyr tibble summarise mutate pull select everything mutate_all .data
#' @importFrom survival is.Surv survfit
#' @importFrom glue glue
#' @keywords internal
#' @noRd
summarise_survival_single = function(surv, times, digits, followup) {
    stopifnot(is.Surv(surv))
    fit = survfit(surv~1)
    
    if (followup) {
        surv_fu = surv
        surv_fu[,2] = 1-surv_fu[,2]
        fit_fu = survfit(surv_fu~1)
    }
    
    if (is.null(times)) {
        times = sort(fit$time)
    }
    
    x = summary(fit, times = times, extend = TRUE)
    counts = glue("{surv} ({event}/{risk})", surv=format_fixed(x$surv, digits), event=x$n.event, risk=x$n.risk)
    rtn = tibble(variable=paste0("t=", times), value=as.character(counts))
    
    if (followup) {
        mediansuiv = round(summary(fit_fu)$table["median"], digits = digits)
        suiv=tibble(time=surv[,1], status=surv[,2]) %>% 
            summarise(minsuiv=min(.data$time[.data$status==0]), maxsuiv=max(.data$time)) %>% 
            mutate(x=glue("{mediansuiv} [{minsuiv} ; {maxsuiv}]")) %>% 
            pull(x) %>% as.character
        rtn=rbind(rtn, c("Median follow up [min ; max]", suiv))
    }
    rtn=rbind(rtn, c("Median survival", x$table["median"]))
    rtn %>% 
        select(.data$variable, everything()) %>% 
        mutate_all(as.character)
}





#' @importFrom dplyr mutate mutate_all rename select everything tibble group_by row_number summarise pull left_join %>% 
#' @importFrom tidyr pivot_wider
#' @importFrom rlang set_names :=
#' @importFrom checkmate assert
#' @importFrom glue glue
#' @importFrom survival is.Surv survfit
#' @keywords internal
#' @noRd
summarise_survival_by = function(surv, by, times, followup, total, digits, showNA,
                                 test, test_args, effect, effect_args) {
    assert(is.Surv(surv))
    if(showNA!="no") by = replace_na(as.character(by), "NA")
    
    if(length(unique(by))==1){
        cname = as.character(unique(by))
        rtn = summarise_survival_single(surv, times, digits, followup)
        rtn = rtn %>% 
            rename(!!cname:=.data$value) %>% 
            mutate(test=if(test) "No test" else NULL, 
                   effect=if(effect) "No Effect" else NULL) %>% 
            select(.data$variable, everything())
        return(rtn)
    }
    
    fit = survfit(surv~by)
    if (is.null(times)) times = sort(fit$time)
    x = summary(fit, times = times, extend = TRUE)
    assert(length(unique(x$strata))>1) #should not happen since by!=NULL
    
    counts = glue("{surv} ({event}/{risk})", surv=format_fixed(x$surv, digits), event=x$n.event, risk=x$n.risk)
    
    rtn = tibble(count=as.character(counts), by=x$strata) %>% 
        group_by(by) %>% mutate(x=row_number()) %>% 
        # mutate_at(vars(by), ~stringr::str_remove(.x, ".*=")) %>% 
        pivot_wider(names_from = "by", values_from = "count") %>% 
        select(-x) %>% 
        set_names(names(table(by))) %>% 
        mutate(variable=paste0("t=", times)) %>% 
        select(.data$variable, everything())
    
    if (followup) {
        surv_fu = surv
        surv_fu[,2] = 1-surv_fu[,2]
        fit_fu = survfit(surv_fu~by)
        mediansuiv = round(summary(fit_fu)$table[, "median"], digits = digits)
        suiv=tibble(time=surv[,1], status=surv[,2], by) %>% 
            group_by(by) %>% 
            summarise(minsuiv=min(.data$time[.data$status==0]), maxsuiv=max(.data$time)) %>% 
            mutate(x=glue("{mediansuiv} [{minsuiv} ; {maxsuiv}]")) %>% 
            pull(x) %>% as.character
        rtn=rbind(rtn, c("Median follow up [min ; max]", suiv))
    }
    rtn=rbind(rtn, c("Median survival", x$table[,"median"]))
    
    .tests=.effect=NULL
    if (effect) {
        .effect = effect_args$show_effect(effect_args$effect_survival(surv~by, effect_args$conf_level), 
                                          digits = effect_args$digits)
    } 
    if (test) {
        .tests = test_args$display_test(test_args$test_survival(surv~by), 
                                     digits = test_args$plim, method = test_args$show_method)
    } 
    if (1 %in% total) {
        rtn_tot = summarise_survival_single(surv, times, digits, followup) %>% 
            rename(Total=.data$value)
        rtn = left_join(rtn, rtn_tot, by=c("variable"))
    }
    
    
    rtn %>% mutate(test=.tests, effect=.effect) %>% 
        select(.data$variable, everything())
}



