
#' @importFrom dplyr everything mutate select
#' @importFrom rlang check_installed
#' @keywords internal
#' @noRd
cross_survival=function(data_x, data_y, showNA, total, label, surv_digits, times, followup,
                        margin, test, test_args, effect, effect_args){
  check_installed("survival", reason="for survival data to be described using `crosstable()`.")

  stopifnot(ncol(data_x)==1 && (is.null(data_y) || ncol(data_y)==1))
  stopifnot(is.Surv(data_x[[1]]))

  if(label){
    x_name = get_label(data_x)
    y_name = get_label(data_y)
  } else {
    x_name = names(data_x)
    y_name = names(data_y)
  }

  if(is.null(data_y)){
    rtn=summarize_survival_single(data_x[[1]], times=times, followup=followup,
                                  digits=surv_digits)
  } else if(is.character.or.factor(data_y[[1]])){
    rtn=summarize_survival_by(data_x[[1]], data_y[[1]], times=times, followup=followup,
                              digits=surv_digits, total=total, showNA=showNA, test=test,
                              test_args=test_args, effect=effect, effect_args=effect_args)
  } else {
    return(NULL)
  }

  rtn = rtn %>%
    mutate(.id=names(data_x), label=unname(x_name)) %>%
    select(".id", "label", everything()) %>%
    as_tibble()

  rtn
}



#' @importFrom dplyr everything mutate pull select summarise
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @keywords internal
#' @noRd
summarize_survival_single = function(surv, times, digits, followup) {
  stopifnot(is.Surv(surv))
  fit = survival::survfit(surv~1)

  if (followup) {
    surv_fu = surv
    surv_fu[,2] = 1-surv_fu[,2]
    fit_fu = survival::survfit(surv_fu~1)
  }

  if (is.null(times)) {
    times = sort(fit$time)
  }

  x = summary(fit, times = times, extend = TRUE)
  counts = glue("{surv} ({event}/{risk})", surv=format_fixed(x$surv, digits), event=x$n.event, risk=x$n.risk)
  rtn = tibble(variable=paste0("t=", times), value=as.character(counts))

  if (followup) {
    #TODO unité ?
    mediansuiv = round(summary(fit_fu)$table["median"], digits = digits)
    suiv=tibble(time=surv[,1], status=surv[,2]) %>%
      summarise(minsuiv=min(.data$time[.data$status==0]), maxsuiv=max(.data$time)) %>%
      mutate(x=glue("{mediansuiv} [{minsuiv} ; {maxsuiv}]")) %>%
      pull(x) %>% as.character
    rtn=rbind(rtn, c("Median follow up [min ; max]", suiv))
  }
  rtn=rbind(rtn, c("Median survival", x$table["median"]))
  rtn %>%
    select("variable", everything())
}





#' @importFrom checkmate assert assert_class
#' @importFrom dplyr everything group_by left_join mutate pull rename row_number select summarise
#' @importFrom glue glue
#' @importFrom rlang set_names
#' @importFrom stats complete.cases
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider replace_na
#' @keywords internal
#' @noRd
summarize_survival_by = function(surv, by, times, followup, total, digits, showNA,
                                 test, test_args, effect, effect_args) {
  assert_class(surv, "Surv")
  by2 = by

  if(showNA!="no") {
    by2 = replace_na(as.character(by), "NA")
  } else {
    complete = complete.cases(surv, by)
    surv = surv[complete]
    by2 = by2[complete]
  }

  if(length(unique(by2))==1){
    cname = as.character(unique(by2))
    rtn = summarize_survival_single(surv, times, digits, followup)
    rtn = rtn %>%
      rename(!!cname:="value") %>%
      mutate(test=if(test) "No test" else NULL,
             effect=if(effect) "No Effect" else NULL) %>%
      select("variable", everything())
    return(rtn)
  }

  fit = survival::survfit(surv~by2)
  if(is.null(times)) times = sort(fit$time)
  x = summary(fit, times=times, extend=TRUE)
  assert(length(unique(x$strata))>1) #should not happen since by2!=NULL

  counts = glue("{surv} ({event}/{risk})",
                surv=format_fixed(x$surv, digits),
                event=x$n.event, risk=x$n.risk)
  rtn = tibble(count=as.character(counts), by=x$strata) %>%
    group_by(by) %>% mutate(x=row_number()) %>%
    # mutate(across(by, ~stringr::str_remove(.x, ".*="))) %>%
    pivot_wider(names_from = "by", values_from = "count") %>%
    select(-x) %>%
    set_names(names(table(by2))) %>%
    mutate(variable=paste0("t=", times)) %>%
    select("variable", everything())

  if (followup) {
    surv_fu = surv
    surv_fu[,2] = 1-surv_fu[,2]
    fit_fu = survival::survfit(surv_fu~by2)
    mediansuiv = round(summary(fit_fu)$table[, "median"], digits = digits)
    suiv=tibble(time=surv[,1], status=surv[,2], by=by2) %>%
      group_by(by) %>%
      summarise(minsuiv=suppressWarnings(min(.data$time[.data$status==0])),
                maxsuiv=suppressWarnings(max(.data$time))) %>%
      mutate(x=glue("{mediansuiv} [{minsuiv} ; {maxsuiv}]")) %>%
      pull(x) %>% as.character
    rtn=rbind(rtn, c("Median follow up [min ; max]", suiv))
  }
  rtn=rbind(rtn, c("Median survival", x$table[,"median"]))

  .tests=.effect=NULL
  if (effect) {
    e = effect_args$effect_survival(surv, by, effect_args$conf_level)
    .effect = effect_args$effect_display(e, digits = effect_args$digits)
  }
  if (test) {
    .tests = test_args$test_display(test_args$test_survival(surv~by),
                                    digits = test_args$plim, method = test_args$show_method)
  }
  if (1 %in% total) {
    rtn_tot = summarize_survival_single(surv, times, digits, followup) %>%
      rename(Total="value")
    rtn = left_join(rtn, rtn_tot, by=c("variable"))
  }


  rtn %>% mutate(test=.tests, effect=.effect) %>%
    select("variable", everything())
}
