
#' @importFrom dplyr everything mutate select
#' @importFrom forcats fct_drop
#' @importFrom purrr map_df
#' @keywords internal
#' @noRd
cross_categorical=function(data_x, data_y, showNA, total, label, percent_digits, percent_pattern,
                           drop_levels, test, test_args, effect, effect_args){

  stopifnot(ncol(data_x)==1 && (is.null(data_y) || ncol(data_y)==1))
  stopifnot(is.character.or.factor(data_x[[1]]))

  if(label){
    x_name = get_label(data_x)
    y_name = get_label(data_y)
  } else {
    x_name = names(data_x)
    y_name = names(data_y)
  }
  if(isTRUE(drop_levels)) data_x[[1]] = fct_drop(data_x[[1]])

  if(is.null(data_y)){
    rtn=summarize_categorical_single(data_x[[1]],
                                     percent_pattern=percent_pattern, showNA=showNA,
                                     total=total, digits=percent_digits)
  } else if(is.character.or.factor(data_y[[1]])){
    if(isTRUE(drop_levels)) data_y[[1]] = fct_drop(data_y[[1]])
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
    select(".id", "label", everything()) %>%
    map_df(as.character)

  rtn
}


#' @importFrom dplyr across bind_rows filter matches mutate select transmute
#' @importFrom glue glue
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na
#' @keywords internal
#' @noRd
summarize_categorical_single = function(x, showNA, total, digits, percent_pattern){
  tbd = table(x, useNA = "always") %>%
    as.data.frame() %>%
    select(x=1, n=2) #needed for an odd bug on fedora-devel
  zero_percent = getOption("crosstable_zero_percent", FALSE)

  rtn = tbd %>%
    mutate(
      n_col=sum(.data$n[!is.na(.data$x)], na.rm=TRUE),
      n_row=.data$n_col, n_tot=.data$n_col,
      n_col_na=sum(.data$n, na.rm=TRUE),
      n_row_na=.data$n_col_na, n_tot_na=.data$n_col_na,
      p_col=.data$n/.data$n_col,
      p_row=p_col, p_tot=p_col,
      p_col_na=.data$n/.data$n_col_na,
      p_row_na=p_col_na, p_tot_na=p_col_na,
      across(c(p_col, p_row, p_tot), ~ifelse(is.na(x), NA, .x))
    ) %>%
    getTableCI(digits=digits) %>%
    transmute(variable=replace_na(x, "NA"),
              value=ifelse(is.na(x) | .data$n==0 & zero_percent,
                           .data$n, glue(percent_pattern$body)))
  .showNA = showNA=="always" || showNA=="ifany" && (anyNA(x))

  if(!.showNA){
    rtn = filter(rtn, .data$variable!="NA")
  }

  if(2 %in% total){
    .total = tibble(variable="Total",
                    n=length(x), n_row=n, n_col=n, n_tot=n,
                    p_tot=1, p_row=p_tot, p_col=p_tot) %>%
      mutate(across_unpack(matches("^[pn]"), ~tibble(na=.x))) %>%
      getTableCI(digits=digits) %>%
      transmute(.data$variable,
                value=ifelse(.data$n==0 & zero_percent, .data$n,
                             glue(percent_pattern$total_all)))
    rtn = bind_rows(rtn, .total)
  }

  rtn %>% map_df(as.character)
}



#' @importFrom dplyr across bind_rows everything filter left_join mutate select transmute
#' @importFrom glue glue
#' @importFrom purrr reduce
#' @importFrom stringr str_replace
#' @importFrom tibble add_row as_tibble
#' @importFrom tidyr pivot_wider replace_na
#' @keywords internal
#' @noRd
summarize_categorical_by = function(x, by,
                                    percent_pattern, margin,
                                    showNA, total, digits,
                                    test, test_args, effect, effect_args){
  zero_percent = getOption("crosstable_zero_percent", FALSE)

  nn = table(x, by, useNA=showNA)
  nn2 = table(x, by, useNA="no")
  nn3 = table(x, by, useNA="ifany")
  .tbl = as.data.frame(nn, responseName="Freq")

  table_n = as.data.frame(nn, responseName="n")
  # n_row = marginSums(nn, margin=1) %>% as.data.frame(responseName="n_row")
  # n_col = marginSums(nn, margin=2) %>% as.data.frame(responseName="n_col")
  table_n_row = marginSums(nn2, margin=1) %>% as.data.frame(responseName="n_row")
  table_n_col = marginSums(nn2, margin=2) %>% as.data.frame(responseName="n_col")
  table_n_row_na = marginSums(nn3, margin=1) %>% as.data.frame(responseName="n_row_na")
  table_n_col_na = marginSums(nn3, margin=2) %>% as.data.frame(responseName="n_col_na")
  n_tot = sum(nn2)
  n_tot_na = sum(nn3)

  table_p_tot = proportions(nn2, margin=NULL) %>% as.data.frame(responseName="p_tot")
  table_p_row  = proportions(nn2, margin=1) %>% as.data.frame(responseName="p_row")
  table_p_col  = proportions(nn2, margin=2) %>% as.data.frame(responseName="p_col")
  table_p_tot_na = proportions(nn3, margin=NULL) %>% as.data.frame(responseName="p_tot_na")
  table_p_row_na  = proportions(nn3, margin=1) %>% as.data.frame(responseName="p_row_na")
  table_p_col_na  = proportions(nn3, margin=2) %>% as.data.frame(responseName="p_col_na")

  .table = reduce(list(table_n, table_p_tot, table_p_row, table_p_col,
                       table_p_tot_na, table_p_row_na, table_p_col_na),
                  left_join, by=c("x", "by")) %>%
    left_join(table_n_row, by="x") %>%
    left_join(table_n_row_na, by="x") %>%
    left_join(table_n_col, by="by") %>%
    left_join(table_n_col_na, by="by") %>%
    mutate(n_tot=.env$n_tot, n_tot_na=.env$n_tot_na,
           across(c(p_col, p_row, p_tot), ~ifelse(is.na(x), NA, .x))) %>%
    getTableCI(digits=digits) %>%
    select(x, by, n, order(colnames(.)))

  rtn = .table %>%
    transmute(variable=x %>% str_replace("NA", "'NA'") %>% replace_na("NA"),
              by=.data$by,
              value=ifelse(is.na(x)|is.na(by)|.data$n==0&zero_percent,
                           .data$n, glue(percent_pattern$body))) %>%
    pivot_wider(names_from="by", values_from = "value")

  if(2 %in% total){
    ms = marginSums(nn3, margin=2) %>% as.data.frame(responseName="n_col")
    ms_na = marginSums(nn3, margin=2) %>% as.data.frame(responseName="n_col_na")
    if(nrow(table_n_col)>nrow(ms)) ms = add_row(ms) #TODO utile ?

    line = ms %>%
      left_join(ms_na, by="by") %>%
      filter(showNA!="no" | !is.na(by)) %>%
      as_tibble() %>%
      mutate(
        n_col = replace_na(n_col, 0), #TODO utile ?
        n=n_col,
        n_tot=sum(n_col[!is.na(by)]),
        n_tot_na=sum(n_col),
        n_row=n_tot,
        p_col=n_col/n_tot,
        p_row=p_col, p_tot=p_col,
        n_row_na=n_tot_na,
        p_col_na=n_col_na/n_tot_na,
        p_row_na=p_col_na, p_tot_na=p_col_na,
        across(c(p_col, p_row, p_tot), ~ifelse(is.na(by), NA, .x))
      ) %>%
      getTableCI(digits=digits) %>%
      transmute(x=fct_explicit_na(.data$by, "NA"),
                value=ifelse(is.na(by) | .data$n==0&zero_percent, .data$n,
                             glue(percent_pattern$total_row))) %>%
      pivot_wider(names_from="x", values_from = "value") %>%
      mutate(variable="Total")

    rtn=bind_rows(rtn, line)
  }

  # pattern_vars = get_glue_vars(percent_pattern$body)
  # if(2 %in% total){
  #   mt = marginSums(nn, margin=2) %>% as.numeric()
  #   line = mt
  #   any_p_ci = pattern_vars %>% str_starts("p_col_") %>% any()
  #   any_p = pattern_vars %>% str_detect("p_row|p_col|p_tot") %>% any()
  #   #TODO si !any_p_ci on garde pattern
  #
  #   if(any_p){
  #     mt2 = margin.table(table(x, by, useNA="no"), margin=2) %>% as.numeric()
  #     pct = format_fixed(100*prop.table(mt2), digits) %>% paste0("%")
  #     length(pct) = length(mt) #expands with NA
  #     line = paste0(mt,ifelse(is.na(pct), "", glue(" ({pct})")))
  #   }
  #   rtn=rbind(rtn, c("Total", line))
  # }

  .effect=.test=.total=NULL
  if(1 %in% total){
    # any_p = pattern_vars %>% str_detect("p_row|p_col|p_tot") %>% any()
    # any_pcol_ci = pattern_vars %>% str_starts("p_col_") %>% any()
    # percent_pattern2 = percent_pattern
    percent_pattern2 = percent_pattern
    percent_pattern2$body=percent_pattern$total_col
    # if(any_p && !any_pcol_ci) percent_pattern2="{n} ({p_col})"
    .total = summarize_categorical_single(x=x, showNA=showNA, total=total,
                                          digits=digits, percent_pattern=percent_pattern2)$value
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
    mutate(Total=.total, effect=.effect, test=.test,
           across(everything(), as.character))
}



#' Percent pattern helper
#'
#' Get a list with pre-filled values for `percent_pattern`.
#'
#' @param margin a vector giving the margins to compute.
#' @param na whether to use `NA`
#'
#' @return a list
#'
#' @export
#' @importFrom cli cli_abort cli_warn
#' @importFrom glue glue glue_collapse
#' @importFrom purrr map
#' @importFrom rlang set_names
#' @importFrom stringr str_replace_all
#'
#' @examples
#' get_percent_pattern(c("cells","row","column"))
#' get_percent_pattern(c("cells","row","column"), na=TRUE)
get_percent_pattern = function(margin=c("row", "column", "cell", "none", "all"), na=FALSE){
  if(missing(margin)) margin="row"
  rtn = list(
    body="{n} ({p_col})",
    total_row="{n} ({p_col})",
    total_col="{n} ({p_row})",
    total_all="{n} ({p_tot})"
  )
  if(length(margin)==1){
    if(margin %in% list(-1, "none")){
      rtn = map(rtn, ~"{n}")
      return(rtn)
    } else if(isTRUE(margin)){
      rtn$body = "{n} ({p_row} / {p_col})"
      return(rtn)
    } else if(margin=="all"){
      rtn$body = "{n} ({p_tot} / {p_row} / {p_col})"
      return(rtn)
    }
  }
  if(any(margin %in% c("none", "all")) && length(margin)>1){
    cli_abort(c('{.code margin="none"} and {.code margin="all"} cannot be combined with other values',
                "Input margin: {.val {margin}}"),
              class="crosstable_incompatible_margin",
              call=crosstable_caller$env)
  }

  marginopts = list(p_row = c(1, "row", "rows"),
                    p_col = c(2, "col", "cols", "column", "columns"),
                    p_tot = c(0, "cell", "cells"))
  unexpected = margin[!margin %in% unlist(marginopts)]
  if(length(unexpected)>0){
    cli_abort(c('`margin` must be one of "row", "column", "cell", "none", or "all".',
                "Problematic value{?s}: {.var {unexpected}}"),
              class="crosstable_unknown_margin",
              call=crosstable_caller$env)
  }
  x = marginopts %>%
    map(~{ #not map_dbl :-( # https://github.com/tidyverse/purrr/issues/841
      rtn = margin[margin %in% .x]
      if(length(rtn)>1){
        cli_warn("Duplicated margin{?s}: {.code {rtn}}",
                 class="crosstable_duplicated_margin",
                 call=crosstable_caller$env)
      }
      rtn[1]
    }) %>%
    unlist() %>% sort() %>% names()
  x = glue_collapse(glue("{{{x}}}"), sep=" / ")
  rtn$body = glue("{{n}} ({x})")

  if(isTRUE(na)){
    # browser()
    ppv = percent_pattern_variables()
    ppv = ppv$na %>% set_names(ppv$std)
    rtn = map(rtn, ~str_replace_all(.x, ppv))
  }

  rtn
}


# Utils -------------------------------------------------------------------

#' @importFrom rlang as_function
#' @keywords internal
#' @noRd
getTable = function(x, by, type=c("n", "p_tot", "p_row", "p_col")){
  fun = switch(type,
               n=identity,
               p_tot=as_function(~proportions(.x)),
               p_row=as_function(~proportions(.x, margin=1)),
               p_col=as_function(~proportions(.x, margin=2))
  )
  useNA = if(type=="p_tot") "always" else "no"
  table(x, by, useNA=useNA) %>%
    fun() %>%
    as.data.frame(responseName=type)
}

#' @keywords internal
#' @noRd
#' @importFrom dplyr across mutate starts_with
getTableCI = function(x, digits, method="wilson"){
  x %>%
    mutate(
      across_unpack(p_tot, ~confint_proportion(.x, n_tot, method=method)),
      across_unpack(p_col,  ~confint_proportion(.x, n_col, method=method)),
      across_unpack(p_row,  ~confint_proportion(.x, n_row, method=method)),
      across_unpack(p_tot_na, ~confint_proportion(.x, n_tot_na, method=method)),
      across_unpack(p_col_na,  ~confint_proportion(.x, n_col_na, method=method)),
      across_unpack(p_row_na,  ~confint_proportion(.x, n_row_na, method=method)),
      across(starts_with("p_"), ~format_fixed(.x, digits=digits, percent=TRUE))
    )
}

# Global Variables -----------------------------------------------------------------------------

#@include utils.r
#add all combinations to global variables
x=c("cell", "row", "col", "tot")
n=c("n", "n_row", "n_col", "n_tot", "n_row_na", "n_col_na", "n_tot_na")
n=c("n", "n_row", "n_col", "n_tot", "n_row_na", "n_col_na", "n_tot_na")
p=paste0("p_", x)
p=paste0("p_", x)
p_na=paste0(p, "_na")
p_na=paste0(p, "_na")
p_ci=map(p, ~paste0(.x, c("_inf", "_sup")))
#' @importFrom purrr map
p_ci=map(p, ~paste0(.x, c("_inf", "_sup")))
p_na_ci=map(p_na, ~paste0(.x, c("_inf", "_sup")))
#' @importFrom purrr map
p_na_ci=map(p_na, ~paste0(.x, c("_inf", "_sup")))
nm = c(n, p, p_na, p_ci, p_na_ci) %>% unlist()
nm = c(n, p, p_na, p_ci, p_na_ci) %>% unlist()
# nm = percent_pattern_variables()
utils::globalVariables(nm)
