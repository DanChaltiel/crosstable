
#' @importFrom dplyr select mutate everything .data
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
    rtn=summarize_categorical_single(data_x[[1]],
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
    select(".id", "label", everything()) %>%
    map_df(as.character)

  rtn
}


#' @importFrom checkmate assert_numeric assert_character
#' @importFrom stringr str_starts
#' @importFrom glue glue
#' @importFrom dplyr mutate select .data
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
      p_row=p_col, p_cell=p_col,
      p_col_na=.data$n/.data$n_col_na,
      p_row_na=p_col_na, p_cell_na=p_col_na,
      across(c(p_col, p_row, p_cell), ~ifelse(is.na(x), NA, .x))
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
                    p_cell=1, p_row=p_cell, p_col=p_cell) %>%
      mutate(across_unpack(matches("^[pn]"), ~tibble(na=.x))) %>%
      getTableCI(digits=digits) %>%
      transmute(.data$variable,
                value=ifelse(.data$n==0 & zero_percent, .data$n,
                             glue(percent_pattern$total_all)))
    rtn = bind_rows(rtn, .total)
  }

  rtn %>% map_df(as.character)
}



#' @importFrom dplyr mutate transmute starts_with left_join pull .data
#' @importFrom purrr map reduce safely
#' @importFrom tidyr unite pivot_wider
#' @importFrom glue glue
#' @importFrom tidyselect peek_vars
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

  table_p_cell = proportions(nn2, margin=NULL) %>% as.data.frame(responseName="p_cell")
  table_p_row  = proportions(nn2, margin=1) %>% as.data.frame(responseName="p_row")
  table_p_col  = proportions(nn2, margin=2) %>% as.data.frame(responseName="p_col")
  table_p_cell_na = proportions(nn3, margin=NULL) %>% as.data.frame(responseName="p_cell_na")
  table_p_row_na  = proportions(nn3, margin=1) %>% as.data.frame(responseName="p_row_na")
  table_p_col_na  = proportions(nn3, margin=2) %>% as.data.frame(responseName="p_col_na")

  .table = reduce(list(table_n, table_p_cell, table_p_row, table_p_col,
                       table_p_cell_na, table_p_row_na, table_p_col_na),
                  left_join, by=c("x", "by")) %>%
    left_join(table_n_row, by="x") %>%
    left_join(table_n_row_na, by="x") %>%
    left_join(table_n_col, by="by") %>%
    left_join(table_n_col_na, by="by") %>%
    mutate(n_tot=.env$n_tot, n_tot_na=.env$n_tot_na,
           across(c(p_col, p_row, p_cell), ~ifelse(is.na(x), NA, .x))) %>%
    getTableCI(digits=digits) %>%
    relocate(x, by, n, sort(peek_vars()))

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
        p_row=p_col, p_cell=p_col,
        n_row_na=n_tot_na,
        p_col_na=n_col_na/n_tot_na,
        p_row_na=p_col_na, p_cell_na=p_col_na,
        across(c(p_col, p_row, p_cell), ~ifelse(is.na(by), NA, .x))
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
  #   any_p = pattern_vars %>% str_detect("p_row|p_col|p_cell") %>% any()
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
    # any_p = pattern_vars %>% str_detect("p_row|p_col|p_cell") %>% any()
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



# Utils -------------------------------------------------------------------

#' @importFrom rlang as_function
#' @keywords internal
#' @noRd
getTable = function(x, by, type=c("n", "p_cell", "p_row", "p_col")){
  fun = switch(type,
               n=identity,
               p_cell=as_function(~proportions(.x)),
               p_row=as_function(~proportions(.x, margin=1)),
               p_col=as_function(~proportions(.x, margin=2))
  )
  useNA = if(type=="p_cell") "always" else "no"
  table(x, by, useNA=useNA) %>%
    fun() %>%
    as.data.frame(responseName=type)
}

#' @importFrom rlang as_function
#' @keywords internal
#' @noRd
getTableCI = function(x, digits, method="wilson"){
  x %>%
    mutate(
      across_unpack(p_cell, ~confint_proportion(.x, n_tot, method=method)),
      across_unpack(p_col,  ~confint_proportion(.x, n_col, method=method)),
      across_unpack(p_row,  ~confint_proportion(.x, n_row, method=method)),
      across_unpack(p_cell_na, ~confint_proportion(.x, n_tot_na, method=method)),
      across_unpack(p_col_na,  ~confint_proportion(.x, n_col_na, method=method)),
      across_unpack(p_row_na,  ~confint_proportion(.x, n_row_na, method=method)),
      across(starts_with("p_"), ~format_fixed(.x, digits=digits, percent=TRUE))
    )
}


#' @importFrom purrr map
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
get_percent_pattern = function(margin=c("row", "column", "cell", "none", "all")){
  rtn = list(
    body="{n} ({p_col})",
    total_row="{n} ({p_col})",
    total_col="{n} ({p_row})",
    total_all="{n} ({p_cell})"
  )
  if(length(margin)==1){
    if(margin %in% list(-1, "none")){
      rtn = map(rtn, ~"{n}")
      return(rtn)
    } else if(isTRUE(margin)){
      rtn$body = "{n} ({p_row} / {p_col})"
      return(rtn)
    } else if(margin=="all"){
      rtn$body = "{n} ({p_cell} / {p_row} / {p_col})"
      return(rtn)
    }
  }
  if(any(margin %in% c("row", "column", "cell")) && any(margin %in% c("none", "all"))){
    cli_abort(c('{.code margin=c("row", "column", "cell")} cannot be combined
                with {.code margin=c("none", "all")}',
                "Input margin: {.val {margin}}"),
              class="crosstable_incompatible_margin",
              call=crosstable_caller$env)
  }
  if(any(margin=="none") && any(margin=="all")){
    cli_abort(c('{.code margin="none"} cannot be combined with {.code margin="all"}',
                "Input margin: {.val {margin}}"),
              class="crosstable_incompatible_margin2",
              call=crosstable_caller$env)
  }

  marginopts = list(p_row = c(1, "row", "rows"),
                    p_col = c(2, "col", "cols", "column", "columns"),
                    p_cell = c(0, "cell", "cells"))
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
  rtn
}


# Global Variables -----------------------------------------------------------------------------

#@include utils.r
#add all combinations to global variables
x=c("cell", "row", "col")
n=c("n", "n_row", "n_col", "n_tot", "n_row_na", "n_col_na", "n_tot_na")
p=paste0("p_", x)
p_na=paste0(p, "_na")
p_ci=map(p, ~paste0(.x, c("_inf", "_sup")))
p_na_ci=map(p_na, ~paste0(.x, c("_inf", "_sup")))
nm = c(n, p, p_na, p_ci, p_na_ci) %>% unlist()
# nm = percent_pattern_variables()
utils::globalVariables(nm)
