
#' @importFrom purrr imap_dfr map_dfr imap_chr
#' @importFrom glue glue glue_data glue_collapse
#' @importFrom cli qty
#' @keywords internal
#' @noRd
cross_by = function(data_x, data_y, funs, funs_arg, percent_pattern, total, percent_digits,
                    showNA, label, test, times, followup,
                    test_args, cor_method, effect, effect_args){
  if(!is.null(data_y) && ncol(data_y)>1) cli_abort(glue("data_y has {ncol(data_y)} columns (max=1)"))
  errors = rlang::env()

  by_levels = length(unique(na.omit(data_y[[1]])))
  if(!is.numeric(data_y[[1]]) && isTRUE(effect) && by_levels!=2){
    cli_warn(c("Cannot calculate crosstable effects as there is not exactly 2 groups in `by`.",
               i="`by` has {by_levels} level{?s}"),
             class = "crosstable_effect_2groups_warning",
             call = crosstable_caller$env)
    effect = FALSE
  }

  rtn_tbl = imap_dfr(data_x, ~{
    if(all(is.na(.x))) .x = "NA"
    if(inherits(.x, "difftime")){
      lab = get_label(.x)
      .x = as.numeric(.x) %>% set_label(lab)
    }

    if(anyNA(.x) && "NA" %in% .x) {
      na_string = as.character(which(.x=="NA"))
      na_proper = as.character(which(is.na(.x)))
      cli_warn(c('Cannot describe column "{.y}" as it contains both `NA` (missing values) and "NA" (string)',
                 i='NA as string{?s} on row{?s}: {na_string}',
                 i='NA missing value{?s} on row{?s}: {na_proper}'),
               class = "crosstable_na_char_warning",
               call = crosstable_caller$env)
      return(NULL)
    }
    if(!is.list(.x)){
      data_x[.y] = .x
      # errors[[.y]] = data.frame(name=.y, class="list")
      # return(NULL)
    }


    if(is.list(.x)){
      rtn=NULL
    } else if(is.numeric.and.not.surv(.x) || is.date(.x)){
      rtn=cross_numeric(data_x[.y], data_y, funs=funs, funs_arg=funs_arg,
                        showNA=showNA, total=total, label=label,
                        cor_digits=percent_digits, cor_method=cor_method,
                        test=test, test_args=test_args, effect=effect, effect_args=effect_args)
    } else if(is.character.or.factor(.x)){
      rtn=cross_categorical(data_x[.y], data_y, percent_pattern=percent_pattern,
                            showNA=showNA, total=total, label=label, percent_digits=percent_digits,
                            test=test, test_args=test_args, effect=effect, effect_args=effect_args)
    } else if(is.Surv(.x)){
      rtn=cross_survival(data_x[.y], data_y, times=times, followup=followup,
                         showNA=showNA, total=total, label=label, surv_digits=percent_digits,
                         test=test, test_args=test_args, effect=effect, effect_args=effect_args)
    } else {
      rtn=NULL
    }
    if(is.null(rtn)){
      errors[[.y]] = data.frame(name=.y, class=paste_classes(.x))
    }

    rtn
  })


  errors = as.list(errors) %>% map_dfr(identity)
  if(nrow(errors)>0){
    errors_s = glue_data(errors, "'{name}' ({class})")
    by_col = glue("'{names(data_y[1])}' ({paste_classes(data_y[[1]])})")
    if(is.null(data_y)){
      cli_warn("Could not describe column{?s} of wrong class: {errors_s}",
               class = "crosstable_wrong_col_class_warning",
               call = crosstable_caller$env)
    } else {
      cli_warn("Could not cross {qty(errors_s)} column{?s} {errors_s} by column {by_col})",
               class = "crosstable_wrong_col_class_by_warning",
               call = crosstable_caller$env)
    }
  }

  if("effect" %in% names(rtn_tbl) && any(rtn_tbl$effect=="No effect?")){
    x=rtn_tbl %>% filter(effect=="No effect?") %>% pull(.data$.id) %>% unique()
    cli_warn("Cannot calculate crosstable effects for variable{?s} {.var {x}}",
             class = "crosstable_effect_other_warning")
  }

  rownames(rtn_tbl)=NULL
  return(rtn_tbl)
}

