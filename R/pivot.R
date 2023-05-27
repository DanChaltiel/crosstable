



#' Pivot a crosstable
#'
#' Pivot a crosstable so the `variable` column is spread across its values.
#'
#' @param ct a crosstable
#'
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr count filter group_by mutate na_if summarise
#' @importFrom glue glue
#' @importFrom tidyr drop_na pivot_wider replace_na
#'
#' @return a tibble of class `pivoted_crosstable`
#'
#' @examples
#' ct = crosstable(mtcars2, c(mpg, drat, wt, qsec))
#' p_ct = pivot_crosstable(ct)
#' as_flextable(p_ct)
pivot_crosstable = function(ct){
  by_levels = attr(ct, "by_levels")
  if(!is.null(by_levels)){
    cli_abort("You cannot pivot a crosstable where {.arg by} is not {.code NULL}.",
              class="crosstable_pivot_by")
  }

  ct %>% mutate(variable=na_if(variable, "NA")) %>% drop_na(variable) %>% count(.id, variable)
  w = ct %>%
    filter(variable!="NA") %>%
    group_by(.id) %>% summarise(variable = paste0("'", variable, "'", collapse=", ")) %>%
    group_by(variable) %>% summarise(.id = paste0("'", .id, "'", collapse=", ")) %>%
    mutate(label = glue(".id=c({.id}) --> variable=c({variable})"))
  if(nrow(w)>1){
    cli_abort(cl("You cannot transpose a crosstable with multiple `variable` strata.",
                 i=ansi_align_by(w$label, "-->")),
              wrap = FALSE,
              class="crosstable_pivot_multi_var")
  }

  rtn = pivot_wider(ct, names_from="variable", values_from="value") %>%
    apply_labels(variable="Variable") %>%
    attributes_from(ct)
  rtn[["NA"]] = rtn[["NA"]] %>% replace_na("0")
  attr(rtn, "by_levels") = list(Variable=unique(ct$variable))
  attr(rtn, "by") = "variable"
  attr(rtn, "by_label") = "Variable"
  class(rtn) = c("pivoted_crosstable", class(ct))
  rtn
}




#' Transpose a crosstable
#'
#' Pivot a crosstable so the `label` column is swapped with the `by` row.
#' This requires the `variable` column to be the same for every data column, like when all columns are numeric of when all columns are factors with the same levels
#'
#' @param x a crosstable
#'
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr across all_of arrange distinct everything group_by mutate select summarise sym
#' @importFrom forcats as_factor
#' @importFrom glue glue
#' @importFrom rlang set_names
#' @importFrom tibble column_to_rownames
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @return a tibble of class `transposed_crosstable`
#'
#' @examples
#' ct = crosstable(mtcars2, c(mpg, drat, wt, qsec), by=am)
#' t_ct = t(ct)
#' as_flextable(t_ct)
transpose_crosstable = function(x){
  if(is.compacted_crosstable(x)){
    cli_abort("You cannot transpose a compacted crosstable.",
              class="crosstable_transpose_compact")
  }
  if(is.multiby_crosstable(x)){
    cli_abort("You cannot transpose a crosstable with multiple `by` strata.",
              class="crosstable_transpose_multi_by")
  }
  if(!all(table(x$.id, x$variable)==1)){
    w = x %>%
      group_by(.id) %>% summarise(variable = paste0("'", variable, "'", collapse=", ")) %>%
      group_by(variable) %>% summarise(.id = paste0("'", .id, "'", collapse=", ")) %>%
      mutate(label = glue(".id=c({.id}) --> variable=c({variable})"))
    cli_abort(cl("You cannot transpose a crosstable with multiple `variable` strata.",
                 i=ansi_align_by(w$label, "-->")),
              wrap = FALSE,
              class="crosstable_transpose_multi_var")
  }
  v_levels = as_factor(x$variable) %>% levels()
  by_level = attr(x, "by_levels")
  if(is.null(by_level)){
    cli_abort("You cannot transpose a crosstable where {.arg by} is {.code NULL}.",
              class="crosstable_transpose_no_by")
  }
  if(is.transposed_crosstable(x)){
    id = attr(x, "transposed_id_labels")
  } else id = NULL

  inner_labels = attr(x, "inner_labels") %>% get_generic_labels()
  col_label = if(is.null(inner_labels)) "label" else inner_labels[["label"]]

  id_label = x %>% select(".id", all_of(col_label)) %>% unique()
  if(anyDuplicated(id_label[[col_label]])){
    w = id_label %>% group_by(!!sym(col_label)) %>%
      summarise(.id = paste0("'", .id, "'", collapse=", ")) %>%
      mutate(label = glue("label=c('{label}') --> .id=c({.id})"))
    cli_abort(cl("Some columns have the same label. Please use `crosstable(label=FALSE)`
                or set a unique label to each column.",
                 i=ansi_align_by(w$label, "-->")),
              class="crosstable_transpose_labels")
  }


  rtn = x %>%
    pivot_longer(-(1:3)) %>%
    mutate(.id = if(!is.null(id)) id[name,] else name) %>%
    pivot_wider(names_from = all_of(col_label)) %>%
    mutate(name=factor(name, levels=by_level[[1]]),
           variable=factor(variable, levels=v_levels)) %>%
    arrange(name, variable) %>%
    mutate(name=as.character(name),
           variable=as.character(variable)) %>%
    select(.id, !!names(by_level):=name, variable, everything())

  rtn = rtn %>% attributes_from(x)

  attr(rtn, "by_levels") = list(x=unique(x[[col_label]])) %>% set_names(col_label)
  # attr(rtn, "variables") = by_level[[1]]
  attr(rtn, "variables") = unique(rtn$.id)
  attr(rtn, "by") = col_label
  attr(rtn, "by_label") = col_label %>% set_names(col_label)

  tbl = table(x[[col_label]])
  names(dimnames(tbl)) = col_label
  attr(rtn, "by_table") = tbl

  attr(rtn, "transposed_id_labels") = distinct(x, .id, across(all_of(col_label))) %>%
    column_to_rownames(col_label)
  attr(rtn, "inner_labels") = list(x=names(by_level)) %>% set_names("label")

  class(rtn) = c("transposed_crosstable", class(x))

  rtn
}


#' @export
#' @rdname transpose_crosstable
t.crosstable = transpose_crosstable
