
utils::globalVariables(c(".id", "variable", "name"))

# Utils -------------------------------------------------------------------


#' Test if an object is a crosstable
#'
#' @param x An object
#'
#' @return TRUE if the object inherits from the `crosstable` class or other subclasses.
#' @export
is.crosstable = function(x) {
  inherits(x, "crosstable")
}

#' @rdname is.crosstable
#' @export
is.transposed_crosstable = function(x) {
  inherits(x, "transposed_crosstable")
}

#' @rdname is.crosstable
#' @export
is.compacted_crosstable = function(x) {
  inherits(x, "compacted_crosstable")
}

#' @rdname is.crosstable
#' @export
is.multiby_crosstable = function(x) {
  inherits(x, "crosstable_multiby")
}



# Transpose -----------------------------------------------------------------------------------


#' Transpose a crosstable
#'
#' Pivot a crosstable in order to swap the `label` column and the `by` row.
#' This requires the `variable` column to be the same for every data column, like when all columns are numeric of when all columns are factors with the same levels
#'
#' @param x a crosstable
#'
#' @export
#' @importFrom tidyr pivot_longer pivot_wider
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
    cli_abort(c("You cannot transpose a crosstable with multiple `variable` strata.",
                i=ansi_align_by(w$label, "-->")),
              wrap = FALSE,
              class="crosstable_transpose_multi_var")
  }
  if(is.transposed_crosstable(x)){
    id = attr(x, "transposed_id_labels")
  } else id = NULL

  inner_labels = attr(x, "inner_labels") %>% get_generic_labels()
  col_label = if(is.null(inner_labels)) "label" else inner_labels[["label"]]

  id_label = x %>% select(.id, all_of(col_label)) %>% unique()
  if(anyDuplicated(id_label[[col_label]])){
    w = id_label %>% group_by(!!sym(col_label)) %>%
      summarise(.id = paste0("'", .id, "'", collapse=", ")) %>%
      mutate(label = glue("label=c('{label}') --> .id=c({.id})"))
    cli_abort(c("Some columns have the same label. Please use `crosstable(label=FALSE)`
                or set a unique label to each column.",
                i=ansi_align_by(w$label, "-->")),
              class="crosstable_transpose_labels")
  }

  v_levels = as_factor(x$variable) %>% levels()
  by_level = attr(x, "by_levels")
  if(is.null(by_level)){
    cli_abort("You cannot transpose a crosstable where {.arg by} is {.code NULL}.",
              class="crosstable_transpose_no_by")
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

  attr(rtn, "transposed_id_labels") = distinct(x, .id, across(all_of(col_label))) %>% column_to_rownames(col_label)
  attr(rtn, "inner_labels") = list(x=names(by_level)) %>% set_names("label")

  class(rtn) = c("transposed_crosstable", class(x))

  # browser()
  rtn
}


#' @export
#' @rdname transpose_crosstable
t.crosstable = transpose_crosstable
