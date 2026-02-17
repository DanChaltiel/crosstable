

#' Generic function to compact a table (publication formatting)
#' @description NULL
#' @rdname ct_compact
#' @usage NULL
#'
#' @author Dan Chaltiel
#' @export
ct_compact = function(data, ...){
  UseMethod("ct_compact")
}



#' @name ct_compact
#' @description NULL
#' @param data the object to compact
#' @param name_from name of the column to be collapsed when compacting
#' @param name_to name of the column that will receive the collapsed column. Will be created if it doesn't exist.
#' @param ... additional arguments (not used)
#' @param id_from name of the columns to use as cut-off. Useful when successive `name_from` have the same value.
#' @param collapse levels to collapse into one row.
#' @param keep_id whether to keep `id_from` in the output.
#' @param wrap_cols name of the columns to wrap
#' @param rtn_flextable whether to return a formatted [flextable::flextable()] object or a simple `data.frame`
#' @rdname ct_compact
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom checkmate assert_scalar
#' @importFrom dplyr across all_of any_of arrange bind_rows distinct everything filter if_else lag mutate pull row_number select summarise
#' @importFrom flextable align bold border
#' @importFrom officer fp_border
#' @importFrom rlang ensym set_names
#' @importFrom stats na.omit
#' @importFrom tidyr replace_na
#'
#' @return a compacted data.frame
#'
#' @examples
#' #dataframes
#' x=iris[c(1:5,51:55,101:105),]
#' ct_compact(x, name_from="Species")
#' ct_compact(x, name_from="Species", name_to="Petal.Length")
#' x$Species2 = substr(x$Species, 1, 1)
#' ct_compact(x, name_from="Species", wrap_cols="Species2")
#' ct_compact(x, name_from="Species", id_from="Species2") #cut on "v"
ct_compact.data.frame = function(data, name_from, name_to="variable", ...,
                                 id_from=name_from, keep_id=TRUE,
                                 collapse=NULL,
                                 wrap_cols=NULL, rtn_flextable=FALSE){
  assert_scalar(name_from)
  assert_scalar(name_to)
  assert_scalar(id_from)
  id = (data[[id_from]]!=lag(data[[id_from]])) %>% replace_na(TRUE)

  if(is.null(data[[name_to]])) data[[name_to]] = ""
  remove_cols = c(name_from, id_from, "gp", "added")
  if(isTRUE(keep_id)){
    remove_cols = setdiff(remove_cols, id_from)
  }

  collapse_grp = NULL
  if(!is.null(collapse)){
    #collapse the group if only 2 options and 50% of "Yes"
    collapse_grp = data %>%
      summarise(x=mean(!!ensym(name_to)==collapse), .by=.id) %>%
      filter(x==0.5) %>%
      pull(all_of(id_from))
  }

  data[[name_to]] = as.character(data[[name_to]])
  rtn =
    data %>%
    distinct(across(all_of(c(set_names(id_from, id_from),
                             set_names(name_from, name_to))))) %>%
    bind_rows(data, .id="added") %>%
    arrange(factor(.data[[id_from]], levels=unique(data[[id_from]])), .data$added) %>%
    select(any_of(c(id_from, name_to)), everything(),
           # -all_of(wrap_cols), all_of(wrap_cols),
           -any_of(remove_cols)) %>%
    mutate(across(
      -any_of(c(id_from, name_to)),
      ~if_else(.data[[id_from]] %in% collapse_grp,
               .x[.data[[name_to]]==collapse] %0% NA, .x)
    )) %>%
    mutate(across(all_of(wrap_cols), ~if_else(row_number()==1, na.omit(.x)[1], NA)),
           .by=all_of(id_from)) %>%
    filter(!.data[[id_from]] %in% collapse_grp | row_number()==1,
           .by=all_of(id_from))
  rownames(rtn) = NULL #resets row numbers

  # if(TRUE){
  #   x = sort(c(seq(nrow(data)), which(id))) #duplicate rows
  #   nf = sym(name_from)
  #   nt = sym(name_to)
  #   rtn2 = data[x,] %>%
  #     mutate(
  #       across(everything(), as.character),
  #       gp = row_number()==1 | !!ifr!=lag(!!ifr),
  #       !!nt:=ifelse(.data$gp, !!nf, !!nt),
  #       across(any_of(wrap_cols), ~ifelse(.data$gp, .x, "")),
  #       across(-any_of(c(name_to, wrap_cols)), ~ifelse(.data$gp, "", .x)),
  #     ) %>%
  #     select(any_of(c(id_from, name_to)), everything(), -any_of(remove_cols))
  # }

  if(rtn_flextable){
    id2 = which(id)+(0:(sum(id)-1))
    rtn = rtn %>% flextable %>% border(id2, border.top = fp_border()) %>%
      bold(id2) %>% align(id2, align="left")
  }

  attr(rtn, "title_rows") = id
  rtn
}



#' @description NULL
#' @rdname ct_compact
#'
#' @param label_with_id `glue` pattern to keep the column name along with the label. If `TRUE`, default to `"{label} ({.id})"`.
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom dplyr intersect mutate
#' @importFrom glue glue
#'
#' @examples
#'
#' #crosstables
#' x=crosstable(mtcars2, c(disp,hp,am), by=vs, test=TRUE, effect=TRUE)
#' ct_compact(x)
#' ct_compact(x, name_from=".id")
ct_compact.crosstable = function(data, name_from=c("label", ".id"), name_to="variable",
                                 id_from=".id", label_with_id=FALSE, keep_id=TRUE, ...){
  by_levels = attr(data, "by_levels")
  by = attr(data, "by")
  name_from = match.arg(name_from)
  wrap_cols = intersect(names(data), c("test", "effect"))
  if(name_from=="label") rcol=".id" else rcol="label"
  if(!isFALSE(label_with_id)) {
    if(isTRUE(label_with_id)) label_with_id = "{label} ({.id})"
    data = data %>% mutate(label = glue(label_with_id))
  }
  # browser()
  rtn = data %>%
    ct_compact.data.frame(name_from=name_from, name_to=name_to, id_from=id_from,
                          wrap_cols=wrap_cols, keep_id=keep_id, rtn_flextable=FALSE, ...)

  new_attr_names = setdiff(names(attributes(data)), names(attributes(rtn)))
  attributes(rtn) = c(attributes(rtn), attributes(data)[new_attr_names])
  class(rtn) = c("compacted_crosstable", class(data))
  rtn
}



#' @description NULL
#' @rdname ct_compact
#' @usage NULL
#' @export
#' @importFrom cli cli_abort
ct_compact.default = function(data, ...) {
  cli_abort("{.fun ct_compact} is not defined for object of class {.cls {class(data)}}",
            class="ct_compact_notfound_error")
}

