

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
#' @param wrap_cols name of the columns to wrap
#' @param rtn_flextable whether to return a formatted [flextable()] object or a simple `data.frame`
#' @param ... additional arguments (not used)
#' @rdname ct_compact
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom tidyr replace_na
#' @importFrom rlang :=
#' @importFrom tidyselect any_of everything
#' @importFrom dplyr lag mutate mutate_at mutate_all vars
#' @importFrom officer fp_border
#' @importFrom flextable align bold border
#'
#' @return a compacted data.frame
#'
#' @examples
#' #dataframes
#' x=iris[c(1:5,51:55,101:105),]
#' ct_compact(x, name_from="Species")
#' ct_compact(x, name_from="Species", name_to="Petal.Length")
ct_compact.data.frame = function(data, name_from, name_to="variable", wrap_cols=NULL, rtn_flextable=FALSE, ...){
  assert_scalar(name_from)
  assert_scalar(name_to)
  id = (data[[name_from]]!=lag(data[[name_from]])) %>% replace_na(TRUE)
  rtn = data.frame()
  for(i in 1:sum(id)) {
    x1 = which(id)[i]
    x2 = which(id)[i+1]-1
    if(is.na(x2)) x2 = nrow(data)
    row = data[1,] %>% mutate_all(~"") %>% mutate(!!sym(name_to):=data[x1,name_from]) %>%
      mutate_all(as.character)
    rows = data[x1:x2,] %>% mutate_all(as.character)
    if(is.null(rows[[name_to]])) rows[[name_to]]=""
    rtn = rbind(rtn, row, rows)
  }

  id2 = which(id)+(0:(sum(id)-1))
  rtn = rtn %>%
    select(any_of(name_to), everything(), -any_of(name_from)) %>%
    mutate_at(vars(any_of(wrap_cols)), ~{
      xx=.x
      xx[id2]=lead(xx)[id2]
      xx[-id2]=""
      xx
    })

  if(rtn_flextable){
    rtn = rtn %>% flextable %>% border(id2, border.top = fp_border()) %>%
      bold(id2) %>% align(id2, align="left")
  }

  rownames(rtn) = NULL #resets row numbers
  attr(rtn, "title_rows") = id
  rtn
}



#' @description NULL
#' @rdname ct_compact
#'
#' @param keep_id `glue` pattern to keep the column name along with the label. If `TRUE`, default to `"{label} ({.id})"`.
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom dplyr select %>% .data intersect
#' @importFrom stringr str_subset
#' @importFrom tidyselect any_of
#'
#' @examples
#'
#' #crosstables
#' x=crosstable(mtcars2, c(disp,hp,am), by=vs, test=TRUE, effect=TRUE)
#' ct_compact(x)
#' ct_compact(x, name_from=".id")
ct_compact.crosstable = function(data, name_from=c("label", ".id"), name_to="variable", keep_id=FALSE, ...){
  by_levels = attr(data, "by_levels")
  by = attr(data, "by")
  name_from = match.arg(name_from)
  wrap_cols = intersect(names(data), c("test", "effect"))
  if(name_from=="label") rcol=".id" else rcol="label"
  if(!isFALSE(keep_id)) {
    if(isTRUE(keep_id)) keep_id = "{label} ({.id})"
    data = data %>% mutate(label = glue(keep_id))
  }

  rtn = data %>%
    select(-any_of(rcol)) %>%
    ct_compact.data.frame(name_from=name_from, name_to=name_to,
                       wrap_cols=wrap_cols, rtn_flextable=FALSE)

  new_attr_names = setdiff(names(attributes(data)), names(attributes(rtn)))
  attributes(rtn) = c(attributes(rtn), attributes(data)[new_attr_names])
  class(rtn) = c("crosstable", "compacted_crosstable", "data.frame")
  rtn
}



#' @description NULL
#' @rdname ct_compact
#' @usage NULL
#' @export
ct_compact.default = function(data, ...) {
  cli_abort("{.fun ct_compact} is not defined for object of class {.cls {class(data)}}",
            class="ct_compact_notfound_error")
}




# Deprecated --------------------------------------------------------------


#' @description NULL
#' @rdname ct_compact
#' @usage NULL
#' @export
compact = function(data, ...){
  UseMethod("compact")
}

#' @description NULL
#' @rdname ct_compact
#' @usage NULL
#' @export
compact.data.frame = function(data, ...){
  deprecate_warn("0.5.0", "compact.data.frame()", "ct_compact.data.frame()", details="Or use purrr::compact()")
  ct_compact.data.frame(data, ...)
}

#' @description NULL
#' @rdname ct_compact
#' @usage NULL
#' @export
compact.crosstable = function(data, ...){
  deprecate_warn("0.5.0", "compact.crosstable()", "ct_compact.crosstable()")
  ct_compact.crosstable(data, ...)
}

#' @description NULL
#' @rdname ct_compact
#' @usage NULL
#' @export
compact.default = function(data, ...) {
  fn=get_defined_function('compact')
  if(is.null(fn) || is.null(fn[[1L]]))
    cli_abort('could not find function "compact" for objects of class other than `crosstable` or `dataframe`',
              class="compact_notfound_error")
  fn[[1L]](data, ...)
}
