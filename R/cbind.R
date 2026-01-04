#' Combine crosstables
#'
#' `r lifecycle::badge("experimental")`\cr
#' Combine multiple crosstables into one. This makes it possible to have multi-column
#' crosstables with intra-group margins. A better solution will be found later.
#'
#' @param ... crosstable objects
#'
#' @returns a wider crosstable
#' @importFrom dplyr full_join
#' @export
#'
#' @examples
#' ct1 = mtcars2 %>%
#' crosstable(c(disp, cyl), by=c(am, vs),
#'            margin=c("row", "col"))
#'
#' ct2 = mtcars2 %>%
#'   dplyr::mutate(vs="Total") %>%
#'   crosstable(c(disp, cyl), by=c(am, vs),
#'              margin=c("row", "col"))
#'
#' ct_bind_cols(ct1, ct2) %>%
#'   af(header_show_n=TRUE)
ct_bind_cols = function(...){

  ct_list = lst(...)

  if(length(ct_list)>2){
    cli_abort("Cannot cbind more than 2 crosstables",
              class="ct_cbind_more_than_2_error")
  }

  by_dim = ct_list %>% map(~attr(.x, "by"))
  multi_by = by_dim %>% map_lgl(~length(.x)>1)
  heterogeneous = !all(multi_by) && any(multi_by)
  if(heterogeneous){
    cli_abort("Cannot cbind crosstable with different `by` dimensions: {.val {by_dim}}",
              class="ct_cbind_hetero_multiby_error")
  }

  all_multi_by = all(multi_by)
  if(all_multi_by){
    if(n_distinct(by_dim)>1){
      cli_abort("Cannot cbind multi-column crosstables with different `by` dimensions: {by_dim}",
                class="ct_cbind_multicol_byname_error")
    }
    rtn = reduce(ct_list, ~full_join(.x, .y, by=c(".id", "label", "variable")))
    attr(rtn, "by_table") = map(ct_list, ~attr(.x, "by_table")) %>% unname() %>% unlist()
  } else {
    cli_abort("Cannot cbind crosstable with only one `by` dimension: {.val {by_dim}}",
              class="ct_cbind_monocol_error")

    id = ct_list %>% map(~unique(.x$.id))
    if(n_distinct(id)>1){
      cli_abort("Cannot cbind mono-column crosstables with different `.id`: {id}",
                class="ct_cbind_hetero_byname_error")
    }
  }

  rtn
}


#' @export
cbind.crosstable = function(...){
  ct_bind_cols(...)
}

