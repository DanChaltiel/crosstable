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
    # ct_list = ct_list %>%
    #   map(~{
    #     by = attr(.x, "by")
    #     if(length(by)>1) return(.x)
    #     lvls = attr(.x, "by_levels")[[1]]
    #     i = names(.x) %in% lvls
    #     new_names = paste0(by, "=", names(.x)[i])
    #     names(.x)[i] = new_names
    #     names(attr(.x, "by_levels")[[1]]) = new_names
    #     # browser()
    #     rownames(attr(.x, "by_table")) = new_names
    #     # attr(.x, "by_levels") %>% names
    #
    #     .x
    #   })
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
    # cli_abort("Cannot cbind crosstable with only one `by` dimension: {.val {by_dim}}",
    #           class="ct_cbind_monocol_error")

    id = ct_list %>% map(~unique(.x$.id))
    if(n_distinct(id)>1){
      cli_abort("Cannot cbind mono-column crosstables with different `.id`: {id}",
                class="ct_cbind_hetero_byname_error")
    }

    # browser()
    ct_list2 = ct_list %>%
      map(~{
        by = attr(.x, "by")
        if(length(by)>1) return(.x)
        lvls = attr(.x, "by_levels")[[1]]
        i = names(.x) %in% lvls
        new_names = paste0(by, "=", names(.x)[i])
        names(.x)[i] = new_names
        # names(attr(.x, "by_levels")[[1]]) = new_names
        # browser()
        rownames(attr(.x, "by_table")) = new_names
        # attr(.x, "by_levels") %>% names

        .x
      })

    #TODO control of rows order, e.g.:
    # ct_list2 = ct_list2[order(lengths(ct_list2))]
    rtn = reduce(ct_list2, ~full_join(.x, .y, by=c(".id", "label", "variable")))
    rtn2 = mtcars2 %>% crosstable(cyl, by=c(am, vs))

    by_label = map(ct_list2, ~attr(.x, "by_label")) %>% unname() %>% unlist()
    attr(rtn, "by_label") = by_label
    attr(rtn, "by_levels") = map(ct_list2, ~attr(.x, "by_levels")) %>% unname() %>% unlist(recursive=F)
    attr(rtn, "by_table") = map(ct_list2, ~attr(.x, "by_table")) %>% unname() %>% unlist() %>%
      set_names(~{
        p = str_remove(.x, "=.*")
        # paste(p, by_label[p], sep="=")
        l = paste(p, by_label[p], sep="=")
        paste(" & ", l)
        l
      })
    # attr(rtn, "by_table") = map(ct_list2, ~attr(.x, "by_table")) %>% unname() %>% unlist() %>%
    #   set_names(~{
    #     # browser()
    #     paste(" & ", .x)
    #     # by_label
    #   })

    # debugonce(af)
    #FIXME en fait c'est une galère car le header se base sur la names(ct), pas sur les attributes...
    # browser()
    rtn %>% af()

    rtn2 %>% af()
    rtn %>% attributes()
    rtn2 %>% attributes()


    attr(rtn, "by_table"); attr(rtn2, "by_table")
    # attr(rtn, "by_label"); attr(rtn2, "by_label")
    # attr(rtn, "by_levels"); attr(rtn2, "by_levels")


    cli_abort("In fact, we can only cbind multi-column crosstables for now.",
              class="ct_cbind_monocol_error")
    # stop("en faisant un cbind, on transforme en multiby! Il faut donc refaire toute l'ingénierie...")

  }

  # browser()

  # ct_list %>% map(names)
  # rtn %>% af()

  # rtn = full_join(ct1, ct2, by=c(".id", "label", "variable"))



  # ct1 %>% attributes()
  # ct2 %>% attributes()
  # .x %>% attributes()
  # rtn %>% attributes()

  # attr(rtn, "by_table") = c(attr(ct1, "by_table"), attr(ct2, "by_table"))
  # attr(rtn, "by_label");attr(ct1, "by_label"); attr(ct2, "by_label")
  # attr(rtn, "by_levels");attr(ct1, "by_levels"); attr(ct2, "by_levels")
  #
  # lst(rtn, ct1, ct2) %>% map(~{attr(.x, "by")})
  #
  # waldo::compare(
  #   # ct1 %>% attributes(),
  #   ct2 %>% attributes(),
  #   rtn %>% attributes()
  # )
  #
  # rtn %>% af(header_show_n=T)
  #
  # browser()

  rtn
}


#' @export
cbind.crosstable = function(...){
  ct_bind_cols(...)
}

