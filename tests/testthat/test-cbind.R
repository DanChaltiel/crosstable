test_that("cbind() works", {

  crosstable_options(header_show_n=TRUE, num_digits=0, percent_digits=0, .local=TRUE)

  ## multi-by ----
  ct1 = mtcars2 %>%
    crosstable(cyl, by=c(am, vs),
               margin=c("row", "col"), total = FALSE)

  ct2 = mtcars2 %>%
    mutate(vs="Total") %>%
    crosstable(c(disp, cyl), by=c(am, vs),
               margin=c("row", "col"), total = FALSE)

  ct3 = mtcars2 %>%
    crosstable(c(mpg), by=c(am, vs),
               margin=c("cell"), total = FALSE)

  expect_snapshot({
    x = ct_bind_cols(ct1, ct2)
    attributes(x)
    y = cbind(ct1, ct2)
    identical(x, y)
  })

  # ct_bind_cols(ct1, ct2) %>%
  #   af()
  # cbind(ct1, ct2, ct3) %>%
  #   af()

  # ct_bis = mtcars2 %>%
  #   crosstable(c(disp, cyl), by=c(am, vs),
  #              margin=c("row", "col"), total = TRUE)
  # ct_bis %>% af()

  ## mono-by ----
  #TODO implémenter pour mono-by
  # ct1 = mtcars2 %>%
  #   crosstable(cyl, by=c(am),
  #              margin=c("row", "col"), total = FALSE)
  #
  # ct2 = mtcars2 %>%
  #   crosstable(c(cyl), by=c(vs),
  #              margin=c("row", "col"), total = FALSE)
  #
  # ct_bind_cols(ct1, ct2) %>%
  #   af()


  ## errors ----

  ct_bind_cols(ct1, ct2, ct2) %>%
    expect_error(class="ct_cbind_more_than_2_error")

  ct_bad = mtcars2 %>%
    mutate(vs="Total") %>%
    crosstable(c(disp, cyl), by=c(am),
               margin=c("row", "col"), total = FALSE)

  ct_bind_cols(ct1, ct_bad) %>%
    expect_error(class="ct_cbind_hetero_multiby_error")

})
