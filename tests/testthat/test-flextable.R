
ct1 = crosstable(mtcars3, c(cyl, vs), by=c(am))
ct2 = crosstable(mtcars3, c(cyl, gear), by=c(am, vs))
ct3 = crosstable(mtcars3, c(disp, gear), by=c(vs, cyl3, am))

get_header_df = function(ft) ft$header$dataset


test_that("flextable header default", {
  expect_snapshot({
    ct1 %>% as_flextable() %>% get_header_df()
    ct2 %>% as_flextable() %>% get_header_df()
    ct3 %>% as_flextable() %>% get_header_df()
  })
})

test_that("flextable header by_header (monoby)", {
  expect_equal(ct1 %>% as_flextable(),
               ct1 %>% as_flextable(by_header=TRUE))
  expect_equal(ct1 %>% as_flextable(by_header=NULL),
               ct1 %>% as_flextable(by_header=TRUE))
  ct2 %>% as_flextable(by_header=FALSE) %>% expect_warning(class="crosstable_asflex_byheader_multi")

  expect_snapshot({
    ct1 %>% as_flextable(by_header=NULL) %>% get_header_df()
    ct1 %>% as_flextable(by_header=FALSE) %>% get_header_df()
  })
})

test_that("flextable generic_labels", {
  expect_snapshot({
    ct = crosstable(mtcars2, am, by=vs, total="both", test=TRUE, effect=TRUE) %>%
      rename(ID=.id, math=variable, Tot=Total, lab=label, pval=test, fx=effect) %>%
      as_flextable(by_header = "Engine shape",
                   generic_labels=list(id = "ID", variable = "math", total="Tot",
                                       label = "lab", test = "pval", effect="fx"))
    ct$header$dataset
  })
})


test_that("flextable header header_show_n+pattern", {
  expect_snapshot({
    ct2 %>%
      as_flextable(header_show_n=TRUE,
                   header_show_n_pattern="{.col_key}:\n{.col_val}\n(N={.n})",
                   remove_header_keys=T) %>%
      get_header_df()
  })
})



# ct2 %>% as_flextable(header_show_n=1:2) %>% get_header_df()

test_that("flextable header remove_header_keys", {
  expect_snapshot({
    ct1 %>% as_flextable(remove_header_keys=T) %>% get_header_df()
    ct3 %>% as_flextable(remove_header_keys=T) %>% get_header_df()
  })
})

test_that("flextable header header_show_n", {
  expect_snapshot({
    ct1 %>% as_flextable(header_show_n=TRUE) %>% get_header_df()
    ct3 %>% as_flextable(header_show_n=TRUE) %>% get_header_df()
  })
})


#TODO Tests à implémenter en snapshot (2.8sec)
# ct1 %>% af(header_show_n=0) %>% {.$header$dataset}
# ct1 %>% af(header_show_n=0)
# ct1 %>% af(header_show_n=1)
# ct1 %>% af(header_show_n=2) #TODO warning?
# ct1 %>% af(header_show_n=TRUE)
# ct1 %>% af(header_show_n=FALSE)
#
# ct2 %>% af(header_show_n=0)
# ct2 %>% af(header_show_n=1)
# ct2 %>% af(header_show_n=2)
# ct2 %>% af(header_show_n=1:2)
# ct2 %>% af(header_show_n=TRUE)
# ct2 %>% af(header_show_n=FALSE)
#
#
#
# ct2 %>% af(header_show_n=TRUE)
# ct2 %>% af(header_show_n=TRUE, header_show_n_pattern="{.col} (N={.n})")
# ct2 %>% af(header_show_n=1, header_show_n_pattern="{.col_key}:\n{.col_val}\n(N={.n})", remove_header_keys=T)
# ct2 %>% af(header_show_n=TRUE, header_show_n_pattern="{.col_key} (N={.n})")



# test_that("flextable header", {
#     expect_snapshot({
#         ct1 %>% as_flextable() %>% get_header_df()
#         ct2 %>% as_flextable() %>% get_header_df()
#         ct3 %>% as_flextable() %>% get_header_df()
#     })
# })



# test_that("flextable header", {
#     expect_snapshot({
#         ct2 %>% as_flextable() %>% {.$header$dataset}
#         ct2 %>% as_flextable(remove_header_keys=T) %>% {.$header$dataset}
#         ct2 %>% as_flextable(header_show_n=TRUE) %>% {.$header$dataset}
#         ct2 %>% as_flextable(header_show_n=1:2) %>% {.$header$dataset}
#     })
# })

