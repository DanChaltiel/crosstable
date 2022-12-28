
ct1 = crosstable(mtcars3, c(cyl, vs), by=c(am))
ct2 = crosstable(mtcars3, c(cyl, gear), by=c(am, vs))
ct3 = crosstable(mtcars3, c(disp, gear), by=c(vs, cyl3, am))

get_header_df = function(ft) ft$header$dataset


test_that("header default", {
  expect_snapshot({
    ct1 %>% as_flextable() %>% get_header_df()
    ct2 %>% as_flextable() %>% get_header_df()
    ct3 %>% as_flextable() %>% get_header_df()
  })
})

test_that("header by_header (monoby)", {
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

test_that("generic_labels", {
  expect_snapshot({
    crosstable(mtcars2, am, by=vs, total="both", test=TRUE, effect=TRUE) %>%
      rename(ID=.id, math=variable, Tot=Total, lab=label, pval=test, fx=effect) %>%
      as_flextable(by_header = "Engine shape",
                   generic_labels=list(id = "ID", variable = "math", total="Tot",
                                       label = "lab", test = "pval", effect="fx")) %>%
      get_header_df()
  })
})

test_that("get_show_n_pattern", {
  expect_snapshot({
    get_show_n_pattern()
    get_show_n_pattern("a")
    get_show_n_pattern(list(cell="a"))
    get_show_n_pattern(list(total="b"))
    get_show_n_pattern(list(cell="a", total="b"))
  })
  get_show_n_pattern(1) %>% expect_error(class="get_show_n_pattern_class")
  get_show_n_pattern(c("a", "b")) %>% expect_error(class="get_show_n_pattern_class")
  get_show_n_pattern(list("a", "b")) %>% expect_error(class="get_show_n_pattern_names")
  get_show_n_pattern(list(cell=1)) %>% expect_error(class="get_show_n_pattern_length")
})


test_that("header header_show_n+pattern", {
  expect_snapshot({
    ct2 %>%
      as_flextable(header_show_n=TRUE,
                   header_show_n_pattern="{.col_key}:\n{.col_val}\n(N={.n})",
                   remove_header_keys=T) %>%
      get_header_df()
  })
})

test_that("header header_show_n+remove_header_keys", {
  expect_snapshot({
    ct2 %>%
      as_flextable(header_show_n=TRUE,
                   remove_header_keys=TRUE) %>%
      get_header_df()
  })
})



# ct2 %>% as_flextable(header_show_n=1:2) %>% get_header_df()

test_that("header remove_header_keys", {
  expect_snapshot({
    ct1 %>% as_flextable(remove_header_keys=T) %>% get_header_df()
    ct3 %>% as_flextable(remove_header_keys=T) %>% get_header_df()
  })
})

test_that("header header_show_n", {
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



# test_that("header", {
#     expect_snapshot({
#         ct1 %>% as_flextable() %>% get_header_df()
#         ct2 %>% as_flextable() %>% get_header_df()
#         ct3 %>% as_flextable() %>% get_header_df()
#     })
# })



# test_that("header", {
#     expect_snapshot({
#         ct2 %>% as_flextable() %>% {.$header$dataset}
#         ct2 %>% as_flextable(remove_header_keys=T) %>% {.$header$dataset}
#         ct2 %>% as_flextable(header_show_n=TRUE) %>% {.$header$dataset}
#         ct2 %>% as_flextable(header_show_n=1:2) %>% {.$header$dataset}
#     })
# })

