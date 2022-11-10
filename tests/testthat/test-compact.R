

# Purrr compatibility -------------------------------------------------------------------------

test_that("Compact method error if list without purrr", {
    ll=list(a = "a", b = NULL, c = integer(0), d = NA, e = list())
    ct=crosstable(iris)
    expect_silent(purrr::compact(ll))
    expect_error(compact(ll), class="compact_notfound_error")
    expect_error(ct_compact(ll), class="ct_compact_notfound_error")
    lifecycle::expect_deprecated(compact(ct))
    lifecycle::expect_deprecated(compact(as.data.frame(ct), name_from="label"))
})

test_that("Compact method OK with purrr", {
    ll=list(a = "a", b = NULL, c = integer(0), d = NA, e = list())
    library(purrr, include.only="compact", warn.conflicts=FALSE)
    compact=crosstable::compact
    expect_identical(compact(ll), list(a="a",d=NA))

    x=sloop::s3_dispatch(compact(ll))
    expect_true("compact.list" %in% x$method)
    x=sloop::s3_dispatch(compact(crosstable(iris)))
    expect_true("compact.crosstable" %in% x$method)
})

# Method checks -----------------------------------------------------------

test_that("Compact method OK with data.frame", {
    df=iris[c(1:5,51:55,101:105),]
    expect_silent(ct_compact(df, name_from="Species", name_to="Petal.Length", rtn_flextable=TRUE))

    x=sloop::s3_dispatch(ct_compact(df, name_from="Species"))
    expect_true("ct_compact.data.frame" %in% x$method)

    expect_snapshot({
      ct_compact(df, name_from="Species")
      ct_compact(df, name_from="Species", name_to="Petal.Length")
      df$Species2 = substr(df$Species, 1, 1)
      ct_compact(df, name_from="Species", name_to="Petal.Length", wrap_cols="Species2")
    })
})

test_that("Compact method OK with crosstable", {
    ct=crosstable(mtcars2, disp+hp+cyl+am~vs)
    expect_snapshot({
      ct_compact(ct)
      ct_compact(ct, name_from=".id")
    })

    x=sloop::s3_dispatch(ct_compact(ct))
    expect_true("ct_compact.crosstable" %in% x$method)
})


test_that("Compacting inside or outside as_flextable.crosstable gives the same result", {
    rlang::local_options(tidyselect_verbosity = "quiet")
    ct1 = crosstable(esoph, by="tobgp", test = TRUE) %>% suppressWarnings() %>% ct_compact()
    expect_equal(dim(ct1), c(22,6))
    expect_s3_class(ct1, c("data.frame", "crosstable", "compacted_crosstable"))

    ct2 = crosstable(esoph, by="tobgp", test = TRUE)
    expect_identical(as_flextable(ct1), as_flextable(ct2, compact=TRUE))
})


# Misc flextable ----------------------------------------------------------


test_that("Flextable: by_header", {
    rlang::local_options(tidyselect_verbosity = "quiet")
    ct = crosstable(esoph, by="tobgp")
    ft=ct %>% as_flextable(by_header="blabla")

    expect_setequal(ft$header$dataset[1,3:5], "blabla")
})

test_that("Flextable: show_test_name", {
    ct = crosstable(esoph, by="tobgp", test = TRUE)
    ft1=ct %>% as_flextable(show_test_name=TRUE) #default
    expect_match(ft1$body$dataset$test[1], "\\n")
    ft2=ct %>% as_flextable(show_test_name=FALSE)
    expect_match(ft2$body$dataset$test[1], ".*?\\d+(\\.\\d+)?\\s+$")
})

test_that("Flextable: keepid", {
    ct = crosstable(esoph, by="tobgp", test = TRUE)
    ft1=ct %>% as_flextable(keep_id=FALSE) #default
    ft2=ct %>% as_flextable(keep_id=TRUE)
    expect_false(".id" %in% ft1$body$col_keys)
    expect_true(".id" %in% ft2$body$col_keys)
})
