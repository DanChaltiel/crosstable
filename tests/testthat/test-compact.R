
# Init --------------------------------------------------------------------



# Method checks -----------------------------------------------------------

test_that("Compact method error if list without purrr", {
    ll=list(a = "a", b = NULL, c = integer(0), d = NA, e = list())
    expect_silent(purrr::compact(ll))
    if(is_testing()){
        expect_error(compact(ll), class="compact_notfound_error")
    }
})

test_that("Compact method OK with purrr", {
    ll=list(a = "a", b = NULL, c = integer(0), d = NA, e = list())
    library(purrr, include.only="compact", warn.conflicts=FALSE)
    compact = crosstable::compact
    expect_identical(compact(ll), list(a="a",d=NA))

    x=sloop::s3_dispatch(compact(ll))
    expect_identical(x$method, c("compact.list", "compact.default"))
})

test_that("Compact method OK with crosstable", {
    ct=crosstable(mtcars2, disp+hp+cyl+am~vs)
    expect_silent(compact(ct, name_from=".id"))
    x1=expect_silent(compact(ct))
    expect_equal(dim(x1), c(17,3))
    expect_equal(sum(x1[[1]]==""), 0)
    expect_equal(sum(x1[[2]]==""), 4)
    
    x=sloop::s3_dispatch(compact(ct))
    expect_true(all(c("compact.data.frame", "compact.default") %in% x$method))
})

test_that("Compact method OK with data.frame", {
    df=iris[c(1:5,51:55,101:105),]
    x1=expect_silent(compact(df, name_from="Species"))
    expect_equal(dim(x1), c(18,5))
    expect_equal(as.character(x1[1,]), c("setosa", "", "", "", ""))

    expect_silent(compact(df, name_from="Species", name_to="Petal.Length", rtn_flextable=TRUE))
    x2=expect_silent(compact(df, name_from="Species", name_to="Petal.Length"))
    expect_equal(dim(x2), c(18,4))
    expect_equal(as.character(x2[1,]), c("setosa", "", "", ""))

    x=sloop::s3_dispatch(compact(x1))
    expect_true(all(c("compact.data.frame", "compact.default") %in% x$method))
    x=sloop::s3_dispatch(compact(x2))
    expect_true(all(c("compact.data.frame", "compact.default") %in% x$method))
})

test_that("Compacting inside or outside as_flextable.crosstable gives the same result", {
    rlang::local_options(tidyselect_verbosity = "quiet")
    ct1 = crosstable(esoph, by="tobgp", test = TRUE) %>% suppressWarnings() %>% compact()
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
