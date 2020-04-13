
# Init --------------------------------------------------------------------

Sys.setenv(LANG = "en")
options(warn = 1)



# Method checks -----------------------------------------------------------

test_that("Compact method error if list without purrr", {
    ll=list(a = "a", b = NULL, c = integer(0), d = NA, e = list())
    expect_silent(purrr::compact(ll))
    if(is_testing())
        expect_error(compact(ll), 'could not find function "compact" for objects of class other than `crosstable` or `dataframe`')
})

test_that("Compact method OK with purrr", {
    ll=list(a = "a", b = NULL, c = integer(0), d = NA, e = list())
    library(purrr)
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
    expect_identical(x$method, c("compact.crosstable", "compact.data.frame", "compact.default"))
})

test_that("Compact method OK with data.frame", {
    df=iris[c(1:5,51:55,101:105),]
    x1=expect_silent(compact(df, name_from="Species"))
    expect_equal(dim(x1), c(18,5))
    expect_equivalent(x1[1,], c("setosa", "", "", "", ""))
    
    expect_silent(compact(df, name_from="Species", name_to="Petal.Length", rtn_flextable=TRUE))
    x2=expect_silent(compact(df, name_from="Species", name_to="Petal.Length"))
    expect_equal(dim(x2), c(18,4))
    expect_equivalent(x2[1,], c("setosa", "", "", ""))
    
    x=sloop::s3_dispatch(compact(x1))
    expect_identical(x$method, c("compact.data.frame", "compact.default"))
    x=sloop::s3_dispatch(compact(x2))
    expect_identical(x$method, c("compact.data.frame", "compact.default"))
})

test_that("Compacting inside or outside cross_to_flextable gives the same result", {
    ct1 = crosstable(esoph, by="tobgp", test = TRUE) %>% compact
    expect_equal(dim(ct1), c(22,6))
    expect_is(ct1, c("data.frame"))
    expect_is(ct1, c("crosstable"))
    expect_is(ct1, c("compacted_crosstable"))
    
    ct2 = crosstable(esoph, by="tobgp", test = TRUE)
    expect_identical(ctf(ct1), ctf(ct2, compact=TRUE))
})
