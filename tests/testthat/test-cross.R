
test_that("cross OK", {
    mytable <- cross(cbind(...) ~ tobgp, esoph, test = TRUE)
    
    expect_equal(dim(mytable), c(18,8)) #Maybe not the most clever test...
    expect_is(mytable, c("data.frame"))
    expect_is(mytable, c("cross"))
})

test_that("compact OK", {
    mytable <- cross(cbind(...) ~ tobgp, esoph, test = TRUE) %>% 
        compact
    
    expect_equal(dim(mytable), c(26,5))
    expect_is(mytable, c("data.frame"))
    expect_is(mytable, c("compacted"))
    expect_is(mytable, c("cross"))
})


test_that("Long formula OK", {
    
    mytable <- cross(cbind(Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, 
                           Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, 
                           Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, 
                           Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Width, Sepal.Length, Sepal.Length, Sepal.Length, 
                           Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, 
                           Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, I(Sepal.Width^2), Sepal.Length, Sepal.Length, 
                           Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, I(Sepal.Width^2), Sepal.Length, Sepal.Length, 
                           Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, 
                           I(Sepal.Width^2), Sepal.Length, Sepal.Length, Sepal.Width, Sepal.Length, Sepal.Length, Sepal.Length, 
                           Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, 
                           Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length) ~ Species, iris)
    
    expect_equal(dim(mytable), c(296,6))
    expect_is(mytable, c("data.frame"))
    expect_is(mytable, c("cross"))
})
