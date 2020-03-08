context("cross tables")

library(Hmisc)
label(iris$Species) = "Espèce"
label(iris$Sepal.Length) = "Longueur du Sépale"
label(iris$Sepal.Width) = "Longueur du Sépale"
label(iris$Petal.Length) = "Longueur du Pétale"
label(iris$Petal.Width) = "Largeur du Pétale"



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



# cross(cbind(Petal.Length, Species) ~ cbind(Petal.Length, Petal.Width), iris)
# cross(Petal.Length + Species ~ Petal.Length + Petal.Width, iris)

# crosstable(cbind(mpg, factor(vs)) + cbind(drat, hp) ~ cbind(cyl, factor(am)), mtcars) #boss de fin

