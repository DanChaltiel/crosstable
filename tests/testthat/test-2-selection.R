
# Init --------------------------------------------------------------------
# Sys.setenv("R_TESTS" = "")


expss::var_lab(iris$Species) = "Espèce"
expss::var_lab(iris$Sepal.Length) = "Longueur du Sépale"
expss::var_lab(iris$Sepal.Width) = "Largeur du Sépale"
expss::var_lab(iris$Petal.Length) = "Longueur du Pétale"
expss::var_lab(iris$Petal.Width) = "Largeur du Pétale"

Sys.setenv(LANG = "en")
options(warn = 1)
options(stringsAsFactors = FALSE)
options(tidyselect_verbosity = "verbose")

irisnames = c(SL="Sepal.Length", SW="Sepal.Width", PL="Petal.Length", PW="Petal.Width", Sp="Species")
iris_num = iris %>% select(-Species)
debug=list()
# debug %>% map_dfr(identity) %>% table

expect_cross = function(expr, xnames, byname, dim, expect=c("nothing", "silent", "warning", "error"), regex){
    expect=match.arg(expect)
    if(expect=="nothing"){
        x=eval(expr, envir=caller_env())
    }
    else if(expect=="silent")
        x=expect_silent(expr)
    else if(expect=="warning")
        x=expect_warning(expr, regex)
    else
        x=expect_error(expr, regex)
    expect_is(x, c("data.frame", "crosstable"))
    expect_equal(dim, dim(x))
    expect_equal(byname, unname(attr(x, "by")))
    
    if(all(xnames %in% names(irisnames)))
        expect_equal(unname(irisnames[xnames]), unique(as.character(x$.id)))
    else
        expect_equal(unname(xnames), unique(x$.id))
    debug <<- c(debug, list(attr(x, "debug")))
}




# Whole table --------------------------------------------------
test_that("crosstable whole table", {
    expect_cross(
        crosstable(iris), 
        xnames=c("SL", "SW", "PL", "PW", "Sp"), byname=NULL, dim=c(19,4))
    expect_cross(
        crosstable(iris, by=Species), 
        xnames=c("SL", "SW", "PL", "PW"), byname="Species", dim=c(16,6))
})


# Unquoted name -------------------------------------------
test_that("crosstable with unquoted name", {
    expect_cross(
        crosstable(iris, Sepal.Length, by=Species),
        xnames=c("SL"), byname="Species", dim=c(4,6))
    expect_cross(
        crosstable(iris, c(Sepal.Length, Sepal.Width), by=Species),
        xnames=c("SL", "SW"), byname="Species", dim=c(8,6))
    expect_cross(
        crosstable(iris, Sepal.Length, Sepal.Width, by=Species),
        xnames=c("SL", "SW"), byname="Species", dim=c(8,6))
    #negation
    expect_cross(
        crosstable(iris, -c(Sepal.Length, Sepal.Width), by="Species"),
        xnames=c("PL", "PW"), byname="Species", dim=c(8,6))
    expect_cross(
        crosstable(iris, -Sepal.Length, -Sepal.Width, by="Species"),
        xnames=c("PL", "PW"), byname="Species", dim=c(8,6))
})


# Character vector ----------------------------------------
test_that("crosstable with character vector", {#TODO: expect warning for all_of ?
    expect_cross(
        crosstable(iris, "Sepal.Length", by="Species"),
        xnames=c("SL"), byname="Species", dim=c(4,6))
    expect_cross(
        crosstable(iris, c("Sepal.Length", "Sepal.Width"), by="Species"),
        xnames=c("SL", "SW"), byname="Species", dim=c(8,6))
    expect_cross(
        crosstable(iris, "Sepal.Length", "Sepal.Width", by="Species"),
        xnames=c("SL", "SW"), byname="Species", dim=c(8,6))
    #negation
    expect_cross(
        crosstable(iris, -c("Sepal.Length", "Sepal.Width"), by="Species"),
        xnames=c("PL", "PW"), byname="Species", dim=c(8,6))
    expect_cross(
        crosstable(iris, -"Sepal.Length", -"Sepal.Width", by="Species"),
        xnames=c("PL", "PW"), byname="Species", dim=c(8,6))
})


# External character vector -------------------------------
test_that("crosstable with external character vector", {
    XX=c("Sepal.Length", "Sepal.Width") #cf helper-crosstable.R
    
    expect_cross(
        crosstable(iris, all_of(XX), by="Species"),
        xnames=c("SL", "SW"), byname="Species", dim=c(8,6))
    
    expect_cross(#0 to remove .vars effect
        crosstable(iris, 0, all_of(XX), by="Species"),
        xnames=c("SL", "SW"), byname="Species", dim=c(8,6))
    
    expect_cross(
        crosstable(iris, all_of(XX), -Sepal.Width, by="Species"),
        xnames=c("SL"), byname="Species", dim=c(4,6))
    
    expect_cross(#expect warning, but only once a session
        crosstable(iris, XX, by="Species"),
        xnames=c("SL", "SW"), byname="Species", dim=c(8,6))
})


# Indices -----------------------------------------------------------------
test_that("crosstable with indices", {
    expect_cross(
        crosstable(iris, 1:2, by="Species"),
        xnames=c("SL", "SW"), byname="Species", dim=c(8,6))
    expect_cross(
        crosstable(iris, -1, by="Species"),
        xnames=c("SW", "PL", "PW"), byname="Species", dim=c(12,6))
    expect_cross(
        crosstable(iris, -(1:2), by="Species"),
        xnames=c("PL", "PW"), byname="Species", dim=c(8,6))
})


# Tidyselect helpers ------------------------------------------------------
test_that("crosstable with tidyselect helpers", {
    expect_cross(
        crosstable(iris, everything()),
        xnames=c("SL", "SW", "PL", "PW", "Sp"), byname=NULL, dim=c(19,4))
    expect_cross(
        crosstable(iris, starts_with("S")),
        xnames=c("SL", "SW", "Sp"), byname=NULL, dim=c(11,4))
    expect_cross(
        crosstable(iris, starts_with("S"), ends_with("idth")),
        xnames=c("SL", "SW", "Sp", "PW"), byname=NULL, dim=c(15,4))
})



# Correlations ------------------------------------------------------------
test_that("crosstable with correlations", {
    expect_cross(
        crosstable(iris, 1:3, by=Petal.Width),
        xnames=c("SL", "SW", "PL"), byname="Petal.Width", dim=c(3,4))
})


# Single function ---------------------------------------------------------
test_that("crosstable with a single function", {
    foo = as_function(~mean(.x)>3.5)
    bar = as_function(~sd(.x)>0.5 & sd(.x)<1)
    
    #standard functions
    expect_cross(
        crosstable(iris_num, where(foo)),
        xnames=c("SL", "PL"), byname=NULL, dim=c(8,4))
    expect_cross(
        crosstable(iris_num, where(bar)),
        xnames=c("SL", "PW"), byname=NULL, dim=c(8,4))
    
    #lambda and anonymous functions
    expect_cross(
        crosstable(iris_num, ~mean(.x)>3.5),
        xnames=c("SL", "PL"), byname=NULL, dim=c(8,4))
    expect_cross(
        crosstable(iris_num, function(A) mean(A)>3.5),
        xnames=c("SL", "PL"), byname=NULL, dim=c(8,4))
})



# Multiple functions ------------------------------------------------------
test_that("crosstable with multiple functions", {
    foo = as_function(~mean(.x)>3.5)
    bar = as_function(~sd(.x)>0.5 & sd(.x)<1)
    
    #standard functions
    expect_cross(
        crosstable(iris_num, where(foo) & where(bar)),
        xnames=c("SL"), byname=NULL, dim=c(4,4))
    expect_cross(
        crosstable(iris_num, where(foo), where(bar)),
        xnames=c("SL", "PL", "PW"), byname=NULL, dim=c(12,4))
    expect_cross(
        crosstable(iris_num, where(bar), where(foo)),
        xnames=c("SL", "PW", "PL"), byname=NULL, dim=c(12,4))
    expect_cross(
        crosstable(iris_num, where(foo) | where(bar)),
        xnames=c("SL", "PL", "PW"), byname=NULL, dim=c(12,4))
    
    #lambda and anonymous functions
    expect_cross(
        crosstable(iris_num, ~mean(.x)>3.5, ~sd(.x)>1),
        xnames=c("SL", "PL"), byname=NULL, dim=c(8,4))
    
    #complex function composition
    expect_cross(
        crosstable(iris_num, where(is.numeric) & (where(foo) | where(bar))),
        xnames=c("SL", "PL", "PW"), byname=NULL, dim=c(12,4))
    expect_cross(
        crosstable(iris_num, where(is.numeric) | (where(foo) & where(bar))),
        xnames=c("SL", "SW", "PL", "PW"), byname=NULL, dim=c(16,4))
    expect_cross(
        crosstable(iris_num, where(foo) | where(bar), where(is.numeric), -Petal.Length),
        xnames=c("SL", "PW", "SW"), byname=NULL, dim=c(12,4))
    expect_cross(
        crosstable(iris_num, where(foo) | where(bar), -where(is.numeric), Petal.Length),
        xnames=c("PL"), byname=NULL, dim=c(4,4))
})


# Formula -----------------------------------------------------------------
test_that("crosstable with formula", {
    
    expect_cross(
        crosstable(iris, Sepal.Length+Sepal.Width~Species),
        xnames=c("SL", "SW"), byname="Species", dim=c(8,6))
    
    expect_cross(
        crosstable(iris, Sepal.Length+I(Sepal.Width^2)+I(Sepal.Length+Sepal.Width)~Species),
        xnames=c("Sepal.Length", "I(Sepal.Width^2)", "I(Sepal.Length + Sepal.Width)"),
        byname="Species", dim=c(12,6))
    
    expect_cross(
        crosstable(iris, Sepal.Length+Sepal.Width~ifelse(Species=="setosa", "foo", "bar")),
        xnames=c("SL", "SW"), dim=c(8,5),
        byname="ifelse(Species == \"setosa\", \"foo\", \"bar\")")
    
    expect_cross(
        crosstable(iris, everything()),
        xnames=c("SL", "SW", "PL", "PW", "Sp"), byname=NULL, dim=c(19,4))
    
    #there can be a bug when formula are longer than 500 characters (because of deparse)
    x=paste0("I(Sepal.Length^", (1:50)/10, ")", collapse=" + ")
    ff = paste0("",x," ~ Species")#cf helper-crosstable.R
    mytable = crosstable(iris, as.formula(ff), label=F)
    expect_equal(nchar(ff), 1097) #>500
    expect_equal(dim(mytable), c(200,6))
    expect_is(mytable, c("data.frame", "crosstable"))
})



# Ultimate selection (TODO) -----------------------------------------------
#TODO ultimate selection
test_that("crosstable ultimate selection", {
    expect_cross(
        crosstable(iris, everything()),
        xnames=c("SL", "SW", "PL", "PW", "Sp"), byname=NULL, dim=c(19,4))
    expect_cross(
        crosstable(iris, ~is.numeric(.x), where(is.double), "Species", -Sepal.Width),
        xnames=c("SL", "PL", "PW", "Sp"), byname=NULL, dim=c(15,4))
})



# Warnings ------------------------------------------------------------------
test_that("crosstable limit tests: warnings", {
    #no selection
    expect_warning(crosstable(iris, function(x) FALSE),
                   "Variable selection in crosstable ended with no variable to describe")
    expect_warning(crosstable(iris, 0),
                   "Variable selection in crosstable ended with no variable to describe")
    expect_warning(crosstable(iris, 0, by="Species"),
                   "Variable selection in crosstable ended with no variable to describe")
    
    #removes unfit variables with a warning
    expect_warning(crosstable(iris, Sepal.Length, Species, by=Petal.Width),
                   "Cannot cross column 'Species' \\(factor\\) by column 'Petal.Width' \\(numeric\\)")
})


# Errors ------------------------------------------------------------------
test_that("crosstable limit tests: errors", {
    
    #Multiple `by` statement
    expect_error(
        crosstable(mtcars, by=c("vs", "am")),
        "Crosstable does not support multiple `by` columns.*")
    expect_error(
        {mtcars %>% dplyr::mutate_at(c("vs", "am"), factor) %>% crosstable(1:2, by=c("vs", "am"))},
        "Crosstable does not support multiple `by` columns.*")
    
    #either formula or `by` but not both
    expect_error(crosstable(iris, Sepal.Width~Species, by="Species"),
                 "`by` argument is ignored when using formula")
    
    #one-sided formula but not a lambda
    A="foobar" #in helper-crosstable.R
    expect_error({A="foobar";crosstable(iris, ~A, by="Species")},
                 "Can't coerce element 1 from a character to a logical.*")
    expect_error(crosstable(iris, ~B, by="Species"),
                 "object 'B' not found.*")
    
    #wrong functions (returning non-scalar)
    expect_error(crosstable(iris, ~.x, by="Species"),
                 "Result 1 must be a single logical, .*", class = "rlang_error")
    expect_error(crosstable(iris, ~c(is.numeric(.x),is.numeric(.x)), by="Species"),
                 "Result 1 must be a single logical, .*", class = "rlang_error")
})











