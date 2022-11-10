



# Whole table --------------------------------------------------
test_that("crosstable whole table", {
  expect_cross(
    crosstable(iris2),
    xnames=c("SL", "SW", "PL", "PW", "Sp"), byname=NULL, dim=c(19,4))
  expect_cross(
    crosstable(iris2, by=Species),
    xnames=c("SL", "SW", "PL", "PW"), byname="Species", dim=c(16,6))
})


# Unquoted name -------------------------------------------
test_that("crosstable with unquoted name", {
  expect_cross(
    crosstable(iris2, Sepal.Length, by=Species),
    xnames=c("SL"), byname="Species", dim=c(4,6))
  expect_cross(
    crosstable(iris2, c(Sepal.Length, Sepal.Width), by=Species),
    xnames=c("SL", "SW"), byname="Species", dim=c(8,6))

  #negation
  expect_cross(
    crosstable(iris2, -c(Sepal.Length, Sepal.Width), by="Species"),
    xnames=c("PL", "PW"), byname="Species", dim=c(8,6))
})


# Character vector ----------------------------------------
test_that("crosstable with character vector", {
  expect_cross(
    crosstable(iris2, "Sepal.Length", by="Species"),
    xnames=c("SL"), byname="Species", dim=c(4,6))
  expect_cross(
    crosstable(iris2, c("Sepal.Length", "Sepal.Width"), by="Species"),
    xnames=c("SL", "SW"), byname="Species", dim=c(8,6))
  #negation
  expect_cross(
    crosstable(iris2, -c("Sepal.Length", "Sepal.Width"), by="Species"),
    xnames=c("PL", "PW"), byname="Species", dim=c(8,6))
})


# External character vector -------------------------------
test_that("crosstable with external character vector", {
  XX=c("Sepal.Length", "Sepal.Width") #cf helper-crosstable.R
  rlang::local_options(tidyselect_verbosity = "verbose")

  expect_cross(
    crosstable(iris2, all_of(XX), by="Species"),
    xnames=c("SL", "SW"), byname="Species", dim=c(8,6)) %>%
    expect_silent()

  expect_cross(
    crosstable(iris2, c(all_of(XX), -Sepal.Width), by="Species"),
    xnames=c("SL"), byname="Species", dim=c(4,6)) %>%
    expect_silent()
})


# Indices -----------------------------------------------------------------
test_that("crosstable with indices", {
  expect_cross(
    crosstable(iris2, 1:2, by="Species"),
    xnames=c("SL", "SW"), byname="Species", dim=c(8,6))
  expect_cross(
    crosstable(iris2, -1, by="Species"),
    xnames=c("SW", "PL", "PW"), byname="Species", dim=c(12,6))
  expect_cross(
    crosstable(iris2, -(1:2), by="Species"),
    xnames=c("PL", "PW"), byname="Species", dim=c(8,6))
})


# Tidyselect helpers ------------------------------------------------------
test_that("crosstable with tidyselect helpers", {
  expect_cross(
    crosstable(iris2, everything()),
    xnames=c("SL", "SW", "PL", "PW", "Sp"), byname=NULL, dim=c(19,4))
  expect_cross(
    crosstable(iris2, starts_with("S")),
    xnames=c("SL", "SW", "Sp"), byname=NULL, dim=c(11,4))
  expect_cross(
    crosstable(iris2, c(starts_with("S"), ends_with("idth"))),
    xnames=c("SL", "SW", "Sp", "PW"), byname=NULL, dim=c(15,4))
})



# Correlations ------------------------------------------------------------
test_that("crosstable with correlations", {
  expect_cross(
    crosstable(iris2, 1:3, by=Petal.Width),
    xnames=c("SL", "SW", "PL"), byname="Petal.Width", dim=c(3,4))
})


# Single function ---------------------------------------------------------
test_that("crosstable with a single function", {
  foo = as_function(~mean(.x)>3.5)
  bar = as_function(~sd(.x)>0.5 & sd(.x)<1)

  #standard functions
  expect_cross(
    crosstable(iris2_num, where(foo)),
    xnames=c("SL", "PL"), byname=NULL, dim=c(8,4))
  expect_cross(
    crosstable(iris2_num, where(bar)),
    xnames=c("SL", "PW"), byname=NULL, dim=c(8,4))

  #lambda and anonymous functions
  expect_cross(
    crosstable(iris2_num, ~mean(.x)>3.5),
    xnames=c("SL", "PL"), byname=NULL, dim=c(8,4))
  expect_cross(
    crosstable(iris2_num, function(A) mean(A)>3.5),
    xnames=c("SL", "PL"), byname=NULL, dim=c(8,4))
})



# Multiple functions ------------------------------------------------------
test_that("crosstable with multiple functions", {
  foo = as_function(~mean(.x)>3.5)
  bar = as_function(~sd(.x)>0.5 & sd(.x)<1)

  #standard functions
  expect_cross(
    crosstable(iris2_num, where(foo) & where(bar)),
    xnames=c("SL"), byname=NULL, dim=c(4,4))
  expect_cross(
    crosstable(iris2_num, where(foo) | where(bar)),
    xnames=c("SL", "PL", "PW"), byname=NULL, dim=c(12,4))
  expect_cross(
    crosstable(iris2_num, c(where(foo), where(bar))),
    xnames=c("SL", "PL", "PW"), byname=NULL, dim=c(12,4))

  #lambda and anonymous functions
  expect_cross(
    crosstable(iris2_num, c(where(~mean(.x)>3.5), where(~sd(.x)>1))),
    xnames=c("SL", "PL"), byname=NULL, dim=c(8,4))

  #complex function composition
  expect_cross(
    crosstable(iris2_num, c(where(is.numeric) & (where(foo) | where(bar)))),
    xnames=c("SL", "PL", "PW"), byname=NULL, dim=c(12,4))
  expect_cross(
    crosstable(iris2_num, c(where(is.numeric) | (where(foo) & where(bar)))),
    xnames=c("SL", "SW", "PL", "PW"), byname=NULL, dim=c(16,4))
  expect_cross(
    crosstable(iris2_num, c(where(foo) | where(bar), where(is.numeric), -Petal.Length)),
    xnames=c("SL", "PW", "SW"), byname=NULL, dim=c(12,4))
  expect_cross(
    crosstable(iris2_num, c(where(foo) | where(bar), -where(is.numeric), Petal.Length)),
    xnames=c("PL"), byname=NULL, dim=c(4,4))
})


# Formula -----------------------------------------------------------------
test_that("crosstable with formula", {

  expect_cross(
    crosstable(iris2, Sepal.Length+Sepal.Width~Species),
    xnames=c("SL", "SW"), byname="Species", dim=c(8,6))

  expect_cross(
    crosstable(iris2, Sepal.Length+I(Sepal.Width^2)+I(Sepal.Length+Sepal.Width)~Species),
    xnames=c("Sepal.Length", "I(Sepal.Width^2)", "I(Sepal.Length + Sepal.Width)"),
    byname="Species", dim=c(12,6))

  expect_cross(
    crosstable(iris2, Sepal.Length+Sepal.Width~ifelse(Species=="setosa", "foo", "bar")),
    xnames=c("SL", "SW"), dim=c(8,5),
    byname="ifelse(Species == \"setosa\", \"foo\", \"bar\")")

  expect_cross(
    crosstable(iris2, everything()),
    xnames=c("SL", "SW", "PL", "PW", "Sp"), byname=NULL, dim=c(19,4))

  #test that there is no error when formula is longer than 500 characters (caused by deparse)
  x = crosstable(iris2, I(Sepal.Length^0.1) + I(Sepal.Length^0.2) + I(Sepal.Length^0.3) +
               I(Sepal.Length^0.4) + I(Sepal.Length^0.5) + I(Sepal.Length^0.6) +
               I(Sepal.Length^0.7) + I(Sepal.Length^0.8) + I(Sepal.Length^0.9) +
               I(Sepal.Length^1.0) + I(Sepal.Length^1.1) + I(Sepal.Length^1.2) +
               I(Sepal.Length^1.3) + I(Sepal.Length^1.4) + I(Sepal.Length^1.5) +
               I(Sepal.Length^1.6) + I(Sepal.Length^1.7) + I(Sepal.Length^1.8) +
               I(Sepal.Length^1.9) + I(Sepal.Length^2.0) + I(Sepal.Length^2.1) +
               I(Sepal.Length^2.2) + I(Sepal.Length^2.3) + I(Sepal.Length^2.4) +
               I(Sepal.Length^2.5) + I(Sepal.Length^2.6) + I(Sepal.Length^2.7) +
               I(Sepal.Length^2.8) + I(Sepal.Length^2.9) + I(Sepal.Length^3.0) +
               I(Sepal.Length^3.1) + I(Sepal.Length^3.2) + I(Sepal.Length^3.3) +
               I(Sepal.Length^3.4) + I(Sepal.Length^3.5) + I(Sepal.Length^3.6) +
               I(Sepal.Length^3.7) + I(Sepal.Length^3.8) + I(Sepal.Length^3.9) +
               I(Sepal.Length^4.0) + I(Sepal.Length^4.1) + I(Sepal.Length^4.2) +
               I(Sepal.Length^4.3) + I(Sepal.Length^4.4) + I(Sepal.Length^4.5) + #nchar=1097
               I(Sepal.Length^4.6) + I(Sepal.Length^4.7) + I(Sepal.Length^4.8) +
               I(Sepal.Length^4.9) + I(Sepal.Length^5.0) ~ Species, label=F)
  expect_equal(dim(x), c(200,6))
  expect_s3_class(x, c("data.frame", "crosstable"))

  #you unfortunately cannot call an external formula
  ff = "Sepal.Length+Sepal.Width~Species"
  expect_error(crosstable(iris2, as.formula(ff), label=F))
})



# Ultimate selection (TODO) -----------------------------------------------
#TODO ultimate selection
test_that("crosstable ultimate selection", {
  expect_cross(
    crosstable(iris2, everything()),
    xnames=c("SL", "SW", "PL", "PW", "Sp"), byname=NULL, dim=c(19,4))
  expect_cross(
    crosstable(iris2, c(where(~is.numeric(.x)), where(is.double), "Species", -Sepal.Width)),
    xnames=c("SL", "PL", "PW", "Sp"), byname=NULL, dim=c(15,4))
})



# Warnings ------------------------------------------------------------------
test_that("crosstable limit tests: warnings", {
  #no selection
  expect_warning(crosstable(iris2, where(function(x) FALSE)),
                 class="crosstable_empty_warning")
  expect_warning(crosstable(iris2, 0),
                 class="crosstable_empty_warning")
  expect_warning(crosstable(iris2, 0, by="Species"),
                 class="crosstable_empty_warning")

  #removes unfit variables with a warning
  expect_warning(crosstable(iris2, c(Sepal.Length, Species), by=Petal.Width),
                 class="crosstable_wrong_col_class_by_warning")

  x = iris2 %>% mutate(xx=list(1))
  expect_warning(crosstable(x, c(xx, Species)),
                 class="crosstable_wrong_col_class_warning")
})

test_that("crosstable limit tests: deprecated features", {
  #dont use ellipsis
  lifecycle::expect_deprecated(crosstable(iris2, Sepal.Length, Sepal.Width, by=Species))

  #dont use .vars
  lifecycle::expect_defunct(crosstable(iris2, .vars=c(Sepal.Length, Sepal.Width), by=Species))
  lifecycle::expect_defunct(crosstable(iris2, Sepal.Length, .vars=c(Sepal.Length, Sepal.Width), by=Species))
})

# Errors ------------------------------------------------------------------
test_that("crosstable limit tests: errors", {

  #either formula or `by` but not both
  expect_error(crosstable(iris2, Sepal.Width~Species, by="Species"),
               class="crosstable_formula_by_error")

  #one-sided formula but not a lambda
  #in helper-crosstable.R
  expect_snapshot_error({
    A="foobar"
    crosstable(iris2, ~A, by="Species")
  })
  expect_snapshot_error({
    crosstable(iris2, ~B, by="Species")
  })

  #wrong functions (returning non-scalar)
  expect_snapshot(crosstable(iris2, ~.x, by="Species"), error = TRUE)
  expect_snapshot(crosstable(iris2, ~c(is.numeric(.x),is.numeric(.x)), by="Species"), error = TRUE)
})











