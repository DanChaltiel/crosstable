

ct = tibble(letter=rep(letters[1:10],3), x=rnorm(30), y=rnorm(30), z=rnorm(30)) %>%
  apply_labels(x="the X", y="the Y", z="the Z") %>%
  crosstable(by=letter)

ct_c = tibble(letter=rep(letters[1:10],3),
            x=factor(rbinom(30, 1, .5), 0:1, c("A", "B")),
            y=factor(rbinom(30, 1, .5), 0:1, c("A", "B")),
            z=factor(rbinom(30, 1, .5), 0:1, c("A", "B"))) %>%
  apply_labels(x="the X", y="the Y", z="the Z") %>%
  crosstable(by=letter)

test_that("transpose works", {
  expect_snapshot({
    ct2 = t(ct)
    class(ct2)
    ct2
    af(ct2)$header$dataset
  })
})

test_that("transpose is interchangeable (numeric)", {
  ct2 = ct %>% t() %>% t()
  #by_table because of https://github.com/r-lib/waldo/issues/148
  attributes(ct2)[c("by_table", "transposed_id_labels", "inner_labels")] = NULL
  attributes(ct)[c("by_table")] = NULL
  class(ct2) = class(ct)
  expect_equal(ct, ct2, ignore_attr=FALSE)
})

test_that("transpose is interchangeable (categorical)", {
  ct2 = ct_c %>% t() %>% t()
  #by_table because of https://github.com/r-lib/waldo/issues/148
  attributes(ct2)[c("by_table", "transposed_id_labels", "inner_labels")] = NULL
  attributes(ct_c)[c("by_table")] = NULL
  class(ct2) = class(ct_c)
  expect_equal(ct_c, ct2, ignore_attr=FALSE)
})


test_that("transpose errors", {

  crosstable(mtcars2, c(mpg, drat), by=vs) %>% ct_compact() %>% t() %>%
    expect_error(class="crosstable_transpose_compact")

  crosstable(mtcars2, c(mpg, drat), by=c(vs, am)) %>% t() %>%
    expect_error(class="crosstable_transpose_multi_by")

  crosstable(mtcars2, c(mpg, am), by=c(vs)) %>% t() %>%
    expect_error(class="crosstable_transpose_multi_var")

  crosstable(mtcars2, c(mpg, drat)) %>% t() %>%
    expect_error(class="crosstable_transpose_no_by")

  tibble(letter=rep(letters[1:10],3), x=rnorm(30), y=rnorm(30), z=rnorm(30))  %>%
    apply_labels(x="the X", y="the X", z="the Z") %>%
    crosstable(by=letter) %>% t() %>%
    expect_error(class="crosstable_transpose_labels")
})
