# crosstable limit tests: deprecated features

    Code
      crosstable(mtcars2, am, cyl) %>% invisible()
    Condition
      Warning:
      The `...` argument of `crosstable()` is deprecated as of crosstable 0.2.0.
      i Please use the `cols` argument instead.
      x Bad : `crosstable(mtcars2, am, cyl, ...)`
      v Good: `crosstable(mtcars2, c(am, cyl), ...)`
    Code
      crosstable(mtcars2, c(am, cyl), hp) %>% invisible()
    Condition
      Warning:
      The `...` argument of `crosstable()` is deprecated as of crosstable 0.2.0.
      i Please use the `cols` argument instead.
      x Bad : `crosstable(mtcars2, c(am, cyl), hp, ...)`
      v Good: `crosstable(mtcars2, c(am, cyl, hp), ...)`
    Code
      crosstable(mtcars2, am, c(hp, mpg)) %>% invisible()
    Condition
      Warning:
      The `...` argument of `crosstable()` is deprecated as of crosstable 0.2.0.
      i Please use the `cols` argument instead.
      x Bad : `crosstable(mtcars2, am, c(hp, mpg), ...)`
      v Good: `crosstable(mtcars2, c(am, hp, mpg), ...)`
    Code
      crosstable(mtcars2, c(am, cyl), c(hp, mpg)) %>% invisible()
    Condition
      Warning:
      The `...` argument of `crosstable()` is deprecated as of crosstable 0.2.0.
      i Please use the `cols` argument instead.
      x Bad : `crosstable(mtcars2, c(am, cyl), c(hp, mpg), ...)`
      v Good: `crosstable(mtcars2, c(am, cyl, hp, mpg), ...)`
    Code
      crosstable(mtcars2, c(am, cyl), c(hp, mpg), c(hp, mpg)) %>% invisible()
    Condition
      Warning:
      The `...` argument of `crosstable()` is deprecated as of crosstable 0.2.0.
      i Please use the `cols` argument instead.
      x Bad : `crosstable(mtcars2, c(am, cyl), c(hp, mpg), c(hp, mpg), ...)`
      v Good: `crosstable(mtcars2, c(am, cyl, hp, mpg), ...)`

# crosstable limit tests: errors

    Predicate must return `TRUE` or `FALSE`, not a string.

---

    object 'B' not found

---

    Code
      crosstable(iris2, ~.x, by = "Species")
    Condition
      Error in `select()`:
      ! Predicate must return `TRUE` or `FALSE`, not a <labelled/numeric> object.

---

    Code
      crosstable(iris2, ~ c(is.numeric(.x), is.numeric(.x)), by = "Species")
    Condition
      Error in `select()`:
      ! Predicate must return `TRUE` or `FALSE`, not a logical vector.

