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

