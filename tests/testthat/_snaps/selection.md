# crosstable limit tests: errors

    Can't coerce element 1 from a character to a logical

---

    object 'B' not found

---

    Code
      crosstable(iris2, ~.x, by = "Species")
    Condition
      Error in `select()`:
      ! Result 1 must be a single logical, not a vector of class `labelled/numeric` and of length 150

---

    Code
      crosstable(iris2, ~ c(is.numeric(.x), is.numeric(.x)), by = "Species")
    Condition
      Error in `select()`:
      ! Result 1 must be a single logical, not a logical vector of length 2

