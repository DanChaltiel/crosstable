
# colonnes test : c(am,mpg,cyl,surv)
# am = factor
# mpg = numeric
# cyl = numeric as factor
# surv = survival


# showNA ------------------------------------------------------------------

test_that("showNA with NA in by", {
  local_reproducible_output(width = 1000)
  expect_true(anyNA(mtcars3$vs))
  expect_snapshot({
    x0=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, times=c(0,100,200,400))
    as.data.frame(x0)
    x1=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, showNA="no", times=c(0,100,200,400))
    as.data.frame(x1)
    x2=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, showNA="ifany", times=c(0,100,200,400))
    as.data.frame(x2)
    x3=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, showNA="always", times=c(0,100,200,400))
    as.data.frame(x3)
  })
})

test_that("showNA without NA in by", {
  local_reproducible_output(width = 1000)
  expect_false(anyNA(mtcars3$am))
  expect_snapshot({
    x0=crosstable(mtcars3, c(vs,mpg,cyl,surv), by=am, times=c(0,100,200,400))
    as.data.frame(x0)
    x1=crosstable(mtcars3, c(vs,mpg,cyl,surv), by=am, showNA="no", times=c(0,100,200,400))
    as.data.frame(x1)
    x2=crosstable(mtcars3, c(vs,mpg,cyl,surv), by=am, showNA="ifany", times=c(0,100,200,400))
    as.data.frame(x2)
    x3=crosstable(mtcars3, c(vs,mpg,cyl,surv), by=am, showNA="always", times=c(0,100,200,400))
    as.data.frame(x3)
  })
})


# Total -------------------------------------------------------------------

test_that("total", {
  local_reproducible_output(width = 1000)
  expect_snapshot({
    x0=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, times=c(0,100,200,400))
    as.data.frame(x0)
    x1=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="none", times=c(0,100,200,400))
    as.data.frame(x1)
    x2=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="row", times=c(0,100,200,400))
    as.data.frame(x2)
    x3=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="col", times=c(0,100,200,400))
    as.data.frame(x3)
    x4=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", times=c(0,100,200,400))
    as.data.frame(x4)
  })
})


# Margins -----------------------------------------------------------------

test_that("Margins without totals", {
  local_reproducible_output(width = 1000)
  expect_snapshot({
    x0=crosstable(mtcars3, c(am, cyl), by=vs, total="none")
    as.data.frame(x0)
    x1=crosstable(mtcars3, c(am, cyl), by=vs, total="none", margin="row")
    as.data.frame(x1)
    x2=crosstable(mtcars3, c(am, cyl), by=vs, total="none", margin="col")
    as.data.frame(x2)
    x3=crosstable(mtcars3, c(am, cyl), by=vs, total="none", margin="cell")
    as.data.frame(x3)
    x4=crosstable(mtcars3, c(am, cyl), by=vs, total="none", margin="none")
    as.data.frame(x4)
    x5=crosstable(mtcars3, c(am, cyl), by=vs, total="none", margin="all")
    as.data.frame(x5)
    x6=crosstable(mtcars3, c(am, cyl), by=vs, total="none", margin=1:2)
    as.data.frame(x6)
  })
})

test_that("Margins with totals", {
  local_reproducible_output(width = 1000)
  expect_snapshot({
    x0=crosstable(mtcars3, c(am, cyl), by=vs, total="both")
    as.data.frame(x0)
    x1=crosstable(mtcars3, c(am, cyl), by=vs, total="both", margin="row")
    as.data.frame(x1)
    x2=crosstable(mtcars3, c(am, cyl), by=vs, total="both", margin="col")
    as.data.frame(x2)
    x3=crosstable(mtcars3, c(am, cyl), by=vs, total="both", margin="cell")
    as.data.frame(x3)
    x4=crosstable(mtcars3, c(am, cyl), by=vs, total="both", margin="none")
    as.data.frame(x4)
    x5=crosstable(mtcars3, c(am, cyl), by=vs, total="both", margin="all")
    as.data.frame(x5)
    x6=crosstable(mtcars3, c(am, cyl), by=vs, total="both", margin=1:2)
    as.data.frame(x6)
  })

  expect_error(crosstable(mtcars3, am, margin=c("row", "column", "cell", "none", "all")),
               class="crosstable_margin_length_3_error")
  expect_error(crosstable(mtcars3, am, margin=c("row", "foo", "bar")),
               class="crosstable_unknown_margin")
  expect_error(crosstable(mtcars3, c(am, cyl), by = vs, total = "both", margin = c("row", "none")),
               class="crosstable_incompatible_margin")
  expect_error(crosstable(mtcars3, c(am, cyl), by = vs, total = "both", margin = c("all", "none")),
               class="crosstable_incompatible_margin")
  expect_warning(crosstable(mtcars3, am, margin=c("row"), percent_pattern="N={n}"),
                 class="crosstable_margin_percent_pattern_warning")
  expect_warning(crosstable(mtcars3, am, margin=c("row", 1, "col")),
                 class="crosstable_duplicated_margin")
})


# Percent pattern ---------------------------------------------------------

test_that("Percent pattern", {
  local_reproducible_output(width = 1000)
  crosstable(mtcars3, cyl, percent_pattern="{p_col} ({n}/{n_col}) [95%CI: {p_col_inf}; {p_col_sup}]", total=TRUE)
  crosstable(mtcars3, cyl, percent_pattern="{p_col_na} ({n}/{n_col_na}) [95%CI: {p_col_na_inf}; {p_col_na_sup}]", total=TRUE)

  #TODO percent_pattern as list
  ULTIMATE_PATTERN=list(body="Cell: p = {p_tot} ({n}/{n_tot}) [95%CI {p_tot_inf}; {p_tot_sup}]
                        Col: p = {p_col} ({n}/{n_col}) [95%CI: {p_col_inf}; {p_col_sup}] \nRow:p = {p_row} ({n}/{n_row}): [95%CI: {p_row_inf}; {p_row_sup}]",
                        total_row="N={n} \nRow:p = {p_row} ({n}/{n_row}) [95%CI: {p_row_inf}; {p_row_sup}]",
                        total_col="N={n} \nCol: p = {p_col} ({n}/{n_col}) [95%CI: {p_col_inf}; {p_col_sup}]",
                        total_all="N={n} (100%) [95%CI: {p_col_inf}; {p_col_sup}]")
  ULTIMATE_PATTERN_NA=list(body="Cell: p = {p_tot_na} ({n}/{n_tot_na}) [95%CI {p_tot_na_inf}; {p_tot_na_sup}]
                        Col: p = {p_col_na} ({n}/{n_col_na}) [95%CI: {p_col_na_inf}; {p_col_na_sup}] \nRow:p = {p_row_na} ({n}/{n_row_na}): [95%CI: {p_row_na_inf}; {p_row_na_sup}]",
                        total_row="N={n} \nRow:p = {p_row_na} ({n}/{n_row_na}) [95%CI: {p_row_na_inf}; {p_row_na_sup}]",
                        total_col="N={n} \nCol: p = {p_col_na} ({n}/{n_col_na}) [95%CI: {p_col_na_inf}; {p_col_na_sup}]",
                        total_all="N={n} (100%) [95%CI: {p_col_na_inf}; {p_col_na_sup}]")

  ULTIMATE_PATTERN2=list(body="N={n} \nCol: p = {p_col} ({n}/{n_col}) [95%CI: {p_col_inf}; {p_col_sup}] \nRow:p[95%CI] = {p_row} ({n}/{n_row}): [95%CI: {p_row_inf}; {p_row_sup}]",
                         # total_row="N={n} \nRow:p = {p_row} ({n}/{n_row}) [95%CI: {p_row_inf}; {p_row_sup}]", #TODO
                         total_col="N={n} \nCol: p = {p_col} ({n}/{n_col}) [95%CI: {p_col_inf}; {p_col_sup}]",
                         total_all="N={n} (100%) {p_col_inf}")


  ULTIMATE_PATTERN=list(
    body="Cell: p = {p_tot} ({n}/{n_tot}) [95%CI {p_tot_inf}; {p_tot_sup}]
          Col: p = {p_col} ({n}/{n_col}) [95%CI: {p_col_inf}; {p_col_sup}]
          Row: p = {p_row} ({n}/{n_row}) [95%CI: {p_row_inf}; {p_row_sup}]",
    total_row="N={n} \nRow:p = {p_row} ({n}/{n_row}) [95%CI: {p_row_inf}; {p_row_sup}]",
    total_col="N={n} \nCol: p = {p_col} ({n}/{n_col}) [95%CI: {p_col_inf}; {p_col_sup}]",
    total_all="N={n} (100%) [95%CI: {p_col_inf}; {p_col_sup}]"
  )

  ULTIMATE_PATTERN=list(
    body="N={n}
          Cell: p = {p_tot} ({n}/{n_tot}) [{p_tot_inf}; {p_tot_sup}]
          Col: p = {p_col} ({n}/{n_col}) [{p_col_inf}; {p_col_sup}]
          Row: p = {p_row} ({n}/{n_row}) [{p_row_inf}; {p_row_sup}]

          Cell (NA): p = {p_tot_na} ({n}/{n_tot_na}) [{p_tot_na_inf}; {p_tot_na_sup}]
          Col (NA): p = {p_col_na} ({n}/{n_col_na}) [{p_col_na_inf}; {p_col_na_sup}]
          Row (NA): p = {p_row_na} ({n}/{n_row_na}) [{p_row_na_inf}; {p_row_na_sup}]",
    total_row="N={n}
               Row: p = {p_row} ({n}/{n_row}) [{p_row_inf}; {p_row_sup}]
               Row (NA): p = {p_row_na} ({n}/{n_row_na}) [{p_row_na_inf}; {p_row_na_sup}]",
    total_col="N={n}
               Col: p = {p_col} ({n}/{n_col}) [{p_col_inf}; {p_col_sup}]
               Col (NA): p = {p_col_na} ({n}/{n_col_na}) [{p_col_na_inf}; {p_col_na_sup}]",
    total_all="N={n}
               P: {p_col} [{p_col_inf}; {p_col_sup}]
               P (NA): {p_col} [{p_col_na_inf}; {p_col_na_sup}]"
  )





  # x1=crosstable(mtcars3, cyl, by=am,
  x1=crosstable(mtcars3, cyl, by=vs,
                percent_digits=0, total=TRUE, showNA="always",
                percent_pattern=ULTIMATE_PATTERN)
  af(x1) %>% flextable::theme_box()
  x2=crosstable(mtcars3, cyl, by=vs,
                percent_digits=0, total=TRUE, showNA="no",
                percent_pattern=ULTIMATE_PATTERN)
  af(x2) %>% flextable::theme_box()




  crosstable(mtcars2, cyl, total = "both") %>% af
  crosstable(mtcars2, cyl, by=am, total = "both") %>% af
  crosstable(mtcars2, cyl, by=am, total = "both", percent_pattern=ULTIMATE_PATTERN) %>% af
  crosstable(mtcars3, c(am, cyl), by = vs, total = "both", margin = "none")


  ULTIMATE_PATTERN="N={n}
                    Cell: p = {p_tot} ({n}/{n_tot}) [95%CI {p_tot_inf}; {p_tot_sup}]
                    Col: p = {p_col} ({n}/{n_col}) [95%CI {p_col_inf}; {p_col_sup}]
                    Row:p = {p_row} ({n}/{n_row}) [95%CI {p_row_inf}; {p_row_sup}]"
  expect_snapshot({
    #no by
    x0=crosstable(mtcars3, cyl,
                  percent_digits=0, total=TRUE, showNA="always",
                  percent_pattern=ULTIMATE_PATTERN)
    as.data.frame(x0)
    #by=am
    x1=crosstable(mtcars3, cyl, by=am,
                  percent_digits=0, total=TRUE, showNA="always",
                  percent_pattern=ULTIMATE_PATTERN)
    as.data.frame(x1)
    #multiby
    x2=crosstable(mtcars3, c(mpg, vs, cyl), by=c(am, dummy),
                  percent_digits=0, total=TRUE, showNA="always",
                  percent_pattern=ULTIMATE_PATTERN)
    as.data.frame(x2)
  })

  expect_error(crosstable(mtcars3, cyl, by=vs, percent_pattern="N={n} \nrow={p_row}, col={xxx}"),
               class="crosstable_percent_pattern_wrong_variable_error")

})


test_that("tables with lots of NA don't cause error", {
  df = tibble(
    x=sample(c("A", "B"), 30, replace=TRUE),
    y=sample(c("A", "B"), 30, replace=TRUE),
  )
  df[1:20,] = NA
  ct = crosstable(df, x, by=y, total=TRUE)
  expect_length(ct, 7)
})


# Unique numeric ----------------------------------------------------------

test_that("Unique numeric", {
  local_reproducible_output(width = 1000)
  mtcars3$gear=as.numeric(mtcars3$gear)
  mtcars3$gear[20:25] = NA

  expect_type(mtcars3$gear, "double")
  expect_type(mtcars3$carb, "double")
  expect_snapshot({
    x0=crosstable(mtcars3, gear)
    as.data.frame(x0)
    x1=crosstable(mtcars3, carb)
    as.data.frame(x1)
    x2=crosstable(mtcars3, carb, unique_numeric=9)
    as.data.frame(x2)
  })
})


# By dummy ---------------------------------------------------------

test_that("By dummy", {
  local_reproducible_output(width = 1000)
  expect_snapshot({
    x0=crosstable(mtcars3, c(am,mpg,cyl,surv), by=dummy)
    as.data.frame(x0)
    x1=crosstable(mtcars3, c(am,mpg,cyl,surv), by=dummy, showNA=TRUE)
    as.data.frame(x1)
  })
  expect_snapshot({#dummy with NA
    x2=crosstable(mtcars3, c(am,mpg,cyl,surv), by=dummy2)
    as.data.frame(x2)
    x3=crosstable(mtcars3, c(am,mpg,cyl,surv), by=dummy2, showNA=FALSE)
    as.data.frame(x3)
  })
})


# By multiple -------------------------------------------------------------

test_that("By multiple", {
  local_reproducible_output(width = 1000)
  expect_snapshot({
    x0=crosstable(mtcars3, c(mpg, gear), by=c(cyl, am, vs))
    as.data.frame(x0)
    x1=crosstable(mtcars3, c(mpg, gear), by=c(cyl, am, vs), showNA=FALSE)
    as.data.frame(x1)
    x2=crosstable(mtcars3, c(mpg, gear), by=c(cyl, am, vs), total=TRUE)
    as.data.frame(x2)
    x3=crosstable(mtcars3, c(mpg, vs, cyl), by=c(am, dummy))
    as.data.frame(x3)
    x4=crosstable(mtcars3, c(mpg, vs, cyl, dummy, surv, hp_date, qsec_posix, diff, cyl3),
                  by=c(am, gear), total=TRUE,
                  times=c(100,200), followup=TRUE)
    as.data.frame(x4)
  })
})


test_that("By multiple (formula)", {
  local_reproducible_output(width = 1000)
  expect_snapshot({
    x = crosstable(mtcars3, mpg+gear~I(am=="auto")+vs, total=TRUE)
    as.data.frame(x)
  })
  # expect_equal(dim(x1), c(8,28))
})


test_that("By multiple warnings", {
  x1 = crosstable(mtcars3, c(mpg, gear, cyl), by=c(am, diff, qsec_posix, hp_date)) %>%
    expect_warning2(class="crosstable_multiby_wrong_class_warning")
  attr(x1, "obj") %>% dim() %>% expect_equal(c(11,5))

  x2 = crosstable(mtcars3, c(mpg, gear, cyl), by=c(am, dummy_na, dummy_na2)) %>%
    expect_warning2(class="crosstable_all_na_by_warning")
  attr(x2, "obj") %>% dim() %>% expect_equal(c(11,5))

  crosstable(mtcars3, c(mpg, gear, cyl), by=c(am, vs), test=TRUE) %>%
    expect_warning(class="crosstable_multiby_test_warning")
  crosstable(mtcars3, c(mpg, gear, cyl), by=c(am, vs), effect=TRUE) %>%
    expect_warning(class="crosstable_multiby_effect_warning")
})

test_that("By multiple errors", {
  #All `by` columns have unsupported classes and were removed
  crosstable(mtcars3, c(mpg, gear, disp, carb, am),
             by=c(hp, surv, diff, qsec_posix, hp_date)) %>%
    expect_error(class="crosstable_multiby_wrong_class_error")

  #All `by` columns have missing values only
  crosstable(mtcars3, c(mpg, gear, cyl), by=c(dummy_na, dummy_na2)) %>%
    expect_error(class="crosstable_all_na_by_warning")
})


test_that("get_percent_pattern()", {
  local_reproducible_output(width = 1000)
  expect_snapshot({
    get_percent_pattern()
    get_percent_pattern(na=TRUE)

    get_percent_pattern(c("cells","row","column"))
    get_percent_pattern(c("cells","row","column"), na=TRUE)

    get_percent_pattern(margin=TRUE)
    get_percent_pattern(margin=1)
    get_percent_pattern(margin=c(1,0,2))
    get_percent_pattern(margin=1:2)
    get_percent_pattern(margin=2:1)
    get_percent_pattern(margin="row")
    get_percent_pattern(margin=c("row","cells","column"))
  })
})


test_that("get_percent_pattern(): errors & warnings", {
  get_percent_pattern(margin=c("row","rows","cells")) %>%
    expect_warning(class="crosstable_duplicated_margin")
  get_percent_pattern(margin=c("row","cells", "rows","column")) %>%
    expect_warning(class="crosstable_duplicated_margin")

  get_percent_pattern(margin=c("none","rows","cells")) %>%
    expect_error(class="crosstable_incompatible_margin")
  get_percent_pattern(margin=c("none","all")) %>%
    expect_error(class="crosstable_incompatible_margin")
  get_percent_pattern(margin=c("foobar", "rows","cells")) %>%
    expect_error(class="crosstable_unknown_margin")
})
