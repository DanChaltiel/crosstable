test_that("is there a problem?", {

  expect_snapshot({
    a = mtcars %>% filter(vs==0) %>% pull(mpg)
    m = median(a)
    m
    format_fixed(m)
  })


})
