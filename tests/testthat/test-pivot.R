


set.seed(1)
ct = tibble(letter=rep(letters[1:10], 3),
            x=sample(c("A", "B", "C"), 30, 1),
            y=sample(c("A", "B", "C"), 30, 1),
            z=sample(c("A", "B", "C"), 30, 1)) %>%
  mutate(across(c(x,z), ~ifelse(rbinom(n(), 1, 0.3), NA, .x))) %>%
  apply_labels(x="the X", y="the Y", z="the Z") %>%
  crosstable(-letter)




test_that("pivot", {
  expect_snapshot({
    pivot_crosstable(ct)
    pivot_crosstable(ct) %>% af(T) %>% {.$header$dataset}
    pivot_crosstable(ct) %>% af(by_header = "hyhfuyre")
  })
})
