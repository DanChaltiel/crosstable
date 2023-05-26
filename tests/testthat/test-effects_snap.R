
skip_on_cran()
skip_on_ci()

# skip("Too long")

# Automatic snapshot tests ------------------------------------------------
# They sometimes fail on other OS than Windows due to tiny RNG differences

can_be_by = function(x){
  length(unique(narm(x)))==2 && !all(is.na(x)) &&
    !is.numeric(x) && !is.Surv(x) && !is.date(x) && !inherits(x, "difftime")
}
# map_lgl(mtcars3, can_be_by)

for(.x in names(mtcars3)){
  set.seed(1234)
  if(can_be_by(mtcars3[[.x]])) {
    test_that(glue("Effect - .x='{.x}' - mean/OR (default)"), {
      skip_on_os(c("mac", "linux", "solaris"))
      local_reproducible_output(width = 1000)
      e_args = crosstable_effect_args()
      expect_snapshot({
        crosstable(mtcars3, -model, by=any_of(.x), effect=T, effect_args=e_args) %>%
          select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
      })
    })

    test_that(glue("Effect - .x='{.x}' - mean_boot/RR"), {
      skip_on_os(c("mac", "linux", "solaris"))
      local_reproducible_output(width = 1000)
      e_args = crosstable_effect_args()
      e_args$effect_summarize = diff_mean_boot
      e_args$effect_tabular = effect_relative_risk
      expect_snapshot({
        crosstable(mtcars3, -model, by=any_of(.x), effect=T, effect_args=e_args) %>%
          select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
      })
    })

    test_that(glue("Effect - .x='{.x}' - median/RD"), {
      skip_on_os(c("mac", "linux", "solaris"))
      local_reproducible_output(width = 1000)
      e_args = crosstable_effect_args()
      e_args$effect_summarize = diff_median_boot
      e_args$effect_tabular = effect_risk_difference
      expect_snapshot({
        crosstable(mtcars3, -model, by=any_of(.x), effect=T, effect_args=e_args) %>%
          select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
      })
    })
  }
}


# test_that("Effects never fail: mean/OR (default)", {
#   skip_on_os(c("mac", "linux", "solaris"))
#   rlang::local_options(width = 100)
#   e_args = crosstable_effect_args()
#
#   names(mtcars3) %>% set_names() %>% map(~{
#     set.seed(1234)
#     if(can_be_by(mtcars3[[.x]])) {
#       if(!is_testing()) print(glue("Effect part 1 - by={.x}"))
#       expect_snapshot({
#         print(glue("Effect part 1 - by={.x}"))
#         crosstable(mtcars3, -model, by=any_of(.x), effect=T, effect_args=e_args) %>%
#           select(.id, effect) %>% distinct() %>% as.data.frame()
#       })
#     }
#     return(0)
#   })
# })
# test_that("Effects never fail: mean_boot/RR", {
#   skip_on_os(c("mac", "linux", "solaris"))
#   rlang::local_options(width = 100)
#   e_args = crosstable_effect_args()
#   e_args$effect_summarize = diff_mean_boot
#   e_args$effect_tabular = effect_relative_risk
#
#   names(mtcars3) %>% set_names() %>% map(~{
#     set.seed(1234)
#     if(can_be_by(mtcars3[[.x]])) {
#       if(!is_testing()) print(glue("Effect part 2 - by={.x}"))
#       expect_snapshot({
#         print(glue("Effect part 2 - by={.x}"))
#         crosstable(mtcars3, -model, by=any_of(.x), effect=T, effect_args=e_args) %>%
#           select(.id, effect) %>% distinct() %>% as.data.frame()
#       })
#     }
#     return(0)
#   })
#
# })
#
# test_that("Effects never fail: median/RD", {
#   skip_on_os(c("mac", "linux", "solaris"))
#   rlang::local_options(width = 100)
#   e_args = crosstable_effect_args()
#   e_args$effect_summarize = diff_median_boot
#   e_args$effect_tabular = effect_risk_difference
#
#   map_lgl(mtcars3, can_be_by)
#
#   names(mtcars3) %>% set_names() %>% map(~{
#     if(!is_testing()) print(glue("Effect part 3 - by={.x}"))
#     if(can_be_by(mtcars3[[.x]])) {
#       expect_snapshot({
#         print(glue("Effect part 3 - by={.x}"))
#         set.seed(1234)
#         crosstable(mtcars3, -model, by=any_of(.x), effect=T, effect_args=e_args) %>%
#           select(.id, effect) %>% distinct() %>% as.data.frame()
#       })
#     }
#     return(0)
#   })
# })
#
#
