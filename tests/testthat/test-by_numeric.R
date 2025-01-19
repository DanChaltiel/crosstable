
# numeric by numeric ------------------------------------------------------

test_that("numeric by numeric", {
  x1=crosstable(mtcars3, where(is.numeric.and.not.surv), by=disp)
  expect_equal(dim(x1), c(7,4))
  expect_equal(sum(is.na(x1)), 0)

  x2=crosstable(mtcars3, where(is.numeric.and.not.surv), by=disp, test=T)
  expect_equal(dim(x2), c(7,5))
  expect_equal(sum(is.na(x2)), 0)

  crosstable(mtcars3, carb, by=disp, cor_method = "kendall")
  crosstable(mtcars3, where(is.numeric.and.not.surv), by=disp, cor_method = "pearson")
  crosstable(mtcars3, where(is.numeric.and.not.surv), by=disp, cor_method = "kendall")
  crosstable(mtcars3, where(is.numeric.and.not.surv), by=disp, cor_method = "spearman")

  expect_warning(crosstable(mtcars2, disp, by=hp, funs=mean),
                 class="crosstable_funs_by_warning")
})




# By numeric=factor minimum of unique levels  ---------------------------------------
test_that("by factor if numeric <= 3 levels", {
  x10 = crosstable(mtcars2, cyl, by=vs, total="both", margin="all")
  x10 %>% as_flextable()
  x10 = as.data.frame(x10)

  expect_identical(x10[3,5], "14 (43.75% / 100.00% / 77.78%)")
  expect_equal(dim(x10), c(4,6))
  expect_equal(sum(is.na(x10)), 0)
})


#
# test_that("parse_funs() -> named list of functions", {
#   envir = current_env()
#   options(xxx=envir)
#   parse_funs(cross_summary) %>% assert_list(type="function", names="named", len=1)
#   parse_funs(meansd) %>% assert_list(type="function", names="named", len=1)
#
#
#   parse_funs(c(" " = cross_summary))
#   parse_funs(c(meansd))
#   parse_funs(c(meansd, mediqr))
#
#   parse_funs(~mean(.x, na.rm=TRUE))
#   parse_funs(c(
#     ~mean(.x, na.rm=TRUE),
#     ~sd(.x, na.rm=TRUE)
#   ))
#
#   #anonymous function
#   parse_funs(function(x) mean(x, na.rm=TRUE))
#   parse_funs(function(x){
#     mean(x, na.rm=TRUE)
#   })
#   parse_funs(list(
#     function(x){
#       mean(x, na.rm=TRUE)
#     },
#     function(x){
#       sd(x, na.rm=TRUE)
#     }
#   ))
#
#
#   x = crosstable(mtcars3, c(disp, hp, am), by=vs, funs=c(meanCI),
#                  funs_arg = list(level=0.99))
#   expect_equal(dim(x), c(4,6))
#   expect_equal(sum(is.na(x)), 0)
# })


# Format --------------------------------------------------------------------------------------

test_that("`format_fixed` works", {
  expect_snapshot({
    x = c(1, 1.2, 12.78749, pi, 0.00000012)
    format_fixed(x, digits=3) #default zero_digits=1
    format_fixed(x, digits=3, zero_digits=2)
    format_fixed(x, digits=3, zero_digits=NULL)

    x_sd = sd(iris$Sepal.Length/10000, na.rm=TRUE)
    format_fixed(x_sd, dig=6)
    format_fixed(x_sd, dig=3, zero_digits=2) #default only_round=FALSE
    format_fixed(x_sd, dig=3, zero_digits=2, only_round=TRUE)
    options("crosstable_only_round"=TRUE)
    format_fixed(x_sd, dig=3, zero_digits=2) #override default
    options("crosstable_only_round"=NULL)

    x2 = c(0.01, 0.1001, 0.500005, 0.00000012)
    format_fixed(x2, scientific=0, dig=1) #everything abs>10^0 gets scientific
    format_fixed(x2, scientific=FALSE, dig=6) #last would be 0 so it is scientific. Try `zero_digits=NA` or `dig=7`
    format_fixed(x2, scientific=FALSE, dig=6, zero_digits=NA)
    format_fixed(x2, scientific=FALSE, dig=7)
    format_fixed(x2, scientific=FALSE, percent=TRUE, dig=0)
    format_fixed(x2, scientific=FALSE, eps=0.05)

    x_date = as.Date("1960-01-01")+c(0,32,400)
    format_fixed(x_date)
    format_fixed(x_date, date_format="%Y/%m/%d")

    x_posix = as.POSIXct("1960-01-01 00:00:01")+c(1,5,10)*1e6
    format_fixed(x_posix)
    format_fixed(x_posix, date_format="%Y/%m/%d")

    withr::with_package("lubridate", format_fixed(lubridate::days(1:5)))
    withr::with_package("lubridate", format_fixed(lubridate::weeks(1:5)))
  })
})



# Functions ---------------------------------------------------------------
# test_that("Functions work", {
#
#   crosstable(mtcars3, c(carb, qsec_posix), funs=c(" " = cross_summary))
#   crosstable(mtcars3, c(carb, qsec_posix), funs=cross_summary)
#   crosstable(mtcars3, c(carb, qsec_posix), funs=meansd)
#   crosstable(mtcars3, c(carb, qsec_posix), funs=c(meansd))
#   crosstable(mtcars3, c(carb, qsec_posix), funs=c(mediqr, cross_summary))
#
#   #lambda
#   crosstable(mtcars3, c(carb, qsec_posix), funs=~mean(.x, na.rm=TRUE))
#   crosstable(mtcars3, c(carb, qsec_posix), funs=c(
#     ~mean(.x, na.rm=TRUE),
#     ~sd(.x, na.rm=TRUE)
#   ))
#
#   #anonymous function
#   crosstable(mtcars3, c(carb, qsec_posix), funs=function(x) mean(x, na.rm=TRUE))
#   crosstable(mtcars3, c(carb, qsec_posix), funs=c(function(x){
#     mean(x, na.rm=TRUE)
#   }))
#   crosstable(mtcars3, c(carb, qsec_posix), funs=function(x){
#     mean(x, na.rm=TRUE)
#   })
#   crosstable(mtcars3, c(carb, qsec_posix), funs=list(
#     function(x){
#       mean(x, na.rm=TRUE)
#     },
#     function(x){
#       sd(x, na.rm=TRUE)
#     }
#   ))
#
#
#   x = crosstable(mtcars3, c(disp, hp, am), by=vs, funs=c(meanCI),
#                  funs_arg = list(level=0.99))
#   expect_equal(dim(x), c(4,6))
#   expect_equal(sum(is.na(x)), 0)
# })

test_that("Function arguments work", {
  x = crosstable(mtcars3, c(disp, hp, am), by=vs, funs=c(meansd, quantile),
                 funs_arg = list(dig=3, probs=c(0.25,0.75)),
                 total=T, showNA="always")
  ft = as_flextable(x)
  x = as.data.frame(x)
  expect_snapshot(x)
  expect_snapshot(ft)
})

test_that("Simple function snapshot", {
  expect_snapshot(cnd_class=TRUE, {
    #unnamed ok
    crosstable(iris2, c(Sepal.Length), funs="mean")
    crosstable(iris2, c(Sepal.Length), funs=c(mean))
    crosstable(iris2, c(Sepal.Length), funs=mean)
    crosstable(iris2, c(Sepal.Length), funs=c(mean, sd))
    crosstable(iris2, c(Sepal.Length), funs=c("mean", "sd"))
    crosstable(iris2, c(Sepal.Length), funs=cross_summary)

    #unnamed warn
    crosstable(iris2, c(Sepal.Length), funs=function(xx) xx[1])
    crosstable(iris2, c(Sepal.Length), funs=function(xx){
      y=4
      xx[1]
    })
    crosstable(iris2, c(Sepal.Length), funs=~mean(.x, na.rm=TRUE))
    crosstable(iris2, c(Sepal.Length), funs=c(
      ~mean(.x, na.rm=TRUE),
      ~sd(.x, na.rm=TRUE)
    ))
    crosstable(iris2, c(Sepal.Length), funs=c(
      function(.x) mean(.x, na.rm=TRUE),
      function(.x) sd(.x, na.rm=TRUE)
    ))

    #named ok
    f = c("m"=mean, s=sd)
    crosstable(iris2, c(Sepal.Length), funs=f)
    crosstable(iris2, c(Sepal.Length), funs=c("m"=mean, s=sd))
    crosstable(iris2, c(Sepal.Length), funs=c("x" = cross_summary))
    crosstable(iris2, c(Sepal.Length), funs=c(" " = cross_summary))
    crosstable(iris2, c(Sepal.Length), funs=list(" " = cross_summary))
    crosstable(iris2, c(Sepal.Length), funs=c("first"=~.x[1]))
    crosstable(iris2, c(Sepal.Length), funs=c("first"=function(xx) xx[1]))
    crosstable(iris2, c(Sepal.Length), funs=c("m"=mean, "first"=function(xx){
      y=4
      xx[1]
    }))
    crosstable(iris2, c(Sepal.Length), funs=c(mean=~mean(.x, na.rm=TRUE)))
    crosstable(iris2, c(Sepal.Length), funs=c(
      mean=~mean(.x, na.rm=TRUE),
      std=~sd(.x, na.rm=TRUE)
    ))
  })


  crosstable(iris2, c(Sepal.Length), funs=NULL) %>%
    expect_error(class="ct_funs_null")

  crosstable(iris2, c(Sepal.Length), funs=c("m"=mean, sd)) %>%
    expect_error(class="ct_funs_unnamed")

  # expect_warning(crosstable(iris2, c(Sepal.Length, Sepal.Width),
  #                           funs=function(y) mean(y, na.rm=TRUE)),
  #                class="crosstable_unnamed_anonymous_warning")
  # expect_warning(crosstable(iris2, c(Sepal.Length, Sepal.Width),
  #                           funs=~mean(.x, na.rm=TRUE)),
  #                class="crosstable_unnamed_lambda_warning")

})


test_that("Complex multiple functions", {
  #avec tous les noms quand il en faut
  x2 = crosstable(iris2, c(Sepal.Length, Sepal.Width),
                  funs=c(
                    "moy_lambda"=~mean(.x, na.rm=TRUE),
                    "moy_fn"=function(.x) mean(.x, na.rm=TRUE),
                    "cross_summary1" = cross_summary
                  ))

  expect_setequal(x2$variable,
                  c("moy_lambda", "moy_fn",
                    paste("cross_summary1", names(cross_summary(1)))) )

  #sans noms
  x4 = crosstable(iris2, c(Sepal.Length, Sepal.Width),
                  funs=c(
                    ~mean(.x, na.rm=TRUE),
                    function(.x){mean(.x, na.rm=TRUE)},
                    var,
                    mean
                  )) %>%
    expect_warning2(class="crosstable_unnamed_anonymous_warning") %>%
    expect_warning2(class="crosstable_unnamed_lambda_warning")

  expect_setequal(attr(x4, "obj")$variable,
                  c("~mean(.x, na.rm = TRUE)",
                    "function(.x){}",
                    "var", "mean"))
})


test_that("Special summary functions", {
  #date
  ct = crosstable(mtcars2, hp_date, date_format="%d/%m/%Y") %>% as.data.frame()
  expect_equal(ct[1,4], "22/02/2010 - 02/12/2010")

  #only_round
  x = mtcars2 %>% dplyr::transmute(mpg=mpg/100000)
  rlang::local_options(crosstable_only_round=NULL)
  ct = crosstable(x, funs_arg=list(dig=2, zero_digits=5)) %>% as.data.frame()
  expect_equal(ct[1,4], "0.000104 / 0.000339")
  rlang::local_options(crosstable_only_round=TRUE)
  ct = crosstable(x, funs_arg=list(dig=2, zero_digits=5)) %>% as.data.frame()
  expect_equal(ct[1,4], "0 / 0")
})

print(("days"))
print(exists("days"))

