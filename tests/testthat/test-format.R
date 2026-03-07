
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

    #test for 0.5 rounding
    format_fixed(1.65)

    withr::with_package("lubridate", format_fixed(lubridate::days(1:5)))
    withr::with_package("lubridate", format_fixed(lubridate::weeks(1:5)))
  })
})
