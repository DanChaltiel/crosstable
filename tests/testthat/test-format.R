
# Format --------------------------------------------------------------------------------------

test_that("`format_fixed` works", {
  expect_snapshot({
    x = c(1, 1.2, 99.999, 152.65, pi, 0.00000012)
    format_fixed(x, digits=2)
    format_fixed(x, digits=3)
    format_fixed(x, digits=3, zero_digits=2) #default zero_digits=1
    format_fixed(x, digits=3, zero_digits=NULL)

    x2 = 0.008280661
    format_fixed(x2, digits=1, zero_digits=1) #rounded to 0 -> zero_digits
    format_fixed(x2, digits=2, zero_digits=1)
    format_fixed(x2, digits=3, zero_digits=1)
    format_fixed(x2, digits=4, zero_digits=1)
    format_fixed(x2, digits=1, zero_digits=2) #rounded to 0 -> zero_digits

    x3 = c(0, 0.500005, 0.1001, 0.01, 0.00000012)
    format_fixed(x3) #last one (<10^-4) gets scientific
    format_fixed(x3, digits=1, scientific=TRUE) #everything but 0 gets scientific
    format_fixed(x3, digits=4, scientific=TRUE)
    format_fixed(x3, digits=6, scientific=FALSE)
    format_fixed(x3, digits=6, scientific=FALSE, zero_digits=NULL)
    format_fixed(x3, digits=7, scientific=FALSE)

    x4 = c(1e-4, 1e-3, 1e-2, 1e-1, 1, 10, 100, 1000, 10000, 100000, 5881747999)
    format_fixed(x4, digits=1, scientific=1)
    format_fixed(x4, digits=1, scientific=2)
    format_fixed(x4, digits=1, scientific=3)
    format_fixed(x4, digits=2, scientific=3)

    x5 = c(-1e-4, -0.01, -0.5, -1)
    format_fixed(x5, digits=2, scientific=2)

    x_date = as.Date("1960-01-01") + c(0, 32, 400)
    format_fixed(x_date)
    format_fixed(x_date, date_format="%Y/%m/%d")

    x_posix = as.POSIXct("1960-01-01 00:00:01") + c(1, 5, 10)*1e6
    format_fixed(x_posix)
    format_fixed(x_posix, date_format="%Y/%m/%d")

    #percent & epsilon
    format_fixed(x3, digits=0, scientific=FALSE, percent=TRUE)
    format_fixed(x3, digits=2, scientific=FALSE, percent=TRUE)
    format_fixed(x3, digits=2, scientific=FALSE, epsilon=0.05)

    # passing to other funs, like minmax:
    minmax(mtcars$mpg/10000, dig=5)
    minmax(mtcars$mpg/10000, dig=1, zero_digits=5)
    minmax(mtcars$mpg/10000, dig=2, zero_digits=NULL)

    #special values
    format_fixed(c(1, 0, NA, 0/0, Inf))
    format_fixed(NULL)
    format_fixed(numeric(0))

    withr::with_package("lubridate", format_fixed(lubridate::days(1:5)))
    withr::with_package("lubridate", format_fixed(lubridate::weeks(1:5)))
  })
})


test_that("0.5 rounding", {
  expect_snapshot({

    format_fixed(1.65)
    format_fixed(2.65)
    format_fixed(3.65)
    format_fixed(4.65)
    format_fixed(15.65)

    round(1.65, digits=1)
    round(2.65, digits=1)
    round(3.75, digits=1)
    round(4.65, digits=1)
    round(15.65, digits=1)

  })
})
