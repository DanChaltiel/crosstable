# `format_fixed` works

    Code
      x = c(1, 1.2, 12.78749, pi, 1.2e-07)
      format_fixed(x, digits = 3)
    Output
      [1] "1.000e+00" "1.200e+00" "1.279e+01" "3.142e+00" "1.200e-07"
    Code
      format_fixed(x, digits = 3, zero_digits = 2)
    Output
      [1] "1.000e+00" "1.200e+00" "1.279e+01" "3.142e+00" "1.200e-07"
    Code
      format_fixed(x, digits = 3, zero_digits = NULL)
    Output
      [1] "1.000e+00" "1.200e+00" "1.279e+01" "3.142e+00" "1.200e-07"
    Code
      x_sd = sd(iris$Sepal.Length / 10000, na.rm = TRUE)
      format_fixed(x_sd, dig = 6)
    Output
      [1] "8.280661e-05"
    Code
      format_fixed(x_sd, dig = 3, zero_digits = 2)
    Output
      [1] "8.281e-05"
    Code
      format_fixed(x_sd, dig = 3, zero_digits = 2, only_round = TRUE)
    Output
      [1] "8.281e-05"
    Code
      options(crosstable_only_round = TRUE)
      format_fixed(x_sd, dig = 3, zero_digits = 2)
    Output
      [1] "8.281e-05"
    Code
      options(crosstable_only_round = NULL)
      x2 = c(0.01, 0.1001, 0.500005, 1.2e-07)
      format_fixed(x2, scientific = 0, dig = 1)
    Output
      [1] "1.0e-02" "1.0e-01" "5.0e-01" "1.2e-07"
    Code
      format_fixed(x2, scientific = FALSE, dig = 6)
    Output
      [1] "0.010000" "0.100100" "0.500005" "1e-07"   
    Code
      format_fixed(x2, scientific = FALSE, dig = 6, zero_digits = NA)
    Output
      [1] "0.010000" "0.100100" "0.500005" "0.000000"
    Code
      format_fixed(x2, scientific = FALSE, dig = 7)
    Output
      [1] "0.0100000" "0.1001000" "0.5000050" "0.0000001"
    Code
      format_fixed(x2, scientific = FALSE, percent = TRUE, dig = 0)
    Output
      [1] "1%"     "10%"    "50%"    "1e-05%"
    Code
      format_fixed(x2, scientific = FALSE, eps = 0.05)
    Output
      [1] "<0.05" "0.1"   "0.5"   "<0.05"
    Code
      x_date = as.Date("1960-01-01") + c(0, 32, 400)
      format_fixed(x_date)
    Output
      [1] "1960-01-01" "1960-02-02" "1961-02-04"
    Code
      format_fixed(x_date, date_format = "%Y/%m/%d")
    Output
      [1] "1960/01/01" "1960/02/02" "1961/02/04"
    Code
      x_posix = as.POSIXct("1960-01-01 00:00:01") + c(1, 5, 10) * 1e+06
      format_fixed(x_posix)
    Output
      [1] "1960-01-12 13:46:41" "1960-02-27 20:53:21" "1960-04-25 17:46:41"
    Code
      format_fixed(x_posix, date_format = "%Y/%m/%d")
    Output
      [1] "1960/01/12" "1960/02/27" "1960/04/25"
    Code
      format_fixed(1.65)
    Output
      [1] "1.6"
    Code
      withr::with_package("lubridate", format_fixed(lubridate::days(1:5)))
    Output
      [1] "1d 0H 0M 0S" "2d 0H 0M 0S" "3d 0H 0M 0S" "4d 0H 0M 0S" "5d 0H 0M 0S"
    Code
      withr::with_package("lubridate", format_fixed(lubridate::weeks(1:5)))
    Output
      [1] "7d 0H 0M 0S"  "14d 0H 0M 0S" "21d 0H 0M 0S" "28d 0H 0M 0S" "35d 0H 0M 0S"

