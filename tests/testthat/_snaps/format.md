# `format_fixed` works

    Code
      x = c(1, 1.2, 99.999, 152.65, pi, 1.2e-07)
      format_fixed(x, digits = 2)
    Output
      [1] "1.00"    "1.20"    "100.00"  "152.65"  "3.14"    "1.2e-07"
    Code
      format_fixed(x, digits = 3)
    Output
      [1] "1.000"    "1.200"    "99.999"   "152.650"  "3.142"    "1.20e-07"
    Code
      format_fixed(x, digits = 3, zero_digits = 2)
    Output
      [1] "1.000"    "1.200"    "99.999"   "152.650"  "3.142"    "1.20e-07"
    Code
      format_fixed(x, digits = 3, zero_digits = NULL)
    Output
      [1] "1.000"   "1.200"   "99.999"  "152.650" "3.142"   "0.000"  
    Code
      x2 = 0.008280661
      format_fixed(x2, digits = 1, zero_digits = 1)
    Output
      [1] "0.008"
    Code
      format_fixed(x2, digits = 2, zero_digits = 1)
    Output
      [1] "0.01"
    Code
      format_fixed(x2, digits = 3, zero_digits = 1)
    Output
      [1] "0.008"
    Code
      format_fixed(x2, digits = 4, zero_digits = 1)
    Output
      [1] "0.0083"
    Code
      format_fixed(x2, digits = 1, zero_digits = 2)
    Output
      [1] "0.0083"
    Code
      x3 = c(0, 0.500005, 0.1001, 0.01, 1.2e-07)
      format_fixed(x3)
    Output
      [1] "0"         "0.5"       "0.1"       "0.0100000" "1e-07"    
    Code
      format_fixed(x3, digits = 1, scientific = TRUE)
    Output
      [1] "0"     "5e-01" "1e-01" "1e-02" "1e-07"
    Code
      format_fixed(x3, digits = 4, scientific = TRUE)
    Output
      [1] "0"         "5.000e-01" "1.001e-01" "1.000e-02" "1.200e-07"
    Code
      format_fixed(x3, digits = 6, scientific = FALSE)
    Output
      [1] "0"         "0.500005"  "0.100100"  "0.010000"  "0.0000001"
    Code
      format_fixed(x3, digits = 6, scientific = FALSE, zero_digits = NULL)
    Output
      [1] "0"        "0.500005" "0.100100" "0.010000" "0.000000"
    Code
      format_fixed(x3, digits = 7, scientific = FALSE)
    Output
      [1] "0"         "0.5000050" "0.1001000" "0.0100000" "0.0000001"
    Code
      x4 = c(1e-04, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 1e+05, 5881747999)
      format_fixed(x4, digits = 1, scientific = 1)
    Output
       [1] "1e-04" "1e-03" "1e-02" "1e-01" "1.0"   "1e+01" "1e+02" "1e+03" "1e+04"
      [10] "1e+05" "6e+09"
    Code
      format_fixed(x4, digits = 1, scientific = 2)
    Output
       [1] "1e-04" "1e-03" "1e-02" "0.1"   "1.0"   "10.0"  "1e+02" "1e+03" "1e+04"
      [10] "1e+05" "6e+09"
    Code
      format_fixed(x4, digits = 1, scientific = 3)
    Output
       [1] "1e-04"  "1e-03"  "0.0100" "0.1"    "1.0"    "10.0"   "100.0"  "1e+03" 
       [9] "1e+04"  "1e+05"  "6e+09" 
    Code
      format_fixed(x4, digits = 2, scientific = 3)
    Output
       [1] "1.0e-04" "1.0e-03" "0.01"    "0.10"    "1.00"    "10.00"   "100.00" 
       [8] "1.0e+03" "1.0e+04" "1.0e+05" "5.9e+09"
    Code
      x5 = c(-1e-04, -0.01, -0.5, -1)
      format_fixed(x5, digits = 2, scientific = 2)
    Output
      [1] "-1.0e-04" "-1.0e-02" "-0.50"    "-1.00"   
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
      format_fixed(x3, digits = 0, scientific = FALSE, percent = TRUE)
    Output
      [1] "0%"       "50%"      "10%"      "1%"       "0.00001%"
    Code
      format_fixed(x3, digits = 2, scientific = FALSE, percent = TRUE)
    Output
      [1] "0%"       "50.00%"   "10.01%"   "1.00%"    "0.00001%"
    Code
      format_fixed(x3, digits = 2, scientific = FALSE, epsilon = 0.05)
    Output
      [1] "<0.05" "0.50"  "0.10"  "<0.05" "<0.05"
    Code
      minmax(mtcars$mpg / 10000, dig = 5)
    Output
      [1] "0.00104 / 0.00339"
    Code
      minmax(mtcars$mpg / 10000, dig = 1, zero_digits = 5)
    Output
      [1] "0.00104 / 0.00339"
    Code
      minmax(mtcars$mpg / 10000, dig = 2, zero_digits = NULL)
    Output
      [1] "0.00 / 0.00"
    Code
      format_fixed(c(1, 0, NA, 0 / 0, Inf))
    Output
      [1] "1.0" "0"   NA    "NaN" "Inf"
    Code
      format_fixed(NULL)
    Output
      NULL
    Code
      format_fixed(numeric(0))
    Output
      NULL
    Code
      withr::with_package("lubridate", format_fixed(lubridate::days(1:5)))
    Output
      [1] "1d 0H 0M 0S" "2d 0H 0M 0S" "3d 0H 0M 0S" "4d 0H 0M 0S" "5d 0H 0M 0S"
    Code
      withr::with_package("lubridate", format_fixed(lubridate::weeks(1:5)))
    Output
      [1] "7d 0H 0M 0S"  "14d 0H 0M 0S" "21d 0H 0M 0S" "28d 0H 0M 0S" "35d 0H 0M 0S"

# 0.5 rounding

    Code
      format_fixed(1.65)
    Output
      [1] "1.7"
    Code
      format_fixed(2.65)
    Output
      [1] "2.7"
    Code
      format_fixed(3.65)
    Output
      [1] "3.7"
    Code
      format_fixed(4.65)
    Output
      [1] "4.7"
    Code
      format_fixed(15.65)
    Output
      [1] "15.7"
    Code
      round(1.65, digits = 1)
    Output
      [1] 1.6
    Code
      round(2.65, digits = 1)
    Output
      [1] 2.6
    Code
      round(3.75, digits = 1)
    Output
      [1] 3.8
    Code
      round(4.65, digits = 1)
    Output
      [1] 4.7
    Code
      round(15.65, digits = 1)
    Output
      [1] 15.7

