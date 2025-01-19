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
      withr::with_package("lubridate", format_fixed(lubridate::days(1:5)))
    Output
      [1] "1d 0H 0M 0S" "2d 0H 0M 0S" "3d 0H 0M 0S" "4d 0H 0M 0S" "5d 0H 0M 0S"
    Code
      withr::with_package("lubridate", format_fixed(lubridate::weeks(1:5)))
    Output
      [1] "7d 0H 0M 0S"  "14d 0H 0M 0S" "21d 0H 0M 0S" "28d 0H 0M 0S" "35d 0H 0M 0S"

# Function arguments work

    Code
      x
    Output
          .id                 label     variable         straight           vshaped
      1  disp Displacement (cu.in.)       meansd 111.856 (58.044) 302.193 (115.524)
      2  disp Displacement (cu.in.) quantile 25%           78.700           217.900
      3  disp Displacement (cu.in.) quantile 75%          120.100           375.500
      4    hp      Gross horsepower       meansd  85.667 (23.452)  187.667 (64.556)
      5    hp      Gross horsepower quantile 25%           66.000           150.000
      6    hp      Gross horsepower quantile 75%          109.000           222.500
      7    am          Transmission         auto       2 (18.18%)        9 (81.82%)
      8    am          Transmission       manual       7 (53.85%)        6 (46.15%)
      9    am          Transmission           NA                0                 0
      10   am          Transmission        Total       9 (37.50%)       15 (62.50%)
                       NA             Total
      1  230.438 (91.499) 230.722 (123.939)
      2           162.375           120.825
      3           296.850           326.000
      4  138.500 (58.241)  146.688 (68.563)
      5           102.500            96.500
      6           176.250           180.000
      7                 8       19 (59.38%)
      8                 0       13 (40.62%)
      9                 0                 0
      10                8      32 (100.00%)

---

    Code
      ft
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA`, `Total` 
      header has 2 row(s) 
      body has 10 row(s) 
      original dataset sample: 
         .id                 label     variable         straight           vshaped
      1 disp Displacement (cu.in.)       meansd 111.856 (58.044) 302.193 (115.524)
      2 disp Displacement (cu.in.) quantile 25%           78.700           217.900
      3 disp Displacement (cu.in.) quantile 75%          120.100           375.500
      4   hp      Gross horsepower       meansd  85.667 (23.452)  187.667 (64.556)
      5   hp      Gross horsepower quantile 25%           66.000           150.000
                      NA             Total
      1 230.438 (91.499) 230.722 (123.939)
      2          162.375           120.825
      3          296.850           326.000
      4 138.500 (58.241)  146.688 (68.563)
      5          102.500            96.500

# Simple function snapshot

    Code
      crosstable(iris2, c(Sepal.Length), funs = "mean")
    Output
      # A tibble: 1 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal mean     5.8  
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(mean))
    Output
      # A tibble: 1 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal mean     5.8  
    Code
      crosstable(iris2, c(Sepal.Length), funs = mean)
    Output
      # A tibble: 1 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal mean     5.8  
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(mean, sd))
    Output
      # A tibble: 2 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal mean     5.8  
      2 Sepal.Length Length of Sepal sd       0.8  
    Code
      crosstable(iris2, c(Sepal.Length), funs = c("mean", "sd"))
    Output
      # A tibble: 2 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal mean     5.8  
      2 Sepal.Length Length of Sepal sd       0.8  
    Code
      crosstable(iris2, c(Sepal.Length), funs = cross_summary)
    Output
      # A tibble: 4 x 4
        .id          label           variable                 value        
        <chr>        <chr>           <chr>                    <chr>        
      1 Sepal.Length Length of Sepal cross_summary Min / Max  4.3 / 7.9    
      2 Sepal.Length Length of Sepal cross_summary Med [IQR]  5.8 [5.1;6.4]
      3 Sepal.Length Length of Sepal cross_summary Mean (std) 5.8 (0.8)    
      4 Sepal.Length Length of Sepal cross_summary N (NA)     150 (0)      
    Code
      crosstable(iris2, c(Sepal.Length), funs = function(xx) xx[1])
    Condition <crosstable_unnamed_anonymous_warning>
      Warning:
      Anonymous functions should be named.
      i Instead of: `funs=function(xx){}`
      i Write: `funs=c("Some calculation"=function(xx){})`
    Output
      # A tibble: 1 x 4
        .id          label           variable       value
        <chr>        <chr>           <chr>          <chr>
      1 Sepal.Length Length of Sepal function(xx){} 5.1  
    Code
      crosstable(iris2, c(Sepal.Length), funs = function(xx) {
        y = 4
        xx[1]
      })
    Condition <crosstable_unnamed_anonymous_warning>
      Warning:
      Anonymous functions should be named.
      i Instead of: `funs=function(xx){}`
      i Write: `funs=c("Some calculation"=function(xx){})`
    Output
      # A tibble: 1 x 4
        .id          label           variable       value
        <chr>        <chr>           <chr>          <chr>
      1 Sepal.Length Length of Sepal function(xx){} 5.1  
    Code
      crosstable(iris2, c(Sepal.Length), funs = ~ mean(.x, na.rm = TRUE))
    Condition <crosstable_unnamed_lambda_warning>
      Warning:
      Anonymous lambda-functions should be named.
      i Instead of: `funs=~mean(.x, na.rm = TRUE)`
      i Write: `funs=c("Some calculation"=~mean(.x, na.rm = TRUE))`
    Output
      # A tibble: 1 x 4
        .id          label           variable                value
        <chr>        <chr>           <chr>                   <chr>
      1 Sepal.Length Length of Sepal ~mean(.x, na.rm = TRUE) 5.8  
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(~ mean(.x, na.rm = TRUE), ~ sd(.x,
        na.rm = TRUE)))
    Condition <crosstable_unnamed_lambda_warning>
      Warning:
      Anonymous lambda-functions should be named.
      i Instead of: `funs=~mean(.x, na.rm = TRUE)`
      i Write: `funs=c("Some calculation"=~mean(.x, na.rm = TRUE))`
      Warning:
      Anonymous lambda-functions should be named.
      i Instead of: `funs=~sd(.x, na.rm = TRUE)`
      i Write: `funs=c("Some calculation"=~sd(.x, na.rm = TRUE))`
    Output
      # A tibble: 2 x 4
        .id          label           variable                value
        <chr>        <chr>           <chr>                   <chr>
      1 Sepal.Length Length of Sepal ~mean(.x, na.rm = TRUE) 5.8  
      2 Sepal.Length Length of Sepal ~sd(.x, na.rm = TRUE)   0.8  
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(function(.x) mean(.x, na.rm = TRUE),
      function(.x) sd(.x, na.rm = TRUE)))
    Condition <crosstable_unnamed_anonymous_warning>
      Warning:
      Anonymous functions should be named.
      i Instead of: `funs=function(.x){}`
      i Write: `funs=c("Some calculation"=function(.x){})`
      Warning:
      Anonymous functions should be named.
      i Instead of: `funs=function(.x){}`
      i Write: `funs=c("Some calculation"=function(.x){})`
    Output
      # A tibble: 2 x 4
        .id          label           variable       value
        <chr>        <chr>           <chr>          <chr>
      1 Sepal.Length Length of Sepal function(.x){} 5.8  
      2 Sepal.Length Length of Sepal function(.x){} 0.8  
    Code
      f = c(m = mean, s = sd)
      crosstable(iris2, c(Sepal.Length), funs = f)
    Output
      # A tibble: 2 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal m        5.8  
      2 Sepal.Length Length of Sepal s        0.8  
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(m = mean, s = sd))
    Output
      # A tibble: 2 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal m        5.8  
      2 Sepal.Length Length of Sepal s        0.8  
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(x = cross_summary))
    Output
      # A tibble: 4 x 4
        .id          label           variable     value        
        <chr>        <chr>           <chr>        <chr>        
      1 Sepal.Length Length of Sepal x Min / Max  4.3 / 7.9    
      2 Sepal.Length Length of Sepal x Med [IQR]  5.8 [5.1;6.4]
      3 Sepal.Length Length of Sepal x Mean (std) 5.8 (0.8)    
      4 Sepal.Length Length of Sepal x N (NA)     150 (0)      
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(` ` = cross_summary))
    Output
      # A tibble: 4 x 4
        .id          label           variable   value        
        <chr>        <chr>           <chr>      <chr>        
      1 Sepal.Length Length of Sepal Min / Max  4.3 / 7.9    
      2 Sepal.Length Length of Sepal Med [IQR]  5.8 [5.1;6.4]
      3 Sepal.Length Length of Sepal Mean (std) 5.8 (0.8)    
      4 Sepal.Length Length of Sepal N (NA)     150 (0)      
    Code
      crosstable(iris2, c(Sepal.Length), funs = list(` ` = cross_summary))
    Output
      # A tibble: 4 x 4
        .id          label           variable   value        
        <chr>        <chr>           <chr>      <chr>        
      1 Sepal.Length Length of Sepal Min / Max  4.3 / 7.9    
      2 Sepal.Length Length of Sepal Med [IQR]  5.8 [5.1;6.4]
      3 Sepal.Length Length of Sepal Mean (std) 5.8 (0.8)    
      4 Sepal.Length Length of Sepal N (NA)     150 (0)      
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(first = ~ .x[1]))
    Output
      # A tibble: 1 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal first    5.1  
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(first = function(xx) xx[1]))
    Output
      # A tibble: 1 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal first    5.1  
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(m = mean, first = function(xx) {
        y = 4
        xx[1]
      }))
    Output
      # A tibble: 2 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal m        5.8  
      2 Sepal.Length Length of Sepal first    5.1  
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(mean = ~ mean(.x, na.rm = TRUE)))
    Output
      # A tibble: 1 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal mean     5.8  
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(mean = ~ mean(.x, na.rm = TRUE),
      std = ~ sd(.x, na.rm = TRUE)))
    Output
      # A tibble: 2 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal mean     5.8  
      2 Sepal.Length Length of Sepal std      0.8  

