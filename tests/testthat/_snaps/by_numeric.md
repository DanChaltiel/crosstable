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

# One function

    Code
      crosstable(iris2, c(Sepal.Length), funs = "mean")
    Output
      # A tibble: 1 x 4
        .id          label           variable   value
        <chr>        <chr>           <chr>      <chr>
      1 Sepal.Length Length of Sepal "\"mean\"" 5.8  
    Code
      crosstable(iris2, c(Sepal.Length), funs = mean)
    Output
      # A tibble: 1 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal mean     5.8  
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
    Condition
      Warning:
      Anonymous functions should be named.
      i Instead of: `` funs=`function(xx){}` ``
      i Write: `` funs=c("Some calculation"=`function(xx){}` ``
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
    Condition
      Warning:
      Anonymous functions should be named.
      i Instead of: `` funs=`function(xx){}` ``
      i Write: `` funs=c("Some calculation"=`function(xx){}` ``
    Output
      # A tibble: 1 x 4
        .id          label           variable       value
        <chr>        <chr>           <chr>          <chr>
      1 Sepal.Length Length of Sepal function(xx){} 5.1  
    Code
      crosstable(iris2, c(Sepal.Length), funs = ~ mean(.x, na.rm = TRUE))
    Condition
      Warning:
      Anonymous lambda-functions should be named.
      i Instead of: `` funs=`~mean(.x, na.rm = TRUE)` ``
      i Write: `` funs=c("Some calculation"=`~mean(.x, na.rm = TRUE)` ``
    Output
      # A tibble: 1 x 4
        .id          label           variable                value
        <chr>        <chr>           <chr>                   <chr>
      1 Sepal.Length Length of Sepal ~mean(.x, na.rm = TRUE) 5.8  
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(~ mean(.x, na.rm = TRUE), ~ sd(.x,
        na.rm = TRUE)))
    Condition
      Warning:
      Anonymous lambda-functions should be named.
      i Instead of: `` funs=`~mean(.x, na.rm = TRUE)` ``
      i Write: `` funs=c("Some calculation"=`~mean(.x, na.rm = TRUE)` ``
      Warning:
      Anonymous lambda-functions should be named.
      i Instead of: `` funs=`~sd(.x, na.rm = TRUE)` ``
      i Write: `` funs=c("Some calculation"=`~sd(.x, na.rm = TRUE)` ``
    Output
      # A tibble: 2 x 4
        .id          label           variable                value
        <chr>        <chr>           <chr>                   <chr>
      1 Sepal.Length Length of Sepal ~mean(.x, na.rm = TRUE) 5.8  
      2 Sepal.Length Length of Sepal ~sd(.x, na.rm = TRUE)   0.8  
    Code
      crosstable(iris2, c(Sepal.Length), funs = c(`My mean` = mean))
    Output
      # A tibble: 1 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal My mean  5.8  
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
      crosstable(iris2, c(Sepal.Length), funs = c(first = function(xx) {
        y = 4
        xx[1]
      }))
    Output
      # A tibble: 1 x 4
        .id          label           variable value
        <chr>        <chr>           <chr>    <chr>
      1 Sepal.Length Length of Sepal first    5.1  
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

