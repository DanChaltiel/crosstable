# showNA with NA in by

    Code
      x0 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, times = c(0, 100, 200,
        400))
      x0
    Output
      # A tibble: 15 x 6
         .id   label                    variable        straight    vshaped   `NA`    
         <chr> <chr>                    <chr>           <chr>       <chr>     <chr>   
       1 am    Transmission             auto            2 (18.18%)  9 (81.82~ 8       
       2 am    Transmission             manual          7 (53.85%)  6 (46.15~ 0       
       3 mpg   Miles/(US) gallon        Min / Max       21.4 / 33.9 10.4 / 2~ 14.3 / ~
       4 mpg   Miles/(US) gallon        Med [IQR]       27.3 [21.5~ 15.5 [14~ 18.4 [1~
       5 mpg   Miles/(US) gallon        Mean (std)      26.8 (5.1)  16.6 (4.~ 19.0 (3~
       6 mpg   Miles/(US) gallon        N (NA)          9 (0)       15 (0)    8 (0)   
       7 cyl   Number of cylinders      4               7 (87.50%)  1 (12.50~ 2       
       8 cyl   Number of cylinders      6               0 (0%)      1 (100.0~ 3       
       9 cyl   Number of cylinders      8               0 (0%)      11 (100.~ 2       
      10 cyl   Number of cylinders      NA              2           2         1       
      11 surv  Dummy survival (disp/am) t=0             1.00 (0/9)  1.00 (0/~ 1.00 (0~
      12 surv  Dummy survival (disp/am) t=100           0.44 (5/4)  1.00 (0/~ 1.00 (0~
      13 surv  Dummy survival (disp/am) t=200           0.17 (2/1)  0.73 (4/~ 1.00 (0~
      14 surv  Dummy survival (disp/am) t=400           0.17 (0/0)  0.52 (2/~ 1.00 (0~
      15 surv  Dummy survival (disp/am) Median survival 95.1        <NA>      <NA>    
    Code
      as_flextable(x0)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA` 
      header has 2 row(s) 
      body has 15 row(s) 
      original dataset sample: 
        .id             label   variable         straight          vshaped
      1  am      Transmission       auto       2 (18.18%)       9 (81.82%)
      2  am      Transmission     manual       7 (53.85%)       6 (46.15%)
      3 mpg Miles/(US) gallon  Min / Max      21.4 / 33.9      10.4 / 26.0
      4 mpg Miles/(US) gallon  Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4]
      5 mpg Miles/(US) gallon Mean (std)       26.8 (5.1)       16.6 (4.2)
                      NA
      1                8
      2                0
      3      14.3 / 24.4
      4 18.4 [17.5;20.1]
      5       19.0 (3.3)
    Code
      x1 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, showNA = "no", times = c(
        0, 100, 200, 400))
      x1
    Output
      # A tibble: 14 x 5
         .id   label                    variable        straight         vshaped      
         <chr> <chr>                    <chr>           <chr>            <chr>        
       1 am    Transmission             auto            2 (18.18%)       9 (81.82%)   
       2 am    Transmission             manual          7 (53.85%)       6 (46.15%)   
       3 mpg   Miles/(US) gallon        Min / Max       21.4 / 33.9      10.4 / 26.0  
       4 mpg   Miles/(US) gallon        Med [IQR]       27.3 [21.5;30.4] 15.5 [14.8;1~
       5 mpg   Miles/(US) gallon        Mean (std)      26.8 (5.1)       16.6 (4.2)   
       6 mpg   Miles/(US) gallon        N (NA)          9 (0)            15 (0)       
       7 cyl   Number of cylinders      4               7 (87.50%)       1 (12.50%)   
       8 cyl   Number of cylinders      6               0 (0%)           1 (100.00%)  
       9 cyl   Number of cylinders      8               0 (0%)           11 (100.00%) 
      10 surv  Dummy survival (disp/am) t=0             1.00 (0/9)       1.00 (0/15)  
      11 surv  Dummy survival (disp/am) t=100           0.44 (5/4)       1.00 (0/15)  
      12 surv  Dummy survival (disp/am) t=200           0.17 (2/1)       0.73 (4/11)  
      13 surv  Dummy survival (disp/am) t=400           0.17 (0/0)       0.52 (2/4)   
      14 surv  Dummy survival (disp/am) Median survival 95.1             <NA>         
    Code
      as_flextable(x1)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped` 
      header has 2 row(s) 
      body has 14 row(s) 
      original dataset sample: 
        .id             label   variable         straight          vshaped
      1  am      Transmission       auto       2 (18.18%)       9 (81.82%)
      2  am      Transmission     manual       7 (53.85%)       6 (46.15%)
      3 mpg Miles/(US) gallon  Min / Max      21.4 / 33.9      10.4 / 26.0
      4 mpg Miles/(US) gallon  Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4]
      5 mpg Miles/(US) gallon Mean (std)       26.8 (5.1)       16.6 (4.2)
    Code
      x2 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, showNA = "ifany",
      times = c(0, 100, 200, 400))
      x2
    Output
      # A tibble: 15 x 6
         .id   label                    variable        straight    vshaped   `NA`    
         <chr> <chr>                    <chr>           <chr>       <chr>     <chr>   
       1 am    Transmission             auto            2 (18.18%)  9 (81.82~ 8       
       2 am    Transmission             manual          7 (53.85%)  6 (46.15~ 0       
       3 mpg   Miles/(US) gallon        Min / Max       21.4 / 33.9 10.4 / 2~ 14.3 / ~
       4 mpg   Miles/(US) gallon        Med [IQR]       27.3 [21.5~ 15.5 [14~ 18.4 [1~
       5 mpg   Miles/(US) gallon        Mean (std)      26.8 (5.1)  16.6 (4.~ 19.0 (3~
       6 mpg   Miles/(US) gallon        N (NA)          9 (0)       15 (0)    8 (0)   
       7 cyl   Number of cylinders      4               7 (87.50%)  1 (12.50~ 2       
       8 cyl   Number of cylinders      6               0 (0%)      1 (100.0~ 3       
       9 cyl   Number of cylinders      8               0 (0%)      11 (100.~ 2       
      10 cyl   Number of cylinders      NA              2           2         1       
      11 surv  Dummy survival (disp/am) t=0             1.00 (0/9)  1.00 (0/~ 1.00 (0~
      12 surv  Dummy survival (disp/am) t=100           0.44 (5/4)  1.00 (0/~ 1.00 (0~
      13 surv  Dummy survival (disp/am) t=200           0.17 (2/1)  0.73 (4/~ 1.00 (0~
      14 surv  Dummy survival (disp/am) t=400           0.17 (0/0)  0.52 (2/~ 1.00 (0~
      15 surv  Dummy survival (disp/am) Median survival 95.1        <NA>      <NA>    
    Code
      as_flextable(x2)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA` 
      header has 2 row(s) 
      body has 15 row(s) 
      original dataset sample: 
        .id             label   variable         straight          vshaped
      1  am      Transmission       auto       2 (18.18%)       9 (81.82%)
      2  am      Transmission     manual       7 (53.85%)       6 (46.15%)
      3 mpg Miles/(US) gallon  Min / Max      21.4 / 33.9      10.4 / 26.0
      4 mpg Miles/(US) gallon  Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4]
      5 mpg Miles/(US) gallon Mean (std)       26.8 (5.1)       16.6 (4.2)
                      NA
      1                8
      2                0
      3      14.3 / 24.4
      4 18.4 [17.5;20.1]
      5       19.0 (3.3)
    Code
      x3 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, showNA = "always",
      times = c(0, 100, 200, 400))
      x3
    Output
      # A tibble: 16 x 6
         .id   label                    variable        straight    vshaped   `NA`    
         <chr> <chr>                    <chr>           <chr>       <chr>     <chr>   
       1 am    Transmission             auto            2 (18.18%)  9 (81.82~ 8       
       2 am    Transmission             manual          7 (53.85%)  6 (46.15~ 0       
       3 am    Transmission             NA              0           0         0       
       4 mpg   Miles/(US) gallon        Min / Max       21.4 / 33.9 10.4 / 2~ 14.3 / ~
       5 mpg   Miles/(US) gallon        Med [IQR]       27.3 [21.5~ 15.5 [14~ 18.4 [1~
       6 mpg   Miles/(US) gallon        Mean (std)      26.8 (5.1)  16.6 (4.~ 19.0 (3~
       7 mpg   Miles/(US) gallon        N (NA)          9 (0)       15 (0)    8 (0)   
       8 cyl   Number of cylinders      4               7 (87.50%)  1 (12.50~ 2       
       9 cyl   Number of cylinders      6               0 (0%)      1 (100.0~ 3       
      10 cyl   Number of cylinders      8               0 (0%)      11 (100.~ 2       
      11 cyl   Number of cylinders      NA              2           2         1       
      12 surv  Dummy survival (disp/am) t=0             1.00 (0/9)  1.00 (0/~ 1.00 (0~
      13 surv  Dummy survival (disp/am) t=100           0.44 (5/4)  1.00 (0/~ 1.00 (0~
      14 surv  Dummy survival (disp/am) t=200           0.17 (2/1)  0.73 (4/~ 1.00 (0~
      15 surv  Dummy survival (disp/am) t=400           0.17 (0/0)  0.52 (2/~ 1.00 (0~
      16 surv  Dummy survival (disp/am) Median survival 95.1        <NA>      <NA>    
    Code
      as_flextable(x3)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA` 
      header has 2 row(s) 
      body has 16 row(s) 
      original dataset sample: 
        .id             label  variable         straight          vshaped
      1  am      Transmission      auto       2 (18.18%)       9 (81.82%)
      2  am      Transmission    manual       7 (53.85%)       6 (46.15%)
      3  am      Transmission        NA                0                0
      4 mpg Miles/(US) gallon Min / Max      21.4 / 33.9      10.4 / 26.0
      5 mpg Miles/(US) gallon Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4]
                      NA
      1                8
      2                0
      3                0
      4      14.3 / 24.4
      5 18.4 [17.5;20.1]

# showNA without NA in by

    Code
      x0 = crosstable(mtcars3, c(vs, mpg, cyl, surv), by = am, times = c(0, 100, 200,
        400))
      x0
    Output
      # A tibble: 16 x 5
         .id   label                    variable        auto             manual       
         <chr> <chr>                    <chr>           <chr>            <chr>        
       1 vs    Engine                   straight        2 (22.22%)       7 (77.78%)   
       2 vs    Engine                   vshaped         9 (60.00%)       6 (40.00%)   
       3 vs    Engine                   NA              8                0            
       4 mpg   Miles/(US) gallon        Min / Max       10.4 / 24.4      15.0 / 33.9  
       5 mpg   Miles/(US) gallon        Med [IQR]       17.3 [14.9;19.2] 22.8 [21.0;3~
       6 mpg   Miles/(US) gallon        Mean (std)      17.1 (3.8)       24.4 (6.2)   
       7 mpg   Miles/(US) gallon        N (NA)          19 (0)           13 (0)       
       8 cyl   Number of cylinders      4               3 (30.00%)       7 (70.00%)   
       9 cyl   Number of cylinders      6               3 (75.00%)       1 (25.00%)   
      10 cyl   Number of cylinders      8               11 (84.62%)      2 (15.38%)   
      11 cyl   Number of cylinders      NA              2                3            
      12 surv  Dummy survival (disp/am) t=0             1.00 (0/19)      1.00 (0/13)  
      13 surv  Dummy survival (disp/am) t=100           1.00 (0/19)      0.62 (5/8)   
      14 surv  Dummy survival (disp/am) t=200           1.00 (0/14)      0.15 (6/2)   
      15 surv  Dummy survival (disp/am) t=400           1.00 (0/4)       0 (2/0)      
      16 surv  Dummy survival (disp/am) Median survival <NA>             120.3        
    Code
      as_flextable(x0)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `auto`, `manual` 
      header has 2 row(s) 
      body has 16 row(s) 
      original dataset sample: 
        .id             label  variable             auto           manual
      1  vs            Engine  straight       2 (22.22%)       7 (77.78%)
      2  vs            Engine   vshaped       9 (60.00%)       6 (40.00%)
      3  vs            Engine        NA                8                0
      4 mpg Miles/(US) gallon Min / Max      10.4 / 24.4      15.0 / 33.9
      5 mpg Miles/(US) gallon Med [IQR] 17.3 [14.9;19.2] 22.8 [21.0;30.4]
    Code
      x1 = crosstable(mtcars3, c(vs, mpg, cyl, surv), by = am, showNA = "no", times = c(
        0, 100, 200, 400))
      x1
    Output
      # A tibble: 14 x 5
         .id   label                    variable        auto             manual       
         <chr> <chr>                    <chr>           <chr>            <chr>        
       1 vs    Engine                   straight        2 (22.22%)       7 (77.78%)   
       2 vs    Engine                   vshaped         9 (60.00%)       6 (40.00%)   
       3 mpg   Miles/(US) gallon        Min / Max       10.4 / 24.4      15.0 / 33.9  
       4 mpg   Miles/(US) gallon        Med [IQR]       17.3 [14.9;19.2] 22.8 [21.0;3~
       5 mpg   Miles/(US) gallon        Mean (std)      17.1 (3.8)       24.4 (6.2)   
       6 mpg   Miles/(US) gallon        N (NA)          19 (0)           13 (0)       
       7 cyl   Number of cylinders      4               3 (30.00%)       7 (70.00%)   
       8 cyl   Number of cylinders      6               3 (75.00%)       1 (25.00%)   
       9 cyl   Number of cylinders      8               11 (84.62%)      2 (15.38%)   
      10 surv  Dummy survival (disp/am) t=0             1.00 (0/19)      1.00 (0/13)  
      11 surv  Dummy survival (disp/am) t=100           1.00 (0/19)      0.62 (5/8)   
      12 surv  Dummy survival (disp/am) t=200           1.00 (0/14)      0.15 (6/2)   
      13 surv  Dummy survival (disp/am) t=400           1.00 (0/4)       0 (2/0)      
      14 surv  Dummy survival (disp/am) Median survival <NA>             120.3        
    Code
      as_flextable(x1)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `auto`, `manual` 
      header has 2 row(s) 
      body has 14 row(s) 
      original dataset sample: 
        .id             label   variable             auto           manual
      1  vs            Engine   straight       2 (22.22%)       7 (77.78%)
      2  vs            Engine    vshaped       9 (60.00%)       6 (40.00%)
      3 mpg Miles/(US) gallon  Min / Max      10.4 / 24.4      15.0 / 33.9
      4 mpg Miles/(US) gallon  Med [IQR] 17.3 [14.9;19.2] 22.8 [21.0;30.4]
      5 mpg Miles/(US) gallon Mean (std)       17.1 (3.8)       24.4 (6.2)
    Code
      x2 = crosstable(mtcars3, c(vs, mpg, cyl, surv), by = am, showNA = "ifany",
      times = c(0, 100, 200, 400))
      x2
    Output
      # A tibble: 16 x 5
         .id   label                    variable        auto             manual       
         <chr> <chr>                    <chr>           <chr>            <chr>        
       1 vs    Engine                   straight        2 (22.22%)       7 (77.78%)   
       2 vs    Engine                   vshaped         9 (60.00%)       6 (40.00%)   
       3 vs    Engine                   NA              8                0            
       4 mpg   Miles/(US) gallon        Min / Max       10.4 / 24.4      15.0 / 33.9  
       5 mpg   Miles/(US) gallon        Med [IQR]       17.3 [14.9;19.2] 22.8 [21.0;3~
       6 mpg   Miles/(US) gallon        Mean (std)      17.1 (3.8)       24.4 (6.2)   
       7 mpg   Miles/(US) gallon        N (NA)          19 (0)           13 (0)       
       8 cyl   Number of cylinders      4               3 (30.00%)       7 (70.00%)   
       9 cyl   Number of cylinders      6               3 (75.00%)       1 (25.00%)   
      10 cyl   Number of cylinders      8               11 (84.62%)      2 (15.38%)   
      11 cyl   Number of cylinders      NA              2                3            
      12 surv  Dummy survival (disp/am) t=0             1.00 (0/19)      1.00 (0/13)  
      13 surv  Dummy survival (disp/am) t=100           1.00 (0/19)      0.62 (5/8)   
      14 surv  Dummy survival (disp/am) t=200           1.00 (0/14)      0.15 (6/2)   
      15 surv  Dummy survival (disp/am) t=400           1.00 (0/4)       0 (2/0)      
      16 surv  Dummy survival (disp/am) Median survival <NA>             120.3        
    Code
      as_flextable(x2)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `auto`, `manual` 
      header has 2 row(s) 
      body has 16 row(s) 
      original dataset sample: 
        .id             label  variable             auto           manual
      1  vs            Engine  straight       2 (22.22%)       7 (77.78%)
      2  vs            Engine   vshaped       9 (60.00%)       6 (40.00%)
      3  vs            Engine        NA                8                0
      4 mpg Miles/(US) gallon Min / Max      10.4 / 24.4      15.0 / 33.9
      5 mpg Miles/(US) gallon Med [IQR] 17.3 [14.9;19.2] 22.8 [21.0;30.4]
    Code
      x3 = crosstable(mtcars3, c(vs, mpg, cyl, surv), by = am, showNA = "always",
      times = c(0, 100, 200, 400))
      x3
    Output
      # A tibble: 16 x 6
         .id   label                    variable        auto             manual  `NA` 
         <chr> <chr>                    <chr>           <chr>            <chr>   <chr>
       1 vs    Engine                   straight        2 (22.22%)       7 (77.~ 0    
       2 vs    Engine                   vshaped         9 (60.00%)       6 (40.~ 0    
       3 vs    Engine                   NA              8                0       0    
       4 mpg   Miles/(US) gallon        Min / Max       10.4 / 24.4      15.0 /~ no NA
       5 mpg   Miles/(US) gallon        Med [IQR]       17.3 [14.9;19.2] 22.8 [~ no NA
       6 mpg   Miles/(US) gallon        Mean (std)      17.1 (3.8)       24.4 (~ no NA
       7 mpg   Miles/(US) gallon        N (NA)          19 (0)           13 (0)  no NA
       8 cyl   Number of cylinders      4               3 (30.00%)       7 (70.~ 0    
       9 cyl   Number of cylinders      6               3 (75.00%)       1 (25.~ 0    
      10 cyl   Number of cylinders      8               11 (84.62%)      2 (15.~ 0    
      11 cyl   Number of cylinders      NA              2                3       0    
      12 surv  Dummy survival (disp/am) t=0             1.00 (0/19)      1.00 (~ <NA> 
      13 surv  Dummy survival (disp/am) t=100           1.00 (0/19)      0.62 (~ <NA> 
      14 surv  Dummy survival (disp/am) t=200           1.00 (0/14)      0.15 (~ <NA> 
      15 surv  Dummy survival (disp/am) t=400           1.00 (0/4)       0 (2/0) <NA> 
      16 surv  Dummy survival (disp/am) Median survival <NA>             120.3   <NA> 
    Code
      as_flextable(x3)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `auto`, `manual`, `NA` 
      header has 2 row(s) 
      body has 16 row(s) 
      original dataset sample: 
        .id             label  variable             auto           manual    NA
      1  vs            Engine  straight       2 (22.22%)       7 (77.78%)     0
      2  vs            Engine   vshaped       9 (60.00%)       6 (40.00%)     0
      3  vs            Engine        NA                8                0     0
      4 mpg Miles/(US) gallon Min / Max      10.4 / 24.4      15.0 / 33.9 no NA
      5 mpg Miles/(US) gallon Med [IQR] 17.3 [14.9;19.2] 22.8 [21.0;30.4] no NA

# total

    Code
      x0 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, times = c(0, 100, 200,
        400))
      x0
    Output
      # A tibble: 15 x 6
         .id   label                    variable        straight    vshaped   `NA`    
         <chr> <chr>                    <chr>           <chr>       <chr>     <chr>   
       1 am    Transmission             auto            2 (18.18%)  9 (81.82~ 8       
       2 am    Transmission             manual          7 (53.85%)  6 (46.15~ 0       
       3 mpg   Miles/(US) gallon        Min / Max       21.4 / 33.9 10.4 / 2~ 14.3 / ~
       4 mpg   Miles/(US) gallon        Med [IQR]       27.3 [21.5~ 15.5 [14~ 18.4 [1~
       5 mpg   Miles/(US) gallon        Mean (std)      26.8 (5.1)  16.6 (4.~ 19.0 (3~
       6 mpg   Miles/(US) gallon        N (NA)          9 (0)       15 (0)    8 (0)   
       7 cyl   Number of cylinders      4               7 (87.50%)  1 (12.50~ 2       
       8 cyl   Number of cylinders      6               0 (0%)      1 (100.0~ 3       
       9 cyl   Number of cylinders      8               0 (0%)      11 (100.~ 2       
      10 cyl   Number of cylinders      NA              2           2         1       
      11 surv  Dummy survival (disp/am) t=0             1.00 (0/9)  1.00 (0/~ 1.00 (0~
      12 surv  Dummy survival (disp/am) t=100           0.44 (5/4)  1.00 (0/~ 1.00 (0~
      13 surv  Dummy survival (disp/am) t=200           0.17 (2/1)  0.73 (4/~ 1.00 (0~
      14 surv  Dummy survival (disp/am) t=400           0.17 (0/0)  0.52 (2/~ 1.00 (0~
      15 surv  Dummy survival (disp/am) Median survival 95.1        <NA>      <NA>    
    Code
      as_flextable(x0)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA` 
      header has 2 row(s) 
      body has 15 row(s) 
      original dataset sample: 
        .id             label   variable         straight          vshaped
      1  am      Transmission       auto       2 (18.18%)       9 (81.82%)
      2  am      Transmission     manual       7 (53.85%)       6 (46.15%)
      3 mpg Miles/(US) gallon  Min / Max      21.4 / 33.9      10.4 / 26.0
      4 mpg Miles/(US) gallon  Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4]
      5 mpg Miles/(US) gallon Mean (std)       26.8 (5.1)       16.6 (4.2)
                      NA
      1                8
      2                0
      3      14.3 / 24.4
      4 18.4 [17.5;20.1]
      5       19.0 (3.3)
    Code
      x1 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, total = "none", times = c(
        0, 100, 200, 400))
      x1
    Output
      # A tibble: 15 x 6
         .id   label                    variable        straight    vshaped   `NA`    
         <chr> <chr>                    <chr>           <chr>       <chr>     <chr>   
       1 am    Transmission             auto            2 (18.18%)  9 (81.82~ 8       
       2 am    Transmission             manual          7 (53.85%)  6 (46.15~ 0       
       3 mpg   Miles/(US) gallon        Min / Max       21.4 / 33.9 10.4 / 2~ 14.3 / ~
       4 mpg   Miles/(US) gallon        Med [IQR]       27.3 [21.5~ 15.5 [14~ 18.4 [1~
       5 mpg   Miles/(US) gallon        Mean (std)      26.8 (5.1)  16.6 (4.~ 19.0 (3~
       6 mpg   Miles/(US) gallon        N (NA)          9 (0)       15 (0)    8 (0)   
       7 cyl   Number of cylinders      4               7 (87.50%)  1 (12.50~ 2       
       8 cyl   Number of cylinders      6               0 (0%)      1 (100.0~ 3       
       9 cyl   Number of cylinders      8               0 (0%)      11 (100.~ 2       
      10 cyl   Number of cylinders      NA              2           2         1       
      11 surv  Dummy survival (disp/am) t=0             1.00 (0/9)  1.00 (0/~ 1.00 (0~
      12 surv  Dummy survival (disp/am) t=100           0.44 (5/4)  1.00 (0/~ 1.00 (0~
      13 surv  Dummy survival (disp/am) t=200           0.17 (2/1)  0.73 (4/~ 1.00 (0~
      14 surv  Dummy survival (disp/am) t=400           0.17 (0/0)  0.52 (2/~ 1.00 (0~
      15 surv  Dummy survival (disp/am) Median survival 95.1        <NA>      <NA>    
    Code
      as_flextable(x1)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA` 
      header has 2 row(s) 
      body has 15 row(s) 
      original dataset sample: 
        .id             label   variable         straight          vshaped
      1  am      Transmission       auto       2 (18.18%)       9 (81.82%)
      2  am      Transmission     manual       7 (53.85%)       6 (46.15%)
      3 mpg Miles/(US) gallon  Min / Max      21.4 / 33.9      10.4 / 26.0
      4 mpg Miles/(US) gallon  Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4]
      5 mpg Miles/(US) gallon Mean (std)       26.8 (5.1)       16.6 (4.2)
                      NA
      1                8
      2                0
      3      14.3 / 24.4
      4 18.4 [17.5;20.1]
      5       19.0 (3.3)
    Code
      x2 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, total = "row", times = c(
        0, 100, 200, 400))
      x2
    Output
      # A tibble: 15 x 7
         .id   label                    variable   straight   vshaped  `NA`    Total  
         <chr> <chr>                    <chr>      <chr>      <chr>    <chr>   <chr>  
       1 am    Transmission             auto       2 (18.18%) 9 (81.8~ 8       19 (59~
       2 am    Transmission             manual     7 (53.85%) 6 (46.1~ 0       13 (40~
       3 mpg   Miles/(US) gallon        Min / Max  21.4 / 33~ 10.4 / ~ 14.3 /~ 10.4 /~
       4 mpg   Miles/(US) gallon        Med [IQR]  27.3 [21.~ 15.5 [1~ 18.4 [~ 19.2 [~
       5 mpg   Miles/(US) gallon        Mean (std) 26.8 (5.1) 16.6 (4~ 19.0 (~ 20.1 (~
       6 mpg   Miles/(US) gallon        N (NA)     9 (0)      15 (0)   8 (0)   32 (0) 
       7 cyl   Number of cylinders      4          7 (87.50%) 1 (12.5~ 2       10 (37~
       8 cyl   Number of cylinders      6          0 (0%)     1 (100.~ 3       4 (14.~
       9 cyl   Number of cylinders      8          0 (0%)     11 (100~ 2       13 (48~
      10 cyl   Number of cylinders      NA         2          2        1       5      
      11 surv  Dummy survival (disp/am) t=0        1.00 (0/9) 1.00 (0~ 1.00 (~ 1.00 (~
      12 surv  Dummy survival (disp/am) t=100      0.44 (5/4) 1.00 (0~ 1.00 (~ 0.84 (~
      13 surv  Dummy survival (disp/am) t=200      0.17 (2/1) 0.73 (4~ 1.00 (~ 0.64 (~
      14 surv  Dummy survival (disp/am) t=400      0.17 (0/0) 0.52 (2~ 1.00 (~ 0.50 (~
      15 surv  Dummy survival (disp/am) Median su~ 95.1       <NA>     <NA>    <NA>   
    Code
      as_flextable(x2)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA`, `Total` 
      header has 2 row(s) 
      body has 15 row(s) 
      original dataset sample: 
        .id             label   variable         straight          vshaped
      1  am      Transmission       auto       2 (18.18%)       9 (81.82%)
      2  am      Transmission     manual       7 (53.85%)       6 (46.15%)
      3 mpg Miles/(US) gallon  Min / Max      21.4 / 33.9      10.4 / 26.0
      4 mpg Miles/(US) gallon  Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4]
      5 mpg Miles/(US) gallon Mean (std)       26.8 (5.1)       16.6 (4.2)
                      NA            Total
      1                8      19 (59.38%)
      2                0      13 (40.62%)
      3      14.3 / 24.4      10.4 / 33.9
      4 18.4 [17.5;20.1] 19.2 [15.4;22.8]
      5       19.0 (3.3)       20.1 (6.0)
    Code
      x3 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, total = "col", times = c(
        0, 100, 200, 400))
      x3
    Output
      # A tibble: 17 x 6
         .id   label                    variable        straight    vshaped   `NA`    
         <chr> <chr>                    <chr>           <chr>       <chr>     <chr>   
       1 am    Transmission             auto            2 (18.18%)  9 (81.82~ 8       
       2 am    Transmission             manual          7 (53.85%)  6 (46.15~ 0       
       3 am    Transmission             Total           9 (37.50%)  15 (62.5~ 8       
       4 mpg   Miles/(US) gallon        Min / Max       21.4 / 33.9 10.4 / 2~ 14.3 / ~
       5 mpg   Miles/(US) gallon        Med [IQR]       27.3 [21.5~ 15.5 [14~ 18.4 [1~
       6 mpg   Miles/(US) gallon        Mean (std)      26.8 (5.1)  16.6 (4.~ 19.0 (3~
       7 mpg   Miles/(US) gallon        N (NA)          9 (0)       15 (0)    8 (0)   
       8 cyl   Number of cylinders      4               7 (87.50%)  1 (12.50~ 2       
       9 cyl   Number of cylinders      6               0 (0%)      1 (100.0~ 3       
      10 cyl   Number of cylinders      8               0 (0%)      11 (100.~ 2       
      11 cyl   Number of cylinders      NA              2           2         1       
      12 cyl   Number of cylinders      Total           9 (35.00%)  15 (65.0~ 8       
      13 surv  Dummy survival (disp/am) t=0             1.00 (0/9)  1.00 (0/~ 1.00 (0~
      14 surv  Dummy survival (disp/am) t=100           0.44 (5/4)  1.00 (0/~ 1.00 (0~
      15 surv  Dummy survival (disp/am) t=200           0.17 (2/1)  0.73 (4/~ 1.00 (0~
      16 surv  Dummy survival (disp/am) t=400           0.17 (0/0)  0.52 (2/~ 1.00 (0~
      17 surv  Dummy survival (disp/am) Median survival 95.1        <NA>      <NA>    
    Code
      as_flextable(x3)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA` 
      header has 2 row(s) 
      body has 17 row(s) 
      original dataset sample: 
        .id             label  variable         straight          vshaped
      1  am      Transmission      auto       2 (18.18%)       9 (81.82%)
      2  am      Transmission    manual       7 (53.85%)       6 (46.15%)
      3  am      Transmission     Total       9 (37.50%)      15 (62.50%)
      4 mpg Miles/(US) gallon Min / Max      21.4 / 33.9      10.4 / 26.0
      5 mpg Miles/(US) gallon Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4]
                      NA
      1                8
      2                0
      3                8
      4      14.3 / 24.4
      5 18.4 [17.5;20.1]
    Code
      x4 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, total = "both", times = c(
        0, 100, 200, 400))
      x4
    Output
      # A tibble: 17 x 7
         .id   label                    variable   straight   vshaped  `NA`    Total  
         <chr> <chr>                    <chr>      <chr>      <chr>    <chr>   <chr>  
       1 am    Transmission             auto       2 (18.18%) 9 (81.8~ 8       19 (59~
       2 am    Transmission             manual     7 (53.85%) 6 (46.1~ 0       13 (40~
       3 am    Transmission             Total      9 (37.50%) 15 (62.~ 8       32 (10~
       4 mpg   Miles/(US) gallon        Min / Max  21.4 / 33~ 10.4 / ~ 14.3 /~ 10.4 /~
       5 mpg   Miles/(US) gallon        Med [IQR]  27.3 [21.~ 15.5 [1~ 18.4 [~ 19.2 [~
       6 mpg   Miles/(US) gallon        Mean (std) 26.8 (5.1) 16.6 (4~ 19.0 (~ 20.1 (~
       7 mpg   Miles/(US) gallon        N (NA)     9 (0)      15 (0)   8 (0)   32 (0) 
       8 cyl   Number of cylinders      4          7 (87.50%) 1 (12.5~ 2       10 (37~
       9 cyl   Number of cylinders      6          0 (0%)     1 (100.~ 3       4 (14.~
      10 cyl   Number of cylinders      8          0 (0%)     11 (100~ 2       13 (48~
      11 cyl   Number of cylinders      NA         2          2        1       5      
      12 cyl   Number of cylinders      Total      9 (35.00%) 15 (65.~ 8       32 (10~
      13 surv  Dummy survival (disp/am) t=0        1.00 (0/9) 1.00 (0~ 1.00 (~ 1.00 (~
      14 surv  Dummy survival (disp/am) t=100      0.44 (5/4) 1.00 (0~ 1.00 (~ 0.84 (~
      15 surv  Dummy survival (disp/am) t=200      0.17 (2/1) 0.73 (4~ 1.00 (~ 0.64 (~
      16 surv  Dummy survival (disp/am) t=400      0.17 (0/0) 0.52 (2~ 1.00 (~ 0.50 (~
      17 surv  Dummy survival (disp/am) Median su~ 95.1       <NA>     <NA>    <NA>   
    Code
      as_flextable(x4)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA`, `Total` 
      header has 2 row(s) 
      body has 17 row(s) 
      original dataset sample: 
        .id             label  variable         straight          vshaped
      1  am      Transmission      auto       2 (18.18%)       9 (81.82%)
      2  am      Transmission    manual       7 (53.85%)       6 (46.15%)
      3  am      Transmission     Total       9 (37.50%)      15 (62.50%)
      4 mpg Miles/(US) gallon Min / Max      21.4 / 33.9      10.4 / 26.0
      5 mpg Miles/(US) gallon Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4]
                      NA            Total
      1                8      19 (59.38%)
      2                0      13 (40.62%)
      3                8     32 (100.00%)
      4      14.3 / 24.4      10.4 / 33.9
      5 18.4 [17.5;20.1] 19.2 [15.4;22.8]

# Margins without totals

    Code
      x0 = crosstable(mtcars3, c(am, cyl), by = vs, total = "none")
      x0
    Output
      # A tibble: 6 x 6
        .id   label               variable straight   vshaped      `NA` 
        <chr> <chr>               <chr>    <chr>      <chr>        <chr>
      1 am    Transmission        auto     2 (18.18%) 9 (81.82%)   8    
      2 am    Transmission        manual   7 (53.85%) 6 (46.15%)   0    
      3 cyl   Number of cylinders 4        7 (87.50%) 1 (12.50%)   2    
      4 cyl   Number of cylinders 6        0 (0%)     1 (100.00%)  3    
      5 cyl   Number of cylinders 8        0 (0%)     11 (100.00%) 2    
      6 cyl   Number of cylinders NA       2          2            1    
    Code
      as_flextable(x0)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA` 
      header has 2 row(s) 
      body has 6 row(s) 
      original dataset sample: 
        .id               label variable   straight      vshaped NA
      1  am        Transmission     auto 2 (18.18%)   9 (81.82%)  8
      2  am        Transmission   manual 7 (53.85%)   6 (46.15%)  0
      3 cyl Number of cylinders        4 7 (87.50%)   1 (12.50%)  2
      4 cyl Number of cylinders        6     0 (0%)  1 (100.00%)  3
      5 cyl Number of cylinders        8     0 (0%) 11 (100.00%)  2
    Code
      x1 = crosstable(mtcars3, c(am, cyl), by = vs, total = "none", margin = "row")
      x1
    Output
      # A tibble: 6 x 6
        .id   label               variable straight   vshaped      `NA` 
        <chr> <chr>               <chr>    <chr>      <chr>        <chr>
      1 am    Transmission        auto     2 (18.18%) 9 (81.82%)   8    
      2 am    Transmission        manual   7 (53.85%) 6 (46.15%)   0    
      3 cyl   Number of cylinders 4        7 (87.50%) 1 (12.50%)   2    
      4 cyl   Number of cylinders 6        0 (0%)     1 (100.00%)  3    
      5 cyl   Number of cylinders 8        0 (0%)     11 (100.00%) 2    
      6 cyl   Number of cylinders NA       2          2            1    
    Code
      as_flextable(x1)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA` 
      header has 2 row(s) 
      body has 6 row(s) 
      original dataset sample: 
        .id               label variable   straight      vshaped NA
      1  am        Transmission     auto 2 (18.18%)   9 (81.82%)  8
      2  am        Transmission   manual 7 (53.85%)   6 (46.15%)  0
      3 cyl Number of cylinders        4 7 (87.50%)   1 (12.50%)  2
      4 cyl Number of cylinders        6     0 (0%)  1 (100.00%)  3
      5 cyl Number of cylinders        8     0 (0%) 11 (100.00%)  2
    Code
      x2 = crosstable(mtcars3, c(am, cyl), by = vs, total = "none", margin = "col")
      x2
    Output
      # A tibble: 6 x 6
        .id   label               variable straight    vshaped     `NA` 
        <chr> <chr>               <chr>    <chr>       <chr>       <chr>
      1 am    Transmission        auto     2 (22.22%)  9 (60.00%)  8    
      2 am    Transmission        manual   7 (77.78%)  6 (40.00%)  0    
      3 cyl   Number of cylinders 4        7 (100.00%) 1 (7.69%)   2    
      4 cyl   Number of cylinders 6        0 (0%)      1 (7.69%)   3    
      5 cyl   Number of cylinders 8        0 (0%)      11 (84.62%) 2    
      6 cyl   Number of cylinders NA       2           2           1    
    Code
      as_flextable(x2)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA` 
      header has 2 row(s) 
      body has 6 row(s) 
      original dataset sample: 
        .id               label variable    straight     vshaped NA
      1  am        Transmission     auto  2 (22.22%)  9 (60.00%)  8
      2  am        Transmission   manual  7 (77.78%)  6 (40.00%)  0
      3 cyl Number of cylinders        4 7 (100.00%)   1 (7.69%)  2
      4 cyl Number of cylinders        6      0 (0%)   1 (7.69%)  3
      5 cyl Number of cylinders        8      0 (0%) 11 (84.62%)  2
    Code
      x3 = crosstable(mtcars3, c(am, cyl), by = vs, total = "none", margin = "cell")
      x3
    Output
      # A tibble: 6 x 6
        .id   label               variable straight   vshaped     `NA` 
        <chr> <chr>               <chr>    <chr>      <chr>       <chr>
      1 am    Transmission        auto     2 (8.33%)  9 (37.50%)  8    
      2 am    Transmission        manual   7 (29.17%) 6 (25.00%)  0    
      3 cyl   Number of cylinders 4        7 (35.00%) 1 (5.00%)   2    
      4 cyl   Number of cylinders 6        0 (0%)     1 (5.00%)   3    
      5 cyl   Number of cylinders 8        0 (0%)     11 (55.00%) 2    
      6 cyl   Number of cylinders NA       2          2           1    
    Code
      as_flextable(x3)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA` 
      header has 2 row(s) 
      body has 6 row(s) 
      original dataset sample: 
        .id               label variable   straight     vshaped NA
      1  am        Transmission     auto  2 (8.33%)  9 (37.50%)  8
      2  am        Transmission   manual 7 (29.17%)  6 (25.00%)  0
      3 cyl Number of cylinders        4 7 (35.00%)   1 (5.00%)  2
      4 cyl Number of cylinders        6     0 (0%)   1 (5.00%)  3
      5 cyl Number of cylinders        8     0 (0%) 11 (55.00%)  2
    Code
      x4 = crosstable(mtcars3, c(am, cyl), by = vs, total = "none", margin = "none")
      x4
    Output
      # A tibble: 6 x 6
        .id   label               variable straight vshaped `NA` 
        <chr> <chr>               <chr>    <chr>    <chr>   <chr>
      1 am    Transmission        auto     2        9       8    
      2 am    Transmission        manual   7        6       0    
      3 cyl   Number of cylinders 4        7        1       2    
      4 cyl   Number of cylinders 6        0        1       3    
      5 cyl   Number of cylinders 8        0        11      2    
      6 cyl   Number of cylinders NA       2        2       1    
    Code
      as_flextable(x4)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA` 
      header has 2 row(s) 
      body has 6 row(s) 
      original dataset sample: 
        .id               label variable straight vshaped NA
      1  am        Transmission     auto        2       9  8
      2  am        Transmission   manual        7       6  0
      3 cyl Number of cylinders        4        7       1  2
      4 cyl Number of cylinders        6        0       1  3
      5 cyl Number of cylinders        8        0      11  2
    Code
      x5 = crosstable(mtcars3, c(am, cyl), by = vs, total = "none", margin = "all")
      x5
    Output
      # A tibble: 6 x 6
        .id   label               variable straight            vshaped           `NA` 
        <chr> <chr>               <chr>    <chr>               <chr>             <chr>
      1 am    Transmission        auto     2 (8.33% / 18.18% ~ 9 (37.50% / 81.8~ 8    
      2 am    Transmission        manual   7 (29.17% / 53.85%~ 6 (25.00% / 46.1~ 0    
      3 cyl   Number of cylinders 4        7 (35.00% / 87.50%~ 1 (5.00% / 12.50~ 2    
      4 cyl   Number of cylinders 6        0 (0% / 0% / 0%)    1 (5.00% / 100.0~ 3    
      5 cyl   Number of cylinders 8        0 (0% / 0% / 0%)    11 (55.00% / 100~ 2    
      6 cyl   Number of cylinders NA       2                   2                 1    
    Code
      as_flextable(x5)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA` 
      header has 2 row(s) 
      body has 6 row(s) 
      original dataset sample: 
        .id               label variable                      straight
      1  am        Transmission     auto   2 (8.33% / 18.18% / 22.22%)
      2  am        Transmission   manual  7 (29.17% / 53.85% / 77.78%)
      3 cyl Number of cylinders        4 7 (35.00% / 87.50% / 100.00%)
      4 cyl Number of cylinders        6              0 (0% / 0% / 0%)
      5 cyl Number of cylinders        8              0 (0% / 0% / 0%)
                               vshaped NA
      1   9 (37.50% / 81.82% / 60.00%)  8
      2   6 (25.00% / 46.15% / 40.00%)  0
      3     1 (5.00% / 12.50% / 7.69%)  2
      4    1 (5.00% / 100.00% / 7.69%)  3
      5 11 (55.00% / 100.00% / 84.62%)  2
    Code
      x6 = crosstable(mtcars3, c(am, cyl), by = vs, total = "none", margin = 1:2)
      x6
    Output
      # A tibble: 6 x 6
        .id   label               variable straight             vshaped          `NA` 
        <chr> <chr>               <chr>    <chr>                <chr>            <chr>
      1 am    Transmission        auto     2 (18.18% / 22.22%)  9 (81.82% / 60.~ 8    
      2 am    Transmission        manual   7 (53.85% / 77.78%)  6 (46.15% / 40.~ 0    
      3 cyl   Number of cylinders 4        7 (87.50% / 100.00%) 1 (12.50% / 7.6~ 2    
      4 cyl   Number of cylinders 6        0 (0% / 0%)          1 (100.00% / 7.~ 3    
      5 cyl   Number of cylinders 8        0 (0% / 0%)          11 (100.00% / 8~ 2    
      6 cyl   Number of cylinders NA       2                    2                1    
    Code
      as_flextable(x6)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA` 
      header has 2 row(s) 
      body has 6 row(s) 
      original dataset sample: 
        .id               label variable             straight               vshaped
      1  am        Transmission     auto  2 (18.18% / 22.22%)   9 (81.82% / 60.00%)
      2  am        Transmission   manual  7 (53.85% / 77.78%)   6 (46.15% / 40.00%)
      3 cyl Number of cylinders        4 7 (87.50% / 100.00%)    1 (12.50% / 7.69%)
      4 cyl Number of cylinders        6          0 (0% / 0%)   1 (100.00% / 7.69%)
      5 cyl Number of cylinders        8          0 (0% / 0%) 11 (100.00% / 84.62%)
        NA
      1  8
      2  0
      3  2
      4  3
      5  2

# Margins with totals

    Code
      x0 = crosstable(mtcars3, c(am, cyl), by = vs, total = "both")
      x0
    Output
      # A tibble: 8 x 7
        .id   label               variable straight   vshaped      `NA`  Total       
        <chr> <chr>               <chr>    <chr>      <chr>        <chr> <chr>       
      1 am    Transmission        auto     2 (18.18%) 9 (81.82%)   8     19 (59.38%) 
      2 am    Transmission        manual   7 (53.85%) 6 (46.15%)   0     13 (40.62%) 
      3 am    Transmission        Total    9 (37.50%) 15 (62.50%)  8     32 (100.00%)
      4 cyl   Number of cylinders 4        7 (87.50%) 1 (12.50%)   2     10 (37.04%) 
      5 cyl   Number of cylinders 6        0 (0%)     1 (100.00%)  3     4 (14.81%)  
      6 cyl   Number of cylinders 8        0 (0%)     11 (100.00%) 2     13 (48.15%) 
      7 cyl   Number of cylinders NA       2          2            1     5           
      8 cyl   Number of cylinders Total    9 (35.00%) 15 (65.00%)  8     32 (100.00%)
    Code
      as_flextable(x0)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA`, `Total` 
      header has 2 row(s) 
      body has 8 row(s) 
      original dataset sample: 
        .id               label variable   straight     vshaped NA        Total
      1  am        Transmission     auto 2 (18.18%)  9 (81.82%)  8  19 (59.38%)
      2  am        Transmission   manual 7 (53.85%)  6 (46.15%)  0  13 (40.62%)
      3  am        Transmission    Total 9 (37.50%) 15 (62.50%)  8 32 (100.00%)
      4 cyl Number of cylinders        4 7 (87.50%)  1 (12.50%)  2  10 (37.04%)
      5 cyl Number of cylinders        6     0 (0%) 1 (100.00%)  3   4 (14.81%)
    Code
      x1 = crosstable(mtcars3, c(am, cyl), by = vs, total = "both", margin = "row")
      x1
    Output
      # A tibble: 8 x 7
        .id   label               variable straight   vshaped      `NA`  Total       
        <chr> <chr>               <chr>    <chr>      <chr>        <chr> <chr>       
      1 am    Transmission        auto     2 (18.18%) 9 (81.82%)   8     19 (59.38%) 
      2 am    Transmission        manual   7 (53.85%) 6 (46.15%)   0     13 (40.62%) 
      3 am    Transmission        Total    9 (37.50%) 15 (62.50%)  8     32 (100.00%)
      4 cyl   Number of cylinders 4        7 (87.50%) 1 (12.50%)   2     10 (37.04%) 
      5 cyl   Number of cylinders 6        0 (0%)     1 (100.00%)  3     4 (14.81%)  
      6 cyl   Number of cylinders 8        0 (0%)     11 (100.00%) 2     13 (48.15%) 
      7 cyl   Number of cylinders NA       2          2            1     5           
      8 cyl   Number of cylinders Total    9 (35.00%) 15 (65.00%)  8     32 (100.00%)
    Code
      as_flextable(x1)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA`, `Total` 
      header has 2 row(s) 
      body has 8 row(s) 
      original dataset sample: 
        .id               label variable   straight     vshaped NA        Total
      1  am        Transmission     auto 2 (18.18%)  9 (81.82%)  8  19 (59.38%)
      2  am        Transmission   manual 7 (53.85%)  6 (46.15%)  0  13 (40.62%)
      3  am        Transmission    Total 9 (37.50%) 15 (62.50%)  8 32 (100.00%)
      4 cyl Number of cylinders        4 7 (87.50%)  1 (12.50%)  2  10 (37.04%)
      5 cyl Number of cylinders        6     0 (0%) 1 (100.00%)  3   4 (14.81%)
    Code
      x2 = crosstable(mtcars3, c(am, cyl), by = vs, total = "both", margin = "col")
      x2
    Output
      # A tibble: 8 x 7
        .id   label               variable straight    vshaped     `NA`  Total       
        <chr> <chr>               <chr>    <chr>       <chr>       <chr> <chr>       
      1 am    Transmission        auto     2 (22.22%)  9 (60.00%)  8     19 (59.38%) 
      2 am    Transmission        manual   7 (77.78%)  6 (40.00%)  0     13 (40.62%) 
      3 am    Transmission        Total    9 (37.50%)  15 (62.50%) 8     32 (100.00%)
      4 cyl   Number of cylinders 4        7 (100.00%) 1 (7.69%)   2     10 (37.04%) 
      5 cyl   Number of cylinders 6        0 (0%)      1 (7.69%)   3     4 (14.81%)  
      6 cyl   Number of cylinders 8        0 (0%)      11 (84.62%) 2     13 (48.15%) 
      7 cyl   Number of cylinders NA       2           2           1     5           
      8 cyl   Number of cylinders Total    9 (35.00%)  15 (65.00%) 8     32 (100.00%)
    Code
      as_flextable(x2)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA`, `Total` 
      header has 2 row(s) 
      body has 8 row(s) 
      original dataset sample: 
        .id               label variable    straight     vshaped NA        Total
      1  am        Transmission     auto  2 (22.22%)  9 (60.00%)  8  19 (59.38%)
      2  am        Transmission   manual  7 (77.78%)  6 (40.00%)  0  13 (40.62%)
      3  am        Transmission    Total  9 (37.50%) 15 (62.50%)  8 32 (100.00%)
      4 cyl Number of cylinders        4 7 (100.00%)   1 (7.69%)  2  10 (37.04%)
      5 cyl Number of cylinders        6      0 (0%)   1 (7.69%)  3   4 (14.81%)
    Code
      x3 = crosstable(mtcars3, c(am, cyl), by = vs, total = "both", margin = "cell")
      x3
    Output
      # A tibble: 8 x 7
        .id   label               variable straight   vshaped     `NA`  Total       
        <chr> <chr>               <chr>    <chr>      <chr>       <chr> <chr>       
      1 am    Transmission        auto     2 (8.33%)  9 (37.50%)  8     19 (59.38%) 
      2 am    Transmission        manual   7 (29.17%) 6 (25.00%)  0     13 (40.62%) 
      3 am    Transmission        Total    9 (37.50%) 15 (62.50%) 8     32 (100.00%)
      4 cyl   Number of cylinders 4        7 (35.00%) 1 (5.00%)   2     10 (37.04%) 
      5 cyl   Number of cylinders 6        0 (0%)     1 (5.00%)   3     4 (14.81%)  
      6 cyl   Number of cylinders 8        0 (0%)     11 (55.00%) 2     13 (48.15%) 
      7 cyl   Number of cylinders NA       2          2           1     5           
      8 cyl   Number of cylinders Total    9 (35.00%) 15 (65.00%) 8     32 (100.00%)
    Code
      as_flextable(x3)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA`, `Total` 
      header has 2 row(s) 
      body has 8 row(s) 
      original dataset sample: 
        .id               label variable   straight     vshaped NA        Total
      1  am        Transmission     auto  2 (8.33%)  9 (37.50%)  8  19 (59.38%)
      2  am        Transmission   manual 7 (29.17%)  6 (25.00%)  0  13 (40.62%)
      3  am        Transmission    Total 9 (37.50%) 15 (62.50%)  8 32 (100.00%)
      4 cyl Number of cylinders        4 7 (35.00%)   1 (5.00%)  2  10 (37.04%)
      5 cyl Number of cylinders        6     0 (0%)   1 (5.00%)  3   4 (14.81%)
    Code
      x4 = crosstable(mtcars3, c(am, cyl), by = vs, total = "both", margin = "none")
      x4
    Output
      # A tibble: 8 x 7
        .id   label               variable straight vshaped `NA`  Total
        <chr> <chr>               <chr>    <chr>    <chr>   <chr> <chr>
      1 am    Transmission        auto     2        9       8     19   
      2 am    Transmission        manual   7        6       0     13   
      3 am    Transmission        Total    9        15      8     32   
      4 cyl   Number of cylinders 4        7        1       2     10   
      5 cyl   Number of cylinders 6        0        1       3     4    
      6 cyl   Number of cylinders 8        0        11      2     13   
      7 cyl   Number of cylinders NA       2        2       1     5    
      8 cyl   Number of cylinders Total    9        15      8     32   
    Code
      as_flextable(x4)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA`, `Total` 
      header has 2 row(s) 
      body has 8 row(s) 
      original dataset sample: 
        .id               label variable straight vshaped NA Total
      1  am        Transmission     auto        2       9  8    19
      2  am        Transmission   manual        7       6  0    13
      3  am        Transmission    Total        9      15  8    32
      4 cyl Number of cylinders        4        7       1  2    10
      5 cyl Number of cylinders        6        0       1  3     4
    Code
      x5 = crosstable(mtcars3, c(am, cyl), by = vs, total = "both", margin = "all")
      x5
    Output
      # A tibble: 8 x 7
        .id   label               variable straight        vshaped       `NA`  Total  
        <chr> <chr>               <chr>    <chr>           <chr>         <chr> <chr>  
      1 am    Transmission        auto     2 (8.33% / 18.~ 9 (37.50% / ~ 8     19 (59~
      2 am    Transmission        manual   7 (29.17% / 53~ 6 (25.00% / ~ 0     13 (40~
      3 am    Transmission        Total    9 (37.50%)      15 (62.50%)   8     32 (10~
      4 cyl   Number of cylinders 4        7 (35.00% / 87~ 1 (5.00% / 1~ 2     10 (37~
      5 cyl   Number of cylinders 6        0 (0% / 0% / 0~ 1 (5.00% / 1~ 3     4 (14.~
      6 cyl   Number of cylinders 8        0 (0% / 0% / 0~ 11 (55.00% /~ 2     13 (48~
      7 cyl   Number of cylinders NA       2               2             1     5      
      8 cyl   Number of cylinders Total    9 (35.00%)      15 (65.00%)   8     32 (10~
    Code
      as_flextable(x5)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA`, `Total` 
      header has 2 row(s) 
      body has 8 row(s) 
      original dataset sample: 
        .id               label variable                      straight
      1  am        Transmission     auto   2 (8.33% / 18.18% / 22.22%)
      2  am        Transmission   manual  7 (29.17% / 53.85% / 77.78%)
      3  am        Transmission    Total                    9 (37.50%)
      4 cyl Number of cylinders        4 7 (35.00% / 87.50% / 100.00%)
      5 cyl Number of cylinders        6              0 (0% / 0% / 0%)
                             vshaped NA        Total
      1 9 (37.50% / 81.82% / 60.00%)  8  19 (59.38%)
      2 6 (25.00% / 46.15% / 40.00%)  0  13 (40.62%)
      3                  15 (62.50%)  8 32 (100.00%)
      4   1 (5.00% / 12.50% / 7.69%)  2  10 (37.04%)
      5  1 (5.00% / 100.00% / 7.69%)  3   4 (14.81%)
    Code
      x6 = crosstable(mtcars3, c(am, cyl), by = vs, total = "both", margin = 1:2)
      x6
    Output
      # A tibble: 8 x 7
        .id   label               variable straight             vshaped   `NA`  Total 
        <chr> <chr>               <chr>    <chr>                <chr>     <chr> <chr> 
      1 am    Transmission        auto     2 (18.18% / 22.22%)  9 (81.82~ 8     19 (5~
      2 am    Transmission        manual   7 (53.85% / 77.78%)  6 (46.15~ 0     13 (4~
      3 am    Transmission        Total    9 (37.50%)           15 (62.5~ 8     32 (1~
      4 cyl   Number of cylinders 4        7 (87.50% / 100.00%) 1 (12.50~ 2     10 (3~
      5 cyl   Number of cylinders 6        0 (0% / 0%)          1 (100.0~ 3     4 (14~
      6 cyl   Number of cylinders 8        0 (0% / 0%)          11 (100.~ 2     13 (4~
      7 cyl   Number of cylinders NA       2                    2         1     5     
      8 cyl   Number of cylinders Total    9 (35.00%)           15 (65.0~ 8     32 (1~
    Code
      as_flextable(x6)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA`, `Total` 
      header has 2 row(s) 
      body has 8 row(s) 
      original dataset sample: 
        .id               label variable             straight             vshaped NA
      1  am        Transmission     auto  2 (18.18% / 22.22%) 9 (81.82% / 60.00%)  8
      2  am        Transmission   manual  7 (53.85% / 77.78%) 6 (46.15% / 40.00%)  0
      3  am        Transmission    Total           9 (37.50%)         15 (62.50%)  8
      4 cyl Number of cylinders        4 7 (87.50% / 100.00%)  1 (12.50% / 7.69%)  2
      5 cyl Number of cylinders        6          0 (0% / 0%) 1 (100.00% / 7.69%)  3
               Total
      1  19 (59.38%)
      2  13 (40.62%)
      3 32 (100.00%)
      4  10 (37.04%)
      5   4 (14.81%)

# Percent pattern

    Code
      x0 = crosstable(mtcars3, cyl, percent_pattern = "N={n} \nrow={p_row}, col={p_col}")
      x0
    Output
      # A tibble: 4 x 4
        .id   label               variable value                           
        <chr> <chr>               <chr>    <chr>                           
      1 cyl   Number of cylinders 4        "N=10 \nrow=100.00%, col=37.04%"
      2 cyl   Number of cylinders 6        "N=4 \nrow=100.00%, col=14.81%" 
      3 cyl   Number of cylinders 8        "N=13 \nrow=100.00%, col=48.15%"
      4 cyl   Number of cylinders NA       "5"                             
    Code
      as_flextable(x0)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `value` 
      header has 1 row(s) 
      body has 4 row(s) 
      original dataset sample: 
        .id               label variable                          value
      1 cyl Number of cylinders        4 N=10 \nrow=100.00%, col=37.04%
      2 cyl Number of cylinders        6  N=4 \nrow=100.00%, col=14.81%
      3 cyl Number of cylinders        8 N=13 \nrow=100.00%, col=48.15%
      4 cyl Number of cylinders       NA                              5
    Code
      x1 = crosstable(mtcars3, cyl, total = TRUE, percent_pattern = "N={n} \np[95%CI] = {p_col} [{p_col_inf}; {p_col_sup}]")
      x1
    Output
      # A tibble: 5 x 4
        .id   label               variable value                                      
        <chr> <chr>               <chr>    <chr>                                      
      1 cyl   Number of cylinders 4        "N=10 \np[95%CI] = 37.04% [14.94%; 66.33%]"
      2 cyl   Number of cylinders 6        "N=4 \np[95%CI] = 14.81% [1.80%; 62.31%]"  
      3 cyl   Number of cylinders 8        "N=13 \np[95%CI] = 48.15% [24.70%; 72.44%]"
      4 cyl   Number of cylinders NA       "5"                                        
      5 cyl   Number of cylinders Total    "32 (100.00%)"                             
    Code
      as_flextable(x1)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `value` 
      header has 1 row(s) 
      body has 5 row(s) 
      original dataset sample: 
        .id               label variable                                     value
      1 cyl Number of cylinders        4 N=10 \np[95%CI] = 37.04% [14.94%; 66.33%]
      2 cyl Number of cylinders        6   N=4 \np[95%CI] = 14.81% [1.80%; 62.31%]
      3 cyl Number of cylinders        8 N=13 \np[95%CI] = 48.15% [24.70%; 72.44%]
      4 cyl Number of cylinders       NA                                         5
      5 cyl Number of cylinders    Total                              32 (100.00%)
    Code
      x2 = crosstable(mtcars3, cyl, showNA = "always", percent_pattern = "N={n} \nrow={p_row}, col={p_col}")
      x2
    Output
      # A tibble: 4 x 4
        .id   label               variable value                           
        <chr> <chr>               <chr>    <chr>                           
      1 cyl   Number of cylinders 4        "N=10 \nrow=100.00%, col=37.04%"
      2 cyl   Number of cylinders 6        "N=4 \nrow=100.00%, col=14.81%" 
      3 cyl   Number of cylinders 8        "N=13 \nrow=100.00%, col=48.15%"
      4 cyl   Number of cylinders NA       "5"                             
    Code
      as_flextable(x2)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `value` 
      header has 1 row(s) 
      body has 4 row(s) 
      original dataset sample: 
        .id               label variable                          value
      1 cyl Number of cylinders        4 N=10 \nrow=100.00%, col=37.04%
      2 cyl Number of cylinders        6  N=4 \nrow=100.00%, col=14.81%
      3 cyl Number of cylinders        8 N=13 \nrow=100.00%, col=48.15%
      4 cyl Number of cylinders       NA                              5
    Code
      x3 = crosstable(mtcars3, c(mpg, vs, cyl), by = c(am, dummy))
      x3
    Output
      # A tibble: 11 x 5
         .id   label               variable   `am=auto & dummy=dummy` `am=manual & du~
         <chr> <chr>               <chr>      <chr>                   <chr>           
       1 mpg   Miles/(US) gallon   Min / Max  10.4 / 24.4             15.0 / 33.9     
       2 mpg   Miles/(US) gallon   Med [IQR]  17.3 [14.9;19.2]        22.8 [21.0;30.4]
       3 mpg   Miles/(US) gallon   Mean (std) 17.1 (3.8)              24.4 (6.2)      
       4 mpg   Miles/(US) gallon   N (NA)     19 (0)                  13 (0)          
       5 vs    Engine              straight   2 (22.22%)              7 (77.78%)      
       6 vs    Engine              vshaped    9 (60.00%)              6 (40.00%)      
       7 vs    Engine              NA         8                       0               
       8 cyl   Number of cylinders 4          3 (30.00%)              7 (70.00%)      
       9 cyl   Number of cylinders 6          3 (75.00%)              1 (25.00%)      
      10 cyl   Number of cylinders 8          11 (84.62%)             2 (15.38%)      
      11 cyl   Number of cylinders NA         2                       3               
    Code
      as_flextable(x3)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `am=auto & dummy=dummy`, `am=manual & dummy=dummy` 
      header has 2 row(s) 
      body has 11 row(s) 
      original dataset sample: 
        .id             label   variable am=auto & dummy=dummy
      1 mpg Miles/(US) gallon  Min / Max           10.4 / 24.4
      2 mpg Miles/(US) gallon  Med [IQR]      17.3 [14.9;19.2]
      3 mpg Miles/(US) gallon Mean (std)            17.1 (3.8)
      4 mpg Miles/(US) gallon     N (NA)                19 (0)
      5  vs            Engine   straight            2 (22.22%)
        am=manual & dummy=dummy
      1             15.0 / 33.9
      2        22.8 [21.0;30.4]
      3              24.4 (6.2)
      4                  13 (0)
      5              7 (77.78%)

# By dummy

    Code
      x0 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = dummy)
      x0
    Output
      # A tibble: 38 x 4
         .id   label               variable   dummy           
         <chr> <chr>               <chr>      <chr>           
       1 am    Transmission        auto       19 (59.38%)     
       2 am    Transmission        manual     13 (40.62%)     
       3 mpg   Miles/(US) gallon   Min / Max  10.4 / 33.9     
       4 mpg   Miles/(US) gallon   Med [IQR]  19.2 [15.4;22.8]
       5 mpg   Miles/(US) gallon   Mean (std) 20.1 (6.0)      
       6 mpg   Miles/(US) gallon   N (NA)     32 (0)          
       7 cyl   Number of cylinders 4          10 (37.04%)     
       8 cyl   Number of cylinders 6          4 (14.81%)      
       9 cyl   Number of cylinders 8          13 (48.15%)     
      10 cyl   Number of cylinders NA         5               
      # ... with 28 more rows
    Code
      as_flextable(x0)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `dummy` 
      header has 2 row(s) 
      body has 38 row(s) 
      original dataset sample: 
        .id             label   variable            dummy
      1  am      Transmission       auto      19 (59.38%)
      2  am      Transmission     manual      13 (40.62%)
      3 mpg Miles/(US) gallon  Min / Max      10.4 / 33.9
      4 mpg Miles/(US) gallon  Med [IQR] 19.2 [15.4;22.8]
      5 mpg Miles/(US) gallon Mean (std)       20.1 (6.0)
    Code
      x1 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = dummy, showNA = TRUE)
      x1
    Output
      # A tibble: 39 x 5
         .id   label               variable   dummy            `NA` 
         <chr> <chr>               <chr>      <chr>            <chr>
       1 am    Transmission        auto       19 (59.38%)      0    
       2 am    Transmission        manual     13 (40.62%)      0    
       3 am    Transmission        NA         0                0    
       4 mpg   Miles/(US) gallon   Min / Max  10.4 / 33.9      no NA
       5 mpg   Miles/(US) gallon   Med [IQR]  19.2 [15.4;22.8] no NA
       6 mpg   Miles/(US) gallon   Mean (std) 20.1 (6.0)       no NA
       7 mpg   Miles/(US) gallon   N (NA)     32 (0)           no NA
       8 cyl   Number of cylinders 4          10 (37.04%)      0    
       9 cyl   Number of cylinders 6          4 (14.81%)       0    
      10 cyl   Number of cylinders 8          13 (48.15%)      0    
      # ... with 29 more rows
    Code
      as_flextable(x1)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `dummy`, `NA` 
      header has 2 row(s) 
      body has 39 row(s) 
      original dataset sample: 
        .id             label  variable            dummy    NA
      1  am      Transmission      auto      19 (59.38%)     0
      2  am      Transmission    manual      13 (40.62%)     0
      3  am      Transmission        NA                0     0
      4 mpg Miles/(US) gallon Min / Max      10.4 / 33.9 no NA
      5 mpg Miles/(US) gallon Med [IQR] 19.2 [15.4;22.8] no NA

---

    Code
      x2 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = dummy2)
      x2
    Output
      # A tibble: 39 x 5
         .id   label               variable   dummy            `NA`            
         <chr> <chr>               <chr>      <chr>            <chr>           
       1 am    Transmission        auto       11 (100.00%)     8               
       2 am    Transmission        manual     13 (100.00%)     0               
       3 mpg   Miles/(US) gallon   Min / Max  10.4 / 33.9      14.3 / 24.4     
       4 mpg   Miles/(US) gallon   Med [IQR]  20.4 [15.2;23.6] 18.4 [17.5;20.1]
       5 mpg   Miles/(US) gallon   Mean (std) 20.5 (6.7)       19.0 (3.3)      
       6 mpg   Miles/(US) gallon   N (NA)     24 (0)           8 (0)           
       7 cyl   Number of cylinders 4          8 (100.00%)      2               
       8 cyl   Number of cylinders 6          1 (100.00%)      3               
       9 cyl   Number of cylinders 8          11 (100.00%)     2               
      10 cyl   Number of cylinders NA         4                1               
      # ... with 29 more rows
    Code
      as_flextable(x2)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `dummy`, `NA` 
      header has 2 row(s) 
      body has 39 row(s) 
      original dataset sample: 
        .id             label   variable            dummy               NA
      1  am      Transmission       auto     11 (100.00%)                8
      2  am      Transmission     manual     13 (100.00%)                0
      3 mpg Miles/(US) gallon  Min / Max      10.4 / 33.9      14.3 / 24.4
      4 mpg Miles/(US) gallon  Med [IQR] 20.4 [15.2;23.6] 18.4 [17.5;20.1]
      5 mpg Miles/(US) gallon Mean (std)       20.5 (6.7)       19.0 (3.3)
    Code
      x3 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = dummy2, showNA = FALSE)
      x3
    Output
      # A tibble: 32 x 4
         .id   label                    variable   dummy           
         <chr> <chr>                    <chr>      <chr>           
       1 am    Transmission             auto       11 (100.00%)    
       2 am    Transmission             manual     13 (100.00%)    
       3 mpg   Miles/(US) gallon        Min / Max  10.4 / 33.9     
       4 mpg   Miles/(US) gallon        Med [IQR]  20.4 [15.2;23.6]
       5 mpg   Miles/(US) gallon        Mean (std) 20.5 (6.7)      
       6 mpg   Miles/(US) gallon        N (NA)     24 (0)          
       7 cyl   Number of cylinders      4          8 (100.00%)     
       8 cyl   Number of cylinders      6          1 (100.00%)     
       9 cyl   Number of cylinders      8          11 (100.00%)    
      10 surv  Dummy survival (disp/am) t=71.1     0.96 (1/24)     
      # ... with 22 more rows
    Code
      as_flextable(x3)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `dummy` 
      header has 2 row(s) 
      body has 32 row(s) 
      original dataset sample: 
        .id             label   variable            dummy
      1  am      Transmission       auto     11 (100.00%)
      2  am      Transmission     manual     13 (100.00%)
      3 mpg Miles/(US) gallon  Min / Max      10.4 / 33.9
      4 mpg Miles/(US) gallon  Med [IQR] 20.4 [15.2;23.6]
      5 mpg Miles/(US) gallon Mean (std)       20.5 (6.7)

# By multiple

    Code
      x0 = crosstable(mtcars3, c(mpg, gear), by = c(cyl, am, vs))
      x0
    Output
      # A tibble: 7 x 22
        .id   label                   variable `cyl=4 & am=aut~ `cyl=6 & am=aut~ `cyl=8 & am=aut~
        <chr> <chr>                   <chr>    <chr>            <chr>            <chr>           
      1 mpg   Miles/(US) gallon       Min / M~ 21.5 / 21.5      NA / NA          NA / NA         
      2 mpg   Miles/(US) gallon       Med [IQ~ 21.5 [21.5;21.5] NA [NA;NA]       NA [NA;NA]      
      3 mpg   Miles/(US) gallon       Mean (s~ 21.5 (NA)        NA (NA)          NA (NA)         
      4 mpg   Miles/(US) gallon       N (NA)   1 (0)            0 (0)            0 (0)           
      5 gear  Number of forward gears 3        1 (7.69%)        0 (0%)           0 (0%)          
      6 gear  Number of forward gears 4        0 (0%)           0 (0%)           0 (0%)          
      7 gear  Number of forward gears 5        0 (0%)           0 (0%)           0 (0%)          
      # ... with 16 more variables: cyl=4 & am=manual & vs=straight <chr>,
      #   cyl=6 & am=manual & vs=straight <chr>,
      #   cyl=8 & am=manual & vs=straight <chr>, cyl=4 & am=auto & vs=vshaped <chr>,
      #   cyl=6 & am=auto & vs=vshaped <chr>, cyl=8 & am=auto & vs=vshaped <chr>,
      #   cyl=4 & am=manual & vs=vshaped <chr>, cyl=6 & am=manual & vs=vshaped <chr>,
      #   cyl=8 & am=manual & vs=vshaped <chr>, cyl=4 & am=auto & vs=NA <chr>,
      #   cyl=6 & am=auto & vs=NA <chr>, cyl=8 & am=auto & vs=NA <chr>, ...
    Code
      as_flextable(x0)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `cyl=4 & am=auto & vs=straight`, `cyl=6 & am=auto & vs=straight`, `cyl=8 & am=auto & vs=straight`, `cyl=4 & am=manual & vs=straight`, `cyl=6 & am=manual & vs=straight`, `cyl=8 & am=manual & vs=straight`, `cyl=4 & am=auto & vs=vshaped`, `cyl=6 & am=auto & vs=vshaped`, `cyl=8 & am=auto & vs=vshaped`, `cyl=4 & am=manual & vs=vshaped`, `cyl=6 & am=manual & vs=vshaped`, `cyl=8 & am=manual & vs=vshaped`, `cyl=4 & am=auto & vs=NA`, `cyl=6 & am=auto & vs=NA`, `cyl=8 & am=auto & vs=NA`, `cyl=4 & am=manual & vs=NA`, `cyl=6 & am=manual & vs=NA`, `cyl=8 & am=manual & vs=NA`, `NA` 
      header has 3 row(s) 
      body has 7 row(s) 
      original dataset sample: 
         .id                   label   variable cyl=4 & am=auto & vs=straight
      1  mpg       Miles/(US) gallon  Min / Max                   21.5 / 21.5
      2  mpg       Miles/(US) gallon  Med [IQR]              21.5 [21.5;21.5]
      3  mpg       Miles/(US) gallon Mean (std)                     21.5 (NA)
      4  mpg       Miles/(US) gallon     N (NA)                         1 (0)
      5 gear Number of forward gears          3                     1 (7.69%)
        cyl=6 & am=auto & vs=straight cyl=8 & am=auto & vs=straight
      1                       NA / NA                       NA / NA
      2                    NA [NA;NA]                    NA [NA;NA]
      3                       NA (NA)                       NA (NA)
      4                         0 (0)                         0 (0)
      5                        0 (0%)                        0 (0%)
        cyl=4 & am=manual & vs=straight cyl=6 & am=manual & vs=straight
      1                     21.4 / 33.9                         NA / NA
      2                30.4 [28.1;31.9]                      NA [NA;NA]
      3                      29.3 (4.5)                         NA (NA)
      4                           6 (0)                           0 (0)
      5                          0 (0%)                          0 (0%)
        cyl=8 & am=manual & vs=straight cyl=4 & am=auto & vs=vshaped
      1                         NA / NA                      NA / NA
      2                      NA [NA;NA]                   NA [NA;NA]
      3                         NA (NA)                      NA (NA)
      4                           0 (0)                        0 (0)
      5                          0 (0%)                       0 (0%)
        cyl=6 & am=auto & vs=vshaped cyl=8 & am=auto & vs=vshaped
      1                      NA / NA                  10.4 / 19.2
      2                   NA [NA;NA]             15.2 [13.3;15.5]
      3                      NA (NA)                   14.6 (2.9)
      4                        0 (0)                        9 (0)
      5                       0 (0%)                   9 (69.23%)
        cyl=4 & am=manual & vs=vshaped cyl=6 & am=manual & vs=vshaped
      1                    26.0 / 26.0                    19.7 / 19.7
      2               26.0 [26.0;26.0]               19.7 [19.7;19.7]
      3                      26.0 (NA)                      19.7 (NA)
      4                          1 (0)                          1 (0)
      5                         0 (0%)                         0 (0%)
        cyl=8 & am=manual & vs=vshaped cyl=4 & am=auto & vs=NA
      1                    15.0 / 15.8             22.8 / 24.4
      2               15.4 [15.2;15.6]        23.6 [23.2;24.0]
      3                     15.4 (0.6)              23.6 (1.1)
      4                          2 (0)                   2 (0)
      5                         0 (0%)                  0 (0%)
        cyl=6 & am=auto & vs=NA cyl=8 & am=auto & vs=NA cyl=4 & am=manual & vs=NA
      1             17.8 / 19.2             14.3 / 16.4                   NA / NA
      2        18.1 [18.0;18.6]        15.3 [14.8;15.9]                NA [NA;NA]
      3              18.4 (0.7)              15.3 (1.5)                   NA (NA)
      4                   3 (0)                   2 (0)                     0 (0)
      5               1 (7.69%)              2 (15.38%)                    0 (0%)
        cyl=6 & am=manual & vs=NA cyl=8 & am=manual & vs=NA               NA
      1                   NA / NA                   NA / NA      18.7 / 22.8
      2                NA [NA;NA]                NA [NA;NA] 21.0 [21.0;21.4]
      3                   NA (NA)                   NA (NA)       21.0 (1.5)
      4                     0 (0)                     0 (0)            5 (0)
      5                    0 (0%)                    0 (0%)                2
    Code
      x1 = crosstable(mtcars3, c(mpg, gear), by = c(cyl, am, vs), showNA = FALSE)
      x1
    Output
      # A tibble: 7 x 15
        .id   label                   variable `cyl=4 & am=aut~ `cyl=6 & am=aut~ `cyl=8 & am=aut~
        <chr> <chr>                   <chr>    <chr>            <chr>            <chr>           
      1 mpg   Miles/(US) gallon       Min / M~ 21.5 / 21.5      NA / NA          NA / NA         
      2 mpg   Miles/(US) gallon       Med [IQ~ 21.5 [21.5;21.5] NA [NA;NA]       NA [NA;NA]      
      3 mpg   Miles/(US) gallon       Mean (s~ 21.5 (NA)        NA (NA)          NA (NA)         
      4 mpg   Miles/(US) gallon       N (NA)   1 (0)            0 (0)            0 (0)           
      5 gear  Number of forward gears 3        1 (10.00%)       0 (0%)           0 (0%)          
      6 gear  Number of forward gears 4        0 (0%)           0 (0%)           0 (0%)          
      7 gear  Number of forward gears 5        0 (0%)           0 (0%)           0 (0%)          
      # ... with 9 more variables: cyl=4 & am=manual & vs=straight <chr>,
      #   cyl=6 & am=manual & vs=straight <chr>,
      #   cyl=8 & am=manual & vs=straight <chr>, cyl=4 & am=auto & vs=vshaped <chr>,
      #   cyl=6 & am=auto & vs=vshaped <chr>, cyl=8 & am=auto & vs=vshaped <chr>,
      #   cyl=4 & am=manual & vs=vshaped <chr>, cyl=6 & am=manual & vs=vshaped <chr>,
      #   cyl=8 & am=manual & vs=vshaped <chr>
    Code
      as_flextable(x1)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `cyl=4 & am=auto & vs=straight`, `cyl=6 & am=auto & vs=straight`, `cyl=8 & am=auto & vs=straight`, `cyl=4 & am=manual & vs=straight`, `cyl=6 & am=manual & vs=straight`, `cyl=8 & am=manual & vs=straight`, `cyl=4 & am=auto & vs=vshaped`, `cyl=6 & am=auto & vs=vshaped`, `cyl=8 & am=auto & vs=vshaped`, `cyl=4 & am=manual & vs=vshaped`, `cyl=6 & am=manual & vs=vshaped`, `cyl=8 & am=manual & vs=vshaped` 
      header has 3 row(s) 
      body has 7 row(s) 
      original dataset sample: 
         .id                   label   variable cyl=4 & am=auto & vs=straight
      1  mpg       Miles/(US) gallon  Min / Max                   21.5 / 21.5
      2  mpg       Miles/(US) gallon  Med [IQR]              21.5 [21.5;21.5]
      3  mpg       Miles/(US) gallon Mean (std)                     21.5 (NA)
      4  mpg       Miles/(US) gallon     N (NA)                         1 (0)
      5 gear Number of forward gears          3                    1 (10.00%)
        cyl=6 & am=auto & vs=straight cyl=8 & am=auto & vs=straight
      1                       NA / NA                       NA / NA
      2                    NA [NA;NA]                    NA [NA;NA]
      3                       NA (NA)                       NA (NA)
      4                         0 (0)                         0 (0)
      5                        0 (0%)                        0 (0%)
        cyl=4 & am=manual & vs=straight cyl=6 & am=manual & vs=straight
      1                     21.4 / 33.9                         NA / NA
      2                30.4 [28.1;31.9]                      NA [NA;NA]
      3                      29.3 (4.5)                         NA (NA)
      4                           6 (0)                           0 (0)
      5                          0 (0%)                          0 (0%)
        cyl=8 & am=manual & vs=straight cyl=4 & am=auto & vs=vshaped
      1                         NA / NA                      NA / NA
      2                      NA [NA;NA]                   NA [NA;NA]
      3                         NA (NA)                      NA (NA)
      4                           0 (0)                        0 (0)
      5                          0 (0%)                       0 (0%)
        cyl=6 & am=auto & vs=vshaped cyl=8 & am=auto & vs=vshaped
      1                      NA / NA                  10.4 / 19.2
      2                   NA [NA;NA]             15.2 [13.3;15.5]
      3                      NA (NA)                   14.6 (2.9)
      4                        0 (0)                        9 (0)
      5                       0 (0%)                   9 (90.00%)
        cyl=4 & am=manual & vs=vshaped cyl=6 & am=manual & vs=vshaped
      1                    26.0 / 26.0                    19.7 / 19.7
      2               26.0 [26.0;26.0]               19.7 [19.7;19.7]
      3                      26.0 (NA)                      19.7 (NA)
      4                          1 (0)                          1 (0)
      5                         0 (0%)                         0 (0%)
        cyl=8 & am=manual & vs=vshaped
      1                    15.0 / 15.8
      2               15.4 [15.2;15.6]
      3                     15.4 (0.6)
      4                          2 (0)
      5                         0 (0%)
    Code
      x2 = crosstable(mtcars3, c(mpg, gear), by = c(cyl, am, vs), total = TRUE)
      x2
    Output
      # A tibble: 8 x 23
        .id   label                   variable `cyl=4 & am=aut~ `cyl=6 & am=aut~ `cyl=8 & am=aut~
        <chr> <chr>                   <chr>    <chr>            <chr>            <chr>           
      1 mpg   Miles/(US) gallon       Min / M~ 21.5 / 21.5      NA / NA          NA / NA         
      2 mpg   Miles/(US) gallon       Med [IQ~ 21.5 [21.5;21.5] NA [NA;NA]       NA [NA;NA]      
      3 mpg   Miles/(US) gallon       Mean (s~ 21.5 (NA)        NA (NA)          NA (NA)         
      4 mpg   Miles/(US) gallon       N (NA)   1 (0)            0 (0)            0 (0)           
      5 gear  Number of forward gears 3        1 (7.69%)        0 (0%)           0 (0%)          
      6 gear  Number of forward gears 4        0 (0%)           0 (0%)           0 (0%)          
      7 gear  Number of forward gears 5        0 (0%)           0 (0%)           0 (0%)          
      8 gear  Number of forward gears Total    1 (3.70%)        0 (0%)           0 (0%)          
      # ... with 17 more variables: cyl=4 & am=manual & vs=straight <chr>,
      #   cyl=6 & am=manual & vs=straight <chr>,
      #   cyl=8 & am=manual & vs=straight <chr>, cyl=4 & am=auto & vs=vshaped <chr>,
      #   cyl=6 & am=auto & vs=vshaped <chr>, cyl=8 & am=auto & vs=vshaped <chr>,
      #   cyl=4 & am=manual & vs=vshaped <chr>, cyl=6 & am=manual & vs=vshaped <chr>,
      #   cyl=8 & am=manual & vs=vshaped <chr>, cyl=4 & am=auto & vs=NA <chr>,
      #   cyl=6 & am=auto & vs=NA <chr>, cyl=8 & am=auto & vs=NA <chr>, ...
    Code
      as_flextable(x2)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `cyl=4 & am=auto & vs=straight`, `cyl=6 & am=auto & vs=straight`, `cyl=8 & am=auto & vs=straight`, `cyl=4 & am=manual & vs=straight`, `cyl=6 & am=manual & vs=straight`, `cyl=8 & am=manual & vs=straight`, `cyl=4 & am=auto & vs=vshaped`, `cyl=6 & am=auto & vs=vshaped`, `cyl=8 & am=auto & vs=vshaped`, `cyl=4 & am=manual & vs=vshaped`, `cyl=6 & am=manual & vs=vshaped`, `cyl=8 & am=manual & vs=vshaped`, `cyl=4 & am=auto & vs=NA`, `cyl=6 & am=auto & vs=NA`, `cyl=8 & am=auto & vs=NA`, `cyl=4 & am=manual & vs=NA`, `cyl=6 & am=manual & vs=NA`, `cyl=8 & am=manual & vs=NA`, `NA`, `Total` 
      header has 3 row(s) 
      body has 8 row(s) 
      original dataset sample: 
         .id                   label   variable cyl=4 & am=auto & vs=straight
      1  mpg       Miles/(US) gallon  Min / Max                   21.5 / 21.5
      2  mpg       Miles/(US) gallon  Med [IQR]              21.5 [21.5;21.5]
      3  mpg       Miles/(US) gallon Mean (std)                     21.5 (NA)
      4  mpg       Miles/(US) gallon     N (NA)                         1 (0)
      5 gear Number of forward gears          3                     1 (7.69%)
        cyl=6 & am=auto & vs=straight cyl=8 & am=auto & vs=straight
      1                       NA / NA                       NA / NA
      2                    NA [NA;NA]                    NA [NA;NA]
      3                       NA (NA)                       NA (NA)
      4                         0 (0)                         0 (0)
      5                        0 (0%)                        0 (0%)
        cyl=4 & am=manual & vs=straight cyl=6 & am=manual & vs=straight
      1                     21.4 / 33.9                         NA / NA
      2                30.4 [28.1;31.9]                      NA [NA;NA]
      3                      29.3 (4.5)                         NA (NA)
      4                           6 (0)                           0 (0)
      5                          0 (0%)                          0 (0%)
        cyl=8 & am=manual & vs=straight cyl=4 & am=auto & vs=vshaped
      1                         NA / NA                      NA / NA
      2                      NA [NA;NA]                   NA [NA;NA]
      3                         NA (NA)                      NA (NA)
      4                           0 (0)                        0 (0)
      5                          0 (0%)                       0 (0%)
        cyl=6 & am=auto & vs=vshaped cyl=8 & am=auto & vs=vshaped
      1                      NA / NA                  10.4 / 19.2
      2                   NA [NA;NA]             15.2 [13.3;15.5]
      3                      NA (NA)                   14.6 (2.9)
      4                        0 (0)                        9 (0)
      5                       0 (0%)                   9 (69.23%)
        cyl=4 & am=manual & vs=vshaped cyl=6 & am=manual & vs=vshaped
      1                    26.0 / 26.0                    19.7 / 19.7
      2               26.0 [26.0;26.0]               19.7 [19.7;19.7]
      3                      26.0 (NA)                      19.7 (NA)
      4                          1 (0)                          1 (0)
      5                         0 (0%)                         0 (0%)
        cyl=8 & am=manual & vs=vshaped cyl=4 & am=auto & vs=NA
      1                    15.0 / 15.8             22.8 / 24.4
      2               15.4 [15.2;15.6]        23.6 [23.2;24.0]
      3                     15.4 (0.6)              23.6 (1.1)
      4                          2 (0)                   2 (0)
      5                         0 (0%)                  0 (0%)
        cyl=6 & am=auto & vs=NA cyl=8 & am=auto & vs=NA cyl=4 & am=manual & vs=NA
      1             17.8 / 19.2             14.3 / 16.4                   NA / NA
      2        18.1 [18.0;18.6]        15.3 [14.8;15.9]                NA [NA;NA]
      3              18.4 (0.7)              15.3 (1.5)                   NA (NA)
      4                   3 (0)                   2 (0)                     0 (0)
      5               1 (7.69%)              2 (15.38%)                    0 (0%)
        cyl=6 & am=manual & vs=NA cyl=8 & am=manual & vs=NA               NA
      1                   NA / NA                   NA / NA      18.7 / 22.8
      2                NA [NA;NA]                NA [NA;NA] 21.0 [21.0;21.4]
      3                   NA (NA)                   NA (NA)       21.0 (1.5)
      4                     0 (0)                     0 (0)            5 (0)
      5                    0 (0%)                    0 (0%)                2
                   Total
      1      10.4 / 33.9
      2 19.2 [15.4;22.8]
      3       20.1 (6.0)
      4           32 (0)
      5      15 (46.88%)
    Code
      x3 = crosstable(mtcars3, c(mpg, vs, cyl), by = c(am, dummy))
      x3
    Output
      # A tibble: 11 x 5
         .id   label               variable   `am=auto & dummy=dummy` `am=manual & du~
         <chr> <chr>               <chr>      <chr>                   <chr>           
       1 mpg   Miles/(US) gallon   Min / Max  10.4 / 24.4             15.0 / 33.9     
       2 mpg   Miles/(US) gallon   Med [IQR]  17.3 [14.9;19.2]        22.8 [21.0;30.4]
       3 mpg   Miles/(US) gallon   Mean (std) 17.1 (3.8)              24.4 (6.2)      
       4 mpg   Miles/(US) gallon   N (NA)     19 (0)                  13 (0)          
       5 vs    Engine              straight   2 (22.22%)              7 (77.78%)      
       6 vs    Engine              vshaped    9 (60.00%)              6 (40.00%)      
       7 vs    Engine              NA         8                       0               
       8 cyl   Number of cylinders 4          3 (30.00%)              7 (70.00%)      
       9 cyl   Number of cylinders 6          3 (75.00%)              1 (25.00%)      
      10 cyl   Number of cylinders 8          11 (84.62%)             2 (15.38%)      
      11 cyl   Number of cylinders NA         2                       3               
    Code
      as_flextable(x3)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `am=auto & dummy=dummy`, `am=manual & dummy=dummy` 
      header has 2 row(s) 
      body has 11 row(s) 
      original dataset sample: 
        .id             label   variable am=auto & dummy=dummy
      1 mpg Miles/(US) gallon  Min / Max           10.4 / 24.4
      2 mpg Miles/(US) gallon  Med [IQR]      17.3 [14.9;19.2]
      3 mpg Miles/(US) gallon Mean (std)            17.1 (3.8)
      4 mpg Miles/(US) gallon     N (NA)                19 (0)
      5  vs            Engine   straight            2 (22.22%)
        am=manual & dummy=dummy
      1             15.0 / 33.9
      2        22.8 [21.0;30.4]
      3              24.4 (6.2)
      4                  13 (0)
      5              7 (77.78%)
    Code
      x4 = crosstable(mtcars3, c(mpg, vs, cyl, dummy, surv, hp_date, qsec_posix, diff,
        cyl3), by = c(am, gear), total = TRUE, times = c(100, 200), followup = TRUE)
      x4
    Output
      # A tibble: 34 x 10
         .id   label               variable `am=auto & gear~ `am=manual & ge~ `am=auto & gear~
         <chr> <chr>               <chr>    <chr>            <chr>            <chr>           
       1 mpg   Miles/(US) gallon   Min / M~ 10.4 / 21.5      NA / NA          17.8 / 24.4     
       2 mpg   Miles/(US) gallon   Med [IQ~ 15.5 [14.5;18.4] NA [NA;NA]       21.0 [18.8;23.2]
       3 mpg   Miles/(US) gallon   Mean (s~ 16.1 (3.4)       NA (NA)          21.1 (3.1)      
       4 mpg   Miles/(US) gallon   N (NA)   15 (0)           0 (0)            4 (0)           
       5 vs    Engine              straight 2 (22.22%)       0 (0%)           0 (0%)          
       6 vs    Engine              vshaped  9 (60.00%)       0 (0%)           0 (0%)          
       7 vs    Engine              NA       4                0                4               
       8 vs    Engine              Total    15 (45.83%)      0 (0%)           4 (0%)          
       9 cyl   Number of cylinders 4        1 (10.00%)       0 (0%)           2 (20.00%)      
      10 cyl   Number of cylinders 6        1 (25.00%)       0 (0%)           2 (50.00%)      
      # ... with 24 more rows, and 4 more variables: am=manual & gear=4 <chr>,
      #   am=auto & gear=5 <chr>, am=manual & gear=5 <chr>, Total <chr>
    Code
      as_flextable(x4)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `am=auto & gear=3`, `am=manual & gear=3`, `am=auto & gear=4`, `am=manual & gear=4`, `am=auto & gear=5`, `am=manual & gear=5`, `Total` 
      header has 2 row(s) 
      body has 34 row(s) 
      original dataset sample: 
        .id             label   variable am=auto & gear=3 am=manual & gear=3
      1 mpg Miles/(US) gallon  Min / Max      10.4 / 21.5            NA / NA
      2 mpg Miles/(US) gallon  Med [IQR] 15.5 [14.5;18.4]         NA [NA;NA]
      3 mpg Miles/(US) gallon Mean (std)       16.1 (3.4)            NA (NA)
      4 mpg Miles/(US) gallon     N (NA)           15 (0)              0 (0)
      5  vs            Engine   straight       2 (22.22%)             0 (0%)
        am=auto & gear=4 am=manual & gear=4 am=auto & gear=5 am=manual & gear=5
      1      17.8 / 24.4        21.0 / 33.9          NA / NA        15.0 / 30.4
      2 21.0 [18.8;23.2]   25.1 [21.3;30.9]       NA [NA;NA]   19.7 [15.8;26.0]
      3       21.1 (3.1)         26.3 (5.4)          NA (NA)         21.4 (6.7)
      4            4 (0)              8 (0)            0 (0)              5 (0)
      5           0 (0%)         6 (66.67%)           0 (0%)         1 (11.11%)
                   Total
      1      10.4 / 33.9
      2 19.2 [15.4;22.8]
      3       20.1 (6.0)
      4           32 (0)
      5       9 (37.50%)

# By multiple formula interface

    Code
      x1 = crosstable(mtcars3, mpg + gear ~ cyl + I(am == "auto") + vs, total = TRUE)
      x1 %>% as_flextable()
    Output
      a flextable object.
      col_keys: `label`, `variable`, `cyl=4 & I(am == "auto")=FALSE & vs=straight`, `cyl=6 & I(am == "auto")=FALSE & vs=straight`, `cyl=8 & I(am == "auto")=FALSE & vs=straight`, `cyl=4 & I(am == "auto")=TRUE & vs=straight`, `cyl=6 & I(am == "auto")=TRUE & vs=straight`, `cyl=8 & I(am == "auto")=TRUE & vs=straight`, `cyl=4 & I(am == "auto")=FALSE & vs=vshaped`, `cyl=6 & I(am == "auto")=FALSE & vs=vshaped`, `cyl=8 & I(am == "auto")=FALSE & vs=vshaped`, `cyl=4 & I(am == "auto")=TRUE & vs=vshaped`, `cyl=6 & I(am == "auto")=TRUE & vs=vshaped`, `cyl=8 & I(am == "auto")=TRUE & vs=vshaped`, `cyl=4 & I(am == "auto")=FALSE & vs=NA`, `cyl=6 & I(am == "auto")=FALSE & vs=NA`, `cyl=8 & I(am == "auto")=FALSE & vs=NA`, `cyl=4 & I(am == "auto")=TRUE & vs=NA`, `cyl=6 & I(am == "auto")=TRUE & vs=NA`, `cyl=8 & I(am == "auto")=TRUE & vs=NA`, `NA`, `Total` 
      header has 3 row(s) 
      body has 8 row(s) 
      original dataset sample: 
         .id                   label   variable
      1  mpg       Miles/(US) gallon  Min / Max
      2  mpg       Miles/(US) gallon  Med [IQR]
      3  mpg       Miles/(US) gallon Mean (std)
      4  mpg       Miles/(US) gallon     N (NA)
      5 gear Number of forward gears          3
        cyl=4 & I(am == "auto")=FALSE & vs=straight
      1                                 21.4 / 33.9
      2                            30.4 [28.1;31.9]
      3                                  29.3 (4.5)
      4                                       6 (0)
      5                                      0 (0%)
        cyl=6 & I(am == "auto")=FALSE & vs=straight
      1                                     NA / NA
      2                                  NA [NA;NA]
      3                                     NA (NA)
      4                                       0 (0)
      5                                      0 (0%)
        cyl=8 & I(am == "auto")=FALSE & vs=straight
      1                                     NA / NA
      2                                  NA [NA;NA]
      3                                     NA (NA)
      4                                       0 (0)
      5                                      0 (0%)
        cyl=4 & I(am == "auto")=TRUE & vs=straight
      1                                21.5 / 21.5
      2                           21.5 [21.5;21.5]
      3                                  21.5 (NA)
      4                                      1 (0)
      5                                  1 (7.69%)
        cyl=6 & I(am == "auto")=TRUE & vs=straight
      1                                    NA / NA
      2                                 NA [NA;NA]
      3                                    NA (NA)
      4                                      0 (0)
      5                                     0 (0%)
        cyl=8 & I(am == "auto")=TRUE & vs=straight
      1                                    NA / NA
      2                                 NA [NA;NA]
      3                                    NA (NA)
      4                                      0 (0)
      5                                     0 (0%)
        cyl=4 & I(am == "auto")=FALSE & vs=vshaped
      1                                26.0 / 26.0
      2                           26.0 [26.0;26.0]
      3                                  26.0 (NA)
      4                                      1 (0)
      5                                     0 (0%)
        cyl=6 & I(am == "auto")=FALSE & vs=vshaped
      1                                19.7 / 19.7
      2                           19.7 [19.7;19.7]
      3                                  19.7 (NA)
      4                                      1 (0)
      5                                     0 (0%)
        cyl=8 & I(am == "auto")=FALSE & vs=vshaped
      1                                15.0 / 15.8
      2                           15.4 [15.2;15.6]
      3                                 15.4 (0.6)
      4                                      2 (0)
      5                                     0 (0%)
        cyl=4 & I(am == "auto")=TRUE & vs=vshaped
      1                                   NA / NA
      2                                NA [NA;NA]
      3                                   NA (NA)
      4                                     0 (0)
      5                                    0 (0%)
        cyl=6 & I(am == "auto")=TRUE & vs=vshaped
      1                                   NA / NA
      2                                NA [NA;NA]
      3                                   NA (NA)
      4                                     0 (0)
      5                                    0 (0%)
        cyl=8 & I(am == "auto")=TRUE & vs=vshaped
      1                               10.4 / 19.2
      2                          15.2 [13.3;15.5]
      3                                14.6 (2.9)
      4                                     9 (0)
      5                                9 (69.23%)
        cyl=4 & I(am == "auto")=FALSE & vs=NA cyl=6 & I(am == "auto")=FALSE & vs=NA
      1                               NA / NA                               NA / NA
      2                            NA [NA;NA]                            NA [NA;NA]
      3                               NA (NA)                               NA (NA)
      4                                 0 (0)                                 0 (0)
      5                                0 (0%)                                0 (0%)
        cyl=8 & I(am == "auto")=FALSE & vs=NA cyl=4 & I(am == "auto")=TRUE & vs=NA
      1                               NA / NA                          22.8 / 24.4
      2                            NA [NA;NA]                     23.6 [23.2;24.0]
      3                               NA (NA)                           23.6 (1.1)
      4                                 0 (0)                                2 (0)
      5                                0 (0%)                               0 (0%)
        cyl=6 & I(am == "auto")=TRUE & vs=NA cyl=8 & I(am == "auto")=TRUE & vs=NA
      1                          17.8 / 19.2                          14.3 / 16.4
      2                     18.1 [18.0;18.6]                     15.3 [14.8;15.9]
      3                           18.4 (0.7)                           15.3 (1.5)
      4                                3 (0)                                2 (0)
      5                            1 (7.69%)                           2 (15.38%)
                      NA            Total
      1      18.7 / 22.8      10.4 / 33.9
      2 21.0 [21.0;21.4] 19.2 [15.4;22.8]
      3       21.0 (1.5)       20.1 (6.0)
      4            5 (0)           32 (0)
      5                2      15 (46.88%)

