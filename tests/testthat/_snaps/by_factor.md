# showNA with NA in by

    Code
      x0 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, times = c(0, 100, 200,
        400))
      as.data.frame(x0)
    Output
          .id                    label        variable         straight
      1    am             Transmission            auto       2 (18.18%)
      2    am             Transmission          manual       7 (53.85%)
      3   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9
      4   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4]
      5   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)
      6   mpg        Miles/(US) gallon          N (NA)            9 (0)
      7   cyl      Number of cylinders               4       7 (87.50%)
      8   cyl      Number of cylinders               6           0 (0%)
      9   cyl      Number of cylinders               8           0 (0%)
      10  cyl      Number of cylinders              NA                2
      11 surv Dummy survival (disp/am)             t=0       1.00 (0/9)
      12 surv Dummy survival (disp/am)           t=100       0.44 (5/4)
      13 surv Dummy survival (disp/am)           t=200       0.17 (2/1)
      14 surv Dummy survival (disp/am)           t=400       0.17 (0/0)
      15 surv Dummy survival (disp/am) Median survival             95.1
                  vshaped               NA
      1        9 (81.82%)                8
      2        6 (46.15%)                0
      3       10.4 / 26.0      14.3 / 24.4
      4  15.5 [14.8;19.4] 18.4 [17.5;20.1]
      5        16.6 (4.2)       19.0 (3.3)
      6            15 (0)            8 (0)
      7        1 (12.50%)                2
      8       1 (100.00%)                3
      9      11 (100.00%)                2
      10                2                1
      11      1.00 (0/15)       1.00 (0/8)
      12      1.00 (0/15)       1.00 (0/8)
      13      0.73 (4/11)       1.00 (0/4)
      14       0.52 (2/4)       1.00 (0/0)
      15             <NA>             <NA>
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
      as.data.frame(x1)
    Output
          .id                    label        variable         straight
      1    am             Transmission            auto       2 (18.18%)
      2    am             Transmission          manual       7 (53.85%)
      3   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9
      4   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4]
      5   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)
      6   mpg        Miles/(US) gallon          N (NA)            9 (0)
      7   cyl      Number of cylinders               4       7 (87.50%)
      8   cyl      Number of cylinders               6           0 (0%)
      9   cyl      Number of cylinders               8           0 (0%)
      10 surv Dummy survival (disp/am)             t=0       1.00 (0/9)
      11 surv Dummy survival (disp/am)           t=100       0.44 (5/4)
      12 surv Dummy survival (disp/am)           t=200       0.17 (2/1)
      13 surv Dummy survival (disp/am)           t=400       0.17 (0/0)
      14 surv Dummy survival (disp/am) Median survival             95.1
                  vshaped
      1        9 (81.82%)
      2        6 (46.15%)
      3       10.4 / 26.0
      4  15.5 [14.8;19.4]
      5        16.6 (4.2)
      6            15 (0)
      7        1 (12.50%)
      8       1 (100.00%)
      9      11 (100.00%)
      10      1.00 (0/15)
      11      1.00 (0/15)
      12      0.73 (4/11)
      13       0.52 (2/4)
      14             <NA>
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
      as.data.frame(x2)
    Output
          .id                    label        variable         straight
      1    am             Transmission            auto       2 (18.18%)
      2    am             Transmission          manual       7 (53.85%)
      3   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9
      4   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4]
      5   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)
      6   mpg        Miles/(US) gallon          N (NA)            9 (0)
      7   cyl      Number of cylinders               4       7 (87.50%)
      8   cyl      Number of cylinders               6           0 (0%)
      9   cyl      Number of cylinders               8           0 (0%)
      10  cyl      Number of cylinders              NA                2
      11 surv Dummy survival (disp/am)             t=0       1.00 (0/9)
      12 surv Dummy survival (disp/am)           t=100       0.44 (5/4)
      13 surv Dummy survival (disp/am)           t=200       0.17 (2/1)
      14 surv Dummy survival (disp/am)           t=400       0.17 (0/0)
      15 surv Dummy survival (disp/am) Median survival             95.1
                  vshaped               NA
      1        9 (81.82%)                8
      2        6 (46.15%)                0
      3       10.4 / 26.0      14.3 / 24.4
      4  15.5 [14.8;19.4] 18.4 [17.5;20.1]
      5        16.6 (4.2)       19.0 (3.3)
      6            15 (0)            8 (0)
      7        1 (12.50%)                2
      8       1 (100.00%)                3
      9      11 (100.00%)                2
      10                2                1
      11      1.00 (0/15)       1.00 (0/8)
      12      1.00 (0/15)       1.00 (0/8)
      13      0.73 (4/11)       1.00 (0/4)
      14       0.52 (2/4)       1.00 (0/0)
      15             <NA>             <NA>
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
      as.data.frame(x3)
    Output
          .id                    label        variable         straight
      1    am             Transmission            auto       2 (18.18%)
      2    am             Transmission          manual       7 (53.85%)
      3    am             Transmission              NA                0
      4   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9
      5   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4]
      6   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)
      7   mpg        Miles/(US) gallon          N (NA)            9 (0)
      8   cyl      Number of cylinders               4       7 (87.50%)
      9   cyl      Number of cylinders               6           0 (0%)
      10  cyl      Number of cylinders               8           0 (0%)
      11  cyl      Number of cylinders              NA                2
      12 surv Dummy survival (disp/am)             t=0       1.00 (0/9)
      13 surv Dummy survival (disp/am)           t=100       0.44 (5/4)
      14 surv Dummy survival (disp/am)           t=200       0.17 (2/1)
      15 surv Dummy survival (disp/am)           t=400       0.17 (0/0)
      16 surv Dummy survival (disp/am) Median survival             95.1
                  vshaped               NA
      1        9 (81.82%)                8
      2        6 (46.15%)                0
      3                 0                0
      4       10.4 / 26.0      14.3 / 24.4
      5  15.5 [14.8;19.4] 18.4 [17.5;20.1]
      6        16.6 (4.2)       19.0 (3.3)
      7            15 (0)            8 (0)
      8        1 (12.50%)                2
      9       1 (100.00%)                3
      10     11 (100.00%)                2
      11                2                1
      12      1.00 (0/15)       1.00 (0/8)
      13      1.00 (0/15)       1.00 (0/8)
      14      0.73 (4/11)       1.00 (0/4)
      15       0.52 (2/4)       1.00 (0/0)
      16             <NA>             <NA>
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
      as.data.frame(x0)
    Output
          .id                    label        variable             auto
      1    vs                   Engine        straight       2 (22.22%)
      2    vs                   Engine         vshaped       9 (60.00%)
      3    vs                   Engine              NA                8
      4   mpg        Miles/(US) gallon       Min / Max      10.4 / 24.4
      5   mpg        Miles/(US) gallon       Med [IQR] 17.3 [14.9;19.2]
      6   mpg        Miles/(US) gallon      Mean (std)       17.1 (3.8)
      7   mpg        Miles/(US) gallon          N (NA)           19 (0)
      8   cyl      Number of cylinders               4       3 (30.00%)
      9   cyl      Number of cylinders               6       3 (75.00%)
      10  cyl      Number of cylinders               8      11 (84.62%)
      11  cyl      Number of cylinders              NA                2
      12 surv Dummy survival (disp/am)             t=0      1.00 (0/19)
      13 surv Dummy survival (disp/am)           t=100      1.00 (0/19)
      14 surv Dummy survival (disp/am)           t=200      1.00 (0/14)
      15 surv Dummy survival (disp/am)           t=400       1.00 (0/4)
      16 surv Dummy survival (disp/am) Median survival             <NA>
                   manual
      1        7 (77.78%)
      2        6 (40.00%)
      3                 0
      4       15.0 / 33.9
      5  22.8 [21.0;30.4]
      6        24.4 (6.2)
      7            13 (0)
      8        7 (70.00%)
      9        1 (25.00%)
      10       2 (15.38%)
      11                3
      12      1.00 (0/13)
      13       0.62 (5/8)
      14       0.15 (6/2)
      15          0 (2/0)
      16            120.3
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
      as.data.frame(x1)
    Output
          .id                    label        variable             auto
      1    vs                   Engine        straight       2 (22.22%)
      2    vs                   Engine         vshaped       9 (60.00%)
      3   mpg        Miles/(US) gallon       Min / Max      10.4 / 24.4
      4   mpg        Miles/(US) gallon       Med [IQR] 17.3 [14.9;19.2]
      5   mpg        Miles/(US) gallon      Mean (std)       17.1 (3.8)
      6   mpg        Miles/(US) gallon          N (NA)           19 (0)
      7   cyl      Number of cylinders               4       3 (30.00%)
      8   cyl      Number of cylinders               6       3 (75.00%)
      9   cyl      Number of cylinders               8      11 (84.62%)
      10 surv Dummy survival (disp/am)             t=0      1.00 (0/19)
      11 surv Dummy survival (disp/am)           t=100      1.00 (0/19)
      12 surv Dummy survival (disp/am)           t=200      1.00 (0/14)
      13 surv Dummy survival (disp/am)           t=400       1.00 (0/4)
      14 surv Dummy survival (disp/am) Median survival             <NA>
                   manual
      1        7 (77.78%)
      2        6 (40.00%)
      3       15.0 / 33.9
      4  22.8 [21.0;30.4]
      5        24.4 (6.2)
      6            13 (0)
      7        7 (70.00%)
      8        1 (25.00%)
      9        2 (15.38%)
      10      1.00 (0/13)
      11       0.62 (5/8)
      12       0.15 (6/2)
      13          0 (2/0)
      14            120.3
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
      as.data.frame(x2)
    Output
          .id                    label        variable             auto
      1    vs                   Engine        straight       2 (22.22%)
      2    vs                   Engine         vshaped       9 (60.00%)
      3    vs                   Engine              NA                8
      4   mpg        Miles/(US) gallon       Min / Max      10.4 / 24.4
      5   mpg        Miles/(US) gallon       Med [IQR] 17.3 [14.9;19.2]
      6   mpg        Miles/(US) gallon      Mean (std)       17.1 (3.8)
      7   mpg        Miles/(US) gallon          N (NA)           19 (0)
      8   cyl      Number of cylinders               4       3 (30.00%)
      9   cyl      Number of cylinders               6       3 (75.00%)
      10  cyl      Number of cylinders               8      11 (84.62%)
      11  cyl      Number of cylinders              NA                2
      12 surv Dummy survival (disp/am)             t=0      1.00 (0/19)
      13 surv Dummy survival (disp/am)           t=100      1.00 (0/19)
      14 surv Dummy survival (disp/am)           t=200      1.00 (0/14)
      15 surv Dummy survival (disp/am)           t=400       1.00 (0/4)
      16 surv Dummy survival (disp/am) Median survival             <NA>
                   manual
      1        7 (77.78%)
      2        6 (40.00%)
      3                 0
      4       15.0 / 33.9
      5  22.8 [21.0;30.4]
      6        24.4 (6.2)
      7            13 (0)
      8        7 (70.00%)
      9        1 (25.00%)
      10       2 (15.38%)
      11                3
      12      1.00 (0/13)
      13       0.62 (5/8)
      14       0.15 (6/2)
      15          0 (2/0)
      16            120.3
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
      as.data.frame(x3)
    Output
          .id                    label        variable             auto
      1    vs                   Engine        straight       2 (22.22%)
      2    vs                   Engine         vshaped       9 (60.00%)
      3    vs                   Engine              NA                8
      4   mpg        Miles/(US) gallon       Min / Max      10.4 / 24.4
      5   mpg        Miles/(US) gallon       Med [IQR] 17.3 [14.9;19.2]
      6   mpg        Miles/(US) gallon      Mean (std)       17.1 (3.8)
      7   mpg        Miles/(US) gallon          N (NA)           19 (0)
      8   cyl      Number of cylinders               4       3 (30.00%)
      9   cyl      Number of cylinders               6       3 (75.00%)
      10  cyl      Number of cylinders               8      11 (84.62%)
      11  cyl      Number of cylinders              NA                2
      12 surv Dummy survival (disp/am)             t=0      1.00 (0/19)
      13 surv Dummy survival (disp/am)           t=100      1.00 (0/19)
      14 surv Dummy survival (disp/am)           t=200      1.00 (0/14)
      15 surv Dummy survival (disp/am)           t=400       1.00 (0/4)
      16 surv Dummy survival (disp/am) Median survival             <NA>
                   manual    NA
      1        7 (77.78%)     0
      2        6 (40.00%)     0
      3                 0     0
      4       15.0 / 33.9 no NA
      5  22.8 [21.0;30.4] no NA
      6        24.4 (6.2) no NA
      7            13 (0) no NA
      8        7 (70.00%)     0
      9        1 (25.00%)     0
      10       2 (15.38%)     0
      11                3     0
      12      1.00 (0/13)  <NA>
      13       0.62 (5/8)  <NA>
      14       0.15 (6/2)  <NA>
      15          0 (2/0)  <NA>
      16            120.3  <NA>
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
      as.data.frame(x0)
    Output
          .id                    label        variable         straight
      1    am             Transmission            auto       2 (18.18%)
      2    am             Transmission          manual       7 (53.85%)
      3   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9
      4   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4]
      5   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)
      6   mpg        Miles/(US) gallon          N (NA)            9 (0)
      7   cyl      Number of cylinders               4       7 (87.50%)
      8   cyl      Number of cylinders               6           0 (0%)
      9   cyl      Number of cylinders               8           0 (0%)
      10  cyl      Number of cylinders              NA                2
      11 surv Dummy survival (disp/am)             t=0       1.00 (0/9)
      12 surv Dummy survival (disp/am)           t=100       0.44 (5/4)
      13 surv Dummy survival (disp/am)           t=200       0.17 (2/1)
      14 surv Dummy survival (disp/am)           t=400       0.17 (0/0)
      15 surv Dummy survival (disp/am) Median survival             95.1
                  vshaped               NA
      1        9 (81.82%)                8
      2        6 (46.15%)                0
      3       10.4 / 26.0      14.3 / 24.4
      4  15.5 [14.8;19.4] 18.4 [17.5;20.1]
      5        16.6 (4.2)       19.0 (3.3)
      6            15 (0)            8 (0)
      7        1 (12.50%)                2
      8       1 (100.00%)                3
      9      11 (100.00%)                2
      10                2                1
      11      1.00 (0/15)       1.00 (0/8)
      12      1.00 (0/15)       1.00 (0/8)
      13      0.73 (4/11)       1.00 (0/4)
      14       0.52 (2/4)       1.00 (0/0)
      15             <NA>             <NA>
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
      as.data.frame(x1)
    Output
          .id                    label        variable         straight
      1    am             Transmission            auto       2 (18.18%)
      2    am             Transmission          manual       7 (53.85%)
      3   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9
      4   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4]
      5   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)
      6   mpg        Miles/(US) gallon          N (NA)            9 (0)
      7   cyl      Number of cylinders               4       7 (87.50%)
      8   cyl      Number of cylinders               6           0 (0%)
      9   cyl      Number of cylinders               8           0 (0%)
      10  cyl      Number of cylinders              NA                2
      11 surv Dummy survival (disp/am)             t=0       1.00 (0/9)
      12 surv Dummy survival (disp/am)           t=100       0.44 (5/4)
      13 surv Dummy survival (disp/am)           t=200       0.17 (2/1)
      14 surv Dummy survival (disp/am)           t=400       0.17 (0/0)
      15 surv Dummy survival (disp/am) Median survival             95.1
                  vshaped               NA
      1        9 (81.82%)                8
      2        6 (46.15%)                0
      3       10.4 / 26.0      14.3 / 24.4
      4  15.5 [14.8;19.4] 18.4 [17.5;20.1]
      5        16.6 (4.2)       19.0 (3.3)
      6            15 (0)            8 (0)
      7        1 (12.50%)                2
      8       1 (100.00%)                3
      9      11 (100.00%)                2
      10                2                1
      11      1.00 (0/15)       1.00 (0/8)
      12      1.00 (0/15)       1.00 (0/8)
      13      0.73 (4/11)       1.00 (0/4)
      14       0.52 (2/4)       1.00 (0/0)
      15             <NA>             <NA>
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
      as.data.frame(x2)
    Output
          .id                    label        variable         straight
      1    am             Transmission            auto       2 (18.18%)
      2    am             Transmission          manual       7 (53.85%)
      3   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9
      4   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4]
      5   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)
      6   mpg        Miles/(US) gallon          N (NA)            9 (0)
      7   cyl      Number of cylinders               4       7 (87.50%)
      8   cyl      Number of cylinders               6           0 (0%)
      9   cyl      Number of cylinders               8           0 (0%)
      10  cyl      Number of cylinders              NA                2
      11 surv Dummy survival (disp/am)             t=0       1.00 (0/9)
      12 surv Dummy survival (disp/am)           t=100       0.44 (5/4)
      13 surv Dummy survival (disp/am)           t=200       0.17 (2/1)
      14 surv Dummy survival (disp/am)           t=400       0.17 (0/0)
      15 surv Dummy survival (disp/am) Median survival             95.1
                  vshaped               NA            Total
      1        9 (81.82%)                8      19 (59.38%)
      2        6 (46.15%)                0      13 (40.62%)
      3       10.4 / 26.0      14.3 / 24.4      10.4 / 33.9
      4  15.5 [14.8;19.4] 18.4 [17.5;20.1] 19.2 [15.4;22.8]
      5        16.6 (4.2)       19.0 (3.3)       20.1 (6.0)
      6            15 (0)            8 (0)           32 (0)
      7        1 (12.50%)                2      10 (37.04%)
      8       1 (100.00%)                3       4 (14.81%)
      9      11 (100.00%)                2      13 (48.15%)
      10                2                1                5
      11      1.00 (0/15)       1.00 (0/8)      1.00 (0/32)
      12      1.00 (0/15)       1.00 (0/8)      0.84 (5/27)
      13      0.73 (4/11)       1.00 (0/4)      0.64 (6/16)
      14       0.52 (2/4)       1.00 (0/0)       0.50 (2/4)
      15             <NA>             <NA>             <NA>
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
      as.data.frame(x3)
    Output
          .id                    label        variable         straight
      1    am             Transmission            auto       2 (18.18%)
      2    am             Transmission          manual       7 (53.85%)
      3    am             Transmission           Total       9 (37.50%)
      4   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9
      5   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4]
      6   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)
      7   mpg        Miles/(US) gallon          N (NA)            9 (0)
      8   cyl      Number of cylinders               4       7 (87.50%)
      9   cyl      Number of cylinders               6           0 (0%)
      10  cyl      Number of cylinders               8           0 (0%)
      11  cyl      Number of cylinders              NA                2
      12  cyl      Number of cylinders           Total       9 (35.00%)
      13 surv Dummy survival (disp/am)             t=0       1.00 (0/9)
      14 surv Dummy survival (disp/am)           t=100       0.44 (5/4)
      15 surv Dummy survival (disp/am)           t=200       0.17 (2/1)
      16 surv Dummy survival (disp/am)           t=400       0.17 (0/0)
      17 surv Dummy survival (disp/am) Median survival             95.1
                  vshaped               NA
      1        9 (81.82%)                8
      2        6 (46.15%)                0
      3       15 (62.50%)                8
      4       10.4 / 26.0      14.3 / 24.4
      5  15.5 [14.8;19.4] 18.4 [17.5;20.1]
      6        16.6 (4.2)       19.0 (3.3)
      7            15 (0)            8 (0)
      8        1 (12.50%)                2
      9       1 (100.00%)                3
      10     11 (100.00%)                2
      11                2                1
      12      15 (65.00%)                8
      13      1.00 (0/15)       1.00 (0/8)
      14      1.00 (0/15)       1.00 (0/8)
      15      0.73 (4/11)       1.00 (0/4)
      16       0.52 (2/4)       1.00 (0/0)
      17             <NA>             <NA>
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
      as.data.frame(x4)
    Output
          .id                    label        variable         straight
      1    am             Transmission            auto       2 (18.18%)
      2    am             Transmission          manual       7 (53.85%)
      3    am             Transmission           Total       9 (37.50%)
      4   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9
      5   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4]
      6   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)
      7   mpg        Miles/(US) gallon          N (NA)            9 (0)
      8   cyl      Number of cylinders               4       7 (87.50%)
      9   cyl      Number of cylinders               6           0 (0%)
      10  cyl      Number of cylinders               8           0 (0%)
      11  cyl      Number of cylinders              NA                2
      12  cyl      Number of cylinders           Total       9 (35.00%)
      13 surv Dummy survival (disp/am)             t=0       1.00 (0/9)
      14 surv Dummy survival (disp/am)           t=100       0.44 (5/4)
      15 surv Dummy survival (disp/am)           t=200       0.17 (2/1)
      16 surv Dummy survival (disp/am)           t=400       0.17 (0/0)
      17 surv Dummy survival (disp/am) Median survival             95.1
                  vshaped               NA            Total
      1        9 (81.82%)                8      19 (59.38%)
      2        6 (46.15%)                0      13 (40.62%)
      3       15 (62.50%)                8     32 (100.00%)
      4       10.4 / 26.0      14.3 / 24.4      10.4 / 33.9
      5  15.5 [14.8;19.4] 18.4 [17.5;20.1] 19.2 [15.4;22.8]
      6        16.6 (4.2)       19.0 (3.3)       20.1 (6.0)
      7            15 (0)            8 (0)           32 (0)
      8        1 (12.50%)                2      10 (37.04%)
      9       1 (100.00%)                3       4 (14.81%)
      10     11 (100.00%)                2      13 (48.15%)
      11                2                1                5
      12      15 (65.00%)                8     32 (100.00%)
      13      1.00 (0/15)       1.00 (0/8)      1.00 (0/32)
      14      1.00 (0/15)       1.00 (0/8)      0.84 (5/27)
      15      0.73 (4/11)       1.00 (0/4)      0.64 (6/16)
      16       0.52 (2/4)       1.00 (0/0)       0.50 (2/4)
      17             <NA>             <NA>             <NA>
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
      as.data.frame(x0)
    Output
        .id               label variable   straight      vshaped NA
      1  am        Transmission     auto 2 (18.18%)   9 (81.82%)  8
      2  am        Transmission   manual 7 (53.85%)   6 (46.15%)  0
      3 cyl Number of cylinders        4 7 (87.50%)   1 (12.50%)  2
      4 cyl Number of cylinders        6     0 (0%)  1 (100.00%)  3
      5 cyl Number of cylinders        8     0 (0%) 11 (100.00%)  2
      6 cyl Number of cylinders       NA          2            2  1
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
      as.data.frame(x1)
    Output
        .id               label variable   straight      vshaped NA
      1  am        Transmission     auto 2 (18.18%)   9 (81.82%)  8
      2  am        Transmission   manual 7 (53.85%)   6 (46.15%)  0
      3 cyl Number of cylinders        4 7 (87.50%)   1 (12.50%)  2
      4 cyl Number of cylinders        6     0 (0%)  1 (100.00%)  3
      5 cyl Number of cylinders        8     0 (0%) 11 (100.00%)  2
      6 cyl Number of cylinders       NA          2            2  1
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
      as.data.frame(x2)
    Output
        .id               label variable    straight     vshaped NA
      1  am        Transmission     auto  2 (22.22%)  9 (60.00%)  8
      2  am        Transmission   manual  7 (77.78%)  6 (40.00%)  0
      3 cyl Number of cylinders        4 7 (100.00%)   1 (7.69%)  2
      4 cyl Number of cylinders        6      0 (0%)   1 (7.69%)  3
      5 cyl Number of cylinders        8      0 (0%) 11 (84.62%)  2
      6 cyl Number of cylinders       NA           2           2  1
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
      as.data.frame(x3)
    Output
        .id               label variable   straight     vshaped NA
      1  am        Transmission     auto  2 (8.33%)  9 (37.50%)  8
      2  am        Transmission   manual 7 (29.17%)  6 (25.00%)  0
      3 cyl Number of cylinders        4 7 (35.00%)   1 (5.00%)  2
      4 cyl Number of cylinders        6     0 (0%)   1 (5.00%)  3
      5 cyl Number of cylinders        8     0 (0%) 11 (55.00%)  2
      6 cyl Number of cylinders       NA          2           2  1
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
      as.data.frame(x4)
    Output
        .id               label variable straight vshaped NA
      1  am        Transmission     auto        2       9  8
      2  am        Transmission   manual        7       6  0
      3 cyl Number of cylinders        4        7       1  2
      4 cyl Number of cylinders        6        0       1  3
      5 cyl Number of cylinders        8        0      11  2
      6 cyl Number of cylinders       NA        2       2  1
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
      as.data.frame(x5)
    Output
        .id               label variable                      straight
      1  am        Transmission     auto   2 (8.33% / 18.18% / 22.22%)
      2  am        Transmission   manual  7 (29.17% / 53.85% / 77.78%)
      3 cyl Number of cylinders        4 7 (35.00% / 87.50% / 100.00%)
      4 cyl Number of cylinders        6              0 (0% / 0% / 0%)
      5 cyl Number of cylinders        8              0 (0% / 0% / 0%)
      6 cyl Number of cylinders       NA                             2
                               vshaped NA
      1   9 (37.50% / 81.82% / 60.00%)  8
      2   6 (25.00% / 46.15% / 40.00%)  0
      3     1 (5.00% / 12.50% / 7.69%)  2
      4    1 (5.00% / 100.00% / 7.69%)  3
      5 11 (55.00% / 100.00% / 84.62%)  2
      6                              2  1
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
      as.data.frame(x6)
    Output
        .id               label variable             straight               vshaped
      1  am        Transmission     auto  2 (18.18% / 22.22%)   9 (81.82% / 60.00%)
      2  am        Transmission   manual  7 (53.85% / 77.78%)   6 (46.15% / 40.00%)
      3 cyl Number of cylinders        4 7 (87.50% / 100.00%)    1 (12.50% / 7.69%)
      4 cyl Number of cylinders        6          0 (0% / 0%)   1 (100.00% / 7.69%)
      5 cyl Number of cylinders        8          0 (0% / 0%) 11 (100.00% / 84.62%)
      6 cyl Number of cylinders       NA                    2                     2
        NA
      1  8
      2  0
      3  2
      4  3
      5  2
      6  1
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
      as.data.frame(x0)
    Output
        .id               label variable   straight      vshaped NA        Total
      1  am        Transmission     auto 2 (18.18%)   9 (81.82%)  8  19 (59.38%)
      2  am        Transmission   manual 7 (53.85%)   6 (46.15%)  0  13 (40.62%)
      3  am        Transmission    Total 9 (37.50%)  15 (62.50%)  8 32 (100.00%)
      4 cyl Number of cylinders        4 7 (87.50%)   1 (12.50%)  2  10 (37.04%)
      5 cyl Number of cylinders        6     0 (0%)  1 (100.00%)  3   4 (14.81%)
      6 cyl Number of cylinders        8     0 (0%) 11 (100.00%)  2  13 (48.15%)
      7 cyl Number of cylinders       NA          2            2  1            5
      8 cyl Number of cylinders    Total 9 (35.00%)  15 (65.00%)  8 32 (100.00%)
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
      as.data.frame(x1)
    Output
        .id               label variable   straight      vshaped NA        Total
      1  am        Transmission     auto 2 (18.18%)   9 (81.82%)  8  19 (59.38%)
      2  am        Transmission   manual 7 (53.85%)   6 (46.15%)  0  13 (40.62%)
      3  am        Transmission    Total 9 (37.50%)  15 (62.50%)  8 32 (100.00%)
      4 cyl Number of cylinders        4 7 (87.50%)   1 (12.50%)  2  10 (37.04%)
      5 cyl Number of cylinders        6     0 (0%)  1 (100.00%)  3   4 (14.81%)
      6 cyl Number of cylinders        8     0 (0%) 11 (100.00%)  2  13 (48.15%)
      7 cyl Number of cylinders       NA          2            2  1            5
      8 cyl Number of cylinders    Total 9 (35.00%)  15 (65.00%)  8 32 (100.00%)
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
      as.data.frame(x2)
    Output
        .id               label variable    straight     vshaped NA        Total
      1  am        Transmission     auto  2 (22.22%)  9 (60.00%)  8  19 (59.38%)
      2  am        Transmission   manual  7 (77.78%)  6 (40.00%)  0  13 (40.62%)
      3  am        Transmission    Total  9 (37.50%) 15 (62.50%)  8 32 (100.00%)
      4 cyl Number of cylinders        4 7 (100.00%)   1 (7.69%)  2  10 (37.04%)
      5 cyl Number of cylinders        6      0 (0%)   1 (7.69%)  3   4 (14.81%)
      6 cyl Number of cylinders        8      0 (0%) 11 (84.62%)  2  13 (48.15%)
      7 cyl Number of cylinders       NA           2           2  1            5
      8 cyl Number of cylinders    Total  9 (35.00%) 15 (65.00%)  8 32 (100.00%)
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
      as.data.frame(x3)
    Output
        .id               label variable   straight     vshaped NA        Total
      1  am        Transmission     auto  2 (8.33%)  9 (37.50%)  8  19 (59.38%)
      2  am        Transmission   manual 7 (29.17%)  6 (25.00%)  0  13 (40.62%)
      3  am        Transmission    Total 9 (37.50%) 15 (62.50%)  8 32 (100.00%)
      4 cyl Number of cylinders        4 7 (35.00%)   1 (5.00%)  2  10 (37.04%)
      5 cyl Number of cylinders        6     0 (0%)   1 (5.00%)  3   4 (14.81%)
      6 cyl Number of cylinders        8     0 (0%) 11 (55.00%)  2  13 (48.15%)
      7 cyl Number of cylinders       NA          2           2  1            5
      8 cyl Number of cylinders    Total 9 (35.00%) 15 (65.00%)  8 32 (100.00%)
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
      as.data.frame(x4)
    Output
        .id               label variable straight vshaped NA Total
      1  am        Transmission     auto        2       9  8    19
      2  am        Transmission   manual        7       6  0    13
      3  am        Transmission    Total        9      15  8    32
      4 cyl Number of cylinders        4        7       1  2    10
      5 cyl Number of cylinders        6        0       1  3     4
      6 cyl Number of cylinders        8        0      11  2    13
      7 cyl Number of cylinders       NA        2       2  1     5
      8 cyl Number of cylinders    Total        9      15  8    32
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
      as.data.frame(x5)
    Output
        .id               label variable                      straight
      1  am        Transmission     auto   2 (8.33% / 18.18% / 22.22%)
      2  am        Transmission   manual  7 (29.17% / 53.85% / 77.78%)
      3  am        Transmission    Total                    9 (37.50%)
      4 cyl Number of cylinders        4 7 (35.00% / 87.50% / 100.00%)
      5 cyl Number of cylinders        6              0 (0% / 0% / 0%)
      6 cyl Number of cylinders        8              0 (0% / 0% / 0%)
      7 cyl Number of cylinders       NA                             2
      8 cyl Number of cylinders    Total                    9 (35.00%)
                               vshaped NA        Total
      1   9 (37.50% / 81.82% / 60.00%)  8  19 (59.38%)
      2   6 (25.00% / 46.15% / 40.00%)  0  13 (40.62%)
      3                    15 (62.50%)  8 32 (100.00%)
      4     1 (5.00% / 12.50% / 7.69%)  2  10 (37.04%)
      5    1 (5.00% / 100.00% / 7.69%)  3   4 (14.81%)
      6 11 (55.00% / 100.00% / 84.62%)  2  13 (48.15%)
      7                              2  1            5
      8                    15 (65.00%)  8 32 (100.00%)
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
      as.data.frame(x6)
    Output
        .id               label variable             straight               vshaped
      1  am        Transmission     auto  2 (18.18% / 22.22%)   9 (81.82% / 60.00%)
      2  am        Transmission   manual  7 (53.85% / 77.78%)   6 (46.15% / 40.00%)
      3  am        Transmission    Total           9 (37.50%)           15 (62.50%)
      4 cyl Number of cylinders        4 7 (87.50% / 100.00%)    1 (12.50% / 7.69%)
      5 cyl Number of cylinders        6          0 (0% / 0%)   1 (100.00% / 7.69%)
      6 cyl Number of cylinders        8          0 (0% / 0%) 11 (100.00% / 84.62%)
      7 cyl Number of cylinders       NA                    2                     2
      8 cyl Number of cylinders    Total           9 (35.00%)           15 (65.00%)
        NA        Total
      1  8  19 (59.38%)
      2  0  13 (40.62%)
      3  8 32 (100.00%)
      4  2  10 (37.04%)
      5  3   4 (14.81%)
      6  2  13 (48.15%)
      7  1            5
      8  8 32 (100.00%)
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
      as.data.frame(x0)
    Output
        .id               label variable                          value
      1 cyl Number of cylinders        4 N=10 \nrow=100.00%, col=37.04%
      2 cyl Number of cylinders        6  N=4 \nrow=100.00%, col=14.81%
      3 cyl Number of cylinders        8 N=13 \nrow=100.00%, col=48.15%
      4 cyl Number of cylinders       NA                              5
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
      as.data.frame(x1)
    Output
        .id               label variable                                     value
      1 cyl Number of cylinders        4 N=10 \np[95%CI] = 37.04% [14.94%; 66.33%]
      2 cyl Number of cylinders        6   N=4 \np[95%CI] = 14.81% [1.80%; 62.31%]
      3 cyl Number of cylinders        8 N=13 \np[95%CI] = 48.15% [24.70%; 72.44%]
      4 cyl Number of cylinders       NA                                         5
      5 cyl Number of cylinders    Total                              32 (100.00%)
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
      as.data.frame(x2)
    Output
        .id               label variable                          value
      1 cyl Number of cylinders        4 N=10 \nrow=100.00%, col=37.04%
      2 cyl Number of cylinders        6  N=4 \nrow=100.00%, col=14.81%
      3 cyl Number of cylinders        8 N=13 \nrow=100.00%, col=48.15%
      4 cyl Number of cylinders       NA                              5
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
      as.data.frame(x3)
    Output
         .id               label   variable am=auto & dummy=dummy
      1  mpg   Miles/(US) gallon  Min / Max           10.4 / 24.4
      2  mpg   Miles/(US) gallon  Med [IQR]      17.3 [14.9;19.2]
      3  mpg   Miles/(US) gallon Mean (std)            17.1 (3.8)
      4  mpg   Miles/(US) gallon     N (NA)                19 (0)
      5   vs              Engine   straight            2 (22.22%)
      6   vs              Engine    vshaped            9 (60.00%)
      7   vs              Engine         NA                     8
      8  cyl Number of cylinders          4            3 (30.00%)
      9  cyl Number of cylinders          6            3 (75.00%)
      10 cyl Number of cylinders          8           11 (84.62%)
      11 cyl Number of cylinders         NA                     2
         am=manual & dummy=dummy
      1              15.0 / 33.9
      2         22.8 [21.0;30.4]
      3               24.4 (6.2)
      4                   13 (0)
      5               7 (77.78%)
      6               6 (40.00%)
      7                        0
      8               7 (70.00%)
      9               1 (25.00%)
      10              2 (15.38%)
      11                       3
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

# Unique numeric

    Code
      x0 = crosstable(mtcars3, gear)
      as.data.frame(x0)
    Output
         .id label variable       value
      1 gear  gear        1 10 (38.46%)
      2 gear  gear        2 11 (42.31%)
      3 gear  gear        3  5 (19.23%)
      4 gear  gear       NA           6
    Code
      as_flextable(x0)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `value` 
      header has 1 row(s) 
      body has 4 row(s) 
      original dataset sample: 
         .id label variable       value
      1 gear  gear        1 10 (38.46%)
      2 gear  gear        2 11 (42.31%)
      3 gear  gear        3  5 (19.23%)
      4 gear  gear       NA           6
    Code
      x1 = crosstable(mtcars3, carb)
      as.data.frame(x1)
    Output
         .id                 label   variable         value
      1 carb Number of carburetors  Min / Max     1.0 / 8.0
      2 carb Number of carburetors  Med [IQR] 2.0 [2.0;4.0]
      3 carb Number of carburetors Mean (std)     2.8 (1.6)
      4 carb Number of carburetors     N (NA)        32 (0)
    Code
      as_flextable(x1)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `value` 
      header has 1 row(s) 
      body has 4 row(s) 
      original dataset sample: 
         .id                 label   variable         value
      1 carb Number of carburetors  Min / Max     1.0 / 8.0
      2 carb Number of carburetors  Med [IQR] 2.0 [2.0;4.0]
      3 carb Number of carburetors Mean (std)     2.8 (1.6)
      4 carb Number of carburetors     N (NA)        32 (0)
    Code
      x2 = crosstable(mtcars3, carb, unique_numeric = 9)
      as.data.frame(x2)
    Output
         .id                 label variable       value
      1 carb Number of carburetors        1  7 (21.88%)
      2 carb Number of carburetors        2 10 (31.25%)
      3 carb Number of carburetors        3   3 (9.38%)
      4 carb Number of carburetors        4 10 (31.25%)
      5 carb Number of carburetors        6   1 (3.12%)
      6 carb Number of carburetors        8   1 (3.12%)
    Code
      as_flextable(x2)
    Output
      a flextable object.
      col_keys: `label`, `variable`, `value` 
      header has 1 row(s) 
      body has 6 row(s) 
      original dataset sample: 
         .id                 label variable       value
      1 carb Number of carburetors        1  7 (21.88%)
      2 carb Number of carburetors        2 10 (31.25%)
      3 carb Number of carburetors        3   3 (9.38%)
      4 carb Number of carburetors        4 10 (31.25%)
      5 carb Number of carburetors        6   1 (3.12%)

# By dummy

    Code
      x0 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = dummy)
      as.data.frame(x0)
    Output
          .id                    label        variable            dummy
      1    am             Transmission            auto      19 (59.38%)
      2    am             Transmission          manual      13 (40.62%)
      3   mpg        Miles/(US) gallon       Min / Max      10.4 / 33.9
      4   mpg        Miles/(US) gallon       Med [IQR] 19.2 [15.4;22.8]
      5   mpg        Miles/(US) gallon      Mean (std)       20.1 (6.0)
      6   mpg        Miles/(US) gallon          N (NA)           32 (0)
      7   cyl      Number of cylinders               4      10 (37.04%)
      8   cyl      Number of cylinders               6       4 (14.81%)
      9   cyl      Number of cylinders               8      13 (48.15%)
      10  cyl      Number of cylinders              NA                5
      11 surv Dummy survival (disp/am)          t=71.1      0.97 (1/32)
      12 surv Dummy survival (disp/am)          t=75.7      0.94 (1/31)
      13 surv Dummy survival (disp/am)          t=78.7      0.91 (1/30)
      14 surv Dummy survival (disp/am)            t=79      0.88 (1/29)
      15 surv Dummy survival (disp/am)          t=95.1      0.84 (1/28)
      16 surv Dummy survival (disp/am)           t=108      0.81 (1/27)
      17 surv Dummy survival (disp/am)         t=120.1      0.81 (0/26)
      18 surv Dummy survival (disp/am)         t=120.3      0.78 (1/25)
      19 surv Dummy survival (disp/am)           t=121      0.75 (1/24)
      20 surv Dummy survival (disp/am)         t=140.8      0.75 (0/23)
      21 surv Dummy survival (disp/am)           t=145      0.71 (1/22)
      22 surv Dummy survival (disp/am)         t=146.7      0.71 (0/21)
      23 surv Dummy survival (disp/am)           t=160      0.64 (2/20)
      24 surv Dummy survival (disp/am)         t=167.6      0.64 (0/18)
      25 surv Dummy survival (disp/am)           t=225      0.64 (0/16)
      26 surv Dummy survival (disp/am)           t=258      0.64 (0/15)
      27 surv Dummy survival (disp/am)         t=275.8      0.64 (0/14)
      28 surv Dummy survival (disp/am)           t=301      0.58 (1/11)
      29 surv Dummy survival (disp/am)           t=304      0.58 (0/10)
      30 surv Dummy survival (disp/am)           t=318       0.58 (0/9)
      31 surv Dummy survival (disp/am)           t=350       0.58 (0/8)
      32 surv Dummy survival (disp/am)           t=351       0.50 (1/7)
      33 surv Dummy survival (disp/am)           t=360       0.50 (0/6)
      34 surv Dummy survival (disp/am)           t=400       0.50 (0/4)
      35 surv Dummy survival (disp/am)           t=440       0.50 (0/3)
      36 surv Dummy survival (disp/am)           t=460       0.50 (0/2)
      37 surv Dummy survival (disp/am)           t=472       0.50 (0/1)
      38 surv Dummy survival (disp/am) Median survival             <NA>
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
      as.data.frame(x1)
    Output
          .id                    label        variable            dummy    NA
      1    am             Transmission            auto      19 (59.38%)     0
      2    am             Transmission          manual      13 (40.62%)     0
      3    am             Transmission              NA                0     0
      4   mpg        Miles/(US) gallon       Min / Max      10.4 / 33.9 no NA
      5   mpg        Miles/(US) gallon       Med [IQR] 19.2 [15.4;22.8] no NA
      6   mpg        Miles/(US) gallon      Mean (std)       20.1 (6.0) no NA
      7   mpg        Miles/(US) gallon          N (NA)           32 (0) no NA
      8   cyl      Number of cylinders               4      10 (37.04%)     0
      9   cyl      Number of cylinders               6       4 (14.81%)     0
      10  cyl      Number of cylinders               8      13 (48.15%)     0
      11  cyl      Number of cylinders              NA                5     0
      12 surv Dummy survival (disp/am)          t=71.1      0.97 (1/32)  <NA>
      13 surv Dummy survival (disp/am)          t=75.7      0.94 (1/31)  <NA>
      14 surv Dummy survival (disp/am)          t=78.7      0.91 (1/30)  <NA>
      15 surv Dummy survival (disp/am)            t=79      0.88 (1/29)  <NA>
      16 surv Dummy survival (disp/am)          t=95.1      0.84 (1/28)  <NA>
      17 surv Dummy survival (disp/am)           t=108      0.81 (1/27)  <NA>
      18 surv Dummy survival (disp/am)         t=120.1      0.81 (0/26)  <NA>
      19 surv Dummy survival (disp/am)         t=120.3      0.78 (1/25)  <NA>
      20 surv Dummy survival (disp/am)           t=121      0.75 (1/24)  <NA>
      21 surv Dummy survival (disp/am)         t=140.8      0.75 (0/23)  <NA>
      22 surv Dummy survival (disp/am)           t=145      0.71 (1/22)  <NA>
      23 surv Dummy survival (disp/am)         t=146.7      0.71 (0/21)  <NA>
      24 surv Dummy survival (disp/am)           t=160      0.64 (2/20)  <NA>
      25 surv Dummy survival (disp/am)         t=167.6      0.64 (0/18)  <NA>
      26 surv Dummy survival (disp/am)           t=225      0.64 (0/16)  <NA>
      27 surv Dummy survival (disp/am)           t=258      0.64 (0/15)  <NA>
      28 surv Dummy survival (disp/am)         t=275.8      0.64 (0/14)  <NA>
      29 surv Dummy survival (disp/am)           t=301      0.58 (1/11)  <NA>
      30 surv Dummy survival (disp/am)           t=304      0.58 (0/10)  <NA>
      31 surv Dummy survival (disp/am)           t=318       0.58 (0/9)  <NA>
      32 surv Dummy survival (disp/am)           t=350       0.58 (0/8)  <NA>
      33 surv Dummy survival (disp/am)           t=351       0.50 (1/7)  <NA>
      34 surv Dummy survival (disp/am)           t=360       0.50 (0/6)  <NA>
      35 surv Dummy survival (disp/am)           t=400       0.50 (0/4)  <NA>
      36 surv Dummy survival (disp/am)           t=440       0.50 (0/3)  <NA>
      37 surv Dummy survival (disp/am)           t=460       0.50 (0/2)  <NA>
      38 surv Dummy survival (disp/am)           t=472       0.50 (0/1)  <NA>
      39 surv Dummy survival (disp/am) Median survival             <NA>  <NA>
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
      as.data.frame(x2)
    Output
          .id                    label        variable            dummy
      1    am             Transmission            auto     11 (100.00%)
      2    am             Transmission          manual     13 (100.00%)
      3   mpg        Miles/(US) gallon       Min / Max      10.4 / 33.9
      4   mpg        Miles/(US) gallon       Med [IQR] 20.4 [15.2;23.6]
      5   mpg        Miles/(US) gallon      Mean (std)       20.5 (6.7)
      6   mpg        Miles/(US) gallon          N (NA)           24 (0)
      7   cyl      Number of cylinders               4      8 (100.00%)
      8   cyl      Number of cylinders               6      1 (100.00%)
      9   cyl      Number of cylinders               8     11 (100.00%)
      10  cyl      Number of cylinders              NA                4
      11 surv Dummy survival (disp/am)          t=71.1      0.96 (1/24)
      12 surv Dummy survival (disp/am)          t=75.7      0.92 (1/23)
      13 surv Dummy survival (disp/am)          t=78.7      0.88 (1/22)
      14 surv Dummy survival (disp/am)            t=79      0.83 (1/21)
      15 surv Dummy survival (disp/am)          t=95.1      0.79 (1/20)
      16 surv Dummy survival (disp/am)           t=108      0.75 (1/19)
      17 surv Dummy survival (disp/am)         t=120.1      0.75 (0/18)
      18 surv Dummy survival (disp/am)         t=120.3      0.71 (1/17)
      19 surv Dummy survival (disp/am)           t=121      0.66 (1/16)
      20 surv Dummy survival (disp/am)         t=140.8      0.66 (0/15)
      21 surv Dummy survival (disp/am)           t=145      0.62 (1/15)
      22 surv Dummy survival (disp/am)         t=146.7      0.62 (0/14)
      23 surv Dummy survival (disp/am)           t=160      0.53 (2/14)
      24 surv Dummy survival (disp/am)         t=167.6      0.53 (0/12)
      25 surv Dummy survival (disp/am)           t=225      0.53 (0/12)
      26 surv Dummy survival (disp/am)           t=258      0.53 (0/12)
      27 surv Dummy survival (disp/am)         t=275.8      0.53 (0/11)
      28 surv Dummy survival (disp/am)         t=275.8      0.53 (0/11)
      29 surv Dummy survival (disp/am)           t=301       0.47 (1/9)
      30 surv Dummy survival (disp/am)           t=304       0.47 (0/8)
      31 surv Dummy survival (disp/am)           t=318       0.47 (0/7)
      32 surv Dummy survival (disp/am)           t=350       0.47 (0/6)
      33 surv Dummy survival (disp/am)           t=351       0.38 (1/5)
      34 surv Dummy survival (disp/am)           t=360       0.38 (0/4)
      35 surv Dummy survival (disp/am)           t=400       0.38 (0/4)
      36 surv Dummy survival (disp/am)           t=440       0.38 (0/3)
      37 surv Dummy survival (disp/am)           t=460       0.38 (0/2)
      38 surv Dummy survival (disp/am)           t=472       0.38 (0/1)
      39 surv Dummy survival (disp/am) Median survival              301
                       NA
      1                 8
      2                 0
      3       14.3 / 24.4
      4  18.4 [17.5;20.1]
      5        19.0 (3.3)
      6             8 (0)
      7                 2
      8                 3
      9                 2
      10                1
      11       1.00 (0/8)
      12       1.00 (0/8)
      13       1.00 (0/8)
      14       1.00 (0/8)
      15       1.00 (0/8)
      16       1.00 (0/8)
      17       1.00 (0/8)
      18       1.00 (0/8)
      19       1.00 (0/8)
      20       1.00 (0/8)
      21       1.00 (0/7)
      22       1.00 (0/7)
      23       1.00 (0/6)
      24       1.00 (0/6)
      25       1.00 (0/4)
      26       1.00 (0/3)
      27       1.00 (0/3)
      28       1.00 (0/3)
      29       1.00 (0/2)
      30       1.00 (0/2)
      31       1.00 (0/2)
      32       1.00 (0/2)
      33       1.00 (0/2)
      34       1.00 (0/2)
      35       1.00 (0/0)
      36       1.00 (0/0)
      37       1.00 (0/0)
      38       1.00 (0/0)
      39             <NA>
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
      as.data.frame(x3)
    Output
          .id                    label        variable            dummy
      1    am             Transmission            auto     11 (100.00%)
      2    am             Transmission          manual     13 (100.00%)
      3   mpg        Miles/(US) gallon       Min / Max      10.4 / 33.9
      4   mpg        Miles/(US) gallon       Med [IQR] 20.4 [15.2;23.6]
      5   mpg        Miles/(US) gallon      Mean (std)       20.5 (6.7)
      6   mpg        Miles/(US) gallon          N (NA)           24 (0)
      7   cyl      Number of cylinders               4      8 (100.00%)
      8   cyl      Number of cylinders               6      1 (100.00%)
      9   cyl      Number of cylinders               8     11 (100.00%)
      10 surv Dummy survival (disp/am)          t=71.1      0.96 (1/24)
      11 surv Dummy survival (disp/am)          t=75.7      0.92 (1/23)
      12 surv Dummy survival (disp/am)          t=78.7      0.88 (1/22)
      13 surv Dummy survival (disp/am)            t=79      0.83 (1/21)
      14 surv Dummy survival (disp/am)          t=95.1      0.79 (1/20)
      15 surv Dummy survival (disp/am)           t=108      0.75 (1/19)
      16 surv Dummy survival (disp/am)         t=120.1      0.75 (0/18)
      17 surv Dummy survival (disp/am)         t=120.3      0.71 (1/17)
      18 surv Dummy survival (disp/am)           t=121      0.66 (1/16)
      19 surv Dummy survival (disp/am)           t=145      0.62 (1/15)
      20 surv Dummy survival (disp/am)           t=160      0.53 (2/14)
      21 surv Dummy survival (disp/am)           t=258      0.53 (0/12)
      22 surv Dummy survival (disp/am)         t=275.8      0.53 (0/11)
      23 surv Dummy survival (disp/am)           t=301       0.47 (1/9)
      24 surv Dummy survival (disp/am)           t=304       0.47 (0/8)
      25 surv Dummy survival (disp/am)           t=318       0.47 (0/7)
      26 surv Dummy survival (disp/am)           t=350       0.47 (0/6)
      27 surv Dummy survival (disp/am)           t=351       0.38 (1/5)
      28 surv Dummy survival (disp/am)           t=400       0.38 (0/4)
      29 surv Dummy survival (disp/am)           t=440       0.38 (0/3)
      30 surv Dummy survival (disp/am)           t=460       0.38 (0/2)
      31 surv Dummy survival (disp/am)           t=472       0.38 (0/1)
      32 surv Dummy survival (disp/am) Median survival              301
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
      as.data.frame(x0)
    Output
         .id                   label   variable cyl=4 & am=auto & vs=straight
      1  mpg       Miles/(US) gallon  Min / Max                   21.5 / 21.5
      2  mpg       Miles/(US) gallon  Med [IQR]              21.5 [21.5;21.5]
      3  mpg       Miles/(US) gallon Mean (std)                     21.5 (NA)
      4  mpg       Miles/(US) gallon     N (NA)                         1 (0)
      5 gear Number of forward gears          3                     1 (7.69%)
      6 gear Number of forward gears          4                        0 (0%)
      7 gear Number of forward gears          5                        0 (0%)
        cyl=6 & am=auto & vs=straight cyl=8 & am=auto & vs=straight
      1                       NA / NA                       NA / NA
      2                    NA [NA;NA]                    NA [NA;NA]
      3                       NA (NA)                       NA (NA)
      4                         0 (0)                         0 (0)
      5                        0 (0%)                        0 (0%)
      6                        0 (0%)                        0 (0%)
      7                        0 (0%)                        0 (0%)
        cyl=4 & am=manual & vs=straight cyl=6 & am=manual & vs=straight
      1                     21.4 / 33.9                         NA / NA
      2                30.4 [28.1;31.9]                      NA [NA;NA]
      3                      29.3 (4.5)                         NA (NA)
      4                           6 (0)                           0 (0)
      5                          0 (0%)                          0 (0%)
      6                      5 (55.56%)                          0 (0%)
      7                      1 (20.00%)                          0 (0%)
        cyl=8 & am=manual & vs=straight cyl=4 & am=auto & vs=vshaped
      1                         NA / NA                      NA / NA
      2                      NA [NA;NA]                   NA [NA;NA]
      3                         NA (NA)                      NA (NA)
      4                           0 (0)                        0 (0)
      5                          0 (0%)                       0 (0%)
      6                          0 (0%)                       0 (0%)
      7                          0 (0%)                       0 (0%)
        cyl=6 & am=auto & vs=vshaped cyl=8 & am=auto & vs=vshaped
      1                      NA / NA                  10.4 / 19.2
      2                   NA [NA;NA]             15.2 [13.3;15.5]
      3                      NA (NA)                   14.6 (2.9)
      4                        0 (0)                        9 (0)
      5                       0 (0%)                   9 (69.23%)
      6                       0 (0%)                       0 (0%)
      7                       0 (0%)                       0 (0%)
        cyl=4 & am=manual & vs=vshaped cyl=6 & am=manual & vs=vshaped
      1                    26.0 / 26.0                    19.7 / 19.7
      2               26.0 [26.0;26.0]               19.7 [19.7;19.7]
      3                      26.0 (NA)                      19.7 (NA)
      4                          1 (0)                          1 (0)
      5                         0 (0%)                         0 (0%)
      6                         0 (0%)                         0 (0%)
      7                     1 (20.00%)                     1 (20.00%)
        cyl=8 & am=manual & vs=vshaped cyl=4 & am=auto & vs=NA
      1                    15.0 / 15.8             22.8 / 24.4
      2               15.4 [15.2;15.6]        23.6 [23.2;24.0]
      3                     15.4 (0.6)              23.6 (1.1)
      4                          2 (0)                   2 (0)
      5                         0 (0%)                  0 (0%)
      6                         0 (0%)              2 (22.22%)
      7                     2 (40.00%)                  0 (0%)
        cyl=6 & am=auto & vs=NA cyl=8 & am=auto & vs=NA cyl=4 & am=manual & vs=NA
      1             17.8 / 19.2             14.3 / 16.4                   NA / NA
      2        18.1 [18.0;18.6]        15.3 [14.8;15.9]                NA [NA;NA]
      3              18.4 (0.7)              15.3 (1.5)                   NA (NA)
      4                   3 (0)                   2 (0)                     0 (0)
      5               1 (7.69%)              2 (15.38%)                    0 (0%)
      6              2 (22.22%)                  0 (0%)                    0 (0%)
      7                  0 (0%)                  0 (0%)                    0 (0%)
        cyl=6 & am=manual & vs=NA cyl=8 & am=manual & vs=NA               NA
      1                   NA / NA                   NA / NA      18.7 / 22.8
      2                NA [NA;NA]                NA [NA;NA] 21.0 [21.0;21.4]
      3                   NA (NA)                   NA (NA)       21.0 (1.5)
      4                     0 (0)                     0 (0)            5 (0)
      5                    0 (0%)                    0 (0%)                2
      6                    0 (0%)                    0 (0%)                3
      7                    0 (0%)                    0 (0%)                0
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
      as.data.frame(x1)
    Output
         .id                   label   variable cyl=4 & am=auto & vs=straight
      1  mpg       Miles/(US) gallon  Min / Max                   21.5 / 21.5
      2  mpg       Miles/(US) gallon  Med [IQR]              21.5 [21.5;21.5]
      3  mpg       Miles/(US) gallon Mean (std)                     21.5 (NA)
      4  mpg       Miles/(US) gallon     N (NA)                         1 (0)
      5 gear Number of forward gears          3                    1 (10.00%)
      6 gear Number of forward gears          4                        0 (0%)
      7 gear Number of forward gears          5                        0 (0%)
        cyl=6 & am=auto & vs=straight cyl=8 & am=auto & vs=straight
      1                       NA / NA                       NA / NA
      2                    NA [NA;NA]                    NA [NA;NA]
      3                       NA (NA)                       NA (NA)
      4                         0 (0)                         0 (0)
      5                        0 (0%)                        0 (0%)
      6                        0 (0%)                        0 (0%)
      7                        0 (0%)                        0 (0%)
        cyl=4 & am=manual & vs=straight cyl=6 & am=manual & vs=straight
      1                     21.4 / 33.9                         NA / NA
      2                30.4 [28.1;31.9]                      NA [NA;NA]
      3                      29.3 (4.5)                         NA (NA)
      4                           6 (0)                           0 (0)
      5                          0 (0%)                          0 (0%)
      6                     5 (100.00%)                          0 (0%)
      7                      1 (20.00%)                          0 (0%)
        cyl=8 & am=manual & vs=straight cyl=4 & am=auto & vs=vshaped
      1                         NA / NA                      NA / NA
      2                      NA [NA;NA]                   NA [NA;NA]
      3                         NA (NA)                      NA (NA)
      4                           0 (0)                        0 (0)
      5                          0 (0%)                       0 (0%)
      6                          0 (0%)                       0 (0%)
      7                          0 (0%)                       0 (0%)
        cyl=6 & am=auto & vs=vshaped cyl=8 & am=auto & vs=vshaped
      1                      NA / NA                  10.4 / 19.2
      2                   NA [NA;NA]             15.2 [13.3;15.5]
      3                      NA (NA)                   14.6 (2.9)
      4                        0 (0)                        9 (0)
      5                       0 (0%)                   9 (90.00%)
      6                       0 (0%)                       0 (0%)
      7                       0 (0%)                       0 (0%)
        cyl=4 & am=manual & vs=vshaped cyl=6 & am=manual & vs=vshaped
      1                    26.0 / 26.0                    19.7 / 19.7
      2               26.0 [26.0;26.0]               19.7 [19.7;19.7]
      3                      26.0 (NA)                      19.7 (NA)
      4                          1 (0)                          1 (0)
      5                         0 (0%)                         0 (0%)
      6                         0 (0%)                         0 (0%)
      7                     1 (20.00%)                     1 (20.00%)
        cyl=8 & am=manual & vs=vshaped
      1                    15.0 / 15.8
      2               15.4 [15.2;15.6]
      3                     15.4 (0.6)
      4                          2 (0)
      5                         0 (0%)
      6                         0 (0%)
      7                     2 (40.00%)
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
      as.data.frame(x2)
    Output
         .id                   label   variable cyl=4 & am=auto & vs=straight
      1  mpg       Miles/(US) gallon  Min / Max                   21.5 / 21.5
      2  mpg       Miles/(US) gallon  Med [IQR]              21.5 [21.5;21.5]
      3  mpg       Miles/(US) gallon Mean (std)                     21.5 (NA)
      4  mpg       Miles/(US) gallon     N (NA)                         1 (0)
      5 gear Number of forward gears          3                     1 (7.69%)
      6 gear Number of forward gears          4                        0 (0%)
      7 gear Number of forward gears          5                        0 (0%)
      8 gear Number of forward gears      Total                     1 (3.70%)
        cyl=6 & am=auto & vs=straight cyl=8 & am=auto & vs=straight
      1                       NA / NA                       NA / NA
      2                    NA [NA;NA]                    NA [NA;NA]
      3                       NA (NA)                       NA (NA)
      4                         0 (0)                         0 (0)
      5                        0 (0%)                        0 (0%)
      6                        0 (0%)                        0 (0%)
      7                        0 (0%)                        0 (0%)
      8                        0 (0%)                        0 (0%)
        cyl=4 & am=manual & vs=straight cyl=6 & am=manual & vs=straight
      1                     21.4 / 33.9                         NA / NA
      2                30.4 [28.1;31.9]                      NA [NA;NA]
      3                      29.3 (4.5)                         NA (NA)
      4                           6 (0)                           0 (0)
      5                          0 (0%)                          0 (0%)
      6                      5 (55.56%)                          0 (0%)
      7                      1 (20.00%)                          0 (0%)
      8                      6 (22.22%)                          0 (0%)
        cyl=8 & am=manual & vs=straight cyl=4 & am=auto & vs=vshaped
      1                         NA / NA                      NA / NA
      2                      NA [NA;NA]                   NA [NA;NA]
      3                         NA (NA)                      NA (NA)
      4                           0 (0)                        0 (0)
      5                          0 (0%)                       0 (0%)
      6                          0 (0%)                       0 (0%)
      7                          0 (0%)                       0 (0%)
      8                          0 (0%)                       0 (0%)
        cyl=6 & am=auto & vs=vshaped cyl=8 & am=auto & vs=vshaped
      1                      NA / NA                  10.4 / 19.2
      2                   NA [NA;NA]             15.2 [13.3;15.5]
      3                      NA (NA)                   14.6 (2.9)
      4                        0 (0)                        9 (0)
      5                       0 (0%)                   9 (69.23%)
      6                       0 (0%)                       0 (0%)
      7                       0 (0%)                       0 (0%)
      8                       0 (0%)                   9 (33.33%)
        cyl=4 & am=manual & vs=vshaped cyl=6 & am=manual & vs=vshaped
      1                    26.0 / 26.0                    19.7 / 19.7
      2               26.0 [26.0;26.0]               19.7 [19.7;19.7]
      3                      26.0 (NA)                      19.7 (NA)
      4                          1 (0)                          1 (0)
      5                         0 (0%)                         0 (0%)
      6                         0 (0%)                         0 (0%)
      7                     1 (20.00%)                     1 (20.00%)
      8                      1 (3.70%)                      1 (3.70%)
        cyl=8 & am=manual & vs=vshaped cyl=4 & am=auto & vs=NA
      1                    15.0 / 15.8             22.8 / 24.4
      2               15.4 [15.2;15.6]        23.6 [23.2;24.0]
      3                     15.4 (0.6)              23.6 (1.1)
      4                          2 (0)                   2 (0)
      5                         0 (0%)                  0 (0%)
      6                         0 (0%)              2 (22.22%)
      7                     2 (40.00%)                  0 (0%)
      8                      2 (7.41%)               2 (7.41%)
        cyl=6 & am=auto & vs=NA cyl=8 & am=auto & vs=NA cyl=4 & am=manual & vs=NA
      1             17.8 / 19.2             14.3 / 16.4                   NA / NA
      2        18.1 [18.0;18.6]        15.3 [14.8;15.9]                NA [NA;NA]
      3              18.4 (0.7)              15.3 (1.5)                   NA (NA)
      4                   3 (0)                   2 (0)                     0 (0)
      5               1 (7.69%)              2 (15.38%)                    0 (0%)
      6              2 (22.22%)                  0 (0%)                    0 (0%)
      7                  0 (0%)                  0 (0%)                    0 (0%)
      8              3 (11.11%)               2 (7.41%)                    0 (0%)
        cyl=6 & am=manual & vs=NA cyl=8 & am=manual & vs=NA               NA
      1                   NA / NA                   NA / NA      18.7 / 22.8
      2                NA [NA;NA]                NA [NA;NA] 21.0 [21.0;21.4]
      3                   NA (NA)                   NA (NA)       21.0 (1.5)
      4                     0 (0)                     0 (0)            5 (0)
      5                    0 (0%)                    0 (0%)                2
      6                    0 (0%)                    0 (0%)                3
      7                    0 (0%)                    0 (0%)                0
      8                    0 (0%)                    0 (0%)                5
                   Total
      1      10.4 / 33.9
      2 19.2 [15.4;22.8]
      3       20.1 (6.0)
      4           32 (0)
      5      15 (46.88%)
      6      12 (37.50%)
      7       5 (15.62%)
      8     32 (100.00%)
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
      as.data.frame(x3)
    Output
         .id               label   variable am=auto & dummy=dummy
      1  mpg   Miles/(US) gallon  Min / Max           10.4 / 24.4
      2  mpg   Miles/(US) gallon  Med [IQR]      17.3 [14.9;19.2]
      3  mpg   Miles/(US) gallon Mean (std)            17.1 (3.8)
      4  mpg   Miles/(US) gallon     N (NA)                19 (0)
      5   vs              Engine   straight            2 (22.22%)
      6   vs              Engine    vshaped            9 (60.00%)
      7   vs              Engine         NA                     8
      8  cyl Number of cylinders          4            3 (30.00%)
      9  cyl Number of cylinders          6            3 (75.00%)
      10 cyl Number of cylinders          8           11 (84.62%)
      11 cyl Number of cylinders         NA                     2
         am=manual & dummy=dummy
      1              15.0 / 33.9
      2         22.8 [21.0;30.4]
      3               24.4 (6.2)
      4                   13 (0)
      5               7 (77.78%)
      6               6 (40.00%)
      7                        0
      8               7 (70.00%)
      9               1 (25.00%)
      10              2 (15.38%)
      11                       3
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
      as.data.frame(x4)
    Output
                .id                              label                     variable
      1         mpg                  Miles/(US) gallon                    Min / Max
      2         mpg                  Miles/(US) gallon                    Med [IQR]
      3         mpg                  Miles/(US) gallon                   Mean (std)
      4         mpg                  Miles/(US) gallon                       N (NA)
      5          vs                             Engine                     straight
      6          vs                             Engine                      vshaped
      7          vs                             Engine                           NA
      8          vs                             Engine                        Total
      9         cyl                Number of cylinders                            4
      10        cyl                Number of cylinders                            6
      11        cyl                Number of cylinders                            8
      12        cyl                Number of cylinders                           NA
      13        cyl                Number of cylinders                        Total
      14      dummy                              dummy                        dummy
      15      dummy                              dummy                        Total
      16       surv           Dummy survival (disp/am)                        t=100
      17       surv           Dummy survival (disp/am)                        t=200
      18       surv           Dummy survival (disp/am) Median follow up [min ; max]
      19       surv           Dummy survival (disp/am)              Median survival
      20    hp_date                 Some nonsense date                    Min / Max
      21    hp_date                 Some nonsense date                    Med [IQR]
      22    hp_date                 Some nonsense date                   Mean (std)
      23    hp_date                 Some nonsense date                       N (NA)
      24 qsec_posix                          Date+time                    Min / Max
      25 qsec_posix                          Date+time                    Med [IQR]
      26 qsec_posix                          Date+time                   Mean (std)
      27 qsec_posix                          Date+time                       N (NA)
      28       diff Difftime hp_date-qsec_posix (days)                    Min / Max
      29       diff Difftime hp_date-qsec_posix (days)                    Med [IQR]
      30       diff Difftime hp_date-qsec_posix (days)                   Mean (std)
      31       diff Difftime hp_date-qsec_posix (days)                       N (NA)
      32       cyl3                               cyl3                        FALSE
      33       cyl3                               cyl3                           NA
      34       cyl3                               cyl3                        Total
                                                      am=auto & gear=3
      1                                                    10.4 / 21.5
      2                                               15.5 [14.5;18.4]
      3                                                     16.1 (3.4)
      4                                                         15 (0)
      5                                                     2 (22.22%)
      6                                                     9 (60.00%)
      7                                                              4
      8                                                    15 (45.83%)
      9                                                     1 (10.00%)
      10                                                    1 (25.00%)
      11                                                   11 (84.62%)
      12                                                             2
      13                                                   15 (48.15%)
      14                                                   15 (46.88%)
      15                                                   15 (46.88%)
      16                                                   1.00 (0/15)
      17                                                   1.00 (0/14)
      18                                             318 [120.1 ; 472]
      19                                                          <NA>
      20                                       2010-04-08 - 2010-09-03
      21                            2010-06-30 [2010-05-31;2010-08-04]
      22                                       2010-06-26 (1.6 months)
      23                                                        15 (0)
      24                     2010-01-16 10:50:24 - 2010-01-21 06:16:48
      25 2010-01-18 11:04:48 [2010-01-18 01:28:48;2010-01-19 01:00:00]
      26                                2010-01-18 17:36:28 (1.3 days)
      27                                                        15 (0)
      28                                                  77.0 / 229.6
      29                                           162.0 [132.9;192.1]
      30                                                  158.4 (48.8)
      31                                                        15 (0)
      32                                                   13 (48.15%)
      33                                                             2
      34                                                   15 (48.15%)
         am=manual & gear=3
      1             NA / NA
      2          NA [NA;NA]
      3             NA (NA)
      4               0 (0)
      5              0 (0%)
      6              0 (0%)
      7                   0
      8              0 (0%)
      9              0 (0%)
      10             0 (0%)
      11             0 (0%)
      12                  0
      13             0 (0%)
      14             0 (0%)
      15             0 (0%)
      16               <NA>
      17               <NA>
      18               <NA>
      19               <NA>
      20            NA / NA
      21         NA [NA;NA]
      22            NA (NA)
      23              0 (0)
      24            NA / NA
      25         NA [NA;NA]
      26            NA (NA)
      27              0 (0)
      28            NA / NA
      29         NA [NA;NA]
      30            NA (NA)
      31              0 (0)
      32             0 (0%)
      33                  0
      34             0 (0%)
                                                      am=auto & gear=4
      1                                                    17.8 / 24.4
      2                                               21.0 [18.8;23.2]
      3                                                     21.1 (3.1)
      4                                                          4 (0)
      5                                                         0 (0%)
      6                                                         0 (0%)
      7                                                              4
      8                                                         4 (0%)
      9                                                     2 (20.00%)
      10                                                    2 (50.00%)
      11                                                        0 (0%)
      12                                                             0
      13                                                    4 (14.81%)
      14                                                    4 (12.50%)
      15                                                    4 (12.50%)
      16                                                    1.00 (0/4)
      17                                                    1.00 (0/0)
      18                                        157.15 [140.8 ; 167.6]
      19                                                          <NA>
      20                                       2010-03-04 - 2010-05-04
      21                            2010-04-20 [2010-03-04;2010-05-04]
      22                                        2010-04-11 (29.0 days)
      23                                                         4 (0)
      24                     2010-01-19 08:12:00 - 2010-01-23 22:36:00
      25 2010-01-20 11:48:00 [2010-01-19 08:12:00;2010-01-21 01:00:00]
      26                                2010-01-21 01:36:00 (2.0 days)
      27                                                         4 (0)
      28                                                  42.0 / 104.7
      29                                             88.1 [64.6;104.2]
      30                                                   80.7 (30.0)
      31                                                         4 (0)
      32                                                    4 (14.81%)
      33                                                             0
      34                                                    4 (14.81%)
                                                    am=manual & gear=4
      1                                                    21.0 / 33.9
      2                                               25.1 [21.3;30.9]
      3                                                     26.3 (5.4)
      4                                                          8 (0)
      5                                                     6 (66.67%)
      6                                                     2 (13.33%)
      7                                                              0
      8                                                     8 (33.33%)
      9                                                     5 (50.00%)
      10                                                        0 (0%)
      11                                                        0 (0%)
      12                                                             3
      13                                                    8 (18.52%)
      14                                                    8 (25.00%)
      15                                                    8 (25.00%)
      16                                                    0.50 (4/4)
      17                                                       0 (4/0)
      18                                                NA [Inf ; 160]
      19                                                          93.5
      20                                       2010-02-22 - 2010-04-21
      21                            2010-03-21 [2010-03-07;2010-04-20]
      22                                        2010-03-25 (24.2 days)
      23                                                         8 (0)
      24                     2010-01-17 12:02:24 - 2010-01-20 22:36:00
      25 2010-01-19 15:31:12 [2010-01-18 01:28:48;2010-01-19 22:36:00]
      26                                2010-01-19 11:26:24 (1.2 days)
      27                                                         8 (0)
      28                                                   33.5 / 93.5
      29                                              60.7 [46.2;91.0]
      30                                                   65.4 (25.0)
      31                                                         8 (0)
      32                                                    5 (18.52%)
      33                                                             3
      34                                                    8 (18.52%)
         am=auto & gear=5
      1           NA / NA
      2        NA [NA;NA]
      3           NA (NA)
      4             0 (0)
      5            0 (0%)
      6            0 (0%)
      7                 0
      8            0 (0%)
      9            0 (0%)
      10           0 (0%)
      11           0 (0%)
      12                0
      13           0 (0%)
      14           0 (0%)
      15           0 (0%)
      16             <NA>
      17             <NA>
      18             <NA>
      19             <NA>
      20          NA / NA
      21       NA [NA;NA]
      22          NA (NA)
      23            0 (0)
      24          NA / NA
      25       NA [NA;NA]
      26          NA (NA)
      27            0 (0)
      28          NA / NA
      29       NA [NA;NA]
      30          NA (NA)
      31            0 (0)
      32           0 (0%)
      33                0
      34           0 (0%)
                                                    am=manual & gear=5
      1                                                    15.0 / 30.4
      2                                               19.7 [15.8;26.0]
      3                                                     21.4 (6.7)
      4                                                          5 (0)
      5                                                     1 (11.11%)
      6                                                     4 (26.67%)
      7                                                              0
      8                                                     5 (20.83%)
      9                                                     2 (20.00%)
      10                                                    1 (25.00%)
      11                                                    2 (15.38%)
      12                                                             0
      13                                                    5 (18.52%)
      14                                                    5 (15.62%)
      15                                                    5 (15.62%)
      16                                                    0.80 (1/4)
      17                                                    0.40 (2/2)
      18                                                NA [Inf ; 351]
      19                                                           145
      20                                       2010-04-02 - 2010-12-02
      21                            2010-06-25 [2010-04-24;2010-09-22]
      22                                       2010-07-15 (3.4 months)
      23                                                         5 (0)
      24                     2010-01-15 13:00:00 - 2010-01-17 22:36:00
      25 2010-01-16 13:00:00 [2010-01-15 15:24:00;2010-01-17 17:48:00]
      26                                2010-01-16 16:21:36 (1.1 days)
      27                                                         5 (0)
      28                                                  74.3 / 320.4
      29                                            159.5 [96.1;249.5]
      30                                                 180.0 (103.9)
      31                                                         5 (0)
      32                                                    5 (18.52%)
      33                                                             0
      34                                                    5 (18.52%)
                                                                 Total
      1                                                    10.4 / 33.9
      2                                               19.2 [15.4;22.8]
      3                                                     20.1 (6.0)
      4                                                         32 (0)
      5                                                     9 (37.50%)
      6                                                    15 (62.50%)
      7                                                              8
      8                                                   32 (100.00%)
      9                                                    10 (37.04%)
      10                                                    4 (14.81%)
      11                                                   13 (48.15%)
      12                                                             5
      13                                                  32 (100.00%)
      14                                                  32 (100.00%)
      15                                                  32 (100.00%)
      16                                                   0.84 (5/27)
      17                                                   0.64 (6/16)
      18                                             304 [120.1 ; 472]
      19                                                          <NA>
      20                                       2010-02-22 - 2010-12-02
      21                            2010-05-04 [2010-04-06;2010-06-30]
      22                                       2010-05-27 (2.3 months)
      23                                                        32 (0)
      24                     2010-01-15 13:00:00 - 2010-01-23 22:36:00
      25 2010-01-18 18:02:24 [2010-01-17 21:52:48;2010-01-19 22:36:00]
      26                                2010-01-18 21:22:12 (1.8 days)
      27                                                        32 (0)
      28                                                  33.5 / 320.4
      29                                            104.4 [76.3;162.5]
      30                                                  128.8 (69.8)
      31                                                        32 (0)
      32                                                  27 (100.00%)
      33                                                             5
      34                                                  32 (100.00%)
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

