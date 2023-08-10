# showNA with NA in by

    Code
      x0 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, times = c(0, 100, 200, 400))
      as.data.frame(x0)
    Output
          .id                    label        variable         straight          vshaped               NA
      1    am             Transmission            auto       2 (18.18%)       9 (81.82%)                8
      2    am             Transmission          manual       7 (53.85%)       6 (46.15%)                0
      3   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9      10.4 / 26.0      14.3 / 24.4
      4   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4] 18.4 [17.5;20.1]
      5   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)       16.6 (4.2)       19.0 (3.3)
      6   mpg        Miles/(US) gallon          N (NA)            9 (0)           15 (0)            8 (0)
      7   cyl      Number of cylinders               4       7 (87.50%)       1 (12.50%)                2
      8   cyl      Number of cylinders               6           0 (0%)      1 (100.00%)                3
      9   cyl      Number of cylinders               8           0 (0%)     11 (100.00%)                2
      10  cyl      Number of cylinders              NA                2                2                1
      11 surv Dummy survival (disp/am)             t=0       1.00 (0/9)      1.00 (0/15)       1.00 (0/8)
      12 surv Dummy survival (disp/am)           t=100       0.44 (5/4)      1.00 (0/15)       1.00 (0/8)
      13 surv Dummy survival (disp/am)           t=200       0.17 (2/1)      0.73 (4/11)       1.00 (0/4)
      14 surv Dummy survival (disp/am)           t=400       0.17 (0/0)       0.52 (2/4)       1.00 (0/0)
      15 surv Dummy survival (disp/am) Median survival             95.1             <NA>             <NA>
    Code
      x1 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, showNA = "no", times = c(0, 100, 200, 400))
      as.data.frame(x1)
    Output
          .id                    label        variable         straight          vshaped
      1    am             Transmission            auto       2 (18.18%)       9 (81.82%)
      2    am             Transmission          manual       7 (53.85%)       6 (46.15%)
      3   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9      10.4 / 26.0
      4   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4]
      5   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)       16.6 (4.2)
      6   mpg        Miles/(US) gallon          N (NA)            9 (0)           15 (0)
      7   cyl      Number of cylinders               4       7 (87.50%)       1 (12.50%)
      8   cyl      Number of cylinders               6           0 (0%)      1 (100.00%)
      9   cyl      Number of cylinders               8           0 (0%)     11 (100.00%)
      10 surv Dummy survival (disp/am)             t=0       1.00 (0/9)      1.00 (0/15)
      11 surv Dummy survival (disp/am)           t=100       0.44 (5/4)      1.00 (0/15)
      12 surv Dummy survival (disp/am)           t=200       0.17 (2/1)      0.73 (4/11)
      13 surv Dummy survival (disp/am)           t=400       0.17 (0/0)       0.52 (2/4)
      14 surv Dummy survival (disp/am) Median survival             95.1             <NA>
    Code
      x2 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, showNA = "ifany", times = c(0, 100, 200, 400))
      as.data.frame(x2)
    Output
          .id                    label        variable         straight          vshaped               NA
      1    am             Transmission            auto       2 (18.18%)       9 (81.82%)                8
      2    am             Transmission          manual       7 (53.85%)       6 (46.15%)                0
      3   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9      10.4 / 26.0      14.3 / 24.4
      4   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4] 18.4 [17.5;20.1]
      5   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)       16.6 (4.2)       19.0 (3.3)
      6   mpg        Miles/(US) gallon          N (NA)            9 (0)           15 (0)            8 (0)
      7   cyl      Number of cylinders               4       7 (87.50%)       1 (12.50%)                2
      8   cyl      Number of cylinders               6           0 (0%)      1 (100.00%)                3
      9   cyl      Number of cylinders               8           0 (0%)     11 (100.00%)                2
      10  cyl      Number of cylinders              NA                2                2                1
      11 surv Dummy survival (disp/am)             t=0       1.00 (0/9)      1.00 (0/15)       1.00 (0/8)
      12 surv Dummy survival (disp/am)           t=100       0.44 (5/4)      1.00 (0/15)       1.00 (0/8)
      13 surv Dummy survival (disp/am)           t=200       0.17 (2/1)      0.73 (4/11)       1.00 (0/4)
      14 surv Dummy survival (disp/am)           t=400       0.17 (0/0)       0.52 (2/4)       1.00 (0/0)
      15 surv Dummy survival (disp/am) Median survival             95.1             <NA>             <NA>
    Code
      x3 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, showNA = "always", times = c(0, 100, 200, 400))
      as.data.frame(x3)
    Output
          .id                    label        variable         straight          vshaped               NA
      1    am             Transmission            auto       2 (18.18%)       9 (81.82%)                8
      2    am             Transmission          manual       7 (53.85%)       6 (46.15%)                0
      3    am             Transmission              NA                0                0                0
      4   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9      10.4 / 26.0      14.3 / 24.4
      5   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4] 18.4 [17.5;20.1]
      6   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)       16.6 (4.2)       19.0 (3.3)
      7   mpg        Miles/(US) gallon          N (NA)            9 (0)           15 (0)            8 (0)
      8   cyl      Number of cylinders               4       7 (87.50%)       1 (12.50%)                2
      9   cyl      Number of cylinders               6           0 (0%)      1 (100.00%)                3
      10  cyl      Number of cylinders               8           0 (0%)     11 (100.00%)                2
      11  cyl      Number of cylinders              NA                2                2                1
      12 surv Dummy survival (disp/am)             t=0       1.00 (0/9)      1.00 (0/15)       1.00 (0/8)
      13 surv Dummy survival (disp/am)           t=100       0.44 (5/4)      1.00 (0/15)       1.00 (0/8)
      14 surv Dummy survival (disp/am)           t=200       0.17 (2/1)      0.73 (4/11)       1.00 (0/4)
      15 surv Dummy survival (disp/am)           t=400       0.17 (0/0)       0.52 (2/4)       1.00 (0/0)
      16 surv Dummy survival (disp/am) Median survival             95.1             <NA>             <NA>

# showNA without NA in by

    Code
      x0 = crosstable(mtcars3, c(vs, mpg, cyl, surv), by = am, times = c(0, 100, 200, 400))
      as.data.frame(x0)
    Output
          .id                    label        variable             auto           manual
      1    vs                   Engine        straight       2 (22.22%)       7 (77.78%)
      2    vs                   Engine         vshaped       9 (60.00%)       6 (40.00%)
      3    vs                   Engine              NA                8                0
      4   mpg        Miles/(US) gallon       Min / Max      10.4 / 24.4      15.0 / 33.9
      5   mpg        Miles/(US) gallon       Med [IQR] 17.3 [14.9;19.2] 22.8 [21.0;30.4]
      6   mpg        Miles/(US) gallon      Mean (std)       17.1 (3.8)       24.4 (6.2)
      7   mpg        Miles/(US) gallon          N (NA)           19 (0)           13 (0)
      8   cyl      Number of cylinders               4       3 (30.00%)       7 (70.00%)
      9   cyl      Number of cylinders               6       3 (75.00%)       1 (25.00%)
      10  cyl      Number of cylinders               8      11 (84.62%)       2 (15.38%)
      11  cyl      Number of cylinders              NA                2                3
      12 surv Dummy survival (disp/am)             t=0      1.00 (0/19)      1.00 (0/13)
      13 surv Dummy survival (disp/am)           t=100      1.00 (0/19)       0.62 (5/8)
      14 surv Dummy survival (disp/am)           t=200      1.00 (0/14)       0.15 (6/2)
      15 surv Dummy survival (disp/am)           t=400       1.00 (0/4)          0 (2/0)
      16 surv Dummy survival (disp/am) Median survival             <NA>            120.3
    Code
      x1 = crosstable(mtcars3, c(vs, mpg, cyl, surv), by = am, showNA = "no", times = c(0, 100, 200, 400))
      as.data.frame(x1)
    Output
          .id                    label        variable             auto           manual
      1    vs                   Engine        straight       2 (22.22%)       7 (77.78%)
      2    vs                   Engine         vshaped       9 (60.00%)       6 (40.00%)
      3   mpg        Miles/(US) gallon       Min / Max      10.4 / 24.4      15.0 / 33.9
      4   mpg        Miles/(US) gallon       Med [IQR] 17.3 [14.9;19.2] 22.8 [21.0;30.4]
      5   mpg        Miles/(US) gallon      Mean (std)       17.1 (3.8)       24.4 (6.2)
      6   mpg        Miles/(US) gallon          N (NA)           19 (0)           13 (0)
      7   cyl      Number of cylinders               4       3 (30.00%)       7 (70.00%)
      8   cyl      Number of cylinders               6       3 (75.00%)       1 (25.00%)
      9   cyl      Number of cylinders               8      11 (84.62%)       2 (15.38%)
      10 surv Dummy survival (disp/am)             t=0      1.00 (0/19)      1.00 (0/13)
      11 surv Dummy survival (disp/am)           t=100      1.00 (0/19)       0.62 (5/8)
      12 surv Dummy survival (disp/am)           t=200      1.00 (0/14)       0.15 (6/2)
      13 surv Dummy survival (disp/am)           t=400       1.00 (0/4)          0 (2/0)
      14 surv Dummy survival (disp/am) Median survival             <NA>            120.3
    Code
      x2 = crosstable(mtcars3, c(vs, mpg, cyl, surv), by = am, showNA = "ifany", times = c(0, 100, 200, 400))
      as.data.frame(x2)
    Output
          .id                    label        variable             auto           manual
      1    vs                   Engine        straight       2 (22.22%)       7 (77.78%)
      2    vs                   Engine         vshaped       9 (60.00%)       6 (40.00%)
      3    vs                   Engine              NA                8                0
      4   mpg        Miles/(US) gallon       Min / Max      10.4 / 24.4      15.0 / 33.9
      5   mpg        Miles/(US) gallon       Med [IQR] 17.3 [14.9;19.2] 22.8 [21.0;30.4]
      6   mpg        Miles/(US) gallon      Mean (std)       17.1 (3.8)       24.4 (6.2)
      7   mpg        Miles/(US) gallon          N (NA)           19 (0)           13 (0)
      8   cyl      Number of cylinders               4       3 (30.00%)       7 (70.00%)
      9   cyl      Number of cylinders               6       3 (75.00%)       1 (25.00%)
      10  cyl      Number of cylinders               8      11 (84.62%)       2 (15.38%)
      11  cyl      Number of cylinders              NA                2                3
      12 surv Dummy survival (disp/am)             t=0      1.00 (0/19)      1.00 (0/13)
      13 surv Dummy survival (disp/am)           t=100      1.00 (0/19)       0.62 (5/8)
      14 surv Dummy survival (disp/am)           t=200      1.00 (0/14)       0.15 (6/2)
      15 surv Dummy survival (disp/am)           t=400       1.00 (0/4)          0 (2/0)
      16 surv Dummy survival (disp/am) Median survival             <NA>            120.3
    Code
      x3 = crosstable(mtcars3, c(vs, mpg, cyl, surv), by = am, showNA = "always", times = c(0, 100, 200, 400))
      as.data.frame(x3)
    Output
          .id                    label        variable             auto           manual    NA
      1    vs                   Engine        straight       2 (22.22%)       7 (77.78%)     0
      2    vs                   Engine         vshaped       9 (60.00%)       6 (40.00%)     0
      3    vs                   Engine              NA                8                0     0
      4   mpg        Miles/(US) gallon       Min / Max      10.4 / 24.4      15.0 / 33.9 no NA
      5   mpg        Miles/(US) gallon       Med [IQR] 17.3 [14.9;19.2] 22.8 [21.0;30.4] no NA
      6   mpg        Miles/(US) gallon      Mean (std)       17.1 (3.8)       24.4 (6.2) no NA
      7   mpg        Miles/(US) gallon          N (NA)           19 (0)           13 (0) no NA
      8   cyl      Number of cylinders               4       3 (30.00%)       7 (70.00%)     0
      9   cyl      Number of cylinders               6       3 (75.00%)       1 (25.00%)     0
      10  cyl      Number of cylinders               8      11 (84.62%)       2 (15.38%)     0
      11  cyl      Number of cylinders              NA                2                3     0
      12 surv Dummy survival (disp/am)             t=0      1.00 (0/19)      1.00 (0/13)  <NA>
      13 surv Dummy survival (disp/am)           t=100      1.00 (0/19)       0.62 (5/8)  <NA>
      14 surv Dummy survival (disp/am)           t=200      1.00 (0/14)       0.15 (6/2)  <NA>
      15 surv Dummy survival (disp/am)           t=400       1.00 (0/4)          0 (2/0)  <NA>
      16 surv Dummy survival (disp/am) Median survival             <NA>            120.3  <NA>

# total

    Code
      x0 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, times = c(0, 100, 200, 400))
      as.data.frame(x0)
    Output
          .id                    label        variable         straight          vshaped               NA
      1    am             Transmission            auto       2 (18.18%)       9 (81.82%)                8
      2    am             Transmission          manual       7 (53.85%)       6 (46.15%)                0
      3   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9      10.4 / 26.0      14.3 / 24.4
      4   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4] 18.4 [17.5;20.1]
      5   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)       16.6 (4.2)       19.0 (3.3)
      6   mpg        Miles/(US) gallon          N (NA)            9 (0)           15 (0)            8 (0)
      7   cyl      Number of cylinders               4       7 (87.50%)       1 (12.50%)                2
      8   cyl      Number of cylinders               6           0 (0%)      1 (100.00%)                3
      9   cyl      Number of cylinders               8           0 (0%)     11 (100.00%)                2
      10  cyl      Number of cylinders              NA                2                2                1
      11 surv Dummy survival (disp/am)             t=0       1.00 (0/9)      1.00 (0/15)       1.00 (0/8)
      12 surv Dummy survival (disp/am)           t=100       0.44 (5/4)      1.00 (0/15)       1.00 (0/8)
      13 surv Dummy survival (disp/am)           t=200       0.17 (2/1)      0.73 (4/11)       1.00 (0/4)
      14 surv Dummy survival (disp/am)           t=400       0.17 (0/0)       0.52 (2/4)       1.00 (0/0)
      15 surv Dummy survival (disp/am) Median survival             95.1             <NA>             <NA>
    Code
      x1 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, total = "none", times = c(0, 100, 200, 400))
      as.data.frame(x1)
    Output
          .id                    label        variable         straight          vshaped               NA
      1    am             Transmission            auto       2 (18.18%)       9 (81.82%)                8
      2    am             Transmission          manual       7 (53.85%)       6 (46.15%)                0
      3   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9      10.4 / 26.0      14.3 / 24.4
      4   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4] 18.4 [17.5;20.1]
      5   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)       16.6 (4.2)       19.0 (3.3)
      6   mpg        Miles/(US) gallon          N (NA)            9 (0)           15 (0)            8 (0)
      7   cyl      Number of cylinders               4       7 (87.50%)       1 (12.50%)                2
      8   cyl      Number of cylinders               6           0 (0%)      1 (100.00%)                3
      9   cyl      Number of cylinders               8           0 (0%)     11 (100.00%)                2
      10  cyl      Number of cylinders              NA                2                2                1
      11 surv Dummy survival (disp/am)             t=0       1.00 (0/9)      1.00 (0/15)       1.00 (0/8)
      12 surv Dummy survival (disp/am)           t=100       0.44 (5/4)      1.00 (0/15)       1.00 (0/8)
      13 surv Dummy survival (disp/am)           t=200       0.17 (2/1)      0.73 (4/11)       1.00 (0/4)
      14 surv Dummy survival (disp/am)           t=400       0.17 (0/0)       0.52 (2/4)       1.00 (0/0)
      15 surv Dummy survival (disp/am) Median survival             95.1             <NA>             <NA>
    Code
      x2 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, total = "row", times = c(0, 100, 200, 400))
      as.data.frame(x2)
    Output
          .id                    label        variable         straight          vshaped               NA            Total
      1    am             Transmission            auto       2 (18.18%)       9 (81.82%)                8      19 (59.38%)
      2    am             Transmission          manual       7 (53.85%)       6 (46.15%)                0      13 (40.62%)
      3   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9      10.4 / 26.0      14.3 / 24.4      10.4 / 33.9
      4   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4] 18.4 [17.5;20.1] 19.2 [15.4;22.8]
      5   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)       16.6 (4.2)       19.0 (3.3)       20.1 (6.0)
      6   mpg        Miles/(US) gallon          N (NA)            9 (0)           15 (0)            8 (0)           32 (0)
      7   cyl      Number of cylinders               4       7 (87.50%)       1 (12.50%)                2      10 (37.04%)
      8   cyl      Number of cylinders               6           0 (0%)      1 (100.00%)                3       4 (14.81%)
      9   cyl      Number of cylinders               8           0 (0%)     11 (100.00%)                2      13 (48.15%)
      10  cyl      Number of cylinders              NA                2                2                1                5
      11 surv Dummy survival (disp/am)             t=0       1.00 (0/9)      1.00 (0/15)       1.00 (0/8)      1.00 (0/32)
      12 surv Dummy survival (disp/am)           t=100       0.44 (5/4)      1.00 (0/15)       1.00 (0/8)      0.84 (5/27)
      13 surv Dummy survival (disp/am)           t=200       0.17 (2/1)      0.73 (4/11)       1.00 (0/4)      0.64 (6/16)
      14 surv Dummy survival (disp/am)           t=400       0.17 (0/0)       0.52 (2/4)       1.00 (0/0)       0.50 (2/4)
      15 surv Dummy survival (disp/am) Median survival             95.1             <NA>             <NA>             <NA>
    Code
      x3 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, total = "col", times = c(0, 100, 200, 400))
      as.data.frame(x3)
    Output
          .id                    label        variable         straight          vshaped               NA
      1    am             Transmission            auto       2 (18.18%)       9 (81.82%)                8
      2    am             Transmission          manual       7 (53.85%)       6 (46.15%)                0
      3    am             Transmission           Total       9 (37.50%)      15 (62.50%)                8
      4   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9      10.4 / 26.0      14.3 / 24.4
      5   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4] 18.4 [17.5;20.1]
      6   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)       16.6 (4.2)       19.0 (3.3)
      7   mpg        Miles/(US) gallon          N (NA)            9 (0)           15 (0)            8 (0)
      8   cyl      Number of cylinders               4       7 (87.50%)       1 (12.50%)                2
      9   cyl      Number of cylinders               6           0 (0%)      1 (100.00%)                3
      10  cyl      Number of cylinders               8           0 (0%)     11 (100.00%)                2
      11  cyl      Number of cylinders              NA                2                2                1
      12  cyl      Number of cylinders           Total       9 (37.50%)      15 (62.50%)                8
      13 surv Dummy survival (disp/am)             t=0       1.00 (0/9)      1.00 (0/15)       1.00 (0/8)
      14 surv Dummy survival (disp/am)           t=100       0.44 (5/4)      1.00 (0/15)       1.00 (0/8)
      15 surv Dummy survival (disp/am)           t=200       0.17 (2/1)      0.73 (4/11)       1.00 (0/4)
      16 surv Dummy survival (disp/am)           t=400       0.17 (0/0)       0.52 (2/4)       1.00 (0/0)
      17 surv Dummy survival (disp/am) Median survival             95.1             <NA>             <NA>
    Code
      x4 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = vs, total = "both", times = c(0, 100, 200, 400))
      as.data.frame(x4)
    Output
          .id                    label        variable         straight          vshaped               NA            Total
      1    am             Transmission            auto       2 (18.18%)       9 (81.82%)                8      19 (59.38%)
      2    am             Transmission          manual       7 (53.85%)       6 (46.15%)                0      13 (40.62%)
      3    am             Transmission           Total       9 (37.50%)      15 (62.50%)                8     32 (100.00%)
      4   mpg        Miles/(US) gallon       Min / Max      21.4 / 33.9      10.4 / 26.0      14.3 / 24.4      10.4 / 33.9
      5   mpg        Miles/(US) gallon       Med [IQR] 27.3 [21.5;30.4] 15.5 [14.8;19.4] 18.4 [17.5;20.1] 19.2 [15.4;22.8]
      6   mpg        Miles/(US) gallon      Mean (std)       26.8 (5.1)       16.6 (4.2)       19.0 (3.3)       20.1 (6.0)
      7   mpg        Miles/(US) gallon          N (NA)            9 (0)           15 (0)            8 (0)           32 (0)
      8   cyl      Number of cylinders               4       7 (87.50%)       1 (12.50%)                2      10 (37.04%)
      9   cyl      Number of cylinders               6           0 (0%)      1 (100.00%)                3       4 (14.81%)
      10  cyl      Number of cylinders               8           0 (0%)     11 (100.00%)                2      13 (48.15%)
      11  cyl      Number of cylinders              NA                2                2                1                5
      12  cyl      Number of cylinders           Total       9 (37.50%)      15 (62.50%)                8     32 (100.00%)
      13 surv Dummy survival (disp/am)             t=0       1.00 (0/9)      1.00 (0/15)       1.00 (0/8)      1.00 (0/32)
      14 surv Dummy survival (disp/am)           t=100       0.44 (5/4)      1.00 (0/15)       1.00 (0/8)      0.84 (5/27)
      15 surv Dummy survival (disp/am)           t=200       0.17 (2/1)      0.73 (4/11)       1.00 (0/4)      0.64 (6/16)
      16 surv Dummy survival (disp/am)           t=400       0.17 (0/0)       0.52 (2/4)       1.00 (0/0)       0.50 (2/4)
      17 surv Dummy survival (disp/am) Median survival             95.1             <NA>             <NA>             <NA>

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
      x5 = crosstable(mtcars3, c(am, cyl), by = vs, total = "none", margin = "all")
      as.data.frame(x5)
    Output
        .id               label variable                      straight                        vshaped NA
      1  am        Transmission     auto   2 (8.33% / 18.18% / 22.22%)   9 (37.50% / 81.82% / 60.00%)  8
      2  am        Transmission   manual  7 (29.17% / 53.85% / 77.78%)   6 (25.00% / 46.15% / 40.00%)  0
      3 cyl Number of cylinders        4 7 (35.00% / 87.50% / 100.00%)     1 (5.00% / 12.50% / 7.69%)  2
      4 cyl Number of cylinders        6              0 (0% / 0% / 0%)    1 (5.00% / 100.00% / 7.69%)  3
      5 cyl Number of cylinders        8              0 (0% / 0% / 0%) 11 (55.00% / 100.00% / 84.62%)  2
      6 cyl Number of cylinders       NA                             2                              2  1
    Code
      x6 = crosstable(mtcars3, c(am, cyl), by = vs, total = "none", margin = 1:2)
      as.data.frame(x6)
    Output
        .id               label variable             straight               vshaped NA
      1  am        Transmission     auto  2 (18.18% / 22.22%)   9 (81.82% / 60.00%)  8
      2  am        Transmission   manual  7 (53.85% / 77.78%)   6 (46.15% / 40.00%)  0
      3 cyl Number of cylinders        4 7 (87.50% / 100.00%)    1 (12.50% / 7.69%)  2
      4 cyl Number of cylinders        6          0 (0% / 0%)   1 (100.00% / 7.69%)  3
      5 cyl Number of cylinders        8          0 (0% / 0%) 11 (100.00% / 84.62%)  2
      6 cyl Number of cylinders       NA                    2                     2  1

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
      8 cyl Number of cylinders    Total 9 (37.50%)  15 (62.50%)  8 32 (100.00%)
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
      8 cyl Number of cylinders    Total 9 (37.50%)  15 (62.50%)  8 32 (100.00%)
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
      8 cyl Number of cylinders    Total  9 (37.50%) 15 (62.50%)  8 32 (100.00%)
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
      8 cyl Number of cylinders    Total 9 (37.50%) 15 (62.50%)  8 32 (100.00%)
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
      x5 = crosstable(mtcars3, c(am, cyl), by = vs, total = "both", margin = "all")
      as.data.frame(x5)
    Output
        .id               label variable                      straight                        vshaped NA        Total
      1  am        Transmission     auto   2 (8.33% / 18.18% / 22.22%)   9 (37.50% / 81.82% / 60.00%)  8  19 (59.38%)
      2  am        Transmission   manual  7 (29.17% / 53.85% / 77.78%)   6 (25.00% / 46.15% / 40.00%)  0  13 (40.62%)
      3  am        Transmission    Total                    9 (37.50%)                    15 (62.50%)  8 32 (100.00%)
      4 cyl Number of cylinders        4 7 (35.00% / 87.50% / 100.00%)     1 (5.00% / 12.50% / 7.69%)  2  10 (37.04%)
      5 cyl Number of cylinders        6              0 (0% / 0% / 0%)    1 (5.00% / 100.00% / 7.69%)  3   4 (14.81%)
      6 cyl Number of cylinders        8              0 (0% / 0% / 0%) 11 (55.00% / 100.00% / 84.62%)  2  13 (48.15%)
      7 cyl Number of cylinders       NA                             2                              2  1            5
      8 cyl Number of cylinders    Total                    9 (37.50%)                    15 (62.50%)  8 32 (100.00%)
    Code
      x6 = crosstable(mtcars3, c(am, cyl), by = vs, total = "both", margin = 1:2)
      as.data.frame(x6)
    Output
        .id               label variable             straight               vshaped NA        Total
      1  am        Transmission     auto  2 (18.18% / 22.22%)   9 (81.82% / 60.00%)  8  19 (59.38%)
      2  am        Transmission   manual  7 (53.85% / 77.78%)   6 (46.15% / 40.00%)  0  13 (40.62%)
      3  am        Transmission    Total           9 (37.50%)           15 (62.50%)  8 32 (100.00%)
      4 cyl Number of cylinders        4 7 (87.50% / 100.00%)    1 (12.50% / 7.69%)  2  10 (37.04%)
      5 cyl Number of cylinders        6          0 (0% / 0%)   1 (100.00% / 7.69%)  3   4 (14.81%)
      6 cyl Number of cylinders        8          0 (0% / 0%) 11 (100.00% / 84.62%)  2  13 (48.15%)
      7 cyl Number of cylinders       NA                    2                     2  1            5
      8 cyl Number of cylinders    Total           9 (37.50%)           15 (62.50%)  8 32 (100.00%)

# Percent pattern

    Code
      x0 = crosstable(mtcars3, cyl, percent_digits = 0, total = TRUE, showNA = "always", percent_pattern = PERCENT_PATTERN)
      as.data.frame(x0)
    Output
        .id               label variable                                                                                                                     value
      1 cyl Number of cylinders        4 N=10\nCell: p = 37% (10/27) [95%CI 22%; 56%]\nCol: p = 37% (10/27) [95%CI 22%; 56%]\nRow:p = 37% (10/27) [95%CI 22%; 56%]
      2 cyl Number of cylinders        6        N=4\nCell: p = 15% (4/27) [95%CI 6%; 32%]\nCol: p = 15% (4/27) [95%CI 6%; 32%]\nRow:p = 15% (4/27) [95%CI 6%; 32%]
      3 cyl Number of cylinders        8 N=13\nCell: p = 48% (13/27) [95%CI 31%; 66%]\nCol: p = 48% (13/27) [95%CI 31%; 66%]\nRow:p = 48% (13/27) [95%CI 31%; 66%]
      4 cyl Number of cylinders       NA                                                                                                                         5
      5 cyl Number of cylinders    Total                                                                                                                 32 (100%)
    Code
      x1 = crosstable(mtcars3, cyl, by = am, percent_digits = 0, total = TRUE, showNA = "always", percent_pattern = PERCENT_PATTERN)
      as.data.frame(x1)
    Output
        .id               label variable                                                                                                                      auto                                                                                                                manual   NA     Total
      1 cyl Number of cylinders        4       N=3\nCell: p = 11% (3/27) [95%CI 4%; 28%]\nCol: p = 18% (3/17) [95%CI 6%; 41%]\nRow:p = 30% (3/10) [95%CI 11%; 60%] N=7\nCell: p = 26% (7/27) [95%CI 13%; 45%]\nCol: p = 70% (7/10) [95%CI 40%; 89%]\nRow:p = 70% (7/10) [95%CI 40%; 89%]    0  10 (37%)
      2 cyl Number of cylinders        6        N=3\nCell: p = 11% (3/27) [95%CI 4%; 28%]\nCol: p = 18% (3/17) [95%CI 6%; 41%]\nRow:p = 75% (3/4) [95%CI 30%; 95%]      N=1\nCell: p = 4% (1/27) [95%CI 1%; 18%]\nCol: p = 10% (1/10) [95%CI 2%; 40%]\nRow:p = 25% (1/4) [95%CI 5%; 70%]    0   4 (15%)
      3 cyl Number of cylinders        8 N=11\nCell: p = 41% (11/27) [95%CI 25%; 59%]\nCol: p = 65% (11/17) [95%CI 41%; 83%]\nRow:p = 85% (11/13) [95%CI 58%; 96%]     N=2\nCell: p = 7% (2/27) [95%CI 2%; 23%]\nCol: p = 20% (2/10) [95%CI 6%; 51%]\nRow:p = 15% (2/13) [95%CI 4%; 42%]    0  13 (48%)
      4 cyl Number of cylinders       NA                                                                                                                         2                                                                                                                     3    0         5
      5 cyl Number of cylinders    Total                                                                                                                  19 (59%)                                                                                                              13 (41%) <NA> 32 (100%)
    Code
      x2 = crosstable(mtcars3, c(mpg, vs, cyl), by = c(am, dummy), percent_digits = 0, total = TRUE, showNA = "always", percent_pattern = PERCENT_PATTERN)
      as.data.frame(x2)
    Output
         .id               label   variable                                                                                                     am=auto & dummy=dummy                                                                                               am=manual & dummy=dummy    NA            Total
      1  mpg   Miles/(US) gallon  Min / Max                                                                                                               10.4 / 24.4                                                                                                           15.0 / 33.9 no NA      10.4 / 33.9
      2  mpg   Miles/(US) gallon  Med [IQR]                                                                                                          17.3 [14.9;19.2]                                                                                                      22.8 [21.0;30.4] no NA 19.2 [15.4;22.8]
      3  mpg   Miles/(US) gallon Mean (std)                                                                                                                17.1 (3.8)                                                                                                            24.4 (6.2) no NA       20.1 (6.0)
      4  mpg   Miles/(US) gallon     N (NA)                                                                                                                    19 (0)                                                                                                                13 (0) no NA           32 (0)
      5   vs              Engine   straight          N=2\nCell: p = 8% (2/24) [95%CI 2%; 26%]\nCol: p = 18% (2/11) [95%CI 5%; 48%]\nRow:p = 22% (2/9) [95%CI 6%; 55%]  N=7\nCell: p = 29% (7/24) [95%CI 15%; 49%]\nCol: p = 54% (7/13) [95%CI 29%; 77%]\nRow:p = 78% (7/9) [95%CI 45%; 94%]     0          9 (38%)
      6   vs              Engine    vshaped     N=9\nCell: p = 38% (9/24) [95%CI 21%; 57%]\nCol: p = 82% (9/11) [95%CI 52%; 95%]\nRow:p = 60% (9/15) [95%CI 36%; 80%] N=6\nCell: p = 25% (6/24) [95%CI 12%; 45%]\nCol: p = 46% (6/13) [95%CI 23%; 71%]\nRow:p = 40% (6/15) [95%CI 20%; 64%]     0         15 (62%)
      7   vs              Engine         NA                                                                                                                         8                                                                                                                     0     0                8
      8   vs              Engine      Total                                                                                                                  19 (59%)                                                                                                              13 (41%)  <NA>        32 (100%)
      9  cyl Number of cylinders          4       N=3\nCell: p = 11% (3/27) [95%CI 4%; 28%]\nCol: p = 18% (3/17) [95%CI 6%; 41%]\nRow:p = 30% (3/10) [95%CI 11%; 60%] N=7\nCell: p = 26% (7/27) [95%CI 13%; 45%]\nCol: p = 70% (7/10) [95%CI 40%; 89%]\nRow:p = 70% (7/10) [95%CI 40%; 89%]     0         10 (37%)
      10 cyl Number of cylinders          6        N=3\nCell: p = 11% (3/27) [95%CI 4%; 28%]\nCol: p = 18% (3/17) [95%CI 6%; 41%]\nRow:p = 75% (3/4) [95%CI 30%; 95%]      N=1\nCell: p = 4% (1/27) [95%CI 1%; 18%]\nCol: p = 10% (1/10) [95%CI 2%; 40%]\nRow:p = 25% (1/4) [95%CI 5%; 70%]     0          4 (15%)
      11 cyl Number of cylinders          8 N=11\nCell: p = 41% (11/27) [95%CI 25%; 59%]\nCol: p = 65% (11/17) [95%CI 41%; 83%]\nRow:p = 85% (11/13) [95%CI 58%; 96%]     N=2\nCell: p = 7% (2/27) [95%CI 2%; 23%]\nCol: p = 20% (2/10) [95%CI 6%; 51%]\nRow:p = 15% (2/13) [95%CI 4%; 42%]     0         13 (48%)
      12 cyl Number of cylinders         NA                                                                                                                         2                                                                                                                     3     0                5
      13 cyl Number of cylinders      Total                                                                                                                  19 (59%)                                                                                                              13 (41%)  <NA>        32 (100%)

# Percent pattern - Ultimate

    Code
      x1 = crosstable(mtcars3, cyl, by = vs, percent_digits = 0, total = TRUE, showNA = "always", percent_pattern = ULTIMATE_PATTERN)
      as.data.frame(x1)
    Output
        .id               label variable                                                                                                                                                                                                                 straight                                                                                                                                                                                                                            vshaped NA                                                                       Total
      1 cyl Number of cylinders        4 N=7\nCell: p = 35% (7/20) [2e+01%; 57%]\nCol: p = 100% (7/7) [65%; 100%]\nRow: p = 88% (7/8) [53%; 98%]\n\nCell (NA): p = 22% (7/32) [11%; 39%]\nCol (NA): p = 78% (7/9) [45%; 94%]\nRow (NA): p = 70% (7/10) [40%; 89%]                    N=1\nCell: p = 5% (1/20) [9e-01%; 24%]\nCol: p = 8% (1/13) [1%; 33%]\nRow: p = 12% (1/8) [2%; 47%]\n\nCell (NA): p = 3% (1/32) [1%; 16%]\nCol (NA): p = 7% (1/15) [1%; 30%]\nRow (NA): p = 10% (1/10) [2%; 40%]  2 N=10\nCol: p = 37% (10/27) [22%; 56%]\nCol (NA): p = 31% (10/32) [18%; 49%]
      2 cyl Number of cylinders        6               N=0\nCell: p = 0% (0/20) [1e-15%; 16%]\nCol: p = 0% (0/7) [0%; 35%]\nRow: p = 0% (0/1) [0%; 79%]\n\nCell (NA): p = 0% (0/32) [0%; 11%]\nCol (NA): p = 0% (0/9) [0%; 30%]\nRow (NA): p = 0% (0/4) [0%; 49%]                  N=1\nCell: p = 5% (1/20) [9e-01%; 24%]\nCol: p = 8% (1/13) [1%; 33%]\nRow: p = 100% (1/1) [21%; 100%]\n\nCell (NA): p = 3% (1/32) [1%; 16%]\nCol (NA): p = 7% (1/15) [1%; 30%]\nRow (NA): p = 25% (1/4) [5%; 70%]  3      N=4\nCol: p = 15% (4/27) [6%; 32%]\nCol (NA): p = 12% (4/32) [5%; 28%]
      3 cyl Number of cylinders        8             N=0\nCell: p = 0% (0/20) [1e-15%; 16%]\nCol: p = 0% (0/7) [0%; 35%]\nRow: p = 0% (0/11) [0%; 26%]\n\nCell (NA): p = 0% (0/32) [0%; 11%]\nCol (NA): p = 0% (0/9) [0%; 30%]\nRow (NA): p = 0% (0/13) [0%; 23%] N=11\nCell: p = 55% (11/20) [3e+01%; 74%]\nCol: p = 85% (11/13) [58%; 96%]\nRow: p = 100% (11/11) [74%; 100%]\n\nCell (NA): p = 34% (11/32) [20%; 52%]\nCol (NA): p = 73% (11/15) [48%; 89%]\nRow (NA): p = 85% (11/13) [58%; 96%]  2 N=13\nCol: p = 48% (13/27) [31%; 66%]\nCol (NA): p = 41% (13/32) [26%; 58%]
      4 cyl Number of cylinders       NA                                                                                                                                                                                                                        2                                                                                                                                                                                                                                  2  1                                                                           5
      5 cyl Number of cylinders    Total                                                                                                                                                 N=9\nRow: p = 38% (9/24) [21%; 57%]\nRow (NA): p = 28% (9/32) [16%; 45%]                                                                                                                                                        N=15\nRow: p = 62% (15/24) [43%; 79%]\nRow (NA): p = 47% (15/32) [31%; 64%]  8                         N=32\nP: 100% [89%; 100%]\nP (NA): 100% [89%; 100%]
    Code
      x2 = crosstable(mtcars3, cyl, by = vs, percent_digits = 0, total = TRUE, showNA = "no", percent_pattern = ULTIMATE_PATTERN)
      as.data.frame(x2)
    Output
        .id               label variable                                                                                                                                                                                                                     straight                                                                                                                                                                                                                                 vshaped                                                                          Total
      1 cyl Number of cylinders        4 N=7\nCell: p = 35% (7/20) [2e+01%; 57%]\nCol: p = 100% (7/7) [65%; 100%]\nRow: p = 88% (7/8) [53%; 98%]\n\nCell (NA): p = 35% (7/20) [2e+01%; 57%]\nCol (NA): p = 100% (7/7) [65%; 100%]\nRow (NA): p = 88% (7/8) [53%; 98%]                      N=1\nCell: p = 5% (1/20) [9e-01%; 24%]\nCol: p = 8% (1/13) [1%; 33%]\nRow: p = 12% (1/8) [2%; 47%]\n\nCell (NA): p = 5% (1/20) [9e-01%; 24%]\nCol (NA): p = 8% (1/13) [1%; 33%]\nRow (NA): p = 12% (1/8) [2%; 47%]    N=8\nCol: p = 40% (8/20) [22%; 61%]\nCol (NA): p = 40% (8/20) [2e+01%; 61%]
      2 cyl Number of cylinders        6               N=0\nCell: p = 0% (0/20) [1e-15%; 16%]\nCol: p = 0% (0/7) [0%; 35%]\nRow: p = 0% (0/1) [0%; 79%]\n\nCell (NA): p = 0% (0/20) [1e-15%; 16%]\nCol (NA): p = 0% (0/7) [0%; 35%]\nRow (NA): p = 0% (0/1) [0%; 79%]                N=1\nCell: p = 5% (1/20) [9e-01%; 24%]\nCol: p = 8% (1/13) [1%; 33%]\nRow: p = 100% (1/1) [21%; 100%]\n\nCell (NA): p = 5% (1/20) [9e-01%; 24%]\nCol (NA): p = 8% (1/13) [1%; 33%]\nRow (NA): p = 100% (1/1) [21%; 100%]       N=1\nCol: p = 5% (1/20) [1%; 24%]\nCol (NA): p = 5% (1/20) [9e-01%; 24%]
      3 cyl Number of cylinders        8             N=0\nCell: p = 0% (0/20) [1e-15%; 16%]\nCol: p = 0% (0/7) [0%; 35%]\nRow: p = 0% (0/11) [0%; 26%]\n\nCell (NA): p = 0% (0/20) [1e-15%; 16%]\nCol (NA): p = 0% (0/7) [0%; 35%]\nRow (NA): p = 0% (0/11) [0%; 26%] N=11\nCell: p = 55% (11/20) [3e+01%; 74%]\nCol: p = 85% (11/13) [58%; 96%]\nRow: p = 100% (11/11) [74%; 100%]\n\nCell (NA): p = 55% (11/20) [3e+01%; 74%]\nCol (NA): p = 85% (11/13) [58%; 96%]\nRow (NA): p = 100% (11/11) [74%; 100%] N=11\nCol: p = 55% (11/20) [34%; 74%]\nCol (NA): p = 55% (11/20) [3e+01%; 74%]
      4 cyl Number of cylinders    Total                                                                                                                                                     N=7\nRow: p = 35% (7/20) [18%; 57%]\nRow (NA): p = 35% (7/20) [18%; 57%]                                                                                                                                                             N=13\nRow: p = 65% (13/20) [43%; 82%]\nRow (NA): p = 65% (13/20) [43%; 82%]                            N=20\nP: 100% [84%; 100%]\nP (NA): 100% [84%; 100%]

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
      x1 = crosstable(mtcars3, carb)
      as.data.frame(x1)
    Output
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

---

    Code
      x2 = crosstable(mtcars3, c(am, mpg, cyl, surv), by = dummy2)
      as.data.frame(x2)
    Output
          .id                    label        variable            dummy               NA
      1    am             Transmission            auto     11 (100.00%)                8
      2    am             Transmission          manual     13 (100.00%)                0
      3   mpg        Miles/(US) gallon       Min / Max      10.4 / 33.9      14.3 / 24.4
      4   mpg        Miles/(US) gallon       Med [IQR] 20.4 [15.2;23.6] 18.4 [17.5;20.1]
      5   mpg        Miles/(US) gallon      Mean (std)       20.5 (6.7)       19.0 (3.3)
      6   mpg        Miles/(US) gallon          N (NA)           24 (0)            8 (0)
      7   cyl      Number of cylinders               4      8 (100.00%)                2
      8   cyl      Number of cylinders               6      1 (100.00%)                3
      9   cyl      Number of cylinders               8     11 (100.00%)                2
      10  cyl      Number of cylinders              NA                4                1
      11 surv Dummy survival (disp/am)          t=71.1      0.96 (1/24)       1.00 (0/8)
      12 surv Dummy survival (disp/am)          t=75.7      0.92 (1/23)       1.00 (0/8)
      13 surv Dummy survival (disp/am)          t=78.7      0.88 (1/22)       1.00 (0/8)
      14 surv Dummy survival (disp/am)            t=79      0.83 (1/21)       1.00 (0/8)
      15 surv Dummy survival (disp/am)          t=95.1      0.79 (1/20)       1.00 (0/8)
      16 surv Dummy survival (disp/am)           t=108      0.75 (1/19)       1.00 (0/8)
      17 surv Dummy survival (disp/am)         t=120.1      0.75 (0/18)       1.00 (0/8)
      18 surv Dummy survival (disp/am)         t=120.3      0.71 (1/17)       1.00 (0/8)
      19 surv Dummy survival (disp/am)           t=121      0.66 (1/16)       1.00 (0/8)
      20 surv Dummy survival (disp/am)         t=140.8      0.66 (0/15)       1.00 (0/8)
      21 surv Dummy survival (disp/am)           t=145      0.62 (1/15)       1.00 (0/7)
      22 surv Dummy survival (disp/am)         t=146.7      0.62 (0/14)       1.00 (0/7)
      23 surv Dummy survival (disp/am)           t=160      0.53 (2/14)       1.00 (0/6)
      24 surv Dummy survival (disp/am)         t=167.6      0.53 (0/12)       1.00 (0/6)
      25 surv Dummy survival (disp/am)           t=225      0.53 (0/12)       1.00 (0/4)
      26 surv Dummy survival (disp/am)           t=258      0.53 (0/12)       1.00 (0/3)
      27 surv Dummy survival (disp/am)         t=275.8      0.53 (0/11)       1.00 (0/3)
      28 surv Dummy survival (disp/am)         t=275.8      0.53 (0/11)       1.00 (0/3)
      29 surv Dummy survival (disp/am)           t=301       0.47 (1/9)       1.00 (0/2)
      30 surv Dummy survival (disp/am)           t=304       0.47 (0/8)       1.00 (0/2)
      31 surv Dummy survival (disp/am)           t=318       0.47 (0/7)       1.00 (0/2)
      32 surv Dummy survival (disp/am)           t=350       0.47 (0/6)       1.00 (0/2)
      33 surv Dummy survival (disp/am)           t=351       0.38 (1/5)       1.00 (0/2)
      34 surv Dummy survival (disp/am)           t=360       0.38 (0/4)       1.00 (0/2)
      35 surv Dummy survival (disp/am)           t=400       0.38 (0/4)       1.00 (0/0)
      36 surv Dummy survival (disp/am)           t=440       0.38 (0/3)       1.00 (0/0)
      37 surv Dummy survival (disp/am)           t=460       0.38 (0/2)       1.00 (0/0)
      38 surv Dummy survival (disp/am)           t=472       0.38 (0/1)       1.00 (0/0)
      39 surv Dummy survival (disp/am) Median survival              301             <NA>
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

# By multiple

    Code
      x0 = crosstable(mtcars3, c(mpg, gear), by = c(cyl, am, vs))
      as.data.frame(x0)
    Output
         .id                   label   variable cyl=4 & am=auto & vs=straight cyl=6 & am=auto & vs=straight cyl=8 & am=auto & vs=straight cyl=NA & am=auto & vs=straight cyl=4 & am=manual & vs=straight cyl=6 & am=manual & vs=straight cyl=8 & am=manual & vs=straight cyl=NA & am=manual & vs=straight cyl=4 & am=auto & vs=vshaped cyl=6 & am=auto & vs=vshaped cyl=8 & am=auto & vs=vshaped cyl=NA & am=auto & vs=vshaped cyl=4 & am=manual & vs=vshaped cyl=6 & am=manual & vs=vshaped cyl=8 & am=manual & vs=vshaped cyl=NA & am=manual & vs=vshaped cyl=4 & am=auto & vs=NA cyl=6 & am=auto & vs=NA cyl=8 & am=auto & vs=NA cyl=NA & am=auto & vs=NA cyl=4 & am=manual & vs=NA cyl=6 & am=manual & vs=NA cyl=8 & am=manual & vs=NA cyl=NA & am=manual & vs=NA
      1  mpg       Miles/(US) gallon  Min / Max                   21.5 / 21.5                       NA / NA                       NA / NA                    21.4 / 21.4                     21.4 / 33.9                         NA / NA                         NA / NA                      22.8 / 22.8                      NA / NA                      NA / NA                  10.4 / 19.2                       NA / NA                    26.0 / 26.0                    19.7 / 19.7                    15.0 / 15.8                     21.0 / 21.0             22.8 / 24.4             17.8 / 19.2             14.3 / 16.4              18.7 / 18.7                   NA / NA                   NA / NA                   NA / NA                    NA / NA
      2  mpg       Miles/(US) gallon  Med [IQR]              21.5 [21.5;21.5]                    NA [NA;NA]                    NA [NA;NA]               21.4 [21.4;21.4]                30.4 [28.1;31.9]                      NA [NA;NA]                      NA [NA;NA]                 22.8 [22.8;22.8]                   NA [NA;NA]                   NA [NA;NA]             15.2 [13.3;15.5]                    NA [NA;NA]               26.0 [26.0;26.0]               19.7 [19.7;19.7]               15.4 [15.2;15.6]                21.0 [21.0;21.0]        23.6 [23.2;24.0]        18.1 [18.0;18.6]        15.3 [14.8;15.9]         18.7 [18.7;18.7]                NA [NA;NA]                NA [NA;NA]                NA [NA;NA]                 NA [NA;NA]
      3  mpg       Miles/(US) gallon Mean (std)                     21.5 (NA)                       NA (NA)                       NA (NA)                      21.4 (NA)                      29.3 (4.5)                         NA (NA)                         NA (NA)                        22.8 (NA)                      NA (NA)                      NA (NA)                   14.6 (2.9)                       NA (NA)                      26.0 (NA)                      19.7 (NA)                     15.4 (0.6)                        21.0 (0)              23.6 (1.1)              18.4 (0.7)              15.3 (1.5)                18.7 (NA)                   NA (NA)                   NA (NA)                   NA (NA)                    NA (NA)
      4  mpg       Miles/(US) gallon     N (NA)                         1 (0)                         0 (0)                         0 (0)                          1 (0)                           6 (0)                           0 (0)                           0 (0)                            1 (0)                        0 (0)                        0 (0)                        9 (0)                         0 (0)                          1 (0)                          1 (0)                          2 (0)                           2 (0)                   2 (0)                   3 (0)                   2 (0)                    1 (0)                     0 (0)                     0 (0)                     0 (0)                      0 (0)
      5 gear Number of forward gears          3                     1 (6.67%)                        0 (0%)                        0 (0%)                      1 (6.67%)                          0 (0%)                          0 (0%)                          0 (0%)                           0 (0%)                       0 (0%)                       0 (0%)                   9 (60.00%)                        0 (0%)                         0 (0%)                         0 (0%)                         0 (0%)                          0 (0%)                  0 (0%)               1 (6.67%)              2 (13.33%)                1 (6.67%)                    0 (0%)                    0 (0%)                    0 (0%)                     0 (0%)
      6 gear Number of forward gears          4                        0 (0%)                        0 (0%)                        0 (0%)                         0 (0%)                      5 (41.67%)                          0 (0%)                          0 (0%)                        1 (8.33%)                       0 (0%)                       0 (0%)                       0 (0%)                        0 (0%)                         0 (0%)                         0 (0%)                         0 (0%)                      2 (16.67%)              2 (16.67%)              2 (16.67%)                  0 (0%)                   0 (0%)                    0 (0%)                    0 (0%)                    0 (0%)                     0 (0%)
      7 gear Number of forward gears          5                        0 (0%)                        0 (0%)                        0 (0%)                         0 (0%)                      1 (20.00%)                          0 (0%)                          0 (0%)                           0 (0%)                       0 (0%)                       0 (0%)                       0 (0%)                        0 (0%)                     1 (20.00%)                     1 (20.00%)                     2 (40.00%)                          0 (0%)                  0 (0%)                  0 (0%)                  0 (0%)                   0 (0%)                    0 (0%)                    0 (0%)                    0 (0%)                     0 (0%)
    Code
      x1 = crosstable(mtcars3, c(mpg, gear), by = c(cyl, am, vs), showNA = FALSE)
      as.data.frame(x1)
    Output
         .id                   label   variable cyl=4 & am=auto & vs=straight cyl=6 & am=auto & vs=straight cyl=8 & am=auto & vs=straight cyl=NA & am=auto & vs=straight cyl=4 & am=manual & vs=straight cyl=6 & am=manual & vs=straight cyl=8 & am=manual & vs=straight cyl=NA & am=manual & vs=straight cyl=4 & am=auto & vs=vshaped cyl=6 & am=auto & vs=vshaped cyl=8 & am=auto & vs=vshaped cyl=NA & am=auto & vs=vshaped cyl=4 & am=manual & vs=vshaped cyl=6 & am=manual & vs=vshaped cyl=8 & am=manual & vs=vshaped cyl=NA & am=manual & vs=vshaped
      1  mpg       Miles/(US) gallon  Min / Max                   21.5 / 21.5                       NA / NA                       NA / NA                    21.4 / 21.4                     21.4 / 33.9                         NA / NA                         NA / NA                      22.8 / 22.8                      NA / NA                      NA / NA                  10.4 / 19.2                       NA / NA                    26.0 / 26.0                    19.7 / 19.7                    15.0 / 15.8                     21.0 / 21.0
      2  mpg       Miles/(US) gallon  Med [IQR]              21.5 [21.5;21.5]                    NA [NA;NA]                    NA [NA;NA]               21.4 [21.4;21.4]                30.4 [28.1;31.9]                      NA [NA;NA]                      NA [NA;NA]                 22.8 [22.8;22.8]                   NA [NA;NA]                   NA [NA;NA]             15.2 [13.3;15.5]                    NA [NA;NA]               26.0 [26.0;26.0]               19.7 [19.7;19.7]               15.4 [15.2;15.6]                21.0 [21.0;21.0]
      3  mpg       Miles/(US) gallon Mean (std)                     21.5 (NA)                       NA (NA)                       NA (NA)                      21.4 (NA)                      29.3 (4.5)                         NA (NA)                         NA (NA)                        22.8 (NA)                      NA (NA)                      NA (NA)                   14.6 (2.9)                       NA (NA)                      26.0 (NA)                      19.7 (NA)                     15.4 (0.6)                        21.0 (0)
      4  mpg       Miles/(US) gallon     N (NA)                         1 (0)                         0 (0)                         0 (0)                          1 (0)                           6 (0)                           0 (0)                           0 (0)                            1 (0)                        0 (0)                        0 (0)                        9 (0)                         0 (0)                          1 (0)                          1 (0)                          2 (0)                           2 (0)
      5 gear Number of forward gears          3                     1 (9.09%)                        0 (0%)                        0 (0%)                      1 (9.09%)                          0 (0%)                          0 (0%)                          0 (0%)                           0 (0%)                       0 (0%)                       0 (0%)                   9 (81.82%)                        0 (0%)                         0 (0%)                         0 (0%)                         0 (0%)                          0 (0%)
      6 gear Number of forward gears          4                        0 (0%)                        0 (0%)                        0 (0%)                         0 (0%)                      5 (62.50%)                          0 (0%)                          0 (0%)                       1 (12.50%)                       0 (0%)                       0 (0%)                       0 (0%)                        0 (0%)                         0 (0%)                         0 (0%)                         0 (0%)                      2 (25.00%)
      7 gear Number of forward gears          5                        0 (0%)                        0 (0%)                        0 (0%)                         0 (0%)                      1 (20.00%)                          0 (0%)                          0 (0%)                           0 (0%)                       0 (0%)                       0 (0%)                       0 (0%)                        0 (0%)                     1 (20.00%)                     1 (20.00%)                     2 (40.00%)                          0 (0%)
    Code
      x2 = crosstable(mtcars3, c(mpg, gear), by = c(cyl, am, vs), total = TRUE)
      as.data.frame(x2)
    Output
         .id                   label   variable cyl=4 & am=auto & vs=straight cyl=6 & am=auto & vs=straight cyl=8 & am=auto & vs=straight cyl=NA & am=auto & vs=straight cyl=4 & am=manual & vs=straight cyl=6 & am=manual & vs=straight cyl=8 & am=manual & vs=straight cyl=NA & am=manual & vs=straight cyl=4 & am=auto & vs=vshaped cyl=6 & am=auto & vs=vshaped cyl=8 & am=auto & vs=vshaped cyl=NA & am=auto & vs=vshaped cyl=4 & am=manual & vs=vshaped cyl=6 & am=manual & vs=vshaped cyl=8 & am=manual & vs=vshaped cyl=NA & am=manual & vs=vshaped cyl=4 & am=auto & vs=NA cyl=6 & am=auto & vs=NA cyl=8 & am=auto & vs=NA cyl=NA & am=auto & vs=NA cyl=4 & am=manual & vs=NA cyl=6 & am=manual & vs=NA cyl=8 & am=manual & vs=NA cyl=NA & am=manual & vs=NA            Total
      1  mpg       Miles/(US) gallon  Min / Max                   21.5 / 21.5                       NA / NA                       NA / NA                    21.4 / 21.4                     21.4 / 33.9                         NA / NA                         NA / NA                      22.8 / 22.8                      NA / NA                      NA / NA                  10.4 / 19.2                       NA / NA                    26.0 / 26.0                    19.7 / 19.7                    15.0 / 15.8                     21.0 / 21.0             22.8 / 24.4             17.8 / 19.2             14.3 / 16.4              18.7 / 18.7                   NA / NA                   NA / NA                   NA / NA                    NA / NA      10.4 / 33.9
      2  mpg       Miles/(US) gallon  Med [IQR]              21.5 [21.5;21.5]                    NA [NA;NA]                    NA [NA;NA]               21.4 [21.4;21.4]                30.4 [28.1;31.9]                      NA [NA;NA]                      NA [NA;NA]                 22.8 [22.8;22.8]                   NA [NA;NA]                   NA [NA;NA]             15.2 [13.3;15.5]                    NA [NA;NA]               26.0 [26.0;26.0]               19.7 [19.7;19.7]               15.4 [15.2;15.6]                21.0 [21.0;21.0]        23.6 [23.2;24.0]        18.1 [18.0;18.6]        15.3 [14.8;15.9]         18.7 [18.7;18.7]                NA [NA;NA]                NA [NA;NA]                NA [NA;NA]                 NA [NA;NA] 19.2 [15.4;22.8]
      3  mpg       Miles/(US) gallon Mean (std)                     21.5 (NA)                       NA (NA)                       NA (NA)                      21.4 (NA)                      29.3 (4.5)                         NA (NA)                         NA (NA)                        22.8 (NA)                      NA (NA)                      NA (NA)                   14.6 (2.9)                       NA (NA)                      26.0 (NA)                      19.7 (NA)                     15.4 (0.6)                        21.0 (0)              23.6 (1.1)              18.4 (0.7)              15.3 (1.5)                18.7 (NA)                   NA (NA)                   NA (NA)                   NA (NA)                    NA (NA)       20.1 (6.0)
      4  mpg       Miles/(US) gallon     N (NA)                         1 (0)                         0 (0)                         0 (0)                          1 (0)                           6 (0)                           0 (0)                           0 (0)                            1 (0)                        0 (0)                        0 (0)                        9 (0)                         0 (0)                          1 (0)                          1 (0)                          2 (0)                           2 (0)                   2 (0)                   3 (0)                   2 (0)                    1 (0)                     0 (0)                     0 (0)                     0 (0)                      0 (0)           32 (0)
      5 gear Number of forward gears          3                     1 (6.67%)                        0 (0%)                        0 (0%)                      1 (6.67%)                          0 (0%)                          0 (0%)                          0 (0%)                           0 (0%)                       0 (0%)                       0 (0%)                   9 (60.00%)                        0 (0%)                         0 (0%)                         0 (0%)                         0 (0%)                          0 (0%)                  0 (0%)               1 (6.67%)              2 (13.33%)                1 (6.67%)                    0 (0%)                    0 (0%)                    0 (0%)                     0 (0%)      15 (46.88%)
      6 gear Number of forward gears          4                        0 (0%)                        0 (0%)                        0 (0%)                         0 (0%)                      5 (41.67%)                          0 (0%)                          0 (0%)                        1 (8.33%)                       0 (0%)                       0 (0%)                       0 (0%)                        0 (0%)                         0 (0%)                         0 (0%)                         0 (0%)                      2 (16.67%)              2 (16.67%)              2 (16.67%)                  0 (0%)                   0 (0%)                    0 (0%)                    0 (0%)                    0 (0%)                     0 (0%)      12 (37.50%)
      7 gear Number of forward gears          5                        0 (0%)                        0 (0%)                        0 (0%)                         0 (0%)                      1 (20.00%)                          0 (0%)                          0 (0%)                           0 (0%)                       0 (0%)                       0 (0%)                       0 (0%)                        0 (0%)                     1 (20.00%)                     1 (20.00%)                     2 (40.00%)                          0 (0%)                  0 (0%)                  0 (0%)                  0 (0%)                   0 (0%)                    0 (0%)                    0 (0%)                    0 (0%)                     0 (0%)       5 (15.62%)
      8 gear Number of forward gears      Total                     1 (3.12%)                        0 (0%)                        0 (0%)                      1 (3.12%)                      6 (18.75%)                          0 (0%)                          0 (0%)                        1 (3.12%)                       0 (0%)                       0 (0%)                   9 (28.12%)                        0 (0%)                      1 (3.12%)                      1 (3.12%)                      2 (6.25%)                       2 (6.25%)               2 (6.25%)               3 (9.38%)               2 (6.25%)                1 (3.12%)                    0 (0%)                    0 (0%)                    0 (0%)                     0 (0%)     32 (100.00%)
    Code
      x3 = crosstable(mtcars3, c(mpg, vs, cyl), by = c(am, dummy))
      as.data.frame(x3)
    Output
         .id               label   variable am=auto & dummy=dummy am=manual & dummy=dummy
      1  mpg   Miles/(US) gallon  Min / Max           10.4 / 24.4             15.0 / 33.9
      2  mpg   Miles/(US) gallon  Med [IQR]      17.3 [14.9;19.2]        22.8 [21.0;30.4]
      3  mpg   Miles/(US) gallon Mean (std)            17.1 (3.8)              24.4 (6.2)
      4  mpg   Miles/(US) gallon     N (NA)                19 (0)                  13 (0)
      5   vs              Engine   straight            2 (22.22%)              7 (77.78%)
      6   vs              Engine    vshaped            9 (60.00%)              6 (40.00%)
      7   vs              Engine         NA                     8                       0
      8  cyl Number of cylinders          4            3 (30.00%)              7 (70.00%)
      9  cyl Number of cylinders          6            3 (75.00%)              1 (25.00%)
      10 cyl Number of cylinders          8           11 (84.62%)              2 (15.38%)
      11 cyl Number of cylinders         NA                     2                       3
    Code
      x4 = crosstable(mtcars3, c(mpg, vs, cyl, dummy, surv, hp_date, qsec_posix, diff, cyl3), by = c(am, gear), total = TRUE, times = c(100, 200), followup = TRUE)
      as.data.frame(x4)
    Output
                .id                              label                     variable                                              am=auto & gear=3 am=manual & gear=3                                              am=auto & gear=4                                            am=manual & gear=4 am=auto & gear=5                                            am=manual & gear=5                                                         Total
      1         mpg                  Miles/(US) gallon                    Min / Max                                                   10.4 / 21.5            NA / NA                                                   17.8 / 24.4                                                   21.0 / 33.9          NA / NA                                                   15.0 / 30.4                                                   10.4 / 33.9
      2         mpg                  Miles/(US) gallon                    Med [IQR]                                              15.5 [14.5;18.4]         NA [NA;NA]                                              21.0 [18.8;23.2]                                              25.1 [21.3;30.9]       NA [NA;NA]                                              19.7 [15.8;26.0]                                              19.2 [15.4;22.8]
      3         mpg                  Miles/(US) gallon                   Mean (std)                                                    16.1 (3.4)            NA (NA)                                                    21.1 (3.1)                                                    26.3 (5.4)          NA (NA)                                                    21.4 (6.7)                                                    20.1 (6.0)
      4         mpg                  Miles/(US) gallon                       N (NA)                                                        15 (0)              0 (0)                                                         4 (0)                                                         8 (0)            0 (0)                                                         5 (0)                                                        32 (0)
      5          vs                             Engine                     straight                                                    2 (22.22%)             0 (0%)                                                        0 (0%)                                                    6 (66.67%)           0 (0%)                                                    1 (11.11%)                                                    9 (37.50%)
      6          vs                             Engine                      vshaped                                                    9 (60.00%)             0 (0%)                                                        0 (0%)                                                    2 (13.33%)           0 (0%)                                                    4 (26.67%)                                                   15 (62.50%)
      7          vs                             Engine                           NA                                                             4                  0                                                             4                                                             0                0                                                             0                                                             8
      8          vs                             Engine                        Total                                                   15 (46.88%)             0 (0%)                                                    4 (12.50%)                                                    8 (25.00%)           0 (0%)                                                    5 (15.62%)                                                  32 (100.00%)
      9         cyl                Number of cylinders                            4                                                    1 (10.00%)             0 (0%)                                                    2 (20.00%)                                                    5 (50.00%)           0 (0%)                                                    2 (20.00%)                                                   10 (37.04%)
      10        cyl                Number of cylinders                            6                                                    1 (25.00%)             0 (0%)                                                    2 (50.00%)                                                        0 (0%)           0 (0%)                                                    1 (25.00%)                                                    4 (14.81%)
      11        cyl                Number of cylinders                            8                                                   11 (84.62%)             0 (0%)                                                        0 (0%)                                                        0 (0%)           0 (0%)                                                    2 (15.38%)                                                   13 (48.15%)
      12        cyl                Number of cylinders                           NA                                                             2                  0                                                             0                                                             3                0                                                             0                                                             5
      13        cyl                Number of cylinders                        Total                                                   15 (46.88%)             0 (0%)                                                    4 (12.50%)                                                    8 (25.00%)           0 (0%)                                                    5 (15.62%)                                                  32 (100.00%)
      14      dummy                              dummy                        dummy                                                   15 (46.88%)             0 (0%)                                                    4 (12.50%)                                                    8 (25.00%)           0 (0%)                                                    5 (15.62%)                                                  32 (100.00%)
      15      dummy                              dummy                        Total                                                   15 (46.88%)             0 (0%)                                                    4 (12.50%)                                                    8 (25.00%)           0 (0%)                                                    5 (15.62%)                                                  32 (100.00%)
      16       surv           Dummy survival (disp/am)                        t=100                                                   1.00 (0/15)               <NA>                                                    1.00 (0/4)                                                    0.50 (4/4)             <NA>                                                    0.80 (1/4)                                                   0.84 (5/27)
      17       surv           Dummy survival (disp/am)                        t=200                                                   1.00 (0/14)               <NA>                                                    1.00 (0/0)                                                       0 (4/0)             <NA>                                                    0.40 (2/2)                                                   0.64 (6/16)
      18       surv           Dummy survival (disp/am) Median follow up [min ; max]                                             318 [120.1 ; 472]               <NA>                                        157.15 [140.8 ; 167.6]                                                NA [Inf ; 160]             <NA>                                                NA [Inf ; 351]                                             304 [120.1 ; 472]
      19       surv           Dummy survival (disp/am)              Median survival                                                          <NA>               <NA>                                                          <NA>                                                          93.5             <NA>                                                           145                                                          <NA>
      20    hp_date                 Some nonsense date                    Min / Max                                       2010-04-08 - 2010-09-03            NA / NA                                       2010-03-04 - 2010-05-04                                       2010-02-22 - 2010-04-21          NA / NA                                       2010-04-02 - 2010-12-02                                       2010-02-22 - 2010-12-02
      21    hp_date                 Some nonsense date                    Med [IQR]                            2010-06-30 [2010-05-31;2010-08-04]         NA [NA;NA]                            2010-04-20 [2010-03-04;2010-05-04]                            2010-03-21 [2010-03-07;2010-04-20]       NA [NA;NA]                            2010-06-25 [2010-04-24;2010-09-22]                            2010-05-04 [2010-04-06;2010-06-30]
      22    hp_date                 Some nonsense date                   Mean (std)                                       2010-06-26 (1.6 months)            NA (NA)                                        2010-04-11 (29.0 days)                                        2010-03-25 (24.2 days)          NA (NA)                                       2010-07-15 (3.4 months)                                       2010-05-27 (2.3 months)
      23    hp_date                 Some nonsense date                       N (NA)                                                        15 (0)              0 (0)                                                         4 (0)                                                         8 (0)            0 (0)                                                         5 (0)                                                        32 (0)
      24 qsec_posix                          Date+time                    Min / Max                     2010-01-16 10:50:24 - 2010-01-21 06:16:48            NA / NA                     2010-01-19 08:12:00 - 2010-01-23 22:36:00                     2010-01-17 12:02:24 - 2010-01-20 22:36:00          NA / NA                     2010-01-15 13:00:00 - 2010-01-17 22:36:00                     2010-01-15 13:00:00 - 2010-01-23 22:36:00
      25 qsec_posix                          Date+time                    Med [IQR] 2010-01-18 11:04:48 [2010-01-18 01:28:48;2010-01-19 01:00:00]         NA [NA;NA] 2010-01-20 11:48:00 [2010-01-19 08:12:00;2010-01-21 01:00:00] 2010-01-19 15:31:12 [2010-01-18 01:28:48;2010-01-19 22:36:00]       NA [NA;NA] 2010-01-16 13:00:00 [2010-01-15 15:24:00;2010-01-17 17:48:00] 2010-01-18 18:02:24 [2010-01-17 21:52:48;2010-01-19 22:36:00]
      26 qsec_posix                          Date+time                   Mean (std)                                2010-01-18 17:36:28 (1.3 days)            NA (NA)                                2010-01-21 01:36:00 (2.0 days)                                2010-01-19 11:26:24 (1.2 days)          NA (NA)                                2010-01-16 16:21:36 (1.1 days)                                2010-01-18 21:22:12 (1.8 days)
      27 qsec_posix                          Date+time                       N (NA)                                                        15 (0)              0 (0)                                                         4 (0)                                                         8 (0)            0 (0)                                                         5 (0)                                                        32 (0)
      28       diff Difftime hp_date-qsec_posix (days)                    Min / Max                                                  77.0 / 229.6            NA / NA                                                  42.0 / 104.7                                                   33.5 / 93.5          NA / NA                                                  74.3 / 320.4                                                  33S / 5M 20S
      29       diff Difftime hp_date-qsec_posix (days)                    Med [IQR]                                           162.0 [132.9;192.1]         NA [NA;NA]                                             88.1 [64.6;104.2]                                              60.7 [46.2;91.0]       NA [NA;NA]                                            159.5 [96.1;249.5]                                        1M 44S [1M 16S;2M 42S]
      30       diff Difftime hp_date-qsec_posix (days)                   Mean (std)                                                  158.4 (48.8)            NA (NA)                                                   80.7 (30.0)                                                   65.4 (25.0)          NA (NA)                                                 180.0 (103.9)                                                2M 9S (1M 10S)
      31       diff Difftime hp_date-qsec_posix (days)                       N (NA)                                                        15 (0)              0 (0)                                                         4 (0)                                                         8 (0)            0 (0)                                                         5 (0)                                                        32 (0)
      32       cyl3                               cyl3                        FALSE                                                   13 (48.15%)             0 (0%)                                                    4 (14.81%)                                                    5 (18.52%)           0 (0%)                                                    5 (18.52%)                                                  27 (100.00%)
      33       cyl3                               cyl3                           NA                                                             2                  0                                                             0                                                             3                0                                                             0                                                             5
      34       cyl3                               cyl3                        Total                                                   15 (46.88%)             0 (0%)                                                    4 (12.50%)                                                    8 (25.00%)           0 (0%)                                                    5 (15.62%)                                                  32 (100.00%)

# By multiple (formula)

    Code
      x = crosstable(mtcars3, mpg + gear ~ I(am == "auto") + vs, total = TRUE)
      as.data.frame(x)
    Output
         .id                   label   variable I(am == "auto")=FALSE & vs=straight I(am == "auto")=TRUE & vs=straight I(am == "auto")=FALSE & vs=vshaped I(am == "auto")=TRUE & vs=vshaped I(am == "auto")=FALSE & vs=NA I(am == "auto")=TRUE & vs=NA            Total
      1  mpg       Miles/(US) gallon  Min / Max                         21.4 / 33.9                        21.4 / 21.5                        15.0 / 26.0                       10.4 / 19.2                       NA / NA                  14.3 / 24.4      10.4 / 33.9
      2  mpg       Miles/(US) gallon  Med [IQR]                    30.4 [25.1;31.4]                   21.4 [21.4;21.5]                   20.4 [16.8;21.0]                  15.2 [13.3;15.5]                    NA [NA;NA]             18.4 [17.5;20.1] 19.2 [15.4;22.8]
      3  mpg       Miles/(US) gallon Mean (std)                          28.4 (4.8)                         21.4 (0.1)                         19.8 (4.0)                        14.6 (2.9)                       NA (NA)                   19.0 (3.3)       20.1 (6.0)
      4  mpg       Miles/(US) gallon     N (NA)                               7 (0)                              2 (0)                              6 (0)                             9 (0)                         0 (0)                        8 (0)           32 (0)
      5 gear Number of forward gears          3                              0 (0%)                         2 (13.33%)                             0 (0%)                        9 (60.00%)                        0 (0%)                   4 (26.67%)      15 (46.88%)
      6 gear Number of forward gears          4                          6 (50.00%)                             0 (0%)                         2 (16.67%)                            0 (0%)                        0 (0%)                   4 (33.33%)      12 (37.50%)
      7 gear Number of forward gears          5                          1 (20.00%)                             0 (0%)                         4 (80.00%)                            0 (0%)                        0 (0%)                       0 (0%)       5 (15.62%)
      8 gear Number of forward gears      Total                          7 (21.88%)                          2 (6.25%)                         6 (18.75%)                        9 (28.12%)                        0 (0%)                   8 (25.00%)     32 (100.00%)

# get_percent_pattern()

    Code
      get_percent_pattern()
    Output
      $body
      {n} ({p_row})
      
      $total_row
      [1] "{n} ({p_col})"
      
      $total_col
      [1] "{n} ({p_row})"
      
      $total_all
      [1] "{n} ({p_tot})"
      
    Code
      get_percent_pattern(na = TRUE)
    Output
      $body
      [1] "{n} ({p_row_na})"
      
      $total_row
      [1] "{n} ({p_col_na})"
      
      $total_col
      [1] "{n} ({p_row_na})"
      
      $total_all
      [1] "{n} ({p_tot_na})"
      
    Code
      get_percent_pattern(c("cells", "row", "column"))
    Output
      $body
      {n} ({p_tot} / {p_col} / {p_row})
      
      $total_row
      [1] "{n} ({p_col})"
      
      $total_col
      [1] "{n} ({p_row})"
      
      $total_all
      [1] "{n} ({p_tot})"
      
    Code
      get_percent_pattern(c("cells", "row", "column"), na = TRUE)
    Output
      $body
      [1] "{n} ({p_tot_na} / {p_col_na} / {p_row_na})"
      
      $total_row
      [1] "{n} ({p_col_na})"
      
      $total_col
      [1] "{n} ({p_row_na})"
      
      $total_all
      [1] "{n} ({p_tot_na})"
      
    Code
      get_percent_pattern(margin = TRUE)
    Output
      $body
      [1] "{n} ({p_row} / {p_col})"
      
      $total_row
      [1] "{n} ({p_col})"
      
      $total_col
      [1] "{n} ({p_row})"
      
      $total_all
      [1] "{n} ({p_tot})"
      
    Code
      get_percent_pattern(margin = 1)
    Output
      $body
      {n} ({p_row})
      
      $total_row
      [1] "{n} ({p_col})"
      
      $total_col
      [1] "{n} ({p_row})"
      
      $total_all
      [1] "{n} ({p_tot})"
      
    Code
      get_percent_pattern(margin = c(1, 0, 2))
    Output
      $body
      {n} ({p_tot} / {p_row} / {p_col})
      
      $total_row
      [1] "{n} ({p_col})"
      
      $total_col
      [1] "{n} ({p_row})"
      
      $total_all
      [1] "{n} ({p_tot})"
      
    Code
      get_percent_pattern(margin = 1:2)
    Output
      $body
      {n} ({p_row} / {p_col})
      
      $total_row
      [1] "{n} ({p_col})"
      
      $total_col
      [1] "{n} ({p_row})"
      
      $total_all
      [1] "{n} ({p_tot})"
      
    Code
      get_percent_pattern(margin = 2:1)
    Output
      $body
      {n} ({p_row} / {p_col})
      
      $total_row
      [1] "{n} ({p_col})"
      
      $total_col
      [1] "{n} ({p_row})"
      
      $total_all
      [1] "{n} ({p_tot})"
      
    Code
      get_percent_pattern(margin = "row")
    Output
      $body
      {n} ({p_row})
      
      $total_row
      [1] "{n} ({p_col})"
      
      $total_col
      [1] "{n} ({p_row})"
      
      $total_all
      [1] "{n} ({p_tot})"
      
    Code
      get_percent_pattern(margin = c("row", "cells", "column"))
    Output
      $body
      {n} ({p_tot} / {p_col} / {p_row})
      
      $total_row
      [1] "{n} ({p_col})"
      
      $total_col
      [1] "{n} ({p_row})"
      
      $total_all
      [1] "{n} ({p_tot})"
      

