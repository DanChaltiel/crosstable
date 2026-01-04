# All options work

    Code
      as.data.frame(ct_noopt)
    Output
                .id                    label        variable                                                      straight                                                       vshaped                                                            NA                                                                                                              effect                                                   test
      1         cyl      Number of cylinders               4                                                    7 (87.50%)                                                    1 (12.50%)                                                             2 Odds ratio [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0 to NA]\n8 vs 4: 5.98e+09 [2.27e-200 to NA] p value: 0.0001 \n(Fisher's Exact Test for Count Data)
      2         cyl      Number of cylinders               6                                                        0 (0%)                                                   1 (100.00%)                                                             3 Odds ratio [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0 to NA]\n8 vs 4: 5.98e+09 [2.27e-200 to NA] p value: 0.0001 \n(Fisher's Exact Test for Count Data)
      3         cyl      Number of cylinders               8                                                        0 (0%)                                                  11 (100.00%)                                                             2 Odds ratio [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0 to NA]\n8 vs 4: 5.98e+09 [2.27e-200 to NA] p value: 0.0001 \n(Fisher's Exact Test for Count Data)
      4         cyl      Number of cylinders              NA                                                             2                                                             2                                                             1 Odds ratio [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0 to NA]\n8 vs 4: 5.98e+09 [2.27e-200 to NA] p value: 0.0001 \n(Fisher's Exact Test for Count Data)
      5        carb    Number of carburetors       Min / Max                                                     1.0 / 2.0                                                     2.0 / 8.0                                                     1.0 / 4.0                     Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2.40 [1.54 to 3.26]             p value: 0.0001 \n(Wilcoxon rank sum test)
      6        carb    Number of carburetors       Med [IQR]                                                 1.0 [1.0;2.0]                                                 4.0 [2.5;4.0]                                                 2.5 [2.0;4.0]                     Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2.40 [1.54 to 3.26]             p value: 0.0001 \n(Wilcoxon rank sum test)
      7        carb    Number of carburetors      Mean (std)                                                     1.3 (0.5)                                                     3.7 (1.6)                                                     2.8 (1.2)                     Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2.40 [1.54 to 3.26]             p value: 0.0001 \n(Wilcoxon rank sum test)
      8        carb    Number of carburetors          N (NA)                                                         9 (0)                                                        15 (0)                                                         8 (0)                     Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2.40 [1.54 to 3.26]             p value: 0.0001 \n(Wilcoxon rank sum test)
      9  qsec_posix                Date+time       Min / Max                     2010-01-17 22:36:00 - 2010-01-21 01:14:24                     2010-01-15 13:00:00 - 2010-01-19 01:00:00                     2010-01-16 21:09:36 - 2010-01-23 22:36:00         Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -1.94e+05 [-2.76e+05 to -1.12e+05]                  p value: 0.0001 \n(Two Sample t-test)
      10 qsec_posix                Date+time       Med [IQR] 2010-01-19 22:36:00 [2010-01-19 15:24:00;2010-01-20 12:16:48] 2010-01-18 01:28:48 [2010-01-16 13:00:00;2010-01-18 15:24:00] 2010-01-19 15:24:00 [2010-01-18 01:28:48;2010-01-21 01:00:00]         Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -1.94e+05 [-2.76e+05 to -1.12e+05]                  p value: 0.0001 \n(Two Sample t-test)
      11 qsec_posix                Date+time      Mean (std)                              2010-01-19 23:16:00 (22.7 hours)                                2010-01-17 17:22:04 (1.2 days)                                2010-01-19 20:44:24 (2.2 days)         Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -1.94e+05 [-2.76e+05 to -1.12e+05]                  p value: 0.0001 \n(Two Sample t-test)
      12 qsec_posix                Date+time          N (NA)                                                         9 (0)                                                        15 (0)                                                         8 (0)         Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -1.94e+05 [-2.76e+05 to -1.12e+05]                  p value: 0.0001 \n(Two Sample t-test)
      13       surv Dummy survival (disp/am)          t=71.1                                                    0.89 (1/9)                                                   1.00 (0/15)                                                    1.00 (0/8)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      14       surv Dummy survival (disp/am)          t=75.7                                                    0.78 (1/8)                                                   1.00 (0/15)                                                    1.00 (0/8)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      15       surv Dummy survival (disp/am)          t=78.7                                                    0.67 (1/7)                                                   1.00 (0/15)                                                    1.00 (0/8)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      16       surv Dummy survival (disp/am)            t=79                                                    0.56 (1/6)                                                   1.00 (0/15)                                                    1.00 (0/8)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      17       surv Dummy survival (disp/am)          t=95.1                                                    0.44 (1/5)                                                   1.00 (0/15)                                                    1.00 (0/8)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      18       surv Dummy survival (disp/am)           t=108                                                    0.33 (1/4)                                                   1.00 (0/15)                                                    1.00 (0/8)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      19       surv Dummy survival (disp/am)         t=120.1                                                    0.33 (0/3)                                                   1.00 (0/15)                                                    1.00 (0/8)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      20       surv Dummy survival (disp/am)         t=120.3                                                    0.33 (0/2)                                                   0.93 (1/15)                                                    1.00 (0/8)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      21       surv Dummy survival (disp/am)           t=121                                                    0.17 (1/2)                                                   0.93 (0/14)                                                    1.00 (0/8)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      22       surv Dummy survival (disp/am)         t=140.8                                                    0.17 (0/1)                                                   0.93 (0/14)                                                    1.00 (0/8)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      23       surv Dummy survival (disp/am)           t=145                                                    0.17 (0/1)                                                   0.87 (1/14)                                                    1.00 (0/7)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      24       surv Dummy survival (disp/am)         t=146.7                                                    0.17 (0/1)                                                   0.87 (0/13)                                                    1.00 (0/7)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      25       surv Dummy survival (disp/am)           t=160                                                    0.17 (0/1)                                                   0.73 (2/13)                                                    1.00 (0/6)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      26       surv Dummy survival (disp/am)         t=167.6                                                    0.17 (0/1)                                                   0.73 (0/11)                                                    1.00 (0/6)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      27       surv Dummy survival (disp/am)           t=225                                                    0.17 (0/1)                                                   0.73 (0/11)                                                    1.00 (0/4)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      28       surv Dummy survival (disp/am)           t=258                                                    0.17 (0/1)                                                   0.73 (0/11)                                                    1.00 (0/3)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      29       surv Dummy survival (disp/am)         t=275.8                                                    0.17 (0/0)                                                   0.73 (0/11)                                                    1.00 (0/3)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      30       surv Dummy survival (disp/am)           t=301                                                    0.17 (0/0)                                                    0.65 (1/9)                                                    1.00 (0/2)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      31       surv Dummy survival (disp/am)           t=304                                                    0.17 (0/0)                                                    0.65 (0/8)                                                    1.00 (0/2)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      32       surv Dummy survival (disp/am)           t=318                                                    0.17 (0/0)                                                    0.65 (0/7)                                                    1.00 (0/2)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      33       surv Dummy survival (disp/am)           t=350                                                    0.17 (0/0)                                                    0.65 (0/6)                                                    1.00 (0/2)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      34       surv Dummy survival (disp/am)           t=351                                                    0.17 (0/0)                                                    0.52 (1/5)                                                    1.00 (0/2)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      35       surv Dummy survival (disp/am)           t=360                                                    0.17 (0/0)                                                    0.52 (0/4)                                                    1.00 (0/2)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      36       surv Dummy survival (disp/am)           t=400                                                    0.17 (0/0)                                                    0.52 (0/4)                                                    1.00 (0/0)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      37       surv Dummy survival (disp/am)           t=440                                                    0.17 (0/0)                                                    0.52 (0/3)                                                    1.00 (0/0)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      38       surv Dummy survival (disp/am)           t=460                                                    0.17 (0/0)                                                    0.52 (0/2)                                                    1.00 (0/0)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      39       surv Dummy survival (disp/am)           t=472                                                    0.17 (0/0)                                                    0.52 (0/1)                                                    1.00 (0/0)                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
      40       surv Dummy survival (disp/am) Median survival                                                          95.1                                                          <NA>                                                          <NA>                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: 0.0002 \n(Logrank test)
    Code
      as.data.frame(ct_opt)
    Output
                .id      label                     variable                straight               vshaped                    NA                 Total                                                                                                                           effect                                                 test
      1         cyl        cyl                            4         7 (87.5%, 100%)       1 (12.5%, 7.7%)                     2              10 (37%) Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0.00e+00 to NA]\n8 vs 4: 5.98e+09 [1.27e-79 to 2.72e+247] p value: <0.1 \n(Fisher's Exact Test for Count Data)
      2         cyl        cyl                            6                       0        1 (100%, 7.7%)                     3             4 (14.8%) Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0.00e+00 to NA]\n8 vs 4: 5.98e+09 [1.27e-79 to 2.72e+247] p value: <0.1 \n(Fisher's Exact Test for Count Data)
      3         cyl        cyl                            8                       0      11 (100%, 84.6%)                     2            13 (48.1%) Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0.00e+00 to NA]\n8 vs 4: 5.98e+09 [1.27e-79 to 2.72e+247] p value: <0.1 \n(Fisher's Exact Test for Count Data)
      4         cyl        cyl                           NA                       2                     2                     1                     5 Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0.00e+00 to NA]\n8 vs 4: 5.98e+09 [1.27e-79 to 2.72e+247] p value: <0.1 \n(Fisher's Exact Test for Count Data)
      5         cyl        cyl                        Total               9 (37.5%)            15 (62.5%)                     8             32 (100%) Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0.00e+00 to NA]\n8 vs 4: 5.98e+09 [1.27e-79 to 2.72e+247] p value: <0.1 \n(Fisher's Exact Test for Count Data)
      6        carb       carb                    Mean (SD)               1.3 (0.5)             3.7 (1.6)             2.8 (1.2)             2.8 (1.6)                                       Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2.4 [2 to 2.8]             p value: <0.1 \n(Wilcoxon rank sum test)
      7  qsec_posix qsec_posix                    Mean (SD) 19/01/2010 (22.7 hours) 17/01/2010 (1.2 days) 19/01/2010 (2.2 days) 18/01/2010 (1.8 days)                         Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -1.9e+05 [-2.8e+05 to -1.1e+05]                  p value: <0.1 \n(Two Sample t-test)
      8        surv       surv                          t=0                 1 (0/9)              1 (0/15)               1 (0/8)              1 (0/32)                                                                 Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: <0.1 \n(Logrank test)
      9        surv       surv                        t=100               0.4 (5/4)              1 (0/15)               1 (0/8)            0.8 (5/27)                                                                 Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: <0.1 \n(Logrank test)
      10       surv       surv Median follow up [min ; max]       258 [120.1 ; 258]     400 [275.8 ; 472]   196.3 [140.8 ; 360]     304 [120.1 ; 472]                                                                 Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: <0.1 \n(Logrank test)
      11       surv       surv              Median survival                    95.1                  <NA>                  <NA>                  <NA>                                                                 Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]                       p value: <0.1 \n(Logrank test)

---

    Code
      ft_noopt
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA`, `Total`, `effect`, `test` 
      header has 2 row(s) 
      body has 11 row(s) 
      original dataset sample: 
      'data.frame':	11 obs. of  9 variables:
       $ .id     : chr  "cyl" "cyl" "cyl" "cyl" ...
       $ label   : chr  "cyl" "cyl" "cyl" "cyl" ...
       $ variable: chr  "4" "6" "8" "NA" ...
       $ straight: chr  "7 (87.5%, 100%)" "0" "0" "2" ...
       $ vshaped : chr  "1 (12.5%, 7.7%)" "1 (100%, 7.7%)" "11 (100%, 84.6%)" "2" ...
       $ NA      : chr  "2" "3" "2" "1" ...
       $ Total   : chr  "10 (37%)" "4 (14.8%)" "13 (48.1%)" "5" ...
       $ effect  : 'glue' chr  "Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0.00e+00 to NA]\n8 vs 4: 5.98e+09 [1.27e"| __truncated__ "Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0.00e+00 to NA]\n8 vs 4: 5.98e+09 [1.27e"| __truncated__ "Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0.00e+00 to NA]\n8 vs 4: 5.98e+09 [1.27e"| __truncated__ "Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0.00e+00 to NA]\n8 vs 4: 5.98e+09 [1.27e"| __truncated__ ...
       $ test    : chr  "p value: <0.1 \n(Fisher's Exact Test for Count Data)" "p value: <0.1 \n(Fisher's Exact Test for Count Data)" "p value: <0.1 \n(Fisher's Exact Test for Count Data)" "p value: <0.1 \n(Fisher's Exact Test for Count Data)" ...
       - attr(*, "debug")=List of 3
        ..$ interface: chr "quosure"
        ..$ x_class  : Named chr [1:4] "factor" "numeric" "POSIXct, POSIXt" "Surv"
        .. ..- attr(*, "names")= chr [1:4] "cyl" "carb" "qsec_posix" "surv"
        ..$ y_class  : Named chr "character"
        .. ..- attr(*, "names")= chr "vs"
       - attr(*, "N")= int 32
       - attr(*, "showNA")= chr "always"
       - attr(*, "variables")= chr [1:4] "cyl" "carb" "qsec_posix" "surv"
       - attr(*, "has_test")= logi TRUE
       - attr(*, "has_effect")= logi TRUE
       - attr(*, "has_total")= int [1:2] 1 2
       - attr(*, "has_label")= logi FALSE
       - attr(*, "by")= chr "vs"
       - attr(*, "by_label")= Named chr "Engine"
        ..- attr(*, "names")= chr "vs"
       - attr(*, "by_table")= 'table' int [1:2(1d)] 9 15
        ..- attr(*, "dimnames")=List of 1
        .. ..$ vs: chr [1:2] "straight" "vshaped"
       - attr(*, "by_levels")=List of 1
        ..$ vs: chr [1:3] "straight" "vshaped" NA
    Code
      ft_opt
    Output
      a flextable object.
      col_keys: `variable`, `straight`, `vshaped`, `NA`, `Total`, `effect`, `test` 
      header has 2 row(s) 
      body has 15 row(s) 
      original dataset sample: 
      'data.frame':	15 obs. of  8 variables:
       $ .id     : chr  "cyl" "cyl" "cyl" "cyl" ...
       $ variable: 'glue' chr  "cyl (cyl)" "4" "6" "8" ...
       $ straight: chr  NA "7 (87.5%, 100%)" "0" "0" ...
       $ vshaped : chr  NA "1 (12.5%, 7.7%)" "1 (100%, 7.7%)" "11 (100%, 84.6%)" ...
       $ NA      : chr  NA "2" "3" "2" ...
       $ Total   : chr  NA "10 (37%)" "4 (14.8%)" "13 (48.1%)" ...
       $ effect  : 'glue' chr  NA "Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0.00e+00 to NA]\n8 vs 4: 5.98e+09 [1.27e"| __truncated__ "Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0.00e+00 to NA]\n8 vs 4: 5.98e+09 [1.27e"| __truncated__ "Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0.00e+00 to NA]\n8 vs 4: 5.98e+09 [1.27e"| __truncated__ ...
       $ test    : chr  NA "p value: <0.1 " "p value: <0.1 " "p value: <0.1 " ...
       - attr(*, "debug")=List of 3
        ..$ interface: chr "quosure"
        ..$ x_class  : Named chr [1:4] "factor" "numeric" "POSIXct, POSIXt" "Surv"
        .. ..- attr(*, "names")= chr [1:4] "cyl" "carb" "qsec_posix" "surv"
        ..$ y_class  : Named chr "character"
        .. ..- attr(*, "names")= chr "vs"
       - attr(*, "N")= int 32
       - attr(*, "showNA")= chr "always"
       - attr(*, "variables")= chr [1:4] "cyl" "carb" "qsec_posix" "surv"
       - attr(*, "has_test")= logi TRUE
       - attr(*, "has_effect")= logi TRUE
       - attr(*, "has_total")= int [1:2] 1 2
       - attr(*, "has_label")= logi FALSE
       - attr(*, "by")= chr "vs"
       - attr(*, "by_label")= Named chr "Engine"
        ..- attr(*, "names")= chr "vs"
       - attr(*, "by_table")= 'table' int [1:2(1d)] 9 15
        ..- attr(*, "dimnames")=List of 1
        .. ..$ vs: chr [1:2] "straight" "vshaped"
       - attr(*, "by_levels")=List of 1
        ..$ vs: chr [1:3] "straight" "vshaped" NA
       - attr(*, "title_rows")= logi [1:11] TRUE FALSE FALSE FALSE FALSE TRUE ...

