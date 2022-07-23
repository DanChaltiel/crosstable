# All options work

    Code
      as.data.frame(ct_noopt)
    Output
                .id                    label        variable
      1         cyl      Number of cylinders               4
      2         cyl      Number of cylinders               6
      3         cyl      Number of cylinders               8
      4         cyl      Number of cylinders              NA
      5        carb    Number of carburetors       Min / Max
      6        carb    Number of carburetors       Med [IQR]
      7        carb    Number of carburetors      Mean (std)
      8        carb    Number of carburetors          N (NA)
      9  qsec_posix                Date+time       Min / Max
      10 qsec_posix                Date+time       Med [IQR]
      11 qsec_posix                Date+time      Mean (std)
      12 qsec_posix                Date+time          N (NA)
      13       surv Dummy survival (disp/am)          t=71.1
      14       surv Dummy survival (disp/am)          t=75.7
      15       surv Dummy survival (disp/am)          t=78.7
      16       surv Dummy survival (disp/am)            t=79
      17       surv Dummy survival (disp/am)          t=95.1
      18       surv Dummy survival (disp/am)           t=108
      19       surv Dummy survival (disp/am)         t=120.1
      20       surv Dummy survival (disp/am)         t=120.3
      21       surv Dummy survival (disp/am)           t=121
      22       surv Dummy survival (disp/am)         t=140.8
      23       surv Dummy survival (disp/am)           t=145
      24       surv Dummy survival (disp/am)         t=146.7
      25       surv Dummy survival (disp/am)           t=160
      26       surv Dummy survival (disp/am)         t=167.6
      27       surv Dummy survival (disp/am)           t=225
      28       surv Dummy survival (disp/am)           t=258
      29       surv Dummy survival (disp/am)         t=275.8
      30       surv Dummy survival (disp/am)         t=275.8
      31       surv Dummy survival (disp/am)           t=301
      32       surv Dummy survival (disp/am)           t=304
      33       surv Dummy survival (disp/am)           t=318
      34       surv Dummy survival (disp/am)           t=350
      35       surv Dummy survival (disp/am)           t=351
      36       surv Dummy survival (disp/am)           t=360
      37       surv Dummy survival (disp/am)           t=400
      38       surv Dummy survival (disp/am)           t=440
      39       surv Dummy survival (disp/am)           t=460
      40       surv Dummy survival (disp/am)           t=472
      41       surv Dummy survival (disp/am) Median survival
                                                              straight
      1                                                     7 (87.50%)
      2                                                         0 (0%)
      3                                                         0 (0%)
      4                                                              2
      5                                                      1.0 / 2.0
      6                                                  1.0 [1.0;2.0]
      7                                                      1.3 (0.5)
      8                                                          9 (0)
      9                      2010-01-17 22:36:00 - 2010-01-21 01:14:24
      10 2010-01-19 22:36:00 [2010-01-19 15:24:00;2010-01-20 12:16:48]
      11                              2010-01-19 23:16:00 (22.7 hours)
      12                                                         9 (0)
      13                                                    0.89 (1/9)
      14                                                    0.78 (1/8)
      15                                                    0.67 (1/7)
      16                                                    0.56 (1/6)
      17                                                    0.44 (1/5)
      18                                                    0.33 (1/4)
      19                                                    0.33 (0/3)
      20                                                    0.33 (0/2)
      21                                                    0.17 (1/2)
      22                                                    0.17 (0/1)
      23                                                    0.17 (0/1)
      24                                                    0.17 (0/1)
      25                                                    0.17 (0/1)
      26                                                    0.17 (0/1)
      27                                                    0.17 (0/1)
      28                                                    0.17 (0/1)
      29                                                    0.17 (0/0)
      30                                                    0.17 (0/0)
      31                                                    0.17 (0/0)
      32                                                    0.17 (0/0)
      33                                                    0.17 (0/0)
      34                                                    0.17 (0/0)
      35                                                    0.17 (0/0)
      36                                                    0.17 (0/0)
      37                                                    0.17 (0/0)
      38                                                    0.17 (0/0)
      39                                                    0.17 (0/0)
      40                                                    0.17 (0/0)
      41                                                          95.1
                                                               vshaped
      1                                                     1 (12.50%)
      2                                                    1 (100.00%)
      3                                                   11 (100.00%)
      4                                                              2
      5                                                      2.0 / 8.0
      6                                                  4.0 [2.5;4.0]
      7                                                      3.7 (1.6)
      8                                                         15 (0)
      9                      2010-01-15 13:00:00 - 2010-01-19 01:00:00
      10 2010-01-18 01:28:48 [2010-01-16 13:00:00;2010-01-18 15:24:00]
      11                                2010-01-17 17:22:04 (1.2 days)
      12                                                        15 (0)
      13                                                   1.00 (0/15)
      14                                                   1.00 (0/15)
      15                                                   1.00 (0/15)
      16                                                   1.00 (0/15)
      17                                                   1.00 (0/15)
      18                                                   1.00 (0/15)
      19                                                   1.00 (0/15)
      20                                                   0.93 (1/15)
      21                                                   0.93 (0/14)
      22                                                   0.93 (0/14)
      23                                                   0.87 (1/14)
      24                                                   0.87 (0/13)
      25                                                   0.73 (2/13)
      26                                                   0.73 (0/11)
      27                                                   0.73 (0/11)
      28                                                   0.73 (0/11)
      29                                                   0.73 (0/11)
      30                                                   0.73 (0/11)
      31                                                    0.65 (1/9)
      32                                                    0.65 (0/8)
      33                                                    0.65 (0/7)
      34                                                    0.65 (0/6)
      35                                                    0.52 (1/5)
      36                                                    0.52 (0/4)
      37                                                    0.52 (0/4)
      38                                                    0.52 (0/3)
      39                                                    0.52 (0/2)
      40                                                    0.52 (0/1)
      41                                                          <NA>
                                                                    NA
      1                                                              2
      2                                                              3
      3                                                              2
      4                                                              1
      5                                                      1.0 / 4.0
      6                                                  2.5 [2.0;4.0]
      7                                                      2.8 (1.2)
      8                                                          8 (0)
      9                      2010-01-16 21:09:36 - 2010-01-23 22:36:00
      10 2010-01-19 15:24:00 [2010-01-18 01:28:48;2010-01-21 01:00:00]
      11                                2010-01-19 20:44:24 (2.2 days)
      12                                                         8 (0)
      13                                                    1.00 (0/8)
      14                                                    1.00 (0/8)
      15                                                    1.00 (0/8)
      16                                                    1.00 (0/8)
      17                                                    1.00 (0/8)
      18                                                    1.00 (0/8)
      19                                                    1.00 (0/8)
      20                                                    1.00 (0/8)
      21                                                    1.00 (0/8)
      22                                                    1.00 (0/8)
      23                                                    1.00 (0/7)
      24                                                    1.00 (0/7)
      25                                                    1.00 (0/6)
      26                                                    1.00 (0/6)
      27                                                    1.00 (0/4)
      28                                                    1.00 (0/3)
      29                                                    1.00 (0/3)
      30                                                    1.00 (0/3)
      31                                                    1.00 (0/2)
      32                                                    1.00 (0/2)
      33                                                    1.00 (0/2)
      34                                                    1.00 (0/2)
      35                                                    1.00 (0/2)
      36                                                    1.00 (0/2)
      37                                                    1.00 (0/0)
      38                                                    1.00 (0/0)
      39                                                    1.00 (0/0)
      40                                                    1.00 (0/0)
      41                                                          <NA>
                                                                                                                                effect
      1  Odds ratio [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5981748110.20 [0.00 to  NA]\n8 vs 4: 5981747998.47 [0.00 to  NA]
      2  Odds ratio [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5981748110.20 [0.00 to  NA]\n8 vs 4: 5981747998.47 [0.00 to  NA]
      3  Odds ratio [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5981748110.20 [0.00 to  NA]\n8 vs 4: 5981747998.47 [0.00 to  NA]
      4  Odds ratio [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5981748110.20 [0.00 to  NA]\n8 vs 4: 5981747998.47 [0.00 to  NA]
      5                                Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2.40 [1.54 to 3.26]
      6                                Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2.40 [1.54 to 3.26]
      7                                Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2.40 [1.54 to 3.26]
      8                                Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2.40 [1.54 to 3.26]
      9                 Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -194035.20 [-276311.92 to -111758.48]
      10                Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -194035.20 [-276311.92 to -111758.48]
      11                Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -194035.20 [-276311.92 to -111758.48]
      12                Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -194035.20 [-276311.92 to -111758.48]
      13                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      14                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      15                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      16                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      17                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      18                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      19                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      20                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      21                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      22                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      23                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      24                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      25                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      26                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      27                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      28                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      29                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      30                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      31                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      32                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      33                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      34                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      35                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      36                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      37                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      38                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      39                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      40                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      41                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
                                                           test
      1  p value: 0.0001 \n(Fisher's Exact Test for Count Data)
      2  p value: 0.0001 \n(Fisher's Exact Test for Count Data)
      3  p value: 0.0001 \n(Fisher's Exact Test for Count Data)
      4  p value: 0.0001 \n(Fisher's Exact Test for Count Data)
      5              p value: 0.0001 \n(Wilcoxon rank sum test)
      6              p value: 0.0001 \n(Wilcoxon rank sum test)
      7              p value: 0.0001 \n(Wilcoxon rank sum test)
      8              p value: 0.0001 \n(Wilcoxon rank sum test)
      9                   p value: 0.0001 \n(Two Sample t-test)
      10                  p value: 0.0001 \n(Two Sample t-test)
      11                  p value: 0.0001 \n(Two Sample t-test)
      12                  p value: 0.0001 \n(Two Sample t-test)
      13                       p value: 0.0002 \n(Logrank test)
      14                       p value: 0.0002 \n(Logrank test)
      15                       p value: 0.0002 \n(Logrank test)
      16                       p value: 0.0002 \n(Logrank test)
      17                       p value: 0.0002 \n(Logrank test)
      18                       p value: 0.0002 \n(Logrank test)
      19                       p value: 0.0002 \n(Logrank test)
      20                       p value: 0.0002 \n(Logrank test)
      21                       p value: 0.0002 \n(Logrank test)
      22                       p value: 0.0002 \n(Logrank test)
      23                       p value: 0.0002 \n(Logrank test)
      24                       p value: 0.0002 \n(Logrank test)
      25                       p value: 0.0002 \n(Logrank test)
      26                       p value: 0.0002 \n(Logrank test)
      27                       p value: 0.0002 \n(Logrank test)
      28                       p value: 0.0002 \n(Logrank test)
      29                       p value: 0.0002 \n(Logrank test)
      30                       p value: 0.0002 \n(Logrank test)
      31                       p value: 0.0002 \n(Logrank test)
      32                       p value: 0.0002 \n(Logrank test)
      33                       p value: 0.0002 \n(Logrank test)
      34                       p value: 0.0002 \n(Logrank test)
      35                       p value: 0.0002 \n(Logrank test)
      36                       p value: 0.0002 \n(Logrank test)
      37                       p value: 0.0002 \n(Logrank test)
      38                       p value: 0.0002 \n(Logrank test)
      39                       p value: 0.0002 \n(Logrank test)
      40                       p value: 0.0002 \n(Logrank test)
      41                       p value: 0.0002 \n(Logrank test)
    Code
      as.data.frame(ct_opt)
    Output
                .id      label                       variable              straight
      1         cyl        cyl                              4           7 (88, 100)
      2         cyl        cyl                              6                     0
      3         cyl        cyl                              8                     0
      4         cyl        cyl                             NA                     2
      5         cyl        cyl                          Total                9 (38)
      6        carb       carb function(x, na.rm, dig, ...){}                 1 (0)
      7  qsec_posix qsec_posix function(x, na.rm, dig, ...){} 19/01/2010 (23 hours)
      8        surv       surv                            t=0               1 (0/9)
      9        surv       surv                          t=100               0 (5/4)
      10       surv       surv   Median follow up [min ; max]     258 [120.1 ; 258]
      11       surv       surv                Median survival                  95.1
                     vshaped                  NA               Total
      1            1 (12, 8)                   2             10 (37)
      2           1 (100, 8)                   3              4 (15)
      3         11 (100, 85)                   2             13 (48)
      4                    2                   1                   5
      5              15 (62)                   8            32 (100)
      6                4 (2)               3 (1)               3 (2)
      7  17/01/2010 (1 days) 19/01/2010 (2 days) 18/01/2010 (2 days)
      8             1 (0/15)             1 (0/8)            1 (0/32)
      9             1 (0/15)             1 (0/8)            1 (5/27)
      10   400 [275.8 ; 472]   196 [140.8 ; 360]   304 [120.1 ; 472]
      11                <NA>                <NA>                <NA>
                                                                                                                                                                                                                                                                                                                                                                                        effect
      1  Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5981748110.20 [0.00 to  NA]\n8 vs 4: 5981747998.47 [0.00 to 27186346135461263890806200044022640026846246408484268260488648884006448242402040080820224866428684660466004884824024240642426044080606868082862846066268200000420064642680200242868046824042244480486866048068002086428806002462242284004446426248468260.00]
      2  Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5981748110.20 [0.00 to  NA]\n8 vs 4: 5981747998.47 [0.00 to 27186346135461263890806200044022640026846246408484268260488648884006448242402040080820224866428684660466004884824024240642426044080606868082862846066268200000420064642680200242868046824042244480486866048068002086428806002462242284004446426248468260.00]
      3  Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5981748110.20 [0.00 to  NA]\n8 vs 4: 5981747998.47 [0.00 to 27186346135461263890806200044022640026846246408484268260488648884006448242402040080820224866428684660466004884824024240642426044080606868082862846066268200000420064642680200242868046824042244480486866048068002086428806002462242284004446426248468260.00]
      4  Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5981748110.20 [0.00 to  NA]\n8 vs 4: 5981747998.47 [0.00 to 27186346135461263890806200044022640026846246408484268260488648884006448242402040080820224866428684660466004884824024240642426044080606868082862846066268200000420064642680200242868046824042244480486866048068002086428806002462242284004446426248468260.00]
      5  Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5981748110.20 [0.00 to  NA]\n8 vs 4: 5981747998.47 [0.00 to 27186346135461263890806200044022640026846246408484268260488648884006448242402040080820224866428684660466004884824024240642426044080606868082862846066268200000420064642680200242868046824042244480486866048068002086428806002462242284004446426248468260.00]
      6                                                                                                                                                                                                                                                                                                 Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2 [2 to 3]
      7                                                                                                                                                                                                                                                                                  Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -194035 [-276312 to -111758]
      8                                                                                                                                                                                                                                                                                                                       Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      9                                                                                                                                                                                                                                                                                                                       Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      10                                                                                                                                                                                                                                                                                                                      Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      11                                                                                                                                                                                                                                                                                                                      Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
                                                           test
      1  p value: 0.0001 \n(Fisher's Exact Test for Count Data)
      2  p value: 0.0001 \n(Fisher's Exact Test for Count Data)
      3  p value: 0.0001 \n(Fisher's Exact Test for Count Data)
      4  p value: 0.0001 \n(Fisher's Exact Test for Count Data)
      5  p value: 0.0001 \n(Fisher's Exact Test for Count Data)
      6              p value: 0.0001 \n(Wilcoxon rank sum test)
      7                   p value: 0.0001 \n(Two Sample t-test)
      8                        p value: 0.0002 \n(Logrank test)
      9                        p value: 0.0002 \n(Logrank test)
      10                       p value: 0.0002 \n(Logrank test)
      11                       p value: 0.0002 \n(Logrank test)

---

    Code
      ft_opt
    Output
      a flextable object.
      col_keys: `variable`, `straight`, `vshaped`, `NA`, `Total`, `effect`, `test` 
      header has 2 row(s) 
      body has 15 row(s) 
      original dataset sample: 
         variable    straight      vshaped NA   Total
      1 cyl (cyl)                                    
      2         4 7 (88, 100)    1 (12, 8)  2 10 (37)
      3         6           0   1 (100, 8)  3  4 (15)
      4         8           0 11 (100, 85)  2 13 (48)
      5        NA           2            2  1       5
                                                                                                                                                                                                                                                                                                                                                                                       effect
      1 Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5981748110.20 [0.00 to  NA]\n8 vs 4: 5981747998.47 [0.00 to 27186346135461263890806200044022640026846246408484268260488648884006448242402040080820224866428684660466004884824024240642426044080606868082862846066268200000420064642680200242868046824042244480486866048068002086428806002462242284004446426248468260.00]
      2                                                                                                                                                                                                                                                                                                                                                                                      
      3                                                                                                                                                                                                                                                                                                                                                                                      
      4                                                                                                                                                                                                                                                                                                                                                                                      
      5                                                                                                                                                                                                                                                                                                                                                                                      
                    test
      1 p value: 0.0001 
      2                 
      3                 
      4                 
      5                 
    Code
      ft_opt
    Output
      a flextable object.
      col_keys: `variable`, `straight`, `vshaped`, `NA`, `Total`, `effect`, `test` 
      header has 2 row(s) 
      body has 15 row(s) 
      original dataset sample: 
         variable    straight      vshaped NA   Total
      1 cyl (cyl)                                    
      2         4 7 (88, 100)    1 (12, 8)  2 10 (37)
      3         6           0   1 (100, 8)  3  4 (15)
      4         8           0 11 (100, 85)  2 13 (48)
      5        NA           2            2  1       5
                                                                                                                                                                                                                                                                                                                                                                                       effect
      1 Odds ratio [70% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5981748110.20 [0.00 to  NA]\n8 vs 4: 5981747998.47 [0.00 to 27186346135461263890806200044022640026846246408484268260488648884006448242402040080820224866428684660466004884824024240642426044080606868082862846066268200000420064642680200242868046824042244480486866048068002086428806002462242284004446426248468260.00]
      2                                                                                                                                                                                                                                                                                                                                                                                      
      3                                                                                                                                                                                                                                                                                                                                                                                      
      4                                                                                                                                                                                                                                                                                                                                                                                      
      5                                                                                                                                                                                                                                                                                                                                                                                      
                    test
      1 p value: 0.0001 
      2                 
      3                 
      4                 
      5                 

