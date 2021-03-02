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
      cross_list
    Output
      [[1]]
                 .id           label variable value
      1 Sepal.Length Length of Sepal     mean   5.8
      2  Sepal.Width  Width of Sepal     mean   3.1
      
      [[2]]
                 .id           label variable value
      1 Sepal.Length Length of Sepal   "mean"   5.8
      2  Sepal.Width  Width of Sepal   "mean"   3.1
      
      [[3]]
                 .id           label variable value
      1 Sepal.Length Length of Sepal  My mean   5.8
      2  Sepal.Width  Width of Sepal  My mean   3.1
      
      [[4]]
                 .id           label variable value
      1 Sepal.Length Length of Sepal  My mean   5.8
      2  Sepal.Width  Width of Sepal  My mean   3.1
      
      [[5]]
                 .id           label                 variable         value
      1 Sepal.Length Length of Sepal  cross_summary Min / Max     4.3 / 7.9
      2 Sepal.Length Length of Sepal  cross_summary Med [IQR] 5.8 [5.1;6.4]
      3 Sepal.Length Length of Sepal cross_summary Mean (std)     5.8 (0.8)
      4 Sepal.Length Length of Sepal     cross_summary N (NA)       150 (0)
      5  Sepal.Width  Width of Sepal  cross_summary Min / Max     2.0 / 4.4
      6  Sepal.Width  Width of Sepal  cross_summary Med [IQR] 3.0 [2.8;3.3]
      7  Sepal.Width  Width of Sepal cross_summary Mean (std)     3.1 (0.4)
      8  Sepal.Width  Width of Sepal     cross_summary N (NA)       150 (0)
      
      [[6]]
                 .id           label   variable         value
      1 Sepal.Length Length of Sepal  Min / Max     4.3 / 7.9
      2 Sepal.Length Length of Sepal  Med [IQR] 5.8 [5.1;6.4]
      3 Sepal.Length Length of Sepal Mean (std)     5.8 (0.8)
      4 Sepal.Length Length of Sepal     N (NA)       150 (0)
      5  Sepal.Width  Width of Sepal  Min / Max     2.0 / 4.4
      6  Sepal.Width  Width of Sepal  Med [IQR] 3.0 [2.8;3.3]
      7  Sepal.Width  Width of Sepal Mean (std)     3.1 (0.4)
      8  Sepal.Width  Width of Sepal     N (NA)       150 (0)
      
      [[7]]
                 .id           label   variable         value
      1 Sepal.Length Length of Sepal  Min / Max     4.3 / 7.9
      2 Sepal.Length Length of Sepal  Med [IQR] 5.8 [5.1;6.4]
      3 Sepal.Length Length of Sepal Mean (std)     5.8 (0.8)
      4 Sepal.Length Length of Sepal     N (NA)       150 (0)
      5  Sepal.Width  Width of Sepal  Min / Max     2.0 / 4.4
      6  Sepal.Width  Width of Sepal  Med [IQR] 3.0 [2.8;3.3]
      7  Sepal.Width  Width of Sepal Mean (std)     3.1 (0.4)
      8  Sepal.Width  Width of Sepal     N (NA)       150 (0)
      
      [[8]]
                  .id           label                  variable         value
      1  Sepal.Length Length of Sepal  cross_summary1 Min / Max     4.3 / 7.9
      2  Sepal.Length Length of Sepal  cross_summary1 Med [IQR] 5.8 [5.1;6.4]
      3  Sepal.Length Length of Sepal cross_summary1 Mean (std)     5.8 (0.8)
      4  Sepal.Length Length of Sepal     cross_summary1 N (NA)       150 (0)
      5  Sepal.Length Length of Sepal               bar2 MinMax     4.3 / 7.9
      6  Sepal.Length Length of Sepal                 bar2 N_NA       150 (0)
      7   Sepal.Width  Width of Sepal  cross_summary1 Min / Max     2.0 / 4.4
      8   Sepal.Width  Width of Sepal  cross_summary1 Med [IQR] 3.0 [2.8;3.3]
      9   Sepal.Width  Width of Sepal cross_summary1 Mean (std)     3.1 (0.4)
      10  Sepal.Width  Width of Sepal     cross_summary1 N (NA)       150 (0)
      11  Sepal.Width  Width of Sepal               bar2 MinMax     2.0 / 4.4
      12  Sepal.Width  Width of Sepal                 bar2 N_NA       150 (0)
      

# Testing everything

    Code
      x
    Output
          .id                    label                     variable
      1  disp    Displacement (cu.in.)                    Min / Max
      2  disp    Displacement (cu.in.)                    Med [IQR]
      3  disp    Displacement (cu.in.)                   Mean (std)
      4  disp    Displacement (cu.in.)                       N (NA)
      5    hp         Gross horsepower                    Min / Max
      6    hp         Gross horsepower                    Med [IQR]
      7    hp         Gross horsepower                   Mean (std)
      8    hp         Gross horsepower                       N (NA)
      9    am             Transmission                         auto
      10   am             Transmission                       manual
      11   am             Transmission                        Total
      12 surv Dummy survival (disp/am)                          t=0
      13 surv Dummy survival (disp/am)                        t=100
      14 surv Dummy survival (disp/am)                        t=200
      15 surv Dummy survival (disp/am)                        t=400
      16 surv Dummy survival (disp/am) Median follow up [min ; max]
      17 surv Dummy survival (disp/am)              Median survival
                                          straight
      1               71.100000000 / 258.000000000
      2  95.100000000 [78.700000000;120.100000000]
      3               111.855555556 (58.043542085)
      4                                      9 (0)
      5               52.000000000 / 113.000000000
      6  93.000000000 [66.000000000;109.000000000]
      7                85.666666667 (23.452078799)
      8                                      9 (0)
      9                2 (8.33% / 18.18% / 22.22%)
      10              7 (29.17% / 53.85% / 77.78%)
      11                                9 (37.50%)
      12                                1.00 (0/9)
      13                                0.44 (5/4)
      14                                0.17 (2/1)
      15                                0.17 (0/0)
      16                         258 [120.1 ; 258]
      17                                      95.1
                                             vshaped
      1                120.300000000 / 472.000000000
      2  304.000000000 [217.900000000;375.500000000]
      3                302.193333333 (115.524335511)
      4                                       15 (0)
      5                 91.000000000 / 335.000000000
      6  180.000000000 [150.000000000;222.500000000]
      7                 187.666666667 (64.556361495)
      8                                       15 (0)
      9                 9 (37.50% / 81.82% / 60.00%)
      10                6 (25.00% / 46.15% / 40.00%)
      11                                 15 (62.50%)
      12                                 1.00 (0/15)
      13                                 1.00 (0/15)
      14                                 0.73 (4/11)
      15                                  0.52 (2/4)
      16                           400 [275.8 ; 472]
      17                                        <NA>
                                                  NA
      1                140.800000000 / 360.000000000
      2  196.300000000 [162.375000000;296.850000000]
      3                 230.437500000 (91.498757798)
      4                                        8 (0)
      5                 62.000000000 / 245.000000000
      6  123.000000000 [102.500000000;176.250000000]
      7                 138.500000000 (58.240879114)
      8                                        8 (0)
      9                                            8
      10                                           0
      11                                           8
      12                                  1.00 (0/8)
      13                                  1.00 (0/8)
      14                                  1.00 (0/4)
      15                                  1.00 (0/0)
      16                         196.3 [140.8 ; 360]
      17                                        <NA>
                                               Total
      1                 71.100000000 / 472.000000000
      2  196.300000000 [120.825000000;326.000000000]
      3                230.721875000 (123.938693831)
      4                                       32 (0)
      5                 52.000000000 / 335.000000000
      6   123.000000000 [96.500000000;180.000000000]
      7                 146.687500000 (68.562868489)
      8                                       32 (0)
      9                                  19 (59.38%)
      10                                 13 (40.62%)
      11                                32 (100.00%)
      12                                 1.00 (0/32)
      13                                 0.84 (5/27)
      14                                 0.64 (6/16)
      15                                  0.50 (2/4)
      16                           304 [120.1 ; 472]
      17                                        <NA>
                                                                                                                                                         effect
      1                                                        Difference in means (bootstrap CI) (straight minus vshaped): -190.34\n95%CI [-260.78 to -119.89]
      2                                                        Difference in means (bootstrap CI) (straight minus vshaped): -190.34\n95%CI [-260.78 to -119.89]
      3                                                        Difference in means (bootstrap CI) (straight minus vshaped): -190.34\n95%CI [-260.78 to -119.89]
      4                                                        Difference in means (bootstrap CI) (straight minus vshaped): -190.34\n95%CI [-260.78 to -119.89]
      5                                                             Difference in means (Welch CI) (straight minus vshaped): -102.00\n95%CI [-140.51 to -63.49]
      6                                                             Difference in means (Welch CI) (straight minus vshaped): -102.00\n95%CI [-140.51 to -63.49]
      7                                                             Difference in means (Welch CI) (straight minus vshaped): -102.00\n95%CI [-140.51 to -63.49]
      8                                                             Difference in means (Welch CI) (straight minus vshaped): -102.00\n95%CI [-140.51 to -63.49]
      9                                                                           Odds ratio (Wald CI) (auto, vshaped vs straight): 5.25\n95%CI [0.80 to 34.43]
      10                                                                          Odds ratio (Wald CI) (auto, vshaped vs straight): 5.25\n95%CI [0.80 to 34.43]
      11                                                                          Odds ratio (Wald CI) (auto, vshaped vs straight): 5.25\n95%CI [0.80 to 34.43]
      12 Hazard ratio (Wald CI) (straight vs NA): 3473769827.94\n95%CI [0.00 to Inf]\nHazard ratio (Wald CI) (vshaped vs NA): 395557782.33\n95%CI [0.00 to Inf]
      13 Hazard ratio (Wald CI) (straight vs NA): 3473769827.94\n95%CI [0.00 to Inf]\nHazard ratio (Wald CI) (vshaped vs NA): 395557782.33\n95%CI [0.00 to Inf]
      14 Hazard ratio (Wald CI) (straight vs NA): 3473769827.94\n95%CI [0.00 to Inf]\nHazard ratio (Wald CI) (vshaped vs NA): 395557782.33\n95%CI [0.00 to Inf]
      15 Hazard ratio (Wald CI) (straight vs NA): 3473769827.94\n95%CI [0.00 to Inf]\nHazard ratio (Wald CI) (vshaped vs NA): 395557782.33\n95%CI [0.00 to Inf]
      16 Hazard ratio (Wald CI) (straight vs NA): 3473769827.94\n95%CI [0.00 to Inf]\nHazard ratio (Wald CI) (vshaped vs NA): 395557782.33\n95%CI [0.00 to Inf]
      17 Hazard ratio (Wald CI) (straight vs NA): 3473769827.94\n95%CI [0.00 to Inf]\nHazard ratio (Wald CI) (vshaped vs NA): 395557782.33\n95%CI [0.00 to Inf]
                                                           test
      1              p value: 0.0002 \n(Wilcoxon rank sum test)
      2              p value: 0.0002 \n(Wilcoxon rank sum test)
      3              p value: 0.0002 \n(Wilcoxon rank sum test)
      4              p value: 0.0002 \n(Wilcoxon rank sum test)
      5            p value: <0.0001 \n(Welch Two Sample t-test)
      6            p value: <0.0001 \n(Welch Two Sample t-test)
      7            p value: <0.0001 \n(Welch Two Sample t-test)
      8            p value: <0.0001 \n(Welch Two Sample t-test)
      9  p value: 0.1049 \n(Fisher's Exact Test for Count Data)
      10 p value: 0.1049 \n(Fisher's Exact Test for Count Data)
      11 p value: 0.1049 \n(Fisher's Exact Test for Count Data)
      12                      p value: <0.0001 \n(Logrank test)
      13                      p value: <0.0001 \n(Logrank test)
      14                      p value: <0.0001 \n(Logrank test)
      15                      p value: <0.0001 \n(Logrank test)
      16                      p value: <0.0001 \n(Logrank test)
      17                      p value: <0.0001 \n(Logrank test)

---

    Code
      ft
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA`, `Total`, `effect`, `test` 
      header has 2 row(s) 
      body has 17 row(s) 
      original dataset sample: 
         .id                 label   variable
      1 disp Displacement (cu.in.)  Min / Max
      2 disp Displacement (cu.in.)  Med [IQR]
      3 disp Displacement (cu.in.) Mean (std)
      4 disp Displacement (cu.in.)     N (NA)
      5   hp      Gross horsepower  Min / Max
                                         straight
      1              71.100000000 / 258.000000000
      2 95.100000000 [78.700000000;120.100000000]
      3              111.855555556 (58.043542085)
      4                                     9 (0)
      5              52.000000000 / 113.000000000
                                            vshaped
      1               120.300000000 / 472.000000000
      2 304.000000000 [217.900000000;375.500000000]
      3               302.193333333 (115.524335511)
      4                                      15 (0)
      5                91.000000000 / 335.000000000
                                                 NA
      1               140.800000000 / 360.000000000
      2 196.300000000 [162.375000000;296.850000000]
      3                230.437500000 (91.498757798)
      4                                       8 (0)
      5                62.000000000 / 245.000000000
                                              Total
      1                71.100000000 / 472.000000000
      2 196.300000000 [120.825000000;326.000000000]
      3               230.721875000 (123.938693831)
      4                                      32 (0)
      5                52.000000000 / 335.000000000
                                                                                                  effect
      1 Difference in means (bootstrap CI) (straight minus vshaped): -190.34\n95%CI [-260.78 to -119.89]
      2 Difference in means (bootstrap CI) (straight minus vshaped): -190.34\n95%CI [-260.78 to -119.89]
      3 Difference in means (bootstrap CI) (straight minus vshaped): -190.34\n95%CI [-260.78 to -119.89]
      4 Difference in means (bootstrap CI) (straight minus vshaped): -190.34\n95%CI [-260.78 to -119.89]
      5      Difference in means (Welch CI) (straight minus vshaped): -102.00\n95%CI [-140.51 to -63.49]
                                                test
      1   p value: 0.0002 \n(Wilcoxon rank sum test)
      2   p value: 0.0002 \n(Wilcoxon rank sum test)
      3   p value: 0.0002 \n(Wilcoxon rank sum test)
      4   p value: 0.0002 \n(Wilcoxon rank sum test)
      5 p value: <0.0001 \n(Welch Two Sample t-test)

# Effects never fail 1

    Code
      print(.x)
    Output
      [1] "mpg"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'mpg' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "cyl"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Could not calculate crosstable effects for variables 'mpg', 'disp', 'hp', 'drat', 'wt', 'qsec', 'gear', 'carb', 'hp_date', 'qsec_posix', 'cyl3', 'dummy', 'dummy_na', 'dummy_num_vs', 'dummy2', and 'diff'. Aren't there 2 groups exactly?
    Output
                                                                                                                                                                                             .
      1 Hazard ratio (Wald CI) (6 vs 4): 0.08\n95%CI [0.01 to 0.86]\nHazard ratio (Wald CI) (8 vs 4): 0.02\n95%CI [0.00 to 0.18]\nHazard ratio (Wald CI) (NA vs 4): 0.15\n95%CI [0.02 to 0.92]
      2                                                                                                                                                                             No effect?
      3                                                              Odds ratio (Wald CI) (A, 6 vs 4): 0.00\n95%CI [0.00 to Inf]\nOdds ratio (Wald CI) (A, 8 vs 4): 0.57\n95%CI [0.11 to 3.04]
      4                                                       Odds ratio (Wald CI) (FALSE, 6 vs 4): 0.00\n95%CI [0.00 to Inf]\nOdds ratio (Wald CI) (FALSE, 8 vs 4): 1.00\n95%CI [0.00 to Inf]
      5                                                    Odds ratio (Wald CI) (auto, 6 vs 4): 7.00\n95%CI [0.50 to 97.75]\nOdds ratio (Wald CI) (auto, 8 vs 4): 12.83\n95%CI [1.69 to 97.19]
      6                                                 Odds ratio (Wald CI) (straight, 6 vs 4): 0.00\n95%CI [0.00 to Inf]\nOdds ratio (Wald CI) (straight, 8 vs 4): 0.00\n95%CI [0.00 to Inf]
        Freq
      1   29
      2   53
      3    2
      4    3
      5    2
      6    3

---

    Code
      print(.x)
    Output
      [1] "disp"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'disp' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "hp"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'hp' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "drat"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'drat' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "wt"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'wt' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "qsec"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'qsec' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "vs"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      A problem occured when calculating crosstable effects (coxph): 'Loglik converged before variable 1,2 ; coefficient may be infinite. '.
      Could not calculate crosstable effects for variables 'cyl', 'gear', 'cyl3', 'dummy', 'dummy_na', and 'dummy2'. Aren't there 2 groups exactly?
    Output
                                                                                                                                                              .
      1                                                             Difference in means (Welch CI) (straight minus vshaped): -102.00\n95%CI [-140.51 to -63.49]
      2                                                             Difference in means (Welch CI) (straight minus vshaped): -104.25\n95%CI [-143.12 to -65.37]
      3                                                               Difference in means (bootstrap CI) (straight minus vshaped): -0.28\n95%CI [-1.11 to 0.54]
      4                                                        Difference in means (bootstrap CI) (straight minus vshaped): -190.34\n95%CI [-260.78 to -119.89]
      5                                                              Difference in means (bootstrap CI) (straight minus vshaped): -2.40\n95%CI [-3.20 to -1.60]
      6                                                                 Difference in means (t-test CI) (straight minus vshaped): -1.48\n95%CI [-2.23 to -0.73]
      7                                                                    Difference in means (t-test CI) (straight minus vshaped): 0.54\n95%CI [0.10 to 0.98]
      8                                                                  Difference in means (t-test CI) (straight minus vshaped): 10.19\n95%CI [6.21 to 14.16]
      9                                                     Difference in means (t-test CI) (straight minus vshaped): 194035.20\n95%CI [111758.48 to 276311.92]
      10                                                                   Difference in means (t-test CI) (straight minus vshaped): 2.25\n95%CI [1.29 to 3.20]
      11 Hazard ratio (Wald CI) (straight vs NA): 3473769827.94\n95%CI [0.00 to Inf]\nHazard ratio (Wald CI) (vshaped vs NA): 395557782.33\n95%CI [0.00 to Inf]
      12                                                                                                                                             No effect?
      13                                                                              Odds ratio (Wald CI) (A, vshaped vs straight): 0.91\n95%CI [0.17 to 4.81]
      14                                                                           Odds ratio (Wald CI) (FALSE, vshaped vs straight): 0.00\n95%CI [0.00 to Inf]
      15                                                                          Odds ratio (Wald CI) (auto, vshaped vs straight): 5.25\n95%CI [0.80 to 34.43]
         Freq
      1     8
      2     4
      3     4
      4     4
      5     4
      6     4
      7     4
      8     4
      9     4
      10    4
      11   29
      12   13
      13    2
      14    3
      15    2

---

    Code
      print(.x)
    Output
      [1] "am"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      A problem occured when calculating crosstable effects (coxph): 'Loglik converged before variable 1 ; coefficient may be infinite. '.
      Could not calculate crosstable effects for variables 'cyl', 'gear', 'cyl3', 'dummy', 'dummy_na', and 'dummy2'. Aren't there 2 groups exactly?
    Output
                                                                                                     .
      1           Difference in means (bootstrap CI) (auto minus manual): -0.11\n95%CI [-0.71 to 0.48]
      2           Difference in means (bootstrap CI) (auto minus manual): -0.19\n95%CI [-1.46 to 1.09]
      3          Difference in means (bootstrap CI) (auto minus manual): -0.76\n95%CI [-1.02 to -0.51]
      4             Difference in means (bootstrap CI) (auto minus manual): 1.36\n95%CI [0.88 to 1.84]
      5        Difference in means (bootstrap CI) (auto minus manual): 146.85\n95%CI [79.28 to 214.41]
      6         Difference in means (bootstrap CI) (auto minus manual): 32.59\n95%CI [-19.33 to 84.52]
      7         Difference in means (bootstrap CI) (auto minus manual): 33.42\n95%CI [-16.28 to 83.11]
      8         Difference in means (bootstrap CI) (auto minus manual): 33.42\n95%CI [-19.05 to 85.88]
      9            Difference in means (t-test CI) (auto minus manual): -7.24\n95%CI [-10.85 to -3.64]
      10              Difference in means (t-test CI) (auto minus manual): 0.82\n95%CI [-0.48 to 2.12]
      11 Difference in means (t-test CI) (auto minus manual): 71120.84\n95%CI [-41157.89 to 183399.57]
      12                   Hazard ratio (Wald CI) (manual vs auto): 3289345612.97\n95%CI [0.00 to Inf]
      13                                                                                    No effect?
      14                          Odds ratio (Wald CI) (A, manual vs auto): 2.20\n95%CI [0.52 to 9.30]
      15                     Odds ratio (Wald CI) (FALSE, manual vs auto): 1.93\n95%CI [0.17 to 21.54]
      16                  Odds ratio (Wald CI) (straight, manual vs auto): 5.25\n95%CI [0.80 to 34.43]
         Freq
      1     4
      2     4
      3     4
      4     4
      5     4
      6     4
      7     4
      8     4
      9     4
      10    4
      11    4
      12   28
      13   13
      14    2
      15    3
      16    3

---

    Code
      print(.x)
    Output
      [1] "gear"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      A problem occured when calculating crosstable effects (coxph): 'Loglik converged before variable 1,2 ; coefficient may be infinite. '.
      Could not calculate crosstable effects for variables 'mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'carb', 'hp_date', 'qsec_posix', 'cyl3', 'dummy', 'dummy_na', 'dummy_num_vs', 'dummy2', and 'diff'. Aren't there 2 groups exactly?
    Output
                                                                                                                                                   .
      1     Hazard ratio (Wald CI) (4 vs 3): 4429358637.03\n95%CI [0.00 to Inf]\nHazard ratio (Wald CI) (5 vs 3): 3158775523.94\n95%CI [0.00 to Inf]
      2                                                                                                                                   No effect?
      3                   Odds ratio (Wald CI) (A, 4 vs 3): 1.60\n95%CI [0.35 to 7.40]\nOdds ratio (Wald CI) (A, 5 vs 3): 0.76\n95%CI [0.10 to 5.96]
      4           Odds ratio (Wald CI) (FALSE, 4 vs 3): 0.29\n95%CI [0.02 to 3.83]\nOdds ratio (Wald CI) (FALSE, 5 vs 3): 0.33\n95%CI [0.02 to 6.65]
      5               Odds ratio (Wald CI) (auto, 4 vs 3): 0.00\n95%CI [0.00 to Inf]\nOdds ratio (Wald CI) (auto, 5 vs 3): 0.00\n95%CI [0.00 to Inf]
      6 Odds ratio (Wald CI) (straight, 4 vs 3): 13.50\n95%CI [1.47 to 123.74]\nOdds ratio (Wald CI) (straight, 5 vs 3): 1.13\n95%CI [0.08 to 16.31]
        Freq
      1   28
      2   54
      3    2
      4    3
      5    2
      6    3

---

    Code
      print(.x)
    Output
      [1] "carb"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'carb' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "cyl3"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Could not calculate crosstable effects for variables 'mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb', 'hp_date', 'qsec_posix', 'cyl6', 'dummy', 'dummy_na', 'dummy_num_vs', 'dummy2', 'test', and 'diff'. Aren't there 2 groups exactly?
    Output
                                                                       . Freq
      1 Hazard ratio (Wald CI) (NA vs FALSE): 1.54\n95%CI [0.42 to 5.63]   29
      2                                                       No effect?   65

---

    Code
      print(.x)
    Output
      [1] "cyl6"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Could not calculate crosstable effects for variables 'cyl', 'gear', 'cyl3', 'dummy', 'dummy_na', and 'dummy2'. Aren't there 2 groups exactly?
    Output
                                                                                                                                            .
      1                                                        Difference in means (Welch CI) (FALSE minus TRUE): 1.44\n95%CI [-1.74 to 4.62]
      2                                                   Difference in means (bootstrap CI) (FALSE minus TRUE): -0.00\n95%CI [-0.56 to 0.56]
      3                                                   Difference in means (bootstrap CI) (FALSE minus TRUE): -0.07\n95%CI [-0.37 to 0.23]
      4                                                   Difference in means (bootstrap CI) (FALSE minus TRUE): -1.01\n95%CI [-3.09 to 1.07]
      5                                                  Difference in means (bootstrap CI) (FALSE minus TRUE): 68.57\n95%CI [6.00 to 131.13]
      6                                                      Difference in means (t-test CI) (FALSE minus TRUE): -0.42\n95%CI [-2.56 to 1.72]
      7                                        Difference in means (t-test CI) (FALSE minus TRUE): -36062.61\n95%CI [-220950.23 to 148825.01]
      8                                                       Difference in means (t-test CI) (FALSE minus TRUE): 0.05\n95%CI [-0.58 to 0.69]
      9                                                   Difference in means (t-test CI) (FALSE minus TRUE): 23.72\n95%CI [-58.40 to 105.83]
      10                                                  Difference in means (t-test CI) (FALSE minus TRUE): 24.13\n95%CI [-59.58 to 107.85]
      11 Hazard ratio (Wald CI) (NA vs FALSE): 1.45\n95%CI [0.39 to 5.40]\nHazard ratio (Wald CI) (TRUE vs FALSE): 0.63\n95%CI [0.08 to 5.04]
      12                                                                                                                           No effect?
      13                                                                   Odds ratio (Wald CI) (A, TRUE vs FALSE): 0.00\n95%CI [0.00 to Inf]
      14                                                              Odds ratio (Wald CI) (auto, TRUE vs FALSE): 1.93\n95%CI [0.17 to 21.54]
      15                                                            Odds ratio (Wald CI) (straight, TRUE vs FALSE): 0.00\n95%CI [0.00 to Inf]
         Freq
      1     4
      2     4
      3     4
      4     4
      5     4
      6     4
      7     4
      8     4
      9     8
      10    4
      11   29
      12   13
      13    2
      14    2
      15    3

---

    Code
      print(.x)
    Output
      [1] "dummy"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Could not calculate crosstable effects for variables 'mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb', 'hp_date', 'qsec_posix', 'cyl3', 'cyl6', 'dummy_na', 'dummy_num_vs', 'dummy2', 'test', and 'diff'. Aren't there 2 groups exactly?
    Output
                 . Freq
      1  No Effect   28
      2 No effect?   66

---

    Code
      print(.x)
    Output
      [1] "dummy_num_vs"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'dummy_num_vs' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "dummy2"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      A problem occured when calculating crosstable effects (coxph): 'Loglik converged before variable 1 ; coefficient may be infinite. '.
      Could not calculate crosstable effects for variables 'mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb', 'hp_date', 'qsec_posix', 'cyl3', 'cyl6', 'dummy', 'dummy_na', 'dummy_num_vs', 'test', and 'diff'. Aren't there 2 groups exactly?
    Output
                                                                              . Freq
      1 Hazard ratio (Wald CI) (dummy vs NA): 312156724.32\n95%CI [0.00 to Inf]   29
      2                                                              No effect?   65

---

    Code
      print(.x)
    Output
      [1] "test"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Could not calculate crosstable effects for variables 'cyl', 'gear', 'cyl3', 'dummy', 'dummy_na', and 'dummy2'. Aren't there 2 groups exactly?
    Output
                                                                                              .
      1            Difference in means (bootstrap CI) (A minus B): -0.62\n95%CI [-1.74 to 0.49]
      2          Difference in means (bootstrap CI) (A minus B): -8.03\n95%CI [-91.84 to 75.77]
      3             Difference in means (bootstrap CI) (A minus B): 0.33\n95%CI [-0.28 to 0.95]
      4               Difference in means (t-test CI) (A minus B): -0.34\n95%CI [-1.05 to 0.36]
      5               Difference in means (t-test CI) (A minus B): -0.42\n95%CI [-1.72 to 0.89]
      6            Difference in means (t-test CI) (A minus B): -19.96\n95%CI [-70.68 to 30.76]
      7            Difference in means (t-test CI) (A minus B): -20.38\n95%CI [-70.12 to 29.37]
      8  Difference in means (t-test CI) (A minus B): -35964.00\n95%CI [-148489.58 to 76561.58]
      9                Difference in means (t-test CI) (A minus B): 0.18\n95%CI [-0.20 to 0.57]
      10               Difference in means (t-test CI) (A minus B): 1.33\n95%CI [-3.06 to 5.73]
      11                            Hazard ratio (Wald CI) (B vs A): 0.63\n95%CI [0.20 to 1.94]
      12                                                                             No effect?
      13                        Odds ratio (Wald CI) (FALSE, B vs A): 0.00\n95%CI [0.00 to Inf]
      14                        Odds ratio (Wald CI) (auto, B vs A): 2.20\n95%CI [0.52 to 9.30]
      15                    Odds ratio (Wald CI) (straight, B vs A): 0.91\n95%CI [0.17 to 4.81]
         Freq
      1     4
      2     4
      3     4
      4     4
      5     4
      6     4
      7     8
      8     4
      9     4
      10    4
      11   29
      12   13
      13    3
      14    2
      15    3

# Effects never fail 2

    Code
      print(.x)
    Output
      [1] "mpg"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'mpg' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "cyl"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      A problem occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      Could not calculate crosstable effects for variables 'mpg', 'disp', 'hp', 'drat', 'wt', 'qsec', 'gear', 'carb', 'hp_date', 'qsec_posix', 'dummy_num_vs', and 'diff'. Aren't there 2 groups exactly?
    Output
                                                                                                                                                                                             .
      1                                                                                                Error (glm: no valid set of coefficients has been found: please supply starting values)
      2 Hazard ratio (Wald CI) (6 vs 4): 0.08\n95%CI [0.01 to 0.86]\nHazard ratio (Wald CI) (8 vs 4): 0.02\n95%CI [0.00 to 0.18]\nHazard ratio (Wald CI) (NA vs 4): 0.15\n95%CI [0.02 to 0.92]
      3                                                                                                                                                                             No effect?
      4                                                Relative risk (Wald CI) (auto, 6 vs 4): 2.50\n95%CI [0.74 to 9.49]\nRelative risk (Wald CI) (auto, 8 vs 4): 2.82\n95%CI [1.29 to 10.15]
        Freq
      1   14
      2   29
      3   47
      4    2

---

    Code
      print(.x)
    Output
      [1] "disp"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'disp' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "hp"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'hp' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "drat"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'drat' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "wt"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'wt' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "qsec"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'qsec' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "vs"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      A problem occured when calculating crosstable effects (coxph): 'Loglik converged before variable 1,2 ; coefficient may be infinite. '.
      Could not calculate crosstable effects for variables 'cyl', and 'gear'. Aren't there 2 groups exactly?
    Output
                                                                                                                                                              .
      1                                                               Difference in means (bootstrap CI) (straight minus vshaped): -0.28\n95%CI [-1.16 to 0.59]
      2                                                              Difference in means (bootstrap CI) (straight minus vshaped): -1.48\n95%CI [-2.11 to -0.85]
      3                                                         Difference in means (bootstrap CI) (straight minus vshaped): -102.00\n95%CI [-137.39 to -66.61]
      4                                                         Difference in means (bootstrap CI) (straight minus vshaped): -102.00\n95%CI [-137.59 to -66.41]
      5                                                         Difference in means (bootstrap CI) (straight minus vshaped): -104.25\n95%CI [-141.51 to -66.98]
      6                                                        Difference in means (bootstrap CI) (straight minus vshaped): -190.34\n95%CI [-260.55 to -120.13]
      7                                                              Difference in means (bootstrap CI) (straight minus vshaped): -2.40\n95%CI [-3.27 to -1.53]
      8                                                                 Difference in means (bootstrap CI) (straight minus vshaped): 0.54\n95%CI [0.11 to 0.97]
      9                                                               Difference in means (bootstrap CI) (straight minus vshaped): 10.19\n95%CI [6.38 to 14.00]
      10                                                 Difference in means (bootstrap CI) (straight minus vshaped): 194035.20\n95%CI [118448.28 to 269622.12]
      11                                                                Difference in means (bootstrap CI) (straight minus vshaped): 2.25\n95%CI [1.38 to 3.11]
      12                                                                Error (glm: no valid set of coefficients has been found: please supply starting values)
      13 Hazard ratio (Wald CI) (straight vs NA): 3473769827.94\n95%CI [0.00 to Inf]\nHazard ratio (Wald CI) (vshaped vs NA): 395557782.33\n95%CI [0.00 to Inf]
      14                                                                                                                                             No effect?
      15                                                                       Relative risk (Wald CI) (auto, vshaped vs straight): 2.70\n95%CI [0.94 to 15.21]
         Freq
      1     4
      2     4
      3     4
      4     4
      5     4
      6     4
      7     4
      8     4
      9     4
      10    4
      11    4
      12   11
      13   29
      14    7
      15    2

---

    Code
      print(.x)
    Output
      [1] "am"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      A problem occured when calculating crosstable effects (coxph): 'Loglik converged before variable 1 ; coefficient may be infinite. '.
      Could not calculate crosstable effects for variables 'cyl', and 'gear'. Aren't there 2 groups exactly?
    Output
                                                                                                        .
      1              Difference in means (bootstrap CI) (auto minus manual): -0.11\n95%CI [-0.74 to 0.52]
      2              Difference in means (bootstrap CI) (auto minus manual): -0.19\n95%CI [-1.51 to 1.13]
      3             Difference in means (bootstrap CI) (auto minus manual): -0.76\n95%CI [-1.02 to -0.50]
      4            Difference in means (bootstrap CI) (auto minus manual): -7.24\n95%CI [-10.84 to -3.65]
      5               Difference in means (bootstrap CI) (auto minus manual): 0.82\n95%CI [-0.44 to 2.09]
      6                Difference in means (bootstrap CI) (auto minus manual): 1.36\n95%CI [0.88 to 1.84]
      7           Difference in means (bootstrap CI) (auto minus manual): 146.85\n95%CI [82.76 to 210.93]
      8            Difference in means (bootstrap CI) (auto minus manual): 32.59\n95%CI [-20.67 to 85.86]
      9            Difference in means (bootstrap CI) (auto minus manual): 33.42\n95%CI [-17.58 to 84.41]
      10           Difference in means (bootstrap CI) (auto minus manual): 33.42\n95%CI [-17.92 to 84.75]
      11 Difference in means (bootstrap CI) (auto minus manual): 71120.84\n95%CI [-34157.74 to 176399.43]
      12          Error (glm: no valid set of coefficients has been found: please supply starting values)
      13                      Hazard ratio (Wald CI) (manual vs auto): 3289345612.97\n95%CI [0.00 to Inf]
      14                                                                                       No effect?
      15                          Relative risk (Wald CI) (A, manual vs auto): 1.46\n95%CI [0.72 to 3.06]
      16                  Relative risk (Wald CI) (straight, manual vs auto): 2.96\n95%CI [0.94 to 17.27]
         Freq
      1     4
      2     4
      3     4
      4     4
      5     4
      6     4
      7     4
      8     4
      9     4
      10    4
      11    4
      12    9
      13   28
      14    7
      15    2
      16    3

---

    Code
      print(.x)
    Output
      [1] "gear"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      A problem occured when calculating crosstable effects (coxph): 'Loglik converged before variable 1,2 ; coefficient may be infinite. '.
      Could not calculate crosstable effects for variables 'mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'carb', 'hp_date', 'qsec_posix', 'dummy_num_vs', and 'diff'. Aren't there 2 groups exactly?
    Output
                                                                                                                                                      .
      1                                                         Error (glm: no valid set of coefficients has been found: please supply starting values)
      2        Hazard ratio (Wald CI) (4 vs 3): 4429358637.03\n95%CI [0.00 to Inf]\nHazard ratio (Wald CI) (5 vs 3): 3158775523.94\n95%CI [0.00 to Inf]
      3                                                                                                                                      No effect?
      4                Relative risk (Wald CI) (A, 4 vs 3): 1.25\n95%CI [0.58 to 2.75]\nRelative risk (Wald CI) (A, 5 vs 3): 0.86\n95%CI [0.16 to 2.38]
      5 Relative risk (Wald CI) (straight, 4 vs 3): 4.12\n95%CI [1.36 to 23.60]\nRelative risk (Wald CI) (straight, 5 vs 3): 1.10\n95%CI [0.06 to 9.25]
        Freq
      1   11
      2   28
      3   48
      4    2
      5    3

---

    Code
      print(.x)
    Output
      [1] "carb"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'carb' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "cyl3"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Could not calculate crosstable effects for variables 'mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb', 'hp_date', 'qsec_posix', 'cyl6', 'dummy', 'dummy_na', 'dummy_num_vs', 'dummy2', 'test', and 'diff'. Aren't there 2 groups exactly?
    Output
                                                                       . Freq
      1 Hazard ratio (Wald CI) (NA vs FALSE): 1.54\n95%CI [0.42 to 5.63]   29
      2                                                       No effect?   65

---

    Code
      print(.x)
    Output
      [1] "cyl6"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      A problem occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'step size truncated due to divergence' and 'glm.fit: algorithm did not converge'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      A problem occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      Could not calculate crosstable effects for variables 'cyl', and 'gear'. Aren't there 2 groups exactly?
    Output
                                                                                                                                            .
      1                                                      Difference in means (bootstrap CI) (FALSE minus TRUE): -0.00\n95%CI [ NA to  NA]
      2                                                      Difference in means (bootstrap CI) (FALSE minus TRUE): -0.07\n95%CI [ NA to  NA]
      3                                                      Difference in means (bootstrap CI) (FALSE minus TRUE): -0.42\n95%CI [ NA to  NA]
      4                                                      Difference in means (bootstrap CI) (FALSE minus TRUE): -1.01\n95%CI [ NA to  NA]
      5                                                  Difference in means (bootstrap CI) (FALSE minus TRUE): -36062.61\n95%CI [ NA to  NA]
      6                                                       Difference in means (bootstrap CI) (FALSE minus TRUE): 0.05\n95%CI [ NA to  NA]
      7                                                       Difference in means (bootstrap CI) (FALSE minus TRUE): 1.44\n95%CI [ NA to  NA]
      8                                                      Difference in means (bootstrap CI) (FALSE minus TRUE): 23.72\n95%CI [ NA to  NA]
      9                                                      Difference in means (bootstrap CI) (FALSE minus TRUE): 24.13\n95%CI [ NA to  NA]
      10                                                     Difference in means (bootstrap CI) (FALSE minus TRUE): 68.57\n95%CI [ NA to  NA]
      11                                              Error (glm: no valid set of coefficients has been found: please supply starting values)
      12 Hazard ratio (Wald CI) (NA vs FALSE): 1.45\n95%CI [0.39 to 5.40]\nHazard ratio (Wald CI) (TRUE vs FALSE): 0.63\n95%CI [0.08 to 5.04]
      13                                                                                                                           No effect?
      14                                                            Relative risk (Wald CI) (auto, TRUE vs FALSE): 1.23\n95%CI [0.44 to 2.08]
         Freq
      1     4
      2     4
      3     4
      4     4
      5     4
      6     4
      7     4
      8     8
      9     4
      10    4
      11   11
      12   29
      13    7
      14    2

---

    Code
      print(.x)
    Output
      [1] "dummy"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Could not calculate crosstable effects for variables 'mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb', 'hp_date', 'qsec_posix', 'cyl3', 'cyl6', 'dummy_na', 'dummy_num_vs', 'dummy2', 'test', and 'diff'. Aren't there 2 groups exactly?
    Output
                 . Freq
      1  No Effect   28
      2 No effect?   66

---

    Code
      print(.x)
    Output
      [1] "dummy_num_vs"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'dummy_num_vs' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "dummy2"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      A problem occured when calculating crosstable effects (coxph): 'Loglik converged before variable 1 ; coefficient may be infinite. '.
      Could not calculate crosstable effects for variables 'mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb', 'hp_date', 'qsec_posix', 'cyl3', 'cyl6', 'dummy', 'dummy_na', 'dummy_num_vs', 'test', and 'diff'. Aren't there 2 groups exactly?
    Output
                                                                              . Freq
      1 Hazard ratio (Wald CI) (dummy vs NA): 312156724.32\n95%CI [0.00 to Inf]   29
      2                                                              No effect?   65

---

    Code
      print(.x)
    Output
      [1] "test"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'no valid set of coefficients has been found: please supply starting values'. You might want to check for complete separation or extreme outliers.
      Could not calculate crosstable effects for variables 'cyl', and 'gear'. Aren't there 2 groups exactly?
    Output
                                                                                                 .
      1               Difference in means (bootstrap CI) (A minus B): -0.34\n95%CI [-1.05 to 0.36]
      2               Difference in means (bootstrap CI) (A minus B): -0.42\n95%CI [-1.73 to 0.90]
      3               Difference in means (bootstrap CI) (A minus B): -0.62\n95%CI [-1.72 to 0.47]
      4            Difference in means (bootstrap CI) (A minus B): -19.96\n95%CI [-69.34 to 29.43]
      5            Difference in means (bootstrap CI) (A minus B): -20.38\n95%CI [-64.50 to 23.75]
      6            Difference in means (bootstrap CI) (A minus B): -20.38\n95%CI [-66.61 to 25.86]
      7  Difference in means (bootstrap CI) (A minus B): -35964.00\n95%CI [-136946.64 to 65018.64]
      8             Difference in means (bootstrap CI) (A minus B): -8.03\n95%CI [-90.96 to 74.90]
      9                Difference in means (bootstrap CI) (A minus B): 0.18\n95%CI [-0.17 to 0.54]
      10               Difference in means (bootstrap CI) (A minus B): 0.33\n95%CI [-0.29 to 0.96]
      11               Difference in means (bootstrap CI) (A minus B): 1.33\n95%CI [-2.66 to 5.32]
      12   Error (glm: no valid set of coefficients has been found: please supply starting values)
      13                               Hazard ratio (Wald CI) (B vs A): 0.63\n95%CI [0.20 to 1.94]
      14                                                                                No effect?
      15                        Relative risk (Wald CI) (auto, B vs A): 1.37\n95%CI [0.77 to 2.70]
      16                    Relative risk (Wald CI) (straight, B vs A): 0.95\n95%CI [0.29 to 2.79]
         Freq
      1     4
      2     4
      3     4
      4     4
      5     4
      6     4
      7     4
      8     4
      9     4
      10    4
      11    4
      12    9
      13   29
      14    7
      15    2
      16    3

# Effects never fail 3

    Code
      print(.x)
    Output
      [1] "mpg"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'mpg' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "cyl"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      A problem occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      A problem occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred'. You might want to check for complete separation or extreme outliers.
      Could not calculate crosstable effects for variables 'mpg', 'disp', 'hp', 'drat', 'wt', 'qsec', 'gear', 'carb', 'hp_date', 'qsec_posix', 'dummy_num_vs', and 'diff'. Aren't there 2 groups exactly?
    Output
                                                                                                                                                                                             .
      1                                                                                                                            Error (glm: need at least two non-NA values to interpolate)
      2 Hazard ratio (Wald CI) (6 vs 4): 0.08\n95%CI [0.01 to 0.86]\nHazard ratio (Wald CI) (8 vs 4): 0.02\n95%CI [0.00 to 0.18]\nHazard ratio (Wald CI) (NA vs 4): 0.15\n95%CI [0.02 to 0.92]
      3                                                                                                                                                                             No effect?
      4                               Risk difference (Wald CI) (A, 6 minus 4): -1897.15\n95%CI [ NA to 35070.90]\nRisk difference (Wald CI) (A, 8 minus 4): -55.96\n95%CI [-229.52 to 109.91]
      5                            Risk difference (Wald CI) (auto, 6 minus 4): 194.59\n95%CI [-49.33 to 518.90]\nRisk difference (Wald CI) (auto, 8 minus 4): 255.20\n95%CI [67.85 to 484.13]
      6                Risk difference (Wald CI) (straight, 6 minus 4): -2251.20\n95%CI [ NA to 358040.74]\nRisk difference (Wald CI) (straight, 8 minus 4): -2251.20\n95%CI [ NA to 45969.57]
        Freq
      1    9
      2   29
      3   47
      4    2
      5    2
      6    3

---

    Code
      print(.x)
    Output
      [1] "disp"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'disp' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "hp"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'hp' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "drat"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'drat' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "wt"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'wt' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "qsec"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'qsec' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "vs"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      A problem occured when calculating crosstable effects (coxph): 'Loglik converged before variable 1,2 ; coefficient may be infinite. '.
      Could not calculate crosstable effects for variables 'cyl', and 'gear'. Aren't there 2 groups exactly?
    Output
                                                                                                                                                              .
      1                                                                                  Difference in medians (bootstrap CI) (): -1.37\n95%CI [-2.10 to -0.64]
      2                                                                            Difference in medians (bootstrap CI) (): -208.90\n95%CI [-290.67 to -127.13]
      3                                                                                  Difference in medians (bootstrap CI) (): -3.00\n95%CI [-4.10 to -1.90]
      4                                                                              Difference in medians (bootstrap CI) (): -87.00\n95%CI [-133.25 to -40.75]
      5                                                                              Difference in medians (bootstrap CI) (): -87.00\n95%CI [-133.41 to -40.59]
      6                                                                              Difference in medians (bootstrap CI) (): -87.61\n95%CI [-135.46 to -39.76]
      7                                                                                    Difference in medians (bootstrap CI) (): 0.28\n95%CI [-0.95 to 1.51]
      8                                                                                     Difference in medians (bootstrap CI) (): 0.85\n95%CI [0.28 to 1.42]
      9                                                                                     Difference in medians (bootstrap CI) (): 1.88\n95%CI [0.78 to 2.98]
      10                                                                                  Difference in medians (bootstrap CI) (): 11.80\n95%CI [4.63 to 18.97]
      11                                                                      Difference in medians (bootstrap CI) (): 162432.00\n95%CI [69357.24 to 255506.76]
      12                                                                                            Error (glm: need at least two non-NA values to interpolate)
      13 Hazard ratio (Wald CI) (straight vs NA): 3473769827.94\n95%CI [0.00 to Inf]\nHazard ratio (Wald CI) (vshaped vs NA): 395557782.33\n95%CI [0.00 to Inf]
      14                                                                                                                                             No effect?
      15                                                                Risk difference (Wald CI) (A, vshaped minus straight): -8.96\n95%CI [-179.73 to 158.35]
      16                                                          Risk difference (Wald CI) (FALSE, vshaped minus straight): -1808.12\n95%CI [ NA to 138809.92]
      17                                                             Risk difference (Wald CI) (auto, vshaped minus straight): 165.82\n95%CI [-10.51 to 379.13]
         Freq
      1     4
      2     4
      3     4
      4     4
      5     4
      6     4
      7     4
      8     4
      9     4
      10    4
      11    4
      12    6
      13   29
      14    7
      15    2
      16    3
      17    2

---

    Code
      print(.x)
    Output
      [1] "am"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      A problem occured when calculating crosstable effects (coxph): 'Loglik converged before variable 1 ; coefficient may be infinite. '.
      Could not calculate crosstable effects for variables 'cyl', and 'gear'. Aren't there 2 groups exactly?
    Output
                                                                                                 .
      1                     Difference in medians (bootstrap CI) (): -0.93\n95%CI [-1.25 to -0.61]
      2                     Difference in medians (bootstrap CI) (): -5.50\n95%CI [-11.48 to 0.48]
      3                       Difference in medians (bootstrap CI) (): 0.00\n95%CI [-0.29 to 0.29]
      4                       Difference in medians (bootstrap CI) (): 0.80\n95%CI [-1.03 to 2.63]
      5                       Difference in medians (bootstrap CI) (): 1.00\n95%CI [-1.23 to 3.23]
      6                        Difference in medians (bootstrap CI) (): 1.20\n95%CI [0.60 to 1.80]
      7                   Difference in medians (bootstrap CI) (): 155.50\n95%CI [82.39 to 228.61]
      8                    Difference in medians (bootstrap CI) (): 66.00\n95%CI [13.59 to 118.41]
      9                    Difference in medians (bootstrap CI) (): 66.00\n95%CI [15.75 to 116.25]
      10                   Difference in medians (bootstrap CI) (): 67.55\n95%CI [10.59 to 124.51]
      11         Difference in medians (bootstrap CI) (): 69120.00\n95%CI [-94341.63 to 232581.63]
      12                               Error (glm: need at least two non-NA values to interpolate)
      13               Hazard ratio (Wald CI) (manual vs auto): 3289345612.97\n95%CI [0.00 to Inf]
      14                                                                                No effect?
      15         Risk difference (Wald CI) (A, manual minus auto): 78.85\n95%CI [-63.28 to 228.68]
      16    Risk difference (Wald CI) (FALSE, manual minus auto): 65.68\n95%CI [-156.96 to 374.77]
      17 Risk difference (Wald CI) (straight, manual minus auto): 165.82\n95%CI [-10.51 to 379.13]
         Freq
      1     4
      2     4
      3     4
      4     4
      5     4
      6     4
      7     4
      8     4
      9     4
      10    4
      11    4
      12    6
      13   28
      14    7
      15    2
      16    3
      17    3

---

    Code
      print(.x)
    Output
      [1] "gear"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred', 'glm.fit: algorithm did not converge' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      A problem occured when calculating crosstable effects (coxph): 'Loglik converged before variable 1,2 ; coefficient may be infinite. '.
      Could not calculate crosstable effects for variables 'mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'carb', 'hp_date', 'qsec_posix', 'dummy_num_vs', and 'diff'. Aren't there 2 groups exactly?
    Output
                                                                                                                                                                          .
      1                                                                                                         Error (glm: need at least two non-NA values to interpolate)
      2                            Hazard ratio (Wald CI) (4 vs 3): 4429358637.03\n95%CI [0.00 to Inf]\nHazard ratio (Wald CI) (5 vs 3): 3158775523.94\n95%CI [0.00 to Inf]
      3                                                                                                                                                          No effect?
      4             Risk difference (Wald CI) (A, 4 minus 3): 47.00\n95%CI [-105.56 to 204.81]\nRisk difference (Wald CI) (A, 5 minus 3): -27.19\n95%CI [-250.74 to 178.57]
      5  Risk difference (Wald CI) (FALSE, 4 minus 3): -123.21\n95%CI [-440.79 to 127.75]\nRisk difference (Wald CI) (FALSE, 5 minus 3): -109.86\n95%CI [-449.00 to 227.74]
      6      Risk difference (Wald CI) (auto, 4 minus 3): -2125.92\n95%CI [ NA to 37117.49]\nRisk difference (Wald CI) (auto, 5 minus 3): -4113.21\n95%CI [ NA to 49159.34]
      7 Risk difference (Wald CI) (straight, 4 minus 3): 260.27\n95%CI [56.04 to 511.91]\nRisk difference (Wald CI) (straight, 5 minus 3): 11.78\n95%CI [-311.19 to 274.88]
        Freq
      1    6
      2   28
      3   48
      4    2
      5    3
      6    2
      7    3

---

    Code
      print(.x)
    Output
      [1] "carb"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'carb' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "cyl3"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Could not calculate crosstable effects for variables 'mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb', 'hp_date', 'qsec_posix', 'cyl6', 'dummy', 'dummy_na', 'dummy_num_vs', 'dummy2', 'test', and 'diff'. Aren't there 2 groups exactly?
    Output
                                                                       . Freq
      1 Hazard ratio (Wald CI) (NA vs FALSE): 1.54\n95%CI [0.42 to 5.63]   29
      2                                                       No effect?   65

---

    Code
      print(.x)
    Output
      [1] "cyl6"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      A problem occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      A problem occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred'. You might want to check for complete separation or extreme outliers.
      Could not calculate crosstable effects for variables 'cyl', and 'gear'. Aren't there 2 groups exactly?
    Output
                                                                                                                                            .
      1                                                                    Difference in medians (bootstrap CI) (): -0.00\n95%CI [ NA to  NA]
      2                                                                    Difference in medians (bootstrap CI) (): -0.08\n95%CI [ NA to  NA]
      3                                                                    Difference in medians (bootstrap CI) (): -1.00\n95%CI [ NA to  NA]
      4                                                                    Difference in medians (bootstrap CI) (): -1.35\n95%CI [ NA to  NA]
      5                                                                    Difference in medians (bootstrap CI) (): -2.00\n95%CI [ NA to  NA]
      6                                                                Difference in medians (bootstrap CI) (): -86400.00\n95%CI [ NA to  NA]
      7                                                                     Difference in medians (bootstrap CI) (): 0.00\n95%CI [ NA to  NA]
      8                                                                   Difference in medians (bootstrap CI) (): 108.20\n95%CI [ NA to  NA]
      9                                                                    Difference in medians (bootstrap CI) (): 27.00\n95%CI [ NA to  NA]
      10                                                                   Difference in medians (bootstrap CI) (): 28.73\n95%CI [ NA to  NA]
      11                                                                          Error (glm: need at least two non-NA values to interpolate)
      12 Hazard ratio (Wald CI) (NA vs FALSE): 1.45\n95%CI [0.39 to 5.40]\nHazard ratio (Wald CI) (TRUE vs FALSE): 0.63\n95%CI [0.08 to 5.04]
      13                                                                                                                           No effect?
      14                                                   Risk difference (Wald CI) (A, TRUE minus FALSE): -1865.31\n95%CI [ NA to 35368.83]
      15                                                 Risk difference (Wald CI) (auto, TRUE minus FALSE): 65.68\n95%CI [-156.96 to 374.77]
      16                                            Risk difference (Wald CI) (straight, TRUE minus FALSE): -1602.71\n95%CI [ NA to 47257.91]
         Freq
      1     4
      2     4
      3     4
      4     4
      5     4
      6     4
      7     4
      8     4
      9     8
      10    4
      11    6
      12   29
      13    7
      14    2
      15    2
      16    3

---

    Code
      print(.x)
    Output
      [1] "dummy"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Could not calculate crosstable effects for variables 'mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb', 'hp_date', 'qsec_posix', 'cyl3', 'cyl6', 'dummy_na', 'dummy_num_vs', 'dummy2', 'test', and 'diff'. Aren't there 2 groups exactly?
    Output
                 . Freq
      1  No Effect   28
      2 No effect?   66

---

    Code
      print(.x)
    Output
      [1] "dummy_num_vs"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Cannot cross columns 'test' (factor), 'vs' (character), 'dummy_na' (character), 'dummy2' (character), 'qsec_posix' (POSIXct, POSIXt), 'hp_date' (Date), 'am' (character), 'cyl3' (character), 'cyl' (factor), 'cyl6' (character), 'surv' (Surv), 'gear' (factor), and 'dummy' (character) by column 'dummy_num_vs' (numeric)
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(.x)
    Output
      [1] "dummy2"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      A problem occured when calculating crosstable effects (coxph): 'Loglik converged before variable 1 ; coefficient may be infinite. '.
      Could not calculate crosstable effects for variables 'mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb', 'hp_date', 'qsec_posix', 'cyl3', 'cyl6', 'dummy', 'dummy_na', 'dummy_num_vs', 'test', and 'diff'. Aren't there 2 groups exactly?
    Output
                                                                              . Freq
      1 Hazard ratio (Wald CI) (dummy vs NA): 312156724.32\n95%CI [0.00 to Inf]   29
      2                                                              No effect?   65

---

    Code
      print(.x)
    Output
      [1] "test"
    Code
      crosstable(mtcars3, by = any_of(.x), effect = T, effect_args = args)$effect %>%
        table %>% as.data.frame()
    Warning <simpleWarning>
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      A problem occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Problems occured when calculating crosstable effects (glm): 'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values'. You might want to check for complete separation or extreme outliers.
      An error occured when calculating crosstable effects (glm): 'need at least two non-NA values to interpolate'. You might want to check for complete separation or extreme outliers.
      Could not calculate crosstable effects for variables 'cyl', and 'gear'. Aren't there 2 groups exactly?
    Output
                                                                                          .
      1               Difference in medians (bootstrap CI) (): -0.42\n95%CI [-1.10 to 0.26]
      2               Difference in medians (bootstrap CI) (): -0.58\n95%CI [-2.17 to 1.00]
      3               Difference in medians (bootstrap CI) (): -1.00\n95%CI [-2.86 to 0.86]
      4           Difference in medians (bootstrap CI) (): -25.44\n95%CI [-102.31 to 51.43]
      5           Difference in medians (bootstrap CI) (): -26.50\n95%CI [-101.54 to 48.54]
      6            Difference in medians (bootstrap CI) (): -26.50\n95%CI [-98.18 to 45.18]
      7  Difference in medians (bootstrap CI) (): -50544.00\n95%CI [-188098.34 to 87010.34]
      8           Difference in medians (bootstrap CI) (): -81.50\n95%CI [-243.97 to 80.97]
      9                Difference in medians (bootstrap CI) (): 0.00\n95%CI [-0.32 to 0.32]
      10               Difference in medians (bootstrap CI) (): 0.20\n95%CI [-0.47 to 0.86]
      11               Difference in medians (bootstrap CI) (): 3.05\n95%CI [-1.15 to 7.25]
      12                        Error (glm: need at least two non-NA values to interpolate)
      13                        Hazard ratio (Wald CI) (B vs A): 0.63\n95%CI [0.20 to 1.94]
      14                                                                         No effect?
      15    Risk difference (Wald CI) (FALSE, B minus A): -1855.45\n95%CI [ NA to 34748.88]
      16       Risk difference (Wald CI) (auto, B minus A): 78.85\n95%CI [-63.28 to 228.68]
      17  Risk difference (Wald CI) (straight, B minus A): -8.96\n95%CI [-179.73 to 158.35]
         Freq
      1     4
      2     4
      3     4
      4     4
      5     4
      6     4
      7     4
      8     4
      9     4
      10    4
      11    4
      12    6
      13   29
      14    7
      15    3
      16    2
      17    3

