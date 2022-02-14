# Testing everything

    Code
      as.data.frame(x)
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
      12 surv Dummy survival (disp/am)                         t=70
      13 surv Dummy survival (disp/am)                        t=100
      14 surv Dummy survival (disp/am)                        t=200
      15 surv Dummy survival (disp/am)                        t=400
      16 surv Dummy survival (disp/am) Median follow up [min ; max]
      17 surv Dummy survival (disp/am)              Median survival
                    straight             vshaped                NA             Total
      1             71 / 258           120 / 472         141 / 360          71 / 472
      2          95 [79;120]       304 [218;376]     196 [162;297]     196 [121;326]
      3             112 (58)           302 (116)          230 (91)         231 (124)
      4                9 (0)              15 (0)             8 (0)            32 (0)
      5             52 / 113            91 / 335          62 / 245          52 / 335
      6          93 [66;109]       180 [150;222]     123 [102;176]      123 [96;180]
      7              86 (23)            188 (65)          138 (58)          147 (69)
      8                9 (0)              15 (0)             8 (0)            32 (0)
      9   2 (8% / 18% / 22%) 9 (38% / 82% / 60%)                 8          19 (59%)
      10 7 (29% / 54% / 78%) 6 (25% / 46% / 40%)                 0          13 (41%)
      11             9 (38%)            15 (62%)                 8         32 (100%)
      12             1 (0/9)            1 (0/15)           1 (0/8)          1 (0/32)
      13           0.4 (5/4)            1 (0/15)           1 (0/8)          1 (5/27)
      14           0.2 (2/1)            1 (4/11)           1 (0/4)          1 (6/16)
      15           0.2 (0/0)             1 (2/4)           1 (0/0)           1 (2/4)
      16   258 [120.1 ; 258]   400 [275.8 ; 472] 196 [140.8 ; 360] 304 [120.1 ; 472]
      17                95.1                <NA>              <NA>              <NA>
                                                                                               effect
      1  Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190 [120 to 261]
      2  Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190 [120 to 261]
      3  Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190 [120 to 261]
      4  Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190 [120 to 261]
      5       Difference in means (Welch CI), ref='straight'\nvshaped minus straight: 102 [63 to 141]
      6       Difference in means (Welch CI), ref='straight'\nvshaped minus straight: 102 [63 to 141]
      7       Difference in means (Welch CI), ref='straight'\nvshaped minus straight: 102 [63 to 141]
      8       Difference in means (Welch CI), ref='straight'\nvshaped minus straight: 102 [63 to 141]
      9      Odds ratio [95% Wald CI], ref='vshaped vs straight'\nmanual vs auto: 0.19 [0.02 to 1.11]
      10     Odds ratio [95% Wald CI], ref='vshaped vs straight'\nmanual vs auto: 0.19 [0.02 to 1.11]
      11     Odds ratio [95% Wald CI], ref='vshaped vs straight'\nmanual vs auto: 0.19 [0.02 to 1.11]
      12                             Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      13                             Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      14                             Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      15                             Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      16                             Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      17                             Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
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
      12                       p value: 0.0002 \n(Logrank test)
      13                       p value: 0.0002 \n(Logrank test)
      14                       p value: 0.0002 \n(Logrank test)
      15                       p value: 0.0002 \n(Logrank test)
      16                       p value: 0.0002 \n(Logrank test)
      17                       p value: 0.0002 \n(Logrank test)

---

    Code
      ft
    Output
      a flextable object.
      col_keys: `label`, `variable`, `straight`, `vshaped`, `NA`, `Total`, `effect`, `test` 
      header has 2 row(s) 
      body has 17 row(s) 
      original dataset sample: 
         .id                 label   variable    straight       vshaped            NA
      1 disp Displacement (cu.in.)  Min / Max    71 / 258     120 / 472     141 / 360
      2 disp Displacement (cu.in.)  Med [IQR] 95 [79;120] 304 [218;376] 196 [162;297]
      3 disp Displacement (cu.in.) Mean (std)    112 (58)     302 (116)      230 (91)
      4 disp Displacement (cu.in.)     N (NA)       9 (0)        15 (0)         8 (0)
      5   hp      Gross horsepower  Min / Max    52 / 113      91 / 335      62 / 245
                Total
      1      71 / 472
      2 196 [121;326]
      3     231 (124)
      4        32 (0)
      5      52 / 335
                                                                                              effect
      1 Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190 [120 to 261]
      2 Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190 [120 to 261]
      3 Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190 [120 to 261]
      4 Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190 [120 to 261]
      5      Difference in means (Welch CI), ref='straight'\nvshaped minus straight: 102 [63 to 141]
                                                test
      1   p value: 0.0002 \n(Wilcoxon rank sum test)
      2   p value: 0.0002 \n(Wilcoxon rank sum test)
      3   p value: 0.0002 \n(Wilcoxon rank sum test)
      4   p value: 0.0002 \n(Wilcoxon rank sum test)
      5 p value: <0.0001 \n(Welch Two Sample t-test)

