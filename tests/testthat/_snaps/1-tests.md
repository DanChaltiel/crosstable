# Testing everything

    Code
      x
    Output
      # A tibble: 17 x 9
         .id   label  variable  straight   vshaped   `NA`    Total   effect    test   
         <chr> <chr>  <chr>     <chr>      <chr>     <chr>   <chr>   <chr>     <chr>  
       1 disp  Displ~ Min / Max 71.100000~ 120.3000~ 140.80~ 71.100~ "Differe~ "p val~
       2 disp  Displ~ Med [IQR] 95.100000~ 304.0000~ 196.30~ 196.30~ "Differe~ "p val~
       3 disp  Displ~ Mean (st~ 111.85555~ 302.1933~ 230.43~ 230.72~ "Differe~ "p val~
       4 disp  Displ~ N (NA)    9 (0)      15 (0)    8 (0)   32 (0)  "Differe~ "p val~
       5 hp    Gross~ Min / Max 52.000000~ 91.00000~ 62.000~ 52.000~ "Differe~ "p val~
       6 hp    Gross~ Med [IQR] 93.000000~ 180.0000~ 123.00~ 123.00~ "Differe~ "p val~
       7 hp    Gross~ Mean (st~ 85.666666~ 187.6666~ 138.50~ 146.68~ "Differe~ "p val~
       8 hp    Gross~ N (NA)    9 (0)      15 (0)    8 (0)   32 (0)  "Differe~ "p val~
       9 am    Trans~ auto      2 (8.33% ~ 9 (37.50~ 8       19 (59~ "Odds ra~ "p val~
      10 am    Trans~ manual    7 (29.17%~ 6 (25.00~ 0       13 (40~ "Odds ra~ "p val~
      11 am    Trans~ Total     9 (37.50%) 15 (62.5~ 8       32 (10~ "Odds ra~ "p val~
      12 surv  Dummy~ t=70      1.00 (0/9) 1.00 (0/~ 1.00 (~ 1.00 (~ "Hazard ~ "p val~
      13 surv  Dummy~ t=100     0.44 (5/4) 1.00 (0/~ 1.00 (~ 0.84 (~ "Hazard ~ "p val~
      14 surv  Dummy~ t=200     0.17 (2/1) 0.73 (4/~ 1.00 (~ 0.64 (~ "Hazard ~ "p val~
      15 surv  Dummy~ t=400     0.17 (0/0) 0.52 (2/~ 1.00 (~ 0.50 (~ "Hazard ~ "p val~
      16 surv  Dummy~ Median f~ 258 [120.~ 400 [275~ 196.3 ~ 304 [1~ "Hazard ~ "p val~
      17 surv  Dummy~ Median s~ 95.1       <NA>      <NA>    <NA>    "Hazard ~ "p val~

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
      1 Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190.34 [119.89 to 260.78]
      2 Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190.34 [119.89 to 260.78]
      3 Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190.34 [119.89 to 260.78]
      4 Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190.34 [119.89 to 260.78]
      5      Difference in means (Welch CI), ref='straight'\nvshaped minus straight: 102.00 [63.49 to 140.51]
                                                test
      1   p value: 0.0002 \n(Wilcoxon rank sum test)
      2   p value: 0.0002 \n(Wilcoxon rank sum test)
      3   p value: 0.0002 \n(Wilcoxon rank sum test)
      4   p value: 0.0002 \n(Wilcoxon rank sum test)
      5 p value: <0.0001 \n(Welch Two Sample t-test)

