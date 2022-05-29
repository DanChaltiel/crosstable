# Effects never fail 1

    Code
      print(glue("Effect part 1 - by={.x}"))
    Output
      Effect part 1 - by=model
    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Cannot calculate crosstable effects as there is not exactly 2 groups in `by`.
      i `by` has 32 levels
      Warning:
      Unknown or uninitialised column: `effect`.
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(glue("Effect part 1 - by={.x}"))
    Output
      Effect part 1 - by=cyl
    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Cannot calculate crosstable effects as there is not exactly 2 groups in `by`.
      i `by` has 3 levels
      Warning:
      Unknown or uninitialised column: `effect`.
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(glue("Effect part 1 - by={.x}"))
    Output
      Effect part 1 - by=vs
    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
        'glm.fit: fitted probabilities numerically 0 or 1 occurred' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Problems occured when calculating crosstable effects (glm-logit):
        'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables 'cyl3', 'dummy', 'dummy_na', 'dummy_na2', and 'dummy2'
    Output
                                                                                                                                     .
      1                               Difference in means (Welch CI), ref='straight'\nvshaped minus straight: 102.00 [63.49 to 140.51]
      2                               Difference in means (Welch CI), ref='straight'\nvshaped minus straight: 104.25 [65.37 to 143.12]
      3                               Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 0.28 [-0.54 to 1.11]
      4                          Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190.34 [119.89 to 260.78]
      5                                Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2.40 [1.60 to 3.20]
      6                                Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -0.54 [-0.98 to -0.10]
      7                              Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -10.19 [-14.16 to -6.21]
      8                 Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -194035.20 [-276311.92 to -111758.48]
      9                                Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -2.25 [-3.20 to -1.29]
      10                                  Difference in means (t-test CI), ref='straight'\nvshaped minus straight: 1.48 [0.73 to 2.23]
      11                                                              Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      12                                                                                                                    No effect?
      13                Odds ratio [95% Wald CI], ref='vshaped vs straight'\n4 vs 3: 0.07 [0.01 to 0.57]\n5 vs 3: 0.89 [0.06 to 22.46]
      14 Odds ratio [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5981748110.20 [0.00 to  NA]\n8 vs 4: 5981747998.47 [0.00 to  NA]
      15                                              Odds ratio [95% Wald CI], ref='vshaped vs straight'\nB vs A: 1.09 [0.21 to 6.03]
      16                                  Odds ratio [95% Wald CI], ref='vshaped vs straight'\nTRUE vs FALSE: 9129960.46 [0.00 to  NA]
      17                                      Odds ratio [95% Wald CI], ref='vshaped vs straight'\nmanual vs auto: 0.19 [0.02 to 1.11]
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
      12    7
      13    3
      14    4
      15    2
      16    3
      17    2

---

    Code
      print(glue("Effect part 1 - by={.x}"))
    Output
      Effect part 1 - by=gear
    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Cannot calculate crosstable effects as there is not exactly 2 groups in `by`.
      i `by` has 3 levels
      Warning:
      Unknown or uninitialised column: `effect`.
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(glue("Effect part 1 - by={.x}"))
    Output
      Effect part 1 - by=cyl6
    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Problems occured when calculating crosstable effects (glm-logit):
        'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      An *error* occured when calculating crosstable effects (glm-logit):
        'need at least two non-NA values to interpolate' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
        'glm.fit: fitted probabilities numerically 0 or 1 occurred' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
        'glm.fit: fitted probabilities numerically 0 or 1 occurred' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables 'cyl3', 'dummy', 'dummy_na', 'dummy_na2', and 'dummy2'
    Output
                                                                                                                             .
      1                                   Difference in means (Welch CI), ref='FALSE'\nTRUE minus FALSE: -1.44 [-4.62 to 1.74]
      2                           Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -68.57 [-131.13 to -6.00]
      3                                Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0.00 [-0.56 to 0.56]
      4                                Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0.07 [-0.23 to 0.37]
      5                                Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 1.01 [-1.07 to 3.09]
      6                                  Difference in means (t-test CI), ref='FALSE'\nTRUE minus FALSE: -0.05 [-0.69 to 0.58]
      7                              Difference in means (t-test CI), ref='FALSE'\nTRUE minus FALSE: -23.72 [-105.83 to 58.40]
      8                              Difference in means (t-test CI), ref='FALSE'\nTRUE minus FALSE: -24.13 [-107.85 to 59.58]
      9                                   Difference in means (t-test CI), ref='FALSE'\nTRUE minus FALSE: 0.42 [-1.72 to 2.56]
      10                    Difference in means (t-test CI), ref='FALSE'\nTRUE minus FALSE: 36062.61 [-148825.01 to 220950.23]
      11                                                            Hazard ratio (Wald CI)\nTRUE vs FALSE: 0.66 [0.08 to 5.41]
      12                                                                                                            No effect?
      13             Odds ratio [95% Wald CI], ref='TRUE vs FALSE'\n4 vs 3: 3.43 [0.28 to 82.10]\n5 vs 3: 3.00 [0.10 to 89.12]
      14 Odds ratio [95% Wald CI], ref='TRUE vs FALSE'\n6 vs 4: 16084415462386691998446.00 [CI error]\n8 vs 4: 1.00 [CI error]
      15                                     Odds ratio [95% Wald CI], ref='TRUE vs FALSE'\nB vs A: 114314914.20 [0.00 to  NA]
      16                                    Odds ratio [95% Wald CI], ref='TRUE vs FALSE'\nmanual vs auto: 0.52 [0.02 to 4.80]
      17                         Odds ratio [95% Wald CI], ref='TRUE vs FALSE'\nvshaped vs straight: 71211286.19 [0.00 to  NA]
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
      12    7
      13    3
      14    4
      15    2
      16    2
      17    3

---

    Code
      print(glue("Effect part 1 - by={.x}"))
    Output
      Effect part 1 - by=dummy
    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Cannot calculate crosstable effects as there is not exactly 2 groups in `by`.
      i `by` has 1 levels
      Warning:
      Unknown or uninitialised column: `effect`.
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

# Effects never fail 2

    Code
      print(glue("Effect part 2 - by={.x}"))
    Output
      Effect part 2 - by=model
    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Cannot calculate crosstable effects as there is not exactly 2 groups in `by`.
      i `by` has 32 levels
      Warning:
      Unknown or uninitialised column: `effect`.
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(glue("Effect part 2 - by={.x}"))
    Output
      Effect part 2 - by=cyl
    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Cannot calculate crosstable effects as there is not exactly 2 groups in `by`.
      i `by` has 3 levels
      Warning:
      Unknown or uninitialised column: `effect`.
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(glue("Effect part 2 - by={.x}"))
    Output
      Effect part 2 - by=vs
    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      An *error* occured when calculating crosstable effects (glm-log):
        'no valid set of coefficients has been found: please supply starting values' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      An *error* occured when calculating crosstable effects (glm-log):
        'no valid set of coefficients has been found: please supply starting values' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      An *error* occured when calculating crosstable effects (glm-log):
        'no valid set of coefficients has been found: please supply starting values' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      An *error* occured when calculating crosstable effects (glm-log):
        'no valid set of coefficients has been found: please supply starting values' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables 'cyl3', 'dummy', 'dummy_na', 'dummy_na2', and 'dummy2'
    Output
                                                                                                                         .
      1                 Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: -0.54 [-0.97 to -0.11]
      2               Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: -10.19 [-14.00 to -6.38]
      3  Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: -194035.20 [-269622.12 to -118448.28]
      4                 Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: -2.25 [-3.11 to -1.38]
      5                   Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 0.28 [-0.59 to 1.16]
      6                    Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 1.48 [0.85 to 2.11]
      7               Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 102.00 [66.41 to 137.59]
      8               Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 102.00 [66.61 to 137.39]
      9               Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 104.25 [66.98 to 141.51]
      10             Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190.34 [120.13 to 260.55]
      11                   Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2.40 [1.53 to 3.27]
      12                                                  Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      13                                                                                                        No effect?
      14          Relative risk [95% Wald CI], ref='vshaped vs straight'\n4 vs 3: 0.31 [CI error]\n5 vs 3: 0.98 [CI error]
      15        Relative risk [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: error [CI error]\n8 vs 4: error [CI error]
      16                               Relative risk [95% Wald CI], ref='vshaped vs straight'\nB vs A: 1.03 [0.51 to 2.02]
      17                           Relative risk [95% Wald CI], ref='vshaped vs straight'\nTRUE vs FALSE: error [CI error]
      18                           Relative risk [95% Wald CI], ref='vshaped vs straight'\nmanual vs auto: 0.56 [CI error]
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
      12   29
      13    7
      14    3
      15    4
      16    2
      17    3
      18    2

---

    Code
      print(glue("Effect part 2 - by={.x}"))
    Output
      Effect part 2 - by=gear
    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Cannot calculate crosstable effects as there is not exactly 2 groups in `by`.
      i `by` has 3 levels
      Warning:
      Unknown or uninitialised column: `effect`.
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(glue("Effect part 2 - by={.x}"))
    Output
      Effect part 2 - by=cyl6
    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      An *error* occured when calculating crosstable effects (glm-log):
        'no valid set of coefficients has been found: please supply starting values' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      A problem occured when calculating crosstable effects (glm-log):
        'glm.fit: fitted probabilities numerically 0 or 1 occurred' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      An *error* occured when calculating crosstable effects (glm-log):
        'no valid set of coefficients has been found: please supply starting values' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      A problem occured when calculating crosstable effects (glm-log):
        'glm.fit: fitted probabilities numerically 0 or 1 occurred' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      An *error* occured when calculating crosstable effects (glm-log):
        'no valid set of coefficients has been found: please supply starting values' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables 'cyl3', 'dummy', 'dummy_na', 'dummy_na2', and 'dummy2'
    Output
                                                                                                                    .
      1                      Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -0.05 [-0.64 to 0.53]
      2                      Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -1.44 [-4.24 to 1.36]
      3                   Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -23.72 [-66.51 to 19.08]
      4                   Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -23.72 [-67.41 to 19.98]
      5                   Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -24.13 [-68.71 to 20.44]
      6                  Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -68.57 [-132.74 to -4.39]
      7                       Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0.00 [-0.54 to 0.55]
      8                       Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0.07 [-0.23 to 0.37]
      9                       Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0.42 [-1.62 to 2.45]
      10                      Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 1.01 [-1.01 to 3.03]
      11        Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 36062.61 [-141723.89 to 213849.10]
      12                                                   Hazard ratio (Wald CI)\nTRUE vs FALSE: 0.66 [0.08 to 5.41]
      13                                                                                                   No effect?
      14 Relative risk [95% Wald CI], ref='TRUE vs FALSE'\n4 vs 3: 2.89 [0.32 to 56.97]\n5 vs 3: 2.60 [0.12 to 58.04]
      15         Relative risk [95% Wald CI], ref='TRUE vs FALSE'\n6 vs 4: error [CI error]\n8 vs 4: error [CI error]
      16                             Relative risk [95% Wald CI], ref='TRUE vs FALSE'\nB vs A: 70037300.41 [CI error]
      17                        Relative risk [95% Wald CI], ref='TRUE vs FALSE'\nmanual vs auto: 0.57 [0.03 to 3.80]
      18                Relative risk [95% Wald CI], ref='TRUE vs FALSE'\nvshaped vs straight: 54917631.04 [CI error]
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
      12   29
      13    7
      14    3
      15    4
      16    2
      17    2
      18    3

---

    Code
      print(glue("Effect part 2 - by={.x}"))
    Output
      Effect part 2 - by=dummy
    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Cannot calculate crosstable effects as there is not exactly 2 groups in `by`.
      i `by` has 1 levels
      Warning:
      Unknown or uninitialised column: `effect`.
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

# Effects never fail 3

    Code
      print(glue("Effect part 3 - by={.x}"))
    Output
      Effect part 3 - by=model
    Code
      set.seed(1234)
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Cannot calculate crosstable effects as there is not exactly 2 groups in `by`.
      i `by` has 32 levels
      Warning:
      Unknown or uninitialised column: `effect`.
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(glue("Effect part 3 - by={.x}"))
    Output
      Effect part 3 - by=cyl
    Code
      set.seed(1234)
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Cannot calculate crosstable effects as there is not exactly 2 groups in `by`.
      i `by` has 3 levels
      Warning:
      Unknown or uninitialised column: `effect`.
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(glue("Effect part 3 - by={.x}"))
    Output
      Effect part 3 - by=vs
    Code
      set.seed(1234)
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
        'glm.fit: fitted probabilities numerically 0 or 1 occurred' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Problems occured when calculating crosstable effects (glm-logit):
        'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables 'cyl3', 'dummy', 'dummy_na', 'dummy_na2', and 'dummy2'
    Output
                                                                                                                                 .
      1                        Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: -0.28 [-1.08 to 1.56]
      2                       Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: -0.85 [-1.07 to -0.12]
      3                       Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: -1.88 [-3.26 to -1.25]
      4                     Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: -11.80 [-15.50 to -3.32]
      5        Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: -162432.00 [-290552.40 to -101044.80]
      6                          Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: 1.37 [0.71 to 2.03]
      7                     Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: 208.90 [94.75 to 286.04]
      8                          Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: 3.00 [1.00 to 3.00]
      9                      Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: 87.00 [53.00 to 144.00]
      10                     Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: 87.00 [53.00 to 144.26]
      11                     Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: 87.61 [49.85 to 150.08]
      12                                                          Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      13                                                                                                                No effect?
      14   Risk difference [95% Wald CI], ref='vshaped vs straight'\n4 vs 3: -2.60 [-5.12 to -0.56]\n5 vs 3: -0.12 [-2.75 to 3.11]
      15 Risk difference [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: 22.51 [-3580.41 to  NA]\n8 vs 4: 22.51 [-459.70 to  NA]
      16                                    Risk difference [95% Wald CI], ref='vshaped vs straight'\nB vs A: 0.09 [-1.58 to 1.80]
      17                           Risk difference [95% Wald CI], ref='vshaped vs straight'\nTRUE vs FALSE: 16.03 [-472.58 to  NA]
      18                           Risk difference [95% Wald CI], ref='vshaped vs straight'\nmanual vs auto: -1.66 [-3.79 to 0.11]
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
      12   29
      13    7
      14    3
      15    4
      16    2
      17    3
      18    2

---

    Code
      print(glue("Effect part 3 - by={.x}"))
    Output
      Effect part 3 - by=gear
    Code
      set.seed(1234)
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Cannot calculate crosstable effects as there is not exactly 2 groups in `by`.
      i `by` has 3 levels
      Warning:
      Unknown or uninitialised column: `effect`.
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

---

    Code
      print(glue("Effect part 3 - by={.x}"))
    Output
      Effect part 3 - by=cyl6
    Code
      set.seed(1234)
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Problems occured when calculating crosstable effects (glm-logit):
        'glm.fit: fitted probabilities numerically 0 or 1 occurred' and 'collapsing to unique 'x' values' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      An *error* occured when calculating crosstable effects (glm-logit):
        'need at least two non-NA values to interpolate' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
        'glm.fit: fitted probabilities numerically 0 or 1 occurred' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
        'glm.fit: fitted probabilities numerically 0 or 1 occurred' 
      You might want to check for complete separation or extreme outliers. 
      Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables 'cyl3', 'dummy', 'dummy_na', 'dummy_na2', and 'dummy2'
    Output
                                                                                                                      .
      1                 Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -108.20 [-152.71 to 77.61]
      2                   Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -27.00 [-75.00 to 40.00]
      3                   Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -27.00 [-75.00 to 46.70]
      4                   Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -28.73 [-74.43 to 51.23]
      5                       Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0.00 [-0.77 to 0.67]
      6                        Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0.00 [0.00 to 0.00]
      7                       Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0.08 [-0.96 to 0.74]
      8                       Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 1.00 [-2.32 to 2.80]
      9                       Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 1.35 [-4.80 to 3.98]
      10                      Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 2.00 [-1.00 to 4.00]
      11        Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 86400.00 [-214272.00 to 243648.00]
      12                                                     Hazard ratio (Wald CI)\nTRUE vs FALSE: 0.66 [0.08 to 5.41]
      13                                                                                                     No effect?
      14 Risk difference [95% Wald CI], ref='TRUE vs FALSE'\n4 vs 3: 1.23 [-1.28 to 4.41]\n5 vs 3: 1.10 [-2.28 to 4.49]
      15         Risk difference [95% Wald CI], ref='TRUE vs FALSE'\n6 vs 4: 51.13 [CI error]\n8 vs 4: -0.00 [CI error]
      16                             Risk difference [95% Wald CI], ref='TRUE vs FALSE'\nB vs A: 18.55 [-347.49 to  NA]
      17                      Risk difference [95% Wald CI], ref='TRUE vs FALSE'\nmanual vs auto: -0.66 [-3.75 to 1.57]
      18               Risk difference [95% Wald CI], ref='TRUE vs FALSE'\nvshaped vs straight: 18.08 [-1388.10 to  NA]
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
      12   29
      13    7
      14    3
      15    4
      16    2
      17    2
      18    3

---

    Code
      print(glue("Effect part 3 - by={.x}"))
    Output
      Effect part 3 - by=dummy
    Code
      set.seed(1234)
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = args)$effect %>% table %>%
        as.data.frame()
    Condition
      Warning:
      Cannot calculate crosstable effects as there is not exactly 2 groups in `by`.
      i `by` has 1 levels
      Warning:
      Unknown or uninitialised column: `effect`.
    Output
      [1] Freq
      <0 rows> (or 0-length row.names)

