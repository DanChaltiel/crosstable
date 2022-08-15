# Effect - .x='vs' - mean/OR (default)

    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = e_args) %>% select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
    Condition
      Warning in `crosstable()`:
      Cannot describe columns `dummy_na` and `dummy_na2` as they contain only missing values.
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Problems occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred" and "collapsing to unique 'x' values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables `cyl3`, `dummy`, and `dummy2`
    Output
                  .id                                                                                                              effect
      1           mpg                   Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -10.19 [-14.16 to -6.21]
      2           cyl Odds ratio [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: 5.98e+09 [0 to NA]\n8 vs 4: 5.98e+09 [2.27e-200 to NA]
      3          disp               Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190.34 [119.89 to 260.78]
      4            hp                    Difference in means (Welch CI), ref='straight'\nvshaped minus straight: 102.00 [63.49 to 140.51]
      5          drat                     Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -0.54 [-0.98 to -0.10]
      6            wt                        Difference in means (t-test CI), ref='straight'\nvshaped minus straight: 1.48 [0.73 to 2.23]
      7          qsec                     Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -2.25 [-3.20 to -1.29]
      8            am                            Odds ratio [95% Wald CI], ref='vshaped vs straight'\nmanual vs auto: 0.19 [0.02 to 1.11]
      9          gear      Odds ratio [95% Wald CI], ref='vshaped vs straight'\n4 vs 3: 0.07 [0.01 to 0.57]\n5 vs 3: 0.89 [0.06 to 22.46]
      10         carb                     Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2.40 [1.60 to 3.20]
      11      hp_date                    Difference in means (Welch CI), ref='straight'\nvshaped minus straight: 102.00 [63.49 to 140.51]
      12   qsec_posix         Difference in means (t-test CI), ref='straight'\nvshaped minus straight: -1.94e+05 [-2.76e+05 to -1.12e+05]
      13         cyl3                                                                                                          No effect?
      14         cyl6                      Odds ratio [95% Wald CI], ref='vshaped vs straight'\nTRUE vs FALSE: 9.13e+06 [5.77e-206 to NA]
      15        dummy                                                                                                          No effect?
      16 dummy_num_vs                    Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 0.28 [-0.54 to 1.11]
      17       dummy2                                                                                                          No effect?
      18         test                                    Odds ratio [95% Wald CI], ref='vshaped vs straight'\nB vs A: 1.09 [0.21 to 6.03]
      19         surv                                                    Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      20         diff                    Difference in means (Welch CI), ref='straight'\nvshaped minus straight: 104.25 [65.37 to 143.12]

# Effect - .x='vs' - mean_boot/RR

    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = e_args) %>% select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
    Condition
      Warning in `crosstable()`:
      Cannot describe columns `dummy_na` and `dummy_na2` as they contain only missing values.
      Warning:
      An error occured when calculating crosstable effects (glm-log):
      i "no valid set of coefficients has been found: please supply starting values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
    Message
      Error (glm-log: "no valid set of coefficients has been found: please supply starting values")
    Condition
      Warning:
      An error occured when calculating crosstable effects (glm-log):
      i "no valid set of coefficients has been found: please supply starting values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
    Message
      Error (glm-log: "no valid set of coefficients has been found: please supply starting values")
    Condition
      Warning:
      An error occured when calculating crosstable effects (glm-log):
      i "no valid set of coefficients has been found: please supply starting values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
    Message
      Error (glm-log: "no valid set of coefficients has been found: please supply starting values")
    Condition
      Warning:
      An error occured when calculating crosstable effects (glm-log):
      i "no valid set of coefficients has been found: please supply starting values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
    Message
      Error (glm-log: "no valid set of coefficients has been found: please supply starting values")
    Condition
      Warning:
      Cannot calculate crosstable effects for variables `cyl3`, `dummy`, and `dummy2`
    Output
                  .id                                                                                                         effect
      1           mpg           Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: -10.19 [-14.12 to -6.26]
      2           cyl     Relative risk [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: error [CI error]\n8 vs 4: error [CI error]
      3          disp          Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 190.34 [121.16 to 259.52]
      4            hp           Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 102.00 [67.45 to 136.55]
      5          drat             Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: -0.54 [-0.92 to -0.15]
      6            wt                Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 1.48 [0.86 to 2.10]
      7          qsec             Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: -2.25 [-3.12 to -1.37]
      8            am                        Relative risk [95% Wald CI], ref='vshaped vs straight'\nmanual vs auto: 0.56 [CI error]
      9          gear       Relative risk [95% Wald CI], ref='vshaped vs straight'\n4 vs 3: 0.31 [CI error]\n5 vs 3: 0.98 [CI error]
      10         carb                Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 2.40 [1.55 to 3.25]
      11      hp_date           Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 102.00 [65.12 to 138.88]
      12   qsec_posix Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: -1.94e+05 [-2.72e+05 to -1.17e+05]
      13         cyl3                                                                                                     No effect?
      14         cyl6                        Relative risk [95% Wald CI], ref='vshaped vs straight'\nTRUE vs FALSE: error [CI error]
      15        dummy                                                                                                     No effect?
      16 dummy_num_vs               Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 0.28 [-0.56 to 1.13]
      17       dummy2                                                                                                     No effect?
      18         test                            Relative risk [95% Wald CI], ref='vshaped vs straight'\nB vs A: 1.03 [0.51 to 2.02]
      19         surv                                               Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      20         diff           Difference in means (bootstrap CI), ref='straight'\nvshaped minus straight: 104.25 [67.74 to 140.76]

# Effect - .x='vs' - median/RD

    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = e_args) %>% select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
    Condition
      Warning in `crosstable()`:
      Cannot describe columns `dummy_na` and `dummy_na2` as they contain only missing values.
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Problems occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred" and "collapsing to unique 'x' values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables `cyl3`, `dummy`, and `dummy2`
    Output
                  .id                                                                                                                  effect
      1           mpg                  Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: -11.80 [-15.98 to -3.95]
      2           cyl Risk difference [95% Wald CI], ref='vshaped vs straight'\n6 vs 4: 22.51 [-3580.41 to NA]\n8 vs 4: 22.51 [-459.70 to NA]
      3          disp                 Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: 208.90 [144.62 to 296.40]
      4            hp                   Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: 87.00 [53.00 to 146.62]
      5          drat                    Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: -0.85 [-1.06 to -0.08]
      6            wt                       Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: 1.37 [0.62 to 2.12]
      7          qsec                    Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: -1.88 [-3.18 to -1.16]
      8            am                         Risk difference [95% Wald CI], ref='vshaped vs straight'\nmanual vs auto: -1.66 [-3.79 to 0.11]
      9          gear Risk difference [95% Wald CI], ref='vshaped vs straight'\n4 vs 3: -2.60 [-5.12 to -0.56]\n5 vs 3: -0.12 [-2.75 to 3.11]
      10         carb                       Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: 3.00 [1.00 to 3.00]
      11      hp_date                   Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: 87.00 [55.00 to 144.00]
      12   qsec_posix        Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: -1.62e+05 [-2.69e+05 to -1.05e+05]
      13         cyl3                                                                                                              No effect?
      14         cyl6                          Risk difference [95% Wald CI], ref='vshaped vs straight'\nTRUE vs FALSE: 16.03 [-472.58 to NA]
      15        dummy                                                                                                              No effect?
      16 dummy_num_vs                     Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: -0.28 [-0.80 to 1.97]
      17       dummy2                                                                                                              No effect?
      18         test                                  Risk difference [95% Wald CI], ref='vshaped vs straight'\nB vs A: 0.09 [-1.58 to 1.80]
      19         surv                                                        Hazard ratio (Wald CI)\nvshaped vs straight: 0.11 [0.03 to 0.42]
      20         diff                   Difference in medians (bootstrap CI), ref='straight'\nvshaped minus straight: 87.61 [55.47 to 150.38]

# Effect - .x='am' - mean/OR (default)

    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = e_args) %>% select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
    Condition
      Warning in `crosstable()`:
      Cannot describe columns `dummy_na` and `dummy_na2` as they contain only missing values.
      Warning:
      Problems occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred", "glm.fit: algorithm did not converge", and "collapsing to unique 'x' values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      A problem occured when calculating crosstable effects (coxph):
      i "Loglik converged before variable 1 ; coefficient may be infinite. "
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables `cyl3`, `dummy`, and `dummy2`
    Output
                  .id                                                                                                                 effect
      1           mpg                                   Difference in means (t-test CI), ref='auto'\nmanual minus auto: 7.24 [3.64 to 10.85]
      2           cyl               Odds ratio [95% Wald CI], ref='manual vs auto'\n6 vs 4: 0.14 [0.01 to 1.64]\n8 vs 4: 0.08 [0.01 to 0.51]
      3          disp                         Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: -146.85 [-214.41 to -79.28]
      4            hp                            Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: -33.42 [-83.11 to 16.28]
      5          drat                                 Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: 0.76 [0.51 to 1.02]
      6            wt                              Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: -1.36 [-1.84 to -0.88]
      7          qsec                                  Difference in means (t-test CI), ref='auto'\nmanual minus auto: -0.82 [-2.12 to 0.48]
      8            vs                               Odds ratio [95% Wald CI], ref='manual vs auto'\nvshaped vs straight: 0.19 [0.02 to 1.11]
      9          gear Odds ratio [95% Wald CI], ref='manual vs auto'\n4 vs 3: 1.71e+09 [6.32e-162 to NA]\n5 vs 3: 7.30e+17 [3.18e-214 to NA]
      10         carb                                Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: 0.19 [-1.09 to 1.46]
      11      hp_date                            Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: -33.42 [-85.88 to 19.05]
      12   qsec_posix                      Difference in means (t-test CI), ref='auto'\nmanual minus auto: -7.11e+04 [-1.83e+05 to 4.12e+04]
      13         cyl3                                                                                                             No effect?
      14         cyl6                                     Odds ratio [95% Wald CI], ref='manual vs auto'\nTRUE vs FALSE: 0.52 [0.02 to 4.80]
      15        dummy                                                                                                             No effect?
      16 dummy_num_vs                                Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: 0.11 [-0.48 to 0.71]
      17       dummy2                                                                                                             No effect?
      18         test                                            Odds ratio [95% Wald CI], ref='manual vs auto'\nB vs A: 0.45 [0.10 to 1.88]
      19         surv                                                            Hazard ratio (Wald CI)\nmanual vs auto: 3.29e+09 [0 to Inf]
      20         diff                            Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: -32.59 [-84.52 to 19.33]

# Effect - .x='am' - mean_boot/RR

    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = e_args) %>% select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
    Condition
      Warning in `crosstable()`:
      Cannot describe columns `dummy_na` and `dummy_na2` as they contain only missing values.
      Warning:
      An error occured when calculating crosstable effects (glm-log):
      i "no valid set of coefficients has been found: please supply starting values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
    Message
      Error (glm-log: "no valid set of coefficients has been found: please supply starting values")
    Condition
      Warning:
      An error occured when calculating crosstable effects (glm-log):
      i "no valid set of coefficients has been found: please supply starting values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
    Message
      Error (glm-log: "no valid set of coefficients has been found: please supply starting values")
    Condition
      Warning:
      A problem occured when calculating crosstable effects (glm-log):
      i "glm.fit: algorithm did not converge"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      A problem occured when calculating crosstable effects (coxph):
      i "Loglik converged before variable 1 ; coefficient may be infinite. "
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables `cyl3`, `dummy`, and `dummy2`
    Output
                  .id                                                                                                      effect
      1           mpg                     Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: 7.24 [3.51 to 10.98]
      2           cyl Relative risk [95% Wald CI], ref='manual vs auto'\n6 vs 4: 0.36 [0.02 to 1.23]\n8 vs 4: 0.22 [0.04 to 0.67]
      3          disp              Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: -146.85 [-215.46 to -78.24]
      4            hp                 Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: -33.42 [-85.64 to 18.81]
      5          drat                      Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: 0.76 [0.50 to 1.03]
      6            wt                   Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: -1.36 [-1.84 to -0.88]
      7          qsec                    Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: -0.82 [-2.06 to 0.42]
      8            vs                     Relative risk [95% Wald CI], ref='manual vs auto'\nvshaped vs straight: 0.51 [CI error]
      9          gear       Relative risk [95% Wald CI], ref='manual vs auto'\n4 vs 3: error [CI error]\n5 vs 3: error [CI error]
      10         carb                     Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: 0.19 [-1.06 to 1.44]
      11      hp_date                 Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: -33.42 [-87.76 to 20.93]
      12   qsec_posix        Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: -7.11e+04 [-1.83e+05 to 4.06e+04]
      13         cyl3                                                                                                  No effect?
      14         cyl6                       Relative risk [95% Wald CI], ref='manual vs auto'\nTRUE vs FALSE: 0.64 [0.04 to 2.29]
      15        dummy                                                                                                  No effect?
      16 dummy_num_vs                     Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: 0.11 [-0.53 to 0.75]
      17       dummy2                                                                                                  No effect?
      18         test                              Relative risk [95% Wald CI], ref='manual vs auto'\nB vs A: 0.63 [0.23 to 1.47]
      19         surv                                                 Hazard ratio (Wald CI)\nmanual vs auto: 3.29e+09 [0 to Inf]
      20         diff                 Difference in means (bootstrap CI), ref='auto'\nmanual minus auto: -32.59 [-83.44 to 18.25]

# Effect - .x='am' - median/RD

    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = e_args) %>% select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
    Condition
      Warning in `crosstable()`:
      Cannot describe columns `dummy_na` and `dummy_na2` as they contain only missing values.
      Warning:
      Problems occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred", "glm.fit: algorithm did not converge", and "collapsing to unique 'x' values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      A problem occured when calculating crosstable effects (coxph):
      i "Loglik converged before variable 1 ; coefficient may be infinite. "
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables `cyl3`, `dummy`, and `dummy2`
    Output
                  .id                                                                                                             effect
      1           mpg                          Difference in medians (bootstrap CI), ref='auto'\nmanual minus auto: 5.50 [2.65 to 14.00]
      2           cyl Risk difference [95% Wald CI], ref='manual vs auto'\n6 vs 4: -1.95 [-5.19 to 0.49]\n8 vs 4: -2.55 [-4.84 to -0.68]
      3          disp                   Difference in medians (bootstrap CI), ref='auto'\nmanual minus auto: -155.50 [-252.00 to -76.59]
      4            hp                     Difference in medians (bootstrap CI), ref='auto'\nmanual minus auto: -66.00 [-109.00 to -2.38]
      5          drat                           Difference in medians (bootstrap CI), ref='auto'\nmanual minus auto: 0.93 [0.41 to 1.07]
      6            wt                        Difference in medians (bootstrap CI), ref='auto'\nmanual minus auto: -1.20 [-1.77 to -0.66]
      7          qsec                         Difference in medians (bootstrap CI), ref='auto'\nmanual minus auto: -0.80 [-2.21 to 1.17]
      8            vs                    Risk difference [95% Wald CI], ref='manual vs auto'\nvshaped vs straight: -1.66 [-3.79 to 0.11]
      9          gear  Risk difference [95% Wald CI], ref='manual vs auto'\n4 vs 3: 21.26 [-371.17 to NA]\n5 vs 3: 41.13 [-491.60 to NA]
      10         carb                         Difference in medians (bootstrap CI), ref='auto'\nmanual minus auto: -1.00 [-2.00 to 2.00]
      11      hp_date                         Difference in medians (bootstrap CI), ref='auto'\nmanual minus auto: -66.00 [-109.00 to 0]
      12   qsec_posix             Difference in medians (bootstrap CI), ref='auto'\nmanual minus auto: -6.91e+04 [-1.86e+05 to 1.04e+05]
      13         cyl3                                                                                                         No effect?
      14         cyl6                          Risk difference [95% Wald CI], ref='manual vs auto'\nTRUE vs FALSE: -0.66 [-3.75 to 1.57]
      15        dummy                                                                                                         No effect?
      16 dummy_num_vs                             Difference in medians (bootstrap CI), ref='auto'\nmanual minus auto: 0 [-0.24 to 0.32]
      17       dummy2                                                                                                         No effect?
      18         test                                 Risk difference [95% Wald CI], ref='manual vs auto'\nB vs A: -0.79 [-2.29 to 0.63]
      19         surv                                                        Hazard ratio (Wald CI)\nmanual vs auto: 3.29e+09 [0 to Inf]
      20         diff                    Difference in medians (bootstrap CI), ref='auto'\nmanual minus auto: -67.55 [-110.85 to -10.56]

# Effect - .x='cyl6' - mean/OR (default)

    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = e_args) %>% select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
    Condition
      Warning in `crosstable()`:
      Cannot describe columns `dummy_na` and `dummy_na2` as they contain only missing values.
      Warning:
      Problems occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred" and "collapsing to unique 'x' values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      An error occured when calculating crosstable effects (glm-logit):
      i "need at least two non-NA values to interpolate"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
    Message
      Error (glm-logit: "need at least two non-NA values to interpolate")
    Condition
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables `cyl3`, `dummy`, and `dummy2`
    Output
                  .id                                                                                                    effect
      1           mpg                      Difference in means (Welch CI), ref='FALSE'\nTRUE minus FALSE: -1.44 [-4.62 to 1.74]
      2           cyl   Odds ratio [95% Wald CI], ref='TRUE vs FALSE'\n6 vs 4: 1.61e+22 [CI error]\n8 vs 4: 1.00e+00 [CI error]
      3          disp              Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -68.57 [-131.13 to -6.00]
      4            hp                 Difference in means (t-test CI), ref='FALSE'\nTRUE minus FALSE: -23.72 [-105.83 to 58.40]
      5          drat                     Difference in means (t-test CI), ref='FALSE'\nTRUE minus FALSE: -0.05 [-0.69 to 0.58]
      6            wt                  Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 5e-04 [-0.56 to 0.56]
      7          qsec                      Difference in means (t-test CI), ref='FALSE'\nTRUE minus FALSE: 0.42 [-1.72 to 2.56]
      8            vs                    Odds ratio [95% Wald CI], ref='TRUE vs FALSE'\nvshaped vs straight: 7.12e+07 [0 to NA]
      9            am                        Odds ratio [95% Wald CI], ref='TRUE vs FALSE'\nmanual vs auto: 0.52 [0.02 to 4.80]
      10         gear Odds ratio [95% Wald CI], ref='TRUE vs FALSE'\n4 vs 3: 3.43 [0.28 to 82.10]\n5 vs 3: 3.00 [0.10 to 89.12]
      11         carb                   Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 1.01 [-1.07 to 3.09]
      12      hp_date                 Difference in means (t-test CI), ref='FALSE'\nTRUE minus FALSE: -23.72 [-105.83 to 58.40]
      13   qsec_posix          Difference in means (t-test CI), ref='FALSE'\nTRUE minus FALSE: 3.61e+04 [-1.49e+05 to 2.21e+05]
      14         cyl3                                                                                                No effect?
      15        dummy                                                                                                No effect?
      16 dummy_num_vs                   Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0.07 [-0.23 to 0.37]
      17       dummy2                                                                                                No effect?
      18         test                         Odds ratio [95% Wald CI], ref='TRUE vs FALSE'\nB vs A: 1.14e+08 [1.22e-151 to NA]
      19         surv                                                Hazard ratio (Wald CI)\nTRUE vs FALSE: 0.66 [0.08 to 5.41]
      20         diff                 Difference in means (t-test CI), ref='FALSE'\nTRUE minus FALSE: -24.13 [-107.85 to 59.58]

# Effect - .x='cyl6' - mean_boot/RR

    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = e_args) %>% select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
    Condition
      Warning in `crosstable()`:
      Cannot describe columns `dummy_na` and `dummy_na2` as they contain only missing values.
      Warning:
      An error occured when calculating crosstable effects (glm-log):
      i "no valid set of coefficients has been found: please supply starting values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
    Message
      Error (glm-log: "no valid set of coefficients has been found: please supply starting values")
    Condition
      Warning:
      A problem occured when calculating crosstable effects (glm-log):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      An error occured when calculating crosstable effects (glm-log):
      i "no valid set of coefficients has been found: please supply starting values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
    Message
      Error (glm-log: "no valid set of coefficients has been found: please supply starting values")
    Condition
      Warning:
      A problem occured when calculating crosstable effects (glm-log):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      An error occured when calculating crosstable effects (glm-log):
      i "no valid set of coefficients has been found: please supply starting values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
    Message
      Error (glm-log: "no valid set of coefficients has been found: please supply starting values")
    Condition
      Warning:
      Cannot calculate crosstable effects for variables `cyl3`, `dummy`, and `dummy2`
    Output
                  .id                                                                                                       effect
      1           mpg                     Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -1.44 [-4.44 to 1.56]
      2           cyl         Relative risk [95% Wald CI], ref='TRUE vs FALSE'\n6 vs 4: error [CI error]\n8 vs 4: error [CI error]
      3          disp                 Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -68.57 [-134.26 to -2.87]
      4            hp                  Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -23.72 [-67.39 to 19.96]
      5          drat                     Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -0.05 [-0.63 to 0.52]
      6            wt                     Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 5e-04 [-0.57 to 0.57]
      7          qsec                      Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0.42 [-1.69 to 2.53]
      8            vs                   Relative risk [95% Wald CI], ref='TRUE vs FALSE'\nvshaped vs straight: 5.49e+07 [CI error]
      9            am                        Relative risk [95% Wald CI], ref='TRUE vs FALSE'\nmanual vs auto: 0.57 [0.03 to 3.80]
      10         gear Relative risk [95% Wald CI], ref='TRUE vs FALSE'\n4 vs 3: 2.89 [0.32 to 56.97]\n5 vs 3: 2.60 [0.12 to 58.04]
      11         carb                      Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 1.01 [-1.06 to 3.08]
      12      hp_date                  Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -23.72 [-67.43 to 20.00]
      13   qsec_posix          Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 3.61e+04 [-1.44e+05 to 2.17e+05]
      14         cyl3                                                                                                   No effect?
      15        dummy                                                                                                   No effect?
      16 dummy_num_vs                      Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0.07 [-0.25 to 0.38]
      17       dummy2                                                                                                   No effect?
      18         test                                Relative risk [95% Wald CI], ref='TRUE vs FALSE'\nB vs A: 7.00e+07 [CI error]
      19         surv                                                   Hazard ratio (Wald CI)\nTRUE vs FALSE: 0.66 [0.08 to 5.41]
      20         diff                  Difference in means (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -24.13 [-67.37 to 19.10]

# Effect - .x='cyl6' - median/RD

    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = e_args) %>% select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
    Condition
      Warning in `crosstable()`:
      Cannot describe columns `dummy_na` and `dummy_na2` as they contain only missing values.
      Warning:
      Problems occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred" and "collapsing to unique 'x' values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      An error occured when calculating crosstable effects (glm-logit):
      i "need at least two non-NA values to interpolate"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
    Message
      Error (glm-logit: "need at least two non-NA values to interpolate")
    Condition
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables `cyl3`, `dummy`, and `dummy2`
    Output
                  .id                                                                                                         effect
      1           mpg                      Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 1.35 [-5.53 to 4.00]
      2           cyl  Risk difference [95% Wald CI], ref='TRUE vs FALSE'\n6 vs 4: 5.11e+01 [CI error]\n8 vs 4: -1.49e-14 [CI error]
      3          disp                Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -108.20 [-159.00 to 78.30]
      4            hp                  Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -27.00 [-75.00 to 53.35]
      5          drat                      Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0.08 [-0.95 to 0.74]
      6            wt                     Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0.005 [-0.66 to 0.66]
      7          qsec                      Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 1.00 [-2.17 to 2.82]
      8            vs                Risk difference [95% Wald CI], ref='TRUE vs FALSE'\nvshaped vs straight: 18.08 [-1388.10 to NA]
      9            am                      Risk difference [95% Wald CI], ref='TRUE vs FALSE'\nmanual vs auto: -0.66 [-3.75 to 1.57]
      10         gear Risk difference [95% Wald CI], ref='TRUE vs FALSE'\n4 vs 3: 1.23 [-1.28 to 4.41]\n5 vs 3: 1.10 [-2.28 to 4.49]
      11         carb                      Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 2.00 [-2.00 to 4.00]
      12      hp_date                  Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -27.00 [-75.00 to 50.80]
      13   qsec_posix          Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 8.64e+04 [-2.12e+05 to 2.42e+05]
      14         cyl3                                                                                                     No effect?
      15        dummy                                                                                                     No effect?
      16 dummy_num_vs                                Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: 0 [0 to 0]
      17       dummy2                                                                                                     No effect?
      18         test                              Risk difference [95% Wald CI], ref='TRUE vs FALSE'\nB vs A: 18.55 [-347.49 to NA]
      19         surv                                                     Hazard ratio (Wald CI)\nTRUE vs FALSE: 0.66 [0.08 to 5.41]
      20         diff                  Difference in medians (bootstrap CI), ref='FALSE'\nTRUE minus FALSE: -28.73 [-77.82 to 46.87]

# Effect - .x='test' - mean/OR (default)

    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = e_args) %>% select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
    Condition
      Warning in `crosstable()`:
      Cannot describe columns `dummy_na` and `dummy_na2` as they contain only missing values.
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables `cyl3`, `dummy`, and `dummy2`
    Output
                  .id                                                                                                          effect
      1           mpg                                      Difference in means (t-test CI), ref='A'\nB minus A: -1.33 [-5.73 to 3.06]
      2           cyl Odds ratio [95% Wald CI], ref='B vs A'\n6 vs 4: 1.73e+08 [4.89e-153 to NA]\n8 vs 4: 1.75e+00 [3.33e-01 to 9.93]
      3          disp                                  Difference in means (bootstrap CI), ref='A'\nB minus A: 8.03 [-75.77 to 91.84]
      4            hp                                    Difference in means (t-test CI), ref='A'\nB minus A: 20.38 [-29.37 to 70.12]
      5          drat                                      Difference in means (t-test CI), ref='A'\nB minus A: -0.18 [-0.57 to 0.20]
      6            wt                                       Difference in means (t-test CI), ref='A'\nB minus A: 0.34 [-0.36 to 1.05]
      7          qsec                                       Difference in means (t-test CI), ref='A'\nB minus A: 0.42 [-0.89 to 1.72]
      8            vs                                Odds ratio [95% Wald CI], ref='B vs A'\nvshaped vs straight: 1.09 [0.21 to 6.03]
      9            am                                     Odds ratio [95% Wald CI], ref='B vs A'\nmanual vs auto: 0.45 [0.10 to 1.88]
      10         gear               Odds ratio [95% Wald CI], ref='B vs A'\n4 vs 3: 0.63 [0.13 to 2.87]\n5 vs 3: 1.31 [0.17 to 12.27]
      11         carb                                    Difference in means (bootstrap CI), ref='A'\nB minus A: 0.62 [-0.49 to 1.74]
      12      hp_date                                    Difference in means (t-test CI), ref='A'\nB minus A: 20.38 [-29.37 to 70.12]
      13   qsec_posix                           Difference in means (t-test CI), ref='A'\nB minus A: 3.60e+04 [-7.66e+04 to 1.48e+05]
      14         cyl3                                                                                                      No effect?
      15         cyl6                               Odds ratio [95% Wald CI], ref='B vs A'\nTRUE vs FALSE: 1.26e+08 [2.48e-154 to NA]
      16        dummy                                                                                                      No effect?
      17 dummy_num_vs                                   Difference in means (bootstrap CI), ref='A'\nB minus A: -0.33 [-0.95 to 0.28]
      18       dummy2                                                                                                      No effect?
      19         surv                                                             Hazard ratio (Wald CI)\nB vs A: 0.63 [0.20 to 1.94]
      20         diff                                    Difference in means (t-test CI), ref='A'\nB minus A: 19.96 [-30.76 to 70.68]

# Effect - .x='test' - mean_boot/RR

    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = e_args) %>% select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
    Condition
      Warning in `crosstable()`:
      Cannot describe columns `dummy_na` and `dummy_na2` as they contain only missing values.
      Warning:
      An error occured when calculating crosstable effects (glm-log):
      i "no valid set of coefficients has been found: please supply starting values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
    Message
      Error (glm-log: "no valid set of coefficients has been found: please supply starting values")
    Condition
      Warning:
      A problem occured when calculating crosstable effects (glm-log):
      i "glm.fit: algorithm did not converge"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      An error occured when calculating crosstable effects (glm-log):
      i "no valid set of coefficients has been found: please supply starting values"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
    Message
      Error (glm-log: "no valid set of coefficients has been found: please supply starting values")
    Condition
      Warning:
      Cannot calculate crosstable effects for variables `cyl3`, `dummy`, and `dummy2`
    Output
                  .id                                                                                              effect
      1           mpg                       Difference in means (bootstrap CI), ref='A'\nB minus A: -1.33 [-5.53 to 2.86]
      2           cyl       Relative risk [95% Wald CI], ref='B vs A'\n6 vs 4: error [CI error]\n8 vs 4: error [CI error]
      3          disp                      Difference in means (bootstrap CI), ref='A'\nB minus A: 8.03 [-82.50 to 98.57]
      4            hp                     Difference in means (bootstrap CI), ref='A'\nB minus A: 20.38 [-27.39 to 68.14]
      5          drat                       Difference in means (bootstrap CI), ref='A'\nB minus A: -0.18 [-0.54 to 0.17]
      6            wt                        Difference in means (bootstrap CI), ref='A'\nB minus A: 0.34 [-0.35 to 1.04]
      7          qsec                        Difference in means (bootstrap CI), ref='A'\nB minus A: 0.42 [-0.75 to 1.59]
      8            vs                 Relative risk [95% Wald CI], ref='B vs A'\nvshaped vs straight: 1.05 [0.43 to 3.10]
      9            am                      Relative risk [95% Wald CI], ref='B vs A'\nmanual vs auto: 0.66 [0.26 to 1.37]
      10         gear Relative risk [95% Wald CI], ref='B vs A'\n4 vs 3: 0.78 [0.30 to 1.74]\n5 vs 3: 1.12 [0.35 to 2.47]
      11         carb                        Difference in means (bootstrap CI), ref='A'\nB minus A: 0.62 [-0.45 to 1.70]
      12      hp_date                     Difference in means (bootstrap CI), ref='A'\nB minus A: 20.38 [-28.04 to 68.79]
      13   qsec_posix            Difference in means (bootstrap CI), ref='A'\nB minus A: 3.60e+04 [-7.09e+04 to 1.43e+05]
      14         cyl3                                                                                          No effect?
      15         cyl6                          Relative risk [95% Wald CI], ref='B vs A'\nTRUE vs FALSE: error [CI error]
      16        dummy                                                                                          No effect?
      17 dummy_num_vs                       Difference in means (bootstrap CI), ref='A'\nB minus A: -0.33 [-0.92 to 0.25]
      18       dummy2                                                                                          No effect?
      19         surv                                                 Hazard ratio (Wald CI)\nB vs A: 0.63 [0.20 to 1.94]
      20         diff                     Difference in means (bootstrap CI), ref='A'\nB minus A: 19.96 [-28.58 to 68.50]

# Effect - .x='test' - median/RD

    Code
      crosstable(mtcars3, -model, by = any_of(.x), effect = T, effect_args = e_args) %>% select(.id, any_of("effect")) %>% distinct() %>% as.data.frame()
    Condition
      Warning in `crosstable()`:
      Cannot describe columns `dummy_na` and `dummy_na2` as they contain only missing values.
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      A problem occured when calculating crosstable effects (glm-logit):
      i "glm.fit: fitted probabilities numerically 0 or 1 occurred"
      * You might want to check for complete separation or extreme outliers.
      * Applying `forcats::fct_rev()` to some columns might help too.
      Warning:
      Cannot calculate crosstable effects for variables `cyl3`, `dummy`, and `dummy2`
    Output
                  .id                                                                                                   effect
      1           mpg                          Difference in medians (bootstrap CI), ref='A'\nB minus A: -3.05 [-6.30 to 2.33]
      2           cyl Risk difference [95% Wald CI], ref='B vs A'\n6 vs 4: 18.97 [-350.71 to NA]\n8 vs 4: 0.56 [-1.10 to 2.30]
      3          disp                      Difference in medians (bootstrap CI), ref='A'\nB minus A: 81.50 [-153.92 to 155.15]
      4            hp                        Difference in medians (bootstrap CI), ref='A'\nB minus A: 26.50 [-52.00 to 85.00]
      5          drat                          Difference in medians (bootstrap CI), ref='A'\nB minus A: -0.20 [-0.80 to 0.56]
      6            wt                           Difference in medians (bootstrap CI), ref='A'\nB minus A: 0.42 [-0.17 to 1.07]
      7          qsec                           Difference in medians (bootstrap CI), ref='A'\nB minus A: 0.58 [-1.10 to 1.88]
      8            vs                   Risk difference [95% Wald CI], ref='B vs A'\nvshaped vs straight: 0.09 [-1.58 to 1.80]
      9            am                       Risk difference [95% Wald CI], ref='B vs A'\nmanual vs auto: -0.79 [-2.29 to 0.63]
      10         gear Risk difference [95% Wald CI], ref='B vs A'\n4 vs 3: -0.47 [-2.05 to 1.06]\n5 vs 3: 0.27 [-1.79 to 2.51]
      11         carb                           Difference in medians (bootstrap CI), ref='A'\nB minus A: 1.00 [-1.00 to 2.00]
      12      hp_date                        Difference in medians (bootstrap CI), ref='A'\nB minus A: 26.50 [-54.76 to 83.00]
      13   qsec_posix               Difference in medians (bootstrap CI), ref='A'\nB minus A: 5.05e+04 [-8.00e+04 to 1.62e+05]
      14         cyl3                                                                                               No effect?
      15         cyl6                        Risk difference [95% Wald CI], ref='B vs A'\nTRUE vs FALSE: 18.65 [-353.69 to NA]
      16        dummy                                                                                               No effect?
      17 dummy_num_vs                                 Difference in medians (bootstrap CI), ref='A'\nB minus A: 0 [-0.49 to 0]
      18       dummy2                                                                                               No effect?
      19         surv                                                      Hazard ratio (Wald CI)\nB vs A: 0.63 [0.20 to 1.94]
      20         diff                        Difference in medians (bootstrap CI), ref='A'\nB minus A: 25.44 [-59.33 to 86.21]

