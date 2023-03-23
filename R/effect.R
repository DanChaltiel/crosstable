
#' Default arguments for calculating and displaying effects in [crosstable()]
#'
#' This helper function provides default parameters for defining how the effect sizes should be computed. It belongs to the `effect_args` argument of the [crosstable()] function. See [effect_summary], [effect_tabular], and [effect_survival] for more insight.
#'
#' @param effect_summarize a function of three arguments (continuous variable, grouping variable and conf_level), used to compare continuous variable. Returns a list of five components: `effect` (the effect value(s)), `ci` (the matrix of confidence interval(s)), `effect.name` (the interpretation(s) of the effect value(s)), `effect.type` (the description of the measure used) and `conf_level` (the confidence interval level). Users can use [diff_mean_auto()], [diff_mean_student()], [diff_mean_boot()], or [diff_median()], or their custom own function.
#' @param effect_tabular a function of three arguments (two categorical variables and conf_level) used to measure the associations between two factors. Returns a list of five components: `effect` (the effect value(s)), `ci` (the matrix of confidence interval(s)), `effect.name` (the interpretation(s) of the effect value(s)), `effect.type` (the description of the measure used) and `conf_level` (the confidence interval level).Users can use [effect_odds_ratio()], [effect_relative_risk()], or [effect_risk_difference()], or their custom own function.
#' @param effect_survival a function of two argument (a formula and conf_level), used to measure the association between a censored and a factor. Returns the same components as created by `effect_summarize`.Users can use [effect_survival_coxph()] or their custom own function.
#' @param effect_display a function to format the effect. See [display_effect()].
#' @param conf_level the desired confidence interval level
#' @param digits the decimal places
#'
#' @return A list with effect parameters
#'
#' @author Dan Chaltiel
#' @export
crosstable_effect_args = function(effect_summarize = diff_mean_auto,
                                  effect_tabular = effect_odds_ratio,
                                  effect_survival = effect_survival_coxph,
                                  effect_display = display_effect,
                                  conf_level = 0.95,
                                  digits = 2){
  list(
    effect_summarize = effect_summarize,
    effect_tabular = effect_tabular,
    effect_survival = effect_survival,
    effect_display = effect_display,
    conf_level = conf_level,
    digits = digits
  )
}


#' Default function to display the effect
#'
#' User can provide their own custom version in [crosstable_effect_args()]
#'
#' @param effect effect
#' @param digits digits
#'
#' @return a character vector
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom dplyr across mutate where
#' @importFrom glue glue glue_collapse
#' @importFrom purrr map_lgl
#' @importFrom rlang is_string
display_effect = function(effect, digits = 4){
  if(is.null(effect) || all(map_lgl(effect, is.null))){
    return("No effect?")
  } else if(is_string(effect)){
    return(effect) #error message
  }

  x = effect$summary %>%
    mutate(across(where(is.numeric),
                  ~format_fixed(.x, digits=digits)))

  if(all(effect$summary$ci_inf=="error", na.rm=TRUE)){
    x$ci = "[CI error]"
  } else {
    x$ci = glue("[{x$ci_inf} to {x$ci_sup}]")
  }

  c(glue("{effect$effect.type}{effect$ref}"),
    glue("{x$name}: {x$effect} {x$ci}")) %>%
    glue_collapse("\n")
}



# Tabular (factor vs factor) ----------------------------------------------



#' Effect measure for association between two categorical variables
#'
#' User can either use or extend these functions to configure effect calculation.
#'
#' @name effect_tabular
#'
#' @param x categorical vector (character, factor, ...)
#' @param by categorical vector (of exactly 2 unique levels)
#' @param conf_level confidence interval level
#'
#' @return A list with five components: effect, ci, effect.name, effect.type, and conf_level
#' @seealso [crosstable_effect_args()]
#'
#' @author Dan Chaltiel, David Hajage
NULL



#' @keywords internal
#' @noRd
#' @importFrom dplyr n_distinct
#' @importFrom glue glue
#' @importFrom stats binomial confint glm
#' @importFrom tibble tibble
calc_effect_tabular = function(x, by, conf_level=0.95, type=c("OR", "RR", "RD")){
  type = match.arg(type)
  tab = table(by, x)
  nn = paste0("x", colnames(tab)[-1])
  if(ncol(tab) <= 1 || nrow(tab) > 2) return(NULL)
  if(n_distinct(x, na.rm=T)==1 || n_distinct(by, na.rm=T)==1) return(NULL)

  ref_x = colnames(tab)[1]
  ref_by = rownames(tab)[1]
  versus_by = rownames(tab)[2] #length==2
  bynum = ifelse(by==ref_by, 0, 1)

  if(type=="OR"){
    mod = tryCatch2(glm(bynum ~ x, family=binomial(link="logit")))
    ci = tryCatch2(exp(confint(mod, level=conf_level)))
    mod_name = "glm-logit"
    effect_type_glue = "Odds ratio [{conf_level*100}% Wald CI]"
  } else if(type=="RR"){
    mod = tryCatch2(glm(bynum ~ x, family=binomial(link="log")))
    ci = tryCatch2(exp(confint(mod, level=conf_level)))
    mod_name = "glm-log"
    effect_type_glue = "Relative risk [{conf_level*100}% Wald CI]"
  } else if(type=="RD"){
    mod = glm(bynum ~ x, family=binomial(link="logit"))
    ci = tryCatch2(confint(mod, level=conf_level))
    mod_name = "glm-logit"
    effect_type_glue = "Risk difference [{conf_level*100}% Wald CI]"
  }

  if (is.null(nrow(ci))) dim(ci) = c(1, length(ci))
  model_warn(mod, ci, type=mod_name)
  if(length(ci)==1 && ci=="error"){
    ci_inf = ci_sup = "error"
  } else {
    ci = rbind(ci, matrix(ncol=2, nrow=length(nn)-(nrow(ci)-1),
                          dimnames=list(nn[!nn %in% rownames(ci)], NULL)))
    ci_inf = ci[nn, 1]
    ci_sup = ci[nn, 2]
  }
  if(inherits(mod, "glm")){
    if(type=="RD") effect = mod$coef[-1]
    else           effect = exp(mod$coef)[-1]
    effect = effect[nn]
  } else {
    effect = "error"
    # ci_inf=ci_sup=NA
    ci_inf = ci_sup = "error"
  }

  effect.type = glue(effect_type_glue)
  reference = glue(", ref='{versus_by} vs {ref_by}'")
  effect.name = glue("{colnames(tab)[-1]} vs {ref_x}")
  summary = tibble(name=effect.name, effect, ci_inf=ci_inf, ci_sup=ci_sup)

  list(effect.type=effect.type, ref=reference, summary=summary)
}


#' @describeIn effect_tabular (**Default**) calculate the odds ratio
#' @export
effect_odds_ratio = function(x, by, conf_level=0.95) {
  calc_effect_tabular(x, by, conf_level, type="OR")
}



#' @describeIn effect_tabular calculate the relative risk
#' @export
# https://stats.stackexchange.com/a/336624/81974
effect_relative_risk = function (x, by, conf_level = 0.95) {
  calc_effect_tabular(x, by, conf_level, type="RR")
}




#' @describeIn effect_tabular calculate the risk difference
#' @export
effect_risk_difference = function (x, by, conf_level = 0.95) {
  calc_effect_tabular(x, by, conf_level, type="RD")
}








# Summary (numeric vs factor) ---------------------------------------------






#' Effect measure for association between one continuous and one categorical variable
#'
#' User can either use or extend these functions to configure effect calculation.
#'
#' @name effect_summary
#'
#' @param x numeric vector
#' @param by categorical vector (of exactly 2 unique levels)
#' @param conf_level confidence interval level
#' @param R number of bootstrap replication
#'
#' @return A list with five components: effect, ci, effect.name, effect.type, and conf_level
#' @seealso [crosstable_effect_args()]
NULL



#' @describeIn effect_summary (**Default**) calculate a specific "difference in means" effect based on normality (Shapiro or Anderson test) and variance homogeneity (Bartlett test)
#' @author Dan Chaltiel, David Hajage
#' @export
diff_mean_auto = function(x, by, conf_level=0.95, R=500) {
  tab = table(by)
  if(length(tab) != 2) return(NULL)

  if (any(test_normality(x, by) < 0.05)) {
    diff_mean_boot(x, by, conf_level, R)
  } else {
    diff_mean_student(x, by, conf_level)
  }
}


#' @describeIn effect_summary calculate a "difference in means" effect with a bootstrapped CI using standard deviation
#' @author Dan Chaltiel, David Hajage
#' @export
#' @importFrom glue glue
#' @importFrom purrr map_dbl
#' @importFrom stats qnorm sd
#' @importFrom tibble tibble
diff_mean_boot = function(x, by, conf_level=0.95, R=500) {
  x = as.numeric(x)
  tab = table(by)
  if (length(tab) != 2) return(NULL)
  by1 = names(tab)[1]
  by2 = names(tab)[2]

  effect = mean(x[by==by2], na.rm = TRUE) - mean(x[by==by1], na.rm = TRUE)
  effect.name = glue("{by2} minus {by1}")
  effect.type = "Difference in means (bootstrap CI)"

  beffect = map_dbl(1:R, ~ {
    ib = sample(length(x), replace = TRUE)
    xi = x[ib]
    byi = by[ib]
    mean(xi[byi == by2], na.rm = TRUE) - mean(xi[byi == by1], na.rm = TRUE)
  })
  ci = effect + qnorm((1-conf_level)/2) * c(1,-1) * sd(beffect, na.rm=TRUE)
  # quantile(beffect, c(0.025,0.975)) #nearly the same CI

  summary = tibble(name=effect.name, effect, ci_inf=ci[1], ci_sup=ci[2])
  reference = glue(", ref='{by1}'")
  list(effect.type=effect.type, ref=reference, summary=summary)
}


#' @describeIn effect_summary calculate a "difference in medians" effect with a bootstrapped CI using quantiles#'
#' @author Dan Chaltiel, David Hajage
#' @export
#' @importFrom glue glue
#' @importFrom purrr map_dbl
#' @importFrom stats median quantile
#' @importFrom tibble tibble
diff_median_boot = function(x, by, conf_level=0.95, R=500) {
  x=as.numeric(x)
  tab = table(by)
  if (length(tab) != 2) return(NULL)
  by1 = names(tab)[1]
  by2 = names(tab)[2]

  effect = median(x[by==by2], na.rm = TRUE) - median(x[by==by1], na.rm = TRUE)
  effect.name = glue("{by2} minus {by1}")
  effect.type = "Difference in medians (bootstrap CI)"

  beffect = map_dbl(1:R, ~ {
    ib = sample(length(x), replace = TRUE)
    xi = x[ib]
    byi = by[ib]
    median(xi[byi == by2], na.rm = TRUE) - median(xi[byi == by1], na.rm = TRUE)
  })
  probs = c((1-conf_level)/2, (1+conf_level)/2)
  ci = quantile(beffect, probs=probs, na.rm=TRUE)

  summary = tibble(name=effect.name, effect, ci_inf=ci[1], ci_sup=ci[2])
  reference = glue(", ref='{by1}'")
  list(effect.type=effect.type, ref=reference, summary=summary)
}
#' @usage NULL
#' @author Dan Chaltiel, David Hajage
#' @export
#' @importFrom lifecycle deprecate_warn
#' @rdname effect_summary
diff_median = function(...){
  deprecate_warn("0.3.0", "diff_median()", "diff_median_boot()")# nocov
  diff_median_boot(...)# nocov
}


#' @describeIn effect_summary  calculate a "difference in means" effect using `t.test` confidence intervals
#' @author Dan Chaltiel, David Hajage
#' @export
#' @importFrom forcats fct_rev
#' @importFrom glue glue
#' @importFrom stats bartlett.test t.test
#' @importFrom tibble tibble
diff_mean_student = function(x, by, conf_level = 0.95) {
  x=as.numeric(x)
  tab = table(by)
  if (length(tab) != 2) return(NULL)

  by1 = names(tab)[1]
  by2 = names(tab)[2]
  effect = mean(x[by==by2], na.rm = TRUE) - mean(x[by==by1], na.rm = TRUE)
  effect.name = glue("{by2} minus {by1}")

  bartlettg = bartlett.test(x, by)$p.value
  if (bartlettg < 0.05) {
    type = "t.unequalvar"
    effect.type = "Difference in means (Welch CI)"
  } else if (bartlettg > 0.05 & length(tab) == 2) {
    type = "t.equalvar"
    effect.type = "Difference in means (t-test CI)"
  }
  by = fct_rev(by) #because t.test messes up the reference
  test = switch(type,
                t.unequalvar = t.test(x ~  by, var.equal = FALSE, conf_level = conf_level),
                t.equalvar = t.test(x ~  by, var.equal = TRUE, conf_level = conf_level))
  ci = unname(test$conf.int)

  summary = tibble(name=effect.name, effect, ci_inf=ci[1], ci_sup=ci[2])
  reference = glue(", ref='{by1}'")
  list(effect.type=effect.type, ref=reference, summary=summary)
}



# Survival ----------------------------------------------------------------


#' Effect measure for association between one censored variable and one categorical variable
#'
#' @param x survival vector (made using [survival::Surv()])
#' @param by categorical vector (of exactly 2 unique levels)
#' @param conf_level confidence interval level
#'
#' @name effect_survival
#' @return a list with two components: p.value and method
#'
#' @author Dan Chaltiel, David Hajage
#' @export
#' @importFrom glue glue
#' @importFrom rlang check_installed
#' @importFrom stats confint
#' @importFrom tibble tibble
effect_survival_coxph = function(x, by, conf_level = 0.95) {
  check_installed("survival", reason="for survival data to be described using `crosstable()`.")

  mod = tryCatch2(survival::coxph(x~by))
  ci = suppressMessages(exp(confint(mod)))
  effect = exp(mod$coef)
  model_warn(mod, ci, type="coxph")

  if (is.null(nrow(ci))) dim(ci) = c(1, length(ci))


  effect.name = glue("{mod$xlevels[[1]][-1]} vs {mod$xlevels[[1]][1]}")
  effect.type = glue("Hazard ratio (Wald CI)")
  reference = ""
  summary = tibble(name=effect.name, effect, ci_inf=ci[,1], ci_sup=ci[,2])

  list(effect.type=effect.type, ref=reference, summary=summary)
}



# Utils -------------------------------------------------------------------



#' @importFrom cli cli_bullets cli_warn
#' @importFrom glue glue_collapse
#' @keywords internal
#' @noRd
model_warn = function(mod, ci, type){
  default_warning = "You might want to check for complete separation or extreme outliers."
  default_warning2 = "Applying `forcats::fct_rev()` to some columns might help too."
  msg = unique(c(attr(mod, "warnings"), attr(ci, "warnings")))
  if(length(msg)>0){
    p = if(length(msg)==1) "A problem" else "Problems"
    cli_warn(c("{p} occured when calculating crosstable effects ({type}):\n",
               i="{.val {msg}}",
               "*"="{default_warning}",
               "*"="{default_warning2}"),
             class="crosstable_effect_warning")
  }

  msg = attr(mod, "errors")
  if(length(msg)==0) msg = unique(c(attr(mod, "errors"), attr(ci, "errors")))
  if(length(msg)>0){
    w = glue_collapse(msg, "', '", last="' and '")
    cli_warn(c("An {.strong error} occured when calculating crosstable effects ({type}):",
               i="{.val {msg}}",
               "*"="{default_warning}",
               "*"="{default_warning2}"),
             class="crosstable_effect_error_warning")
    return(cli_bullets("Error ({type}: {.val {msg}})"))
  }
}
