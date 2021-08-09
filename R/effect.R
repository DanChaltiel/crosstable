
#' Default arguments for calculating and displaying effects in [crosstable()]
#'
#' This helper function provides default parameters for defining how the effect sizes should be computed. It belongs to the `effect_args` argument of the [crosstable()] function. See [effect_summary], [effect_tabular], and [effect_survival] for more insight.
#'
#' @return A list with testing parameters:
#' \itemize{
#'   \item `effect_summarize` - a function of three arguments (continuous variable, grouping variable and conf_level), used to compare continuous variable. Returns a list of five components: `effect` (the effect value(s)), `ci` (the matrix of confidence interval(s)), `effect.name` (the interpretation(s) of the effect value(s)), `effect.type` (the description of the measure used) and `conf_level` (the confidence interval level). See [diff_mean_auto()], [diff_mean_student()], [diff_mean_boot()], or [diff_median()] for some examples of such functions. Users can provide their own function.
#'   \item `effect_tabular` - a function of three arguments (two categorical variables and conf_level) used to measure the associations between two factors. Returns a list of five components: `effect` (the effect value(s)), `ci` (the matrix of confidence interval(s)), `effect.name` (the interpretation(s) of the effect value(s)), `effect.type` (the description of the measure used) and `conf_level` (the confidence interval level). See [effect_odds_ratio()], [effect_relative_risk()], or [effect_risk_difference()] for some examples of such functions. Users can provide their own function.
#'   \item `effect_survival` - a function of two argument (a formula and conf_level), used to measure the association between a censored and a factor. Returns the same components as created by `effect_summarize`. See [effect_survival_coxph()]. Users can provide their own function.
#'   \item `effect_display` - a function to format the effect. See [display_effect()].
#'   \item `conf_level` - the desired confidence interval level
#'   \item `digits` - the decimal places
#' }
#' 
#' @export
#' @author Dan Chaltiel
crosstable_effect_args = function(){
    list( 
        effect_summarize = diff_mean_auto, 
        effect_tabular = effect_odds_ratio,
        effect_survival = effect_survival_coxph,
        effect_display = display_effect,
        conf_level = 0.95,
        digits = 2
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
#' @importFrom rlang is_string
#' @export
display_effect = function(effect, digits = 4){
    if(is.null(effect) || all(map_lgl(effect, is.null))){
        return("No effect?")
    } else if(is_string(effect)){ 
        return(effect) #error message
    }
  
    x = effect$summary %>% 
        mutate_if(is.numeric, 
                  ~formatC(.x, format="f", digits=digits))
    # print(effect$summary$ci_inf)
    if(identical(effect$summary$ci_inf, "error")){
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
#' User can either use or extend these functions to parametrize effect calculation.
#'
#' @name effect_tabular
#' 
#' @param x categorical vector (character, factor, ...)
#' @param by categorical vector (of exactly 2 unique levels)
#' @param conf_level confidence interval level
#'
#' @return A list with five components: effect, ci, effect.name, effect.type, and conf_level
#' @seealso [crosstable_effect_args()] 
NULL



#' @describeIn effect_tabular (**Default**) calculate the odds ratio
#' @importFrom stats glm binomial confint.default
#' @export
effect_odds_ratio = function (x, by, conf_level=0.95) {
    tab = table(by, x)
    if(ncol(tab) <= 1 || 
       nrow(tab) > 2 ||
       n_distinct(x, na.rm=T)==1 || 
       n_distinct(by, na.rm=T)==1){
        return(NULL)
    }
    
    ref = rownames(tab)[1]
    versus = rownames(tab)[2]
    bynum = ifelse(by==ref, 0, 1)
    
    mod = glm(bynum ~ x, family = binomial(link = "logit"))
    ci = tryCatch2(exp(confint(mod, level=conf_level)[-1, ]))
    if (is.null(nrow(ci))) dim(ci) = c(1, length(ci))
    model_warn(mod, ci, type="glm-logit")
    if(inherits(mod, "glm")){
      effect = exp(mod$coef)[-1] 
    } else {
      effect = "error"
    }
    if(length(ci)==1 && ci=="error"){
      ci = cbind("error", "error")
    }
    effect.name = glue("{colnames(tab)[-1]} vs {colnames(tab)[1]}")
    effect.type = glue("Odds ratio [{conf_level*100}% Wald CI]")
    reference = glue(", ref='{versus} vs {ref}'")
    summary = tibble(name=effect.name, effect, ci_inf=ci[,1], ci_sup=ci[,2])
    list(effect.type=effect.type, ref=reference, summary=summary)
}



#' @describeIn effect_tabular calculate the relative risk
#'
#' @importFrom stats glm binomial confint
#' @importFrom glue glue
#' @export 
# https://stats.stackexchange.com/a/336624/81974
effect_relative_risk = function (x, by, conf_level = 0.95) { 
    tab = table(by, x)
    if (ncol(tab) <= 1 | nrow(tab) > 2) return(NULL)
    
    ref = rownames(tab)[1]
    versus = rownames(tab)[2]
    bynum = ifelse(by==ref, 0, 1)
    mod = tryCatch2(glm(bynum ~ x, family = binomial(link = "log")))
    ci = tryCatch2(exp(confint(mod)[-1, ]))
    if (is.null(nrow(ci))) dim(ci) = c(1, length(ci))
    model_warn(mod, ci, type="glm-log")
    if(inherits(mod, "glm")){
      effect = exp(mod$coef)[-1] 
    } else {
      effect = "error"
    }
    if(length(ci)==1 && ci=="error"){
      ci = cbind("error", "error")
    }
    effect.name = glue("{colnames(tab)[-1]} vs {colnames(tab)[1]}")
    effect.type = glue("Relative risk [{conf_level*100}% Wald CI]")
    reference = glue(", ref='{versus} vs {ref}'")
    summary = tibble(name=effect.name, effect, ci_inf=ci[,1], ci_sup=ci[,2])
    list(effect.type=effect.type, ref=reference, summary=summary)
}




#' @describeIn effect_tabular calculate the risk difference
#'
#' @importFrom stats glm binomial confint 
#' @importFrom glue glue 
#' @export
effect_risk_difference = function (x, by, conf_level = 0.95) {
    tab = table(by, x)
    if (ncol(tab) <= 1 || nrow(tab) > 2) {
        return(NULL)
    }
    ref = rownames(tab)[1]
    versus = rownames(tab)[2]
    bynum = ifelse(by==ref, 0, 1)
    mod = tryCatch2(glm(bynum ~ x, family = binomial(link = "logit")))
    ci = tryCatch2(confint(mod)[-1, ])
    if (is.null(nrow(ci))) dim(ci) = c(1, length(ci))
    model_warn(mod, ci, type="glm-logit")
    if(inherits(mod, "glm")){
      effect = mod$coef[-1] 
    } else {
      effect = "error"
    }
    if(length(ci)==1 && ci=="error"){
      ci = cbind("error", "error")
    }
    
    
    effect.name = glue("{colnames(tab)[-1]} vs {colnames(tab)[1]}")
    effect.type = glue("Risk difference [{conf_level*100}% Wald CI]")
    reference = glue(", ref='{versus} vs {ref}'")
    summary = tibble(name=effect.name, effect, ci_inf=ci[,1], ci_sup=ci[,2])
    list(effect.type=effect.type, ref=reference, summary=summary)
}









# Summary (numeric vs factor) ---------------------------------------------






#' Effect measure for association between one continuous and one categorical variable
#' 
#' User can either use or extend these functions to parametrize effect calculation.
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
#' @importFrom stats sd qnorm bartlett.test t.test
#' @importFrom forcats fct_rev
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
#' @importFrom stats sd qnorm 
#' @export
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


#' @describeIn effect_summary calculate a "difference in medians" effect with a bootstrapped CI using quantiles
#' @importFrom stats sd qnorm
#' @export
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
#' @importFrom lifecycle deprecate_warn
#' @export
#' @rdname effect_summary
diff_median = function(...){
  deprecate_warn("0.3.0", "diff_median()", "diff_median_boot()")# nocov
  diff_median_boot(...)# nocov
}


#' @describeIn effect_summary  calculate a "difference in means" effect using `t.test` confidence intervals
#' @importFrom stats bartlett.test t.test
#' @export
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
#' @importFrom stats confint
#' @importFrom survival coxph
#' @export
effect_survival_coxph = function(x, by, conf_level = 0.95) {
    mod = tryCatch2(coxph(x~by))
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



#' @keywords internal
#' @noRd
model_warn = function(mod, ci, type){
    default_warning = "You might want to check for complete separation or extreme outliers."
    default_warning2 = "Applying `forcats::fct_rev()` to some columns might help too."
    msg = unique(c(attr(mod, "warnings"), attr(ci, "warnings")))
    if(length(msg)>0){
        p = if(length(msg)==1) "A problem" else "Problems"
        w = glue_collapse(msg, "', '", last="' and '")
        warn(glue("{p} occured when calculating crosstable effects ({type}):\n",
                  "  '{w}' \n{default_warning} \n{default_warning2}"),
             class="crosstable_effect_warning")
    }
    
    msg = attr(mod, "errors")
    if(length(msg)==0) msg = unique(c(attr(mod, "errors"), attr(ci, "errors")))
    if(length(msg)>0){
        w = glue_collapse(msg, "', '", last="' and '")
        warn(glue("An *error* occured when calculating crosstable effects ({type}):\n",
                  "  '{w}' \n{default_warning} \n{default_warning2}"),
             class="crosstable_effect_error_warning")
        return(glue("Error ({type}: {w})"))
    }
}
