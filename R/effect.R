
#' Default arguments for calculating and displaying effects in [crosstable()]
#'
#' This helper function provides default parameters for defining how the effect sizes should be computed. It belongs to the `effect_args` argument of the [crosstable()] function. See [effect_summary], [effect_tabular], and [effect_survival] for more insight.
#'
#' @return A list with testing parameters:
#' \itemize{
#'   \item `effect_summarize` - a function of three arguments (continuous variable, grouping variable and conf_level), used to compare continuous variable. Returns a list of five components: `effect` (the effect value(s)), `ci` (the matrix of confidence interval(s)), `effect.name` (the interpretation(s) of the effect value(s)), `effect.type` (the description of the measure used) and `conf_level` (the confidence interval level). See [diff_mean_auto()], [diff_mean_student()], [diff_mean_boot()], or [diff_median()] for some examples of such functions. Users can provide their own function.
#'   \item `effect_tabular` - a function of three arguments (two categorical variables and conf_level) used to measure the associations between two factors. Returns a list of five components: `effect` (the effect value(s)), `ci` (the matrix of confidence interval(s)), `effect.name` (the interpretation(s) of the effect value(s)), `effect.type` (the description of the measure used) and `conf_level` (the confidence interval level). See [effect_odds_ratio()], [effect_relative_risk()], or [effect_risk_difference()] for some examples of such functions. Users can provide their own function.
#'   \item `effect_survival` - a function of two argument (a formula and conf_level), used to measure the association between a censored and a factor. Returns the same components as created by `effect_summarize`. See [effect_survival_coxph()]. Users can provide their own function.
#'   \item `conf_level` - the desired confidence interval level
#'   \item `digits` - the decimal places
#'   \item `show_effect` - a function to format the effect. See [display_effect()].
#' }
#' 
#' @export
#' @author Dan Chaltiel
crosstable_effect_args = function(){
    list( 
        effect_summarize = diff_mean_auto, 
        effect_tabular = effect_odds_ratio,
        effect_survival = effect_survival_coxph, 
        conf_level = 0.95,
        digits = 2,
        show_effect = display_effect
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
display_effect = function(effect, digits = 4) {
    if (is.null(effect) || all(map_lgl(effect, is.null))){
        return("No effect?")
    } else if (is_string(effect)){ #error message
        return(effect)
    } else {
        paste(paste0(effect$effect.type, " (", effect$effect.name, "): ", formatC(effect$effect, format = "f", digits = digits), "\n", effect$conf_level*100, "%CI [", paste(formatC(effect$ci[, 1], format = "f", digits = digits), formatC(effect$ci[, 2], format = "f", digits = digits), sep = " to "), "]"), collapse = "\n")
    }
}





# Tabular (factor vs factor) ----------------------------------------------



#' Effect measure for association between two categorical variables
#' 
#' User can either use or extend these functions to parametrize effect calculation.
#'
#' @name effect_tabular
#' 
#' @param x vector (of exactly 2 unique levels)
#' @param y another vector
#' @param conf_level confidence interval level
#'
#' @return A list with five components: effect, ci, effect.name, effect.type, and conf_level
#' @seealso [crosstable_effect_args()] 
NULL



#' @describeIn effect_tabular (**Default**) calculate the odds ratio
#' @importFrom stats glm binomial confint.default
#' @export
effect_odds_ratio = function (x, y, conf_level = 0.95) {
    tab = table(x, y)
    if (ncol(tab) <= 1 | nrow(tab) > 2) {
        return(NULL)
    } else if(n_distinct(x, na.rm=T)==1 || n_distinct(y, na.rm=T)==1){
        return(NULL)
    } else {
        xnum = ifelse(x == rownames(tab)[1], 1, 0)
        mod = glm(xnum ~ y, family = binomial(link = "logit"))
        effect = exp(mod$coef)[-1]
        ci = suppressMessages(exp(confint.default(mod)[-1, ]))
        if (is.null(nrow(ci))) {
            dim(ci) = c(1, length(ci))
        }
        effect.name = paste0(rownames(tab)[1], ", ", paste(colnames(tab)[-1], colnames(tab)[1], sep = " vs "))
        effect.type = "Odds ratio (Wald CI)"
    }    
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf_level = conf_level)
}



#' @describeIn effect_tabular calculate the relative risk
#'
#' @importFrom stats glm binomial confint
#' @importFrom stringr str_squish str_detect
#' @importFrom rlang is_string  
#' @importFrom glue glue glue_collapse
#' @export 
effect_relative_risk = function (x, y, conf_level = 0.95) { 
    tab = table(x, y)
    if (ncol(tab) <= 1 | nrow(tab) > 2) {
        return(NULL)
    } else {
        # https://stats.stackexchange.com/a/336624/81974
        default_warning = "You might want to check for complete separation or extreme outliers."
        xnum = ifelse(x == rownames(tab)[1], 1, 0)
        mod = tryCatch2(glm(xnum ~ y, family = binomial(link = "log")))
        ci = tryCatch2(suppressMessages(exp(confint(mod)[-1, ])))
        
        msg = unique(c(attr(mod, "warnings"), attr(ci, "warnings")))
        if(!is.null(msg)){
            p = if(length(msg)==1) "A problem" else "Problems"
            w = glue_collapse(msg, "', '", last="' and '")
            warning(str_squish(glue("{p} occured when calculating 
                        crosstable effects (glm): '{w}'. {default_warning}")), 
                    call. = F)
        }
        
        msg = unique(c(attr(mod, "errors"), attr(ci, "errors")))
        if(length(msg)>1) msg = attr(mod, "errors")
        if(!is.null(msg)) {
            w = glue_collapse(msg, "', '", last="' and '")
            warning(str_squish(glue("An error occured when calculating 
                crosstable effects (glm): '{w}'. {default_warning}")), 
                    call. = F)
            return(glue("Error (glm: {w})"))
        }
        
        if (is.null(nrow(ci))) {
            dim(ci) = c(1, length(ci))
        }
        effect = exp(mod$coef)[-1]
        effect.name = paste0(rownames(tab)[1], ", ", paste(colnames(tab)[-1], colnames(tab)[1], sep = " vs "))
        effect.type = "Relative risk (Wald CI)"
    }    
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf_level = conf_level)
}




#' @describeIn effect_tabular calculate the risk difference
#'
#' @importFrom stats glm binomial confint 
#' @importFrom stringr str_squish
#' @export
effect_risk_difference = function (x, y, conf_level = 0.95) {
    tab = table(x, y)
    if (ncol(tab) <= 1 || nrow(tab) > 2) {
        return(NULL)
    } else {
        default_warning = "You might want to check for complete separation or extreme outliers."
        xnum = ifelse(x == rownames(tab)[1], 1, 0)
        mod = tryCatch2(glm(xnum ~ y, family = binomial(link = "logit")))
        ci = tryCatch2(100*suppressMessages(confint(mod)[-1, ]))
        
        msg = unique(c(attr(mod, "warnings"), attr(ci, "warnings")))
        if(!is.null(msg)){
            p = if(length(msg)==1) "A problem" else "Problems"
            w = glue_collapse(msg, "', '", last="' and '")
            warning(str_squish(glue("{p} occured when calculating 
                        crosstable effects (glm): '{w}'. {default_warning}")), 
                    call. = F)
        }
        
        msg = unique(c(attr(mod, "errors"), attr(ci, "errors")))
        if(length(msg)>1) msg = attr(mod, "errors")
        if(!is.null(msg)) {
            w = glue_collapse(msg, "', '", last="' and '")
            warning(str_squish(glue("An error occured when calculating 
                crosstable effects (glm): '{w}'. {default_warning}")), 
                    call. = F)
            return(glue("Error (glm: {w})"))
        }
        
        if (is.null(nrow(ci))) {
            dim(ci) = c(1, length(ci))
        }
        effect = 100*mod$coef[-1] 
        effect.name = paste0(rownames(tab)[1], ", ", paste(colnames(tab)[-1], colnames(tab)[1], sep = " minus "))
        effect.type = "Risk difference (Wald CI)"
    }    
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf_level = conf_level)
}



# Summary (numeric vs factor) ---------------------------------------------






#' Effect measure for association between one continuous and one categorical variable
#' 
#' User can either use or extend these functions to parametrize effect calculation.
#'
#' @name effect_summary
#' 
#' @param x a numeric vector
#' @param g another vector (of exactly 2 unique levels)
#' @param conf_level confidence interval level
#' @param R number of bootstrap replication
#'
#' @return A list with five components: effect, ci, effect.name, effect.type, and conf_level
#' @seealso [crosstable_effect_args()] 
NULL



#' @describeIn effect_summary (**Default**) calculate a specific "difference in means" effect based on normality (Shapiro or Anderson test) and variance homogeneity (Bartlett test)
#' @importFrom stats sd qnorm bartlett.test t.test
#' @export
diff_mean_auto = function(x, g, conf_level = 0.95, R = 500) {
    x=as.numeric(x)
    ng = table(g)
    if (length(ng) != 2) {
        # TODO find an effect to calculate for groups of more than 2
        return(list(effect=NULL, ci=NULL, effect.name=NULL, effect.type=NULL, conf_level=NULL))
    } 
    
    effect = unname(diff(rev(tapply(x, g, mean, na.rm = TRUE))))
    effect.name = paste(names(ng), collapse = " minus ")
    
    normg = test_normality(x, g)
    if (any(normg < 0.05)) {
        beffect = vector("numeric", R)
        for (i in 1:R) {
            ib = sample(1:length(x), replace = TRUE)
            xi = x[ib]
            gi = g[ib]
            if(length(table(gi))!=2)
                beffect[i] = NA
            else
                beffect[i] = unname(diff(rev(tapply(xi, gi, mean, na.rm = TRUE))))
        }
        sd.effect = sd(beffect, na.rm=TRUE)
        ci = effect + qnorm(c((1-conf_level)/2, 1-(1-conf_level)/2))*sd.effect
        dim(ci) = c(1, 2)
        effect.type = "Difference in means (bootstrap CI)"
    } else {
        bartlettg = bartlett.test(x, g)$p.value
        if (bartlettg < 0.05) {
            type = "t.unequalvar"
            effect.type = "Difference in means (Welch CI)"
        } else if (bartlettg > 0.05 & length(ng) == 2) {
            type = "t.equalvar"
            effect.type = "Difference in means (t-test CI)"
        }
        test = switch(type,
                      t.unequalvar = t.test(x ~  g, var.equal = FALSE, conf_level = conf_level),
                      t.equalvar = t.test(x ~  g, var.equal = TRUE, conf_level = conf_level))
        ci = unname(test$conf.int)
        attributes(ci) = NULL
        dim(ci) = c(1, 2)
    }
    
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf_level = conf_level)
}


#' @describeIn effect_summary calculate a "difference in means" effect by bootstrapping
#' @importFrom stats sd qnorm 
#' @export
diff_mean_boot = function(x, g, conf_level = 0.95, R = 500) {
    x=as.numeric(x)
    ng = table(g)
    if (length(ng) <= 1 | length(ng) > 2) {
        return(NULL)
    } else if (length(ng) == 2) {
        effect = unname(diff(rev(tapply(x, g, mean, na.rm = TRUE))))
        effect.name = paste(names(ng), collapse = " minus ")
        beffect = vector("numeric", R)
        for (i in 1:R) {
            ib = sample(1:length(x), replace = TRUE)
            xi = x[ib]
            gi = g[ib]
            if(length(table(gi))!=2)
                beffect[i] = NA
            else
                beffect[i] = unname(diff(rev(tapply(xi, gi, mean, na.rm = TRUE))))
        }
        sd.effect = sd(beffect)
        ci = effect + qnorm(c((1-conf_level)/2, 1-(1-conf_level)/2))*sd.effect
        dim(ci) = c(1, 2)
        effect.type = "Difference in means (bootstrap CI)"
    }            
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf_level = conf_level)
}


#' @describeIn effect_summary calculate a "difference in medians" effect by bootstrapping
#' @importFrom stats sd qnorm
#' @export
diff_median = function(x, g, conf_level = 0.95, R = 500) {
    x=as.numeric(x)
    ng = table(g)
    if (length(ng) <= 1 | length(ng) > 2) {
        return(NULL)
    } else if (length(ng) == 2) {
        effect = unname(diff(rev(tapply(x, g, median, na.rm = TRUE))))
        
        beffect = vector("numeric", R)
        for (i in 1:R) {
            ib = sample(1:length(x), replace = TRUE)
            xi = x[ib]
            gi = g[ib]
            if(length(table(gi))!=2)
                beffect[i] = NA
            else
                beffect[i] = unname(diff(rev(tapply(xi, gi, median, na.rm = TRUE))))
        }
        sd.effect = sd(beffect)
        ci = effect + qnorm(c((1-conf_level)/2, 1-(1-conf_level)/2))*sd.effect
        dim(ci) = c(1, 2)
        effect.type = "Difference in medians (bootstrap CI)"
    }
    list(effect = effect, ci = ci, effect.type = effect.type, conf_level = conf_level)
}


#' @describeIn effect_summary  calculate a "difference in means" effect using `t.test` confidence intervals
#' @importFrom stats bartlett.test t.test
#' @export
diff_mean_student = function(x, g, conf_level = 0.95) {
    x=as.numeric(x)
    ng = table(g)
    if (length(ng) <= 1 | length(ng) > 2) {
        return(NULL)
    } else if (length(ng) == 2) {
        effect = unname(diff(rev(tapply(x, g, mean, na.rm = TRUE))))
        effect.name = paste(names(ng), collapse = " minus ")
        bartlettg = bartlett.test(x, g)$p.value
        if (bartlettg < 0.05) {
            type = "t.unequalvar"
            effect.type = "Difference in means (Welch CI)"
        } else if (bartlettg > 0.05 & length(ng) == 2) {
            type = "t.equalvar"
            effect.type = "Difference in means (t-test CI)"
        }
        test = switch(type,
                      t.unequalvar = t.test(x ~  g, var.equal = FALSE, conf_level = conf_level),
                      t.equalvar = t.test(x ~  g, var.equal = TRUE, conf_level = conf_level))
        ci = unname(test$conf.int)
        attributes(ci) = NULL
        dim(ci) = c(1, 2)
    }
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf_level = conf_level)
}



# Survival ----------------------------------------------------------------


#' Effect measure for association between one censored variable and one categorical variable
#'
#' @param formula a formula
#' @param conf_level the confidence level required
#'
#' @name effect_survival
#' @return a list with two components: p.value and method
#' 
#' @importFrom stats confint
#' @importFrom survival coxph
#' @export
effect_survival_coxph = function(formula, conf_level = 0.95) {
    mod = tryCatch2(coxph(formula))
    
    msg = attr(mod, "warnings")
    if(!is.null(msg)){
        p = if(length(msg)==1) "A problem" else "Problems"
        w = glue_collapse(msg, "', '", last="' and '")
        warning(str_squish(glue("{p} occured when calculating 
                crosstable effects (coxph): '{w}'.")), 
                call. = F)
    }
    
    msg = attr(mod, "errors")
    if(!is.null(msg)) {
        w = glue_collapse(msg, "', '", last="' and '")
        warning(str_squish(glue("An error occured when calculating 
                crosstable effects (coxph): '{w}'.")), 
                call. = F)
        return(glue("Error (coxph: {w})"))
    }
    
    effect = exp(mod$coef)
    ci = suppressMessages(exp(confint(mod)))
    if (is.null(nrow(ci))) {
        dim(ci) = c(1, length(ci))
    }
    effect.name = paste(mod$xlevels[[1]][-1], mod$xlevels[[1]][1], sep = " vs ")
    effect.type = "Hazard ratio (Wald CI)"
    
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf_level = conf_level)
}


