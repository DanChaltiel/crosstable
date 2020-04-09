
#' Default arguments for [crosstable()] effects
#'
#' @return A list with testing parameters:
#' \itemize{
#'   \item `effect.summarize` - a function of three arguments (continuous variable, grouping variable and conf.level), used to compare continuous variable. Returns a list of five components : \code{effect} (the effect value(s)), \code{ci} (the matrix of confidence interval(s)), \code{effect.name} (the interpretiation(s) of the effect value(s)), \code{effect.type} (the description of the measure used) and \code{conf.level} (the confidence interval level). See \code{diff_mean.auto}, \code{diff_mean.student} or \code{diff_mean.boot} for some examples of such functions. Users can provide their own function.
#'   \item `effect.tabular` - a function of three arguments (two categorical variables and conf.level) used to measure the associations between two factors. Returns a list of five components : \code{effect} (the effect value(s)), \code{ci} (the matrix of confidence interval(s)), \code{effect.name} (the interpretation(s) of the effect value(s)), \code{effect.type} (the description of the measure used) and \code{conf.level} (the confidence interval level). See \code{or.row.by.col}, \code{rr.row.by.col}, \code{rd.row.by.col}, \code{or.col.by.row}, \code{rr.col.by.row}, or \code{rd.col.by.row} for some examples of such functions. Users can provide their own function.
#'   \item `effect.survival` - a function of two argument (a formula and conf.level), used to measure the association between a consored and a factor. Returns the same components as created by \code{effect.summarize}. See \code{effect.survival.coxph}. Users can provide their own function.
#'   \item `conf.level` - the desired confidence interval level
#'   \item `digits` - the decimal places
#'   \item `show.effect` - a function to format the effect. See [display.effect()].
#' }
#' 
#' @export
#' @author Dan Chaltiel
crosstable_effect_args = function(){
    list( 
        effect.summarize = diff_mean.auto, 
        effect.tabular = or.row.by.col,
        effect.survival = effect.survival.coxph, 
        conf.level = 0.95,
        digits = 2,
        show.effect = display.effect
    )
}


#' Display Effect
#'
#' @param effect effect
#' @param digits digits
#'
#' @export
display.effect <- function(effect, digits = 4) {
    if (all(sapply(effect, is.null))){
        warning("Could not calculate effect. Is there not 2 groups exactly?")
        return("No effect?")
    }else {
        paste(paste0(effect$effect.type, " (", effect$effect.name, "): ", formatC(effect$effect, format = "f", digits = digits), "\nCI", effect$conf.level*100, "%[", paste(formatC(effect$ci[, 1], format = "f", digits = digits), formatC(effect$ci[, 2], format = "f", digits = digits), sep = " to "), "]"), collapse = "\n")
    }
}


#' Effect measure for association between two factors
#'
#' @param x vector
#' @param y another vector
#' @param conf.level confidence interval level
#'
#' @return a list with five componments
#' @importFrom stats glm binomial confint.default
#' @export
or.row.by.col <- function (x, y, conf.level = 0.95) {
    tab <- table(x, y)
    if (ncol(tab) <= 1 | nrow(tab) > 2) {
        ## Je ne sais pas quel effet calculer 
        effect <- NULL
        ci <- NULL
        effect.name <- NULL
        effect.type <- NULL
        conf.level <- NULL
    } else if(n_distinct(x, na.rm=T)==1 || n_distinct(y, na.rm=T)==1){
        effect <- NULL
        ci <- NULL
        effect.name <- NULL
        effect.type <- NULL
        conf.level <- NULL
    } else {
        xnum <- ifelse(x == rownames(tab)[1], 1, 0)
        mod <- glm(xnum ~ y, family = binomial(link = "logit"))
        effect <- exp(mod$coef)[-1]
        ci <- suppressMessages(exp(confint.default(mod)[-1, ]))
        if (is.null(nrow(ci))) {
            dim(ci) <- c(1, length(ci))
        }
        effect.name <- paste0(rownames(tab)[1], ", ", paste(colnames(tab)[-1], colnames(tab)[1], sep = " vs "))
        effect.type <- "Odds ratio (Wald CI)"
    }    
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf.level = conf.level)
}

#' Effect measure for association between two factors
#'
#' @param x vector
#' @param y another vector
#' @param conf.level confidence interval level
#'
#' @return a list with five componments
#' @importFrom stats glm binomial confint
#' @export
rr.row.by.col <- function (x, y, conf.level = 0.95) {
    tab <- table(x, y)
    if (ncol(tab) <= 1 | nrow(tab) > 2) {
        ## Je ne sais pas quel effet calculer 
        effect <- NULL
        ci <- NULL
        effect.name <- NULL
        effect.type <- NULL
        conf.level <- NULL
    } else {
        xnum <- ifelse(x == rownames(tab)[1], 1, 0)
        mod <- glm(xnum ~ y, family = binomial(link = "log"))
        effect <- exp(mod$coef)[-1]
        ci <- suppressMessages(exp(confint(mod)[-1, ]))
        if (is.null(nrow(ci))) {
            dim(ci) <- c(1, length(ci))
        }
        effect.name <- paste0(rownames(tab)[1], ", ", paste(colnames(tab)[-1], colnames(tab)[1], sep = " vs "))
        effect.type <- "Relative risk (Wald CI)"
    }    
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf.level = conf.level)
}

#' Effect measure for association between two factors
#'
#' @param x vector
#' @param y another vector
#' @param conf.level confidence interval level
#'
#' @return a list with five componments
#' @importFrom stats glm binomial confint
#' @export
rd.row.by.col <- function (x, y, conf.level = 0.95) {
    tab <- table(x, y)
    if (ncol(tab) <= 1 | nrow(tab) > 2) {
        ## Je ne sais pas quel effet calculer 
        effect <- NULL
        ci <- NULL
        effect.name <- NULL
        effect.type <- NULL
        conf.level <- NULL
    } else {
        xnum <- ifelse(x == rownames(tab)[1], 1, 0)
        mod <- glm(xnum ~ y, family = binomial(link = "logit"))
        effect <- 100*mod$coef[-1]
        ci <- 100*suppressMessages(confint(mod)[-1, ])
        if (is.null(nrow(ci))) {
            dim(ci) <- c(1, length(ci))
        }
        effect.name <- paste0(rownames(tab)[1], ", ", paste(colnames(tab)[-1], colnames(tab)[1], sep = " minus "))
        effect.type <- "Risk difference (Wald CI)"
    }    
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf.level = conf.level)
}

#' Effect measure for association between two factors
#'
#' @param x vector
#' @param y another vector
#' @param conf.level confidence interval level
#'
#' @return a list with five componments
#' @importFrom stats glm binomial confint
#' @export
or.col.by.row <- function (x, y, conf.level = 0.95) {
    tab <- table(y, x)
    if (ncol(tab) <= 1 | nrow(tab) > 2) {
        ## Je ne sais pas quel effet calculer 
        effect <- NULL
        ci <- NULL
        effect.name <- NULL
        effect.type <- NULL
        conf.level <- NULL
    } else {
        ynum <- ifelse(y == rownames(tab)[1], 1, 0)
        mod <- glm(ynum ~ x, family = binomial(link = "logit"))
        effect <- exp(mod$coef)[-1]
        ci <- suppressMessages(exp(confint(mod)[-1, ]))
        if (is.null(nrow(ci))) {
            dim(ci) <- c(1, length(ci))
        }
        effect.name <- paste0(rownames(tab)[1], ", ", paste(colnames(tab)[-1], colnames(tab)[1], sep = " vs "))
        effect.type <- "Odds ratio (Wald CI)"
    }    
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf.level = conf.level)
}

#' Effect measure for association between two factors
#'
#' @param x vector
#' @param y another vector
#' @param conf.level confidence interval level
#'
#' @return a list with five componments
#' @importFrom stats glm binomial confint
#' @export
rr.col.by.row <- function (x, y, conf.level = 0.95) {
    tab <- table(y, x)
    if (ncol(tab) <= 1 | nrow(tab) > 2) {
        ## Je ne sais pas quel effet calculer 
        effect <- NULL
        ci <- NULL
        effect.name <- NULL
        effect.type <- NULL
        conf.level <- NULL
    } else {
        ynum <- ifelse(y == rownames(tab)[1], 1, 0)
        mod <- glm(ynum ~ x, family = binomial(link = "log"))
        effect <- exp(mod$coef)[-1]
        ci <- suppressMessages(exp(confint(mod)[-1, ]))
        if (is.null(nrow(ci))) {
            dim(ci) <- c(1, length(ci))
        }
        effect.name <- paste0(rownames(tab)[1], ", ", paste(colnames(tab)[-1], colnames(tab)[1], sep = " vs "))
        effect.type <- "Relative risk (Wald CI)"
    }    
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf.level = conf.level)
}

#' Effect measure for association between two factors
#'
#' @param x vector
#' @param y another vector
#' @param conf.level confidence interval level
#'
#' @return a list with five componments
#' @importFrom stats glm binomial confint
#' @export
rd.col.by.row <- function (x, y, conf.level = 0.95) {
    tab <- table(y, x)
    if (ncol(tab) <= 1 | nrow(tab) > 2) {
        ## Je ne sais pas quel effet calculer 
        effect <- NULL
        ci <- NULL
        effect.name <- NULL
        effect.type <- NULL
        conf.level <- NULL
    } else {
        ynum <- ifelse(y == rownames(tab)[1], 1, 0)
        mod <- glm(ynum ~ x, family = binomial(link = "logit"))
        effect <- 100*mod$coef[-1]
        ci <- 100*suppressMessages(confint(mod)[-1, ])
        if (is.null(nrow(ci))) {
            dim(ci) <- c(1, length(ci))
        }
        effect.name <- paste0(rownames(tab)[1], ", ", paste(colnames(tab)[-1], colnames(tab)[1], sep = " minus "))
        effect.type <- "Risk difference (Wald CI)"
    }    
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf.level = conf.level)
}

#' Effect measure for association between one continuous and one categorical variable
#'
#' @param x vector
#' @param g another vector
#' @param conf.level confidence interval level
#' @param R number of bootstrap replication
#'
#' @return a list with five componments
#' @importFrom stats sd qnorm bartlett.test shapiro.test t.test
#' @importFrom nortest ad.test
#' @export
diff_mean.auto <- function(x, g, conf.level = 0.95, R = 500) {
    ng <- table(g)
    if (length(ng) <= 1 | length(ng) > 2) {
        ## Je ne sais pas quel effet calculer par défaut quand il existe plus de 2 catégories en colonne
        effect <- NULL
        ci <- NULL
        effect.name <- NULL
        effect.type <- NULL
        conf.level <- NULL
    } else if (length(ng) == 2) {
        effect <- unname(diff(rev(tapply(x, g, mean, na.rm = TRUE))))
        effect.name <- paste(names(ng), collapse = " minus ")
        if (any(ng < 50)) {
            normg <- tapply(x, g, function(x) shapiro.test(x)$p.value)
        } else {
            normg <- tapply(x, g, function(x) ad.test(x)$p.value)
        }
        if (any(normg < 0.05)) {
            beffect <- vector("numeric", R)
            for (i in 1:R) {
                ib <- sample(1:length(x), replace = TRUE)
                xi <- x[ib]
                gi <- g[ib]
                beffect[i] <- unname(diff(rev(tapply(xi, gi, mean, na.rm = TRUE))))
            }
            sd.effect <- sd(beffect)
            ci <- effect + qnorm(c((1-conf.level)/2, 1-(1-conf.level)/2))*sd.effect
            dim(ci) <- c(1, 2)
            effect.type <- "Difference in means (bootstrap CI)"
            
        } else {
            bartlettg <- bartlett.test(x, g)$p.value
            if (bartlettg < 0.05) {
                type <- "t.unequalvar"
                effect.type <- "Difference in means (Welch CI)"
            } else if (bartlettg > 0.05 & length(ng) == 2) {
                type <- "t.equalvar"
                effect.type <- "Difference in means (t-test CI)"
            }
            test <- switch(type,
                           t.unequalvar = t.test(x ~  g, var.equal = FALSE, conf.level = conf.level),
                           t.equalvar = t.test(x ~  g, var.equal = TRUE, conf.level = conf.level))
            ci <- unname(test$conf.int)
            attributes(ci) <- NULL
            dim(ci) <- c(1, 2)
        }
        
    }
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf.level = conf.level)
}

#' Effect measure for association between one continuous and one categorical variable
#'
#' @param x vector
#' @param g another vector
#' @param conf.level confidence interval level
#' @param R number of bootstrap replication
#'
#' @return a list with five componments
#' @importFrom stats sd qnorm 
#' @export
diff_mean.boot <- function(x, g, conf.level = 0.95, R = 500) {
    ng <- table(g)
    if (length(ng) <= 1 | length(ng) > 2) {
        ## Je ne sais pas quel effet calculer par défaut quand il existe plus de 2 catégories en colonne
        effect <- NULL
        ci <- NULL
        effect.name <- NULL
        effect.type <- NULL
        conf.level <- NULL
    } else if (length(ng) == 2) {
        effect <- unname(diff(rev(tapply(x, g, mean, na.rm = TRUE))))
        effect.name <- paste(names(ng), collapse = " minus ")
        beffect <- vector("numeric", R)
        for (i in 1:R) {
            ib <- sample(1:length(x), replace = TRUE)
            xi <- x[ib]
            gi <- g[ib]
            beffect[i] <- unname(diff(rev(tapply(xi, gi, mean, na.rm = TRUE))))
        }
        sd.effect <- sd(beffect)
        ci <- effect + qnorm(c((1-conf.level)/2, 1-(1-conf.level)/2))*sd.effect
        dim(ci) <- c(1, 2)
        effect.type <- "Difference in means (bootstrap CI)"
    }            
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf.level = conf.level)
}

#' Effect measure for association between one continuous and one categorical variable
#'
#' @param x vector
#' @param g another vector
#' @param conf.level confidence interval level
#'
#' @return a list with five componments
#' @importFrom stats bartlett.test t.test
#' @export
diff_mean.student <- function(x, g, conf.level = 0.95) {
    ng <- table(g)
    if (length(ng) <= 1 | length(ng) > 2) {
        ## Je ne sais pas quel effet calculer par défaut quand il existe plus de 2 catégories en colonne
        effect <- NULL
        ci <- NULL
        effect.name <- NULL
        effect.type <- NULL
        conf.level <- NULL
    } else if (length(ng) == 2) {
        effect <- unname(diff(rev(tapply(x, g, mean, na.rm = TRUE))))
        effect.name <- paste(names(ng), collapse = " minus ")
        bartlettg <- bartlett.test(x, g)$p.value
        if (bartlettg < 0.05) {
            type <- "t.unequalvar"
            effect.type <- "Difference in means (Welch CI)"
        } else if (bartlettg > 0.05 & length(ng) == 2) {
            type <- "t.equalvar"
            effect.type <- "Difference in means (t-test CI)"
        }
        test <- switch(type,
                       t.unequalvar = t.test(x ~  g, var.equal = FALSE, conf.level = conf.level),
                       t.equalvar = t.test(x ~  g, var.equal = TRUE, conf.level = conf.level))
        ci <- unname(test$conf.int)
        attributes(ci) <- NULL
        dim(ci) <- c(1, 2)
    }
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf.level = conf.level)
}

#' Effect measure for association between one continuous and one categorical variable
#'
#' @param x vector
#' @param g another vector
#' @param conf.level confidence interval level
#' @param R number of bootstrap replication
#'
#' @return a list with five componments
#' @importFrom stats sd qnorm
#' @export
diff_median <- function(x, g, conf.level = 0.95, R = 500) {
    ng <- table(g)
    if (length(ng) <= 1 | length(ng) > 2) {
        ## Je ne sais pas quel effet calculer par défaut quand il existe plus de 2 catégories en colonne
        effect <- NULL
        ci <- NULL
        effect.type <- NULL
        conf.level <- NULL
    } else if (length(ng) == 2) {
        effect <- unname(diff(rev(tapply(x, g, median, na.rm = TRUE))))
        
        beffect <- vector("numeric", R)
        for (i in 1:R) {
            ib <- sample(1:length(x), replace = TRUE)
            xi <- x[ib]
            gi <- g[ib]
            beffect[i] <- unname(diff(rev(tapply(xi, gi, median, na.rm = TRUE))))
        }
        sd.effect <- sd(beffect)
        ci <- effect + qnorm(c((1-conf.level)/2, 1-(1-conf.level)/2))*sd.effect
        dim(ci) <- c(1, 2)
        effect.type <- "Difference in medians (bootstrap CI)"
    }
    list(effect = effect, ci = ci, effect.type = effect.type, conf.level = conf.level)
}

##' Effect measure for association between one consored variable and one categorical variable
##'
##' @param formula a formula
##' @param conf.level the confidence level required
##'
##' @return a list with two componments: p.value and method
##' @author David Hajage
##' @importFrom stats confint
##' @importFrom survival coxph
##' @export
effect.survival.coxph <- function(formula, conf.level = 0.95) {
    mod <- coxph(formula)
    effect <- exp(mod$coef)
    ci <- suppressMessages(exp(confint(mod)))
    if (is.null(nrow(ci))) {
        dim(ci) <- c(1, length(ci))
    }
    effect.name <- paste(mod$xlevels[[1]][-1], mod$xlevels[[1]][1], sep = " vs ")
    effect.type <- "Hazard ratio (Wald CI)"
    
    list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf.level = conf.level)
}




if(FALSE){
    
    ## Calculs des effets de traitement
    
    # prop.test.multi <- function(x, y, conf.level = 0.95) {
    #     # require(PropCIs)
    #     tab <- table(x, y)
    #     n <- colSums(tab)
    #     res <- matrix(nrow = nrow(tab), ncol = 2)
    #     for (i in 1:nrow(tab)) {
    #         # res[i, ] <- diffscoreci(tab[i, 1], n[1], tab[i, 2], n[2], conf.level)$conf.int
    #         res[i, ] <- prop.test(tab[i, ], n, conf.level = conf.level)$conf.int
    #     }
    #     res
    # }
    
    # rd.row.by.col <- function (x, y, conf.level = 0.95) {
    #     tab <- table(x, y)
    #     if (ncol(tab) <= 1 | ncol(tab) > 2) {
    #         ## Je ne sais pas quel effet calculer par défaut quand il existe plus de 2 catégories en colonne
    #         effect <- NULL
    #         ci <- NULL
    #         effect.name <- NULL
    #         effect.type <- NULL
    #         conf.level <- NULL
    #     } else if (ncol(tab) == 2) {
    #         ptab <- prop.table(tab, 2)
    #         effect <- 100*(ptab[, 1] - ptab[, 2])
    #         
    #         ci <- 100*prop.test.multi(x, y, conf.level = conf.level)
    #         effect.name <- paste0(paste0(colnames(tab), collapse = " minus "), ", ", names(effect))
    #         effect.type <- "Difference in risks (Newcombe CI)"
    #     }    
    #     list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf.level = conf.level)
    # }
    
    # rr.multi <- function(x, y, conf.level = 0.95) {
    #     tab <- table(x, y)
    #     n <- colSums(tab)
    #     res <- matrix(nrow = nrow(tab), ncol = 2)
    #     for (i in 1:nrow(tab)) {
    #         logRR <- log((tab[i, 1]/1/n[1])/(tab[i, 2]/n[2]))
    #         selogRR <- sqrt(1/tab[i, 1] - 1/n[1] + 1/tab[i, 2] - 1/n[2])
    #         res[i, ] <- exp(logRR + qnorm(c((1-conf.level)/2, 1-(1-conf.level)/2))*selogRR)
    #     }
    #     res
    # }
    
    # rr.row.by.col <- function (x, y, conf.level = 0.95) {
    #     tab <- table(x, y)
    #     if (ncol(tab) <= 1 | ncol(tab) > 2) {
    #         ## Je ne sais pas quel effet calculer par défaut quand il existe plus de 2 catégories en colonne
    #         effect <- NULL
    #         ci <- NULL
    #         effect.name <- NULL
    #         effect.type <- NULL
    #         conf.level <- NULL
    #     } else if (ncol(tab) == 2) {
    #         ptab <- prop.table(tab, 2)
    #         effect <- (ptab[, 1]/ptab[, 2])
    #         
    #         ci <- rr.multi(x, y, conf.level = conf.level)
    #         effect.name <- paste0(paste0(colnames(tab), collapse = "/"), ", ", names(effect))
    #         effect.type <- "Relative risk (Delta method CI)"
    #     }    
    #     list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf.level = conf.level)
    # }
    
    # or.multi <- function(x, y, conf.level = 0.95) {
    #     tab <- table(x, y)
    #     ptab <- prop.table(tab, 2)
    #     logOR <- log((ptab[, 1]/(1-ptab[, 1]))/(ptab[, 2]/(1-ptab[, 2])))
    #     selogOR <- sqrt(1/tab[1, 1] + 1/tab[1, 2] + 1/tab[2, 1] + 1/tab[2, 2])
    #     res <- matrix(nrow = nrow(tab), ncol = 2)
    #     for (i in 1:nrow(tab)) {
    #         res[i, ] <- exp(logOR[i] + qnorm(c((1-conf.level)/2, 1-(1-conf.level)/2))*selogOR)
    #     }
    #     res
    # }
    # 
    # or.row.by.col <- function (x, y, conf.level = 0.95) {
    #     tab <- table(x, y)
    #     if (ncol(tab) <= 1 | ncol(tab) > 2) {
    #         ## Je ne sais pas quel effet calculer par défaut quand il existe plus de 2 catégories en colonne
    #         effect <- NULL
    #         ci <- NULL
    #         effect.name <- NULL
    #         effect.type <- NULL
    #         conf.level <- NULL
    #     } else if (ncol(tab) == 2) {
    #         ptab <- prop.table(tab, 2)
    #         effect <- (ptab[, 1]/(1-ptab[, 1]))/(ptab[, 2]/(1-ptab[, 2]))
    #         
    #         ci <- or.multi(x, y, conf.level = conf.level)
    #         effect.name <- paste0(paste0(colnames(tab), collapse = "/"), ", ", names(effect))
    #         effect.type <- "Odds ratio (Delta method CI)"
    #     }    
    #     list(effect = effect, ci = ci, effect.name = effect.name, effect.type = effect.type, conf.level = conf.level)
    # }
}