#' Title
#'
#' @param effect effect
#' @param digits digits
#'
#' @export
display.effect <- function(effect, digits = 4) {
    if (all(sapply(effect, is.null))){
        warning("Could not calculate effect. Is there not 2 groups exactly?", immediate.=T)
        return("No effect?")
    }else {
        paste(paste0(effect$effect.type, " (", effect$effect.name, "): ", formatC(effect$effect, format = "f", digits = digits), " CI", effect$conf.level*100, "%[", paste(formatC(effect$ci[, 1], format = "f", digits = digits), formatC(effect$ci[, 2], format = "f", digits = digits), sep = " to "), "]"), collapse = "\n")
    }
}

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

#' Effect measure for association between two factors
#'
#' @param x vector
#' @param y another vector
#' @param conf.level confidence interval level
#'
#' @return a list with five componments
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
    } else {
        xnum <- ifelse(x == rownames(tab)[1], 1, 0)
        mod <- glm(xnum ~ y, family = binomial(link = "logit"))
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
        mod <- glm(xnum ~ y, family = binomial(link = "identity"))
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
        mod <- glm(ynum ~ x, family = binomial(link = "identity"))
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
#' @param y another vector
#' @param conf.level confidence interval level
#' @param R number of bootstrap replication
#'
#' @return a list with five componments
#' @importFrom nortest ad.test
#' @export
diff.mean.auto <- function(x, g, conf.level = 0.95, R = 500) {
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
#' @param y another vector
#' @param conf.level confidence interval level
#' @param R number of bootstrap replication
#'
#' @return a list with five componments
#' @export
diff.mean.boot <- function(x, g, conf.level = 0.95, R = 500) {
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
#' @param y another vector
#' @param conf.level confidence interval level
#'
#' @return a list with five componments
#' @export
diff.mean.student <- function(x, g, conf.level = 0.95) {
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
#' @param y another vector
#' @param conf.level confidence interval level
#' @param R number of bootstrap replication
#'
#' @return a list with five componments
#' @export
diff.median <- function(x, g, conf.level = 0.95, R = 500) {
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
##' @return a list with two componments: p.value and method
##' @author David Hajage
##' @export
##' @import survival
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

