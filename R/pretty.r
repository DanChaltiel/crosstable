rdv <- function(x, ...) {
    UseMethod("rdv")
}

remove.term <- function(f, term) {
    lr <- left_right(f)
    l <- lr$left
    r <- lr$right
    r2 <- r[!grepl(paste("^", term, "$", sep = ""), r) & !grepl(paste("^", term, ":", sep = ""), r) & !grepl(paste(":", term, "$", sep = ""), r) & !grepl(paste(":", term, ":", sep = ""), r)]
    paste(l, "~", paste(r2, collapse = "+"))
}

rdv.glm <- function(x, ...) {

}

rdv.coxph <- function(x, ...) {

}

pretty <- function (x, ...) {
    UseMethod("pretty")
}

pretty.glm <- function(x, ...) {

}

pretty.coxph <- function(x, ...) {

}
