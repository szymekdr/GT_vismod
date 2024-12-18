# helpers

lumpest <- function(model) {
    est1 <- summary(model)$coefficients["TypeBright", "Estimate"]
    est2 <- summary(model)$coefficients["TypeBright:AgeA2", "Estimate"]
    est <- est1 + est2

    vcv <- vcov(model, correlation = FALSE)
    se1 <- summary(model)$coefficients["TypeBright", "Std. Error"]
    se2 <- summary(model)$coefficients["TypeBright:AgeA2", "Std. Error"]
    se <- sqrt(se1^2 + se2^2 + 2 * vcv["TypeBright", "TypeBright:AgeA2"])

    return(c(Estimate = est, SE = se))
}

starring <- function(lumpest) {
    zet <- lumpest[1] / lumpest[2]
    pval <- 2 * (1 - pnorm(abs(zet)))
    if (pval < 0.001) {
        stars <- "***"
    } else if (pval < 0.01) {
        stars <- "**"
    } else if (pval < 0.05) {
        stars <- "*"
    } else {
        stars <- ""
    }

    return(stars)
}
