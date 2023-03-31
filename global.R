if (!require("pacman")) {
  install.packages("pacman") #v.0.5.1
}
pkg <- c("shiny", 
         "shinydashboard",
         "shinydashboardPlus", 
         "shinyWidgets", 
         "lmerTest", 
         "hnp", 
         "DescTools",
         "DT", 
         "lcc", 
         "devtools",
         "plotly", 
         "splines",
         "GGally", 
         "kableExtra", 
         "lme4",
         "magrittr",
         "measurements",
         "ggridges",
         "tidyverse",
         "rlang",
         "gtools")
version <- c("1.7.4", 
             "0.7.2",
             "2.0.3",
             "0.7.5", 
             "3.1.3",
             "1.2.6",
             "0.99.47",
             "0.26",
             "1.1.4",
             "2.4.5",
             "4.10.1",
             "4.1.3",
             "2.1.2",
             "1.3.4",
             "1.1.29",
             "2.0.3",
             "1.5.0",
             "0.5.4",
             "1.3.2",
             "1.1.0",
             "3.9.4"
)
data.frame(pkg, version)

p_install_version(
  pkg,
  version
)

if (!require("d3heatmap")) {
  devtools::install_github("talgalili/d3heatmap")
}
#=======================================================================
# Loading packages
#=======================================================================
lapply(pkg, library, character.only = TRUE)
library(d3heatmap)


#-----------------------------------------------------------------------
# Data
#-----------------------------------------------------------------------
data3 <- read.csv("./www/2017-2018-regular.csv")
data4 <- read.csv("./www/2018-2019-regular.csv")
data3$Year <- "./www/2017-2018"
data4$Year <- "./www/2018-2019"

data <- smartbind(data3, data4)
#=======================================================================
# Removing PerGame
#=======================================================================
dim(data)
data <- data %>%
  select(-contains("PerGame")) %>%
  droplevels()
#-----------------------------------------------------------------------
# Organizing the data
#-----------------------------------------------------------------------
data$X.Home.Team.ID <- as.factor(data$X.Home.Team.ID)
data$X.Player.ID <- as.factor(data$X.Player.ID)
#-----------------------------------------------------------------------
options(warn=-1)
newHeight <- data %>%
  tidyr::separate(X.Height, into = c("X.Height1", "X.Height2"), convert = TRUE) %>%
  transmute(X.Height = conv_unit(X.Height1, "ft", "cm") +
              conv_unit(X.Height2, "inch", "cm"))
data$X.Height <- newHeight[, 1]
newWeight <- data %>%
  transmute(X.Weight = conv_unit(X.Weight, "lbs", "kg"))
data$X.Weight <- newWeight[, 1]
colnames(data) <- gsub(x = colnames(data), pattern = "X.",
                       replacement = "")
data$GamesStarted <- as.factor(data$GamesStarted)
data$FoulFlag2 <- as.factor(data$FoulFlag2)
options(warn=0)
#-----------------------------------------------------------------------
# Calculating PCA Score
#-----------------------------------------------------------------------
cols <- c("Game.ID", "Away.Team.ID", "Jersey.Num", "Team.ID")
data[cols] <- lapply(data[cols], factor)
data.shiny <- dplyr::select_if(data, is.numeric)
#-----------------------------------------------------------------------
CorS <- cor(data.shiny, use = "pairwise.complete.obs",
            method = c("spearman"))
CorS <- CorS[rowSums(!is.na(CorS))!=0, colSums(!is.na(CorS))!=0]
cc <- rainbow(ncol(CorS), start = 0, end = .3)
#-----------------------------------------------------------------------
# Functions
#-----------------------------------------------------------------------
ggplotRegression <- function(data, inp1, inp2){
  ggplot(data.shiny, aes(x = data[, inp2],
                         y = data[, inp1])) +
    geom_point(shape = 1) +
    stat_smooth(method = "lm", col = "blue") +
    scale_fill_grey() +
    labs(x = inp2, y = inp1) +
     theme(legend.position='none',legend.justification=c(1,0),
            axis.title=element_text(size= "12", color="Black"),
            panel.background = element_rect(fill = "white"),
            legend.key.size=unit(0.5,"cm"),legend.key.width=unit(1.4,"cm"),
            axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
}

fitRegression <- function(data, inp1, inp2){
  print.summary.lm2(
    summary.lm2(lm(data[, inp1] ~ data[, inp2], data = data)),
    inp2 = inp2
  )
}
#-----------------------------------------------------------------------
# Print lm
#-----------------------------------------------------------------------
summary.lm2 <- function(object, correlation = FALSE,
                        symbolic.cor = FALSE, ...)
{
    z <- object
    p <- z$rank
    rdf <- z$df.residual
    if (p == 0) {
        r <- z$residuals
        n <- length(r)
        w <- z$weights
        if (is.null(w)) {
            rss <- sum(r^2)
        }
        else {
            rss <- sum(w * r^2)
            r <- sqrt(w) * r
        }
        resvar <- rss/rdf
        ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
        class(ans) <- "summary.lm"
        ans$aliased <- is.na(coef(object))
        ans$residuals <- r
        ans$df <- c(0L, n, length(ans$aliased))
        ans$coefficients <- matrix(NA_real_, 0L, 4L, dimnames = list(NULL,
            c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
        ans$sigma <- sqrt(resvar)
        ans$r.squared <- ans$adj.r.squared <- 0
        ans$cov.unscaled <- matrix(NA_real_, 0L, 0L)
        if (correlation)
            ans$correlation <- ans$cov.unscaled
        return(ans)
    }
    if (is.null(z$terms))
        stop("invalid 'lm' object:  no 'terms' component")
    if (!inherits(object, "lm"))
        warning("calling summary.lm(<fake-lm-object>) ...")
    Qr <- stats:::qr.lm(object)
    n <- NROW(Qr$qr)
    if (is.na(z$df.residual) || n - p != z$df.residual)
        warning("residual degrees of freedom in object suggest this is not an \"lm\" fit")
    r <- z$residuals
    f <- z$fitted.values
    w <- z$weights
    if (is.null(w)) {
        mss <- if (attr(z$terms, "intercept"))
            sum((f - mean(f))^2)
        else sum(f^2)
        rss <- sum(r^2)
    }
    else {
        mss <- if (attr(z$terms, "intercept")) {
            m <- sum(w * f/sum(w))
            sum(w * (f - m)^2)
        }
        else sum(w * f^2)
        rss <- sum(w * r^2)
        r <- sqrt(w) * r
    }
    resvar <- rss/rdf
    if (is.finite(resvar) && resvar < (mean(f)^2 + var(c(f))) *
        1e-30)
        warning("essentially perfect fit: summary may be unreliable")
    p1 <- 1L:p
    R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
    se <- sqrt(diag(R) * resvar)
    est <- z$coefficients[Qr$pivot[p1]]
    tval <- est/se
    ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
    ans$residuals <- r
    ans$coefficients <- cbind(Estimate = est, `Std. Error` = se,
        `t value` = tval, `Pr(>|t|)` = 2 * pt(abs(tval), rdf,
            lower.tail = FALSE))
    ans$aliased <- is.na(z$coefficients)
    ans$sigma <- sqrt(resvar)
    ans$df <- c(p, rdf, NCOL(Qr$qr))
    if (p != attr(z$terms, "intercept")) {
        df.int <- if (attr(z$terms, "intercept"))
            1L
        else 0L
        ans$r.squared <- mss/(mss + rss)
        ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n -
            df.int)/rdf)
        ans$fstatistic <- c(value = (mss/(p - df.int))/resvar,
            numdf = p - df.int, dendf = rdf)
    }
    else ans$r.squared <- ans$adj.r.squared <- 0
    ans$cov.unscaled <- R
    dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1,
        1)]
    if (correlation) {
        ans$correlation <- (R * resvar)/outer(se, se)
        dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
        ans$symbolic.cor <- symbolic.cor
    }
    if (!is.null(z$na.action))
        ans$na.action <- z$na.action
    class(ans) <- "summary.lm"
    ans
}
#-----------------------------------------------------------------------
# Print lm
#-----------------------------------------------------------------------
print.summary.lm2 <-
  function (x, digits = max(3, getOption("digits") - 3),
            symbolic.cor = x$symbolic.cor, inp2,
            signif.stars = getOption("show.signif.stars"), ...)
  {
#    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
#        "\n\n", sep = "")
    resid <- x$residuals
    df <- x$df
    rdf <- df[2L]
    cat(if (!is.null(x$w) && diff(range(x$w)))
      "Weighted ", "Residuals:\n", sep = "")
    if (rdf > 5L) {
      nam <- c("Min", "1Q", "Median", "3Q", "Max")
      rq <- if (length(dim(resid)) == 2L)
              structure(apply(t(resid), 1L, quantile),
                        dimnames = list(nam, dimnames(resid)[[2L]]))
      else {
        zz <- zapsmall(quantile(resid), digits + 1)
        structure(zz, names = nam)
      }
      print(rq, digits = digits, ...)
    }
    else if (rdf > 0L) {
      print(resid, digits = digits, ...)
    }
    else {
      cat("ALL", df[1L],
          "residuals are 0: no residual degrees of freedom!\n")
    }
    if (length(x$aliased) == 0L) {
      cat("\nNo Coefficients\n")
    }
    else {
      if (nsingular <- df[3L] - df[1L])
        cat("\nCoefficients: (", nsingular,
            " not defined because of singularities)\n",
            sep = "")
        else cat("\nCoefficients:\n")
      coefs <- x$coefficients
      if (!is.null(aliased <- x$aliased) && any(aliased)) {
        cn <- names(aliased)
        coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn,
                                                                colnames(coefs)))
        coefs[!aliased, ] <- x$coefficients
      }
      rownames(coefs) <- c("(Intercept)", inp2)
      printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
                   na.print = "NA", ...)
    }
    cat("\ns:", format(signif(x$sigma, digits)), "on", rdf,
        "degrees of freedom\n")
    if (nzchar(mess <- naprint(x$na.action)))
      cat("  (", mess, ")\n", sep = "")
    if (!is.null(x$fstatistic)) {
      cat("Multiple R-squared:", formatC(x$r.squared, digits = digits))
      cat(",\nAdjusted R-squared:", formatC(x$adj.r.squared,
                                            digits = digits),
          "\nF-statistic:", formatC(x$fstatistic[1L],
                                    digits = digits), "on",
          x$fstatistic[2L], "and",
          x$fstatistic[3L], "DF,  p-value:",
          format.pval(pf(x$fstatistic[1L],
                         x$fstatistic[2L],
                         x$fstatistic[3L], lower.tail = FALSE),
                      digits = digits), "\n")
    }
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- NCOL(correl)
        if (p > 1L) {
          cat("\nCorrelation of Coefficients:\n")
          if (is.logical(symbolic.cor) && symbolic.cor) {
            print(symnum(correl, abbr.colnames = NULL))
          }
          else {
            correl <- format(round(correl, 2), nsmall = 2,
                  digits = digits)
            correl[!lower.tri(correl)] <- ""
            print(correl[-1, -p, drop = FALSE], quote = FALSE)
          }
        }
    }
    cat("\n")
    invisible(x)
  }
#=======================================================================
# PCA
#=======================================================================
load("./www/score.RData")
score$Score$Year <- as.factor(score$Score$Year)
score_data <- score$Score
saveRDS(score_data, file = "./www/score_data.rds")
score_data <- readRDS("./www/score_data.rds")
load("./www/pca-cor.RData")
load("./www/data.selected.RData")
#=======================================================================
# Model
#=======================================================================
load("./www/model-final.RData")
#=======================================================================
# Test data
#=======================================================================
load("./www/new-data.RData")
load("./www/new-model.RData")
load("./www/eigenvectors.RData")
pred.data <- test.data
pred.data$Type <- as.factor(as.numeric(as.character(pred.data$Home.Team.ID)) ==
                              as.numeric(as.character(pred.data$Team.ID)))
levels(pred.data$Type) <- c("Away", "Home")

match.var2 <- match(tolower(rownames(A)),
                   tolower(colnames(pred.data)))
pred.var <- pred.data[match.var2]

sc <- function(x, center, scale, CV) {
    CV * (x - center)/scale
  }
for (i in 1:length(score$scale)) {
  pred.var[, i] <-  sc(x = pred.var[, i], center = score$center[i],
                       scale = score$scale[i], CV = score$CV[i])
}
newpredvars <- as.matrix(pred.var) %*% as.matrix(A)
pred.data <- data.frame(pred.data, newpredvars)
test.data <- pred.data %>%
  filter(Birth.Country %in% levels(as.factor(data$Birth.Country))) %>%
  droplevels()
#=======================================================================
load("./www/bp-team.RData")
#=======================================================================
# Best players
#=======================================================================
load("./www/data-model.RData")
MinMax <- function(x, min.s, max.s) {
  ((x - min.s) * 100)/(max.s - min.s)
}
#=======================================================================
anova_model <- anova(model.final)
saveRDS(anova_model, file = "./www/anova_model.rds")
#-----------------------------------------------------------------------
