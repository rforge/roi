require("ROI.models.finance")
source("../R/portfolio.R")

require("ROI")
## get standard test data from Rmetrics Portfolio book
require("fPortfolio")
data("LPP2005.RET")
lppData <- 100 * LPP2005.RET[, 1:6]

prob <- ROI_model_mean_variance_portfolio( lppData, target = mean(lppData), minrisk = TRUE, fullinvest = TRUE )

out <- ROI_solve(prob, "quadprog")
out

out <- ROI_solve(prob, "cplex")
w <- out$sol
## wir muessen beim bauen der anderen Variante noch schummeln
## TODO: rbind.constraints general!!!


x <- lppData
sigma <- sqrt(t(w) %*% cov(x) %*% w)
full_invest <- L_constraint( L = rep(1, ncol(x)), dir = "==", rhs = 1)

prob <- OP( colMeans(x), Q_constraint(Q = list(cov(x), NULL), L = rbind(rep(0,
                                                              ncol(x)), rep(1, ncol(x))), c("<=", "<="), c(sigma^2, 1)), maximum = TRUE )

out <- ROI_solve(prob, "cplex")
