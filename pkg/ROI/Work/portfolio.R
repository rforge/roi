################################################################################
## Package: ROI
## File:    portfolio.R
## Content: Selected examples of the book by Diethelm Wuertz et al:
##          'Advanced Portfolio Optimization with R/Rmetrics'.
## Author:  Stefan Theussl
## Changed: 2011-10-04
################################################################################

require("ROI")

## Example: MAD Portfolio
##          Chapter 18.2 in Wuertz et al (2011)
##############################################################
require("fPortfolio")

## Prepare data
nAssets <- 6
data <- 100 * LPP2005REC[1:18, 1:nAssets]
nScenarios <- nrow(data)
Mean <- colMeans(data)
Data <- data - matrix( rep(Mean, nScenarios), byrow = TRUE, ncol = nAssets)
targetReturn <- mean( data )

## Modelling:
## - Objective
obj <- L_objective( c(rep(0, nAssets),  rep(1/nScenarios, nScenarios)) )

## - Constraints
MAD.LE <- L_constraint( cbind(as.matrix(Data), -diag(nScenarios)), rep("<=", nScenarios), rep(0, nScenarios) )
MAD.GE <- L_constraint( cbind(as.matrix(Data),  diag(nScenarios)), rep(">=", nScenarios), rep(0, nScenarios) )
RETURN <- L_constraint( c(Mean, rep(0, nScenarios)), "==", targetReturn )
BUDGET <- L_constraint( c(rep(1, nAssets), rep(0, nScenarios)), "==", 1 )
X <- L_constraint( cbind(diag(nAssets), matrix(rep(0, nScenarios*nAssets), nrow = nAssets)), rep(">=", nAssets), rep(0, nAssets) )
WEIGHTS <- L_constraint( cbind(matrix(rep(0, nAssets*nScenarios), ncol = nAssets), diag(nScenarios)), rep(">=", nScenarios), rep(0, nScenarios) )


MAD <- OP( objective = obj,
           constraints = c(MAD.LE, MAD.GE, RETURN, BUDGET, X, WEIGHTS) )

## OK
## for cplex we need cplex/ampl interface
#ROI_solve( MAD, solver = "solveLP", control = list(solver = "snopt", invoke = "AMPL") )
#ROI_solve( MAD, solver = "solveLP", control = list(solver = "symphony", invoke = "AMPL") )

ROI_solve( MAD, solver = "glpk" )
ROI_solve( MAD, solver = "symphony" )
## FAILS
#ROI_solve( MAD, solver = "quadprog" )

## Example: Mean/Variance (Markowitz) Portfolio
##          Chapter xx.x in Wuertz et al (2011)
##############################################################

nAssets <- 6
data <- 100 * LPP2005REC[, 1:nAssets]
Mean <- colMeans(data)
targetReturn <- mean( data )

MIN_RISK <- Q_objective( Q = cov(data), L = rep(0, ncol(data)) )
FULL_INVEST <- L_constraint( rep(1, ncol(data)), "==", 1 )
RETURN <- L_constraint( Mean, "==", targetReturn )

MV <- OP( objective = MIN_RISK,
          constraints = c(FULL_INVEST, RETURN) )

sol <- ROI_solve( MV, solver = "quadprog" )
sol

w <- round(sol$solution, 4)
w

sqrt(t(w) %*% cov(data) %*% w)

