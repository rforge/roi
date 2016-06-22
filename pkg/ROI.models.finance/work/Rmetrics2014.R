################################################################################
## ROI - Intro
## Author: Stefan Theussl (c) 2014
## Licence: GPL-2
################################################################################


## Example:
## (Mixed Integer) Linear Programming Example.

require( "Rglpk" )
?Rglpk_solve_LP

## Simple mixed integer linear program.
## maximize:    3 x_1 + 1 x_2 + 3 x_3
## subject to: -1 x_1 + 2 x_2 +   x_3 <= 4
##                      4 x_2 - 3 x_3 <= 2
##                x_1 - 3 x_2 + 2 x_3 <= 3
##                x_1, x_3 are non-negative integers
##                x_2 is a non-negative real number

obj <- c(3, 1, 3)
mat <- matrix(c(-1, 0, 1, 2, 4, -3, 1, -3, 2), nrow = 3)
dir <- c("<=", "<=", "<=")
rhs <- c(4, 2, 3)
types <- c("I", "C", "I")
max <- TRUE

sol <- Rglpk_solve_LP(obj, mat, dir, rhs, types = types, max = max)


################################################################################
## ROI
################################################################################

require( "ROI" )

## Which solvers are installed (looks for plug-in packages in package
## library).
ROI_available_solvers()
## which solvers are registered with ROI
ROI_registered_solvers()


## Constructing and computing on optimization problems.
## Example from above.
## Note: using as.L_objective() is also possible
obj <- L_objective( obj )

cnstr <- L_constraint( c(-1, 2, 1), "<=", 4 )
cnstr

cnstr <- rbind( cnstr, L_constraint(mat[-1, ], dir[-1], rhs[-1]) )
cnstr

op <- OP( obj, cnstr, types = types, maximum = max )

objective( op )
objective( op )( sol$solution )
length( objective(op) )

constraints( op )$L
length( constraints(op) )

## Solve the problem
sol <- ROI_solve( op, solver = "glpk" )
sol

unclass( sol )

## Currently it is not not straightforward to find a proper solver for
## solving a given problem, only by using this workaround:
names( ROI:::get_solver_methods(ROI:::OP_signature(op)) )

ROI_solve( op, solver = "symphony" )$solution


################################################################################
## Portfolio Optimization
################################################################################

## Example:
## Risk-return tradeof of stocks.

## Package where we collect portfolio models (work in progress):
## install.packages( "ROI.models.finance", repos = "http://r-forge.r-project.org" )
require( "ROI.models.finance" )

## Public domain data from quandl (http://blog.quandl.com/blog/quandl-open-data/).
data( US30 )
head( US30 )
tail( US30 )
par( mfrow = c(2,1) )
plot( US30[, "IBM"], main = "IBM" )
plot( US30[, "JPM"], main = "JPMorgan Chase" )
par( mfrow = c(1,1) )

## We need some helper functions for financial time series.
require( "TTR" )

## Next, calculate weekly simple returns for each asset in the index.
r <- na.omit( ROC(US30[endpoints(index(US30), on = "weeks")], type = "discrete") )

## Scatterplot of asset returns
plot( x = as.numeric(r[,"IBM"]), y = as.numeric(r[,"JPM"]) )

## Covariance matrix between all assets in the index
S <- cov( r )

## Risk return trade-off
mu <- apply( r, 2, mean )
sigma <- apply( r, 2, sd )
plot( sigma, mu, xlim = c(0, max(sigma)*1.1), ylim = c(0, max(mu)*1.1),
      main = "Risk-Return Tradeoff" )
text( x = sigma, y = mu, labels = colnames(US30), pos = 3 )

## Equal-weights portfolio
w <- rep( 1/length(mu), length(mu) )
ew <- c( sqrt(w %*% S %*% w),
         w %*% mu )
points( x = ew[1], y = ew[2], col = "orange" )
text( x = ew[1], y = ew[2], labels = "EW", pos = 3, col = "orange" )


## Example:
## MV Optimization using ROI (long-only, fully invested).

## Constraints:
full_invest <- L_constraint( rep(1, ncol(r)), "==", 1 )
long_only <- L_constraint( diag(1, ncol(r), ncol(r) ), rep(">=", ncol(r)), rep(0, ncol(r)) )

## Specify minimum variance problem object:
MV <- OP( objective = Q_objective(S),
          constraints = c(full_invest, long_only) )

res <- ROI_solve( MV, solver = "quadprog" )
names( res$solution ) <- colnames( US30 )

## Optimal weights:
round( res$solution*100, 2 )

mv <- c( sqrt(res$solution %*% S %*% res$solution),
        res$solution %*% mu )
points( x = mv[1], y = mv[2], col = "green" )
text( x = mv[1], y = mv[2], labels = "MV", pos = 3, col = "green" )

## This function samples for given asset properties n random
## portfolios in risk/return space.
make_random_portfolios <- function( MU, COV, n ){
    u <-  matrix( runif(n * length(MU)), nrow = n )
    constituents <- sample( 1:length(MU), n, replace = TRUE )
    l <- matrix( FALSE, nrow = nrow(u), ncol = ncol(u) )
    for( i in 1:length(constituents) )
        l[i, sample(1:length(mu), constituents[i])] <- TRUE
    u <- ifelse( l, u, 0 )
    w <- sweep( u, 1, rowSums(u), "/" )
    sigma <- apply( w, 1, function(x) sqrt(x %*% COV %*% x) )
    mu <- apply( w, 1, function(x) x %*% MU  )
    list( sd = sigma, mu = mu, w )
}

rpf <- make_random_portfolios( mu, S, 10^5 )
plot( sigma, mu, xlim = c(0, max(sigma)*1.1), ylim = c(0, max(mu)*1.1),
      main = "Risk-Return Tradeoff" )
points( cbind( rpf[["sd"]], rpf[["mu"]] ), col = "lightgrey" )
points( sigma, mu )
text( x = sigma, y = mu, labels = colnames(US30), pos = 3 )
points( x = ew[1], y = ew[2], col = "orange" )
text( x = ew[1], y = ew[2], labels = "EW", pos = 3, col = "orange" )
points( x = mv[1], y = mv[2], col = "green" )
text( x = mv[1], y = mv[2], labels = "MV", pos = 3, col = "green" )


## Example:
## Mean-Variance Portfolio.

## Say, we like the return of the EW protfolio, but with minimum risk.
target_return <- L_constraint( mu, ">=", ew[2] )

constraints(MV)
op <- MV
constraints(op) <- rbind( constraints(MV), target_return )

res <- ROI_solve( op, solver = "quadprog" )
names( res$solution ) <- colnames( US30 )

round( res$solution*100, 2 )

mvtr <- c( sqrt(res$solution %*% S %*% res$solution),
        res$solution %*% mu )
points( x = mvtr[1], y = mvtr[2], col = "blue" )
text( x = mvtr[1], y = mvtr[2], labels = "MeanVar", pos = 3, col = "blue" )


## Example:
## Calculate efficient frontier.

## define the grid from MV portfolio
grid_start <- mv[2]
grid_end <- mu[ which.max(mu) ]
grid_points <- 1000
grid <- seq( from = grid_start, to = grid_end, length.out = 1000 )

l <- lapply( grid, function(r) {
    target_return <- L_constraint( mu, ">=", r )
    op <- MV
    constraints(op) <- c( constraints(op), target_return )
    res <- ROI_solve( op, solver = "quadprog" )
    c( sqrt(res$solution %*% S %*% res$solution), r )
} )

efficient_frontier <- do.call( rbind, l )
lines( efficient_frontier, col = "red" )


## Example:
## Integer Programming - Constructing an Index Fund (from
## Cornuejols and Tuetuencue, 2008).

## We use approx. 60 months for calibration and 12 months for
## testing. Out of simplicity we take only stocks into account, which
## where included in the US30 index at all time from early 2008 to the
## end of 2013.
r <- na.omit( ROC(US30[endpoints(index(US30["::20121231"]), on = "weeks")], type = "discrete" ) )

## We'd like to choose 15 stocks to replicate the index
q <- 15


## Now, build the optimization problem:

## Coefficients to objective variables (similarities between assets).
rho <- as.numeric( cor(r) )

## Number of assets.
n <- ncol( r )

## The objective function.
Z <- L_objective( c(rho, rep(0, n)) )

## The first constraint.
min_stocks_included <- L_constraint( c(rep(0, length(rho)), rep(1, n)), "==", q )

## Expand additional constraint matrix 1.
full_block <- matrix( 0, nrow = n, ncol = dim(Z$L)[2] )
for( i in 1:n ){
    idx_start <- (i-1)*n+1
    full_block[ i, idx_start:(idx_start+n-1) ] <- 1
}
sum_most_similar <- L_constraint( full_block, rep("==", n), rep(1, n) )

## Expand additional constraint matrix 2.
block_x <- diag( 1, length(rho) )
block_y <- matrix( rep(diag( -1,  n ), n ), ncol = n, byrow = TRUE )
x_ij_constraints <- L_constraint( cbind(block_x, block_y),
                                  rep("<=", nrow(block_x)),
                                  rep(0,  nrow(block_x)) )

## The ROI problem object.
op <- OP( Z, c(min_stocks_included, sum_most_similar, x_ij_constraints),
          types = rep("B", length(Z)), maximum = TRUE )

## Solve the specified problem using an LP solver.
sol <- ROI_solve( op, solver = "glpk" )

## Calculate and print the assets included in the index fund.
idx_included <- sol$solution[ (length(sol$solution)-n+1):length(sol$solution) ]
names( idx_included ) <- colnames( r )

idx_included

