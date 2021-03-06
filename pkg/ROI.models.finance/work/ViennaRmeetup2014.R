################################################################################
## ROI - Intro
## Author: Stefan Theussl
## Licence: GPL-2
################################################################################



################################################################################
## PART I: Optimization general
################################################################################

## Let's start with the nonlinear case and general purpose continuous solvers

## tools in base R
?optim
?nlminb

?constrOptim
?nlm
?optimize

## for a recent discussion on these solvers, see http://www.jstatsoft.org/v43/i09/paper


## Example 1)
## Nonlinear optimization of the function from the slides

## generate a 3D Plot
x <- y <- seq(from = -2.5, to = 2.5, length.out = 100)
f <- function(x, y){
    3*(1-x)^2*exp(-x^2-(y+1)^2) - 10 * (x/5 - x^3 - y^5) * exp(-x^2 - y^2) - 1/3 * exp(-(x+1)^2 - y^2)
}
z <- outer(x, y, f)
pp <- persp(x, y, z,
#            main="Function to be optimized",
            col="lightgreen",
            theta = -30, phi = 20, r = 10, d = 1, expand = 1,
            ltheta = 90, lphi = 180,shade = 0.75,
            ticktype = "detailed",
            nticks = 5)
fun <- function(x){
    3*(1-x[1])^2*exp(-x[1]^2-(x[2]+1)^2) - 10 * (x[1]/5 - x[1]^3 - x[2]^5) * exp(-x[1]^2 - x[2]^2) -
    1/3 * exp(-(x[1]+1)^2 - x[2]^2)
}

## find optimum using the "Nelder-Mead" algorithm
start <- c( 0, 0 )
optim( start, fun, method = "Nelder-Mead" )

## optimization using a multistart procedure
start <- list( c(0, 0), c(-1, -1), c(0, -1), c(0, 1) )
sol <- lapply( start, function(par)
               optim(par, fun, method = "Nelder-Mead",
                     control = list(maxit = 1000000, beta = 0.01, reltol = 1e-15)) )

sapply( sol, function(x) x$value )


## Example 2a)

## travelling salesperson (see optim help)
## optim via SANN heuristics

## from optim() man page
eurodistmat <- as.matrix(eurodist)

## calculate coordinates using the distance matrix
loc <- -cmdscale(eurodist, add = TRUE)$points
x <- loc[,1]; y <- loc[,2]
## rotate for conventional orientation
plot(x, y, type = "n", asp = 1, xlab = "", ylab = "",
     main = "Cities in Europe", axes = FALSE)
text(x, y, labels(eurodist), cex = 0.8)

## calculate distance
distance <- function(sq) {  # Target function
    sq2 <- embed(sq, 2)
    sum(eurodistmat[cbind(sq2[,2], sq2[,1])])
}

sq <- c(1:nrow(eurodistmat), 1)  # Initial sequence: alphabetic
distance(sq)

## for using SANN optimizer we need a function to generate a new
## candidate sequence in order to solve a combinatorial optimization
## problem
genseq <- function(sq) {
    idx <- seq(2, NROW(eurodistmat)-1)
    changepoints <- sample(idx, size = 2, replace = FALSE)
    tmp <- sq[changepoints[1]]
    sq[changepoints[1]] <- sq[changepoints[2]]
    sq[changepoints[2]] <- tmp
    sq
}

## start the optimizer (simulated annealing)
set.seed(123) # chosen to get a good soln relatively quickly
res <- optim(sq, distance, genseq, method = "SANN",
             control = list(maxit = 30000, temp = 2000, trace = TRUE,
             REPORT = 500))
res

## plot the optimal route
plot(x, y, type = "n", asp = 1, xlab = "", ylab = "",
     main = "optim() 'solving' traveling salesman problem", axes = FALSE)
tspres <- loc[res$par,]
s <- seq_len(nrow(eurodistmat))
arrows(tspres[s,1], tspres[s,2], tspres[s+1,1], tspres[s+1,2],
       angle = 10, col = "red")
text(x, y, labels(eurodist), cex = 0.8)



## Contributed packages: CRAN Task View Optimization
## and Mathematical Programming
## http://CRAN.R-Project.org/view=Optimization


## Example 2b)
## Solver for specific purposes: e.g., the TSP package.
require( "TSP" )

## Eurodist example from above
tsp <- as.TSP( eurodistmat )
tsp

## solve the TSP using the following methods:
methods <- c("nearest_insertion", "cheapest_insertion", "farthest_insertion",
             "arbitrary_insertion", "nn", "repetitive_nn", "2-opt")

## calculate tours
tours <- lapply(methods, FUN = function(m) solve_TSP(tsp, method = m))
names(tours) <- methods

tours$optim_sann <- TOUR( res$par[-length(res$par)], "optim_sann", tsp )

tour_lengths <- c( sapply(tours, FUN = attr, "tour_length") )

dotchart( tour_lengths/min(tour_lengths)*100 - 100, xlab = 'percent excess over "optimum"' )


## Example 3)
## (Mixed Integer) Linear Programming Example

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

Rglpk_solve_LP(obj, mat, dir, rhs, types = types, max = max)



################################################################################
## PART II: ROI
################################################################################

require( "ROI" )

## see optimization modeling using ROI on the slides

## Example 4)
## L1 Regression

require( "quantreg" )
data( stackloss )

## function which creates the ROI optimization object from the given data set
create_L1_problem <- function(x, j) {
    len <- 1 + ncol(x) + 2 * nrow(x)
    beta <- rep(0, len)
    beta[j + 1] <- 1
    OP(## objective
        L_objective(c(rep(0, ncol(x) + 1), rep(1, 2 * nrow(x)))),
       ## constraints
       rbind(L_constraint(cbind(1, as.matrix(x), diag(nrow(x)),
                                -diag(nrow(x))), rep("==", nrow(x)), rep(0, nrow(x))), # (1)
             L_constraint(beta, "==", -1)),                                            # (2)
       bounds = V_bound(li = seq_len(ncol(x) + 1), ui = seq_len(ncol(x) + 1),
                        lb = rep(-Inf, ncol(x) + 1), ub = rep(Inf, ncol(x) + 1), nobj = len))
}

## solve the optimization object LP
L1 <- create_L1_problem(stackloss, 4)
ROI_solve( L1, solver = "glpk" )$solution

## compare to regression model implemented in package quantreg
rq( stack.loss ~ stack.x, 0.5 ) # median (l1) regression  fit for the stackloss data



## which other solvers are installed
ROI_installed_solvers()

## currently it is not not straightforward to find a proper solver for
## solving a given problem
names( ROI:::get_solver_methods(ROI:::OP_signature(op)) )

ROI_solve( L1, solver = "symphony" )$solution



################################################################################
## PART III: Portfolio Optimization
################################################################################

## Example 5)
## Risk-return tradeof of stocks

## Package where we collect portfolio models (work in progress)
## install.packages( "ROI.models.finance", repos = "http://r-forge.r-project.org" )
require( "ROI.models.finance" )

## Public domain data from quandl (http://blog.quandl.com/blog/quandl-open-data/)
data( US30 )
tail( US30 )
par( mfrow = c(2,1) )
plot( US30[, "IBM"], main = "IBM" )
plot( US30[, "JPM"], main = "JPMorgan Chase" )
par( mfrow = c(1,1) )

## need some helper functions for financial time series
require( "TTR" )

## calculate weekly simple returns for each asset in the index
r <- na.omit( ROC( US30[ endpoints(index(US30), on = "weeks") ], type = "discrete" ) )

## scatterplots of asset returns
plot( x = as.numeric(r[,"IBM"]), y = as.numeric(r[,"JPM"]) )

## Covariance matrix between all assets in the index
S <- cov( r )

## risk return trade-off
mu <- apply( r, 2, mean )
sigma <- apply( r, 2, sd )
plot( sigma, mu, xlim = c(0, max(sigma)*1.1), ylim = c(0, max(mu)*1.1),
      main = "Risk-Return Tradeoff" )
text( x = sigma, y = mu, labels = colnames(US30), pos = 3 )

## equal-weights portfolio
w <- rep( 1/length(mu), length(mu) )
ew <- c( sqrt(w %*% S %*% w),
         w %*% mu )
points( x = ew[1], y = ew[2], col = "orange" )
text( x = ew[1], y = ew[2], labels = "EW", pos = 3, col = "orange" )


## Example 6)
## MV Optimization using ROI (long-only, fully invested)

## Constraints
full_invest <- L_constraint( rep( 1, ncol(r)), "==", 1 )
long_only <- L_constraint( diag( 1, ncol(r), ncol(r) ), rep(">=", ncol(r)), rep(0, ncol(r)) )

## Specify minimum variance problem object
MV <- OP( objective = Q_objective(S), constraints = c(full_invest, long_only) )

res <- ROI_solve( MV, solver = "quadprog" )
names( res$solution ) <- colnames( US30 )

## optimal weights
round( res$solution*100, 2 )

mv <- c( sqrt(res$solution %*% S %*% res$solution),
        res$solution %*% mu )
points( x = mv[1], y = mv[2], col = "green" )
text( x = mv[1], y = mv[2], labels = "MV", pos = 3, col = "green" )

## this function samples for given asset properties n random portfolios in risk/return space
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


## Example 7)
## Mean-Variance Portfolio

## Say we like the return of the EW protfolio, but with minimal risk
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


## Example 8)
## Calculate efficient frontier

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


## Example 9)
## Integer Programming: Constructing an Index Fund (Cornuejols and Tuetuencue, 2008)

## We use approx 60 months for calibration and 12 months for
## testing. We take only stocks into account, which where included
## in the US30 or US500 index at all time from early 2008 to the end of 2013.
#us_large <- na.locf( US500["20080101::"], maxgap = 2 ) # too large for dense representation
#us_large <- us_large[, !apply(us_large, 2, function(x) any(is.na(x))) ]
r <- na.omit( ROC(US30[endpoints(index(US30["::20121231"]), on = "weeks")], type = "discrete" ) )

## we'd like to choose 15 stocks to replicate the index
q <- 15

## build the optimization problem

## coefficients to objective variables (similarities between assets)
rho <- as.numeric( cor(r) )

## number of assets
n <- ncol( r )

## the objective function
Z <- L_objective( c(rho, rep(0, n)) )

## the constraints
min_stocks_included <- L_constraint( c(rep(0, length(rho)), rep(1, n)), "==", q )

full_block <- matrix( 0, nrow = n, ncol = dim(Z$L)[2] )
for( i in 1:n ){
    idx_start <- (i-1)*n+1
    full_block[ i, idx_start:(idx_start+n-1) ] <- 1
}
sum_most_similar <- L_constraint( full_block, rep("==", n), rep(1, n) )

block_x <- diag( 1, length(rho) )
block_y <- matrix( rep(diag( -1,  n ), n ), ncol = n, byrow = TRUE )
x_ij_constraints <- L_constraint( cbind(block_x, block_y),
                                  rep("<=", nrow(block_x)),
                                  rep(0,  nrow(block_x)) )

## the ROI optimization problem object
op <- OP( Z, c(min_stocks_included, sum_most_similar, x_ij_constraints),
          types = rep("B", length(Z)), maximum = TRUE )

## solve the specified problem using an LP solver
sol <- ROI_solve( op, solver = "glpk" )

## calculate and print the assets included in the index fund
idx_included <- sol$solution[ (length(sol$solution)-n+1):length(sol$solution) ]
names( idx_included ) <- colnames( r )

idx_included


## NOTE: we should really use market capitalization instead

## calculate weights of assets in index fund
x_ij <- matrix( sol$solution[ 1:(length(sol$solution)-n) ], nrow = n, byrow = TRUE )
w_j <- apply( sweep( x_ij, 1, as.numeric(last(US30["20121231"])), "*"), 2, sum )
fund_w <- w_j/sum(w_j)
names( fund_w ) <- colnames( r )

## calculate benchmark weights
bm_w <- as.numeric(last(US30["::20121231"]))/sum(as.numeric(last(US30["::20121231"])))

# compare weights
cbind( bm_w, fund_w)

## approx out of sample performance comparison
## Q1 perf comparison bm vs fund
last( US30["::20130331"] ) %*% fund_w / (last( US30["::20130331"] ) %*% bm_w)
## Q2 perf comparison bm vs fund
last( US30["::20130630"] ) %*% fund_w / (last( US30["::20130630"] ) %*% bm_w)
## 1Y perf comparison bm vs fund
last( US30 ) %*% fund_w / (last( US30 ) %*% bm_w)

## plot index fund equity line (indexed at 2012-01-01)
os <- US30["20120101::"]
fund_el <- xts( apply(coredata(os) , 1, function(x) x%*% fund_w ), order.by = index(os) )
bm_el <- xts( apply(coredata(os) , 1, function(x) x%*% bm_w ), order.by = index(os) )

require( "PerformanceAnalytics" )
chart.CumReturns( cbind(FUND = ROC(fund_el, type = "discrete"), BM = ROC(bm_el, type = "discrete")), legend = "topleft" )
