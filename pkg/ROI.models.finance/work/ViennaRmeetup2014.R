################################################################################
## ROI - Intro
## Author: Stefan Theussl
## Licence: GPL-2
################################################################################



################################################################################
## Optimization general
################################################################################

## what is optimization?

## tools in base R
?optim
?nlminb

?constrOptim
?nlm
?optimize

## some function





## travelling salesperson (see optim help)
## optim via SANN heuristics

## from optim() man page
eurodistmat <- as.matrix(eurodist)

distance <- function(sq) {  # Target function
    sq2 <- embed(sq, 2)
    sum(eurodistmat[cbind(sq2[,2], sq2[,1])])
}

genseq <- function(sq) {  # Generate new candidate sequence
    idx <- seq(2, NROW(eurodistmat)-1)
    changepoints <- sample(idx, size = 2, replace = FALSE)
    tmp <- sq[changepoints[1]]
    sq[changepoints[1]] <- sq[changepoints[2]]
    sq[changepoints[2]] <- tmp
    sq
}

sq <- c(1:nrow(eurodistmat), 1)  # Initial sequence: alphabetic
distance(sq)
                                        # rotate for conventional orientation
loc <- -cmdscale(eurodist, add = TRUE)$points
x <- loc[,1]; y <- loc[,2]
s <- seq_len(nrow(eurodistmat))
tspinit <- loc[sq,]

plot(x, y, type = "n", asp = 1, xlab = "", ylab = "",
     main = "initial solution of traveling salesman problem", axes = FALSE)
arrows(tspinit[s,1], tspinit[s,2], tspinit[s+1,1], tspinit[s+1,2],
       angle = 10, col = "green")
text(x, y, labels(eurodist), cex = 0.8)

set.seed(123) # chosen to get a good soln relatively quickly
res <- optim(sq, distance, genseq, method = "SANN",
             control = list(maxit = 30000, temp = 2000, trace = TRUE,
             REPORT = 500))
res  # Near optimum distance around 12842

tspres <- loc[res$par,]
plot(x, y, type = "n", asp = 1, xlab = "", ylab = "",
     main = "optim() 'solving' traveling salesman problem", axes = FALSE)
arrows(tspres[s,1], tspres[s,2], tspres[s+1,1], tspres[s+1,2],
       angle = 10, col = "red")
text(x, y, labels(eurodist), cex = 0.8)



## contributed packages: CRAN Task View Optimization and Mathematical Programming
## http://CRAN.R-Project.org/view=Optimization



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



################################################################################
## ROI
################################################################################



## ROI

require( "ROI" )

## Portfolio Optimization

## Package where we collect portfolio models (work in progress)
## install.packages( "ROI.models.finance", repos = "http://r-forge.r-project.org" )
require( "ROI.models.finance" )

## Public domain data from quandl (http://blog.quandl.com/blog/quandl-open-data/)
data( US30 )
tail( US30 )
par( mfrow = c(2,1) )
plot( US30[, "IBM"], main = "IBM" )
plot( US30[, "JPM"], main = "JPMorgan Chase" )

## need some helper functions for financial time series
require( "TTR" )

r <- na.omit( ROC( US30[ endpoints(index(US30), on = "weeks") ], type = "discrete" ) )
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


## MV Optimization using ROI
################################################################################

## Constraints
full_invest <- L_constraint( rep( 1, ncol(r)), "==", 1 )
long_only <- L_constraint( diag( 1, ncol(r), ncol(r) ), rep(">=", ncol(r)), rep(0, ncol(r)) )

## Specify minimum variance problem object
op <- OP( objective = Q_objective(S), constraints = c(full_invest, long_only) )

res <- ROI_solve( op, solver = "quadprog" )
names( res$solution ) <- colnames( US30 )

## optimal weights
round( res$solution*100, 2 )

mv <- c( sqrt(res$solution %*% S %*% res$solution),
        res$solution %*% mu )
points( x = mv[1], y = mv[2], col = "green" )
text( x = mv[1], y = mv[2], labels = "MV", pos = 3, col = "green" )


make_random_portfolios <- function( MU, COV, n ){
    u <-  matrix( runif(n * length(mu)), nrow = n )
    constituents <- sample( 1:length(mu), n, replace = TRUE )
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


## Target Return
################################################################################

## Say we like the return of the EW protfolio, but with minimal risk
target_return <- L_constraint( mu, ">=", ew[2] )


constraints(op)
constraints(op) <- rbind( constraints(op),  )

res <- ROI_solve( op, solver = "quadprog" )
names( res$solution ) <- colnames( US30 )

round( res$solution*100, 2 )

mvtr <- c( sqrt(res$solution %*% S %*% res$solution),
        res$solution %*% mu )
points( x = mvtr[1], y = mvtr[2], col = "blue" )
text( x = mvtr[1], y = mvtr[2], labels = "MinVar_tr", pos = 3, col = "blue" )


## Calculate efficient frontier
################################################################################

grid_start <- mv[2]
grid_end <- mu[ which.max(mu) ]
grid_points <- 1000

grid <- seq( from = grid_start, to = grid_end, length.out = 1000 )
MV <- OP( objective = Q_objective(S), constraints = c(full_invest, long_only) )

l <- lapply( grid, function(r) {
    target_return <- L_constraint( mu, ">=", r )
    op <- MV
    constraints(op) <- c( constraints(op), target_return )
    res <- ROI_solve( op, solver = "quadprog" )
    c( sqrt(res$solution %*% S %*% res$solution), r )
} )

efficient_frontier <- do.call( rbind, l )
lines( efficient_frontier, col = "red" )


## CVAR
################################################################################

model <- "max_cva"
CV <- ROI_model_portfolio( r, model, control = list(long_only = TRUE,
                                                    fully_invest = TRUE,
                                                    alpha = 0.1) )
sol <- ROI_solve( CV, solver = "glpk" )
w <- sol$solution[ 1:ncol(r) ]

cvar <- c( sqrt(w %*% S %*% w),
           w %*% mu )
points( x = cvar[1], y = cvar[2], col = "purple" )
text( x = cvar[1], y = cvar[2], labels = "CVaR", pos = 3, col = "purple" )

grid_cvar <- grid[ grid >= cvar[2] ]
## efficient CVaR portfolios
l <- lapply( grid, function(r) {
    constr_vec <- numeric( ncol(constraints(CV)$L) )
    constr_vec[1:length(mu)] <- mu
    target_return <- L_constraint( constr_vec, ">=", r )
    op <- CV
    constraints(op) <- c( constraints(op), target_return )
    res <- ROI_solve( op, solver = "glpk" )
    w <- res$solution[ 1:length(mu) ]
    c( sqrt(w %*% S %*% w), r )
} )

efficient_cvar <- do.call( rbind, l )
lines( efficient_cvar, col = "brown" )


## Integer Programming: Constructing an Index Fund (Cornuejols and Tuetuencue, 2008)
################################################################################

# r <- r[,1:5]
q <- 15

rho <- as.numeric( cor(r) )
n <- ncol( r )

#x11, x12, x13, ..., x21, x22, x23, ..., xn1, xn2, xn3 ...
Z <- L_objective( c(rho, rep(0, n)) )

min_stocks_included <- L_constraint( c(rep(0, length(rho)), rep(1, n)), "==", q )

full_block <- matrix( 0, nrow = n, ncol = dim(Z$L)[2] )
for( i in 1:n ){
    idx_start <- (i-1)*n+1
    full_block[ i, idx_start:(idx_start+n-1) ] <- 1
}
sum_most_similar <- L_constraint( full_block, rep("==", n), rep(1, n) )

block_x <- diag(1, length(rho))
block_y <- matrix( rep(diag( 1,  n ), n ), ncol = n, byrow = TRUE )

x_ij_constraints <- L_constraint( cbind(block_x, block_y), rep("<=", nrow(block_x)), rep( 0,  nrow(block_x) ) )

op <- OP( Z, c(min_stocks_included, sum_most_similar, x_ij_constraints), types = rep("B", length(Z$L)) )

ROI_solve( op, solver = "glpk", maximum = TRUE )
