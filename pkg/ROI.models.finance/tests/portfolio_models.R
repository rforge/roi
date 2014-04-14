## Packages to compare
require( "SERmodels" )

## Data for each example below
data( US30 )
r <- na.omit( US30 / lag(US30, 1, na.pad = TRUE) - 1 )



################################################################################
## Check results for sanity
################################################################################

## Comparison data
################################################################################

input <- read.csv( "weights.csv", stringsAsFactors = FALSE )
weights <- as.matrix(input[,-1])
rownames( weights ) <- input[,1]


## Minimum Variance Portfolio
################################################################################

model <- "minimum_variance"
MV <- portfolio_model( r, model )
sol <- ROI_solve( MV, solver = "quadprog" )
w <- sol$solution
names( w ) <- colnames( US30 )
stopifnot( all.equal(w, weights[model, ]) )


## Minimum Tail Dependence
################################################################################

model <- "minimum_tail_dependence"
MTD <- portfolio_model( r, model )
sol <- ROI_solve( MTD, solver = "quadprog" )
## Rescale weights
sd <- apply( r, 2, sd )
w <- sol$solution/sd
w <- w / sum( w )
names( w ) <- names( US30 )
stopifnot( all.equal(w, weights[model, ]) )


## Maximum Diversification
################################################################################

model <- "maximum_diversification"
MD <- portfolio_model( r, model )
sol <- ROI_solve( MD, solver = "quadprog" )
## Rescale weights
sd <- apply( r, 2, sd )
w <- sol$solution/sd
w <- w/sum(w)
names( w ) <- names( US30 )
stopifnot( all.equal(w, weights[model, ]) )
