## Packages to compare
require( "SERmodels" )

## Data for each example "B)" below
data( US30 )
r <- na.omit( US30 / lag(US30, 1, na.pad = TRUE) - 1 )

results <- list()

## Minimum Variance Portfolio
################################################################################

model <- "minimum_variance"
MV <- portfolio_model( r, model )
sol <- ROI_solve( MV, solver = "quadprog" )
w <- sol$solution
names( w ) <- colnames( US30 )
results[[ model ]] <- w


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
results[[ model ]] <- w


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
results[[ model ]] <- w


## Compile output
################################################################################

weights <- do.call( rbind, results )
write.csv( weights, file = "../tests/weights.csv" )

