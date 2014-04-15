## Packages to compare
require( "ROI.models.finance" )

## Data for each example "B)" below
data( US30 )
r <- na.omit( US30 / lag(US30, 1, na.pad = TRUE) - 1 )

results <- list()

## Minimum Variance Portfolio
################################################################################

model <- "min_var"
MV <- ROI_model_portfolio( r, model, control = list(long_only = TRUE,
                                                    fully_invest = TRUE) )
sol <- ROI_solve( MV, solver = "quadprog" )
w <- sol$solution
names( w ) <- colnames( US30 )
results[[ model ]] <- w


## Minimum Tail Dependence
################################################################################

## model <- "min_tdp"
## MTD <- ROI_model_portfolio( r, model, control = list(long_only = TRUE,
##                                                     fully_invest = TRUE) )
## sol <- ROI_solve( MTD, solver = "quadprog" )
## ## Rescale weights
## sd <- apply( r, 2, sd )
## w <- sol$solution/sd
## w <- w / sum( w )
## names( w ) <- names( US30 )
## results[[ model ]] <- w


## Maximum Diversification
################################################################################

model <- "max_div"
MD <- ROI_model_portfolio( r, model, control = list(long_only = TRUE,
                                                    fully_invest = TRUE) )
sol <- ROI_solve( MD, solver = "quadprog" )
## Rescale weights
sd <- apply( r, 2, sd )
w <- sol$solution/sd
w <- w/sum(w)
names( w ) <- names( US30 )
results[[ model ]] <- w

## Maximum CVAR
################################################################################

model <- "max_cva"
CV <- ROI_model_portfolio( r, model, control = list(long_only = TRUE,
                                                    fully_invest = TRUE,
                                                    alpha = 0.1) )
sol <- ROI_solve( CV, solver = "glpk" )
## only the first few variables are our objectives
w <- sol$solution[ 1:ncol(US30) ]
names( w ) <- colnames( US30 )
results[[ model ]] <- w

## Compile output
################################################################################

weights <- do.call( rbind, results )
write.csv( weights, file = "../tests/weights.csv" )

