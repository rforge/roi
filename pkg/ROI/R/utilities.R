## utilities.R

###############################################################
## ROI helper functions


.xtQx <-
function(Q, x)
{
    ## Value of quadratic form t(x) %*% Q %*% x.
    ## As we implement simple triplet matrices in S3, we could only have
    ## %*% and crossprod methods if we created S3 generics for these ...
    if(is.simple_triplet_matrix(Q))
        sum(Q$v * x[Q$i] * x[Q$j])
    else
        c(crossprod(x, Q %*% x))
}


available_row_sense <- function( )
  c('<', '<=', "==", ">", ">=")

.expand_row_sense<- function( x, n ) {
    if( is.null(x) ) {
        ## >= by default.
        rep.int(">=", n)
    }
    else {
        if( !row_sense_is_feasible(x) )
            stop( "Invalid direction of constraints." )
        ## Be nicer than necessary ...
        rep( x, length.out = n )
    }
}

.expand_types <- function( x, n ) {
    if( is.null(x) ) {
        ## Continuous by default.
        rep.int("C", n)
    }
    else {
        if( !is.character(x) || !all(x %in% available_types()) )
            stop( "Invalid MIP variable types." )
        ## Be nicer than necessary ...
        rep( x, length.out = n )
    }
}

## returns solver name based on package name
## Convention: ROI.plugin.<solver> => <solver>
.plugin_prefix <- function()
    "ROI.plugin"

get_solver_name <- function( pkgname )
    sub(sprintf("%s.", .plugin_prefix()), "", as.character(pkgname))

get_package_name <- function( solver )
    paste(.plugin_prefix(), as.character(solver), sep = ".")
