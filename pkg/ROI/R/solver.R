
## Solver functions for a given OP signature

## Signature: objective_constraints_types<_bounds_maximize>

## Signature: L_L_C<_FALSE_FALSE> (LP)
L_L_C <- function(x)
    UseMethod( "L_L_C" )

## Signature: L_L_I<_FALSE_FALSE> (LIP)
L_L_I <- function(x)
    UseMethod( "L_L_I" )

## Signature: L_L_C+I<_FALSE_FALSE> (MILP)
L_L_CI <- function(x)
    UseMethod( "L_L_CI" )

all_signatures <- function(){
    sigs <- expand.grid( objective = names(available_objective_classes()),
                         constraints = names(available_objective_classes()),
                         types = c("C", "I", "B", "CI", "CB", "IB", "CIB"),
                         bounds = names(available_bound_classes()),
                         maximum = c("TRUE", "FALSE") )
    stopifnot( ncol(sigs) == length(formals(OP)) )
    stopifnot( identical(colnames(sigs), names(formals(OP))) )
    sigs
}

##' Check if the solver is applicable for a given problem!
##' @param x an optimization problem of class \code{"OP"}
##' @param solver a character vector specifying the solver to use.
##' @return a boolean indicating if the given solver can be used on the 
##'         optimization problem based on the signature
##' @noRd
## TODO: FS put this in again!
## .check_solver_applicable <- function(x, solver) {
##     if (!solver %in% names(ROI_registered_solvers()))
##         stop(sprintf('"%s" is not amongst the registered solvers!', solver), call.=FALSE)
##     solver_signature <- do.call(rbind, solver_db$get_entries(solver))
##     fun <- function(y) {
##         OP_signature(qp)[,y] %in% unlist(solver_signature[,y], use.names=FALSE)
##     }
##     sapply(colnames(OP_signature(x)), fun)
##}

#make_generics <- function(){
#    lapply( apply( all_signatures(), 1, .make_signature), function(x) sprintf('%s <- function(x) UseMethod( "%s" )', x, x))
#}
