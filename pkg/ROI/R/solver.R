
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

## In reformulations.R I use reduce_signature
signature_to_list <- function(x) {
    x <- reduce_signature(x)
    x[['types']] <- c("B", "I", "C")[sapply(c("B", "I", "C"), function(i) isTRUE(x[[i]]))]
    x[['B']] <- NULL
    x[['I']] <- NULL
    x[['C']] <- NULL
    x
}

SolverDatabase <- function() {
    env <- new.env(parent = emptyenv())
    env$solvers <- list()

    ## name   solver name
    ## signature   the signature as data.frame
    env$set <- function(name, signature) {
        stopifnot(is.character(name))
        self <- parent.env(environment())$env
        self$solvers[[name]] <- signature ## signature_to_list(signature)
        invisible(NULL)
    }

    env$get <- function(name) {
        self <- parent.env(environment())$env
        self$solvers[[name]]
    }
    env
}

##  -----------------------------------------------------------
##  ROI_solver_signature
##  ====================
##' @title Obtain Solver Signature
##'
##' @description Obtain the signature of a registered solver.
##' @param solver a character string giving the name of the solver.
##' @return the solver signature if the specified solver is registered \code{NULL} otherwise.
##' @examples
##' ROI_solver_signature("nlminb")
##' @export
ROI_solver_signature <- function(solver) {
    stopifnot(is.character(solver), (length(solver) == 1L))
    solver_signature_db$get(solver)
}


