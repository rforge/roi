################################################################################
## Package: ROI
## File:    roi.R
## Author:  Stefan Theussl
## Changed: 2016-05-27
################################################################################

## Imports
#' @importFrom "stats" "variable.names"
#' @importFrom "stats" "setNames"
#' @importFrom "stats" "na.omit"
#' @importFrom "stats" "terms"
#' @importFrom "utils" "str"
#' @import "slam"
#

################################################################################
## MAIN FUNCTION TO SOLVE OPTIMIZATION PROBLEMS USING ROI
################################################################################

##  -----------------------------------------------------------
##  ROI_solve =========
##' @title Solve an Optimization Problem
##' @description Solve a given optimization problem.  This function
##'     uses the given solver (or searches for an appropriate solver)
##'     to solve the supplied optimization problem.
##' @param x an optimization problem of class \code{"OP"}.
##' @param solver a character vector specifying the solver to use. If
##'     missing, then the default solver returned by
##'     \code{\link{ROI_options}} is used.
##' @param control a list with additional control parameters for the
##'     solver.  This is solver specific so please consult the
##'     corresponding documentation.
##' @param ... a list of control parameters (overruling those
##'     specified in \code{control}).
##' @return a list containing the solution and a message from the
##'     solver.
##' @examples
##' ## Rosenbrock Banana Function
##' ## -----------------------------------------
##' ## objective
##' f <- function(x) {
##'    return( 100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2 )
##' }
##' ## gradient
##' g <- function(x) {
##'    return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
##'              200 * (x[2] - x[1] * x[1])) )
##' }
##' ## bounds
##' b <- V_bound(li = 1:2, ui = 1:2, lb = c(-3, -3), ub = c(3, 3))
##' op <- OP( objective = F_objective(f, n = 1L, G = g),
##'           bounds = b )
##' res <- ROI_solve( op, solver = "nlminb", control = list(start = c( -1.2, 1 )) )
##' solution( res )
##' ## Portfolio optimization - minimum variance
##' ## -----------------------------------------
##' ## get monthly returns of 30 US stocks
##' data( US30 )
##' r <- na.omit( US30 )
##' ## objective function to minimize
##' obj <- Q_objective( 2*cov(r) )
##' ## full investment constraint
##' full_invest <- L_constraint( rep(1, ncol(US30)), "==", 1 )
##' ## create optimization problem / long-only
##' op <- OP( objective = obj, constraints = full_invest )
##' ## solve the problem - only works if a QP solver is registered
##' \dontrun{
##' res <- ROI_solve( op )
##' res
##' sol <- solution( res )
##' names( sol ) <- colnames( US30 )
##' round( sol[ which(sol > 1/10^6) ], 3 )
##' }
##' @author Stefan Theussl
##' @export
##  -----------------------------------------------------------
ROI_solve <- function( x, solver, control = list(), ... ){

    ## if no second argument is supplied we use the default solver
    if( missing(solver) )
        solver <- ROI_options("default_solver")

    dots <- list(...)
    control[names(dots)] <- dots

    x <- as.OP( x )

    ## handle the boundary case of no variables.
    ## #FIXME: @ST: should also consider other
    ## #NOTE: @FS: The idea is that it sould also be possible to use the solvers
    ##             to just find a feasible solution.
    ##if( !length(terms(objective(x))$L) ) {
    ##    y <- .solve_empty_OP(x)
    ##    return(y)
    ##}

    methods <- get_solver_methods( OP_signature(x) )
    if ( !length(methods) ) {
        ## CASE: no method found for this signature
        sig <- OP_signature( x )
        stop( "no solver found for this signature:\n\t",
              paste(paste(names(sig), sig, sep=": "), collapse="\n\t") )
    }
    if ( isTRUE(solver != "auto") ) {
        SOLVE <- methods[[ solver ]]
        if ( !is.function(SOLVE) ) {
            ## CASE: applicable solvers found but the solver provided is wrong
            ##       => issue warning and fallback to the other solver
            SOLVE <- methods[[1]]
            warning( "solver '", solver, "' not found or applicable, ROI uses '",
                     names(methods)[1], "' instead" )
            solver <- names( methods )[1]
        }
    } else {
        SOLVE <- select_solver(x, methods)
        solver <- names( SOLVE )[1]
        SOLVE <- SOLVE[[1]]
    }

    if( length(control) )
        if( all(!names(ROI_translate(control, solver)) %in% get_solver_controls_from_db(solver)) )
            warning( sprintf("some control arguments not available in solver '%s'.", solver) )

    ## TODO: handle default ROI controls separately
    ## FIXME: what if verbose and solver specific verbosity are set at the same time?
    control$verbose <- ifelse( length(control$verbose), control$verbose, FALSE )
    if( control$verbose )
        writeLines( "<SOLVER MSG>  ----" )
    out <- SOLVE( x, ROI_translate(control, solver) )
    if( control$verbose )
        writeLines( "<!SOLVER MSG> ----" )
    if ( any(!c(is.null(variable.names(constraints(x))), is.null(variable.names(objective(x))))) ) {
        if ( is.null(variable.names(constraints(x))) ) {
            if ( length(out$solution) == length(variable.names(objective(x))) )
                names(out$solution) <- variable.names(objective(x))
        } else if ( is.null(variable.names(objective(x))) ) {
            if ( length(out$solution) == length(variable.names(constraints(x))) )
                names(out$solution) <- variable.names(constraints(x))
        } else {
            if ( identical(variable.names(objective(x)), variable.names(constraints(x))) &
                 (length(out$solution) == length(variable.names(objective(x)))) ) {
                names(out$solution) <- variable.names(objective(x))
            }
        }
    }
    out
}

which_op_type <- function(x) {
    if ( any(x$C) ) {
        if ( all(x$cones == "free") ) {
            if ( all(x[,c('objective', 'constraints')] == "L") ) { ## LP
                return("LP")
            } else { ## QP
                return("QP")
            }
        } else { ## CONIC PROBLEM
            return("CP")
        }
    } else { ## MIXED INTEGER
        if ( all(x$cones == "free") ) {
            if ( all(x[,c('objective', 'constraints')] == "L") ) { ## LP
                return("MILP")
            } else { ## QP
                return("MIQP")
            }
        } else { ## CONIC PROBLEM
            return("MICP")
        }
    }
    return("NLP")
}

## select_solver gets an optimization problem "x" and the applicable methods
## "methods" and returns a solver.
select_solver <- function(x, methods) {
    signature <- OP_signature(x)
    type <- which_op_type(signature)
    solver_selection_table <- ROI_options("solver_selection_table")
    b <- solver_selection_table[[type]] %in% names(methods)
    if ( sum(b) > 0) {
        solver <- solver_selection_table[[type]][which(b)[1]]
        return( methods[solver] )
    }
    b <- solver_selection_table[["default"]] %in% names(methods)
    if ( sum(b) ) {
        solver <- solver_selection_table[["default"]][which(b)[1]]
        return( methods[solver] )
    }
    return( methods[1] )
}

################################################################################
## UTILITY FUNCTIONS TO QUERY SOLVERS
################################################################################

##' @title Solver Tools
##' @description Retrieve the names of installed or registered solvers.
##' @details
##'   Whereas \code{ROI_installed_solvers()} and
##'   \code{ROI_available_solvers()} may list the names of installed
##'   solvers that do not necessarily work,
##'   \code{ROI_registered_solvers()} lists all solvers that can be used
##'   to solve optimization problems.
##'
##' @param ... arguments passed on to \code{\link{installed.packages}}.
##' @return a named character vector.
##' @author Stefan Theussl
##' @export
ROI_registered_solvers <- function( ... ){
    ## solvers registered
    get_solver_packages_from_db()
}

##' @rdname ROI_registered_solvers
##' @export
ROI_installed_solvers <- function( ... ) {
    dots <- list(...)
    if ( "lib.loc" %in% names(dots) ) lib.loc <- dots$lib.loc
    else lib.loc <- .libPaths()
    pkgs <- grep( .plugin_prefix(), unlist(lapply(lib.loc, dir)), value = TRUE )
    structure( pkgs, names = .ROI_plugin_get_solver_name(pkgs) )
}

##' @rdname ROI_registered_solvers
##' @export
ROI_available_solvers <- function( ... ){
    ROI_installed_solvers( ... )
}

## ---------------------------------------------------------
##
##  ROI_applicable_solvers
##  ======================
##
##' @title Obtain Applicable Solvers
##' @description \code{ROI_applicable_solvers} takes as argument an
##'   optimization problem (object of class \code{'OP'}) and returns a vector
##'   giving the applicable solver. The set of applicable solver is restricted
##'   on the available solvers, which means if solver \code{"A"} and \code{"B"}
##'   would be applicable but a \code{ROI.plugin} is only installed for solver
##'   \code{"A"} only solver \code{"A"} would be listed as applicable solver.
##' @param op an \pkg{ROI}-object of type \code{'OP'}.
##' @return An character vector giving the applicable solver,
##'   for a certain optimization problem.
##'
##' @export
## ---------------------------------------------------------
ROI_applicable_solvers <- function( op ){
    unname(names(get_solver_methods( OP_signature( op ) )))
}

################################################################################
## HELPER FUNCTIONS (not exported)
################################################################################

## returns solver method from signatures
get_solver_methods <- function( signatures ) {
    if ( nrow(signatures) == 1 ) return( get_solver_methods_from_signature(signatures) )
    solvers <- unlist(apply(signatures, 1, get_solver_methods_from_signature))
    solver_names <- unique(names(solvers)[table(names(solvers)) == nrow(signatures)])
    solvers[solver_names]
}

## returns solver method form signature
get_solver_methods_from_signature <- function( signature ){
    entries <- do.call( solver_db$get_entries, as.list(signature) )
    solvers <- unlist(lapply( entries, function(x) x$solver ))
    structure( lapply(entries, function(x) x$FUN), names = solvers)
}

## returns available solvers from db
get_solvers_from_db <- function( ) {
    unique( solver_db$get_field_entries("solver", unlist = TRUE) )
}

## returns package names of available solvers from db
get_solver_packages_from_db <- function ( ){
    solvers <- get_solvers_from_db()
    structure( get_package_name(solvers), names = solvers )
}

.sort_types <- function(x){
    stopifnot( all(x %in% available_types()) )
    ord <- c(C = 1, I = 2, B = 3)
    ordered <- order(ord[x])
    x[ordered]
}

ROI_expand <- function(...){
    base::expand.grid(..., stringsAsFactors = FALSE)
}
