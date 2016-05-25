################################################################################
## Package: ROI
## File:    roi.R
## Author:  Stefan Theussl
## Changed: 2011-10-03
################################################################################

## Imports
#' @importFrom "stats" "variable.names"
#' @importFrom "stats" "setNames"
#' @importFrom "stats" "na.omit"
#' @importFrom "stats" "terms"
#' @import "slam"
#

## TODO: create a solver ranking based on simulations.
Solver_Order <- c("ecos", "glpk", "nloptr", "quadprog", "symphony", "ipop")

################################################################################
## MAIN FUNCTION TO SOLVE OPTIMIZATION PROBLEMS USING ROI
################################################################################

##  -----------------------------------------------------------
##  ROI_solve
##  =========
##' @title Solve an Optimization Problem
##' @description
##'   Solve a given optimization problem.
##'   This function uses the given solver (or searches for an appropriate
##'   solver) to solve the supplied optimization problem.
##' @param x an optimization problem of class \code{"OP"}.
##' @param solver a character vector specifying the solver to use.
##' @param control a list with additional control parameters for the solver.
##'   This is solver specific so please consult the corresponding documentation.
##' @param ... a list of control parameters (overruling those specified in \code{control}).
##' @return a list containing the solution and a message from the solver.
##' @examples
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
##' sol <- res$solution
##' names( sol ) <- colnames( US30 )
##' round( sol[ which(sol > 1/10^6) ], 3 )
##' }
##' @author Stefan Theussl
##' @export
##  -----------------------------------------------------------
ROI_solve <- function( x, solver="", control = list(), ... ){

    ## TODO: would be nice if we have an order if no solver is provided!
    ## NOTE: nloptr can take additional parameters,
    ##       we supply them by using control$args
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
    if ( solver != "" ) {
        SOLVE <- methods[[ solver ]]
    } else {
        SOLVE <- methods[[ 1 ]]
    }
    if ( !is.function(solve) ) {
        ## CASE: applicable solvers found but the solver name is wrong
        ##       => issue warning and fallback to the other solver
        SOLVE <- methods[[1]]
        warning( "solver '", solver, "' not found or applicable, ROI uses '",
                 names(methods)[1], "' instead" )
    }
    SOLVE( x, control )
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



################################################################################
## REGISTER NEW SOLVER METHODS
################################################################################



##  -----------------------------------------------------------
##  .ROI_plugin_register_solver_method
##  ==========================
##' @title Register Solver Method
##'
##' @description Register a new solver method.
##' @param signatures a data.frame with the supported signatures.
##' @param solver a character string giving the solver name.
##' @param method a function registered as solver method.
##' @return TRUE on success
##' @family plugin functions
##' @rdname ROI_plugin_register_solver_method
##' @export
.ROI_plugin_register_solver_method <- function( signatures, solver, method ){
    for( i in 1:nrow(signatures) )
        do.call(solver_db$set_entry, c(as.list(signatures[i,]),
                                             list(solver = solver),
                                             list(FUN = method)))

    invisible( TRUE )
}



################################################################################
## SIGNATURES
################################################################################

ROI_required_signature <- function()
    c("objective", "constraints", "types", "bounds", "cones", "maximum")

##' Create a solver signature, the solver signatures are used to indicate
##' which problem types can be solved by a given solver.
##'
##' @title Make Signatures
##' @param ... signature definitions
##' @return a data.frame with the supported signatures
##' @examples
##' ## ROI_make_LP_signatures
##' lp_signature <- .ROI_plugin_make_signature( objective = "L",
##'                                     constraints = "L",
##'                                     types = c("C"),
##'                                     bounds = c("X", "V"),
##'                                     cones = c("free"),
##'                                     maximum = c(TRUE, FALSE) )
##' @family plugin functions
##' @rdname ROI_plugin_make_signature
##' @export
.ROI_plugin_make_signature <- function(...){
    dotargs <- list(...)
    required <- ROI_required_signature() ## names(formals(OP))
    if( length(dotargs) < 2 )
        stop( sprintf("Signature element for '%s' and '%s' need to be given.",
                      required[1], required[2]) )
    length(dotargs) <- length(required)
    if( is.null(names(dotargs)) )
        names(dotargs) <- required
    else {
        nam <- names(dotargs)
        nam[nam == ""] <- required[!(required %in% nam)]
        names(dotargs) <- nam
    }
    stopifnot( all(names(dotargs) %in% required) )

    ## FIXME: handle NULL case
    #.sort_types(unique(dotargs$types))
    #out <- dotargs[ names(dotargs)[-which(names(dotargs) == "types")] ]

    dotargs <- lapply(dotargs, function(x) if( is.null(x) ) FALSE else x)
    .make_signature(do.call(ROI_expand, dotargs))
}

##  ROI plugin convenience functions:
##  make a set of signatures based on problem class
##  ---------------------------------------------------------
##  .ROI_plugin_make_signatures
##  ======================
##  ---------------------------------------------------------
##' @title Make Signatures
##' @description A set of default signatures.
##' @details TODO: Create one function with a parameter type or
##'   put them into the plugins!
##' @return An R object containing the signature.
##' @export
.ROI_plugin_make_LP_signatures <- function()
    .ROI_plugin_make_signature( objective = "L",
                        constraints = c("X", "L"),
                        types = c("C"),
                        bounds = c("X", "V"),
                        cones = c("free"),
                        maximum = c(TRUE, FALSE) )

##' @rdname .ROI_plugin_make_LP_signatures
##' @export
.ROI_plugin_make_QP_signatures <- function()
    .ROI_plugin_make_signature( objective = "Q",
                        constraints = c("X", "L"),
                        types = c("C"),
                        bounds = c("X", "V"),
                        cones = c("free"),
                        maximum = c(TRUE, FALSE) )

##' @rdname .ROI_plugin_make_LP_signatures
##' @export
.ROI_plugin_make_MILP_signatures <- function()
    .ROI_plugin_make_signature( objective = "L",
                        constraints = c("X", "L"),
                        types = c("C", "I", "B", "CI", "CB", "IB", "CIB"),
                        bounds = c("X", "V"),
                        cones = c("free"),
                        maximum = c(TRUE, FALSE) )

##' @rdname .ROI_plugin_make_LP_signatures
##' @export
.ROI_plugin_make_MIQP_signatures <- function()
    .ROI_plugin_make_signature( objective = c("L", "Q"),
                        constraints = c("X", "L"),
                        types = c("C", "I", "B", "CI", "CB", "IB", "CIB"),
                        bounds = c("X", "V"),
                        cones = c("free"),
                        maximum = c(TRUE, FALSE) )

##' @rdname .ROI_plugin_make_LP_signatures
##' @export
.ROI_plugin_make_MIQCP_signatures <- function()
    .ROI_plugin_make_signature( objective = c("L", "Q"),
                        constraints = c("X", "L", "Q"),
                        types = c("C", "I", "B", "CI", "CB", "IB", "CIB"),
                        bounds = c("X", "V"),
                        cones = c("free"),
                        maximum = c(TRUE, FALSE) )

.make_signature <- function( x ){
    required <- ROI_required_signature()
    if ( !identical(colnames(x), required) ) {
        ## hint <- "It seems, the signature is missing the entries or has to much entries!"
        hint1 <- sprintf("Required entries are: '%s'", paste(required, collapse="', '"))
        hint2 <- sprintf("Given entries are: '%s'", paste(colnames(x), collapse="', '"))
        hint <- sprintf("%s\n\t%s", hint1, hint2)
        error( "MISSPECIFIED_SIGNATURE", "The signature doesn't match the required signature!",
               ".make_signature", hint=hint, call=NULL )
    }
    types <- strsplit(as.character(x[["types"]]), "")
    types <- do.call(rbind, lapply( types, function(t) available_types() %in% t) )
    colnames(types) <- available_types()
    cbind(x[, colnames(x) != "types"], types)
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
