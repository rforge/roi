################################################################################
## Package: ROI
## File:    solution.R
## Author:  Stefan Theussl
## Changed: 2016-05-20
################################################################################

## ---------------------------------------------------------
## Extract Solutions
## =================
## ---------------------------------------------------------

##  --------------------------------------------------------
## 
##  solution
##  ========
##' @title Extract Solution
##' @description The solution can be accessed via the method \code{'solution'}.
##' @param sol an object of type \code{'OP_solution'}.
##' @return the extracted solution.
##' @export
solution <- function(sol) UseMethod( "solution")

solution.default <- function(sol) {
    sol$solution
}

##' @title Extract Dual Solution
##' @description The dual solution can be accessed via the method \code{'solution_dual'}.
##' @param sol an object of type \code{'OP_solution'}.
##' @return the extracted solution.
##' @export
solution_dual <- function(sol) UseMethod("solution_dual")

solution_dual.default <- function(sol) {
    stop(sprintf("not available for solver '%s'", sol$solver))
}

##' @title Extract SDP Solution
##' @description The matrix part of the solution from an semidefinite program can 
##'   be accessed via the method \code{'solution_dual'}.
##' @param sol an object of type \code{'OP_solution'}.
##' @return the matrix part of the solution from an semidefinite program if 
##'   available.
##' @export
solution_sdp <- function(sol) UseMethod("solution_sdp")

solution_sdp.default <- function(sol) {
    stop(sprintf("not available for solver '%s'", sol$solver))
}

solution_aux <- function(sol) UseMethod("solution_aux")

solution_aux.default <- function(sol) {
    stop(sprintf("not available for solver '%s'", sol$solver))
}

##' @title Extract Original Solver Solution
##' @description The orginal soltion from the solver utilized by the ROI plugin,
##'   can be accessed via the method \code{'solution_sdp'}.
##' @param sol an object of type \code{'OP_solution'}.
##' @return the matrix part of the solution from an semidefinite program if 
##'   available.
##' @export
solution_solver <- function(sol) UseMethod("solution_solver")

solution_solver.default <- function(sol) {
    stop(sprintf("not available for solver '%s'", sol$solver))
}

################################################################################
## Solution object
################################################################################

.solve_empty_OP <- function( x ) {
    ## Check whether constraints are satisfied (interpreting each lhs as
    ## empty sum with value 0):
    constraints <- split( constraints(x)$rhs, constraints(x)$dir )
    if( all(unlist(Map(function(dir, rhs) get(dir)(0, rhs),
                       names(constraints), constraints))) )
        make_OP_solution( double(), 0, 0L, solver = "ROI_NULL" )
    else
        make_OP_solution( double(), NA_real_, 2L, solver = "ROI_NULL" )
}

make_OP_solution <- function(solution, objval, status, solver, message=NULL, ...)
    structure( list(solution = solution,
                    objval   = objval,
                    status   = status,
                    message  = message),
              meta  = list(solver = solver, ...),
              class = "OP_solution" )

################################################################################
## Methods on solution object
################################################################################

##' @noRd
##' @export
print.OP_solution <- function(x, ...){
    success <- x$status$code == 0
    if( !success ){
        writeLines( "No solution found." )
        writeLines( sprintf("The solver message was: %s", x$status$msg$message) )
    } else{
        writeLines( "Optimal solution found." )
    }
    writeLines( sprintf("The objective value is: %e", x$objval) )
}



################################################################################
## CANONICALIZER
################################################################################

canonicalize_status <- function( status, solver ){
    msg <- get_status_message_from_db( solver, status )
    list( code = msg$roi_code, msg = msg )
}

##  -----------------------------------------------------------
##  Plug-in convenience function: canonicalize_solution
##  =====================
##' @title Canonicalize Solution
##'
##' @description Transform the solution to a standardized form.
##' @param solution a numeric or integer vector giving
##'        the solution of the optimization problem.
##' @param optimum a numeric giving the optimal value.
##' @param status an integer giving the status code (exit flag).
##' @param solver a character string giving the name of the solver.
##' @param message an optional \R object giving the original solver message.
##' @param ... further arguments to be stored in the solution object.
##' @return an object of class \code{"OP_solution"}.
##' @family plugin functions
##' @rdname ROI_plugin_canonicalize_solution
##' @export
.ROI_plugin_canonicalize_solution <- function( solution, optimum, status, solver, message=NULL, ... ) {
    status <- canonicalize_status( status, solver )
    make_OP_solution( solution = solution,
                      objval   = optimum,
                      status   = status,
                      solver   = solver,
                      message  = message, ... )
}
