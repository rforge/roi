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
##' @param x an object of type \code{'OP_solution'} or \code{'OP_solution_set'}.
##' @param type a character giving the name of the solution to be extracted.
##' @param force a logical to control the return value in the case that the 
##'        status code is equal to 1 (i.e. something went wrong).
##'        By default force is \code{FALSE} and a solution is only provided
##'        if the status code is equal to 0 (i.e. success). If force is \code{TRUE}
##'        \pkg{ROI} ignores the status code and also returns solutions
##'        where the solver signaled an issue.
##' @param ... further arguments passed to or from other methods.
##' @return the extracted solution.
##' @export
solution <- function(x, type = c("primal", "dual", "aux", "psd", "msg", "objval", 
                                 "status", "status_code"), force = FALSE, ...) {
    UseMethod("solution")
}

##' @noRd
##' @export
solution.default <- function(x, type = c("primal", "dual", "aux", "psd", "msg", 
                                         "objval", "status", "status_code"), force = FALSE, ...) {
    type <- match.arg(type)
    switch(type,
           primal = ROI_plugin_solution_prim(x, force = force),
           dual   = ROI_plugin_solution_dual(x),
           aux    = ROI_plugin_solution_aux(x),
           psd    = ROI_plugin_solution_psd(x),
           msg    = ROI_plugin_solution_msg(x),
           objval = ROI_plugin_solution_objval(x, force = force),
           status = ROI_plugin_solution_status(x),
           status_code = ROI_plugin_solution_status_code(x) )
}

##' @title Extract solution from the solver.
##' @description Generic getter functions used by the function 
##'     \code{\link{solution}}. These functions can be used to write
##'     a solver specific getter function.
##' 
##' @param x an \code{R} object inheriting from \code{solution} or \code{solutions}.
##' @param force a logical to control the return value in the case that the 
##'        status code is equal to 1 (i.e. something went wrong).
##'        By default force is \code{FALSE} and a solution is only provided
##'        if the status code is equal to 0 (i.e. success). If force is \code{TRUE}
##'        \pkg{ROI} ignores the status code and also returns solutions
##'        where the solver signaled an issue.
##' @return the corresponding solution/s.
##' @family plugin functions
##' @rdname ROI_plugin_solution
##' @export
ROI_plugin_solution_prim <- function(x, force = FALSE) {
    UseMethod("ROI_plugin_solution_prim")
}

##' @rdname ROI_plugin_solution
##' @export
ROI_plugin_solution_prim.OP_solution <- function(x, force = FALSE) {
    if ( isTRUE(as.logical(x[["status"]][["code"]])) & !force )
        return( rep(NA_real_, length(x$solution)) )
    x$solution
}

##' @rdname ROI_plugin_solution
##' @export
ROI_plugin_solution_prim.OP_solution_set <- function(x, force = FALSE) {
    lapply(x, ROI_plugin_solution_prim)
}

##  @title Extract Dual Solution
##  @description The dual solution can be accessed via the method \code{'solution_dual'}.
##  @param x an object of type \code{'OP_solution'}.
##  @return the extracted solution.
##' @rdname ROI_plugin_solution
##' @export
ROI_plugin_solution_dual <- function(x) {
    UseMethod("ROI_plugin_solution_dual")
}

##' @noRd
##' @export
ROI_plugin_solution_dual.OP_solution <- function(x) {
    NA
}

##' @noRd
##' @export
ROI_plugin_solution_dual.OP_solution_set <- function(x) {
    NA
}

##' @rdname ROI_plugin_solution
##' @export
ROI_plugin_solution_aux <- function(x) {
    UseMethod("ROI_plugin_solution_aux")
}

##' @noRd
##' @export
ROI_plugin_solution_aux.OP_solution <- function(x) {
    NA
}

##' @noRd
##' @export
ROI_plugin_solution_aux.OP_solution_set <- function(x) {
    NA
}

## @title Extract Positive Semi-Definite Matrices
## @description The matrix part of the solution from an semidefinite program can
##   be accessed via the method \code{'solution_psd'}.
## @param sol an object of type \code{'OP_solution'}.
## @return the matrix part of the solution from an semidefinite program if
##   available.
##' @rdname ROI_plugin_solution
##' @export
ROI_plugin_solution_psd <- function(x) {
    UseMethod("ROI_plugin_solution_psd")
}

##' @noRd
##' @export
ROI_plugin_solution_psd.OP_solution <- function(x) {
    NA
}

##' @noRd
##' @export
ROI_plugin_solution_psd.OP_solution_set <- function(x) {
    NA
}

##' @rdname ROI_plugin_solution
##' @export
ROI_plugin_solution_msg <- function(x) {
    UseMethod("ROI_plugin_solution_msg")
}

##' @noRd
##' @export
ROI_plugin_solution_msg.OP_solution <- function(x) {
    x$message
}

##' @noRd
##' @export
ROI_plugin_solution_msg.OP_solution_set <- function(x) {
    lapply(x, "[[", "message")
}

##' @rdname ROI_plugin_solution
##' @export
ROI_plugin_solution_status_code <- function(x) {
    UseMethod("ROI_plugin_solution_status_code")
}

##' @noRd
##' @export
ROI_plugin_solution_status_code.OP_solution <- function(x) {
    x$status$code
}

##' @noRd
##' @export
ROI_plugin_solution_status_code.OP_solution_set <- function(x) {
    lapply(x, ROI_plugin_solution_status_code)
}

##' @rdname ROI_plugin_solution
##' @export
ROI_plugin_solution_status <- function(x) {
    UseMethod("ROI_plugin_solution_status")
}

##' @noRd
##' @export
ROI_plugin_solution_status.OP_solution <- function(x) {
    x$status
}

##' @noRd
##' @export
ROI_plugin_solution_status.OP_solution_set <- function(x) {
    lapply(x, ROI_plugin_solution_status)
}

##' @rdname ROI_plugin_solution
##' @export
ROI_plugin_solution_objval <- function(x, force = FALSE) {
    UseMethod("ROI_plugin_solution_objval")
}

##' @noRd
##' @export
ROI_plugin_solution_objval.OP_solution <- function(x, force = FALSE) {
    if ( isTRUE(as.logical(x[["status"]][["code"]])) & !force )
        return( NA_real_ )
    x$objval
}

##' @noRd
##' @export
ROI_plugin_solution_objval.OP_solution_set <- function(x, force = FALSE) {
    lapply(x, ROI_plugin_solution_objval)
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

make_OP_solution <- function(solution, objval, status, solver, message = NULL, ...){
    if( is.null(status$code) ) ## a status code for the solution is a necessary condition.
        stop( sprintf("unknown solver status code. Please contact the ROI.plugin.%s maintainer.", solver) )
    structure( list(solution = solution,
                    objval   = objval,
                    status   = status,
                    message  = message),
              meta  = list(solver = solver, ...),
              class = c(sprintf("%s_solution", solver), "OP_solution") )
}
################################################################################
## Methods on solution object
################################################################################

##' @noRd
##' @export
print.OP_solution <- function(x, ...){
    success <- x$status$code == 0
    if( !success ){
        writeLines( "No optimal solution found." )
        writeLines( sprintf("The solver message was: %s", x$status$msg$message) )
    } else{
        writeLines( "Optimal solution found." )
    }
    writeLines( sprintf("The objective value is: %e", x$objval) )
}

##' @noRd
##' @export
print.OP_solution_set <- function(x, ...) {
    success <- x[[1L]]$status$code == 0
    if( !success ){
        writeLines( "No optimal solution found." )
        writeLines( sprintf("The solver message was: %s", x[[1L]]$status$msg$message) )
    } else{
        writeLines( sprintf("%i optimal solutions found.", length(x)) )
    }
    writeLines( sprintf("The objective value is: %e", x[[1L]]$objval) )   
}

