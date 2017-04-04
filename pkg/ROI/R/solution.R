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
##' @param x an object of type \code{'OP_solution'}.
##' @param type a character giving the name of the solution to be extracted.
##' @param ... further arguments passed to or from other methods.
##' @return the extracted solution.
##' @export
solution <- function(x, type=c("primal", "dual", "aux", "psd", "msg"), ...) {
    UseMethod("solution")
}

##' @noRd
##' @export
solution.default <- function(x, type=c("primal", "dual", "aux", "psd", "msg"), ...) {
    type <- match.arg(type)
    switch(type,
           primal = ROI_plugin_solution_prim(x),
           dual   = ROI_plugin_solution_dual(x),
           aux    = ROI_plugin_solution_aux(x) ,
           psd    = ROI_plugin_solution_psd(x) ,
           msg    = ROI_plugin_solution_msg(x)  )
}

##' @title Extract solution from the solver.
##' @description Define a getter function for the solution of the solver.
##' @param x an \code{R} object inheriting from \code{solution} or \code{solutions}.
##' @return the corresponding solution/s.
##' @examples
##' ## TODO
##' @family plugin functions
##' @rdname ROI_plugin_solution
##' @export
ROI_plugin_solution_prim <- function(x) {
    UseMethod("ROI_plugin_solution_prim")
}

##' @rdname ROI_plugin_solution
##' @export
ROI_plugin_solution_prim.OP_solution <- function(x) {
    x$solution
}

##' @rdname ROI_plugin_solution
##' @export
ROI_plugin_solution_prim.OP_solutions <- function(x) {
    lapply(x, "[[", "solution")
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
ROI_plugin_solution_dual.OP_solutions <- function(x) {
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
ROI_plugin_solution_aux.OP_solutions <- function(x) {
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
ROI_plugin_solution_psd.OP_solutions <- function(x) {
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
ROI_plugin_solution_msg.OP_solutions <- function(x) {
    lapply(x, "[[", "message")
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
    structure( list(solution = if( !status$code )
                                   solution
                               else
                                   rep(NA_real_, length(solution)),
                    objval   = ifelse( !status$code, objval, NA_real_ ),
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
        writeLines( "No solution found." )
        writeLines( sprintf("The solver message was: %s", x$status$msg$message) )
    } else{
        writeLines( "Optimal solution found." )
    }
    writeLines( sprintf("The objective value is: %e", x$objval) )
}

##' @noRd
##' @export
print.OP_solutions <- function(x, ...) {
    success <- x[[1L]]$status$code == 0
    if( !success ){
        writeLines( "No solution found." )
        writeLines( sprintf("The solver message was: %s", x[[1L]]$status$msg$message) )
    } else{
        writeLines( sprintf("%i optimal solutions found.", length(x)) )
    }
    writeLines( sprintf("The objective value is: %e", x[[1L]]$objval) )   
}

