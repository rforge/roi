################################################################################
## Package: ROI
## File:    plugin.R
## Author:  Stefan Theussl
## Changed: 2016-05-27
################################################################################


##  -----------------------------------------------------------
##  .ROI_plugin_register_solver_control
##  ==========================
##' @title Register Solver Controls
##'
##' @description Register a new solver control argument.
##' @param args a character vector specifying with the supported signatures.
##' @param solver a character string giving the solver name.
##' @param roi_control a character vector specifying the corresponding ROI control argument.
##' @return TRUE on success
##' @family plugin functions
##' @rdname ROI_plugin_register_solver_control
##' @export
.ROI_plugin_register_solver_control <- function( args, solver, roi_control = "X" ){
    args <- as.character( args )
    if( length(roi_control) == 1L )
        roi_control <- rep( as.character(roi_control), length(args) )
    stopifnot( length(args) == length(roi_control) )
    for( i in seq_along(args) )
        control_db$set_entry( args[i], solver, roi_control[i] )
    invisible( TRUE )
}

ROI_available_solver_controls <- function(){
    c( "X",         ## no corresponding ROI control
       "verbose" )  ## LOGICAL: turn on/off solver output on terminal
}
