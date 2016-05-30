## ROI plugin: NLOPTR
## based on nloptr interface

ROI_make_NLP_FXCV_signatures <- function()
    .ROI_plugin_make_signature( objective = c("F"),
                                constraints = c("X", "F"),
                                types = c("C"),
                                bounds = c("X", "V"),
                                cones = c("free"),
                                maximum = c(TRUE, FALSE) )

## SOLVER CONTROLS
.add_controls <- function(solver) {
    ## NLOPTR
    .ROI_plugin_register_solver_control( solver, "maxeval", "max_iter" )
    .ROI_plugin_register_solver_control( solver, "xtol_rel", "tol" )
    invisible( TRUE )
}

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- .ROI_plugin_get_solver_name( pkgname )
        ##solver <- "nloptr"
        ##pkgname <- "ROI.plugin.nloptr"
        ## obj: F    constr: X    types: C    bounds: V
        .ROI_plugin_register_solver_method( 
            signatures = ROI_make_NLP_FXCV_signatures(),
            solver = solver,
            method = getFunction( "solve_nloptr", where = getNamespace(pkgname)) )
        .add_status_codes()
        .add_controls( solver )
    }
}
