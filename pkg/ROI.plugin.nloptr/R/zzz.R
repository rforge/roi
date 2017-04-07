## ROI plugin: NLOPTR
## based on nloptr interface

ROI_make_NLP_FXCV_signatures <- function()
    ROI_plugin_make_signature( objective = c("L", "Q", "F"),
                                constraints = c("X", "L", "Q", "F"),
                                types = c("C"),
                                bounds = c("X", "V"),
                                cones = c("X"),
                                maximum = c(TRUE, FALSE) )

## SOLVER CONTROLS
.add_controls <- function(solver) {
    ## NLOPTR
    ROI_plugin_register_solver_control( solver, "maxeval", "max_iter" )
    ROI_plugin_register_solver_control( solver, "xtol_rel", "tol" )
    ROI_plugin_register_solver_control( solver, "algorithm", "method" )
    ROI_plugin_register_solver_control( solver, "x0", "start" )
    ROI_plugin_register_solver_control( solver, "maxtime", "max_time" )
    ROI_plugin_register_solver_control( solver, "stopval", "X" )
    ROI_plugin_register_solver_control( solver, "ftol_rel", "X" )
    ROI_plugin_register_solver_control( solver, "ftol_abs", "X" )
    ROI_plugin_register_solver_control( solver, "xtol_abs", "X" )
    ROI_plugin_register_solver_control( solver, "tol_constraints_ineq", "X" )
    ROI_plugin_register_solver_control( solver, "tol_constraints_eq", "X" )
    ROI_plugin_register_solver_control( solver, "print_level", "X" )
    ROI_plugin_register_solver_control( solver, "check_derivatives", "X" )
    ROI_plugin_register_solver_control( solver, "check_derivatives_tol", "X" )
    ROI_plugin_register_solver_control( solver, "check_derivatives_print", "X" )
    ROI_plugin_register_solver_control( solver, "print_options_doc", "X" )
    ROI_plugin_register_solver_control( solver, "population", "X" )
    ROI_plugin_register_solver_control( solver, "ranseed", "X" )
    ROI_plugin_register_solver_control( solver, "local_opts", "X" )

    invisible( TRUE )
}

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- ROI_plugin_get_solver_name( pkgname )
        ##solver <- "nloptr"
        ##pkgname <- "ROI.plugin.nloptr"
        ## obj: F    constr: X    types: C    bounds: V
        ROI_plugin_register_solver_method(
            signatures = ROI_make_NLP_FXCV_signatures(),
            solver = solver,
            method = getFunction( "solve_nloptr", where = getNamespace(pkgname)) )
        .add_status_codes()
        .add_controls( solver )
    }
}
