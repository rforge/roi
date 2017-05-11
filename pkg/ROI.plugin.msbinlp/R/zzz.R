make_BILP_signatures <- function()
    ROI_plugin_make_signature( objective = c("L"),
                               constraints = c("X", "L"),
                               types = c("B", "CB", "IB", "CIB"),
                               bounds = c("X", "V"),
                               cones = c("X"),
                               maximum = c(TRUE, FALSE) )

## SOLVER CONTROLS
.add_controls <- function(solver) {
    ## lp_solve
    ROI_plugin_register_solver_control( solver, "dry_run", "dry_run" )
    ROI_plugin_register_solver_control( solver, "nsol_max", "nsol_max" )
    ROI_plugin_register_solver_control( solver, "verbose", "verbose" )

    ROI_plugin_register_solver_control( solver, "presolve", "presolve" )
    ROI_plugin_register_solver_control( solver, "max_iter", "max_iter" )
    ROI_plugin_register_solver_control( solver, "max_time", "max_time" )
    ROI_plugin_register_solver_control( solver, "tol", "tol" )
    ROI_plugin_register_solver_control( solver, "abs_tol", "abs_tol" )
    ROI_plugin_register_solver_control( solver, "rel_tol", "rel_tol" )
    ROI_plugin_register_solver_control( solver, "method", "method" )
    ROI_plugin_register_solver_control( solver, "start", "start" )

    ROI_plugin_register_solver_control( solver, "order", "X" )

    invisible( TRUE )
}

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- ROI_plugin_get_solver_name( pkgname )
        ROI_plugin_register_solver_method( 
            signatures = make_BILP_signatures(),
            solver = solver,
            method = getFunction( "solve_OP", where = getNamespace(pkgname)) )
        ## Finally, for status code canonicalization add status codes to data base
        .add_status_codes()
        .add_controls(solver)
    }
}
