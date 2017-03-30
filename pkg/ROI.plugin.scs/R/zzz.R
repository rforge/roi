## ROI plugin: SCS
## based on scs interface
make_SCS_signatures <- function()
    ROI_plugin_make_signature( objective = c("L"),
                               constraints = c("X", "L", "C"),
                               types = c("C"),
                               bounds = c("X", "C", "V", "CV"),
                               cones = c("X", "zero", "nonneg", "soc", "psd", "expp", "expd", "powp", "powd"),
                               maximum = c(TRUE, FALSE) )


## SOLVER CONTROLS
.add_controls <- function(solver) {
    ## SCS
    ROI_plugin_register_solver_control( solver, "verbose", "verbose" )
    ROI_plugin_register_solver_control( solver, "max_iters", "max_iter" )
    ROI_plugin_register_solver_control( solver, "eps", "tol" )

    ROI_plugin_register_solver_control( solver, "normalize", "X" )
    ROI_plugin_register_solver_control( solver, "cg_rate", "X" )
    ROI_plugin_register_solver_control( solver, "scale", "X" )
    ROI_plugin_register_solver_control( solver, "rho_x", "X" )
    ROI_plugin_register_solver_control( solver, "alpha", "X" )
    invisible( TRUE )
}

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- ROI_plugin_get_solver_name( pkgname )
        ROI_plugin_register_solver_method( 
            signatures = make_SCS_signatures(),
            solver = solver,
            method = getFunction( "solve_OP", where = getNamespace(pkgname)) )
        ## Finally, for status code canonicalization add status codes to data base
        .add_status_codes()
        .add_controls( solver )
    }
}

