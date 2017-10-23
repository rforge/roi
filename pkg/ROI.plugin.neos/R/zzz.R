make_QCQP_signatures <- function()
    ROI_plugin_make_signature( objective = c("L", "Q"),
                               constraints = c("X", "L", "Q"),
                               types = c("C", "I", "B", "CI", "CB", "IB", "CIB"),
                               bounds = c("X", "V"),
                               cones = c("X"),
                               maximum = c(TRUE, FALSE) )

## SOLVER CONTROLS
.add_controls <- function(solver) {
    ## lp_solve
    ROI_plugin_register_solver_control( solver, "dry_run", "dry_run" )
    ROI_plugin_register_solver_control( solver, "method", "method" )

    ROI_plugin_register_solver_control( solver, "user", "X" )
    ROI_plugin_register_solver_control( solver, "email", "X" )
    ROI_plugin_register_solver_control( solver, "interface", "X" )
    ROI_plugin_register_solver_control( solver, "id", "X" )
    ROI_plugin_register_solver_control( solver, "wait", "X" )

    invisible( TRUE )
}

.add_reader_writer <- function(solver) {
    ROI_plugin_register_writer("gams", solver, make_QCQP_signatures(), write_gams)
    invisible(NULL)
}

.onLoad <- function( libname, pkgname ) {
    if( ! pkgname %in% ROI_registered_solvers() ){   
        solver <- "neos"
        ROI_plugin_register_solver_method( 
            signatures = make_QCQP_signatures(),
            solver = solver,
            method = getFunction( "solve_OP", where = getNamespace(pkgname)) )
        .add_status_codes()
        .add_controls(solver)
        .add_reader_writer(solver)
    }
}
