
.add_controls <- function(solver) {
    ROI_plugin_register_solver_control( solver, "start", "start")
    ROI_plugin_register_solver_control( solver, "dry_run", "dry_run")
    
    ROI_plugin_register_solver_control( solver, "trace", "verbose" )
    ROI_plugin_register_solver_control( solver, "maxiters", "max_iter" )
    ROI_plugin_register_solver_control( solver, "tol", "tol" )
    ROI_plugin_register_solver_control( solver, "abstol", "abs_tol" )
    ROI_plugin_register_solver_control( solver, "reltol", "rel_tol" )

    ROI_plugin_register_solver_control( solver, "feastol", "X" )
    ROI_plugin_register_solver_control( solver, "stepadj", "X" )
    ROI_plugin_register_solver_control( solver, "beta", "X" )

    invisible( TRUE )
}

