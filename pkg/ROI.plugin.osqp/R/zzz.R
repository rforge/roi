make_osqp_signatures <- function()
    ROI_plugin_make_signature( objective = c("Q", "L"),
                               constraints = c("X", "L"),
                               types = c("C"),
                               bounds = c("X", "V"),
                               cones = c("X"),
                               maximum = c(TRUE, FALSE) )

.add_controls <- function(solver) {

    ROI_plugin_register_solver_control( solver, "rho", "X" )
    ROI_plugin_register_solver_control( solver, "sigma", "X" )
    ROI_plugin_register_solver_control( solver, "max_iter", "max_iter" )
    ROI_plugin_register_solver_control( solver, "eps_abs", "abs_tol" )
    ROI_plugin_register_solver_control( solver, "eps_rel", "rel_tol" )
    ROI_plugin_register_solver_control( solver, "eps_prim_inf", "X" )
    ROI_plugin_register_solver_control( solver, "eps_dual_inf", "X" )
    ROI_plugin_register_solver_control( solver, "alpha", "X" )
    ROI_plugin_register_solver_control( solver, "linsys_solver", "X" )
    ROI_plugin_register_solver_control( solver, "delta", "X" )
    ROI_plugin_register_solver_control( solver, "polish", "X" )
    ROI_plugin_register_solver_control( solver, "polish_refine_iter", "X" )
    ROI_plugin_register_solver_control( solver, "iterative", "X" )
    ROI_plugin_register_solver_control( solver, "verbose", "verbose" )
    ROI_plugin_register_solver_control( solver, "scaled_termination", "X" )
    ROI_plugin_register_solver_control( solver, "check_termination", "X" )
    ROI_plugin_register_solver_control( solver, "warm_start", "X" )
    ROI_plugin_register_solver_control( solver, "scaling", "X" )
    ROI_plugin_register_solver_control( solver, "adaptive_rho", "X" )
    ROI_plugin_register_solver_control( solver, "adaptive_rho_interval", "X" )
    ROI_plugin_register_solver_control( solver, "adaptive_rho_tolerance", "X" )

    invisible( TRUE )
}

.onLoad <- function( libname, pkgname ) {
    solver <- "osqp"
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        ROI_plugin_register_solver_method(
            signatures = make_osqp_signatures(),
            solver = solver,
            method = getFunction( "solve_OP", where = getNamespace(pkgname)) )
        ## Finally, for status code canonicalization add status codes to data base
        .add_status_codes( solver )
        .add_controls( solver )
    }
}

