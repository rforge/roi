## ROI plugin: optimx

ROI_make_NLP_FXCV_signatures <- function()
    ROI_plugin_make_signature( objective = c("F"),
                                constraints = c("X"),
                                types = c("C"),
                                bounds = c("X", "V"),
                                cones = c("X"),
                                maximum = c(TRUE, FALSE) )

## SOLVER CONTROLS
.add_controls <- function(solver) {
    ## optimx
    ROI_plugin_register_solver_control( solver, "method", "method" )
    ROI_plugin_register_solver_control( solver, "par", "start" )
    ROI_plugin_register_solver_control( solver, "itnmax", "max_iter" )
    ROI_plugin_register_solver_control( solver, "trace", "verbose" )
    ROI_plugin_register_solver_control( solver, "abstol", "abs_tol" )
    ROI_plugin_register_solver_control( solver, "reltol", "rel_tol" )


    ROI_plugin_register_solver_control( solver, "hessian", "X" )
    ROI_plugin_register_solver_control( solver, "follow.on", "X" )
    ROI_plugin_register_solver_control( solver, "save.failures", "X" )
    ROI_plugin_register_solver_control( solver, "all.methods", "X" )
    ROI_plugin_register_solver_control( solver, "kkt", "X" )
    ROI_plugin_register_solver_control( solver, "kkttol", "X" )
    ROI_plugin_register_solver_control( solver, "kkt2tol", "X" )
    ROI_plugin_register_solver_control( solver, "starttests", "X" )
    ROI_plugin_register_solver_control( solver, "dowarn", "X" )
    ROI_plugin_register_solver_control( solver, "badval", "X" )
    ROI_plugin_register_solver_control( solver, "usenumDeriv", "X" )

    ## control elements apply only to some of the methods
    ROI_plugin_register_solver_control( solver, "fnscale", "X" )
    ROI_plugin_register_solver_control( solver, "parscale", "X" )
    ROI_plugin_register_solver_control( solver, "ndeps", "X" )
    ROI_plugin_register_solver_control( solver, "maxit", "X" )
    ROI_plugin_register_solver_control( solver, "alpha", "X" )
    ROI_plugin_register_solver_control( solver, "beta", "X" )
    ROI_plugin_register_solver_control( solver, "gamma", "X" )
    ROI_plugin_register_solver_control( solver, "REPORT", "X" )
    ROI_plugin_register_solver_control( solver, "type", "X" )
    ROI_plugin_register_solver_control( solver, "lmm", "X" )
    ROI_plugin_register_solver_control( solver, "factr", "X" )
    ROI_plugin_register_solver_control( solver, "pgtol", "X" )

    invisible( TRUE )
}

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- ROI_plugin_get_solver_name( pkgname )
        ROI_plugin_register_solver_method(
            signatures = ROI_make_NLP_FXCV_signatures(),
            solver = solver,
            method = getFunction( "solve_optimx", where = getNamespace(pkgname)) )
        .add_status_codes()
        .add_controls( solver )
    }
}

