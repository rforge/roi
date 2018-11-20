## ROI plugin: ECOS
## based on ecos interface

make_ECOS_signatures <- function()
    ROI_plugin_make_signature( objective = c("L"),
                                constraints = c("X", "L", "C"),
                                types = c("C", "I", "B", "CI", "CB", "IB", "CIB"),
                                bounds = c("X", "V"),
                                cones = c("X", "zero", "nonneg", "soc", "expp"),
                                maximum = c(TRUE, FALSE) )

## SOLVER CONTROLS
.add_controls <- function(solver) {
    ## ECOS
    ROI_plugin_register_solver_control( solver, "verbose", "verbose" )
    ROI_plugin_register_solver_control( solver, "maxit", "max_iter" )
    ROI_plugin_register_solver_control( solver, "feastol", "tol" ) ## tolerance on the primal and dual residual

    ROI_plugin_register_solver_control( solver, "reltol", "X" )
    ROI_plugin_register_solver_control( solver, "abstol", "X" )
    ROI_plugin_register_solver_control( solver, "feastol_inacc", "X" )
    ROI_plugin_register_solver_control( solver, "abstol_inacc", "X" )
    ROI_plugin_register_solver_control( solver, "reltol_inacc", "X" )
    
    ROI_plugin_register_solver_control( solver, "mi_max_iters", "X" )
    ROI_plugin_register_solver_control( solver, "mi_int_tol", "X" )
    ROI_plugin_register_solver_control( solver, "mi_abs_eps", "X" )
    ROI_plugin_register_solver_control( solver, "mi_rel_eps", "X" )

    invisible( TRUE )
}

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- "ecos"
        ROI_plugin_register_solver_method(
            signatures = make_ECOS_signatures(),
            solver = solver,
            method = getFunction( "solve_OP", where = getNamespace(pkgname)) )
        ## Finally, for status code canonicalization add status codes to data base
        .add_status_codes()
        .add_controls( solver )
    }
}

