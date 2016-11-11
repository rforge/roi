## ROI plugin: alabama
## based on Rsolnp interface

make_solnp_signature <- function()
    .ROI_plugin_make_signature( objective = c("L", "Q", "F"),
                                constraints = c("X", "L", "Q", "F"),
                                types = c("C"),
                                bounds = c("X", "V"),
                                cones = c("free"),
                                maximum = c(TRUE, FALSE) )

## SOLVER CONTROLS
.add_controls <- function(solver) {
    ## Rsolnp
    .ROI_plugin_register_solver_control( solver, "pars", "start" )
    .ROI_plugin_register_solver_control( solver, "xtol_rel", "tol" )
    .ROI_plugin_register_solver_control( solver, "trace", "verbose" )

    .ROI_plugin_register_solver_control( solver, "rho", "X" )
    .ROI_plugin_register_solver_control( solver, "outer.iter", "X" )
    .ROI_plugin_register_solver_control( solver, "inner.iter", "X" )
    .ROI_plugin_register_solver_control( solver, "delta", "X" )

    invisible( TRUE )
}

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        solver <- "alabama"
        ## obj: F    constr: X    types: C    bounds: V
        .ROI_plugin_register_solver_method(
            signatures = make_solnp_signature(),
            solver = solver,
            method = getFunction("solve_alabama_auglag",
                                 where = getNamespace(pkgname)) )
        .solnp_add_status_codes()
        .add_controls( solver )
    }
}
