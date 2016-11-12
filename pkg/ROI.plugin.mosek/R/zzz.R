##' TODO:
##' - add quadratic and conic formulations

make_MIQCP_signatures <- function()
    .ROI_plugin_make_signature( objective = c("L", "Q"),
                                constraints = c("X", "L"),## "Q",
                                types = c("C", "I", "B"),##, "CI", "CB", "IB", "CIB"),
                                bounds = c("X", "V"),
                                cones = c("free"),
                                maximum = c(TRUE, FALSE) )

## SOLVER CONTROLS
.add_controls <- function(solver) {
    ## mosek
    .ROI_plugin_register_solver_control( solver, "dry_run", "X" )

    .ROI_plugin_register_solver_control( solver, "verbose", "verbose" )

    .ROI_plugin_register_solver_control( solver, "usesol", "X" )
    .ROI_plugin_register_solver_control( solver, "useparam", "X" )
    .ROI_plugin_register_solver_control( solver, "soldetail", "X" )
    .ROI_plugin_register_solver_control( solver, "getinfo", "X" )
    .ROI_plugin_register_solver_control( solver, "scofile", "X" )

    invisible( TRUE )
}

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- .ROI_plugin_get_solver_name( pkgname )
        .ROI_plugin_register_solver_method( 
            signatures = make_MIQCP_signatures(),
            solver = solver,
            method = getFunction( "solve_OP", where = getNamespace(pkgname)) )
        ## Finally, for status code canonicalization add status codes to data base
        .add_status_codes()
        .add_controls(solver)
    }
}
