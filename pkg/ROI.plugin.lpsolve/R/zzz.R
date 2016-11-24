make_MILP_signatures <- function()
    .ROI_plugin_make_signature( objective = c("L"),
                                constraints = c("X", "L"),
                                types = c("C", "I", "B", "CI", "CB", "IB", "CIB"),
                                bounds = c("X", "V"),
                                cones = c("free"),
                                maximum = c(TRUE, FALSE) )

## SOLVER CONTROLS
.add_controls <- function(solver) {
    ## lp_solve
    .ROI_plugin_register_solver_control( solver, "dry_run", "X" )

    .ROI_plugin_register_solver_control( solver, "verbose", "verbose" )

    .ROI_plugin_register_solver_control( solver, "anti.degen", "X" )
    .ROI_plugin_register_solver_control( solver, "basis.crash", "X" )
    .ROI_plugin_register_solver_control( solver, "bb.depthlimit", "X" )
    .ROI_plugin_register_solver_control( solver, "bb.floorfirst", "X" )
    .ROI_plugin_register_solver_control( solver, "bb.rule", "X" )
    .ROI_plugin_register_solver_control( solver, "break.at.first", "X" )
    .ROI_plugin_register_solver_control( solver, "break.at.value", "X" )
    .ROI_plugin_register_solver_control( solver, "epslevel", "X" )
    .ROI_plugin_register_solver_control( solver, "epsb", "X" )
    .ROI_plugin_register_solver_control( solver, "epsd", "X" )
    .ROI_plugin_register_solver_control( solver, "epsel", "X" )
    .ROI_plugin_register_solver_control( solver, "epsint", "X" )
    .ROI_plugin_register_solver_control( solver, "epsperturb", "X" )
    .ROI_plugin_register_solver_control( solver, "epspivot", "X" )
    .ROI_plugin_register_solver_control( solver, "improve", "X" )
    .ROI_plugin_register_solver_control( solver, "infinite", "X" )
    .ROI_plugin_register_solver_control( solver, "maxpivot", "X" )
    .ROI_plugin_register_solver_control( solver, "mip.gap", "X" )
    .ROI_plugin_register_solver_control( solver, "negrange", "X" )
    .ROI_plugin_register_solver_control( solver, "obj.in.bas", "X" )
    .ROI_plugin_register_solver_control( solver, "pivoting", "X" )
    .ROI_plugin_register_solver_control( solver, "presolve", "X" )
    .ROI_plugin_register_solver_control( solver, "scalelimit", "X" )
    .ROI_plugin_register_solver_control( solver, "scaling", "X" )
    .ROI_plugin_register_solver_control( solver, "sense", "X" )
    .ROI_plugin_register_solver_control( solver, "simplextype", "X" )
    .ROI_plugin_register_solver_control( solver, "timeout", "X" )
    invisible( TRUE )
}

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- .ROI_plugin_get_solver_name( pkgname )
        .ROI_plugin_register_solver_method( 
            signatures = make_MILP_signatures(),
            solver = solver,
            method = getFunction( "solve_OP", where = getNamespace(pkgname)) )
        ## Finally, for status code canonicalization add status codes to data base
        .add_status_codes()
        .add_controls(solver)
    }
}
