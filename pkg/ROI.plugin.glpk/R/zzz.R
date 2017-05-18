make_MILP_signatures <- function() {
    ROI_plugin_make_signature( objective = "L",
                        constraints = c("X", "L"),
                        types = c("C", "I", "B", "CI", "CB", "IB", "CIB"),
                        bounds = c("X", "V"),
                        cones = c("X"),
                        maximum = c(TRUE, FALSE) )
}

.add_reader_writer <- function(solver) {
    ROI_plugin_register_reader("mps_fixed", solver, make_MILP_signatures(), read_lp_mps_fixed)
    ROI_plugin_register_reader("mps_free", solver, make_MILP_signatures(), read_lp_mps_free)
    ROI_plugin_register_reader("lp_cplex", solver, make_MILP_signatures(), read_lp_cplex_lp)
    ROI_plugin_register_reader("mathprog", solver, make_MILP_signatures(), read_lp_math_prog)

    ROI_plugin_register_writer("mps_fixed", solver, make_MILP_signatures(), write_lp_mps_fixed)
    ROI_plugin_register_writer("mps_free", solver, make_MILP_signatures(), write_lp_mps_free)
    ROI_plugin_register_writer("lp_cplex", solver, make_MILP_signatures(), write_lp_cplex_lp)
    ## If I write out and read in again I get an error.
    ## ROI_plugin_register_writer("mathprog", solver, make_MILP_signatures(), write_lp_math_prog)
    invisible(NULL)
}

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- ROI_plugin_get_solver_name( pkgname )
        ROI_plugin_register_solver_method( signatures = make_MILP_signatures(),
                                            solver = solver,
                                            method =
            getFunction( "solve_OP", where = getNamespace(pkgname)) )
        ## For status code canonicalization add status codes to data base
        .add_status_codes()
        ## Finally, for control argument canonicalization add controls to data base
        .add_controls()
        .add_reader_writer(solver)
    }
}

#.onUnload <- function( libpath ){
#    ROI_plugin_deregister_solver_methods( solver = "glpk" )
#}
