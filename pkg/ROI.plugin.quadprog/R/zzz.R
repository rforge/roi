make_QP_signatures <- function() {
    ROI_plugin_make_signature( objective = "Q",
                        constraints = c("X", "L"),
                        types = c("C"),
                        bounds = c("X", "V"),
                        cones = c("X"),
                        maximum = c(TRUE, FALSE) )
}

.onLoad <- function( libname, pkgname ) {
  library.dynam( "quadprog", package = "quadprog", lib.loc = .libPaths() )
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- ROI_plugin_get_solver_name( pkgname )
        ROI_plugin_register_solver_method( signatures = make_QP_signatures(),
                                            solver = solver,
                                            method =
            getFunction( "solve_QP", where = getNamespace(pkgname)) )
        ## Finally, for status code canonicalization add status codes to data base
        .add_status_codes()
    }
}

#.onUnload <- function( libpath ){
#    ROI_plugin_deregister_solver_methods( solver = "glpk" )
#}
