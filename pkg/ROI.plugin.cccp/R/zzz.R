cccp_signature <- function()
    ROI_plugin_make_signature(  objective = c("L", "F"),
                                constraints = c("X", "L"),
                                types = c("C"),
                                bounds = c("X", "V"),
                                cones = c("X"),
                                maximum = c(TRUE, FALSE) )

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- "cccp"
        ROI_plugin_register_solver_method( 
            signatures = cccp_signature(),
            solver = solver,
            method = getFunction( "solve_OP", where = getNamespace(pkgname)) 
        )
        ## Finally, for status code canonicalization add status codes to data base
        .add_status_codes(solver)
        .add_controls( solver )
    }
}
