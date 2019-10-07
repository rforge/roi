make_cplexapi_signatures <- function()
    ROI_plugin_make_signature( objective = c("L", "Q"),
                               constraints = c("X", "L", "Q"),
                               types = c("C", "I", "B", "CI", "CB", "IB", "CIB"),
                               bounds = c("X", "V"),
                               cones = c("X"),
                               maximum = c(TRUE, FALSE) )

.add_controls <- function(solver) {

    ROI_plugin_register_solver_control( solver, "method", "X" )

    invisible( TRUE )
}

.onLoad <- function( libname, pkgname ) {
    solver <- "cplexapi"
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        ROI_plugin_register_solver_method(
            signatures = make_cplexapi_signatures(),
            solver = solver,
            method = getFunction( "solve_OP", where = getNamespace(pkgname)) )
        ## Finally, for status code canonicalization add status codes to data base
        .add_status_codes( solver )
        .add_controls( solver )
    }
}

