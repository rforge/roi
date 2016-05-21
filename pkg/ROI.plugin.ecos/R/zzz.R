## ROI plugin: ECOS
## based on ecos interface

make_ECOS_signatures <- function()
    ROI_make_signature( objective = c("L"),
                        constraints = c("L"),
                        types = c("B", "I", "C"),
                        bounds = c("C"),
                        cones = c("free", "nonneg", "soc", "expp"),
                        maximum = c(TRUE, FALSE) )

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- .ROI_plugin_get_solver_name( pkgname )
        .ROI_plugin_register_solver_method(
            signatures = make_ECOS_signatures(),
            solver = solver,
            method = getFunction( "solve_OP", where = getNamespace(pkgname)) )
        ## Finally, for status code canonicalization add status codes to data base
        .add_status_codes()
    }
}

