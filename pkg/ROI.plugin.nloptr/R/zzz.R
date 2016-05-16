## ROI plugin: NLOPTR
## based on nloptr interface

ROI_make_NLP_FXCV_signatures <- function()
    ROI_make_signature( objective = c("F"),
                        constraints = c("X", "F"),
                        types = c("C"),
                        bounds = c("X", "V"),
                        cones = c("free"),
                        maximum = c(TRUE, FALSE) )

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- get_solver_name( pkgname )
        ##solver <- "nloptr"
        ##pkgname <- "ROI.plugin.nloptr"
        ## obj: F    constr: X    types: C    bounds: V
        ROI_register_solver_method( 
            signatures = ROI_make_NLP_FXCV_signatures(),
            solver = solver,
            method = getFunction( "solve_nloptr", where = getNamespace(pkgname)) )
        .add_status_codes()
    }
}
