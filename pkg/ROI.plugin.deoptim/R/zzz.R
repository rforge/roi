## ROI plugin: DEoptim

##' VTR = -Inf
##' strategy = 2
##' bs = FALSE
##' NP = NA
##' itermax = 200
##' CR = 0.5
##' F = 0.8
##' trace = TRUE
##' initialpop = NULL
##' storepopfrom = itermax + 1
##' storepopfreq = 1
##' p = 0.2
##' c = 0
##' reltol
##' steptol
##' parallelType = 0
##' packages = c()
##' parVar = c()
##' foreachArgs = list()

ROI_make_NLP_FXCV_signatures <- function()
    ROI_plugin_make_signature( objective = c("L", "Q", "F"),
                                constraints = c("X"),
                                types = c("C"),
                                bounds = c("X", "V"),
                                cones = c("X"),
                                maximum = c(TRUE, FALSE) )

## SOLVER CONTROLS
.add_controls <- function(solver) {
    ## DEoptim

    ROI_plugin_register_solver_control( solver, "VTR", "X")
    ROI_plugin_register_solver_control( solver, "strategy", "X")
    ROI_plugin_register_solver_control( solver, "bs", "X")
    ROI_plugin_register_solver_control( solver, "NP", "X")
    ROI_plugin_register_solver_control( solver, "itermax", "max_iter")
    ROI_plugin_register_solver_control( solver, "CR", "X")
    ROI_plugin_register_solver_control( solver, "F", "X")
    ROI_plugin_register_solver_control( solver, "trace", "verbose")
    ROI_plugin_register_solver_control( solver, "initialpop", "X")
    ROI_plugin_register_solver_control( solver, "storepopfrom", "X")
    ROI_plugin_register_solver_control( solver, "storepopfreq", "X")
    ROI_plugin_register_solver_control( solver, "p", "X")
    ROI_plugin_register_solver_control( solver, "c", "X")
    ROI_plugin_register_solver_control( solver, "reltol", "rel_tol")
    ROI_plugin_register_solver_control( solver, "steptol", "X")
    ROI_plugin_register_solver_control( solver, "parallelType", "X")
    ROI_plugin_register_solver_control( solver, "packages", "X")
    ROI_plugin_register_solver_control( solver, "parVar", "X")
    ROI_plugin_register_solver_control( solver, "foreachArgs", "X")

    invisible( TRUE )
}

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        solver <- ROI_plugin_get_solver_name( pkgname )
        ROI_plugin_register_solver_method(
            signatures = ROI_make_NLP_FXCV_signatures(),
            solver = solver,
            method = getFunction( "solve_deoptim", where = getNamespace(pkgname)) )
        .add_status_codes()
        .add_controls( solver )
    }
}

