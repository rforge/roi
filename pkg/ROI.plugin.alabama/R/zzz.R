## ROI plugin: alabama
## based the alabama package

make_signature <- function()
    ROI_plugin_make_signature( objective = c("L", "Q", "F"),
                               constraints = c("X", "L", "Q", "F"),
                               types = c("C"),
                               bounds = c("X", "V"),
                               cones = c("X"),
                               maximum = c(TRUE, FALSE) )

## control.outer.default <- list(lam0 = 10, sig0 = 100, eps = 1e-07,
##        itmax = 50, method = "BFGS", trace = TRUE, NMinit = FALSE, ilack.max=6,
## i.scale = 1, e.scale = 1, kkt2.check=TRUE)
## 
## control.optim.default <- list(trace = 0, fnscale = 1, parscale = rep.int(1,
##        length(par)), ndeps = rep.int(0.001, length(par)), maxit = 100L,
##        abstol = -Inf, reltol = sqrt(.Machine$double.eps), alpha = 1,
##        beta = 0.5, gamma = 2, REPORT = 10, type = 1, lmm = 5,
##        factr = 1e+07, pgtol = 0, tmax = 10, temp = 10)

## SOLVER CONTROLS
.add_controls <- function(solver) {
    ## alabama
    ROI_plugin_register_solver_control( solver, "par", "start" )
    ROI_plugin_register_solver_control( solver, "trace", "verbose" )

    ##
    ## outer loop
    ##
    ROI_plugin_register_solver_control( solver, "lam0", "X" )         ## initial value for lagrangian parameter
    ROI_plugin_register_solver_control( solver, "sig0", "X" )         ## 
    ROI_plugin_register_solver_control( solver, "eps", "tol" )        ##
    ROI_plugin_register_solver_control( solver, "itmax", "max_iter" ) ##
    ROI_plugin_register_solver_control( solver, "method", "method" )  ##
    ROI_plugin_register_solver_control( solver, "NMinit", "X" )       ##
    ROI_plugin_register_solver_control( solver, "ilack.max", "X" )    ##
    ROI_plugin_register_solver_control( solver, "i.scale", "X" )      ##
    ROI_plugin_register_solver_control( solver, "e.scale", "X" )      ##
    ROI_plugin_register_solver_control( solver, "kkt2.check", "X" )   ##

    ##
    ## inner loop
    ##
    ROI_plugin_register_solver_control( solver, "control.optim", "X" )

    ROI_plugin_register_solver_control( solver, "dry_run", "X" )

    invisible( TRUE )
}

.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        solver <- "alabama"
        ## obj: F    constr: X    types: C    bounds: V
        ROI_plugin_register_solver_method(
            signatures = make_signature(),
            solver = solver,
            method = getFunction("solve_alabama_auglag",
                                 where = getNamespace(pkgname)) )
        .add_status_codes( solver )
        .add_controls( solver )
    }
}
