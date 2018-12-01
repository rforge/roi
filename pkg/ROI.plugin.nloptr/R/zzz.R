## ROI plugin: NLOPTR
## based on nloptr interface

ROI_make_nlopt_constraint_signatures <- function()
    ROI_plugin_make_signature(objective = c("L", "Q", "F"),
                              constraints = c("X", "L", "Q", "F"),
                              types = c("C"),
                              bounds = c("X", "V"),
                              cones = c("X"),
                              maximum = c(TRUE, FALSE))

ROI_make_nlopt_signatures <- function()
    ROI_plugin_make_signature(objective = c("F"),
                              constraints = c("X"),
                              types = c("C"),
                              bounds = c("X", "V"),
                              cones = c("X"),
                              maximum = c(TRUE, FALSE))


## SOLVER CONTROLS
.add_controls <- function(solver) {
    ROI_plugin_register_solver_control( solver, "dry_run", "dry_run" )
    ROI_plugin_register_solver_control( solver, "x0", "start" )

    ROI_plugin_register_solver_control( solver, "xtol_rel", "rel_tol" )
    ROI_plugin_register_solver_control( solver, "xtol_abs", "abs_tol" )
    ROI_plugin_register_solver_control( solver, "maxeval", "max_iter" )
    ROI_plugin_register_solver_control( solver, "maxtime", "max_time" )
    ROI_plugin_register_solver_control( solver, "print_level", "verbosity_level" )

    ROI_plugin_register_solver_control( solver, "stopval", "X" )
    ROI_plugin_register_solver_control( solver, "ftol_rel", "X" )
    ROI_plugin_register_solver_control( solver, "ftol_abs", "X" )
    
    ROI_plugin_register_solver_control( solver, "tol_constraints_ineq", "X" )
    ROI_plugin_register_solver_control( solver, "tol_constraints_eq", "X" )

    ROI_plugin_register_solver_control( solver, "check_derivatives", "X" )
    ROI_plugin_register_solver_control( solver, "check_derivatives_tol", "X" )
    ROI_plugin_register_solver_control( solver, "check_derivatives_print", "X" )
    ROI_plugin_register_solver_control( solver, "print_options_doc", "X" )

    ROI_plugin_register_solver_control( solver, "population", "X" )
    ROI_plugin_register_solver_control( solver, "ranseed", "X" )
    ROI_plugin_register_solver_control( solver, "local_opts", "X" )

    ROI_plugin_register_solver_control( solver, "randomized", "X" )
    ROI_plugin_register_solver_control( solver, "precond", "X" )
    ROI_plugin_register_solver_control( solver, "restart", "X" )
    ROI_plugin_register_solver_control( solver, "args", "X" )

    invisible( TRUE )
}


.onLoad <- function( libname, pkgname ) {
    ## Solver plugin name (based on package name)
    if( ! pkgname %in% ROI_registered_solvers() ){

        plugin <- "nloptr"

        ##
        ## Unconstrained Optimizers
        ##
        solver <- "nloptr.bobyqa"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_signatures(),
            solver = solver,
            method = getFunction("solve_bobyqa", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        solver <- "nloptr.crs2lm"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_signatures(),
            solver = solver,
            method = getFunction("solve_crs2lm", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        solver <- "nloptr.direct"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_signatures(),
            solver = solver,
            method = getFunction("solve_direct", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        solver <- "nloptr.directL"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_signatures(),
            solver = solver,
            method = getFunction("solve_directL", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        ## solver <- "nloptr.direct_parallel"
        ## ROI_plugin_register_solver_method(
        ##     signatures = ROI_make_nlopt_signatures(),
        ##     solver = solver,
        ##     method = getFunction("solve_direct_parallel", where = getNamespace(pkgname)),
        ##     plugin = plugin )
        ## .add_status_codes( solver )
        ## .add_controls( solver )


        solver <- "nloptr.lbfgs"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_signatures(),
            solver = solver,
            method = getFunction("solve_lbfgs", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        solver <- "nloptr.neldermead"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_signatures(),
            solver = solver,
            method = getFunction("solve_neldermead", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        solver <- "nloptr.newuoa"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_signatures(),
            solver = solver,
            method = getFunction("solve_newuoa", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        solver <- "nloptr.sbplx"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_signatures(),
            solver = solver,
            method = getFunction("solve_sbplx", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        solver <- "nloptr.stogo"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_signatures(),
            solver = solver,
            method = getFunction("solve_stogo", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        solver <- "nloptr.tnewton"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_signatures(),
            solver = solver,
            method = getFunction("solve_tnewton", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        solver <- "nloptr.varmetric"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_signatures(),
            solver = solver,
            method = getFunction("solve_varmetric", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )



        ##
        ## Inequality Constrained Optimization Problems
        ##
        solver <- "nloptr.cobyla"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_constraint_signatures(),
            solver = solver,
            method = getFunction("solve_cobyla", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        solver <- "nloptr.mma"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_constraint_signatures(),
            solver = solver,
            method = getFunction("solve_mma", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        ##
        ## Inequality + Equality Constrained Optimization Problems
        ##
        solver <- "nloptr.auglag"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_constraint_signatures(),
            solver = solver,
            method = getFunction("solve_auglag", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        solver <- "nloptr.isres"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_constraint_signatures(),
            solver = solver,
            method = getFunction("solve_isres", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )


        solver <- "nloptr.slsqp"
        ROI_plugin_register_solver_method(
            signatures = ROI_make_nlopt_constraint_signatures(),
            solver = solver,
            method = getFunction("solve_slsqp", where = getNamespace(pkgname)),
            plugin = plugin )
        .add_status_codes( solver )
        .add_controls( solver )
    }
}
