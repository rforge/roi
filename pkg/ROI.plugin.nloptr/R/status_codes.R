## STATUS CODES
.add_status_codes <- function(){
    ## nloptr
    ## success
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    ROI_plugin_add_status_code_to_db(solver, 1L, "NLOPT_SUCCESS",
                          "Generic success return value.", 0L)
    ROI_plugin_add_status_code_to_db(solver, 2L, "NLOPT_STOPVAL_REACHED",
                          "Optimization stopped because stopval (above) was reached.",
                          0L)
    ROI_plugin_add_status_code_to_db(solver, 3L, "NLOPT_FTOL_REACHED",
                          "Optimization stopped because ftol_rel or ftol_abs (above) was reached.",
                          0L)
    ROI_plugin_add_status_code_to_db(solver, 4L, "NLOPT_XTOL_REACHED",
                          "Optimization stopped because xtol_rel or xtol_abs (above) was reached.",
                          0L)
    ROI_plugin_add_status_code_to_db(solver, 5L, "NLOPT_MAXEVAL_REACHED",
                          "Optimization stopped because maxeval (above) was reached.",
                          0L)
    ROI_plugin_add_status_code_to_db(solver, 6L, "NLOPT_MAXTIME_REACHED",
                          "Optimization stopped because maxtime (above) was reached.",
                          0L)
    ## failure
    ROI_plugin_add_status_code_to_db(solver, -1L, "NLOPT_FAILURE",
                          "Generic failure code.")
    ROI_plugin_add_status_code_to_db(solver, -2L, "NLOPT_INVALID_ARGS",
                          "Invalid arguments (e.g. lower bounds are bigger than upper bounds, an unknown algorithm was specified, etcetera).")
    ROI_plugin_add_status_code_to_db(solver, -3L, "NLOPT_OUT_OF_MEMORY",
                          "Ran out of memory.")
    ROI_plugin_add_status_code_to_db(solver, -4L, "NLOPT_ROUNDOFF_LIMITED",
                          "Halted because roundoff errors limited progress. (In this case, the optimization still typically returns a useful result.)")
    ROI_plugin_add_status_code_to_db(solver, -5L, "NLOPT_FORCED_STOP",
                          "Halted because of a forced termination: the user called nlopt_force_stop(opt) on the optimization's nlopt_opt object opt from the user's objective function or constraints.")
    invisible(TRUE)
}
