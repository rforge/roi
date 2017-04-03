## STATUS CODES
.add_status_codes <- function() {
    ## success
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    ROI_plugin_add_status_code_to_db(solver, 0L, "SUCCESS",
                          "Generic success return value.", 0L)
    ## failure
    ROI_plugin_add_status_code_to_db(solver, -1L, "FAILURE",
                          "Generic failure code.")

    ROI_plugin_add_status_code_to_db(solver, 1L, "ITERATION_LIMIT_REACHED",
                          "Iteration limit maxit has been reached.")
    ROI_plugin_add_status_code_to_db(solver, 20L, "INADMISSIBLE_PARAMETERS",
                          "The initial set of parameters is inadmissible.")
    ROI_plugin_add_status_code_to_db(solver, 21L, "INADMISSIBLE_PARAMETERS",
                          "Intermediate set of parameters is inadmissible.")
    ROI_plugin_add_status_code_to_db(solver, 10L, "FAILURE",
                          "Degeneracy of the Nelder-Mead simplex.")

    invisible(TRUE)
}
