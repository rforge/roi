.add_status_codes <- function( solver ) {

    ROI_plugin_add_status_code_to_db(solver,  1L, "SOLVED", "An optimal solution was obtained.", 0L)

    ROI_plugin_add_status_code_to_db(solver,  4L, "DUAL_INFEASIBLE_INACCURATE", "A dual infeasible inaccurate solution was obtained.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  3L, "PRIMAL_INFEASIBLE_INACCURATE", "A primal infeasible inaccurate solution was obtained.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  2L, "SOLVED_INACCURATE", "An inaccurate solution was obtained.", 1L)

    ROI_plugin_add_status_code_to_db(solver,  -2L, "MAX_ITER_REACHED", "Maximum number of iterations was reached.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  -3L, "PRIMAL_INFEASIBLE", "Primal infeasible.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  -4L, "DUAL_INFEASIBLE", "Dual infeasible.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  -5L, "SIGINT", "Interrupted by user.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  -6L, "TIME_LIMIT_REACHED", "Time limit reached.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  -7L, "NON_CVX", "Problem is non-convex.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  -10L, "UNSOLVED", "Unsolved. Only setup function has been called.", 1L)
}


