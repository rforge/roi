
.add_status_codes <- function() {
    solver <- .ROI_plugin_get_solver_name( getPackageName() )

    ##' ----------------------
    ##' Termination - Codes
    ##' ----------------------
    .ROI_plugin_add_status_code_to_db(solver,  0L, "LPS_OPTIMAL", "An optimal solution was obtained.", 0L)
    .ROI_plugin_add_status_code_to_db(solver,  9L, "LPS_PRESOLVE", "The model could be solved by presolve.", 0L)

    .ROI_plugin_add_status_code_to_db(solver,  1L, "LPS_SUB-OPTIMAL", "The model is sub-optimal.", 1L)
    .ROI_plugin_add_status_code_to_db(solver,  2L, "LPS_INFEASIBLE", "The model is infeasible.", 1L)
    .ROI_plugin_add_status_code_to_db(solver,  3L, "LPS_UNBOUNDED", "The model is unbounded.", 1L)
    .ROI_plugin_add_status_code_to_db(solver,  4L, "LPS_DEFENERATE", "The model is degenerate.", 1L)
    .ROI_plugin_add_status_code_to_db(solver,  5L, "LPS_NUM_ERR", "Numerical failure encountered.", 1L)
    .ROI_plugin_add_status_code_to_db(solver,  6L, "LPS_STOP", "Process aborted", 1L)
    .ROI_plugin_add_status_code_to_db(solver,  7L, "LPS_TIMEOUT", "timeout", 1L)
    
    .ROI_plugin_add_status_code_to_db(solver, 10L, "LPS_BRANCH_AND_BOUND_FAILED", 
                                                    "The branch and bound routine failed.", 1L)
    .ROI_plugin_add_status_code_to_db(solver, 11L, "LPS_BRANCH_AND_BOUND_FAILED", 
                                                   "The branch and bound was stopped because of a break-at-first or break-at-value.", 1L)
    .ROI_plugin_add_status_code_to_db(solver, 12L, "LPS_BRANCH_AND_BOUND_STOPPED", 
                                                   "A feasible branch and bound solution was found.", 1L)
    .ROI_plugin_add_status_code_to_db(solver, 13L, "LPS_BRANCH_AND_BOUND_NO_SOLUTION", 
                                                   "No feasible branch and bound solution was found.", 1L)

}
