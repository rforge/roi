
.add_status_codes <- function() {
    solver <- ROI_plugin_get_solver_name( getPackageName() )

    ##' ----------------------
    ##' Termination - Codes
    ##' ----------------------
    ROI_plugin_add_status_code_to_db(solver,  1L, "NORMAL_COMPLETION", "An optimal solution was obtained.", 0L)

    ROI_plugin_add_status_code_to_db(solver,  2L, "ITERATION_INTERRUPT", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  3L, "RESOURCE_INTERRUPT", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  4L, "TERMINATED_BY_SOLVER", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  5L, "EVALUATION_ERROR_LIMIT", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  6L, "CAPABILITY_PROBLEMS", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  7L, "LICENSING_PROBLEMS", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  8L, "USER_INTERRUPT", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  9L, "ERROR_SETUP_FAILURE", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  10L, "ERROR_SOLVER_FAILURE", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  11L, "ERROR_INTERNAL_SOLVER_FAILURE", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  12L, "SOLVE_PROCESSING_SKIPPED", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  13L, "ERROR_SYSTEM_FAILURE", "", 1L)
}
