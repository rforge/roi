.add_status_codes <- function( solver ) {

    ROI_plugin_add_status_code_to_db(solver,  1L, "CPX_STAT_OPTIMAL", "An optimal solution was obtained.", 0L)
    ROI_plugin_add_status_code_to_db(solver,  101L, "CPX_STAT_OPTIMAL", "An optimal solution was obtained.", 0L)

    ROI_plugin_add_status_code_to_db(solver,  2L, "CPX_STAT_UNBOUNDED", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  3L, "CPX_STAT_INFEASIBLE", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  4L, "CPX_STAT_INForUNBD", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  5L, "CPX_STAT_OPTIMAL_INFEAS", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  6L, "CPX_STAT_NUM_BEST", "", 1L)
    ROI_plugin_add_status_code_to_db(solver, 10L, "CPX_STAT_ABORT_IT_LIM", "", 1L)
    ROI_plugin_add_status_code_to_db(solver, 11L, "CPX_STAT_ABORT_TIME_LIM", "", 1L)
    ROI_plugin_add_status_code_to_db(solver, 12L, "CPX_STAT_ABORT_OBJ_LIM", "", 1L)
    ROI_plugin_add_status_code_to_db(solver, 13L, "CPX_STAT_ABORT_USER", "", 1L)
    ROI_plugin_add_status_code_to_db(solver, 14L, "CPX_STAT_FEASIBLE_RELAXED_SUM", "", 1L)
    ROI_plugin_add_status_code_to_db(solver, 15L, "CPX_STAT_OPTIMAL_RELAXED_SUM", "", 1L)
    ROI_plugin_add_status_code_to_db(solver, 16L, "CPX_STAT_FEASIBLE_RELAXED_INF", "", 1L)
    ROI_plugin_add_status_code_to_db(solver, 17L, "CPX_STAT_OPTIMAL_RELAXED_INF", "", 1L)
    ROI_plugin_add_status_code_to_db(solver, 18L, "CPX_STAT_FEASIBLE_RELAXED_QUAD", "", 1L)
    ROI_plugin_add_status_code_to_db(solver, 19L, "CPX_STAT_OPTIMAL_RELAXED_QUAD", "", 1L)
    ROI_plugin_add_status_code_to_db(solver, 23L, "CPX_STAT_FEASIBLE", "", 1L)
    ROI_plugin_add_status_code_to_db(solver, 25L, "CPX_STAT_ABORT_DETTIME_LIM", "", 1L)

}
