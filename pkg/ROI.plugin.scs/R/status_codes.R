.add_status_codes <- function() {
    solver <- .ROI_plugin_get_solver_name( getPackageName() )
    .ROI_plugin_add_status_code_to_db( solver,
                                       1L,
                                       "SCS_SOLVED",
                                       "Optimal solution found.",
                                       0L )
    .ROI_plugin_add_status_code_to_db( solver,
                                       2L,
                                       "SCS_SOLVED_INACCURATE",
                                       "SCS_SOLVED_INACCURATE" )
    .ROI_plugin_add_status_code_to_db( solver,
                                       0L,
                                       "SCS_UNFINISHED",
                                       "SCS_UNFINISHED" )
    .ROI_plugin_add_status_code_to_db( solver,
                                       -1L,
                                       "SCS_UNBOUNDED",
                                       "SCS_UNBOUNDED" )
    .ROI_plugin_add_status_code_to_db( solver,
                                       -2L,
                                       "SCS_INFEASIBLE",
                                       "SCS_INFEASIBLE" )
    .ROI_plugin_add_status_code_to_db( solver,
                                       -3L,
                                       "SCS_INDETERMINATE",
                                       "SCS_INDETERMINATE" )
    .ROI_plugin_add_status_code_to_db( solver,
                                       -4L,
                                       "SCS_FAILED",
                                       "SCS_FAILED" )
    .ROI_plugin_add_status_code_to_db( solver,
                                       -5L,
                                       "SCS_SIGINT",
                                       "SCS_SIGINT" )
    .ROI_plugin_add_status_code_to_db( solver,
                                       -6L,
                                       "SCS_UNBOUNDED_INACCURATE",
                                       "SCS_UNBOUNDED_INACCURATE" )
    .ROI_plugin_add_status_code_to_db( solver,
                                       -7L,
                                       "SCS_INFEASIBLE_INACCURATE",
                                       "SCS_INFEASIBLE_INACCURATE" )
    invisible(TRUE)
}
