## STATUS CODES
.add_status_codes <- function() {
    ## success
    solver <- .ROI_plugin_get_solver_name( getPackageName() )
    .ROI_plugin_add_status_code_to_db(solver, 0L, "SUCCESS",
                          "Generic success return value.", 0L)
    ## failure
    .ROI_plugin_add_status_code_to_db(solver, -1L, "FAILURE",
                          "Generic failure code.")

    invisible(TRUE)
}
