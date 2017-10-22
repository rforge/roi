## STATUS CODES
.add_status_codes <- function(solver) {
    ## success
    ROI_plugin_add_status_code_to_db(solver, 0L, "SUCCESS",
                          "Generic success return value.", 0L)
    ## failure
    ROI_plugin_add_status_code_to_db(solver, -1L, "FAILURE",
                          "Generic failure code.")

    ROI_plugin_add_status_code_to_db(solver, 1L, "FAILURE",
                          "Generic failure code.")


    invisible(TRUE)
}
