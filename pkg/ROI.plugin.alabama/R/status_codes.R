## STATUS CODES
.solnp_add_status_codes <- function() {
    ## success
    solver <- "alabama"
    ROI_plugin_add_status_code_to_db(solver, 0L, "SUCCESS",
                                     "Generic success return value.", 0L)
    ## failure
    ROI_plugin_add_status_code_to_db(solver, 1L, "FAILURE",
                                     "Generic failure code.", 1L)

    ROI_plugin_add_status_code_to_db(solver, 7L, "MAX_ITER_REACHED",
                                     "ALABaMA ran out of iterations and did not converge.", 1L)

    ROI_plugin_add_status_code_to_db(solver, 9L, "NO_PROGRESS",
                                     "Convergence due to lack of progress in parameter updates.", 1L)

    ROI_plugin_add_status_code_to_db(solver, 11L, "OBJECTIVE_DECREASED",
                                     "Objective function decreased at outer iteration.", 1L)

    invisible(TRUE)
}
