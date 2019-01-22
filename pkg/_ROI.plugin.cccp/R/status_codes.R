.add_status_codes <- function(solver) {
    ROI_plugin_add_status_code_to_db(solver,
                              1L,
                              "NOT OPTIMAL",
                              "Model was not solved to optimality."
                            )
    ROI_plugin_add_status_code_to_db(solver,
                              0L,
                              "OPTIMAL",
                              "Model was solved to optimality.",
                              0L
                              )
    invisible(TRUE)
}
