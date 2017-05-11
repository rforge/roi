
.add_status_codes <- function() {
    solver <- ROI_plugin_get_solver_name( getPackageName() )

    ##' ----------------------
    ##' Termination - Codes
    ##' ----------------------
    ROI_plugin_add_status_code_to_db(solver,  0L, "OPTIMAL", "An optimal solution was obtained.", 0L)
    ROI_plugin_add_status_code_to_db(solver,  1L, "ERROR", "Some error occured.", 1L)
}
