## ROI: status_codes.R
## overview of solver status codes and their canonicalization

##  adds a new status code to db, default roi_code is 1L, i.e. a failure
##
##  -----------------------------------------------------------
##  add_status_code_to_db
##  =====================
##' @title Add Status Code to the Status Database
##'
##' @description Add a status code to the status database.
##' @param solver a character string giving the name of the solver.
##' @param code an integer giving the status code of the solver.
##' @param symbol a character string giving the status symbol.
##' @param message a character string used as status message.
##' @param roi_code an integer giving the ROI status code, 1L for failure and 0L for success.
##' @return NULL
##' @examples
##' \dontrun{
##' .ROI_plugin_add_status_code_to_db("ecos", 0L, "ECOS_OPTIMAL", "Optimal solution found.", 0L)
##' .ROI_plugin_add_status_code_to_db(solver, -7L, "ECOS_FATAL", "Unknown problem in solver.", 1L)
##' .ROI_plugin_add_status_code_to_db(solver, 5L, "GLP_OPT", "Solution is optimal.", 0L)
##' .ROI_plugin_add_status_code_to_db(solver, 1L, "GLP_UNDEF", "Solution is undefined.", 1L)
##' }
##' @family plugin functions
##' @rdname ROI_plugin_add_status_code_to_db
##' @export
.ROI_plugin_add_status_code_to_db <- function(solver, code, symbol, message, roi_code = 1L){
    status_db$set_entry(solver = solver,
                        code = code,
                        symbol = symbol,
                        message = message,
                        roi_code = roi_code)
    ## return NULL else it returns the registry as list
    invisible(NULL)
}

get_status_message_from_db <- function(solver, code){
    status_db[[solver, code]]
}

delete_status_code_from_db <- function(solver, code){
  status_db$delete_entry(solver = solver,
                         code = code)
}

available_in_status_codes_db <- function( )
  unique( status_db$get_field_entries("solver") )

