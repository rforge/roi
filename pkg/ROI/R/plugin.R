################################################################################
## Package: ROI
## File:    plugin.R
## Author:  Stefan Theussl
## Changed: 2016-05-27
################################################################################



################################################################################
## REGISTER SOLVER CONTROLS
################################################################################

##  -----------------------------------------------------------
##  .ROI_plugin_register_solver_control
##  ==========================
##' @title Register Solver Controls
##'
##' @description Register a new solver control argument.
##' @param solver a character string giving the solver name.
##' @param args a character vector specifying with the supported signatures.
##' @param roi_control a character vector specifying the corresponding ROI control argument.
##' @return TRUE on success
##' @family plugin functions
##' @rdname ROI_plugin_register_solver_control
##' @export
.ROI_plugin_register_solver_control <- function( solver, args, roi_control = "X" ){
    args <- as.character( args )
    if( length(roi_control) == 1L )
        roi_control <- rep( as.character(roi_control), length(args) )
    stopifnot( length(args) == length(roi_control) )
    for( i in seq_along(args) )
        control_db$set_entry( solver, args[i], roi_control[i] )
    invisible( TRUE )
}

ROI_available_solver_controls <- function(){
    c( "X",               ## no corresponding ROI control
       "verbose",         ## LOGICAL: turn on/off solver output on terminal
       "verbosity_level"  ## INTEGER: level of output
      )
}

get_solver_controls_from_db <- function( solver ){
    sapply( control_db$get_entries(solver), function(x) x[[ "control" ]] )
}



################################################################################
## REGISTER SOLVER STATUS CODES
################################################################################

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



################################################################################
## REGISTER NEW SOLVER METHODS
################################################################################

##  -----------------------------------------------------------
##  .ROI_plugin_register_solver_method
##  ==========================
##' @title Register Solver Method
##'
##' @description Register a new solver method.
##' @param signatures a data.frame with the supported signatures.
##' @param solver a character string giving the solver name.
##' @param method a function registered as solver method.
##' @return TRUE on success
##' @family plugin functions
##' @rdname ROI_plugin_register_solver_method
##' @export
.ROI_plugin_register_solver_method <- function( signatures, solver, method ){
    for( i in 1:nrow(signatures) )
        do.call(solver_db$set_entry, c(as.list(signatures[i,]),
                                             list(solver = solver),
                                             list(FUN = method)))

    invisible( TRUE )
}



################################################################################
## SIGNATURES
################################################################################

ROI_required_signature <- function()
    c("objective", "constraints", "types", "bounds", "cones", "maximum")

##' Create a solver signature, the solver signatures are used to indicate
##' which problem types can be solved by a given solver.
##'
##' @title Make Signatures
##' @param ... signature definitions
##' @return a data.frame with the supported signatures
##' @examples
##' ## ROI_make_LP_signatures
##' lp_signature <- .ROI_plugin_make_signature( objective = "L",
##'                                     constraints = "L",
##'                                     types = c("C"),
##'                                     bounds = c("X", "V"),
##'                                     cones = c("free"),
##'                                     maximum = c(TRUE, FALSE) )
##' @family plugin functions
##' @rdname ROI_plugin_make_signature
##' @export
.ROI_plugin_make_signature <- function(...){
    dotargs <- list(...)
    required <- ROI_required_signature() ## names(formals(OP))
    if( length(dotargs) < 2 )
        stop( sprintf("Signature element for '%s' and '%s' need to be given.",
                      required[1], required[2]) )
    length(dotargs) <- length(required)
    if( is.null(names(dotargs)) )
        names(dotargs) <- required
    else {
        nam <- names(dotargs)
        nam[nam == ""] <- required[!(required %in% nam)]
        names(dotargs) <- nam
    }
    stopifnot( all(names(dotargs) %in% required) )

    ## FIXME: handle NULL case
    #.sort_types(unique(dotargs$types))
    #out <- dotargs[ names(dotargs)[-which(names(dotargs) == "types")] ]

    dotargs <- lapply(dotargs, function(x) if( is.null(x) ) FALSE else x)
    .make_signature(do.call(ROI_expand, dotargs))
}

.make_signature <- function( x ){
    required <- ROI_required_signature()
    if ( !identical(colnames(x), required) ) {
        ## hint <- "It seems, the signature is missing the entries or has to much entries!"
        hint1 <- sprintf("Required entries are: '%s'", paste(required, collapse="', '"))
        hint2 <- sprintf("Given entries are: '%s'", paste(colnames(x), collapse="', '"))
        hint <- sprintf("%s\n\t%s", hint1, hint2)
        error( "MISSPECIFIED_SIGNATURE", "The signature doesn't match the required signature!",
               ".make_signature", hint=hint, call=NULL )
    }
    types <- strsplit(as.character(x[["types"]]), "")
    types <- do.call(rbind, lapply( types, function(t) available_types() %in% t) )
    colnames(types) <- available_types()
    cbind(x[, colnames(x) != "types"], types)
}



################################################################################
## Plug-in and solver naming
################################################################################

## returns solver name based on package name
## Convention: ROI.plugin.<solver> => <solver>
.plugin_prefix <- function()
    "ROI.plugin"

## NOTE: all plugin related functions must be prefixed with ".ROI_plugin_" and
##       exported.

##  -----------------------------------------------------------
##  get_solver_name
##  ===============
##' @title Get Solver Name
##
##' @description Get the name of the solver plugin.
##' @param pkgname a string giving the package name.
##' @return Returns the name of the solver as character.
##' @family plugin functions
##' @rdname ROI_plugin_get_solver_name
##' @export
.ROI_plugin_get_solver_name <- function( pkgname )
    sub(sprintf("%s.", .plugin_prefix()), "", as.character(pkgname))



################################################################################
## CANONICALIZER
################################################################################

canonicalize_status <- function( status, solver ){
    msg <- get_status_message_from_db( solver, status )
    list( code = msg$roi_code, msg = msg )
}

##  -----------------------------------------------------------
##  Plug-in convenience function: canonicalize_solution
##  =====================
##' @title Canonicalize Solution
##'
##' @description Transform the solution to a standardized form.
##' @param solution a numeric or integer vector giving
##'        the solution of the optimization problem.
##' @param optimum a numeric giving the optimal value.
##' @param status an integer giving the status code (exit flag).
##' @param solver a character string giving the name of the solver.
##' @param message an optional \R object giving the original solver message.
##' @param ... further arguments to be stored in the solution object.
##' @return an object of class \code{"OP_solution"}.
##' @family plugin functions
##' @rdname ROI_plugin_canonicalize_solution
##' @export
.ROI_plugin_canonicalize_solution <- function( solution, optimum, status, solver, message=NULL, ... ) {
    status <- canonicalize_status( status, solver )
    make_OP_solution( solution = solution,
                      objval   = optimum,
                      status   = status,
                      solver   = solver,
                      message  = message, ... )
}
