##' @nord
##' @import registry
ROI_options <-
local({
    options <- list()
    function(option, value) {
        if (missing(option)) return(options)
        if (missing(value))
            options[[option]]
        else
            options[[option]] <<- value
    }
})

## STATUS_DB
## create registry object containing status codes
status_db <- registry( )
status_db$set_field("solver",   type = "character", is_key = TRUE)
status_db$set_field("code",     type = "integer",   is_key = TRUE)
status_db$set_field("symbol",   type = "character")
status_db$set_field("message",  type = "character")
status_db$set_field("roi_code", type = "integer",   alternatives = 0:1)

## SOLVER_DB
solver_db <- registry( )
solver_db$set_field( "solver",      type = "character", is_key = TRUE )
solver_db$set_field( "objective",   type = "character", validity_FUN = function(x) x %in% names(available_objective_classes()), is_key = TRUE)
solver_db$set_field( "constraints", type = "character", validity_FUN = function(x) x %in% names(available_constraint_classes()), is_key = TRUE)
for( type in available_types() )
    solver_db$set_field( type,      type = "logical", is_key = TRUE)
solver_db$set_field( "bounds",      type = "logical", is_key = TRUE)
solver_db$set_field( "maximum",     type = "logical", is_key = TRUE)
solver_db$set_field( "FUN",         type = "function" )

stopifnot( all(names(formals(OP)) %in% c(solver_db$get_field_names(), "types")) )


.onLoad <- function( libname, pkgname ) {
    ## SET DEFAULTS: for the time being 'ROI_NULL' for solving empty
    ## OPs is the default solver
    ROI_options("default_solver", "ROI_NULL")
}

.onAttach <- function( libname, pkgname ){
    ## Search for all solvers in same library as ROI and register found solvers
    ## implicitely be running the corresponding .onLoad() function.
    solvers <- ROI_installed_solvers( lib.loc = libname )
    lapply( solvers, function( pkgname ){ nmspc <- tryCatch(getNamespace(pkgname), error = identity)
                                          if( !inherits(nmspc, "error") ){
                                              load <- methods::getFunction( ".onLoad", where = nmspc )
                                              load( libname = libname, pkgname = pkgname )
                                          }} )
    ## Startup messages
    packageStartupMessage( sprintf("%s: R Optimization Infrastructure", pkgname) )
    packageStartupMessage( sprintf("Registered solver plugins: %s.",
                                   paste(names(ROI_registered_solvers()), collapse = ", ")) )
    packageStartupMessage( sprintf("Default solver: %s.", ROI_options("default_solver")) )
}
