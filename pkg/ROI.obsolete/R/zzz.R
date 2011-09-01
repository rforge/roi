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
status_db <- registry( )
status_db$set_field("solver",   type = "character", is_key = TRUE)
status_db$set_field("code",     type = "integer",   is_key = TRUE)
status_db$set_field("symbol",   type = "character")
status_db$set_field("message",  type = "character")
status_db$set_field("roi_code", type = "integer",   alternatives = 0:1)

## create registry object containing status codes
solver_db <- registry( )

## SOLVER_DB
## standard fields
solver_db$set_field( "solver",  type = "character", is_key = TRUE )
solver_db$set_field( "package", type = "character" )
solver_db$set_field( "types",   type = "character", validity_FUN = function(x) stopifnot(all(x %in% available_problem_types())) )
## we need to add the following field as we use it for solving MILPs
solver_db$set_field( "multiple_solutions", type = "logical", default = FALSE )
## further db fields can be added by the user later

.onLoad <- function( libname, pkgname ) {
    ## register all solver methods supported by this plugin

    ROI:::ROI_register_solver_method( solver = "glpk",
                                      package = pkgname,
                                      signatures = ROI:::ROI_make_LP_signatures(),
                                      method = ROI.plugin.glpk:::.solve_LP.glpk )

}
