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
  ## PLUGINS:
  
  ## CPLEX
  ROI_register_plugin( ROI_plugin(solver = "cplex",
                                  package = "Rcplex",
                                  types = c("LP", "MILP", "QP", "MIQP"),
                                  status_codes = ROI:::.add_cplex_status_codes,
                                  multiple_solutions = TRUE
                                  ) )

  ## LP_SOLVE
  ROI_register_plugin( ROI_plugin(solver = "lpsolve",
                                  package = "lpSolve",
                                  types = c("LP", "MILP"),
                                  status_codes = ROI:::.add_lpsolve_status_codes
                                  ) )
  
  ## GLPK
  ROI_register_plugin( ROI_plugin(solver = "glpk",
                                  package = "Rglpk",
                                  types = c("LP", "MILP"),
                                  status_codes = ROI:::.add_glpk_status_codes
                                  ) )
  ## quadprog
  ROI_register_plugin( ROI_plugin(solver = "quadprog",
                                  package = "quadprog",
                                  types = c("QP"),
                                  status_codes = ROI:::.add_quadprog_status_codes
                                  ) )

  ## SYMPHONY
  ROI_register_plugin( ROI_plugin(solver = "symphony",
                                  package = "Rsymphony",
                                  types = c("LP", "MILP"),
                                  status_codes = ROI:::.add_symphony_status_codes
                                  ) )
  
  ## SEAL DBs
  status_db$seal_entries()
  solver_db$seal_entries()

  ## SET DEFAULTS
  ## for the time being 'glpk' is the default solver
  ROI_options("default_solver", "glpk")
  
  writeLines( sprintf("%s: R Optimization Infrastructure", pkgname) )
  writeLines( sprintf("Installed solver plugins: %s.", paste(available_solver_plugins(), collapse = ", ")) )
  writeLines( sprintf("Default solver: %s.", ROI_options("default_solver")) )
}
