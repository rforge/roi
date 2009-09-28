## solver.R

## Problem types which can be handled by 'ROI':
available_problem_types <- function( )
  c( "LP", "QP", "QCP", "MILP", "MIQP" ) #, "NLP", "MIQCP", "MINLP")

## we have three necessary arguments in this function:
##  * solver)  the name of the solver
##  * package) package which provides the solver
##  * types)   the problem types which can be handled by the solver
## further arguments:
##  * multiple_solutions) TRUE/FALSE depending on whether the solver is capable
##                        of finding more than one solution (MIP case) or not
##  * and many more via '...'
add_solver_to_db <- function( solver, package, types, dotargs ){

  ## check if status codes for the solver have been registered
  if( !(solver %in% available_in_status_codes_db()) )
    stop( sprintf("status codes for solver '%s' not in status_db.") )

  ## for each problem type there must be a corresponding S3 solver method
  ## naming convention for methods: .solve_<type>.<solver>
  for( type in types ){
    status <- tryCatch(utils::getS3method(sprintf(".solve_%s", type), solver),
                       error = identity)
    if( inherits(status, "error") )
      stop( sprintf("method for type '%s' and solver '%s' not found.",
                    type, solver) )
  }
  
  ## each plugin must provide solution canonicalization S3 method
  ## naming convention for this method: .canonicalize_solution.<solver>
  status <- tryCatch(utils::getS3method(".canonicalize_solution", solver),
                     error = identity)
  if( inherits(status, "error") )
    stop( sprintf("method to canonicalize solutions from solver '%s' not found.", solver) )

  ## if everything is ok, add solver to db
  solver_db$set_entry( solver = solver,
                       package = package,
                       types = types)

  ## add further information about the solver to db
  if( length(dotargs) )
    for( entry in names(dotargs) )
      eval(parse(text = sprintf("solver_db$modify_entry( solver = solver, %s = dotargs[[entry]] )", entry)))

  invisible(TRUE)
}

## returns available solvers from db
get_solvers_from_db <- function( ) {
  solver_db$get_entry_names()
}

## returns package names of available solvers from db
get_solver_packages_from_db <- function ( ){
  solver_db$get_field_entries( "package" )
}

## returns a list of available solvers with their capabilities
get_solver_types_from_db <- function ( ){
  solver_db$get_field_entries("types", unlist = FALSE)
}

## returns the contents of specific solver options
get_solver_option_from_db <- function(solver, option) {
  solver_db$get_entry(solver = solver)[[option]]
}

## searches for available solver plugins and returns the names of
## the solvers found
available_solver_plugins <- function(){
  ## solvers registered
  registered_solvers <- get_solvers_from_db()
  names(registered_solvers) <-  get_solver_packages_from_db( )[registered_solvers]
  ## solver packages installed
  pkgs_installed <- rownames( utils::installed.packages() )
  if( !is.null(pkgs_installed) )
    registered_solvers[ pkgs_installed[ pkgs_installed %in% get_solver_packages_from_db() ] ]
  else
    NA
}

