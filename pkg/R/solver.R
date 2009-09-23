## create registry object containing status codes
solver_db <- registry( )

solver_db$set_field( "solver", type = "character", is_key = TRUE )
solver_db$set_field( "LP",     type = "logical" )
solver_db$set_field( "QP",     type = "logical" )
solver_db$set_field( "QCP",    type = "logical" )
solver_db$set_field( "NLP",    type = "logical" )
solver_db$set_field( "MILP",   type = "logical" )
solver_db$set_field( "MIQP",   type = "logical" )
solver_db$set_field( "MIQCP",  type = "logical" )
solver_db$set_field( "MINLP",  type = "logical" )

available_problem_types <- function( )
  c("LP", "QP", "QCP", "MILP", "MIQP") #, "NLP", "MIQCP", "MINLP")

## FIXME: the last field in the db indicates the mapping on the generic
##        (ROI) status codes.
## status_db$set_field("roi_code", type = "integer", alternatives = 1:5)

add_solver_to_db <- function( solver, capabilities ){
  stopifnot( all(capabilities %in% available_problem_types()) )
  for( type in available_problem_types() )
    eval( sprintf("solver_db$set_entry( solver = solver, %s = any(type == capabilities)",
                  type) )
}

## searching for available solver plugins
search_for_solver_plugins <- function(){
  registered_solvers <- get_solvers_from_db()
  pkgs_installed <- rownames(installed.packages())
  if(!is.null(pkgs_installed))
    avail <- pkgs_installed %in% registered_solvers
  set_default_solver(registered_solvers[avail][1])
  
}

set_default_solver <- function(x)
  solver_db$default <- x


.as_Rcplex_sense <- function(x) {
  TABLE <- c("L", "L", "G", "G", "E")
  names(TABLE) <- c("<", "<=", ">", ">=", "==")
  TABLE[x]
}
