## FIXME: add registry to depends in DESCRIPTION
require("registry")

## create registry object containing status codes
solver_db <- registry()

solver_db$set_field("solver",  type = "character", is_key = TRUE)
solver_db$set_field("MILP",    type = "logical")

## FIXME: the last field in the db indicates the mapping on the generic
##        (ROI) status codes.
## status_db$set_field("roi_code", type = "integer", alternatives = 1:5)

add_solver_to_db <- function(solver, capabilities)
  solver_db$set_entry(solver = solver,
                      MILP = MILP)


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
