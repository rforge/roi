
## ROI solver routine

ROI_solve <- function( x, solver, control = NULL, ... )
  UseMethod( "ROI_solve" )

ROI_solve.LP <- function( x, solver, control = NULL, ... )
  solve_LP( x, solver, control )

ROI_solve.QCP <- function( x, solver, control = NULL, ... )
  solve_QCP( x, solver, control )

ROI_solve.QP <- function( x, solver, control = NULL, ... )
  solve_QP( x, solver, control )

ROI_solve.MILP <- function( x, solver, control = NULL, ... )
  solve_MILP( x, solver, control )

ROI_solve.MIQP <- function( x, solver, control = NULL, ... )
  solve_MIQP( x, solver, control )

ROI_solve.MIQCP <- function(x, solver, control = NULL, ...)
  solve_MIQCP( x, solver, control )

##ROI_solve.MINLP <- function(x, solver, control = NULL, ...)
##  solve_MINLP( x, solver, control )

## ROI plugins

ROI_plugin <- function( solver, package, types, status_codes, ... )
  structure(list(solver = as.character(solver),
                 package = as.character(package),
                 types = as.character(types),
                 status_codes = match.fun(status_codes),
                 dotargs = list(...)),
            class = "ROI_plugin")

ROI_register_plugin <- function(x){
  stopifnot(inherits(x, "ROI_plugin"))

  ## first register status codes
  x$status_codes()

  ## then register solver
  ## FIXME: what todo with status_codes if solver registration fails?
  ##        we should catch the error and delete status entries
  add_solver_to_db( solver = x$solver, package = x$package, types = x$types, dotargs = x$dotargs )

  invisible(TRUE)
}
