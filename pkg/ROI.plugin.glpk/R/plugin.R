## ROI plugin: GLPK
## based on Rglpk interface

## BASIC SOLVER METHOD
solve_OP <- function( x, control ){
    if(is.null(control))
        control <- list()
    ## ROI has its own translation table, thus we do not need to canonicalize in Rglpk
    control$canonicalize_status = FALSE
    if( all(ROI::types(x) == "C") )
        out <- .solve_LP( x, control )
    else
        out <- .solve_MILP( x, control )
    out
}

## SOLVER SUBMETHODS
.solve_LP <- function( x, control ) {
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    out <- Rglpk_solve_LP( terms(objective(x))[["L"]],
                           constraints(x)$L,
                           constraints(x)$dir,
                           constraints(x)$rhs,
                           bounds = bounds(x),
                           max = x$maximum,
                           control = control)
    ## FIXME: keep oiginal solution (return value)
    ROI_plugin_canonicalize_solution( solution = out$solution,
                                       optimum = out$optimum,
                                       status = out$status,
                                       solver = solver,
                                       message = out)
}

.solve_MILP <- function( x, control ) {
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    out <- Rglpk_solve_LP( terms(objective(x))[["L"]],
                           constraints(x)$L,
                           constraints(x)$dir,
                           constraints(x)$rhs,
                           bounds = bounds(x),
                           types = types(x),
                           max = x$maximum,
                           control = control)
    ROI_plugin_canonicalize_solution( solution = out$solution,
                                       optimum = out$optimum,
                                       status = out$status,
                                       solver = solver,
                                       message = out)
}

## STATUS CODES
.add_status_codes <- function(){
    ## GLPK
    ## from GLPK 4.34 reference manual and glpk.h (symbol, code, message)
    ## FIXME: change in solver interface, canonicalization now done in ROI
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    ROI_plugin_add_status_code_to_db(solver,
                                      1L,
                                      "GLP_UNDEF",
                                      "Solution is undefined."
                                      )
    ROI_plugin_add_status_code_to_db(solver,
                                      2L,
                                      "GLP_FEAS",
                                      "Solution is feasible."
                                      )
    ROI_plugin_add_status_code_to_db(solver,
                                      3L,
                                      "GLP_INFEAS",
                                      "Solution is infeasible."
                                      )
    ROI_plugin_add_status_code_to_db(solver,
                                      4L,
                                      "GLP_NOFEAS",
                                      "No feasible solution exists."
                                      )
    ROI_plugin_add_status_code_to_db(solver,
                                      5L,
                                      "GLP_OPT",
                                      "Solution is optimal.",
                                      0L
                                      )
    ROI_plugin_add_status_code_to_db(solver,
                                      6L,
                                      "GLP_UNBND",
                                      "Solution is unbounded."
                                      )
    invisible(TRUE)
}

## SOLVER CONTROLS
.add_controls <- function(){
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    ## GLPK
    ROI_plugin_register_solver_control( solver,
                                        "verbose",
                                        "verbose" )
    ROI_plugin_register_solver_control( solver,
                                        "presolve",
                                        "presolve" )
    ROI_plugin_register_solver_control( solver,
                                        "tm_limit",
                                        "max_time" )
    invisible( TRUE )
}

## SOLUTION EXTRACTORS
ROI_plugin_solution_dual.glpk_solution <- function( x ){
    x$message$solution_dual
}

## FIXME: final structure of return value not yet decided
ROI_plugin_solution_aux.glpk_solution <- function ( x ){
    x$message$auxiliary
}


read_lp_mps_fixed <- function(file, ...) {
    a <- list(...)
    Rglpk::Rglpk_read_file(file, type = "MPS_fixed", 
                           isTRUE(a$ignore_first_row), isTRUE(a$verbose))
}

read_lp_mps_free <- function(file, ...) {
    a <- list(...)
    Rglpk::Rglpk_read_file(file, type = "MPS_free",
                           isTRUE(a$ignore_first_row), isTRUE(a$verbose))
}

read_lp_cplex_lp <- function(file, ...) {
    a <- list(...)
    Rglpk::Rglpk_read_file(file, type = "CPLEX_LP",
                           isTRUE(a$ignore_first_row), isTRUE(a$verbose))
}

read_lp_math_prog <- function(file, ...) {
    a <- list(...)
    Rglpk::Rglpk_read_file(file, type = "MathProg",
                           isTRUE(a$ignore_first_row), isTRUE(a$verbose))
}

