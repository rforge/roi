## ROI: status_codes.R
## overview of solver status codes and their canonicalization

## FIXME: add registry to depends in DESCRIPTION
require("registry")

## create registry object containing status codes
status_db <- registry()

status_db$set_field("solver",  type = "character", is_key = TRUE)
status_db$set_field("code",    type = "integer",   is_key = TRUE)
status_db$set_field("symbol",  type = "character")
status_db$set_field("message", type = "character")
## FIXME: the last field in the db indicates the mapping on the generic
##        (ROI) status codes.
## status_db$set_field("roi_code", type = "integer", alternatives = 1:5)

add_status_code_to_db <- function(solver, code, symbol, message){
  status_db$set_entry(solver = solver,
                      code = code,
                      symbol = symbol,
                      message = message)
}

get_status_message_from_db <- function(solver, code){
  status_db[[solver, code]]
}

delete_status_code_from_db <- function(solver, code){
  status_db$delete_entry(solver = solver,
                         code = code)
}

## CPLEX status codes
## from CPLEX 11 reference manual
## (see e.e., http://www.decf.berkeley.edu/help/apps/ampl/cplex-doc/refcallablelibrary/index.html)
## FIXME: only a (relevant) subset from CPLEX status codes

## general status codes
add_status_code_to_db("cplex", 
                      1L,
                      "CPX_STAT_OPTIMAL",
                      "(Simplex or barrier): optimal solution."
                      )
add_status_code_to_db("cplex", 
                      2L,
                      "CPX_STAT_UNBOUNDED",
                      "(Simplex or Barrier): Problem is unbounded."
                      )
add_status_code_to_db("cplex", 
                      3L,
                      "CPX_STAT_INFEASIBLE",
                      "(Simplex or Barrier): Problem has been proven infeasible."
                      )
add_status_code_to_db("cplex", 
                      4L,
                      "CPX_STAT_INForUNBD"
                      "(Simplex or Barrier): Problem has been proven either infeasible or unbounded."
                      )
add_status_code_to_db("cplex", 
                      5L,
                      "CPX_STAT_OPTIMAL_INFEAS",
                      "(Simplex or Barrier) Optimal solution is available, but with infeasibilities after unscaling"
                      )
add_status_code_to_db("cplex", 
                      11L,
                      "CPX_STAT_ABORT_TIME_LIM",
                      "(Simplex or Barrier): Stopped due to a time limit."
                      )
## MIP status codes
add_status_code_to_db("cplex", 
                      101L,
                      "CPXMIP_OPTIMAL",
                      "(MIP): optimal integer solution."
                      )
add_status_code_to_db("cplex", 
                      102L,
                      "CPXMIP_OPTIMAL_TOL",
                      "(MIP): optimal soluton with the tolerance defined by epgap or epagap has been found."
                      )
add_status_code_to_db("cplex", 
                      103L,
                      "CPXMIP_INFEASIBLE"
                      "(MIP): Solution is integer infeasible."
                      )
add_status_code_to_db("cplex", 
                      104L,
                      "CPXMIP_SOL_LIM",
                      "(MIP): The limit on mixed integer solutions has been reached."
                      )
add_status_code_to_db("cplex", 
                      107L,
                      "CPXMIP_TIME_LIM_FEAS",
                      "(MIP): Time limit exceeded, but integer solution exists."
                      )
add_status_code_to_db("cplex", 
                      108L,
                      "CPXMIP_TIME_LIM_INFEAS",
                      "(MIP): Time limit exceeded, no integer solution exists."
                      )
add_status_code_to_db("cplex", 
                      109L,
                      "CPXMIP_FAIL_FEAS",
                      "(MIP): Terminated because of an error, but integer solution exists."
                      )
add_status_code_to_db("cplex", 
                      110L,
                      "CPXMIP_FAIL_INFEAS",
                      "(MIP):Terminated because of an error; no integer solution."
                      )
add_status_code_to_db("cplex", 
                      113L,
                      "CPXMIP_ABORT_FEAS",
                      "(MIP): Stopped, but an integer solution exists."
                      )
add_status_code_to_db("cplex", 
                      114L,
                      "CPXMIP_ABORT_INFEAS",
                      "(MIP): Stopped, no integer solution found."
                      )
add_status_code_to_db("cplex", 
                      115L,
                      "CPXMIP_OPTIMAL_INFEAS",
                      "(MIP): Problem is optimal with unscaled infeasibilities"
                      )
add_status_code_to_db("cplex", 
                      118L,
                      "CPXMIP_UNBOUNDED",
                      "(MIP): Problem is unbounded."
                      )
add_status_code_to_db("cplex", 
                      128L,
                      "CPXMIP_POPULATESOL_LIM",
                      "(MIP MultSols): The limit on optimal mixed integer solutions generated by populate has been reached."
                      )
add_status_code_to_db("cplex", 
                      129L,
                      "CPXMIP_OPTIMAL_POPULATED",
                      "(MIP-MultSols): Populate has completed the enumeration of all solutions it could enumerate."
                      )
add_status_code_to_db("cplex", 
                      130L,
                      "CPXMIP_OPTIMAL_POPULATED_TOL",
                      "(MIP-MultSols): similar to 129L but additionally objective value fits the tolerance specified by paramaters."
                      )

## lp_solve
## from lp_solve.h
## FIXME: currently only repetition of symbol in message

add_status_code_to_db("lpsolve", 
                      -5L,
                      "UNKNOWNERROR", 
                      "UNKNOWNERROR"
                      )
add_status_code_to_db("lpsolve", 
                      -4L,
                      "DATAIGNORED", 
                      "DATAIGNORED"
                      )
add_status_code_to_db("lpsolve", 
                      -3L,
                      "NOBFP", 
                      "NOBFP"
                      )
add_status_code_to_db("lpsolve", 
                      -2L,
                      "NOMEMORY", 
                      "NOMEMORY"
                      )
add_status_code_to_db("lpsolve", 
                      -1L,
                      "NOTRUN", 
                      "NOTRUN"
                      )
add_status_code_to_db("lpsolve", 
                      0L,
                      "OPTIMAL", 
                      "OPTIMAL"
                      )
add_status_code_to_db("lpsolve", 
                      1L,
                      "SUBOPTIMAL", 
                      "SUBOPTIMAL"
                      )
add_status_code_to_db("lpsolve", 
                      2L,
                      "INFEASIBLE", 
                      "INFEASIBLE"
                      )
add_status_code_to_db("lpsolve", 
                      3L,
                      "UNBOUNDED", 
                      "UNBOUNDED"
                      )
add_status_code_to_db("lpsolve", 
                      4L,
                      "DEGENERATE", 
                      "DEGENERATE"
                      )
add_status_code_to_db("lpsolve", 
                      5L,
                      "NUMFAILURE", 
                      "NUMFAILURE"
                      )
add_status_code_to_db("lpsolve", 
                      6L,
                      "USERABORT", 
                      "USERABORT"
                      )
add_status_code_to_db("lpsolve", 
                      7L,
                      "TIMEOUT", 
                      "TIMEOUT"
                      )
add_status_code_to_db("lpsolve", 
                      8L,
                      "RUNNING", 
                      "RUNNING"
                      )
add_status_code_to_db("lpsolve", 
                      9L,
                      "PRESOLVED", 
                      "PRESOLVED"
                      )

## GLPK
## from GLPK 4.34 reference manual and glpk.h (symbol, code, message)

add_status_code_to_db("glpk", 
                      1L,
                      "GLP_UNDEF",
                      "Solution is undefined."
                      )
add_status_code_to_db("glpk", 
                      2L,
                      "GLP_FEAS",
                      "Solution is feasible."
                      )
add_status_code_to_db("glpk", 
                      3L,
                      "GLP_INFEAS",
                      "Solution is infeasible."
                      )
add_status_code_to_db("glpk", 
                      4L,
                      "GLP_NOFEAS",
                      "No feasible solution exists."
                      )
add_status_code_to_db("glpk", 
                      5L,
                      "GLP_OPT",
                      "Solution is optimal."
                      )
add_status_code_to_db("glpk", 
                      6L,
                      "GLP_UNBND",
                      "Solution is unbounded."
                      )

## SYMPHONY
## SYMPHONY 5.5.10 (no reference found yet)
## FIXME: currently only repetition of symbol in message

add_status_code_to_db("symphony", 
                      225L,
                      "TM_NO_PROBLEM",
                      "TM_NO_PROBLEM"
                      )
add_status_code_to_db("symphony", 
                      226L,
                      "TM_NO_SOLUTION",
                      "TM_NO_SOLUTION"
                      )
add_status_code_to_db("symphony", 
                      227L,
                      "TM_OPTIMAL_SOLUTION_FOUND",
                      "Solution is optimal."
                      )
add_status_code_to_db("symphony", 
                      228L,
                      "TM_TIME_LIMIT_EXCEEDED",
                      "TM_TIME_LIMIT_EXCEEDED"
                      )
add_status_code_to_db("symphony", 
                      229L,
                      "TM_NODE_LIMIT_EXCEEDED",
                      "TM_NODE_LIMIT_EXCEEDED"
                      )
add_status_code_to_db("symphony", 
                      230L,
                      "TM_TARGET_GAP_ACHIEVED",
                      "TM_TARGET_GAP_ACHIEVED"
                      )
add_status_code_to_db("symphony", 
                      231L,
                      "TM_FOUND_FIRST_FEASIBLE",
                      "TM_FOUND_FIRST_FEASIBLE"
                      )
add_status_code_to_db("symphony", 
                      232L,
                      "TM_FINISHED",
                      "TM_FINISHED"
                      )
add_status_code_to_db("symphony", 
                      233L,
                      "TM_UNFINISHED",
                      "TM_UNFINISHED"
                      )
add_status_code_to_db("symphony", 
                      240L,
                      "TM_FEASIBLE_SOLUTION_FOUND",
                      "TM_FEASIBLE_SOLUTION_FOUND"
                      )
add_status_code_to_db("symphony", 
                      235L,
                      "TM_SIGNAL_CAUGHT",
                      "TM_SIGNAL_CAUGHT"
                      )
add_status_code_to_db("symphony", 
                      -251L,
                      "TM_ERROR__NO_BRANCHING_CANDIDATE",
                      "TM_ERROR__NO_BRANCHING_CANDIDATE"
                      )
add_status_code_to_db("symphony", 
                      -252L,
                      "TM_ERROR__ILLEGAL_RETURN_CODE",
                      "TM_ERROR__ILLEGAL_RETURN_CODE"
                      )
add_status_code_to_db("symphony", 
                      -253L,
                      "TM_ERROR__NUMERICAL_INSTABILITY",
                      "TM_ERROR__NUMERICAL_INSTABILITY"
                      )
add_status_code_to_db("symphony", 
                      -254L,
                      "TM_ERROR__COMM_ERROR",
                      "TM_ERROR__COMM_ERROR"
                      )
add_status_code_to_db("symphony", 
                      -275L,
                      "TM_ERROR__USER",
                      "TM_ERROR__USER"
                      )

## seal entries
status_db$seal_entries()
