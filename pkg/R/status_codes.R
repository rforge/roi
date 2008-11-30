## ROI: status_codes.R
## overview of solver status codes and their canonicalization

initialize_solver_status_db <- function(solver, status_db){
  solver_status_db <- paste(solver, "status_db", sep = "_")
  if(!exists(solver_status_db, status_db))
    assign(solver_status_db, list(code = integer(),
                                  status = character(),
                                  mesg = character()),
           status_db)
  else
    warning(paste("Status DB for solver", solver, "already exists."))
  invisible(status_db)
}

add_to_solver_status_db <- function(solver, status_db, code, status, msg){
  solver_status_db <- paste(solver, "status_db", sep = "_")
  db <- get(solver_status_db, status_db)
  if(!(code %in% db$code)){
    db$code <- c(db$code, code)
    db$status <- c(db$status, status)
    db$msg <- c(db$msg, msg)
    assign(solver_status_db, db, status_db)
  }
  else
    warning(paste("Status code", code, "already exists."))
  invisible(status_db)
}

get_solver_status_from_db <- function(solver, status_db, code){
  solver_status_db <- paste(solver, "status_db", sep = "_")
  db <- get(solver_status_db, status_db)
  db$status[which(db$code == code)]
}

get_solver_message_from_db <- function(solver, status_db, code){
  solver_status_db <- paste(solver, "status_db", sep = "_")
  db <- get(solver_status_db, status_db)
  db$msg[which(db$code == code)]
}

## Create new status_db
status_db <- new.env()


## CPLEX
## from CPLEX 11 reference manual (see e.e., http://www.decf.berkeley.edu/help/apps/ampl/cplex-doc/refcallablelibrary/index.html)

solver <- "cplex"
initialize_solver_status_db(solver, status_db)

## general status codes
add_to_solver_status_db(solver, status_db,
                        1L,
                        "CPX_STAT_OPTIMAL",
                        "(Simplex or barrier): optimal solution."
                        )
add_to_solver_status_db(solver, status_db,
                        2L,
                        "CPX_STAT_UNBOUNDED",
                        "(Simplex or Barrier): Problem is unbounded."
                        )
add_to_solver_status_db(solver, status_db,
                        3L,
                        "CPX_STAT_INFEASIBLE",
                        "(Simplex or Barrier): Problem has been proven infeasible."
                        )
add_to_solver_status_db(solver, status_db,
                        4L,
                        "CPX_STAT_INForUNBD"
                        "(Simplex or Barrier): Problem has been proven either infeasible or unbounded."
                        )
add_to_solver_status_db(solver, status_db,
                        5L,
                        "CPX_STAT_OPTIMAL_INFEAS",
                        "(Simplex or Barrier) Optimal solution is available, but with infeasibilities after unscaling"
                        )
add_to_solver_status_db(solver, status_db,
                        11L,
                        "CPX_STAT_ABORT_TIME_LIM",
                        "(Simplex or Barrier): Stopped due to a time limit."
                        )
## MIP status codes
add_to_solver_status_db(solver, status_db,
                        101L,
                        "CPXMIP_OPTIMAL",
                        "(MIP): optimal integer solution."
                        )
add_to_solver_status_db(solver, status_db,
                        102L,
                        "CPXMIP_OPTIMAL_TOL",
                        "(MIP): optimal soluton with the tolerance defined by epgap or epagap has been found."
                        )
add_to_solver_status_db(solver, status_db,
                        103L,
                        "CPXMIP_INFEASIBLE"
                        "(MIP): Solution is integer infeasible."
                        )
add_to_solver_status_db(solver, status_db,
                        104L,
                        "CPXMIP_SOL_LIM",
                        "(MIP): The limit on mixed integer solutions has been reached."
                        )
add_to_solver_status_db(solver, status_db,
                        107L,
                        "CPXMIP_TIME_LIM_FEAS",
                        "(MIP): Time limit exceeded, but integer solution exists."
                        )
add_to_solver_status_db(solver, status_db,
                        108L,
                        "CPXMIP_TIME_LIM_INFEAS",
                        "(MIP): Time limit exceeded, no integer solution exists."
                        )
add_to_solver_status_db(solver, status_db,
                        109L,
                        "CPXMIP_FAIL_FEAS",
                        "(MIP): Terminated because of an error, but integer solution exists."
                        )
add_to_solver_status_db(solver, status_db,
                        110L,
                        "CPXMIP_FAIL_INFEAS",
                        "(MIP):Terminated because of an error; no integer solution."
                        )
add_to_solver_status_db(solver, status_db,
                        113L,
                        "CPXMIP_ABORT_FEAS",
                        "(MIP): Stopped, but an integer solution exists."
                        )
add_to_solver_status_db(solver, status_db,
                        114L,
                        "CPXMIP_ABORT_INFEAS",
                        "(MIP): Stopped, no integer solution found."
                        )
add_to_solver_status_db(solver, status_db,
                        115L,
                        "CPXMIP_OPTIMAL_INFEAS",
                        "(MIP): Problem is optimal with unscaled infeasibilities"
                        )
add_to_solver_status_db(solver, status_db,
                        118L,
                        "CPXMIP_UNBOUNDED",
                        "(MIP): Problem is unbounded."
                        )
add_to_solver_status_db(solver, status_db,
                         128L,
                        "CPXMIP_POPULATESOL_LIM",
                        "(MIP MultSols): The limit on optimal mixed integer solutions generated by populate has been reached."
                        )
add_to_solver_status_db(solver, status_db,
                        129L,
                        "CPXMIP_OPTIMAL_POPULATED",
                        "(MIP-MultSols): Populate has completed the enumeration of all solutions it could enumerate."
                        )
add_to_solver_status_db(solver, status_db,
                         130L,
                        "CPXMIP_OPTIMAL_POPULATED_TOL",
                        "(MIP-MultSols): similar to 129L but additionally objective value fits the tolerance specified by paramaters."
                        )

## lp_solve
## from lp_solve.h
## TODO: messages incomplete

solver <- "lp_solve"
initialize_solver_status_db(solver, status_db)

lp_solve_status <- matrix(c(
                            "UNKNOWNERROR", -5, "Unkown error.",
                            "DATAIGNORED" , -4, "",
                            "NOBFP"       , -3, "",
                            "NOMEMORY"    , -2, "",
                            "NOTRUN"      , -1, "",
                            "OPTIMAL"     ,  0, "Solution is optimal.",
                            "SUBOPTIMAL"  ,  1, "",
                            "INFEASIBLE"  ,  2, "Solution is infeasible.",
                            "UNBOUNDED"   ,  3, "Solution is unbounded.",
                            "DEGENERATE"  ,  4, "Degenerated solution.",
                            "NUMFAILURE"  ,  5, "",
                            "USERABORT"   ,  6, "",
                            "TIMEOUT"     ,  7, "",
                            "RUNNING"     ,  8, "",
                            "PRESOLVED"   ,  9, ""), ncol = 3, byrow = TRUE)

for(i in nrow(lp_solve_status)){
  add_to_solver_status_db(solver, status_db,
                        as.integer(lp_solve_status[i, 2]),
                        as.character(lp_solve_status[i, 1]),
                        as.character(lp_solve_status[i, 3]))
}


## GLPK
## from GLPK 4.33 reference manual and glpk.h

solver <- "glpk"
initialize_solver_status_db(solver, status_db)

add_to_solver_status_db(solver, status_db,
                        1L,
                        "GLP_UNDEF",
                        "Solution is undefined."
                        )
add_to_solver_status_db(solver, status_db,
                        2L,
                        "GLP_FEAS",
                        "Solution is feasible."
                        )
add_to_solver_status_db(solver, status_db,
                        3L,
                        "GLP_INFEAS",
                        "Solution is infeasible."
                        )
add_to_solver_status_db(solver, status_db,
                        4L,
                        "GLP_NOFEAS",
                        "No feasible solution exists."
                        )
add_to_solver_status_db(solver, status_db,
                        5L,
                        "GLP_OPT",
                        "Solution is optimal."
                        )
add_to_solver_status_db(solver, status_db,
                        6L,
                        "GLP_UNBND",
                        "Solution is unbounded."
                        )

## SYMPHONY
##

symphony_status <- matrix(c("TM_NO_PROBLEM", 225L,                    "",
                            "TM_NO_SOLUTION", 226L,                   "",
                            "TM_OPTIMAL_SOLUTION_FOUND", 227L,        "",
                            "TM_TIME_LIMIT_EXCEEDED", 228L,           "",
                            "TM_NODE_LIMIT_EXCEEDED", 229L,           "",
                            "TM_TARGET_GAP_ACHIEVED", 230L,           "",
                            "TM_FOUND_FIRST_FEASIBLE", 231L,          "",
                            "TM_FINISHED", 232L,                      "",
                            "TM_UNFINISHED", 233L,                    "",
                            "TM_FEASIBLE_SOLUTION_FOUND", 240L,       "",
                            "TM_SIGNAL_CAUGHT", 235L,                 "",
                            "TM_ERROR__NO_BRANCHING_CANDIDATE", -251L,"",
                            "TM_ERROR__ILLEGAL_RETURN_CODE", -252L,   "",
                            "TM_ERROR__NUMERICAL_INSTABILITY", -253L, "",
                            "TM_ERROR__COMM_ERROR", -254L,            "",
                            "TM_ERROR__USER", -275L,                  ""
                            ), ncol = 3, byrow = TRUE)
                             
for(i in nrow(symphony_status)){
  add_to_solver_status_db(solver, status_db,
                          as.integer(symphony_status[i, 2]),
                          as.character(symphony_status[i, 1]),
                          as.character(symphony_status[i, 3]))
}
