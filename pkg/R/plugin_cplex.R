## ROI plugin: CPLEX
## based on Rcplex interface

## basic structure of a plugin:\
## * a corresponding solver method one has to be provided for each problem type
##   e.g., for solving MILPs with cplex: .solve_MILP.cplex
## * a solution canonicalizer has to be provided
## * solver status codes have to be provided (needed by the canonicalizer)

## SOLVER METHODS
.solve_LP.cplex <- function( x, control ) {
  .solve_MIP_via_cplex(x, control)
}

.solve_QCP.cplex <-
function(x, control)
{
    .solve_MIP_via_cplex(x, control)
}

.solve_QP.cplex <- function( x, control ) {
  .solve_MIP_via_cplex(x, control)
}

.solve_MILP.cplex <-
function(x, control)
{
    ## Wrap into the common MIP CPLEX framework.
    ##x$objective <- list(Q = NULL, L = x$objective)
    .solve_MIP_via_cplex(x, control)
}

.solve_MIQCP.cplex <-
function(x, control)
{
    .solve_MIP_via_cplex(x, control)
}

.solve_MIQP.cplex <-
function(x, control)
{
    .solve_MIP_via_cplex(x, control)
}

.solve_MIP_via_cplex <-
function(x, control)
{
    if( ! is.null(terms(objective(x))$Q) ) {
      ## Ensure that the coefficient matrix of the quadratic term is
      ## symmetric, required by Rcplex.
      Q <- terms(objective(x))$Q
      x$objective$Q <- (Q + t(Q)) / 2
    }

    n_obj <- ifelse( !is.null(terms(objective(x))$Q),
                     dim(terms(objective(x))$Q)[1],
                     length(terms(objective(x))$L))
    
    types <- .expand_types(x$types, n_obj)

    ## variable bounds
    lb <- 0
    ub <- Inf
    if( ! is.null(bounds(x)) ){
      lb <- rep(lb, n_obj)
      ub <- rep(ub, n_obj)
      lb[ bounds(x)$lower$ind ] <- bounds(x)$lower$val
      ub[ bounds(x)$upper$ind ] <- bounds(x)$upper$val
    }

    if(is.null(nos <- control$n)) nos <- 1L
      value_is_list_of_solutions <- !identical(as.integer(nos), 1L)
    
    out <- if( is.QCP(x) || is.MIQCP(x) ) {
        ## which are the quadratic constraints
        qc <- which( ! sapply(constraints(x)$Q, .all_zero_in_simple_triplet_matrix_or_NULL) )

        ## first linear part
        mat <- constraints(x)$L[ -qc, ]
        sense <- .as_Rcplex_sense(constraints(x)$dir[ -qc ])

        ## whereas quadratic constrains do not need column major ordered matrices
        ## the linear part does ..        
        column_major_order <- order(mat$j)
        mat$i <- mat$i[column_major_order]
        mat$j <- mat$j[column_major_order]
        mat$v <- mat$v[column_major_order]
        
        ## we need to handle quadratic constraint
        tryCatch( Rcplex:::Rcplex_solve_QCP(Qmat = terms(objective(x))$Q,
                                            cvec = terms(objective(x))$L,
                                            QC = list(QC = list(Q = constraints(x)$Q[ qc ],
                                                                L = .make_list_of_linear_constraints(
                                                                  constraints(x)$L[ qc, ])),
                                                      dir = .as_Rcplex_sense(constraints(x)$dir[ qc ]),
                                                      b = constraints(x)$rhs[ qc ]),
                                            Amat = mat,
                                            sense = sense,
                                            bvec = constraints(x)$rhs[ -qc ],
                                            vtype = types,
                                            lb = lb,
                                            ub = ub,
                                            objsense = if(x$maximum) "max" else "min",
                                            control = list(trace = 0, round = 1),
                                            n = nos
                                            ),
                  error = identity )

        }
    else {
        mat <- constraints(x)$L
        sense <- .as_Rcplex_sense(constraints(x)$dir)
        ## FIXME: always simple triplet matrix with problem constructors
        if(is.simple_triplet_matrix(mat)) {
            ## Reorder indices as CPLEX needs a column major order
            ## representation i.e., column indices j have to be in ascending
            ## order.
            column_major_order <- order(mat$j)
            mat$i <- mat$i[column_major_order]
            mat$j <- mat$j[column_major_order]
            mat$v <- mat$v[column_major_order]
        } else {
            mat <- as.matrix(mat)
        }
        tryCatch( Rcplex::Rcplex(Qmat = terms(objective(x))$Q,
                                cvec = terms(objective(x))$L,
                                Amat = mat,
                                sense = sense,
                                bvec = constraints(x)$rhs,
                                vtype = types,
                                lb = lb,
                                ub = ub,
                                objsense = if(x$maximum) "max" else "min",
                                control = list(trace = 0, round = 1),
                                n = nos
                                ),
                  error = identity )
      }
    if(inherits(out, "error")) {
        ## Explicitly catch and rethrow CPLEX unavailability errors.
        msg <- conditionMessage(out)
        if(regexpr("Could not open CPLEX environment\\.", msg) > -1L)
           stop(msg, call. = FALSE)
        ## Currently, Rcplex signals problems via error() rather than
        ## returning a non-zero status.  Hence, we try catching these
        ## errors.  (Of course, these could also be real errors ...).
        solution <- rep(NA_real_, length(types))
        objval <- NA_real_
        status <- 2                     # or whatever ...
        names(status) <- msg            # should be of length one ...
        out <- .make_MIP_solution(solution, objval, status)
        if(value_is_list_of_solutions) out <- list(out)
    } else {
        class(out) <- c(class(x), class(out))
        out <- if(value_is_list_of_solutions)
            lapply( out, .canonicalize_solution(out, x) )
        else
            .canonicalize_solution(out, x)
    }
    out
}

## CANONICALIZER
.canonicalize_solution.cplex <- function(out, x)
{
    solution <- out$xopt
    ## For the time being ...
    ## Since Rcplex 0.1-4 integers are rounded (via control argument
    ## 'round' which we set accordingly when calling Rcplex()) but no
    ## new optimal solution based on these values is calculated.  Hence,
    ## we no longer round ourselves, but recompute objval.
    objval <- objective(x)(solution)
    status <- .canonicalize_status(out$status, class(out)[1])
    .make_MIP_solution(solution, objval, status)
}

## STATUS CODES
.add_cplex_status_codes <- function( ) {
  
  ## CPLEX status codes
  ## from CPLEX 11 reference manual
  ## (see e.e., http://www.decf.berkeley.edu/help/apps/ampl/cplex-doc/refcallablelibrary/index.html)
  ## FIXME: only a (relevant) subset from CPLEX status codes

  ## generic status codes
  add_status_code_to_db("cplex", 
                        1L,
                        "CPX_STAT_OPTIMAL",
                        "(Simplex or barrier): optimal solution.",
                        0L
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
                        "CPX_STAT_INForUNBD",
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
                        "(MIP): optimal integer solution.",
                        0L
                        )
  add_status_code_to_db("cplex", 
                        102L,
                        "CPXMIP_OPTIMAL_TOL",
                        "(MIP): optimal soluton with the tolerance defined by epgap or epagap has been found.",
                        0L
                        )
  add_status_code_to_db("cplex", 
                        103L,
                        "CPXMIP_INFEASIBLE",
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
                        "(MIP MultSols): The limit on optimal mixed integer solutions generated by populate has been reached.",
                        0L
                        )
  add_status_code_to_db("cplex", 
                        129L,
                        "CPXMIP_OPTIMAL_POPULATED",
                        "(MIP-MultSols): Populate has completed the enumeration of all solutions it could enumerate.",
                        0L
                        )
  add_status_code_to_db("cplex", 
                        130L,
                        "CPXMIP_OPTIMAL_POPULATED_TOL",
                        "(MIP-MultSols): similar to 129L but additionally objective value fits the tolerance specified by paramaters.",
                        0L
                        )
  invisible(TRUE)
}

## UTILITIES
.as_Rcplex_sense <- function(x) {
  TABLE <- c("L", "L", "G", "G", "E")
  names(TABLE) <- c("<", "<=", ">", ">=", "==")
  TABLE[x]
}

.all_zero_in_simple_triplet_matrix_or_NULL <- function(x){
  if( is.null(x) )
    return(TRUE)
  stm <- as.simple_triplet_matrix(x)
  if( ! length(stm$v) )
    return(TRUE)
  FALSE
}

.make_list_of_linear_constraints <- function(x){
  if( is.null(x) )
    return(list(NULL))
  stm <- as.simple_triplet_matrix(x)
  apply(x, 1, function(x) as.simple_triplet_matrix(matrix(x, nrow = 1)))
}
