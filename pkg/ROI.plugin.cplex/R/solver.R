## ROI.plugin.cplex: solver interfaces
## Description: provides problem object <-> solver mappings

## TODO: this file is still based on old design

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

