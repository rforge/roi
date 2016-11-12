## ROI.plugin.cplex: solver interfaces
## Description: provides problem object <-> solver mappings

## TODO: this file is still based on old design

## SOLVER METHODS
.solve_MIP_via_mosek <-
function(x, control)
{
    if( ! is.null(terms(objective(x))$Q) ) {
      ## Ensure that the coefficient matrix of the quadratic term is
      ## symmetric, a lower triangular matrix is required by Rmosek
      Q <- terms(objective(x))$Q
      ind <- x$objective$Q$j <= x$objective$Q$i
      x$objective$Q$j <- x$objective$Q$j[ind]
      x$objective$Q$i <- x$objective$Q$i[ind]
      x$objective$Q$v <- x$objective$Q$v[ind]
    }

    n_obj <- ifelse( !is.null(terms(objective(x))$Q),
                     dim(terms(objective(x))$Q)[1],
                     length(terms(objective(x))$L))

    ## initialize problem
    prob <- list()

    ## FIXME: make this more efficient
    types <- ROI:::.expand_types(x$types, n_obj)
    if( any(c("I", "B") %in% types) )
        prob$intsub <- which( types %in% c("B", "I") )

    ## variable bounds
    lb <- rep(0, n_obj)
    ub <- rep(Inf, n_obj)
    if( ! is.null(bounds(x)) ){
      lb[ bounds(x)$lower$ind ] <- bounds(x)$lower$val
      ub[ bounds(x)$upper$ind ] <- bounds(x)$upper$val
    }
    ## there are no binary variables supported per se, so adding box constraints
    if( "B" %in% types ){
        indb <- which(types == "B")
        lb[indb] <- 0
        ub[indb] <- 1
    }
    ## add to mosek problem object
    prob$bx <- rbind(lb, ub)

    ## rbind( prob$bx -> ... obj variables, prob%bc ... rhs
    ## prob$bx <- rbind(lb, ub)
    ## prob$bc <- ...


    out <- if( any(is.Q_constraint(constraints(x))) ) {
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

