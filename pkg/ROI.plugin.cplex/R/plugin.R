## ROI.plugin.cplex: solver interfaces
## Description: provides problem object <-> solver mappings

solve_OP <- function( x, control = list() ){
    if(is.null(control))
       control <- list()

    if( ! is.null(terms(objective(x))$Q) ) {
      ## Ensure that the coefficient matrix of the quadratic term is
      ## symmetric, required by Rcplex.
      Q <- terms(objective(x))$Q
      x$objective$Q <- (Q + t(Q)) / 2
    }

    n_obj <- ifelse( !is.null(terms(objective(x))$Q),
                     dim(terms(objective(x))$Q)[1],
                     length(objective(x)) )

    types <- ROI:::.expand_types(x$types, n_obj)

    ## variable bounds
    lb <- 0
    ub <- Inf
    if( ! is.default_bound(bounds(x)) ){
      lb <- rep(lb, n_obj)
      ub <- rep(ub, n_obj)
      lb[ bounds(x)$lower$ind ] <- bounds(x)$lower$val
      ub[ bounds(x)$upper$ind ] <- bounds(x)$upper$val
    }

    if(is.null(nos <- control$n)) nos <- 1L
      value_is_list_of_solutions <- !identical(as.integer(nos), 1L)

    if( is.null(control$trace) )
        control$trace <- 0
    if( is.null(control$round) )
        control$round <- 1

    out <- if( ! is.null(constraints(x)$Q) ) {
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
        tryCatch( Rcplex_solve_QCP(Qmat = terms(objective(x))$Q,
                                   cvec = as.numeric(as.matrix(terms(objective(x))$L)), # these are STMs
                                   QC = list(QC = list(Q = lapply( constraints(x)$Q[ qc ], function(Q) 1/2*Q), # in contrast to standard formulation ROI uses s.t. x'Qx + c'x <= b, thus dividing by 2
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
                                   control = control,
                                   n = nos
                                   ),
                 error = identity )

           } else {
               mat <- constraints(x)$L
               sense <- .as_Rcplex_sense(constraints(x)$dir)
               ## Reorder indices as CPLEX needs a column major order
               ## representation i.e., column indices j have to be in ascending
               ## order.
               column_major_order <- order(mat$j)
               mat$i <- mat$i[column_major_order]
               mat$j <- mat$j[column_major_order]
               mat$v <- mat$v[column_major_order]

               tryCatch( Rcplex(Qmat = terms(objective(x))$Q,
                                cvec = as.numeric(as.matrix(terms(objective(x))$L)),
                                Amat = mat,
                                sense = sense,
                                bvec = constraints(x)$rhs,
                                vtype = types,
                                lb = lb,
                                ub = ub,
                                objsense = if(x$maximum) "max" else "min",
                                control = control,
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
        out <- ROI_plugin_canonicalize_solution(solution, objval, status, ROI_plugin_get_solver_name(getPackageName()) )
        if(value_is_list_of_solutions) out <- list(out)
    } else {
        Rcplex.close()
        #class(out) <- c(class(x), class(out))
        out <- if(value_is_list_of_solutions)
            lapply( out, ROI_plugin_canonicalize_solution(solution = out$xopt,
                                                           optimum  = objective(x)(out$xopt),
                                                           status   = out$status,
                                                           solver   = ROI_plugin_get_solver_name(getPackageName()),
                                                           message = out) )
        else
            ROI_plugin_canonicalize_solution( solution = out$xopt,
                                               optimum  = objective(x)(out$xopt),
                                               status   = out$status,
                                               solver   = ROI_plugin_get_solver_name(getPackageName()),
                                               message  = out)
    }
    out
}

## solution extractor functions
ROI_plugin_solution_aux.cplex_solution <- function( x ){
    list( primal = x$message$extra$slack,
          dual = x$message$extra$lambda )
}
