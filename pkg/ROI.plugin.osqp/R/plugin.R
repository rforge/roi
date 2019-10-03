## -----------------------------------------------------------------------------
##
##   OSQP:
##
## -----------------------------------------------------------------------------

as_dgCMatrix <- function( x, ... ) {
  if (is.null(x)) return(NULL)
  Matrix::sparseMatrix(i=x$i, j=x$j, x=x$v, dims=c(x$nrow, x$ncol))
}


to_dense_vector <- function(x, len, default = 0L) {
    y <- rep.int(default, len)
    if ( is.null(x$ind) ) return(y)
    y[x$ind] <- x$val
    return(y)
}

is_lower_unbounded <- function(x) {
    if ( length(bounds(x)$lower$ind) < x$n_of_variables ) return(FALSE)
    all(bounds(x)$lower$ind == -Inf)
}

is_upper_unbounded <- function(x) {
    isTRUE(length(bounds(x)$upper$ind) == 0L)
}

solve_OP <- function(x, control = list()){
    solver <- "osqp"

    nvariables <- x$n_of_variables
    nconstraints <- x$n_of_constraints

    if ( maximum(x) ) {
        P <- as_dgCMatrix(-terms(objective(x))[["Q"]])
        q <- -as.vector(terms(objective(x))[["L"]])
    } else {
        P <- as_dgCMatrix(terms(objective(x))[["Q"]])
        q <- as.vector(terms(objective(x))[["L"]])
    }

    if ( nconstraints > 0L ) {
        constr <- constraints(x)
        A <- constr$L
        lbA <- rep.int(-Inf, nconstraints)
        ubA <- rep.int( Inf, nconstraints)
        cdir <- factor(constr$dir,  c("==", "<=", ">="))
        ctab <- tabulate(cdir)
        rhs <- constr$rhs
        if ( ctab[1L] ) {
            b <- constr$dir == "=="
            lbA[b] <- rhs[b]
            ubA[b] <- rhs[b]
        }

        if ( ctab[2L] ) {
            b <- constr$dir == "<="
            ubA[b] <- rhs[b]
        }

        if ( ctab[3L] ) {
            b <- constr$dir == ">="
            lbA[b] <- rhs[b]
        }
    } else {
        A <- NULL
        lbA <- ubA <- NULL
    }
    ## cbind(lhs = lbA, as.matrix(A), rhs = ubA)

    if ( !is_lower_unbounded(x) ) {
        lb <- to_dense_vector(bounds(x)$lower, nvariables)
        j <- which(lb != -Inf)
        LB <- simple_triplet_matrix(seq_along(j), j, rep.int(1L, length(j)),
            nrow = length(j), ncol = nvariables)
        lbLB <- lb[j]
        ubLB <- rep.int(Inf, length(j))
    } else {
        LB <- lbLB <- ubLB <- NULL
    }

    if ( !is_upper_unbounded(x) ) {
        ub <- to_dense_vector(bounds(x)$upper, nvariables, Inf)
        j <- which(lb != -Inf)
        UB <- simple_triplet_matrix(seq_along(j), j, rep.int(1L, length(j)),
            nrow = length(j), ncol = nvariables)
        lbUB <- rep.int(-Inf, length(j))
        ubUB <- ub[j]
    } else {
        UB <- lbUB <- ubUB <- NULL
    }

    A <- as_dgCMatrix(rbind(A, LB, UB))
    lbA <- c(lbA, lbLB, lbUB)
    ubA <- c(ubA, ubLB, ubUB)
    cbind(lhs = lbA, as.matrix(A), rhs = ubA)

    if ( is.null(control$verbose) ) control$verbose <- FALSE
    dry_run <- control$dry_run
    control$dry_run <- NULL
    cntrl <- do.call(osqpSettings, control)
    
    m <- osqp(P = P, q = q, A = A, l = lbA, u = ubA, pars = cntrl)

    if ( isTRUE(dry_run) ) {
        return(m)
    }

    sol <- m$Solve()
    objval <- tryCatch(objective(x)(sol$x), error = function(e) as.numeric(NA))
   
    ROI_plugin_canonicalize_solution( solution = sol$x, optimum = objval,
        status = sol$info$status_val, solver = solver, message = sol )
}
