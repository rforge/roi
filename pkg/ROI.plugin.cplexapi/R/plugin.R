## -----------------------------------------------------------------------------
##
##   CPLEX:
##
## -----------------------------------------------------------------------------

as_dgCMatrix <- function( x, ... ) {
  if (is.null(x)) return(NULL)
  Matrix::sparseMatrix(i=x$i, j=x$j, x=x$v, dims=c(x$nrow, x$ncol))
}

make_csc_matrix <- function(x) UseMethod("make_csc_matrix")

make_csc_matrix.matrix <- function(x) {
    if(!is.matrix(x))
        stop("Argument 'x' must be a matrix.")
   
    ind <- which(x != 0, arr.ind = TRUE)    
    matbeg <- c(0L, cumsum(tabulate(ind[, 2L], ncol(x))))
    matind <- ind[, 1] - 1L
    list(beg = matbeg, cnt = diff(c(matbeg, length(matind))),
         ind = matind, val = x[ind])
}

make_csc_matrix.simple_triplet_matrix <- function(x) {
    if(!inherits(x, "simple_triplet_matrix"))
        stop("Argument 'x' must be of class 'simple_triplet_matrix'.")

    ## The matrix method assumes that indices for non-zero entries are
    ## in row-major order, but the simple_triplet_matrix() constructor
    ## currently does not canonicalize accordingly ...
    ind <- order(x$j, x$i)
    matbeg <- c(0L, cumsum(tabulate(x$j[ind], x$ncol)))
    matind <- x$i[ind] - 1L
    list(beg = matbeg, cnt = diff(c(matbeg, length(matind))),
         ind = matind, val = x$v[ind])
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

is_diag_matrix <- function(x) {
    all(x$i == x$j)
}

is_zero_matrix <- function(x) {
    stopifnot(inherits(x, "simple_triplet_matrix"))
    length(x$i) == 0
}

is_mixed_intger <- function(x) {
    var_types <- types(x)
    if (length(var_types) == 0L) return(FALSE)
    any(var_types != "C")
}

map_sense <- function(x) {
    sense_map <- setNames(c('L', 'E', 'G'), c('<=', '==', '>='))
    sense_map[constraints(x)$dir]
}

#' LP
#' primopt   Primal Simplex
#' dual      Dual Simplex
#' hybnet    Network Simplex
#' bar       Barrier optimizer
#' siftopt   Sifting optimizer
#' Concurrent optimizer
cplex_methods <- function() {
    list(bar = baroptCPLEX, # Barrier Algorithm: LP, QP, QCP
         dual = dualoptCPLEX, # Dual Simplex Algorithm
         feas = feasOptCPLEX, # Minimum-Cost Relaxation
         hybbar = hybbaroptCPLEX, # Barrier Optimizer
         hybnet = hybnetoptCPLEX, # Network Optimizer
         lpopt = lpoptCPLEX, # Linear Optimizer: LP
         mipopt = mipoptCPLEX, # Mixed Integer Program
         primopt = primoptCPLEX, # Primal Simplex Method
         qpopt = qpoptCPLEX, # Continuous Quadratic Program: QP
         siftopt = siftoptCPLEX # Solve a Reduced Model
    )
}

cplex_solve <- function(env, prob, method) {
    # method <- "lpopt"
    optimize <- cplex_methods()[[method]]
    if ( is.null(optimize) ) {
        errmsg <- sprintf("unknown method %s allowed methods are %s", shQuote(method),
            paste(shQuote(names(cplex_methods())), collapse = ", "))        
        stop(errmsg)
    }

    status <- optimize(env, prob)
    status_code <- getStatCPLEX(env, prob)
    status_msg <- status_codeCPLEX(env, status_code)
    c(solutionCPLEX(env, prob), info = list(solnInfoCPLEX(env, prob)),
      status = list(list(code = status_code, msg = status_msg)))
}

# setDblParmCPLEX
# setDefaultParmCPLEX
# setIntParmCPLEX
# setLongParmCPLEX
# setStrParmCPLEX
# tuneParmCPLEX
select_method <- function(x, control) {
    if ( !is.null(control$method) ) return(control$method)
    sig <- OP_signature(x)
    if ( sig$objective == "Q" | sig$constraints == "Q" ) {
        if ( any(unlist(sig[c("B", "I")])) ) {
            "mipopt"
        } else {
            "qpopt"
        }
    } else {
        if ( any(unlist(sig[c("B", "I")])) ) {
            "mipopt"
        } else {
            "lpopt"
        }
    }
}

solve_LP <- function(x, control = list()) {
    env <- openEnvCPLEX()
    cplex_version <- getVersionCPLEX(env)
    prob <- initProbCPLEX(env)
    
    # Number of columns in the constraint matrix.
    nCols <- x$n_of_variables
    # Number of rows in the constraint matrix.
    nRows  <- x$n_of_constraints 
    # Single integer value that specifies whether the problem is a minimization or maximization problem.
    lpdir <- (-1)^maximum(x)
    objf <- as.vector(terms(objective(x))$L)

    mat <- make_csc_matrix(constraints(x)$L)
    sense <- map_sense(x)
    rhs <- constraints(x)$rhs
        
    bo <- as.data.frame(bounds(x))
    bo$lower[bo$lower == -Inf] <- -CPX_INFBOUND
    bo$upper[bo$upper ==  Inf] <-  CPX_INFBOUND
    # Containing the lower bound on each of the variables.
    lb <- bo$lower
    # Containing the lower bound on each of the variables.
    ub <- bo$upper
    # Containing the range value of each ranged constraint.
    rngval <- NULL 

    status <- copyLpCPLEX(env, prob, nCols, nRows, lpdir, objf, rhs, sense,
        mat$beg, mat$cnt, mat$ind, mat$val, lb, ub, rngval = rngval)
    if ( status != 0L ) stop("error in 'copyLpCPLEX'")

    if ( is_mixed_intger(x) ) {
        copyColTypeCPLEX(env, prob, types(x))
        method <- if (is.null(control$method)) "mipopt" else control$method
    } else {
        method <- if (is.null(control$method)) "lpopt" else control$method
    }

    # writeProbCPLEX(env, prob, fname = "tmp.lp")
    if ( isTRUE(is.finite(control$time_limit)) ) {
        setDblParmCPLEX(env, CPXPARAM_TimeLimit, control$time_limit)
    }

    sol <- cplex_solve(env, prob, method)
    # str(sol)
    delProbCPLEX(env, prob)
    closeEnvCPLEX(env)
    
    objval <- tryCatch(objective(x)(sol$x), error = function(e) as.numeric(NA))
    ROI_plugin_canonicalize_solution( solution = sol$x, optimum = objval,
        status = sol$status$code, solver = "cplexapi", message = sol )
}

solve_QP <- function(x, control = list()) {
    env <- openEnvCPLEX()
    cplex_version <- getVersionCPLEX(env)
    prob <- initProbCPLEX(env)

    nCols <- x$n_of_variables
    nRows  <- x$n_of_constraints
    lpdir <- (-1)^maximum(x)
    objf <- as.vector(terms(objective(x))$L)

    bo <- as.data.frame(bounds(x))
    bo$lower[bo$lower == -Inf] <- -CPX_INFBOUND
    bo$upper[bo$upper ==  Inf] <-  CPX_INFBOUND

    ## constraints
    is_qcon <- isTRUE(inherits(constraints(x), "Q_constraint", TRUE) == 1L)
    if ( is_qcon ) { # q-constraints
        QL <- terms(constraints(x))$Q
        is_lconstr <- sapply(QL, is_zero_matrix)
        mat <- make_csc_matrix(constraints(x)$L[is_lconstr,])
        as.matrix(constraints(x)$L[is_lconstr,])
        xsense <- map_sense(x)
        xrhs <- constraints(x)$rhs
        nRows <- sum(is_lconstr)
        rhs <- xrhs[is_lconstr]
        sense <- xsense[is_lconstr]
    } else {
        mat <- make_csc_matrix(constraints(x)$L)
        sense <- map_sense(x)
        rhs <- constraints(x)$rhs
    }        

    status <- copyLpCPLEX(env, prob, nCols, nRows = nRows, lpdir, objf, rhs, sense,
        mat$beg, mat$cnt, mat$ind, mat$val, lb = bo$lower, ub = bo$upper)
    if ( status != 0L ) stop("error in 'copyLpCPLEX'")

    ## Q-Objective
    Q0 <- terms(objective(x))$Q
    if ( !is.null(Q0) ) {
        if ( is_diag_matrix(Q0) ) {
            qsepvec <- double(nCols)
            qsepvec[Q0$i] <- Q0$v
            copyQPsepCPLEX(env, prob, qsepvec)
        } else {
            qmat <- make_csc_matrix(Q0)
            copyQuadCPLEX(env, prob, qmat$beg, qmat$cnt, qmat$ind, qmat$val)
        }
    }

    ## Q-Constraint
    if ( is_qcon ) {
        for ( i in which(!is_lconstr) ) {
            QLi <- QL[[i]]
            lc <- constraints(x)$L[i,]
            addQConstrCPLEX(env, prob, lzn = length(lc$j), qzn = length(QLi$v), 
                rhs = xrhs[i], sense = xsense[i], lind = lc$j - 1L, lval = lc$v ,
                qrow = QLi$i - 1L, qcol = QLi$j - 1L, qval = QLi$v / 2)
        }
    }
    
    ## types
    if ( is_mixed_intger(x) ) {
        copyColTypeCPLEX(env, prob, types(x))
    }
    
    #writeProbCPLEX(env, prob, fname = "tmp.lp")
    method <- select_method(x, control)

    if ( isTRUE(is.finite(control$time_limit)) ) {
        setDblParmCPLEX(env, CPXPARAM_TimeLimit, control$time_limit)
    }

    sol <- cplex_solve(env, prob, method)
    # str(sol)
    delProbCPLEX(env, prob)
    closeEnvCPLEX(env)
    
    objval <- tryCatch(objective(x)(sol$x), error = function(e) as.numeric(NA))
    ROI_plugin_canonicalize_solution( solution = sol$x, optimum = objval,
        status = sol$status$code, solver = "cplexapi", message = sol )
}

solve_OP <- function(x, control = list()) {
    if ( any(OP_signature(x)[1:2] == "Q") ) {
        solve_QP(x, control = control)
    } else {
        solve_LP(x, control = control)
    }
}

