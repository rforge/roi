
cccp_control_variables <- function() {
    c("maxiters", "abstol", "reltol", "feastol", "stepadj", "beta", "trace")
}

lower_bounds <- function(bo, n) {
    i <- bo$lower$ind
    if ( length(i) == 0L ) {
        G <- diag(-1, n)
        h <- as.matrix(double(n))
    } else {
        v <- bo$lower$val
        lower <- double(n)
        lower[i] <- v
        i <- which(lower != -Inf)
        if ( length(i) == 0L ) return( list(G = NULL, h = NULL) )
        v <- lower[i]
        m <- length(v)
        G <- as.matrix(simple_triplet_matrix(seq_len(m), i, rep.int(-1, m), nrow = m, ncol = n))
        h <- as.matrix(-v)
    }
    ##list(conType = "NNOC", G = G, h = h,  dims = nrow(G))
    list(G = G, h = h)
}

upper_bounds <- function(bo, n) {
    i <- bo$upper$ind
    if ( length(i) == 0L ) return( list(G = NULL, h = NULL) )
    v <- bo$upper$val
    m <- length(v)
    G <- as.matrix(simple_triplet_matrix(seq_len(m), i, rep.int(1, m), nrow = m, ncol = n))
    list(G = G, h = v)
}

build_linear_constraints <- function(con) {
    i <- which(con$dir == "==")
    if ( length(i) ) {
        A <- as.matrix(con$L[i,])
        b <- as.matrix(con$rhs[i])
    } else {
        A <- b <- NULL
    }

    i <- which(con$dir == "<=")
    if ( length(i) ) {
        G0 <- as.matrix(con$L[i,])
        h0 <- as.matrix(con$rhs[i])
    } else {
        G0 <- h0 <- NULL
    }

    i <- which(con$dir == ">=")
    if ( length(i) ) {
        G1 <- as.matrix(-con$L[i,])
        h1 <- as.matrix(-con$rhs[i])
    } else {
        G1 <- h1 <- NULL
    }
    list(A = A, b = b, G = rbind(G0, G1), h = c(h0, h1))
}

solveLobjLcon <- function( x, control = list() ) {
    ## objective
    if ( maximum(x) ) {
        obj <- -as.vector(terms(objective(x))$L)
    } else {
        obj <- as.vector(terms(objective(x))$L)
    }

    ## bounds
    bo <- bounds(x)
    lb <- lower_bounds(bo, length(obj))
    ub <- upper_bounds(bo, length(obj))

    ## constraints
    con <- build_linear_constraints(constraints(x))
    
    G <- rbind(lb$G, ub$G, con$G)
    h <- c(lb$h, ub$h, con$h)

    if ( isTRUE(nrow(G) > 0) ) {
        cone_constraint <- list(list(conType = "NNOC", G = G, h = matrix(h),  dims = nrow(G)))
    } else {
        cone_constraint <- list()
    }

    ## options
    if (is.null(control$trace)) control$trace <- FALSE
    cntrl <- do.call(cccp::ctrl, control[intersect(names(control), cccp_control_variables())])

    cp <- cccp::dlp(obj, A = con$A, b = con$b, cList = cone_constraint)
    s <- cccp::cps(cp, cntrl)

    message <- list(x = as.double(getx(s)), y = as.double(gety(s)), s = as.double(gets(s)), 
        z = as.double(getz(s)), state = getstate(s), status = getstatus(s), niter = getniter(s), 
        params = tryCatch(getparams(s), error = function(e) NULL))

    

    status <- switch(message$status, optimal = 0L, unknown = 1L, 1L)
    obj_val <- as.vector(crossprod(as.vector(terms(objective(x))$L), message$x))
    
    ## ROI_plugin_canonicalize_solution
    list(solution  = message$x,
                                     optimum   = obj_val,
                                     status    = status,
                                     solver    = "cccp",
                                     message   = message)
}

.dcp_default <- function() {
    list(solve_dcp, x0 = NULL, f0 = NULL, g0 = NULL, h0 = NULL, cList = list(), 
         nlfList = list(), nlgList = list(), nlhList = list(), A = NULL, b = NULL, control = control)
}

Hessian <- function(x) {
    h0 <- terms(x)$H
    if ( is.null(h0) ) {
        hessian_function <- numDeriv::hessian(terms(x)$F, x)
        h0 <- function(x) hessian_function(x)
    }
    h0
}

## control = list(start = 1:3)
solve_dcp <- function(x0, f0, g0, h0, cList = list(), nlfList = list(), nlgList = list(), 
    nlhList = list(), A = NULL, b = NULL, control = NULL) {
    model <- cccp::dcp(x0 = x0, f0 = f0, g0 = g0, h0 = h0, cList = cList, nlfList = nlfList, 
        nlgList = nlgList, nlhList = nlhList, A = A, b = b)
    cccp::cps(model, control)
}

solveFobjLcon <- function( x, control = list() ) {
    solver <- "cccp"
    m <- .dcp_default()
    n <- length(objective(x))

    if ( is.null(control$start) ) stop("start values are missing")
    if ( !isTRUE(length(control$start) == n) ) 
        stop("length of the objective does not match the length of the starting values")
    
    m$x0 <- control$start

    ## NOTE: For equality constraints there has to be 
    ## z <- x
    ## constraints(x)$dir
    ## objective(z) <- L_objective(double(n))
    ## m$x0 <- solution(ROI_solve(z, "glpk"))

    ## objective gradient hessian
    if ( isTRUE(maximum(x)) ) {
        objective_function <- environment(objective(x))$F ## terms(objective(x))$F
        m$f0 <- function(x0) -objective_function(x0)
        gradient_function <- G(objective(x))
        m$g0 <- function(x0) -gradient_function(x0)
        hessian_function <- Hessian(objective(x))
        m$h0 <- function(x0) -hessian_function(x0)
    } else {
        m$f0 <- terms(objective(x))$F
        m$g0 <- G(objective(x))
        m$h0 <- Hessian(objective(x))
    }

    ## bounds
    bo <- bounds(x)
    lb <- lower_bounds(bo, n)
    ub <- upper_bounds(bo, n)

    ## constraints
    con <- build_linear_constraints(constraints(x))
    
    G <- rbind(lb$G, ub$G, con$G)
    h <- c(lb$h, ub$h, con$h)

    if ( isTRUE(nrow(G) > 0) ) {
        m$cList <- list(list(conType = "NNOC", G = G, h = matrix(h),  dims = nrow(G)))
    }

    ## options
    if (is.null(control$trace)) control$trace <- FALSE
    m$control <- do.call(cccp::ctrl, control[intersect(names(control), cccp_control_variables())])

    mode(m) <- "call"

    if ( isTRUE(control$dry_run) )
        return(m)
    
    s <- eval(m)
   
    ##model <- cccp::dcp(x0 = m$x0, f0 = m$f0, g0 = m$g0, h0 = m$h0, cList = m$cList, A = con$A, b = con$b)
    ##s <- cccp::cps(model, cntrl)
    ##getx(s)

    message <- list(x = as.double(getx(s)), y = as.double(gety(s)), s = as.double(gets(s)), 
        z = as.double(getz(s)), state = getstate(s), status = getstatus(s), niter = getniter(s), 
        params = tryCatch(getparams(s), error = function(e) NULL))
    
    status <- switch(message$status, optimal = 0L, 1L)
    obj_val <- tryCatch(objective(x)(message$x), error = function(e) NA_real_)

    ## ROI_plugin_canonicalize_solution
    list(solution  = message$x,
                                     optimum   = obj_val,
                                     status    = status,
                                     solver    = "cccp",
                                     message   = message)
}
