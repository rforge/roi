
## --------------------------------------
## Box Bound Constraints
## --------------------------------------
is_na <- function(x) {
    if ( is.vector(x) ) {
        if ( any(is.na(x)) ) return(TRUE)
    }
    return( FALSE )
}

## get_lb
## ======
##
## get lower bound constraints
get_lb <- function(x) {
    ##if( !length(bounds(x)$lower$val) ) {
    ##    lb <- NULL
    ##} else {
        lb <- numeric( length(x$objective) )
        lb[ bounds(x)$lower$ind ] <- bounds(x)$lower$val
    ##}
    return(lb)
}

## get_ub
## ======
##
## get upper bound constraints
get_ub <- function(x) {
    ##if( !length(bounds(x)$upper$val) ) {
    ##    ub <- NULL
    ##} else {
        ub <- rep.int(Inf, length(x$objective))
        ub[ bounds(x)$upper$ind ] <- bounds(x)$upper$val
    ##}
    return(ub)
}

##' build_inequality_constraints 
##' ============================
##' 
build_inequality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
    xtol <- (tol / 100)
    b <- constraints(x)$dir %in% c("<", "<=", ">=", ">")
    if ( !any(b) ) return( NULL )
    
    ## there are 2 different types those where rhs, is 0 and the others
    ## rhs_is_0 <- abs(rhs) < xtol
    ## n <- sum(b)

    build_fun <- function(CFUN, rhs, dir) {
        if (dir %in% c("<", "<=")) {
            return(function(x) CFUN(x) - rhs)    
        } else if (dir %in% c(">", ">=")) {
            return(function(x) rhs - CFUN(x))
        }
        stop("ERROR in build_fun")
    }

    F <- mapply(build_fun, constraints(x)$F[b], constraints(x)$rhs[b], constraints(x)$dir[b])

    ineq_fun <- function(x) {
        unlist(lapply(F, function(f) f(x)), FALSE, FALSE)
    }
    return(ineq_fun)
}

##' build_equality_constraints 
##' ==========================
##' 
build_equality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
    b <- constraints(x)$dir == "=="
    if ( !any(b) ) return( NULL )
    
    build_fun <- function(CFUN, rhs, dir) {
        if ( rhs == 0 ) return(CFUN)
        return(function(x) CFUN(x) - rhs)
    }

    F <- mapply(build_fun, constraints(x)$F[b], constraints(x)$rhs[b], constraints(x)$dir[b])

    eq_fun <- function(x) {
        unlist(lapply(F, function(f) f(x)), FALSE, FALSE)
    }
    return(eq_fun)
}

##' build_jacobian_inequality_constraints 
##' =====================================
##' 
build_jacobian_inequality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
    b <- constraints(x)$dir %in% c("<", "<=", ">=", ">")
    if ( !any(b) | is.null(constraints(x)$J) ) return( NULL )
    
    build_fun <- function(CFUN, rhs, dir) {
        if ( dir %in% c("<", "<=") ) return(CFUN)
        return(function(x) -CFUN(x))
    }

    J <- mapply(build_fun, constraints(x)$J[b], constraints(x)$rhs[b], constraints(x)$dir[b])

    ineq_fun <- function(x) {
        do.call(rbind, lapply(J, function(f) f(x)))
    }

    return(ineq_fun)

}

## ineq_fun <- function(x) {
##     lapply(J, function(f) f(x))
## }
## 
## constraints(x)$J[[1]](c(1, 1, 1))
## 
## z <- c(1, 1, 1)
## ineq_fun(c(1, 1, 1))
## J[[1]]
## lapply(J, function(f) f(x))


##' build_jacobian_equality_constraints 
##' ===================================
##' 
build_jacobian_equality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
    b <- constraints(x)$dir == "=="
    if ( !any(b) | is.null(constraints(x)$J) ) return( NULL )

    J <- constraints(x)$J[b]

    eq_fun <- function(x) {
        do.call(rbind, lapply(J, function(f) f(x)))
    }

    return(eq_fun)
}
