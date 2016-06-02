
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

##' get_callable
##' ============
##'
##' get the callable part of a function
get_callable <- function(f) {
    ### fl <- as.list(f)
    ## fl[sapply(fl, class) != "name"][[1]]
    body(f)
}


##' standardize_leq
##' ===============
##'
##' ROI allows to define leq constraints as
##' g(x) <= a
##' but nloptr needs
##' g(x)' := g(x) - a <= 0
##'
##' @param f a function
##' @param rhs a numeric of length 1
##' @noRd
standardize_leq <- function(FUN, RHS) {
    c( "TMPFUN <- function()", deparse(get_callable(FUN)),
       paste(c("TMPFUN() -",  as.character(RHS)), collapse=" ") )
}

##' standardize_geq
##' ===============
##'
##' ROI allows to define geq constraints as
##' g(x) >= a
##' but nloptr needs
##' g(x)' := -g(x) + a <= 0
##'
##' @param f a function
##' @param rhs a numeric of length 1
##' @noRd
standardize_geq <- function(FUN, RHS) {
    c( "TMPFUN <- function()", deparse(get_callable(FUN)),
       paste(c(as.character(RHS), "- TMPFUN()"), collapse=" ") )
}

##' standardize_eq
##' ==============
##'
##' ROI allows to define leq constraints as
##' h(x) == a
##' but nloptr needs
##' h(x)' := h(x) - a == 0
##'
##' @param f a function
##' @param rhs a numeric of length 1
##' @noRd
standardize_eq <- function(FUN, RHS) {
    c( "TMPFUN <- function()", deparse(get_callable(FUN)),
       paste(c("TMPFUN() -",  as.character(RHS)), collapse=" ") )
}

##' build_inequality_constraints 
##' ============================
##' 
build_inequality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
    xtol <- (tol / 100)
    b <- constraints(x)$dir %in% c("<", "<=", ">=", ">")
    if ( !any(b) ) return( NULL )
    
    F <- constraints(x)$F[b]
    dir <- constraints(x)$dir[b]
    rhs <- constraints(x)$rhs[b]
    
    fargs <- unique(unlist(lapply(F, function(f) names(formals(f)))))
    fargs <- sprintf("function(%s)", paste(fargs, collapse=", "))

    ## there are 2 different types those where rhs, is 0 and the others
    rhs_is_0 <- abs(rhs) < xtol
    n <- sum(b)
    code <- ""
    for ( i in seq_len(n) ) {
        f0 <- sprintf("FUN%i <- function()", i)
        if ( dir[i] == "<" ) {
            ## since nloptr only knows <= add some epsilon smaller than the tolerance to obtain <
            code <- c(code, f0, "{", standardize_leq(F[[i]], rhs[[i]] + xtol), "}", "")
        } else if ( dir[i] == "<=" ) {
            if ( rhs_is_0[i] ) { ########### RHS == 0
                code <- c(code, f0, deparse(get_callable(F[[i]])), "" ) 
            } else {             ########### RHS != 0
                code <- c(code, f0, "{", standardize_leq(F[[i]], rhs[[i]]), "}", "")
            }
        } else if ( dir[i] == ">=" ) {
            code <- c(code, f0, "{", standardize_geq(F[[i]], rhs[[i]]), "}", "")
        } else if ( dir[i] == ">" ) {
            code <- c(code, f0, "{", standardize_geq(F[[i]], rhs[[i]] - xtol), "}", "")
        } else {
            stop(" :( (sad face)")
        }
    }
    ret_val <- sprintf("c(%s)", paste(sprintf("FUN%i()", seq_len(n)), collapse=", "))
    eval(parse(text=c(fargs, "{", code, "", ret_val, "", "}" )), envir=list())
}

##' build_equality_constraints 
##' ==========================
##' 
build_equality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
    xtol <- (tol / 100)
    b <- constraints(x)$dir == "=="
    if ( !any(b) ) return( NULL )
    
    F <- constraints(x)$F[b]
    dir <- constraints(x)$dir[b]
    rhs <- constraints(x)$rhs[b]

    fargs <- unique(unlist(lapply(F, function(f) names(formals(f)))))
    fargs <- sprintf("function(%s)", paste(fargs, collapse=", "))

    ## there are 2 different types those where rhs, is 0 and the others
    rhs_is_0 <- abs(rhs) < xtol
    n <- sum(b)
    code <- ""
    for ( i in seq_len(n) ) {
        f0 <- sprintf("FUN%i <- function()", i)
        if ( rhs_is_0[i] ) { ########### RHS == 0
            code <- c(code, f0, deparse(get_callable(F[[i]])), "" ) 
        } else {             ########### RHS != 0
            code <- c(code, f0, "{", standardize_eq(F[[i]], rhs[[i]]), "}", "")
        }
    }
    ret_val <- sprintf("c(%s)", paste(sprintf("FUN%i()", seq_len(n)), collapse=", "))
    eval(parse(text=c(fargs, "{", code, "", ret_val, "", "}" )), envir=list())
}

build_jacobian_inequality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
    xtol <- (tol / 100)
    b <- constraints(x)$dir %in% c("<", "<=", ">=", ">")
    if ( !any(b) ) return( NULL )
    
    J <- constraints(x)$J[b]
    dir <- constraints(x)$dir[b]
    rhs <- constraints(x)$rhs[b]

    if ( all(sapply(J, is_na)) ) return( NULL )
    if ( any(sapply(J, is_na)) ) {
        stop("missing jacobian for inequality constrains")
    }
    
    fargs <- unique(unlist(lapply(J, function(f) names(formals(f)))))
    fargs <- sprintf("function(%s)", paste(fargs, collapse=", "))

    ## there are 2 different types those where rhs, is 0 and the others
    rhs_is_0 <- abs(rhs) < xtol
    n <- sum(b)
    code <- ""
    for ( i in seq_len(n) ) {
        f0 <- sprintf("FUN%i <- function()", i)
        if ( dir[i] == "<" ) {
            ## since nloptr only knows <= add some epsilon smaller than the tolerance to obtain <
            code <- c(code, f0, "{", standardize_leq(J[[i]], rhs[[i]] + xtol), "}", "")
        } else if ( dir[i] == "<=" ) {
            if ( rhs_is_0[i] ) { ########### RHS == 0
                code <- c(code, f0, deparse(get_callable(J[[i]])), "" ) 
            } else {             ########### RHS != 0
                code <- c(code, f0, "{", standardize_leq(J[[i]], rhs[[i]]), "}", "")
            }
        } else if ( dir[i] == ">=" ) {
            code <- c(code, f0, "{", standardize_geq(J[[i]], rhs[[i]]), "}", "")
        } else if ( dir[i] == ">" ) {
            code <- c(code, f0, "{", standardize_geq(J[[i]], rhs[[i]] - xtol), "}", "")
        } else {
            stop(" :( (sad face)")
        }
    }
    ret_val <- sprintf("rbind(%s)", paste(sprintf("FUN%i()", seq_len(n)), collapse=", "))
    eval(parse(text=c(fargs, "{", code, "", ret_val, "", "}" )), envir=list())
}

build_jacobian_equality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
    xtol <- (tol / 100)
    b <- constraints(x)$dir == "=="
    if ( !any(b) ) return( NULL )
    
    J <- constraints(x)$J[b]
    dir <- constraints(x)$dir[b]
    rhs <- constraints(x)$rhs[b]
    
    if ( all(sapply(J, is_na)) ) return( NULL )
    if ( any(sapply(J, is_na)) ) {
        stop("missing jacobian for inequality constrains")
    }
    
    fargs <- unique(unlist(lapply(J, function(f) names(formals(f)))))
    fargs <- sprintf("function(%s)", paste(fargs, collapse=", "))

    ## there are 2 different types those where rhs, is 0 and the others
    rhs_is_0 <- abs(rhs) < xtol
    n <- sum(b)
    code <- ""
    for ( i in seq_len(n) ) {
        f0 <- sprintf("FUN%i <- function()", i)
        if ( rhs_is_0[i] ) { ########### RHS == 0
            code <- c(code, f0, deparse(get_callable(J[[i]])), "" ) 
        } else {             ########### RHS != 0
            code <- c(code, f0, "{", standardize_eq(J[[i]], rhs[[i]]), "}", "")
        }
    }
    ret_val <- sprintf("rbind(%s)", paste(sprintf("FUN%i()", seq_len(n)), collapse=", "))
    eval(parse(text=c(fargs, "{", code, "", ret_val, "", "}" )), envir=list())
}
