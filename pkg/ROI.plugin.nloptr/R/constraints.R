
## --------------------------------------
## Box Bound Constraints
## --------------------------------------
unique_formals <- function(x) {
    x <- do.call(c, lapply(x, formals))
    x[!duplicated(names(x))]
}

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
##build_inequality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
##    xtol <- (tol / 100)
##    b <- constraints(x)$dir %in% c("<", "<=", ">=", ">")
##    if ( !any(b) ) return( NULL )
##
##    F <- constraints(x)$F[b]
##    dir <- constraints(x)$dir[b]
##    rhs <- constraints(x)$rhs[b]
##
##    fargs <- unique(unlist(lapply(F, function(f) names(formals(f)))))
##    fnew <- sprintf("function(%s) ", paste(fargs, collapse=", "))
##
##    ## there are 2 different types those where rhs, is 0 and the others
##    rhs_is_0 <- abs(rhs) < xtol
##    n <- sum(b)
##    code <- ""
##
##    E <- list()
##    fnew_body <- character()
##
##    for ( i in seq_len(n) ) {
##        ## nur <=
##        form <- paste(names(formals(F[[i]])), collapse=", ")
##        f0 <- sprintf("function(%s)", form)
##        code <- c(f0, "{", standardize_leq(F[[i]], rhs[[i]]), "}", "")
##        E[[i]] <- eval(parse(text=code))
##        environment(E[[i]]) <- environment(F[[i]])
##        fnew_body <- c(fnew_body, sprintf("E[[%i]](%s)", i, form))
##    }
##
##    fnew_body <- sprintf("c(%s)", paste(fnew_body, collapse=", "))
##    fnew <- paste(c(fnew, "{", fnew_body, "}"), collapse="\n")
##    return(eval(parse(text=fnew)))
##}

if (FALSE) {
    z <- c(1, 1, 1)
    a <- 2
    b <- 1
    f <- eval_g0
    l(eval_g0)

    f10 <- function(z) {
        z
    }

    f10(eval(as.name(names(formals(f10)))))

    call("y[[1]]", as.name(names(formals(y[[1]]))))
    dcall <- function(x)

    rm(E)
    y <- build_inequality_constraints(x)
    y
    y(c(1, 1, 1))
    ls(environment(y))
    ls(environment(environment(y)$E[[1]]))

    y
    (z)

    y[[1]](c(1, 1, 1))

    g0(c(1, 1, 1))
    tol
    subtitue(1/3)

    ls(environment(y[[1]]))
    ls(environment(y))
    ls(environment(FUN1))
    environment(FUN1) <- environment(x$constraint$F[[1]])
    environment(FUN1)
    ls(environment(x$constraint$F[[1]]))
    ls(environment(environment(FUN1)$FUN1))
    ls(environment(environment(environment(FUN1)$FUN1)$FUN1))


    i <- 1L
    for ( i in seq_len(n) ) {
        f0 <- sprintf("FUN%i <- function(%s)", i, names(formals(F[[i]])))
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
    cat(c(fargs, "{", code, "", ret_val, "", "}" ), sep="\n")
    eval(parse(text=c(fargs, "{", code, "", ret_val, "", "}" )), envir=environment(F[[i]]))
}

build_inequality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
    xtol <- (tol / 100)
    b <- constraints(x)$dir %in% c("<", "<=", ">=", ">")
    if ( !any(b) ) return( NULL )
    
    F <- constraints(x)$F[b]
    dir <- constraints(x)$dir[b]
    rhs <- constraints(x)$rhs[b]
   
    ## there are 2 different types those where rhs, is 0 and the others
    rhs_is_0 <- abs(rhs) < xtol
    n <- sum(b)

    E <- list()
    leq_fun <- function(i) {
        DFUN <- F[[i]]
        RHS <- rhs[i]
        function(x) {
            f <- c(list(DFUN), lapply(names(formals(DFUN)), as.name))
            mode(f) <- "call"
            eval(f) - RHS
        }
    }

    geq_fun <- function(i) {
        DFUN <- F[[i]]
        RHS <- rhs[i]
        function(x) {
            f <- c(list(DFUN), lapply(names(formals(DFUN)), as.name))
            mode(f) <- "call"
            RHS - eval(f)
        }   
    }
    
    for ( i in seq_len(n) ) {
        if ( dir[i] %in% c("<", "<=") ) {
            if ( dir[i] == "<" ) rhs[i] <- rhs[i] - tol
            E[[i]] <- leq_fun(i)
        } else {
            if ( dir[i] == ">" ) rhs[i] <- rhs[i] + tol
        }
        formals(E[[i]]) <- formals(F[[i]])
    }

    ineq_fun <- function(x) {
        FUN <- E
        X <- numeric(n)
        for (i in seq_along(X)) {
            f <- c(list(FUN[[i]]), lapply(names(formals(FUN[[i]])), as.name))
            mode(f) <- "call"
            X[i] <- eval(f)
        }
        return(X)
    }
    formals(ineq_fun) <- unique_formals(F)
    return(ineq_fun)
}

if (FALSE) {
    library(nloptr)
    attach(getNamespace("ROI.plugin.nloptr"))
    fnew <- build_inequality_constraints(x)
    fnew(c(1, 1, 1))
    fnew
    fnew[[1]]
    fnew[[1]](c(1, 1, 1))
    fnew[[2]](c(1, 1, 1))
    fnew[[3]](c(1, 1, 1))

    traceback()

    dcall <- function(DFUN) {
        f <- c(list(DFUN), lapply(names(formals(DFUN)), as.name))
        mode(f) <- "call"
        eval(f)
    }


    dcall <- function(DFUN) {
        eval(parse(text=sprintf("DFUN(%s)", paste(names(formals(DFUN)), collapse=", "))))
        
    }

    a <- 1
    b <- 2
    c <- 3
    hello <- function(a, b, c) (a + b + c)
    hella <- function(a, b, f) (a + b + f)
    hello(1, 2, 3)
    call("hello")

    do.call(c, lapply(list(hello, hella), formals))

    unique_formals(hello, hella)

    c(formals(hello), formals(hella))[!duplicated(names(c(formals(hello), formals(hella))))]

    g <- c(list(hello), lapply(names(formals(hello)), as.name))
    mode(g) <- "call"
    eval(g)
}

## build_inequality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
##     xtol <- (tol / 100)
##     b <- constraints(x)$dir %in% c("<", "<=", ">=", ">")
##     if ( !any(b) ) return( NULL )
## 
##     F <- constraints(x)$F[b]
##     dir <- constraints(x)$dir[b]
##     rhs <- constraints(x)$rhs[b]
## 
##     fargs <- unique(unlist(lapply(F, function(f) names(formals(f)))))
##     fargs <- sprintf("function(%s)", paste(fargs, collapse=", "))
## 
##     ## there are 2 different types those where rhs, is 0 and the others
##     rhs_is_0 <- abs(rhs) < xtol
##     n <- sum(b)
##     code <- ""
##     for ( i in seq_len(n) ) {
##         f0 <- sprintf("FUN%i <- function()", i)
##         if ( dir[i] == "<" ) {
##             ## since nloptr only knows <= add some epsilon smaller than the tolerance to obtain <
##             code <- c(code, f0, "{", standardize_leq(F[[i]], rhs[[i]] + xtol), "}", "")
##         } else if ( dir[i] == "<=" ) {
##             if ( rhs_is_0[i] ) { ########### RHS == 0
##                 code <- c(code, f0, deparse(get_callable(F[[i]])), "" ) 
##             } else {             ########### RHS != 0
##                 code <- c(code, f0, "{", standardize_leq(F[[i]], rhs[[i]]), "}", "")
##             }
##         } else if ( dir[i] == ">=" ) {
##             code <- c(code, f0, "{", standardize_geq(F[[i]], rhs[[i]]), "}", "")
##         } else if ( dir[i] == ">" ) {
##             code <- c(code, f0, "{", standardize_geq(F[[i]], rhs[[i]] - xtol), "}", "")
##         } else {
##             stop(" :( (sad face)")
##         }
##     }
##     ret_val <- sprintf("c(%s)", paste(sprintf("FUN%i()", seq_len(n)), collapse=", "))
##     eval(parse(text=c(fargs, "{", code, "", ret_val, "", "}" )), envir=environment(F[[i]]))
## }


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
   
    ## there are 2 different types those where rhs, is 0 and the others
    rhs_is_0 <- abs(rhs) < xtol
    n <- sum(b)

    E <- vector(mode="list", n)
    eq_fun <- function(i) {
        DFUN <- F[[i]]
        RHS <- rhs[i]
        function(x) {
            f <- c(list(DFUN), lapply(names(formals(DFUN)), as.name))
            mode(f) <- "call"
            eval(f) - RHS
        }
    }

    for ( i in seq_len(n) ) {    
        E[[i]] <- eq_fun(i)
        formals(E[[i]]) <- formals(F[[i]])
    }

    EFUN <- function(x) {
        FUN <- E
        X <- numeric(n)
        for (i in seq_along(X)) {
            f <- c(list(FUN[[i]]), lapply(names(formals(FUN[[i]])), as.name))
            mode(f) <- "call"
            X[i] <- eval(f)
        }
        return(X)
    }
    formals(EFUN) <- unique_formals(F)
    return(EFUN)
}

##build_equality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
##    xtol <- (tol / 100)
##    b <- constraints(x)$dir == "=="
##    if ( !any(b) ) return( NULL )
##
##    F <- constraints(x)$F[b]
##    dir <- constraints(x)$dir[b]
##    rhs <- constraints(x)$rhs[b]
##
##    fargs <- unique(unlist(lapply(F, function(f) names(formals(f)))))
##    fargs <- sprintf("function(%s)", paste(fargs, collapse=", "))
##
##    ## there are 2 different types those where rhs, is 0 and the others
##    rhs_is_0 <- abs(rhs) < xtol
##    n <- sum(b)
##    code <- ""
##    for ( i in seq_len(n) ) {
##        f0 <- sprintf("FUN%i <- function()", i)
##        if ( rhs_is_0[i] ) { ########### RHS == 0
##            code <- c(code, f0, deparse(get_callable(F[[i]])), "" ) 
##        } else {             ########### RHS != 0
##            code <- c(code, f0, "{", standardize_eq(F[[i]], rhs[[i]]), "}", "")
##        }
##    }
##    ret_val <- sprintf("c(%s)", paste(sprintf("FUN%i()", seq_len(n)), collapse=", "))
##    eval(parse(text=c(fargs, "{", code, "", ret_val, "", "}" )), envir=environment(F[[i]]))
##}

build_jacobian_inequality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
    b <- constraints(x)$dir %in% c("<", "<=", ">=", ">")
    if ( !any(b) ) return( NULL )
    n <- sum(b)
    J <- constraints(x)$J[b]
    dir <- constraints(x)$dir[b]
    JFUN <- function() {
        X <- vector("list", n)
        for ( i in seq_len(n) ) {
            DFUN <- J[[i]]
            f <- c(list(DFUN), lapply(names(formals(DFUN)), as.name))
            mode(f) <- "call"
            X[[i]] <- (-1)^(dir[i] %in% c(">", ">=")) * eval(f)
        }
        do.call(rbind, X)
    }
    formals(JFUN) <- unique_formals(constraints(x)$J[b])
    return(JFUN)
}

if (FALSE) {
    build_jacobian_inequality_constraints(x)(c(1, 1, 1))
}

## build_jacobian_inequality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
##     xtol <- (tol / 100)
##     b <- constraints(x)$dir %in% c("<", "<=", ">=", ">")
##     if ( !any(b) ) return( NULL )
## 
##     J <- constraints(x)$J[b]
##     dir <- constraints(x)$dir[b]
##     rhs <- constraints(x)$rhs[b]
## 
##     if ( all(sapply(J, is_na)) ) return( NULL )
##     if ( any(sapply(J, is_na)) ) {
##         stop("missing jacobian for inequality constrains")
##     }
## 
##     fargs <- unique(unlist(lapply(J, function(f) names(formals(f)))))
##     fargs <- sprintf("function(%s)", paste(fargs, collapse=", "))
## 
##     ## there are 2 different types those where rhs, is 0 and the others
##     rhs_is_0 <- abs(rhs) < xtol
##     n <- sum(b)
##     code <- ""
##     for ( i in seq_len(n) ) {
##         f0 <- sprintf("FUN%i <- function()", i)
##         if ( dir[i] == "<" ) {
##             ## since nloptr only knows <= add some epsilon smaller than the tolerance to obtain <
##             code <- c(code, f0, "{", standardize_leq(J[[i]], rhs[[i]] + xtol), "}", "")
##         } else if ( dir[i] == "<=" ) {
##             if ( rhs_is_0[i] ) { ########### RHS == 0
##                 code <- c(code, f0, deparse(get_callable(J[[i]])), "" ) 
##             } else {             ########### RHS != 0
##                 code <- c(code, f0, "{", standardize_leq(J[[i]], rhs[[i]]), "}", "")
##             }
##         } else if ( dir[i] == ">=" ) {
##             code <- c(code, f0, "{", standardize_geq(J[[i]], rhs[[i]]), "}", "")
##         } else if ( dir[i] == ">" ) {
##             code <- c(code, f0, "{", standardize_geq(J[[i]], rhs[[i]] - xtol), "}", "")
##         } else {
##             stop(" :( (sad face)")
##         }
##     }
##     ret_val <- sprintf("rbind(%s)", paste(sprintf("FUN%i()", seq_len(n)), collapse=", "))
##     eval(parse(text=c(fargs, "{", code, "", ret_val, "", "}" )), envir=environment(J[[i]]))
## }

##build_jacobian_inequality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
##    xtol <- (tol / 100)
##    b <- constraints(x)$dir %in% c("<", "<=", ">=", ">")
##    if ( !any(b) ) return( NULL )
##
##    J <- constraints(x)$J[b]
##    dir <- constraints(x)$dir[b]
##    rhs <- constraints(x)$rhs[b]
##
##    if ( all(sapply(J, is_na)) ) return( NULL )
##    if ( any(sapply(J, is_na)) ) {
##        stop("missing jacobian for inequality constrains")
##    }
##
##    fargs <- unique(unlist(lapply(J, function(f) names(formals(f)))))
##    fargs <- sprintf("function(%s)", paste(fargs, collapse=", "))
##
##    ## there are 2 different types those where rhs, is 0 and the others
##    rhs_is_0 <- abs(rhs) < xtol
##    n <- sum(b)
##    code <- ""
##    for ( i in seq_len(n) ) {
##        f0 <- sprintf("FUN%i <- function()", i)
##        if ( dir[i] == "<" ) {
##            ## since nloptr only knows <= add some epsilon smaller than the tolerance to obtain <
##            code <- c(code, f0, "{", standardize_leq(J[[i]], rhs[[i]] + xtol), "}", "")
##        } else if ( dir[i] == "<=" ) {
##            if ( rhs_is_0[i] ) { ########### RHS == 0
##                code <- c(code, f0, deparse(get_callable(J[[i]])), "" ) 
##            } else {             ########### RHS != 0
##                code <- c(code, f0, "{", standardize_leq(J[[i]], rhs[[i]]), "}", "")
##            }
##        } else if ( dir[i] == ">=" ) {
##            code <- c(code, f0, "{", standardize_geq(J[[i]], rhs[[i]]), "}", "")
##        } else if ( dir[i] == ">" ) {
##            code <- c(code, f0, "{", standardize_geq(J[[i]], rhs[[i]] - xtol), "}", "")
##        } else {
##            stop(" :( (sad face)")
##        }
##    }
##    ret_val <- sprintf("rbind(%s)", paste(sprintf("FUN%i()", seq_len(n)), collapse=", "))
##    eval(parse(text=c(fargs, "{", code, "", ret_val, "", "}" )), envir=environment(J[[i]]))
##}

##build_jacobian_equality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
##    xtol <- (tol / 100)
##    b <- constraints(x)$dir == "=="
##    if ( !any(b) ) return( NULL )
##
##    J <- constraints(x)$J[b]
##    dir <- constraints(x)$dir[b]
##    rhs <- constraints(x)$rhs[b]
##
##    if ( all(sapply(J, is_na)) ) return( NULL )
##    if ( any(sapply(J, is_na)) ) {
##        stop("missing jacobian for inequality constrains")
##    }
##
##    fargs <- unique(unlist(lapply(J, function(f) names(formals(f)))))
##    fargs <- sprintf("function(%s)", paste(fargs, collapse=", "))
##
##    ## there are 2 different types those where rhs, is 0 and the others
##    rhs_is_0 <- abs(rhs) < xtol
##    n <- sum(b)
##    code <- ""
##    for ( i in seq_len(n) ) {
##        f0 <- sprintf("FUN%i <- function()", i)
##        if ( rhs_is_0[i] ) { ########### RHS == 0
##            code <- c(code, f0, deparse(get_callable(J[[i]])), "" ) 
##        } else {             ########### RHS != 0
##            code <- c(code, f0, "{", standardize_eq(J[[i]], rhs[[i]]), "}", "")
##        }
##    }
##    ret_val <- sprintf("rbind(%s)", paste(sprintf("FUN%i()", seq_len(n)), collapse=", "))
##    eval(parse(text=c(fargs, "{", code, "", ret_val, "", "}" )), envir=environment(J[[i]]))
##}

build_jacobian_equality_constraints <- function(x, tol=nloptr_defaults("tol_constraints_ineq")) {
    b <- constraints(x)$dir == "=="
    if ( !any(b) ) return( NULL )
    n <- sum(b)
    J <- constraints(x)$J[b]
    dir <- constraints(x)$dir[b]
    JFUN <- function() {
        X <- vector("list", n)
        for ( i in seq_len(n) ) {
            DFUN <- J[[i]]
            f <- c(list(DFUN), lapply(names(formals(DFUN)), as.name))
            mode(f) <- "call"
            X[[i]] <- eval(f)
        }
        do.call(rbind, X)
    }
    formals(JFUN) <- unique_formals(constraints(x)$J[b])
    return(JFUN)
}
