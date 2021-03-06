## get_lb
## ======
##
## get lower bound constraints
get_lb <- function(x) {
    lb <- numeric( length(objective(x)) )
    lb[ bounds(x)$lower$ind ] <- bounds(x)$lower$val
    return(lb)
}

## get_ub
## ======
##
## get upper bound constraints
get_ub <- function(x) {
    ub <- rep.int(Inf, length(objective(x)))
    ub[ bounds(x)$upper$ind ] <- bounds(x)$upper$val
    return(ub)
}

mat <- function(i, j, v, nrow=max(i), ncol=max(j)) {
    stopifnot((length(i) == length(j)), (length(j) == length(v)))
    M <- matrix(0, nrow=nrow, ncol=ncol)
    for (k in seq_along(i)) M[i[k], j[k]] <- v[k]
    M
}

## hin: hin[j] > 0
##      x - lb > 0
##      ub - x > 0
bounds_to_constraints <- function(x) {
    n <- length(objective(x))
    lb <- get_lb(x)
    i <- which(lb != -Inf)
    ub <- get_ub(x)
    j <- which(ub != Inf)
    rm(x) ## cleanup hin and hin.jac environment!

    if ( !(length(i) + length(j)) )
        return( list(hin=NULL, hin.jac=NULL) )

    hin <- function(x) c((x-lb)[i], (ub-x)[j])
    JAC <- mat(i=seq_len(length(i) + length(j)), 
               j=c(i, j), 
               v=c(rep.int(1, length(i)), rep.int(-1, length(j))))
    hin.jac <- function(x) JAC
    return( list(hin=hin, hin.jac=hin.jac) )
}

outer_control_names <- c("lam0", "sig0", "eps", "itmax", "method", 
    "trace", "NMinit", "ilack.max", "i.scale", "e.scale", "kkt2.check")


## alabama
## =======
solve_alabama_auglag <- function( x, control = list() ) {
    if ( is.null(control$par) ) {
        stop("no start value, please provide a start value via control$start!")
    }

    args <- list()
    args$call_fun <- alabama::auglag
    args$par <- control$par
    if ( isTRUE(x$max) ) {
        objective_function <- objective(x)
        args$fn <- function(x) -objective_function(x)
        gradient_objective_function <- G(objective(x))
        args$gr <- function(x) -gradient_objective_function(x)
    } else {
        args$fn <- objective(x)
        args$gr <-G(objective(x))
    }   

    ## h_inequality constraints
    hin <- ROI_plugin_build_inequality_constraints(x, type="geq_zero")
    args$hin <- hin$F
    if ( is.null(hin$J) ) {
        if ( is.null(hin$F) ) {
            args$hin.jac <- NULL
        } else {
            args$hin.jac <- J(hin$F)
        }
    } else {
        args$hin.jac <- hin$J
    }

    ## NOTE: 
    ##  Since auglag doesn't pass lower and upper bounds to optim and
    ##  nlminb we can not set them directly but have to provide them
    ##  as functions. Sneeking it in via the ... operator is just to
    ##  unsafe.
    bc <- bounds_to_constraints(x)
    if ( !is.null(bc$hin) ) {
        if ( is.null(args$hin) ) {
            args$hin <- bc$hin
            args$hin.jac <- bc$hin.jac
        } else {
            hin <- args$hin
            args$hin <- function(x) c(hin(x), bc$hin(x))
            hin.jac <- args$hin.jac
            args$hin.jac <- function(x) rbind(hin.jac(x), bc$hin.jac(x))
        }
    }

    heq <- ROI_plugin_build_equality_constraints(x, type="eq_zero")
    args$heq <- heq$F
    if ( is.null(heq$J) ) {
        if ( is.null(heq$F) ) {
            args$heq.jac <- NULL
        } else {
            args$heq.jac <- J(heq$F)
        }
    } else {
        args$heq.jac <- heq$J
    }

    args$control.outer <- modifyList(as.list(control$control.outer), 
                                     control[names(control) %in% outer_control_names])

    if ( is.null(args$control.outer$trace) )
        args$control.outer$trace <- FALSE
    args$control.optim <- control$control.optim

    mode(args) <- "call"

    if ( isTRUE(control$dry_run) )
        return(args)
    
    res <- eval(args)

    ROI_plugin_canonicalize_solution(solution  = res$par,
                                     optimum   = objective(x)(res$par),
                                     status    = res$convergence,
                                     solver    = "alabama",
                                     message   = res)
}

