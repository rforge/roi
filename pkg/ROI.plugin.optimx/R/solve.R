## get_lb
## ======
##
## get lower bound constraints
get_lb <- function(x) {
    if( !length(bounds(x)$lower$val) ) {
        lb <- 0
    } else {
        lb <- numeric( length(x$objective) )
        lb[ bounds(x)$lower$ind ] <- bounds(x)$lower$val
    }
    return(lb)
}

## get_ub
## ======
##
## get upper bound constraints
get_ub <- function(x) {
    if( !length(bounds(x)$upper$val) ) {
        ub <- Inf
    } else {
        ub <- rep.int(Inf, length(x$objective))
        ub[ bounds(x)$upper$ind ] <- bounds(x)$upper$val
    }
    return(ub)
}

solve_optimx <- function( x, control ) {
    solver <- ROI_plugin_get_solver_name( getPackageName() )

    if ( is.null(control$par) )
        stop("argument 'start' is missing with no default")

    if ( is.null(control$method) )
        control$method <- "L-BFGS-B"

    lb <- get_lb(x)
    ub <- get_ub(x)

    opti <- list(optimx)
    opti$par <- control$par
    opti$fn <- terms(objective(x))$F
    opti$gr <- G(objective(x))
    opti$hess <- control$hess
    opti$lower <- lb
    opti$upper <- ub
    opti$method <- control$method
    opti$itnmax <- control$itnmax    
    opti$hessian <- if ( is.null(control$hessian) ) FALSE else control$hessian
    if ( is.null(control$method) ) control$method <- "nlminb"
    control$maximize <- x$maximum
    cn <- setdiff(names(control), c("par", "hess", "method", "itnmax", "hessian"))
    opti$control <- control[cn]
    mode(opti) <- "call"

    if ( isTRUE(control$dry_run) )
        return( opti )

    out <- eval(opti)

    i <- if ( x$maximum ) which.max(out$value) else which.min(out$value)

    solution <- as.numeric(out[i, seq_len(length(control$par))])
    algorithm <- if ( is.null(control$method) ) "all" else control$method

    ROI_plugin_canonicalize_solution(  solution  = solution,
                                        optimum   = out$value[i],
                                        status    = out$convcode[i],
                                        solver    = solver,
                                        message   = out,
                                        algorithm = rownames(out)[i])
}

