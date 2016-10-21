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

## control$par <- control$start
## control$start <- NULL

solve_optimr <- function( x, control ) {
    solver <- .ROI_plugin_get_solver_name( getPackageName() )

    if ( is.null(control$par) )
        stop("argument 'start' is missing with no default")

    lb <- get_lb(x)
    ub <- get_ub(x)

    opti <- list(optimr)
    opti$par <- control$par
    opti$fn <- terms(objective(x))$F
    opti$gr <- G(objective(x))
    opti$lower <- lb
    opti$upper <- ub
    opti$method <- control$method
    opti$hessian <- if ( is.null(control$hessian) ) FALSE else control$hessian
    if ( is.null(control$method) ) control$all.methods <- TRUE
    control$maximize <- x$maximum
    if ( is.null(control$fnscale) ) control$fnscale <- if ( x$maximum ) -1 else 1
    cn <- setdiff(names(control), c("par", "hess", "method", "itnmax", "hessian"))
    opti$control <- control[cn]
    mode(opti) <- "call"

    if ( isTRUE(control$dry_run) )
        return( opti )

    out <- eval(opti)

    sol <- .ROI_plugin_canonicalize_solution( solution  = out$par,
                                              optimum   = out$value,
                                              status    = sanitize_status(out$convergence),
                                              solver    = solver,
                                              message   = out,
                                              algorithm = control$method )
    sol$status$msg$message <- out$message
    return(sol)
}

sanitize_status <- function(status) {
    if ( !length(status) ) 
        return(-1L)
    if ( isTRUE(status != 0) )
        return(-1L)
    return(0L)
}