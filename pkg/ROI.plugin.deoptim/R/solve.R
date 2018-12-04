## get_lb
## ======
##
## get lower bound constraints
get_lb <- function(x) {
    lb <- numeric( length(x$objective) )
    lb[ ROI::bounds(x)$lower$ind ] <- ROI::bounds(x)$lower$val
    return(lb)
}

## get_ub
## ======
##
## get upper bound constraints
get_ub <- function(x, .machine.max=Inf) {
    ub <- rep.int(.machine.max, length(x$objective))
    ub[ ROI::bounds(x)$upper$ind ] <- ROI::bounds(x)$upper$val
    return(ub)
}

.deoptim_control_names <- c("VTR", "strategy", "NP", "itermax", "CR", "F", "bs", "trace",  "initialpop", "storepopfrom", "storepopfreq", "p", "c", "reltol",  "steptol", "parallelType", "packages", "parVar", "foreachArgs")

solve_op_deoptim <- function( x, control ) {
    solver <- "deoptim"

    if ( is.null(control$trace) )
        control$trace <- FALSE

    lb <- get_lb(x)
    ub <- get_ub(x)

    lb <- replace(lb, lb == -Inf, -1e30)
    ub <- replace(ub, ub ==  Inf,  1e30)

    if ( !is.null(control$start) & is.null(control$initialpop) ) {
        if ( is.vector(control$start) ) {
            if ( is.null(control$NP) ) {
                n <- length(objective(x))
                control$NP <- 10 * n ## the default value of deoptim
                brunif <- function(min, max) runif(1, min, max)
                start <- lapply(seq_len(control$NP), function(z) mapply(brunif, lb, ub))
                control$initialpop <- do.call(rbind, start)
            }
        }
    }

    opti <- list(DEoptim)
    if ( isTRUE(x$maximum) ) {
        objective_function <- terms(objective(x))$F
        opti$fn <- function(x) -objective_function(x)
    } else {
        opti$fn <- terms(objective(x))$F
    }
    opti$lower <- lb
    opti$upper <- ub
    opti$fnMap <- control$fnMap
    opti$control <- control[intersect(names(control), .deoptim_control_names)]
    
    mode(opti) <- "call"

    if ( isTRUE(control$dry_run) )
        return( opti )

    out <- eval(opti)

    x.solution <- setNames(out$optim$bestmem, terms(objective(x))$names)

    ROI_plugin_canonicalize_solution(  solution  = x.solution,
                                       optimum   = out$optim$bestval,
                                       status    = 0L,
                                       solver    = solver,
                                       message   = out)
}

.deoptimr_control_names <- c("eps", "NP", "Fl", "Fu", "tau_F", "tau_CR", 
                             "tau_pF", "jitter_factor", "tol", "maxiter",
                             "fnscale", "compare_to", "add_to_init_pop", 
                             "trace", "triter", "details")

.deoptimr_default <- function(d) {
    list(DEoptimR::JDEoptim,
         lower = NULL, upper = NULL, fn = NULL, constr = NULL, meq = 0L, 
         eps = 1e-05, NP = 10 * d, Fl = 0.1, Fu = 1, tau_F = 0.1, tau_CR = 0.1, 
         tau_pF = 0.1, jitter_factor = 0.001, tol = 1e-15, maxiter = 200 * d, 
         fnscale = 1, compare_to = c("median", "max"), add_to_init_pop = NULL, 
         trace = FALSE, triter = 1, details = FALSE)
}

##
## h_i(x) == 0   i = 1, ..., meq
## g_i(x) <= 0
solve_op_deoptimr <- function(x, control = list()) {
    solver <- "deoptimr"
    if ( is.null(control$trace) )
        control$trace <- FALSE
    if ( !is.null(control$start) & is.null(control$add_to_init_pop) ) {
        control$add_to_init_pop <- control$start
    }

    m <- .deoptimr_default(length(objective(x)))
  
    lower <- get_lb(x)
    m$lower <- replace(lower, lower == -Inf, -1e30)
    upper <- get_ub(x)
    m$upper <- replace(upper, upper ==  Inf,  1e30)

    if ( isTRUE(x$maximum) ) {
        objective_function <- terms(objective(x))$F
        m$fn <- function(x) -objective_function(x)
    } else {
        m$fn <- terms(objective(x))$F
    }

    constraints(x) <- as.F_constraint(constraints(x))
    eqcon <- ROI_plugin_build_equality_constraints(x, "eq_zero")
    leqcon <- ROI_plugin_build_inequality_constraints(x, "leq_zero")
    if ( !is.null(eqcon$F) ) {
        m$meq <- length(eqcon$F(control[["start"]]))
    }

    if ( !is.NO_constraint(constraints(x)) ) {
        m$constr <- build_constraint(eqcon$F, leqcon$F)    
    }

    for (key in intersect(names(control), .deoptimr_control_names)) {
        m[[key]] <- control[[key]]
    }
    
    mode(m) <- "call"

    if ( isTRUE(control$dry_run) )
        return(m)

    res <- eval(m)

    obj_val <- objective(x)(res$par)

    ROI_plugin_canonicalize_solution(solution  = res$par,
                                     optimum   = obj_val,
                                     status    = as.integer(res$convergence),
                                     solver    = solver,
                                     message   = res)

}

build_constraint <- function(EQFUN, LEQFUN) {
    if ( is.null(EQFUN) )
        return(function(x, ...) LEQFUN(x))
    if ( is.null(LEQFUN) )
        return(function(x, ...) EQFUN(x))
    function(x, ...) c(EQFUN(x), LEQFUN(x))
}

