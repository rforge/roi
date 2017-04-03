## get_lb
## ======
##
## get lower bound constraints
get_lb <- function(x) {
    lb <- numeric( length(x$objective) )
    lb[ bounds(x)$lower$ind ] <- bounds(x)$lower$val
    return(lb)
}

## get_ub
## ======
##
## get upper bound constraints
get_ub <- function(x, .machine.max=Inf) {
    ub <- rep.int(.machine.max, length(x$objective))
    ub[ bounds(x)$upper$ind ] <- bounds(x)$upper$val
    return(ub)
}

.deoptim_control_names <- c("VTR", "strategy", "NP", "itermax", "CR", "F", "bs", "trace",  "initialpop", "storepopfrom", "storepopfreq", "p", "c", "reltol",  "steptol", "parallelType", "packages", "parVar", "foreachArgs")

solve_deoptim <- function( x, control ) {
    solver <- ROI_plugin_get_solver_name( getPackageName() )

    lb <- get_lb(x)
    ub <- get_ub(x)

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

