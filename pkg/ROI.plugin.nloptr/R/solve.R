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
    lb <- numeric( length(x$objective) )
    lb[ bounds(x)$lower$ind ] <- bounds(x)$lower$val
    return(lb)
}

## get_ub
## ======
##
## get upper bound constraints
get_ub <- function(x) {
    ub <- rep.int(Inf, length(x$objective))
    ub[ bounds(x)$upper$ind ] <- bounds(x)$upper$val
    return(ub)
}

nloptr_defaults <- function(x=NULL) {
    d <- nloptr.get.default.options()
    ## set variables needed to evaluate the default values
    x0 <- 0L
    num_constraints_ineq <- 1L
    num_constraints_eq <- 1L
    ## fix typo in nloptr types
    d[,'type'] <- gsub("interger", "integer", d[,'type'])
    defaults <- list()
    for (i in seq_len(nrow(d))) {
        if (d[i, 'type'] == "character") {
            defaults[[d[i, 'name']]] <- d[i, 'default']
        } else {
            defaults[[d[i, 'name']]] <- 
                tryCatch({
                    as(d[i, 'default'], d[i, 'type'])
                },
                         warning = function(w) {
                             as(eval(parse(text=d[i, 'default'])), d[i, 'type'])
                         })
        }
    }
    if ( is.null(x) ) return( defaults )
    if ( length(x) == 1L ) return( defaults[[x]] )
    return( defaults[x] )
}



OF <- function(x) {
    environment(objective(x))$F
}

change_sign <- function(F) {
    if ( is.null(F) )
        return(NULL)
    function(x) {
        -F(x)
    }
}

nl.opts_names <- function() {
    c("stopval", "xtol_rel", "maxeval", "ftol_rel", "ftol_abs", 
      "check_derivatives")
}

build_nl.opts <- function(control) {
    nl_opts <- control[names(control) %in% nl.opts_names()]
    nl_opts <- if (length(nl_opts)) nloptr::nl.opts(nl_opts) else nloptr::nl.opts()
    ## Set algorithm to NULL else bobyqa complaints!
    nl_opts$algorithm <- NULL
}

nlopt_problem_constrained <- function(x, start, derivate_free = FALSE) {
    m <- list(call_fun = nloptr::nloptr, x0 = start)

    if (!inherits(constraints(x), "F_constraint")) {
        constraints(x) <- as.F_constraint(constraints(x))
    }
    
    inq_con <- ROI_plugin_build_inequality_constraints(x, type = "leq_zero") 
    eq_con <- ROI_plugin_build_equality_constraints(x, type = "eq_zero")

    m$eval_f <- if ( maximum(x) ) change_sign(objective(x)) else objective(x)
    m$eval_g_ineq = inq_con[["F"]]
    m$eval_g_eq = eq_con[["F"]]

    if ( !derivate_free ) {
        m$eval_grad_f <- if ( maximum(x) ) change_sign(G(objective(x))) else G(objective(x))
        m$eval_jac_g_ineq = inq_con[["J"]]
        m$eval_jac_g_eq = eq_con[["J"]]
    }

    m$lb <- get_lb(x)
    m$ub <- get_ub(x)
    m
}

nlopt_problem_ieq_constrained <- function(x, start, derivate_free = FALSE) {
    m <- list(call_fun = nloptr::nloptr, x0 = start)

    if (!inherits(constraints(x), "F_constraint")) {
        constraints(x) <- as.F_constraint(constraints(x))
    }

    inq_con <- ROI_plugin_build_inequality_constraints(x, type = "leq_zero") 

    m$eval_f <- if ( maximum(x) ) change_sign(objective(x)) else objective(x)
    m$eval_g_ineq = inq_con[["F"]]

    if ( !derivate_free ) {
        m$eval_grad_f <- if ( maximum(x) ) change_sign(G(objective(x))) else G(objective(x))
        m$eval_jac_g_ineq = inq_con[["J"]]
    }

    m$lb <- get_lb(x)
    m$ub <- get_ub(x)
    m
}

nlopt_problem <- function(x, start, derivate_free = FALSE) {
    m <- list(call_fun = nloptr::nloptr, x0 = start)

    m$eval_f <- if ( maximum(x) ) change_sign(objective(x)) else objective(x)
    if ( !derivate_free ) {
        m$eval_grad_f <- if ( maximum(x) ) change_sign(G(objective(x))) else G(objective(x))
    }

    m$lb <- get_lb(x)
    m$ub <- get_ub(x)
    m
}

nlopt_options <- function(control, defaults = list()) {
    opts_names <- c("algorithm", "stopval", "ftol_rel", "ftol_abs", "xtol_rel", 
        "xtol_abs", "maxeval", "maxtime", "tol_constraints_ineq", "tol_constraints_eq",
        "print_level", "check_derivatives", "check_derivatives_tol",
        "check_derivatives_print", "print_options_doc", "population", "ranseed",
        "local_opts")

    opts <- control[names(control) %in% opts_names]

    i <- which(!names(defaults) %in% names(opts))
    if ( length(i) ) {
        opts <- c(opts, defaults[i])
    }

    if ( is.null(opts$xtol_rel) )
        opts$xtol_rel <- 1e-4

    opts
}

solve_auglag <- function(x, control = list()) {
    solver <- "nloptr.auglag"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")
    
    algorithm <- if ( any(constraints(x)$dir == "eq") ) "NLOPT_LD_AUGLAG_EQ" else "NLOPT_LD_AUGLAG"

    auglag_defaults <- list(algorithm = algorithm)
    opts <- nlopt_options(control, auglag_defaults)
    if ( is.null(opts$local_opts) )
        opts$local_opts <- list(algorithm = "NLOPT_LD_LBFGS", xtol_rel = opts$xtol_rel)

    m <- nlopt_problem_constrained(x, start = control$x0, derivate_free = FALSE)
    m$opts <- opts

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)

    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution  = out$solution,
                                     optimum   = objval,
                                     status    = out$status,
                                     solver    = solver,
                                     message   = out)
}


solve_bobyqa <- function(x, control = list()) {
    solver <- "nloptr.bobyqa"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")

    bobyqa_defaults <- list(algorithm = "NLOPT_LN_BOBYQA")

    m <- nlopt_problem(x, start = control$x0, derivate_free = TRUE)
    m$opts <- nlopt_options(control, bobyqa_defaults)

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution  = out$solution,
                                     optimum   = objval,
                                     status    = out$status,
                                     solver    = solver,
                                     message   = out)
}

solve_cobyla <- function(x, control = list()) {
    solver <- "nloptr.cobyla"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")

    if ( any(constraints(x)$dir == "==") )
        stop("cobyla does not support equality constraints")
    
    defaults <- list(algorithm = "NLOPT_LN_COBYLA")
    opts <- nlopt_options(control, defaults)

    m <- nlopt_problem_ieq_constrained(x, start = control$x0, derivate_free = TRUE)
    m$opts <- opts

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution  = out$solution,
                                     optimum   = objval,
                                     status    = out$status,
                                     solver    = solver,
                                     message   = out)
}


solve_crs2lm <- function(x, control = list()) {
    solver <- "nloptr.crs2lm"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")

    crs2lm_defaults <- list(algorithm = "NLOPT_GN_CRS2_LM", maxeval = 10000, 
        pop.size = 10 * (length(control$x0) + 1), xtol_rel = 1e-06)

    m <- nlopt_problem(x, start = control$x0, derivate_free = TRUE)
    m$opts <- nlopt_options(control, crs2lm_defaults)
    if ( !is.null(control$ranseed) ) 
        opts$ranseed <- as.integer(control$ranseed)

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution  = out$solution,
                                     optimum   = objval,
                                     status    = out$status,
                                     solver    = solver,
                                     message   = out)
}

partition_space <- function(lower, upper) {
    lower <- replace(lower, lower == -Inf, -1e30)
    upper <- replace(upper, upper ==  Inf,  1e30)
    size <- upper - lower
    partition_rule_1 <- function(x) x / 2
    partition_rule_2 <- function(x) exp(log(x) / 2)   
    lower + ifelse(size <= 1000, partition_rule_1(size), partition_rule_2(size))
}

solve_direct <- function(x, control = list()) {
    solver <- "nloptr.direct"

    if ( !is.null(control$x0) )
        warning("argument 'start' provided but not needed, 'start' will be ignored")

    x0 <- partition_space(get_lb(x), get_ub(x))
  
    direct_defaults <- list(algorithm = "NLOPT_GN_DIRECT", maxeval = 10000L)

    m <- nlopt_problem(x, start = x0, derivate_free = TRUE)
    m$opts <- nlopt_options(control, direct_defaults)

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution  = out$solution,
                                     optimum   = objval,
                                     status    = out$status,
                                     solver    = solver,
                                     message   = out)
}

solve_directL <- function(x, control = list()) {
    solver <- "nloptr.directL"

    if ( !is.null(control$x0) )
        warning("argument 'start' provided but not needed, 'start' will be ignored")

    x0 <- partition_space(get_lb(x), get_ub(x))
    
    use_direct_rand <- (isTRUE(control$randomized) | is.null(control$randomized))
    algo <- if ( use_direct_rand ) "NLOPT_GN_DIRECT_L_RAND" else "NLOPT_GN_DIRECT_L"
    direct_defaults <- list(algorithm = algo, maxeval = 10000L, ftol_rel = 1e-14)

    m <- nlopt_problem(x, start = x0, derivate_free = TRUE)
    m$opts <- nlopt_options(control, direct_defaults)

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution  = out$solution,
                                     optimum   = objval,
                                     status    = out$status,
                                     solver    = solver,
                                     message   = out)
}

## solve_direct_parallel <- function(x, control = list()) {
##     solver <- "nloptr.direct_parallel"
## 
##     if ( !is.null(control$x0) )
##         warning("argument 'start' provided but not needed, 'start' will be ignored")
## 
##     x0 <- partition_space(get_lb(x), get_ub(x))    
## 
##     direct_defaults <- list(algorithm = "NLOPT_GN_DIRECT_L_RAND", 
##         maxeval = 10000L, ftol_rel = 1e-14)
## 
##     m <- nlopt_problem(x, start = x0, derivate_free = TRUE)
##     m$opts <- nlopt_options(control, direct_defaults)
## 
##     if ( !is.null(control$args) )
##         m <- c(m, control$args)
## 
##     if (isTRUE(control$dry_run)) {
##         mode(m) <- "call"
##         return(m)
##     }
## 
##     parallel_solve <- function(i, optimization_problem) {
##         optimization_problem$opts$ranseed <- i + max(-1, optimization_problem$opts$ranseed) 
##         mode(optimization_problem) <- "call"
##         eval(optimization_problem)
##     }
## 
##     ncores <- detectCores()
##     out <- mclapply(seq_len(ncores), parallel_solve, optimization_problem = m, 
##                     mc.cores = ncores)
## 
##     if ( maximum(x) ) {
##         k <- max(1, which.max(sapply(out, "[[", "objective")))
##     } else {
##         k <- max(1, which.min(sapply(out, "[[", "objective")))
##     }
##     out <- out[[k]]
##     objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)
## 
##     ROI_plugin_canonicalize_solution(solution  = out$solution,
##                                      optimum   = objval,
##                                      status    = out$status,
##                                      solver    = solver,
##                                      message   = out)
## }


solve_isres <- function(x, control = list()) {
    solver <- "nloptr.isres"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")
    
    isres_defaults <- list(maxeval = 10000, xtol_rel = 1e-06, 
        population = 20 * (length(control$x0) + 1), algorithm = "NLOPT_GN_ISRES")
    opts <- nlopt_options(control, isres_defaults)

    m <- nlopt_problem_constrained(x, start = control$x0, derivate_free = TRUE)
    m$opts <- opts

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)

    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution  = out$solution,
                                     optimum   = objval,
                                     status    = out$status,
                                     solver    = solver,
                                     message   = out)
}


solve_lbfgs <- function(x, control = list()) {
    solver <- "nloptr.lbfgs"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")

    lbfgs_defaults <- list(algorithm = "NLOPT_LD_LBFGS")

    m <- nlopt_problem(x, start = control$x0, derivate_free = FALSE)
    m$opts <- nlopt_options(control, lbfgs_defaults)

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution = out$solution,
                                     optimum  = objval,
                                     status   = out$status,
                                     solver   = solver,
                                     message  = out)
}


solve_mma <- function(x, control = list()) {
    solver <- "nloptr.mma"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")

    if ( any(constraints(x)$dir == "==") )
        stop("mma does not support equality constraints")
    
    defaults <- list(algorithm = "NLOPT_LD_MMA")
    opts <- nlopt_options(control, defaults)

    m <- nlopt_problem_ieq_constrained(x, start = control$x0, derivate_free = FALSE)
    m$opts <- opts

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution  = out$solution,
                                     optimum   = objval,
                                     status    = out$status,
                                     solver    = solver,
                                     message   = out)
}


solve_neldermead <- function(x, control = list()) {
    solver <- "nloptr.neldermead"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")

    neldermead_defaults <- list(algorithm = "NLOPT_LN_NELDERMEAD")

    m <- nlopt_problem(x, start = control$x0, derivate_free = TRUE)
    m$opts <- nlopt_options(control, neldermead_defaults)

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution = out$solution,
                                     optimum  = objval,
                                     status   = out$status,
                                     solver   = solver,
                                     message  = out)
}


solve_newuoa <- function(x, control = list()) {
    solver <- "nloptr.newuoa"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")

    defaults <- list(algorithm = "NLOPT_LN_NEWUOA")

    m <- nlopt_problem(x, start = control$x0, derivate_free = TRUE)
    m$opts <- nlopt_options(control, defaults)

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution = out$solution,
                                     optimum  = objval,
                                     status   = out$status,
                                     solver   = solver,
                                     message  = out)
}


solve_sbplx <- function(x, control = list()) {
    solver <- "nloptr.sbplx"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")

    defaults <- list(algorithm = "NLOPT_LN_SBPLX")

    m <- nlopt_problem(x, start = control$x0, derivate_free = TRUE)
    m$opts <- nlopt_options(control, defaults)

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution = out$solution,
                                     optimum  = objval,
                                     status   = out$status,
                                     solver   = solver,
                                     message  = out)
}


solve_slsqp <- function(x, control = list()) {
    solver <- "nloptr.slsqp"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")
    
    defaults <- list(algorithm = "NLOPT_LD_SLSQP")
    opts <- nlopt_options(control, defaults)

    m <- nlopt_problem_constrained(x, start = control$x0, derivate_free = FALSE)
    m$opts <- opts

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution = out$solution,
                                     optimum  = objval,
                                     status   = out$status,
                                     solver   = solver,
                                     message  = out)
}


solve_stogo <- function(x, control = list()) {
    solver <- "nloptr.stogo"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")

    use_stogo_rand <- (isTRUE(control$randomized) | is.null(control$randomized))
    algo <- if ( use_stogo_rand ) "NLOPT_GD_STOGO_RAND" else "NLOPT_GD_STOGO"
    defaults <- list(algorithm = algo, maxeval = 10000L, ftol_rel = 1e-14)

    m <- nlopt_problem(x, start = control$x0, derivate_free = FALSE)
    m$opts <- nlopt_options(control, defaults)

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution = out$solution,
                                     optimum  = objval,
                                     status   = out$status,
                                     solver   = solver,
                                     message  = out)
}


solve_tnewton <- function(x, control = list()) {
    solver <- "nloptr.tnewton"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")

    use_preconditioning <- (isTRUE(control$precond) | is.null(control$precond))

    if (use_preconditioning) {
        algo <- if (isTRUE(control$restart)) "NLOPT_LD_TNEWTON_PRECOND_RESTART" else "NLOPT_LD_TNEWTON_PRECOND"
    } else {
        algo <- if (isTRUE(control$restart)) "NLOPT_LD_TNEWTON_RESTART" else "NLOPT_LD_TNEWTON"
    }
    
    defaults <- list(algorithm = algo)

    m <- nlopt_problem(x, start = control$x0, derivate_free = FALSE)
    m$opts <- nlopt_options(control, defaults)

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution = out$solution,
                                     optimum  = objval,
                                     status   = out$status,
                                     solver   = solver,
                                     message  = out)
}


solve_varmetric <- function(x, control = list()) {
    solver <- "nloptr.varmetric"

    if ( is.null(control$x0) )
        stop("argument 'start' is missing with no default")

    use_rank2 <- isTRUE(control$rank2) | is.null(control$rank2) 
    algo <- if (use_rank2) "NLOPT_LD_VAR2" else "NLOPT_LD_VAR1"
    defaults <- list(algorithm = algo)

    m <- nlopt_problem(x, start = control$x0, derivate_free = FALSE)
    m$opts <- nlopt_options(control, defaults)

    if ( !is.null(control$args) )
        m <- c(m, control$args)

    mode(m) <- "call"

    if (isTRUE(control$dry_run)) 
        return(m)
    
    out <- eval(m)
    objval <- tryCatch(objective(x)(out$solution), error = function(e) NA_real_)

    ROI_plugin_canonicalize_solution(solution = out$solution,
                                     optimum  = objval,
                                     status   = out$status,
                                     solver   = solver,
                                     message  = out)
}


## NLOPT Algorithmen
## =================
## These constants are mostly of the form NLOPT_{G,L}{N,D}_xxxx, where 
## G/L denotes global/local optimization and 
## N/D denotes derivative-free/gradient-based algorithms, respectively.
## GN ... global derivate-free
## LN ... local derivate-free
## NLOPT_GN_DIRECT
## NLOPT_GN_DIRECT_L
## NLOPT_GN_DIRECT_L_RAND
## NLOPT_GN_DIRECT_NOSCAL
## NLOPT_GN_DIRECT_L_NOSCAL
## NLOPT_GN_DIRECT_L_RAND_NOSCAL
## NLOPT_GN_ORIG_DIRECT
## NLOPT_GN_ORIG_DIRECT_L
## NLOPT_GD_STOGO
## NLOPT_GD_STOGO_RAND
## NLOPT_LD_SLSQP
## NLOPT_LD_LBFGS_NOCEDAL
## NLOPT_LD_LBFGS
## NLOPT_LN_PRAXIS
## NLOPT_LD_VAR1
## NLOPT_LD_VAR2
## NLOPT_LD_TNEWTON
## NLOPT_LD_TNEWTON_RESTART
## NLOPT_LD_TNEWTON_PRECOND
## NLOPT_LD_TNEWTON_PRECOND_RESTART
## NLOPT_GN_CRS2_LM
## NLOPT_GN_MLSL
## NLOPT_GD_MLSL
## NLOPT_GN_MLSL_LDS
## NLOPT_GD_MLSL_LDS
## NLOPT_LD_MMA
## NLOPT_LN_COBYLA
## NLOPT_LN_NEWUOA
## NLOPT_LN_NEWUOA_BOUND
## NLOPT_LN_NELDERMEAD
## NLOPT_LN_SBPLX
## NLOPT_LN_AUGLAG
## NLOPT_LD_AUGLAG
## NLOPT_LN_AUGLAG_EQ
## NLOPT_LD_AUGLAG_EQ
## NLOPT_LN_BOBYQA
## NLOPT_GN_ISRES

