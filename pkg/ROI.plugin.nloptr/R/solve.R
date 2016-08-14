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

nloptr_get_default <- function(x) {
    nloptr.get.default.options()[nloptr.get.default.options()[,'name'] == x, 'default']
}

## get_algorithms() returns an overview
## get_algorithms(T) returns vector with global algorithms
## get_algorithms(T) returns vector with local algorithms
## get_algorithms(NULL, T) returns vector with derivative algorithms
## get_algorithms(NULL, F) returns vector with no derivative algorithms
get_algorithms <- function(global=NULL, derivatives=NULL) {
    d <- nloptr.get.default.options()
    rownames(d) <- d[,"name"]
    a <- unlist(strsplit(d["algorithm", "possible_values"], ",\\s*"))
    algo <- data.frame(algorithm=a, stringsAsFactors = FALSE)
    ## first is Global "G" vs Local "L"
    algo$global <- substr(a, 7, 7) == "G"
    ## secound Derivate "D" vs No Derivate "N"
    algo$derivatives <- substr(a, 8, 8) == "D"
    if ( is.null(global) & is.null(derivatives) )
        return( algo )
    if ( is.null(global) )
        return( algo[algo$derivatives == derivatives, 1] )
    if ( is.null(derivatives) )
        return( algo[algo$global == global, 1] )
    algo[( (algo$derivatives == derivatives) & (algo$global == global) ), 1]
}

get_algo_properties <- function() {
    get_algorithms()
}

is_derivate_free_algorithm <- function(x) {
    any(grepl("(_GN_|_LN_)", x))
}

## nloptr
## ======
##
## R interface to NLopt
##
## nloptr(x0, eval_f, eval_grad_f = NULL, lb = NULL, ub = NULL,
##        eval_g_ineq = NULL, eval_jac_g_ineq = NULL, eval_g_eq = NULL,
##        eval_jac_g_eq = NULL, opts = list(), ...)
## if(FALSE) {
##     library(nloptr)
##     attach(getNamespace("ROI.plugin.nloptr"))    
## }

solve_nloptr <- function( x, control ) {
    solver <- .ROI_plugin_get_solver_name( getPackageName() )
    lb <- get_lb(x)
    ub <- get_ub(x)

    objective_fun <- objective(x)
    if ( is.null(control$algorithm) ) { 
        control$algorithm <- "NLOPT_LD_AUGLAG_EQ"
    }
    if ( !is_derivate_free_algorithm(control$algorithm) ) {
        gradient_fun <- G(objective(x))
    } else {
        gradient_fun <-  NULL
    }
    x$constraints <- as.F_constraint(x$constraints)

    if ( x$maximum ) {
        OBJ_FUN <- objective_fun
        objective_fun <- function(x) -(OBJ_FUN(x))
        if ( !is.null(gradient_fun) ) {
            GRAD_FUN <- gradient_fun
            gradient_fun <- function(x) -(GRAD_FUN(x))
        }
    }

    if ( is.null(control$x0) & is.null(control$start) ) {
        stop("no start value, please provide a start value")
    } else if ( is.null(control$x0) ) {
        control$x0 <- control$start
    }
    j <- na.exclude(match(c("gradient", "nl.info", "x0", "args"), names(control)))
    if ( is.null(control$xtol_rel) ) control[['xtol_rel']] <- nloptr_defaults('xtol_rel')
    if ( is.null(control$tol_constraints_ineq) ) 
        control[['tol_constraints_ineq']] <- nloptr_defaults("tol_constraints_ineq")
    if ( is.null(control$tol_constraints_eq) ) 
        control[['tol_constraints_eq']] <- nloptr_defaults("tol_constraints_eq")

    eval_g_ineq <- build_inequality_constraints(x, control$tol_constraints_ineq)
    eval_jac_g_ineq <- build_jacobian_inequality_constraints(x, control$tol_constraints_ineq)
    eval_g_eq <- build_equality_constraints(x, control$tol_constraints_eq)
    eval_jac_g_eq <- build_jacobian_equality_constraints(x, control$tol_constraints_eq)

    for (i in seq_len(3)) {
        capture.output(
        o <- try(nloptr(x0 = control$x0, eval_f = objective_fun, eval_grad_f = gradient_fun,
                        lb = lb, ub = ub, eval_g_ineq = eval_g_ineq, eval_jac_g_ineq = eval_jac_g_ineq,
                        eval_g_eq = eval_g_eq, eval_jac_g_eq = eval_jac_g_eq, opts = control[-j] ), silent=TRUE)
        )
        if ( (class(o) != "try-error") & 
             check_eval_g_ineq(eval_g_ineq, o$solution, control$tol_constraints_ineq) & 
             check_eval_g_eq(eval_g_eq, o$solution, control$tol_constraints_eq) ) {
            break
        } 
    }
    if (class(o) == "try-error") {
        stop(attr(o, 'condition')[["message"]])
    }

    optimum <- (-1)^x$maximum * o$objective

    .ROI_plugin_canonicalize_solution(  solution  = o$solution,
                                        optimum   = optimum,
                                        status    = o$status,
                                        solver    = solver,
                                        message   = o,
                                        algorithm = control$algorithm   )
}

check_eval_g_ineq <- function(eval_g_ineq, sol, tol) {
    if ( is.null(eval_g_ineq) )
        return(TRUE)
    return( all(eval_g_ineq(sol) <= tol) )
}

check_eval_g_eq <- function(eval_g_eq, sol, tol) {
    if ( is.null(eval_g_eq) )
        return(TRUE)
    ## difference to check_eval_g_ineq is only the abs
    return( all(abs(eval_g_eq(sol)) <= tol) )
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

