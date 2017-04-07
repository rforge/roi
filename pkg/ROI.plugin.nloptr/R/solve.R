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

build_options <- function(control) {
    opts <- control[!names(control) %in% c("gradient", "nl.info", "x0", "args")]
    
    if ( is.null(opts$xtol_rel) ) 
        opts[['xtol_rel']] <- nloptr_defaults('xtol_rel')

    if ( is.null(opts$tol_constraints_ineq) ) 
        opts[['tol_constraints_ineq']] <- nloptr_defaults("tol_constraints_ineq")

    if ( is.null(opts$tol_constraints_eq) ) 
        opts[['tol_constraints_eq']] <- nloptr_defaults("tol_constraints_eq")

    opts
}

solve_nloptr <- function( x, control) {

    args <- list()
    args$call_fun <- nloptr::nloptr
    args$x0 <- control$x0 ## FIXME

    ## objective function
    if ( isTRUE(x$maximum) ) {
        objective_function <- objective(x)
        args$eval_f <- function(x) -objective_function(x)
    } else {
        args$eval_f <- objective(x)
    }

    ## gradient of the objective function
    if ( !is_derivate_free_algorithm(control$algorithm) ) {
        if ( isTRUE(x$maximum) ) {
            gradient_function <- G(objective(x))
            args$eval_grad_f <- function(x) -gradient_function(x)
        } else {
            args$eval_grad_f <- G(objective(x))
        }
    }

    ## lower and upper bounds
    args$lb <- get_lb(x)
    args$ub <- get_ub(x)

    ## inequality constraints
    inequality_constraint <- ROI_plugin_build_inequality_constraints(x, type="leq_zero")
    args$eval_g_ineq <- inequality_constraint[["F"]]

    ## jacobian inequality constraints
    args$eval_jac_g_ineq <- inequality_constraint[["J"]]
  
    ## equality constraints
    equality_constraint <- ROI_plugin_build_equality_constraints(x, type = "eq_zero")
    args$eval_g_eq <- equality_constraint[["F"]]

    ## jacobian equality constraints
    args$eval_jac_g_eq <- equality_constraint[["J"]]

    ## nloptr control (opts)
    opts <- build_options(control)
    args$opts <- opts

    ## ... args
    args <- c(args, control$args)

    mode(args) <- "call"

    if ( isTRUE(control$dry_run) )
        return(args)

    for ( i in seq_len(3) ) {
        ## NLOPTR has some unfixed issues!
        capture.output(res <- try(eval(args), silent=TRUE))
        if ( class(res) != "try-error") {
            check_ineq <- check_eval_g_ineq(inequality_constraint[["F"]], 
                                            res$solution, opts[["tol_constraints_ineq"]])
            check_eq <- check_eval_g_eq(equality_constraint[["F"]], 
                                        res$solution, opts[["tol_constraints_eq"]])
            if ( check_ineq & check_eq ) {
                break
            }
        }
    }
    
    ## TODO: check the conditions since nloptr sometimes gives
    ##       results which violate the constraints
    if (class(res) == "try-error") {
        stop(attr(res, 'condition')[["message"]])
    }
    
    ROI_plugin_canonicalize_solution( solution  = res$solution,
                                      optimum   = objective(x)(res$solution),
                                      status    = res$status,
                                      solver    = "nloptr",
                                      message   = res,
                                      algorithm = control$algorithm )

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

