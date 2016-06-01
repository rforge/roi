## ROI plugin: nlminb
## based on MySolver Template
## DISABLED - need to find literature first


################################################################################
## Utility Functions
################################################################################

## get_lb, get_ub taken from Florians nloptr plugins
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



################################################################################
## SOLVER METHODS
################################################################################

.solve_NLP_nlminb <- function( x, control ) {

    ## since this is contributed with ROI we have to specify the solver name directly
    solver <- "nlminb"

    ## solve the NLP
    ## adjust arguments depending on problem class

    ## from nlminb2NLP() by Diethelm Wuertz

    ## Set Box Constraints:
    ## FIXME: use as.no_V_bound() instead?
    lb <- get_lb(x)
    ub <- get_ub(x)

    env <- parent.frame()

    FC <- constraints(x)$F
    dir <- constraints(x)$dir
    rhs <- constraints(x)$rhs

    idx_eq <- dir == "=="
    if( length(dir) > sum(idx_eq) )
        warning( "only equality constraints supported with nlminb, ignoring others." )

    # Set Linear and Function Equality Constraints:
    if ( any(idx_eq) ) {
        eqfun <- function(x){
            ans <- double(sum(idx_eq))
            for (i in 1:sum(idx_eq))
                    ans[i] <- FC[idx_eq][[i]](x) - rhs[i]
            return(as.double(eval(ans, env))) }
    } else {
        eqfun <- NULL
    }

    ## # Set Linear and Function Inequality Constraints:
    ## if (!is.null(ineqA) || length(ineqFun) > 0) {
    ##     leqfun <- function(x) {
    ##         ans <- NULL
    ##         if(!is.null(ineqA))
    ##             ans <- c(ans, +ineqA %*% x - ineqA.upper)
    ##         if(!is.null(ineqA))
    ##             ans <- c(ans, -ineqA %*% x + ineqA.lower)
    ##         if (length(ineqFun) > 0)
    ##             for (i in 1:length(ineqFun))
    ##                 ans <- c(ans, +ineqFun[[i]](x) - ineqFun.upper[i])
    ##         if (length(ineqFun) > 0)
    ##             for (i in 1:length(ineqFun))
    ##                 ans <- c(ans, -ineqFun[[i]](x) + ineqFun.lower[i])
    ##         return(as.double(eval(ans, env))) }
    ## } else {
        leqfun <- NULL
    ## }

    ## now run nlminb2 solver
    out <- ROI:::nlminb2( start = control$start,
                    objective = objective(x),
                    eqFun = eqfun,
                    leqFun = NULL, #FIXME: leqfun,
                    upper = ub,
                    lower = lb,
                    gradient = G(objective(x)),
                    hessian = control$hessian,
                    control = control )
    ## .ROI_plugin_canonicalize_solution( solution = out$solution,
    ##                                    optimum = objective(x)(out$solution),
    ##                                    status = out$convergence,
    ##                                    solver = solver,
    ##                                    message = out )
out
}


################################################################################
## NOTE: this is work by Diethelm Wuertz taken out of the Rnlminb2
## package available at R-Forge:
## https://r-forge.r-project.org/scm/viewvc.php/pkg/Rnlminb2/
################################################################################

################################################################################
# FUNCTION:                DESCRIPTION:
#  nlminb2                  Nonlinear programming with nonlinear constraints
#  .Log                     Returns log taking care of negative values
################################################################################


##' Nonlinear programming with nonlinear constraints.
##'
##' This function was contributed by Diethelm Wuertz.
##' @param start numeric vector of start values.
##' @param objective the function to be minimized \eqn{f(x)}.
##' @param eqFun functions specifying equal constraints of the form
##' \eqn{h_i(x) = 0}. Default: \code{NULL} (no equal constraints).
##' @param leqFun functions specifying less equal constraints of the
##' form \eqn{g_i(x) <= 0}. Default: \code{NULL} (no less equal
##' constraints).
##' @param lower a numeric representing lower variable
##' bounds. Repeated as needed. Default: \code{-Inf}.
##' @param upper a numeric representing upper variable
##' bounds. Repeated as needed. Default: \code{Inf}.
##' @param gradient gradient of \eqn{f(x)}. Default: \code{NULL} (no
##' gradiant information).
##' @param hessian hessian of \eqn{f(x)}. Default: \code{NULL} (no
##' hessian provided).
##' @param control a list of control parameters. See
##' \code{\link[stats]{nlminb}()} for details. The parameter
##' \code{"scale"} is set here in contrast to
##' \code{\link[stats]{nlminb}()} .
##' @author Diethelm Wuertz
##' @examples
##' ## Equal constraint function
##' eval_g0_eq <- function( x, params = c(1,1,-1)) {
##'        return( params[1]*x^2 + params[2]*x + params[3] )
##'    }
##' eval_f0 <- function( x, ... ) {
##'        return( 1 )
##'    }
##'
##'
##' @return list()
nlminb2 <- function( start, objective, eqFun = NULL, leqFun = NULL,
                     lower = -Inf, upper = Inf, gradient = NULL,
                    hessian = NULL, control = list() ) {

    ## Details:
    ##                        min f(x)
    ##    s.t.
    ##                 lower_i < x_i < upper_i
    ##                       h_i(x)  = 0
    ##                       g_i(x) <= 0

    # Arguments:
    #   start - numeric vector of start values
    #   objective - objective function to be minimized f(x)
    #   eqFun - equal constraint functions h_i(x) = 0
    #   leqFun - less equal constraint functions g_i(x) <= 0
    #   lower, upper - lower and upper bounds
    #   gradient - optional gradient of f(x)
    #   hessian - optional hessian of f(x)
    #   scale - control parameter
    #   control - control list
    #       eval.max - maximum number of evaluations (200)
    #       iter.max - maximum number of iterations (150)
    #       trace - value of the objective function and the parameters
    #           is printed every trace'th iteration (0)
    #       abs.tol - absolute tolerance (1e-20)
    #       rel.tol - relative tolerance (1e-10)
    #       x.tol - X tolerance (1.5e-8)
    #       step.min - minimum step size (2.2e-14)

    ## TODO:  R, N and alpha should become part of the control list.

    ## FIXME: is it possible to set default start values (like 1/n)
    stopifnot( is.numeric(start) )

    ## Control list:
    ctrl <- .make_nlminb2_control_defaults()
    if( length(control) > 0 )
        for( name in names(control) )
            ctrl[[ name ]] <- control[[ name ]]
    control <- ctrl

    # Minimization:
    steps.tol <- control$steps.tol
    R <- control$R
    beta <- control$beta
    scale <- control$scale

    # Trace:
    TRACE <- control$trace > 0

    # Reset Control:
    control2 <- control
    control2[["R"]] <- NULL
    control2[["beta"]] <- NULL
    control2[["steps.max"]] <- NULL
    control2[["steps.tol"]] <- NULL
    control2[["scale"]] <- NULL

    ## Unconstrained problem (inserted by st):
    is_unconstrained <- is.null(eqFun) && is.null(leqFun)

    if( is_unconstrained ){
        ans <- nlminb( start = start, objective = objective,
                       gradient = gradient, hessian = hessian,
                       scale = scale, control = control2,
                       lower = lower, upper = upper )
    } else {
        ##
        ## Composed Objective Function:
        if( !is.null(gradient) ){
            ## FIXME: we could use default ROI option gradient?
            gradient <- NULL
            warning( "gradient not recognized in constrained NLPs for solver 'nlminb'." )
        }
        gradient <- NULL
        if( is.null(eqFun) ){
            ## type: "leq"
            fun <- function( x, r ){
                objective( x ) - r * sum( .Log(-leqFun(x)) ) }
        } else if( is.null(leqFun) ){
            ## type: "eq"
            fun <- function( x, r ){
                objective( x ) + sum( (eqFun(x))^2 / r ) }
        } else {
            ## type: "both"
            fun <- function( x, r ){
                objective( x ) + sum( (eqFun(x))^2 / r ) - r * sum( .Log(-leqFun(x)) ) }
        }

        counts <- 0
        test <- 0
        while (counts < control$steps.max && test == 0) {
            counts <- counts + 1
            ans <- nlminb(
                start = start, objective = fun,
                gradient = gradient, hessian = hessian,
                scale = scale, control = control2, lower = lower, upper = upper,
                r = R )
            start <- ans$par
            tol <- abs((fun(ans$par, R) - objective(ans$par))/objective(ans$par))
            if (!is.na(tol))
                if (tol < steps.tol) test <- 1
            if (TRACE) {
                cat("Iterations:  ", counts)
                cat("\nConvergence: ", ans$convergence)
                cat("\nMessage:     ", ans$message)
                cat("\nSolution:    ", ans$par)
                cat("\nR-Objective: ", fun(start, R))
                cat("\nFunction-Obj:", objective(start))
                cat("\n")
            }
            R <- beta * R
        }
    }
    # Return Value (plain return of nlminb()):
    ans
}

##' Returns default control list for the nlminb solver.
##'
##' A function contributed by Diethelm Wuertz.
##' @return a list of control parameters.
##' ##' @noRd
.make_nlminb2_control_defaults <- function()
    list( eval.max = 500,
          iter.max = 400,
          trace = 0,
          abs.tol =  1.0e-20,
          rel.tol = 1.0e-10,
          x.tol = 1.5e-8,
          step.min = 2.2e-14,
          scale = 1,
          R = 1,
          beta = 0.01,
         steps.max = 10,
         steps.tol = 1e-6 )

##' Returns log taking care of negative values.
##' @param x a numeric from which to calculate the log.
##' @return a numeric.
##' @noRd
.Log <-
function(x)
{
    # Check for negative values:
    x[x < 0] <- 0

    # Return Value:
    log(x)
}
# ------------------------------------------------------------------------------



################################################################################
## FIXME: we need for each problem class a separate solver method
## the following QP function is currently defunct.
.solve_QP_nlminb <- function( x, control ) {
    ## if needed, add constraints made from variable bounds
    ##if( length(bounds(x)) )
    ##  constraints(x) <- rbind(constraints(x),
    ##                         .make_box_constraints_from_bounds(bounds(x),
    ##                                     dim(terms(objective(x))$Q)[1]) )

    solver <- "nlminb"
    ## solve the QP
    ## adjust arguments depending on problem class
    out <- .nlminb_solve_QP( Q = terms(objective(x))$Q,
                             L = terms(objective(x))$L,
                             mat = constraints(x)$L,
                             dir = constraints(x)$dir,
                             rhs = constraints(x)$rhs,
                             bounds = bounds(x),
                             max = x$maximum,
                             gradient = G(objective(x)),
                             #hessian = control$hessian,
                             control = control )
    .ROI_plugin_canonicalize_solution( solution = out$solution,
                                       optimum = objective(x)(out$solution),
                                       status = out$convergence,
                                       solver = solver,
                                       message = out )
}

.nlminb_solve_QP <- function(Q, L, mat, dir, rhs, bounds, max, gradient, control = list()) {

    # nlminb does not directly support constraints
    # we need to translate Ax ~ b constraints to lower, upper bounds
    ## FIXME: what about variable bounds????
    stopifnot( is.null(bounds) )

    A <- solve(t(mat))
    n_obj <- ifelse( !is.null(Q),
                     dim(Q)[1],
                     length(L) )

    ## start
    start <- as.numeric( control$start )
    if( !length(start) )
        start <- slam::tcrossprod_simple_triplet_matrix( mat, matrix(rep(1/n_obj, n_obj), nrow = 1))
    stopifnot( length(start) == n_obj )

    lower <- rhs
    upper <- c(Inf, Inf, Inf)

    ## possibly transformed objective function
    foo <- function(x, L, A, Q) {
        X = A %*% x
        Objective = - slam::tcrossprod_simple_triplet_matrix(L, t(X)) + 0.5 * ( t(X) %*% slam::tcrossprod_simple_triplet_matrix(Q, t(X)))
        Objective[[1]]
    }
    ## FIXME: SPARSE!!! control list handling ok? what about "scale" parameter?
    out <- nlminb(start, foo, gradient = gradient, hessian = control$hessian,
                  L = L, A = A, Q = Q,
                  control = control, lower = lower, upper = upper)
    out$solution <- as.numeric(A %*% out$par)

    # Return Value:
    out
}
################################################################################



################################################################################
## STATUS CODES
################################################################################

.add_nlminb_status_codes <- function(){
    ## add all status codes generated by the solver to db

    ## Two examples are listed here:
    .ROI_plugin_add_status_code_to_db("nlminb",
                                      0L,
                                      "CONVERGENCE",
                                      "Solution is optimal",
                                      0L
                                      )
    .ROI_plugin_add_status_code_to_db("nlminb",
                                      1L,
                                      "NON_CONVERGENCE",
                                      "No solution."
                                      )
    invisible(TRUE)
}

