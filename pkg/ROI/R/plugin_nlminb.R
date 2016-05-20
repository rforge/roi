## ROI plugin: nlminb
## based on MySolver Template
## DISABLED - need to find literature first

## SOLVER METHODS

## we need for each problem class a separate solver method

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
                             control = control )
    .ROI_plugin_canonicalize_solution( solution = out$solution,
                           optimum = objective(x)(out$solution),
                           status = out$convergence,
                           solver = solver )
}

.nlminb_solve_QP <- function(Q, L, mat, dir, rhs, bounds, max, control = list()) {

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
    out <- nlminb(start, foo, gradient = control$gradient, hessian = control$hessian,
                  L = L, A = A, Q = Q,
                  control = control, lower = lower, upper = upper)
    out$solution <- as.numeric(A %*% out$par)

    # Return Value:
    out
}



.solve_NLP_nlminb <- function( x, control ) {

    solver <- "nlminb"
    ## solve the NLP
    ## adjust arguments depending on problem class

    ## from nlminb2NLP() by Diethelm Wuertz

    ## Set Box Constraints:
    if( !length(bounds(x)$lower$val) ){
        lb <- 0
    } else {
        lb <- numeric(length(objective( x )))
        lb[ bounds(x)$lower$ind ] <- bounds(x)$lower$val
    }
    if( !length(bounds(x)$upper$val) ){
        ub <- Inf
    } else {
        ub <- numeric(length(objective( x )))
        ub[ bounds(x)$upper$ind ] <- bounds(x)$upper$val
    }

    FC <- constraints(x)$F
    dir <- constraints(x)$dir

    # Set Linear and Function Equality Constraints:
    if ( any(dir == "==") ) {
        eqfun <- function(x){

            for (i in 1:length(eqFun))
                    ans <- c(ans, eqFun[[i]](x) - eqFun.bound[i])
            return(as.double(eval(ans, env))) }
    } else {
        eqfun <- NULL
    }

    # Set Linear and Function Inequality Constraints:
    if (!is.null(ineqA) || length(ineqFun) > 0) {
        leqfun <- function(x) {
            ans <- NULL
            if(!is.null(ineqA))
                ans <- c(ans, +ineqA %*% x - ineqA.upper)
            if(!is.null(ineqA))
                ans <- c(ans, -ineqA %*% x + ineqA.lower)
            if (length(ineqFun) > 0)
                for (i in 1:length(ineqFun))
                    ans <- c(ans, +ineqFun[[i]](x) - ineqFun.upper[i])
            if (length(ineqFun) > 0)
                for (i in 1:length(ineqFun))
                    ans <- c(ans, -ineqFun[[i]](x) + ineqFun.lower[i])
            return(as.double(eval(ans, env))) }
    } else {
        leqfun <- NULL
    }

    ## now run nlminb2 solver
    out <- nlminb2( start = control$start,
                    objective = objective(x),
                    eqFun = eqfun,
                    leqFun = leqfun,
                    upper = ub,
                    lower = lb,
                    gradient = control$gradiant,
                    hessian = control$hessian,
                    control = control )
    .ROI_plugin_canonicalize_solution( solution = out$solution,
                           optimum = objective(x)(out$solution),
                           status = out$convergence,
                           solver = solver )
}


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

    ## TODO:  R, N and alpha should become part of the control list.

    ## Control list:
    ctrl <- .make_nlminb2_control_defaults()
    if( length(control) > 0 )
        for( name in names(control) )
            ctrl[[ name ]] <- control[[ name ]]
    control <- ctrl

    ## Composed Objective Function:
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

    # Minimization:
    R <- control$R
    beta.tol <- control$beta.tol
    beta.step <- control$beta.step
    beta <- ( beta.tol )^( 1/beta.step )
    scale <- control$scale

    # Reset Control:
    control.nlminb <- control
    control.nlminb$R <- NULL
    control.nlminb$beta.tol <- NULL
    control.nlminb$beta.step <- NULL

    # Trace:
    trace <- control$trace
    if( trace > 0 )
        TRACE <- TRUE
    else TRACE <- FALSE

    iterations <- Inf
    nIterations <- 0
    for( i in 1:beta.step ){
        if( iterations > 1 ){
            if( TRACE )
                cat( "\n\nIteration step:", i, "  R:", R, "\n" )
            ans <- nlminb( start = start, objective = fun,
                           gradient = gradient, hessian = hessian,
                           scale = scale, control = control.nlminb,
                           lower = lower, upper = upper, r = R)
            start <- ans$par
            iterations <- ans$iterations
            if( TRACE ) {
                cat("Iterations:", ans$iterations)
                cat("\nConvergence:", ans$convergence)
                cat("\n Message:", ans$message)
                cat("\nSolution:", ans$par)
                cat("\nR - Objective:       ", fun(start, R))
                cat("\nFunction - Objective:", objective(start))
                cat("\n")
            }
            R <- beta * R
            nIterations <- nIterations + iterations
            ans$iterations <- nIterations
        }
    }

    # Return value (plain return of nlminb())
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
          beta.tol = 1.0e-20,
          beta.step = 20 )


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


################################################################################





## STATUS CODES

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

