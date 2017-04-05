################################################################################
## Package: ROI
## File:    plugin.R
## Author:  Stefan Theussl
## Changed: 2016-05-27
################################################################################



################################################################################
## REGISTER SOLVER CONTROLS
################################################################################

##  -----------------------------------------------------------
##  ROI_plugin_register_solver_control
##  ==========================
##' @title Register Solver Controls
##'
##' @description Register a new solver control argument.
##' @param solver a character string giving the solver name.
##' @param args a character vector specifying with the supported signatures.
##' @param roi_control a character vector specifying the corresponding ROI control argument.
##' @return TRUE on success
##' @family plugin functions
##' @rdname ROI_plugin_register_solver_control
##' @export
ROI_plugin_register_solver_control <- function( solver, args, roi_control = "X" ){
    args <- as.character( args )
    if( length(roi_control) == 1L )
        roi_control <- rep( as.character(roi_control), length(args) )
    stopifnot( length(args) == length(roi_control) )
    for( i in seq_along(args) )
        control_db$set_entry( solver, args[i], roi_control[i] )
    invisible( TRUE )
}

ROI_available_solver_controls <- function(){
    c( "X",                ## no corresponding ROI control
       "verbose",          ## LOGICAL: turn on/off solver output on terminal
       "presolve",         ## LOGICAL: turn on/off presolve
       "verbosity_level",  ## INTEGER: level of output
       "max_iter",         ## INTEGER: maximum number of iterations
       "max_time",         ## INTEGER: maximum time spent solving the problem in milliseconds before abort is triggered
       "tol",              ## NUMERIC: tolerance of termination criterion
       "abs_tol",          ## NUMERIC: absolute tolerance of termination criterion
       "rel_tol",          ## NUMERIC: relative tolerance of termination criterion
       "method",           ## CHARACTER: giving the algorithm
       "start",            ## NUMERIC: a numeric vector giving the start values
       "nsol"              ## INTEGER: a integer giving the maximum number of solutions
      )
}

get_solver_controls_from_db <- function( solver ){
    sapply( control_db$get_entries(solver), function(x) x[[ "control" ]] )
}

ROI_translate <- function( control, solver ){
    trans <- sapply( control_db$get_entries(solver), function( x ) x[[ "roi_control" ]] )
    idx_trans <- names( control ) %in% trans
    if( any(idx_trans) )
        names( control )[idx_trans] <- sapply( names( control )[idx_trans], function(item) control_db$get_entries(solver)[ item == trans  ][[1]]$control )
    control
}


################################################################################
## REGISTER SOLVER STATUS CODES
################################################################################

## ROI: status_codes.R
## overview of solver status codes and their canonicalization

##  adds a new status code to db, default roi_code is 1L, i.e. a failure
##
##  -----------------------------------------------------------
##  add_status_code_to_db
##  =====================
##' @title Add Status Code to the Status Database
##'
##' @description Add a status code to the status database.
##' @param solver a character string giving the name of the solver.
##' @param code an integer giving the status code of the solver.
##' @param symbol a character string giving the status symbol.
##' @param message a character string used as status message.
##' @param roi_code an integer giving the ROI status code, 1L for failure and 0L for success.
##' @return NULL
##' @examples
##' \dontrun{
##' solver <- "ecos"
##' ROI_plugin_add_status_code_to_db(solver, 0L, "ECOS_OPTIMAL", "Optimal solution found.", 0L)
##' ROI_plugin_add_status_code_to_db(solver, -7L, "ECOS_FATAL", "Unknown problem in solver.", 1L)
##' solver <- "glpk"
##' ROI_plugin_add_status_code_to_db(solver, 5L, "GLP_OPT", "Solution is optimal.", 0L)
##' ROI_plugin_add_status_code_to_db(solver, 1L, "GLP_UNDEF", "Solution is undefined.", 1L)
##' }
##' @family plugin functions
##' @rdname ROI_plugin_add_status_code_to_db
##' @export
ROI_plugin_add_status_code_to_db <- function(solver, code, symbol, message, roi_code = 1L){
    status_db$set_entry(solver = solver,
                        code = code,
                        symbol = symbol,
                        message = message,
                        roi_code = roi_code)
    ## return NULL else it returns the registry as list
    invisible(NULL)
}

get_status_message_from_db <- function(solver, code){
    status_db[[solver, code]]
}

delete_status_code_from_db <- function(solver, code){
  status_db$delete_entry(solver = solver,
                         code = code)
}

available_in_status_codes_db <- function( )
  unique( status_db$get_field_entries("solver") )



################################################################################
## REGISTER NEW SOLVER METHODS
################################################################################

##  -----------------------------------------------------------
##  ROI_plugin_register_solver_method
##  ==========================
##' @title Register Solver Method
##'
##' @description Register a new solver method.
##' @param signatures a data.frame with the supported signatures.
##' @param solver a character string giving the solver name.
##' @param method a function registered as solver method.
##' @return TRUE on success
##' @family plugin functions
##' @rdname ROI_plugin_register_solver_method
##' @export
ROI_plugin_register_solver_method <- function( signatures, solver, method ){
    for( i in 1:nrow(signatures) )
        do.call(solver_db$set_entry, c(as.list(signatures[i,]),
                                             list(solver = solver),
                                             list(FUN = method)))

    invisible( TRUE )
}



################################################################################
## SIGNATURES
################################################################################

ROI_required_signature <- function()
    c("objective", "constraints", "types", "bounds", "cones", "maximum")

##' Create a solver signature, the solver signatures are used to indicate
##' which problem types can be solved by a given solver.
##'
##' @title Make Signatures
##' @param ... signature definitions
##' @return a data.frame with the supported signatures
##' @examples
##' ## ROI_make_LP_signatures
##' lp_signature <- ROI_plugin_make_signature( objective = "L",
##'                                            constraints = "L",
##'                                            types = c("C"),
##'                                            bounds = c("X", "V"),
##'                                            cones = c("X"),
##'                                            maximum = c(TRUE, FALSE) )
##' @family plugin functions
##' @rdname ROI_plugin_make_signature
##' @export
ROI_plugin_make_signature <- function(...){
    dotargs <- list(...)
    required <- ROI_required_signature() ## names(formals(OP))
    if( length(dotargs) < 2 )
        stop( sprintf("Signature element for '%s' and '%s' need to be given.",
                      required[1], required[2]) )
    length(dotargs) <- length(required)
    if( is.null(names(dotargs)) ) {
        names(dotargs) <- required
    } else {
        nam <- names(dotargs)
        nam[nam == ""] <- required[!(required %in% nam)]
        names(dotargs) <- nam
    }
    stopifnot( all(names(dotargs) %in% required) )

    signature_default <- list(objective="L", constraints="L", types="C", bounds="C",
                              cones="X", maximum=FALSE)
    set_defaults <- function(name, x) if (is.null(x)) signature_default[[name]] else x
    dotargs <- mapply(set_defaults, names(dotargs), dotargs, SIMPLIFY=FALSE)

    dotargs <- lapply(dotargs, function(x) if( is.null(x) ) FALSE else x)
    .make_signature(do.call(ROI_expand, dotargs))
}

.make_signature <- function( x ){
    required <- ROI_required_signature()
    m <- match(colnames(x), required)
    x <- x[,m]
    if ( !identical(colnames(x), required) ) {
        ## hint <- "It seems, the signature is missing the entries or has to much entries!"
        hint1 <- sprintf("Required entries are: '%s'", paste(required, collapse="', '"))
        hint2 <- sprintf("Given entries are: '%s'", paste(colnames(x), collapse="', '"))
        hint <- sprintf("%s\n\t%s", hint1, hint2)
        msg <- "The signature doesn't match the required signature!"
        msg <- sprintf("%s\n\t%s", msg, hint)
        error( "MISSPECIFIED_SIGNATURE", msg, ".make_signature", call=NULL )
    }
    types <- strsplit(as.character(x[["types"]]), "")
    types <- do.call(rbind, lapply( types, function(t) available_types() %in% t) )
    colnames(types) <- available_types()
    cbind(x[, colnames(x) != "types"], types)
}



################################################################################
## Plug-in and solver naming
################################################################################

## returns solver name based on package name
## Convention: ROI.plugin.<solver> => <solver>
.plugin_prefix <- function()
    "ROI.plugin"

## NOTE: all plugin related functions must be prefixed with "ROI_plugin_" and
##       exported.

##  -----------------------------------------------------------
##  get_solver_name
##  ===============
##' @title Get Solver Name
##
##' @description Get the name of the solver plugin.
##' @param pkgname a string giving the package name.
##' @return Returns the name of the solver as character.
##' @family plugin functions
##' @rdname ROI_plugin_get_solver_name
##' @export
ROI_plugin_get_solver_name <- function( pkgname )
    sub(sprintf("%s.", .plugin_prefix()), "", as.character(pkgname))



################################################################################
## CANONICALIZER
################################################################################

canonicalize_status <- function( status, solver ){
    msg <- get_status_message_from_db( solver, status )
    list( code = msg$roi_code, msg = msg )
}

##  -----------------------------------------------------------
##  Plug-in convenience function: canonicalize_solution
##  =====================
##' @title Canonicalize Solution
##'
##' @description Transform the solution to a standardized form.
##' @param solution a numeric or integer vector giving
##'        the solution of the optimization problem.
##' @param optimum a numeric giving the optimal value.
##' @param status an integer giving the status code (exit flag).
##' @param solver a character string giving the name of the solver.
##' @param message an optional \R object giving the original solver message.
##' @param ... further arguments to be stored in the solution object.
##' @return an object of class \code{"OP_solution"}.
##' @family plugin functions
##' @rdname ROI_plugin_canonicalize_solution
##' @export
ROI_plugin_canonicalize_solution <- function( solution, optimum, status, solver, message=NULL, ... ) {
    status <- canonicalize_status( status, solver )
    make_OP_solution( solution = solution,
                      objval   = optimum,
                      status   = status,
                      solver   = solver,
                      message  = message, ... )
}



##  -----------------------------------------------------------
##  build_equality_constraints
##  ==========================
##' @title Build Functional Equality Constraints
##
##' @description There exist different forms of functional equality constraints,
##'              this function transforms the form used in \pkg{ROI} into the
##'              forms commonly used by \R optimization solvers.
##' @param x an object of type \code{"OP"}.
##' @param type an character giving the type of the function to be returned,
##'        possible values are \code{"eq_zero"} or \code{"eq_rhs"}.
##'        For more information see Details.
##' @details There are two types of equality constraints commonly used in \R
##' \enumerate{
##' \item{\code{eq\_zero}:}{ \eqn{h(x) = 0} and}
##' \item{\code{eq\_rhs}:}{ \eqn{h(x) = rhs} .}
##' }
##' @note This function only intended for plugin authors.
##' @return Returns one function, which combines all the functional constraints.
##' @family plugin functions
##' @rdname ROI_plugin_build_equality_constraints
##' @export
ROI_plugin_build_equality_constraints <- function(x, type=c("eq_zero", "eq_rhs")) {
    stopifnot(type %in% c("eq_zero", "eq_rhs"))
    co <- as.F_constraint(constraints(x))
    if ( any(is.NO_constraint(co), is.null(co)) )
        return( list(F=NULL, J=NULL) )
    
    stopifnot( is.F_constraint(co) )
    x0 <- rep.int(0, length(objective(x)))
    b <- co$dir == "=="
    if ( !any(b) )
        return( list(F=NULL, J=NULL) )
    J <- if ( is.null(J) ) NULL else co$J[b]
    if ( isTRUE(type == "eq_rhs") )
        return( build_equality_constraints_rhs_x(co$F[b], J, x0) )
    return( build_equality_constraints_rhs_zero(co$F[b], J, co$rhs[b], x0) )
}

length_F_constraint <- function(F, x0) {
    sum(unlist(lapply(F, function(f) length(f(x0)))))
}

## h(x) == 0
## return value is h
build_equality_constraints_rhs_zero <- function(F, J, rhs, x0) {
    if ( isTRUE(length(F) == 1) ) {
        J <- if ( is.null(J) ) NULL else J[[1]]
        if ( all(rhs == 0) ) {
            return( list(F=F[[1]], J=J) )
        } else {
            return( list(F=function(x) (F[[1]](x) - rhs), J=J) )
        }
    }
    if ( is.null(J) ) {
        jaccobian_fun <- function(x) {
            do.call(rbind, lapply(J, function(f) f(x)))
        }    
    } else {
        jaccobian_fun <- NULL
    }
    if ( all( rhs == 0 ) ) {
        eq_fun <- function(x) {
            unlist(lapply(F, function(f) f(x)), FALSE, FALSE)
        }
        return( list(F=eq_fun, J=jaccobian_fun) )
    }
    ## NOTE: Since we allow single constraints to have length bigger than
    ##       1 we have to group them.
    ##       Grouped constraints are all obligated to have the same dir.
    F_len <- sapply(F, function(f) length(f(x0)))
    grhs <- mapply(seq, cumsum(c(1L, F_len[-length(F_len)])), cumsum(F_len), SIMPLIFY = FALSE)
    build_fun <- function(CFUN, rhs) {
        if ( all(rhs == 0) )
            return(CFUN)
        return( function(x) - rhs )
    }
    rhs <- lapply(grhs, function(i) rhs[i])
    F <- mapply(build_fun, F, rhs, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    eq_fun <- function(x) {
        unlist(lapply(F, function(f) f(x)), FALSE, FALSE)
    }
    return( list(F=eq_fun, J=jaccobian_fun) )
}

## h(x) == rhs
## return value is h
build_equality_constraints_rhs_x <- function(F, J, x0) {
    if ( isTRUE(length(F) == 1) )
        J <- if ( is.null(J) ) NULL else J[[1]]
        return( list(F=F[[1]], J=J) )
    eq_fun <- function(x) {
        unlist(lapply(F, function(f) f(x)), FALSE, FALSE)
    }
    if ( is.null(J) ) {
        jaccobian_fun <- function(x) {
            do.call(rbind, lapply(J, function(f) f(x)))
        }    
    } else {
        jaccobian_fun <- NULL
    }
    return( list(F=eq_fun, J=jaccobian_fun) )
}

## ## Build inequality constraints
## There are five types of inequality constraints,    
## 1. **leq_zero**
## $$ g(x) \leq 0 $$
## 2. **geq_zero**
## $$ g(x) \geq 0 $$
## 3. **leq_rhs**
## $$ g(x) \leq rhs $$    
## 4. **geq_rhs**
## $$ g(x) \geq rhs $$
## 5. **leq_geq_rhs**
## $$ lhs \leq g(x) \leq rhs $$
## Es gibt weniger relevante faelle.
## c("leq_zero", "geq_zero", "leq_rhs", "geq_rhs", "leq_geq_rhs")
##  -----------------------------------------------------------
##  build_inequality_constraints
##  ============================
##' @title Build Functional Inequality Constraints
##
##' @description There exist different forms of functional inequality constraints,
##'              this function transforms the form used in \pkg{ROI} into the
##'              forms commonly used by \R optimization solvers.
##' @param x an object of type \code{"OP"}.
##' @param type an character giving the type of the function to be returned,
##'        possible values are \code{"leq\_zero"} or \code{"geq\_zero"} or
##'        or \code{"leq\_rhs"} or \code{"geq\_rhs"} or \code{"leq\_geq\_rhs"}.
##'        For more information see Details.
##' @details There are three types of inequality constraints commonly used in \R
##' \enumerate{
##' \item{\code{leq\_zero}:}{ \eqn{h(x) \leq 0} and}
##' \item{\code{geq\_zero}:}{ \eqn{h(x) \geq 0} and}
##' \item{\code{leq_geq\_rhs}:}{ \eqn{lhs \geq h(x) \leq rhs} .}
##' }
##' @note This function only intended for plugin authors.
##' @return Returns one function, which combines all the functional constraints.
##' @family plugin functions
##' @rdname ROI_plugin_build_inequality_constraints
##' @export
ROI_plugin_build_inequality_constraints <- function(x, type=c("leq_zero", "geq_zero", "leq_geq_rhs")) {
    stopifnot(type %in% c("leq_zero", "geq_zero", "leq_geq_rhs"))
    co <- as.F_constraint(constraints(x))
    if ( any(is.NO_constraint(co), is.null(co)) )
        return( list(F=NULL, J=NULL) )

    stopifnot( is.F_constraint(co) )
    x0 <- rep.int(0, length(objective(x)))
    ## for now we will treat < equal to <=
    b <- co$dir %in% c("<", "<=", ">=", ">")
    if ( !any(b) ) 
        return( list(F=NULL, J=NULL) )

    F_len <- sapply(co$F, function(f) length(f(x0)))
    grhs <- mapply(seq, cumsum(c(1L, F_len[-length(F_len)])), cumsum(F_len), SIMPLIFY = FALSE)
    dir <- lapply(grhs, function(i) co$dir[i])
    ## TODO: this needs to be checked (we need a check which ensures that vector valued functions
    ##       always use the same direction)
    b <- sapply(dir, function(v) all(v %in% c("<", "<=", ">=", ">"))) 
    J <- if ( is.null(co$J) ) NULL else co$J[b]
    if ( isTRUE(type == "leq_zero") )
        return( build_inequality_constraints(co$F[b], J, co$dir[b], co$rhs[b], x0, c("<", "<=")) )
    else if ( isTRUE(type == "geq_zero") )
        return( build_inequality_constraints(co$F[b], J, co$dir[b], co$rhs[b], x0, c(">", ">=")) )
    ## else leq_geq_rhs
    stop("TODO")
}


build_fun_dir_zero <- function(CFUN, dir, rhs, default_dir=c("<", "<=")) {
    if ( is.null(CFUN) ) return(NULL)
    if ( all(rhs == 0) ) {
        if ( all(dir %in% default_dir) ) {
            return( CFUN )
        } 
        return( function(x) (- CFUN(x)) )
    }
    if ( all(dir %in% default_dir) ) {
        return( function(x) (CFUN(x) - rhs) )
    }
    return( function(x) (rhs - CFUN(x)) )
}

## F <- co$F[b]; dir <- co$dir[b]; rhs <- co$rhs[b]; default_dir <- c(">", ">=")
## g(x) <= 0
build_inequality_constraints <- function(F, J, dir, rhs, x0, default_dir) {
    build_fun <- function(CFUN, dir, rhs) build_fun_dir_zero(CFUN, dir, rhs, default_dir)
    if ( isTRUE(length(F) == 1) ) {
        CF <- build_fun(F[[1]], dir, rhs)
        CJ <- if ( is.null(J) ) J else build_fun(J[[1]], dir, 0)
        return( list(F=CF, J=CJ) )
    }

    F_len <- sapply(F, function(f) length(f(x0)))
    ## group right hand side
    grhs <- mapply(seq, cumsum(c(1L, F_len[-length(F_len)])), cumsum(F_len), SIMPLIFY = FALSE)
    rhs <- lapply(grhs, function(i) rhs[i])
    dir <- lapply(grhs, function(i) dir[i])
    F <- mapply(build_fun, F, dir, rhs, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    if ( !is.null(J) ) {
        J <- mapply(build_fun, J, dir, rep.int(0, length(dir)), SIMPLIFY = FALSE, USE.NAMES = FALSE)
        jaccobian_fun <- function(x) {
            do.call(rbind, lapply(J, function(f) f(x)))
        }
    } else {
        jaccobian_fun <- NULL
    }
    ineq_fun <- function(x) {
        unlist(lapply(F, function(f) f(x)), FALSE, FALSE)
    }
    return( list(F=ineq_fun, J=jaccobian_fun) )
}

build_inequality_constraints_leq_geq_rhs <- function(F, dir, rhs, x0) {
    build_fun <- function(CFUN, dir, rhs) {
        if ( all(dir %in% c("<", "<=")) ) {
            return( list(fun=CFUN, lb=rep.int(-Inf, length(rhs)), ub=rhs) )
        }
        return( list(fun=CFUN, lb=rhs, ub=rep.int(Inf, length(rhs))) )
    }
    
    if ( isTRUE(length(F) == 1) ) {
        return( build_fun(F[[1]], dir, rhs) )
    }

    F_len <- sapply(F, function(f) length(f(x0)))
    grhs <- mapply(seq, cumsum(c(1L, F_len[-length(F_len)])), cumsum(F_len), SIMPLIFY = FALSE)
    rhs <- lapply(grhs, function(i) rhs[i])
    dir <- lapply(dir, function(i) dir[i])
    X <- mapply(build_fun, F, dir, rhs, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    F <- lapply(X, "[[", "F")
    lb <- lapply(X, "[[", "lb")
    ub <- lapply(X, "[[", "ub")
    ineq_fun <- function(x) {
        unlist(lapply(F, function(f) f(x)), FALSE, FALSE)
    }
    return( list(fun=ineq_fun, lb=lb, ub=ub) )
}

