## Original mip.R currently in package 'relations'
## functions are removed if ported to ROI 

## A simple framework for representing and solving linear (LP),
## mixed integer linear programs (MILPs) of the form
##   optimize obj' x
##   such that mat %*% x dir rhs
## and quadratic (QP), mixed integer quadratic programs (MIQPs) of the form
##   optimize x' Q x / 2 + c' x
##   such that mat %*% x dir rhs
## with possibly given types (C/I/B for continuous/integer/binary) and
## given additional (lower and upper) bounds on x.
## (Default of course x >= 0).

### * MILPs
## ported. moved to problem_constructor.R

## FIXED the following note by KH:

## Currently, 'constraints' always is a (not necessarily named) list
## with mat, dir and rhs, which really is the most general case of
## linear constraints we can think of.  Let us add names for now;
## eventually, there should be more sanity checking and maybe a
## creator for linear constraint objects.

## get registered LP solvers
.LP_solvers <- function(){
  db <- sapply(get_solver_types_from_db(), function(x) "LP" %in% x)
  names(db)[db]
}

solve_LP <- function( x, solver = NULL, control = list() ) {  
  ## for more notes see solve_MILP
  ## Handle the boundary case of no variables.
    if( !length(terms(objective(x))$L) ) {
        y <- .solve_empty_MIP(x)
        return(y)
    }

    solver <- match.arg(solver, .LP_solvers())

    class(x) <- c(solver, class(x))

    .solve_LP(x, control)

}

.QCP_solvers <- function(){
  db <- sapply(get_solver_types_from_db(), function(x) "QCP" %in% x)
  names(db)[db]
}

solve_QCP <- function( x, solver = NULL, control = list() ) {  
  ## for more notes see solve_MILP
  ## Handle the boundary case of no variables.
    if( !length(terms(objective(x))$L) && !length(terms(objective(x))$Q) ) {
        y <- .solve_empty_MIP(x)
        return(y)
    }

    solver <- match.arg(solver, .QCP_solvers())

    class(x) <- c(solver, class(x))

    .solve_QCP(x, control)

}

.QP_solvers <- function(){
  db <- sapply(get_solver_types_from_db(), function(x) "QP" %in% x)
  names(db)[db]
}

solve_QP <- function( x, solver = NULL, control = list() ) {  
  ## for more notes see solve_MILP
  ## Handle the boundary case of no variables.
    if( !length(terms(objective(x))$L) && !length(terms(objective(x))$Q) ) {
        y <- .solve_empty_MIP(x)
        return(y)
    }

    solver <- match.arg(solver, .QP_solvers())

    class(x) <- c(solver, class(x))

    .solve_QP(x, control)

}

## get registered MILP solvers
.MILP_solvers <- function(){
  db <- sapply(get_solver_types_from_db(), function(x) "MILP" %in% x)
  names(db)[db]
}

solve_MILP <- function( x, solver = NULL, control = list() ) {
  ## In ROI we now use the registry package for solvers.
  ## <NOTE>
  ## Currently, there is only little support for control arguments.
  ## In particular, one cannot directly pass arguments to the solver.
  ## </NOTE>
  
    ## Handle the boundary case of no variables.
    if( !length(terms(objective(x))$L) ) {
        y <- .solve_empty_MIP(x)
        if(!is.null(nos <- control$n)
           && !identical(as.integer(nos), 1L))
            y <- list(y)
        return(y)
    }

    solver <- match.arg(solver, .MILP_solvers())

    ## If more than one (binary) solution is sought and the solver does
    ## not provide direct support, use poor person's branch and cut:
    if( !is.null(nos <- control$n) && (!get_solver_option_from_db(solver, "multiple_solutions")) ) {
        control$n <- NULL
        ## Mimic the mechanism currently employed by Rcplex(): return a
        ## list of solutions only if nos > 1 (or NA).
        if(!identical(as.integer(nos), 1L)) {
            add <- identical(control$add, TRUE)
            control$add <- NULL
            return(.find_up_to_n_binary_MILP_solutions(x, nos, add,
                                                       solver, control))
        }
    }
    ## Note that lpSolve could find all binary solutions for all-binary
    ## programs.

    class(x) <- c(solver, class(x))

    .solve_MILP(x, control)

}

### * MICQPs

.MIQCP_solvers <- function(){
  db <- sapply(get_solver_types_from_db(), function(x) "MIQCP" %in% x)
  names(db)[db]
}

solve_MIQCP <-
function(x, solver = NULL, control = list())
{
    ## Currently, only CPLEX can generally be used for solving MIQCPs.

    ## Handle the boundary case of no variables.
    if( !length(terms(objective(x))$L) && !length(terms(objective(x))$Q) ) {
        y <- .solve_empty_MIP(x)
        if(!is.null(nos <- control$n)
           && !identical(as.integer(nos), 1L))
            y <- list(y)
        return(y)
    }

    solver <- match.arg(solver, .MIQCP_solvers())

    class(x) <- c(solver, class(x))

    .solve_MIQCP(x, control)

}

### * MIQPs

## get registered MILP solvers
.MIQP_solvers <- function(){
  db <- sapply(get_solver_types_from_db(), function(x) "MIQP" %in% x)
  names(db)[db]
}

solve_MIQP <-
function(x, solver = NULL, control = list())
{
    ## Currently, only CPLEX can generally be used for solving MIQPs.
    ## For the other MILP solvers, all-binary programs can be solved via
    ## linearization.
    ## <NOTE>
    ## Actually, linearization only requires that the quadratic part is
    ## all-binary.  Maybe support the mixed linear part case eventually.
    ## </NOTE>

    ## Handle the boundary case of no variables.
    if( !length(terms(objective(x))$L) && !length(terms(objective(x))$Q) ) {
        y <- .solve_empty_MIP(x)
        if(!is.null(nos <- control$n)
           && !identical(as.integer(nos), 1L))
            y <- list(y)
        return(y)
    }

    is_BQP <- identical(unique(x$types), "B")
    solver <- if(is_BQP) {
        ## If this is an all-binary problem, we can linearize, so use
        ## non-commercial defaults (obviously, there should eventually
        ## be a way to specify the default MILP and MIQP solver).
        match.arg( solver, .MILP_solvers() )
    } else {
        ## Use a MIQP solver by default.
        match.arg( solver, c(.MIQP_solvers(), .MILP_solvers()) )
    }
    ## For real MIQP solvers (currently only CPLEX), do not linearize by
    ## default, but allow for doing so for debugging purposes.
    if(solver %in% .MIQP_solvers) {
        if(identical(control$linearize, TRUE))
            .solve_BQP_via_linearization(x, solver, control)
        else {
            ## if not all-binary problem we use a quadratic solver
            class(x) <- c(solver, class(x))
            .solve_MIQP(x, control)
        }
    } else {
        ## If this is an all-binary problem, we can linearize.
        if(is_BQP)
            .solve_BQP_via_linearization(x, solver, control)
        else
            stop(gettextf("Solver '%s' can only handle all-binary quadratic programs.",
                          solver))
    }
}

.solve_BQP_via_linearization <-
function(x, solver, control)
{
    ## Number of variables.
    n <- length(x$objective$L)
    ## Solve via linearization.
    y <- solve_MILP(.linearize_BQP(x), solver, control)
    ## Reduce solution to the original variables.
    finisher <- function(e) {
        e$solution <- e$solution[seq_len(n)]
        e
    }
    ## <FIXME>
    ## Wouldn't it be simpler to check if y inherits from MIP_solution?
    if(!is.null(nos <- control$n) && !identical(nos, 1L))
        lapply(y, finisher)
    else
        finisher(y)
    ## </FIXME>
}

### * Solver interfaces

## solver interfaces are provided in separate files
## naming convention: plugin_<solver> where <solver> is replaced by
##                    the solver name in lower case 

### * Utilities

## utilities moved to utilities.R


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
