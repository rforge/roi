##
## reformulations
##

suppressPackageStartupMessages( require("slam") )
suppressPackageStartupMessages( require("ROI") )
suppressPackageStartupMessages( require("ROI.plugin.ecos") )
suppressPackageStartupMessages( require("ROI.plugin.glpk") )

DEVEL <- FALSE
if ( DEVEL ) {

    q("no")
    Rdevel

    library(slam)
    library(ROI)

}

check <- function(domain, condition, level=1, message="", call=sys.call(-1L)) {
    if ( isTRUE(condition) ) return(invisible(NULL))
    msg <- sprintf("in %s", domain)
    if ( all(nchar(message) > 0) ) msg <- sprintf("%s\n\t%s", msg, message)
    stop(msg)
    return(invisible(NULL))
}

is_solver_registered <- function(solver) {
    isTRUE(solver %in% names(ROI_registered_solvers(solver)))
}

skip_test_solver_msg <- function(solver, test) {
    msg <- "NOTE: Skip test '%s' since the solver '%s' is not registered!\n"
    cat(sprintf(msg, test, solver))
}

## Example from 
## "Pseudo-Boolean Optimization" by E. Boros and P. Hammer (boros01pseudoboolean.pdf)
## 
## f(x, y, z) = 6 - x - 4 y - z + 3 x y + y z
## Objective value: 2
## Solution: c(0, 1, 1)
test_reformulate_bqp <- function() {   
    Q <- rbind(c(0, 3, 0), c(3, 0, 1), c(0, 1, 0))
    bqp <- OP(Q_objective(Q = Q, L = c(-1, -4, -1)))
    types(bqp) <- rep("B", 3)

    solver <- "glpk"
    if ( is_solver_registered(solver) ) {
        milp <- ROI_reformulate(bqp, ROI_solver_signature(solver))
        s <- ROI_solve(milp, solver = solver)
        check("reformulate_bqp", equal(solution(s, "objval") + 6, 2))
    } else {
        skip_test_solver_msg(solver, "reformulate_bqp")
    }
}

## Example from the quadprog package
## S original by Berwin A. Turlach R port by Andreas Weingessel
## GPL-3
test_reformulate_qp_01 <- function() {
    A <- cbind(c(-4, -3, 0), c( 2,  1, 0), c( 0, -2, 1))
    qp <- OP(Q_objective(Q = diag(3), L =  c(0, -5, 0)),
             L_constraint(L = t(A), dir = geq(3), rhs = c(-8, 2, 0)))

    solver <- "ecos"
    if ( is_solver_registered(solver) ) {
        socp <- ROI_reformulate(qp, ROI_solver_signature(solver))
        s <- ROI_solve(socp, solver = solver)
        ref_sol <- c(0.476190476190476, 1.04761904761905, 2.0952380952381)        
        check("reformulate_qp_01", equal(head(solution(s), -1), ref_sol, tol = 1e-4))
    } else {
        skip_test_solver_msg(solver, "reformulate_qp_01")
    }
}

test_reformulate_qp_02 <- function() {
    qp <- OP(Q_objective(Q = matrix(c(25, -5, -5, 5), 2), 
                         L =  c(8, -6)))
    constraints(qp) <- c(L_constraint(c(1, 1), "==", 3),
                         L_constraint(c(1, 0), ">=", 1),
                         L_constraint(c(0, 1), ">=", 2))

    solver <- "ecos"
    if ( is_solver_registered(solver) ) {
        socp <- ROI_reformulate(qp, ROI_solver_signature(solver))
        s <- ROI_solve(socp, solver = solver)
        check("reformulate_qp_02", equal(head(solution(s), -1), c(1, 2)))
    } else {
        skip_test_solver_msg(solver, "reformulate_qp_01")
    }
}

## 
## Start Testing
##
test_reformulate_bqp()

test_reformulate_qp_01()

test_reformulate_qp_02()


