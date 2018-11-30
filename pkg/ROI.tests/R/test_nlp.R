##
## Rosenbrock Banana function.
##
test_nlp_01 <- function(solver) {
    objval <- 0
    sol <- c(1, 1)
    
    obj <- function(x) {
        return( 100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2 )
    }

    grad <- function(x) {
        return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
                   200 * (x[2] - x[1] * x[1])) )
    }

    x <- OP(F_objective(F = obj, n = 2L, G = grad))
    bounds(x) <- V_bound(lb = c(-3, -3), ub = c(3, 3))

    opt <- ROI_solve(x, solver = solver, 
                     control = solver_control(solver, sol))

    check("NLP-01@01", equal(solution(opt), sol, tol = 1e-4))
    check("NLP-01@02", equal(solution(opt, "objval"), objval, tol = 1e-4))
}

##
## Example from NLopt tutorial.
##
test_nlp_02 <- function(solver) {
    objval <- 0.5443311
    sol <- c(1/3, 8/27)

    obj <- function(x) sqrt(x[2])
    grad <- function(x) c(0, 0.5 / sqrt(x[2]))

    con <- function(x) (c(2, -1) * x[1] + c(0,  1))^3 - x[2]
    jac <- function(x) {
        a <- c(2, -1)
        b <- c(0,  1)
        rbind(c(3 * a[1] * (a[1] * x[1] + b[1])^2, -1.0), 
              c(3 * a[2] * (a[2] * x[1] + b[2])^2, -1.0))
    }

    x <- OP(F_objective(F = obj, n = 2L, G = grad),
        F_constraint(con, c("<=", "<="), c(0, 0), J = jac))
    
    opt <- ROI_solve(x, solver, control = solver_control(solver, sol))

    check("NLP-02@01", equal(solution(opt), sol, tol = 1e-4))
    check("NLP-02@02", equal(solution(opt, "objval"), objval, tol = 1e-4))
}

##
## Problem 23 from the Hock-Schittkowsky test suite.
##
test_nlp_03 <- function(solver) {
    objval <- 2
    sol <- c(1, 1)

    obj <- function(x) x[1]^2 + x[2]^2
    grad <- function(x) c(2 * x[1], 2 * x[2])


    con <- function(x) {
        c(1 - x[1] - x[2], 1 - x[1]^2 - x[2]^2, 9 - 9*x[1]^2 - x[2]^2,
          x[2] - x[1]^2, x[1] - x[2]^2)
    }

    jac <- function(x) {
        rbind(c(-1, -1), c(-2 * x[1], -2 * x[2]), c(-18 * x[1], -2 * x[2]),
              c(-2 * x[1], 1), c(1, -2 * x[2]))
    }

    x <- OP(objective = F_objective(F = obj, n = 2L, G = grad),
        constraints = F_constraint(F = con, dir = leq(5), rhs = double(5), J = jac),
        bounds = V_bound(ld = -50, ud = 50, nobj = 2L))

    opt <- ROI_solve(x, solver, control = solver_control(solver, sol))

    check("NLP-03@01", equal(solution(opt), sol, tol = 1e-4))
    check("NLP-03@02", equal(solution(opt, "objval"), objval, tol = 1e-4))
}

##
## Problem 71 from the Hock-Schittkowsky test suite.
##
test_nlp_04 <- function(solver) {
    objval <- 17.01402
    sol <- c(1, 4.74299963, 3.82114998, 1.37940829)

    obj <- function(x) x[1] * x[4] * (x[1] + x[2] + x[3]) + x[3]
    
    grad <- function(x) {
        c(x[1] * x[4] + x[4] * (x[1] + x[2] + x[3]), x[1] * x[4],
          x[1] * x[4] + 1.0, x[1] * (x[1] + x[2] + x[3]))
    }

    con <- c(function(x) c(25 - x[1] * x[2] * x[3] * x[4]),
             function(x) x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40)

    jac <- c(function(x) c(-x[2] * x[3] * x[4], -x[1] * x[3] * x[4],
                           -x[1] * x[2] * x[4], -x[1] * x[2] * x[3]),
             function(x) c(2.0 * x[1], 2.0 * x[2], 2.0 * x[3], 2.0 * x[4]))

    x <- OP(objective = F_objective(F = obj, n = 4L, G = grad),
        constraints = F_constraint(F = con, dir = c("<=", "=="), rhs = c(0, 0), J = jac),
        bounds = V_bound(ld = 1, ud = 5, nobj = 4L))

    opt <- ROI_solve(x, solver, control = solver_control(solver, sol))

    check("NLP-04@01", equal(solution(opt), sol, tol = 1e-4))
    check("NLP-04@02", equal(solution(opt, "objval"), objval, tol = 1e-4))
}
