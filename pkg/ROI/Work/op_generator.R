q("no")
Rdevel

library(ROI)


##'
##' @title Random Quadratic Problem
##' @params n an integer
##' @params m an integer
##' @description 
##'   \frac{1}{2} x\top Q x + q x
##'                     A x = b
##'                 lb <= x <= ub
rqp <- function(n, m) {
    A <- round(10 * matrix(runif(m * n), m, n))
    b <- rowSums(A)
    Q <- matrix(rnorm(n * n), n)
    Q <- t(Q) %*% Q ## psd matrix
    Q <- t(Q) + Q   ## symmetric psd matrix
    OP(objective = Q_objective(Q = Q, L = runif(n)),
       constraint = L_constraint(L = A, dir = eq(m), rhs = b), 
       bounds = V_bound(ui = seq_len(n), ub = rep.int(4 * max(b), n)))
}

## start with a simple solution
rbqp <- function(n, m) {
    A <- round(10 * matrix(runif(m * n), m, n))
    b <- rowSums(A)
    Q <- matrix(rnorm(n * n), n)
    Q <- t(Q) %*% Q ## psd matrix
    Q <- t(Q) + Q   ## symmetric psd matrix
    OP(objective = Q_objective(Q = Q, L = runif(n)),
       constraint = L_constraint(L = A, dir = eq(m), rhs = b), 
       types = rep("B", n),
       bounds = V_bound(ui = seq_len(n), ub = rep.int(4 * max(b), n)))
}


n <- 3
m <- 2
qp <- rqp(3, 2)
str(qp)
x <- ROI_solve(qp)
solution(x)
str(x)
as.matrix(constraints(qp)$L) %*% solution(x) - constraints(qp)$rhs
which.min(c(objective(qp)(solution(x)), objective(qp)(rep(1, n)))) == 1L
ROI_applicable_solvers(qp)
