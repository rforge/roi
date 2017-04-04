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

## start with a simple solution
## the solution to this problem always should be a vector of ones!
riqp <- function(n, m) {
    A <- matrix(sample(c(rep(0, 9), seq_len(9)), m*n, TRUE), m, n)
    b <- rowSums(A)
    for (i in seq_len(100)) {
        tmp <- sample(c(-seq_len(9), rep(0, 18), seq_len(9)), n*n, TRUE)
        Q <- matrix(tmp, n, n)
        Q <- t(Q) %*% Q ## psd matrix
        Q <- t(Q) + Q   ## symmetric psd matrix
        is_psd <- try(chol(Q), silent = TRUE)
        if (!inherits(is_psd, "try-error")) {
            break
        }
    }
    L <- sample(c(rep(0, 9), seq_len(9)), n, TRUE)
    OP(objective = Q_objective(Q = Q, L = L),
       constraint = L_constraint(L = A, dir = eq(m), rhs = b), 
       types = rep("I", n),
       bounds = V_bound(ui = seq_len(n), ub = rep.int(4 * max(b), n)))
}

riqp2 <- function(n, m) {
    A <- matrix(sample(c(rep(0, 9), seq_len(9)), m*n, TRUE), m, n)
    b <- rowSums(A)
    for (i in seq_len(100)) {
        tmp <- sample(c(-seq_len(9), rep(0, 18), seq_len(9)), n*n, TRUE)
        Q <- matrix(tmp, n, n)
        Q <- t(Q) %*% Q ## psd matrix
        Q <- t(Q) + Q   ## symmetric psd matrix
        is_psd <- try(chol(Q), silent = TRUE)
        if (!inherits(is_psd, "try-error")) {
            break
        }
    }
    L <- sample(c(rep(0, 9), seq_len(9)), n, TRUE)
    OP(objective = Q_objective(Q = Q, L = L),
       constraint = L_constraint(L = A, dir = eq(m), rhs = b), 
       types = rep("I", n),
       bounds = V_bound(ui = seq_len(n), ub = rep.int(4 * max(b), n)))
}

n <- 3
m <- 2
qp <- rqp(3, 2)
str(qp)
x <- ROI_solve(qp)
solution(x)
solution(ROI_solve(qp_to_socp(qp), tol = 1e-12))

qp$maximum <- TRUE
qp$objective$Q <- - qp$objective$Q
x <- ROI_solve(qp)
str(x)
solution(x)
solution(ROI_solve(qp_to_socp(qp)))


str(x)
as.matrix(constraints(qp)$L) %*% solution(x) - constraints(qp)$rhs
which.min(c(objective(qp)(solution(x)), objective(qp)(rep(1, n)))) == 1L
ROI_applicable_solvers(qp)


ROI_vs_gurobi_mixed_integer_qp <- function() {
    correct <- list()
    ones <- rep(1L, 4)
    x <- riqp(4, 12)
    so1 <- ROI_solve(x, "gurobi")
    correct[["gurobi"]] <- all(solution(so1) == ones)

    o <- reformulate(x, .LPLC.BCI.SOC)
    so2 <- ROI_solve(o, "ecos", tol = 1e-12, mi_int_tol = 1e-12)
    correct[["roi_ecos_qp"]] <- all(round(head(solution(so2), -1)) == ones)
    if (is.na(correct[["roi_ecos_qp"]]))
        correct[["roi_ecos_qp"]] <- FALSE
    correct
}

unlist(ROI_vs_gurobi_mixed_integer_qp())

nsim <- 100L
correct <- vector("list", nsim)
for (i in seq_len(nsim)) {
    print(i)
    correct[[i]] <- tryCatch(ROI_vs_gurobi_mixed_integer_qp(),
                             error = function(e) NULL)
}

stats <- do.call(rbind, correct)
table(as.logical(stats[,1]))
table(as.logical(stats[,2]))

o <- reformulate(x, .LPLC.BCI.SOC)
str(o)

as.matrix(terms(objective(x))$Q)
as.matrix(terms(objective(x))$L)

as.matrix(constraints(x)$L)
as.matrix(constraints(x)$rhs)
as.matrix(constraints(x)$dir)

as.matrix(constraints(x)$L)

str(bounds(x))

op1 <- x

to

pprint.Q_objective <- function(x) {
    writeLines("Q:")
    print(as.matrix(terms(x)$Q), rowlab = NULL)
}
pprint.Q_objective(objective(x))

print
capture.output(showMethods("print"))
pp

as.matrix(terms(objective(x))$Q)


pprint.L_constraint <- function(x) {

}


install.packages("ascii")
library(ascii)
