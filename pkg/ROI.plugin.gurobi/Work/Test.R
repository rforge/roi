library(ROI)
library(gurobi)

##' --------------------------
##' LP
##' --------------------------
mat <- matrix(c(3, 4, 2,
                2, 1, 2,
                1, 3, 2), nrow=3, byrow=TRUE)
x <- OP(objective = c(2, 4, 3),
        constraints = L_constraint(L = mat,
                                   dir = c("<=", "<=", "=="),
                                   rhs = c(60, 40, 1)),
        maximum = TRUE)

opt <- ROI_solve(x, solver="glpk")
solution(opt)

opt <- ROI_solve(x, solver = "gurobi")
solution(opt) ## c(0, 20/3, 50/3), tol=1e-4)
opt$objval ## 76.6666


##' --------------------------
##' MILP
##' --------------------------


##' --------------------------
##' QP
##' --------------------------
R
library(gurobi)
library(Matrix)
qo <- Q_objective(Q = diag(2), L =  numeric(2))
qc <- Q_constraint(rbind(c(1, 0), c(0, 0)), c(0, 0), dir=">=", rhs=0.5)
x <- OP(qo, qc)
## solution <- c(1, 0)
x
opt <- ROI_solve(x, solver="alabama", start=c(3, 3))
opt


qo <- Q_objective(Q = diag(2), L =  numeric(2))
qc <- Q_constraint(rbind(c(1, 0), c(0, 0)), c(0, 0), dir=">=", rhs=0.5)
x <- OP(qo, qc)
## solution <- c(1, 0)
x
opt <- ROI_solve(x, solver="alabama", start=c(3, 3))
opt


##' --------------------------
##' gurobi
##' --------------------------
model <- list()
## objective
model$obj <- numeric(2)
model$Q <- diag(2)
## constraints
Qc <- -rbind(c(1, 0), c(0, 0)) / 2
##model$quadcon <- list(list(Qc=as(Qc, "sparseMatrix"), q=-c(0, 0), rhs=-0.5))
## model$quadcon <- list(list(Qc=Qc, q=-c(0, 0), rhs=-0.5))
#sense gets ignored# 
model$quadcon <- list(list(Qc=-Qc, q=c(0, 0), rhs=0.5, QcSense=">"))

model$A <- matrix(0, 1, 2)
model$sense <- "<="
model$rhs <- 1

model$modelsense <- "min"

result <- NULL
result <- gurobi(model)
result$x


QCSense

##y <- OP(qo)