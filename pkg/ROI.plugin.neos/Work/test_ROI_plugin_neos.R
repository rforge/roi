##
## TODO: The mixed integr case is currently not implemented
##

q("no")
Rdevel

setwd("/home/florian/work/Optimization/ROI/ROI_R-Forge/pkg/ROI.plugin.neos/Work")

library(rneos)
Sys.setenv(ROI_LOAD_PLUGINS = FALSE)
library(ROI)
library(ROI.plugin.neos)
library(ROI.plugin.cplex)

## attach(getNamespace("ROI.plugin.neos"))

check <- function(domain, condition, level=1, message="", call=sys.call(-1L)) {
    if ( isTRUE(condition) ) return(invisible(NULL))
    msg <- sprintf("in %s", domain)
    if ( all(nchar(message) > 0) ) msg <- sprintf("%s\n\t%s\n", msg, message)
    cat("Diff:", msg)
    return(invisible(NULL))
}


solver <- "neos"


x <- OP()
objective(x) <- c(1:2)
constraints(x) <- L_constraint(diag(2), leq(2), c(1000, 10000))
types(x) <- c("I", "I")
maximum(x) <- TRUE

cplex0 <- ROI_solve(x, "cplex")
cplex0
solution(cplex0)

cat(ROI.plugin.neos:::roi_lp_to_gams(x))

control <- list(method = "cplex", user = "fschwend", email = "fschwend@wu.ac.at")
opt0 <- ROI_solve(x, "neos", control)

opt0
solution(opt0)

##
## LP (OK: 23.10.2017)
##
mat <- matrix(c(3, 4, 2,
                2, 1, 2,
                1, 3, 2), nrow=3, byrow=TRUE)
x <- OP(objective = c(2, 4, 3),
        constraints = L_constraint(L = mat,
                                   dir = c("<=", "<=", "<="),
                                   rhs = c(60, 40, 80)),
        bounds = V_bound(ui = seq_len(3), ub = c(1000, Inf, 1000), nobj = 3),
        maximum = TRUE)

control <- list(method = "BDMLP", user = "fschwend", email = "fschwend@wu.ac.at")
opt0 <- ROI_solve(x, solver = solver, control = control)

## job <- eval(opt0)
## results <- NgetFinalResults(obj = job, convert = TRUE)
## cat(results@ans)

solution(opt0)
check("LP-01@01", equal(opt0$solution, c(0, 20/3, 50/3), tol=1e-4))
check("LP-01@02", equal(opt0$objval, 230/3, tol=1e-4))


##
## MILP (OK)
##
## min:  3 x + 1 y + 3 z
## s.t.
##      -1 x  +    y  +   z  <=  4
##               4 y  - 3 z  <=  2
##         x  -  3 y  + 2 z  <=  3
##     x, z \in Z_+
##     y >= 0, z >= 2, x <= 4, y <= 100
obj <- c(3, 1, 3)
A <- rbind(c(-1,  2,  1),
           c( 0,  4, -3),
           c( 1, -3,  2))
b <- c(4, 2, 3)
bounds <- V_bound(li = c(1L, 3L), ui = c(1L, 2L),
                  lb = c(-Inf, 2), ub = c(4, 100), nobj = 3L)

x <- OP(objective = obj,
        constraints = L_constraint(L = A,
                                   dir = c("<=", "<=", "<="),
                                   rhs = b),
        types = c("I", "C", "I"),
        bounds = bounds,
        maximum = TRUE)


cat(ROI.plugin.neos:::roi_lp_to_gams(x))

cplex1 <- ROI_solve(x, solver="cplex")

control <- list(method = "cplex", user = "fschwend", email = "fschwend@wu.ac.at")
opt1 <- ROI_solve(x, solver = solver, control = control)
opt1
solution(opt1)
solution(cplex1)
check("MILP-01@01", equal(solution(opt1), c(4, 2.5, 3), tol=1e-01))

msg <- opt1$message

cat(as.list(opt1)$xmlstring)
z <- eval(opt1)
job <- z

results <- NgetFinalResults(obj = z, convert = TRUE)
results
solution(sol_cplex)

opt <- ROI_solve(x, solver = solver, control = control)
opt

control <- list(method = "scip", user = "fschwend", email = "fschwend@wu.ac.at")
opt0 <- ROI_solve(x, solver=solver, control=control)
solution(opt0)

str(opt0)
cat(opt0$message)

control <- list(method = "MOSEK", user = "fschwend", email = "fschwend@wu.ac.at")
opt1 <- ROI_solve(x, solver=solver, control=control)
solution(opt1)
check("MILP-01@01", equal(opt1$solution , c(4, 2.5, 3), tol=1e-01))
str(opt1)
cat(opt1$message)


opt1 <- ROI_solve(x, solver="cplex")
solution(opt)
check("MILP-01@01", equal(opt$solution , c(4, 2.5, 3), tol=1e-01))


##
## QP
##

q("no")
Rdevel

setwd("/home/florian/work/Optimization/ROI/ROI_R-Forge/pkg/ROI.plugin.neos/Work")

library(rneos)
Sys.setenv(ROI_LOAD_PLUGINS = FALSE)
library(ROI)
library(ROI.plugin.neos)
library(ROI.plugin.cplex)
solver <- "neos"

## attach(getNamespace("ROI.plugin.neos"))

A <- cbind(c(-4, -3, 0), 
           c( 2,  1, 0), 
           c( 0, -2, 1))
x <- OP(Q_objective(diag(3), L =  c(0, -5, 0)),
        L_constraint(L = t(A),
                     dir = rep(">=", 3),
                     rhs = c(-8, 2, 0)))

cat(ROI.plugin.neos:::ROI_to_gams.OP(x))

control <- list(method = "CONOPT", 
                user = "fschwend", email = "fschwend@wu.ac.at")
opt3 <- ROI_solve(x, solver=solver, control = control)
opt3
solution(opt3)

cplex3 <- ROI_solve(x, solver = "cplex")
cplex3
solution(cplex3)

job <- eval(opt3)
results <- NgetFinalResults(obj = job, convert = TRUE)
results

solution(opt3)
solution <- c(0.476190476190476, 1.04761904761905, 2.0952380952381)
check("QP-01@01", equal(opt3$solution, solution) )
check("QP-01@02", equal(opt3$objval, -2.38095238095238) )

##
## MIQP
##
A <- cbind(c(-4, -3, 0), 
           c( 2,  1, 0), 
           c( 0, -2, 1))
x <- OP(Q_objective(diag(3), L =  c(0, -5, 0)),
        L_constraint(L = t(A),
                     dir = rep(">=", 3),
                     rhs = c(-8, 2, 0)),
        types = sample(c("B", "I", "C")))
types(x)

cat(ROI.plugin.neos:::roi_qp_to_gams(x), file = "model.gms")

control <- list(method = "cplex", user = "fschwend", email = "fschwend@wu.ac.at")
opt4 <- ROI_solve(x, solver=solver, control = control)
## cat(as.list(opt4)$xmlstring, file = "model.gms")

job <- eval(opt4)
results <- NgetFinalResults(obj = job, convert = TRUE)
results

solution(opt4)

cplex4 <- ROI_solve(x, solver = "cplex")
cplex4
solution(cplex4)

job <- eval(opt3)
results <- NgetFinalResults(obj = job, convert = TRUE)
results

##
## QCQP (OK)
## 

q("no")
Rdevel

library(rneos)
Sys.setenv(ROI_LOAD_PLUGINS = FALSE)
library(ROI)
library(ROI.plugin.neos)
library(ROI.plugin.cplex)
library(ROI.plugin.gurobi)

solver <- "neos"

x <- OP()
objective(x) <- Q_objective(Q = diag(2), L =  numeric(2))
constraints(x) <- Q_constraint(rbind(c(1, 0), c(0, 0)), c(0, 0), dir=">=", rhs=0.5)

cat(ROI.plugin.neos:::roi_qcqp_to_gams(x))

control <- list(method = "BARON", user = "fschwend", email = "fschwend@wu.ac.at")
opt4 <- ROI_solve(x, solver = solver, control = control)
opt4
solution(opt4)


cplex4 <- ROI_solve(x, solver = "cplex")
cplex4
solution(cplex4)


check("QCQP-01@01", equal(opt4$solution, c(1, 0)) )
check("QCQP-01@02", equal(opt4$objval, 0.5) )

control <- list(method = "Knitro", user = "fschwend", email = "fschwend@wu.ac.at")
m <- ROI_solve(x, solver = solver, control = control, dry_run = TRUE)
cat(as.list(m)$xmlstring)

##
## QCQP
##


create_sym_matrix <- function(n) {
    x <- runif(n * n, -49.5, 49.5)
    M <- matrix(x, n, n)
    M <- round(t(M) + M)
    M
}

create_psd_matrix <- function(n) {
    ## NOTE: works only for small n
    m <- 1000
    for (i in seq_len(m)) {
        sym <- create_sym_matrix(n)
        if ( all(eigen(sym)$values < 0) )
            break
    }
    if ( i == m )
        stop("no psd found")
    sym
}

cat(deparse(create_psd_matrix(3)))
x <- OP()
objective(x) <- Q_objective(Q = matrix(c( 86, -42, -45, 
                                         -42,  77,   8, 
                                         -45,   8,  41), 3, 3), 
                            L = c(2, 3, 5))

constraints(x) <- Q_constraint(Q = list(matrix(c(-89,  -8,   4, 
                                                  -8, -69, -13, 
                                                   4, -13, -35), 3, 3),
                                        matrix(c(-75,  13,   1, 
                                                  13,  -6, -14, 
                                                   1, -14, -98), 3, 3)),
                               L = matrix(c( 600, 5800, 1550, 
                                            1050, 5500, 3650), 2, 3),
                               dir = c(">=", ">="), 
                               rhs = c(50000, 10000))
round(runif(3, 0, 100), 2)

z <- c(20.48, 73.85, 6.66)
z %*% as.matrix(constraints(x)$Q[[2]]) %*% z 
as.vector(constraints(x)$L[2,]) %*% z
c(1050, 5500, 3650) %*% z + z %*% as.matrix(constraints(x)$Q[[1]]) %*% z 

gurobi5 <- ROI_solve(x, "gurobi")
gurobi5
(s <- solution(gurobi5))
as.F_constraint(constraints(x))$F[[1]](s)
as.F_constraint(constraints(x))$F[[2]](s)


control <- list(method = "BARON", user = "fschwend", email = "fschwend@wu.ac.at")
opt5 <- ROI_solve(x, solver = solver, control = control)
opt5

job <- eval(opt5)
results <- NgetFinalResults(obj = job, convert = TRUE)
results

solution(opt5)
solution(opt5) - solution(gurobi5)

control <- list(method = "Knitro", user = "fschwend", email = "fschwend@wu.ac.at")
opt5 <- ROI_solve(x, solver = solver, control = control)
opt5
solution(opt5)
solution(opt5) - solution(gurobi5)


##
## MIQCQP
##
types(x) <- sample(c("B", "I", "C"))

gurobi6 <- ROI_solve(x, "gurobi")
gurobi6
solution(gurobi6)

control <- list(method = "baron", user = "fschwend", email = "fschwend@wu.ac.at")
opt6 <- ROI_solve(x, solver = solver, control = control)
opt6
solution(opt6) - solution(gurobi6)

control <- list(method = "knitro", user = "fschwend", email = "fschwend@wu.ac.at")
opt6 <- ROI_solve(x, solver = solver, control = control)
opt6
solution(opt6)- solution(gurobi6)



