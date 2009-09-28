##############################################################
## linear constraints

## valid input
## simplest linear constraint

L_constraint(1, "<", 4)

## more sophisticated

L_constraint(matrix(c(1, 1), nrow = 2), c("<", "=="), c(5, 4))

L_constraint(matrix(c(1:4), ncol = 2), c("<", "<"), c(4, 5))

## invalid input

L_constraint(1, "<", c(5, 4))

L_constraint(matrix(c(1, 1), nrow = 2), c("<", "=="), 4)

L_constraint(matrix(c(1, 1), nrow = 2), c("<", "!="), c(5, 4))

##############################################################
## quadratic constraints

## valid input
## simplest quadratic constraint

Q_constraint(2, 1, "<", 5)

Q_constraint(matrix(rep(2, 4), ncol = 2), c(1, 2), "<", 5)

##############################################################
## function constraints

f <- function(x){
  sum(x^3) - 1:length(x)%*%x
}

F_constraint(f, "<", 5)

##############################################################
## problem constructors

## From Rglpk_solve_LP man page

## Example 1:
## Simple linear program.
## maximize:   2 x_1 + 4 x_2 + 3 x_3
## subject to: 3 x_1 + 4 x_2 + 2 x_3 <= 60
##             2 x_1 +   x_2 +   x_3 <= 40
##               x_1 + 3 x_2 + 2 x_3 <= 80
##               x_1, x_2, x_3 are non-negative real numbers
library("ROI")
ex1_lp <- LP(objective = c(2, 4, 3),
             constraints = L_constraint(L = matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3),
               dir = c("<=", "<=", "<="),
               rhs = c(60, 40, 80)),
             maximum = TRUE)

lpsolvers <- ROI:::.LP_solvers()
lp_results <- data.frame(objval = rep(NA, length.out = length(lpsolvers)), timing = NA)
rownames(lp_results) <- lpsolvers

for(solver in lpsolvers){
  timing <- system.time(res <- ROI_solve(ex1_lp, solver = solver))["elapsed"]
  lp_results[solver, ] <- c(res$objval, timing)
}

lp_results

## Example 2:
## Simple mixed integer linear program.
## maximize:    3 x_1 + 1 x_2 + 3 x_3
## subject to: -1 x_1 + 2 x_2 +   x_3 <= 4
##                      4 x_2 - 3 x_3 <= 2
##                x_1 - 3 x_2 + 2 x_3 <= 3
##                x_1, x_3 are non-negative integers
##                x_2 is a non-negative real number

ex2_milp <- MILP(objective = c(3, 1, 3),
                 constraints = L_constraint(L = matrix(c(-1, 0, 1, 2, 4, -3, 1, -3, 2), nrow = 3),
                   dir = c("<=", "<=", "<="),
                   rhs = c(4, 2, 3)),
                 types = c("I", "C", "I"),
                 maximum = TRUE)

milpsolvers <- ROI:::.MILP_solvers()
milp_results <- data.frame(objval = rep(NA, length.out = length(milpsolvers)), timing = NA)
rownames(milp_results) <- milpsolvers

for(solver in milpsolvers){
  timing <- system.time(res <- ROI_solve(ex2_milp, solver = solver))["elapsed"]
  milp_results[solver, ] <- c(res$objval, timing)
}

milp_results


## Example 3:
## MILP same as in Example 2 but with bounds replaced by
## -Inf <  x_1 <= 4
##    0 <= x_2 <= 100
##    2 <= x_3 <  Inf

## FIXME: work with bounds replacement function
bounds <- list(lower = list(ind = c(1L, 3L), val = c(-Inf, 2)),
               upper = list(ind = c(1L, 2L), val = c(4, 100)))

ex3a_milp <- MILP(objective = c(3, 1, 3),
                 constraints = L_constraint(L = matrix(c(-1, 0, 1, 2, 4, -3, 1, -3, 2), nrow = 3),
                   dir = c("<=", "<=", "<="),
                   rhs = c(4, 2, 3)),
                 types = c("I", "C", "I"),
                 bounds = bounds,
                 maximum = TRUE)

for(solver in milpsolvers){
  timing <- system.time(res <- ROI_solve(ex3a_milp, solver = solver))["elapsed"]
  milp_results[solver, ] <- c(res$objval, timing)
}

milp_results

## force negative values in solution
bounds <- list(lower = list(ind = c(1L, 2L, 3L), val = c(-Inf, -Inf, 2)),
               upper = list(ind = c(1L, 2L), val = c(4, -0.5)))

ex3b_milp <- MILP(objective = c(3, 1, 3),
                 constraints = L_constraint(L = matrix(c(-1, 0, 1, 2, 4, -3, 1, -3, 2), nrow = 3),
                   dir = c("<=", "<=", "<="),
                   rhs = c(4, 2, 3)),
                 types = c("I", "C", "I"),
                 bounds = bounds,
                 maximum = TRUE)

ex3c_milp <- .add_box_constraints_to_MIP(ex3b_milp)

## Example 4:
## Simple quadratic program (QP)
## Example from 'quadprog'
## minimize:          - 5 x_2      + 1/2 (x_1^2 + x_2^2 + x_3^2)
## subject to: -4 x_1 - 3 x_2      >= -8
##             2 x_1 +   x_2       >= 2
##                   - 2 x_2 + x_3 >= 0

ex4_qp <- QP( Q_objective (Q = diag(1, 3), L = c(0, -5, 0)),
              L_constraint(L = matrix(c(-4,-3,0,2,1,0,0,-2,1),
                             ncol = 3, byrow = TRUE),
                           dir = rep(">=", 3),
                           rhs = c(-8,2,0)) )

## Example 5:
## Another QP (this is qpex1.c in the CPLEX examples)
## maximize:     x_1 + 2 x_2 + 3  x_3 - 1/2 (33 x_1^2 + 22 x_2^2 + 11 x_3^2) + 6 x_1 x_2 + 11.5 x_2 x_3
## subject to: - x_1 +   x_2 +    x_3 <= 20
##               x_1 - 3 x_2 +    x_3 <= 30
##           

ex5_qp <- QP( Q_objective(Q = matrix(c(-33, 6, 0, 6, -22, 11.5, 0, 11.5, -11),
                        byrow = TRUE, ncol = 3),
                      L = c(1, 2, 3)),
          L_constraint(L = matrix(c(-1, 1, 1, 1, -3, 1),
                         byrow = TRUE, ncol = 3),
                       dir = rep("<=", 2),
                       rhs = c(20, 30)),
         maximum = TRUE)

## Example 6:
## QCP (this is qcpex1.c in the CPLEX examples)
## maximize:     x_1 + 2 x_2 + 3 x_3 - 1/2 (33 x_1^2 + 22 x_2^2 + 11 x_3^2) + 6 x_1 x_2 + 11.5 x_2 x_3
## subject to: - x_1 +   x_2 +   x_3   <= 20
##               x_1 - 3 x_2 +   x_3   <= 30
##               x_1^2 + x_2^2 + x_3^2 <= 1

