## ROI test suite

## Configuration
require("ROI")
## solver to check
solver <- "cplex"

## From Rglpk_solve_LP man page

## Example 1:
## Simple linear program.
## maximize:   2 x_1 + 4 x_2 + 3 x_3
## subject to: 3 x_1 + 4 x_2 + 2 x_3 <= 60
##             2 x_1 +   x_2 +   x_3 <= 40
##               x_1 + 3 x_2 + 2 x_3 <= 80
##               x_1, x_2, x_3 are non-negative real numbers
ex1_lp <- OP(objective = c(2, 4, 3),
             constraints = L_constraint(L = matrix(c(3, 2, 1, 4, 1,
                                                     3, 2, 2, 2), nrow = 3),
               dir = c("<=", "<=", "<="),
               rhs = c(60, 40, 80)),
             maximum = TRUE)


res <- ROI_solve( ex1_lp, solver = solver )

solution( res )
solution( res, "aux" )
solution( res, "msg" )

## Example 2:
## Simple mixed integer linear program.
## maximize:    3 x_1 + 1 x_2 + 3 x_3
## subject to: -1 x_1 + 2 x_2 +   x_3 <= 4
##                      4 x_2 - 3 x_3 <= 2
##                x_1 - 3 x_2 + 2 x_3 <= 3
##                x_1, x_3 are non-negative integers
##                x_2 is a non-negative real number

ex2_milp <- OP(objective = c(3, 1, 3),
               constraints = L_constraint(L = matrix(c(-1, 0, 1, 2, 4, -3,
                                          1, -3, 2), nrow = 3),
               dir = c("<=", "<=", "<="),
               rhs = c(4, 2, 3)),
               types = c("I", "C", "I"),
               maximum = TRUE)

res <- ROI_solve( ex2_milp, solver = solver )

solution( res )
solution( res, "aux" )
solution( res, "msg" )

## Example 3:
## MILP same as in Example 2 but with bounds replaced by
## -Inf <  x_1 <= 4
##    0 <= x_2 <= 100
##    2 <= x_3 <  Inf

ex3a_milp <- OP(objective = c(3, 1, 3),
                 constraints = L_constraint(L = matrix(c(-1,  0, 1,  2,
                                                          4, -3, 1, -3, 2),
                                                       nrow = 3),
                                            dir = c("<=", "<=", "<="),
                                            rhs = c(4, 2, 3)),
                 types = c("I", "C", "I"),
                 bounds = V_bound( li = c(1L, 3L),  ui = c(1L, 2L),
                                   lb = c(-Inf, 2), ub = c(4, 100) ),
                 maximum = TRUE)

res <- ROI_solve( ex3a_milp, solver = solver )

solution( res )
solution( res, "aux" )
solution( res, "msg" )

## force negative values in solution
ex3b_milp <- ex3a_milp
## FIXME: sanity check on replacement implemented?
bounds(ex3b_milp) <- V_bound( c(1L, 2L, 3L), c(1L, 2L),
                              c(-Inf, -Inf, 2), c(4, -0.5) )

res <- ROI_solve(ex3b_milp, solver = solver)

solution( res )
solution( res, "aux" )
solution( res, "msg" )

## no boxes
ex3c_milp <-ROI:::as.no_V_bounds_OP(ex3b_milp)

res <- ROI_solve(ex3b_milp, solver = solver)

solution( res )
solution( res, "aux" )
solution( res, "msg" )

## Example 4:
## Simple quadratic program (QP)
## Example from 'quadprog'
## minimize:          - 5 x_2      + 1/2 (x_1^2 + x_2^2 + x_3^2)
## subject to: -4 x_1 - 3 x_2      >= -8
##             2 x_1 +   x_2       >= 2
##                   - 2 x_2 + x_3 >= 0
ex4_qp <- OP( Q_objective (Q = diag(1, 3), L = c(0, -5, 0)),
              L_constraint(L = matrix(c(-4,-3,0,2,1,0,0,-2,1),
                             ncol = 3, byrow = TRUE),
                           dir = rep(">=", 3),
                           rhs = c(-8,2,0)) )
res <- ROI_solve(ex4_qp, solver = solver)

solution( res )
solution( res, "aux" )
solution( res, "msg" )

## Example 5:
## Another QP (this is qpex1.c in the CPLEX examples)
## maximize:     x_1 + 2 x_2 + 3  x_3 - 1/2 (33 x_1^2 + 22 x_2^2 + 11 x_3^2) + 6 x_1 x_2 + 11.5 x_2 x_3
## subject to: - x_1 +   x_2 +    x_3 <= 20
##               x_1 - 3 x_2 +    x_3 <= 30
##

ex5_qp <- OP( Q_objective(Q = matrix(c(-33, 6, 0, 6, -22, 11.5, 0, 11.5, -11),
                        byrow = TRUE, ncol = 3),
                      L = c(1, 2, 3)),
          L_constraint(L = matrix(c(-1, 1, 1, 1, -3, 1),
                         byrow = TRUE, ncol = 3),
                       dir = rep("<=", 2),
                       rhs = c(20, 30)),
         maximum = TRUE)

res <- ROI_solve(ex5_qp, solver = solver)

solution( res )
solution( res, "aux" )
solution( res, "msg" )

## Example 6:
## QP same as in Example 5 but with bounds replaced by (may not make sense)
## -Inf <  x_1 <= 4
##    0 <= x_2 <= 100
##    2 <= x_3 <  Inf
ex6_qp <- ex5_qp
bounds(ex6_qp) <- V_bound( li = c(1L, 3L),  ui = c(1L, 2L),
                          lb = c(-Inf, 2), ub = c(-4, 100) )

res <- ROI_solve(ex6_qp, solver = solver)

solution( res )
solution( res, "aux" )
solution( res, "msg" )

## Example 7:
## QCP (this is qcpex1.c in the CPLEX examples)
## maximize:     x_1 + 2 x_2 + 3 x_3 - 1/2 (33 x_1^2 + 22 x_2^2 + 11 x_3^2) + 6 x_1 x_2 + 11.5 x_2 x_3
## subject to: - x_1 +   x_2 +   x_3   <= 20
##               x_1 - 3 x_2 +   x_3   <= 30
##               1/2 (2 x_1^2 + 2 x_2^2 + 2 x_3^2) <= 1

ex7_qcp <- OP( Q_objective(Q = matrix(c(-33, 6, 0, 6, -22, 11.5, 0, 11.5, -11),
                            byrow = TRUE, ncol = 3),
                          L = c(1, 2, 3)),
              Q_constraint(Q = list(NULL, NULL, diag(2, nrow = 3)),
                           L = matrix(c(-1, 1, 1, 1, -3, 1, 0, 0, 0),
                             byrow = TRUE, ncol = 3),
                           dir = rep("<=", 3),
                           rhs = c(20, 30, 1)),
             maximum = TRUE)

res <- ROI_solve(ex7_qcp, solver = solver)

solution( res )
solution( res, "aux" )
solution( res, "msg" )
