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

milp <- MILP(objective = c(2, 4, 3),
             constraints = L_constraint(L   = matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3),
                                        dir = c("<=", "<=", "<="),
                                        rhs = c(60, 40, 80)),
             maximum = TRUE)

## Example 2:
## Simple mixed integer linear program.
## maximize:    3 x_1 + 1 x_2 + 3 x_3
## subject to: -1 x_1 + 2 x_2 +   x_3 <= 4
##                      4 x_2 - 3 x_3 <= 2
##                x_1 - 3 x_2 + 2 x_3 <= 3
##                x_1, x_3 are non-negative integers
##                x_2 is a non-negative real number

     obj <- c(3, 1, 3)
     mat <- matrix(c(-1, 0, 1, 2, 4, -3, 1, -3, 2), nrow = 3)
     dir <- c("<=", "<=", "<=")
     rhs <- c(4, 2, 3)
     types <- c("I", "C", "I")
     max <- TRUE

     Rglpk_solve_LP(obj, mat, dir, rhs, types, max)

## Example 3:
## MILP same as in Example 2 but with bounds replaced by
## -Inf <  x_1 <= 4
##    0 <= x_2 <= 100
##    2 <= x_3 <  Inf

     bounds <- list(lower = list(ind = c(1L, 3L), val = c(-Inf, 2)),
                    upper = list(ind = c(1L, 2L), val = c(4, 100)))
     Rglpk_solve_LP(obj, mat, dir, rhs, types, max, bounds)

