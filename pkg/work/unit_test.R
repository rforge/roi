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
