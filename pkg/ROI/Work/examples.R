
require("ROI")


## Simple linear program.
## maximize:   2 x_1 + 4 x_2 + 3 x_3
## subject to: 3 x_1 + 4 x_2 + 2 x_3 <= 60
##             2 x_1 +   x_2 +   x_3 <= 40
##               x_1 + 3 x_2 + 2 x_3 <= 80
##               x_1, x_2, x_3 are non-negative real numbers

LP <- OP( c(2, 4, 3),
          L_constraint(L = matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3),
                       dir = c("<=", "<=", "<="),
                       rhs = c(60, 40, 80)),
         max = TRUE )


################################################################################
## linear constraints

## valid input
## simplest linear constraint

L_constraint(1, "<", 4)

## more sophisticated

L_constraint(matrix(c(1, 1), nrow = 2), c("<", "=="), c(5, 4))

L_constraint(matrix(c(1:4), ncol = 2), c("<", "<"), c(4, 5))

rbind( L_constraint(matrix(c(1, 1, 0, 0), nrow = 2), c("<", "=="), c(5, 4)),
       L_constraint(matrix(c(1:4),  ncol = 2), c("<", "<"), c(4, 5)) )

## invalid input

stopifnot( inherits(tryCatch(L_constraint(1, "<", c(5, 4)), error = identity), "error") )

stopifnot( inherits(tryCatch(L_constraint(matrix(c(1, 1), nrow = 2), c("<", "=="), 4), error=identity), "error") )

stopifnot( inherits(tryCatch(L_constraint(matrix(c(1, 1), nrow = 2), c("<", "!="), c(5, 4)), error=identity), "error") )

################################################################################
## quadratic constraints

## valid input
## simplest quadratic constraint

Q_constraint(2, 1, "<", 5)

Q_constraint(matrix(rep(2, 4), ncol = 2), c(1, 2), "<", 5)

################################################################################
## function constraints

f <- function(x){
  sum(x^3) - 1:length(x)%*%x
}

F_constraint(f, "<", 5)

################################################################################
## problem constructors

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


ROI:::OP_signature(ex1_lp)

lpsolvers <- names( ROI:::get_solver_methods(ROI:::OP_signature(ex1_lp)) )
lp_results <- data.frame(objval = rep(NA, length.out = length(lpsolvers)),
                         timing = NA)
rownames(lp_results) <- lpsolvers
## FIXME: symphony not working yet
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

ex2_milp <- OP(objective = c(3, 1, 3),
               constraints = L_constraint(L = matrix(c(-1, 0, 1, 2, 4, -3,
                                          1, -3, 2), nrow = 3),
               dir = c("<=", "<=", "<="),
               rhs = c(4, 2, 3)),
               types = c("I", "C", "I"),
               maximum = TRUE)

ROI:::OP_signature(ex2_milp)

milpsolvers <- names( ROI:::get_solver_methods(ROI:::OP_signature(ex2_milp)) )
milp_results <- data.frame(objval = rep(NA, length.out = length(milpsolvers)),
                           timing = NA)
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

ROI:::OP_signature( ex3a_milp )

for(solver in milpsolvers){
  milp_results[solver, ] <- c(NA, NA)
  timing <- system.time(res <- tryCatch(ROI_solve(ex3a_milp, solver = solver),
                                        error = identity))["elapsed"]
  if(!inherits(res, "error"))
    milp_results[solver, ] <- c(res$objval, timing)
}

milp_results

## force negative values in solution
ex3b_milp <- ex3a_milp
## FIXME: sanity check on replacement implemented?
bounds(ex3b_milp) <- V_bound( c(1L, 2L, 3L), c(1L, 2L),
                              c(-Inf, -Inf, 2), c(4, -0.5) )

for(solver in milpsolvers){
  milp_results[solver, ] <- c(NA, NA)
  timing <- system.time(res <- tryCatch(ROI_solve(ex3b_milp, solver = solver),
                                        error = identity))["elapsed"]
  if(!inherits(res, "error"))
    milp_results[solver, ] <- c(res$objval, timing)
}

milp_results
milp_results_bound <- milp_results

## FIXME: does not work yet
#ex3c_milp <- ROI:::.make_box_constraints_from_bounds_in_MIP(ex3b_milp)

ex3c_milp <-ROI:::as.no_V_bounds_OP(ex3b_milp)

for(solver in milpsolvers){
  milp_results[solver, ] <- c(NA, NA)
  timing <- system.time(res <- tryCatch(ROI_solve(ex3b_milp, solver = solver),
                                        error = identity))["elapsed"]
  if(!inherits(res, "error"))
    milp_results[solver, ] <- c(res$objval, timing)
}

milp_results
stopifnot( all(milp_results$objval  == milp_results_bound$objval) )
## FIXME: as.no_V_bound_OP should make unbounded problem!!!

## Example 4:
## Simple quadratic program (QP)
## Example from 'quadprog'
## minimize:          - 5 x_2      + 1/2 (x_1^2 + x_2^2 + x_3^2)
## subject to: -4 x_1 - 3 x_2      >= -8
##             2 x_1 +   x_2       >= 2
##                   - 2 x_2 + x_3 >= 0
require("ROI")
ex4_qp <- OP( Q_objective (Q = diag(1, 3), L = c(0, -5, 0)),
              L_constraint(L = matrix(c(-4,-3,0,2,1,0,0,-2,1),
                             ncol = 3, byrow = TRUE),
                           dir = rep(">=", 3),
                           rhs = c(-8,2,0)) )
ROI:::OP_signature( ex4_qp )
qpsolvers <- names( ROI:::get_solver_methods(ROI:::OP_signature(ex4_qp)) )
qp_results <- data.frame(objval = rep(NA, length.out = length(qpsolvers)),
                           timing = NA)
rownames(qp_results) <- qpsolvers

## FIXME: QP not working with both solvers
for(solver in qpsolvers){
  qp_results[solver, ] <- c(NA, NA)
  timing <- system.time(res <- tryCatch(ROI_solve(ex4_qp, solver = solver),
                                        error = identity))["elapsed"]
  if(!inherits(res, "error"))
    qp_results[solver, ] <- c(res$objval, timing)
}

qp_results




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

for(solver in qpsolvers){
  qp_results[solver, ] <- c(NA, NA)
  timing <- system.time(res <- tryCatch(ROI_solve(ex5_qp, solver = solver),
                                        error = identity))["elapsed"]
  if(!inherits(res, "error"))
    qp_results[solver, ] <- c(res$objval, timing)
}

qp_results

## Example 6:
## QP same as in Example 5 but with bounds replaced by (may not make sense)
## -Inf <  x_1 <= 4
##    0 <= x_2 <= 100
##    2 <= x_3 <  Inf
ex6_qp <- ex5_qp
bounds(ex6_qp) <- V_bound( li = c(1L, 3L),  ui = c(1L, 2L),
                          lb = c(-Inf, 2), ub = c(-4, 100) )

for(solver in qpsolvers){
  qp_results[solver, ] <- c(NA, NA)
  timing <- system.time(res <- tryCatch(ROI_solve(ex6_qp, solver = solver),
                                        error = identity))["elapsed"]
  if(!inherits(res, "error"))
    qp_results[solver, ] <- c(res$objval, timing)
}

qp_results

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

ROI:::OP_signature( ex7_qcp )

qcpsolvers <- names( ROI:::get_solver_methods(ROI:::OP_signature(ex7_qcp)) )
qcp_results <- data.frame(objval = rep(NA, length.out = length(qcpsolvers)),
                           timing = NA)
rownames(qcp_results) <- qcpsolvers

for(solver in qcpsolvers){
  qcp_results[solver, ] <- c(NA, NA)
  timing <- system.time(res <- tryCatch(ROI_solve(ex7_qcp, solver = solver),
                                        error = identity))["elapsed"]
  if(!inherits(res, "error"))
    qcp_results[solver, ] <- c(res$objval, timing)
}

qcp_results

## Example 8: NLP
## Rosenbrock Banana objective function

eval_f <- function(x) {
    return( 100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2 )
}

eval_grad_f <- function(x) {
    return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
              200 * (x[2] - x[1] * x[1])) )
}

## initial values
x0 <- c( -1.2, 1 )

## lower and upper bounds
lb <- c( -3, -3 )
ub <- c(  3,  3 )

control <- list(start = x0)

ex8_nlp <- OP( objective = F_objective(eval_f, n = 1L, G = eval_grad_f),
               bounds = V_bound(li = 1:2, ui = 1:2, lb = lb, ub = ub) )

ROI:::OP_signature( ex8_nlp )

nlpsolvers <- ROI_applicable_solvers( ex8_nlp )

## Solve Rosenbrock Banana function.
res <- ROI_solve(ex8_nlp, solver = "nlminb", control)

solution( res )
round( objective(ex8_nlp)(solution(res)), 3 )

## Example 9: NLP
## Solve system of equations

## Objective function
eval_f0 <- function( x ) {
    return( 1 )
}

## Gradient of objective function.
eval_grad_f0 <- function( x ) {
    return( 0 )
}

## Equality constraint function.
eval_g0_eq <- function( x, params = c(1, 1, -1) ) {
    return( params[1]*x^2 + params[2]*x + params[3] )
}

## Jacobian of constraint.
eval_jac_g0_eq <- function( x, params = c(1, 1, -1) ) {
    return( 2*params[1]*x + params[2] )
}

## Define vector with addiitonal data.
params <- c(1, 1, -1)

ex9_nlp <- OP( objective = F_objective(F = eval_f0, n = 1L, G = eval_grad_f0),
               constraints = F_constraint(F=eval_g0_eq, dir="==", rhs=0, J=eval_jac_g0_eq),
               bounds = V_bound(1, 1, -Inf, Inf) )

control <- list( start = -5 )

res <- ROI_solve( ex9_nlp, solver = "nlminb", control )

## -1.61803398875
solution( res )

## unbounded QP
zero <- .Machine$double.eps * 100
qo <- Q_objective(Q=rbind(c(1, 0), c(0, 0)), L=c(-2, 1))
lc <- L_constraint(L=matrix(c(1, 0), nrow=1), dir="<=", rhs=3)
op <- OP(qo, lc, maximum = TRUE)
res <- ROI_solve(op, "ipop")
res <- ROI_solve(op, "quadprog")
res <- ROI_solve(op, "cplex")

## Example 10: QP
## Solve the portfolio optimization problem

## portfolio optimization
data( US30 )
r <- na.omit( US30 )
## objective function to minimize
obj <- Q_objective( 2*cov(r) )
## full investment constraint
full_invest <- L_constraint( rep(1, ncol(US30)), "==", 1 )
## create optimization problem / long-only
ex10_qp <- OP( objective = obj, constraints = full_invest )
## solve the problem - only works if a QP solver is registered

ROI:::OP_signature( ex10_qp )
qpsolvers <- ROI_applicable_solvers( ex10_qp )
qp_results <- data.frame(objval = rep(NA, length.out = length(qpsolvers)),
                           timing = NA)
rownames(qp_results) <- qpsolvers

## FIXME: QP not working with both solvers
for(solver in qpsolvers){
  qp_results[solver, ] <- c(NA, NA)
  timing <- system.time(res <- tryCatch(ROI_solve(ex10_qp, solver = solver),
                                        error = identity))["elapsed"]
  if(!inherits(res, "error"))
    qp_results[solver, ] <- c(res$objval, timing)
}

qp_results

res <- ROI_solve( op )
res
## add weight constraint
b <- V_bound( ui = 1:ncol(r), ub = rep(0.25, ncol(r)) )
ex10a_qp <- OP( objective = obj, constraints = c(full_invest),
               bounds = b)

qp_results <- data.frame(objval = rep(NA, length.out = length(qpsolvers)),
                           timing = NA)
rownames(qp_results) <- qpsolvers

## FIXME: QP not working with both solvers
for(solver in qpsolvers){
  qp_results[solver, ] <- c(NA, NA)
  timing <- system.time(res <- tryCatch(ROI_solve(ex10a_qp, solver = solver),
                                        error = identity))["elapsed"]
  if(!inherits(res, "error"))
    qp_results[solver, ] <- c(res$objval, timing)
}

qp_results


## Example 11:
## modified example 2
## Simple mixed integer linear program.
## maximize:    3 x_1 + 1 x_2 + 6 x_3
## subject to: -1 x_1 + 2 x_2 +   x_3 <= 4
##                      4 x_2 - 3 x_3 <= 2
##                x_1         + 2 x_3 <= 3
##                x_1, x_3 are non-negative integers
##                x_2 is a non-negative real number

ex11_milp <- OP(objective = c(3, 1, 6),
               constraints = L_constraint(L = matrix(c(-1, 0, 1, 2, 4, 0,
                                          1, -3, 2), nrow = 3),
               dir = c("<=", "<=", "<="),
               rhs = c(4, 2, 3)),
               types = c("I", "C", "I"),
               maximum = TRUE)

ROI:::OP_signature(ex11_milp)

milpsolvers <- names( ROI:::get_solver_methods(ROI:::OP_signature(ex11_milp)) )
milp_results <- data.frame(objval = rep(NA, length.out = length(milpsolvers)),
                           timing = NA)
rownames(milp_results) <- milpsolvers

sol <- matrix( NA, ncol = length(objective(ex11_milp)), nrow = length(milpsolvers) )
rownames(sol) <- milpsolvers
for(solver in milpsolvers){
  timing <- system.time(res <- ROI_solve(ex11_milp, solver = solver))["elapsed"]
  milp_results[solver, ] <- c(res$objval, timing)
  sol[solver, ] <- solution(res)
}

milp_results
sol

idx_int <- which(types(ex11_milp) == "I") ## FIXME, also "B"?



cast_to_non_strict_inequalities <- function( x, i ){
    UseMethod( "cast_to_non_strict_inequalities" )
}

cast_to_non_strict_inequalities.L_constraint <- function( x, i ){
    A <- terms(x)$L
    dir <- terms(x)$dir
    strict_ineq <- dir %in% c(">", "<")
    row_valid <- !( 1:nrow(A) %in% A$i[ !A$j %in% i ] ) & strict_ineq

    sense_db <- c(">" = ">=", ">=" = ">=", "<" = "<=", "<=" = "<=")
    if( any(xor(row_valid, strict_ineq)) )
        warning( "some constraints cannot be casted to non-strict inequalities" )

    if( any(row_valid) ){
        B <- A
        B$v <- as.integer( B$v )
        all_coef_integer <- row_sums(A[row_valid, i] - B[row_valid, i]) == 0

        n_ints <- row_sums( A[row_valid, i] != 0 )

        ## the all integer coefficient case
        idx_cast <- all_integer & n_ints > 1
        if( any(idx_cast) ){
            eps <- c(">" = 1, "<" = -1)[x$dir[row_valid][idx_cast]]
            x$rhs[row_valid][idx_cast] <- x$rhs[row_valid][idx_cast] + eps
            x$dir[row_valid][idx_cast] <- sense_db[ x$dir[row_valid][idx_cast] ]
        } else {
            ## the scalar case
            idx_cast <- n_ints == 1
            if( any(idx_cast) ){
                coefs <- row_sums( x$L[row_valid,][idx_cast,] )
                x$dir[row_valid][idx_cast] <- ifelse( coefs >= 0, x$dir[row_valid][idx_cast], c(">" = "<", "<" = ">")[x$dir[row_valid][idx_cast]] )
                x$L[row_valid,][idx_cast,]$v <- 1
                x$rhs[row_valid][idx_cast] <- x$rhs[row_valid][idx_cast]/coefs
                x$dir[row_valid][idx_cast] <- sense_db[ x$dir[row_valid][idx_cast] ]
            } else {
                warning( "some constraints cannot be casted to non-strict inequalities" )
            }
        }
    }
    x
}

op <- op_nonstrict <- ex11_milp

constraints(op_nonstrict) <- cast_to_non_strict_inequalities( constraints(op), which(types(op) == "I") )

