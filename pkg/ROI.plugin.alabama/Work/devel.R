args(solnp)

f <- function(x) x
ef <- function(x) x

library(Rsolnp)

args(solnp)

solnp(0, f, LB=-1, UB=1)$pars
solnp(0, f, ef, eqB=0, LB=-1, UB=1)$pars
s$pars
str(s)


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

solnp(x0, eval_f, LB=lb, UB=ub, control=list(trace=FALSE))$pars



f_objective <- function(x) x[1]^2 + x[2]^2
g_constraint <- function(x) {
    c( 1 -   x[1]   - x[2]  ,
       1 -   x[1]^2 - x[2]^2,
       9 - 9*x[1]^2 - x[2]^2,
         -   x[1]^2 + x[2]  ,
             x[1]   - x[2]^2 )
}

start <- c( 3, 1 )
solution.opt <- c( 1, 1 )

ineqlb <- rep(-Inf, 5)
inequb <- rep(0, 5)

solnp(start, f_objective, ineqfun=g_constraint, ineqLB=ineqlb, 
      ineqUB=inequb, control=list(trace=FALSE), LB=c(-50, -50), UB=c(50, 50))$pars

g_con <- function(x) {
    c( x[1] + x[2]  ,
       x[1]^2 + x[2]^2,
     9*x[1]^2 + x[2]^2,
       x[1]^2 - x[2]  ,
      -x[1]   + x[2]^2 )
}

g_constraint(c(0, 0))
g_con(c(0, 0))


ineqlb <- c(1, 1, 9, 0, 0)
inequb <- c(Inf, Inf, Inf, Inf, Inf)

solnp(start, f_objective, ineqfun=g_con, ineqLB=ineqlb, 
      ineqUB=inequb, control=list(trace=FALSE), LB=c(-50, -50), UB=c(50, 50))$pars


control <- list(pars=start)

library(ROI)


g_con <- function(x) {
    c( x[1] + x[2]  ,
       x[1]^2 + x[2]^2,
     9*x[1]^2 + x[2]^2,
       x[1]^2 - x[2]  ,
      -x[1]   + x[2]^2 )
}


F_constraint(F=, dir=rep(">=", 5), rhs=c(1, 1, 9, 0, 0))

c1 <- function(x)   x[1]   + x[2]
c2 <- function(x)   x[1]^2 + x[2]^2
c3 <- function(x) 9*x[1]^2 + x[2]^2
c4 <- function(x)   x[1]^2 - x[2]
c5 <- function(x)  -x[1]   + x[2]^2

f <- F_constraint(F=c1, dir=">=", rhs=1)

fc <- rbind(F_constraint(F=c1, dir=">=", rhs=1),
            F_constraint(F=c2, dir=">=", rhs=1),
            F_constraint(F=c3, dir=">=", rhs=9),
            F_constraint(F=c4, dir=">=", rhs=0),
            F_constraint(F=c5, dir=">=", rhs=0))


length(fc)
fc$F
lapply(fc$f, "[[", )

F <- fc$F
F
F_constraint(F=c1, dir=">=", rhs=1)$F

x <- OP(F_objective(sum, n=2L), fc, bounds=V_bound(li=1:2, lb=c(-50, -50), ui=1:2, ub=c(50, 50)))
x
args(V_bound)



build_equality_constraints(x)