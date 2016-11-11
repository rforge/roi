library(ROI)

eval_f0 <- function( x ) { 
    return( sqrt(x[2]) )
}

## constraint function
g1 <- function( x ) {
    a <- 2; b <- 0 
    return( (a*x[1] + b)^3 - x[2] )
}

g2 <- function( x ) {
    a <- -1; b <- 1 
    return( (a*x[1] + b)^3 - x[2] )
}

g2(x0)

# Define optimal solution.
solution.opt <- c( 1/3, 8/27 )

x <- OP(objective = F_objective(F=eval_f0, n=2L), 
        constraints = F_constraint(F= c(g1, g2), dir=c("<=", "<="), 
                                   rhs=c(0, 0)),
        bounds=V_bound(li=1:2, lb=numeric(2)))

## Solve Rosenbrock Banana function.
control <- list( start = c(1.234, 5.678) )
control <- list( start = c(0.33, 0.29) )
res0 <- ROI_solve( x, solver="solnp", control )
solution(res0)
res0$message

res0 <- ROI_solve( x, solver="solnp", control, dry_run=TRUE )
res0
eval(res0)

ROI_applicable_solvers(x)

control <- list( xtol_rel = 1e-4, start = c( 1.234, 5.678 ) )
control$algorithm <- "NLOPT_LN_COBYLA"
control$xtol_rel <- 1e-6
res0 <- ROI_solve( x, solver="nloptr", control )
solution(res0)

res0 <- ROI_solve( x, solver="nlminb", control )
res0
solution.opt