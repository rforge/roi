q("no")
Rdevel
library(ROI)

attach(getNamespace("ROI.plugin.alabama"))

f <- function(x) {
    return( 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 )
}

f.gradient <- function(x) {
    return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
                200 * (x[2] - x[1] * x[1])) )
}

x <- OP( objective = F_objective(f, n = 2L, G = f.gradient), 
         bounds = V_bound(li=1:2, ui=1:2, lb=c(-3, -3), ub=c(3, 3)) )
    
cntrl <- list(start = c(-2, 2.4), method = "L-BFGS-B")
s <- ROI_solve(x, cntrl, solver =  "alabama")

## c(1, 1)
solution(s)


f <- function(x) {
    return( sum(x) )
}

x <- OP( objective = F_objective(f, n = 2L), 
         constraints = F_constraint(function(x) x[2], "==", 1),
         bounds = V_bound(ld = -2, ud = 2, nobj = 2L ) )

control <- list(par = c(1, 1), method = "L-BFGS-B")    
control <- list(start = c(1, 1), method = "L-BFGS-B")
s <- ROI_solve(x, control, solver =  "alabama")
## c(1, 1)
solution(s)

traceback()



args(optim)



f <- function(x, y, ...) {
    list(...)
}

xcall <- list(f, x = 3, lower = 1:3, upper = 1:5)
mode(xcall) <- "call"
eval(xcall)

xcall <- list(f, lower = 1:3, x = 3 , upper = 1:5)
mode(xcall) <- "call"
eval(xcall)
