\name{Example-1}
\title{NLP 1}

\description{
The following example solves the Rosenbrock function (\url{https://en.wikipedia.org/wiki/Rosenbrock_function}).
\deqn{minimize \ f(x) = 100 (x_2 - x_1^2)^2 + (1 - x_1)^2}
}

\examples{

library(ROI)

f <- function(x) {
    return( 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 )
}

f.gradient <- function(x) {
    return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
               200 * (x[2] - x[1] * x[1])) )
}

control <- list(algorithm  = "NLOPT_GD_MLSL",
              	maxeval    = 10000,
              	population = 4,
              	local_opts = list(algorithm = "NLOPT_LD_LBFGS", xtol_rel  = 1e-4))

x <- OP( objective = F_objective(f, n=1L, G=f.gradient), 
         bounds = V_bound(li=1:2, ui=1:2, lb=c(-3, -3), ub=c(3,  3)) )

nlp <- ROI_solve(x, solver="nloptr", control, start=c(-1.2, 1))
nlp
## Optimal solution found.
## The objective value is: 3.049556e-23
solution(nlp)
## [1] 1 1
}

