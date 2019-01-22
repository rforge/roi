\name{Example-2}
\title{Hock-Schittkowski-Collection Problem 16}

\description{
The following example solves problem 16 from the Hock-Schittkowski-Collection 
(\url{http://www.ai7.uni-bayreuth.de/test_problem_coll.pdf}).
\deqn{minimize \ f(x) = 100 (x_2 - x_1^2)^2 + (1 - x_1)^2}
\deqn{subject \ to: \ \ x_1 + x_2^2 \geq 0 \ \ \ x_1^2 + x_2 \geq 0}
\deqn{-2 \geq x_1 \geq 0.5 \ \ \ x_2 \geq 1}
Solution: \code{c(0.5, 0.25)}
}

\examples{
Sys.setenv(ROI_LOAD_PLUGINS = FALSE)
library(ROI)
library(ROI.plugin.deoptim)

f <- function(x) {
    return( 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 )
}

f.gradient <- function(x) {
    return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
               200 * (x[2] - x[1] * x[1])) )
}

x <- OP( objective = F_objective(f, n=2L, G=f.gradient), 
         constraints = c(F_constraint(F=function(x) x[1] + x[2]^2, ">=", 0,
                                      J=function(x) c(1, 2*x[2])),
                         F_constraint(F=function(x) x[1]^2 + x[2], ">=", 0,
                                      J=function(x) c(2*x[1], x[2]))),
         bounds = V_bound(li=1:2, ui=1:2, lb=c(-2, -Inf), ub=c(0.5,  1)) )

nlp <- ROI_solve(x, solver="deoptimr", start=c(0.4, 0.3))
nlp
## Optimal solution found.
## The objective value is: 2.499999e-01
solution(nlp)
## [1] 0.5000001 0.2499994
}
