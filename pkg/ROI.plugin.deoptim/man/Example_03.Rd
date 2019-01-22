\name{Example-3}
\title{Hock-Schittkowski-Collection Problem 36}

\description{
The following example solves exmaple 36 from the Hock - Schittkowski 
models (\url{http://apmonitor.com/wiki/index.php/Apps/HockSchittkowski}).
\deqn{minimize \ -x_1 x_2 x_3}
\deqn{subject \ to: \ x_1 + 2 x_2 + x_3 \leq 72}
\deqn{0 \leq x_1 \leq 20, \ 0 \leq x_2 \leq 11, \ 0 \leq x_3 \leq 42}
}

\examples{
Sys.setenv(ROI_LOAD_PLUGINS = FALSE)
library(ROI)
library(ROI.plugin.deoptim)

hs036_obj <- function(x) {
    -x[1] * x[2] * x[3]
}

hs036_con <- function(x) {
    x[1] + 2 * x[2] + 2 * x[3]
}


x <- OP( objective = F_objective(hs036_obj, n = 3L), 
         constraints = F_constraint(hs036_con, "<=", 72),
         bounds = V_bound(ub = c(20, 11, 42)) )

nlp <- ROI_solve(x, solver = "deoptimr", start = c(10, 10, 10), 
                 max_iter = 2000)
nlp
## Optimal solution found.
## The objective value is: -3.300000e+03
solution(nlp, "objval")
## [1] -3300
solution(nlp)
## [1] 20 11 15

}

