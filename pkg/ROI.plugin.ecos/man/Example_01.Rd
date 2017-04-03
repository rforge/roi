\name{Example-1}
\title{SOCP 1}

\description{
\deqn{maximize \ \ x + y}
\deqn{subject \ to \ \ x^2 + y^2 \leq 1}
\deqn{x \geq 0, y \geq 0}
}

\examples{

library(ROI)
obj <- L_objective(c(1, 1))
## NOTE: chol(diag(2)) == diag(2)
con <- C_constraint(L = rbind(0, -diag(2)), 
                    cones = K_soc(3), 
                    rhs = c(1, 0, 0))
op <- OP(obj, con, maximum=TRUE)
x <- ROI_solve(op, solver="ecos")
x
## Optimal solution found.
## The objective value is: 1.414214e+00
solution(x)
## [1] 0.7071068 0.7071068

}

