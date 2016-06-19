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
con <- L_constraint(rbind(0, -diag(2)), dir=rep("==", 3), rhs=c(1, 0, 0))
op <- OP(obj, con, bounds=as.C_bound(list(soc=list(1:3))), maximum=TRUE)
x <- ROI_solve(op, solver="scs")
x
## Optimal solution found.
## The objective value is: 1.414214e+00
solution(x)
## [1] 0.7071068 0.7071068

}

