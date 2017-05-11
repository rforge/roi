\name{Example-2}
\title{Multiple Solutions - Binary LP}

\description{
\deqn{maximize \ \ - x_1 - x_2 - x_3 - x_4 - 99 x_5 }
\deqn{subject \ to}
\deqn{x_1 + x_2 \leq 1}
\deqn{x_3 + x_4 \leq 1}
\deqn{x_4 + x_5 \leq 1}
\deqn{ x_i \in \{0, 1\} }
}

\examples{
\dontrun{
library(ROI)
op <- OP()
objective(op) <- L_objective(c(-1, -1, -1, -1, -99))
mat <- simple_triplet_matrix(rep(1:3, 2), 
                             c(c(1, 3, 4), c(2, 4, 5)), 
                             rep(1, 6))
constraints(op) <- L_constraint(mat, 
                               dir = leq(3), 
                               rhs = rep.int(1, 3))
types(op) <- rep("B", length(op))

x <- ROI_solve(op, solver = "msbinlp", method = "glpk", nsol_max = 2L)
x 
## 2 optimal solutions found.
## The objective value is: -1.010000e+02
solution(x)
## [[1]]
## [1] 0 1 1 0 1
## 
## [[2]]
## [1] 1 0 1 0 1
}
}

\references{
 Matteo Fischetti and Domenico Salvagnin (2010) \emph{Pruning moves}.
 INFORMS Journal on Computing 22.1: 108-119.
}
