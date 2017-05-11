\name{Example-1}
\title{Multiple Solutions - Binary LP}

\description{
\deqn{maximize \ \ x + y}
\deqn{subject \ to \ \ x + y = 1}
\deqn{ x, y \in \{0, 1\} }
}

\examples{
\dontrun{
library(ROI)
op <- OP(objective = c(1, 1),
         constraints = L_constraint(c(1, 1), "==", 1),
         types = c("B", "B"))

x <- ROI_solve(op, solver = "msbinlp", method = "glpk", nsol_max = 2L)
x
## 2 optimal solutions found.
## The objective value is: 1.000000e+00
solution(x)
## [[1]]
## [1] 1 0
##
## [[2]]
## [1] 0 1
}
}
