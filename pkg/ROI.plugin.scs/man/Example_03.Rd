\name{Example-3}
\title{SOCP 3}
\description{
The following example is originally from the \code{CVXOPT} 
(\url{http://cvxopt.org/userguide/coneprog.html}) homepage.
\deqn{minimize \ \ -2x_1 + x_2 + 5 x_3}
subject to 
\deqn{
\left\|
\begin{array}{c}
-13 x_1 +  3 x_2 + 5 x_3 - 3 \\
-12 x_1 + 12 x_2 - 6 x_3 - 2
\end{array}
\right\|_2 \leq -12 x_1 - 6 x_2 + 5 x_3 - 12
}
\deqn{
\left\|
\begin{array}{c}
-3 x_1 +  6 x_2 + 2 x_3      \\
   x_1 +  9 x_2 + 2 x_3 +  3 \\
 - x_1 - 19 x_2 + 3 x_3 - 42 
\end{array}
\right\|_2 \leq -3 x_1 + 6 x_2 - 10 x_3 + 27
}
}

\references{
\code{[CVXOPT]}  Andersen, Martin S and Dahl, Joachim and Vandenberghe, Lieven (2016)
  CVXOPT: A Python package for convex optimization, version 1.1.8,
  \url{http://cvxopt.org/}
}

\examples{

library(ROI)
lo <- L_objective(c(-2, 1, 5))
lc1 <- rbind(c(12, 6, -5), c(13, -3, -5), c(12, -12, 6))
lc2 <- rbind(c(3, -6, 10), c(3, -6, -2), c(-1, -9, -2), c(1, 19, -3))
lc <- C_constraint(L = rbind(lc1, lc2), 
                   cones = K_soc(c(3, 4)), 
                   rhs = c(c(-12, -3, -2), c(27, 0, 3, -42)))
vb <- V_bound(li=1:3, lb=rep(-Inf, 3))
op <- OP(objective = lo, constraints = lc, bounds = vb)
x <- ROI_solve(op, solver="scs")
x
## Optimal solution found.
## The objective value is: -3.834637e+01
solution(x)
## [1] -5.014767 -5.766924 -8.521796
}
