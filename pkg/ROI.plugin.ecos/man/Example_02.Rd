\name{Example-2}
\title{SOCP 2}
\description{
The following example is also known as \code{Problem 10} from the 
Hock-Schittkowski-Collection \code{[HOCK]}.

\deqn{minimize \ \ x - y \\}
\deqn{subject \ to \ \ -3 x^2 + 2 x y  + 1 \geq 0}
}

\references{
\code{[HOCK]}  W. Hock, K. Schittkowski (1981): Test Examples for Nonlinear Programming Codes, Lecture Notes in Economics and Mathematical Systems, Vol. 187, Springer
  \url{http://www.ai7.uni-bayreuth.de/tp_coll1.htm}
}

\examples{

library(ROI)
obj <- L_objective(c(1, -1))
L <- chol(rbind(c(3, -1), c(-1, 1)))
con <- L_constraint(rbind(0, -L), dir=rep("==", 3), rhs=c(1, 0, 0))
op <- OP(objective = obj, constraints = con, 
         bounds = c(V_bound(li=1:2, lb=rep(-Inf, 2)), 
                    C_bound(1:3, type="soc")))
x <- ROI_solve(op, solver="ecos")
x
## Optimal solution found.
## The objective value is: -1.000000e+00
solution(x)
## [1] 1.996387e-10 1.000000e+00


}
