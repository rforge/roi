\name{write.lp}
\alias{write.lp}

\title{Write Optimization Problem}
\description{Write a optimization problem to a file.}
\usage{
write.lp(x, file, type=c("lp", "mps", "freemps"))
}
\arguments{
\item{x}{an object of type \code{OP}.}
\item{file}{a character giving the name of the file the optimization problem
            is written to.}
\item{type}{a character giving the name of the file format used to store the 
      optimization problem.}
}
\details{
  The optimization problems can be written to the three file formats \code{"lp"},
  \code{"mps"} and \code{"freemps"}.
  Where it seems important to note that the \code{"lp"} format refers to
  \code{lpsolves} native file format 
  (\url{http://lpsolve.sourceforge.net/5.5/lp-format.htm}) 
  and not to the \code{CPLEX LP format}.
}
\examples{
\dontrun{
mat <- matrix(c(3, 4, 2,
                2, 1, 2,
                1, 3, 2), nrow=3, byrow=TRUE)
x <- OP(objective = c(2, 4, 3),
        constraints = L_constraint(L = mat,
                                   dir = c("<=", "<=", "<="),
                                   rhs = c(60, 40, 80)),
        bounds = V_bound(ui = seq_len(3), ub = c(1000, Inf, 1000), nobj = 3),
        maximum = TRUE)
write.lp(x, "optimization_problem.lp")
}
}
