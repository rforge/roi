\name{read.lp}
\alias{read.lp}

\title{Read Optimization Problem}
\description{Read a optimization problem from a file.}
\usage{
read.lp(file, type=c("lp", "mps", "freemps"))
}
\arguments{
\item{file}{a character giving the name of the file the optimization problem
            is read from.}
\item{type}{a character giving the name of the file format the optimization
            problem is stored in.}
}
\details{
  The optimization problems can be read from the three file formats \code{"lp"},
  \code{"mps"} and \code{"freemps"}.
  Where it seems important to note that the \code{"lp"} format refers to
  \code{lpsolves} native file format 
  (\url{http://lpsolve.sourceforge.net/5.5/lp-format.htm})
  and not to the \code{CPLEX LP format}.
}
\examples{
\dontrun{
op <- read.lp("optimization_problem.lp")
sol <- ROI_solve(op)
solution(sol)
}
}
