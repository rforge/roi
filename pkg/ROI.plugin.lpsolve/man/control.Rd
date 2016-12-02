\name{control}
\alias{control}

\title{Control Variables}
\description{The control variables are all optional, but can in some cases be 
             used to improve the solving time and the solutions.}

\arguments{
\item{basis}{
    an optional named list used to set the initial basis of the \code{LP},
    e.g., \cr
    \code{R> control <- list(basis=list(basis = c(1, 2, 3),} \cr
    \code{+                             nonbasic = TRUE, default = TRUE))}. \cr
    The elements of \code{basis} are passed to the \pkg{lpSolveAPI} function 
    \link[lpSolveAPI]{set.basis} as arguments.
    \itemize{
        \item{\code{basis} a numeric vector giving the indices of the basis.}
        \item{\code{nonbasic} an optional logical, if \code{TRUE} the nonbasic
              variables included in the basis as well.}
        \item{\code{default} an optional logical, if \code{TRUE} the 
              default basis is used and the arguments \code{basis} and 
              \code{nonbasic} are ignored.}
    }
}
\item{branch.mode}{
    an optional list used to set the branch and bound mode, 
    e.g., \cr
    \code{R> control <- list(branch.mode=list(columns=c(1, 2, 3),} \cr
    \code{+                                   modes=c("ceiling", "auto", "floor"))}
    The elements of \code{branch.mode} are passed to \pkg{lpSolveAPI} function 
    \link[lpSolveAPI]{set.branch.mode} as arguments.
    \itemize{
        \item{\code{columns} a vector of integer giving the column indices for
              which the mode is set.}
        \item{\code{modes} a character vector giving the modes of the columns
              specified in \code{columns}.}
    }
}
\item{branch.weights}{
    an optional numeric vector giving the weights for the decision variables.
    The length of the \code{branch.weights} must be equal to length of the
    objective function.
}
\item{verbose}{
    a character string (for more information see \link[lpSolveAPI]{lp.control}).
}
\item{anti.degen}{
    a character vector (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{basis.crash}{
    a character string (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{bb.depthlimit}{
    a integer giving the maximum branch-and-bound depth (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{bb.floorfirst}{
    a character string (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{bb.rule}{
    a character vector giving the branch-and-bound rule (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{break.at.first}{
    a logical controlling whether the branch-and-bound algorithm should be 
    stopped at the first solution or the branch-and-bound algorithm continuous 
    until an optimal solution is found 
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{break.at.value}{
    a numeric, if given the branch-and-bound algorithm stops
    when the objective function becomes smaller than the specified value.
 (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{epslevel}{
    a character string giving the type of thresholds
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{epsb}{
    a numeric giving the tolerance for the right-hand-side 
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{epsd}{
    a numeric 
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{epsel}{
    a numeric
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{epsint}{
    a numeric
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{epsperturb}{
    a numeric
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{epspivot}{
    a numeric
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{improve}{
    a character vector
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{infinite}{
    a numeric
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{maxpivot}{
    a integer
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{mip.gap}{
    a numeric vector
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{negrange}{
    a numeric
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{obj.in.bas}{
    a logical
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{pivoting}{
    a character vector
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{presolve}{
    a character vector
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{scalelimit}{
    a numeric
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{scaling}{
    a character vector
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{simplextype}{
    a character vector which an take one of the following values
    \code{c("primal")}, \code{c("dual")}, \code{c("primal", "dual")} or
    \code{c("dual", "primal")}
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
\item{timeout}{
    a integer giving the number of seconds till a timeout occurs
    (for more information see \code{\link[lpSolveAPI]{lp.control}}).
}
}
