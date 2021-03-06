% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plugin.R
\name{ROI_plugin_canonicalize_solution}
\alias{ROI_plugin_canonicalize_solution}
\title{Canonicalize Solution}
\usage{
ROI_plugin_canonicalize_solution(
  solution,
  optimum,
  status,
  solver,
  message = NULL,
  ...
)
}
\arguments{
\item{solution}{a numeric or integer vector giving
the solution of the optimization problem.}

\item{optimum}{a numeric giving the optimal value.}

\item{status}{an integer giving the status code (exit flag).}

\item{solver}{a character string giving the name of the solver.}

\item{message}{an optional \R object giving the original solver message.}

\item{...}{further arguments to be stored in the solution object.}
}
\value{
an object of class \code{"OP_solution"}.
}
\description{
Transform the solution to a standardized form.
}
\seealso{
Other plugin functions: 
\code{\link{ROI_plugin_add_status_code_to_db}()},
\code{\link{ROI_plugin_build_equality_constraints}()},
\code{\link{ROI_plugin_build_inequality_constraints}()},
\code{\link{ROI_plugin_get_solver_name}()},
\code{\link{ROI_plugin_make_signature}()},
\code{\link{ROI_plugin_register_solver_control}()},
\code{\link{ROI_plugin_register_solver_method}()},
\code{\link{ROI_plugin_solution_prim}()},
\code{\link{ROI_registered_solver_control}()}
}
\concept{plugin functions}
