\name{ROI.plugin.osqp-package}
\docType{package}
\alias{ROI.plugin.osqp-package}

\title{osqp}

\description{
This package provides an interface to \code{OSQP}.
The \code{OSQP} solver is a numerical optimization package or solving convex quadratic programs written in \code{C} and based on the alternating direction method of multipliers.
}

\section{Control Arguments}{
The following description of the control parameters
is mostly copied from the \pkg{osqp} manual.

\itemize{
    \item[] \code{rho}   ADMM step rho
    \item[] \code{sigma}   ADMM step sigma
    \item[] \code{max_iter}   maximum iterations
    \item[] \code{abs_tol}   absolute convergence tolerance
    \item[] \code{rel_tol}   relative convergence tolerance
    \item[] \code{eps_prim_inf}   primal infeasibility tolerance
    \item[] \code{eps_dual_inf}   dual infeasibility tolerance
    \item[] \code{alpha}   relaxation parameter
    \item[] \code{linsys_solver}   which linear systems solver to use, 0=QDLDL, 1=MKL Pardiso
    \item[] \code{delta}   regularization parameter for polish
    \item[] \code{polish}   boolean, polish ADMM solution
    \item[] \code{polish_refine_iter}   iterative refinement steps in polish
    \item[] \code{verbose}   boolean, write out progress
    \item[] \code{scaled_termination}   boolean, use scaled termination criteria
    \item[] \code{check_termination}   integer, check termination interval. If 0, termination checking is disabled
    \item[] \code{warm_start}   boolean, warm start
    \item[] \code{scaling}   heuristic data scaling iterations. If 0, scaling disabled
    \item[] \code{adaptive_rho}   cboolean, is rho step size adaptive?
    \item[] \code{adaptive_rho_interval}   Number of iterations between rho adaptations rho. If 0, it is automatic
    \item[] \code{adaptive_rho_tolerance}   Tolerance X for adapting rho. The new rho has to be X times larger or 1/X times smaller than the current one to trigger a new factorization
    \item[] \code{adaptive_rho_fraction}   Interval for adapting rho (fraction of the setup time)
}
}


\references{
Bartolomeo Stellato and Goran Banjac and Paul Goulart and Alberto Bemporad and Stephen Boyd.
\code{OSQP}: An Operator Splitting Solver for Quadratic Programs
\url{https://arxiv.org/abs/1711.08013}, 2017


Bartolomeo Stellato and Goran Banjac. \code{OSQP} ``webpage''
\url{https://osqp.org/}, 2019
}
