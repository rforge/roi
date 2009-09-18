## this is only example code

objective <- function(x, ...)
  UseMethod("objective")

objective.MILP <- function(x)
  x$objective

constraints <- function(x, ...)
  UseMethod("constraints")

constraints.MILP <- function(x)
  x$constraints

LP <-
function(objective, constraints, bounds = NULL, maximum = FALSE)
{
  
  names(constraints) <- c("mat", "dir", "rhs")
  
  structure(list(objective = objective, constraints = constraints(L = constraints),
                 bounds = bounds, types = types, maximum = maximum),
            class = "MILP")
}


is_quadratically_constrained <- function(x) {
  !is.null(constraints(x)$Q)
}

linear_constraints <- function(mat, dir, rhs)
  structure(list(mat = mat, dir = dir, rhs = rhs), 
            class = "linear_constraints")

quadratic_constraint <- function(mat, dir, rhs)
  structure(list(mat = mat, dir = dir, rhs = rhs), 
            class = "quadratic_constraint")

conic_constraint <- function(vec, mat, dir, rhs)
  structure(list(vec, mat = mat, dir = dir, rhs = rhs), 
            class = "conic_constraint")

k_constraint <- function(x)


  
