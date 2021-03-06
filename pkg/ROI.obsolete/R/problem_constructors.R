###############################################################
## Linear program (LP)

LP <- function(objective, constraints, bounds = NULL, maximum = FALSE){
  structure(list(objective = as.L_objective(objective),
                 constraints = as.L_constraint(constraints),
                 bounds = bounds,
                 maximum = maximum),
            class = "LP")
}

as.LP.LP <- identity

as.LP.MILP <- function(x){
  LP( objective = as.L_objective(objective(x)),
      constraints = constraints(x),
      bounds = x$bounds,
      maximum = x$maximum)
}

is.LP <- function( x ) {
    inherits( x, "LP" )
}

print.LP <- function(x, ...){
    types <- c(L_constraint = "linear", Q_constraint = "quadratic", F_constraint = "nonlinear" )
    writeLines( sprintf("A linear programming problem with %d constraints of type %s.", length(constraints(x)),
                        paste(na.omit(types[class(constraints(x))]), collapse = ", ")) )
}
###############################################################
## Quadratic program (QP)

QP <- function( objective, constraints, bounds = NULL, maximum = FALSE ) {
  structure(list(objective = as.Q_objective(objective),
                 constraints = as.L_constraint(constraints),
                 bounds = bounds,
                 maximum = maximum),
            class = "QP")
}

as.QP.QP <- identity

as.QP.MIQP <- function(x){
  LP( objective = as.Q_objective(objective(x)),
      constraints = constraints(x),
      bounds = x$bounds,
      maximum = x$maximum)
}

is.QP <- function( x ) {
  inherits( x, "QP" )
}

print.QP <- function(x, ...){
    types <- c(L_constraint = "linear", Q_constraint = "quadratic", F_constraint = "nonlinear" )
    writeLines( sprintf("A quadratic programming problem with %d constraints of type %s.", length(constraints(x)),
                        paste(na.omit(types[class(constraints(x))]), collapse = ", ")) )
}

###############################################################
## Mixed integer quadratically constraint  program (MIQCP)

QCP <- function(objective, constraints, bounds = NULL, maximum = FALSE){
  structure(list(objective = as.Q_objective(objective),
                 constraints = as.Q_constraint(constraints),
                 bounds = bounds,
                 maximum = maximum),
            class = "QCP")
}

is.QCP <- function( x ) {
  inherits( x, "QCP" )
}

print.QCP <- function(x, ...){
    types <- c(L_constraint = "linear", Q_constraint = "quadratic", F_constraint = "nonlinear" )
    writeLines( sprintf("A quadratic programming problem with %d constraints of type %s.", length(constraints(x)),
                        paste(na.omit(types[class(constraints(x))]), collapse = ", ")) )
}

###############################################################
## Mixed integer linear program (MILP)

MILP <- function(objective, constraints, bounds = NULL, types = NULL, maximum = FALSE){
  structure(list(objective = as.L_objective(objective),
                 constraints = as.L_constraint(constraints),
                 bounds = bounds,
                 types = types,
                 maximum = maximum),
            class = "MILP")
}

as.MILP.MILP <- identity

as.MILP.MIQP <- function(x){
  MILP(objective = as.L_objective(objective(x)),
       constraints = constraints(x),
       bounds = x$bounds,
       types = x$types,
       maximum = x$maximum)
}

is.MILP <- function( x ) {
  inherits( x, "MILP" )
}

print.MILP <- function(x, ...){
    types <- c(L_constraint = "linear", Q_constraint = "quadratic", F_constraint = "nonlinear" )
    writeLines( sprintf("A mixed integer linear programming problem with %d constraints of type %s.", length(constraints(x)),
                        paste(na.omit(types[class(constraints(x))]), collapse = ", ")) )
}

###############################################################
## Mixed integer quadratic program (MIQP)

MIQP <- function(objective, constraints, bounds = NULL, types = NULL, maximum = FALSE){
  structure(list(objective = as.Q_objective(objective),
                 constraints = as.L_constraint(constraints),
                 bounds = bounds,
                 types = types,
                 maximum = maximum),
            class = "MIQP")
}

is.MIQP <- function( x ) {
  inherits( x, "MIQP" )
}

print.MIQP <- function(x, ...){
    types <- c(L_constraint = "linear", Q_constraint = "quadratic", F_constraint = "nonlinear" )
    writeLines( sprintf("A mixed integer quadratic programming problem with %d constraints of type %s.", length(constraints(x)),
                        paste(na.omit(types[class(constraints(x))]), collapse = ", ")) )
}

###############################################################
## Mixed integer quadratically constraint  program (MIQCP)

MIQCP <- function(objective, constraints, bounds = NULL, types = NULL, maximum = FALSE){
  structure(list(objective = as.Q_objective(objective),
                 constraints = as.Q_constraint(constraints),
                 bounds = bounds,
                 types = types,
                 maximum = maximum),
            class = "MIQCP")
}

is.MIQCP <- function( x ) {
  inherits( x, "MIQCP" )
}

print.MIQCP <- function(x, ...){
    types <- c(L_constraint = "linear", Q_constraint = "quadratic", F_constraint = "nonlinear" )
    writeLines( sprintf("A mixed integer quadratic programming problem with %d constraints of type %s.", length(constraints(x)),
                        paste(na.omit(types[class(constraints(x))]), collapse = ", ")) )
}

###############################################################
## Mixed integer nonlinear program (MINLP)

MINLP <- function(objective, constraints, bounds = NULL, types = NULL, maximum = FALSE){
  structure(list(objective = as.F_objective(objective),
                 constraints = as.F_constraint(constraints),
                 bounds = bounds,
                 types = types,
                 maximum = maximum),
            class = "MINLP")
}

is.MINLP <- function( x ) {
  inherits( x, "MINLP" )
}

## class structure:
## LP
##list(objective = list(L = numeric()),
##     constraints = list(L = list(mat = matrix(), dir = dir, rhs = numeric())),
##     bounds = list(upper = list(i = integer(), v = set()), lower = list(i = integer(), v = set())),
##     maximum = logical()
##     )

## QP/QCP
##list(objective = list(L = numeric(),
##                      Q = matrix()),
##     constraints = list(L = list(mat, dir, rhs),
##                        Q = list(list(list(L = numeric(), Q = matrix())), dir, rhs)),
##     bounds = list(upper = list(i = integer(), v = set()), lower = list(i = integer(), v = set())),
##     maximum = logical()
##     )


