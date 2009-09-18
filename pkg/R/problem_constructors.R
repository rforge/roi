LP <- function(objective, constraints, bounds = NULL, maximum = FALSE){
  structure(list(objective = as.L_objective(objective),
                 constraints = as.L_constraints(constraints), 
                 bounds = bounds,
                 maximum = maximum),
            class = "LP")
}

as.LP <- function(x, ...)
  UseMethod("as.LP")

as.LP.LP <- identity

as.LP.MILP <- function(x){
  LP(objective = x$objective,
     constraints = x$constraints,
     bounds = x$bounds,
     maximum = x$maximum)
}

MILP <- function(objective, constraints, bounds = NULL, types = NULL, maximum = FALSE){
  structure(list(objective = as.L_objective(objective),
                 constraints = as.L_constraints(constraints), 
                 bounds = bounds,
                 types = types,
                 maximum = maximum),
            class = "MILP")
}

as.MILP <- function(x, ...)
  UseMethod("as.MILP")

as.MILP.MILP <- identity

as.MILP.MIQP <- function(x){
  MILP(objective = as.L_objective(x$objective),
       constraints = x$constraints,
       bounds = x$bounds,
       types = types,
       maximum = x$maximum)
}

MIQP <- function(objective, constraints, bounds = NULL, types = NULL, maximum = FALSE){
  structure(list(objective = as.Q_objective(objective),
                 constraints = as.L_constraints(constraints), 
                 bounds = bounds,
                 types = types,
                 maximum = maximum),
            class = "MIQP")
}
  
MIQCP <- function(objective, constraints, bounds = NULL, types = NULL, maximum = FALSE){
  structure(list(objective = as.Q_objective(objective),
                 constraints = as.Q_constraint(constraints), 
                 bounds = bounds,
                 types = types,
                 maximum = maximum),
            class = "MIQCP")
}

MINLP <- function(objective, constraints, bounds = NULL, types = NULL, maximum = FALSE){
  structure(list(objective = as.F_objective(objective),
                 constraints = as.F_constraint(constraints), 
                 bounds = bounds,
                 types = types,
                 maximum = maximum),
            class = "MINLP")
}



## LP
list(objective = list(L = numeric()),
     constraints = list(L = list(mat = matrix(), dir = dir, rhs = numeric())),
     bounds = list(upper = list(i = integer(), v = set()), lower = list(i = integer(), v = set())),
     maximum = logical()
     )

## QP/QCP
list(objective = list(L = numeric(),
                      Q = matrix()),
     constraints = list(L = list(mat, dir, rhs),
                        Q = list(list(list(L = numeric(), Q = matrix())), dir, rhs)),
     bounds = list(upper = list(i = integer(), v = set()), lower = list(i = integer(), v = set())),
     maximum = logical()
     )

             
