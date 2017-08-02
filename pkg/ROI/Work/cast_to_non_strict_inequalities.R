
require( "ROI" )
require( "slam" )

## Example:

## Simple mixed integer linear program.
## maximize:    3 x_1 + 1 x_2 + 6 x_3
## subject to: -1 x_1 + 2 x_2 +   x_3 <= 4
##                      4 x_2 - 3 x_3 <= 2
##                x_1         + 2 x_3 < 3
##                x_1, x_3 are non-negative integers
##                x_2 is a non-negative real number

## initialize ROI optimization problem
op <- OP( objective = c(3, 1, 6),
          constraints = L_constraint(L = matrix(c(-1, 0, 1, 2, 4, 0,
                                                  1, -3, 2), nrow = 3),
                                     dir = c("<=", "<=", "<"),
                                     rhs = c(4, 2, 3)),
          types = c("I", "C", "I"),
          maximum = TRUE )

## my installed milp solvers ...
milpsolvers <- names( ROI:::get_solver_methods(ROI:::OP_signature(op)) )

## initialize results objects
milp_results <- data.frame(objval = rep(NA, length.out = length(milpsolvers)),
                           timing = NA)
rownames(milp_results) <- milpsolvers
sol <- matrix( NA, ncol = length(objective(op)), nrow = length(milpsolvers) )
rownames( sol) <- milpsolvers

## run optimizations (as currently implemented)
for(solver in milpsolvers){
  timing <- system.time(res <- ROI_solve(op, solver = solver))["elapsed"]
  milp_results[solver, ] <- c(res$objval, timing)
  sol[solver, ] <- solution(res)
}

milp_results
sol

## check constraints
g <- rowapply_simple_triplet_matrix( constraints(op)$L, function(x) x %*% sol[1,] )
sapply( seq_along(g), function(i) getFunction(constraints(op)$dir[i])(g[i], constraints(op)$rhs[i]) )



cast_to_non_strict_inequalities <- function( x, i ){
    UseMethod( "cast_to_non_strict_inequalities" )
}

cast_to_non_strict_inequalities.L_constraint <- function( x, i ){
    A <- terms(x)$L
    dir <- terms(x)$dir
    strict_ineq <- dir %in% c(">", "<")
    row_valid <- !( 1:nrow(A) %in% A$i[ !A$j %in% i ] ) & strict_ineq

    sense_db <- c(">" = ">=", ">=" = ">=", "<" = "<=", "<=" = "<=")
    if( any(xor(row_valid, strict_ineq)) )
        warning( "some constraints cannot be casted to non-strict inequalities" )

    if( any(row_valid) ){
        B <- A
        B$v <- as.integer( B$v )
        all_coef_integer <- row_sums(A[row_valid, i] - B[row_valid, i]) == 0

        n_ints <- row_sums( A[row_valid, i] != 0 )

        ## the all integer coefficient case
        idx_cast <- all_coef_integer & n_ints > 1
        if( any(idx_cast) ){
            eps <- c(">" = 1, "<" = -1)[x$dir[row_valid][idx_cast]]
            x$rhs[row_valid][idx_cast] <- x$rhs[row_valid][idx_cast] + eps
            x$dir[row_valid][idx_cast] <- sense_db[ x$dir[row_valid][idx_cast] ]
        } else {
            ## the scalar case
            idx_cast <- n_ints == 1
            if( any(idx_cast) ){
                coefs <- row_sums( x$L[row_valid,][idx_cast,] )
                x$dir[row_valid][idx_cast] <- ifelse( coefs >= 0, x$dir[row_valid][idx_cast], c(">" = "<", "<" = ">")[x$dir[row_valid][idx_cast]] )
                x$L[row_valid,][idx_cast,]$v <- 1
                x$rhs[row_valid][idx_cast] <- x$rhs[row_valid][idx_cast]/coefs
                x$dir[row_valid][idx_cast] <- sense_db[ x$dir[row_valid][idx_cast] ]
            } else {
                warning( "some constraints cannot be casted to non-strict inequalities" )
            }
        }
    }
    ## FIXME: convert all remaining row senses to non-strict inequalities here?
    x
}

op_nonstrict <- op

constraints(op_nonstrict) <- cast_to_non_strict_inequalities( constraints(op), which(types(op) == "I") )

## run optimizations (as will be implemented)
for(solver in milpsolvers){
  timing <- system.time(res <- ROI_solve(op_nonstrict, solver = solver))["elapsed"]
  milp_results[solver, ] <- c(res$objval, timing)
  sol[solver, ] <- solution(res)
}

milp_results
sol

## check constraints against original formulation -> OK
g <- rowapply_simple_triplet_matrix( constraints(op)$L, function(x) x %*% sol[1,] )
sapply( seq_along(g), function(i) getFunction(constraints(op)$dir[i])(g[i], constraints(op)$rhs[i]) )
