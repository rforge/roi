## ROI.plugin.cplex: helper functions
## Description: provides several helper functions

## UTILITIES
.as_Rcplex_sense <- function(x) {
  TABLE <- c("L", "L", "G", "G", "E")
  names(TABLE) <- c("<", "<=", ">", ">=", "==")
  TABLE[x]
}

.all_zero_in_simple_triplet_matrix_or_NULL <- function(x){
  if( is.null(x) )
    return(TRUE)
  stm <- as.simple_triplet_matrix(x)
  if( ! length(stm$v) )
    return(TRUE)
  FALSE
}

.make_list_of_linear_constraints <- function(x){
  if( is.null(x) )
    return(list(NULL))
  stm <- as.simple_triplet_matrix(x)
  apply(x, 1, function(x) as.simple_triplet_matrix(matrix(x, nrow = 1)))
}

as.mosek_triplet_matrix <- function( x )
    UseMethod("as.mosek_triplet_matrix")

as.mosek_triplet_matrix.simple_triplet_matrix <- function( x )
    unclass( x )

