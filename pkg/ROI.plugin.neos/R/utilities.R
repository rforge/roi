
## get_lb
## ======
##
## get lower bound constraints
get_lb <- function(x) {
    lb <- numeric( length(objective(x)) )
    lb[ bounds(x)$lower$ind ] <- bounds(x)$lower$val
    return(lb)
}

## get_ub
## ======
##
## get upper bound constraints
get_ub <- function(x) {
    ub <- rep.int(Inf, length(objective(x)))
    ub[ bounds(x)$upper$ind ] <- bounds(x)$upper$val
    return(ub)
}
