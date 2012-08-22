
ROI_model_mean_variance_portfolio <- function(x, target, minrisk = TRUE, fullinvest = TRUE){
    full_invest <- NULL
    if(fullinvest)
        full_invest <- L_constraint( L = rep(1, ncol(x)), dir = "==", rhs = 1)

    if( minrisk ){
        obj <- Q_objective( Q = cov(as.matrix(x)), L = NULL )
        target_constr <- L_constraint( L = colMeans(x), dir = "==", rhs = target )
    } else {
        obj <- L_objective( L = colMeans(x) )
        target_constr <- Q_constraint( Q = cov(x), L = NULL, dir = "<=", rhs = target^2 )
    }
    OP( objective = obj,
        constraints = rbind(target_constr, full_invest),
        maximum = !minrisk )
}
