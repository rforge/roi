################################################################################
## Creating portfolio optimization problems
################################################################################

##' Generate a portfolio model
##'
##' This function returns the specified portfolio optimization model as
##' a ROI problem object.
##' @param x return time series.
##' @param model a character vector of length 1 specifying the
##' optimization model. Should be of the following form:
##' \code{"foo_bar"}, where "foo" is the direction of optimization and
##' "bar" specifies the objective function. Currently the following
##' models are supported: \code{"min_var", "max_div",
##' "max_cva"}. See Section Details for further
##' information.
##' @param control further arguments passed down to the individual
##' modelling routine. The following list of optional arguments is
##' supported:
##' \itemize{
##'   \item{\code{long_only}} Logical value. If \code{TRUE} each
##'   objective variable has to be in the interval \eqn{[0,\infty)}.
##'   If \code{FALSE} no usage of this sort of constraint. Defaults to
##'   \code{FALSE}. Passed by user or calculated value.
##'   \item{\code{long_short}} Logical value. If \code{TRUE} each
##'   objective variable is allowed to vary in the interval
##'   \eqn{(-\infty,\infty)}.
##'   If \code{FALSE} no usage of this sort of constraint. Defaults to
##'   \code{FALSE}. Passed by user or calculated value.
##'   \item{\code{fully_invest}} Logical value. If \code{TRUE} the sum of
##'   objective variables has to be equal \eqn{1}.
##'   If \code{FALSE} no usage of this sort of constraint. Defaults to
##'   \code{FALSE}. Passed by user or calculated value.
##'   \item{\code{market_neutral}} Logical value. If \code{TRUE} the sum
##'   of objective variables has to be equal \eqn{0}.
##'   If \code{FALSE} no usage of this sort of constraint. Defaults to
##'   \code{FALSE}. Passed by user or calculated value.
##'   \item{\code{n_assets}} The number of assets, i.e. column dimension of
##'   \code{x}. Calculated value.
##'   \item{\code{n_period}} The number of time periods, i.e. row dimension
##'   of \code{x}. Calculated value.
##'   \item{\code{n_objective}} The number of objective values. Typically
##'   the same as \code{n_assets}
##'   except for optimization problems that require dummy variables.
##'   Calculated value.
##'   \item{\code{col_means}} Vector specifying the mean returns for each
##'   asset. If not given the column means of \code{x} are used. Passed by
##'   user or calculated value.
##'   \item{\code{target}} Integer specifying a target minimum return.
##'   If not given no target return has to be achieved. Passed by user.
##'   \item{\code{alpha}} Numeric value specifying the significancy level
##'   \eqn{\alpha} for the CVaR Portfolio.
##'   If not given \code{alpha} is set to \code{0.1}. Only used for
##'   \code{model="max_cva"}. Passed by user or calculated value.
##'   }
##' @param ... currently ignored.
##' @return an object of class \code{"ROI_portfolio_model"} inheriting
##' from class \code{"OP"}.
##'
##' The subclass \code{"ROI_portfolio_model"} extends \code{"OP"} with
##' the following attributes:
##' \itemize{
##' \item{\code{model}} the name of the model used to generate the problem
##' object (reflects the corresponding supplied function argument).
##' \item{\code{control}} the control list as it is used in this function
##' to generate the problem object.
##' \item{\code{asset_names}} names that identify the individual assets
##' (derived from the time series object).
##' \item{\code{meta}} further meta data about the process of creating
##' the problem object like the timestamp and system information.}
##' @details The following models are currently supported.  Note that
##' \code{long_only} and \code{fully_invest} constraints are shown
##' below, but other types of constraints can be used as well.
##'
##' \itemize{
##'   \item{Minimum Variance}{
##'         \deqn{ argmin_w~ \frac{1}{2} w'\Sigma w }{ argmin_w 1/2 w'\Sigma w }
##'         \deqn{s.t.~ w'\iota=1,~w_i\geq 0}{s.t. w'\iota=1, w_i\geq 0}
##'         where,\cr
##'         \eqn{w}{w} is the vector of asset weights,\cr
##'         \eqn{\iota}{\iota} is a vector of \eqn{1s}{1s} and\cr
##'         \eqn{\Sigma}{\Sigma} is the covariance matrix of returns.}
## 	\item{Risk Parity}{
##     	\deqn{w_i=argmin_w~ \sum\limits_{i=1}^{n}\sum\limits_{j=1}^{n}\left[w_i cov(r_i,r_p)-w_j cov(r_j,r_p)\right]^2 }{w_i = argmin_w }
##         \deqn{s.t.~ w'\iota=1,~w_i\geq 0}{}
##			where,\cr
##			\eqn{r_i}{r_i} and \eqn{r_p}{r_p} are the returns of asset \eqn{i} and portfolio \eqn{p}.}
##'		\item{Maximum Diversification}{
##'			\deqn{argmax_w~ \frac{\sum\limits_{i=1}^{n}w_i \sigma_i}{w'\Sigma w}}{}
##'			\deqn{s.t.~ w'\iota=1,~w_i\geq 0}{s.t. w'\iota=1, w_i\geq 0}
##'			where,\cr
##'         \eqn{\sigma_i}{\sigma_i} is the standard deviation of asset \eqn{i}{i}. Numerically the Maximum Diversification Portfolio can be solved
##'			by minimizing \eqn{\psi' P\psi}{\psi' P\psi}, where \eqn{P}{P} is the correlation matrix of the returns. Therefore the
##'			Maximum Diversification optimization is almost the same as in the Minimum Variance case. The final weights are then retrieved by
##'			rescaling the intermediate weight vector \eqn{\psi}{\psi} with the standard deviations of the asset returns.}
##'		\item{Minimum Tail Dependence}{
##'			\deqn{argmin_\psi~ \frac{1}{2}\psi' T\psi}{}
##'			\deqn{s.t.~\psi'\iota=1,~\psi\geq 0}
##'			where,\cr
##'			\eqn{T}{T} is the asset-by-asset tail dependency matrix. The final weights are then retrieved by
##'			rescaling the intermediate weight vector \eqn{\psi}{\psi} with the standard deviations of the asset returns.}
##'			\item{Conditional Value at Risk}{
##'			\deqn{argmin_w~ CVaR_\alpha(w)=\frac{1}{1-\alpha}\int\limits_{f(w,r)\leq VaR_\alpha(w)}f(w,r)p(r)dr}{argmin_w CVaR_\alpha(w)}
##'         \deqn{s.t.~ w'\iota=1,~w_i\geq 0}{s.t. w'\iota=1, w_i\geq 0}
##'			where,\cr
##'			\eqn{r} is the asset return distribution,\cr
##'			\eqn{p(r)} the corresponding density function,\cr
##'			\eqn{f(w,r)} is a given loss function and\cr
##'			\eqn{VaR_\alpha(w)} is the Value at Risk of the weighted portfolio.\cr
##'			The key is to approximate the continuous joint density function \eqn{p(r)} with a number of discrete scenarios,
##'			e.g. \eqn{r_s} for \eqn{s=1,...,S}, which typically represent historical returns. Then the optimization problem transforms to
##'			\deqn{argmin_w~ VaR_\alpha+\frac{1}{(1-\alpha S)}\sum\limits_{s=1}^{S}(f(w,r_s)-VaR_\alpha)^+.}
##'			Typically it is assumend that \eqn{f(w,r_s)} is a linear function of \eqn{w}.
##'			Therefore the CVaR optimization can be solved by linear optimization algorithms.}
##' }
##' @export
##' @import ROI
##' @import slam
##' @examples
##' ## daily returns (discrete) of 30 US stocks from 2008-03-20 to 2013-12-31
##' data( US30 )
##' r <- na.omit( US30 / lag(US30, 1, na.pad = TRUE) - 1 )
##'
##' ## generate and solve the "maximum diversification" optimization problem
##' MD  <- ROI_model_portfolio( r, model = "max_div", control = list(long_only = TRUE,
##'                                                                  fully_invest = TRUE) )
##' sol <- ROI_solve( MD, solver = "quadprog" )
##' w_md <- round( sol$solution, 5 )
##'
##' ## generate and solve the "minimum variance" optimization problem
##' MV  <- ROI_model_portfolio( r, model = "min_var", control = list(long_only = TRUE,
##'                                                                  fully_invest = TRUE) )
##' sol <- ROI_solve( MV, solver = "quadprog" )
##' w_mv <- round( sol$solution, 5 )
##'
##' ## generate and solve the "conditional value at risk" optimization problem
##' CV  <- ROI_model_portfolio( r, model = "max_cva", control = list(long_only = TRUE,
##'                                                                  fully_invest = TRUE,
##'                                                                  alpha = 0.1) )
##' sol <- ROI_solve( CV, solver = "glpk" )
##' w_cv <- round( sol$solution[1:ncol(r)], 5 )
ROI_model_portfolio <- function( x, model = c("min_var", "max_div", "max_cva"),
                                 control = list(), ... ){

    ## validate input
    model <- match.arg( model )

    ## control list handling
    ## FIXME: need to document that these are reserved control arguments
    control$n_assets <- ncol( x )
    control$n_periods <- nrow( x )
    ## for target return constraints user may supply "mean
    ## return". Default: arithmetic mean on the asset returns.
    if( is.null(control$col_means) )
        control$col_means <- apply( x, 2, mean, na.rm=TRUE )

    ## build objective function and model specific constraints
    FUN <- getFunction( paste(".make_op", model, sep = "_") )
    op <- FUN( x, control )

    ## FIXME: need to document that this is a reserved control argument
    control$n_objective <- length( objective(op) )

    ## make model invariant contstraints, e.g. non-negativity
    ## value: a list with two elements
    ##        o constr - general constraints,
    ##        o bounds - box constraints
    constr <- make_constraints_from_control( control = control )

    ## add constraints to OP
    if( length(constr$constr) ){
        ## FIXME: we currently support only L_constraints, rbind does not
        ## work otherwise since it demands that every input object must be
        ## of the same class (really??)
        stopifnot( is.L_constraint(constr$constr) )
        constraints(op) <- rbind( as.L_constraint(constraints(op)), constr$constr )
    }

    if( length(constr$bounds) )
    bounds(op) <- constr$bounds

    ## Subclass "ROI_model_portfolio" extends "OP". See documentation.
    structure( op, class= c("ROI_model_portfolio", class(op)),
               model = model,
               control = control,
               asset_names = colnames(x),
               meta = list(timestamp = Sys.time(), info = Sys.info() ) )
}



################################################################################
## Methods on class "ROI_model_portfolio"
################################################################################

##' @S3method print ROI_model_portfolio
print.ROI_model_portfolio <- function( x, ... ){
    asset_names <- attributes(x)$asset_names
    asset_names <- ifelse( length(asset_names) > 5,
                          c(asset_names[1:3], "...",
                            asset_names[length(asset_names)]), asset_names )
    writeLines( sprintf("A %s portfolio model on the following assets:\n%s",
                        attributes(x)$model,
                        paste(asset_names, collapse = ", ")) )
  writeLines( sprintf("Model: %s\n", attributes(x)$model) )
  NextMethod(x)
}



################################################################################
## Sanity checks
################################################################################

.check_control_for_sanity <- function( control ){

  if( !control$long_only && !control$long_short ){
  	warning( "no constraints on individual weights added." )
	}else if( control$long_only && control$long_short ){
		stop( "long_only and long_short constraints selected. Please choose one of them." )
  }

  if( !control$fully_invest && !control$target_invest && !control$market_neutral ){
  	warning( "no constraints on sum of weights added. Fully investment chosen on default." )
	}else if( length(which(c(control$fully_invest,control$target_invest,control$market_neutral)))>1 ){
    stop( "too many constraints on sum of weight. Please choose one of them." )
	}

}



################################################################################
## Constraint creator
################################################################################

make_constraints_from_control <- function( control ){

  ## reformat control arguments to logical values if they are NULL
  control$long_only     <- ifelse( is.logical(control$long_only), control$long_only, FALSE )
  control$long_short    <- ifelse( is.logical(control$long_short), control$long_short, FALSE )
  control$fully_invest  <- ifelse( is.logical(control$fully_invest), control$fully_invest, FALSE )
  control$target_invest <- ifelse( is.logical(control$target_invest), control$target_invest, FALSE )
  control$market_neutral<- ifelse( is.logical(control$market_neutral), control$market_neutral, FALSE )

  ## check for sanity
  .check_control_for_sanity( control )

  ## constraints on each weight
  A <- dir <- rhs <- bnds <- NULL

  if( control$long_only ){
      A   <- rbind( A, slam::simple_triplet_diag_matrix(1, control$n_assets) )
      dir <- c( dir, rep(">=",control$n_assets) )
      rhs <- c( rhs, rep(0,control$n_assets) )
  }

  if( control$long_short ){
      bnds <- V_bound( li   = 1:control$n_assets,
                       ui   = 1:control$n_assets,
                       lb   = rep(-Inf,control$n_assets),
                       ub   = rep(Inf,control$n_assets),
                       nobj = control$n_assets )
  }

  ## constraints on sum of weights
  A <- rbind( A,
              slam::simple_triplet_matrix(i = rep(1, control$n_assets),
                                          j = 1:control$n_assets,
                                          v = rep(1, control$n_assets)) )
  dir  <- c( dir, "==" )
  rhs  <- c( rhs, ifelse(control$market_neutral, 0, 1) )

  ## add target minimum return if specified via model
  if( !is.null(control$target) ){
      A <- rbind( A,
                  slam::simple_triplet_matrix(i = rep(1,control$n_assets),
                                              j = 1:control$n_assets,
                                              v = control$col_means) )
      dir <- c( dir, ">=" )
      rhs <- c( rhs, as.numeric(control$target) )
  }

  if( !is.null(A) ){
  	## NOTE: if objective function has more variables than assets,
    ## we need fill up (with zero coefficients) the contraints object
    ## such that it fullfills dimensionality requirements
      if( control$n_objective > dim(A)[2] )
          A <- cbind( A,
                      slam::simple_triplet_zero_matrix(nrow = dim(A)[1],
                                                       ncol=control$n_objective - dim(A)[2]) )
  }

  list( bounds = bnds, constr = L_constraint(L = A, dir, rhs) )
}



################################################################################
## OP creators
################################################################################

## Minimum variance Portfolio
## Author: Theussl
.make_op_min_var <- function( x, control ) {
    COV <- if( !is.null(control$COV) )
        control$COV
    else
        stats::cov

    OP( objective = Q_objective(COV(x)) )
}

## Risk parity portfolio
## Author: Zebhauser
## FIXME: need new ROI release which has F_objective sanity check fixed
.make_op_min_rpa <- function ( x, control ) {
    COV <- if( !is.null(control$COV) )
        control$COV
    else
        stats::cov

    n_assets <- control$n_assets

    SIGMA <- COV( x )
    start  <- rep( 1/n_assets, n_assets)

    f <- function( w, SIGMA ) {
        pr <- sqrt( t(w) %*% SIGMA %*% w )
        mrc <- c( w * SIGMA %*% w ) / pr
        sd( mrc )
    }

    OP( F_objective(f, n_assets) )
}

## Maximimum Diversification
## Author: Zebhauser
.make_op_max_div <- function( x, control ) {
    if( !is.null(control$COR) ){
        COR <- control$COR}
    else{
        COR <- stats::cor}

    OP( objective = Q_objective(COR(x)) )
}

## Minimum Tail Dependence
## Author: Zebhauser
## .make_op_min_tdp <- function( x, control ) {
##     if( !is.null(control$TD) ){
##         TD_base <- control$TD}
##     else{
##         ## FIXME: can we define a default in our package?
##         TD_base <- <REPLACE_WITH_PROPER_PKG>::tdc}
##     TD <- TD_base

##     ## alternative minimum tail dep (change: st)
##     if( !is.null(control$diag_var) )
##         if( control$diag_var )
##             TD <- function( x ) {
##                 out <- TD_base( x )
##                 diag( out ) <- apply( x, 2, var )
##                 out
##             }

##     OP( objective = Q_objective(TD(x)) )
## }

## Conditional Value at Risk
## Author: Kopatz
.make_op_max_cva <- function( x, control ) {
    if( !is.null(control$alpha) ){
        alpha <- control$alpha
    } else {
        alpha <- 0.10
    }

    n_assets <- control$n_assets
    n_periods <- control$n_periods

    ## CVaR constraints
    A    <- cbind( matrix(coredata(x),
                          ncol = ncol(x)),
                  slam::simple_triplet_diag_matrix(1, n_periods),
                  slam::simple_triplet_matrix(i = 1:n_periods,
                                              j = rep(1, n_periods),
                                              v = rep(1,n_periods)) )
    dir <- rep( ">=", n_periods )
    rhs <- rep( 0, n_periods )

    objL = c( rep(0, n_assets), rep(-1/(alpha*n_periods), n_periods), -1 )

    OP( objective   = objL,
        constraints = L_constraint(L = A, dir = dir, rhs = rhs),
        maximum     = TRUE )
}

