stopifnot(require(alabama))


library(ROI)
library(ROI.plugin.alabama)

check <- function(domain, condition, level=1, message="", call=sys.call(-1L)) {
    if ( isTRUE(condition) ) return(invisible(NULL))
    msg <- sprintf("in %s", domain)
    if ( all(nchar(message) > 0) ) msg <- sprintf("%s\n\t%s", msg, message)
    stop(msg)
    return(invisible(NULL))
}

test_nlp_01 <- function() {
    f <- function(x) {
        return( 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 )
    }

    f.gradient <- function(x) {
        return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
                    200 * (x[2] - x[1] * x[1])) )
    }

    x <- OP( objective = F_objective(f, n=2L, G=f.gradient), 
             bounds = V_bound(li=1:2, ui=1:2, lb=c(-3, -3), ub=c(3, 3)) )

    if ( length(ROI_applicable_solvers(x)) ) {
        nlp <- ROI_solve(x, start=c(-2, 2.4))
        stopifnot( equal(nlp$objval, 0) )
        stopifnot( equal(solution(nlp), c(1, 1)) )
    }
}

test_nlp_02 <- function() {
    f <- function(x) {
        return( 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 )
    }

    f.gradient <- function(x) {
        return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
                   200 * (x[2] - x[1] * x[1])) )
    }

    x <- OP( objective = F_objective(f, n=2L, G=f.gradient), 
             constraints = c(F_constraint(F=function(x) x[1] + x[2]^2, ">=", 0,
                                          J=function(x) c(1, 2*x[2])),
                             F_constraint(F=function(x) x[1]^2 + x[2], ">=", 0,
                                          J=function(x) c(2*x[1], x[2]))),
             bounds = V_bound(li=1:2, ui=1:2, lb=c(-2, -Inf), ub=c(0.5,  1)) )

    solver <- "alabama"
    if ( length(solver) ) {
        nlp <- ROI_solve(x, solver = solver[1L], start = c(-2, 1))
        stopifnot( equal(nlp$objval, 1/4) )
        stopifnot( equal(solution(nlp), c(1/2, 1/4)) )
    }
}

## SOURCE: Rglpk manual
## https://cran.r-project.org/web/packages/Rglpk/Rglpk.pdf
## 
## LP - Example - 1
## max:  2 x_1 + 4 x_2 + 3 x_3
## s.t.
## 3 x_1  +  4 x_2  +  2 x_3  <= 60
## 2 x_1  +    x_2  +  2 x_3  <= 40
##   x_1  +  3 x_2  +  2 x_3  <= 80 
## x_1, x_2, x_3 >= 0
test_nlp_03 <- function() {
    ## -----------------------------------------------------
    ## Test transformation from LP to NLP
    ## -----------------------------------------------------
    mat <- matrix(c(3, 4, 2, 2, 1, 2, 1, 3, 2), nrow=3, byrow=TRUE)
    lo <- L_objective(c(2, 4, 3))
    lc <- L_constraint(L = mat, dir = c("<=", "<=", "<="), rhs = c(60, 40, 80))
    lp <- OP(objective = lo, constraints = lc, maximum = TRUE)
    opt.solution <- c(0, 6.66666666666667, 16.6666666666667)

    nlp_opt <- ROI_solve(lp, solver="alabama", start=c(1, 1, 1))
    
    stopifnot( equal(solution(nlp_opt), opt.solution) )
}

## This test detects if each solver is using the same definition
## for quadratic constraints.
## minimize:    0.5 * (x^2 + y^2)
## subject to:  0.5 * x^2 >= 0.5
##      x, y >= 0
## solution <- c(1, 0)
test_qcqp_01 <- function() {
    qo <- Q_objective(Q = diag(2), L =  numeric(2))
    qc <- Q_constraint(rbind(c(1, 0), c(0, 0)), c(0, 0), dir=">=", rhs=0.5)
    x <- OP(qo, qc)

    opt <- ROI_solve(x, solver="alabama", start=c(3, 3))
   
    ## local_opts <- list( algorithm = "NLOPT_LD_LBFGS", xtol_rel  = 1e-4 )
    ## opt <- ROI_solve(x, solver="nloptr", start=c(2, 2), method="NLOPT_LD_MMA")

    stopifnot( equal(solution(opt), c(1, 0)) )
    stopifnot( equal(opt$objval, 0.5) )
}

if ( !any("alabama" %in% names(ROI_registered_solvers())) ) {
    ## This should never happen.
    cat("ROI.plugin.alabama cloud not be found among the registered solvers.\n")
} else {
    file = Sys.getenv("ROI_TEST_LOG_FILE")
    ROI_TEST_ERRORS <- 0L
    rt <- function(expr, silent = FALSE) {
        err <- try(expr, silent = silent)
        if ( inherits(err, "try-error") ) 
            ROI_TEST_ERRORS <<- ROI_TEST_ERRORS + 1L
        err
    }

    rt( test_nlp_01() )
    rt( test_nlp_02() )
    rt( test_nlp_03() )
    rt( test_qcqp_01() )

    if ( ROI_TEST_ERRORS > 0 ) {
        stop("ROI_Test_Error ", ROI_TEST_ERRORS, " occurred during testing.")
    }
}
