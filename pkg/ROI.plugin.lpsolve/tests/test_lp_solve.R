library(ROI)
library(ROI.plugin.lpsolve)

check <- function(domain, condition, level=1, message="", call=sys.call(-1L)) {
    if ( isTRUE(condition) ) return(invisible(NULL))
    msg <- sprintf("in %s", domain)
    if ( all(nchar(message) > 0) ) msg <- sprintf("%s\n\t%s", msg, message)
    stop(msg)
    return(invisible(NULL))
}

## LP - Example - 1
## max:  2 x_1 + 4 x_2 + 3 x_3
## s.t.
## 3 x_1  +  4 x_2  +  2 x_3  <= 60
## 2 x_1  +    x_2  +  2 x_3  <= 40
##   x_1  +  3 x_2  +  2 x_3  <= 80 
## x_1, x_2, x_3 >= 0
test_lp_01 <- function(solver) {
    mat <- matrix(c(3, 4, 2,
                    2, 1, 2,
                    1, 3, 2), nrow=3, byrow=TRUE)
    x <- OP(objective = c(2, 4, 3),
            constraints = L_constraint(L = mat,
                                       dir = c("<=", "<=", "<="),
                                       rhs = c(60, 40, 80)),
            bounds = V_bound(ui = seq_len(3), ub = c(1000, Inf, 1000), nobj = 3),
            maximum = TRUE)

    opt <- ROI_solve(x, solver = solver)
    check("LP-01@01", equal(opt$solution, c(0, 20/3, 50/3), tol=1e-4))
    check("LP-01@02", equal(opt$objval, 230/3, tol=1e-4))


    x <- OP(objective = c(2, 4, 3),
        constraints = L_constraint(L = mat,
                                   dir = c("<=", "<=", "<="),
                                   rhs = c(60, 40, 80)),
        maximum = TRUE)

    opt <- ROI_solve(x, solver = solver)
    check("LP-01@01", equal(opt$solution, c(0, 20/3, 50/3), tol=1e-4))
    check("LP-01@02", equal(opt$objval, 230/3, tol=1e-4))
}

## Test if ROI can handle empty constraint matrix.
test_lp_02 <- function(solver) {
    x <- OP(objective = c(2, 4, 3),
            constraints = L_constraint(L=matrix(0, nrow=0, ncol=3), 
                                       dir=character(), rhs=double()),
            maximum = FALSE)

    opt <- ROI_solve(x, solver = solver)
    check("LP-02@01", equal(opt$solution, c(0, 0, 0), tol=1e-4))
    check("LP-02@02", equal(opt$objval, 0, tol=1e-4))
}

## Test if ROI can handle when the constraint is equal to NULL.
test_lp_03 <- function(solver) {
    x <- OP(objective = c(2, 4, 3), constraints = NULL, maximum = FALSE)

    opt <- ROI_solve(x, solver = solver)
    check("LP-03@03", equal(opt$solution, c(0, 0, 0), tol=1e-4))
    check("LP-03@03", equal(opt$objval, 0, tol=1e-4))
}

if ( !any("lpsolve" %in% names(ROI_registered_solvers())) ) {
    ## This should never happen.
    cat("ROI.plugin.lpsolve cloud not be found among the registered solvers.\n")
} else {
    print("Start Testing!")
    solver <- "lpsolve"
    local({test_lp_01(solver)})
    local({test_lp_02(solver)})
    local({test_lp_03(solver)})
}

