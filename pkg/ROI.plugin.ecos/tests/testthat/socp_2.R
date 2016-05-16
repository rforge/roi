context("SOCP")

## SOCP - Example - 2
## min:  0 x1 - 2 x2 - 2 x3 + 0 x4 - 2 x5 - 2 x6
## s.t.     x1 == sqrt(2)
##          x4 == sqrt(2)
##          x1 >= ||(x2, x3)||
##          x4 >= ||(x5, x6)||
##
## c(sqrt(2), 1, 1, sqrt(2), 1, 1)
test_that("Example 2", {

    library( testthat )
    library( ROI )

    obj <- c(0, -2, -2, 0, -2, -2)
    A <- rbind(c(1, 0, 0, 0, 0, 0),
               c(0, 0, 0, 1, 0, 0))
    b <- c(sqrt(2), sqrt(2))
    G <- diag(x=-1, 6)
    h <- rep(0, 6)
    cones <- list("free"=c(1, 2), "soc"=list(3:5, 6:8))
    bound <- as.C_bound(cones)

    x <- OP(objective = obj,
            constraints = L_constraint(L = rbind(A, G), dir=rep("==", length(c(b, h))), rhs = c(b, h)),
            types = rep("C", 6),
            bounds =  bound,
            maximum = FALSE)

    opt <- ROI_solve(x, solver=SOLVER, control=list(eps=1e-12))
    expect_equal( sum(abs(opt$solution - c(sqrt(2), 1, 1, sqrt(2), 1, 1))), 0 )

} )
