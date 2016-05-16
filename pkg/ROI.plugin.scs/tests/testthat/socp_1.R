context("SOCP")

## SOCP - Example - 1
## min:  1 x1 + 1 x2 + 1 x3
## s.t.     x1 == sqrt(2)
##          x1 >= ||(x2, x3)||
##
## c(sqrt(2), -1, -1)

test_that("Example 1", {

    cat("SOCP - Example - 1\n")
    if ( identical(parent.frame(), globalenv()) ) {
        library( testthat )
        library( ROI )
    }

    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0))
    b <- c(sqrt(2))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1), "soc"=list(2:4))
    bound <- as.C_bound(cones)

    x <- OP(objective = obj,
            constraints = L_constraint(L = rbind(A, G), dir=rep("==", length(c(b, h))), rhs = c(b, h)),
            types = rep("C", 3),
            bounds =  bound,
            maximum = FALSE)

    opt <- ROI_solve(x, solver=SOLVER, control=list(eps=1e-12))
    expect_equal( sum(abs(opt$solution - c(sqrt(2), -1, -1))), 0 ) 
    expect_equal( opt$objval, (sqrt(2) - 2) )
     
} )
