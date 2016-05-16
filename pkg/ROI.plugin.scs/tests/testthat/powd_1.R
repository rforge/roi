context("POWD")

## POWD - Example - 1
## min:  u + v + w
## s.t.
##      (u/a)^a * ( v/(1-a) )^(1-a) >= |w|
##      u == 2
##      v == 2
##      a == 1/2
##
## c(2, 2, 4)

test_that("Example 1", {

    cat("POWD - Example - 1\n")
    if ( identical(parent.frame(), globalenv()) ) {
        library( testthat )
        library( ROI )
    }

    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0),
               c(0, 1, 0))
    b <- c(2, 2)
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1, 2), "powd"=list(list(i=3:5, a=0.5)))
    bound <- as.C_bound(cones)

    x <- OP(objective = obj,
            constraints = L_constraint(L = rbind(A, G), dir=rep("==", length(c(b, h))), rhs = c(b, h)),
            types = rep("C", 3),
            bounds =  bound,
            maximum = TRUE)

    opt <- ROI_solve(x, solver=SOLVER, control=list(eps=1e-12))
    expect_equal( opt$solution, c(2, 2, 4) )
      
} )
