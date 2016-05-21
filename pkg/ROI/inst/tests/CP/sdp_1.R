context("SDP")

print("TODO!")

## The following example is from the cvxopt documentation and released under GPL-3
## (c) 2016-2016 Florian Schwendinger
## (c) 2012-2015 M. Andersen and L. Vandenberghe.
## CVXOPT is free software; you can redistribute it and/or modify it under the terms of the 
## GNU General Public License as published by the Free Software Foundation; 
## either version 3 of the License, or (at your option) any later version.

## SDP - Example - 1
## for the example definition see ROI.plugin.scs inst/doc
## or http://cvxopt.org/userguide/coneprog.html

test_that("Example 1", {

    library( testthat )
    library( ROI )

    obj <- c(-6, -4, -5)
    G <- cbind(c( 16, 7,  24,  -8,   8, -1, 0, -1,  0,  0, 7, -5,  1, -5,  1,  -7,  1,  -7,  -4),
               c(-14, 2,   7, -13, -18,  3, 0,  0, -1,  0, 3, 13, -6, 13, 12, -10, -6, -10, -28),
               c(  5, 0, -15,  12,  -6, 17, 0,  0,  0, -1, 9,  6, -6,  6, -7,  -7, -6,  -7, -11))
    h <- c(-3, 5, 12, -2, -14, -13, 10, 0, 0, 0, 68, -30, -19, -30, 99, 23, -19, 23, 10)
##    dims <- c(C_bound(1:3, type="free"),  C_bound(4, 5, type="nonneg"), C_bound(6:9, type="soc"), 
##              C_bound(10:13, type="soc"), C_bound(14:19, type="psd"))

    dims <- c(C_bound(1:6, type="free"), C_bound(7, 8, type="nonneg"), C_bound(9:12, type="soc"), 
              C_bound(13:16, type="soc"), C_bound(17:19, type="psd"))

    length(14:19)

    x <- OP(objective = obj,
            constraints = L_constraint(L = G, dir=rep("==", length(h)), rhs = h),
            types = rep("C", length(obj)),
            bounds =  dims,
            maximum = FALSE)

    opt <- ROI_solve(x, solver = SOLVER)

    str(opt)

    expect_that( sum(abs(opt$solution - c(sqrt(2), -1, -1))), equals( 0 ) ) 
    expect_that( opt$objval, equals( sqrt(2) - 2 ) )
      
} )
