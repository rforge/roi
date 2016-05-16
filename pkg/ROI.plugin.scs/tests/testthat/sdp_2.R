context("SDP")
## TODO: change scs plugin to fitt the the example

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

    cat("SDP - Example - 1\n")
    if ( identical(parent.frame(), globalenv()) ) {
        library( testthat )
        library( ROI )
    }

    ## this function or something similar should go into ROI
    vectorize_psd <- function(...) {
        x <- list(...)
        fun <- function(M) c(M[lower.tri(M, TRUE)])
        do.call(cbind, lapply(x, fun))
    }

    obj <- c(1, -1, 1)
    A1 <- matrix(c(-7, -11, -11,  3), 2)
    A2 <- matrix(c( 7, -18, -18,  8), 2)
    A3 <- matrix(c(-2,  -8,  -8,  1), 2)
    a  <- matrix(c(33,  -9,  -9, 26), 2)
    B1 <- matrix(c(-21, -11,  0, -11,  10,   8,  0,    8, 5), 3)
    B2 <- matrix(c(  0,  10,  16, 10, -10, -10,  16, -10, 3), 3)
    B3 <- matrix(c( -5,   2, -17,  2,  -6,   8, -17,   8, 6), 3)
    b  <- matrix(c( 14,   9,  40,  9,  91,  10,  40,  10,15), 3)

    ## PSD matrices have to be vectorized
    G1 <- vectorize_psd(A1, A2, A3)
    h1 <- vectorize_psd(a)
    G2 <- vectorize_psd(B1, B2, B3)
    h2 <- vectorize_psd(b)
    h <- c(h1, h2)
    bounds <- c(C_bound(1:3, type="psd"), C_bound(3+1:6, type="psd"))

    x <- OP(objective = obj,
            constraints = L_constraint(L = rbind(G1, G2), dir=rep("==", length(h)), rhs = h),
            types = rep("C", length(obj)),
            bounds =  bounds,
            maximum = FALSE)

    opt <- ROI_solve(x, solver = "scs", control=list(eps=1e-9))
    
    ROI_psd <- function(x) attributes(x)$meta$psd
    
    ## NOTE: The solutions I compare with are from cvxopt where I used the default settings,
    ##       therefore it is possible that scs just provides a solution with a smaler eps

    expect_that( c(obj %*% opt$solution) <= c(obj %*% c(-0.367666090041563, 1.89832827158511, -0.887550426343585)), equals(TRUE) )
    
    ## solution from cvxopt
    ## [-3.68e-01 1.90e+00 -8.88e-01]
    ## oder c(-0.367666090041563, 1.89832827158511, -0.887550426343585)
    expect_that( sum(abs(opt$solution - c(-0.367666090041563, 1.89832827158511, -0.887550426343585))) < 1e-3, equals( TRUE ) )   

    ## [ 3.96e-03 -4.34e-03]
    ## [-4.34e-03  4.75e-03]
    ## c(0.00396107103000518, -0.00433836779348354, -0.00433836779348354,  0.00475162592559036) 
    expect_true( 
                sum(abs( as.numeric(as.matrix(ROI_psd(opt)[[1]]))
                        - c( 0.00396107103000518, -0.00433836779348354, 
                            -0.00433836779348354,  0.00475162592559036) )) < 1e-5 )
    
    ## [ 5.58e-02 -2.41e-03  2.42e-02]
    ## [-2.41e-03  1.04e-04 -1.05e-03]
    ## [ 2.42e-02 -1.05e-03  1.05e-02]
    ## c(0.0558011514407859, -0.00240909203896524, 0.0242146296992217,  -0.00240909203896524, 
    ##   0.000104021271556218, -0.00104543254168053,  0.0242146296992217, -0.00104543254168053, 
    ##   0.0105078600239678) 
    expect_that( 
                sum(abs( as.numeric(as.matrix(ROI_psd(opt)[[2]]))
                        - c( 0.0558011514407859, -0.00240909203896524, 0.0242146296992217,  
                            -0.00240909203896524, 0.000104021271556218, -0.00104543254168053,  
                            0.0242146296992217, -0.00104543254168053, 0.0105078600239678) )) < 1e-5, equals(TRUE) )

} )


