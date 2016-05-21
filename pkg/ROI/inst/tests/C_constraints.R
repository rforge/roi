
require(testthat)
require(ROI)
invisible(capture.output(pyConnect()))

A <- rbind(c(1, 0, 0, 0, 0, 0),
           c(0, 0, 0, 1, 0, 0))
b <- c(sqrt(2), sqrt(2))
G <- diag(x=-1, 6)
h <- rep(0, 6)
cones=list("free"=c(1L, 2L), "soc"=list(3:5, 6:8))

nC <- NO_constraint(3)
cC <- C_constraint(L = rbind(A, G), cones=cones, rhs = c(b, h))
lC <- list(L = rbind(A, G), cones=cones, rhs = c(b, h))

#' ## as
expect_that(as.C_constraint(cC), equals(cC))
expect_that(as.C_constraint(lC), equals(cC))
## TODO: how to interpret no constraint?
## as.C_constraint(nC)

#' ## as
expect_that(is.C_constraint(cC), equals(TRUE))




