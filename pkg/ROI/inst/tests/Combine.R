context("Comine")
test_that("Combine L_constraints", {

    library( testthat )
    library( ROI )

    dim_1 <- sample(1:10, 2)
    dim_2 <- c(sample(1:10, 1), dim_1[2])

    mat_1 <- matrix(sample(1:100, prod(dim_1)), ncol=dim_1[2])
    dir_1 <- sample(c("==", "<=", ">="), dim_1[1], replace=TRUE)
    rhs_1 <- sample(1:100, dim_1[1])

    mat_2 <- matrix(sample(1:100, prod(dim_2)), ncol=dim_2[2])
    dir_2 <- sample(c("==", "<=", ">="), dim_2[1], replace=TRUE)
    rhs_2 <- sample(1:100, dim_2[1])
    
    lc_1 <- L_constraint(L = mat_1, dir = dir_1, rhs = rhs_1)
    lc_2 <- L_constraint(L = mat_2, dir = dir_2, rhs = rhs_2)

    lc_3 <- rbind(lc_1, lc_2)
    expect_that( nrow(lc_1) + nrow(lc_2), equals(nrow(lc_3)) )
      
} )


Q_constraint()

