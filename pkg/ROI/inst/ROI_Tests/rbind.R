context("rbind")

is_stm_zero_matrix <- function(x) {
    if ( is.null(x) ) return(TRUE)
    if ( slam::is.simple_triplet_matrix(x) ) {
        if ( length(x$i) == 0 ) return(TRUE)
    }
    return(FALSE)
}

mat <- matrix(c(3, 4, 2,
                2, 1, 2,
                1, 3, 2), nrow=3, byrow=TRUE)
lc <- L_constraint(L = mat,
                   dir = c("<=", "<=", "<="),
                   rhs = c(60, 40, 80) )

qc <- Q_constraint(Q = list(NULL, NULL, diag(1, nrow = 3)),
                   L = matrix(c(-1, 1, 1, 1, -3, 1, 0, 0, 0), byrow = TRUE, ncol = 3),
                   dir = rep("<=", 3),
                   rhs = c(20, 30, 1))

test_that( "rbind L_constraints", {

    lc1 <- L_constraint( L = mat[1,], dir = "<=", rhs = 60 )
    lc2 <- L_constraint( L = mat[2,], dir = "<=", rhs = 40 )
    lc3 <- L_constraint( L = mat[3,], dir = "<=", rhs = 80 )
    lc_new <- rbind(lc1, lc2, lc3)

    expect_that( equal(lc, lc_new), equals( TRUE ) )

} )

test_that( "rbind Q_constraints", {

    qc1 <- Q_constraint(Q = NULL, L = matrix(c(-1,  1,  1), nrow=1), dir = "<=", rhs = 20 )
    qc2 <- Q_constraint(Q = NULL, L = matrix(c( 1, -3,  1), nrow=1), dir = "<=", rhs = 30 )
    qc3 <- Q_constraint(Q = diag(1, nrow = 3),
                        L = matrix(c( 0,  0,  0), nrow=1), dir = "<=", rhs =  1 )

    qc_new <- rbind(qc1, qc2, qc3)
    expect_that( equal(qc$L, qc_new$L), equals( TRUE ) )
    expect_that( unique(unlist(lapply(qc_new$Q, dim))), equals(3) )
    expect_that( equal(sapply(qc_new$Q, is_stm_zero_matrix), sapply(qc$Q, is.null)), equals( TRUE ) )
    expect_that( equal(qc$Q[[3]], qc_new$Q[[3]]), equals(TRUE) )

} )

test_that( "rbind L_constraints and Q_constraints", {

    lc1 <- L_constraint(L = matrix(c(-1,  1,  1), nrow=1), dir = "<=", rhs = 20 )
    lc2 <- L_constraint(L = matrix(c( 1, -3,  1), nrow=1), dir = "<=", rhs = 30 )
    qc3 <- Q_constraint(Q = diag(1, nrow = 3),
                        L = matrix(c( 0,  0,  0), nrow=1), dir = "<=", rhs =  1 )

    qc_new <- rbind(lc1, lc2, qc3)
    expect_that( equal(qc$L, qc_new$L), equals( TRUE ) )
    expect_that( unique(unlist(lapply(qc_new$Q, dim))), equals(3) )
    expect_that( equal(sapply(qc_new$Q, is_stm_zero_matrix), sapply(qc$Q, is.null)), equals( TRUE ) )
    expect_that( equal(qc$Q[[3]], qc_new$Q[[3]]), equals(TRUE) )

} )
