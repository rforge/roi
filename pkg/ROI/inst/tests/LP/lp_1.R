context("LP")

## LP - Example - 1
## max:  2 x_1 + 4 x_2 + 3 x_3
## s.t.
## 3 x_1  +  4 x_2  +  2 x_3  <= 60
## 2 x_1  +    x_2  +  2 x_3  <= 40
##   x_1  +  3 x_2  +  2 x_3  <= 80 
## x_1, x_2, x_3 >= 0

test_that("LP Example 1", {

    cat("LP Example 1\n")
    library( testthat )
    library( ROI )

    mat <- matrix(c(3, 4, 2,
                    2, 1, 2,
                    1, 3, 2), nrow=3, byrow=TRUE)
    lp <- OP(objective = c(2, 4, 3),
             constraints = L_constraint(L = mat,
                                        dir = c("<=", "<=", "<="),
                                        rhs = c(60, 40, 80)),
             maximum = TRUE)

    for ( SOLVER in OP_applicable_solver(lp) ) {

        cat("\t", SOLVER)      
        error <-  tryCatch({
            opt <- ROI_solve(lp, solver = SOLVER, control=list(DEBUG=TRUE))
            expect_that( opt$solution, equals( c(0, 20/3, 50/3) ) ) 
            expect_that( opt$objval, equals( 230/3 ) )
            FALSE}, error = function(e) TRUE)

        if (error) cat(testthat:::colourise("\tERROR!\n", "error"))
        else       cat(testthat:::colourise("\tOK!\n", "passed"))
        
    }
      
} )
