context("LP")

## LP - Example - 1
## max:  2 x_1 + 4 x_2 + 3 x_3
## s.t.
## 3 x_1  +  4 x_2  +  2 x_3  <= 60
## 2 x_1  +    x_2  +  2 x_3  <= 40
##   x_1  +  3 x_2  +  2 x_3  <= 80 
## x_1, x_2, x_3 >= 0

test_that("LP Example 1", {

    q("no")
    Rdevel

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

    lp
    opt <- ROI_solve(lp, solver = "glpk", control=list(DEBUG=TRUE))
    opt$solution
    opt <- ROI_solve(lp, solver = "scs", control=list(DEBUG=TRUE))
    round(opt$solution, 5)
    mat
    as.matrix(cxL)
    as.matrix(cxL[ind,])

    mat2 <- cbind(mat, diag(3))
    mat2 <- rbind(mat2, cbind(diag(0, 3), diag(3)))
    mat2

    lp2 <- OP(objective = c(2, 4, 3),
              constraints = L_constraint(L = mat,
                                        dir = c("<=", "<=", "<="),
                                        rhs = c(60, 40, 80)),
              bounds = V_bound(li=1, ui=2, lb=1, ub=4),
              maximum = TRUE)

    opt <- ROI_solve(lp2, solver = "glpk", control=list(DEBUG=TRUE))
    opt$solution
    opt <- ROI_solve(lp2, solver = "scs", control=list(DEBUG=TRUE))
    round(opt$solution, 5)

    library(scs)

    mat2 <- cbind(mat, diag(3))
    mat2 <- rbind(mat2, cbind(diag(-1L, 6)))
    mat2
    dim(mat2)

    obj <- -c( 2,  4,  3, rep.int(0L, 3))
    b   <-  c(60, 40, 80, rep.int(0L, 6))
    o   <- scs(mat2, b, obj, cone=list(f=3, l=6L))
    round(o$x, 6)
    o$info$status

    ## 1 variable soll groesser gleich 1 sein!
    mat2 <- cbind(mat, diag(3))
    mat2 <- rbind(mat2, cbind(diag(-1L, 6)))
    mat2

    obj <- -c( 2,  4,  3, rep.int(0L, 3))
    b   <-  c(60, 40, 80, -1, rep.int(0L, 5))
    o   <- scs(mat2, b, obj, cone=list(f=3, l=6L))
    round(o$x, 6)
    o$info$status

    ## 1 variable soll groesser gleich 1 sein!
    mat2 <- cbind(mat, diag(3))
    mat2 <- rbind(mat2, diag(-1L, 6), diag(1L, 6)[2,])
    mat2
    dim(mat2)

    obj <- -c( 2,  4,  3, rep.int(0L, 3))
    b0   <-  c(60, 40, 80, -1, rep.int(0L, 5), 4)
    length(b0)
    b0
    str(list(f=3, l=7L))
    o   <- scs(mat2, b0, obj, cone=list(f=3, l=7L))
    round(o$x, 6)
    o$info$status

    library(ECOSolveR)
    library(Matrix)
    ls("package:ECOSolveR")

    args(ECOS_csolve)
    oe <- ECOS_csolve(obj, G=as(mat2, "dgCMatrix"), h=b, dims=list(l=6L))
    oe$x


    as.matrix(cxL[ind,])
      
} )

vb <- V_bound(1, 3, -5, 5)
str(vb)

as.matrix(cxL)

round(out$x, 3)
out

q("no")
Rdevel