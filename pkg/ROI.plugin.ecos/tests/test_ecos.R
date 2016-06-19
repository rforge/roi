
library(ROI)
SOLVER <- "ecos"
check <- function(x, msg="") {
    if ( !isTRUE(x) ) stop(msg)
}

local({
## SOCP - Example - 1
## min:  1 x1 + 1 x2 + 1 x3
## s.t.     x1 == sqrt(2)
##          x1 >= ||(x2, x3)||
##
## c(sqrt(2), -1, -1)
    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0))
    b <- c(sqrt(2))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1), "soc"=list(2:4))
    bound <- c(as.C_bound(cones), V_bound(li=1:3, lb=rep.int(-Inf, 3)))

    x <- OP(objective = obj,
            constraints = L_constraint(L = rbind(A, G), dir=rep("==", length(c(b, h))), rhs = c(b, h)),
            types = rep("C", 3),
            bounds =  bound,
            maximum = FALSE)

    opt <- ROI_solve(x, solver=SOLVER)
    check(equal(sum(abs(opt$solution - c(sqrt(2), -1, -1))), 0), "SCOP - Example 1 - Test 1")
    check(equal(opt$objval, (sqrt(2) - 2)), "SCOP - Example 1 - Test 2")

})

local({
## SOCP - Example - 2
## min:  0 x1 - 2 x2 - 2 x3 + 0 x4 - 2 x5 - 2 x6
## s.t.     x1 == sqrt(2)
##          x4 == sqrt(2)
##          x1 >= ||(x2, x3)||
##          x4 >= ||(x5, x6)||
##
## c(sqrt(2), 1, 1, sqrt(2), 1, 1)
    obj <- c(0, -2, -2, 0, -2, -2)
    A <- rbind(c(1, 0, 0, 0, 0, 0),
               c(0, 0, 0, 1, 0, 0))
    b <- c(sqrt(2), sqrt(2))
    G <- diag(x=-1, 6)
    h <- rep(0, 6)
    cones <- list("free"=c(1, 2), "soc"=list(3:5, 6:8))
    bound <-  c(as.C_bound(cones), V_bound(li=1:6, lb=rep.int(-Inf, 6)))

    x <- OP(objective = obj,
            constraints = L_constraint(L = rbind(A, G), dir=rep("==", length(c(b, h))), rhs = c(b, h)),
            types = rep("C", 6),
            bounds =  bound,
            maximum = FALSE)

    opt <- ROI_solve(x, solver=SOLVER)
    check(equal(sum(abs(opt$solution - c(sqrt(2), 1, 1, sqrt(2), 1, 1))), 0),
         "SCOP - Example 1 - Test 1")

} )

local({
## EXPP - Example - 1
## min:  x + y + z
## s.t.
## y e^(x/y) <= z
## y > 0
## x == 1
## y == 2
## c(1, 2, 2*exp(1/2))
    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0),
               c(0, 1, 0))
    b <- c(1, 2)
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1, 2), "expp"=list(3:5))
    bound <- as.C_bound(cones)

    x <- OP(objective = obj,
            constraints = L_constraint(L = rbind(A, G), dir=rep("==", length(c(b, h))), rhs = c(b, h)),
            types = rep("C", 3),
            bounds =  bound,
            maximum = FALSE)

    opt <- ROI_solve(x, solver=SOLVER)
    check(equal(opt$solution , c(1, 2, 2*exp(1/2))), "EXPP - Example 1 - Test 1")

} )

local({
## EXPP - Example - 2
## max:  x + y + z
## s.t.
## y e^(x/y) <= z
## y > 0
## y == 2
## z == 2 * exp(1/2)
## c(1, 2, 2*exp(1/2))
    obj <- c(1, 1, 1)
    A <- rbind(c(0, 1, 0),
               c(0, 0, 1))
    b <- c(2, 2*exp(1/2))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1, 2), "expp"=list(3:5))
    bound <- as.C_bound(cones)

    x <- OP(objective = obj,
            constraints = L_constraint(L = rbind(A, G), dir=rep("==", length(c(b, h))), rhs = c(b, h)),
            types = rep("C", 3),
            bounds =  bound,
            maximum = TRUE)

    opt <- ROI_solve(x, solver=SOLVER)
    check(equal(opt$solution , c(1, 2, 2*exp(1/2))), "EXPP - Example 1 - Test 2")

} )

local({
## EXPP - Example - 3
## max:  x + y + z
## s.t.
## y e^(x/y) <= z
## y > 0
## y == 1
## z == exp(1)
## c(1, 1, exp(1))
    obj <- c(1, 1, 1)
    A <- rbind(c(0, 1, 0),
               c(0, 0, 1))
    b <- c(1, exp(1))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1, 2), "expp"=list(3:5))
    bound <- as.C_bound(cones)

    x <- OP(objective = obj,
            constraints = L_constraint(L = rbind(A, G), 
                                       dir=rep("==", length(c(b, h))), 
                                       rhs = c(b, h)),
            types = rep("C", 3),
            bounds =  bound,
            maximum = TRUE)

    opt <- ROI_solve(x, solver=SOLVER)
    check(equal(opt$solution , c(1, 1, exp(1))) , "EXPP - Example 1 - Test 3")

} )
