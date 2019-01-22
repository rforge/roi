
if (FALSE) {
    
    library(slam)
    library(ROI)
    library(cccp)

    ## Solving linear and quadratic programs with cone constraints
    cccp(P = NULL, q = NULL, A = NULL, b = NULL, cList = list(),
         x0 = NULL, f0 = NULL, g0 = NULL, h0 = NULL,
         nlfList = list(), nlgList = list(), nlhList = list(),
         optctrl = ctrl())

    ## Creating objects of reference-class CTRL
    ctrl(maxiters = 100L, abstol = 1e-07, reltol = 1e-06,
         feastol = 1e-07, stepadj = 0.95, beta = 0.5, trace = TRUE)

    ## Creating a member object of the reference-class DCP
    dcp(x0, f0, g0, h0, cList = list(), nlfList = list(), nlgList = list(),
        nlhList = list(), A = NULL, b = NULL)

    ## Creating a member object of the reference-class DLP
    dlp(q, A = NULL, b = NULL, cList = list())

    ## Creating a member object of the reference-class DNL
    dnl(q, A = NULL, b = NULL, cList = list(),
        x0, nlfList = list(), nlgList = list(), nlhList = list())

    ## Creating a member object of the reference-class DQP
    dqp(P, q, A = NULL, b = NULL, cList = list())

    ## Geometric program
    gp(F0, g0, FList = list(), gList = list(), nno = NULL, A = NULL, b = NULL, optctrl = ctrl())

    ## Definition of nonlinear inequality constraints
    nlfc(G, h)

    ## Definition of linear inequality constraints
    nnoc(G, h)

    ## Definition of positive semidefinite cone inequality constraints
    psdc(Flist, F0)

    ## Definition of second-oder cone inequality constraints
    socc(F, g, d, f)

    ##
    list(conType = "NNOC", G = as.matrix(G), h = as.matrix(h), 
        dims = nrow(G))

    mat <- matrix(c(3, 4, 2,
                    2, 1, 2,
                    1, 3, 2), nrow=3, byrow=TRUE)

    x <- OP(objective = c(2, 4, 3),
            constraints = L_constraint(L = mat,
                                       dir = c("<=", "<=", "<="),
                                       rhs = c(60, 40, 80)),
            maximum = TRUE)

    solution(ROI_solve(x))
    round(message$x, 4)

    prob <- as.list(ROI_solve(x, "ecos", dry_run = TRUE))
    prob$G
    prob$h

    solveLobjLcon(x)


    x <- OP(objective = c(2, 4, 3),
            constraints = L_constraint(L = mat,
                                       dir = c("<=", "<=", "<="),
                                       rhs = c(60, 40, 80)),
            bounds = V_bound(lb = c(0, 3, -Inf), ub = c(Inf, 3, 4)),
            maximum = TRUE)

    ROI_solve(x)
    solution(ROI_solve(x))
    solveLobjLcon(x)

    prob <- as.list(ROI_solve(x, "ecos", dry_run = TRUE))
    prob$G
    prob$h

    F0 <- function(x) {
        print(x)
        drop(crossprod(c(2, 4, 3), x))
    }
    G0 <- function(x) c(2, 4, 3)
    H0 <- function(x) diag(0, 3L)

    objective(x) <- F_objective(F = F0, n = 3L, G = G0, H = H0)

    x0 <- rep(1, 3)
    grad(objective(x),  x0)
    hessian(objective(x),  x0)

    y <- x
    objective(x) <- ROI:::as.F_objective.L_objective(objective(x))

    solution(ROI_solve(x, solver = "alabama", start = double(3)))

    environment(objective(x))$F

    F0(c(0, 0, 0))
    
    s <- NULL
    control <- list(start = double(3), maxiters = 1000L)
    s <- solveFobjLcon(x, control)
    s
    ## 13.33333  3.00000  4.00000


    start <- c(3, 0)
    start <- c(0.6, 1.8)
    F0 <- function(x) {
        print(class(x))
        crossprod(c(7, 8), x)
    }
    G0 <- function(x) c(7, 8)
    H0 <- function(x) diag(0, 2L)
    A <- rbind(c(3, 4))
    b <- c(9)

    model <- cccp::dcp(x0 = start, f0 = F0, g0 = G0, h0 = H0, 
        cList = list(), nlfList = list(), 
        nlgList = list(), nlhList = list(), A = A, b = b)
    s <- cccp::cps(model, cccp::ctrl())
    c(7, 8) %*% cccp::getx(s)

    library(ROI)

    lp  <- OP(objective = L_objective(c(7, 8), names=c("x", "y")),
              constraints = L_constraint(L = rbind(c(3, 4)), 
                                     dir = c("==", "=="), rhs = c(9)),
          bounds = V_bound(lb = c(-Inf, -Inf)))
    s <- ROI_solve(lp)
    c(7, 8) %*% solution(s)

}
