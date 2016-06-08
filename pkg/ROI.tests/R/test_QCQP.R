## min: 1/2 * (x^2 + y^2)
## s.t. 1/2 * x^2 >= 1/2
##      x, y >= 0
## solution <- c(1, 0)
test_qcqp_01 <- function(solver) {
    qo <- Q_objective(Q = diag(2), L =  numeric(2))
    qc <- Q_constraint(rbind(c(1, 0), c(0, 0)), c(0, 0), dir=">=", rhs=1/2)
    x <- OP(qo, qc)

    opt <- ROI_solve(x, solver=solver)
   
    ## local_opts <- list( algorithm = "NLOPT_LD_LBFGS", xtol_rel  = 1e-4 )
    ## opt <- ROI_solve(x, solver="nloptr", start=c(2, 2), method="NLOPT_LD_MMA")

    solution <- c(0.476190476190476, 1.04761904761905, 2.0952380952381)
    check("QCQP-01@01", equal(opt$solution, c(1, 0)) )
    check("QCQP-01@02", equal(opt$objval, 1/2) )
}
