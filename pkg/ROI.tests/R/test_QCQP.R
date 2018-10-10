## This test detects if each solver is using the same definition
## for quadratic constraints.
## minimize:    0.5 * (x^2 + y^2)
## subject to:  0.5 * x^2 >= 0.5
##      x, y >= 0
## solution <- c(1, 0)
test_qcqp_01 <- function(solver) {
    qo <- Q_objective(Q = diag(2), L =  numeric(2))
    qc <- Q_constraint(rbind(c(1, 0), c(0, 0)), c(0, 0), dir=">=", rhs=0.5)
    x <- OP(qo, qc)

    sol <- c(1, 0)

    opt <- ROI_solve(x, solver = solver,
                     control = solver_control(solver, sol))
   
    ## local_opts <- list( algorithm = "NLOPT_LD_LBFGS", xtol_rel  = 1e-4 )
    ## opt <- ROI_solve(x, solver="nloptr", start=c(2, 2), method="NLOPT_LD_MMA")

    check("QCQP-01@01", equal(opt$solution, sol) )
    check("QCQP-01@02", equal(opt$objval, 0.5) )
}
