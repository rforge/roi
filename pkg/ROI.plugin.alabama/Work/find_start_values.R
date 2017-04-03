library(ROI)
library(slam)

qo <- Q_objective(Q = diag(2), L =  numeric(2))
qc <- Q_constraint(rbind(c(1, 0), c(0, 0)), c(0, 0), dir=">=", rhs=0.5)
x <- OP(qo, qc)
opt <- ROI_solve(x, solver="alabama", start=c(3, 3))


opt <- ROI_solve(x, solver="alabama", start=c(3, 3), dry_run=TRUE)
eval(opt)


x1 <- c(0, seq(1, 1e6, length.out=4))

xfun <- function(..., CALL) {
    opt <- as.list(CALL)
    opt$par <- c(...)
    if ( is.null(CALL$control.outer) ) opt$control.outer <- list()
    opt$control.outer$itmax <- 100
    mode(opt) <- "call"
    sol <- eval(opt)
    list(solution=sol$par, objval=sol$value)
}

z <- apply(expand.grid(x1, x1), 1, xfun, CALL=opt)
sapply(z, "[[", "solution")
sapply(z, "[[", "objval")



eval(opt)
x <- opt


as.list(opt)

opt$control.outer <- list()
opt$control.outer$itmax <- 1
mode(opt) <- "call"
eval(opt)
