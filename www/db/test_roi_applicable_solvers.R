library(ROI)

ROI_available_solvers()

## LP
op <- OP(1:2, L_constraint(1:2, "<=", 1))
ROI_available_solvers(op)[,1:2]

## BLP
op <- OP(1:2, L_constraint(1:2, "<=", 1), types = c("B", "B"))
ROI_available_solvers(op)[,1:2]

