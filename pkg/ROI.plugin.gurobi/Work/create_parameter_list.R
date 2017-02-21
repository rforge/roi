
x <- read.table("Gurobi_Parameters.csv", sep=";")

tail(x)
y <- sprintf('    .ROI_plugin_register_solver_control( solver, "%s", "X" )    ## %s', x[,1], x[,2])
writeLines(y, "solver_controls.R")

