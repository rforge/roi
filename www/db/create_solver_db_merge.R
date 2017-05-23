
solver_db_cran <- readRDS(file = "SOLVERS_CRAN.rds")
solver_db_rforge <- readRDS(file = "SOLVERS_R-Forge.rds")

stopifnot( all(colnames(solver_db_cran) == colnames(solver_db_rforge)) )

solver_db <- rbind(solver_db_cran, solver_db_rforge)
rownames(solver_db) <- NULL

saveRDS(solver_db, "SOLVERS.rds")
