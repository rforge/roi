setwd("/home/florian/work/Optimization/ROI/ROI_R-Forge/www/db")

solver_db_cran <- readRDS(file = "SOLVERS_CRAN.rds")
solver_db_rforge <- readRDS(file = "SOLVERS_R-Forge.rds")
solver_db_github <- readRDS(file = "SOLVERS_GITHUB.rds")

stopifnot( all(colnames(solver_db_cran) == colnames(solver_db_rforge)) )
stopifnot( all(colnames(solver_db_cran) == colnames(solver_db_github)) )

solver_db <- rbind(solver_db_cran, solver_db_rforge, solver_db_github)
rownames(solver_db) <- NULL
colnames(solver_db)
solver_db[, c("Package", "Repository")]
length(unique(solver_db[, c("Package")]))
## solver_db <- solver_db[!solver_db[, "Package"] == "ROI.plugin.cccp",]

stopifnot( all(sapply(solver_db$Signature, NROW) > 1L) )

saveRDS(solver_db, "SOLVERS.rds")
