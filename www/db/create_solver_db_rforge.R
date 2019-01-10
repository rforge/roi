
setwd("/home/florian/work/Optimization/ROI/ROI_R-Forge/www/db")

source("create_solver_db_functions.R")


r_version <- "R"
lib.loc <- head(.libPaths(), 1)
repos <- "https://r-forge.r-project.org"

solver_db_rforge <- create_solver_db_rforge(r_version, lib.loc, repos)
b <- sapply(solver_db_rforge$Signature, nrow) == 0L
solver_db_rforge$Package[b]

saveRDS(solver_db_rforge, file = "SOLVERS_R-Forge.rds")

if (FALSE) {

    r_version <- R
    lib.loc <- head(.libPaths(), 1L)
    repos <- "http://R-Forge.R-project.org"

    rownames(roi_solver_cran)

    solver_db_rforge <- readRDS("SOLVERS_R-Forge.rds")
    solver_db_rforge$Package
    head(solver_db_rforge, 1)
    class(solver_db_rforge[1, "Signature"])

    rownames(solvers)
    solvers[,1:2]
    solvers <- readRDS("SOLVERS.rds")
    solvers[3, "Signature"]
    class(solvers[3, "Signature"])

    colnames(solver_db_rforge)
    solver_db_rforge$Package

}


