
setwd("/home/florian/work/Optimization/ROI/ROI_R-Forge/www/db")

source("create_solver_db_functions.R")

r_version <- "R"
lib.loc <- head(.libPaths(), 1)
repos <- "https://r-forge.r-project.org"

## r_version, lib.loc, repos 
solver_db_cran <- create_solver_db_cran(r_version, lib.loc, repos)
solver_db_cran$Repository <- "https://CRAN.R-project.org"
solver_db_cran$Repository

b <- sapply(solver_db_cran$Signature, nrow) == 0L
solver_db_cran$Package[b]

saveRDS(solver_db_cran, file = "SOLVERS_CRAN.rds")

if (FALSE) {

    r_version <- R
    lib.loc <- head(.libPaths(), 1L)
    cran <- "https://cran.r-project.org/"
    rforge <- "http://R-Forge.R-project.org"

    rownames(roi_solver_cran)

    solver_db_cran$Package
    
}


