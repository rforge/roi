
setwd("/home/florian/work/Optimization/ROI/ROI_R-Forge/www/db")

source("create_solver_db.R")

R <- "/home/florian/bin/R-devel/bin/R"
CRAN <- "https://cran.r-project.org/"
RFORGE <- "https://r-forge.r-project.org"

## r_version, lib.loc, repos 
solver_db_cran <- create_solver_db_cran(R, head(.libPaths()), CRAN)
saveRDS(solver_db_cran, file = "SOLVERS_CRAN.rds")

if (FALSE) {
    r_version <- R
    lib.loc <- R_LIBS_PATH
    cran <- "https://cran.r-project.org/"
    rforge <- "http://R-Forge.R-project.org"

    rownames(roi_solver_cran)
}


