
setwd("/home/f/work/Optimization/ROI/ROI_R-Forge/www/db")

source("create_solver_db_functions.R")

r_version <- "R"
lib.loc <- head(.libPaths(), 1)
repos <- "https://cran.r-project.org"

## r_version, lib.loc, repos 
solver_db_cran <- create_solver_db_cran(r_version, lib.loc, repos)
solver_db_cran$Repository <- "https://CRAN.R-project.org"
solver_db_cran$Repository
solver_db_cran$Package

b <- !sapply(solver_db_cran$Signature, is.data.frame)
solver_db_cran$Package[b]

saveRDS(solver_db_cran, file = "SOLVERS_CRAN.rds")

if (FALSE) {

    r_version <- R
    lib.loc <- head(.libPaths(), 1L)
    cran <- "https://cran.r-project.org/"
    rforge <- "http://R-Forge.R-project.org"

    rownames(roi_solver_cran)


    library("ROI.plugin.qpoases")

    i <- which(solver_db_cran$Package == "ROI.plugin.qpoases")
    i
    solver_db_cran[[i, 'Signature']] <- extract_signature("qpoases")

    solver_db_cran$Package
    solver_db_cran$Repository
    
}


