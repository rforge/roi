
setwd("/home/florian/work/Optimization/ROI/ROI_R-Forge/www/db")

source("create_solver_db_functions.R")


r_version <- "R"
lib.loc <- head(.libPaths(), 1)
repos <- c("dirkschumacher/ROI.plugin.cbc", "datastorm-open/ROI.plugin.clp",
           "FlorianSchwendinger/ROI.plugin.gurobi", "FlorianSchwendinger/ROI.plugin.mosek")
CRAN <- "https://cran.r-project.org/"

## r_version, lib.loc, repos 
solver_db_github <- create_solver_db_github(r_version, lib.loc, repos, CRAN)

b <- sapply(solver_db_github$Signature, nrow) == 0L
solver_db_github$Package[b]

saveRDS(solver_db_github, file = "SOLVERS_GITHUB.rds")

if (FALSE) {

    r_version <- R
    lib.loc <- head(.libPaths(), 1L)
    cran <- CRAN
    repos <- REPOS


    as <- ROI:::ROI_available_solvers(milp)[, c("Package", "Repository")]
    sub("/ROI.plugin.*", "", as$Repository)

}
