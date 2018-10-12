
q("no")
Rdevel

library(ROI)
library(ROI.plugin.qpoases)

qps_path <- "/home/florian/work/Optimization/ROI/QPS"

folder <- file.path(qps_path, "qp_models")
files <- dir(folder)
fp <- file.path(folder, files)

sols <- readRDS(file.path(qps_path, "qpdata_solutions.rds"))

i <- 3
i <- 11L
for ( i in seq_along(sols) ) {
    print(i)
    pattern <- sprintf("%s.qps$", names(sols[i]))
    k <- grep(pattern, fp, ignore.case = TRUE)
    fn <- fp[k]
    x <- readRDS(fn)
    x
    z <- ROI_solve(x, "qpoases", cputime = 300)
    solution(z, "status")
    solution(z, force = TRUE)
    sols[[i]]$solution
    cat(files[i], "  equal: ", equal(solution(z), sols[[i]]$solution), "\n")
}

