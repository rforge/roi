q("no")

Rdevel

qplib_path <- "~/Data/qplib"


opath <- "~/Data/qplib_gurobi_solutions"
if ( !dir.exists(opath) ) dir.create(opath)

## ---------------------------
## Gurobi
## ---------------------------
library(ROI)

files <- dir(qplib_path)

i <- 5L
op <- readRDS(file.path(qplib_path, files[i]))
sol <- ROI_solve(op, solver="gurobi")
solution(sol)

error_files <- c("qplib_0158.rds")
files <- setdiff(files, c(dir(opath), error_files))

## sol <- vector("list", length(files))
for (i in seq_along(files)) {
	cat("\n1 -", files[i])
	op <- readRDS(file.path(qplib_path, files[i]))
	## sol[[i]] <- try(ROI_solve(op, solver="gurobi"))
	sol <- tryCatch({ROI_solve(op, solver="gurobi", Threads=2L)}, 
		            error = function(e) NULL)
	if ( !is.null( sol ) ) {
		if ( sol$status$code == 0 )
			cat("\t:)")
		else 
			cat("\t:(")
	} else cat("\t:(")
	oname <- file.path(opath, files[i])
	saveRDS(sol, file=oname)
}

