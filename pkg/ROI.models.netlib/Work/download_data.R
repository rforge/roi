library(ROI.models.netlib)

download_folder <- "/home/florian/work/Optimization/ROI/ROI_R-Forge/pkg/ROI.models.netlib/data"

netlib_download(download_folder)

fn <- dir(download_folder)
setwd(download_folder)
for (f in fn[-grep("meta", fn)]) {
	file.rename(f, gsub(".rda", ".rds", f, fixed=TRUE))
}

load("meta.rda")
ls()

saveRDS(meta, "meta.rds")
