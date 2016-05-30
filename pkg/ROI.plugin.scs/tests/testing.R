
q("no")
Rdevel

getwd()
##setwd("/home/florian/work/Optimization/ROI/ROI/devel/ROI.plugin.ecos/tests")
setwd("/home/florian/mount/Lapi/work/Optimization/ROI/ROI_R-Forge/pkg/ROI.plugin.scs/tests")

library(testthat)
library(ROI)

SOLVER <- "scs"
folder <- "testthat"
files <- file.path(folder, dir(folder))
length(files)

files[1]
source( files[1] )

files[2]
source( files[2] )

files[3]
source( files[3] )

files[4]
source( files[4] )

files[5]
source( files[5] )

files[6]
source( files[6] )

files[7]
source( files[7] )

files[8]
source( files[8] )

files[9]
source( files[9] )
