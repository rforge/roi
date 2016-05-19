
q("no")
Rdevel

getwd()
##setwd("/home/florian/work/Optimization/ROI/ROI/devel/ROI.plugin.ecos/tests")

library(testthat)
library(ROI)

SOLVER <- "ecos"
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

