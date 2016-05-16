
q("no")
R

source("/home/florian/work/Optimization/ROI/ROI/devel/ROI.plugin.nloptr/R/constraints.R")

getwd()
setwd("/home/florian/work/Optimization/ROI/ROI/devel/ROI.plugin.nloptr/tests")

library(testthat)
library(ROI)
library(nloptr)

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

