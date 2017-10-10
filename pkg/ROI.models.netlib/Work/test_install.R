q("no")
Rdevel

remove.packages("ROI.models.netlib")

install.packages("ROI.models.netlib")

install.packages("ROI.models.netlib", repos = "http://R-Forge.R-project.org")

library(ROI.models.netlib)

netlib()
netlib("metainfo")
netlib("agg2")
