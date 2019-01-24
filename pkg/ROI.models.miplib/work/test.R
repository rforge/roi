
q("no")
Rdevel

library(ROI)
library(ROI.models.miplib)

ls("package:ROI")
ls("package:ROI.models.miplib")

miplib_download_benchmark()
miplib_download_metainfo()

miplib()[61]
op <- miplib(miplib()[61])
ROI_solve(op)
