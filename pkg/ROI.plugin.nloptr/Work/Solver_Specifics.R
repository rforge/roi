
library(ROI)
library(nloptr)
x <- nloptr.get.default.options()
class(x)
write.table(x, "nloptr_default_options.csv")

