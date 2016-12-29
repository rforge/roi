q("no")
Rdevel 

library(ROI)
library(ROI.models.netlib)

netlib()
## get all problems as a list
ntlb <- netlib("all")
## get a certain problem by name
netlib("afiro")
ntlb[["afiro"]]
## get the meta info to the problems
netlib("metainfo")
## solve a problem
sol <- ROI_solve(netlib("afiro"))
sol
sol$objval - as.numeric(netlib("metainfo")["afiro", "optimal_value"])



getwd()
remove.packages("ROI.models.netlib")
netlib_download()

f <- system.file("data", package = "ROI.models.netlib")
dir(f)

netlib_ls <- function(file_extension=TRUE) {
    folder <- system.file("data", package = "ROI.models.netlib")
    fnames <- setdiff(dir(folder), "meta.rda")
    if ( !file_extension ) {
        fnames <- gsub(".rda", "", fnames, fixed=TRUE)
    }
    return(fnames)
}

netlib_single_op <- function(op_name) {
    folder <- system.file("data", package = "ROI.models.netlib")
    fname <- file.path(folder, op_name)
    env <- new.env(hash=FALSE, parent=emptyenv())
    load(fname, envir=env)
    return(env$op)
}

netlib_op <- function(op_names=netlib_ls()) {
    stopifnot( any(op_names %in% netlib_ls()) )
    if ( length(op_names) == 1 )  return(netlib_single_op(op_names))
    x <- lapply(op_names, netlib_single_op)
    names(x) <- gsub(".rda", "", op_names, fixed=TRUE)
    return(x)
}

dir()

("../data/")

netlib_meta()
netlib_ls()
netlib_ls(FALSE)

opn <- netlib_ls()[4]
opn
op <- netlib_op(opn)
op
all_ops <- netlib_op()
all_ops


