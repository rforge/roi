
clean_urls <- function(x) {
    x <- grep("<a", x, value=TRUE, fixed=TRUE)
    x <- gsub(".*href=.\\s*", "", x)
    x <- gsub(".>.*", "", x)
    return(x)
}

as.OP.MILP <- function(x){
    obj <- t(x$objective)
    rownames(obj) <- attr(x, "objective_name")
    colnames(obj) <- attr(x, "objective_vars_names")
    mat <- x$constraints[[1]]
    rownames(mat) <- attr(x, "constraint_names")
    OP( objective = L_objective( obj ),
        constraints = L_constraint(mat, x$constraints[[2]], x$constraints[[3]]),
        bounds = V_bound( li = x$bounds$lower$ind,
                          ui = x$bounds$upper$ind,
                          lb = x$bounds$lower$val,
                          ub = x$bounds$upper$val,
                          nobj = attr(x,"n_objective_vars") ),
        types = x$types,
        maximum = x$maximum )
}

download_mps_files <- function() {
    ## the files in the subfolder kennington are compressed with
    ## gz else on has to install some special "emps.c" for decompression
    ##netlib_lp_data_url <- "http://www.netlib.org/lp/data/kennington"
    netlib_lp_data_url <- "http://www.zib.de/koch/perplex/data/netlib/mps"
    suppressWarnings(x <- try(readLines(netlib_lp_data_url), silent = TRUE))
    if ( class(x) == "try-error") {
        stop(sprintf("The requested URL '%s' cloud not be found!", netlib_lp_data_url))
    }
    mps_files <- grep(".mps.gz", x, value=TRUE)
    mps_files <- file.path(netlib_lp_data_url, clean_urls(mps_files))
    roi_ops <- list()
    tmpf0 <- tempfile()
    tmpf1 <- tempfile()
    for (i in seq_along(mps_files)) {
        f <- mps_files[i]
        download.file(f, tmpf0, quiet = TRUE)
        ## unfortunately Rglpk_read_file checks if the file exists
        ## with file.exists therefore it doesn't take connections and
        ## urls.
        writeLines(readLines(tmpf0), tmpf1)
        op <- try(as.OP(Rglpk_read_file(tmpf1, type = "MPS_free")), silent=TRUE)
        if ( class(op) == "try-error" ) {
            next()
        }
        op_name <- tolower(make.names(gsub(".*/", "", gsub(".mps.gz", "", f))))
        roi_ops <- c(roi_ops, setNames(list(op), op_name))
    }
    return(roi_ops)
}

##  -----------------------------------------------------------
##  netlib_download 
##  ===============
##' @title Download the 'NETLIB-LP' Test Problem Set
##' @description The \code{NETLIB-LP} test problem set is downloaded and
##'     transformed from the \code{MPS} format into the \pkg{ROI} format.
##'     The results are stored as \code{'.rda'} file at the location provided 
##'     via the parameter \code{folder}.
##' @param folder an optional character giving the location where the
##'     'NETLIB-LP' test problem set should be downloaded to.
##' @examples
##' \dontrun{
##' netlib_download()
##' netlib_download("data/netlib")
##' }
##' @export
##  -----------------------------------------------------------
netlib_download <- function(folder = system.file("data", package = "ROI.models.netlib")) {
    stopifnot( dir.exists(folder) )
    folder <- normalizePath(folder)
    netlib_mps <- download_mps_files()
    
    for (n in names(netlib_mps)) {
        fname <- file.path(folder, sprintf("%s.rda", n))
        op <- netlib_mps[[n]]
        save(op, file=fname)
    }
}

##  -----------------------------------------------------------
##  netlib_meta
##  ===========
##' @title Get Meta Data
##' @description Ḿeta data for \code{NETLIB-LP} test problem set can
##'   be obtained by simply calling \code{"netlib_met"}.
##' @return
##' @examples
##' \dontrun{
##' netlib_download()
##' netlib_download("data/netlib")
##' }
##' @export
##  -----------------------------------------------------------
netlib_meta <- function() {
    folder <- system.file("data", package = "ROI.models.netlib")
    fname <- file.path(folder, "meta.rda")
    stopifnot( isTRUE(file.exists(fname)) )
    env <- new.env(hash=FALSE, parent=emptyenv())
    load(fname, envir=env)
    return(env$meta)
}


netlib_ls <- function(folder=NULL, file_extension=TRUE) {
    if ( is.null(folder) ) {
        folder <- system.file("data", package = "ROI.models.netlib")
    } else {
        folder <- normalizePath(folder)
    }
    fnames <- setdiff(dir(folder), "meta.rda")
    if ( !file_extension ) {
        fnames <- gsub(".rda", "", fnames, fixed=TRUE)
    }
    return(fnames)
}

netlib_single_op <- function(op_name, folder) {
    fname <- file.path(folder, op_name)
    env <- new.env(hash=FALSE, parent=emptyenv())
    load(fname, envir=env)
    return(env$op)
}

netlib_op <- function(op_names=netlib_ls(), folder=NULL) {
    stopifnot( any(op_names %in% netlib_ls()) )
    if ( is.null(folder) ) {
        folder <- system.file("data", package = "ROI.models.netlib")
    } else {
        folder <- normalizePath(folder)
    }
    if ( length(op_names) == 1 )  return(netlib_single_op(op_names, folder))
    x <- lapply(op_names, netlib_single_op, folder=folder)
    names(x) <- gsub(".rda", "", op_names, fixed=TRUE)
    return(x)
}
