## Imports
#' @importFrom ROI as.OP OP L_objective L_constraint V_bound
#' @importFrom Rglpk Rglpk_read_file
#' @importFrom R.utils gunzip
#' @importFrom utils download.file setTxtProgressBar tail txtProgressBar untar head
#

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

as.OP.NULL <- function(x) NULL

download_library <- function(url, path, method = NULL, quiet = TRUE, force = FALSE) {
    if ( !quiet )
        cat("\n download MIPLIB\n\n")
    if ( is.null(method) ) {
        method <- if ( .Platform[['OS.type']] == "unix" ) "wget" else "internal"
    }
    if ( file.exists(file.path(path, "non_emtpy_folder")) ) {
        file.remove(file.path(path, "non_emtpy_folder"))
    }
    destfile <- file.path(path, tail(unlist(strsplit(url, "/", fixed=TRUE), TRUE, FALSE), 1))
    ## exdir <- gsub(".tgz", "", destfile)
    if ( !file.exists(destfile) | force ) {
        download.file(url, destfile, method = method, quiet = quiet)
    }
    if ( file.exists(destfile) ) {
        ## Files contained in the tgz-file.
        tgz_files <- untar(tarfile = destfile, list = TRUE, exdir = path)
        miplib_files <- grep("instances", tgz_files, value = TRUE)
        exdir <- tail(names(sort(table(dirname(miplib_files)))), 1)
        miplib_folder <- normalizePath(file.path(path, exdir))
        if ( !dir.exists(miplib_folder) | force ) {
            untar(tarfile = destfile, exdir = path)
        }
    } else {
        stop("download error")
    }
    downloaded <- c(miplib_folder = miplib_folder, destfile = destfile)
    return(downloaded)
}

untar_all <- function(path, quiet=TRUE) {
    fps <- file.path(path, dir(path, pattern=".gz"))
    n <- length(fps)
    if ( !quiet ) {
        cat("\n unzip MIPLIB\n\n")
        pb <- txtProgressBar(min = 0, max = n, style=3)
    }
    for (i in seq_len(n)) {
        suppressWarnings(R.utils::gunzip(fps[i]))
        if ( !quiet ) setTxtProgressBar(pb, i)
    }
    if ( !quiet ) close(pb)
    NULL
}

build_miplib <- function(in_path, out_path, quiet = TRUE) {
    fps <- dir(in_path, pattern = ".mps$", full.names = TRUE)
    stopifnot( length(fps) >  0 )
    n <- length(fps)
    if ( !quiet ) {
        cat("\n convert mps to ROI:\n\n")
        pb <- txtProgressBar(min = 0, max = n, style=3)
    }
    for (i in seq_len(n)) {
        op <- tryCatch(as.OP(Rglpk::Rglpk_read_file(fps[i], type="MPS_free")), error=function(e) NULL)
        if ( is.null(op) ) {            
            warning(sprintf("could not convert '%s' to OP", basename(fps[i])))
            next()
        }
        op_name <- make.names(gsub(".mps$", "", basename(fps[i])))
        op_name <- sprintf("%s/%s.rds", out_path, op_name)
        saveRDS(op, file = op_name)
        if ( !quiet ) setTxtProgressBar(pb, i)
    }
    if ( !quiet ) close(pb)
    NULL
}

miplib_download <- function(url, folder, method = NULL, quiet = TRUE, force = FALSE, cleanup = TRUE) {
    stopifnot( dir.exists(folder) )
    folder <- normalizePath(folder)
    downloaded <- download_library(url, folder, method, quiet, force)
    untar_all(downloaded["miplib_folder"], quiet = quiet)
    build_miplib(in_path = downloaded["miplib_folder"], out_path = folder, quiet)
    if ( cleanup ) {
        cleanup_files <- dir(folder, include.dirs = TRUE, full.names = TRUE)
        cleanup_files <- grep("\\.rds$", cleanup_files, value = TRUE, invert = TRUE)
        for (delme in cleanup_files) {
            unlink(delme, recursive = TRUE)
        }
    }
}

#  -----------------------------------------------------------
#  miplib_download_all
#  ===================
#' @title Download the 'MIPLIB 2010' Test Problem Set
#' @description
#'     The \code{MIPLIB 2010} test problem set is downloaded and
#'     transformed from the \code{MPS} format into the \pkg{ROI} format.
#'     The results are stored as \code{'.rds'} files at the location provided 
#'     via the parameter \code{folder}.
#' @param url a character giving the url to \code{MIPLIB 2010}.
#' @param folder an optional character giving the location where the
#'     \code{MIPLIB 2010} test problem set should be downloaded to.
#' @param method a character giving the method to be used for downloading
#'     files, for more information see \code{\link{download.file}}.
#' @param quiet a logical giving if status status messages should be suppressed.
#' @param force a logical if \code{TRUE} (default is \code{FALSE}) the download is forced, 
#'     regardless if the metainfo was downloaded previously.
#' @param cleanup a logical if \code{TRUE} (default is \code{TRUE}) the downloaded
#'     temporary files are deleted after the download an conversion is completed.
#' 
#' @details
#'     \itemize{
#'         \item{\code{miplib_download_all} download all MIPLIB-2010 instances (arround 1.3 GB).}
#' 
#'         \item{\code{miplib_download_benchmark} download the MIPLIB-2010 benchmark instances (arround 94 MB).}
#'
#'         \item{\code{miplib_download_metinfo} download the available meta information.}
#'      }
#' @examples
#' \dontrun{
#'
#' ## download all MIPLIB-2010 instances (arround 1.3 GB)
#' miplib_download_all()
#' ## or
#' miplib_download_all(folder = "data/miplib")
#' 
#' ## download MIPLIB-2010 benchmark instances (arround 94 MB)
#' miplib_download_benchmark()
#' ## or 
#' miplib_download_benchmark(folder = "data/miplib")
#'
#' ## download meta information
#' miplib_download_metinfo()
#' }
#' @name miplib_download
#' @rdname miplib_download
#' @export
#  -----------------------------------------------------------
#  old: http://miplib.zib.de/download/miplib2010-complete.tgz
#  http://miplib2010.zib.de/download/miplib2010-1.1.3-complete.tgz
miplib_download_all <- function(
    url = "http://miplib2010.zib.de/download/miplib2010-1.1.3-complete.tgz",
    folder = system.file("roi_op", package = "ROI.models.miplib"),
    method = NULL, quiet = TRUE, force = FALSE, cleanup = TRUE) {
    miplib_download(url, folder, method, quiet, force, cleanup)
}

##  -----------------------------------------------------------
##  miplib_download_benchmark 
##  =========================
##' @rdname miplib_download
##' @export
##  -----------------------------------------------------------
##  old: http://miplib.zib.de/download/miplib2010-benchmark.tgz
##  http://miplib2010.zib.de/download/miplib2010-1.1.3-benchmark.tgz
miplib_download_benchmark <- 
    function(url = "http://miplib2010.zib.de/download/miplib2010-1.1.3-benchmark.tgz",
             folder = system.file("roi_op", package = "ROI.models.miplib"),
             method = NULL, quiet = TRUE, force = FALSE, cleanup = TRUE) {
    miplib_download(url, folder, method, quiet, force, cleanup)
}

##  -----------------------------------------------------------
##  miplib_download_metainfo
##  ========================
##'  @title Download the 'MIPLIB 2010' Test Problem Set
##'  @description
##'      The meta information from the \code{MIPLIB 2010} test problem 
##'      set is downloaded and transformed into an \code{data.frame}.
##'  @param url a character giving the url to the meta information.
##'  @param folder an optional character giving the location where the
##'      metainfo should be downloaded to.
##'  @param force a logical if \code{TRUE} the download is forced, regardless
##'      if the metainfo was downloaded previously.
##'  
##'  @examples
##'  \dontrun{
##' 
##'  miplib_download_metinfo()
##' 
##'  }
##' @name miplib_download
##' @rdname miplib_download
##' @export
##  -----------------------------------------------------------
##  old: http://miplib.zib.de/download/miplib2010_all.solu
##  http://miplib2010.zib.de/download/miplib2010_all.solu
miplib_download_metainfo <- function(
    url = "http://miplib2010.zib.de/download/miplib2010_all.solu",
    folder = system.file("roi_op", package = "ROI.models.miplib"),
    force = FALSE) {

    file <- if (is.null(folder)) "" else file.path(folder, "metainfo.rds")
    if ( file.exists(file) & !force ) {
        return(readRDS(file))
    } else {
        x <- strsplit(readLines(url), "\\s+")
        nam <- make.names(sapply(x, "[[", 2))
        metainfo <- data.frame(name = nam, stringsAsFactors = FALSE, row.names = nam)
        metainfo$optimal_value <- as.numeric(sapply(x, "[", 3))
        metainfo$status <- sapply(x, "[[", 1)
        if ( !is.null(folder) )
            saveRDS(metainfo, file)
        return( metainfo )
    }
}

##  -----------------------------------------------------------
##  miplib_ls_ops
##  =============
##  @title List the Available Test Problems
##  @description
##      List the optimization problems available.
##  @param folder a character giving the location of the test problems.
##  @examples
##  \dontrun{
##  miplib_ls_ops()
##  }
##  -----------------------------------------------------------
miplib_ls_ops <- function(folder=system.file("miplib", package = "ROI.models.MIPLIB")) {
    stopifnot(file.exists(folder))
    ops <- dir(folder)
    ## remove dummy folder
    ops <- setdiff(ops, "non_emtpy_folder")
    return( ops )
}

##  -----------------------------------------------------------
##  miplib_get_ops
##  ==============
##  @title Get Optimization Problems
##  @description
##      Get one or more optimization problems.
##  @param ops a character giving the names of the optimization of the problems
##         to be returned, if \code{ops} is \code{"all"} all available problems
##         are returned.
##  @param folder the folder where the optimization problems are stored.
##  @details
##      The function \code{miplib_get_ops} searches in the given folder for
##      \code{.rds} files
##  @examples
##  \dontrun{
##  miplib_get_ops(miplib_ls_ops()[1])
##  }
##  -----------------------------------------------------------
miplib_get_ops <- function(ops="all", folder=system.file("miplib", package = "ROI.models.MIPLIB")) {
    stopifnot(file.exists(folder), is.character(ops), is.character(folder))
    if ( ops == "all" )
        ops <- dir(folder, pattern=".rds")
    stopifnot( length(ops) > 0 )
    stopifnot( all(ops %in% dir(folder, pattern=".rds")) )
    ops_path <- file.path(folder, ops)
    lapply(ops_path, readRDS)
}


##  -----------------------------------------------------------
##  miplib
##  ======
##' @title Access the Downloaded \code{MIPLIB}  
##' @description
##'     Get one or more optimization problems, meta information or a listing
##'     of the available \code{MIPLIB 2010} problems.
##' @param x a character giving the names of the optimization problems
##'        to be returned, if \code{x} is \code{"all"} all available problems
##'        are returned, if \code{x} is the name of a single problem the
##'        given problem is returned. If \code{x} is missing a listing 
##'        of all available problems is returned. If \code{x} is \code{"metainfo"}
##'        the meta information about the problems is returned.
##' @param folder the folder where the optimization problems are stored.
##' @details
##'     The function \code{miplib} searches in the given folder for
##'     \code{.rds} files and returns them.
##' @examples
##' \dontrun{
##' ## list all available MIPLIB-2010 problems
##' miplib()
##' ## get all miplib problems
##' miplib("all")
##' ## get a single problem
##' miplib("rmine6")
##' ## get the meta information
##' miplib("metainfo")
##' }
##' @export
##  -----------------------------------------------------------
miplib <- function(x, folder = system.file("roi_op", package = "ROI.models.miplib")) {
    stopifnot(file.exists(folder))
    if ( missing(x) ) {
        lst <- setdiff(dir(folder, pattern=".rds$"), c("metainfo.rds"))
        return( gsub(".rds$", "", lst) )
    }
    stopifnot(is.character(x), (length(x) > 0))
    if ( x == "all" ) {
        lst <- setdiff(dir(folder, pattern=".rds$"), c("metainfo.rds"))
        files <- file.path(folder, lst)
        prob <- lapply(files, readRDS)
        names(prob) <- gsub(".rds$", "", lst)
    } else {
        files <- file.path(folder, sprintf("%s.rds", x))
        stopifnot(all(file.exists(files)))
        if ( length(x) == 1 ) {
            prob <- readRDS(files)
        } else {
            prob <- lapply(files, readRDS)
        }
    }
    prob
}

