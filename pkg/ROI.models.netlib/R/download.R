
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

download_netlib <- function(folder = system.file("data", package = "ROI.models.netlib")) {
    stopifnot( dir.exists(folder) )
    netlib_mps <- download_mps_files()
    
    for (n in names(netlib_mps)) {
        fname <- file.path(folder, sprintf("%s.rda", n))
        op <- netlib_mps[[n]]
        save(op, file=fname)
    }

}

if ( FALSE ) {
    q("no")
    R
    require("Rglpk")
    require(ROI)

    i <- 1L

    netlib_mps <- download_mps_files()
    names(netlib_mps)
    meta <- readRDS("../data/meta.rds")
    nam <- tolower(rownames(meta))
    length(names(netlib_mps))
    length(nam)
    intersect(names(netlib_mps), nam)
    length(intersect(names(netlib_mps), nam))
    setdiff(names(netlib_mps), nam)
    setdiff(nam, names(netlib_mps))
}