b <- c(require("testthat", quietly = TRUE), require("ROI", quietly = TRUE))
SOLVER <- "ecos"
if( all(b) ) {
    test_check("ROI.plugin.ecos")
}

