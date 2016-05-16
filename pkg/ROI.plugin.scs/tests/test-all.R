b <- c(require("testthat", quietly = TRUE), require("ROI", quietly = TRUE))
SOLVER <- "scs"
if ( isTRUE(all(b)) ) {
    test_check( "ROI.plugin.scs" )
}
