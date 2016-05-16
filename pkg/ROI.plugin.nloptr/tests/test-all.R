b <- c(require("testthat", quietly = TRUE), require("ROI", quietly = TRUE))
if( all(b) ) {
    test_check("ROI.plugin.nloptr")
}

