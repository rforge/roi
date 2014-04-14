## Documentation of included data sets

##' Daily price data for 30 of the largest US stocks
##'
##' This dataset contains the historical daily prices of 30 of the
##' largest US stocks from 1998-12-31 to 2013-12-31. This data is
##' dividend adjusted based on the CRSP methodology.
##'
##' The selected stocks reflect the DJ 30 Industrial Average Index
##' members as of 2013-09-20.
##'
##' The data was downloaded from Quandl. Data flagged as "WIKI" is
##' public domain.
##' @docType data
##' @keywords datasets
##' @format A time series object of class \code{"xts"} with 30 columns
##' (representing stocks) and 5037 rows (days).
##' @source \url{http://www.quandl.com/WIKI}
##' @import xts
##' @name US30
NULL

##' Weekly price data for 500 of the largest US stocks
##'
##' This dataset contains the historical end-of-week prices of 500 of the
##' largest US stocks from 2004-01-02 to 2013-12-31. This data is
##' dividend adjusted based on the CRSP methodology.
##'
##' The selected stocks reflect the S&P 500 Index members as of
##' 2013-12-21.
##'
##' The data was downloaded from Quandl. Data flagged as "WIKI" is
##' public domain.
##' @docType data
##' @keywords datasets
##' @format A time series object of class \code{"xts"} with 500 columns
##' (representing stocks) and 523 rows (weeks).
##' @source \url{http://www.quandl.com/WIKI}
##' @name US500
NULL
