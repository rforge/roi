## add a data set to the package
require( "Quandl" )

## Open data. Retrieve price data for US stocks: http://blog.quandl.com/blog/quandl-open-data/
l <- lapply( 1:25, function(i) Quandl.search("WIKI", page = i) )
last_company <- Quandl.search("WIKI", page = 26)[[1]]

## all 500 companies in public domain
all_companies <- c(unlist(lapply( l, function(p) lapply(p, function(e) e$code) )), last_company$code)[-1]

dat <- lapply( all_companies, function( x ) Quandl( x, type="xts" ) )

dat <- list()

for( i in seq_along(all_companies) ){
    writeLines( sprintf( "%s (%d of %d)", all_companies[i], i, length(all_companies)) )
    dat[[i]] <- Quandl( all_companies[i], type="xts" )
}

## assign symbols as names
all_sym <- substr( all_companies, 6, 100 )
names( dat ) <- all_sym

## make meta data frame
unl <- unlist(l, recursive = FALSE )
meta <- c( unl[2:500], list(last_company) )
## save( dat, file = "C:/LocalData/WZHTSS/_data/QUANDL_US_STOCKS_data.rda" )
## save( meta, file = "C:/LocalData/WZHTSS/_data/QUANDL_US_STOCKS_meta.rda" )

dj30 <- read.csv2( file = "C:/LocalData/WZHTSS/_data/INDEX_MEMB_DJ30-20130920.csv",
                   stringsAsFactors = FALSE )
sp500 <- read.csv2( file = "C:/LocalData/WZHTSS/_data/INDEX_MEMB_SP500-20131221.csv",
                    stringsAsFactors = FALSE )

stopifnot( all(dj30$Symbol %in% all_sym) )
stopifnot( all(sp500$Symbol %in% all_sym) )

## 30 US Stocks Adj Close price reflecting the members of the DJ 30 as of 2013-09-20
l <- lapply( dj30$Symbol, function(sym) dat[[sym]][,"Adj. Close"] )
US30 <- do.call(cbind, l)[ "19981231::20131231" ]
colnames( US30 ) <- dj30$Symbol
save( US30, file = "../data/US30.rda", compress = "bzip2" )

## 500 US Stocks Adj Close price reflecting the members of the S&P 500 as of 2013-12-21
l <- lapply( sp500$Symbol, function(sym) dat[[sym]][,"Adj. Close"] )
US500 <- do.call(cbind, l)[ "20031231::20131231" ]
colnames( US500 ) <- sp500$Symbol

## for daily data remove NA dates
##idx_na <- apply( coredata(US500), 1, function(x) all( is.na(x)) )
##US500 <- US500[!idx_na, ]

## generate weekly data
idx_weeks <- endpoints( index(US500), on = "weeks" )
US500 <- US500[idx_weeks, ]

save( US500, file = "../data/US500.rda", compress = "bzip2"  )
