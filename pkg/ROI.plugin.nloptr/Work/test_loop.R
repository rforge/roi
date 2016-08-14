q("no")

library(ROI)
##library(nloptr)

source("../tests/test_nloptr.R")

for ( i in seq_len(1000) ) {
    print(i)
    test_nlp_03()
}

opts <- as.list(DEBUG)
class(opts) <- "list"
opts

library(nloptr)
library(testthat)

rm(DEBUG)
source("test_hs023.R")
for (i in seq_len(1000)) {
    source("test_hs023.R")
}

opts <- as.list(DEBUG)
class(opts) <- "list"
opts

opts.user <- list("algorithm"            = "NLOPT_LD_MMA",
             "xtol_rel"             = 1.0e-7, 
             "tol_constraints_ineq" = rep( 1.0e-6, 5 ),
             "print_level"          = 0 )
x0 <- c( 3, 1 )

num_constraints_ineq <- 5L
num_constraints_eq <- 0L

dopts <- nloptr:::nloptr.add.default.options(opts.user = opts.user, x = x0,
                                             num_constraints_ineq = num_constraints_ineq, 
                                             num_constraints_eq = num_constraints_eq)

nloptr.get.default.options()[1,]
dopts


paste("num_constraints_ineq:", num_constraints_ineq)

yes <- rep.int("YES", 5)
no <- rep.int("NO", 5)
ifelse(TRUE, yes, no)
ifelse(c(TRUE, FALSE), yes, no)
ifelse(c(TRUE, FALSE, TRUE), yes, no)


source("nloptr.add.default.options.R")

## install.packages("microbenchmark")
library(microbenchmark)

mb <- microbenchmark(
    add0=nloptr.add.default.options(opts.user, x0, num_constraints_ineq, num_constraints_eq),
    add1=nloptr.add.default.options_1(opts.user, x0, num_constraints_ineq, num_constraints_eq),
    add2=nloptr.add.default.options_2(opts.user, x0, num_constraints_ineq, num_constraints_eq),
    add3=nloptr.add.default.options_3(opts.user, x0, num_constraints_ineq, num_constraints_eq)
)

boxplot(mb)





