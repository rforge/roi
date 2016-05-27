
q("no")
Rdevel

check <- function(x, y, eps=1e-5) {
    has.error <- !all( abs(x - y) < eps )
    if ( any(is.na(has.error)) ) has.error <- TRUE
    if ( any(has.error) ) {
        cat(testthat:::colourise("\tERROR!", "error"))
    } else {
        cat(testthat:::colourise("\tOK!", "passed"))
    }
}

ROI_compare <- function(x, y, eps=1e-5) {
    !all( abs(x - y) < eps )
}

## install.packages("testthat")
library( "testthat" )
library( "ROI" )

getwd()
setwd("/home/florian/work/Optimization/ROI/ROI/devel/ROI/inst/unit_test")

SOLVER <- "ecos"
SOLVER <- "scs"
SOLVER <- "cplex"

## -------------------------------------
## Linear Programming
## -------------------------------------
dir("LP")
source( file.path("LP", "lp_1.R") )

## -------------------------------------
## Quadratic Programming
## -------------------------------------
source( file.path("QP", "socp_1.R") )
source( file.path("CP", "socp_2.R") )

## -------------------------------------
## Mixed Integer Linear Programming
## -------------------------------------
dir("MILP")
source( file.path("MILP", "milp_1.R") )
source( file.path("MILP", "milp_2.R") )

## -------------------------------------
## Mixed Integer Quadratic Programming
## -------------------------------------
dir("MIQP")
source( file.path("MIQP", "miqp_1.R") )

## -------------------------------------
##
## Conic Programming
##
## -------------------------------------
dir("CP")

## -------------------------------------
## Free Cone Problems
## -------------------------------------


## -------------------------------------
## Linear Cone Problems
## -------------------------------------


## -------------------------------------
## Second-Order Cone Problems
## -------------------------------------
source( file.path("CP", "socp_1.R") )
source( file.path("CP", "socp_2.R") )

## -------------------------------------
## Positive Semidefinite Cone
## -------------------------------------
## TODO: source( file.path("CP", "sdp_1.R") )
source( file.path("CP", "sdp_2.R") )

## -------------------------------------
## Exponential Cone
## -------------------------------------
source( file.path("CP", "expp_1.R") )
source( file.path("CP", "expp_2.R") )
source( file.path("CP", "expp_3.R") )

## -------------------------------------
## Dual Exponential Cone
## -------------------------------------
source( file.path("CP", "expd_1.R") )

## -------------------------------------
## Power Cone
## -------------------------------------
source( file.path("CP", "powp_1.R") )

## -------------------------------------
## Dual Power Cone
## -------------------------------------
source( file.path("CP", "powd_1.R") )
