q("no")
Rdevel

Sys.setenv(ROI_LOAD_PLUGINS = FALSE)
library(ROI)
library(ROI.models.globalOptTests)

ROI.models.globalOptTests:::globopt("")


start <- apply(rbind(bounds(globopt(x = "Rastrigin"))$lower$val,
                     bounds(globopt(x = "Rastrigin"))$upper$val), 2,
               function(x) runif(1, min(x), max(x)))

ROI_applicable_solvers(globopt(x = "Rastrigin"))

z <- ROI_solve( globopt(x = "Rastrigin"), solver = "nloptr", start = start, method = "NLOPT_GD_STOGO")
solution(z)
z

z <- ROI_solve(globopt(x = "Rastrigin"), solver = "nloptr", 
                   start = start, method = method)

for (i in 1:100) {
    start <- runif(10, -1, 1)
    ## method <- "NLOPT_GN_CRS2_LM"
    method <- "NLOPT_GN_ISRES"
    ## method <- "NLOPT_GD_STOGO_RAND"
    ## method <- "NLOPT_LD_SLSQP"
    ## method <- "NLOPT_GD_STOGO"
    z <- ROI_solve(globopt(x = "Rastrigin"), start = start)
    print(solution(z))
    print(solution(z, "objval"))
}

objective(globopt(x = "Rastrigin"))(double(10))

globopt("metainfo")["Rastrigin", "dimension"]
globopt("metainfo")["Rastrigin", "optimum"]


z <- ROI_solve( globopt(x = "Rastrigin"), start =  double(10))
abs(solution(z, "objval") - globopt("metainfo")["Rastrigin", "optimum"])



##
##
##
q("no")
Rdevel

library(globalOptTests)
Sys.setenv(ROI_LOAD_PLUGINS = FALSE)
library(ROI)
library(ROI.models.globalOptTests)

globopt()
## get all test problems

## get a single problem
globopt("MieleCantrell")
## get the meta information
globopt("metainfo")

x <- globopt("all")

probs <- ROI.models.globalOptTests:::globopt_names()

for ( i in seq_along(probs) ) {
    test_name <- probs[i]
    test_name
    i
    n <- getProblemDimen(test_name)
    db <- getDefaultBounds(test_name)
    vb <- V_bound(li=seq_along(db$lower), ui=seq_along(db$upper), 
                  lb=db$lower, ub=db$upper)
    fun <- function(x) {
        finiteize(goTest(x, test_name))
    }
    fun(rep.int(0, n))
    fobj <- F_objective(F= fun, n=n)
    OP(objective=fobj, bounds=vb)
}


build_ROI_object(test_name)

finiteize <- function(x) {
    if ( !is.finite(x) ) .Machine[["double.xmax"]] else x
}
