

build_ROI_object <- function(test_name) {
    n <- getProblemDimen(test_name)
    db <- getDefaultBounds(test_name)
    vb <- V_bound(li=seq_along(db$lower), ui=seq_along(db$upper), 
                  lb=db$lower, ub=db$upper)
    fun <- function(x) {
      finiteize(goTest(x, test_name))
    }
    fobj <- F_objective(F= fun, n=n)
    OP(objective=fobj, bounds=vb)
}

finiteize <- function(x) {
    if ( !is.finite(x) ) .Machine[["double.xmax"]] else x
}

globopt_names <- function() {
    c("Ackleys", "AluffiPentini", "BeckerLago", "Bohachevsky1", "Bohachevsky2", 
      "Branin", "Camel3", "Camel6", "CosMix2", "CosMix4", "DekkersAarts", 
      "Easom", "EMichalewicz", "Expo", "GoldPrice", "Griewank", "Gulf", "Hartman3", 
      "Hartman6", "Hosaki", "Kowalik", "LM1", "LM2n10", "LM2n5", "McCormic",
      "MeyerRoth", "MieleCantrell", "Modlangerman", "ModRosenbrock", "MultiGauss", 
      "Neumaier2", "Neumaier3", "Paviani", "Periodic", "PowellQ", "PriceTransistor", 
      "Rastrigin", "Rosenbrock", "Salomon", "Schaffer1", "Schaffer2", "Schubert", 
      "Schwefel", "Shekel10", "Shekel5", "Shekel7", "Shekelfox5", "Wood", 
      "Zeldasine10", "Zeldasine20")
}

##  -----------------------------------------------------------
##  globopt
##  =======
##' @title Access \code{globalOptTests}
##' @description
##'     Get one or more optimization problems, meta information or a listing
##'     of the available \code{globalOptTests} problems.
##' @param x a character giving the names of the optimization problems
##'        to be returned, if \code{x} is \code{"all"} all available problems
##'        are returned, if \code{x} is the name of a single problem the
##'        given problem is returned. If \code{x} is missing a listing 
##'        of all available problems is returned. If \code{x} is \code{"metainfo"}
##'        the meta information about the problems is returned.
##' @examples
##' ## list all available MIPLIB-2010 problems
##' globopt()
##' ## get all miplib problems
##' globopt("all")
##' ## get a single problem
##' globopt("MieleCantrell")
##' ## get the meta information
##' globopt("metainfo")
##  -----------------------------------------------------------
globopt <- function(x = c("all", "metainfo", "Ackleys", "AluffiPentini", "BeckerLago", 
                          "Bohachevsky1", "Bohachevsky2", "Branin", "Camel3", "Camel6", 
                          "CosMix2", "CosMix4", "DekkersAarts", "Easom", "EMichalewicz", 
                          "Expo", "GoldPrice", "Griewank", "Gulf", "Hartman3", "Hartman6", 
                          "Hosaki", "Kowalik", "LM1", "LM2n10", "LM2n5", "McCormic",
                          "MeyerRoth", "MieleCantrell", "Modlangerman", "ModRosenbrock", 
                          "MultiGauss", "Neumaier2", "Neumaier3", "Paviani", "Periodic", 
                          "PowellQ", "PriceTransistor", "Rastrigin", "Rosenbrock", 
                          "Salomon", "Schaffer1", "Schaffer2", "Schubert", "Schwefel", 
                          "Shekel10", "Shekel5", "Shekel7", "Shekelfox5", "Wood", 
                          "Zeldasine10", "Zeldasine20")) {

    if ( missing(x) ) {
        return(globopt_names())
    }

    problem_name <- match.arg(x)
    if ( isTRUE(x == "all") ) {
        return(lapply(globopt_names(), build_ROI_object))
    }

    if ( isTRUE(x == "metainfo") ) {
        x <- globopt_names()
        df <- data.frame(name = x,
                         dimension = as.integer(lapply(x, getProblemDimen)),
                         stringsAsFactors = FALSE)
        df$lower <- lapply(x, function(name) getDefaultBounds(name)[["lower"]])
        df$upper <- lapply(x, function(name) getDefaultBounds(name)[["upper"]])
        df$optimum <- as.double(lapply(x, getGlobalOpt))
        rownames(df) <- x
        return(df)
    }

    build_ROI_object(problem_name)
}

