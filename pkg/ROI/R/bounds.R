## ---------------------------------------------------------
##
##  bound
##  =====
##' @title bound
##' @description \pkg{ROI} distinguishes between 2 different 
##'   types of bounds: 
##'   \itemize{
##'   	\item No Bounds \code{NO_bound} 
##'   	\item Variable Bounds \code{\link{V_bound}} (inherits from \code{"bound"})
##'   }
##' @param x object to be tested
##' @param \ldots arguments (inheriting from bound) to be combined
##' @details \pkg{ROI} provides the method \code{\link{V_bound}} 
##'   as constructor for variable bounds.
##'   \code{NO_bound} is not explicitly implemented but represented by \code{NULL}.
##' @name bound (Constructors)
##' @rdname ROI_bound
## ---------------------------------------------------------
NULL

available_bound_classes <- function()
    c(V = "V_bound", X = "NO_bound")

valid_bound <- function(x) x %in% names(available_bound_classes())

## A utility function to combine values of type V_bound
c_V_bound <- function(...) {
    xb <- list(...) 
    structure( list(lower = list(ind = unlist(lapply(xb, function(x) x$lower$ind)),
                                 val = unlist(lapply(xb, function(x) x$lower$val))),
                    upper = list(ind = unlist(lapply(xb, function(x) x$upper$ind)), 
                                 val = unlist(lapply(xb, function(x) x$upper$val))),
                    nobj = as.integer(max(sapply(xb, "[[", "nobj")))),
              class = c("V_bound", "bound") )
}

c_2_bounds <- function(x, y) {
    z <- list()
    ## V_bound
    z$lower$ind <- c(x$lower$ind, y$lower$ind)
    z$lower$val <- c(x$lower$val, y$lower$val)
    z$upper$ind <- c(x$upper$ind, y$upper$ind)
    z$upper$val <- c(x$upper$val, y$upper$val)
    nobj <- max(c(-1L, x$nobj, y$nobj))
    z$nobj <- if (nobj < 0L) NULL else nobj

    return(z)
}   

##' @rdname ROI_bound
##' @export
c.bound <- function(...)  structure(Reduce(c_2_bounds, list(...)), class="bound")

##' @rdname ROI_bound
##' @export
is.bound <- function(x) inherits(x, "bound")

##' @noRd
##' @export
print.bound <- function(x, ...) {
    if ( (!is.null(x$lower)) | (!is.null(x$upper)) ) {
        print.V_bound(x, ...)
        writeLines("\n")
    }
}

################################################################################
## 'bounds'

################################################################################
##  'V_bound' constructor
##  =====================
##' @title Objective Variable Bounds
##' @description Constructs a variable bounds object.
##' @details
##'   This function returns a sparse representation of objective
##'   variable bounds.
##' @param li an integer vector specifying the indices of non-standard
##' (i.e., values != 0) lower bounds.
##' @param ui an integer vector specifying the indices of non-standard
##' (i.e., values != Inf) upper bounds.
##' @param lb a numeric vector with lower bounds.
##' @param ub a numeric vector with upper bounds.
##' @param nobj an integer representing the number of objective variables
##' @param x object to be coerced or tested.
##' @param \ldots objects to be combined.
##' @return An S3 object of class \code{"V_bound"} containing lower and
##' upper bounds of the objective variables.
##' @examples
#' V_bound(li=1:3, lb=rep.int(-Inf, 3))
#' V_bound(li=c(1, 5, 10), ui=13, lb=rep.int(-Inf, 3), ub=100, nobj=20)
##' @author Stefan Theussl
##' @export
V_bound <- function( li, ui, lb, ub, nobj) {
    if ( missing(li) ) li <- integer()
    if ( missing(ui) ) ui <- integer()
    if ( missing(lb) ) lb <- double()
    if ( missing(ub) ) ub <- double()
    if ( missing(nobj) ) 
        nobj <- max(li, ui)
    li <- as.integer(li)
    ui <- as.integer(ui)
    lb <- as.double(lb)
    ub <- as.double(ub)
    if ( length(lb) ) {
        zero <- lb == 0
        lb <- lb[!zero]
        li <- li[!zero]
    }
    if ( length(ub) ) {
        inf <- ub == Inf
        ub <- ub[!inf]
        ui <- ui[!inf]
    }

    ## Sanity checking
    if ( (length(li) != length(lb)) || (length(ui) != length(ub)) )
        stop("length of indices must be equal to the length of the corresponding values.")
    if ( any(duplicated(li)) || any(duplicated(ui)) )
        stop("duplicated entries in indices.")
    if ( length(li) )
        if( max(li) > nobj )
            stop("indices must not exceed number of objective coefficients.")
    if ( length(ui) )
        if( max(ui) > nobj )
            stop("indices must not exceed number of objective coefficients.")
    if ( any(lb >= Inf) )
        stop("lower bound cannot be 'Inf'.")
    if ( any(ub <= -Inf) )
        stop("upper bounds cannot be '-Inf'.")
    structure( list(lower = list(ind = li, val = lb),
                    upper = list(ind = ui, val = ub),
                    nobj = as.integer(nobj)),
              class = c("V_bound", "bound") )
}

##' @rdname V_bound
##' @export
c.V_bound <- c.bound

##  V_bound
##
##  Function to coerce to \code{"V_bound"}.
##  @param x object to be coerced.
##' @rdname V_bound
##' @export
as.V_bound <- function( x ){
    UseMethod( "as.V_bound" )
}

##' @noRd
##' @export
as.V_bound.V_bound <- identity

##' @noRd
##' @export
as.V_bound.NULL <- function( x )
    .make_standard_bounds()

##' @rdname V_bound
##' @export
is.V_bound <- function(x) inherits(x, "V_bound")

##' @noRd
##' @export
as.list.V_bound <- function( x, ... )
  unclass( x )

##' @noRd
##' @export
print.V_bound <- function(x, ...){
    writeLines( "ROI Variable Bounds:\n" )

    writeLines( sprintf("%d lower and %d upper non-standard variable bounds.",
                        length(x$lower$ind), length(x$upper$ind)) )
}

################################################################################
## 'bounds' extractor functions

##  Extract bounds from its argument (typically \pkg{ROI} objects) and
##  return them.
##
##  Currently, there is no default method. See \code{\link{bounds.OP}}
##  for extracting bounds from \pkg{ROI} objects of class \code{"OP"}.
##  @title Extract Objective Variable Bounds

##' @title Bounds - Accessor and Mutator Functions
##' @description The \link{bounds} of a given optimization problem (\link{OP}) 
##'   can be accessed or mutated via the method \code{'bounds'}.
##' @param x an object of type \code{'OP'} used to select the method.
##' @param value  an object derived from \code{'bound'} 
##'   (\code{'\link{V_bound}'}) or \code{NULL}.
##' @return the extracted bounds object on get and the altered \code{'\link{OP}'}
##'   object on set.
##' @name bounds (Set/Get)
##' @rdname Bounds_Accessor_Mutator
##' @examples
##' \dontrun{
##' lp_obj <- L_objective(c(1, 2))
##' lp_con <- L_constraint(c(1, 1), dir="==", rhs=2)
##' lp_bound <- V_bound(ui=1:2, ub=c(3, 3))
##' lp <- OP(objective=lp_obj, constraints=lp_con, bounds=lp_bound, maximum=FALSE)
##' bounds(lp)
##' x <- ROI_solve(lp)
##' x$objval
##' x$solution
##' bounds(lp) <- V_bound(ui=1:2, ub=c(1, 1))
##' y <- ROI_solve(lp)
##' y$objval
##' y$solution
##' }
##' @export
bounds <- function( x ) UseMethod("bounds")

##  Extract bounds from ROI objects of class \code{"OP"} and return
##  them.
## 
##  @title Extract Objective Variable Bounds
##  @param x an object of class \code{"OP"}.
##  @return an object of class \code{"V_bound"}.
##  @author Stefan Theussl
##' @rdname Bounds_Accessor_Mutator
##' @export
bounds.OP <- function( x ) x$bounds

################################################################################
## 'bounds' replacement functions

##  Replaces the (variable) bounds in R objects (typically ROI
##  objects of class \code{"OP"}).
## 
##  Currently, there is no default method. Bounds in ROI objects of
##  class \code{"OP"} given by the argument \code{x} are replaced with
##  \code{value}, either being an object of class \code{"V_bound"} or
##  \code{NULL} (standard variable bound). The updated \code{"OP"}
##  object will be returned.
##  @title Replacement of Variable Bounds
##  @name bounds-replace
##  @aliases bounds<- bounds<-.OP
##  @usage bounds(x) <- value
##  @param x an R object.
##  @param value an R object.
##  @return the updated object.
##  @author Stefan Theussl
##' @rdname Bounds_Accessor_Mutator
##' @export bounds<-
'bounds<-' <- function( x, value )
    UseMethod("bounds<-")


##' @noRd
##' @export
'bounds<-.OP' <- function( x, value ) {
    if ( is.null(value) ) {
        x["bounds"] <- list(NULL)
    } else {
        x$bounds <- as.V_bound(value)
    }
    x
}

.make_standard_bounds <- function( x )
    NULL
