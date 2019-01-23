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
    z$lower$ind <- c(x$lower$ind, x$nobj + y$lower$ind)
    z$lower$val <- c(x$lower$val, y$lower$val)
    z$upper$ind <- c(x$upper$ind, x$nobj + y$upper$ind)
    z$upper$val <- c(x$upper$val, y$upper$val)
    nobj <- max(c(-1L, x$nobj, y$nobj))
    z$nobj <- if (nobj < 0L) NULL else nobj

    return(z)
}   

##' @rdname ROI_bound
##' @export
c.bound <- function(...)  structure(Reduce(c_2_bounds, list(...)), class=c("V_bound", "bound"))

##' @rdname ROI_bound
##' @export
is.bound <- function(x) inherits(x, "bound")

## ---------------------------------------------------------
##
##  is.default_bound
##  ================
##' @title Check for default bounds
##' @description tests if the given object is an variable bound
##'              which represents default values only 
##'              (i.e., all lower bounds are \code{0} 
##'              and all upper bounds as \code{Inf}).
##' @param x object to be tested
##' @return a logical of length one indicating wether default bounds are given
##' @export
is.default_bound <- function(x) {
    UseMethod("is.default_bound")
}

##' @noRd
##' @export
is.default_bound.NULL <- function(x) {
    TRUE
}

##' @noRd
##' @export
is.default_bound.V_bound <- function(x) {
    ( length(x$lower$ind) + length(x$upper$ind) ) == 0L
}

##' @noRd
##' @export
print.bound <- function(x, ...) {
    if ( (!is.null(x$lower)) | (!is.null(x$upper)) ) {
        print.V_bound(x, ...)
        writeLines("\n")
    }
}

##' @noRd
##' @export
length.V_bound <- function(x) {
    x[["nobj"]]
}

##' @noRd
##' @export
str.V_bound <- function(object, ...) {
    str(unclass(object))
    cat(sprintf(' - attr(*, "class")='))
    str(class(object))
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
##' @param ld a numeric giving lower default bound.
##' @param ud a numeric giving upper default bound.
##' @param names a character vector giving the names of the bounds.
##' @param x object to be coerced or tested.
##' @param \ldots objects to be combined.
##' @return An S3 object of class \code{"V_bound"} containing lower and
##' upper bounds of the objective variables.
##' @examples
#' V_bound(li=1:3, lb=rep.int(-Inf, 3))
#' V_bound(li=c(1, 5, 10), ui=13, lb=rep.int(-Inf, 3), ub=100, nobj=20)
##' @export
V_bound <- function( li, ui, lb, ub, nobj, ld = 0, ud = Inf, names = NULL) {
    stopifnot(is.numeric(ld), length(ld) == 1L, is.numeric(ud), length(ud) == 1L)
    if ( missing(li) ) li <- NULL
    if ( missing(ui) ) ui <- NULL
    if ( missing(lb) ) lb <- double()
    if ( missing(ub) ) ub <- double()
    if ( is.null(lb) ) lb <- double()
    if ( is.null(ub) ) ub <- double()
    if ( missing(nobj) ) nobj <- max(li, ui, length(lb), length(ub))

    if ( is.null(li) ) {
        if ( !length(lb) ) {
            li <- integer()
        } else {
            if ( length(lb) == nobj ) {
                li <- seq_len(nobj)
            } else {
                stop("length mismatch - length of 'lb' must be equal to ",
                     "'nobj' if no index 'li' is provided.")
            }
        }
    }
    if ( is.null(ui) ) {
        if ( !length(ub) ) {
            ui <- integer()    
        } else {
            if ( length(ub) == nobj ) {
                ui <- seq_len(nobj)
            } else {
                stop("length mismatch - length of 'ub' must be equal to ",
                     "'nobj' if no index 'ui' is provided.")
            }
        }
    }
    li <- as.integer(li)
    ui <- as.integer(ui)
    lb <- as.double(lb)
    ub <- as.double(ub)
    
    if ( ld != 0 ) {
        i <- li
        tmp <- lb
        li <- seq_len(nobj)
        lb <- rep.int(ld, nobj)
        lb[i] <- tmp
        if ( any(b <- lb == 0) ) {
            li <- li[!b]
            lb <- lb[!b]
        }
    } else {
        if ( length(lb) ) {
            zero <- lb == 0
            lb <- lb[!zero]
            li <- li[!zero]
        }
    }

    if ( ud != Inf ) {
        i <- ui
        tmp <- ub
        ui <- seq_len(nobj)
        ub <- rep.int(ud, nobj)
        ub[i] <- tmp
        if ( any(b <- ub == Inf) ) {
            ui <- ui[!b]
            ub <- ub[!b]
        }
    } else {
        if ( length(ub) ) {
            inf <- ub == Inf
            ub <- ub[!inf]
            ui <- ui[!inf]
        }
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
                    nobj = as.integer(nobj),
                    names = names),
              class = c("V_bound", "bound") )
}


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

##' @noRd
##' @export
as.V_bound.list <- function(x) {
    stopifnot(any(c("lower", "upper", "nobj") %in% names(x)), 
              all(names(x) %in% c("lower", "upper", "nobj", "names")))

    vb <- V_bound(nobj = max(0, x$nobj, len_vb(x$lower), len_vb(x$upper)))
    vb[["lower"]] <- as.variable_bound(x$lower)
    vb[["upper"]] <- as.variable_bound(x$upper)
    vb
}


len_vb <- function(x) UseMethod("len_vb")
len_vb.NULL <- function(x) 0L
len_vb.numeric <- function(x) length(x)
len_vb.list <- function(x) max(c(0L, x$ind))


as.variable_bound <- function(x) UseMethod("as.variable_bound")

as.variable_bound.NULL <- function(x) list(ind = integer(), val = double())

as.variable_bound.numeric <- function(x) {
    ind <- which(x != 0)
    list(ind = ind, val = x[ind])
}

as.variable_bound.list <- function(x) {
    stopifnot(all(sort(names(x)) == c("ind", "val")))
    x
}


##' @rdname V_bound
##' @export
is.V_bound <- function(x) inherits(x, "V_bound")

##' @noRd
##' @export
as.list.V_bound <- function( x, ... )
  unclass( x )

##' @noRd
##' @export
as.data.frame.V_bound <- function(x, ...) {
    n_of_variables <- length(x)
    to_dense <- function(sparse, n, default_value = 0) {
        dense <- rep(default_value, n)
        if ( !is.null(sparse$ind) ) 
            dense[sparse$ind]  <- sparse$val
        dense
    }
    d <- data.frame(lower = to_dense(x$lower, n_of_variables), 
                    upper = to_dense(x$upper, n_of_variables, Inf), 
                    stringsAsFactors = FALSE)
    d
}

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
    stopifnot( is.null(value) | inherits(value, "bound") )
    if ( is.null(value) ) { ## (-Inf, Inf)
        if ( is.na(x[["n_of_variables"]]) ) {
            ## do nothing
            x["bounds"] <- list(NULL)
        } else {
            #PLANED-API-CHANGE# x$bounds <- V_bound(ld = -Inf, nobj = x[["n_of_variables"]])
            x$bounds <- V_bound(nobj = x[["n_of_variables"]])
        }
    } else if ( is.deferred_bound(value) ) { ## [0, Inf)
        if ( !is.na(x[["n_of_variables"]]) ) {
            x$bounds <- V_bound(nobj = x[["n_of_variables"]])
        } else {
            x$bounds <- value
        }
    } else {
        bounds <- as.V_bound(value)
        if ( is.na(x[["n_of_variables"]]) ) {
            x[["n_of_variables"]] <- length(bounds)
        } else {
            stopifnot(x[["n_of_variables"]] == length(bounds))
        }
        x$bounds <- bounds
    }
    x
}

.make_standard_bounds <- function( x )
    NULL

deferred_bound <- function() {
    structure(list(), class = c("deferred_bound", "bound"))
}

is.deferred_bound <- function(x) {
    inherits(x, "deferred_bound")
}

