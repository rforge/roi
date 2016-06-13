## ---------------------------------------------------------
##
##  bound
##  =====
##' @title bound
##' @description \pkg{ROI} distinguishes between 3 different 
##'   types of bounds: 
##'   \itemize{
##'   	\item No Bounds \code{NO_bound} 
##'   	\item Variable Bounds \code{\link{V_bound}} (inherits from \code{"bound"})
##'   	\item Conic Bounds \code{\link{C_bound}} (inherits from \code{"bound"})
##'   }
##' @param x object to be tested
##' @param \ldots arguments (inheriting from bound) to be combined
##' @details \pkg{ROI} provides the methods \code{\link{V_bound}} and 
##'   \code{\link{C_bound}} to be used as constructors for the corresponding bounds.
##'   \code{NO_bound} is not explicitly implemented but represented by \code{NULL}.
##' @name bound (Constructors)
##' @rdname ROI_bound
## ---------------------------------------------------------
NULL

available_bound_classes <- function()
    c(C = "C_bound", V = "V_bound", X = "NO_bound")

valid_bound <- function(x) x %in% names(available_bound_classes())

available_cone_types <- function()
    c("free", "nonneg", "soc", "psd", "expp", "expd", "powp", "powd")

valid_cone <- function(x) x %in% available_cone_types()

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

## A utility function to combine values of type C_bound
c_C_bound <- function(...) {
    x <- unlist(lapply(list(...), "[[", "cones"), recursive = FALSE)
    for (n in unique(names(x))) {
        i <- which(names(x) == n)
        if ( length(i) > 1 ) {
            tmp <- unlist(x[i], recursive=FALSE, use.names=FALSE)
            x <- x[-i]
            x[[n]] <- tmp
        }
    }
    structure(list(cones=x), class = c("C_bound", "bound"))
}

c_2_cones <- function(x, y) {
    cone <- list()
    for (co in union(names(x), names(y))) {
        cone[[co]] <- c(x[[co]], y[[co]])
    }
    return(cone)
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
    ## C_bound
    z$cones <- c_2_cones(x$cones, y$cones)
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
print.bound <- function(x, ...){
    if ( !is.null(x$lower) ) print.V_bound(x, ...)
    if ( !is.null(x$cones) ) {
        if ( !is.null(x$lower) ) writeLines("\n")
        print.C_bound(x, ...)
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
V_bound <- function( li, ui, lb, ub, nobj = max(li, ui) ) {
    if ( missing(li) ) li <- integer()
    if ( missing(ui) ) ui <- integer()
    if ( missing(lb) ) lb <- double()
    if ( missing(ub) ) ub <- double()
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
        if( (max_li <- max(li)) > nobj )
            stop("indices must not exceed number of objective coefficients.")
    if ( length(ui) )
        if( max(ui) > nobj )
            stop("indices must not exceed number of objective coefficients.")
    if ( any(lb >= Inf) )
        stop("lower bound cannot be 'Inf'.")
    if ( any(ub <= -Inf) )
        stop("upper bounds cannot be '-Inf'.")
    ##if ( length(li) & length(ui) & (max_li < min(ui)) ) {
    ##    m <- match(li, ui)
    ##    b <- !is.na(m)
    ##    if ( any(lb[b] > ub[m[b]]) | any(lb[li %in% ui[ub < 0]] > 0) ) {
    ##        error("MISSPECIFICATION", "lower bounds must not exceed upper bounds.", "V_bound")
    ##    }
    ##}
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
##'   (normally \code{'\link{V_bound}'} or \code{'\link{C_bound}'}).
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
   x$bounds <- as.V_bound(value)
   x
}

.make_standard_bounds <- function( x )
  NULL

.as_int_with_check <- function( x, type ) {
    if ( type %in% c("expp", "expd", "powp", "powd") ) {
        if ( length(x) != 3L ) {
            stop("conic bounds of type '", type, 
                 "' have to be of length 3", call. = FALSE)
        }
    }
    as.integer(x)
}

## ---------------------------------------------------------
##
## C_bound
## =======
##   
## NOTES:
##  - ROI models Conic Constraints as Linear Contraints, where
##    the bounds are set to be in a Cone.
##
## ---------------------------------------------------------
##' @title Conic Bounds
##' @description Construct a conic bounds object.
##' @details 
##'   The \pkg{ROI} cone formulation sticks closely to the formulation
##'   used by the solvers CVXOPT, ECOS and SCS.
##'   \deqn{minimize \ c^\top x \ \ \ s.t. \ A x + s  = b \ \ \ s \in K \ \ x \in R^n}
##' @references 
##'   \code{[CVXOPT]}  Andersen, Martin S and Dahl, Joachim and Vandenberghe, Lieven (2016)
##'   CVXOPT: A Python package for convex optimization, version 1.1.8,
##'   \url{http://cvxopt.org/}
##'   \cr \cr
##'   \code{[ECOS]}  Domahidi, A. and Chu, E. and Boyd, S. (2013) 
##'   {ECOS}: {A}n {SOCP} solver for embedded systems. 
##'   European Control Conference (ECC), 3071-3076
##'   \cr \cr
##'   \code{[SCS]}  O'Donoghue, Brendan and Chu, Eric and Parikh, Neal and Boyd, Stephen (2016)
##'   Conic Optimization via Operator Splitting and Homogeneous Self-Dual Embedding
##'   Journal of Optimization Theory and Applications, 1-27
##' @param ... arguments which give the row index in the A matrix or 
##'   objects to be combined.
##' @param type an character giving the type of the bound, 
##'   valid types are \code{"free"}, \code{"nonneg"}, \code{"soc"}, \code{"psd"}, 
##'   \code{"expp"}, \code{"expd"}, \code{"powp"} and \code{"powd"}.
##' @param x an object to be coerced or tested.
##' @return An S3 object of class \code{"C_bound"} containing the conic
##'         bounds.
##' @examples
##' conic_bounds <- c(C_bound(1, type="free"), C_bound(2:4, type="soc"))
##' cones <- list("free"=c(1), "soc"=list(2:4))
##' conic_bounds <- as.C_bound(cones)
##' cones <- list("free"=c(1, 2), "expd"=list(3:5))
##' bound <- as.C_bound(cones)
##' @export
C_bound <- function(..., type=c("free", "nonneg", "soc", "psd", "expp", "expd", "powp", "powd") ) {
    if ( length(c(...)) == 0 ) return(NULL)
    if ( type %in% c("free", "nonneg", "expp", "expd", "soc", "psd") ) {
        x <- c(...)
        if ( type %in% c("soc", "psd", "expp", "expd") ) {
            if ( !all(sapply(x, is.numeric)) )
            stop("type missmatch a vector of type integer or numeric or a list ",
                 "of numeric vectors is required  ", 
                 "to create conic bounds of type '", type, "'!")
            if ( is.list(x) ) {
                x <- lapply(x, .as_int_with_check, type=type)
            } else {
                x <- list(.as_int_with_check(x, type))
            }
            x <- setNames(list(x), type)
        } else {
            if ( !is.numeric(x) )
            stop("type missmatch a vector of type integer or numeric is required",
                 " to create conic bounds of type '", type, "'!")
            x <- setNames(list(as.integer(x)), type)
        }
    } else if ( type %in% c("powp", "powd") ) {
        x <- list(...)
        if ( !"a" %in% names(x) )
            stop("for power cones the parameter 'a' ",
                 "is mandatory!\n e.g. C_bound(1, 2, 3, a=0.3, type='powp')")
        a <- x[["a"]]
        x <- unlist(x[-which( names(x) == "a" )])
        x <- .as_int_with_check(x, type=type)
        x <- setNames(list(list(list(i=x, a=a))), type)
    } else {
        stop("unknown cone type: ", type,
             "! Allowed values are: ", shQuote(available_cone_types()))        
    }
    return( structure(list(cones=x), class=c("C_bound", "bound")) )
}

##' @rdname C_bound
##' @export
c.C_bound <- c.bound


##' @noRd
##' @export
print.C_bound <- function(x, ...) {
    writeLines( "ROI Conic Bounds:\n" )
    for ( cone in available_cone_types() ) {
        n <- length(x$cones[[cone]])
        if (n > 0) {
            msg <- sprintf("\t%i %s conic bounds", n, cone)
            writeLines(msg)            
        }
    }   
    invisible(NULL)
}

##  C_bounds
## 
##  Function to coerce to \code{"C_bound"}.
##  @param x object to be coerced.
##' @rdname C_bound
##' @export
as.C_bound <- function( x ){
    UseMethod( "as.C_bound" )
}

##' @noRd
##' @export
as.C_bound.C_bound <- identity

##' @noRd
##' @export
as.C_bound.NULL <- function( x ) structure(list(cones=NULL), class=c("C_bound", "bound"))

##' @rdname C_bound
##' @export
is.C_bound <- function(x) inherits(x, "C_bound")

##' @noRd
##' @export
as.C_bound.list <- function( x, ... ) {
    y <- as.C_bound(NULL)
    for ( cone in available_cone_types() ) {
        if ( cone %in% c("soc", "psd", "expp", "expd") ) {
            for ( soc in x[[cone]] ) {
                y <- c(y, C_bound( soc , type=cone))
            }
        } else if ( cone %in% c("powp", "powd") ) {
            for ( pow in x[[cone]] ) {
                y <- c(y, C_bound(unlist(pow[-which(names(pow)=="a")]), a=pow$a , type=cone))
            }
        } else {
            y <- c(y, C_bound( x[[cone]] , type=cone))
        }
    }
    structure(y, class=c("C_bound", "bound"))
}

##' @noRd
##' @export
as.list.C_bound <- function( x, ... ) unclass( x )
