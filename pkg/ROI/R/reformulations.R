
is.objective <- function(x) {
    inherits(x, "objective")
}

is.L_objective <- function(x) {
    (inherits(x, "L_objective", TRUE) == 2L)
}

is.Q_objective <- function(x) {
    (inherits(x, "Q_objective", TRUE) == 2L)
}

is.F_objective <- function(x) {
    (inherits(x, "F_objective", TRUE) == 2L)
}

##
## Define default signatures
##
LPLC.C <- function() {
    ROI_plugin_make_signature( objective = "L",
                               constraints = c("X", "L"),
                               types = c("C"),
                               bounds = c("X", "V"),
                               cones = c("X"),
                               maximum = c(TRUE, FALSE) )
}

LPLC.BC <- function() {
    ROI_plugin_make_signature( objective = "L",
                               constraints = c("X", "L"),
                               types = c("B", "C"),
                               bounds = c("X", "V"),
                               cones = c("X"),
                               maximum = c(TRUE, FALSE) )
}

LPLC.BCI <- function() {
    ROI_plugin_make_signature( objective = "L",
                               constraints = c("X", "L"),
                               types = c("B", "I", "C"),
                               bounds = c("X", "V"),
                               cones = c("X"),
                               maximum = c(TRUE, FALSE) )
}

QPLC.B <- function() {
    ROI_plugin_make_signature( objective = "Q",
                               constraints = c("X", "L"),
                               types = c("B"),
                               bounds = c("X", "V"),
                               cones = c("X"),
                               maximum = c(TRUE, FALSE) )
}

QPLC.BCI <- function() {
    ROI_plugin_make_signature( objective = "Q",
                               constraints = c("X", "L"),
                               types = c("B", "I", "C"),
                               bounds = c("X", "V"),
                               cones = c("X"),
                               maximum = c(TRUE, FALSE) )
}


LPLC.BCI.SOC <- function() {
    ROI_plugin_make_signature( objective = "L",
                               constraints = c("X", "L", "C"),
                               types = c("B", "I", "C"),
                               bounds = c("X", "V"),
                               cones = c("X", "zero", "nonneg", "soc"),
                               maximum = c(TRUE, FALSE) )
}

LPLC.BCI.PSD <- function() {
    ROI_plugin_make_signature( objective = "L",
                               constraints = c("X", "L", "C"),
                               types = c("B", "I", "C"),
                               bounds = c("X", "V"),
                               cones = c("X", "zero", "nonneg", "psd"),
                               maximum = c(TRUE, FALSE) )
}

.linearize_BQP <- function(x) {
    ## Linearize an all-binary quadratic program
    ##   \sum_{i,j} q_{ij} x_i x_j / 2 + \sum_i c_i x_i
    ## as described e.g. in "Pseudo-Boolean Optimization" by E. Boros
    ## and P. Hammer (boros01pseudoboolean.pdf): rewrite the criterion
    ## function as
    ##   \sum_{i < j} r_{ij} y_{ij} + \sum_i s_i x_i
    ## with
    ##  r_{ij} = (q_{ij} + q_{ji}) / 2
    ##  s_i = c_i + q_{ii} / 2
    ## and the additional constraints
    ##   y_{ij} <= x_i, y_{ij} <= x_j              (A)
    ##   y_{ij} >= 0, y_{ij} >= x_i + x_j - 1      (B)
    ## where for a minimization problem (A) is redundant if r_{ij} > 0
    ## and (B) if \gamma_{ij} < 0, and vice versa for a maximization
    ## problem.

    if(!is.Q_objective(objective(x)) && !identical(unique(x$types), "B"))
        stop("Can only linearize all-binary quadratic programs.")

    ## Could do some sanity checking here.
    Q <- terms(objective(x))$Q
    c <- as.vector(terms(objective(x))$L)
    n <- length(c)
    R <- (Q + t(Q)) / 2
    if ( is.simple_triplet_matrix(Q) ) {
        ## Transform coefficients.
        ## Cannot easily have a diag() method for simple triplet
        ## matrices.
        s <- c + Q[cbind(seq_len(n), seq_len(n))] / 2
        ## Quadratic coefficients and respective variables.
        p <- (R$i < R$j) & (R$v != 0)
        i <- R$i[p]
        j <- R$j[p]
        r <- R$v[p]
    } else {
        ## Transform coefficients.
        s <- c + diag(Q) / 2
        ## Quadratic coefficients and respective variables.
        I <- upper.tri(R)
        r <- R[I]
        p <- which(r != 0)
        I <- which(I, arr.ind = TRUE)
        i <- I[p, 1L]
        j <- I[p, 2L]
        r <- r[p]
    }
    nr <- length(r)

    ## Constraints.
    mat <- constraints(x)$L ## x$constraints$mat
    pn <- which(r < 0)                  # Negative positions.
    pp <- which(r > 0)                  # Positive positions.
    ## <NOTE>
    ## To experiment with not dropping redundant constraints, do:
    ##    pn <- pp <- seq_along(r)
    ## </NOTE>
    npn <- length(pn)
    npp <- length(pp)
    if ( x$maximum ) {
        if ( is.simple_triplet_matrix(mat) ) {
            add_i <- c(rep.int(seq_len(npp), 2L),
                       rep.int(seq_len(npp) + npp, 2L),
                       rep.int(seq_len(npn) + 2L * npp, 3L))
            add_j <- c(i[pp], n + pp,
                       j[pp], n + pp,
                       i[pn], j[pn], n + pn)
            add_v <- rep.int(c(-1, 1, -1, 1, -1, 1),
                             c(npp, npp, npp, npp, 2L * npn, npn))
            mat <- rbind(cbind(mat,
                               simple_triplet_zero_matrix(nrow(mat), nr)),
                         simple_triplet_matrix(add_i, add_j, add_v,
                                               npn + 2L * npp, n + nr))
        } else {
            add <- matrix(0, npn + 2L * npp, n + nr)
            ## Constraints
            ##    y_{ij} <= x_i, y_{ij} <= x_j             (A)
            ## if r_{ij} > 0:
            ind <- seq_len(npp)
            add[cbind(ind, i[pp])] <- -1
            add[cbind(ind, n + pp)] <- 1
            ind <- ind + npp
            add[cbind(ind, j[pp])] <- -1
            add[cbind(ind, n + pp)] <- 1
            ## Constraints
            ##   y_{ij} >= 0, y_{ij} >= x_i + x_j - 1      (B)
            ## if r_{ij} < 0 (where the former is implicit):
            ind <- seq_len(npn) + 2L * npp
            add[cbind(ind, i[pn])] <- -1
            add[cbind(ind, j[pn])] <- -1
            add[cbind(ind, n + pn)] <- 1
            mat <- rbind(cbind(mat, matrix(0, nrow(mat), nr)), add)
        }
        dir <- c(x$constraints$dir,
                 rep.int("<=", 2L * npp),
                 rep.int(">=", npn))
        rhs <- c(x$constraints$rhs,
                 rep.int(0, 2L * npp),
                 rep.int(-1, npn))
    } else {
        if( is.simple_triplet_matrix(mat) ) {
            add_i <- c(rep.int(seq_len(npn), 2L),
                       rep.int(seq_len(npn) + npn, 2L),
                       rep.int(seq_len(npp) + 2L * npn, 3L))
            add_j <- c(i[pn], n + pn,
                       j[pn], n + pn,
                       i[pp], j[pp], n + pp)
            add_v <- rep.int(c(-1, 1, -1, 1, -1, 1),
                             c(npn, npn, npn, npn, 2L * npp, npp))
            mat <- rbind(cbind(mat,
                               simple_triplet_zero_matrix(nrow(mat), nr)),
                         simple_triplet_matrix(add_i, add_j, add_v,
                                               npp + 2L * npn, n + nr))
        } else {
            add <- matrix(0, 2L * npn + npp, n + nr)
            ## Constraints
            ##    y_{ij} <= x_i, y_{ij} <= x_j             (A)
            ## if r_{ij} < 0:
            ind <- seq_len(npn)
            add[cbind(ind, i[pn])] <- -1
            add[cbind(ind, n + pn)] <- 1
            ind <- ind + npn
            add[cbind(ind, j[pn])] <- -1
            add[cbind(ind, n + pn)] <- 1
            ## Constraints
            ##   y_{ij} >= 0, y_{ij} >= x_i + x_j - 1      (B)
            ## if r_{ij} > 0 (where the former is implicit):
            ind <- seq_len(npp) + 2L * npn
            add[cbind(ind, i[pp])] <- -1
            add[cbind(ind, j[pp])] <- -1
            add[cbind(ind, n + pp)] <- 1
            mat <- rbind(cbind(mat, matrix(0, nrow(mat), nr)), add)
        }
        dir <- c(x$constraints$dir,
                 rep.int("<=", 2L * npn),
                 rep.int(">=", npp))
        rhs <- c(x$constraints$rhs,
                 rep.int(0, 2L * npn),
                 rep.int(-1, npp))
    }

    OP(objective = L_objective(L = c(s, r)),
       constraints = L_constraint(L = mat, dir = dir, rhs = rhs),
       bounds = bounds(x),
       types = rep.int(c("B", "C"), c(n, nr)),
       maximum = x$maximum)
}

reduce_signature <- function(x) {
    lapply(x, unique)
}

## checks if x is a subset from y
is.subset <- function(x, y) {
    stopifnot(all(names(x) == names(y)))    
    for (i in seq_along(x)) {
        if (!all(x[[i]] %in% y[[i]])) {
            return(FALSE)
        }
    }
    return(TRUE)
}

## as_dgCMatrix <- function( x, ... ) {
##     if (class(x) == "simple_triplet_matrix")
##         return( Matrix::sparseMatrix( i=x$i, j=x$j, x=x$v,
##                                       dims=c(x$nrow, x$ncol) ) )
##     as(x, "dgCMatrix")
## }

## interesting
ReformulationDatabase <- function() {
    env <- new.env(parent = emptyenv())
    env$from <- list()
    env$to <- list()
    env$methods <- list()
    env$meta <- data.frame(name = character(), description = character(), 
                           cite = character(), author = character(),
                           stringsAsFactors = FALSE)

    ## NOTE: The combination of apply and paste adds sometimes unwanted spaces
    ##       therefore we need gsub to canonicalize.
    to_id <- function(x) {
        gsub(" ", "", apply(x, 1, paste, collapse = ""), fixed = TRUE)
    }

    env$append <- function(from, to, method_name, method, 
                           description = "", cite = "", author = "") {
        self <- parent.env(environment())$env
        if ( method_name %in% names(self$methods) ) {
            msg <- sprintf("reformulation '%s' already exists!", method_name)
            stop(msg)
        }
        
        i <- NROW(self$meta) + 1L
        self$meta[i, "name"] <- method_name
        self$meta[i, "description"] <- description
        self$meta[i, "cite"] <- cite
        self$meta[i, "author"] <- author

        from_keys <- to_id(from)
        to_keys <- to_id(to)
        id <- length(self$methods) + 1L
        self$methods[[id]] <- method
        names(self$methods)[[id]] <- method_name
        for (key in from_keys) {
            self$from[[key]] <- c(self$from[[key]], id)
        }
        self$to[[length(self$to) + 1L]] <- reduce_signature(to)
        invisible(NULL)
    }

    env$get <- function(from, to, method_name =  NULL) {
        self <- parent.env(environment())$env
        from_key <- to_id(from)
        fids <- self$from[[from_key]]
        tids <- which(sapply(self$to, function(x) is.subset(x, to)))
        applicable_methods <- intersect(fids, tids)
        if ( length(applicable_methods) == 0 ) {
            return(structure(list(msg="no applicable method"), class="roi_error"))
        }
        
        if ( is.null(method_name) ) {
            method <- self$methods[[applicable_methods[1]]]
        } else {
            namen <- names(self$methods)
            k <- which(method_name %in% namen[applicable_methods])
            if ( !length(k) ) {
                return(structure(list(msg="method not applicable"), class="roi_error"))
            }
            method <- self$methods[[applicable_methods[k]]]
        }
        method
    }
    env
}

qp_to_socp <- function(x) {
    Q <- terms(objective(x))[['Q']]
    L <- terms(objective(x))[['L']]
    if ( x$maximum ) {
        Q <- -Q
        L <- -L
        x$maximum <- FALSE
    }
    ## fix zeros in the diagonal
    k <- setdiff(seq_len(NROW(Q)), Q$i[Q$i == Q$j])
    if ( length(k) > 0 ) {
        Q[k, k] <- 1e-12
    }
    ## Q <- as_dgCMatrix( Q )
    F <- chol(Q)
    Finv <- backsolve(F, diag(1, nrow(F)))
    a <- numeric(length(objective(x)) + 1L)
    a[length(a)] <- 1
    lo <- L_objective(L = a)

    L1 <- as.matrix(cbind(rbind(0, -F), c(-1, rep.int(0, nrow(F)))))
    rhs <- c(0, as.vector(t(Finv) %*% as.vector(L)))

    ## lc <- L_constraint(L = L1, dir = eq(nrow(L1)), rhs = rhs)
    ## x$constraints$L <- cbind(x$constraints$L, numeric(x$constraints$L$nrow))
    ## lc <- rbind(lc, constraints(x))
    ## n1 <- nrow(constraints(x))
    ## bo <- c(C_bound(seq_len(nrow(L1)), type = "soc"),
    ##         C_bound(nrow(L1) + seq_len(n1), type = "zero"),
    ##         bounds(x), V_bound(li=length(a), lb=-Inf))
    c.L <- constraints(x)$L
    con <- C_constraint(L = rbind(L1, cbind(c.L, numeric(NROW(c.L)))),
                        cones = c(K_soc(NROW(L1)), K_zero(nrow(c.L))),
                        rhs = c(rhs, constraints(x)$rhs))
    bo <- c(bounds(x), V_bound(li=length(a), lb=-Inf))

    if ( is.null(x$types) ) {
        ty <- NULL
    } else {
        ty <- c(x$types, "C")
    }
    OP(objective = lo, constraints = con, types = ty, bounds = bo)
}

##  -----------------------------------------------------------
##  ROI_reformulate
##  ===============
##' @title Reformulate a Optimization Problem
##'
##' @description Register a new reformulation method.
##' @param x an object of class \code{'OP'} giving the optimization problem.
##' @param to a \code{data.frame} with the supported signatures.
##' @param method a character string giving the name of the method.
##' @details Currently \pkg{ROI} provides two reformulation methods.
##' \enumerate{
##'   \item \code{bqp_to_lp} transforms binary quadratic problems to 
##'       linear mixed integer problems.
##'   \item \code{qp_to_socp} transforms quadratic problems with linear 
##'       constraints to second-order cone problems.
##' }
##' @return the reformulated optimization problem.
##' @family reformulate functions
##' @examples
##' ## Example from 
##' ## Boros, Endre, and Peter L. Hammer. "Pseudo-boolean optimization."
##' ## Discrete applied mathematics 123, no. 1 (2002): 155-225.
##'
##' ## minimize: 3 x y + y z - x - 4 y - z + 6
##'
##' Q <- rbind(c(0, 3, 0), 
##'            c(3, 0, 1), 
##'            c(0, 1, 0))
##' L <- c(-1, -4, -1)
##' x <- OP(objective = Q_objective(Q = Q, L = L), types = rep("B", 3))
##'
##' ## reformulate into a mixed integer linear problem
##' milp <- ROI_reformulate(x, "lp")
##'
##' ## reformulate into a second-order cone problem
##' socp <- ROI_reformulate(x, "socp")
##' 
##' @export
ROI_reformulate <- function(x, to, method = NULL) {
    from <- OP_signature(x)
    is.signature <- function(x) {
        cn <- c("objective", "constraints", "bounds", "cones", "maximum", "C",  "I", "B")
        ( is.data.frame(x) & all(colnames(x) == cn) )
    }
    stopifnot(inherits(x, "OP"), (is.character(to) | is.signature(to)), 
              (is.character(method) | is.null(method)))

    if ( is.character(to) ) {
        to <- switch(to, 
                     lp   = LPLC.BCI(), 
                     socp = LPLC.BCI.SOC(),
                     sdp  = LPLC.BCI.PSD(),
                     NULL)
        if ( is.null(to) ) {
            stop("signature not found")
        }

    }

    fun <- reformulation_db$get(from, to, method)
    if ( inherits(fun, "roi_error") )
        stop(fun$msg)
    fun(x)
}


##  -----------------------------------------------------------
##  ROI_plugin_register_reformulation
##  =================================
##' @title Register Reformulation Method
##'
##' @description Register a new reformulation method to be used with 
##'        \code{\link{ROI_reformulate}}.
##' @param from a data.frame with the supported signatures.
##' @param to a data.frame with the supported signatures.
##' @param method_name a character string giving the name of the method.
##' @param method a function registered as solver method.
##' @param description a optional character string giving a description of what
##'        the reformulation does.
##' @param cite a optional character string indicating a reference, 
##'        such as the name of a book.
##' @param author a optional character string giving the name of the author.
##' @return TRUE on success
##' @family reformulate functions
##' @export
ROI_plugin_register_reformulation <- function(from, to, method_name, method, 
                                              description = "", cite = "", author = "") {
    reformulation_db$append(from, to, method_name, method, description, cite, author)
    invisible(TRUE)
}


##  -----------------------------------------------------------
##  ROI_registered_reformulations
##  =============================
##' @title Registered Reformulations
##' @description Retrieve meta information about the registered reformulations.
##' @return a data.frame giving some information about the registered reformulation 
##'     methods.
##' @family reformulate functions
##' @examples
##' ROI_registered_reformulations()
##' @export
ROI_registered_reformulations <- function() {
    reformulation_db$meta
}

