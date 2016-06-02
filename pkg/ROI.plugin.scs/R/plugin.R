## ROI plugin: SCS
## based on scs interface

as_dgCMatrix <- function( x, ... ) 
    Matrix::sparseMatrix(i=x$i, j=x$j, x=x$v, dims=c(x$nrow, x$ncol))

cone_dims <- function(x, ctype) {
    wcol <- which(colnames(x) == ctype)
    if ( length(wcol) == 0 ) return(NULL)
    as.integer(table(x$v[x$j == wcol]))
}

cone_counts <- function(x, ctype) {
    wcol <- which(colnames(x) == ctype)
    if ( length(wcol) == 0 ) return(0)
    sum(x$v[x$j == wcol])
}

## SLAM - VECH
##
## unvech
## ======
unvech <- function(x) {
    ## length(vech(M)) := m * (m-1) / 2; where n is the dimension of the m x m matrix M
    n <- as.integer((- 1 + sqrt(1 + 8 * length(x))) / 2)
    k <- scale_which(n)
    x[k] <- x[k] / sqrt(2)
    idx <- seq_len(n)
    i <- unlist(lapply(idx, seq.int, to=n), recursive=FALSE, use.names=FALSE)
    j <- unlist(mapply(rep_len, idx, rev(idx), SIMPLIFY=FALSE, USE.NAMES=FALSE))
    simple_triplet_matrix(c(i, j[k]), c(j, i[k]), c(x, x[k]))
}

##
## svec
## ====
## @param x a symmetric matrix of type numeric
## @return a numeric vector
## example: Let A be defined as,
## a11 a12 a13
## a21 a22 a23
## a31 a32 a33
## then svec(A) returns
## s2 <- sqrt(2)
## c(a11, s2*a21, s2*a31, a22, s2*a32, a33)
## n x (n+1) / 2 = 3 * 2 = 6
## multiplies a lower triangular matrix with sqrt(2)
svec <- function(x) {
    x <- as.matrix(x)
    x[lower.tri(x)] <- sqrt(2) * x[lower.tri(x)]
    as.numeric(x[lower.tri(x, TRUE)])
}

##
## svec_inv
## ========
##
## is the backward transformation
## 
svec_inv <- function(x) {
    m <- length(x)
    n <- as.integer((- 1 + sqrt(1 + 8 * m)) / 2)
    M <- matrix(0, nrow=n, ncol=n)
    M[lower.tri(M, TRUE)] <- x
    M[lower.tri(M)] <- M[lower.tri(M)] / sqrt(2)
    M[upper.tri(M)] <- t(M)[upper.tri(M)]
    M
}

##
## vector_to_psd
## =============
##
## is the backward transformation
## 
vector_to_psd <- function(x) {
    n <- as.integer((- 1 + sqrt(1 + 8 * length(x))) / 2)
    M <- matrix(0, nrow=n, ncol=n)
    M[lower.tri(M, TRUE)] <- x
    M[upper.tri(M)] <- t(M)[upper.tri(M)]
    M
}

## scale_which
## ===========
##  
## gives the indices which should be scaled in an vectorized n x n matrix
##
## @param n an integer giving the dimension of the n x n matrix
##
scale_which <- function(n) {
    fun <- function(x)  cbind( ((x - 1) * n) + seq.int(x+1, n), rep.int(x, n - x) )
    x <- do.call(rbind, lapply(seq_len(n-1), fun))
    vec_correction <- cumsum(seq_len(n) - 1)
    x[,1] - vec_correction[x[,2]]
}

## sym_vec_scale_lower_tri
## scales the off diagonal elements of a vectorized lower triangular matrix
## by a given vector
## @param x a numeric vector containing a lower triangular matrix
## @returns the scaled vector
sym_vec_scale_lower_tri <- function(x, scale=sqrt(2)) {
  i <- 1L
  n <- calc_psd_matrix_dim(length(x))
  fun <- function(y) {
    if ( i == 1 ) {
       ## do nothing 
    } else if ( i > n ) {
       i <<- 1L
       n <<- n - 1
    } else {
       y <- y * scale
    }
    i <<- i + 1L
    return(y)
  }
  sapply(x, fun)
}

to_dense_vector <- function(x, len) {
    y <- rep.int(0L, len)
    if ( is.null(x$ind) ) return(y)
    y[x$ind] <- x$val
    return(y)
}

calc_psd_matrix_dim <- function(m) as.integer((- 1 + sqrt(1 + 8 * m)) / 2)

check_cone_types <- function(x) {
    b = !( x %in% c("free", "nonneg", "soc", "psd", "expp", "expd", "powp", "powd") )
    if ( any(b) ) stop("SCS doesn't support cones of type ",
                       paste(shQuote(x[b]), collapse=" or "), "!")
    invisible(NULL)
}

get_mapping <- function() {
    setNames(c("f", "l", "q", "s", "ep", "ed", "p", "p"), 
             c("free", "nonneg", "soc", "psd", "expp", "expd", "powp", "powd"))
}

build_cone_dims <- function( roi_cones ) {
    map <- get_mapping()
    
    ## check if ecos supports the provided cone types
    check_cone_types( names(roi_cones) )

    cone_dims <- list()
    for (cone in c("free", "nonneg", "expp", "expd")) {
        ## NOTE: ("expp", "expd") since the dimension is know the data structure is more 
        ##       compressed than for soc or psd! list(c(1, 2, 3), c(4, 5, 6)) translates to c(2)
        if ( length(roi_cones[[cone]]) > 0 ) cone_dims[[map[cone]]] <- length( roi_cones[[cone]] )
    }
    if ( length(roi_cones[["soc"]]) > 0 ) {
        cone_dims[[map["soc"]]] <- sapply(roi_cones[["soc"]], length)
    }
    if ( length(roi_cones[["psd"]]) > 0) {
        cone_dims[[map["psd"]]] <- sapply(roi_cones[["psd"]], function(x) calc_psd_matrix_dim(length(x)))
    }
    if ( length(roi_cones[["powp"]])  > 0 ) {
        cone_dims[[map["powp"]]] <- sapply(roi_cones[["powp"]], "[[", "a")
    }
    if ( length(roi_cones[["powd"]]) > 0 ) {
        cone_dims[[map["powd"]]] <- c( cone_dims[[map["pow"]]], 
                                     -sapply(roi_cones[["powd"]], "[[", "a") )
    }
    cone_dims
}

which_scs_default_lower_bounds <- function(lower_bounds) {
    which_inf <- which(is.infinite(lower_bounds$val))
    if ( length(which_inf) ) return(lower_bounds$ind[which_inf])
    return(NULL)
}

as.bound <- function( x, ... ) UseMethod( "as.bound" )
as.bound.bound <- identity
as.bound.NULL <- function( x, ... ) structure(NULL, class="bound")

if (FALSE) {
library(slam)
library(scs)


x <- lp
x <- lp2

x <- prob$op
control=list()
}

absmax <- function(x) max(abs(x))

solve_OP <- function(x, control=list()) {
    solver <- .ROI_plugin_get_solver_name( getPackageName() )

    len_objective <- length(objective(x))
    len_dual_objective <- nrow(constraints(x)$L)
    bo <- bounds(x)
    b <- constraints(x)$rhs

    ## -------------------------------
    ## Constraints dir    <  <=  >  >=
    ## -------------------------------
    cxL <- constraints(x)$L
    if ( is.null(cxL) ) cxL <- simple_triplet_zero_matrix(0, len_objective)
    noeq <- which(constraints(x)$dir %in% c("<", "<=", ">", ">="))

    if ( length(noeq) ) {
        nc0 <- ncol(cxL)
        b <- c(b, rep.int(0L, length(noeq)))
        bo <- c(as.bound(bo), C_bound(nrow(cxL) + seq_len(length(noeq)), type="nonneg"))

        slack_noeq <- simple_triplet_matrix(noeq, seq_along(noeq), rep.int(1L, length(noeq)), 
                                            nrow=nrow(cxL), ncol=length(noeq))
        cxL <- cbind(cxL, slack_noeq)

        nc <- ncol(cxL)
        loeq <- which(constraints(x)$dir %in% c("<", "<="))
        if ( length(loeq) ) {
            i <- seq_along(loeq)
            j <- nc0 + match(loeq, noeq)
            cxL <- rbind(cxL, simple_triplet_matrix(i, j, v=rep.int(-1L, length(loeq)),
                                                    nrow=length(loeq), ncol=nc) )
        }

        goeq <- which(constraints(x)$dir %in% c(">", ">="))
        if ( length(goeq) ) {
            i <- seq_along(goeq)
            j <- nc0 + match(goeq, noeq)
            cxL <- rbind(cxL, simple_triplet_matrix(i, j, v=rep.int(1L, length(goeq)),
                                                    nrow=length(goeq), ncol=nc) )
        }
    }

    if (FALSE) {
        ## FIXME:: Das combine von bounds hat einen Fehler!
        str(bo)
        
        ##str(c(bo, C_bound(lbi, type="nonneg")))
        str(c(bo, C_bound((nrow(cxL) + lbi), type="nonneg")))
        ##xb <- list(as.bound(bo), C_bound(lbi, type="nonneg"))
        duplicated(bo$cones$nonneg)
        bo$cones$nonneg
        str(bo$cones)
        dim(cxL)
        absmax <- function(x) max(abs(x))
        apply(as.matrix(cxL), 2, absmax)
        unname(rowSums(as.matrix(cxL)))
    }

    ## ------------------------------
    ## V_bounds (ROI default is [0, Inf))
    ## ------------------------------
    ## build lower bounds (NOTE: ROI default is 0 scs default is Inf)
    lower_bounds <- to_dense_vector(bounds(x)$lower, len_objective)
    not_is_scs_default <- !is.infinite(lower_bounds) ## NOTE: we can assume that the lower bound is never Inf since ROI would complain before
    if ( any(not_is_scs_default) ) {
        lbi <- which(not_is_scs_default)
        bo <- c(as.bound(bo), C_bound((nrow(cxL) + seq_along(lbi)), type="nonneg"))
        cxL <- rbind(cxL, simple_triplet_matrix(seq_along(lbi), lbi, v=rep.int(-1L, length(lbi)), 
                                                nrow=length(lbi), ncol=ncol(cxL)))
        b <- c(b, -lower_bounds[not_is_scs_default])
    }
    ## build upper bounds (upper bounds are for ROI and scs Inf)
    which_exclude <- is.infinite(bounds(x)$upper$val)
    if ( any(!which_exclude) ) {
        ubi <- bounds(x)$upper$ind
        bo <- c(as.bound(bo), C_bound((nrow(cxL) + seq_along(ubi)), type="nonneg"))
        cxL <- rbind(cxL, simple_triplet_matrix(seq_along(ubi), ubi, v=rep.int(1L, length(ubi)), 
                                                nrow=length(ubi), ncol=ncol(cxL)))
        b <- c(b, bounds(x)$upper$val)
    }
    
    roi_cones <- as.list( bo$cones )
    cone_dims <- build_cone_dims( roi_cones )
    
    ## The NO_PSD_SCALING mode is only for testing purposes
    if ( !is.null(cone_dims$s) & is.null(control$NO_PSD_SCALING) ) {
        psd_j <- list()
        for ( i in seq_along(roi_cones$psd) ) {
            psd_dim <- cone_dims$s[i]
            psd_j[[i]] <- roi_cones$psd[[i]][scale_which( psd_dim )]
            k <- cxL$i %in% psd_j[[i]]
            cxL$v[k] <- sqrt(2) * cxL$v[k]
            b[psd_j[[i]]] <- sqrt(2) * b[psd_j[[i]]]
        }
    }

    map <- get_mapping()
    ind <- unlist(c(roi_cones[setdiff(names(map), c("powp", "powd"))]), use.names=FALSE)
    ind <- c(ind, unlist(lapply(c(roi_cones[["powp"]], roi_cones[["powd"]]), "[[", 1), use.names=FALSE))

    missing_indizes <- setdiff(seq_len(nrow(cxL)), ind)
    if ( length(missing_indizes) > 0 ) {
        ## If I give no warnig it can be used like every other solver.
        ## warning("  No cone was provided for the indices ", 
        ##     paste(missing_indizes, collapse=", "), " they will be set to the free cone.")
        cone_dims$f <- sum(c(cone_dims$f, length(missing_indizes)))
        ind <- c(missing_indizes, ind)
    }

    obj <- as.numeric( as.matrix(terms(objective(x))[["L"]]) )

    ## if we add slack variables for e.g. <= we have to fix the dim of the
    ## objective ## TODO: add a check of something was added
    if ( length(obj) < ncol(cxL) ) {
        obj <- c(obj, rep.int(0L, ncol(cxL) - length(obj)))
    }

    if ( length(b) < nrow(cxL) ) {
        b <- c(b, rep.int(0L, nrow(cxL) - length(b))) ## TODO: check if this case can still occur (I assume not!)
    }
    if (length(b) != length(ind)) ## this shouldn't happen
        stop("LENGTH_MISSMATCH in 'ROI.plugin.scs$solve_OP'\n",
             "\tthe dimensions of the 'rhs' and it's permutation vector don't match!")
    b <- b[ind]

    if ( x$maximum ) obj <- -obj
    if ( is.null(control$verbose) ) control$verbose <- FALSE
    if ( is.null(control$eps) ) control$eps <- 1e-6

    ## return(list(A = cxL[ind,], b = b, obj = obj, cone = cone_dims, control = control))
    out <- scs(A = cxL[ind,], b = b, obj = obj, cone = cone_dims, control = control)
    x_sol <- out$x[seq_len(len_objective)]
    out$len_objective <- len_objective
    out$len_dual_objective <- len_dual_objective
    ## out$y <- out$y[seq_len(len_dual_objective)]
    ## out$s <- out$s[seq_len(len_dual_objective)]

    if ( "s" %in% names(cone_dims)  ) {
        out$psd <- lapply(roi_cones$psd, function(j) unvech(out$y[j]))
    } else {
        sdp <- NULL
    }
    optimum <- (-1)^x$maximum * tryCatch({as.numeric(x_sol %*% obj[seq_len(len_objective)])}, error=function(e) as.numeric(NA))
    .ROI_plugin_canonicalize_solution( solution = x_sol,  optimum  = optimum,
                                       status   = out[["info"]][["statusVal"]],
                                       solver   = solver, message  = out )
}

.ROI_plugin_solution_dual.scs_solution <- function(x) {
    x$message$y[seq_len(x$message$len_dual_objective)]
}

.ROI_plugin_solution_psd.scs_solution <- function(x) {
    x$message$psd
}
