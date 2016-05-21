## ROI plugin: SCS
## based on scs interface

## TODO: powp
## TODO: powd
## FIXME: implement a briges for
##   - bounds
##   - different directions
##   - ...

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

solve_OP <- function(x, control=list()) {
    solver <- .ROI_plugin_get_solver_name( getPackageName() )

    if ( any( constraints(x)$dir != "==" ) ) {
        stop("TODO in 'ROI.plugin.scs@solve_OP'\n\t", "ROI.plugin.scs currently only supports equality constraints!\n")
    }
    ## NOTE: since we have no solver with V_bounds and C_bounds
    ##       we do not habe to consider the case when to join V_bounds and C_bounds
    roi_cones <- as.list( bounds(x)$cones )
    cone_dims <- build_cone_dims( roi_cones )
    
    ## The NO_PSD_SCALING mode is only for testing purposes
    if ( !is.null(cone_dims$s) & is.null(control$NO_PSD_SCALING) ) {
        psd_j <- list()
        for ( i in seq_along(roi_cones$psd) ) {
            psd_dim <- cone_dims$s[i]
            psd_j[[i]] <- roi_cones$psd[[i]][scale_which( psd_dim )]
            k <- constraints(x)$L$i %in% psd_j[[i]]
            constraints(x)$L$v[k] <- sqrt(2) * constraints(x)$L$v[k]
            constraints(x)$rhs[psd_j[[i]]] <- sqrt(2) * constraints(x)$rhs[psd_j[[i]]]
        }
    }

    map <- get_mapping()
    ind <- unlist(c(roi_cones[setdiff(names(map), c("powp", "powd"))]), use.names=FALSE)
    ind <- c(ind, unlist(lapply(c(roi_cones[["powp"]], roi_cones[["powd"]]), "[[", 1), use.names=FALSE))

    missing_indizes <- setdiff(seq_len(length(constraints(x)$rhs)), ind)
    if ( length(missing_indizes) > 0 ) {
        warning("  No cone was provided for the indices ", 
            paste(missing_indizes, collapse=", "), " they will be set to the linear cone.")
        cone_dims$l <- sum(c(cone_dims$l, length(missing_indizes)))
        ind <- c(missing_indizes, ind)
    }

    obj <- as.numeric( as.matrix(terms(objective(x))[["L"]]) )
    if ( x$maximum ) obj <- -obj
    if ( is.null(control$verbose) ) control$verbose <- FALSE
    
    out <- scs( A = constraints(x)$L[ind,],
                b = constraints(x)$rhs[ind],
                obj = obj,
                cone = cone_dims,
                control = control )

    if ( "s" %in% names(cone_dims)  ) {
        out$sdp <- lapply(roi_cones$psd, function(j) unvech(out$y[j]))
    } else {
        sdp <- NULL
    }

    .ROI_plugin_canonicalize_solution( solution = out$x,
                                       optimum  = tryCatch( {as.numeric(out$x %*% obj)}, 
                                                             error=function(e) as.numeric(NA) ),
                                        status   = out[["info"]][["statusVal"]],
                                        solver   = solver,
                                        message  = out )
}

.add_status_codes <- function() {
    solver <- .ROI_plugin_get_solver_name( getPackageName() )
    .ROI_plugin_add_status_code_to_db( solver,
                           1L,
                           "SCS_SOLVED",
                           "Optimal solution found.",
                           0L )
    .ROI_plugin_add_status_code_to_db( solver,
                           2L,
                           "SCS_SOLVED_INACCURATE",
                           "SCS_SOLVED_INACCURATE" )
    .ROI_plugin_add_status_code_to_db( solver,
                           0L,
                           "SCS_UNFINISHED",
                           "SCS_UNFINISHED" )
    .ROI_plugin_add_status_code_to_db( solver,
                           -1L,
                           "SCS_UNBOUNDED",
                           "SCS_UNBOUNDED" )
    .ROI_plugin_add_status_code_to_db( solver,
                           -2L,
                           "SCS_INFEASIBLE",
                           "SCS_INFEASIBLE" )
    .ROI_plugin_add_status_code_to_db( solver,
                           -3L,
                           "SCS_INDETERMINATE",
                           "SCS_INDETERMINATE" )
    .ROI_plugin_add_status_code_to_db( solver,
                           -4L,
                           "SCS_FAILED",
                           "SCS_FAILED" )
    .ROI_plugin_add_status_code_to_db( solver,
                           -5L,
                           "SCS_SIGINT",
                           "SCS_SIGINT" )
    .ROI_plugin_add_status_code_to_db( solver,
                           -6L,
                           "SCS_UNBOUNDED_INACCURATE",
                           "SCS_UNBOUNDED_INACCURATE" )
    .ROI_plugin_add_status_code_to_db( solver,
                           -7L,
                           "SCS_INFEASIBLE_INACCURATE",
                           "SCS_INFEASIBLE_INACCURATE" )
    invisible(TRUE)
}
