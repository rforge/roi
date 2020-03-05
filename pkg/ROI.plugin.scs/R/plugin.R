## ROI plugin: SCS
## based on scs interface

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
        cone_dims[[map["powd"]]] <- c( cone_dims[[map["powp"]]], 
                                     -sapply(roi_cones[["powd"]], "[[", "a") )
    }
    cone_dims
}

calc_expp_dims <- function(x) {
    y <- x$id[x$cone == scs_cones['expp']]
    if ( !length(y) )
        return(NULL)
    length(unique(y))
}

calc_expd_dims <- function(x) {
    y <- x$id[x$cone == scs_cones['expd']]
    if ( !length(y) )
        return(NULL)
    length(unique(y))
}

calc_soc_dims <- function(x) {
    y <- x$id[x$cone == scs_cones['soc']]
    if ( !length(y) )
        return(NULL)
    as.integer(table(y))
}

calc_psd_matrix_dim <- function(m) as.integer((- 1 + sqrt(1 + 8 * m)) / 2)
calc_psd_dims <- function(x) {
    y <- x$id[x$cone == scs_cones['psd']]
    if ( !length(y) )
        return(NULL)
    sapply(table(y), calc_psd_matrix_dim)
}

calc_pow_dims <- function(x) {
    powp <- powd <- NULL
    ids <- unique(x$id[x$cone == scs_cones['powp']])
    if ( length(ids) )
        powp <- sapply(as.character(ids), function(id) x$params[[id]]['a'], USE.NAMES = FALSE)

    ids <- unique(x$id[x$cone == scs_cones['powd']])
    if ( length(ids) )
        powd <- sapply(as.character(ids), function(id) -x$params[[id]]['a'], USE.NAMES = FALSE)

    unname(c(powp, powd))
}

calc_dims <- function(cones) {
    dims <- list()
    dims$f <- sum(cones$cone == scs_cones["zero"])
    dims$l <- sum(cones$cone == scs_cones["nonneg"])
    dims$ep <- calc_expp_dims(cones)
    dims$ed <- calc_expd_dims(cones)

    dims$q <- calc_soc_dims(cones)
    dims$s <- calc_psd_dims(cones)

    dims$p <- calc_pow_dims(cones)

    dims
}

which_scs_default_lower_bounds <- function(lower_bounds) {
    which_inf <- which(is.infinite(lower_bounds$val))
    if ( length(which_inf) ) return(lower_bounds$ind[which_inf])
    return(NULL)
}

as.bound <- function( x, ... ) UseMethod( "as.bound" )
as.bound.bound <- identity
as.bound.NULL <- function( x, ... ) structure(list(), class="bound")

## get the indices of the conic bounds which are not the free cone
get_indizes_nonfree <- function(bo) {
    if ( is.null(bo) )
        return(integer())
    if ( is.null(bo$cones) )
        return(integer())

    c(unlist(bo$cones$nonneg), unlist(bo$cones$soc), unlist(bo$cones$psd),
      unlist(bo$cones$expp), unlist(bo$cones$expd), 
      unlist(lapply(bo$cones$powp, "[[", "i")), 
      unlist(lapply(bo$cones$powd, "[[", "i")))
}

scs_cones <-  c("zero" = 1L, "nonneg" = 2L, "soc" = 3L, "psd" = 4L, 
                "expp" = 5L, "expd" = 6L, "powp" = 7L, "powd" = 8L)


solve_OP <- function(x, control = list()) {

    constr <- as.C_constraint(constraints(x))

    ## check if "scs" supports the provided cone types
    stopifnot(all(constr$cones$cone %in% scs_cones))

    obj <- as.vector(terms(objective(x))[["L"]])
    if ( maximum(x) ) 
        obj <- -obj

    AL <- AU <- NULL
    AL.rhs <- AU.rhs <- double()

    ## lower bounds
    lower_bounds <- to_dense_vector(bounds(x)$lower, length(objective(x)))
    not_is_scs_default <- !is.infinite(lower_bounds)
    if ( any(not_is_scs_default) ) {
        li <- which(not_is_scs_default)
        AL <- simple_triplet_matrix(i = seq_along(li), j = li, 
                                    v = rep.int(-1, length(li)), 
                                    nrow = length(li), ncol = length(obj))
        AL.rhs <- -lower_bounds[not_is_scs_default]
    }

    ## upper bounds
    ui <- bounds(x)$upper$ind
    ub <- bounds(x)$upper$val
    if ( length(ui) ) {
        AU <- simple_triplet_matrix(i = seq_along(ui), j = ui,
                                    v = rep.int(1, length(ui)), 
                                    nrow = length(ui), ncol = length(obj))
        AU.rhs <- ub
    }

    A <- rbind(constr$L, AL, AU)
    A.rhs <- c(constr$rhs, AL.rhs, AU.rhs)
    cones <- c(constr$cones, K_lin(length(AL.rhs)), K_lin(length(AU.rhs)))

    if ( nrow(constr) > 0 ) {
        i <- with(cones, order(cone, id))
        ordered_cones <- list(cone = cones$cone[i], id = cones$id[i])
        A <- A[i,]
        A.rhs <- A.rhs[i]
        dims <- calc_dims(cones)
    } else {
        dims <- calc_dims(cones)
    }    

    ## The NO_PSD_SCALING mode is only for testing purposes
    if ( !is.null(dims$s) & is.null(control$NO_PSD_SCALING) ) {
        psd_j <- list()
        b <- ordered_cones$cone == scs_cones["psd"]
        roi_cones <- split(seq_along(ordered_cones$cone)[b], ordered_cones$id[b])
        for ( i in seq_along(roi_cones) ) {
            psd_dim <- dims$s[i]
            psd_j[[i]] <- roi_cones[[i]][scale_which( psd_dim )]
            k <- A$i %in% psd_j[[i]]
            A$v[k] <- sqrt(2) * A$v[k]
            A.rhs[psd_j[[i]]] <- sqrt(2) * A.rhs[psd_j[[i]]]
        }
    }

    if ( is.null(control$verbose) ) control$verbose <- FALSE
    if ( is.null(control$eps) ) control$eps <- 1e-6

    solver_call <- list(scs, A = A, b = A.rhs, obj = obj, 
                        cone = dims, control = control)
    mode(solver_call) <- "call"
    if ( isTRUE(control$dry_run) )
        return(solver_call) 

    out <- eval(solver_call)
    out$len_objective <- length(objective(x))
    out$len_dual_objective <- nrow(constraints(x))

    if ( "s" %in% names(dims)  ) {
        out$psd <- lapply(roi_cones, function(j) unvech(out$y[j]))
    } else {
        sdp <- NULL
    }
    optimum <- (-1)^x$maximum * tryCatch({as.numeric(out$x %*% obj)}, error=function(e) as.numeric(NA))
    ROI_plugin_canonicalize_solution( solution = out$x,  optimum  = optimum,
                                       status   = out[["info"]][["statusVal"]],
                                       solver   = "scs", message  = out )
}

ROI_plugin_solution_dual.scs_solution <- function(x) {
    x$message$y[seq_len(x$message$len_dual_objective)]
}

ROI_plugin_solution_psd.scs_solution <- function(x) {
    x$message$psd
}
