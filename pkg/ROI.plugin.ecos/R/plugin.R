## -----------------------------------------------------------------------------
##
##   ECOS:
##
##   Exponential Cone:
##   =================
##      The exponential cone is a little bit different defined than in the SCS
##      solver, essentially y and z are switched.
##
## -----------------------------------------------------------------------------
## TODO: V_bound

as_dgCMatrix <- function( x, ... ) 
    sparseMatrix(i=x$i, j=x$j, x=x$v, dims=c(x$nrow, x$ncol))

check_cone_types <- function(x) {
    b = !( x %in% c("free", "nonneg", "soc", "expp") )
    if ( any(b) ) stop("ECOS doesn't support cones of type ",
                       paste(shQuote(x[b]), collapse=" or "), "!")
    invisible(NULL)
}

to_dense_vector <- function(x, len) {
    y <- rep.int(0L, len)
    if ( is.null(x$ind) ) return(y)
    y[x$ind] <- x$val
    return(y)
}

sanitize_control <- function(control) {
    cont <- ecos.control()
    key <- names(control)[names(control) %in% names(cont)]
    cont[key] <- control[key]
    cont
}

as.bound <- function( x, ... ) UseMethod( "as.bound" )
as.bound.bound <- identity
as.bound.NULL <- function( x, ... ) structure(NULL, class="bound")

if ( FALSE ) {
    
    library(ECOSolveR)
    library( Matrix )  
    library(slam)

    x <- lp
    control <- list()
    solver <- "ecos"

}

## BASIC SOLVER METHOD
solve_OP <- function(x, control=list()){
    solver <- .ROI_plugin_get_solver_name( getPackageName() )

    ## check if ecos supports the provided cone types
    check_cone_types(names(bounds(x)$cones))

    len_objective <- length(objective(x))
    len_dual_objective <- ncol(constraints(x)$L)
    bo <- bounds(x)
    b <- constraints(x)$rhs
    
    ## -------------------------------
    ## Constraints dir    <  <=  >  >=
    ## -------------------------------
    cxL <- constraints(x)$L
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

    ## ------------------------------
    ## V_bounds (ROI default is [0, Inf))
    ## ------------------------------
    ## build lower bounds (NOTE: ROI default is 0 scs default is Inf)
    lower_bounds <- to_dense_vector(bounds(x)$lower, len_objective)
    not_is_ecos_default <- !is.infinite(lower_bounds) ## NOTE: we can assume that the lower bound is never Inf since ROI would complain before
    if ( any(not_is_ecos_default) ) {
        lbi <- which(not_is_ecos_default)
        bo <- c(as.bound(bo), C_bound((nrow(cxL) + seq_along(lbi)), type="nonneg"))
        cxL <- rbind(cxL, simple_triplet_matrix(seq_along(lbi), lbi, v=rep.int(-1L, length(lbi)), 
                                                nrow=length(lbi), ncol=ncol(cxL)))
        b <- c(b, -lower_bounds[not_is_ecos_default])
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

    cones <- as.list( bo$cones )
    rowsA <- cones$free  
    if ( !is.null(cones$expp) ) {
        cones$expp <- lapply(cones$expp, function(x) x[c(1L, 3L, 2L)])
    }
    rowsG <- unlist(c(cones$nonneg, cones$soc, cones$expp), use.names=FALSE)   

    missing_indizes <- setdiff(seq_len(nrow(cxL)), c(rowsA, rowsG))
    if ( length(missing_indizes) > 0 ) {
        ## If I give no warnig it can be used like every other solver.
        ## warning("  No cone was provided for the indices ", 
        ##     paste(missing_indizes, collapse=", "), " they will be set to the free cone.")
        cones$f <- sum(c(cones$f, length(missing_indizes)))
        rowsA <- c(missing_indizes, rowsA)
    }

    obj <- as.numeric( as.matrix(terms(objective(x))[["L"]]) )
    ## if we add slack variables for e.g. <= we have to fix the dim of the
    ## objective ## TODO: add a check of something was added
    if ( length(obj) < ncol(cxL) ) {
        obj <- c(obj, rep.int(0L, ncol(cxL) - length(obj)))
    }
    if (length(b) != sum(c(length(rowsA), length(rowsG)))) ## this shouldn't happen
        stop("LENGTH_MISSMATCH in 'ROI.plugin.scs$solve_OP'\n",
             "\tthe dimensions of the 'rhs' and it's permutation vector don't match!")

    if ( x$maximum ) obj <- -obj
    dimq <- as.integer(unlist(lapply(cones$soc, length), use.names=FALSE))
    
    out <- ECOS_csolve(c = obj, 
                       G = if (length(rowsG) > 0) as_dgCMatrix(cxL[rowsG,]) else NULL,
                       h = if (length(rowsG) > 0) b[rowsG] else numeric(0),
                       dims = list(
                           l = length(cones$nonneg),
                           q = dimq,
                           e = length(cones$expp) ),
                       A = if (length(rowsA) > 0) as_dgCMatrix(cxL[rowsA,]) else NULL,
                       b = if (length(rowsA) > 0) b[rowsA] else numeric(0),
                       bool_vars = which( types(x) == "B" ),                     
                       int_vars  = which( types(x) == "I" ),
                       control   = sanitize_control(control) )
    out$x <- out$x[seq_len(len_objective)]
    out$y <- out$y[seq_len(len_dual_objective)]
    out$s <- out$s[seq_len(len_dual_objective)]
    optimum <- tryCatch({as.numeric(out$x %*% obj[seq_len(len_objective)])}, error=function(e) as.numeric(NA))

    .ROI_plugin_canonicalize_solution( solution = out$x, optimum  = optimum,
                                       status   = out[["retcodes"]]["exitFlag"],
                                       solver   = solver, message = out )
}

## STATUS CODES
.add_status_codes <- function() {
    solver <- .ROI_plugin_get_solver_name( getPackageName() )
    .ROI_plugin_add_status_code_to_db( solver,
                           0L,
                           "ECOS_OPTIMAL",
                           "Optimal solution found.",
                           0L )
    .ROI_plugin_add_status_code_to_db( solver,
                           1L,
                           "ECOS_PINF",
                           "Certificate of primal infeasibility found." )
    .ROI_plugin_add_status_code_to_db( solver,
                           2L,
                           "ECOS_DINF",
                           "Certificate of dual infeasibility found." )
    .ROI_plugin_add_status_code_to_db( solver,
                           10L,
                           "ECOS_OPTIMAL + ECOS_INACC_OFFSET",
                           "Optimal solution found subject to reduced tolerances." )
    .ROI_plugin_add_status_code_to_db( solver,
                           11L,
                           "ECOS_PINF + ECOS_INACC_OFFSET",
                           "Certificate of primal infeasibility found subject to reduced tolerances." )
    .ROI_plugin_add_status_code_to_db( solver,
                           12L,
                           "ECOS_DINF + ECOS_INACC_OFFSET",
                           "Certificate of dual infeasibility found subject to reduced tolerances." )
    .ROI_plugin_add_status_code_to_db( solver,
                           -1L,
                           "ECOS_MAXIT",
                           "Maximum number of iterations reached." )
    .ROI_plugin_add_status_code_to_db( solver,
                           -2L,
                           "ECOS_NUMERICS",
                           "Numerical problems (unreliable search direction)." )
    .ROI_plugin_add_status_code_to_db( solver,
                           -3L,
                           "ECOS_OUTCONE",
                           "Numerical problems (slacks or multipliers outside cone)." )
    .ROI_plugin_add_status_code_to_db( solver,
                           -4L,
                           "ECOS_SIGINT",
                           "Interrupted by signal or CTRL-C." )
    .ROI_plugin_add_status_code_to_db( solver,
                           -7L,
                           "ECOS_FATAL",
                           "Unknown problem in solver." )
    invisible(TRUE)
}
