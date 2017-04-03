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
as_dgCMatrix <- function( x, ... ) {
    sparseMatrix(i=x$i, j=x$j, x=x$v, dims=c(x$nrow, x$ncol))
}

ecos_cones <- c("zero" = 1L, "nonneg" = 2L, "soc" = 3L, "expp" = 5L)

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
    if ( is.numeric(cont$VERBOSE) & (length(cont$VERBOSE) == 1) ) {
        cont$VERBOSE <- if ( is.finite(cont$VERBOSE) ) as.integer(cont$VERBOSE) else 0L
    } else {
        cont$VERBOSE <- 0L
    }
    cont
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

soc_dims <- function(x) {
    y <- x$id[x$cone == ecos_cones['soc']]
    if ( length(y) )
        y <- table(y)
    as.integer(y)
}

calc_dims <- function(cones, dims) {
    dims$l <- sum( cones$cone == ecos_cones['nonneg'] )
    dims$q <- as.integer(soc_dims(cones))
    if ( !length(dims$q) ) {
        dims["q"] <- list(NULL)
    }
    dims$e <- length(unique(cones$id[cones$cone == ecos_cones['expp']]))
    dims
}


## BASIC SOLVER METHOD
## attach(getNamespace("ROI.plugin.ecos")); control <- list(); library(slam) ## for debugging
solve_OP <- function(x, control = list()){
    solver <- "ecos"

    constr <- as.C_constraint(constraints(x))

    ## check if ecos supports the provided cone types
    stopifnot(all(constr$cones$cone %in% ecos_cones))
    
    obj <- as.vector(terms(objective(x))[["L"]])
    if ( maximum(x) ) 
        obj <- -obj

    b <- constr$cones$cone == ecos_cones['zero']
    if ( any(b) ) {
        A     <- as_dgCMatrix(constr$L[b,])
        A.rhs <- constr$rhs[b]
    } else {
        A <- NULL
        A.rhs <- double()
    }

    dims <- list(l = 0L, q = NULL, e = 0L)

    if ( any(!b) ) {
        G     <- constr$L[!b,]
        G.rhs <- constr$rhs[!b]
        cones <- constr$cones[!b]
        dims <- calc_dims(cones, dims)

        ## Since ecos uses a different cone definition we have to 
        ## permutate the cones.
        ## Change the order from c(1, 2, 3) to c(1, 3, 2)
        k <- which(cones$cone %in% ecos_cones['expp'])
        if ( length(k) ) {
            cids <- aggregate(id ~ cone_id, 
                              data = list(cone_id = cones$id[k], id = k), 
                              FUN = c, simplify = FALSE)
            for (i in seq_len(nrow(cids))) {
                j <- cids[[i, 2]]
                cones$id[j] <- cones$id[j] + c(0.1, 0.3, 0.2)
            }
        }

        i <- with(cones, order(cone, id))
        
        G     <- G[i,]
        G.rhs <- G.rhs[i]

    } else {
        G <- NULL
        G.rhs <- double()
    }

    GL <- GU <- NULL
    GL.rhs <- GU.rhs <- integer()

    ## lower bounds
    lower_bounds <- to_dense_vector(bounds(x)$lower, length(objective(x)))
    not_is_ecos_default <- !is.infinite(lower_bounds)
    if ( any(not_is_ecos_default) ) {
        li <- which(not_is_ecos_default)
        GL <- simple_triplet_matrix(i = seq_along(li), j = li, 
                                    v = rep.int(-1, length(li)), 
                                    nrow = length(li), ncol = length(obj))
        GL.rhs <- -lower_bounds[not_is_ecos_default]
    }

    ## upper bounds
    ui <- bounds(x)$upper$ind
    ub <- bounds(x)$upper$val
    if ( length(ui) ) {
        GU <- simple_triplet_matrix(i = seq_along(ui), j = ui,
                                    v = rep.int(1, length(ui)), 
                                    nrow = length(ui), ncol = length(obj))
        GU.rhs <- ub
    }

    G      <- as_dgCMatrix(rbind(GL, GU, G))
    G.rhs  <- c(GL.rhs, GU.rhs, G.rhs)
    dims$l <- dims$l + length(GL.rhs) + length(GU.rhs)

    which_logical <- which( types(x) == "B" )
    which_integer <- which( types(x) == "I" )

    ## TODO add a dims check!

    ecos <- list(ECOS_csolve, c = obj, G = G, h = G.rhs,
                 dims = dims, A = A, b = A.rhs,
                 bool_vars = which_logical, int_vars  = which_integer,
                 control   = sanitize_control(control))
    
    mode(ecos) <- "call"

    if ( isTRUE(control$dry_run) ) {
        return(ecos)
    }

    out <- eval(ecos)
    x_sol <- out$x

    if ( any(b <- types(x) %in% c("B", "I")) ) {
        x_sol[b] <- round(x_sol[b])
    }

    optimum <- tryCatch({as.numeric(x_sol %*% obj)}, error=function(e) as.numeric(NA))
    optimum <- ((-1)^maximum(x) * optimum)

    ROI_plugin_canonicalize_solution( solution = x_sol, optimum  = optimum,
                                       status   = out[["retcodes"]]["exitFlag"],
                                       solver   = solver, message = out )
}

ROI_plugin_solution_dual.ecos_solution <- function(x) {
    x$message$y[seq_len(x$message$len_dual_objective)]
}

## STATUS CODES
.add_status_codes <- function() {
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    ROI_plugin_add_status_code_to_db( solver,
                           0L,
                           "ECOS_OPTIMAL",
                           "Optimal solution found.",
                           0L )
    ROI_plugin_add_status_code_to_db( solver,
                           1L,
                           "ECOS_PINF",
                           "Certificate of primal infeasibility found." )
    ROI_plugin_add_status_code_to_db( solver,
                           2L,
                           "ECOS_DINF",
                           "Certificate of dual infeasibility found." )
    ROI_plugin_add_status_code_to_db( solver,
                           10L,
                           "ECOS_OPTIMAL + ECOS_INACC_OFFSET",
                           "Optimal solution found subject to reduced tolerances.", 0L)
    ROI_plugin_add_status_code_to_db( solver,
                           11L,
                           "ECOS_PINF + ECOS_INACC_OFFSET",
                           "Certificate of primal infeasibility found subject to reduced tolerances." )
    ROI_plugin_add_status_code_to_db( solver,
                           12L,
                           "ECOS_DINF + ECOS_INACC_OFFSET",
                           "Certificate of dual infeasibility found subject to reduced tolerances." )
    ROI_plugin_add_status_code_to_db( solver,
                           -1L,
                           "ECOS_MAXIT",
                           "Maximum number of iterations reached." )
    ROI_plugin_add_status_code_to_db( solver,
                           -2L,
                           "ECOS_NUMERICS",
                           "Numerical problems (unreliable search direction)." )
    ROI_plugin_add_status_code_to_db( solver,
                           -3L,
                           "ECOS_OUTCONE",
                           "Numerical problems (slacks or multipliers outside cone)." )
    ROI_plugin_add_status_code_to_db( solver,
                           -4L,
                           "ECOS_SIGINT",
                           "Interrupted by signal or CTRL-C." )
    ROI_plugin_add_status_code_to_db( solver,
                           -7L,
                           "ECOS_FATAL",
                           "Unknown problem in solver." )
    invisible(TRUE)
}
