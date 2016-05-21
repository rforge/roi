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

sanitize_control <- function(control) {
    cont <- ecos.control()
    key <- names(control)[names(control) %in% names(cont)]
    cont[key] <- control[key]
    cont
}

## library(ECOSolveR)
## library( Matrix )

## BASIC SOLVER METHOD
solve_OP <- function(x, control=list()){
    solver <- .ROI_plugin_get_solver_name( getPackageName() )

    ## check if ecos supports the provided cone types
    check_cone_types(names(bounds(x)$cones))
    
    obj <- as.numeric( as.matrix(terms(objective(x))[["L"]]) )
    if ( x$maximum ) 
        obj <- -obj

    cones <- as.list( bounds(x)$cones )
    rowsA <- cones$free

    if ( !is.null(cones$expp) ) {
        cones$expp <- lapply(cones$expp, function(x) x[c(1L, 3L, 2L)])
    }

    rowsG <- unlist(c(cones$nonneg, cones$soc, cones$expp), use.names=FALSE)   
    dimq <- as.integer(unlist(lapply(cones$soc, length), use.names=FALSE))
    
    out <- ECOS_csolve(c = obj, 
                       G = if (length(rowsG) > 0) as_dgCMatrix(constraints(x)$L[rowsG,]) else NULL,
                       h = if (length(rowsG) > 0) constraints(x)$rhs[rowsG] else numeric(0),
                       dims = list(
                           l = length(cones$nonneg),
                           q = dimq,
                           e = length(cones$expp) ),
                       A = if (length(rowsA) > 0) as_dgCMatrix(constraints(x)$L[rowsA,]) else NULL,
                       b = if (length(rowsA) > 0) constraints(x)$rhs[rowsA] else numeric(0),
                       bool_vars = which( types(x) == "B" ),                     
                       int_vars  = which( types(x) == "I" ),
                       control   = sanitize_control(control) )

    .ROI_plugin_canonicalize_solution( solution = out$x,
                           optimum  = tryCatch( {as.numeric(out$x %*% obj)}, 
                                                error=function(e) as.numeric(NA) ),
                           status   = out[["retcodes"]]["exitFlag"],
                           solver   = solver )
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
