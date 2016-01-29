
make_ECOS_signatures <- function()
    ROI_make_signature( objective = c("L"),
        ## TODO: add transformation method quadratic to conic
                              constraints = c("L", "Q", "C"),
                              types = c("B", "I", "C"),
                              bounds = c(TRUE, FALSE),
                              maximum = c(TRUE, FALSE) )

as_dgCMatrix <- function( x, ... ) 
    sparseMatrix(i=x$i, j=x$j, x=x$v, dims=c(x$nrow, x$ncol), dimnames=x$dimnames)

## BASIC SOLVER METHOD
## NOTE:
##   - for now I follow the scs syntax
## TODO:
##   - allow L and Q
##   - For now we assume the everything is already a conic constraint!
solve_OP <- function(x, control=list()){
    solver <- ROI:::get_solver_name( getPackageName() )

    ## setup control
    cont <- ecos.control()
    key <- names(control)[names(control) %in% names(cont)]
    cont[key] <- control[key]

    ## check if ecos supports the provided cone types
    b <- !names(constraints(x)[["cones"]]) %in% c("free", "nonneg", "soc", "expp")

    if ( any(b) ) stop("ECOS doesn't support cone type ",
                       paste(shQuote(names(constraints(x)[["cones"]])[b]), collapse=" and "), "!")

    ## all free cones are A and b (cf are the indices of the free cones)
    cf <- as.integer(constraints(x)[["cones"]][["free"]])
    ## liner cones
    cl <- as.integer(constraints(x)[["cones"]][["nonneg"]])
    ## Second-order cones
    ## a list of vectors (for each constraint) e.g. list(2:4, 6:9)
    cq <- constraints(x)[["cones"]][["soc"]]
    ce <- as.integer(constraints(x)[["cones"]][["expp"]])

    ## check if for every constraint a cone was provided
    if ( all(seq_len(length(constraints(x))) != sort(c(cf, cl, unlist(cq), ce))) ) {
        i <- which(seq_len(length(constraints(x))) != sort(c(cf, cl, unlist(cq), ce)))
        stop("no cone was provided for the constraints: ", paste(i, collapse=", "))
    }

    ## the matrix G has to be ordered, first "nonneg" cones then "soc" last "expp" cones
    o1 <- c(rep(1L, length(cl)), rep(2L, length(unlist(cq))), rep(3L, length(ce)))
    indices <- c(cl, unlist(cq), ce)
    j <- indices[order(o1, indices)]

    ## ECOSolveR needs a numeric as objective
    obj <- as.numeric(as.matrix(terms(objective(x))[["L"]]))

    out <- ECOS_csolve(c = obj, 
                       G = if (length(j) > 0) as_dgCMatrix(constraints(x)$L[j,]) else NULL,
                       h = if (length(j) > 0) constraints(x)$rhs[j] else numeric(0),
                       dims = list(
                           l = max(cl, 0L),
                           q = unlist(lapply(cq, length)),
                           e = max(ce, 0L) ),
                       A = if (length(cf) > 0) as_dgCMatrix(constraints(x)$L[cf,]) else NULL,
                       b = if (length(cf) > 0) constraints(x)$rhs[cf] else numeric(0),
                       bool_vars = which( types(x) == "B" ),                     
                       int_vars  = which( types(x) == "I" ),
                       control   = cont )  

    

    ROI:::canonicalize_solution( solution = out$x,
                                 optimum  = tryCatch( {as.numeric(out$x %*% obj)}, 
                                                      error=function(e) as.numeric(NA) ),
                                 status   = out[["retcodes"]]["exitFlag"],
                                 solver   = solver )
}

## STATUS CODES
.add_status_codes <- function() {
    solver <- ROI:::get_solver_name( getPackageName() )
    ROI:::add_status_code_to_db(solver,
                                0L,
                                "ECOS_OPTIMAL",
                                "Optimal solution found.",
                                0L
                                )
    ROI:::add_status_code_to_db(solver,
                                1L,
                                "ECOS_PINF",
                                "Certificate of primal infeasibility found."
                                )
    ROI:::add_status_code_to_db(solver,
                                2L,
                                "ECOS_DINF",
                                "Certificate of dual infeasibility found."
                                )
    ROI:::add_status_code_to_db(solver,
                                10L,
                                "ECOS_OPTIMAL + ECOS_INACC_OFFSET",
                                "Optimal solution found subject to reduced tolerances."
                                )
    ROI:::add_status_code_to_db(solver,
                                11L,
                                "ECOS_PINF + ECOS_INACC_OFFSET",
                                "Certificate of primal infeasibility found subject to reduced tolerances."
                                )
    ROI:::add_status_code_to_db(solver,
                                12L,
                                "ECOS_DINF + ECOS_INACC_OFFSET",
                                "Certificate of dual infeasibility found subject to reduced tolerances."
                                )
    ROI:::add_status_code_to_db(solver,
                                -1L,
                                "ECOS_MAXIT",
                                "Maximum number of iterations reached."
                                )
    ROI:::add_status_code_to_db(solver,
                                -2L,
                                "ECOS_NUMERICS",
                                "Numerical problems (unreliable search direction)."
                                )
    ROI:::add_status_code_to_db(solver,
                                -3L,
                                "ECOS_OUTCONE",
                                "Numerical problems (slacks or multipliers outside cone)."
                                )
    ROI:::add_status_code_to_db(solver,
                                -4L,
                                "ECOS_SIGINT",
                                "Interrupted by signal or CTRL-C."
                                )
    ROI:::add_status_code_to_db(solver,
                                -7L,
                                "ECOS_FATAL",
                                "Unknown problem in solver."
                                )
    invisible(TRUE)
}
