as_dgCMatrix <- function( x, ... ) 
    sparseMatrix(i=x$i, j=x$j, x=x$v, dims=c(x$nrow, x$ncol))

to_dense_vector <- function(x, len) {
    y <- rep.int(0L, len)
    if ( is.null(x$ind) ) return(y)
    y[x$ind] <- x$val
    return(y)
}

V_bound_2_matrix <- function(bound, len) {
    blx <- rep.int(0L, len)
    bux <- rep.int(Inf, len)
    if ( length(bound$lower$ind) > 0 ) {
        blx[bound$lower$ind] <- bound$lower$val
    }
    if ( length(bound$upper$ind) > 0 ) {
        bux[bound$upper$ind] <- bound$upper$val
    }
    return(rbind(blx, bux))
}

## problem
## The optimization problem.
## problem                      LIST
## ..$sense                     STRING
## ..$c                         NUMERIC VECTOR
## ..$c0                        NUMERIC                  (OPTIONAL)
## ..$A                         SPARSE MATRIX
## ..$bc                        NUMERIC MATRIX (2 rows)
## ..$bx                        NUMERIC MATRIX (2 rows)
## ..$cones                     LIST MATRIX (2 rows)     (OPTIONAL)
## ..$intsub                    NUMERIC VECTOR           (OPTIONAL)
## ..$qobj                      LIST                     (OPTIONAL)
## ..$scopt                     LIST                     (OPTIONAL)
## ..$iparam/$dparam/$sparam    LIST                     (OPTIONAL)
## ....$<MSK_PARAM>             STRING / NUMERIC         (OPTIONAL)
## ..$sol                       LIST                     (OPTIONAL)
## ....$itr/$bas/$int           LIST                     (OPTIONAL)
##
## opts
## The interface options.
## opts               LIST                 (OPTIONAL)
## ..$verbose         NUMERIC              (OPTIONAL)
## ..$usesol          BOOLEAN              (OPTIONAL)
## ..$useparam        BOOLEAN              (OPTIONAL)
## ..$soldetail       NUMERIC              (OPTIONAL)
## ..$getinfo         BOOLEAN              (OPTIONAL)
## ..$writebefore     STRING (filepath)    (OPTIONAL)
## ..$writeafter      STRING (filepath)    (OPTIONAL)

is.LP <- function(x) {
    if ( inherits(objective(x), "L_objective")
       & ( is.NO_constraint(constraints(x)) | is.L_constraint(constraints(x)) )
       & all(types(x) == "C") ) {
        return(TRUE)
    }    
    return(FALSE)
}

.control_args  <- c("verbose", "usesol", "useparam", "soldetail", "getinfo", "scofile")
.control_types <- c("numeric", "logical", "logical", "numeric",   "logical", "character")

##
solve_OP <- function(x, control=list()) {
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    leno <- length(objective(x))
    m <- list()
    m$sense <- c("min", "max")[1 + x$maximum]
    
    ## objective
    m$c <- as.numeric(as.matrix(terms(objective(x))$L))
    if ( !is.null(terms(objective(x))$Q) ) {
        m$qobj <- unclass(terms(objective(x))$Q)[c("i", "j", "v")]
    }

    ## constraints
    if ( nrow(constraints(x)) ) {
        m$A <- constraints(x)$L
        m$bc <- rbind( rep.int(-Inf, length(constraints(x))), 
                       rep.int( Inf, length(constraints(x))) )
        rownames(m$bc) <- c("blc", "buc")


        eq <- which(constraints(x)$dir == "==")
        if ( length(eq) ) {
            m$bc[1, eq] <- m$bc[2, eq] <- constraints(x)$rhs[eq]
        }

        leq <- which(constraints(x)$dir %in% c("<", "<="))
        if ( length(leq) ) {
            m$bc[2, leq] <- constraints(x)$rhs[leq]
        }

        geq <- which(constraints(x)$dir %in% c(">", ">="))
        if ( length(geq) ) {
            m$bc[1, geq] <- constraints(x)$rhs[geq]
        }
    } else {
        m$A <- simple_triplet_matrix(i=1, j=1, v=1, nrow=1, ncol=length(objective(x)))
        m$bc <- rbind( rep.int(-Inf, 1L), rep.int( Inf, 1L) )
    }

    ## bounds
    m$bx <- V_bound_2_matrix(bounds(x), leno)

    ## types
    if ( !all(types(x) == "C") ) {
        i <- which(types(x) %in% c("B", "I"))
        m$intsub <- i
        ## binary are defined as integer and 0 <= x <= 1
        i <- which(types(x) == "B")
        if ( length(i) ) {
            m$bx[1, i] <- sapply(m$bx[1, i], max, 0)
            m$bx[2, i] <- sapply(m$bx[2, i], min, 1)
        }
    } 

    ## control
    control$verbose <- as.integer(control$verbose)

    ## tmp <- Rmosek:::mosek(problem=m, 
    ##                       opts=control[intersect(names(control), .control_args)])
    ## str(tmp)

    m_call <- list(Rmosek::mosek, problem=m, 
                   opts=control[intersect(names(control), .control_args)])
    mode(m_call) <- "call"
    if ( isTRUE(control$dry_run) )
        return(m_call)

    out <- eval(m_call)

    ##' mosek destinguishes between 3 types of solutions
    ##' 1. itr - Interior solution
    ##' 2. bas - Basic solution
    ##' 3. int - Integer solution
    if ( !is.null(out$sol$bas$xx) ) {
        x.solution <- out$sol$bas$xx
    } else if ( !is.null(out$sol$itr$xx) ) {
        x.solution <- out$sol$itr$xx
    } else {
        x.solution <- out$sol$int$xx
    }

    optimum <- tryCatch({as.numeric(objective(x)(x.solution))}, 
                        error=function(e) as.numeric(NA))
    return( ROI_plugin_canonicalize_solution( solution = x.solution, 
                                               optimum  = optimum,
                                               status   = as.integer(out$response$code),
                                               solver   = solver, 
                                               message  = out ) 
    )
}
