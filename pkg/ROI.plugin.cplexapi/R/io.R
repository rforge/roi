
cplexSolve <- function(env, prob) {
    status <- lpoptCPLEX(env, prob)
    
    if ( status != 0 )
        status <- dualoptCPLEX(env, prob)

    if ( status != 0 )
        status <- mipoptCPLEX(env, prob)

    if ( status != 0 )
        status <- qpoptCPLEX(env, prob)

    if ( status != 0 )
        status <- hybbaroptCPLEX(env, prob, 0L)

    if ( status != 0 )
        status <- primoptCPLEX(env, prob)

    c(solutionCPLEX(env, prob), info=list(solnInfoCPLEX(env, prob)))
}

cplexapi_solve_lp <- function(file) {
    solver <- "cplexapi"
    env <- openEnvCPLEX()
    prob <- initProbCPLEX(env)
    readCopyProbCPLEX(env, prob, fname = file)

    sol <- cplexSolve(env, prob)
    # str(sol)
    delProbCPLEX(env, prob)
    closeEnvCPLEX(env)
    
    status <- tryCatch(ROI:::canonicalize_status(sol$status$code, solver), error = function(e) as.integer(NA))
    structure(list(solution = sol$x, objval = sol$objval, status = status, message = sol), 
        meta = list(solver = solver), class = c(sprintf("%s_solution", solver), "OP_solution"))
}



cplex_to_Q_constraint <- function(x, nobj) {
    Q <- simple_triplet_matrix(i=x$quadrow+1L, j=x$quadcol+1L, v=x$quadval, nrow=nobj, ncol=nobj)
    L <- numeric(nobj)
    L[x$linind + 1L] <- x$linval
    Q_constraint(Q=Q, L=L, dir=map_dir(x$sense), rhs=x$rhs)
}

cplex_matrix_to_simple_triplet_matrix <- function(A, nrow, ncol) {
    if ( is.null(A) ) return(A)
    if ( any(grepl("qmat", names(A))) ) {
        names(A) <- gsub("qmat", "mat", names(A))
    }
    A$matcnt <- diff(c(A$matbeg, length(A$matind)))
    get_column <- function(j, A) {  
        A$matind[seq(from=A$matbeg[j]+1L, length.out=A$matcnt[j])]
    }
    i <- 1L + unlist(lapply(seq_len(ncol), get_column, A=A))
    j <- unlist(mapply(rep.int, seq_along(A$matcnt), A$matcnt, SIMPLIFY=FALSE))
    simple_triplet_matrix(i = i, j = j, v = A$matval, nrow = nrow, ncol = ncol)
}

map_dir <- function(x) {
    dir_map <- setNames(c('<=', '==', '>='), c('L', 'E', 'G'))
    stopifnot( all(unique(x) %in% names(dir_map)) )
    dir_map[x]
}

cplex_to_roi <- function(env, prob) {
    problem_name <- getProbNameCPLEX(env, prob)

    nobj <- getNumColsCPLEX(env, prob)
    A.nrow <- getNumRowsCPLEX(env, prob)

    obj.L <- getObjCPLEX(env, prob, 0L, nobj-1L)
    obj.Q <- cplex_matrix_to_simple_triplet_matrix(getQuadCPLEX(env, prob, 0L, nobj-1L), nobj, nobj)
    obj.names <- getColNameCPLEX(env, prob, 0L, nobj-1L)

    if ( is.null(obj.Q) ) {
        obj <- L_objective(obj.L, names=obj.names)
    } else {
        obj <- Q_objective(obj.Q, obj.L, names=obj.names)
    }    

    dir_map <- setNames(c('<=', '==', '>='), c('L', 'E', 'G'))
    if (A.nrow) {
        con.L <- cplex_matrix_to_simple_triplet_matrix(getColsCPLEX(env, prob, 0L, nobj-1L), A.nrow, nobj)
        con.L.dir <- map_dir(getSenseCPLEX(env, prob, 0L, A.nrow-1L))
        con.L.rhs <- getRhsCPLEX(env, prob, 0L, A.nrow-1L)
        con.L.names <- getRowNameCPLEX(env, prob, 0L, A.nrow-1L)
        rownames(con.L) <- con.L.names
        con.L <- L_constraint(con.L, con.L.dir, con.L.rhs)
    } else {
        con.L <- NO_constraint(nobj)
    }

    nqconstrs <- getNumQConstrsCPLEX(env, prob)
    if ( nqconstrs ) {
        con.Q <- vector("list", nqconstrs)
        for (i in seq_along(con.Q)) {
            cplex_Q_con <- getQConstrCPLEX(env, prob, i-1L)
            con.Q[[i]] <- cplex_to_Q_constraint(cplex_Q_con, nobj)
        }
        con.Q <- do.call(c, con.Q)
    } else {
        con.Q <- NO_constraint(nobj)
    }

    ## FIXME
    if ( is.NO_constraint(con.L) & is.NO_constraint(con.Q) ) {
        con <- NULL
    } else if ( is.NO_constraint(con.L) ) {
        con <- con.Q
    } else if ( is.NO_constraint(con.Q) ) {
        con <- con.L
    } else {
        con <- c(con.L, con.Q)
    }

    typ <- getColTypeCPLEX(env, prob, 0L, nobj-1L)
    if ( inherits(typ, "cpxerr") ) typ <- NULL

    lb <- getLowerBndsCPLEX(env, prob, 0L, nobj-1L)
    ub <- getUpperBndsCPLEX(env, prob, 0L, nobj-1L)
    bou <- V_bound(li=seq_along(lb), ui=seq_along(ub), lb=lb, ub=ub, nobj=nobj)

    ## -1 maximize  and 1 minimize
    maximum <- c(TRUE, NA, FALSE)[getObjDirCPLEX(env, prob) + 2L]

    OP(objective=obj, constraints=con, types=typ, bounds=bou, maximum = maximum)
}

## NOTE: cplexAPI writes directly to stout therefore we can not capture the error 
##       messages
ROI_read_lp <- function(fname) {
    env <- openEnvCPLEX(ptrtype = "cplex_env")
    prob <- initProbCPLEX(env, pname = "CPLEX_PROB", ptrtype = "cplex_prob")
    readCopyProbCPLEX(env, prob, fname, ftype = NULL)

    roi_op <- cplex_to_roi(env, prob)

    delProbCPLEX(env, prob)
    closeEnvCPLEX(env)
    return(roi_op)
}
