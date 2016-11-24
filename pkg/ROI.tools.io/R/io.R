read.lp <- ROI.plugin.lpsolve::read.lp
write.lp <- ROI.plugin.lpsolve::write.lp

get_namespace <- function(x) {
	tryCatch(getNamespace(x), error=function(e) NULL)
}

read.qp <- function(file, type = c("auto", "SAV", "MPS", "LP")) {
    type <- match.arg(type)
    if (type == "auto") type <- NULL

    if ( is.null((cplex <- get_namespace("cplexAPI"))) ) {
        env <- cplex$openEnvCPLEX(ptrtype = "cplex_env")
        prob <- cplex$initProbCPLEX(env, pname = "CPLEX_PROB", ptrtype = "cplex_prob")
        cplex$readCopyProbCPLEX(env, prob, file, ftype = type)

        roi_op <- cplex$cplex_to_roi(env, prob, cplex)

        cplex$delProbCPLEX(env, prob)
        cplex$closeEnvCPLEX(env)  
    } else {
        stop("ROI.tools.op needs the package cplexAPI for reading QP files.")  
    }
    return(roi_op)
}

cplex_to_Q_constraint <- function(x, nobj) {
    Q <- simple_triplet_matrix(i=x$quadrow+1L, j=x$quadcol+1L, v=x$quadval, nrow=nobj, ncol=nobj)
    L <- numeric(nobj)
    L[x$linind] <- x$linval
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

cplex_to_roi <- function(env, prob, cplex) {
    problem_name <- cplex$getProbNameCPLEX(env, prob)

    nobj <- cplex$getNumColsCPLEX(env, prob)
    A.nrow <- cplex$getNumRowsCPLEX(env, prob)

    obj.L <- cplex$getObjCPLEX(env, prob, 0L, nobj-1L)
    obj.Q <- cplex$cplex_matrix_to_simple_triplet_matrix(cplex$getQuadCPLEX(env, prob, 0L, nobj-1), nobj, nobj)
    obj.names <- cplex$getColNameCPLEX(env, prob, 0L, nobj-1L)

    if ( is.null(obj.Q) ) {
        obj <- L_objective(obj.L, names=obj.names)
    } else {
        obj <- Q_objective(obj.Q, obj.L, names=obj.names)
    }    

    dir_map <- setNames(c('<=', '==', '>='), c('L', 'E', 'G'))
    if (A.nrow) {
        con.L <- cplex$cplex_matrix_to_simple_triplet_matrix(cplex$getColsCPLEX(env, prob, 0L, nobj-1L), A.nrow, nobj)
        con.L.dir <- map_dir(cplex$getSenseCPLEX(env, prob, 0L, A.nrow-1L))
        con.L.rhs <- cplex$getRhsCPLEX(env, prob, 0L, A.nrow-1L)
        con.L.names <- cplex$getRowNameCPLEX(env, prob, 0L, A.nrow-1L)
        rownames(con.L) <- con.L.names
        con.L <- L_constraint(con.L, con.L.dir, con.L.rhs)
    } else {
        con.L <- NO_constraint(nobj)
    }

    nqconstrs <- cplex$getNumQConstrsCPLEX(env, prob)
    if ( nqconstrs ) {
        con.Q <- vector("list", nqconstrs)
        for (i in seq_along(con.Q)) {
            con.Q[[i]] <- cplex$cplex_to_Q_constraint(cplex$getQConstrCPLEX(env, prob, i-1L), nobj)
        }
        i
        con.Q <- do.call(c, con.Q)
    } else {
        con.Q <- NO_constraint(nobj)
    }

    con <- c(con.L, con.Q)
    if ( is.NO_constraint(con) ) con <- NULL

    typ <- cplex$getColTypeCPLEX(env, prob, 0L, nobj-1L)
    if ( inherits(typ, "cpxerr") ) typ <- NULL

    lb <- cplex$getLowerBndsCPLEX(env, prob, 0L, nobj-1L)
    ub <- cplex$getUpperBndsCPLEX(env, prob, 0L, nobj-1L)
    bou <- V_bound(li=seq_along(lb), ui=seq_along(ub), lb=lb, ub=ub, nobj=nobj)

    maximum <- c(FALSE, NA, TRUE)[cplex$getObjDirCPLEX(env, prob) + 2L]

    OP(objective=obj, constraints=con, types=typ, bounds=bou, maximum = maximum)
}

