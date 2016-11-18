build_bounds <- function(bo) {
    ind <- sort(union(bo$lower$ind, bo$upper$ind))
    lower <- double(length(ind))
    upper <- rep.int(Inf, length(ind))
    lower[which(bo$lower$ind %in% ind)] <- bo$lower$val
    upper[which(bo$upper$ind %in% ind)] <- bo$upper$val
    list(lower=lower, upper=upper, ind=ind)
}

map_dir <- function(x) {
    map <- setNames(seq_len(3), c("<=", ">=", "=="))
    map[x]
}

map_types <- function(x) {
    if ( !length(types(x)) )
        return( list(real = seq_len(length(objective(x)))) )
    map <- setNames(c("integer", "binary", "real"), c("I", "B", "C"))
    aggregate(id ~ type, data=data.frame(type=map[types(x)], id=seq_along(types(x)), 
              stringsAsFactors=FALSE), FUN=c)
}

.verbose_modes <- c("neutral", "critical", "severe", "important", "normal", "detailed", "full")

map_verbose <- function(x) {
    if ( is.null(x) ) {
        return("neutral")
    } else if ( is.logical(x) ) {
        if ( x ) return( "normal")
        else return("neutral")
    } else if ( is.numeric(x) ) {
        if ( is.element(x, c(0, 1)) ) {
            if ( x ) return( "normal")
            else return("neutral")
        } else {
            if ( x >= 1 & x < 10 ) {
                return(.verbose_modes[as.integer(x)])
            }
        }
    } else if ( is.character(x) ) {
        if ( is.element(x, .verbose_modes) )
            return(x)
    }
    return("neutral")
}

solve_OP <- function(x, control=list()) {
    solver <- .ROI_plugin_get_solver_name( getPackageName() )

    nr <- length(constraints(x))
    nc <- length(objective(x))

    control$verbose <- map_verbose(control$verbose)
    lp <- make.lp(nr, nc, verbose = control$verbose)

    ## objective
    set.objfn(lp, terms(objective(x))[['L']]$v, terms(objective(x))[['L']]$j)
    
    ## constraints
    for (i in seq_len(nr)) {
        irow <- constraints(x)[['L']][i,]
        set.row(lp, i, irow$v, irow$j)
    }
    set.rhs(lp, constraints(x)[['rhs']], seq_len(nr))
    set.constr.type(lp, map_dir(constraints(x)[['dir']]), seq_len(nr))
    
    ## types
    xtypes <- map_types(x)
    for ( typ in c("integer", "binary", "real") ) {
        if ( !is.null(xtypes[[typ]]) )
            set.type(lp, xtypes[[typ]], typ)
    }

    ## bounds (lp_solve has the lower bound default by zero)
    if ( !is.null(bounds(x)) ) {
        bo <- build_bounds(bounds(x))
        set.bounds(lp, bo$lower, bo$upper, bo$ind)
    }

    ## maximum
    control$sense <- if (x$maximum) "max" else "min"

    ## control options
    ## - basis 
    ##     list(basis = c(1, 2, 3), nonbasic = TRUE, default = TRUE)
    if ( !is.null(control$basis) ) {
        stopifnot(length(control$basis$basis), is.numeric(control$basis$basis))
        if ( is.null(control$basis$nonbasic) ) control$basis$nonbasic <- FALSE
        if ( is.null(control$basis$default) ) control$basis$default <- FALSE
        set.basis(lp, control$basis$basis, control$basis$nonbasic, control$basis$default)
    }
    ## - branch.mode
    ##    list(columns =, modes = c("ceiling"))
    if ( !is.null(control$branch.mode) ) {
        .branch_modes <- c("ceiling", "floor", "auto", "default")
        stopifnot(length(control$branch.mode$columns), is.numeric(control$branch.mode$columns))
        stopifnot(all(control$branch.mode$modes %in% .branch_modes))
        stopifnot( length(control$branch.mode$columns) == length(control$branch.mode$modes) )
        set.branch.mode(lp, control$branch.mode$columns, control$branch.mode$modes)
    }
    ## - branch.weights
    if ( !is.null(control$branch.weights) ) {
        stopifnot(is.numeric(control$branch.weights), length(control$branch.weights) == length(objective(x)))
        set.branch.weights(control$branch.weights)
    }
    ## TODO: set.semicont
    ## TODO: add.SOS
    ## NOTE: The sense option
    control.rm <- c("dry_run", "basis", "branch.mode", "branch.weights")
    lp_control <- do.call(lp.control, c(lp, control[!is.element(names(control), control.rm)]))

    if ( isTRUE(control$dry_run) )
        return(lp)

    status <- solve(lp)

    sol <- list()
    sol$solution_count <- get.solutioncount(lp)
    sol$solutions <- vector("list", sol$solution_count)
    sol$dual_solutions <- vector("list", sol$solution_count)
    for ( i in seq_len(sol$solution_count) ) {
        select.solution(lp, i)
        sol$solutions[[i]] <- get.primal.solution(lp)
        sol$dual_solutions[[i]] <- get.dual.solution(lp)
    }
    sol$sensitivity_objfun <- get.sensitivity.objex(lp)
    sol$sensitivity_rhs <- get.sensitivity.rhs(lp)
    sol$total_iter <- get.total.iter(lp)
    sol$total_nodes <- get.total.nodes(lp)

    delete.lp(lp)

    x.solution <- tail(sol$solutions[[1]], nc)
    optimum <- tryCatch({as.numeric(objective(x)(x.solution))}, 
                        error=function(e) as.numeric(NA))
    return( .ROI_plugin_canonicalize_solution( solution = x.solution, 
                                               optimum  = optimum,
                                               status   = status,
                                               solver   = solver, 
                                               message  = sol ) 
    )
}
