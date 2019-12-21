
read.lp.lp <- function(file, ...) {
    ROI.plugin.lpsolve::read.lp(file, type = "lp")
}

read.lp.mps <- function(file, ...) {
    ROI.plugin.lpsolve::read.lp(file, type = "mps")
}

read.lp.freemps <- function(file, ...) {
    ROI.plugin.lpsolve::read.lp(file, type = "freemps")
}

write.lp.lp <- function(x, file, ...) {
    ROI.plugin.lpsolve::write.lp(x, file, type = "lp")   
}

write.lp.mps <- function(x, file, ...) {
    ROI.plugin.lpsolve::write.lp(x, file, type = "mps")   
}

write.lp.freemps <- function(x, file, ...) {
    ROI.plugin.lpsolve::write.lp(x, file, type = "freemps")   
}

build_bounds <- function(bo) {
    ind <- seq_len(bo$nobj)
    lower <- double(bo$nobj)
    upper <- rep.int(Inf, bo$nobj)
    lower[bo$lower$ind] <- bo$lower$val
    upper[bo$upper$ind] <- bo$upper$val
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
    ty <- aggregate(id ~ type, data=data.frame(type=map[types(x)], id=seq_along(types(x)), 
                    stringsAsFactors=FALSE), FUN=c, simplify=FALSE)
    setNames(ty[,2], ty[,1])
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

is.all_integer <- function(types) {
    if ( is.null(types) ) {
        return(FALSE)
    }
    all(types %in% c("B", "I"))
}

solve_OP <- function(x, control = list()) {
    solver <- "lpsolve"

    nr <- length(constraints(x))
    nc <- length(objective(x))

    control$verbose <- map_verbose(control$verbose)
    om <- make.lp(nr, nc, verbose = control$verbose)

    ## objective
    set.objfn(om, terms(objective(x))[['L']]$v, terms(objective(x))[['L']]$j)
    
    ## constraints
    for (i in seq_len(nr)) {
        irow <- constraints(x)[['L']][i,]
        set.row(om, i, irow$v, irow$j)
    }
    set.rhs(om, constraints(x)[['rhs']], seq_len(nr))
    set.constr.type(om, map_dir(constraints(x)[['dir']]), seq_len(nr))

    ## bounds (lp_solve has the lower bound default by zero)
    if ( !is.null(bounds(x)) ) {
        bo <- build_bounds(bounds(x))
        set.bounds(om, bo$lower, bo$upper, bo$ind)
    }
    
    ## types
    xtypes <- map_types(x)
    for ( typ in c("integer", "binary", "real") ) {
        if ( !is.null(xtypes[[typ]]) ) {
            set.type(om, xtypes[[typ]], typ)
        }
    }
    all_integer <- if (is.all_integer(types(x))) TRUE else FALSE

    ## maximum
    control$sense <- if (x$maximum) "max" else "min"

    ## control options
    ## - basis 
    ##     list(basis = c(1, 2, 3), nonbasic = TRUE, default = TRUE)
    if ( !is.null(control$basis) ) {
        stopifnot(length(control$basis$basis), is.numeric(control$basis$basis))
        if ( is.null(control$basis$nonbasic) ) control$basis$nonbasic <- FALSE
        if ( is.null(control$basis$default) ) control$basis$default <- TRUE
        set.basis(om, control$basis$basis, control$basis$nonbasic, control$basis$default)
    }
    ## - branch.mode
    ##    list(columns =, modes = c("ceiling"))
    if ( !is.null(control$branch.mode) ) {
        .branch_modes <- c("ceiling", "floor", "auto", "default")
        stopifnot(length(control$branch.mode$columns), is.numeric(control$branch.mode$columns))
        stopifnot(all(control$branch.mode$modes %in% .branch_modes))
        stopifnot( length(control$branch.mode$columns) == length(control$branch.mode$modes) )
        set.branch.mode(om, control$branch.mode$columns, control$branch.mode$modes)
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
    lp_control <- do.call(lp.control, c(om, control[!is.element(names(control), control.rm)]))

    if ( isTRUE(control$dry_run) ) {
        return(list(lpSolveAPI::solve.lpExtPtr, om))
    }

    if ( isTRUE(control$nsol_max > 1) ) {
        ## return multiple solutions
        solutions <- .find_up_to_n_binary_MILP_solutions(om, x, nsol = control$nsol_max)
        i <- which(!sapply(solutions, is.null))
        solutions <- solutions[i]
        class(solutions) <- c("lpsolve_solution_set", "OP_solution_set")
        return(solutions)
    } else {
        ## return 1 solution
        status <- solve(om)

        sol <- list()
        sol$solution_count <- get.solutioncount(om)
        if ( sol$solution_count > 0 ) {
            sol$solutions <- vector("list", sol$solution_count)
            sol$dual_solutions <- vector("list", sol$solution_count)
            for ( i in seq_len(sol$solution_count) ) {
                select.solution(om, i)
                sol$solutions[[i]] <- get.variables(om)
                sol$dual_solutions[[i]] <- get.dual.solution(om)
            }
        
            if ( all( x$types == "C" ) ) { ## these two functions are only for lp available
                sol$sensitivity_objfun <- get.sensitivity.obj(om) 
                ## NOTE: Get Sensitivity - Objective Extended
                ## get.sensitivity.objex(om)
                sol$sensitivity_rhs <- get.sensitivity.rhs(om)
            }
            sol$total_iter <- get.total.iter(om)
            sol$total_nodes <- get.total.nodes(om)

            optimum <- objective_value(objective(x), sol$solutions[[1L]])
        } else {
            sol$solutions <- list(rep.int(NA_real_, nc))
            sol$dual_solutions <- list(rep.int(NA_real_, nr))
            optimum <- NA_real_
        }

        return( ROI_plugin_canonicalize_solution( solution = sol$solutions[[1L]], 
                                                  optimum  = optimum,
                                                  status   = status,
                                                  solver   = solver, 
                                                  message  = sol ) )
    }
}

lp_solve <- function(om) {
    status <- solve(om)
    x <- vector("list", 5)
    names(x) <- c("solution", "optimum", "status", "solver", "message")
    x[["solution"]] <- get.variables(om)
    x[["optimum"]] <- get.objective(om)
    x[["status"]] <- status
    x[["solver"]] <- "lpsolve"
    dual <- tryCatch(get.dual.solution(om),
                     error = function(e) rep.int(NA, NROW(om)))
    x[["message"]] <- list(solution = x[["solution"]], 
                           dual_solution = dual)
    do.call(ROI_plugin_canonicalize_solution, x)
}

objective_value <- function(obj_fun, solution) {
    tryCatch({as.numeric(obj_fun(solution))}, error=function(e) as.numeric(NA))
}

## nsol <- control$nsol
.find_up_to_n_binary_MILP_solutions <- function(om, x, nsol, tolerance = 1e-4) {
    k <- which(types(x) == "B")
    if ( length(k) == 0 ) {
        stop("no 'binary' variables found")
    }

    solutions <- vector("list", nsol)
    solutions[[1L]] <- lp_solve(om)
    if ( solutions[[1L]]$status$code != 0 ) {
        return(solutions[1L])
    }
    obj_val <- solutions[[1L]]$objval

    ## one row of A (A x <= b)
    ak <- double(length(k))

    for ( i in seq_len(nsol - 1L) ) {
        sol <- solutions[[i]]$solution[k]
        rhs <- sum(sol) - 1L
        ak[sol == 1] <-  1
        ak[sol == 0] <- -1
        add.constraint(om, xt = ak, type = "<=", rhs = rhs, indices = k)

        sobj <- lp_solve(om)
        if ( sobj$status$code != 0 ) {
            return(solutions)
        }

        ## if we get the same solution as last time we quit
        if ( sum(abs(sobj$solution - solutions[[i]]$solution)) < tolerance ) {
            return(solutions)
        }

        if ( abs(sobj$objval - obj_val) > tolerance ) {
            return(solutions)
        }

        solutions[[i + 1L]] <- sobj
    }
    return(solutions)
}

ROI_plugin_solution_dual.lpsolve_solution <- function(x) {
    x[['message']][['dual_solutions']][[1L]]
}

write.lp <- function(x, file, type=c("lp", "mps", "freemps")) {
    type <- match.arg(type)
    om <- as.list(solve_OP(x, list(dry_run=TRUE, verbose="neutral")))[[2]]
    lpSolveAPI::write.lp(om, file, type)
    invisible(NULL)
}

col_to_slam <- function(col, j) {
    names(col) <- c("v", "i")
    col$j <- rep.int(j, length(col$v))
    as.data.frame(col[c("i", "j", "v")])
}

lp_to_slam <- function(om, ncol) {
    fun <- function(j) col_to_slam(get.column(om, j), j)
    stm <- as.list(do.call(rbind, lapply(seq_len(ncol), fun)))
    stm[['i']] <- stm[['i']] + 1L
    stm <- c(stm, nrow=max(stm$i), ncol=ncol, dimnames=list(NULL))
    class(stm) <- "simple_triplet_matrix"
    stm
}

.type_map <- setNames(c("I", "B", "C"), c("integer", "binary", "real"))

read.lp <- function(file, type=c("lp", "mps", "freemps")) {
    type <- match.arg(type)
    
    om <- do.call(lpSolveAPI::read.lp, as.list(c(filename=file, type=type, 
                  verbose="neutral")))

    nr <- nrow(om)
    nc <- ncol(om)
    coeff <- lp_to_slam(om, nc)

    ## objective
    obj <- coeff[1,]

    ## constraints
    con.L <- coeff[-1,]
    con.dir <- c("<=", ">=", "==")[get.constr.type(om, as.char=FALSE)]
    con.rhs <- get.rhs(om)
    
    ## types
    ty <- unname(.type_map[get.type(om)])

    ## bounds
    bo <- get.bounds(om)
    bo <- V_bound(li=seq_len(nc), ui=seq_len(nc), lb=bo$lower, ub=bo$upper, nobj=nc)
    
    ## maximium
    is_maxi <- lp.control(om)$sense == "maximize"

    x <- OP(objective = L_objective(obj), 
            constraints = L_constraint(con.L, con.dir, con.rhs),
            types = ty, bounds = bo, maximum = is_maxi)
    x
}

