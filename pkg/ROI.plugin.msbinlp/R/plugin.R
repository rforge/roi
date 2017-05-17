##
## This code is originally from the relations package with minor changes to 
## make it compatible with ROI.
## 

.expand_types <- function(x, n) {
    if ( is.null(x) ) {
        ## Continuous by default.
        rep.int("C", n)
    } else {
        if ( !is.character(x) || !all(x %in% c("C", "I", "B")) )
            stop("Invalid MIP variable types.")
        ## Be nicer than necessary ...
        rep_len(x, n)
    }
}

as_roi_control <- function(x) {
    x[['order']] <- NULL
    x
}

.find_up_to_n_binary_MILP_solutions <-
function(x, nos = 1L, add = FALSE, solver = NULL, control = NULL)
{
    ## Find up to n binary MILP solutions using a simple branch and cut
    ## approach (repeatedly splitting the binary variables and cutting
    ## the non-optimal branches).
    objfun <- objective(x)

    if(is.na(nos))
        nos <- .Machine$integer.max

    y <- ROI_solve(x, solver, as_roi_control(control))
    if((y$status$code != 0) || (nos == 1L) || !any(x$types == "B"))
        return(list(y))

    Vopt <- y$objval
    tol <- 1e-8
    ## (Smaller than .Machine$double.eps^0.5 as e.g. used in the
    ## all.equal() comparisons.)
    ## We used to have 1e-10, but SYMPHONY was not as precise as this.
    v_is_not_optimal <- if(x$maximum) {
        function(v) v < Vopt - tol
    } else {
        function(v) v > Vopt + tol
    }
    ## Improved versions could use relative tolerance, and/or take the
    ## number of variables into account.  Or maybe we can figure out the
    ## tolerance employed by the solvers when they declare optimality?

    if(add) {
        ## Find up to n solutions by adding binary constraints for each
        ## split.  This is most space efficient, but typically takes
        ## considerably more time that the default based on successive
        ## reductions by substitution.
        return(.find_up_to_n_binary_MILP_solutions_via_add(x, nos,
                                                           solver,
                                                           control,
                                                           y,
                                                           v_is_not_optimal))
    }

    ## Find solutions by recursively splitting binary variable and
    ## substituting the values into the program.

    ## Suppose x_j is one of the binary variables.  Let x[-j] be the
    ## vector of variables after dropping x_j.  If x_j has value b_j,
    ## the objective function is
    ##   c[-j]' x[-j] + c_j b_j,
    ## and the constraints become
    ##   (mat[, -j] %*% x[-j]) dir (rhs - mat[, j] * b_j)
    ## Note that the new constraint matrix and dir do not depend on the
    ## value of b_j.
    ## When recursively splitting, we need to keep track of the b
    ## values, the sum of the c_j b_j terms to add to the reduced
    ## objective value to obtain the value of the original problem, and
    ## the right hand sides.
    ## Also, binary positions are a nuisance to keep track of, so we
    ## start by rearranging variables to have the binary variables come
    ## last in reverse order of splitting.

    n_of_variables <- length(x$objective)
    types <- .expand_types(x$types, n_of_variables)
    binary_positions <- which(types == "B")
    n_of_binary_variables <- length(binary_positions)

    verbose <- identical(control$verbose, TRUE)

    .make_node <- function(b, y, v, r) list(b = b, y = y, v = v, r = r)

    .split_single_binary_variable <-
        function(node, i, pos_i, obj_i, mat_i, pos_b) {
            ## Try to avoid unnecessary copying of x via <<-
            ## manipulations.
            ## We know that the solution with the current b[i] is
            ## optimal, so we try the effect of flipping b[i].
            b <- node$b
            node$y$solution <- node$y$solution[-pos_i]
            if(b[i] == 0) {
                b1 <- b
                b1[i] <- 1
                v1 <- node$v + obj_i
                r1 <- node$r - mat_i
                x$constraints$rhs <<- r1
                y1 <- ROI_solve(x, solver, as_roi_control(control))
                ## Uncomment for debugging ...
                ## if(verbose) {
                ##     V <- y1$objval + v1
                ##     split <- (y1$status == 0) && !v_is_not_optimal(V)
                ##     message(sprintf("b[i] = 0, flip objval: %f, Delta: %.12f, status: %d, split: %s",
                ##                     V, Vopt - V, y1$status, split))
                ## }
                if((y1$status$code != 0) || v_is_not_optimal(y1$objval + v1))
                    list(node)
                else {
                    ## Fill the rest of b with the optimal entries.
                    b1[-seq_len(i)] <- y1$solution[pos_b]
                    list(node, .make_node(b1, y1, v1, r1))
                }
            } else {
                b0 <- b
                b0[i] <- 0
                v0 <- node$v
                node$v <- v0 + obj_i
                r0 <- node$r
                node$r <- r0 - mat_i
                x$constraints$rhs <<- r0
                y0 <- ROI_solve(x, solver, as_roi_control(control))
                ## Uncomment for debugging ...
                ## if(verbose) {
                ##     V <- y0$objval + v0
                ##     split <- (y0$status == 0) && !v_is_not_optimal(V)
                ##     message(sprintf("b[i] = 1, flip objval: %f, Delta: %.12f, status: %d, split: %s",
                ##                     V, Vopt - V, y0$status, split))
                ## }
                if((y0$status$code != 0) || v_is_not_optimal(y0$objval + v0))
                    list(node)
                else {
                    ## Fill the rest of b with the optimal entries.
                    b0[-seq_len(i)] <- y0$solution[pos_b]
                    list(node, .make_node(b0, y0, v0, r0))
                }
            }
        }

    ## We allow callers to specify the order in which binary splits
    ## should be attempted (so the i-th split is for binary variable
    ## order[i]).
    order <- control$order
    if(is.null(order) || length(order) != n_of_binary_variables)
        order <- seq_len(n_of_binary_variables)
    ## Rearrange variables to have the binary ones last in reverse split
    ## order.
    ind <- c(which(types != "B"), rev(which(types == "B")[order]))
    x$objective <- x$objective[ind]
    x$constraints$L <- x$constraints$L[, ind, drop = FALSE]
    x$types <- x$types[ind]
    y$solution <- y$solution[ind]

    pos_i <- n_of_variables
    pos_b <- seq.int(from = n_of_variables,
                     length.out = n_of_binary_variables,
                     by = -1L)
    nodes <- list(.make_node(y$solution[pos_b], y, 0, x$constraints$rhs))
    for(i in seq_len(n_of_binary_variables)) {
        pos_b <- pos_b[-1L]
        obj_i <- x$objective[pos_i]
        mat_i <- c(x$constraints$L[, pos_i])
        x$objective <- L_objective(x$objective[-pos_i])
        x$constraints$L <- x$constraints$L[, -pos_i, drop = FALSE]
        x$types <- x$types[-pos_i]
        if ( length(x$types) == 0L )
            break
        nodes <- do.call(c,
                         lapply(nodes,
                                .split_single_binary_variable,
                                i, pos_i, obj_i, mat_i, pos_b))
        len <- length(nodes)
        if(verbose)
            message(gettextf("N_of_binary_variables: %d *** N_of_optimal_branches: %d",
                             i, len))
        if(len >= nos) {
            nodes <- nodes[seq_len(nos)]
            break
        }
        pos_i <- pos_i - 1L
    }

    pos <- order(c(which(types != "B"), which(types == "B")[order]))
    finisher <- function(node) {
        ## Need to reconstruct solutions from the binary and non-binary
        ## parts and the respective objective values.
        y <- node$y
        y$solution <- c(y$solution, node$b)[pos]
        y$objval <- objfun(y$solution)## y$objval + node$v
        y
    }

    lapply(nodes, finisher)
}

.find_up_to_n_binary_MILP_solutions_via_add <-
function(x, nos = 1L, solver = NULL, control = NULL, y,
         v_is_not_optimal)
{
    ## Recursively add 0/1 constraints for the binary variables.
    ## Note that one can do this via adding the additional constraints
    ## to the original ones, or by maintaining them separately (e.g., in
    ## a vector of values for the binary variables).  We do the latter
    ## which uses as little space as possible, but requires extra time
    ## for merging both constraints when solving augmented problems.
    ## Alternatively, one could allow choosing between either approach.
    ## Note also that we need to keep new constraints and corresponding
    ## solutions to avoid recomputing solutions when enough were found.

    n_of_variables <- length(x$objective)
    types <- .expand_types(x$types, n_of_variables)
    binary_positions <- which(types == "B")
    n_of_binary_variables <- length(binary_positions)

    verbose <- identical(control$verbose, TRUE)

    mat <- x$constraints$L

    .make_additional_constraint_matrix <-
        if(is.simple_triplet_matrix(mat)) {
            function(bpos) {
                len <- length(bpos)
                simple_triplet_matrix(seq_len(len), bpos,
                                      rep.int(1, len),
                                      len, n_of_variables)
            }
        } else {
            function(bpos) {
                len <- length(bpos)
                add <- matrix(0, len, n_of_variables)
                add[cbind(seq_len(len), bpos)] <- 1
                add
            }
        }

    .solve_MILP_with_additional_binary_constraints <-
        function(x, bpos, bval) {
            len <- length(bpos)
            x$constraints <-
                L_constraint(L = rbind(mat,
                                       .make_additional_constraint_matrix(bpos)),
                             dir = c(x$constraints$dir, rep.int("==", len)),
                             rhs = c(x$constraints$rhs, bval))
            ROI_solve(x, solver, as_roi_control(control))
        }

    .split_single_binary_variable <- function(y, i) {
        oi <- order[i]
        ind <- order[seq_len(i)]
        pos <- binary_positions[ind]
        b <- y$solution[binary_positions]
        ## Try flipping the i-th binary variable and see whether this
        ## also delivers optimal solutions.
        b[oi] <- 1 - b[oi]
        yf <- .solve_MILP_with_additional_binary_constraints(x, pos, b[ind])
        if((yf$status$code != 0) || v_is_not_optimal(yf$objval))
            list(y)
        else
            list(y, yf)
    }

    ylist <- list(y)
    ## We allow callers to specify the order in which binary splits
    ## should be attempted (so the i-th split is for binary variable
    ## order[i], i.e., at binary_positions[order[i]]).
    order <- control$order
    if(is.null(order))
        order <- seq_len(n_of_binary_variables)
    for(i in seq_len(n_of_binary_variables)) {
        ylist <-
            do.call(c, lapply(ylist, .split_single_binary_variable, i))
        len <- length(ylist)
        if(verbose)
            message(gettextf("N_of_binary_variables: %d *** N_of_optimal_branches: %d",
                             i, len))
        if(len >= nos) {
            ylist <- ylist[seq_len(nos)]
            break
        }
    }
    ylist
}

solve_OP <- function(x, control=list()) {
    solver <- ROI_plugin_get_solver_name( getPackageName() )

    if ( is.null(control$nsol_max) ) {
        nos <- 1L    
    } else {
        if ( is.numeric(control$nsol_max) ) {
            nos <- as.integer(control$nsol_max)
        } else {
            nos <- NA
        }
    }
    method <- if ( is.null(control$method) ) "glpk" else control$method
    control[['method']] <- NULL
    control[['nsol_max']] <- NULL
    control[['dry_run']] <- NULL

    out <- .find_up_to_n_binary_MILP_solutions(x, nos = nos,
                                                  add = TRUE, 
                                                  solver = method,
                                                  control = control )
    class(out) <- c("msbinlp_solution_set", "OP_solution_set")
    return(out)
}

