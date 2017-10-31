
## NOTE: Gurobi
##       The Gurobi license for NEOS does not permit connections via XML-RPC. 
##       If you look at a solver page for Gurobi, for example, 
##       https://neos-server.org/neos/solvers/milp:Gurobi/AMPL.html, you will 
##       see that the XML-RPC interface is disabled. The license only allows 
##       submissions via the web interface. If you were able to submit jobs via 
##       XML-RPC previously, that was a mistake on our part introduced during an 
##       update to the NEOS codebase.

neos_milp_solver <- c("Cbc", "CPLEX", "FICO-Xpress", "MOSEK", "scip")
neos_lp_solver <- union(c("BDMLP", "CPLEX", "FICO-Xpress", "MOSEK"), neos_milp_solver)

neos_miqcqp_solver <- union(c("AlphaECP", "BARON", "Bonmin", "Couenne", "DICOPT", 
                              "Knitro",  "LINDOGlobal", "SBB", "scip"), 
                            c("MOSEK", "CPLEX"))
neos_miqp_solver <- neos_miqcqp_solver
neos_qcqp_solver <- union(c("CONOPT", "Ipopt", "Knitro", "MINOS", "MOSEK", 
                            "PATHNLP", "SNOPT"), 
                          neos_miqcqp_solver)
neos_qp_solver <- neos_qcqp_solver

neos_solver <- unique(c(neos_lp_solver, neos_milp_solver, neos_qcqp_solver, neos_miqcqp_solver))
neos_solver_mapping <- setNames(neos_solver, tolower(neos_solver))

is.slam_zero_matrix <- function(x) {
    inherits(x, "simple_triplet_matrix") & isTRUE(length(x$v) == 0L)
}

strip <- function(x) gsub("(^\\s+|\\s+$)", "", x)

prefix_spaces <- function(x, xnchar) {
    paste(c(rep.int(" ", xnchar - nchar(x)), x), collapse = "")
}

subfix_spaces <- function(x, xnchar) {
    paste(c(x, rep.int(" ", xnchar - nchar(x))), collapse = "")
}

create_table <- function(mat, table_name, row_names, col_names) {
    table_header <- sprintf("Table %s(i, j)", table_name)
    A <- as.character(mat)
    ncmax <- max(nchar(A), nchar(col_names))
    A <- sapply(A, prefix_spaces, ncmax + 1L)
    A <- matrix(A, nrow(mat))
    col_names <- unname(sapply(col_names, prefix_spaces, ncmax + 1L))
    A <- rbind(col_names, A)

    row_names = sprintf("  %s", row_names)
    nrmax <- max(nchar(row_names))
    row_names <- unname(sapply(c("", row_names), subfix_spaces, nrmax + 1L))
    A <- apply(cbind(row_names, A), 1, paste, collapse = "")

    sprintf("%s\n%s;\n", table_header, paste(A, collapse = "\n"))
}

create_sparse_vector <- function(vec, par_name) {
    header <- sprintf("Parameter %s(j)", par_name)
    sprintf("%s\n/%s/;\n", header, 
            paste(sprintf("C%i %s", vec$j, as.character(vec$v)), collapse = "\n"))
}

create_sparse_matrix <- function(mat, par_name, row_prefix = "R", col_prefix = "C") {
    header <- sprintf("Parameter %s", par_name)
    sprintf("%s\n/%s/;\n", header, 
            paste(sprintf("%s%i.%s%i %s", row_prefix, mat$i, col_prefix, 
                          mat$j, as.character(mat$v)), collapse = "\n"))
}

## constr is a list of sparse matrices
create_sparse_array_from_Q_constraint <- function(constr, par_name, row_prefix = "R", col_prefix = "C") {
    ## Parname should be (keq, j, j)
    header <- sprintf("Parameter %s", par_name)
    array_index <- function(id, x) {
        if (is.slam_zero_matrix(x)) integer(0) else rep.int(id, length(x$v))
    }
    k <- unlist(mapply(array_index, seq_along(constr), constr, 
                       SIMPLIFY = FALSE, USE.NAMES = FALSE))
    i <- unlist(lapply(constr, "[[", "i"))
    j <- unlist(lapply(constr, "[[", "j"))
    v <- unlist(lapply(constr, "[[", "v"))

    sprintf("%s\n/%s/;\n", header, 
            paste(sprintf("%s%i.%s%i.%s%i %s", row_prefix, k, 
                          col_prefix, i, col_prefix, j,
                          as.character(v)), collapse = "\n"))
}

create_parameter_vector <- function(vec, name, index_name, names) {
    vec <- as.character(vec)
    param <- sprintf("%s %s", names, vec)
    sprintf("Parameter %s(%s)\n/%s/ ;\n", name, index_name, paste(param, collapse = "\n"))
}


has.eq <- function(x) {
    any(constraints(x)$dir == "==")
}

has.leq <- function(x) {
    any(constraints(x)$dir %in% c("<=", "<"))
}

has.geq <- function(x) {
    any(constraints(x)$dir %in% c(">=", ">"))
}

extract_results <- function(results, jbin, jint, jcon) UseMethod("extract_results")

extract_results.NeosAns <- function(results, n) {
    s <- gsub(".*\\-\\-\\-BEGIN\\.SOLUTION\\-\\-\\-\\s*", "", results@ans)
    s <- gsub("---END\\.SOLUTION---.*", "", s)
    s <- unlist(strsplit(s, "\\n+"))
    s <- strip(s)
    s <- s[!substr(s, 1, 3) == "---"]
    s <- paste(s, collapse = ", ")
    if ( any(grep("ALL", s, ignore.case = TRUE)) ) {
        cnames <- sprintf("C%i", seq_len(n))
        values <- rep.int(as.double(gsub("[^0-9.]", "", s)), n)
    } else {
        s <- unlist(strsplit(s, ",", fixed = TRUE))
        s <- strip(s)
        s <- strsplit(s, "\\s+")
        cnames <- sapply(s, "[", 1)
        values <- sapply(s, "[", 2)
    }
    s <- setNames(as.double(values), cnames)
    nam <- sprintf("C%s", seq_len(n))
    s <- s[nam]
    names(s) <- nam
    s[is.na(s)] <- 0
    s
}

select_method <- function(model_type, is_mip) {
    if ( is_mip ) {
        if ( model_type == "lp" )
            method <- "CPLEX"
        else 
            method <- "Knitro"
    } else {
        if ( model_type == "lp" )
            method <- "CPLEX"
        else 
            method <- "Knitro"
    }
    unname(method)
}

check_control_arguments <- function(control) {
    stopifnot(is.numeric(control$id))
    if ( !all( c("user", "email") %in% names(control) ) ) {
        missing_args <- shQuote(setdiff(c("user", "email"), names(control)))
        stop("the control argument", if ( length(missing_args) > 1 ) "s " else " ",
             paste(missing_args, collapse = ", "), 
             if ( length(missing_args) > 1 ) " are " else " is  ",
             "missing", call. = FALSE)
    }
}

which_model_type <- function(x) {
    if ( !inherits(objective(x), "Q_objective") | !inherits(constraints(x), "Q_constraint") ) {
        stop("'ROI_to_gams' only supports linear and quadratic objectives and ",
             "linear and quadratic constraints!", call. = FALSE)
    }
    if ( inherits(objective(x), "L_objective") & is.L_constraint(constraints(x)) ) {
        model_type <- "lp"
    } else if ( is.L_constraint(constraints(x)) ) {
        model_type <- "qp"
    } else {
        model_type <- "qcqp"
    }
    model_type
}


ROI_to_gams <- function(x) UseMethod("ROI_to_gams")

ROI_to_gams.OP <- function(x) {
    model_type <- which_model_type(x)
    to_gams <- switch(model_type, lp   = roi_lp_to_gams, 
                                  qp   = roi_qp_to_gams, 
                                  qcqp = roi_qcqp_to_gams)
    to_gams(x)
}

write_gams <- function(x, file, ...) {
    model <- ROI_to_gams(x)
    writeLines(model, con = file)
}

solve_OP <- function(x, control = list()) {
    neos_solve_gams(x, control)
}

check_method <- function(method, model_type, is_mip) {
    if ( is_mip ) {
        if ( model_type == "lp" ) {
            if ( !method %in% neos_milp_solver ) {
                if ( method %in% neos_miqcqp_solver ) {
                    warning("a MINLP solver is used for a MILP problem. ", 
                            "Most likely one of the following solver would be ",
                            "better suited for solving this problem: ", 
                            paste(shQuote(neos_milp_solver), collapse = ", "))
                } else {
                    stop("solver ", shQuote(method), " not applicable. ",
                         "Most likely one of the following solver would be ",
                         "better suited for solving this problem: ", 
                         paste(shQuote(neos_milp_solver), collapse = ", "))
                }
            }
        } else {
            if ( !method %in% neos_miqcqp_solver ) {
                stop("solver ", shQuote(method), " not applicable. ",
                     "Most likely one of the following solver would be ",
                     "better suited for solving this problem: ", 
                     paste(shQuote(neos_miqcqp_solver), collapse = ", "))                
            }
        }
    } else {
        if ( model_type == "lp" ) {
            if ( !method %in% neos_solver ) {
                stop("solver ", shQuote(method), " not applicable. ",
                     "Most likely one of the following solver would be ",
                     "better suited for solving this problem: ", 
                     paste(shQuote(neos_lp_solver), collapse = ", "))
            }
            if ( !method %in% neos_lp_solver ) {
                warning("a MINLP solver is used for a MILP problem. ", 
                        "Most likely one of the following solver would be ",
                        "better suited for solving this problem: ", 
                        paste(shQuote(neos_lp_solver), collapse = ", "))
            }
        } else {
            if ( !method %in% neos_qcqp_solver ) {
                stop("solver ", shQuote(method), " not applicable. ",
                     "Most likely one of the following solver would be ",
                     "better suited for solving this problem: ", 
                     paste(shQuote(neos_qcqp_solver), collapse = ", "))
            }
        }
    }
}

neos_solve_gams <- function(x, control = list()) {

    if ( is.null(control$interface) ) control$interface <- ""
    if ( is.null(control$id) ) control$id <- 0

    check_control_arguments(control)
    
    language <- "GAMS"
    model_type <- which_model_type(x)
    model <- ROI_to_gams.OP(x)

    is_mip <- any(types(x) %in% c("B", "I"))

    if ( is.null(control$method) ) {
        control$method <- select_method(model_type, is_mip)
        warning("no method provided set to ", shQuote(control$method))
    } else {
        solver_method <- neos_solver_mapping[tolower(control$method)]
        if ( is.na(solver_method) ) {
            stop("unknown solver: ", shQuote(control$method))
        } else {
            control$method <- solver_method
        }
    }
    check_method(control$method, model_type, is_mip)

    cate <- unname(match_category(control$method))
    ##template <- NgetSolverTemplate(category = "minco", solvername = "Knitro", inputMethod = language)
    template <- NgetSolverTemplate(category = cate, solvername = unname(control$method), 
                                   inputMethod = language)
    argslist <- list(model = model, options = "", gdx = "", 
                     wantgdx = "", wantlog = "", comments = "")
    xmls <- CreateXmlString(neosxml = template, cdatalist = argslist)
    ## some solvers need a working email address not supported by rneos therefore
    ## we inject it
    email_insertion <- sprintf("<email>%s</email>\n <model>", control$email)
    xmls <- gsub("<model>", email_insertion, xmls, fixed = TRUE)

    solver_call <- list(NsubmitJob, xmlstring = xmls, user = control$user, 
                        interface = control$interface, id = control$id)
    mode(solver_call) <- "call"
    if ( isTRUE(control$dry_run) )
        return(solver_call)

    job <- eval(solver_call)

    if ( any(grep("Error", job@password, ignore.case = TRUE)) )
        stop( paste(job@password, collapse = "\n"), 
              " In some cases the solver licence does not permit connections via", 
              " XML-RPC. Therefore these solvers can not be accessed by 'ROI.plugin.neos'",
              " directly. An alternative is to write the problem out via",
              " write.op(model, 'my_op.gms', 'gams') and commit it via the web-interface.",
              " Or just use a alternative solver." )

    if ( isTRUE(control$wait) )
        return(job)

    results <- NgetFinalResults(obj = job, convert = TRUE)

    ## gams_errors <- extract_error(results@ans)
    ##  if ( length(gams_errors) )
    ##      stop(paste(gams_errors, collapse = "\n"))

    n <- length(objective(x))
    sol <- extract_results(results, n)
    names(sol) <- names(objective(x))

    optimum <- objective(x)(sol)

    ROI_plugin_canonicalize_solution( solution = sol, optimum  = optimum,
                                      status   = extract_status(results@ans),
                                      solver   = "neos", message = results@ans )
}

extract_status <- function(msg) {
    m <- gregexpr("SOLVER STATUS.*?\n", msg)
    s <- gsub("\\D", "", strip(unlist(regmatches(msg, m))))
    status <- suppressWarnings(as.integer(s))
    if ( is.na(status) | isTRUE(status < 1L) | isTRUE(status > 13L) )
        status <- 100L
    status
}

extract_error <- function(msg) {
    s <- strsplit(msg, "\n")[[1]]
    i <- grep("error", s, ignore.case = TRUE)
    s[i]
}

neos_solve_lp <- function(x, solver, language, user, email, interface = "", id = 0L) {
    model <- roi_lp_to_gams(x)

    template <- NgetSolverTemplate(category = "lp", solvername = solver, inputMethod = language)
    argslist <- list(model = model)
    xmls <- CreateXmlString(neosxml = template, cdatalist = argslist)
    ## some solvers need a working email address not supported by rneos therefore
    ## we inject it
    email_insertion <- sprintf("<email>%s</email>\n <options>", email)
    xmls <- gsub("<options>", email_insertion, xmls, fixed = TRUE)

    job <- NsubmitJob(xmlstring = xmls, user = user, interface = interface, id = id)

    results <- NgetFinalResults(obj = job, convert = TRUE)    
    xsol <- extract_results(results)

    n <- length(objective(x))

    sol <- setNames(double(n), sprintf("C%i", seq_len(n)))
    sol[names(xsol)] <- xsol
    optimum <- objective(x)(sol)

    status <- strip(unlist(regmatches(results@ans, gregexpr("SOLVER STATUS.*?\n", results@ans))))
    status <- as.integer(gsub("\\D", "", status))

    ROI_plugin_canonicalize_solution( solution = sol, optimum  = optimum,
                                      status   = status,
                                      solver   = solver, message = results@ans )
}

## returns the category given the solver name this 
## is necessary since the neos solver classification is kind of strange.
## I kind of assume that this actually has no influence on the solution!
## NOTE: for now just return lp
match_category <- function(solver) {
    mapping <- c("alphaecp" = "minco", "baron" = "minco", "bdmlp" = "lp", 
                 "bonmin" = "minco", "cbc" = "milp", "conopt" = "nco", 
                 "couenne" = "minco", "cplex" = "milp", "dicopt" = "minco", 
                 "fico-xpress" = "milp", "gurobi" = "lp", "ipopt" = "nco", 
                 "knitro" = "minco", "lindoglobal" = "minco", "minos" = "nco", 
                 "mosek" = "milp", "pathnlp" = "nco", "sbb" = "minco", 
                 "scip" = "minco" , "snopt" = "nco")
    x <- mapping[tolower(solver)]
    if ( is.na(x) )
        stop("unknown solver: ", shQuote(solver))
    x
}

neos_solve_qp <- function(x, solver, language, user, email, interface = "", id = 0L) {
    model <- roi_qp_to_gams(x)

    cate <- unname(match_category(solver))
    ##template <- NgetSolverTemplate(category = "minco", solvername = "Knitro", inputMethod = language)
    template <- NgetSolverTemplate(category = cate, solvername = solver, inputMethod = language)
    argslist <- list(model = model)
    xmls <- CreateXmlString(neosxml = template, cdatalist = argslist)
    ## some solvers need a working email address not supported by rneos therefore
    ## we inject it
    email_insertion <- sprintf("<email>%s</email>\n <options>", email)
    xmls <- gsub("<options>", email_insertion, xmls, fixed = TRUE)

    job <- NsubmitJob(xmlstring = xmls, user = user, interface = interface, id = id)
    job

    results <- NgetFinalResults(obj = job, convert = TRUE)
    results

    xsol <- extract_results(results)

    n <- length(objective(x))

    sol <- setNames(double(n), sprintf("C%i", seq_len(n)))
    sol[names(xsol)] <- xsol
    optimum <- objective(x)(sol)

    status <- strip(unlist(regmatches(results@ans, gregexpr("SOLVER STATUS.*?\n", results@ans))))
    status <- as.integer(gsub("\\D", "", status))

    ROI_plugin_canonicalize_solution( solution = sol, optimum  = optimum,
                                      status   = status,
                                      solver   = solver, message = results@ans )
}

neos_solve_qcqp <- function(x, solver, language, user, email, interface = "", id = 0L, wait = TRUE) {
    model <- roi_qcqp_to_gams(x)

    cate <- unname(match_category(solver))
    ##template <- NgetSolverTemplate(category = "minco", solvername = "Knitro", inputMethod = language)
    template <- NgetSolverTemplate(category = cate, solvername = solver, inputMethod = language)
    argslist <- list(model = model, options = NULL, gdx = NULL, 
                     wantgdx = NULL, wantlog = NULL, comments = NULL)
    xmls <- CreateXmlString(neosxml = template, cdatalist = argslist)
    ## some solvers need a working email address not supported by rneos therefore
    ## we inject it
    email_insertion <- sprintf("<email>%s</email>\n <model>", email)
    xmls <- gsub("<model>", email_insertion, xmls, fixed = TRUE)

    job <- NsubmitJob(xmlstring = xmls, user = user, interface = interface, id = id)
    if ( !wait )
        return(job)

    results <- NgetFinalResults(obj = job, convert = TRUE)

    xsol <- extract_results(results)

    n <- length(objective(x))

    sol <- setNames(double(n), sprintf("C%i", seq_len(n)))
    sol[names(xsol)] <- xsol
    optimum <- objective(x)(sol)

    status <- strip(unlist(regmatches(results@ans, gregexpr("SOLVER STATUS.*?\n", results@ans))))
    status <- as.integer(gsub("\\D", "", status))

    ROI_plugin_canonicalize_solution( solution = sol, optimum  = optimum,
                                      status   = status,
                                      solver   = solver, message = results@ans )
}

sparsity <- function(x) {
    stopifnot(inherits(x, "simple_triplet_matrix"))
    1 - ( length(x$i) / (x$nrow * x$ncol) )
}

is_sparse <- function(x) {
    ## simple triplet matrix uses 3 cooardinates therfore it makes only sense
    ## if less than 1 / 3 are non-zero entries
    ( length(x$i) / (x$nrow * x$ncol) ) < (1 / 3)
}
