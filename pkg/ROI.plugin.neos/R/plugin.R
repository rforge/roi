
## NOTE: Gurobi
##       The Gurobi license for NEOS does not permit connections via XML-RPC. 
##       If you look at a solver page for Gurobi, for example, 
##       https://neos-server.org/neos/solvers/milp:Gurobi/AMPL.html, you will 
##       see that the XML-RPC interface is disabled. The license only allows 
##       submissions via the web interface. If you were able to submit jobs via 
##       XML-RPC previously, that was a mistake on our part introduced during an 
##       update to the NEOS codebase.

write_gams <- function(x, file, ...) {
    model <- ROI_to_gams(x)
    writeLines(model, con = file)
}

solve_OP <- function(x, control = list()) {
    neos_solve_gams(x, control)
}

check_method <- function(method, model_type, is_mip) {
    if ( isTRUE(tolower(method) == "gurobi") ) {
        stop("The Gurobi license for NEOS does not permit connections via XML-RPC. ",
             "Therefore Gurobi is not available via ROI.plugin.neos.")
    }
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
    if ( is.null(control$user) ) control$user <- "rneos"

    ## check_control_arguments(control)
    
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
    template <- NgetSolverTemplate(category = cate, solvername = unname(control$method), 
                                   inputMethod = language)
    argslist <- list(model = model, options = "", gdx = "", 
                     wantgdx = "", wantlog = "", comments = "")
    xmls <- CreateXmlString(neosxml = template, cdatalist = argslist)
    ## some solvers need a working email address not supported by rneos therefore
    ## we inject it
    if ( !is.null(control$email) ) {
        email_insertion <- sprintf("<email>%s</email>\n <model>", control$email)
        xmls <- gsub("<model>", email_insertion, xmls, fixed = TRUE)
    }

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
    if ( !is.null(email) ) {
        email_insertion <- sprintf("<email>%s</email>\n <options>", email)
        xmls <- gsub("<options>", email_insertion, xmls, fixed = TRUE)
    }

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
    if ( !is.null(email) ) {
        email_insertion <- sprintf("<email>%s</email>\n <options>", email)
        xmls <- gsub("<options>", email_insertion, xmls, fixed = TRUE)
    }

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
    if ( !is.null(email) ) {
        email_insertion <- sprintf("<email>%s</email>\n <model>", email)
        xmls <- gsub("<model>", email_insertion, xmls, fixed = TRUE)
    }

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
