
neos_milp_solver <- function() {
    c("Cbc", "CPLEX", "FICO-Xpress", "MOSEK", "scip")
}
neos_lp_solver <- function() {
    union(c("BDMLP", "CPLEX", "FICO-Xpress", "MOSEK"), neos_milp_solver())
}

neos_miqcqp_solver <- function() {
    union(c("AlphaECP", "BARON", "Bonmin", "Couenne", "DICOPT", 
            "Knitro",  "LINDOGlobal", "SBB", "scip"), c("MOSEK", "CPLEX"))
}

neos_miqp_solver <- neos_miqcqp_solver

neos_qcqp_solver <- function() {
    union(c("CONOPT", "Ipopt", "Knitro", "MINOS", "MOSEK", "PATHNLP", "SNOPT"), 
          neos_miqcqp_solver())   
}

neos_qp_solver <- neos_qcqp_solver

neos_solver <- function() {
    unique(c(neos_lp_solver(), neos_milp_solver(), neos_qcqp_solver(), neos_miqcqp_solver()))
}

neos_solver_mapping <- function() {
    setNames(neos_solver(), clean(neos_solver()))
}

check_selected_solver <- function(method, model_type, is_mip) {
    if ( isTRUE(tolower(method) == "gurobi") ) {
        stop("The Gurobi license for NEOS does not permit connections via XML-RPC. ",
             "Therefore Gurobi is not available via ROI.plugin.neos.")
    }
    if ( is_mip ) {
        if ( model_type == "lp" ) {
            if ( !method %in% neos_milp_solver() ) {
                if ( method %in% neos_miqcqp_solver() ) {
                    warning("a MINLP solver is used for a MILP problem. ", 
                            "Most likely one of the following solver would be ",
                            "better suited for solving this problem: ", 
                            paste(shQuote(neos_milp_solver()), collapse = ", "))
                } else {
                    stop("solver ", shQuote(method), " not applicable. ",
                         "Most likely one of the following solver would be ",
                         "better suited for solving this problem: ", 
                         paste(shQuote(neos_milp_solver()), collapse = ", "))
                }
            }
        } else {
            if ( !method %in% neos_miqcqp_solver() ) {
                stop("solver ", shQuote(method), " not applicable. ",
                     "Most likely one of the following solver would be ",
                     "better suited for solving this problem: ", 
                     paste(shQuote(neos_miqcqp_solver()), collapse = ", "))                
            }
        }
    } else {
        if ( model_type == "lp" ) {
            if ( !method %in% neos_solver() ) {
                stop("solver ", shQuote(method), " not applicable. ",
                     "Most likely one of the following solver would be ",
                     "better suited for solving this problem: ", 
                     paste(shQuote(neos_lp_solver()), collapse = ", "))
            }
            if ( !method %in% neos_lp_solver() ) {
                warning("a MINLP solver is used for a MILP problem. ", 
                        "Most likely one of the following solver would be ",
                        "better suited for solving this problem: ", 
                        paste(shQuote(neos_lp_solver()), collapse = ", "))
            }
        } else {
            if ( !method %in% neos_qcqp_solver() ) {
                stop("solver ", shQuote(method), " not applicable. ",
                     "Most likely one of the following solver would be ",
                     "better suited for solving this problem: ", 
                     paste(shQuote(neos_qcqp_solver()), collapse = ", "))
            }
        }
    }
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
    x <- mapping[clean(solver)]
    if ( is.na(x) )
        stop("unknown solver: ", shQuote(solver))
    x
}
