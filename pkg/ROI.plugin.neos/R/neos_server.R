
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