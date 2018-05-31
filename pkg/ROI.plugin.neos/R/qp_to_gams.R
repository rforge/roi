roi_qp_to_gams <- function(x) {
    n_of_variables <- length(objective(x))
    n_of_constraints <- nrow(constraints(x))
    
    stopifnot(length(objective(x)) > 0)

    signature <- unlist(OP_signature(x))

    row_names <- sprintf("R%i", seq_len(nrow(constraints(x))))
    col_names <- sprintf("C%i", seq_len(length(objective(x))))

    ## Options
    Options <- "Option IntVarUp = 0;"

    ## Sets
    Sets  <- create_sets(x)
    Alias <- "alias (j, jj);"

    ## Parameters
    ## - Objective
    if ( is.slam_zero_matrix(terms(objective(x))$L) ) {
        objL <- NULL
    } else {
        objL <- create_sparse_vector(terms(objective(x))$L, "objL")
    }
    if ( is.slam_zero_matrix(terms(objective(x))$Q) ) {
        objQ <- NULL
    } else {
        objQ <- create_sparse_matrix(terms(objective(x))$Q, "objQ(j, jj)", "C", "C")
    }
    
    ## - Constraints
    if ( n_of_constraints > 0 ) {
        rhs <- create_parameter_vector(constraints(x)$rhs, "rhs", "i", row_names)    
    } else {
        rhs <- NULL
    }    

    if ( nrow(constraints(x)) ) {
        constrL <- create_sparse_matrix(constraints(x)$L, "constrL(i, j)")
    } else {
        constrL <- NULL
    }

    j_binary <- which(types(x) == "B")
    j_integer <- which(types(x) == "I")
    j_continuous <- setdiff(seq_len(n_of_variables), union(j_binary, j_integer))

    ## Variables
    ## NOTE: We define the variables as positive to get the ROI
    ##       default bounds but we can alter the bounds later anyways!
    Variables <- paste(c("Variables obj;", 
                         "Positive Variables x(j);",
                         if (length(j_binary)) "Binary Variables bin(jbin);" else NULL,
                         if (length(j_integer)) "Integer Variables int(jint);" else NULL),
                       collapse = "\n")

    ## Variable Bounds
    ## x.lo
    ## x.up
    ## x.lo('C2') = -inf;
    LoB <- build_lower_bounds(x, j_integer)
    UpB <- build_upper_bounds(x, j_integer)

    ## Equations
    Equations_declaration <- "Equations\n    ObjSum"

    if ( is.null(terms(objective(x))$Q) ) {
        ObjSum <- "ObjSum .. obj =e= sum(j, x(j) * objL(j)) ;"
    } else if ( is.slam_zero_matrix(terms(objective(x))$L) ) {
        ObjSum <- "ObjSum .. obj =e= 0.5  * sum(j, x(j) * sum(jj, objQ(j, jj) * x(jj)) ) ;"
    } else {
        ObjSum <- "ObjSum .. obj =e= 0.5  * sum(j, x(j) * sum(jj, objQ(j, jj) * x(jj)) )  + sum(j, x(j) * objL(j)) ;"
    }


    if ( has.eq(x) ) {
        Eq <- "LinEq(ieq) .. sum(j, constrL(ieq, j) * x(j)) =e= rhs(ieq) ;\n"
        Equations_declaration <- paste(Equations_declaration, "    LinEq(ieq)", sep = "\n")
    } else {
        Eq <- NULL
    }

    if ( has.leq(x) ) {
        Leq <- "LinLeq(ileq) .. sum(j, constrL(ileq, j) * x(j)) =l= rhs(ileq) ;\n"
        Equations_declaration <- paste(Equations_declaration, "    LinLeq(ileq)", sep = "\n")
    } else {
        Leq <- NULL
    }

    if ( has.geq(x) ) {
        Geq <- "LinGeq(igeq) .. sum(j, constrL(igeq, j) * x(j)) =g= rhs(igeq) ;\n"
        Equations_declaration <- paste(Equations_declaration, "    LinGeq(igeq)", sep = "\n")
    } else {
        Geq <- NULL
    }

    if ( length(j_integer) ) {
        IntEq <- "IntEq(jint) .. x(jint) =e= int(jint);"
        Equations_declaration <- paste(Equations_declaration, "    IntEq(jint)", sep = "\n")
    } else {
        IntEq <- NULL
    }

    if ( length(j_binary) ) {
        BinEq <- "BinEq(jbin) .. x(jbin) =e= bin(jbin);"
        Equations_declaration <- paste(Equations_declaration, "    BinEq(jbin)", sep = "\n")
    } else {
        BinEq <- NULL
    }

    Equations_declaration <- sprintf("%s;\n", Equations_declaration)


    ## Model
    Model <- "Model QuadraticProblem /all/ ;\n"

    ## Solve
    model_type <- if ( any(types(x) %in% c("B", "I")) ) "MIQCP" else "QCP"
    Solve <- sprintf("Solve QuadraticProblem using %s %s obj ;\n", model_type,
                     if ( maximum(x) ) "maximizing" else "minimizing" )

    ## Display
    Display_options <- "option decimals = 8;\n" ## 8 is the maximum
    Display <- "display '---BEGIN.SOLUTION---', x.l, '---END.SOLUTION---';\n\n"

    Export_results <- c("file results /results.txt/;", 
        "results.nw = 0;", ## numeric field lenght, 0 means as much as needed
        "results.nd = 15;", 
        "results.nr = 2;", ## display in scientific notation
        "results.nz = 0;", ## don't round for display reasons
        "put results;",
        "put 'solution:'/;", "loop(j, put, x.l(j)/);",
        "put 'objval:'/;", "put QuadraticProblem.objval/;",
        "put 'solver_status:'/;", "put QuadraticProblem.solvestat/;",
        "put 'model_status:'/;", "put QuadraticProblem.modelstat/;")

    model <- paste(c(Options, Sets, Alias, "", objL, objQ, constrL, rhs,
                     Variables, LoB, UpB, "", Equations_declaration, ObjSum,
                     Eq, Leq, Geq, IntEq, BinEq, Model, Solve, 
                     Display_options, Display, Export_results), collapse = "\n")
    model
}
