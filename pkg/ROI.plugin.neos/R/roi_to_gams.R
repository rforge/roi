roi_lp_to_gams <- function(x) {
    ## TODO: handle case no constraint
    stopifnot(length(objective(x)) > 0, nrow(constraints(x)) > 0)

    row_names <- sprintf("R%i", seq_len(nrow(constraints(x))))
    col_names <- sprintf("C%i", seq_len(length(objective(x))))

    ## Sets
    Sets  <- create_sets(x)

    ## Parameters
    Parameters <- paste(create_parameter_vector(terms(objective(x))$L, "objL", "j", col_names),
                        create_parameter_vector(constraints(x)$rhs, "rhs", "i", row_names), sep = "\n")

    ## Matrices
    if ( nrow(constraints(x)) ) {
        ## Tables <- create_table(constraints(x)$L, "A", row_names, col_names)
        Matrix <- create_sparse_matrix(constraints(x)$L, "A")
    } else {
        Matrix <- ""
    }

    ## Variables
    Variables <- "Variables\n    obj;\nPositive Variables\n    x(j);\n"

    ## Variable Bounds
    ## x.lo
    ## x.up
    ## x.lo('C2') = -inf;
    if ( length(bounds(x)$lower$ind) ) {
        val <- as.character(bounds(x)$lower$val)
        LoB <- paste(sprintf("x.lo('C%i') = %s;\n", bounds(x)$lower$ind, val), collapse = "")
    } else {
        LoB <- NULL
    }
    
    if ( length(bounds(x)$upper$ind) ) {
        val <- as.character(bounds(x)$upper$val)
        UpB <- paste(sprintf("x.up('C%i') = %s;\n", bounds(x)$upper$ind, val), collapse = "")
    } else {
        UpB <- NULL
    }

    ## Equations
    Equations_declaration <- "Equations\n    ObjSum"

    ObjSum <- "ObjSum .. obj =e= sum(j, x(j) * objL(j)) ;"

    if ( has.eq(x) ) {
        Eq <- "LinEq(ieq) .. sum(j, A(ieq, j) * x(j)) =e= rhs(ieq) ;\n"
        Equations_declaration <- paste(Equations_declaration, "    LinEq(ieq)", sep = "\n")
    } else {
        Eq <- NULL
    }

    if ( has.leq(x) ) {
        Leq <- "LinLeq(ileq) .. sum(j, A(ileq, j) * x(j)) =l= rhs(ileq) ;\n"
        Equations_declaration <- paste(Equations_declaration, "    LinLeq(ileq)", sep = "\n")
    } else {
        Leq <- NULL
    }

    if ( has.geq(x) ) {
        Geq <- "LinGeq(igeq) .. sum(j, A(igeq, j) * x(j)) =g= rhs(igeq) ;\n"
        Equations_declaration <- paste(Equations_declaration, "    LinGeq(igeq)", sep = "\n")
    } else {
        Geq <- NULL
    }

    Equations_declaration <- sprintf("%s;\n", Equations_declaration)


    ## Model
    Model <- "Model LinearProblem /all/ ;\n"

    ## Solve
    Solve <- sprintf("Solve LinearProblem using LP %s obj ;\n", 
                     if ( maximum(x) ) "maximizing" else "minimizing" )

    ## Display
    Display_options <- "option decimals = 8;" ## 8 is the maximum
    Display <- "display '---BEGIN.SOLUTION---', x.l, '---END.SOLUTION---';\n"


    model <- paste(c(Sets, Parameters, Matrix, Variables, LoB, UpB,
                     Equations_declaration, ObjSum,
                     Eq, Leq, Geq, Model, Solve, 
                     Display_options, Display), collapse = "\n")
    model
}









roi_qp_to_gams <- function(x) {
    ## TODO: handle case no constraint
    stopifnot(length(objective(x)) > 0, nrow(constraints(x)) > 0)

    signature <- unlist(OP_signature(x))

    row_names <- sprintf("R%i", seq_len(nrow(constraints(x))))
    col_names <- sprintf("C%i", seq_len(length(objective(x))))

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
    rhs <- create_parameter_vector(constraints(x)$rhs, "rhs", "i", row_names)

    if ( nrow(constraints(x)) ) {
        constrL <- create_sparse_matrix(constraints(x)$L, "constrL(i, j)")
    } else {
        constrL <- NULL
    }

    ## Variables
    ## NOTE: We define the variables as positive to get the ROI
    ##       default bounds but we can alter the bounds later anyways!
    Variables <- "Variables\n    obj;\nPositive Variables\n    x(j);\n"

    ## Variable Bounds
    ## x.lo
    ## x.up
    ## x.lo('C2') = -inf;
    if ( length(bounds(x)$lower$ind) ) {
        val <- as.character(bounds(x)$lower$val)
        LoB <- paste(sprintf("x.lo('C%i') = %s;\n", bounds(x)$lower$ind, val), collapse = "")
    } else {
        LoB <- NULL
    }
    
    if ( length(bounds(x)$upper$ind) ) {
        val <- as.character(bounds(x)$upper$val)
        UpB <- paste(sprintf("x.up('C%i') = %s;\n", bounds(x)$upper$ind, val), collapse = "")
    } else {
        UpB <- NULL
    }

    ## Equations
    Equations_declaration <- "Equations\n    ObjSum"

    if ( is.null(terms(objective(x))$Q) ) {
        ObjSum <- "ObjSum .. obj =e= sum(j, x(j) * objL(j)) ;"
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

    Equations_declaration <- sprintf("%s;\n", Equations_declaration)


    ## Model
    Model <- "Model QuadraticProblem /all/ ;\n"

    ## Solve
    model_type <- if ( any(types(x) %in% c("B", "I")) ) "MIQCP" else "QCP"
    Solve <- sprintf("Solve QuadraticProblem using %s %s obj ;\n", model_type,
                     if ( maximum(x) ) "maximizing" else "minimizing" )

    ## Display
    Display_options <- "option decimals = 8;" ## 8 is the maximum
    Display <- "display '---BEGIN.SOLUTION---', x.l, '---END.SOLUTION---';\n"


    model <- paste(c(Sets, Alias, objL, objQ, constrL, rhs,
                     Variables, LoB, UpB, Equations_declaration, ObjSum,
                     Eq, Leq, Geq, Model, Solve, 
                     Display_options, Display), collapse = "\n")
    model
}










roi_qcqp_to_gams <- function(x) {
    ## TODO: handle case no constraint
    stopifnot(length(objective(x)) > 0, nrow(constraints(x)) > 0)

    signature <- unlist(OP_signature(x))

    row_names <- sprintf("R%i", seq_len(nrow(constraints(x))))
    col_names <- sprintf("C%i", seq_len(length(objective(x))))

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
    rhs <- create_parameter_vector(constraints(x)$rhs, "rhs", "i", row_names)

    if ( nrow(constraints(x)) ) {
        if ( is.slam_zero_matrix(constraints(x)$L) ) {
            constrL <- NULL
        } else {
            constrL <- create_sparse_matrix(constraints(x)$L, "constrL(i, j)")
        }
        if ( is.L_constraint(constraints(x)) ) {
            constrQ <- NULL
        } else {
            constrQ <- create_sparse_array_from_Q_constraint(constraints(x)$Q, "constrQ(i, j, jj)", "R", "C")
        }
    } else {
        constrL <- NULL
        constrQ <- NULL
    }

    ## Variables
    ## NOTE: We define the variables as positive to get the ROI
    ##       default bounds but we can alter the bounds later anyways!
    Variables <- "Variables\n    obj;\nPositive Variables\n    x(j);\n"

    ## Variable Bounds
    ## x.lo
    ## x.up
    ## x.lo('C2') = -inf;
    if ( length(bounds(x)$lower$ind) ) {
        val <- as.character(bounds(x)$lower$val)
        LoB <- paste(sprintf("x.lo('C%i') = %s;\n", bounds(x)$lower$ind, val), collapse = "")
    } else {
        LoB <- NULL
    }
    
    if ( length(bounds(x)$upper$ind) ) {
        val <- as.character(bounds(x)$upper$val)
        UpB <- paste(sprintf("x.up('C%i') = %s;\n", bounds(x)$upper$ind, val), collapse = "")
    } else {
        UpB <- NULL
    }

    ## Equations
    Equations_declaration <- "Equations\n    ObjSum"

    if ( is.null(terms(objective(x))$Q) ) {
        ObjSum <- "ObjSum .. obj =e= sum(j, x(j) * objL(j)) ;"
    } else if ( is.slam_zero_matrix(terms(objective(x))$L) ) {
        ObjSum <- "ObjSum .. obj =e= 0.5  * sum(j, x(j) * sum(jj, objQ(j, jj) * x(jj)) ) ;"
    } else {
        ObjSum <- "ObjSum .. obj =e= 0.5  * sum(j, x(j) * sum(jj, objQ(j, jj) * x(jj)) )  + sum(j, x(j) * objL(j)) ;"
    }

    is_quad <- !unlist(lapply(constraints(x)$Q, is.slam_zero_matrix), 
                       recursive = FALSE, use.names = FALSE)

    is_eq <- constraints(x)$dir == "=="
    if ( any( (!is_quad) & is_eq ) & !is.slam_zero_matrix(constraints(x)$L) ) {
        EqL <- "LinEq(ieq) .. sum(j, constrL(ieq, j) * x(j)) =e= rhs(ieq) ;\n"
        Equations_declaration <- paste(Equations_declaration, "    LinEq(ieq)", sep = "\n")
    } else {
        EqL <- NULL
    }
    if ( any( is_quad & is_eq ) ) {
        if ( is.slam_zero_matrix(constraints(x)$L) ) {
            EqQ <- "QuadEq(keq) .. 0.5  * sum(j, x(j) * sum(jj, constrQ(keq, j, jj) * x(jj)) ) =e= rhs(keq) ;"
        } else {
            EqQ <- "QuadEq(keq) .. 0.5  * sum(j, x(j) * sum(jj, constrQ(keq, j, jj) * x(jj)) )  + sum(j, x(j) * constrL(keq, j)) =e= rhs(keq) ;"
        }
        Equations_declaration <- paste(Equations_declaration, "    QuadEq(keq)", sep = "\n")
    } else {
        EqQ <- NULL
    }      

    is_leq <- constraints(x)$dir %in% c("<", "<=")
    if ( any( (!is_quad) & is_leq ) ) {
        LeqL <- "LinLeq(ileq) .. sum(j, constrL(ileq, j) * x(j)) =l= rhs(ileq) ;\n"
        Equations_declaration <- paste(Equations_declaration, "    LinLeq(ileq)", sep = "\n")
    } else {
        LeqL <- NULL
    }
    if ( any( is_quad & is_leq ) ) {
        if ( is.slam_zero_matrix(constraints(x)$L) ) {
            LeqQ <- "QuadLeq(kleq) .. 0.5  * sum(j, x(j) * sum(jj, constrQ(kleq, j, jj) * x(jj)) ) =l= rhs(kleq) ;"
        } else {
            LeqQ <- "QuadLeq(kleq) .. 0.5  * sum(j, x(j) * sum(jj, constrQ(kleq, j, jj) * x(jj)) )  + sum(j, x(j) * constrL(kleq, j)) =l= rhs(kleq) ;"
        }
        Equations_declaration <- paste(Equations_declaration, "    QuadLeq(kleq)", sep = "\n")
    } else {
        LeqQ <- NULL
    }

    is_geq <- constraints(x)$dir %in% c(">", ">=")
    if ( any( (!is_quad) & is_geq ) ) {
        GeqL <- "LinGeq(igeq) .. sum(j, constrL(igeq, j) * x(j)) =g= rhs(igeq) ;\n"
        Equations_declaration <- paste(Equations_declaration, "    LinGeq(igeq)", sep = "\n")
    } else {
        GeqL <- NULL
    }
    if ( any( is_quad & is_geq ) ) {
        if ( is.slam_zero_matrix(constraints(x)$L) ) {
            GeqQ <- "QuadGeq(kgeq) .. 0.5  * sum(j, x(j) * sum(jj, constrQ(kgeq, j, jj) * x(jj)) ) =g= rhs(kgeq) ;"            
        } else {
            GeqQ <- "QuadGeq(kgeq) .. 0.5  * sum(j, x(j) * sum(jj, constrQ(kgeq, j, jj) * x(jj)) )  + sum(j, x(j) * constrL(kgeq, j)) =g= rhs(kgeq) ;"
        }
        Equations_declaration <- paste(Equations_declaration, "    QuadGeq(kgeq)", sep = "\n")
    } else {
        GeqQ <- NULL
    }        


    Equations_declaration <- sprintf("%s;\n", Equations_declaration)


    ## Model
    Model <- "Model LinearProblem /all/ ;\n"

    ## Solve
    model_type <- if ( any(types(x) %in% c("B", "I")) ) "MIQCP" else "QCP"
    Solve <- sprintf("Solve LinearProblem using %s %s obj ;\n", model_type,
                     if ( maximum(x) ) "maximizing" else "minimizing" )

    ## Display
    Display_options <- "option decimals = 8;" ## 8 is the maximum
    Display <- "display '---BEGIN.SOLUTION---', x.l, '---END.SOLUTION---';\n"


    model <- paste(c(Sets, Alias, objL, objQ, constrL, constrQ, rhs,
                     Variables, LoB, UpB, Equations_declaration, ObjSum,
                     EqL, EqQ, LeqL, LeqQ, GeqL, GeqQ, Model, Solve, 
                     Display_options, Display), collapse = "\n")
    model
}

