roi_lp_to_gams <- function(x) {
    n_of_variables <- length(objective(x))
    n_of_constraints <- nrow(constraints(x))
    ## TODO: handle case no constraint
    stopifnot(length(objective(x)) > 0, nrow(constraints(x)) > 0)

    row_names <- sprintf("R%i", seq_len(nrow(constraints(x))))
    col_names <- sprintf("C%i", seq_len(length(objective(x))))

    ## Options
    Options <- "Option IntVarUp = 0;"

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

    j_binary <- which(types(x) == "B")
    j_integer <- which(types(x) == "I")
    j_continuous <- setdiff(seq_len(n_of_variables), union(j_binary, j_integer))

    ## Variables
    Variables <- paste(c("Variables obj;",
                         "Positive Variables x(j);",
                         if (length(j_binary)) "Binary Variables bin(jbin);" else NULL,
                         if (length(j_integer)) "Integer Variables int(jint);" else NULL,
                         ""),
                       collapse = "\n")

    ## Variable Bounds
    ## x.lo
    ## x.up
    ## x.lo('C2') = -inf;
    LoB <- build_lower_bounds(x, j_integer)
    UpB <- build_upper_bounds(x, j_integer)

    ## Equations
    Equations_declaration <- "Equations\n    ObjSum"

    ObjSum <- "ObjSum .. obj =e= sum(j, x(j) * objL(j)) ;"

    if ( has.eq(x) ) {
        Eq <- "LinEq(ieq) .. sum(j, A(ieq, j) * x(j)) =e= rhs(ieq) ;"
        Equations_declaration <- paste(Equations_declaration, "    LinEq(ieq)", sep = "\n")
    } else {
        Eq <- NULL
    }

    if ( has.leq(x) ) {
        Leq <- "LinLeq(ileq) .. sum(j, A(ileq, j) * x(j)) =l= rhs(ileq) ;"
        Equations_declaration <- paste(Equations_declaration, "    LinLeq(ileq)", sep = "\n")
    } else {
        Leq <- NULL
    }

    if ( has.geq(x) ) {
        Geq <- "LinGeq(igeq) .. sum(j, A(igeq, j) * x(j)) =g= rhs(igeq) ;"
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
    Model <- "Model LinearProblem /all/ ;\n"

    ## Solve
    Solve <- sprintf("Solve LinearProblem using %s %s obj ;\n", 
                     if ( length(j_binary) + length(j_integer) ) "MIP" else "LP",
                     if ( maximum(x) ) "maximizing" else "minimizing" )

    ## Display
    Display_options <- "option decimals = 8;\n" ## 8 is the maximum
    Display <- "display '---BEGIN.SOLUTION---', x.l, '---END.SOLUTION---';\n\n"


    model <- paste(c(Options, Sets, Parameters, Matrix, Variables, LoB, UpB,
                     "", Equations_declaration, ObjSum,
                     Eq, Leq, Geq, IntEq, BinEq, "", Model, Solve, 
                     Display_options, Display), collapse = "\n")
    model
}

