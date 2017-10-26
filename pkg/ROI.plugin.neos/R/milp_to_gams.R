##
## I need to also consider integer and binary variables unfortunately in GAMS
## it is not possible to set them to integer but we have to define integer
## or binary variables.
##
## - Binary Variables    | y
## - Integer Variables   | z
## - Postitive Variables | x
roi_milp_to_gams <- function(x) {
    ## TODO: handle case no constraint
    n_of_variables <- length(objective(x))
    n_of_constraints <- nrow(constraints(x))
    stopifnot(n_of_variables > 0, n_of_constraints > 0)

    row_names <- sprintf("R%i", seq_len(n_of_constraints))
    col_names <- sprintf("C%i", seq_len(n_of_constraints))

    ## Sets
    Sets  <- create_sets(x)

    ## Parameters
    Parameters <- paste(create_parameter_vector(terms(objective(x))$L, "objL", "j", col_names),
                        create_parameter_vector(constraints(x)$rhs, "rhs", "i", row_names), sep = "\n")

    ## Matrices
    if ( nrow(constraints(x)) ) {
        Matrix <- create_sparse_matrix(constraints(x)$L, "A")
    } else {
        Matrix <- ""
    }

    j_binary <- which(types(x) == "B")
    j_integer <- which(types(x) == "I")
    j_continuous <- setdiff(seq_len(n_of_variables), union(j_binary, j_integer))
    
    ## Variables
    Variables <- paste(c("Variables  obj;", 
                         if (length(j_continuous)) "Positive Variables  x(jcon);" else NULL,
                         if (length(j_binary)) "Binary Variables  y(jbin);" else NULL,
                         if (length(j_integer)) "Integer Variables  z(jint);" else NULL), 
                       collapse = "\n")

    ## Variable Bounds
    ## x.lo
    ## x.up
    ## x.lo('C2') = -inf;
    LoB <- NULL
    ## FIXME: Des is falsch da integer Werte nicht be 0 beschraenkt sind!
    if ( length(bounds(x)$lower$ind) ) {
        val <- bounds(x)$lower$val
        i <- which(bounds(x)$lower$ind %in% j_integer)
        val[i][is.infinite(val[i])] <- -.Machine$integer.max
        val <- as.character(val)
        if ( length(j_integer) ) {
            i <- match(j_integer, bounds(x)$lower$ind)
            j_integer_val <- val[i]
            j_integer_val[is.na(j_integer_val)] <- "0"
            LoB <- sprintf("z.lo('C%i') = %s;\n", j_integer, j_integer_val)
        }
        if ( length(j_continuous) ) {
            i <- match(j_continuous, bounds(x)$lower$ind)
            j_continuous_val <- val[i]
            b <- !is.na(j_continuous_val)
            LoB <- paste(c(LoB, 
                           sprintf("x.lo('C%i') = %s;\n", 
                                   j_continuous[b], j_continuous_val[b])), 
                         collapse = "")
        }
    }
    
    UpB <- NULL
    if ( length(bounds(x)$upper$ind) ) {
        val <- as.character(bounds(x)$upper$val)
        if ( length(j_integer) ) {
            i <- match(j_integer, bounds(x)$upper$ind)
            j_integer_val <- val[i]
            ## NOTE: Gams sets the default upper bound of integer variables to 100
            ##       and gives an error when one tries to use Inf so 
            ##       we use now 2147483647
            j_integer_val[is.na(j_integer_val)] <- as.character(.Machine$integer.max)
            UpB <- sprintf("z.up('C%i') = %s;\n", j_integer, j_integer_val)
        }
        if ( length(j_continuous) ) {
            i <- match(j_continuous, bounds(x)$upper$ind)
            j_continuous_val <- val[i]
            b <- !is.na(j_continuous_val)
            UpB <- paste(c(UpB, 
                           sprintf("x.up('C%i') = %s;\n", 
                                   j_continuous[b], j_continuous_val[b])), 
                         collapse = "")
        }
    }

    ## Equations
    Equations_declaration <- "Equations\n    ObjSum"

    objfun <- function(b, var, index) {
        if (b) sprintf("sum(%2$s, %1$s(%2$s) * objL(%2$s))", var, index) else NULL
    }
    ObjRhs <- paste(c(objfun(length(j_binary), "y", "jbin"),
                      objfun(length(j_integer), "z", "jint"),
                      objfun(length(j_continuous), "x", "jcon")), collapse = " + ")
    ObjSum <- sprintf("ObjSum .. obj =e= %s;",  ObjRhs)                     

    if ( has.eq(x) ) {
        eqfun <- function(b, var, index) {
            if (b) sprintf("sum(%2$s, A(ieq, %2$s) * %1$s(%2$s))", var, index) else NULL
        }
        EqLhs <- paste(c(eqfun(length(j_binary), "y", "jbin"),
                         eqfun(length(j_integer), "z", "jint"),
                         eqfun(length(j_continuous), "x", "jcon")), collapse = " + ")
        Eq <- sprintf("LinEq(ieq) .. %s =e= rhs(ieq) ;\n", EqLhs)
        Equations_declaration <- paste(Equations_declaration, "    LinEq(ieq)", sep = "\n")
    } else {
        Eq <- NULL
    }

    if ( has.leq(x) ) {
        leqfun <- function(b, var, index) {
            if (b) sprintf("sum(%2$s, A(ileq, %2$s) * %1$s(%2$s))", var, index) else NULL
        }
        LeqLhs <- paste(c(leqfun(length(j_binary), "y", "jbin"),
                          leqfun(length(j_integer), "z", "jint"),
                          leqfun(length(j_continuous), "x", "jcon")), collapse = " + ")
        Leq <- sprintf("LinLeq(ileq) .. %s =l= rhs(ileq) ;\n", LeqLhs)
        Equations_declaration <- paste(Equations_declaration, "    LinLeq(ileq)", sep = "\n")
    } else {
        Leq <- NULL
    }

    if ( has.geq(x) ) {
        geqgun <- function(b, var, index) {
            if (b) sprintf("sum(%2$s, A(igeq, %2$s) * %1$s(%2$s)", var, index) else NULL
        }
        GeqLhs <- paste(c(geqgun(length(j_binary), "y", "jbin"),
                          geqgun(length(j_integer), "z", "jint"),
                          geqgun(length(j_continuous), "x", "jcon")), collapse = " + ")
        Geq <- sprintf("LinGeq(igeq) .. %s =g= rhs(igeq) ;\n", GeqLhs)
        Equations_declaration <- paste(Equations_declaration, "    LinGeq(igeq)", sep = "\n")
    } else {
        Geq <- NULL
    }

    Equations_declaration <- sprintf("%s;\n", Equations_declaration)

    ## Model
    Model <- "Model LinearProblem /all/ ;\n"

    ## Solve
    Solve <- sprintf("Solve LinearProblem using %s %s obj ;\n", 
                     if ( length(j_binary) + length(j_integer) ) "MIP" else "LP",
                     if ( maximum(x) ) "maximizing" else "minimizing" )

    ## Display
    Display_options <- "option decimals = 8;" ## 8 is the maximum
    solfun <- function(b, var, txt) {
        fmt <- "display '---BEGIN.%2$s.SOLUTION---', %1$s.l, '---END.%2$s.SOLUTION---';\n"
        if (b) sprintf(fmt, var, txt) else NULL
    }
    Display <- paste(c(solfun(length(j_binary), "y", "B"),
                       solfun(length(j_integer), "z", "I"),
                       solfun(length(j_continuous), "x", "C")), collapse = "\n")


    model <- paste(c(Sets, Parameters, Matrix, Variables, LoB, UpB,
                     Equations_declaration, ObjSum,
                     Eq, Leq, Geq, Model, Solve, 
                     Display_options, Display), collapse = "\n")
    model
}

