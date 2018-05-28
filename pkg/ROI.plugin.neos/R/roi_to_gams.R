
to_gams <- function(x) UseMethod("ROI_to_gams")

to_gams.OP <- function(x) {
    model_type <- which_model_type(x)
    to_gams <- switch(model_type, lp   = roi_lp_to_gams, 
                                  qp   = roi_qp_to_gams, 
                                  qcqp = roi_qcqp_to_gams)
    to_gams(x)
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

create_sets <- function(x) {
    if ( is.L_constraint(constraints(x)) ) {
        lin_leq  <- which(constraints(x)$dir %in% c("<=", "<"))
        lin_geq  <- which(constraints(x)$dir %in% c(">=", ">"))
        lin_eq   <- which(constraints(x)$dir == "==")
        quad_leq <- quad_geq <- quad_eq <- integer(0)
    } else {
        b <- unlist(lapply(constraints(x)$Q, is.slam_zero_matrix), 
                    recursive = FALSE, use.names = FALSE)
        
        lin_leq  <- which( (constraints(x)$dir %in% c("<=", "<")) & b )
        lin_geq  <- which( (constraints(x)$dir %in% c(">=", ">")) & b )
        lin_eq   <- which( (constraints(x)$dir == "==") & b )
        quad_leq <- which( (constraints(x)$dir %in% c("<=", "<")) & !b )
        quad_geq <- which( (constraints(x)$dir %in% c(">=", ">")) & !b )
        quad_eq  <- which( (constraints(x)$dir == "==") & !b )
    }    

    j_int <- which(types(x) == "I")
    j_bin <- which(types(x) == "B")
    j_con <- setdiff(seq_len(length(objective(x))), union(j_bin, j_int))
    
    row_index <- function(i, name) {
        if ( length(i) == 0L ) return(NULL)
        sprintf("Set %s(i) / %s / ;", name, paste(sprintf("R%i", i), collapse = ", "))
    }

    col_index <- function(j, name) {
        if ( length(j) == 0L ) return(NULL)
        sprintf("Set %s(j) / %s / ;", name, paste(sprintf("C%i", j), collapse = ", "))
    }

    paste(c("", sprintf("Set i / R1*R%i / ;", nrow(constraints(x))),
            row_index(lin_leq, "ileq"), row_index(lin_geq, "igeq"), row_index(lin_eq, "ieq"),
            row_index(quad_leq, "kleq"), row_index(quad_geq, "kgeq"), row_index(quad_eq, "keq"),
            sprintf("Set j / C1*C%i / ;", length(objective(x))),
            col_index(j_int, "jint"), col_index(j_bin, "jbin"), ""), collapse = "\n")
}

build_lower_bounds <- function(x, j_integer) {
    if ( !length(bounds(x)$lower$ind) )
        return(NULL)

    lowb <- function(varname, i, rhs) {
        sprintf("%s.lo('C%i') = %s;", varname, i, as.character(rhs))
    }
    LoB <- lowb("x", bounds(x)$lower$ind, bounds(x)$lower$val)
    if ( length(j_integer) ) {
        i <- which(j_integer %in% bounds(x)$lower$ind)
        if ( length(i) ) {
            LoB <- c(LoB,
                     lowb("int", bounds(x)$lower$ind[i], bounds(x)$lower$val[i]))
        }
    }
    paste(LoB, collapse = "\n")
}

build_upper_bounds <- function(x, j_integer) {
    if ( !length(bounds(x)$upper$ind) )
        return(NULL)

    uppb <- function(varname, i, rhs) {
        sprintf("%s.up('C%i') = %s;", varname, i, as.character(rhs))   
    }
    UpB <- uppb("x", bounds(x)$upper$ind, bounds(x)$upper$val)
    if ( length(j_integer) ) {
        i <- which(j_integer %in% bounds(x)$upper$ind)
        if ( length(i) ) {
            UpB <- c(UpB,
                     uppb("int", bounds(x)$upper$ind[i], bounds(x)$upper$val[i]))
        }                 
    }
    paste(UpB, collapse = "\n")
}



