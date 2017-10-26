
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



