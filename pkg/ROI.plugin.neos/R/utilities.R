
new_xml_node <- function(name, data) {
    node <- xml_new_root(name)
    xml_add_child(node, xml_cdata(data))
    xml_child(xml_parent(node))
}

xml_copy <- function(x) {
    read_xml(as.character(x))
}

##
## R CMD check doesn't like this version.
##
## xml_copy_2 <- function(x) {
##     write_con <- textConnection("deep_copy", open = "w", local = TRUE)
##     xml_serialize(x, write_con)
##     close(write_con)
##     read_con <- textConnection(deep_copy)
##     x_copy <- xml_unserialize(read_con)
##     close(read_con)
##     x_copy
## }

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

has_valid_objective <- function(x) inherits(objective(x), "Q_objective")

has_valid_constraints <- function(x) inherits(constraints(x), c("Q_constraint", "NO_constraint"))

which_model_type <- function(x) {
    if ( !has_valid_objective(x) | !has_valid_constraints(x) ) {
        stop("'ROI_to_gams' only supports linear and quadratic objectives and ",
             "linear and quadratic constraints!", call. = FALSE)
    }
    if ( is_lp(x) ) {
        model_type <- "lp"
    } else if ( is.L_constraint(constraints(x)) ) {
        model_type <- "qp"
    } else {
        model_type <- "qcqp"
    }
    model_type
}

is_lp <- function(x) {
    ( inherits(objective(x), "L_objective") 
    & inherits(constraints(x), c("L_constraint", "NO_constraint")) )
}

is.slam_zero_matrix <- function(x) {
    if ( is.null(x) ) return(TRUE)
    inherits(x, "simple_triplet_matrix") & isTRUE(length(x$v) == 0L)
}

clean <- function(x) {
    tolower(gsub("\\W", "", x))
}

strip <- function(x) gsub("(^\\s+|\\s+$)", "", x)

prefix_spaces <- function(x, xnchar) {
    paste(c(rep.int(" ", xnchar - nchar(x)), x), collapse = "")
}

subfix_spaces <- function(x, xnchar) {
    paste(c(x, rep.int(" ", xnchar - nchar(x))), collapse = "")
}

## get_lb
## ======
##
## get lower bound constraints
get_lb <- function(x) {
    lb <- numeric( length(objective(x)) )
    lb[ bounds(x)$lower$ind ] <- bounds(x)$lower$val
    return(lb)
}

## get_ub
## ======
##
## get upper bound constraints
get_ub <- function(x) {
    ub <- rep.int(Inf, length(objective(x)))
    ub[ bounds(x)$upper$ind ] <- bounds(x)$upper$val
    return(ub)
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
