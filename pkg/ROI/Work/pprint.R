
library(ROI)

rqp <- function(n, m) {
    A <- round(10 * matrix(runif(m * n), m, n))
    b <- rowSums(A)
    Q <- matrix(rnorm(n * n), n)
    Q <- t(Q) %*% Q ## psd matrix
    Q <- t(Q) + Q   ## symmetric psd matrix
    OP(objective = Q_objective(Q = Q, L = runif(n)),
       constraint = L_constraint(L = A, dir = eq(m), rhs = b), 
       bounds = V_bound(ui = seq_len(n), ub = rep.int(4 * max(b), n)))
}


z <- rqp(4, 6)

pprint <- function(x, digits) {
    UseMethod("pprint")
}

str(x)

pprint.OP <- function(x, digits = getOption("digits")) {
    sense <- if (x$maximum) "maximize" else "minimize"
    cat("\n    ", sense, "_x  ", sep = "")
    pprint(objective(x))
    pprint(constraints(x))
    if ( is.null(bounds(x)) ) {
        cat("variable bounds:\n")
        cat("     0 <= x[i] <= Inf\n") 
    } else {
        pprint(bounds(x))    
    }
    invisible(NULL)
}

x <- z
pprint.OP(x)
names(x)

pprint.L_objective <- function(x) {
    cat("L %*% x \n\n")
    obj.L <- capture.output(as.vector(terms(x)$L))
    obj.L <- gsub(".*\\]\\s{0,1}", "", obj.L)
    cat("objective:\n")
    cat(" - L:\n")
    cat(sprintf("     %s", obj.L), sep="\n")
    invisible(NULL)
}

pprint.Q_objective <- function(x) {
    cat("1 / 2 * t(x) %*% Q %*% x + L %*% x \n\n")
    obj.Q <- capture.output(as.matrix(terms(x)$Q))
    obj.Q <- gsub(".*\\]\\s{0,1}", "", obj.Q[-1L])
    obj.L <- capture.output(as.vector(terms(x)$L))
    obj.L <- gsub(".*\\]\\s{0,1}", "", obj.L)
    cat("objective:\n")
    cat(" - Q:\n")
    cat(sprintf("     %s", obj.Q), sep="\n")
    cat(" - L:\n")
    cat(sprintf("     %s", obj.L), sep="\n")
    invisible(NULL)
}

pprint.L_constraint <- function(x) {
    con.L <- capture.output(as.matrix(x$L))
    con.L <- gsub(".*\\]\\s{0,1}", "", con.L[-1L])
    rhs <- as.vector(x$rhs)
    rhs <- gsub(".*\\]\\s{0,1}", "", rhs)
    cat("constraints:\n")
    cat(" - L: \n")
    cat(sprintf("     %s  %s  %s", con.L, x$dir, rhs), sep="\n")
    invisible(NULL)
}

pprint(constraints(x))

vec2char <- function(x) {
    a <- as.character(x)
    mc <- max(nchar(a))
    fill <- function(s, n) {
        space <- paste(rep.int(" ", n), collapse="")
        paste(space, s, sep="")
    }
    mapply(fill, a, mc - nchar(a), USE.NAMES =  FALSE)
}

nspaces <- function(n) paste(rep.int(" ", n), collapse = "")

pprint.V_bound <- function(x) {    
    li <- x$lower$ind
    ui <- x$upper$ind    
    i <- union(li, ui)
    lb <- rep.int(0, length(i))
    ub <- rep.int(Inf, length(i))
    lb[li] <- x$lower$val
    ub[ui] <- x$upper$val
    cat("variable bounds:\n")
    cat(sprintf("     %s <= x[%i] <= %s", 
                vec2char(lb), i, vec2char(ub)), sep="\n")
    invisible(NULL)
}

pprint_types <- function(x) {
    ty <- types(x)

}

x <- z
pprint.OP(x)


x <- objective(z)
pprint.Q_objective(x)
x$types

x <- constraints(z)
pprint.L_constraint(x)

x <- bounds(z)
pprint.V_bounds(x)

##
## LP
##
mat <- matrix(c(3, 4, 2,
                2, 1, 2,
                1, 3, 2), nrow=3, byrow=TRUE)
x <- OP(objective = c(2, 4, 3),
        constraints = L_constraint(L = mat,
                                   dir = c("<=", "<=", "<="),
                                   rhs = c(60, 40, 80)),
        maximum = TRUE)

pprint(x)

##
## QP
##
A <- cbind(c(-4, -3, 0), 
           c( 2,  1, 0), 
           c( 0, -2, 1))
x <- OP(Q_objective(diag(3), L =  c(0, -5, 0)),
        L_constraint(L = t(A),
                     dir = rep(">=", 3),
                     rhs = c(-8, 2, 0)))
pprint(x)


x1, x2, x3 %*% 

##
## SOCP
##
obj <- c(1, 1, 1)
A <- rbind(c(1, 0, 0))
b <- c(sqrt(2))
G <- diag(x=-1, 3)
h <- rep(0, 3)

bound <- V_bound(li = 1:3, lb = rep(-Inf, 3))

lc <- C_constraint(L = rbind(A, G), 
                   cones = c(K_zero(1), K_soc(3)), 
                   rhs = c(b, h))
x <- OP(objective = obj, constraints = lc, types = rep("C", 3),
        bounds =  bound, maximum = FALSE)



capture.output(obj.Q)


