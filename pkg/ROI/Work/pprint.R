
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


pprint.OP <- function(x) {
    sense <- if (x$maximum) "maximize" else "minimize"
    cat(paste("\n    ", sense, "_x  1 / 2 * t(x) %*% Q %*% x + L %*% x \n\n", sep=""))
    pprint.Q_objective(objective(x))
    pprint.L_constraint(constraints(x))
    pprint.V_bounds(bounds(x))
    invisible(NULL)
}

x <- z
pprint.OP(x)

pprint.Q_objective <- function(x) {
    obj.Q <- capture.output(as.matrix(terms(x)$Q))
    obj.Q <- gsub(".*\\]\\s{0,1}", "", obj.Q[-1L])
    obj.L <- as.vector(terms(x)$L)
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
    cat(" - L:\n")
    cat(sprintf("     %s  %s  %s", con.L, x$dir, rhs), sep="\n")
    invisible(NULL)
}
x <- constraints(z)
pprint.L_constraint(x)

vec2char <- function(x) {
    a <- as.character(x)
    mc <- max(nchar(a))
    fill <- function(s, n) {
        space <- paste(rep.int(" ", n), collapse="")
        paste(space, s, sep="")
    }
    mapply(fill, a, mc - nchar(a), USE.NAMES =  FALSE)
}

pprint.V_bounds <- function(x) {    
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

x <- z
pprint.OP(x)


x <- objective(z)
pprint.Q_objective(x)

x <- constraints(z)
pprint.L_constraint(x)

x <- bounds(z)
pprint.V_bounds(x)



