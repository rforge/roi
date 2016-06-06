as.F_constraint <- function(x, ...) UseMethod("as.F_constraint")

as_F_constraint <- function(x, ...) {
    F_fun <- function(i) {
        g <- as.matrix(x$L[i,])
        function(x) as.numeric(g %*% x)
    }
    F <- lapply(seq_len(NROW(x$L)), F_fun)
    J_fun <- function(i) {
        g <- as.matrix(x$L[i,])
        function(x) as.numeric(g)
    }
    J <- lapply(seq_len(NROW(x$L)), J_fun)
    F_constraint(F=F, dir=x$dir, rhs=x$rhs, J=J)
}

as.function.L_constraint <- function(x, ...) {
    F_fun <- function(i) {
        g <- as.matrix(x$L[i,])
        function(x) as.numeric(g %*% x)
    }
    lapply(seq_len(NROW(x$L)), F_fun)
}

as.function.Q_objective <- function( x, ... ){
  L <- terms(x)[["L"]]
  ## FIXME: shouldn't this already be initialized earlier?
  if( !length(L) )
      L <- slam::simple_triplet_zero_matrix(ncol = length(x), nrow = 1L)

  Q <- terms(x)[["Q"]]
  ## FIXME: what about objective function names?
  out <- function(x)
      structure( c(slam::tcrossprod_simple_triplet_matrix(L, t(x)) + 0.5 * .xtQx(Q, x)), names = NULL )
  class(out) <- c(class(out), class(x))
  out
}

as.F_constraint_L <- function(x, ...) {
    F_constraint(as.function(x), x$dir, x$rhs, J(x))
}

G <- function( x, ... ) UseMethod("G")

G.F_objective <- function( x, ... ){
    args <- list(...)
    args$func <- terms(x)$F
    g <- terms(x)$G
    if(is.null(g))
        g <- function(x){
            args$x <- x
            do.call(ROI_options("gradient"), args = args)
        }
    stopifnot( is.function(g) )
    g
}

G.L_objective <- function( x, ... ){
    L <- terms(x)$L
    function(x)
        as.numeric(as.matrix(L))
}

G.Q_objective <- function( x, ... ){
    L <- terms(x)$L
    Q <- terms(x)$Q
    function(x)
        c(slam::tcrossprod_simple_triplet_matrix(Q, t(x)) + L)
}

J <- function( x, ... ) UseMethod("J")

J.L_constraint <- function(x, ...) {
    J_fun <- function(i) {
        g <- as.matrix(x$L[i,])
        function(x) as.numeric(g)
    }
    J <- lapply(seq_len(NROW(x$L)), J_fun)
    J
}

J.Q_constraint <- function(x, ...) {
    J_fun <- function(i) {
        L <- x$L[i,]
        Q <- x$Q[[i]]
        ## shouldn't be possible!
        ## if ( is.null(Q) ) {
        ##     return(function(x) as.numeric(L))
        ## }
        function(x) function(x)
            c(slam::tcrossprod_simple_triplet_matrix(Q, t(x)) + L)
    }
    lapply(seq_len(NROW(x$L)), J_fun)
}
