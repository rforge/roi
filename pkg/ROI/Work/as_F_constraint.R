## TODO
q("no")
Rdevel

library(ROI)
library(slam)

.xtQx <- function(Q, x) {
    ## Value of quadratic form t(x) %*% Q %*% x.
    ## As we implement simple triplet matrices in S3, we could only have
    ## %*% and crossprod methods if we created S3 generics for these ...
    if(is.simple_triplet_matrix(Q))
        sum(Q$v * x[Q$i] * x[Q$j])
    else
        c(crossprod(x, Q %*% x))
}

mat <- matrix(c(3, 4, 2,
                2, 1, 2,
                1, 3, 2), nrow=3, byrow=TRUE)
lp <- OP(objective = c(2, 4, 3),
         constraints = L_constraint(L = mat,
                                    dir = c("<=", "<=", "<="),
                                    rhs = c(60, 40, 80)),
         maximum = TRUE)

opt <- ROI_solve(lp)
opt
opt$solution

f0 <- function(x) -as.numeric(c(2, 4, 3) %*% x)
grad_f0 <- function(x) -c(2, 4, 3)
g0 <- function(x) as.numeric(mat %*% x) - c(60, 40, 80)
grad_g0 <- function(x) mat

obj <- F_objective(f0, 3, grad_f0)
con <- F_constraint(g0, dir="<=", rhs=0, J=grad_g0)
op <- OP(objective = obj, constraints = con, maximum = FALSE)
control <- list(algorithm = "NLOPT_LD_MMA", start=c(1, 7, 2))
ROI_solve(op, solver="nloptr", control=control)$solution

f0(c(1, 1, 1))
objective(x)(c(1, 1, 1))
grad_f0(c(1, 1, 1))
x$objective$G(c(1, 1, 1))
g0(c(1, 1, 1))
eval_g_ineq(c(1, 1, 1))
environment(eval_g_ineq)$g
eval_jac_g_ineq(c(1, 1, 1))

control <- list(method = "NLOPT_LD_MMA", start=c(1, 7, 2))
ROI_solve(op, solver="nloptr", control=control)$solution


f0 <- function(x) as.numeric(c(2, 4, 3) %*% x)
grad_f0 <- function(x) c(2, 4, 3)
g0 <- function(x) as.numeric(mat %*% x) - c(60, 40, 80)
grad_g0 <- function(x) mat

obj <- F_objective(f0, 3, grad_f0)
con <- F_constraint(g0, dir="<=", rhs=0, J=grad_g0)
op <- OP(objective = obj, constraints = con, maximum = TRUE)
control <- list(algorithm = "NLOPT_LD_MMA", start=c(1, 7, 2))
ROI_solve(op, solver="nloptr", control=control)$solution

## -------------------------------------
## Convert Linear Constraints
## -------------------------------------
lo <- L_objective(-c(2, 4, 3))
lc <- L_constraint(L = mat, dir = c("<=", "<=", "<="), rhs = c(60, 40, 80))
lp <- OP(objective = lo, constraints = lc, maximum = FALSE)

obj <- as.F_objective(lp$objective)

fun <- function(i) {
    g <- as.matrix(tmp[i,])
    function(x) as.numeric(g %*% x)
}
tmp <- lc$L
F <- lapply(seq_len(NROW(tmp)), fun)
rm(tmp)
F[[1]](c(1, 1, 1))
lapply(F, function(g) g(c(1, 1, 1)))

FF <- function(x) {
    unlist(lapply(F, function(g) g(x)))
}

fcon <- F_constraint(F, dir = c("<=", "<=", "<="), rhs = c(60, 40, 80))
rhs <- c(60, 40, 80)
FF <- function(x) {
    unlist(mapply(function(g, r) g(x) - r, F, rhs))
}
FF(c(1, 1, 1))

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

flo <- as.F_objective(lo)
flc <- as.F_constraint_L(lc)
flp <- OP(objective = flo, constraints = flc, maximum = FALSE)
x <- flp
control <- list(algorithm="NLOPT_LD_MMA", x0=c(1, 7, 2))
ROI_solve(flp, control=control)

control <- list()
ROI_solve(op, solver="nloptr", control=control)$solution
o$solution

as.function.Q_constraint <- function(x, ...) {
    if (is.null(x$L)) x$L <- simple_triplet_zero_matrix(nrow = nrow(x), ncol = ncol(x))
    F_fun <- function(i) {
        L <- x$L[i,]
        Q <- x$Q[[i]]
        ## this shouldn't happen!
        ## if ( is.null(Q) ) return(tcrossprod_simple_triplet_matrix(L, t(x)))
        c(tcrossprod_simple_triplet_matrix(L, t(x)) + 0.5 * .xtQx(Q, x))
    }
    lapply(seq_len(NROW(x)), F_fun)
}

as.function(lc)
as.function(qc)
traceback()
length(qc)
dim(qc)


tcrossprod_simple_triplet_matrix(lc$L[1,], t(c(1, 1, 1)))

lc$L
NROW(qc)
fc <- as_F_constraint(lc)
names(as_F_constraint(lc))
str(fc$J)
traceback()
fc$J[[1]](c(1, 1, 1))
fc$J[[2]](c(1, 1, 1))

## -------------------------------------
## Convert Quadratic Constraints
## -------------------------------------
qo <- Q_objective(Q = matrix(c(-33, 6, 0, 6, -22, 11.5, 0, 11.5, -11),
                  byrow = TRUE, ncol = 3),
                  L = c(1, 2, 3))
qc <- Q_constraint(Q = list(NULL, NULL, diag(1, nrow = 3)),
                   L = matrix(c(-1, 1, 1, 1, -3, 1, 0, 0, 0), byrow = TRUE, ncol = 3),
                   dir = rep("<=", 3),
                   rhs = c(20, 30, 1))
qcqp <- OP(qo, qc, maximum = TRUE )

as.Q_term(list(NULL, NULL, diag(1, nrow = 3)), nrow=3, ncol=3)
as.Q_term


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

J(lc)
J(qc)



as.F_objective(qo)



## Des ist ned so anfoch denn wie des design jetzt ist, würde es bedeuten 
## das der Solver eine Liste von Funktionen verkraften muss.
library(nloptr)

mat <- matrix(c(3, 4, 2,
                2, 1, 2,
                1, 3, 2), nrow=3, byrow=TRUE)
mat
lp <- OP(objective = c(2, 4, 3),
         constraints = L_constraint(L = mat,
                                    dir = c("<=", "<=", "<="),
                                    rhs = c(60, 40, 80)),
         maximum = TRUE)


f0 <- function(x) -as.numeric(c(2, 4, 3) %*% x)
grad_f0 <- function(x) -c(2, 4, 3)
g0 <- function(x) as.numeric(mat %*% x) - c(60, 40, 80)
grad_g0 <- function(x) mat
    
g0()

s <- nloptr( x0=c(1, 6, 2), eval_f=f0, eval_grad_f=grad_f0, 
             lb = c(0, 0, 0), ub = c(Inf, Inf, Inf),
             eval_g_ineq = g0, eval_jac_g_ineq = grad_g0, 
             opts = list("algorithm" = "NLOPT_LD_MMA", xtol_abs = 1.0e-7))

s$solution
str(s)

args(check.derivatives)
check.derivatives(.x=c(0,0,0), func=f0, func_grad=grad_f0)

F <- x$constraints$F
f <- x$constraints$F[[1]]
dir <- x$constraints$dir[[1]]
rhs <- x$constraints$rhs[[1]]

eval_g0 <- function( z, a, b ) {
    return( (a*z[1] + b)^3 - z[2] )
}

formals(f)
formals(eval_g0)

f1 <- function() {
    dummy
}
formals(f1) <- formals(f)
body(f1) <- 


form <- formals(f1)
args <- as.list(form)
for (a in names(args)) {
    args[[a]] <- get(a)
}
args
as.list()

fa <- function(x) {
    objective(x)
}

fa(as.pairlist(args))
eval(call("fa", as.pairlist(args)))


mapply(function(f, dir, rhs) (f() - rhs), F, )



