library(ROI)
library(slam)

cn <- c("D", "DE", "DP", "DY", "ED", "EP", "EY", "PD", "PE", "PY", "YD", "YE", "YP")

obj <- L_objective(c(1, rep.int(0, length(cn)-1)))

c0 <- L_constraint(simple_triplet_zero_matrix(0, length(cn)), character(0), integer(0), names=cn)
dollar <- L_constraint(       c(1,    1,    1,    1, -0.8706, -1.4279, -0.0075), dir="==", rhs=1, 
                       names=c("D", "DE", "DP", "DY",    "ED",    "PD",   "YD"))
euro <-  L_constraint(c(          1,    1,   1, -1.1486, -1.6401, -0.00861), dir="==", rhs=0, 
                       names=c("ED", "EP", "EY",   "DE",    "PE",     "YE"))
pound <- L_constraint(c(         1,     1,    1, -0.7003, -0.6097, -0.00525), dir="==", rhs=0, 
                       names=c("PD", "PE", "PY",    "DP",    "EP",     "YP"))
yen <-  L_constraint(c(          1,     1,    1, -133.38, -116.12, -190.45), dir="==", rhs=0, 
                       names=c("YD", "YE", "YP",    "DY",    "EY",    "PY"))

con <- rbind(c0, dollar, euro, pound, yen, use.names=TRUE)
M <- as.matrix(con$L)
colnames(M) <- cn
M

length(cn)
op1 <- OP(obj, con, bounds=V_bound(ui=1, ub=10000), maximum=TRUE)
op2 <- OP(obj, con, bounds=V_bound(li=1:13, lb=-rep.int(Inf, 13), ui=1, ub=10000), maximum=TRUE)

ROI_applicable_solvers(o)

glpk1 <- ROI_solve(op1, "glpk")
glpk2 <- ROI_solve(op2, "glpk")
setNames(glpk1$solution, cn)
setNames(glpk2$solution, cn)

ecos <- ROI_solve(op1, "ecos", control=list(MAXIT = 1000L, DEBUG=TRUE))
ecos$G
ecos$h
ecos$A
ecos$b
str(z)
length(z$c)
length(z$c)
dim(z$G)
dim(z$A)
as.matrix(G)
z$h

s 
library(ECOSolveR)
args(ECOS_csolve)
library(Matrix)

length(o)
o <- -as.numeric(as.matrix(obj$L))
A <- as.matrix(con$L)
b <- c(1, 0, 0, 0)
G <- rbind(diag(-1, 13), diag(-1, 1, 13))
h <- c(rep.int(0, 13), 10000)

control <- ecos.control()
control$MAXIT <- 10000L
z <- ECOS_csolve(c=o, G=as(G, "dgCMatrix"), h=h, dims=list(l=14L), 
                 A=as(A, "dgCMatrix"), b=b, control=control)
z
z
