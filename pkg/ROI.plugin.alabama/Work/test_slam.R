


library(slam)
x <- simple_triplet_matrix(1, 1, 2, 1, 2)
y <- simple_triplet_zero_matrix(1, 2)
c(x + y)
c(y + x)
c(x + x)
c(y + y)
as.vector(x) + as.vector(y)
