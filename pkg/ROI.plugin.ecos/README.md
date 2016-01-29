# Second Order Cone Program with ROI.plugin.ecos

## Supported Cones


## Examples
### Example 1

$$ minimize:  \ - 2 x_2 - 2 x_3 - 2 x_5 - 2 x_6 $$
$$
\begin{aligned}
\text{subject to:} &  \\
               x_1 &= \sqrt{2} \\
               x_4 &= \sqrt{2} \\
               x_1 &\geq |(x_2, \ x_3)| \\
               x_4 &\geq |(x_5, \ x_6)|
\end{aligned}
$$

```{r}
library(ROI)
o <- c(0, -2, -2, 0, -2, -2)
A <- rbind(c(1, 0, 0, 0, 0, 0),
           c(0, 0, 0, 1, 0, 0))
b <- c(sqrt(2), sqrt(2))
G <- diag(x=-1, 6)
h <- rep(0, 6)
cones=list("free"=c(1L, 2L), "soc"=list(3:5, 6:8))

x <- OP(objective = o,
        constraints = C_constraint(L = rbind(A, G), cones=cones, rhs = c(b, h)),
        types = rep("C", 6),
        maximum = FALSE)

opt <- ROI_solve(x, solver = "ecos")
print(opt$solution)
print(opt$objval)
```
