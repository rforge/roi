# ROI.models.globalOptTests

## `globalOptTests`
```r
library(ROI)
library(ROI.models.globalOptTests)
```
`globalOptTests` is a `R` package which contains several non-linear 
programming problems with box constraints. 
More information about `globalOptTests` can be found in 
[Mullen (2014)](http://dx.doi.org/10.18637/jss.v060.i06).

## Using `ROI.models.globalOptTests`
```r
globopt()
```
lists all the names of the `globalOptTests` instances.    
    

```r
globopt(x = "all")
```
gives all the `globalOptTests` instances converted into a **ROI**
optimization problem.

```r
globopt(x = "metainfo")
```
gives the available meta information.


```r
globopt(x = "Rastrigin")
```
returns the `Rastrigin` optimization problem as an ROI optimization problem.


```r
z <- ROI_solve( globopt(x = "Rastrigin"), start =  double(10))
abs(solution(z, "objval") - globopt("metainfo")["Rastrigin", "optimum"])
```


## References
  
  + Mullen, K. (2014). Continuous Global Optimization in R. Journal of Statistical Software, 60(6), 1 - 45. doi:[`http://dx.doi.org/10.18637/jss.v060.i06`](http://dx.doi.org/10.18637/jss.v060.i06)

