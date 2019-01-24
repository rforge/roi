# ROI.models.miplib

## `MIPLIB 2010`
```r
library(ROI)
library(ROI.models.miplib)
```
`MIPLIB 2010` is a library of mixed integer programming problems which can be obtained
from `http://miplib.zib.de/`. `ROI.models.miplib` provides utility functions
to download `MIPLIB 2010` and convert it into [ROI](https://CRAN.R-project.org/package=ROI)
optimization problems. Since `MIPLIB 2010` is kind of big only functions 
for downloading and accessing the library are provided but not the library itself.

## Download `MIPLIB`
There are two versions of `MIPLIB` available at `http://miplib.zib.de/`
a smaller version for benchmarking (around 94 MB) and the full version (around 1.3 GB)
which contains all benchmark instances.    
    
Download the **benchmark** instances.
```r
miplib_download_benchmark(quiet = FALSE)
```
    
Download the **all** instances.
```r
miplib_download_all(quiet = FALSE)
```
    
By default the optimization problems are stored in the package folder as 
`.rds`-files and can be accessed via the function `miplib`.


## Download `MIPLIB 2010` Metainfo
At `http://miplib.zib.de/` also solutions to the problem instances are 
provided which can easily be obtained by executing.
```r
miplib_download_metainfo()
```

## Using `MIPLI 2010`
After downloading `MIPLIB 2010` it can be accessed by the function `miplib`.   
```r
miplib()
## or if a non-default folder name was provided at the download step
miplib(folder = folder_name)
```
lists all the names of the (downloaded) `MIPLIB 2010` instances.    
    

```r
miplib(x = "all")
## or if a non-default folder name was provided at the download step
miplib(x = "all", folder = folder_name)
```
gives all the (downloaded) `MIPLIB` instances.


```r
miplib(x = "metainfo")
## or if a non-default folder name was provided at the download step
miplib(x = "metainfo", folder = folder_name)
```
gives the available meta information.


```r
miplib(x = "rmine6")
## or if a non-default folder name was provided at the download step
miplib(x = "rmine6", folder = folder_name)
```
returns the `rmine6` optimization problem as an ROI optimization problem.


```r
ROI_solve( miplib(x = "iis.100.0.cov") )
```


## References
  
  + Thorsten Koch, Tobias Achterberg, Erling Andersen, Oliver Bastert, Timo Berthold, Robert E. Bixby, Emilie Danna, Gerald Gamrath, Ambros M. Gleixner, Stefan Heinz, Andrea Lodi, Hans Mittelmann, Ted Ralphs, Domenico Salvagnin, Daniel E. Steffy, Kati Wolter (2011).    
  **MIPLIB 2010**    
  URL [`http://mpc.zib.de/index.php/MPC/article/view/56/28`](http://mpc.zib.de/index.php/MPC/article/view/56/28)

  + R. E. Bixby, E. A. Boyd, R. R. Indovina (1992).    
  **MIPLIB: A Test Set of Mixed Integer Programming Problems**
