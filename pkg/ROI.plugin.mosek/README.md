# Installation
Before `ROI.plugin.mosek` can be installed `mosek` and its corresponding 
**R** package `Rmosek` need to be installed. Both can be obtained from
[http://docs.mosek.com](http://docs.mosek.com) 
and more information about the installation can be found in the `Rmosek` documentation at
[http://docs.mosek.com/8.0/rmosek/install.html](http://docs.mosek.com/8.0/rmosek/install.html).    


`ROI.plugin.mosek` was tested on `Debian GNU/Linux 8 (jessie)` with the
`mosek` version `8`.      


After `mosek` and `Rmosek` are installed simply use
```r
install.packages("ROI.plugin.mosek", repos="http://R-Forge.R-project.org")
## or (recommended version)
remotes:::install_svn("svn://svn.r-forge.r-project.org/svnroot/roi/pkg/ROI.plugin.mosek")
## or
remotes:::install_github("fl0sch/ROI.plugin.mosek")
```
to install `ROI.plugin.mosek`.
