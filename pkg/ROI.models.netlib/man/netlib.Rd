\name{netlib}
\alias{netlib}
\title{Obtain Netlib Linear Problems}
\usage{
netlib(x=c("all", "metainfo", "adlittle", "afiro", "agg", "agg2", 
           "agg3",  "bandm", "beaconfd", "blend", "bnl1", "bnl2", 
           "boeing1", "boeing2", "bore3d", "brandy", "capri",  "cycle", 
           "czprob", "d2q06c", "d6cube", "degen2",  "degen3", "dfl001", 
           "e226", "etamacro", "fffff800",  "finnis", "fit1d", "fit1p", 
           "fit2d", "fit2p",  "forplan", "ganges", "gfrd.pnc", "greenbea", 
           "greenbeb", "grow15", "grow22", "grow7", "israel", "kb2", 
           "lotfi", "maros.r7", "maros", "modszk1", "nesm", "perold", 
           "pilot.ja", "pilot",  "pilot.we", "pilot4", "pilot87", 
           "pilotnov",  "recipe", "sc105", "sc205", "sc50a", "sc50b", 
           "scagr25", "scagr7", "scfxm1", "scfxm2", "scfxm3", "scorpion", 
           "scrs8", "scsd1", "scsd6", "scsd8",  "sctap1", "sctap2", "sctap3", 
           "seba", "share1b",  "share2b", "shell", "ship04l", "ship04s", "ship08l", 
           "ship08s", "ship12l", "ship12s", "sierra", "stair", 
           "standata", "standmps", "stocfor1", "stocfor2", "stocfor3", 
           "truss", "tuff", "vtp.base", "wood1p",  "woodw","x25fv47", "x80bau3b"))
}
\arguments{
  \item{x}{a character giving the name of the optimization problem to be returned.}
}
\description{
  If x is missing a \code{character} vector giving the names of all available problems is
  returned. If x is \code{"all"} a \code{list} containing all the optimization problems
  is returned. If x is the name of an optimization problem, the given optimization
  problem is returned. If x is \code{"metainfo"} a \code{data.frame} containing all the 
  meta info is returned.
  
}
\details{
  Netlib is a software repository, which amongst many other software for
  scientific computing contains a collection of linear programming 
  problems. For more information about the Netlib-LP collection see
  \code{[NETLIB-LP]}.
}
\examples{
\dontrun{
  library(ROI)
  library(ROI.models.netlib)
  ## list all available problems
  netlib()
  ## get all problems as a list
  ntlb <- netlib("all")
  ## get a certain problem by name
  netlib("afiro")
  ntlb[["afiro"]]
  ## get the meta info to the problems
  netlib("metainfo")
  ## solve a problem
  sol <- ROI_solve(netlib("afiro"))
  sol
  sol$objval - as.numeric(netlib("metainfo")["afiro", "optimal_value"])
}
}
\references{
\code{[NETLIB-LP]} Koch, Thorsten (2004)
  The final NETLIB-LP results.
  Operations Research Letters
  \url{https://opus4.kobv.de/opus4-zib/files/727/ZR-03-05.pdf}
}
