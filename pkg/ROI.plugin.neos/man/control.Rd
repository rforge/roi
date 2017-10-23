\name{control}
\alias{control}

\title{Control Variables}
\description{The control variables for \code{ROI.plugin.neos}.}

\arguments{
\item{user}{a character string giving the username.}
\item{email}{a character string giving the email address.}
\item{dry\_run}{a logical if \code{TRUE} \pkg{ROI} returns the solver call.}
\item{wait}{a logical indicating whether the R interpreter should wait for the 
            command to finish, or run it asynchronously. If \code{TRUE} 
            \pkg{ROI} returns an object of class \code{"NeosJob"}.}
\item{id}{an integer giving the identifier for the submitted job 
          (passed to \code{rneos::NsubmitJob}).}
\item{interface}{a character string passed to \code{rneos::NsubmitJob}
}
}
