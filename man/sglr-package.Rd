\name{sglr-package}
\alias{sglr-package}
\alias{sglr}
\docType{package}

\title{
  A package for computing the boundaries for sequential generalized
  likelihood ratio test for pre-licensure vaccine studies
}

\description{
  
  This package is an implementation of the methodology of Shih, Lai,
  Heyse, and Chen (to appear in Statistics in Medicine) for computing
  Generalized Likelihood Ratio test boundaries for pre-licensure vaccine
  studies
}

\details{
  \tabular{ll}{
    Package: \tab sglr\cr
    Type: \tab Package\cr
    Version: \tab 0.05\cr
    Date: \tab 2010-04-20\cr
    License: \tab GPL (version 2 or later)\cr
    LazyLoad: \tab yes\cr
  }
  
  The package provides several functions. The function \code{glrSearch}
  computes boundaries for testing a given \eqn{p_0} versus \eqn{p_1}
  (specified as a two-dimensional vector) given a significance level
  \eqn{\alpha} and a type II error \eqn{\beta}.  The function
  \code{computeBoundary} computes the boundary in terms of a more
  understandable and usable quantity, such as the number of adverse
  events in a pre-licensure vaccine study for example. It takes as input
  a set of given boundaries for the GLR statistic. The third function is
  \code{plotBoundary} which also takes the same arguments as
  \eqn{computeBoundary} and produces a plot. The last two functions can
  make use of statistics computed previously for the problem, which can
  be specified as an argument; otherwise, the statistics are computed
  from scratch.
}

\author{
  Balasubramanian Narasimhan with input from Tze Lai and Mei-Chiung Shih.
  Maintainer: Balasubramanian Narasimhan <naras@stat.stanford.edu>
}

\references{
``Sequential Generalized Likelihood Ratio Tests for Vaccine Safety Evaluation''
\doi{10.1002/sim.4036}.
}

\keyword{ design }
\examples{
library(sglr)
result <- glrSearch(p=c(.5, .75), alpha=0.05, beta=0.10)
## print(result)  ## large amounts of output possible!
result[1:3]
}
