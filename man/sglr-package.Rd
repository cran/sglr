\name{sglr-package}
\alias{sglr-package}
\alias{sglr}
\docType{package}

\title{
An R package for power and boundary calculations in pre-licensure vaccine trials using a sequential generalized likelihood ratio test
}

\description{

  This package is an implementation of the methodology of Shih, Lai,
  Heyse, and Chen (see reference below) for computing Generalized
  Likelihood Ratio test boundaries in pre-licensure vaccine studies
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
  Mei-Chiung Shih, Tze Leung Lai, Joseph F. Heyse, and Jie
  Chen. Sequential Generalized Likelihood Ratio Tests for Vaccine Safety
  Evaluation (Statistics in Medicine, Volume 29, issue 26, p.2698-2708,
  2010.)

  Please also consult the website
  \url{http://med.stanford.edu/biostatistics/ClinicalTrialMethodology/}
  for further developments.
}
\keyword{ design }
\examples{
library(sglr)
result <- glrSearch(p=c(.5, .75), alpha=0.05, beta=0.10)
## print(result)  ## large amounts of output possible!
result[1:3]
}
