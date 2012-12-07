\name{plotBoundary}
\alias{plotBoundary}
\title{
  A function to plot the boundary of the decision region
}
\description{
  This function attempts to plot the boundary of the decision region, but
  currently falls flat.  Will be rewritten.
}
\usage{
plotBoundary(b1, b0, p, glrTables = NULL, tol=1e-7,
             legend=FALSE, textXOffset=2, textYSkip=2)
}

\arguments{
  \item{b1}{
    The acceptance boundary value (corresponds to the boundary \eqn{b_1} in the appendix of reference)
  }
  \item{b0}{
    The rejection boundary value (corresponds to the boundary \eqn{b_0} in the appendix of reference)
  }
  \item{p}{
    The vector of probabilities, \eqn{(p_0, p_1)} with \eqn{p_0 < p_1}.
  }
  \item{glrTables}{
    A previously computed set of likelihood functions, to speed up
    computation for the \emph{same hypothesis testing problem}. This can
    speed up computations.
  }
  \item{tol}{
    The tolerance, default of 1e-7
  }
  \item{legend}{
    A flag indicating if a legend is desired or not, default false
  }
  \item{textXOffset}{
    Horizontal offset for legend text
    }
  \item{textYSkip}{
    Vertical skip for legend text
    }
}
\details{
  This essentially computes the recursion and the probabilities of hitting
  the boundaries and returns a ggplot2 object
}
\value{
  A ggplot2 object
}
\author{
  Balasubramanian Narasimhan
}

\seealso{
  See Also \code{\link{glrSearch}}
}
\examples{
plotBoundary(b1=2.8, b0=3.3, p=c(.5, .75))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ design }
