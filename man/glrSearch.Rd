\name{glrSearch}
\alias{glrSearch}
\title{
  This function searches through a space of design boundaries (scalar
  values a and b) to find values that achieve close to specified
  type I and type II errors for the sequential generalized
  likelihood ratio test of p0 versus p1 (specified
  respectively as vector of length 2) in pre-licensure vaccine trials
}
\description{

  The search through the space of \eqn{b_1} (corresponds to \eqn{b_1} in
  paper) and \eqn{b_0} (corresponds to \eqn{b_0} in paper) is greedy
  initially. Then refinements to the boundary are made by adjusting the
  boundaries by the step-size. It is entirely possible that the
  step-size is so small that a maximum number of iterations can be
  reached. Depending on how close \eqn{p_0} and \eqn{p_1} are the memory
  usage can grow significantly. The process is computationally intensive
  being dominated by a recursion deep in the search.

}
\usage{
glrSearch(p, alpha, beta, stepSize = 0.5, tol = 1e-7,
          startB1 = log(1/beta), startB0 = log(1/alpha),
          maxIter = 25, gridIt = FALSE, nGrid = 5,
          verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
    The vector of \eqn{p_0} and \eqn{p_1}, with \eqn{p_0 < p_1}
  }
  \item{alpha}{
    A value for type I error \eqn{\alpha} between 0 and 1 typically 0.05 which is the
    default value
  }
  \item{beta}{
    A value for type II error (\eqn{\beta}) between 0 and 1 typically below .2,
    default 0.10
  }
  \item{stepSize}{
    A value to use for moving the boundaries during the search, 0.5
    default seems to work.
  }
  \item{tol}{
    A value that is used for deciding when to terminate the search. A
    euclidean metric is used. Default 1e-7.
  }
  \item{startB1}{
    A starting value for the futility boundary, default is log of reciprocal
    type I error
  }
  \item{startB0}{
    A starting value for the rejection boundary, default is log of reciprocal
    type II error

  }
  \item{maxIter}{
    A maximum number of iterations to be used for the search. This
    allows for a bailout if the step size is too small.
  }
  \item{gridIt}{
    A logical value indicating if a grid of values should be evaluated
    once the boundaries are bracketed in the search.
  }
  \item{nGrid}{
    The number of grid points to use, default 5
  }
  \item{verbose}{
    A logical flag indicating if you want verbose output during
    search. Useful for situations where the code gets confused.
}
}
\details{
  One should not use this code without a basic understanding of the
  Shih, Lai, Heyse and Chen paper cited below, particularly the section
  on the pre-licensure vaccine trials.

  As the search can be computationally intensive, the program needs to
  use some variables internally by reference, particularly large tables
  that stay constant.

  In our experiments, starting off with the default step size has
  usually worked, but in other cases the step size and the maximum
  number of iterations may need to be adjusted.

}

\value{
  \item{b1}{The explored values of the futility boundary
    \eqn{b_1} (corresponds to the boundary \eqn{b_1} in the appendix of reference)}
  \item{b0}{The explored values of the rejection boundary
    \eqn{b_0} (corresponds to the boundary \eqn{b_0} in the appendix of reference)}
  \item{estimate }{The estimated \eqn{\alpha} and \eqn{\beta}
    values corresponding to the explored boundaries (a 2-column matrix);
    first column is \eqn{\alpha}, second is \eqn{\beta}}
  \item{glrTables}{The constant values of the log likelihoods under
    \eqn{p_0}, \eqn{p_1} and the estimate probability of terminating at that
    step. The first two, are, in turn, lists of length \eqn{n} where \eqn{n}
    is the maximum number of adverse events that might be needed for the
    test. The last element is a matrix of 2 columns, specifying the
    probability of terminating at each value of \eqn{n}}
  \item{alphaTable}{a matrix (nGrid x nGrid) of \eqn{\alpha} values
    corresponding to the combinations of boundaries \eqn{b} and \eqn{a}
    (which are the row and column names of the matrix). This is computed
  only if \code{gridIt=TRUE} }
  \item{betaTable}{a matrix (nGrid x nGrid) of \eqn{\beta} values
    corresponding to the combinations of boundaries \eqn{b} and \eqn{a}
    (which are the row and column names of the matrix). This is computed
    only if \code{gridIt=TRUE}}
  \item{b1Vals}{the vector of \eqn{b_1} (or equivalently \eqn{b_1}) values
    used in the grid, computed only if \code{gridIt=TRUE}}
  \item{b0Vals}{the vector of \eqn{b_0} (or equivalently \eqn{b_0}) values
    used in the grid, computed only if \code{gridIt=TRUE}}
  \item{iterations}{The number of iterations actually used}
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

\author{
  Balasubramanian Narasimhan
}

\examples{
library(sglr)
result <- glrSearch(p=c(.5, .75), alpha=0.05, beta=0.10)

result <- glrSearch(p=c(.5, .75), alpha=0.05, beta=0.10, verbose=TRUE)

result <- glrSearch(p=c(.5, .75), alpha=0.05, beta=0.10, gridIt=TRUE)
print(result$alphaTable)
print(result$betaTable)

## takes a while
result <- glrSearch(p=c(.5, 2/3), alpha=0.05, beta=0.10)
print(names(result))

##result <- glrSearch(p=c(.5, 2/3), alpha=0.05, beta=0.10, gridIt=TRUE)
##print(result$alphaTable)
##print(result$betaTable)
}
\keyword{ design }
