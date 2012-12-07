\name{computeBoundary}
\alias{computeBoundary}
\title{
  A function to compute the boundary of the decision region in terms of
  the number of adverse events (AEs) of interest, such as vaccine AEs.
}
\description{
  This function computes the boundary of the decision region in a manner
  that can be employed in the field, as a table, for example. See
  section 4.2 of the reference below.
}
\usage{
computeBoundary(b1, b0, p, glrTables = NULL, tol=1e-7)
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
    computation for the \emph{same hypothesis testing
    problem}. Otherwise, it is computed ab initio, resulting in a longer
    running time.
  }
  \item{tol}{
    A numerical tolerance, defaults to 1e-7
  }
}
\details{
  This essentially computes the probabilities of hitting
  the boundaries using a recursion.
}

\value{
  \item{upper}{The upper boundary that indicates rejection of the null
  hypothesis}
  \item{lower}{The upper boundary that indicates acceptance of the null
  hypothesis}
  \item{estimate }{The estimated \eqn{\alpha} and \eqn{\beta}
    values corresponding to the two boundaries}
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

\seealso{
  See Also \code{\link{glrSearch}}
}
\examples{
computeBoundary(b1=2.8, b0=3.3, p=c(.5, .75))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ design }
