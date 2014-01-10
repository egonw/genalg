\name{summary.rbga}
\alias{summary.rbga}
\title{R Based Genetic Algorithm Summary Function}
\description{
  Summarizes the genetic algorithm results.
}
\usage{
\method{summary}{rbga}(object, echo=FALSE, ...)
}
\arguments{
  \item{object}{a rbga object.}
  \item{echo}{if true, the summary will be printed to STDOUT as well as returned.}
  \item{...}{other options (ignored)}
}
\examples{
evaluate <- function(string=c()) {
    returnVal = 1 / sum(string);
    returnVal
}

rbga.results = rbga.bin(size=10, mutationChance=0.01, zeroToOneRatio=0.5,
    evalFunc=evaluate)

summary(rbga.results)
}
\keyword{multivariate}
