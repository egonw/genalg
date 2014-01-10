\name{plot.rbga}
\alias{plot.rbga}
\title{R Based Genetic Algorithm Plot Function}
\description{
  Plots features of the genetic algorithm optimization run. The default plot
  shows the minimal and mean evaluation value, indicating how far the GA has
  progressed.
  
  The "hist" plot shows for binary chromosome the gene selection frequency,
  i.e. the times one gene in the chromosome was selected in the current population.
  In case of floats chromosomes, it will make histograms for each variable to
  indicate the selected values in the population.
  
  The "vars" plot the evaluation function versus the variable value. This is
  useful to look at correlations between the variable and the evaluation values.
}
\usage{
\method{plot}{rbga}(x, type="default", breaks=10, ...)
}
\arguments{
  \item{x}{a rbga object.}
  \item{type}{one of "hist", "vars" or "default".}
  \item{breaks}{the number of breaks in a histogram.}
  \item{...}{options directly passed to the plot function.}
}
\examples{
evaluate <- function(string=c()) {
    returnVal = 1 / sum(string);
    returnVal
}

rbga.results = rbga.bin(size=10, mutationChance=0.01, zeroToOneRatio=0.5,
    evalFunc=evaluate)

plot(rbga.results)
plot(rbga.results, type="hist")
}
\keyword{multivariate}
