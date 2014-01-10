\name{rbga.bin}
\alias{rbga.bin}
\title{R Based Genetic Algorithm (binary chromosome)}
\description{
  A R based genetic algorithm that optimizes, using a user set evaluation
  function, a binary chromosome which can be used for variable selection.
  The optimum is the chromosome for which the evaluation value is minimal.

  It requires a \code{evalFunc} method to be supplied that takes as argument
  the binary chromosome, a vector of zeros and ones. 
  Additionally, the GA optimization can be monitored by setting a 
  \code{monitorFunc} that takes a \code{rbga} object as argument.
  
  Results can be visualized with \code{\link{plot.rbga}} and summarized with
  \code{\link{summary.rbga}}.
}
\usage{
rbga.bin(size=10,
         suggestions=NULL,
         popSize=200, iters=100, 
         mutationChance=NA,
         elitism=NA, zeroToOneRatio=10,
         monitorFunc=NULL, evalFunc=NULL,
         showSettings=FALSE, verbose=FALSE)
}
\arguments{
  \item{size}{the number of genes in the chromosome.}
  \item{popSize}{the population size.}
  \item{iters}{the number of iterations.}
  \item{mutationChance}{the chance that a gene in the chromosome mutates. By default 1/(size+1).
        It affects the convergence rate and the probing of search space: a low chance results
        in quicker convergence, while a high chance increases the span of the search space.}
  \item{elitism}{the number of chromosomes that are kept into the next generation.
        By default is about 20\% of the population size.}
  \item{zeroToOneRatio}{the change for a zero for mutations and initialization. This option is used
        to control the number of set genes in the chromosome. For example, when doing variable
        selectionm this parameter should be set high to }
  \item{monitorFunc}{Method run after each generation to allow monitoring of the
        optimization}
  \item{evalFunc}{User supplied method to calculate the evaluation function for
        the given chromosome}
  \item{showSettings}{if true the settings will be printed to screen. By default False.}
  \item{verbose}{if true the algorithm will be more verbose. By default False.}
  \item{suggestions}{optional list of suggested chromosomes}
}
\section{References}{
  C.B. Lucasius and G. Kateman (1993).
  Understanding and using genetic algorithms - Part 1. Concepts, properties and context.
  \emph{Chemometrics and Intelligent Laboratory Systems 19:1-33}.

  C.B. Lucasius and G. Kateman (1994).
  Understanding and using genetic algorithms - Part 2. Representation, configuration and hybridization.
  \emph{Chemometrics and Intelligent Laboratory Systems 25:99-145}.
}
\seealso{
  \code{\link{rbga}}
  \code{\link{plot.rbga}}
}
\examples{
# a very simplistic optimization
evaluate <- function(string=c()) {
    returnVal = 1 / sum(string);
    returnVal
}

rbga.results = rbga.bin(size=10, mutationChance=0.01, zeroToOneRatio=0.5,
    evalFunc=evaluate)

plot(rbga.results)

# in this example the four variables in the IRIS data set are complemented
# with 36 random variables. Variable selection should find the four original
# variables back (example by Ron Wehrens).
\dontrun{
data(iris)
library(MASS)
X <- cbind(scale(iris[,1:4]), matrix(rnorm(36*150), 150, 36))
Y <- iris[,5]

iris.evaluate <- function(indices) {
  result = 1
  if (sum(indices) > 2) {
    huhn <- lda(X[,indices==1], Y, CV=TRUE)$posterior
    result = sum(Y != dimnames(huhn)[[2]][apply(huhn, 1,
               function(x)
               which(x == max(x)))]) / length(Y)
  }
  result
}

monitor <- function(obj) {
    minEval = min(obj$evaluations);
    plot(obj, type="hist");
}

woppa <- rbga.bin(size=40, mutationChance=0.05, zeroToOneRatio=10,
  evalFunc=iris.evaluate, verbose=TRUE, monitorFunc=monitor)
}

# another realistic example: wavelenght selection for PLS on NIR data
\dontrun{
library(pls.pcr)
data(NIR)

numberOfWavelenghts = ncol(NIR$Xtrain)
evaluateNIR <- function(chromosome=c()) {
    returnVal = 100
    minLV = 2
    if (sum(chromosome) < minLV) {
        returnVal
    } else {
        xtrain = NIR$Xtrain[,chromosome == 1];
        pls.model = pls(xtrain, NIR$Ytrain, validation="CV", grpsize=1, 
                        ncomp=2:min(10,sum(chromosome)))
        returnVal = pls.model$val$RMS[pls.model$val$nLV-(minLV-1)]
        returnVal
    }
}

monitor <- function(obj) {
    minEval = min(obj$evaluations);
    filter = obj$evaluations == minEval;
    bestObjectCount = sum(rep(1, obj$popSize)[filter]);
    # ok, deal with the situation that more than one object is best
    if (bestObjectCount > 1) {
        bestSolution = obj$population[filter,][1,];
    } else {
        bestSolution = obj$population[filter,];
    }
    outputBest = paste(obj$iter, " #selected=", sum(bestSolution),
                       " Best (Error=", minEval, "): ", sep="");
    for (var in 1:length(bestSolution)) {
        outputBest = paste(outputBest,
            bestSolution[var], " ",
            sep="");
    }
    outputBest = paste(outputBest, "\n", sep="");

    cat(outputBest);
}

nir.results = rbga.bin(size=numberOfWavelenghts, zeroToOneRatio=10, 
    evalFunc=evaluateNIR, monitorFunc=monitor,
    popSize=200, iters=100, verbose=TRUE)
}
}
\keyword{multivariate}
