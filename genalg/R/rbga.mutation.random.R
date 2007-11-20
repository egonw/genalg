rbga.mutation.random <- function(population=NA, mutationChance=NA, stringMin=NA, stringMax=NA) {
    if (is.na(population)) stop("A population must be given as input.")
    if (is.matrix(population)) stop("The population must be matrix.")

    vars = ncol(population)
    popSize = nrow(population)

    if (is.na(mutationChance)) mutationChance = 1/(vars+1)

    mutationCount = 0
    for (object in 1:popSize) { # don't mutate the best
        for (var in 1:vars) {
            if (runif(1) < mutationChance) { # ok, do mutation
                ## sample new bit with zeroToOneRatio change
                population[object,var] = stringMin[var] + runif(1)*(stringMax[var]-stringMin[var]);
                mutationCount = mutationCount + 1;
            }
        }
    }
    mutationCount
}
