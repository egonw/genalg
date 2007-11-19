rbga.bin.mutation.switchbit <- function(population=NA, mutationChance=NA) {
    if (is.na(population)) stop("A population must be given as input.")
    if (is.matrix(population)) stop("The population must be matrix.")

    vars = ncol(population)
    popSize = nrow(population)

    if (is.na(mutationChance)) mutationChance = 1/(vars+1)

    mutationCount = 0
    for (object in 1:popSize) { # don't mutate the best
        for (var in 1:vars) {
            if (runif(1) < mutationChance) { # ok, do mutation
                ## switch bit
                if (population[object,var] == 0) {
                    population[object,var] = 1;
                } else {
                    population[object,var] = 0;
                }
                mutationCount = mutationCount + 1;
            }
        }
    }
    mutationCount
}