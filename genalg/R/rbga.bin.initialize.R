rbga.bin.initialize.random <- function(size=0, bits=0) {
    # one bit per chromosome on average
    if (is.na(zeroToOneRatio)) zeroToOneRatio = bits;

    population = matrix(nrow=size, ncol=bits);
    # fill values
    for (child in 1:size) {
        population[child,] = sample(c(rep(0,zeroToOneRatio),1), bits, rep=TRUE);
        while (sum(population[child,]) == 0) {
            # at least one bit must be set
            population[child,] = sample(c(rep(0,zeroToOneRatio),1), bits, rep=TRUE);
        }
    }
    return(population)
}