# This Code is Licensed under the Terms of the GPLv3
#
# Authors:
# Original Code by: Dr E.L. Willighagen 
# Modifications by: Frank Schwidom, Dipl.-Inf. (FH)

# Notice: this code is currently not well formed, but does function so far
# TODO: increase redability
# TODO: user-defined *apply function

# d3f7dce7c9ba49e9b7c292433dbba2fa eliteDuplicatesIdx: redundanzen unter den elitaeren chromosomen verhindern, ist jetzt nicht der Bringer, koennte aber noch wichtig sein (inzest verhindern)
# c18154b862e34346a274cfd0cc6bf8dc sortierung vor Ende
# 8779f033359e4ca5a9bf763083475a74 Fehler hatte weitreichende Folgen
# 1df67b3102d846f6b8ae02b0e5f63454 2 bugs solved: berechnung der mutation und wenn length( suggestions) == popSize

rbga2 <- function (
        stringMin=c(), stringMax=c(),
        suggestions=NULL,
        popSize=200, iters=100, 
        mutationChance=NA,
        elitism=NA,
        monitorFunc=NULL, evalFunc=NULL,
        showSettings=FALSE, verbose=FALSE,
        # fPerIter= NULL, 
        # elitismDifference= 0.001,
        # fElitismDifferenceTooLow= function( chromCurr, chromNext){ sum( abs( chromCurr - chromNext)) < elitismDifference},
        fElitismDifferenceTooLow= NULL
        # dummy= NULL
) {

    pi2= pi*2


    if (is.null(evalFunc)) {
        stop("A evaluation function must be provided. See the evalFunc parameter.")
    }
    vars = length(stringMin)
    if (is.na(mutationChance)) {
        mutationChance = 1/(vars + 1)
    }
    if (is.na(elitism)) {
        elitism = floor(popSize/5)

     # optimizations
     popSizeProl= popSize- elitism
     popSizeProlRange= (elitism + 1):popSize

     popSizeVars= popSize* vars
     popSizeProlVars= popSizeProl* vars

     stringMinProlM= matrix( stringMin, byrow= TRUE,
      nrow = popSizeProl, ncol = vars)

     stringMaxProlM= matrix( stringMax, byrow= TRUE,
      nrow = popSizeProl, ncol = vars)

     stringMinM= matrix( stringMin, byrow= TRUE,
      nrow = popSize, ncol = vars)

     stringMaxM= matrix( stringMax, byrow= TRUE,
      nrow = popSize, ncol = vars)

    }
    if (verbose) 
        cat("Testing the sanity of parameters...\n")
    if (length(stringMin) != length(stringMax)) {
        stop("The vectors stringMin and stringMax must be of equal length.")
    }
    if (popSize < 5) {
        stop("The population size must be at least 5.")
    }
    if (iters < 1) {
        stop("The number of iterations must be at least 1.")
    }
    if (!(elitism < popSize)) {
        stop("The population size must be greater than the elitism.")
    }
    if (showSettings) {
        if (verbose) 
            cat("The start conditions:\n")
        result = list(stringMin = stringMin, stringMax = stringMax, 
            suggestions = suggestions, popSize = popSize, iters = iters, 
            elitism = elitism, mutationChance = mutationChance)
        class(result) = "rbga"
        cat(summary(result))
    }
    else {
        if (verbose) 
            cat("Not showing GA settings...\n")
    }
    if (vars > 0) {
        if (!is.null(suggestions)) {
            if (verbose) 
                cat("Adding suggestions to first population...\n")
            population = matrix(nrow = popSize, ncol = vars)
            suggestionCount = dim(suggestions)[1]
            population[ 1:suggestionCount, ] = suggestions[ 1:suggestionCount, ] 
            #for (i in 1:suggestionCount) {
            #    population[i, ] = suggestions[i, ]
            #}
            if (verbose) 
                cat("Filling others with random values in the given domains...\n")
            for (var in 1:vars) {
             if( (suggestionCount + 1) < popSize) # Korrektur 1df67b3102d846f6b8ae02b0e5f63454
             {
                population[(suggestionCount + 1):popSize, var] = stringMin[var] + 
                  runif(popSize - suggestionCount) * (stringMax[var] - 
                    stringMin[var])
             }
            }
        }
        else {
            if (verbose) 
                cat("Starting with random values in the given domains...\n")
            population = matrix(nrow = popSize, ncol = vars)

            #population[ , 1:vars]= t( stringMin[1:vars] + (stringMax[1:vars] - stringMin[1:vars]) * matrix( 1, ncol=popSize, nrow= vars) * runif(popSize))

            population= stringMinM + (stringMaxM - stringMinM) * 
             matrix( runif(popSizeVars), nrow=popSize, ncol= vars)

            #for (var in 1:vars) {
            #    population[, var] = stringMin[var] + runif(popSize) * 
            #      (stringMax[var] - stringMin[var])
            #}
        }
        bestEvals = rep(NA, iters)
        meanEvals = rep(NA, iters)
        evalVals = rep(NA, popSize)
        for (iter in 1:iters)
        {
            if (verbose) 
                cat(paste("Starting iteration", iter, "\n"))
            if (verbose) 
                cat("Calucating evaluation values... ")

            # if( !is.null( fPerIter)) fPerIter( iter)

            nas= is.na(evalVals) # 8779f033359e4ca5a9bf763083475a74
            #evalVals[ nas]= as.vector( sapply( as.data.frame( t( population))[ , nas], evalFunc))
            #evalVals[ nas]= as.vector( sapply( as.data.frame( t( population[ nas, ])), evalFunc))
            #evalVals[ nas]= unlist( sapply( as.data.frame( t( population[ nas, ])), evalFunc))
            #evalVals[ nas]= unlist( lapply( as.data.frame( t( population[ nas, ])), evalFunc))

            if( 1)
            {
            for (object in ( 1:popSize)[ nas]) # 8779f033359e4ca5a9bf763083475a74
            { # for is faster than Xapply, no apply version for matrices found!
                  evalVals[object] = evalFunc(population[object, ])
                  if (verbose) 
                    cat(".")
            }
            }
            else if( 1)
            {
             evalVals[ nas]= unlist( lapply( as.data.frame( t( population[ nas, ])), evalFunc)) # 8779f033359e4ca5a9bf763083475a74
            }
            else # old code
            {
            for (object in 1:popSize) {
                if (is.na(evalVals[object])) {
                  evalVals[object] = evalFunc(population[object, 
                    ])
                  if (verbose) 
                    cat(".")
                } # else { print( "!is.na")} # treten auf
            }
            }

            bestEvals[iter] = min(evalVals)
            meanEvals[iter] = mean(evalVals)
            if (verbose) 
                cat(" done.\n")
            if (!is.null(monitorFunc)) {
                if (verbose) 
                  cat("Sending current state to rgba.monitor()...\n")
                result = list(type = "floats chromosome", stringMin = stringMin, 
                  stringMax = stringMax, popSize = popSize, iter = iter, 
                  iters = iters, population = population, elitism = elitism, 
                  mutationChance = mutationChance, evaluations = evalVals, 
                  best = bestEvals, mean = meanEvals)
                class(result) = "rbga"
                monitorFunc(result)
            }

            { # c18154b862e34346a274cfd0cc6bf8dc hier hin gelagert
            if (verbose) 
              cat("  sorting results...\n")
            sortedEvaluations = sort(evalVals, index= TRUE) 
            sortedPopulation = population[ sortedEvaluations$ix, ]

            }


            #print( paste( iter, ' ', iters))

            if (iter == iters)
            {
             evalVals= sortedEvaluations$x 
             population= sortedPopulation
            }
            else # iter < iters
            {
                if (verbose) 
                  cat("Creating next generation...\n")
                newPopulation = matrix(nrow = popSize, ncol = vars)
                newEvalVals = rep(NA, popSize)

                # c18154b862e34346a274cfd0cc6bf8dc ausgelagert

                if( !is.null( fElitismDifferenceTooLow)) # d3f7dce7c9ba49e9b7c292433dbba2fa
                {
                eliteDuplicatesIdx= c()

                for( i in 2:elitism)
                {
                 if( fElitismDifferenceTooLow( sortedPopulation[ ( i-1), ], sortedPopulation[ ( i), ]))
                  eliteDuplicatesIdx= c( eliteDuplicatesIdx, i)
                }

                #print( 'eliteDuplicatesIdx')
                #print( eliteDuplicatesIdx)
                #print( length( eliteDuplicatesIdx))
                if( ! is.null( eliteDuplicatesIdx))
                {
                 #print( 'sortedPopulation[ eliteDuplicatesIdx, ]')
                 #print( sortedPopulation[ eliteDuplicatesIdx, ])

                 sortedPopulation[ eliteDuplicatesIdx, ]= stringMinM[ eliteDuplicatesIdx, ] +
                 (stringMaxM - stringMinM)[ eliteDuplicatesIdx, ] *
                  matrix( runif( length( eliteDuplicatesIdx)* vars),
                   nrow=length( eliteDuplicatesIdx), ncol= vars)

                 sortedEvaluations$x[ eliteDuplicatesIdx]= NA # 8779f033359e4ca5a9bf763083475a74 fehler geloest

                 #print( sortedPopulation[ eliteDuplicatesIdx, ])
                }

                }

                if (elitism > 0) {
                  if (verbose) 
                    cat("  applying elitism...\n")
                  newPopulation[1:elitism, ] = sortedPopulation[1:elitism, 
                    ]
                  newEvalVals[1:elitism] = sortedEvaluations$x[1:elitism]
                }
                if (vars > 1) {
                  if (verbose) 
                    cat("  applying crossover...\n")
                  for (child in popSizeProlRange) {
                    parentIDs = sample(1:popSize, 2)
                    parents = sortedPopulation[parentIDs, ]

                    #crossOverPoint = sample(0:vars, 1)
                    #if (crossOverPoint == 0) {
                    #  newPopulation[child, ] = parents[2, ]
                    #  newEvalVals[child] = sortedEvaluations$x[parentIDs[2]]
                    #}
                    #else if (crossOverPoint == vars) {
                    #  newPopulation[child, ] = parents[1, ]
                    #  newEvalVals[child] = sortedEvaluations$x[parentIDs[1]]
                    #}
                    #else {
                    #  newPopulation[child, ] = c(parents[1, ][1:crossOverPoint], 
                    #    parents[2, ][(crossOverPoint + 1):vars])
                    #}

                    # verlangsamung um faktor 2
                    #crossOverPoints= sort( sample(1:vars, 2))
                    # # keine Fallunterscheidung, da die eltern bereits randomisiert sind

                    #newPopulation[child, ] = c( 
                    # parents[1, ][ 1:crossOverPoints[ 1]], 
                    # parents[2, ][ (1 + crossOverPoints[ 1]):crossOverPoints[ 2]],
                    # c( parents[1, ], NA)[ (1 + crossOverPoints[ 2]):( 1+ vars)]
                    # )[ -( 1+ vars)]

                    # crossing with 2 or more Points:
                    # ist sehr schnell:
                    # alle 3 Varianten sind akzeptabel, aber noch nicht hinreichend getestet
                    #crossOverPointPattern= runif( vars) < 0.5 #
                    #crossOverPointPattern= runif( vars) # 

                    crossOverPointPattern= # 
                     sin( runif( 1)*pi2+ ((1:vars)-1)/vars*pi2*runif( 1, min=0.5, max=1.5)) <=0

                    newPopulation[child, ] =
                     parents[1, ] * crossOverPointPattern +
                      parents[2, ] * ( 1 -  crossOverPointPattern)
                  }
                }
                else {
                  if (verbose) 
                    cat("  cannot crossover (#vars=1), using new randoms...\n")
                  print( "flag") # does it function?
                  newPopulation[ popSizeProlRange, ] =
                   sortedPopulation[ sample(1:popSize, popSizeProl), ]
                }
                population = newPopulation
                evalVals = newEvalVals
                if (mutationChance > 0) {
                  if (verbose) 
                    cat("  applying mutations... ")
                  mutationCount = 0

                  #optimierung ab hier
                  dempeningFactor = (iters - iter)/iters

                  if( 1)
                  {

                  mutationChanceM= matrix( runif( popSizeProlVars) < mutationChance,
                   nrow = popSizeProl, ncol = vars)

                  mutationDirectionM= matrix( sample( c( -1, 1), popSizeProlVars, replace=TRUE),
                   nrow = popSizeProl, ncol = vars)

                  mutationValV= ( stringMax - stringMin) * 0.67 # correction ? 1df67b3102d846f6b8ae02b0e5f63454
                  #mutationValV= stringMax - stringMin * 0.67 # old formula
                  mutationValM= matrix( mutationValV, byrow= TRUE,
                   nrow = popSizeProl, ncol = vars)

                  mutationM= population[ popSizeProlRange, ] +
                   mutationChanceM* mutationDirectionM * mutationValM * dempeningFactor

                  mutationM[ mutationM < stringMin]= stringMinProlM[ mutationM < stringMin]
                  mutationM[ mutationM > stringMax]= stringMaxProlM[ mutationM > stringMax]
                  population[ popSizeProlRange, ]= mutationM
                  evalVals[ popSizeProlRange]= NA
                  mutationCount = mutationCount + sum( mutationChanceM)
                  }
                  else
                  {
                  for (object in (elitism + 1):popSize) {
                    for (var in 1:vars) {
                      if (runif(1) < mutationChance) {
                        direction = sample(c(-1, 1), 1)
                        #mutationVal = stringMax[var] - stringMin[var] * 0.67 # old formula
                        mutationVal = ( stringMax[var] - stringMin[var]) * 0.67 # correction ? 1df67b3102d846f6b8ae02b0e5f63454
                        mutation = population[object, var] + 
                          direction * mutationVal * dempeningFactor
                        if (mutation < stringMin[var]) 
                          mutation = stringMin[var] + runif(1) * 
                            (stringMax[var] - stringMin[var])
                        if (mutation > stringMax[var]) 
                          mutation = stringMin[var] + runif(1) * 
                            (stringMax[var] - stringMin[var])
                        population[object, var] = mutation
                        evalVals[object] = NA
                        mutationCount = mutationCount + 1
                      }
                    }
                  }
                  }
                  if (verbose) 
                    cat(paste(mutationCount, "mutations applied\n"))
                }
            }
        }
    }
    result = list(type = "floats chromosome", stringMin = stringMin, 
        stringMax = stringMax, popSize = popSize, iters = iters, 
        suggestions = suggestions, population = population, elitism = elitism, 
        mutationChance = mutationChance, evaluations = evalVals, 
        best = bestEvals, mean = meanEvals)
    class(result) = "rbga"
    return(result)
}



