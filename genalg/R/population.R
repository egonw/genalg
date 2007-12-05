require(methods) || stop("Couldn't load package methods")

setClass("population",
  representation(
    type = "character",
    size = "numeric",
    bits = "numeric",
    chromosomes = "matrix",
    evals = "vector",
    generation = "numeric"
  ),
  prototype(
    type = "binary",
    size = 0,
    bits = 0,
    chromosomes = matrix(nrow=0, ncol=0),
    evals = c(),
    generation = 0
  )
)

population <- function(type="binary", size=100, bits=10, initFunc=NA, ...) {
    if (!(type %in% c("binary", "float", "integer"))) {
        stop("The type must be one of: binary, float, integer");
    }

    if (is.null(initFunc)) {
        if (type == "binary") {
            stop("An initialization function must be provided.");
        }
    }

    object <- new("population")

    object@type <- type
    object@size <- size
    object@bits <- bits
    object@chromosomes <- initFunc(size, bits)
    object@evals = rep(0,bits)
    object@generation = 0

    return(object)
}
