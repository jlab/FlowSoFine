#' Create a Matrix containing Weights for each Combination of Hexagons
#'
#' @param template A \code{\link{CoreTemplate}}
#' @param method "exp" for exponential, "disc" or anything else for discrete approach
#' @param gamma A Number controlling the speed of the decrease. Only applies when method = "exp".
#' @param val A vector containing weights for each ranked distance between hexagons. Only applies when method = "disc".
#'
#' @return A matrix containing weights for each combination of hexagons. To be used in \code{\link{weightedBray}}.
#' @importFrom dplyr dense_rank
#' @export
#'
#' @examples
weightMatrix <- function(template, method = "exp", gamma = 8, val = c(.5,.25,.25)) {

  coords <- template@coords
  nBins <- nrow(template@coords)

  euclDist <- as.matrix(dist(coords, method = "euclidean"))


  if(method == "exp") {

    w <- exp(-1*gamma*euclDist)

  } else {

    w <- as.vector(round(euclDist, 3))
    w <- dplyr::dense_rank(w)
    w <- matrix(w, ncol = nBins, nrow = nBins)


    w2 <- w
    w[w2 > length(val)] <- 0
    for (i in 1:length(val)) {
      w[w2 == i] <- val[i]
    }

  }

  w

}

#' Compute Weighted Bray Distance between Samples of a CoreTemplate
#'
#' @param template A \code{\link{CoreTemplate}} Object
#' @param gamma A positive number specifying the drop off of the weight matrix when w = NA
#' @param w Optionally you may provide your own weight matrix created with \code{\link{weightMatrix}}
#'
#' @return A distance matrix
#' @export
#'
#' @examples
weightedBray <- function(template, gamma = 8, w = NA) {
  #computes weighted bray distance between samples
  #if w is undefined then w = exp(-1*gamma*euclDist)

  h <- t(frequencies(template))

  if(is.na(w)) {
    w <- weightMatrix(template, gamma = gamma)
  }

  len <- ncol(template@counts)
  d <- matrix(nrow = len, ncol = len)

  for(i in 1:len) {
    for(j in 1:len) {

      d[i,j] <- sum(w*abs(outer(h[,i], h[,j], FUN = "-"))) / sum(w*outer(h[,i], h[,j], FUN = "+"))

    }
  }

  as.dist(d)

}
