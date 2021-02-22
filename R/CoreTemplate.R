#' The S4 CoreTemplate class
#'
#' @slot coords A data.frame containing the coordinates of every bin
#' @slot counts A data.frame containing the number of events for every bin
#'
#' @return
#' @export
#'
#' @examples
setClass("CoreTemplate",
         representation(coords = "data.frame", counts = "data.frame"))



#' Create A CoreTemplate Object
#'
#' @param flowset A flowCore \code{flowSet}
#' @param channels A vector of channel names
#' @param resolution A number specifying the number of bins on one axis
#'
#' @return A CoreTemplate
#' @export
#'
#' @import flowCore data.table dplyr
#'
#' @examples
CoreTemplate <- function(flowset, channels, resolution = 4) {

  #turn flowset environment into list and reorder alphabetically
  fl <- as.list(flowset@frames)
  fl <- fl[order(names(fl))]

  nDim <- length(channels)


  #asinh instead?
  logD <- lapply(fl, function(x) as.data.frame(log10(x@exprs[, channels])))

  #filter out -Inf
  logD <- lapply(logD, function(x) {
    x[x < 0] <- 0
    x
  })

  channelAll <- dplyr::bind_rows(logD)

  rangeV <- apply(channelAll, 2, range)

  dimensions <- lapply(1:ncol(rangeV), function(x) {
    d <- seq(rangeV[1,x], rangeV[2,x], length.out = resolution)
    d <- d + (min(dist(d))/2)
  })

  maxBins <- expand.grid(dimensions)
  setDT(maxBins)
  setkey(maxBins)
  colnames(maxBins) <- channels



  full <- list()
  coords <- data.frame()

  g <- 1
  for(z in logD) {
    cat("\r","Processing sample",g,"/",length(logD))
    g = g + 1


    ldList <- as.list(z)


    gated <- sapply(1:nDim, function(i) {
      dt1 <- data.table(dimensions[[i]])
      setkey(dt1)

      dt2 <- data.table(ldList[[i]])

      ind <- dt1[dt2, roll = "nearest", which = T] #rolling join, get indices of closest bins
      dt1[ind]

    })



    setDT(gated)
    ind2 <- maxBins[gated, which = T]
    tab <- data.table(table(ind2))
    tab$ind2 <- as.integer(tab$ind2)

    binCount <- rep(0, resolution^nDim)

    binCount[tab$ind2] <- tab$N

    gates_0 <- which(binCount == 0)
    coords2 <- maxBins[-gates_0]
    binCount <- binCount[-gates_0]
    binCount

    #dynamically remove 0 bins
    if (length(full) == 0) {
      full <- list(binCount)
      coords <- coords2
    } else {

      uc <- union(coords, coords2)
      j <- uc[coords2, on = .NATURAL, which = T]
      j1 <- uc[coords, on = .NATURAL, which = T]

      nb <- rep(0, nrow(uc))
      nb[j] <- binCount

      full <- lapply(full, function(x) {
        nb <- rep(0, nrow(uc))
        nb[j1] <- x
        nb
      })

      coords <- uc
      full <- c(full, list(nb))
    }

  }


  counts <- as.data.frame(full)
  colnames(coords) <- channels
  colnames(counts) <- names(fl)

  new("CoreTemplate", coords = coords,
      counts = counts)
}
