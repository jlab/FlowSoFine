#' The S4 FSFTemplate class
#'
#' @slot coords A data.frame containing the coordinates of every bin
#' @slot counts A data.frame containing the number of events for every bin
#'
#' @return
#' @export
#'
#' @examples
setClass("FSFTemplate",
         representation(coords = "data.frame",
                        counts = "data.frame",
                        index = "vector",
                        nSamples = "integer",
                        channels = "vector",
                        nBins = "numeric",
                        resolution = "numeric",
                        dimen = "list"))



#' Create A FSFTemplate Object
#'
#' @param flowset A flowCore \code{flowSet}
#' @param channels A vector of channel names
#' @param resolution A number specifying the number of bins on one axis
#'
#' @return A FSFTemplate
#' @export
#'
#' @import flowCore data.table
#'
#' @examples
FSFTemplate <- function(flowset, channels, resolution = 4, transformation = log10, verbose = T) {


  if(is.null(transformation)) {
    transformation <- function(x) {
      return(x)
    }
  }


  #turn flowset environment into list and reorder alphabetically
  fl <- as.list(flowset@frames)
  fl <- fl[order(names(fl))]

  nDim <- length(channels)


  if(verbose) cat("Transforming and subsetting flowSet\n")
  logD <- lapply(fl, function(x) as.data.frame(transformation(x@exprs[, channels])))

  #filter out -Inf
  logD <- lapply(logD, function(x) {
    x[x < 0] <- 0
    x
  })

  channelAll <- do.call(rbind, logD)

  if(verbose) cat("Building coordinates for FSFtemplate with",
                  length(channels),
                  "dimensions and a resolution of", resolution, "\n")

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
  coords <- data.table()



  g <- 1
  for(z in logD) {
    if (verbose) cat("\r","Processing sample",g,"/",length(logD))
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
    #indexCount <- rep(0, resolution^nDim)

    binCount[tab$ind2] <- tab$N
    #indexCount[tab$ind2] <- tab$ind2

    gates_0 <- which(binCount == 0)


    #coords for grids with > 0 entries
    if (length(gates_0) == 0) {
      coords2 <- maxBins
      #binCount <- binCount
    } else {
      coords2 <- maxBins[-gates_0]
      binCount <- binCount[-gates_0]
      #indexCount <- indexCount[-gates_0]
    }


    #dynamically remove 0 bins for decreased RAM usage
    if (length(full) == 0) {
      #add first sample to the list of counts
      full <- list(binCount)
      #coords at this point have all 0 gates removed
      coords <- coords2
    } else {

      #coords in the new sample without 0 gates (coords2) plus coords in all other samples (coords)
      uc <- funion(coords, coords2)


      #return indices of new sample's entries in combined coords
      j <- uc[coords2, on = .NATURAL, which = T]
      #return indices of the other samples' entries in combined coords
      j1 <- uc[coords, on = .NATURAL, which = T]

      nb <- rep(0, nrow(uc))
      nb[j] <- binCount

      #shift all of the other samples' entries to accommodate the new sample
      full <- lapply(full, function(x) {
        nb <- rep(0, nrow(uc))
        nb[j1] <- x
        nb
      })

      coords <- uc
      full <- c(full, list(nb))
    }

  }




  #sorting rows by channel coordinates
  setDT(coords)
  setDT(full)
  colnames(coords) <- channels
  sNames <- names(fl)
  colnames(full) <- sNames




  dt <- cbind(coords, full)

  setorderv(dt, channels)

  index <- maxBins[dt[,..channels], which = T]



  if(verbose) cat("\n")


  new("FSFTemplate",
      coords = as.data.frame(dt[, ..channels]),
      counts = as.data.frame(dt[, ..sNames]),
      nSamples = length(fl),
      channels = channels,
      nBins = nrow(coords),
      resolution = resolution,
      index = index,
      dimen = dimensions)
}
