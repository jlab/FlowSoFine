#' Calculates semivariograms for a grid resolution (by FSFTemplate) for all available individuals and models them successively by the given models
#'
#' The NSR = nugget-sill-ratio. The NSR takes values in the interval [0,1].
#' The smaller the value, the smaller is the loss of information due to measurement errors, or due to the accumulation of data in range units (e.g. by a grid).
#' The smaller the NSR, the greater the spatial dependence of the data. The MNSR is the mean value of the NSRs generated for each individual.
#' Note: The NSR depends, among other things, on the grid structure, the generated distance intervals in the semivariogram and the used models!
#'
#' @param modells Character vector that can include the following models: "gaussian", "exponential", "cauchy", "circular", "cubic", "matern", "spherical", "linear", "gneiting", "wave".
#' @param template FSFTemplate object
#' @param min Integer. Minimum number of events in an interval. If an interval does not meet the minimum number, it is merged with the previous interval. If the first interval does not meet the minimum number, it will be merged with the next interval that meets the minimum number.
#' @param endCut Numeric value. A distance specification from which values are forced into an interval in the semivariogram.
#' @param cores Integer. Threads to use.
#'
#' @return List containing \enumerate{
#' \item ergValues - Dataframe with best model, nugget, sill, ML value and NSR value.
#' \item MNSR - Mean NSR-Value
#' \item MML - Mean maximum likelihood value
#' }
#' @export
#' @import geoR parallel
#'
#' @examples
vario_func <- function(modells, template, min = 20, endCut = NULL, cores = 1){

  coords <- template@coords
  dc <- dist(coords)

  # Berechnung kuerzeste Distanz der Felder:
  shortestDistance <- min(dc)
  l <- shortestDistance / 2.1 # Intervallbreite soll kleiner als 1/2 * Spanne
  # des raumlichen Feldes sein.

  # Intervallberechnung
  breaks <- seq(0, max(dc), by = l)
  tableBreaks <- table(cut(dc, breaks))
  breaks <- breaks[sapply(tableBreaks, function(x){
    if(x > min) return(TRUE)
    return(FALSE)
  })]
  if(length(breaks) == 0) stop(
    "Not enough data for building a semivariogram!
  Increase the resolution, which is recommend,
  or lower the minimum of allowed values of the
  intervals of distance by adjusting 'min'.")
  maxDist <- max(breaks)
  if(!is.null(endCut)){
    breaks <- breaks[which(breaks == maxDist | breaks < endCut)]
  }

  # Erstellung Semivariogramm
  variogObj <- variog(coords = coords,
                      data = template@counts, breaks = breaks,
                      max.dist = maxDist)

  # Modellanpassungen an das Semivariogramm fuer ausgewaehlte Individuen.
  onefit <- function(i){

    maxML <- -Inf
    bestMethod <- NA_real_
    minNSR <- 1

    for(j in seq_along(modells)){
      x <- variofit(variogObj, cov.model = modells[j],  fix.nugget = FALSE,
                    simul.number = i, messages = FALSE)

      ML <- -Inf # Falls Maximum-Likelihood nicht berechenbar, als -Inf setzen.
      try(ML <- loglik.GRF(obj.model = x, coords = coords,
                           data = template@counts[, i],  cov.pars = x$cov.pars,
                           nugget = x$nugget, cov.model = x$cov.model))


      if(maxML < ML){
        maxML <- ML
        bestMethod <- modells[j]
        minNSR <- min(x$nugget / x$cov.pars[1], 1)
      }
    }

    output <- list(maxML = maxML, bestMethod = bestMethod, minNSR = minNSR)
    return(output)
  }

  # Paralellisierung
  if(cores > 1){
    cluster <- makeCluster(cores)
    clusterEvalQ(cluster, {
      library("geoR")
    })
    clusterExport(cluster, c("modells", "template", "min", "onefit",
                             "variogObj"), envir = environment())

    ergValues <- as.data.frame(do.call(rbind, parLapply(cluster,
                                                        1:ncol(template@counts),
                                                        onefit)))
    stopCluster(cluster)
  } else {
    ergValues <- as.data.frame(do.call(rbind, lapply(1:ncol(template@counts),
                                                     onefit)))
  }

  # Endergebnisse
  MNSR <- mean(unlist(ergValues$minNSR))
  MML <- mean(unlist(ergValues$maxML))

  output <- list(ergValues = ergValues, MNSR = MNSR, MML = MML)
  return(output)
}

#' Applies vario_func for an arbitrary number of grid resolutions
#'
#' @param flowset A \code{\link{flowSet}}
#' @param channels Vector of flow cytometry channels
#' @param resolutions numerical vector of grid resolutions to check
#' @param transformation Transformation function passed to \code{\link{FSFTemplate}}
#' @param verbose Boolean. Passed to \code{\link{FSFTemplate}}
#' @param modells Character vector passed to \code{\link{vario_func}}
#' @param min Integer. Minimum number of events in an interval. Passed to \code{\link{vario_func}}
#' @param endCut Numerical value. Passed to \code{\link{vario_func}}
#' @param cores Integer. Threads to use.
#'
#' @return Data.frame containing \enumerate{
#' \item resolution - FSFTemplate grid resolution
#' \item MNSR - Mean NSR-Value
#' \item MML - Mean maximum likelihood value
#' }
#' @export
#'
#' @examples
vario_nsr <- function(flowset, channels, resolutions, transformation = log10,
                      verbose = TRUE, modells, min = 20, endCut = NULL,
                      cores = 1){

  stopifnot(is.numeric(resolutions), resolutions %% 1 == 0, resolutions > 0,
            is.numeric(min), min %% 1 == 0, min >= 0,
            is.numeric(cores), cores %% 1 == 0, cores > 0)

  help_func <- function(resolution){
    template <- FSFTemplate(flowset = flowset,
                            channels = channels,
                            resolution = resolution,
                            transformation = transformation,
                            verbose = verbose)
    erg <- vario_func(modells = modells, template = template, min = min,
                      endCut = endCut, cores = cores)
    df <- data.frame(resolution = resolution, MML = erg$MML, MNSR = erg$MNSR)
    str <- paste("Variogram of resolution", resolution, "finished!")
    print(str)
    return(df)
  }

  output <- do.call(rbind, lapply(resolutions, help_func))

  return(output)
}

#' Graphical representation of the calculated MNSRs over the respective grid resolutions. Highlighting of the optimal MNSR value.
#'
#' @param obj The output of \code{\link{vario_nsr}}
#' @param ylimit Numerical value. y-axis limit
#' @param xtext Numerical value. x-coordinate for the text within the graphic, which marks the optimal MNSR value. Default is NULL, so the value is automatically set depending on the position of the optimal MNSR_value.
#' @param ytext Numerical value. Analogous to xtext
#' @param legend Position of the legend. See \code{\link{legend}}. Default is "topright".
#'
#' @return A graph showing the MNSR values of \code{\link{vario_nsr}} depending on the respective grid resolution. In addition, the optimal MNSR value is shown on the corresponding grid resolution with a red line with the lettering which indicates the respective grid resolution.
#' @export
#'
#' @examples
plot_nsr <- function(obj, ylimit = NULL, xtext = NULL, ytext = NULL,
                     legend = "topright"){
  if(is.null(ylimit)) ylimit <- max(obj$MNSR) + 0.05

  plot(x = obj$resolution, y = obj$MNSR, xlab = "Grid Resolution",
       ylab = "MNSR",
       main = "MNSR for several grid resolutions", type = "b", pch = 16,
       ylim = c(0, ylimit))

  optimalnsr <- obj$resolution[which.min(obj$MNSR)]
  abline(v = optimalnsr, col = "red")

  # Textkoordinaten setzen
  if(is.null(xtext)){
    if(optimalnsr > tail(obj$resolution, 1)/2){
      xtext <- optimalnsr - 1
    } else {
      xtext <- optimalnsr + 1
    }
  }
  if(is.null(ytext)){
    ytext <- (max(obj$MNSR) - min(obj$MNSR)) / 2
  }
  text(xtext, ytext, paste("resolution =", optimalnsr), col = "red")

  legend(legend, legend = c("MNSR", "Optimal MNSR"),
         col = c("black", "red"), lty = 1, pch = c(16, NA))
}
