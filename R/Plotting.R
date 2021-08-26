#' Plot A 1- to 3-Dimensional FSFTemplate
#'
#' @param template The \code{\link{FSFTemplate}} to plot
#' @param sample The sample to plot
#' @param limits Limits of the fill aesthetic
#' @param ... Additional arguments passed to ggplot (1D) or geom_raster (2D)
#'
#' @return A ggplot2 object
#' @export
#' @method plot FSFTemplate
#'
#' @import ggplot2
#'
#' @examples
plot.FSFTemplate <- function(template, sample = NA, z = NULL, limits = NULL, ...) {

  points <- as.data.frame(template@coords)

  if(!is.na(sample)) {
    counts <- template@counts[,sample]
    sName <- colnames(template@counts[,sample, drop = F])
  }
  channels <- colnames(points)

  colours <- c("blue4", "cyan3", "darkgreen", "mediumseagreen",  "chartreuse1", "goldenrod" , "gold", "darkorange", "firebrick1", "firebrick4")

  if(length(channels) == 1) {

    if(is.na(sample)) {

      ggplot(points, aes(x = points[,1], ...)) +
        geom_point() +
        geom_line() +
        theme_minimal() +
        labs(x = channels) +
        coord_flip()

    } else {

      ggplot(mapping = aes(y = log10(counts), x = points[,1])) +
        geom_bar(stat = "identity", aes(fill = log10(counts))) +
        geom_point() +
        geom_line() +
        scale_fill_gradientn(colours = colours, limits = limits) +
        coord_flip() +
        theme_minimal() +
        labs(x = channels, title = sName)

    }

  } else if(length(channels) == 2) {

    if(is.na(sample)) {

      ggplot(points, aes(x = points[,1], y = points[,2])) +
        geom_raster(...) +
        theme_minimal() +
        labs(x = channels[1], y = channels[2])

    } else {

      ggplot(points, aes(x = points[,1], y = points[,2], fill = log10(counts))) +
        geom_raster(...) +
        scale_fill_gradientn(colours = colours, limits = limits) +
        labs(x = channels[1], y = channels[2], fill = "log10(Events)", title = sName) +
        theme_minimal()

    }



  } else if(length(channels) == 3) {

    colormap(template, sample, z, limits = limits)

  }
}

#' Plot a pseudo 3-dimensional Plot with the mean z Channel Coordinate as fill Color
#'
#' @param template A 3-dimensional FSFTemplate
#' @param sample The sample to plot
#' @param z The channel that should be used as fill color
#'
#' @return A ggplot2 plot
#' @export
#'
#' @import ggplot2 data.table
#'
#'
#' @examples
colormap <- function(template, sample, z = NULL, limits = NULL) {
  channels <- colnames(template@coords)

  if(length(channels) != 3) stop("Template has to contain exactly 3 channels")

  if(is.null(z)) z <- channels[3]

  channels <- channels[-which(channels == z)]


  temp2d <- shrink(template, channels)


  subPoints <- template@coords[,channels, drop = F]

  zChannel <- template@coords[,z]

  zcounts <- template@counts
  zcounts <- zcounts * zChannel

  zPoints <- cbind(subPoints, zcounts)
  setDT(zPoints)


  subPoints <- cbind(subPoints, template@counts)
  setDT(subPoints)



  #get sum of events
  dSized <- subPoints[,lapply(.SD, sum), by = channels]

  #get sum of events multiplied by z channel coordinates
  zSized <- zPoints[,lapply(.SD, sum), by = channels]

  #get mean coordinates of z channel
  zMean <- as.data.frame(zSized[,3:ncol(zSized)]/dSized[,3:ncol(dSized)])

  #colours <- c("blue4", "cyan3", "darkgreen", "mediumseagreen",  "chartreuse1", "goldenrod" , "gold", "darkorange", "firebrick1", "firebrick4")

  plot(temp2d, mapping = aes(fill = zMean[,sample])) +
    #scale_fill_gradientn(colours = colours, limits = limits) +
    #scale_fill_gradient(low = "darkslateblue", high = "skyblue1", limits = limits) +
    labs(title = colnames(zMean)[sample], fill = paste0("mean(",z,")"))

}


#' Plot A flowCore flowFrame
#'
#' @param flowFrame A \code{\link{flowCore}} flowFrame
#' @param channels A vector of channel names
#' @param resolution Resolution of the plot
#' @param transformation Transformation. Default is log10. transformation = NULL for no transformation
#' @param ... Additional arguments passed to plot.FSFTemplate
#'
#' @return A ggplot2 plot
#' @export
#' @importFrom flowCore flowSet
#' @importFrom flowCore identifier
#'
#' @examples
plotFF <- function(flowFrame, channels, resolution = 50, transformation = log10, ...) {
  if(length(channels) > 3) stop("Too many channels. Only 1 - 3 channels supported.")

  ct <- FSFTemplate(flowSet(flowFrame),
                     channels,
                     resolution = resolution,
                     transformation = transformation,
                     verbose = F)
  plot(ct, 1, ...) + labs(title = identifier(flowFrame))
}

#' Plot A flowCore flowFrame (3 channels)
#'
#' @param flowFrame A \code{\link{flowCore}} flowFrame
#' @param channels A vector of length 3 containing the channel names
#' @param z The channel that should be used as fill color
#' @param resolution Resolution of the plot
#' @param transformation Transformation. Default is log10. transformation = NULL for no transformation
#' @param ... Additional arguments passed to plot.FSFTemplate
#'
#' @return A ggplot2 plot
#' @export
#' @importFrom flowCore flowSet
#'
#' @examples
colormapFF <- function(flowFrame, channels, z = NULL, resolution = 50, transformation = log10, ...) {
  ct <- FSFTemplate(flowSet(flowFrame),
                    channels,
                    resolution = resolution,
                    transformation = transformation,
                    verbose = F)

  colormap(ct, 1, z = z, ...) + labs(title = identifier(flowFrame))

}


#' Plot t-scores in 1 or 2 Dimensions
#'
#' @param template A FSFTemplate Object
#' @param ts A vector of t-scores
#' @param limits Limits of the color scale
#'
#' @return A ggplot Object
#' @export
#' @import ggplot2
#' @examples
plotTscores <- function(template, ts, limits = NULL) {
  channels <- colnames(template@coords)

  if(length(channels) == 2) {

    plot(template, mapping = aes(fill = ts)) +
      scale_fill_gradient2(low = "blue", high = "red", limits = limits) +
      labs(title = colnames(ts))

  } else if(length(channels) == 1) {

    plot(template, y = ts, fill = ts)+
      geom_bar(stat = "identity") +
      scale_fill_gradient2(low = "blue", high = "red", limits = limits) +
      labs(title = colnames(ts), y = "t-scores")

  }

}
