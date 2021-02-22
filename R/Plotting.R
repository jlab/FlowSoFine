#' Plot A 1- to 3-Dimensional CoreTemplate
#'
#' @param template The \code{\link{CoreTemplate}} to plot
#' @param sample The sample to plot
#'
#' @return Depending on the number of dimensions nothing, a ggplot2 object or a plotly object
#' @export
#' @method plot CoreTemplate
#'
#' @import plotly ggplot2
#'
#' @examples
plot.CoreTemplate <- function(template, sample) {
  points <- as.data.frame(template@coords)
  counts <- template@counts[,sample]
  channels <- colnames(points)
  #channels <- channels

  colours <- c("blue4", "cyan3", "darkgreen", "mediumseagreen",  "chartreuse1", "goldenrod" , "gold", "darkorange", "firebrick1", "firebrick4")

  if(length(channels) == 1) {

    plot(y = log10(counts), x = points[,1], xlab = channels, type = "b")

  } else if(length(channels) == 2) {

    ggplot(points, aes(x = points[,1], y = points[,2], fill = log10(counts))) +
      geom_raster() +
      scale_fill_gradientn(colours = colours) +
      labs(x = channels[1], y = channels[2])

  } else if(length(channels) == 3) {

    colnames(points) <- c("V1", "V2", "V3")
    counts <- log10(counts)

    points <- points[-which(counts<0),]
    counts <- counts[-which(counts < 0)]

    fig <- plot_ly(points, x = ~V1, y = ~V2, z = ~V3,
                   type = "scatter3d",
                   mode = "markers",
                   color = counts,
                   colors = colours)#1/length(counts)*4000)
    layout(fig,
           scene = list(
             xaxis = list(title = channels[1]),
             yaxis = list(title = channels[2]),
             zaxis = list(title = channels[3])
           )
    )
  }
}


#' Plot Every 1- Or 2-Dimensional Combination Of Axes Of A NDTemplate
#'
#' @param template The NDTemplate to plot
#' @param sample The sample to plot
#' @param dimen The number of axes to plot (1 or 2)
#'
#' @return
#' @export
#' @method plot NDTemplate
#' @importFrom gridExtra grid.arrange
#'
#' @examples
plot.NDTemplate <- function(template, sample, dimen = 1) {
  dimL <- template@templates[[dimen]]
  l <- ceiling(sqrt(length(dimL)))

  if(dimen == 1) {
    par(mfrow = c(l, l))
    for (i in dimL) plot(i, sample)
    par(mfrow=c(1, 1))
  } else if(dimen == 2) {
    glist <- lapply(dimL, plot, sample)
    grid.arrange(grobs = glist, nrow = l)
  }

}
