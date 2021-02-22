#' The S4 NDTemplate class
#'
#' @slot templates A list of combinations of \code{CoreTemplate}s. Sorted by number of dimensions.
#' @slot metadata A metadata table
#' @slot resolution The number of bins on one axis
#' @slot nSamples The number of samples
#'
#' @return
#' @export
#'
#' @examples
#'
setClass("NDTemplate",
         representation(templates = "list",
                        metadata = "data.frame", resolution = "numeric", nSamples = "numeric"))


#' Create A NDTemplate Object
#'
#' @param flowset A flowCore \code{flowSet}
#' @param channels A vector of channel names
#' @param resolution A number specifying the number of bins on one axis
#' @param metadata A metadata table
#'
#' @return A NDTemplate
#' @export
#'
#' @examples
#'
NDTemplate <- function(flowset, channels, resolution = 4, metadata = data.frame()) {

  comb <- lapply(1:(length(channels)-1), function(x) combn(channels, x))

  templates <- lapply(rev(comb), function(y) {

    apply(y, 2, function(x) {

      CoreTemplate(flowset, x, resolution)

    })

  })

  templates <- c(templates, list(list(CoreTemplate(flowset, channels, resolution))))

  nSamples <- length(flowset)

  new("NDTemplate", templates = templates,
      metadata = metadata,
      resolution = resolution,
      nSamples = nSamples)

}
