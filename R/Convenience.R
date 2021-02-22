#' Get Table of Frequencies of every Bin's Event for Every Sample
#'
#' @param template A \code{\link{CoreTemplate}}
#'
#' @return a data.frame
#' @export
#'
#' @examples
frequencies <- function(template) {

  t(sapply(template@counts, function(x) x/sum(x)*100 ))
}


#' Extract a CoreTemplate from a NDTemplate based on Channel Names
#'
#' @param template A \code{\link{NDTemplate}}
#' @param channels A vector of channel names
#'
#' @return A \code{\link{CoreTemplate}}
#' @export
#'
#' @examples
getCT <- function(template, channels) {
  dList <- template@templates[[length(channels)]]

  ind <- 0
  for(i in 1:length(dList)) { #look for correct channel configuration
    if(identical(sort(colnames(dList[[i]]@coords)), sort(channels))) {
      ind <- i
      break
    }
  }

  template@templates[[length(channels)]][[ind]]

}
