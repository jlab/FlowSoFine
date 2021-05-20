#' Get Table of Frequencies of every Bin's Event for Every Sample
#'
#' @param template A \code{\link{FSFTemplate}}
#'
#' @return a data.frame
#' @export
#'
#' @examples
frequencies <- function(template) {

  f <- t(sapply(template@counts, function(x) x/sum(x)*100 ))

}


#' Create lower Dimension FSFTemplate from higher Dimension FSFTemplate through Addition.
#'
#' @param template A FSFTemplate
#' @param channels Channels of the new FSFTemplate
#'
#' @return A FSFTemplate
#' @import data.table
#' @export
#'
#' @examples
shrink <- function(template, channels) {
  cnames <- colnames(template@counts)
  subPoints <- template@coords[,channels, drop = F]

  setDT(subPoints)
  subPoints <- cbind(subPoints, template@counts)

  dSized <- subPoints[,lapply(.SD, sum), by = channels]
  counts <- dSized[,..cnames]
  coords <- dSized[,..channels]

  new("FSFTemplate", coords = as.data.frame(coords),
      counts = as.data.frame(counts))
}

#' Simplified pairwise adonis2 for only one Term
#'
#' @param distM A distance Matrix
#' @param term The term to compute \code{\link{adonis2}} for
#' @param data The data from which to take the term
#' @param adjust p-value adjustment method for \code{\link{p.adjust}}
#'
#' @return Printed Output
#' @export
#' @importFrom vegan adonis2
#'
#' @examples
pw.adonis2 <- function(distM, term, data, adjust = "bonferroni") {
  meta <- data[,term, drop = F]

  factors <- unique(meta)
  factorComb <- combn(factors[,1], m = 2)

  dM <- as.matrix(distM)
  pv <- sapply(1:ncol(factorComb), function(i) {
    subs <- meta == factorComb[,i][1] | meta == factorComb[,i][2]
    dMsubs <- as.dist(dM[subs, subs])
    metaSubs <- meta[subs,1,drop = F]

    cat("====================== \n")
    cat(factorComb[,i], "\n")
    ad <- adonis2(dMsubs ~ ., data = metaSubs)
    print(ad)

    ad['Pr(>F)'][1,1]
  })

  pv <- p.adjust(pv, method = adjust)
  cat("====================== \n")
  cat("Adjusted p-values ", "(",adjust,")","\n")
  for(i in 1:ncol(factorComb)) {
    cat("-----------------\n")
    cat(factorComb[,i],": ", pv[i], "\n")
  }


}
