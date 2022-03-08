#' Get Table of Frequencies of every Bin's Event for Every Sample
#'
#' @param template A \code{\link{FSFTemplate}}
#'
#' @return a data.frame
#' @export
#'
#' @examples
frequencies <- function(template) {

  t(template@counts)/colSums(template@counts) * 100

}


#' Print Info about a FSFTemplate
#'
#' @param template A FSFTemplate
#'
#' @return
#' @export
#' @method print FSFTemplate
#'
#' @examples
print.FSFTemplate <- function(template, samples = 1) {

  cat("FSFTemplate with", template@nBins, "bins,",
      template@nSamples, "Samples and",
      length(template@channels), "channels. \n\n")

  nChannels = length(template@channels)

  for (sample in samples) {
    cat(colnames(template@counts)[sample], "[sample", sample, "/", template@nSamples,"]", "\n")
    cat("----------------\n")
    cat("coords", rep("\t", nChannels + 1),"counts \n")
    cat("------", rep("\t", nChannels + 1), "------ \n")
    cat(paste0(template@channels,"\t"), "\n")

    for (i in 1:3) {
      cat(unlist(template@coords[i, ]), "\t", template@counts[i, sample], "\n")
    }

    cat("//", rep("\t", nChannels + 1), "// \n")

    for (i in 3:1) {
      cat(unlist(template@coords[template@nBins - i, ]), "\t", template@counts[template@nBins - i, sample], "\n")
    }
    cat("\n")
  }

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

  chIndex <- which(template@channels %in% channels)
  channels <- template@channels[chIndex] #reorder channels correctly
  cnames <- colnames(template@counts)
  subPoints <- template@coords[,channels, drop = F]

  setDT(subPoints)
  subPoints <- cbind(subPoints, template@counts)

  dSized <- subPoints[,lapply(.SD, sum), by = channels]

  setorderv(dSized, channels)

  counts <- dSized[,..cnames]
  coords <- dSized[,..channels]

  #for the index

  maxBins <- expand.grid(template@dimen[chIndex])
  colnames(maxBins) <- channels
  setDT(maxBins)
  setkey(maxBins)
  index <- maxBins[coords, which = T, nomatch = NA]
  new("FSFTemplate",
      coords = as.data.frame(coords),
      counts = as.data.frame(counts),
      nSamples = template@nSamples,
      channels = colnames(coords),
      nBins = nrow(coords),
      resolution = template@resolution,
      dimen = template@dimen,
      index = index)

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

    #cat("====================== \n")
    #cat(factorComb[,i], "\n")
    ad <- adonis2(dMsubs ~ ., data = metaSubs)
    #print(ad)

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

#' Check which flow cytometry channels show significant difference between given metadata groups
#'
#' @param template A FSFTemplate
#' @param term The term to compute \code{\link{adonis2}} for
#' @param data The data from which to take the term
#' @param adjust p-value adjustment method for \code{\link{p.adjust}}
#' @param ... Additional arguments passed to \code{\link{adonis2}}
#'
#' @return printed output
#' @import vegan
#' @export
#'
#' @examples
singleChannel.adonis2 <- function(template, term, data, adjust = "bonferroni", ...) {

  meta <- data[,term, drop = F]
  channels <- colnames(template@coords)

  pv <- sapply(channels, function(channels) {
    temp <- shrink(template, channels)
    distM <- as.matrix(vegdist(frequencies(temp)))

    cc <- complete.cases(meta)
    metaCC <- meta[cc, term, drop = F]
    distM <- distM[cc, cc]

    if (!identical(meta, metaCC)) {
      warning("NAs removed in metadata")
    }

    ad <- adonis2(distM ~ ., data = metaCC, ...)$`Pr(>F)`[1]

  })

  pv <- p.adjust(pv, adjust)

  pv

}
