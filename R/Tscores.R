#' Get t-scores according to Welch's t-statistic for a FSFTemplate and a Metadata Group
#'
#' @param template A FSFTemplate Object
#' @param group A vector of metadata
#' @param rev Reverse group ordering
#'
#' @return A DataFrame of t-scores
#' @export

#' @examples
tscores <- function(template, group, reverse = FALSE) {

  relative <- t(frequencies(template))
  treatment <- as.factor(group)


  gr <- expand.grid(unique(treatment), unique(treatment))
  gr <- gr[!duplicated(t(apply(gr, 1, sort))), ]
  gr <- gr[gr[,1] != gr[,2],]

  if(reverse) gr <- rev(gr)

  ts <- apply(gr, 1, function(x) {
    s1mean <- apply(relative[,which(treatment == x[1])],1,mean)
    s2mean <- apply(relative[,which(treatment == x[2])],1,mean)

    v1 <- apply(relative[,which(treatment == x[1])],1,var)
    v2 <- apply(relative[,which(treatment == x[2])],1,var)

    N1 <- ncol(relative[,which(treatment == x[1])])
    N2 <- ncol(relative[,which(treatment == x[2])])


    (s1mean - s2mean)/sqrt((v1/N1)+(v2/N2))

  })


  colnames(ts) <- paste(gr[,1],gr[,2], sep = "-")
  ts

}
