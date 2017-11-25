#'
#' Multidimensional multiple changepoint algorithm with pruning
#'
#' @description Dynamic programming algorithm in multiple dimensions with pruning
#' @param file the complete path + file name of the data to use
#' @param beta the penalty coefficient
#' @param type an integer equal to 0,1,2 or 3. Complexity of the pruning. (see function pruning). If type = i, we use pruning method from 1 to i
#' @param show an integer. Following the actual iteration during computation. Display every 'show' iteration.
#' @return A multiSeg object = (chgpt, means, nb, type, delay).
#' With n the number of observations, all these elements are vector of size n except means which is a matrix of size nxp.
#' 'Chgpt' is a vector of last changepoint at each position.
#' 'mean' is the matrix of means on each last segment at each position.
#' 'nb' is a vector whose elements are the number of present lastchangepoint candidates at each position.
#' 'type' is a vector saving how the candidate was pruned
#' 'delay' is the number of iterations before pruning for each data point.
#' @examples
#' n <- 100
#' var <- 1
#' means <- matrix(c(0,1,2,1,0,1,2,2,0,0,1,1),3,4)
#' changes <- c(0.3,0.6,1)
#' data <- dataG(n,means,changes, var)
#' file = paste(getwd(),"/dataG.txt",sep = "")
#' multiSeg(file,4*2*log(n),1)

multiSeg <- function(file, beta, type = 3, show = 0){

  res <- transfert(file, beta, type, show)
  res$means <- do.call(rbind, res$means)

  response <- list(chgpt = res$lastChangePoint, means = res$means, nb = res$nbSet, type = res$pruningType, delay = res$pruningDelay)

  attr(response, "class") <- "multiSeg"

  return(response)
}