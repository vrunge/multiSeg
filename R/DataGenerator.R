#'
#' Multidimensional Gaussian Data Generator
#'
#' @description Simulating Multidimensional Gaussian Data with a model of segmentation, that is a given matrix of means and a vector of changepoints.
#' @param n number of obsersations to generate.
#' @param means matrix of means, the number of columns = number of variables = number of dimensions. The i-th raw corresponds tp the i-th segment mean in all dimensrions.
#' @param chgpt vector of changepoints (an increasing sequence of integers).
#' @param sigma standard deviation of the gaussian model
#' @return The matrix of simulated data + the matrix is saved in the file dataG.txt at the current path (obtained by getwd())
#' @examples
#' data <- dataG(500, matrix(c(0,1,2,1,0,1,2,2,0,0,1,1),3,4),c(0.3,0.6,1))
#' head(data)

dataG <- function(n, means, chgpt, sigma = 1){
  p <- dim(means)[2]
  M <- matrix(0,n,p)
  for (i in 1:n){
    index <- which(chgpt>=i/n)[1]
    M[i,] <- rnorm(p,means[index,],sigma)
  }
  write.table(M, file='dataG.txt', row.names=FALSE, col.names=FALSE)
  return(M)
}
