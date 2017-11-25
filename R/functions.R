#'
#' Plots of the pruning path
#'
#' @description Displaying the properties of the pruning
#' @param res A multiSeg object (obtained with function multiSeg)
#' @return a plot with 3 graphics. First plot = number of candidates (nb)
#' Second plot (in red) = delay before pruning for each candidate (delay)
#' Third plot = cumulative sums for all type of pruning (0, 1, 2 or 3).
#' type = 0 (black curve): no pruning
#' type = 1 (red curve): radius < 0
#' type = 2 (green curve): intersection = emptyset
#' type = 3 (blue curve): candidate hidden by one sphere of the past
#' @examples
#' n <- 500
#' var <- 1
#' means <- matrix(c(0,1,2,1,0,1,2,2,0,0,1,1),3,4)
#' changes <- c(0.3,0.6,1)
#' data <- dataG(n,means,changes, var)
#' file = paste(getwd(),"/dataG.txt",sep = "")
#' res <- multiSeg(file,4*2*log(n),1)
#' pruning(res)
#' res$type

pruning <- function(res){

  par(mfrow=c(3,1),mai=c(0.3,0.4,0.1,0.4))

  n <- length(res$nb)
  plot(1:n,res$nb, type ='l', xlab = "", ylab = "")

  maxi = max(res$delay)
  res$delay[res$delay==-1] = -maxi

  plot(1:n,res$delay, type ='l', col = 2, xlab = "", ylab = "")

  maxi = max(table(res$type))
  if(sum(res$type == 0) != 0){
    c0 <- cumsum(res$type == 0)
    plot(1:n,c0,type = 'l', col = 1, xlab = "", ylab = "",ylim = c(0,maxi))
    par(new = TRUE)
    }
  if(sum(res$type == 1) != 0){
    c1 <- cumsum(res$type == 1)
    plot(1:n,c1,type = 'l', col = 2, xlab = "", ylab = "",ylim = c(0,maxi))
    par(new = TRUE)
    }
  if(sum(res$type == 2) != 0){
    c2 <- cumsum(res$type == 2)
    plot(1:n,c2,type = 'l', col = 3, xlab = "", ylab = "",ylim = c(0,maxi))
    par(new = TRUE)
    }
  if(sum(res$type == 3) != 0){
    c3 <- cumsum(res$type == 3)
    plot(1:n,c3,type = 'l', col = 4, xlab = "", ylab = "",ylim = c(0,maxi))
    }
  par(new = FALSE)

}

#'
#' Screening of the segmentation
#'
#' @description Display the segments and changepoints
#' @param file the path + name of the data file
#' @param res A multiSeg object (obtained with function multiSeg)
#' @examples
#' n <- 500
#' var <- 1
#' means <- matrix(c(0,1,2,1,0,1,2,2,0,0,1,1),3,4)
#' changes <- c(0.3,0.6,1)
#' dataG(n,means,changes, var)
#' file = paste(getwd(),"/dataG.txt",sep = "")
#' res <- multiSeg(file,4*2*log(n),1)
#' segmentation(file,res)

segmentation <- function(file, res){
  p <- dim(res$means)[2]
  n <- dim(res$means)[1]

  chgpt <- res$chgpt[n]
  while(chgpt[1] != 0){chgpt = c(res$chgpt[chgpt[1]-1],chgpt)}

  chgpt <- c(chgpt,n)

  par(mfrow=c(p,1),mai=c(0.3,0.4,0.1,0.4))

  data <- read.table(file)

  for(i in 1:p){
    mini <- min(data[,i])
    maxi <- max(data[,i])
  plot(1:n,data[,i], type ='l', xlab = "", ylab = "",ylim = c(mini,maxi))
    abline(v = chgpt, col = 2, lwd = 2)
    par(new = TRUE)
    selection <- c()
      for(j in 1:(length(chgpt)-1)){
      selection = c(selection,rep(chgpt[j+1],chgpt[j+1]-chgpt[j]))
      }

    plot(1:n,res$means[selection-1,i], type ='l', xlab = "", ylab = "", col = 3, ylim = c(mini,maxi),lwd = 2)
    }
}


#'
#' Vector of changepoints
#'
#' @description Vector of changepoints
#' @param res A multiSeg object (obtained with function multiSeg)
#' @return The vector of changepoints
#' @examples
#' n <- 500
#' var <- 1
#' means <- matrix(c(0,1,2,1,0,1,2,2,0,0,1,1),3,4)
#' changes <- c(0.3,0.6,1)
#' dataG(n,means,changes, var)
#' file = paste(getwd(),"/dataG.txt",sep = "")
#' res <- multiSeg(file,4*2*log(n),1)
#' changepoints(res)
#'
#'
changepoints <- function(res){
  n <- dim(res$means)[1]
  chgpt <- res$chgpt[n]
  while(chgpt[1] != 0){chgpt = c(res$chgpt[chgpt[1]-1],chgpt)}
  chgpt <- c(chgpt,n)
return(chgpt)
}




