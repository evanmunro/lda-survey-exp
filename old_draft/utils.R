topBetas <- function(beta,identifier,top=5) {
  library(dplyr)
  beta = as.data.frame(beta)
  K <- ncol(beta)

  result = beta[order(-beta[,1])] 
  colnames(result) = c("State 1", "Probabilities 1")
  for (k in 2:K) {
    stateName = paste("V",k,sep="")
    result = bind_cols(result,beta[order(-beta[,stateName]),c("Perm",stateName)])
  }
  return(result[1:top,])

}

vizTheta = function(model, compare = NULL, main = "Estimated Theta",
                    varNames = NULL, groupNames = NULL,nrow = NULL, fitNames = NULL, indices = NULL) {
  
  # Internal Variables to set
  h.space <- .25
  v.space <- .1
  
  if(is.null(varNames)) {
    varNames <- paste("Var", c(1:model$J))
  }
  
  if(is.null(groupNames)) {
    groupNames <- paste("Group", c(1:model$K))
  }
  
  if (is.null(nrow)) {
    nrow <- model$J
  }
  
  if(is.null(indices)) {
    indices <- c(1:model$J)
  }
  
  if(is.null(fitNames)) {
    if(is.null(compare)) { 
      fitNames <- paste("Model", 1)
    } else {
      fitNames <- paste("Model", c(1:2))
    }
  }
  
  graphics::par(oma = c(3,5,3,1), mfrow = c(nrow, model$K), mar = rep(.1,4))
  count = 1
  for(j in indices)
  {
    if(model$dist[j]=="multinomial"|model$dist[j]=="rank")
    {
      for(k in 1:model$K)
      {
        graphics::plot(model$theta[j,k,], type = "h", lwd = 2, col = "blue", ylim = c(-v.space, 1+v.space), xlim = c(h.space, model$Vj[j] + h.space),
                       yaxt = "n", xaxt = "n", pch = 16)
        if(!is.null(compare)) {
          graphics::points(c(1:model$Vj[j]), compare[j,k,c(1:model$Vj[j])], col = "red", pch = 4, lwd = 1.5)
        }
        if (k == 1) {
          graphics::mtext(varNames[j], line = 3, side = 2, cex = .7)
          graphics::axis(side = 2, at = c(0,.5,1), labels = c(0,.5,1))
        }
        if(count == indices[length(indices)]| (count %% nrow) == 0) {
          graphics::mtext(paste(groupNames[k], sep = " "), line = .2, side = 1, cex = 1-min(model$J,10)*.4)
        }
      }
    } else if (model$dist[j] == "bernoulli") {
      for(k in 1:model$K) {
        graphics::plot(model$theta[j,k,], type = "h", lwd = 2, col = "blue", ylim = c(-v.space,1+ v.space), xlim = c(h.space, 1 + h.space),
                       yaxt = "n", xaxt = "n", pch = 16)
        if(!is.null(compare)) {
          graphics::points(c(1:model$Vj[j]), compare[j,k,], col = "red", pch = 4, lwd = 1.5)
        }
        if(k == 1){
          graphics::mtext(varNames[j], line = 3, side = 2, cex = .7)
          graphics::axis(side = 2, at = c(0,.5,1), labels = c(0,.5,1))
        }
        if(count == indices[length(indices)]| (count %% nrow) == 0) {
          graphics::mtext(paste(groupNames[k], sep = " "), line = .2, side = 1, cex = .8)
        }
      }
    } 
    if((count %% nrow) == 0) {
      graphics::title(main = main, outer = T, cex = 1.2)      
      graphics::par(fig = c(0, 1, 0, 1), oma = c(0,5,0,1), mar = rep(0, 4), new = T)
      
      graphics::plot(0, 0, type = "n", bty = "n", xaxt ="n", yaxt = "n")
      
      if(is.null(compare)){
        graphics::legend("bottom", legend = fitNames, pch = 19,
                         col = "black", cex = .8)
        
      } else {
        graphics::legend("bottom", legend = fitNames,
                         pch = c(19, 4), col = c("black", "red"), ncol = 2, cex = .8)
      }
      graphics::par(oma = c(3,5,3,1), mfrow = c(nrow, model$K), mar = rep(.1,4))
    }
    count = count + 1
  }
  graphics::title(main = main, outer = T, cex = 1.2)      
  graphics::par(fig = c(0, 1, 0, 1), oma = c(0,5,0,1), mar = rep(0, 4), new = T)
  
  graphics::plot(0, 0, type = "n", bty = "n", xaxt ="n", yaxt = "n")
  
  if(is.null(compare)){
    graphics::legend("bottom", legend = fitNames, pch = 19,
                     col = "black", cex = .8)
    
  } else {
    graphics::legend("bottom", legend = fitNames,
                     pch = c(19, 4), col = c("black", "red"), ncol = 2, cex = .8)
  }
  graphics::par(oma = c(3,5,3,1), mfrow = c(nrow, model$K), mar = rep(.1,4))
}