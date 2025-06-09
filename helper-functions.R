# helper functions

#' Function to find the size of each rectangle (necessary for strong sampling)
#' 
#' @param r vector of length 4. Coordinates of the rectangle
findSize <- function (r) {
  (abs((r[1]-r[3]))*(abs(r[2]-r[4])))
}


#' Sets up all possible rectangles within a given hypothesis space 
#' @param xrange vector describing the x axis (default 0:10)
#' @param yrange vector describing the y axis (default 0:10)
#' @return A dataframe where each row is one rectangle, 
#' the column num is a unique identifier for each hypothesis
#' the columns (x1,y1,x2,y2) are the coordinates 
#' the column prior indicates the prior probability of that hypothesis (initially equal)
#' nPos and nNeg will track the number of points of each, right now 0
#' likePos and likeNeg are the likelihood of each of those things, right now 1
#' consPos and consNeg are TRUE if the hypothsis is consistent with the pos/neg data
#' posterior is the posterior, right now equal to prior
makeBorders = function(xrange=0:10,yrange=0:10){
  
  # Create array with all possible rectangle coordinates within the range 
  borders = expand.grid(xrange,yrange,xrange,yrange)
  # get full size
  fullSize <- (length(xrange)-1)*(length(yrange)-1)
  
  for (i in 1:length(borders[,1])){ # replace duplicate rectangles with NA
    if (borders[i,1]-borders[i,3] > 0 | borders[i,2]-borders[i,4] > 0){
      borders[i,] = NA
    }
  }
  
  borders = borders[complete.cases(borders),] # delete any rows with NA (rows that previously held duplicate rectangles)
  
  # replace rows where size = 0 with NA
  for (i in 1:length(borders[,1])){
    if (borders[i,1] == borders[i,3] | borders[i,2] == borders[i,4]) {
      borders[i,] = NA 
    }
  }
  
  borders = borders[complete.cases(borders),] # delete any rows with NA (rows that previously held rectangles with size 0) 
  
  nHyp <- nrow(borders)
  size <- rep(0,nHyp)
  for (i in 1:nHyp) {
    size[i] <- findSize(c(borders[i,1],borders[i,2],borders[i,3],borders[i,4]))
  }
  negSize <- fullSize - size
  prior <- rep(1,nHyp)/nHyp
  posterior <- prior
  nPos <- rep(0,nHyp)
  nNeg <- rep(0,nHyp)
  likePos <- rep(1,nHyp)
  likeNeg <- rep(1,nHyp)
  consPos <- rep(TRUE,nHyp)
  consNeg <- rep(TRUE,nHyp)
  b <- data.frame(borders,size,negSize,prior,
                  nPos,consPos,likePos,nNeg,consNeg,likeNeg,posterior)
  colnames(b) <- c("x1","y1","x2","y2","size","negSize",
                   "prior","nPos","consPos","likePos","nNeg",
                   "consNeg","likeNeg","posterior")
  rownames(b) <- paste0("h",1:nrow(b))
  return(b)   
}

# **********************
#     isInRectangle
# **********************
#' Function to figure out whether a certain observation is within a rectangle
#' @param p vector of length 2. The point that you want to know is inside the rectangle or not
#' @param r vector of length 4. Coordinates of the rectangle (x1,y1,x2,y2)
#' @returns TRUE if in the rectangle, false if not
isInRectangle <- function (p,r) {
  return (p[1]>=r[1] & p[1]<=r[3] & p[2]>=r[2] & p[2]<=r[4])
}



# ********************************
#     findConsistencyOfPoints
# ********************************
#' Given a data frame with a set of points, returns a data frame where each row corresponds to a point
#' and each column corresponds to a hypothesis. The cell is TRUE if that point
#' is in that hypothesis and false if it is not
#' @param hyp Data frame of hypotheses (each row is one, columns are x1,y1,x2,y2,prob)
#' @param pts Data frame of points (each row is one, columns are x and y)
#' @return A dataframe containing information about all points
#' the rownames correspond to the points in pts
#' the colnames correspond to the hypotheses in hyp

findConsistencyOfPoints = function(hyp,pts){
  
  nHyp <- nrow(hyp)
  nPts <- nrow(pts)
  consPt <- matrix(FALSE,nrow=nPts,ncol=nHyp)
  rownames(consPt) <- rownames(pts)
  colnames(consPt) <- rownames(hyp)
  
  for (p in 1:nPts) {
    for (h in 1:nHyp) {
      if (isInRectangle(c(pts[p,1],pts[p,2]),c(hyp$x1[h],hyp$y1[h],hyp$x2[h],hyp$y2[h]))) {
        consPt[p,h] <- TRUE
      } 
    }
  }
  
  consPt <- data.frame(consPt) 
  return(consPt)
}


# ********************************
#     plotColourfulDistribution
# ********************************
#' @title plotColourfulDistribution
#' @description Like plotDistribution, except includes the probabilities of 
#' things and also makes points red if they are negative and green if positive
#' @param trueRectangle length 4 vector of true rectangle (default: none)
#' @param obs observations to be plotted with it (default: none)
#' @param allPts set of all of the points along with their probabilities 
#' @param xrange vector describing the x axis (default 0:10)
#' @param yrange vector describing the y axis (default 0:10)
#' @param title character string for a title for the graph (default:"Sampling distribution")
#' @param subtitle character string for a title for the graph (default:"Yellow rectangle is the true hypothesis")

plotColourfulDistribution = function(obs=NA, trueRectangle=c(0,0,0,0), allPts,
                                     xrange = 0:10, yrange=0:10,
                                     title="Sampling distribution", 
                                     subtitle="Yellow rectangle is the true hypothesis",
                                     manual_scale = NULL){
  xlow <- min(xrange)
  xhigh <- max(xrange)
  ylow <- min(yrange)
  yhigh <- max(yrange)
  nPts <- nrow(allPts)
  trueR <- data.frame(x1=trueRectangle[1],y1=trueRectangle[2],
                      x2=trueRectangle[3],y2=trueRectangle[4])
  # makes it so all observations are black
  
  if (!is.null(nrow(obs))) {
    allPts$posterior[obs$index] <- 0
  }
  
  pRect <- ggplot() +
    xlim(xlow,xhigh) +
    ylim(ylow,yhigh) +  
    theme(panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "black"), 
          panel.grid.minor = element_line(size = 0.05, linetype = 'solid',
                                          colour = "black"),
          panel.background = element_rect(fill = "black", colour = "black",
                                          size = 2, linetype = "solid")) +
    labs(title=title, subtitle=subtitle)
  
  pRect <- pRect + 
    geom_rect(data=allPts, 
              mapping=aes(xmin=x-0.5, xmax=x+0.5, ymin=y-0.5, ymax=y+0.5, 
                          fill=posterior),show.legend=FALSE) 
  
  #geom_text(data=allPts, mapping=aes(x=x, y=y, label=abs(p)),
  #          show.legend=FALSE) +
  if (is.null(manual_scale)) {
    
    pRect <- pRect + 
      scale_fill_gradient2(low="red",mid="black",high="green")
    
  } else {
    pRect <- pRect + 
      scale_fill_gradient2(
        limits = manual_scale,
        low = "red",
        mid = "black",   
        high = "green"   
      )
  }
  
  
  
  
  pRect <- pRect +
    geom_rect(data=trueR, mapping=aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2), color="yellow",
              fill=NA,linetype="dashed")+
    
    
    
    # add in observations if they exist
    if (!is.null(nrow(obs))) {
      nPos <- sum(obs$category=="positive")
      nNeg <- sum(obs$category=="negative")
      if (nPos > 0 & nNeg > 0) {
        pRect <- pRect +
          geom_point(data=obs, mapping=aes(x=x, y=y, color=category, shape=category), size=6,
                     show.legend=FALSE) +
          scale_shape_manual(values=c(4,19)) +
          scale_color_manual(values=c("#FF0000FF","#00A600"))
      } else if (nNeg > 0) {
        pRect <- pRect +
          geom_point(data=obs, mapping=aes(x=x, y=y), color="white", shape=4, size=6,
                     show.legend=FALSE)        
      } else {
        pRect <- pRect +
          geom_point(data=obs, mapping=aes(x=x, y=y), color="white", shape=19, size=6,
                     show.legend=FALSE)        
      }
    }
  
  return(pRect)
}



plotMultiplePointPosteriors <- function(hyp, point_probs, yrange = 0:2, xrange = 0:2) {
  max_value <- max(point_probs)
  min_value <- min(point_probs)
  plot_list <- list()
  for (i in 1:nrow(hyp)) {
    d_i <- cbind.data.frame(pts, posterior = point_probs[, i])
    rect <- hyp[i, 1:4]
    hyp_name <- colnames(point_probs)[i]
    p <- plotColourfulDistribution(trueRectangle = rect, allPts = d_i, xrange = xrange, yrange = yrange, t = hyp_name, subtitle = NULL, manual_scale = c(min_value, max_value))
    plot_list[[i]] <- p
  }
  ggarrange(plotlist = plot_list)
}



# ********************************
#     findProbabilityOfPoints
# ********************************
#' Given a data frame with a set of points and another dataframe with a set of hypotheses
#' with weights, returns a dataframe where each row corresponds to a point
#' and each column corresponds to a hypothesis. The cell gives the probability
#' of that point for that hypothesis, depending on alpha. 
#' @param hyp Data frame of hypotheses (each row is one, columns are x1,y1,x2,y2,prob)
#' @param pts Data frame of points (each row is one, columns are x and y)
#' @param whichObs Says whether the points are positive or negative ("pos" or "neg")
#' @param alpha If zero, this is weak. -1 is deceptive. 1 is helpful. default=0
#' @return A dataframe containing information about all points
#' the rownames correspond to the points in pts
#' the colnames correspond to the hypotheses in hyp

findProbabilityOfPoints = function(hyp,pts,whichObs,alpha=0){
  
  nHyp <- nrow(hyp)
  nPts <- nrow(pts)
  consPt <- matrix(0,nrow=nPts,ncol=nHyp)
  rownames(consPt) <- rownames(pts)
  colnames(consPt) <- rownames(hyp)
  
  for (p in 1:nPts) {
    for (h in 1:nHyp) {
      if (isInRectangle(c(pts[p,1],pts[p,2]),c(hyp$x1[h],hyp$y1[h],hyp$x2[h],hyp$y2[h]))) {
        if (whichObs=="pos") {
          consPt[p,h] <- (1/hyp$size[h])^alpha
        } 
      } else {
        if (whichObs=="neg") {
          consPt[p,h] <- (1/hyp$negSize[h])^alpha 
        }
      }
    }
  }
  
  consPt <- data.frame(consPt) 
  return(consPt)
}



