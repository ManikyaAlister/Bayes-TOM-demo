# helper functions

#' Calculate the area (size) of a rectangle
#'
#' This function is used in strong sampling calculations.
#'
#' @param r A numeric vector of length 4 representing the rectangle coordinates (x1, y1, x2, y2).
#' @return The area of the rectangle.
find_size <- function (r) {
  (abs((r[1]-r[3]))*(abs(r[2]-r[4])))
}


#' Generate a hypothesis space of all rectangles within a grid
#'
#' @param xrange A vector defining the x-axis grid range (default is 0:10).
#' @param yrange A vector defining the y-axis grid range (default is 0:10).
#' @return A dataframe where each row is a rectangle hypothesis including prior, size, and placeholders for posterior calculations.
make_borders = function(xrange=0:10,yrange=0:10){
  
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
  
  # replace rows wherelinewidth = 0 with NA
  for (i in 1:length(borders[,1])){
    if (borders[i,1] == borders[i,3] | borders[i,2] == borders[i,4]) {
      borders[i,] = NA 
    }
  }
  
  borders = borders[complete.cases(borders),] # delete any rows with NA (rows that previously held rectangles with size 0) 
  
  nHyp <- nrow(borders)
  size <- rep(0,nHyp)
  for (i in 1:nHyp) {
    size[i] <- find_size(c(borders[i,1],borders[i,2],borders[i,3],borders[i,4]))
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

#' Check whether a point lies within a rectangle
#'
#' @param p A vector of length 2 representing a point (x, y).
#' @param r A vector of length 4 representing a rectangle (x1, y1, x2, y2).
#' @return TRUE if the point is inside the rectangle, otherwise FALSE.
is_in_rectangle <- function (p,r) {
  return (p[1]>=r[1] & p[1]<=r[3] & p[2]>=r[2] & p[2]<=r[4])
}



#' Determine consistency of each point with each hypothesis
#'
#' @param hyp A dataframe of hypotheses (with x1, y1, x2, y2 columns).
#' @param pts A dataframe of points (with x and y columns).
#' @return A logical dataframe indicating for each point (row) whether it is inside each hypothesis (column).

find_consistency_of_points = function(hyp,pts){
  
  nHyp <- nrow(hyp)
  nPts <- nrow(pts)
  consPt <- matrix(FALSE,nrow=nPts,ncol=nHyp)
  rownames(consPt) <- rownames(pts)
  colnames(consPt) <- rownames(hyp)
  
  for (p in 1:nPts) {
    for (h in 1:nHyp) {
      if (is_in_rectangle(c(pts[p,1],pts[p,2]),c(hyp$x1[h],hyp$y1[h],hyp$x2[h],hyp$y2[h]))) {
        consPt[p,h] <- TRUE
      } 
    }
  }
  
  consPt <- data.frame(consPt) 
  return(consPt)
}


#' Plot probability distribution over grid with colored points
#'
#' @param obs Dataframe of observed points including x, y, index, and category.
#' @param trueRectangle Vector of length 4 representing the true rectangle.
#' @param allPts Dataframe of points with posterior values.
#' @param xrange Vector for x-axis limits.
#' @param yrange Vector for y-axis limits.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param manual_scale Vector of length 2 setting manual fill scale limits.
#' @return A ggplot object.

plot_colourful_distribution = function(obs=NA, trueRectangle=c(0,0,0,0), allPts,
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
                                         linewidth = 2, linetype = "solid")) +
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
          geom_point(data=obs, mapping=aes(x=x, y=y, color=category, shape=category),linewidth=6,
                     show.legend=FALSE) +
          scale_shape_manual(values=c(4,19)) +
          scale_color_manual(values=c("#FF0000FF","#00A600"))
      } else if (nNeg > 0) {
        pRect <- pRect +
          geom_point(data=obs, mapping=aes(x=x, y=y), color="white", shape=4,linewidth=6,
                     show.legend=FALSE)        
      } else {
        pRect <- pRect +
          geom_point(data=obs, mapping=aes(x=x, y=y), color="white", shape=19,linewidth=6,
                     show.legend=FALSE)        
      }
    }
  
  return(pRect)
}



#' Plot posterior distributions for multiple hypotheses
#'
#' @param hyp A dataframe of hypotheses.
#' @param point_probs A dataframe of point posterior probabilities for each hypothesis.
#' @param yrange Range of y-axis values (default 0:2).
#' @param xrange Range of x-axis values (default 0:2).
#' @return A combined ggplot of subplots.
plot_multiple_point_posteriors <- function(hyp, point_probs, yrange = 0:2, xrange = 0:2) {
  max_value <- max(point_probs)
  min_value <- min(point_probs)
  plot_list <- list()
  for (i in 1:nrow(hyp)) {
    d_i <- cbind.data.frame(pts, posterior = point_probs[, i])
    rect <- hyp[i, 1:4]
    hyp_name <- colnames(point_probs)[i]
    p <- plot_colourful_distribution(trueRectangle = rect, allPts = d_i, xrange = xrange, yrange = yrange, t = hyp_name, subtitle = NULL, manual_scale = c(min_value, max_value))
    plot_list[[i]] <- p
  }
  ggarrange(plotlist = plot_list)
}



#' Compute observation probabilities for each hypothesis
#'
#' @param hyp A dataframe of hypotheses with weight attributes.
#' @param pts A dataframe of observation points.
#' @param whichObs Whether the observations are "pos" or "neg".
#' @param alpha Informativeness parameter: 0 (weak), 1 (helpful), -1 (deceptive).
#' @return A dataframe of point probabilities for each hypothesis.

find_probability_of_points = function(hyp,pts,whichObs,alpha=0){
  
  nHyp <- nrow(hyp)
  nPts <- nrow(pts)
  consPt <- matrix(0,nrow=nPts,ncol=nHyp)
  rownames(consPt) <- rownames(pts)
  colnames(consPt) <- rownames(hyp)
  
  for (p in 1:nPts) {
    for (h in 1:nHyp) {
      if (is_in_rectangle(c(pts[p,1],pts[p,2]),c(hyp$x1[h],hyp$y1[h],hyp$x2[h],hyp$y2[h]))) {
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

#' Plot all hypotheses as rectangles, optionally adding a point
#'
#' @param hyp Dataframe of hypotheses including x1, y1, x2, y2.
#' @param pt Optional point dataframe with x, y, and category.
#' @return A ggplot object.
plot_empty_hypotheses <- function(hyp, pt = NULL) {
  plot <- ggplot() +
    geom_rect(data = hyp, fill = "lightblue", color = "black", alpha = 0.7,
              aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2)) +
    scale_x_continuous(breaks = 0:H, minor_breaks = 0:H) +
    scale_y_continuous(breaks = 0:H, minor_breaks = 0:H) +
    coord_fixed() +
    facet_wrap(~ rect_id) +
    theme_minimal() +
    labs(title = "All possible hypotheses", x = "X", y = "Y")
  
  if (!is.null(pt)) {
    plot <- plot + geom_point(data = pt, aes(x = x, y = y,colour = category))+
      scale_color_manual(values = c("positive" = "green", "negative" = "red")) 
  }
  plot
}

#' Sample a set of points with assigned categories
#'
#' @param pts Dataframe of all possible points.
#' @param size Number of points to sample (default = 2).
#' @return A dataframe of sampled points with assigned category (positive or negative).
sample_points <- function(pts, size = 2) {
  index <- sample(1:nrow(pts), size = size, replace = FALSE)
  # randomly assign a sign to the points
  # "positive" points are inside the rectangle, "negative" points are outside
  sign <- sample(c("positive", "negative"), size = size, replace = TRUE, prob = c(0.5, 0.5))
  point <- cbind(index = index, pts[index, ], category = sign)
  point
}

#' Update posterior distribution over hypotheses based on new observed points
#'
#' Given new observations, this function updates the posterior distribution
#' over a set of rectangle hypotheses using precomputed likelihoods for
#' positive and negative evidence.
#'
#' @param new_pts A dataframe of new observed points. Must include:
#'   \itemize{
#'     \item \code{index}: Row index of the point in the likelihood matrix
#'     \item \code{category}: Either "positive" or "negative"
#'   }
#' @param likelihoods A named list with elements `"positive"` and `"negative"`,
#'   each a matrix of likelihoods (points × hypotheses).
#' @param hyp A dataframe of hypotheses. Must contain:
#'   \itemize{
#'     \item \code{prior}: Prior probability of each hypothesis
#'   }
#'
#' @return A dataframe of updated hypotheses with posterior probabilities.
#'   Hypotheses with zero posterior are removed.
get_hyp_dist <- function(new_pts, likelihoods, hyp) {
  # Start with prior distribution
  posterior <- hyp$prior
  
  # Multiply in the likelihood of each new observation
  for (i in 1:nrow(new_pts)) {
    category <- new_pts[i, "category"]
    hyp_probs <- likelihoods[[category]][new_pts[i, "index"], ]
    posterior <- posterior * hyp_probs
  }
  
  # Normalize to get a proper probability distribution
  posterior <- posterior / sum(posterior)
  
  # Update hypothesis dataframe
  hyp$posterior <- posterior
  
  # Remove hypotheses with zero posterior
  hyp <- hyp[hyp$posterior != 0, ]
  
  return(hyp)
}

#' Calculate normalized likelihoods for a given category
#'
#' Computes the normalized likelihood of each hypothesis given observed points
#' and a specified category (e.g., "pos" or "neg"), using a reliability parameter `alpha`.
#'
#' @param hyp Data frame of hypotheses (rectangles).
#' @param pts Data frame of observed points.
#' @param category A string, either "pos" (positive) or "neg" (negative).
#' @param alpha Numeric trust parameter determining the strength of the informant's signal.
#'
#' @return A normalized numeric matrix of likelihoods for each hypothesis.
#' @export
calculate_normalized_likelihoods <- function(hyp, pts, category, alpha) {
  likelihood <- find_probability_of_points(hyp, pts, category, alpha = alpha)
  likelihood <- data.matrix(likelihood / sum(likelihood))
  return(likelihood)
}

#' Compute likelihoods for both point categories
#'
#' Wrapper that returns normalized likelihoods for both positive and negative points,
#' given a single reliability parameter.
#'
#' @param hyp Data frame of hypotheses.
#' @param pts Data frame of observed points.
#' @param alpha Numeric trust parameter for the informant.
#'
#' @return A named list with "positive" and "negative" likelihood matrices.
#' @export
compute_likelihoods_for_informant <- function(hyp, pts, alpha) {
  list(
    positive = calculate_normalized_likelihoods(hyp, pts, "pos", alpha),
    negative = calculate_normalized_likelihoods(hyp, pts, "neg", alpha)
  )
}

#' Update posterior beliefs based on clues and likelihoods
#'
#' Combines prior hypotheses with the given clues and category likelihoods
#' to generate a posterior distribution over hypotheses.
#'
#' @param clues Data frame of observed points with associated categories.
#' @param likelihoods A list of likelihood matrices for each category.
#' @param hyp Data frame of prior hypotheses with columns `x1`, `x2`, `y1`, `y2`.
#'
#' @return A data frame of hypotheses with updated posterior probabilities.
#' @export
update_hypotheses_posterior <- function(clues, likelihoods, hyp) {
  get_hyp_dist(clues, likelihoods, hyp)
}

#' Plot posterior probabilities over hypotheses
#'
#' Creates a bar plot visualizing the posterior probabilities for each hypothesis.
#'
#' @param hypotheses Data frame containing at least `rect_id` and `posterior` columns.
#' @param color A string indicating the fill color for the bars.
#' @param title A string to display as the plot title.
#'
#' @return A `ggplot2` object.
#' @export
plot_posterior_probabilities <- function(hypotheses, color, title) {
  ggplot(hypotheses, aes(x = rect_id, y = posterior)) +
    geom_bar(stat = "identity", fill = color) +
    labs(title = title, x = "Hypothesis", y = "Probability") +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Extract the top hypothesis and confidence score
#'
#' Identifies the hypothesis with the highest posterior probability and computes
#' a confidence score relative to chance.
#'
#' @param hypotheses A data frame of hypotheses with posterior probabilities.
#'
#' @return A list with the best rectangle ID and a numeric confidence score.
#' @export
get_recommendation_and_confidence <- function(hypotheses) {
  chance <- 1 / nrow(hypotheses)
  recommended <- hypotheses[which.max(hypotheses$posterior), ]
  confidence <- recommended[,"posterior"] - chance
  list(rectangle = rownames(recommended), confidence = confidence)
}
