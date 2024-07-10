# Author: Wajid Jawaid
# Email: wj241@cam.ac.uk
# Date: 14 November 2016

#' Freehand select
#'
#' Freehand select function. First generate a 2D plot using R's plot function, 
#' then select gate region by left clicking. Close polygon by right clicking.
##' The function will return the rownames of the enclosed points by the rownames
##' of th co-ordinates given in \code{data}.
##' @title Freehand select
##' @param data Data frame or matrix of co-ordinates. (x,y) co-ordinates for each
##' point will be on rows. Rownames of selected points will be returned.
##' @param mark Default TRUE. Predicate marking of selected points.
##' @param names Default TRUE. If TRUE will return rownames of data frame with
##' points within polygon. If FALSE will return logical vector.
##' @param ... Additional parameters passed to \code{\link{points}}.
##' @return Returns character vector of rownames of the selected points from \code{data} if
##' names parameter is TRUE. If names is FALSE then a logical vector indicating whether points
##' are in the polygon is returned.
##' @author Wajid Jawaid
##' @importFrom lifecycle deprecate_warn
##' @export
##' @examples
##' if(interactive()) {
##'   x <- cbind(1:10, 1:10)
##'   rownames(x) <- 1:10
##'   plot(x, pch = 16, col = "red")
##'   fhs(x)
##' }
##' @importFrom graphics locator lines points
fhs <- function(data, mark = TRUE, names = TRUE, ...) {
  lifecycle::deprecate_warn("1.0.0", "tidygate::fhs()", with = "tidygate::gate()")
  
  cat("Mark region on plot.\n")
  if (!(is.data.frame(data) || is.matrix(data))) stop("data must be a data frame or matrix")
  if (is.null(rownames(data))) rownames(data) <- 1:nrow(data)
  sel <- selectGate()
  xr <- range(sel$x)
  yr <- range(sel$y)
  xPass <- (data[,1] > xr[1]) & (data[,1] < xr[2])
  yPass <- (data[,2] > yr[1]) & (data[,2] < yr[2])
  inROI <- applyGate(data[xPass & yPass,,drop=FALSE], sel)
  if (mark) points(data[xPass & yPass,,drop=FALSE][inROI,1:2,drop=FALSE], ...)
  cp <- rep(FALSE, nrow(data))
  cp[xPass & yPass][inROI] <- TRUE
  cNames <- rownames(data)[cp]
  attr(cNames, "gate") <- attr(cp, "gate") <- sel
  if (names) return(cNames)
  return(cp)
}

selectGate <- function() {
  sel <- locator(type = "l")
  if (length(sel$x) < 3) stop("Please select at least 3 points to define a shape.")
  numPoints <- length(sel$x)
  lines(x = sel$x[c(1, numPoints)], y = sel$y[c(1, numPoints)])
  sel <- as.data.frame(sel)
  return(sel)
}

applyGate <- function(data, v) {
  np <- nrow(data)
  nl <- nrow(v)
  gv <- c(v[,1], v[1,1])
  ip <- blw <- blw1 <- blw2 <- chk <- inGate <- vector("logical", np)
  for (i in 1:np) {
    ip <- as.logical(abs(diff(data[i,1] <= gv)))
    blw1 <- data[i,2] >= v[,2]
    blw2 <- data[i,2] >= c(v[-1,2], v[1,2])
    blw <- blw1 & blw2
    chk <- xor(blw1, blw2)
    for (j in (1:length(chk))[chk]) {
      k = (j %% nl) + 1
      ## x1 = v[j, 1]; x2 = v[k, 1]; y1 = v[j, 2]; y2 = v[k,2]
      if ( v[j,1] != v[k, 1]) {
        cfs <- (matrix(c(1, -v[k, 1], -1, v[j,1]), 2) / (v[j, 1] - v[k, 1])) %*%
          c(v[c(j,k), 2])
        py <- matrix(c(data[i, 1], 1), 1) %*% cfs
        if (data[i, 2] > py) blw[j] <- TRUE
      } else {
        blw[j] <- TRUE
      }
    }
    inGate[i] <- sum(ip & blw) %% 2
  }
  return(as.logical(inGate))
}
