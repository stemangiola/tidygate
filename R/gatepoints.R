# Authors@R: person("Wajid", "Jawaid", email = "wj241@cam.ac.uk", role = c("aut", "cre"))
# This has been copied from https://github.com/wjawaid/gatepoints with the permission of the author, who is also author on tidygate
# For allowing programmatically application of defined gates, since these functions are hidden from the package

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