interpolate_profile <-
function(x, y, z,
  xo = seq(min(x), max(x), length = 100),
  yo = seq(min(y), max(y), length = 100),
  output = c("xyz", "grid")) {
  
  valid <- !(is.na(x) | is.na(y) | is.na(z))
  grid <- interp(x[valid], y[valid], z[valid], xo = xo, yo = yo)
  if (output == "grid")
    return(grid)
  else {
    dd <- expand.grid(x = grid$x, y = grid$y)
    dd$z <- as.vector(grid$z)
    return(dd)
  }
}

