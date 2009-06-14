`xybarplot` <-
function(x, y, wbox = min(diff(x))*0.9, ofs=0.5, ...) {
  barplot(y, wbox, space = c(x[1] - wbox * ofs, (diff(x) - wbox)) / wbox, ...)
}

