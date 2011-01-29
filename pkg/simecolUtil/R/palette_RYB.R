palette_RYB <-
function(n=100) {
   palette <- colorRampPalette(c(brewer.pal(11, "RdYlBu"), "darkblue"))
   return(palette(n))
}

