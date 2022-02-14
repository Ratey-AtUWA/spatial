# function to draw map based on digitized data from
# '../spatial/main/afr_map_v2.csv'
afrMap <- function(
  proj = "utm",
  xlim = NULL,
  ylim = NULL,
  maplabels = TRUE
)
{ require(prettymapr)
   if (proj == "utm"){
      z <- 2
      axlabs <- c("UTM Easting (m)","UTM Northing (m)")
    } else {
      z <- 0
      axlabs <- c("Longitude (\u00B0E)","Latitude (\u00B0S)")
    }

  afr_map <- read.csv(file="https://raw.githubusercontent.com/Ratey-AtUWA/spatial/main/afr_map_v2.csv", 
                    stringsAsFactors = TRUE)
  palette(c("black","red3","darkgreen","blue2","sienna",
            "purple","darkcyan","gold2","gray50","white"))
  par(mar = c(3.5, 3.5, 1, 1), mgp = c(1.7, 0.3, 0), tcl = 0.25, font.lab = 2,
      lend = "round", ljoin = "mitre")
  plot(afr_map[,10+z], afr_map[,11+z], lwd=1, col = "darkkhaki", lty = 3, asp=1, type="l",
       xlab = axlabs[1], ylab = axlabs[2], xlim = xlim, ylim = ylim)
  polygon(afr_map[,6+z], afr_map[,7+z], border = "#9AB79A", col = "#9AB79A")
  polygon(afr_map[,20+z], afr_map[,21+z], border = "dodgerblue", col = "lightblue")
  polygon(afr_map[1:67, 24+z], afr_map[1:67, 25+z], border = "dodgerblue", col = "lightblue")
  polygon(afr_map[69:92, 24+z], afr_map[69:92, 25+z], border = "dodgerblue", col = 10)
  lines(afr_map[,2+z],afr_map[,3+z], lwd=5, col = "grey")
  lines(afr_map[,15+z],afr_map[,16+z], lwd = 2, col = "royalblue3")
  lines(afr_map[,29+z],afr_map[,30+z], lty = 2, col = "grey67")
  
  addnortharrow()
  addscalebar(plotepsg=32750, htin = 0.15, 
              label.cex = 1.2)

    if(isTRUE(maplabels)) {
  text(400200, 6467820, labels = "Swan River", 
       col = "royalblue4", font = 3, cex = 1.2, 
       srt = 330)
  text(400200, 6468135, 
       labels = "Chapman St Drain", 
       col = "royalblue3", srt = 45)
  text(399970, 6468100, 
       labels = "Kitchener St Drain", 
       col = "royalblue3", srt = 300)
  text(400040, 6468240, 
       labels = "Woolcock\nCt Drain", 
       col = "royalblue3", pos = 4)
  text(399900, 6468230, 
       labels = "Hardy Road", 
       col = "grey65", srt = 45)
  text(400540, 6468490, 
       labels = "Iveson Place", 
       col = "grey65", srt = 50)
  text(400875, 6468230, 
       labels = "West Road", 
       col = "grey65", srt = 59)
  text(400450, 6468000, 
       labels = "Ashfield Flats\nReserve", 
       col = "darkolivegreen", font = 3, cex = 1.2)
  points(400280, 6468395, 
         pch = 19, cex = 0.75, 
         col = "black")
  text(400280, 6468395, 
       labels = "Lookout", 
       col = "grey65", pos = 2)
  box()
  } else {
    box()
  }
}
