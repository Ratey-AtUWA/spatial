require(OpenStreetMap)
require(prettymapr)
require(gstat)
require(sp)
require(fields)
require(lctools)
require(geoR) # especially  for locations.inside() function

# make grid for inverse distance interpolation ####
mesh <- c(35,53) # grid dimensions c(x=Easting, y=Northing)
sp.grid.sv <- expand.grid(seq(min(sv17soil$Easting), max(sv17soil$Easting), l = mesh[1]),
                          seq(min(sv17soil$Northing), max(sv17soil$Northing), l = mesh[2]))
coordinates(sp.grid.sv) <- ~Var1+Var2
projection(sp.grid.sv) <- "+proj=utm +zone=50 +south +datum=WGS84"

# make data subset of coords and variable with no missing observations ####
sp.sv17soil.Cu <- na.omit(sv17soil[,c("Cu", "Easting", "Northing")])
sp.sv17soil.Cu$Cu <- log10(sp.sv17soil.Cu$Cu)
coordinates(sp.sv17soil.Cu) <- ~Easting+Northing
projection(sp.sv17soil.Cu) <- "+proj=utm +zone=50 +south +datum=WGS84"

# perform inverse distance interpolation ####
Cu.idw <- idw(Cu~1, locations = sp.sv17soil.Cu, newdata = sp.grid.sv, idp = 2)

# make idw object into spatial object
Cu.idw.xyz <- as.data.frame(cbind(Cu.idw@coords[,1], Cu.idw@coords[,2], Cu.idw@data[,1]))
colnames(Cu.idw.xyz) <- c("Easting","Northing","Cu")

# find the row numbers of points inside the site boundary... ####
# n.b. the object sv_boundary contains coordinates of the 
# boundary around the sampling area. So the masking can be
# omitted if we want to plot the whole interpolation grid.
inside <- as.numeric(row.names(locations.inside(Cu.idw.xyz[,1:2], 
                                    sv_boundary[,c("Easting","Northing")])))

# ...and use this to find the rows *outside* the boundary ####
mask_in <- rep(0, length(Cu.idw.xyz$Cu))
mask_in[inside] <- rep(1, length(inside))
mask_out <- which(mask_in==0)

# make values outside the boundary NA ####
Cu.idw.xyz$Cu[mask_out] <- rep(NA, length(mask_out))

# set up plotting ####
sf <- 0.85
palette(c("black","blue4","blue2","cyan4","darkolivegreen3","gold2","darkorange1",
          "tomato3","red2","white","transparent"))
par(mfrow = c(1,1), lend = 2, ljoin = 1, mgp = c(2.0,0.3,0)*sf, 
    oma = c(0,3.5,0,1)*sf, mar = c(3.5,0,1.5,0)*sf, tcl = 0.25*sf)

# draw map ####
# draw base map
plot(SV.light.utm, removeMargin=FALSE)
axis(1, tcl=0.25, cex.axis=1.4*sf, mgp=c(2.2,0.7,0)*sf, lwd=sf, tcl=0.25*sf) # HORIZ # 
mtext("Zone 50 UTM Easting (m)", side=1, line=2*sf, cex=1.4*sf, font=2) # HORIZ AXIS LABEL
axis(2, tcl=0.25, cex.axis=1.4*sf, at=seq(6466200,6466700,100), 
     labels=seq(6466200,6466700,100), mgp=c(2.0,0.4,0)*sf, 
     lwd=sf, tcl=0.25*sf) # VERT # 
mtext("Zone 50 UTM Northing (m)", side=2, line=2*sf, cex=1.4*sf, font=2) # VERT AXIS LABEL

# make background rectangle for legend
rect(390930, 6466195, 391225, 6466290,
     col = "grey96", border = "grey", lwd = 2*sf)

# overlay plot of interpolated values ####
# use image.plot() function from fields:: package 2
bands <- seq(min(Cu.idw.xyz$Cu, na.rm=T),max(Cu.idw.xyz$Cu, na.rm=T), l=22)
image.plot(seq(min(sv17soil$Easting), max(sv17soil$Easting), l = mesh[1]),
           seq(min(sv17soil$Northing), max(sv17soil$Northing), l = mesh[2]), 
           matrix(data = Cu.idw.xyz$Cu, nrow = 35, ncol = 53), add = T,
           col = colorRampPalette(c("#C0C0FF", "#D0D000", "#C00000"), space="Lab")(21),
           horizontal = TRUE, smallplot= c(0.1,0.45,0.15,0.19), 
           breaks = bands, lab.breaks = signif(10^bands,3),
           legend.args = list( text = expression(bold("Soil Cu (mg/kg)")),
                               cex = 1.25*sf, side = 3, line = .5*sf),
           axis.args = list(cex.axis = 1.2*sf, mgp=c(2.0,0.7,0)*sf)) # (mg/kg)
rm(bands)

# optionally overlay map annotations etc. ####
polygon(CVR.buildings$Easting,CVR.buildings$Northing,col="grey85",border="grey80", lwd=sf)
rect(391346,6466700,391360,6466720,col="cyan")
rect(391346,6466700,391360,6466720, density=7, lwd = 3)
text(391353, 6466693, labels = "Children's\nPlayground", font = 3, cex = 0.9*sf)
rect(391375,6466645,391395,6466665, col="#004E75",border="#004E75", lwd=sf)
text(391385,6466655,labels="P", col=10, cex=1.5*sf, font=2)
rect(391213,6466554,391233,6466574, col="#004E75",border="#004E75", lwd=sf)
text(391223,6466564,labels="P", col=10, cex=1.5*sf, font=2)
contour(seq(min(sv17soil$Easting), max(sv17soil$Easting), l = mesh[1]),
        seq(min(sv17soil$Northing), max(sv17soil$Northing), l = mesh[2]), 
        matrix(data = Cu.idw.xyz$Cu, nrow = 35, ncol = 53), add = T,
        col = "steelblue4", lwd=sf, labcex=sf,
        levels=log10(c(0.01,0.02,0.03,0.1,0.2,0.3,1,2,3,10,20,30,
                       50,100,200,300,500,1000,2000,3000,1e4)),
        labels=c(0.01,0.02,0.03,0.1,0.2,0.3,1,2,3,10,20,30,
                 50,100,200,300,500,1000,2000,3000,10000))
with(sv17soil, points(Northing ~ Easting, pch = 3, lwd = sf, cex=0.9*sf))
lines(c(391241,391411), c(6466532,6466534), col = 10, lwd = 12*sf)
text(391216,6466625,labels="Charles Veryard\nReserve", col="grey33", font=3, cex=0.85*sf)
polygon(SmithsLk.boundary$Easting, SmithsLk.boundary$Northing, lwd=1*sf, 
        col="#40D0F0", border="#3080E0")
text(391355,6466464,labels="Smiths\n  Lake", col="grey33", font=3, cex=0.85*sf)
rect(391534, 6466193, 391548, 6466271, col = 10, border = 10, lwd = 1*sf)
text(391270,6466500, labels="Electrical\nSupply\nSubstation", font=3, col="grey33", cex=0.85*sf)
text(391538,6466380,labels="Charles Street",col="grey65", font=2, srt = 270, cex=0.9*sf)
# text(390935,6466652,labels="Loftus Street",col="grey45", font=2, srt = 90, cex=0.9)
text(391418,6466385,labels="Kayle St",col="grey65", font=2, srt = 90, cex=0.8*sf)
# text(391385,6466535,labels="Bourke",col="grey45", font=2, cex=0.8)
box(lwd=sf)
addnortharrow(padin=c(0.3,0.2)*sf, scale=1.2*sf, lwd=1*sf)
addscalebar(plotepsg=32750, htin=0.2*sf, padin=c(0.3,0.3)*sf, 
            label.cex=1.2*sf, lwd=1*sf, pos="bottomright", widthhint = 0.3)

# add LISA based on Local Moran's I ####
data_temp <- na.omit(sv17soil[,c("Easting", "Northing", "Cu")])
data_temp$Cu <- log10(data_temp$Cu) # comment this line if not needed
Coords <- cbind(data_temp$Easting, data_temp$Northing)
bw <- 4
locMI_sv17soil_Cu <- l.moransI(Coords,bw,data_temp$Cu, scatter.plot = F)
plotdata <- as.data.frame(cbind(Coords[,1:2], data_temp$Cu, 
                                locMI_sv17soil_Cu$Ii, 
                                locMI_sv17soil_Cu$p.value))
colnames(plotdata) <- c("Easting", "Northing", "Cu", "MoranI", "p_value") # ;head(plotdata)
medCu <- median(plotdata$Cu, na.rm = TRUE)
HiHi <- subset(plotdata, plotdata$p_value<=0.05&plotdata$MoranI>0&plotdata$Cu>=medCu)
LoLo <- subset(plotdata, plotdata$p_value<=0.05&plotdata$MoranI>0&plotdata$Cu<medCu)
HiLo <- subset(plotdata, plotdata$p_value<=0.05&plotdata$MoranI<0&plotdata$Cu>=medCu)
LoHi <- subset(plotdata, plotdata$p_value<=0.05&plotdata$MoranI<0&plotdata$Cu<medCu)

# plot "LISA" points
palette(c("black","red3","blue3","orange","skyblue",
          "#FF8080","#8080FF","#FFA50040","#87CEEB40","white"))
with(HiHi, points(Northing ~ Easting, pch = 22,
                  col = 2, bg = 6, lwd = 2*sf, cex = 1.8*sf))
with(LoLo, points(Northing ~ Easting, pch = 21, 
                  col = 3, bg = 7, lwd = 2*sf, cex = 2*sf))
with(HiLo, points(Northing ~ Easting, pch = 25,  
                  col = 2, bg = 7, lwd = 3*sf, cex = 1.4*sf))
with(LoHi, points(Northing ~ Easting, pch = 24,
                  col = 3, bg = 6, lwd = 3*sf, cex = 1.4*sf))
# legend background
rect(390930, 6466295, 391110, 6466560,
     col = "grey96", border = "grey", lwd = 2*sf)
# legend title
text(390930, 6466530, font = 2,
     labels = "Local Moran's I\nfor soil Cu",
     cex = 1.1*sf, offset = 0.3*sf, pos = 4, col = 1)
mI <- moransI(Coords, bw, data_temp$Cu)
# legend subtitle
text(390930, 6466465,
     labels = paste("Based on", bw, "nearest\n  neighbour points",
                    "\nOnly plotting points\n  with p \u2264 0.05"),
     cex = 1.1*sf, offset = 0.3*sf, pos = 4, col = 1, font = 3)
# legend for LISA points
legend(390930,6466430, bty = "n", inset = 0.03, cex = 1.2*sf,
       legend = c("High-High", "Low-Low","High-Low","Low-High"),
       pch = c(22, 21, 25,24), pt.cex = c(1.8,2,1.4,1.4)*sf, 
       pt.lwd=c(2,2,3,3),
       col = c(2,3,2,3), pt.bg = c(6,7,7,6), text.col = 1,
       box.col = 10, box.lwd = 1.3*sf, bg = "#E0E0E0",
       y.intersp = 1.2)
# legend footnote
text(390930, 6466315,
     labels = paste("Global Moran's I =", round(mI$Morans.I,3)),
     cex = 1.1*sf, offset = 0.3*sf, pos = 4, col = 1)

# delete temporary objects made in code
rm(list = c("sf","mI","data_temp", "plotdata", "HiHi", "LoLo", "HiLo", "LoHi",
            "mask_in", "mask_out"))
rm(list = ls(pattern = "med"))
rm(list = ls(pattern = "locMI_"))
