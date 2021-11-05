# setwd(" ... /GABI-biogeography")

## LATITUDINAL MIGRATIONS BY BIOME

  final.table_bp <- read.csv("Paleo_analyses/final_table_graph_GABI_15_nov16.csv")

##  Boxplot by latitude
  pana <- final.table_bp[77,]

## temperate_norteamerica
## Subset localities on latitude greather than 23.5
  temp_norte <- subset(final.table_bp[,c("v.timeBin", "v.paleolat", "percent_norte", "percent_sur")], 
		     final.table_bp$v.paleolat >= 23.5 & 
		     final.table_bp$v.timeBin <= 11.5)

## Subset localities on latitude between 13 and 23.5
  trop_norte <- subset(final.table_bp[,c("v.timeBin", "v.paleolat", "percent_norte", "percent_sur")], 
		     final.table_bp$v.paleolat <= 23.5
		     & final.table_bp$v.paleolat >= 13 & final.table_bp$v.timeBin <= 11.5)

## To add panama locality to the trop_norte subset
  trop_norte <- rbind(trop_norte, pana[, c("v.timeBin", "v.paleolat", "percent_norte", "percent_sur")]) 
 
## Subset localities on latitude between 13 and -23.5
  trop_sur <- subset(final.table_bp[,c("v.timeBin", "v.paleolat", "percent_norte", "percent_sur")], 
		final.table_bp$v.paleolat >= -23.5
		& final.table_bp$v.paleolat <= 13 & final.table_bp$v.timeBin <= 11.5)
 
##  substract panama locality from "trop_sur" subset
  trop_sur <- trop_sur[rownames(trop_sur) != rownames(pana),] 	

  temp_sur <- subset(final.table_bp[,c("v.timeBin", "v.paleolat", "percent_norte", "percent_sur")], 
		   final.table_bp$v.paleolat <= -23.5
		   & final.table_bp$v.timeBin <= 11.5)

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# Modern (geographical) data to set the origin

load("Mammals_data\\Spatial_mamals")

# Geographic database (aggregate data by cell numbar - HBWID)
  geomams <- aggregate(spamams$X_COORD ~ spamams$HBWID, FUN = unique)
  colnames(geomams) <- c("HBWID","X_COORD")
  geomams$Y_COORD <- aggregate(spamams$Y_COORD ~ spamams$HBWID, FUN = unique)[,2]
  geomams$IsIsland  <- aggregate(spamams$IsIsland  ~ spamams$HBWID, FUN = unique)[,2]

# Identify Caribean Islands (mannually)
  geomams$Caribe <- 0 
  geomams$Caribe[c(2216, 2217, 2218, 2219, 2220, 2221, 2222, 2223, 2244, 2245, 2246, 
 	2247, 2248, 2268, 2269, 2270, 2271, 2272, 2291, 2292, 2293, 2294, 2295, 2296, 
 	2297, 2298, 2299, 2321, 2322, 2323, 2324, 2325, 2326, 2327, 2351, 2356)] <- 1

# Set coordinates as numeric (not factors)
  geomams$Y_COORD <- as.numeric(as.character(geomams$Y_COORD))
  geomams$X_COORD <- as.numeric(as.character(geomams$X_COORD))

# Separate NAm y SAm
  geomams$GeoOri <- ifelse(geomams$Y_COORD > 13, "LAUR","GONDW")
  geomams$GeoOri <- ifelse(geomams$X_COORD < -82, "LAUR",geomams$GeoOri)

# manually set Centroamerica cells to NA
  geomams$GeoOri[c(2421, 2431, 2444, 2445, 2464, 2465, 2466, 2467, 2468, 2469, 2470, 2488, 2489, 2490, 2491,
  	2492, 2493, 2494, 2515, 2516, 2517, 2518, 2519, 2538, 2539, 2562, 2563)] <- "LAUR"

# Delete origin from the islannds
  geomams$GeoOri[geomams$IsIsland == 1] <- NA
  geomams$GeoOri[geomams$Caribe == 1] <- NA

# Richness
  geomams$Rich <- aggregate(spamams$scientificname ~ spamams$HBWID, FUN = length)[,2]

# origin class (how many species are from north / south origin)
  geomams$OC_NAm <- aggregate(spamams$Origin_Class ~ spamams$HBWID, FUN = table)[,2][,1]
  geomams$OC_SAm <- aggregate(spamams$Origin_Class ~ spamams$HBWID, FUN = table)[,2][,2]

# Proportion of north / South origin by cell
  geomams$OC_PropNAm <- geomams$OC_NAm / geomams$Rich
  geomams$OC_PropSAm <- geomams$OC_SAm / geomams$Rich

# Elevation   # load raster from: 
# https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/bedrock/cell_registered/georeferenced_tiff/
# cells values were aggregated (60 x 60) and the median values were used
# to ~ match the  mammals database resolution

  library(raster)
  america_elev <- raster("Paleo_analyses/Elevation.tif")
  geomams$Med_elev <- extract(america_elev, geomams[ ,c("X_COORD","Y_COORD")])

# by latitude
  geomams$by_lat <- ifelse(geomams$GeoOri == "LAUR" & geomams$Y_COORD > 23.5, "NA_Te",
	ifelse(geomams$GeoOri == "LAUR" & geomams$Y_COORD < 23.5, "NA_Tr",
	ifelse(geomams$GeoOri == "GONDW" & geomams$Y_COORD > -23.5, "SA_Tr",
	ifelse(geomams$GeoOri == "GONDW" & geomams$Y_COORD < -23.5, "SA_Te","indet"))))

# by latitude + Andes
  geomams$by_latelev <- ifelse(geomams$by_lat == "SA_Tr" & geomams$Med_elev > 1500, "AH_Tr",geomams$by_lat)
  geomams$by_latelev <- ifelse(geomams$by_lat == "SA_Te" & geomams$Med_elev > 500, "AH_Te",geomams$by_latelev)

# Proportion of NAO/SAO
  geomams$NAO <- aggregate(spamams$Origin_Class ~ spamams$HBWID, FUN = table)[,2][,1]
  geomams$SAO <- aggregate(spamams$Origin_Class ~ spamams$HBWID, FUN = table)[,2][,2]
  geomams$PercentNAO <- geomams$NAO*100 / geomams$Rich
  geomams$PercentSAO <- geomams$SAO*100 / geomams$Rich

# take out: Islands, Caribe and samples  with less than 5 mammals
  gmams <- geomams[-(which(geomams$IsIsland == 1 | geomams$Caribe == 1 | geomams$Rich < 5)), ]

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
#  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
#
#	PLOT MIGRATION BY LATITUDE
# 
#

  layout(matrix(1:4, ncol=1), widths = c(lcm(12)),
		 heights=c(lcm(3), lcm(3), lcm(3), lcm(3)))

  par(mar=c(0,0,0,0), lwd=0.6, xaxs="i", yaxs="i")

  plot(0,t="n",ylim=c(0,100), xlim=c(12.5, -1.5), axes=F)
    rect(12.5, 0, -1.5, 100,col=rgb(0.9, 0, 0 ,0.1), border=NA)
  boxplot(temp_norte$percent_sur ~ as.factor(temp_norte$v.timeBin), 
	ylim=c(0,100), xlim=c(12.5, -0.5), boxwex=0.5, cex.axis=0.7, las=1, pch=20, 
	cex=0.75, boxlwd=0.85, axes=F, at=unique(temp_norte$v.timeBin), col="lightblue", lwd=0.7, add=T)

  segments(12.5,100,-1.5,100, lwd=0.9)
  segments(12.5,-1.5,12.5,100, lwd=0.75)
  segments(-1.5,-1.5,-1.5,100, lwd=0.75)
  axis(4, pos=-1.5, las=1, cex.axis=0.9, at=c(0,25,50,75,100))
  axis(2, pos=12.5, las=1, cex.axis=0.9, at=c(0,100), labels=c("23.5° N", "80° N"))
  abline(h=0,lty=2, lwd=0.75, col="darkgreen")
  mtext("Migrational Percentage of terrestrial mammals with both \nNorth and South American origin during the last 12 Ma", 
	side=3, line= 2, cex=1.1)

  text(as.numeric(names(table(temp_norte$v.timeBin))), 
	c(35, 40, 55, 50, 7, 7, 17, 9, 9), 
	paste("n=", as.numeric(table(temp_norte$v.timeBin)), sep=""), cex=0.9)

   legend(12,97, legend=c("North American", "South American"),pch= 22,cex=0.9, 
	bg="white", col="black",pt.bg=c("coral","lightblue"),pt.cex=2)
  boxplot(gmams$PercentSAO ~ gmams$by_latelev, subset = gmams$by_latelev == "NA_Te",at=-0.5,col ="lightblue", outline = F, add=T,axes =F)

##								   
  par(mar=c(0,0,0,0), lwd=0.6, xaxs="i", yaxs="i")
  plot(0,t="n",ylim=c(0,100), xlim=c(12.5, -1.5), axes=F)
    rect(12.5, 0, -1.5, 100,col=rgb(0.9, 0, 0 ,0.1), border=NA)
  boxplot(trop_norte$percent_sur ~ as.factor(trop_norte$v.timeBin),
	ylim=c(0,100), xlim=c(12.5, -1.5), boxwex=0.5, cex.axis=0.7, las=1, pch=20, 
	cex=0.75, boxlwd=0.85, axes=F, at=unique(trop_norte$v.timeBin), col="lightblue", add=T)

  legend("topleft", "Tropic of Cancer",text.col ="darkgreen", bty="n")
  segments(12.5,-0.5,12.5,100, lwd=0.75)
  segments(-1.5,-1.5,-1.5,100, lwd=0.75)
  axis(4, pos=-1.5, las=1, cex.axis=0.9, at=c(0,25,50,75))
  abline(h=0,lty=2, lwd=0.75, col="grey")
  axis(2, pos=12.5, las=1, cex.axis=0.9, at=c(0, 100), labels=c("13° N", ""))

  text(as.numeric(names(table(trop_norte$v.timeBin))), c(82, 55, 45, 25), 
	paste("n=", as.numeric(table(trop_norte$v.timeBin)), sep=""), cex=0.9)
  boxplot(gmams$PercentSAO~ gmams$by_latelev, subset = gmams$by_latelev == "NA_Tr",at=-0.5,col ="lightblue", outline = F, add=T,axes =F)

##

  par(mar=c(0,0,0,0), lwd=0.6, xaxs="i", yaxs="i")
  plot(0,t="n",ylim=c(0,100), xlim=c(12.5, -1.5), axes=F)
    rect(12.5, 0, -1.5, 100,col=rgb(0, 0.2, 0.9 ,0.1), border=NA)
  boxplot(trop_sur$percent_norte ~ as.factor(trop_sur$v.timeBin), 
	ylim=c(0,100), xlim=c(12.5, -0.5), boxwex=0.5, cex.axis=0.7, las=1, pch=20, 
	cex=0.75, boxlwd=0.85, axes=F, at=unique(trop_sur$v.timeBin), col="coral", add=T)

  segments(12.5,-1.5,12.5,100, lwd=0.75)
  segments(-1.5,-1.5,-1.5,100, lwd=0.75)
  axis(4, pos=-1.5, las=1, cex.axis=0.9, at=c(0,25,50,75))
  abline(h=0,lty=2, lwd=0.75, col="darkgreen")
  axis(2, pos=12.5, las=1, cex.axis=0.9, at=c(100,0), labels=c("", ""))
  mtext("          Paleolatitude", side=2, line=3.75, cex=1.2)

  text(as.numeric(names(table(trop_sur$v.timeBin))), c(76, 72, 25, rep(10,3)), 
	paste("n=", as.numeric(table(trop_sur$v.timeBin)), sep=""), cex=0.9)

  boxplot(gmams$PercentNAO~ gmams$by_latelev, subset = gmams$by_latelev == "SA_Tr",at=-0.5,col ="coral", outline = F, add=T,axes =F)


## Subset localities on latitude less than -23.5

  par(mar=c(0,0,0,0), lwd=0.6, xaxs="i", yaxs="i", xpd=TRUE)
  plot(0,t="n",ylim=c(0,100), xlim=c(12.5, -1.5), axes=F)
    rect(12.5, 0, -1.5, 100,col=rgb(0, 0.2, 0.9 ,0.1), border=NA)
  boxplot(temp_sur$percent_norte ~ as.factor(temp_sur$v.timeBin),
	ylim=c(0,100), xlim=c(12.5, -0.5), boxwex=0.5, cex.axis=0.7, las=1, pch=20, 
	cex=0.75, boxlwd=0.85, axes=F, at=unique(temp_sur$v.timeBin), col="coral", lwd=0.7, add=T)

  legend("topleft", "Tropic of Capricorn",text.col ="darkgreen", bty="n")
  segments(-1.5,-1.5,-1.5,100, lwd=0.75)
  axis(1, pos=0, lwd=0.9, at=seq(-0.5, 11.5, 1), labels=c(0,seq(0.5, 11.5, 1)))
  axis(1, pos=0, at=c(-1.5,13), labels=c("",""), lwd=0.5)
  axis(4, pos=-1.5, las=1, cex.axis=0.9, at=c(0,25,50,75))
  axis(2, pos=12.5, las=1, cex.axis=0.9, at=c(100,0), labels=c("23.5° S", "80° S"))
  mtext("Time (ma)", side=1, line=3.75, cex=1.2)

  text(as.numeric(names(table(temp_sur$v.timeBin))), c(85, 55, 30, 13, 12, 12, 12), 
		paste("n=", as.numeric(table(temp_sur$v.timeBin)), sep=""), cex=0.9)
  boxplot(gmams$PercentNAO~ gmams$by_latelev, subset = gmams$by_latelev == "SA_Te",at=-0.5,col ="coral", outline = F, add=T,axes =F)

#
# dev.off()
