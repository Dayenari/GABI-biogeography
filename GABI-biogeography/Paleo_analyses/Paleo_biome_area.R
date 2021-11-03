# setwd(" ... /GABI-biogeography")


  final.table_biome <- read.csv("Paleo_analyses/final_new_loc_with_biome.csv")

## Create vector to group PLATE IDs that establish a Biome.
## Exclude the PLATE IDs with cero ocurrences.
  NATeG <- c(10024, 10046, 10021) 
  NATeF <- 10027
  SATrG <- c(10012, 10006, 10036)
  SATeG <- c(20027, 20015, 20006, 20009, 20024)
  AH <- c(20012, 20018, 10030)
  SATrF <- c(10033, 10003, 10015, 10009)
  NATrF <- c(10043, 10018, 10076, 10067)
  SATeF <- 20021

## Subset new-localities by biome and timeBin less than 12 Ma.
  NATeG_table <- final.table_biome[which(final.table_biome$PLATEID1 %in% NATeG & final.table_biome$v_timBn < 12),]
  NATeF_table <- final.table_biome[which(final.table_biome$PLATEID1 %in% NATeF & final.table_biome$v_timBn < 12),]
  SATrG_table <- final.table_biome[which(final.table_biome$PLATEID1 %in% SATrG & final.table_biome$v_timBn < 12),]
  SATeG_table <- final.table_biome[which(final.table_biome$PLATEID1 %in% SATeG & final.table_biome$v_timBn < 12),]
  SATrF_table <- final.table_biome[which(final.table_biome$PLATEID1 %in% SATrF & final.table_biome$v_timBn < 12),]
  NATrF_table <- final.table_biome[which(final.table_biome$PLATEID1 %in% NATrF & final.table_biome$v_timBn < 12),]
  SATeF_table <- final.table_biome[which(final.table_biome$PLATEID1 %in% SATeF& final.table_biome$v_timBn < 12),]
  AH_table <- final.table_biome[which(final.table_biome$PLATEID1 %in% AH & final.table_biome$v_timBn < 12),]

## Read information stored in "table_area_biome_by_time.csv"
## It contains the area of each biome at every Time Snapshot. (range [0 - 55 Ma], each 0.5 Ma)

  cum_area <- read.csv("Paleo_analyses/table_area_biome_by_time.csv")

## Units in million km^2
  cum_area$AREA_Mkm2 <- round((cum_area$AREA / 1000000), 1) 

## Create a vector with timeBin index less than 12 Ma.
  final_new_loc <- read.csv("Paleo_analyses/final_table_graph_GABI_15_nov16.csv",  stringsAsFactors = FALSE)

  timebin <- unique(final_new_loc[(final_new_loc$v.timeBin) <= 11.5, "v.timeBin"])

## Subset biome areas table (table_area_biome_by_time.csv) by Biome and time Bin.
  NATeG_area <- cum_area[which(cum_area$Biome == "NATeG" & cum_area$TIME %in% timebin),]
  NATeF_area <- cum_area[which(cum_area$Biome == "NATeF" & cum_area$TIME %in% timebin),]
  SATrG_area <- cum_area[which(cum_area$Biome == "SATrG" & cum_area$TIME %in% timebin),]
  SATeG_area <- cum_area[which(cum_area$Biome == "SATeG" & cum_area$TIME %in% timebin),]
  AH_area <- cum_area[which(cum_area$Biome == "AH" & cum_area$TIME %in% timebin),]
  SATrF_area <- cum_area[which(cum_area$Biome == "SATrF" & cum_area$TIME %in% timebin),]
  NATrF_area <- cum_area[which(cum_area$Biome == "NATrF" & cum_area$TIME %in% timebin),]
  SATeF_area <- cum_area[which(cum_area$Biome == "SATeF" & cum_area$TIME %in% timebin),]

# . . . . . . . . . . . . . . . . . . . . . . . . 

## Modern spatial data 

  load(file="Mammals_data/mamPhy360_wwf360Grid_land_NewWorld_NewBiomeAreas_REFINED.Rda")
  # Ignore Warnings (about ecorregion names punctuation)

# Take out islands
  cmams <- mamPhy360_NW_wNewBiomes[mamPhy360_NW_wNewBiomes$IsIsland == 0,]

# Rectification from Paramos (JUAN's)
  rectified <- read.csv("Mammals_data/Mountain Ecoregion Species.csv") 
  not <- rectified[which(rectified$RECTIFIED == "yes" ), c("GENUS","SPECIES","ECOREGION.CODE") ]
  not_c <- paste(not$GENUS, not$SPECIES, not$ECOREGION.CODE, sep= "_")
  mams_c <- paste(cmams$scientificname,cmams$eco_code, sep = "_")

# which(mams_c %in% not_c == TRUE)# 292 rectified records (15 unique records)
  spamams <- cmams[mams_c %in% not_c == FALSE, ]

# richness per ecoregion (by ecorregion_code)
  ecoregs <- aggregate(spamams$scientificname ~ spamams$eco_code, FUN = length)
  colnames(ecoregs)<-c("ECO_CODE","Richness")
  ecoregs$NAO <- aggregate(spamams$Origin_Class ~ spamams$eco_code, FUN = table)[,2][,1]
  ecoregs$SAO <- aggregate(spamams$Origin_Class ~ spamams$eco_code, FUN = table)[,2][,2]
  ecoregs$Prop_NAO <- ecoregs$NAO / ecoregs$Richness
  ecoregs$Prop_SAO <- ecoregs$SAO / ecoregs$Richness
  ecoregs$New_Biome <- aggregate(spamams$New_Biome ~ spamams$eco_code, FUN = unique)[,2]

# total area by biome (some cells holds >1 ecorregion oand/or biomes)

# how many biomes  on each cell?
  biomes_in_cell = aggregate(spamams$New_Biome ~ spamams$HBWID , FUN = function(x) length(unique(x)))

# proportional area by biome on each cell
  biomes_in_cell$prop_biome <-1/(biomes_in_cell[,2])

# auxiliar function to easily swap objects from a dataframe
  swap <- function(x, from, to) {   tmp <- to[ match(x, from) ]
  	return(tmp)}

# which unique cells within each biome
  cells_in_biomes <- aggregate(spamams$HBWID  ~ spamams$New_Biome , FUN = unique)
  prop_biomes <-  lapply(cells_in_biomes[,2],FUN = function(x) swap(x, biomes_in_cell[,1], biomes_in_cell[,3]))

  area_by_biome <-data.frame(Biome = cells_in_biomes[,1], n_cells = unlist(lapply(prop_biomes,sum)))
  area_by_biome$Area <- area_by_biome[,2]* 12364.3
  area_by_biome$AREA_Mkm2 <- round((area_by_biome[,3] / 1000000), 1) 

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 



 # PLOTS  MIGRATION BY BIOME + AREA

  ## Boxplot biome NATeG = North American temperate Grasslands/shrublands/xerophytic
 
    par(mar=c(3, 3, 3, 3))
    plot(0,t="n",ylim=c(0,100), xlim=c(12.5, -1.5), axes=F)
    rect(12.5, 0, -1.5, 100,col=rgb(0.9, 0, 0 ,0.1), border="black")
    boxplot(NATeG_table$prcnt_s ~ as.factor(NATeG_table$v_timBn), 
	 boxwex = 0.5, cex.axis=0.7, , axes=F, pch=20, cex=0.75, boxlwd=0.85, 
	 at=unique(NATeG_table$v_timBn), col="lightblue", lwd=0.7, add=T)

    axis(4, pos=-1.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	tcl=0.4, mgp = c(2, 0.3, 0))
    mtext("Percent South American (%)", side=4, line=1, cex=1.2)
    mtext("Biome NATeG", side=3, line= 0.75, cex=1.1)
    text(as.numeric(names(table(NATeG_table$v_timBn))), 
	c(16, 21, 13, 17, 12, 3, 2), 
	paste("n=", as.numeric(table(NATeG_table$v_timBn)), 
	      sep=""), cex=0.7)
    points(NATeG_area$TIME, NATeG_area$AREA_Mkm2 * 4, 
	NATeG_area$AREA, cex=1, col="red", type="b", pch=10)
    axis(2, pos=12.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	labels=seq(0, 25, 5), tcl=0.4, mgp = c(2, 0.3, 0), col="red", 
	col.axis="red")
    mtext("Area (million square kilometers)", side=2, line=1, cex=1.2, col=2)
    axis(1, pos=0, lwd=0.9, at=seq(-0.5, 11.5, 1), cex.axis=0.8, labels= c(0,seq(0.5, 11.5, 1)))
    mtext("Time (ma)", side=1, line=1.75, cex=1.2)
  
   legend(12,97, legend=c("South American", "Area"),pch=c(22, 10),cex=0.9, 
	bg="white", col=c("black","red"),pt.bg="lightblue",pt.cex=c(2.5,1))

# modern
  boxplot(100*(ecoregs$Prop_SAO) ~ ecoregs$New_Biome, subset = ecoregs$New_Biome == "NA_TeG",
	boxwex = 0.5, cex.axis=0.7, axes=F, pch=20, cex=0.75, boxlwd = 0.85, 
	at = rep(-0.5,10), col = "lightblue", lwd = 0.7, add = T)
  a <- area_by_biome[which(area_by_biome[,1]== "NA_TeG"),3]
  points (-0.5, (a/1000000)*4, pch=10,col=2)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


  ## Boxplot biome NATeF = North American Temperate Forests

    par(mar=c(3, 3, 3, 3))
    plot(0,t="n",ylim=c(0,100), xlim=c(12.5, -1.5), axes=F)
    rect(12.5, 0, -1.5, 100,col=rgb(0.9, 0, 0 ,0.1), border="black")
    boxplot(NATeF_table$prcnt_s ~ as.factor(NATeF_table$v_timBn), 
	 boxwex=0.5, cex.axis=0.7, , axes=F, pch=20, cex=0.75, boxlwd=0.85, 
	 at=unique(NATeF_table$v_timBn), col="lightblue", lwd=0.7, add=T)

    axis(4, pos=-1.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	tcl=0.4, mgp = c(2, 0.3, 0))
    mtext("Percent South American (%)", side=4, line=1, cex=1.2)
    mtext("Biome NATeF", side=3, line= 0.75, cex=1.1)
    text(as.numeric(names(table(NATeF_table$v_timBn))), 
	c(27, 33, 49, 42, 2, 2, 11, 2),
	paste("n=", as.numeric(table(NATeF_table$v_timBn)), 
	      sep=""), cex=0.7)
    points(NATeF_area$TIME, NATeF_area$AREA_Mkm2* 4, 
	NATeF_area$AREA_Mkm2, cex=1, col="red", type="b", pch=10)
    axis(2, pos=12.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	labels=seq(0, 25, 5), tcl=0.4, mgp = c(2, 0.3, 0), col="red", 
	col.axis="red")
    mtext("Area (million square kilometers)", side=2, line=1, cex=1.2, col=2)
    axis(1, pos=0, lwd=0.9, at=seq(-0.5, 11.5, 1), cex.axis=0.8, labels= c(0,seq(0.5, 11.5, 1)))
    mtext("Time (ma)", side=1, line=1.75, cex=1.2)
  
   legend(12,97, legend=c("South American", "Area"),pch=c(22, 10),cex=0.9, 
	bg="white", col=c("black","red"),pt.bg="lightblue",pt.cex=c(2.5,1))
    
# modern
  boxplot(100*(ecoregs$Prop_SAO) ~ ecoregs$New_Biome, subset = ecoregs$New_Biome == "NA_TeF",
	boxwex=0.5, cex.axis=0.7, axes=F, pch=20, cex=0.75, boxlwd=0.85, 
	at = rep(-0.5,10), col = "lightblue", lwd = 0.7, add = T)

  a <- area_by_biome[which(area_by_biome[,1]== "NA_TeF"),3]
  points (-0.5, (a/1000000)*4, pch=10,col=2)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 


  ## Boxplot biome SATrG = South American Tropical Grasslands/shrublands/xerophytic/desert

    par(mar=c(3, 3, 3, 3))
    plot(0,t="n",ylim=c(0,100), xlim=c(12.5, -1.5), axes=F)
    rect(12.5, 0, -1.5, 100,col=rgb(0, 0.2, 0.9 ,0.1), border="black")
    boxplot(SATrG_table$prcnt_n ~ as.factor(SATrG_table$v_timBn), 
	 boxwex = 0.5, cex.axis=0.7, axes = F, pch=20, cex=0.75, boxlwd=0.85, 
	 at = unique(SATrG_table$v_timBn), col="coral", lwd=0.7, add=T)

    axis(4, pos=-1.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	tcl=0.4, mgp = c(2, 0.3, 0))
    mtext("Percent North American (%)", side=4, line=1, cex=1.2)
    mtext("Biome SATrG", side=3, line= 0.75, cex=1.1)
    text(as.numeric(names(table(SATrG_table$v_timBn))), 
	c(68, 65),
	paste("n=", as.numeric(table(SATrG_table$v_timBn)), 
	      sep=""), cex=0.7)
    points(SATrG_area$TIME, SATrG_area$AREA_Mkm2* 4, 
	SATrG_area$AREA_Mkm2, cex=1, col="red", type="b", pch=10)
    axis(2, pos=12.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	labels=seq(0, 25, 5), tcl=0.4, mgp = c(2, 0.3, 0), col="red", 
	col.axis="red")
    mtext("Area (million square kilometers)", side=2, line=1, cex=1.2, col=2)
    axis(1, pos=0, lwd=0.9, at=seq(-0.5, 11.5, 1), cex.axis=0.8, labels= c(0,seq(0.5, 11.5, 1)))
    mtext("Time (ma)", side=1, line=1.75, cex=1.2)
  
   legend(12,97, legend=c("North American", "Area"),pch=c(22, 10),cex=0.9, 
	bg="white", col=c("black","red"),pt.bg="coral",pt.cex=c(2.5,1))
    
# modern
  boxplot(100*(ecoregs$Prop_NAO) ~ ecoregs$New_Biome, subset = ecoregs$New_Biome == "SA_TrG",
	boxwex = 0.5, cex.axis=0.7,axes=F, pch=20, cex=0.75, boxlwd=0.85, 
	at = rep(-0.5,10), col = "coral", lwd = 0.7, add = T)

  a <- area_by_biome[which(area_by_biome[,1]== "SA_TrG"),3]
  points (-0.5, (a/1000000)*4, pch=10,col=2)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

 
  ## Boxplot biome SATeG = South American Temperate Grasslands/shrublands/xerophytic (including paramo)

    par(mar=c(3, 3, 3, 3))
    plot(0,t="n",ylim=c(0,100), xlim=c(12.5, -1.5), axes=F)
    rect(12.5, 0, -1.5, 100,col=rgb(0, 0.2, 0.9 ,0.1), border="black")
    boxplot(SATeG_table$prcnt_n ~ as.factor(SATeG_table$v_timBn), 
	 boxwex=0.5, cex.axis=0.7, axes=F, pch=20, cex=0.75, boxlwd=0.85, 
	 at=unique(SATeG_table$v_timBn), col="coral", lwd=0.7, add=T)

    axis(4, pos=-1.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	tcl=0.4, mgp = c(2, 0.3, 0))
    mtext("Percent North American (%)", side=4, line=1, cex=1.2)
    mtext("Biome SATeG", side=3, line= 0.75, cex=1.1)
    text(as.numeric(names(table(SATeG_table$v_timBn))), 
	c(57, 61, 28, 10, 8, 9, 10), 
	paste("n=", as.numeric(table(SATeG_table$v_timBn)), 
	      sep=""), cex=0.7)
    points(SATeG_area$TIME, SATeG_area$AREA_Mkm2* 4, 
	SATeG_area$AREA_Mkm2, cex=1, col="red", type="b", pch=10)
    axis(2, pos=12.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	labels=seq(0, 25, 5), tcl=0.4, mgp = c(2, 0.3, 0), col="red", 
	col.axis="red")
    mtext("Area (million square kilometers)", side=2, line=1, cex=1.2, col=2)
    axis(1, pos=0, lwd=0.9, at=seq(-0.5, 11.5, 1), cex.axis=0.8, labels= c(0,seq(0.5, 11.5, 1)))
    mtext("Time (ma)", side=1, line=1.75, cex=1.2)
  
   legend(12,97, legend=c("North American", "Area"),pch=c(22, 10),cex=0.9, 
	bg="white", col=c("black","red"),pt.bg="coral",pt.cex=c(2.5,1))
    
# modern
  boxplot(100*(ecoregs$Prop_NAO) ~ ecoregs$New_Biome, subset = ecoregs$New_Biome == "SA_TeG",
	boxwex=0.5, cex.axis=0.7,axes=F, pch=20, cex=0.75, boxlwd=0.85, 
	at = rep(-0.5,10), col = "coral", lwd = 0.7, add = T)

  a <- area_by_biome[which(area_by_biome[,1]== "SA_TeG"),3]
  points (-0.5, (a/1000000)*4, pch=10,col=2)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

  ## Boxplot biomes AH = Andes >2000m within tropical zone (andean cloud forest) 
     ## and Andes > 300m within temperate regions (Yungas)
  
    par(mar=c(3, 3, 3, 3))
    plot(0,t="n",ylim=c(0,100), xlim=c(12.5, -1.5), axes=F)
    rect(12.5, 0, -1.5, 100,col=rgb(0, 0.2, 0.9 ,0.1), border="black")
    boxplot(AH_table$prcnt_n ~ as.factor(AH_table$v_timBn), 
	 boxwex=0.5, cex.axis=0.7, axes=F, pch=20, cex=0.75, boxlwd=0.85, 
	 at=unique(AH_table$v_timBn), col="coral", lwd=0.7, add=T)

    axis(4, pos=-1.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	tcl=0.4, mgp = c(2, 0.3, 0))
    mtext("Percent North American (%)", side=4, line=1, cex=1.2)
    mtext("Biome AH", side=3, line= 0.75, cex=1.1)
    text(as.numeric(names(table(AH_table$v_timBn))), 
	 c(82, 38, 11, 3, 3), 
	paste("n=", as.numeric(table(AH_table$v_timBn)), 
	      sep=""), cex=0.7)
    points(AH_area$TIME, AH_area$AREA_Mkm2* 4, 
	AH_area$AREA_Mkm2, cex=1, col="red", type="b", pch=10)
    axis(2, pos=12.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	labels=seq(0, 25, 5), tcl=0.4, mgp = c(2, 0.3, 0), col="red", 
	col.axis="red")
    mtext("Area (million square kilometers)", side=2, line=1, cex=1.2, col=2)
    axis(1, pos=0, lwd=0.9, at=seq(-0.5, 11.5, 1), cex.axis=0.8, labels= c(0,seq(0.5, 11.5, 1)))
    mtext("Time (ma)", side=1, line=1.75, cex=1.2)
  
   legend(12,97, legend=c("North American", "Area"),pch=c(22, 10),cex=0.9, 
	bg="white", col=c("black","red"),pt.bg="coral",pt.cex=c(2.5,1))

# modern
  boxplot(100*(ecoregs$Prop_NAO) ~ ecoregs$New_Biome, subset = ecoregs$New_Biome == "A_H",
	boxwex=0.5, cex.axis=0.7,axes=F, pch=20, cex=0.75, boxlwd=0.85, 
	at = rep(-0.5,10), col = "coral", lwd = 0.7, add = T)

  a <- area_by_biome[which(area_by_biome[,1]== "A_H"),3]
  points (-0.5, (a/1000000)*4, pch=10,col=2)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

  ## Boxplot biomes SATrF = South American Tropical Rain Forests
  
    par(mar=c(3, 3, 3, 3))
    plot(0,t="n",ylim=c(0,100), xlim=c(12.5, -1.5), axes=F)
    rect(12.5, 0, -1.5, 100,col=rgb(0, 0.2, 0.9 ,0.1), border="black")
    boxplot(SATrF_table$prcnt_n ~ as.factor(SATrF_table$v_timBn), 
	 boxwex=0.5, cex.axis=0.7, axes=F, pch=20, cex=0.75, boxlwd=0.85, 
	 at=unique(SATrF_table$v_timBn), col="coral", lwd=0.7, add=T)

    axis(4, pos=-1.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	tcl=0.4, mgp = c(2, 0.3, 0))
    mtext("Percent North American (%)", side=4, line=1, cex=1.2)
    mtext("Biome SATrF", side=3, line= 0.75, cex=1.1)
    text(as.numeric(names(table(SATrF_table$v_timBn))), 
	c(21, 3, 3),
	paste("n=", as.numeric(table(SATrF_table$v_timBn)), 
	      sep=""), cex=0.7)
    points(SATrF_area$TIME, SATrF_area$AREA_Mkm2 * 4, 
	SATrF_area$AREA_Mkm2, cex=1, col="red", type="b", pch=10)
    axis(2, pos=12.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	labels=seq(0, 25, 5), tcl=0.4, mgp = c(2, 0.3, 0), col="red", 
	col.axis="red")
    mtext("Area (million square kilometers)", side=2, line=1, cex=1.2, col=2)
    axis(1, pos=0, lwd=0.9, at=seq(-0.5, 11.5, 1), cex.axis=0.8, labels= c(0,seq(0.5, 11.5, 1)))
    mtext("Time (ma)", side=1, line=1.75, cex=1.2)
  
   legend(12,97, legend=c("North American", "Area"),pch=c(22, 10),cex=0.9, 
	bg="white", col=c("black","red"),pt.bg="coral",pt.cex=c(2.5,1))

# modern
  boxplot(100*(ecoregs$Prop_NAO) ~ ecoregs$New_Biome, subset = ecoregs$New_Biome == "SA_TrF",
	boxwex=0.5, cex.axis=0.7,axes=F, pch=20, cex=0.75, boxlwd=0.85, 
	at = rep(-0.5,10), col = "coral", lwd = 0.7, add = T)

  a <- area_by_biome[which(area_by_biome[,1]== "SA_TrF"),3]
  points (-0.5, (a/1000000)*4, pch=10,col="white")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

  ## Boxplot biomes NATrF = North American Tropical Rain Forests    

    par(mar=c(3, 3, 3, 3))
    plot(0,t="n",ylim=c(0,100), xlim=c(12.5, -1.5), axes=F)
    rect(12.5, 0, -1.5, 100,col=rgb(0.9, 0, 0 ,0.1), border="black")
    boxplot(NATrF_table$prcnt_s ~ as.factor(NATrF_table$v_timBn), 
	 boxwex=0.5, cex.axis=0.7, axes=F, pch=20, cex=0.75, boxlwd=0.85, 
	 at=unique(NATrF_table$v_timBn), col="lightblue", lwd=0.7, add=T)

    axis(4, pos=-1.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	tcl=0.4, mgp = c(2, 0.3, 0))
    mtext("Percent South American (%)", side=4, line=1, cex=1.2)
    mtext("Biome NATrF", side=3, line= 0.75, cex=1.1)
    text(as.numeric(names(table(NATrF_table$v_timBn))), 
	c(74, 37),
	paste("n=", as.numeric(table(NATrF_table$v_timBn)), 
	      sep=""), cex=0.7)
    points(NATrF_area$TIME, NATrF_area$AREA_Mkm2* 4, 
	NATrF_area$AREA_Mkm2, cex=1, col="red", type="b", pch=10)
    axis(2, pos=12.5, las=1, cex.axis=0.9, at=seq(0, 100, 20), 
	labels=seq(0, 25, 5), tcl=0.4, mgp = c(2, 0.3, 0), col="red", 
	col.axis="red")
    mtext("Area (million square kilometers)", side=2, line=1, cex=1.2, col=2)
    axis(1, pos=0, lwd=0.9, at=seq(-0.5, 11.5, 1), cex.axis=0.8, labels= c(0,seq(0.5, 11.5, 1)))
    mtext("Time (ma)", side=1, line=1.75, cex=1.2)
  
   legend(12,97, legend=c("South American", "Area"),pch=c(22, 10),cex=0.9, 
	bg="white", col=c("black","red"),pt.bg="lightblue",pt.cex=c(2.5,1))
# modern
  boxplot(100*(ecoregs$Prop_NAO) ~ ecoregs$New_Biome, subset = ecoregs$New_Biome == "NA_TrF",
	boxwex=0.5, cex.axis=0.7,axes=F, pch=20, cex=0.75, boxlwd=0.85, 
	at = rep(-0.5,10), col = "lightblue", lwd = 0.7, add = T)

  a <- area_by_biome[which(area_by_biome[,1]== "NA_TrF"),3]
  points (-0.5, (a/1000000)*4, pch=10,col=2)

#  dev.off()

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 









