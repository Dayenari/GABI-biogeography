# setwd(" ... /GABI-biogeography")

load("Mammals_data/Biogeog_mams")

# AGGREGATE DATA BY BIOMES = all mammals = 

# AH & YM together
# take out repeated species by biome
biomams1 <- unique(biogeo_mams[ , c("SCIENTIFIC_NAME", "ORDER","NEW_BIOME_1", "Origin_Class")])

bybiome1 <- aggregate(biomams1$SCIENTIFIC_NAME ~ biomams1$NEW_BIOME_1, FUN = length)
names(bybiome1)<- c("Biome","RICHNESS")

bybiome1$NAO <- aggregate(biomams1$Origin ~ biomams1$NEW_BIOME_1, FUN = table)[,2][,1]
bybiome1$SAO <- aggregate(biomams1$Origin ~ biomams1$NEW_BIOME_1, FUN = table)[,2][,2]

bybiome1$propNAO <- bybiome1$NAO * 100 / bybiome1$RICHNESS
bybiome1$propSAO <- bybiome1$SAO * 100 / bybiome1$RICHNESS

# AH & YM separated
# take out repeated species by biome
biomams2 <- unique(biogeo_mams[ , c("SCIENTIFIC_NAME", "ORDER","NEW_BIOME_2", "Origin_Class")])

bybiome2 <- aggregate(biomams2$SCIENTIFIC_NAME ~ biomams2$NEW_BIOME_2, FUN = length)
names(bybiome2)<- c("Biome","RICHNESS")

bybiome2$NAO <- aggregate(biomams2$Origin ~ biomams2$NEW_BIOME_2, FUN = table)[,2][,1]
bybiome2$SAO <- aggregate(biomams2$Origin ~ biomams2$NEW_BIOME_2, FUN = table)[,2][,2]

bybiome2$propNAO <- bybiome2$NAO * 100 / bybiome2$RICHNESS
bybiome2$propSAO <- bybiome2$SAO * 100 / bybiome2$RICHNESS

bybiome1[1,1] <- "AH+YM"
bybiome <- rbind(bybiome2,bybiome1[1,])

# - - - - - - - - - - - - - - - - - - -

# ONLY RODENTS

rbioms <- biogeo_mams[biogeo_mams$ORDER == "Rodentia", ]

# AH & YM together
rbioms1 <- unique(rbioms[ , c("SCIENTIFIC_NAME", "ORDER","NEW_BIOME_1", "Origin_Class")])

rbybiome1 <- aggregate(rbioms1$SCIENTIFIC_NAME ~ rbioms1$NEW_BIOME_1, FUN = length)
names(rbybiome1)<- c("Biome","RICHNESS")

rbybiome1$NAO <- aggregate(rbioms1$Origin ~ rbioms1$NEW_BIOME_1, FUN = table)[,2][,1]
rbybiome1$SAO <- aggregate(rbioms1$Origin ~ rbioms1$NEW_BIOME_1, FUN = table)[,2][,2]

rbybiome1$propNAO <- rbybiome1$NAO * 100 / rbybiome1$RICHNESS
rbybiome1$propSAO <- rbybiome1$SAO * 100 / rbybiome1$RICHNESS

# AH & YM separated
rbioms2 <- unique(rbioms[ , c("SCIENTIFIC_NAME", "ORDER","NEW_BIOME_2", "Origin_Class")])

rbybiome2 <- aggregate(rbioms2$SCIENTIFIC_NAME ~ rbioms2$NEW_BIOME_2, FUN = length)
names(rbybiome2)<- c("Biome","RICHNESS")

rbybiome2$NAO <- aggregate(rbioms2$Origin ~ rbioms2$NEW_BIOME_2, FUN = table)[,2][,1]
rbybiome2$SAO <- aggregate(rbioms2$Origin ~ rbioms2$NEW_BIOME_2, FUN = table)[,2][,2]

rbybiome2$propNAO <- rbybiome2$NAO * 100 / rbybiome2$RICHNESS
rbybiome2$propSAO <- rbybiome2$SAO * 100 / rbybiome2$RICHNESS

rbybiome1[1,1] <- "AH+YM"
rbybiome <- rbind(rbybiome2,rbybiome1[1,])

# - - - - - - - - - - - - - - - - - - -

# WITHOUT RODENTS

wrbioms <- biogeo_mams[biogeo_mams$ORDER != "Rodentia", ]

# AH & YM together
wrbioms1 <- unique(wrbioms[ , c("SCIENTIFIC_NAME", "ORDER","NEW_BIOME_1", "Origin_Class")])

wrbybiome1 <- aggregate(wrbioms1$SCIENTIFIC_NAME ~ wrbioms1$NEW_BIOME_1, FUN = length)
names(wrbybiome1)<- c("Biome","RICHNESS")

wrbybiome1$NAO <- aggregate(wrbioms1$Origin ~ wrbioms1$NEW_BIOME_1, FUN = table)[,2][,1]
wrbybiome1$SAO <- aggregate(wrbioms1$Origin ~ wrbioms1$NEW_BIOME_1, FUN = table)[,2][,2]

wrbybiome1$propNAO <- wrbybiome1$NAO * 100 / wrbybiome1$RICHNESS
wrbybiome1$propSAO <- wrbybiome1$SAO * 100 / wrbybiome1$RICHNESS

# AH & YM separated
wrbioms2 <- unique(wrbioms[ , c("SCIENTIFIC_NAME", "ORDER","NEW_BIOME_2", "Origin_Class")])

wrbybiome2 <- aggregate(wrbioms2$SCIENTIFIC_NAME ~ wrbioms2$NEW_BIOME_2, FUN = length)
names(wrbybiome2)<- c("Biome","RICHNESS")

wrbybiome2$NAO <- aggregate(wrbioms2$Origin ~ wrbioms2$NEW_BIOME_2, FUN = table)[,2][,1]
wrbybiome2$SAO <- aggregate(wrbioms2$Origin ~ wrbioms2$NEW_BIOME_2, FUN = table)[,2][,2]

wrbybiome2$propNAO <- wrbybiome2$NAO * 100 / wrbybiome2$RICHNESS
wrbybiome2$propSAO <- wrbybiome2$SAO * 100 / wrbybiome2$RICHNESS

wrbybiome1[1,1] <- "AH+YM"
wrbybiome <- rbind(wrbybiome2,wrbybiome1[1,])

#. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

# = = = P L O T S  = = = (proportions of NAO / SAO)

bor <-c(3,7,4,8,5,9,6,10,1,11,12) # just an order for BIOMES

# pdf("NAO_proportion_biome_SEP_2021.pdf")

par(mfrow=c(3,1), mar= c(4,4,2,2))

barplot(t(bybiome[bor,5:6]), las= 1,col=c("lightblue","coral"), las=1, ylab= "% NAO",xlim=c(1,15),
	names.arg= bybiome[bor,1], cex.axis=0.7, cex.names=0.7, space= c(0.5,0.25), ylim=c(0,110), main= "TOTAL") 

text(c(1,2.25,3.75,5,6.5,7.75,9.25,10.5,12,13.25,14.75),105,labels=paste ("S",bybiome$RICHNESS[bor],sep="="),cex=0.7)

barplot(t(rbybiome[bor,5:6]), las= 1,col=c("lightblue","coral"), las=1, ylab= "% NAO",xlim=c(1,15),
	names.arg= rbybiome[bor,1], cex.axis=0.7, cex.names=0.7, space= c(0.5,0.25), ylim=c(0,110), main= "Rodents") 

text(c(1,2.25,3.75,5,6.5,7.75,9.25,10.5,12,13.25,14.75),105,labels=paste ("S",rbybiome$RICHNESS[bor],sep="="),cex=0.7)

barplot(t(wrbybiome[bor,5:6]), las= 1,col=c("lightblue","coral"), las=1, ylab= "% NAO",xlim=c(1,15),
	names.arg= wrbybiome[bor,1], cex.axis=0.7, cex.names=0.7, space= c(0.5,0.25), ylim=c(0,110), main= "Non-Rodents") 

text(c(1,2.25,3.75,5,6.5,7.75,9.25,10.5,12,13.25,14.75),105,labels=paste ("S",wrbybiome$RICHNESS[bor],sep="="),cex=0.7)

# dev.off()

