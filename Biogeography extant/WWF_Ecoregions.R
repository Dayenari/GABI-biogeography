# Distribution of mammals in the Americas among different Biomes

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
# auxiliar function su wap objects from a dataframe
swap <- function(x, from, to) {   tmp <- to[ match(x, from) ]
  return(tmp)}
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# Load and clean WWF data
# To run this script you will need to download the files of WildFinder Database
# https://www.worldwildlife.org/publications/wildfinder-database

# set the working directory to the folder containing this script:
# (similar to RStudio menu "Session - Set Working Directory - To Source File Location"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()

# Load data from the WildFinder Database

# Option 1
install.packages("RODBC")
library(RODBC)

wwf <- odbcConnectAccess2007(path.expand("WildfinderUpdate.mdb")) 
spp <- sqlFetch(wwf, "species") 
genus <- sqlFetch(wwf, "genus") 
family <- sqlFetch(wwf, "family")
order <- sqlFetch(wwf, "order_")
classx <- sqlFetch(wwf, "class")
ecorspp <- sqlFetch(wwf, "ecoregion_species")
ecor <- sqlFetch(wwf, "ecoregions") 
biome <- sqlFetch(wwf, "biomes") 

# Option 2
# You must install the mdbtools package
# https://github.com/mdbtools/mdbtools
# For a Mac in the terminal 
# install Home brew https://brew.sh/
# then run: brew install mdbtools
install.packages("Hmisc")
library(Hmisc)

# Read .mdb file
wwf <-mdb.get('WildfinderUpdate.mdb')
contents(wwf)
spp <- wwf$species
names(spp) <- gsub("\\.", "_", names(spp))
genus <-wwf$genus
names(genus) <- gsub("\\.", "_", names(genus))
family <- wwf$family
names(family) <- gsub("\\.", "_", names(family))
order <- wwf$order_
names(order) <- gsub("\\.", "_", names(order))
classx <- wwf$class
names(classx) <- gsub("\\.", "_", names(classx))
ecorspp <- wwf$ecoregion_species
names(ecorspp) <- gsub("\\.", "_", names(ecorspp))
ecor <- wwf$ecoregions 
names(ecor) <- gsub("\\.", "_", names(ecor))
biome <- wwf$biomes 
names(biome) <- gsub("\\.", "_", names(biome))
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# Clean taxonomical dataframe

wwf_by_sp <- ecorspp[ ,c("ECOREGION_CODE","SPECIES_ID")]

# set spp name and code from the next level

wwf_by_sp$SPECIES   <- swap(wwf_by_sp$SPECIES_ID, spp$SPECIES_ID, spp$SPECIES)
wwf_by_sp$GENUS_ID  <- swap(wwf_by_sp$SPECIES_ID, spp$SPECIES_ID, spp$GENUS_ID)
wwf_by_sp$GENUS     <- swap(wwf_by_sp$GENUS_ID, genus$GENUS_ID, genus$GENUS)
wwf_by_sp$FAMILY_ID <- swap(wwf_by_sp$GENUS, genus$GENUS, genus$FAMILY_ID)
wwf_by_sp$FAMILY    <- swap(wwf_by_sp$FAMILY_ID, family$FAMILY_ID, family$FAMILY) 
wwf_by_sp$ORDER_ID  <- swap(wwf_by_sp$FAMILY_ID, family$FAMILY_ID, family$ORDER_ID) 
wwf_by_sp$ORDER     <- swap(wwf_by_sp$ORDER_ID, order$ORDER_ID, order$ORDER_DESC) 
wwf_by_sp$CLASS_ID  <- swap(wwf_by_sp$ORDER_ID, order$ORDER_ID, order$CLASS_ID) 
wwf_by_sp$CLASS     <- swap(wwf_by_sp$CLASS_ID, classx$CLASS_ID, classx$CLASS) 

# FILTER ONLY MAMMALS
wwf_mams <- wwf_by_sp[which(wwf_by_sp$CLASS == "Mammalia"),]

# SET ECOREGIONS AND FILTER AMERICAN ECOREGIONS
wwf_mams$ECOREGION_NAME <- as.character(swap(wwf_mams$ECOREGION_CODE, ecor$ECOREGION_CODE, ecor$ECOREGION_NAME))

# load table with equivalences of wwf biomes and the biomes defined in this work
install.packages("readxl")
library (readxl)
wwf_biomes <- read_excel("wwf_New_Biomes.xlsx", col_names=TRUE)

# match ecoregion names
wwf_econames <- match(unique(wwf_biomes$ECO_NAME),unique(ecor$ECOREGION_NAME))
no_match <- unique(wwf_biomes$ECO_NAME)[which(is.na(wwf_econames))]

# some names are not recognized, loading equivalencies
equi_names <- read.csv("Ecoregions_matches.csv")

# match names
change <- which(is.na(match(wwf_mams$ECOREGION_NAME,equi_names[,2]))==FALSE)
for(i in change){
	wwf_mams$ECOREGION_NAME[i] <- swap(wwf_mams$ECOREGION_NAME[i],equi_names[,2],equi_names[,1])
}

# Set biomes
wwf_mams$NEW_BIOME <- swap(wwf_mams$ECOREGION_NAME, wwf_biomes$ECO_NAME, wwf_biomes$New_Biome)

# Yunga y Mata Atlantica separados de A H
wwf_biomes$New_Biome2 <- ifelse(is.na(wwf_biomes$...6) == FALSE, "YM", wwf_biomes$New_Biome)
wwf_mams$NEW_BIOME_2 <- swap(wwf_mams$ECOREGION_NAME, wwf_biomes$ECO_NAME, wwf_biomes$New_Biome2)

# take out NA's and Non american biomes
amer_mams <- wwf_mams[wwf_mams$NEW_BIOME_2 %in% unique(wwf_biomes$New_Biome2),]
 
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# SET ORIGINS FOR MAMMALS

# taxonomical info by spp 
amer_mams$SCIENTIFIC_NAME <- paste(amer_mams$GENUS, amer_mams$SPECIES, sep = "_")
tax_info <- aggregate(amer_mams$GENUS ~ amer_mams$SCIENTIFIC_NAME, FUN = unique)
names(tax_info)<-c("SCIENTIFIC_NAME","GENUS")
tax_info$FAMILY <- aggregate(amer_mams$FAMILY ~ amer_mams$SCIENTIFIC_NAME, FUN = unique)[ ,2]
tax_info$ORDER <- aggregate(amer_mams$ORDER ~ amer_mams$SCIENTIFIC_NAME, FUN = unique)[ ,2]


# from Nathan's dataframe
#load(file="C:/Users/ASUS/Dropbox/MS_2021_GABI/GABI_mammalClades/mamPhy360_wwf360Grid_land_NewWorld_NewBiomeAreas_REFINED.Rda")
load("mamPhy360_wwf360Grid_land_NewWorld_NewBiomeAreas_REFINED.Rda")

# from spp
ori_class <- aggregate(mamPhy360_NW_wNewBiomes$Origin_Class ~ mamPhy360_NW_wNewBiomes$scientificname, FUN = unique)
tax_info$Origin_Class <-swap(tax_info$SCIENTIFIC_NAME, ori_class[,1], ori_class[ ,2])

# add a new level for factor Origin Class (both) 
levels(tax_info$Origin_Class) <- c(levels(tax_info$Origin_Class),"both")

# from genus
ori_class_gen <- aggregate(mamPhy360_NW_wNewBiomes$Origin_Class ~ mamPhy360_NW_wNewBiomes$gen, FUN = unique)
# check NAs
na_sp = which(is.na(tax_info$Origin_Class))
tax_info$Origin_Class[na_sp] <- swap(tax_info$GENUS[na_sp], ori_class_gen[,1], ori_class_gen[ ,2])

# from family
ori_class_fam <- aggregate(mamPhy360_NW_wNewBiomes$Origin_Class ~ mamPhy360_NW_wNewBiomes$family, FUN = unique)
ori_class_fam[ ,2] <- ifelse(lapply(ori_class_fam[ ,2], length) == 1,
	as.character(unlist(lapply(ori_class_fam[ ,2], function(xi) xi[[1]]))),"both")

# check NAs
na_gen = which(is.na(tax_info$Origin_Class))
tax_info$Origin_Class[na_gen] <- as.factor(swap(toupper(tax_info$FAMILY[na_gen]), ori_class_fam[,1], ori_class_fam[ ,2]))

# from Juan
juans <- read.csv("no_matched_spp_origin.csv")

na_fam = which(is.na(tax_info$Origin_Class))
tax_info$Origin_Class[na_fam] <- as.factor(swap(tax_info$SCIENTIFIC_NAME[na_fam],juans$SCIENTIFIC_NAME,juans$Origin))

# . . . . . . . . . . . . . . . . . . . 

# Set  Equivalent names

# from VertLife
vertlife <- read.csv("taxonomy_mamPhy_5911species.csv")

tax_info$Equivalent <- swap(tax_info$SCIENTIFIC_NAME, vertlife$MSW3_sciName_matched, vertlife$Species_Name)
tax_info$Equivalent_source[is.na(tax_info$Equivalent) == FALSE] <- "VertLife_MSW3"

# Species with no equivalent in  Vert or Nathan's Database
# na_vl_equiv <- which(is.na(tax_info$Equivalent))
# write.csv(tax_info[na_vl_equiv,], "taxa_without_equivalent.csv")
# We checked manually the species from the MSW3 taxonomy (used in the WildFinder Database) that had no match in
# VertLife taxonomy. We matched the synonyms for each species following the taxonomic information from VertLife
# and the ASM Mammal Diversity Database
# https://www.mammaldiversity.org/index.html


# . . . . . . . . . . . . . . . . . . . 

# Set Orgin on the occurrences df
amer_mams$Origin_Class <- swap(amer_mams$SCIENTIFIC_NAME, tax_info$SCIENTIFIC_NAME, tax_info$Origin_Class)
which(is.na(amer_mams$Origin_Class)) # zero

# Revision of occurrences in Paramos Ecoregions
rectified <- read.csv("Mountain Ecoregion Species.csv") 
not <- rectified[which(rectified$RECTIFIED == "yes" ), c("GENUS","SPECIES","ECOREGION.CODE") ]
not_c <- paste(not$GENUS, not$SPECIES, not$ECOREGION.CODE, sep= ":")
mams_c <- paste(amer_mams$GENUS,amer_mams$SPECIES,amer_mams$ECOREGION_CODE, sep = ":")
which(mams_c %in% not_c == TRUE)# 21 (all records)

# Take out rectified occurrences in Paramos
biomams <- amer_mams[mams_c %in% not_c == FALSE, ]

# take out indetermined origin(both) # Nyctinomops
biomams <- biomams[biomams$Origin_Class != "both", ]

# take out repeated records 
biomams <- unique(biomams)

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# AGGREGATE DATA BY BIOMES = all mammals = 

# AH & YM together
# take out repeated species by biome
biomams1 <- unique(biomams[ , c("SCIENTIFIC_NAME", "ORDER","NEW_BIOME", "Origin_Class")])

bybiome1 <- aggregate(biomams1$SCIENTIFIC_NAME ~ biomams1$NEW_BIOME, FUN = length)
names(bybiome1)<- c("Biome","RICHNESS")

bybiome1$NAO <- aggregate(biomams1$Origin ~ biomams1$NEW_BIOME, FUN = table)[,2][,1]
bybiome1$SAO <- aggregate(biomams1$Origin ~ biomams1$NEW_BIOME, FUN = table)[,2][,2]

bybiome1$propNAO <- bybiome1$NAO * 100 / bybiome1$RICHNESS
bybiome1$propSAO <- bybiome1$SAO * 100 / bybiome1$RICHNESS

# AH & YM separated
# take out repeated species by biome
biomams2 <- unique(biomams[ , c("SCIENTIFIC_NAME", "ORDER","NEW_BIOME_2", "Origin_Class")])

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

rbioms <- biomams[biomams$ORDER == "Rodentia", ]

# AH & YM together
rbioms1 <- unique(rbioms[ , c("SCIENTIFIC_NAME", "ORDER","NEW_BIOME", "Origin_Class")])

rbybiome1 <- aggregate(rbioms1$SCIENTIFIC_NAME ~ rbioms1$NEW_BIOME, FUN = length)
names(rbybiome1)<- c("Biome","RICHNESS")

rbybiome1$NAO <- aggregate(rbioms1$Origin ~ rbioms1$NEW_BIOME, FUN = table)[,2][,1]
rbybiome1$SAO <- aggregate(rbioms1$Origin ~ rbioms1$NEW_BIOME, FUN = table)[,2][,2]

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

wrbioms <- biomams[biomams$ORDER != "Rodentia", ]

# AH & YM together
wrbioms1 <- unique(wrbioms[ , c("SCIENTIFIC_NAME", "ORDER","NEW_BIOME", "Origin_Class")])

wrbybiome1 <- aggregate(wrbioms1$SCIENTIFIC_NAME ~ wrbioms1$NEW_BIOME, FUN = length)
names(wrbybiome1)<- c("Biome","RICHNESS")

wrbybiome1$NAO <- aggregate(wrbioms1$Origin ~ wrbioms1$NEW_BIOME, FUN = table)[,2][,1]
wrbybiome1$SAO <- aggregate(wrbioms1$Origin ~ wrbioms1$NEW_BIOME, FUN = table)[,2][,2]

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


