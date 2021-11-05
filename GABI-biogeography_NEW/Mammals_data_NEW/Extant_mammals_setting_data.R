
# EXTANT MAMMALS DATA CURATION 
# Distribution of mammals in the Americas among different Biomes

# Load and clean WWF data
# To run this script you will need to download the files of WildFinder Database
# https://www.worldwildlife.org/publications/wildfinder-database

# set the working directory to the folder containing this script:
# (similar to RStudio menu "Session - Set Working Directory - To Source File Location"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()

# Load data from the WildFinder Database

if(Sys.info()[[1]] == "Windows") {
	# Option 1 (Windows)

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
} else {
	# Option 2 (Mac / Linux)
	# You must install the mdbtools package
	# https://github.com/mdbtools/mdbtools
	# For a Mac in the terminal 
	# install Home brew https://brew.sh/
	# then run: brew install mdbtools

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
}
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# auxiliar function to wap objects from a dataframe
swap <- function(x, from, to) {   tmp <- to[ match(x, from) ]
  return(tmp)}

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

# Filter mammals only
wwf_mams <- wwf_by_sp[which(wwf_by_sp$CLASS == "Mammalia"),]

# Set ecoregions
wwf_mams$ECOREGION_NAME <- as.character(swap(wwf_mams$ECOREGION_CODE, ecor$ECOREGION_CODE, ecor$ECOREGION_NAME))

# load (American) biomes and set the Yunga and Mata Atlántica from Andean Highlands
wwf_biomes <- read.csv("wwf_American_Biomes.csv")

# Set biomes in the 
wwf_mams$NEW_BIOME_1 <- swap(wwf_mams$ECOREGION_CODE, wwf_biomes$ECOREGION_CODE, wwf_biomes$New_Biome)

# Yunga y Mata Atlantica separated from Andean Highlands
wwf_mams$NEW_BIOME_2 <- swap(wwf_mams$ECOREGION_CODE, wwf_biomes$ECOREGION_CODE, wwf_biomes$New_Biome_2)

# take out NA's and Non american biomes
amer_mams <- wwf_mams[wwf_mams$NEW_BIOME_2 %in% unique(wwf_biomes$New_Biome_2),]

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# SET BIOGEOGRAPHIC ORIGIN FOR MAMMALS

# taxonomical info by spp 
amer_mams$SCIENTIFIC_NAME <- paste(amer_mams$GENUS, amer_mams$SPECIES, sep = "_")
tax_info <- aggregate(amer_mams$GENUS ~ amer_mams$SCIENTIFIC_NAME, FUN = unique)
names(tax_info) <- c("SCIENTIFIC_NAME", "GENUS")
tax_info$FAMILY <- aggregate(amer_mams$FAMILY ~ amer_mams$SCIENTIFIC_NAME, FUN = unique)[ ,2]
tax_info$ORDER <- aggregate(amer_mams$ORDER ~ amer_mams$SCIENTIFIC_NAME, FUN = unique)[ ,2]

# from Nathan's dataframe
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

# Origin from not matched spp (Juan)
ori_j <- read.csv("no_matched_spp_origin.csv")

na_fam <- which(is.na(tax_info$Origin_Class))
tax_info$Origin_Class[na_fam] <- as.factor(swap(tax_info$SCIENTIFIC_NAME[na_fam],ori_j$SCIENTIFIC_NAME,ori_j$Origin))

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


# Set Equivalent names

# from VertLife
vertlife <- read.csv("taxonomy_mamPhy_5911species.csv")

tax_info$Equivalent <- swap(tax_info$SCIENTIFIC_NAME, vertlife$MSW3_sciName_matched, vertlife$Species_Name)
tax_info$Equivalent_source[is.na(tax_info$Equivalent) == FALSE] <- "VertLife_MSW3"

# Species with no equivalent in  Vert or Nathan's Database
na_vl_equiv <- which(is.na(tax_info$Equivalent))
write.csv(tax_info[na_vl_equiv,], "taxa_without_equivalent.csv")

# Set Orgin on the occurrences df
amer_mams$Origin_Class <- swap(amer_mams$SCIENTIFIC_NAME, tax_info$SCIENTIFIC_NAME, tax_info$Origin_Class)
which(is.na(amer_mams$Origin_Class)) # zero = all records has origin
# . . . . . . . . . . . . . . . . . . . 

# Rectification from Paramos (JUAN)
rectified <- read.csv("Mountain Ecoregion Species.csv") 
not <- rectified[which(rectified$RECTIFIED == "yes" ), c("GENUS","SPECIES","ECOREGION.CODE") ]
not_c <- paste(not$GENUS, not$SPECIES, not$ECOREGION.CODE, sep= ":")
mams_c <- paste(amer_mams$GENUS,amer_mams$SPECIES,amer_mams$ECOREGION_CODE, sep = ":")
which(mams_c %in% not_c == TRUE)# 23 (all rectified records)

# Take out rectified occurrences in Paramos
biomams <- amer_mams[mams_c %in% not_c == FALSE, ]

# take out indetermined origin(both) # Nyctinomops
biomams <- biomams[biomams$Origin_Class != "both", ]

# take out repeated records 
biogeo_mams <- unique(biomams)

# biogeo_mams contain information of species by ecoregion and it is the basic df for all analyses


