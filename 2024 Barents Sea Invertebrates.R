#                                 DOWNLOAD AND CLEANING DATA ####
library(rgbif)
library(terra)
library("raster")
library(dplyr)
library(plyr)
##keys ####
Ani <- name_backbone(name = "Animalia")
usage_key_A <- Ani$usageKey
#study zone WTK= barents sea
s= "MultiPolygon (((-0.85213733967620842 81.70108501467477424, 4.41434933479705904 82.20578656789656691, 6.69151219603364211 82.3815247237690329, 15.12438689259141533 82.94756841748665011, 64.78783447416481067 82.96765390108214433, 64.42742020835758865 80.93215826269275226, 66.39331620366972686 78.9875301729643553, 68.19538753270589382 76.96050488054099503, 67.29435186818780323 76.47310743251446752, 63.10044041152188043 75.94598032698718271, 58.88195525491454418 75.28889766503512249, 57.11264885913359279 74.25661130916654429, 55.67099179590467628 73.30511081528203476, 54.09827499965494724 71.9914351130697554, 55.3924898632354612 70.97720001198734963, 57.16179625901639838 70.63791550980914735, 61.01167591650271049 69.57624863623347267, 61.91271158102077266 68.8170264827784024, 60.43828958453666189 68.12572663983753785, 58.84919032165933572 67.89261592716542282, 55.29009944681294542 67.91726461520279656, 51.63680938885784855 68.15012822387886615, 48.65520046263441856 67.08515015992013275, 47.47566286544712 66.5238038294563836, 42.57320972713741014 65.02003055375652707, 39.72266053393479268 63.72357898970557244, 37.56836617240520582 63.21132400074150581, 34.76286876242847512 64.02475695979828174, 32.27273383503307258 65.78876273639716032, 31.20377788758209547 66.87856075437851189, 28.69726049355909936 68.94393721133536701, 25.29789866833181833 69.06134183955326478, 20.54698334632744405 68.76071746133717966, 18.23705555183565963 67.66026045169542158, 16.63157382233073278 67.16952089150183269, 15.59947842479184921 67.04205550206029329, 14.64929536039097258 67.11224455807283107, 11.99533576671955792 67.74107163708639234, 12.01171823334716393 68.73102381869925637, 17.02475302139315971 70.00773022694158954, 17.0739004212759653 73.9865657397714358, 12.53595716543040339 76.17878705643417447, 10.2710811541645235 77.00478589373116733, 8.31747200882306714 76.99925885925637203, 4.61093893432827073 79.35226524332823317, 1.8914494741464547 80.65433692599226845,
-0.85213733967620842 81.70108501467477424)))"

##download animalia in s from GBIF per period ####
#animalia pre-1900
occ_download(
  pred("taxonKey", usage_key_A),
  pred_within(s),
  pred_lte("year", 1900), #<=1900 
  format = "SIMPLE_CSV"
)
d <- occ_download_get('0042176-230530130749713') %>%
  occ_download_import()

Ani_pre_1900 <- d

#animalia 1901-1950
occ_download(
  pred("taxonKey", usage_key_A),
  pred_within(s),
  pred_and(pred_gt("year", 1900), pred_lte("year", 1950)), # >1900, <=1950
  format = "SIMPLE_CSV"
)
d <- occ_download_get('0029353-230530130749713') %>%
  occ_download_import()
Ani_1901_1950 <- d #change name

#animalia 1951-1980
occ_download(
  pred("taxonKey", usage_key_A),
  pred_within(s),
  pred_and(pred_gt("year", 1950), pred_lte("year", 1980)), #>1950, <=1980
  format = "SIMPLE_CSV")
d <- occ_download_get('0029359-230530130749713') %>%
  occ_download_import()
Ani_1951_1980 <- d

#animalia 1981-2000
occ_download(
  pred("taxonKey", usage_key_A),
  pred_within(s),
  pred_and(pred_gt("year", 1980), pred_lte("year", 2000)), #>1980, <=2000
  format = "SIMPLE_CSV")
d <- occ_download_get('0029367-230530130749713') %>%
  occ_download_import()
Ani_1981_2000 <- d

#animalia 2001-2020 
occ_download(
  pred("taxonKey", usage_key_A),
  pred_within(s),
  pred_and(pred_gt("year", 2000), pred_lte("year", 2020)), #>2000, <=2020
  format = "SIMPLE_CSV")
d <- occ_download_get('0029369-230530130749713') %>%
  occ_download_import()
Ani_2001_2020 <- d

##clean data ####
#from animalia to invertebrates
#remove Genus sp.
Ani_pre_1900 = Ani_pre_1900[Ani_pre_1900$species > "",] 
Ani_1901_1950 = Ani_1901_1950[Ani_1901_1950$species > "",] 
Ani_1951_1980 = Ani_1951_1980[Ani_1951_1980$species > "",] 
Ani_1981_2000 = Ani_1981_2000[Ani_1981_2000$species > "",] 
Ani_2001_2020 = Ani_2001_2020[Ani_2001_2020$species > "",] 

#remove Chordates
Inv_pre_1900 = Ani_pre_1900[!Ani_pre_1900$phylum == "Chordata",] 
Inv_1901_1950 = Ani_1901_1950[!Ani_1901_1950$phylum == "Chordata",] 
Inv_1951_1980 = Ani_1951_1980[!Ani_1951_1980$phylum == "Chordata",] 
Inv_1981_2000 = Ani_1981_2000[!Ani_1981_2000$phylum == "Chordata",] 
Inv_2001_2020 = Ani_2001_2020[!Ani_2001_2020$phylum == "Chordata",] 

#remove Insecta
Inv_pre_1900 = Inv_pre_1900[!Inv_pre_1900$class == "Insecta",] 
Inv_1901_1950 = Inv_1901_1950[!Inv_1901_1950$class == "Insecta",] 
Inv_1951_1980 = Inv_1951_1980[!Inv_1951_1980$class == "Insecta",] 
Inv_1981_2000 = Inv_1981_2000[!Inv_1981_2000$class == "Insecta",] 
Inv_2001_2020 = Inv_2001_2020[!Inv_2001_2020$class == "Insecta",] 

#add Ascidicea (from Chordates)
asci = Ani_pre_1900[Ani_pre_1900$class == "Ascidiacea" ,] 
Inv_pre_1900= rbind(Inv_pre_1900,asci)            
asci = Ani_1901_1950[Ani_1901_1950$class == "Ascidiacea" ,] 
Inv_1901_1950= rbind(Inv_1901_1950,asci)
asci = Ani_1951_1980[Ani_1951_1980$class == "Ascidiacea" ,] 
Inv_1951_1980= rbind(Inv_1951_1980,asci)
asci = Ani_1981_2000[Ani_1981_2000$class == "Ascidiacea" ,] 
Inv_1981_2000= rbind(Inv_1981_2000,asci)
asci = Ani_2001_2020[Ani_2001_2020$class == "Ascidiacea" ,] 
Inv_2001_2020= rbind(Inv_2001_2020,asci)

#add thaliacea (no occ in this datasets)
#add Appendicularia (no occ in this datasets)

###remove species with <10 occurrences per period ####
#pre_00
Inv_pre_1900_species <- Inv_pre_1900 |> group_by(species) |> 
  dplyr::summarize(count = n()) #sort by species
Inv_pre_1900_sp_tbr = Inv_pre_1900_species[Inv_pre_1900_species$N <= 10,] #to be removed (TBR) rows with less than 10 occurences
Inv_pre_1900 = Inv_pre_1900[!(Inv_pre_1900$species %in% Inv_pre_1900_sp_tbr$species),] 


#1901-1950
Inv_1901_1950_species <- Inv_1901_1950 |> group_by(species) |> 
  dplyr::summarize(count = n()) #sort by species
Inv_1901_1950_sp_tbr = Inv_1901_1950_species[Inv_1901_1950_species$N <= 10,] 
Inv_1901_1950 = Inv_1901_1950[!(Inv_1901_1950$species %in% Inv_1901_1950_sp_tbr$species),] 

#1951-1980
Inv_1951_1980_species <- Inv_1951_1980 |> group_by(species) |> 
  dplyr::summarize(count = n()) #sort by species
Inv_1951_1980_sp_tbr = Inv_1951_1980_species[Inv_1951_1980_species$N <= 10,] 
Inv_1951_1980 = Inv_1951_1980[!(Inv_1951_1980$species %in% Inv_1951_1980_sp_tbr$species),] 

#1981-2000
Inv_1981_2000_species <- Inv_1981_2000 |> group_by(species) |> 
  dplyr::summarize(count = n()) #sort by species
Inv_1981_2000_sp_tbr = Inv_1981_2000_species[Inv_1981_2000_species$N <= 10,] 
Inv_1981_2000 = Inv_1981_2000[!(Inv_1981_2000$species %in% Inv_1981_2000_sp_tbr$species),] 

#2001-2020
Inv_2001_2020_species <- Inv_2001_2020 |> group_by(species) |> 
  dplyr::summarize(count = n()) #sort by species
Inv_2001_2020_sp_tbr = Inv_2001_2020_species[Inv_2001_2020_species$N <= 10,] 
Inv_2001_2020 = Inv_2001_2020[!(Inv_2001_2020$species %in% Inv_2001_2020_sp_tbr$species),] 

#                                 SET UP DF_FINAL FOR TESTING ####
library(purrr)
library(scales)
##barents sea shapefile, multipolygon with 3 zones
barents= readOGR("/Users/.../barents sea.shp")
plot(barents)

## set up df1 ####
### calculate relative occurrence (R) in % ####
#percentage: number of occurrence for one species/total number of occurrence in this period
#pre_1900 
df <- Inv_pre_1900_species
somme <-sum(df$N, na.rm = FALSE)
perc<- df$N/somme 
df$percentage <- percent(perc, accuracy= 0.001) 
Inv_pre_1900_species<- df #update correct name

#I_1901_50 
df <- Inv_1901_1950_species
somme <-sum(df$N, na.rm = FALSE)
perc<- df$N/somme 
df$percentage <- percent(perc, accuracy= 0.001) 
Inv_1901_1950_species<- df #update correct name

#I_1951_80
df <- Inv_1951_1980_species 
somme <-sum(df$N, na.rm = FALSE)
perc<- df$N/somme
df$percentage <- percent(perc, accuracy= 0.001) 
Inv_1951_1980_species <- df 

#I_1981_00
df <- Inv_1981_2000_species 
somme <-sum(df$N, na.rm = FALSE)
perc<- df$N/somme
df$percentage <- percent(perc, accuracy= 0.001) 
Inv_1981_2000_species <- df 

#I_2001_20 
df <- Inv_2001_2020_species 
somme <-sum(df$N, na.rm = FALSE)
perc<- df$N/somme
df$percentage <- percent(perc, accuracy= 0.001) 
Inv_2001_2020_species <- df 

###merge into df1 ####
#put all _species df into list
df_list <- list(Inv_pre_1900_species,
                Inv_1901_1950_species,
                Inv_1951_1980_species,
                Inv_1981_2000_species,
                Inv_2001_2020_species)
#merge all data frames in list
df1 <- df_list %>% reduce(full_join, by='species')
#rename column 
colnames(df1)[2]= "N pre 1900"
colnames(df1)[3]= "p pre 1900"
colnames(df1)[4]= "N 1901-50"
colnames(df1)[5]= "p 1901-50"
colnames(df1)[6]= "N 1951-80"
colnames(df1)[7]= "p 1951-80"
colnames(df1)[8]= "N 1981-2000"
colnames(df1)[9]= "p 1981-2000"
colnames(df1)[10]= "N 2001-2020"
colnames(df1)[11]= "p 2001-2020"

write_xlsx(df1,"/Users/.../df1.xlsx") #save as xlsx

## set up df2 ####
###  isolate coordinates & occ  ####

Inv_pre_1900_coord <- cbind.data.frame(species=Inv_pre_1900$species,x=Inv_pre_1900$decimalLongitude, y=Inv_pre_1900$decimalLatitude)
Inv_1901_1950_coord <- cbind.data.frame(species=Inv_1901_1950$species,x=Inv_1901_1950$decimalLongitude, y=Inv_1901_1950$decimalLatitude)
Inv_1951_1980_coord <- cbind.data.frame(species=Inv_1951_1980$species,x=Inv_1951_1980$decimalLongitude, y=Inv_1951_1980$decimalLatitude)
Inv_1981_2000_coord <- cbind.data.frame(species=Inv_1981_2000$species,x=Inv_1981_2000$decimalLongitude, y=Inv_1981_2000$decimalLatitude)
Inv_2001_2020_coord <- cbind.data.frame(species=Inv_2001_2020$species,x=Inv_2001_2020$decimalLongitude, y=Inv_2001_2020$decimalLatitude)


### sort coord into the multipolygon's three zones ####
# pre 1900
period = Inv_pre_1900_coord 
period_numeric = period[,c(2,3)]
xy.ll <- SpatialPoints(period_numeric,
                       proj4string=CRS('+proj=longlat +datum=WGS84'))
plot(barents)
points(xy.ll) #visualize in plot
zone <- over(xy.ll, barents)$zone 
period$zone <- zone
table(period$zone)
table_species_zone_pre_1900 <- as.data.frame.matrix(with(period,table(species,zone)))

# 1901-1950
period = Inv_1901_1950_coord 
period_numeric = period[,c(2,3)]
xy.ll <- SpatialPoints(period_numeric,
                       proj4string=CRS('+proj=longlat +datum=WGS84'))
plot(barents)
points(xy.ll) 
zone <- over(xy.ll, barents)$zone 
period$zone <- zone
table(period$zone)
table_species_zone_1901_50 <- as.data.frame.matrix(with(period,table(species,zone)))


# 1951-1980
period = Inv_1951_1980_coord 
period_numeric = period[,c(2,3)]
xy.ll <- SpatialPoints(period_numeric,
                       proj4string=CRS('+proj=longlat +datum=WGS84'))     
plot(barents)
points(xy.ll) 
zone <- over(xy.ll, barents)$zone 
period$zone <- zone
table(period$zone)
table_species_zone_1951_80 <- as.data.frame.matrix(with(period,table(species,zone)))

# 1981-2000
period = Inv_1981_2000_coord 
period_numeric = period[,c(2,3)]
xy.ll <- SpatialPoints(period_numeric,
                       proj4string=CRS('+proj=longlat +datum=WGS84'))     
plot(barents)
points(xy.ll) 
zone <- over(xy.ll, barents)$zone 
period$zone <- zone
table(period$zone)
table_species_zone_1981_00 <- as.data.frame.matrix(with(period,table(species,zone)))

# 2001-2020
period = Inv_2001_2020_coord 
period_numeric = period[,c(2,3)]
xy.ll <- SpatialPoints(period_numeric,
                       proj4string=CRS('+proj=longlat +datum=WGS84'))     
plot(barents)
points(xy.ll) 
zone <- over(xy.ll, barents)$zone 
period$zone <- zone
table(period$zone)
table_species_zone_2001_20 <- as.data.frame.matrix(with(period,table(species,zone)))

#turn rownames (species into column 1)
table_species_zone_pre_1900 = tibble::rownames_to_column(table_species_zone_pre_1900)
table_species_zone_1901_50 = tibble::rownames_to_column(table_species_zone_1901_50)
table_species_zone_1951_80 = tibble::rownames_to_column(table_species_zone_1951_80)
table_species_zone_1981_00 = tibble::rownames_to_column(table_species_zone_1981_00)
table_species_zone_2001_20 = tibble::rownames_to_column(table_species_zone_1901_20)
#rename "rowname" to "species"
colnames(table_species_zone_pre_1900)[1] ="species"
colnames(table_species_zone_1901_50)[1] ="species"
colnames(table_species_zone_1951_80)[1] ="species"
colnames(table_species_zone_1981_00)[1] ="species"
colnames(table_species_zone_1901_20)[1] ="species"

### merge into df2 ####
#put all table into list
df_list2 <- list( table_species_zone_pre_00,
                  table_species_zone_00_50,
                  table_species_zone_50_80,
                  table_species_zone_80_00,
                  table_species_zone_00_20)
#merge all data frames in list
df2 <- df_list2 %>% reduce(full_join, by='species')
#rename column
colnames(df2)[2]= "pre 1900 Z1 'cold'"
colnames(df2)[3]= "pre 1900 Z2 'mixed'"
colnames(df2)[4]= "pre 1900 Z3 'warm'"
colnames(df2)[5]= "1901-50 Z1 'cold'"
colnames(df2)[6]= "1901-50 Z2 'mixed'"
colnames(df2)[7]= "1901-50 Z3 'warm'"
colnames(df2)[8]= "1951-80 Z1 'cold'"
colnames(df2)[9]= "1951-80 Z2 'mixed'"
colnames(df2)[10]= "1951-80 Z3 'warm'"
colnames(df2)[11]= "1981-00 Z1 'cold'"
colnames(df2)[12]= "1981-00 Z2 'mixed'"
colnames(df2)[13]= "1981-00 Z3 'warm'"
colnames(df2)[14]= "2001-20 Z1 'cold'"
colnames(df2)[15]= "2001-20 Z2 'mixed'"
colnames(df2)[16]= "2001-20 Z3 'warm'"

## Merge df1 (the relative abundance) & df2 (occurrence per zone per period)

df_list3 <- list(df1,
                 df2)
df_final <- df_list3 %>% reduce(full_join, by='species')
write_xlsx(df_final,"/Users/.../df_final.xlsx")

#                                 ARTEFACT REMOVAL ####
#remove artefact based on WoRMS taxon match, 
#remove diatom Actinocyclus octonarius incorrectly registered as a mollusc in GBIF (as of June 2023), 
#fossil, freshwater or terrestrial species still in WoRMS
#after this line for replication please use the df_final.xlsx provided in supplementary material
#                                 MANUAL SORTING BETWEEN BENTHIC AND PELAGIC ####

#                                 CORRESPONDANCE ANALYSIS ####
library(ade4)
library(adegraphics)
library(explor)
library(made4)
library(readxl)

df_final <- read_excel("/df_final.xlsx")
df_TG_benthic <- read_excel("/df_TG/df_TG_benthic.xlsx")
df_TG_pelagic <- read_excel("/df_TG/df_TG_pelagic.xlsx")

##df_final= Benthic + Pelagic ####
# NAs become 0
df_final[,-(1:13)][is.na(df_final[,-(1:13)])] <-0

benthic_pelagic <- dudi.coa(df = df_final[, -(1:13)], scannf = FALSE, nf = 4)

scatter(benthic_pelagic)
s.label(benthic_pelagic$li,label=df_final$Species)
s.label(benthic_pelagic$co, label=names(df_final)[-(1:13)])
##Pelagic ####

df_TG_pelagic[,-(1:13)][is.na(df_TG_pelagic[,-(1:13)])] <-0
pelagic <- dudi.coa(df = df_TG_pelagic[, -(1:13)], scannf = FALSE, nf = 4)

explor(pelagic) #interactive 2D CA
#CA is pulled by one of more species
scatter(pelagic)
s.label(pelagic$li,label=df_TG_pelagic$Species)
s.label(pelagic$co, label=names(df_TG_pelagic)[-(1:13)])

## remove pelagic artefacts until the CA is no longer pulled by one speices ####
df_TG_pelagic2 = df_TG_pelagic[-c(63),] #row correspond to the species pulling the CA as seen in explor(pelagic)
df_TG_pelagic2[,-(1:13)][is.na(df_TG_pelagic2[,-(1:13)])] <-0
pelagic2 <- dudi.coa(df = df_TG_pelagic2[, -(1:13)], scannf = FALSE, nf = 4)
explor(pelagic2) #investigate if the CA is correct, if not remove the artefact remaining

df_TG_pelagic3 = df_TG_pelagic2[-c(90),]
df_TG_pelagic3[,-(1:13)][is.na(df_TG_pelagic3[,-(1:13)])] <-0
pelagic3 <- dudi.coa(df = df_TG_pelagic3[, -(1:13)], scannf = FALSE, nf = 4)
explor(pelagic3)

df_TG_pelagic4 = df_TG_pelagic3[-c(71),]
df_TG_pelagic4[,-(1:13)][is.na(df_TG_pelagic4[,-(1:13)])] <-0
pelagic4 <- dudi.coa(df = df_TG_pelagic4[, -(1:13)], scannf = FALSE, nf = 4)
explor(pelagic4)

df_TG_pelagic5 = df_TG_pelagic4[-c(77),]
df_TG_pelagic5[,-(1:13)][is.na(df_TG_pelagic5[,-(1:13)])] <-0
pelagic5 <- dudi.coa(df = df_TG_pelagic5[, -(1:13)], scannf = FALSE, nf = 4)
explor(pelagic5)

df_TG_pelagic6 = df_TG_pelagic5[-c(32),]
df_TG_pelagic6[,-(1:13)][is.na(df_TG_pelagic6[,-(1:13)])] <-0
pelagic6 <- dudi.coa(df = df_TG_pelagic6[, -(1:13)], scannf = FALSE, nf = 4)
explor(pelagic6)

df_TG_pelagic7 = df_TG_pelagic6[-c(86),]
df_TG_pelagic7[,-(1:13)][is.na(df_TG_pelagic7[,-(1:13)])] <-0
pelagic7 <- dudi.coa(df = df_TG_pelagic7[, -(1:13)], scannf = FALSE, nf = 4)
explor(pelagic7)

df_TG_pelagic8 = df_TG_pelagic7[-c(86),]
df_TG_pelagic8[,-(1:13)][is.na(df_TG_pelagic8[,-(1:13)])] <-0
pelagic8 <- dudi.coa(df = df_TG_pelagic8[, -(1:13)], scannf = FALSE, nf = 4)
explor(pelagic8) 

pelagic <- pelagic8 #once satified change the name for the figures

##Benthic ####
df_TG_benthic[,-(1:13)][is.na(df_TG_benthic[,-(1:13)])] <-0
benthic <- dudi.coa(df = df_TG_benthic[, -(1:13)], scannf = FALSE, nf = 4)

explor(benthic) #interactive 2D CA
scatter(benthic)
s.label(benthic$li,label=df_TG_benthic$Species)
s.label(benthic$co, label=names(df_TG_benthic)[-(1:13)])

#                                 FIGURE ####
library(ggplot2)
library(tidyverse)
library(cowplot)
## CA plot (Figure 3) arrows added manualy ####
###Pelagic ####
# CA
df_TG_pelagic[,-(1:13)][is.na(df_TG_pelagic[,-(1:13)])] <-0
pelagic <- dudi.coa(df = df_TG_pelagic[, -(1:13)], scannf = FALSE, nf = 4)
# labels
x <- pelagic$co[,1]
y <- pelagic$co[,2]
z <- pelagic$co[,3]
lx <- "Axis 1 (36.9%)"
ly <- "Axis 2 (24.4%)"
lz <- "Axis 3 (12.0%)"
lab <- colnames(pelagic$tab)
#shape per zone
#shape  15 square = warm
#       16 circle = mixed
#       17 triangle = cold
colnames(pelagic$tab)
reg.zone <- colnames(pelagic$tab)
czone<- c(17,16,15, 
          17,16,15,
          17,16,15,
          17,16,15,
          17,16,15)
#color per period
cperiod <- c("#E69F00","#E69F00","#E69F00",
             "#D55E00","#D55E00","#D55E00",
             "#009E73","#009E73","#009E73",
             "#0072B2","#0072B2","#0072B2",
             "#56B4E9","#56B4E9","#56B4E9")
col <- data.frame(reg.zone, czone, cperiod) #dataframe with name, shape and color of each point
# plots
A<- ggplot(data = pelagic$co,mapping = aes(x = x, y = y))+                  
  geom_point(color = col$cperiod, fill= col$cperiod, shape= col$czone, size=4) +
  labs(title = 'CA pelagic invertebrates axis 2 by 1',
       x = lx,
       y = ly)
B<- ggplot(data = pelagic$co,mapping = aes(x = x, y = z))+                  
  geom_point(color = col$cperiod, fill= col$cperiod, shape= col$czone, size=4) +
  labs(title = 'CA pelagic invertebrates axis 3 by 1',
       x = lx,
       y = lz)

# save plots
p<- plot_grid(A,B)
p
ggsave("CA pelagic.pdf", p)

###Benthic ####
# CA
df_TG_benthic[,-(1:13)][is.na(df_TG_benthic[,-(1:13)])] <-0
benthic <- dudi.coa(df = df_TG_benthic[, -(1:13)], scannf = FALSE, nf = 4)
explor(benthic)

# labels
x <- benthic$co[,1]
y <- benthic$co[,2]
z <- benthic$co[,3]
lx <- "Axis 1 (21.8%)"
ly <- "Axis 2 (15.13%)"
lz <- "Axis 3 (13.1%)"

lab <- colnames(benthic$tab)
#shape per zone
#shape  15 square = warm
#       16 circle = mixed
#       17 triangle = cold
colnames(benthic$tab)
reg.zone <- colnames(benthic$tab)
czone<- c(17,16,15, 
          17,16,15,
          17,16,15,
          17,16,15,
          17,16,15)
#color per period
cperiod <- c("#E69F00","#E69F00","#E69F00",
             "#D55E00","#D55E00","#D55E00",
             "#009E73","#009E73","#009E73",
             "#0072B2","#0072B2","#0072B2",
             "#56B4E9","#56B4E9","#56B4E9")
col <- data.frame(reg.zone, czone, cperiod) #dataframe with name, shape and color of each point
# plots
A<- ggplot(data = benthic$co,mapping = aes(x = x, y = y))+                  
  geom_point(color = col$cperiod, fill= col$cperiod, shape= col$czone, size=4) +
  labs(title = 'CA benthic invertebrates axis 2 by 1',
       x = lx,
       y = ly)
B<- ggplot(data = benthic$co,mapping = aes(x = x, y = z))+                  
  geom_point(color = col$cperiod, fill= col$cperiod, shape= col$czone, size=4) +
  labs(title = 'CA benthic invertebrates axis 3 by 1',
       x = lx,
       y = lz)

# save plots
p<- plot_grid(A, B)
p
ggsave("CA benthic.pdf", p)

## Figure 2 ####
library(ggplot2)
##  Annual mean Land-Ocean Temperature Index in 0.01 degrees Celsius
# selected zonal means
#  sources:  GHCN-v4 1880-09/2023 + SST: ERSST v5 1880-09/2023
# using elimination of outliers and homogeneity adjustment
# Note: ***** = missing - base period: 1951-1980
# https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.txt

GlobalT <- read.table("ZonAnn.Ts+dSST.txt", header=T)
summary(GlobalT)

ggplot(GlobalT, aes(Year, Z64N.90N/100)) + geom_point()+ geom_smooth() + ylab("Mean Land-Ocean Temperature Index (°C)")

## Supplementary Figure 1 ####
library(forcats)
palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Number of occurence reccord per period per zone
dates <- c(rep("pre 1900" , 4) , rep("1901-50" , 4) , rep("1951-80" , 4) , rep("1981-00" , 4), rep("2001-20", 4) )
zones <- rep(c("total" , "cold" , "mixed", "warm") , 5)
value <- c(6685, 280,	3293,	3112, 
           19968,2562,3582,13824,
           12605,504,2699,9402,
           32745,1414,6422,24909,
           389919,1029,38749,350141)
data <- data.frame(dates,zones,value)

p<- ggplot(data, aes( fill=factor(zones, levels = c('cold' , 'mixed', 'warm','total')), 
                      y=value, 
                      x=factor(dates, levels = c('pre 1900', '1901-50', '1951-80', '1981-00', '2001-20')))) + 
  geom_col(position="dodge", aes(y=log(value))) +
  labs(title="Number of occurrence reccords per period per zone", fill= "Zones",
       x="Periods", y = "Number of occurrences (log)") +
  scale_fill_manual(values=palette) +
  theme_minimal() 
p











