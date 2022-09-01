## Cleaning
library(tidyverse)
library(ggplot2)
library(mgcv)
library(dplyr)
library(tidyverse) 
library(dplyr)
library(ggplot2)

## read in data
cn <- read_csv("CNRatios.csv") 
covera <- read_csv("CoverAll.csv")
coverbl <- read_csv("CoverBromusLupine.csv")
volume <- read_csv("HabitatVolume.csv")
ld <- read_csv("LeafDamage.csv")
orth <- read_csv("OrthopteraDensity.csv")
trophic <- read_csv("InvertTrophic.csv")
guild <- read.csv("InvertHerbs.csv")



# CN
cnclean <- cn %>%
  filter(Treatment != "C_H-")%>%
  filter(Treatment != "NPK_H-")

## write csv
write.csv(cnclean, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\CNRatios_c.csv")




# ALL COVER

## make longer so easier to work with
cover2021 <- covera%>%
  pivot_longer(
    cols = c("CoverExclosure", "CoverControl", "CoverNutMain"),
    names_to = "SubPlot",
    values_to = "PCover"
  )

## get rid of things unwanted and replace NA with 0
cover2021means <- cover2021%>%
  filter(Group != "Litter")%>%
  filter(Group != "Cover")%>%
  filter(Group != "Woody")%>%
  filter(Group != "Gopher mound")%>%
  filter(Group != "Ground")%>%
  filter(Treatment != "C_H-")%>%
  filter(Treatment != "NPK_H-")

cover2021means[is.na(cover2021means)] <- 0

## check species with NO cover in any plot
speciescheck <- cover2021means%>%
  group_by(Species)%>%
  summarize(specsum = sum(PCover))

## remove those species with no cover in 2021
cover2021means <- cover2021means%>%
  filter(Species != "A. caryophyllea") %>%
  filter(Species != "A. formosa")%>%
  filter(Species != "Aster sp.")%>%
  filter(Species != "C. pauciflora") %>%
  filter(Species != "Caryophyllaceae sp.") %>%
  filter(Species != "E. angustifolium") %>% 
  filter(Species != "F. viridula") %>% 
  filter(Species != "Fraxinus sp.") %>%
  filter(Species != "Grass sp.") %>%
  filter(Species != "H. aurantiacum") %>%
  filter(Species != "M. lateriflora") %>%
  filter(Species != "P. aquilinum") %>%
  filter(Species != "Poa sp.") %>%
  filter(Species != "Polemoniaceae sp.") %>%
  filter(Species != "S. bellum")

## make phlox its own category
forbsphlox <- cover2021means%>%
  filter(Species == "P. diffusa")

forbsphlox$Group[forbsphlox$Group == "Forb"] <- "Phlox"

## remove phlox from "other forbs," then join dataframes
nophlox <- cover2021means %>% 
  filter(Species != "P. diffusa")

covercleaned = full_join(nophlox, forbsphlox)

## write csv
write.csv(covercleaned, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\CoverAll_c.csv")




# BROMUS AND LUPINE COVER

## turn NA's into 0's
coverbl[is.na(coverbl)] <- 0

## remove fence plots
BLnoF <- coverbl%>%
  filter(Treatment != "C_H-")%>%
  filter(Treatment != "NPK_H-")


## make longer
cover2021BL <- BLnoF%>%
  pivot_longer(
    cols = c("CoverExclosure", "CoverControl", "CoverNutMain"),
    names_to = "SubPlot",
    values_to = "PCover"
  )

## write csv
write.csv(cover2021BL, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\CoverBromusLupine_c.csv")




# HABITAT VOLUME

## create two height classes
volumeclasses <- volume %>%
  mutate(bottom = StrArea_H1 + StrArea_H2, top = StrArea_H2 + StrArea_H4 + StrArea_H5)

## pivot longer
volumelong <- volumeclasses%>%
  pivot_longer(
    cols = c(10:17),
    names_to = "Region",
    values_to = "Volume"
  )

## write csv
write.csv(volumelong, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\HabitatVolume_c.csv")




# LEAF DAMAGE

## drop na's an dget rid of sucking data
ldclean<- ld %>%
  na.omit(Chewing)

## remove fence plots
ldcleannoF <- ldclean%>%
  filter(Treatment != "C_H-")%>%
  filter(Treatment != "NPK_H-")

ldcleannoF = subset(ldcleannoF, select = -c(12))

## write csv
write.csv(ldcleannoF, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\LeafDamage_c.csv")




# ORTHOPTERA DENSITY - already cleaned

## write csv
write.csv(orth, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\OrthopteraDensity_c.csv")






# Trophic Groups 

## reshape/clean data
trophic <- gather(trophic, Guild, Count, predator...9:herbivore...88, factor_key = TRUE)

trophic$Trt <- factor(trophic$Trt, levels = c("C", "K", "N", "P", "NK", "NP", "PK", "NPK"))

trophic <- trophic %>%
  mutate(WeekNo = Sample - 1)


## Predators - filter, make NA's 0's and then take the sum for each plot over the season
data_longP <- trophic %>%
  mutate(GuildX = case_when(startsWith(as.character(Guild), "pre") ~ "Predator"))%>%
  filter(GuildX == "Predator")

data_longP[is.na(data_longP)] = 0

##plotC is just "sum," used because easy to run through models
predstotal <- data_longP %>%
  group_by(Block, Plot, Trt, Nitrogen, Phosphorous, Potassium)%>%
  summarize(plotC = sum(Count))

## write csv
write.csv(predstotal, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\Predators_c.csv")


## Parasitoids; same process as above
data_longPa <- trophic %>%
  mutate(GuildX = case_when(startsWith(as.character(Guild), "par") ~ "Parasitoid"))%>%
  filter(GuildX == "Parasitoid")
data_longPa[is.na(data_longPa)] = 0

##plotC is just "sum," used because easy to run through models
pastotal <- data_longPa %>%
  group_by(Block, Plot, Trt, Nitrogen, Phosphorous, Potassium)%>%
  summarize(plotC = sum(Count))

## write csv
write.csv(pastotal, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\Parasitoids_c.csv")


## herbivores; same process as above
data_longH <- trophic %>%
  mutate(GuildX = case_when(startsWith(as.character(Guild), "her") ~ "Herbivore"))%>%
  filter(GuildX == "Herbivore")

data_longH[is.na(data_longH)] = 0

hstotal <- data_longH %>%
  group_by(Block, Plot, Trt, Nitrogen, Phosphorous, Potassium)%>%
  summarize(plotC = sum(Count))

## write csv
write.csv(hstotal, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\Herbivores_c.csv")




# Invertebrate Herbivores by type

## reshape/clean data
data_long <- gather(guild, herbtype, Count, other.part:leaf.9, factor_key = TRUE)

data_long$Trt <- factor(data_long$Trt, levels = c("C", "K", "N", "P", "NK", "NP", "PK", "NPK"))

data_long <- data_long %>%
  mutate(WeekNo = Sample - 1)

##pollen/nectar
herbpoll <- data_long %>%
  mutate(Type = case_when(startsWith(as.character(herbtype), "poll") ~ "Pollen/Nectar"))%>%
  filter(Type == "Pollen/Nectar")

herbpoll[is.na(herbpoll)] = 0

LDB <- herbpoll %>%
  group_by(Block, Plot, Trt, Nitrogen, Phosphorous, Potassium)%>%
  summarize(plotC = sum(Count))

## write csv
write.csv(LDB, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\PollenNectarH_c.csv")


##leaf
herbpoll <- data_long %>%
  mutate(Type = case_when(startsWith(as.character(herbtype), "lea") ~ "Leaf"))%>%
  filter(Type == "Leaf")

herbpoll[is.na(herbpoll)] = 0

LDB <- herbpoll %>%
  group_by(Block, Plot, Trt, Nitrogen, Phosphorous, Potassium)%>%
  summarize(plotC1 = sum(Count))

## write csv
write.csv(LDB, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\LeafH_c.csv")

## suck
herbpoll <- data_long %>%
  mutate(Type = case_when(startsWith(as.character(herbtype), "suck") ~ "Suck"))%>%
  filter(Type == "Suck")

herbpoll[is.na(herbpoll)] = 0

LDB <- herbpoll %>%
  group_by(Block, Plot, Trt, Nitrogen, Phosphorous, Potassium)%>%
  summarize(plotC = sum(Count))

## write csv
write.csv(LDB, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\SuckH_c.csv")
