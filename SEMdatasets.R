library(tidyverse)
library(dplyr)

## Combining datasets for SEMS

## Hoppers:
# need LTR, hopper density, CN, habitat volume, & grass/legume abundances
# read in data & rename columns
plantsH <- read_csv("plantshopperSEM.csv")
plantsH = subset(plantsH, select = -c(1))

volumeH <- read_csv("volumeSEM.csv")
volumeH = subset(volumeH, select = -c(1))

ltrH <- read_csv("LTRforSEM.csv")
ltrH = subset(ltrH, select = -c(1, 11))
ltrwide <- ltrH%>%
  pivot_wider(
    names_from = Species, values_from = plotLTR)
names(ltrwide)[names(ltrwide) == "Bromus"] <- "BROltr"
names(ltrwide)[names(ltrwide) == "Lupine"] <- "LUPltr"

cnratios <- read_csv("CNRatios_c.csv")
cnratios = subset(cnratios, select = -c(1:6))
cnplot <- cnratios %>%
  group_by(Plot, Block, Treatment, N, P, K)%>%
  summarize(meanCNr = mean(ratio))

hops1 = left_join(ltrwide, plantsH)
hops2 = left_join(hops1, volumeH)
hops3 = left_join(hops2, cnplot)

## write csv
write.csv(hops3, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\hopperSEMdata.csv")



## Invertebrates:
# need invert abundances, plant functional group abundances, and habitat volume
# read in data & rename columns
Lherbivores <- read_csv("LeafH_c.csv")
names(Lherbivores)[names(Lherbivores) == "plotC1"] <- "LeafH"
Lherbivores = subset(Lherbivores, select = -c(1))

Pherbivores <- read_csv("PollenNectarH_c.csv")
names(Pherbivores)[names(Pherbivores) == "plotC"] <- "PollH"
Pherbivores = subset(Pherbivores, select = -c(1))

Sherbivores <- read_csv("SuckH_c.csv")
names(Sherbivores)[names(Sherbivores) == "plotC"] <- "SapH"
Sherbivores = subset(Sherbivores, select = -c(1))

predators <- read_csv("Predators_c.csv")
names(predators)[names(predators) == "plotC"] <- "Preds"
predators = subset(predators, select = -c(1))

parasitoids <- read_csv("Parasitoids_c.csv") 
names(parasitoids)[names(parasitoids) == "plotC"] <- "Pars"
parasitoids = subset(parasitoids, select = -c(1))

invert1 = left_join(Lherbivores, Pherbivores)
invert2 = left_join(invert1, Sherbivores)
invert3 = left_join(invert2, predators)
invert4 = left_join(invert3, parasitoids)

names(invert4)[names(invert4) == "Nitrogen"] <- "N"
names(invert4)[names(invert4) == "Phosphorous"] <- "P"
names(invert4)[names(invert4) == "Potassium"] <- "K"
names(invert4)[names(invert4) == "Trt"] <- "Treatment"

plantsI <- read_csv("plantsinvertSEM.csv")
plantsI = subset(plantsI, select = -c(1))

volumeI <- read_csv("volumeSEM.csv")
volumeI = subset(volumeI, select = -c(1))

addplantsI = left_join(invert4, plantsI)
addvolumeI = left_join(addplantsI, volumeI)

## write csv
write.csv(addvolumeI, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\invertSEMdata.csv")


