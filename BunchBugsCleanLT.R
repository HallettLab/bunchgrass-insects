## Cleaning
library(tidyverse)
library(ggplot2)
library(mgcv)
library(dplyr)
library(tidyverse) 
library(dplyr)
library(ggplot2)

## read in data
source("data-cleaning.R")
rm(scaledat)
coveralt <- read_csv("CoverAll21LT.csv")

## select for bunchgrass
bunchgrass <- nutdat %>% 
  filter(site_code == "bnch.us")

## select only wanted columns
bunchgrassfix <- bunchgrass %>%
  select(plot, block, year_trt, trt, Taxon, functional_group, max_cover)

## get rid of things in 2021
cover2021fix <- coveralt %>%
  select(plot, block, year_trt, trt, Taxon, functional_group, max_cover)%>%
  filter(functional_group != "REMOVE")

## combine dataframes
bunchgrassall <- rbind(bunchgrassfix, cover2021fix)


## continue to remove things; everything but grass, forb, legume, and sedge
bunchgrassall <- bunchgrassall %>%
  filter(functional_group != "BRYOPHYTE")%>%
  filter(functional_group != "COVER")%>%
  filter(functional_group != "LICHEN")%>%
  filter(functional_group != "LITTER")%>%
  filter(functional_group != "WOODY")%>%
  filter(trt != "Fence")%>%
  filter(trt != "NPK+Fence")%>%
  filter(Taxon != "OTHER LITTER")%>%
  filter(Taxon != "OTHER ROCK")%>%
  filter(Taxon != "OTHER ANIMAL DIGGINGS")%>%
  filter(Taxon != "OTHER ANIMAL DROPPINGS")%>%
  filter(functional_group != "NON-LIVE")

##add N and P columns
bunchgrassall$N[bunchgrassall$trt=="N"]<-"Nitrogen"
bunchgrassall$N[bunchgrassall$trt=="NP"]<-"Nitrogen"
bunchgrassall$N[bunchgrassall$trt=="NK"]<-"Nitrogen"
bunchgrassall$N[bunchgrassall$trt=="NPK"]<-"Nitrogen"

bunchgrassall$N[bunchgrassall$trt=="Control"]<-"Control"
bunchgrassall$N[bunchgrassall$trt=="P"]<-"Control"
bunchgrassall$N[bunchgrassall$trt=="K"]<-"Control"
bunchgrassall$N[bunchgrassall$trt=="PK"]<-"Control"

bunchgrassall$P[bunchgrassall$trt=="P"]<-"Phosphorous"
bunchgrassall$P[bunchgrassall$trt=="NP"]<-"Phosphorous"
bunchgrassall$P[bunchgrassall$trt=="PK"]<-"Phosphorous"
bunchgrassall$P[bunchgrassall$trt=="NPK"]<-"Phosphorous"

bunchgrassall$P[bunchgrassall$trt=="Control"]<-"Control"
bunchgrassall$P[bunchgrassall$trt=="N"]<-"Control"
bunchgrassall$P[bunchgrassall$trt=="K"]<-"Control"
bunchgrassall$P[bunchgrassall$trt=="NK"]<-"Control"

## order columns for end graphs
bunchgrassall$trt <- factor(bunchgrassall$trt, levels = c("N", "NP", "NK", "NPK", "Control", "P", "K", "PK"))
bunchgrassall$N <- factor(bunchgrassall$N, levels = c("Nitrogen", "Control"))
bunchgrassall$P <- factor(bunchgrassall$P, levels = c("Phosphorous", "Control"))


## filter out phlox and non-phlox
forbsphlox <- bunchgrassall%>%
  filter(Taxon == "PHLOX DIFFUSA")

forbsphlox$functional_group[forbsphlox$functional_group == "FORB"] <- "PHLOX"

nophlox <- bunchgrassall %>% 
  filter(Taxon != "PHLOX DIFFUSA")

##rejoin
LTcleaned = full_join(nophlox, forbsphlox)

## write csv
write.csv(LTcleaned, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\LongTerm_c.csv")

