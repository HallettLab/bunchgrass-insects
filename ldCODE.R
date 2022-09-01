library(tidyverse)
library(ggplot2)
library(mgcv)
library(dplyr)
library(tidyverse) 
library(dplyr)
library(ggplot2)
library(deSolve)
library(ggsignif)
library(lme4)
library(lmerTest)
library(car)
library(ggpubr)
library(reshape2)
library(gridExtra)
library(grid)
library(cowplot)
library(ggtext)
library(effectsize)
library(glmmTMB)
library(bbmle)


## SUMMARY: 



## read in data
leafdamage <- read_csv("LeafDamage_c.csv") 
ringsm <- read_csv("OrthopteraDenM.csv") 
BLm <- read_csv("BLcovm.csv")


## calculate mean chewing
LTRmean <- leafdamage %>%
  group_by(Block, Plot, Treatment, N, P, K, Species)%>%
  summarize(chewed = mean(Chewing))


## combine with hobber ab and BL cover dataframes
LTRdata1 = left_join(LTRmean, ringsm)

LTRdata1 = subset(LTRdata1, select = -c(9))

## "chewed" = mean percent chewed; "plotab" = hopper ab across plot; "total" = percent cover across plot 
LTRdata = left_join(LTRdata1, BLm)


## calculate plot level ltr, multiply by .01 to get percentage
LTRplot <- LTRdata%>%
  mutate(plotLTR = total*chewed* 0.01)


## calculate plot level LTR per capita
LTRdone <- LTRplot%>%
  mutate(LTRper = plotLTR / plotab)




## create and export dataframe for SEMS
# remove unwanted columns and rename some columns
LTRforSEMS = subset(LTRdone, select = -c(8,10,11))
names(LTRforSEMS)[names(LTRforSEMS) == "plotab"] <- "OrthPlotDens"

## write csv
write.csv(LTRforSEMS, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\LTRforSEM.csv")







## stats
## plot level
LTRbro <- LTRdone%>%
  filter(Species == "Bromus")

modelltrb1 = lmer(formula = plotLTR ~ N + P + (1|Block), data = LTRbro)
summary(modelltrb1)
anova(modelltrb1)
confint(modelltrb1)
xgN = residuals(modelltrb1)
hist(xgN)
shapiro.test(xgN)
effectsize(modelltrb1, partial = FALSE)



LTRlup <- LTRdone%>%
  filter(Species == "Lupine")

modelltrb1 = lmer(formula = plotLTR ~ N + P + (1|Block), data = LTRlup)
summary(modelltrb1)
anova(modelltrb1)
confint(modelltrb1)
xgN = residuals(modelltrb1)
hist(xgN)
shapiro.test(xgN)
effectsize(modelltrb1, partial = FALSE)



##percap
LTRbro <- LTRdone%>%
  filter(Species == "Bromus")

modelltrb1 = lmer(formula = LTRper ~ N + P + (1|Block), data = LTRbro)
summary(modelltrb1)
anova(modelltrb1)
confint(modelltrb1)
xgN = residuals(modelltrb1)
hist(xgN)
shapiro.test(xgN)
effectsize(modelltrb1, partial = FALSE)



LTRlup <- LTRdone%>%
  filter(Species == "Lupine")

modelltrb1 = lmer(formula = LTRper ~ N + P + (1|Block), data = LTRlup)
summary(modelltrb1)
anova(modelltrb1)
confint(modelltrb1)
xgN = residuals(modelltrb1)
hist(xgN)
shapiro.test(xgN)
effectsize(modelltrb1, partial = FALSE)


## reorder N and P and control for figs
LTRdone$N <- factor(LTRdone$N,levels = c("Nitrogen", "Control"))
LTRdone$P <- factor(LTRdone$P,levels = c("Phosphorous", "Control"))