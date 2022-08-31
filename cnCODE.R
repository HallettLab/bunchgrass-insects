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


## SUMMARY: CN decreases for both bromus and lupine


## read in data
cnratios <- read_csv("CNRatios_c.csv")

## stats

#bro
cnBro21 <- cnratios%>%
  filter(Species == "Bromus")

modelcn = lmer(formula = ratio ~ N + (1|Block), data = cnBro21)
summary(modelcn)
anova(modelcn)
confint(modelcn)
xgN = residuals(modelcn)
hist(xgN)
shapiro.test(xgN)
effectsize(modelcn, partial = FALSE)


#lup
cnlup21 <- cnratios%>%
  filter(Species == "Lupine")

modelcn = lmer(formula = ratio ~ N + (1|Block), data = cnlup21)
summary(modelcn)
anova(modelcn)
confint(modelcn)
xgN = residuals(modelcn)
hist(xgN)
shapiro.test(xgN)
effectsize(modelcn, partial = FALSE)
