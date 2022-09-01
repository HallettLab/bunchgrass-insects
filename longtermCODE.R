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
library(car)
library(ggpubr)
library(reshape2)
library(gridExtra)
library(grid)
library(cowplot)
library(ggtext)

LTdata <- read_csv("LongTerm_c.csv")

bunchgrassall <- LTdata %>%
  group_by(block, plot, trt, year_trt, N, P,functional_group)%>%
  summarize(means = sum(max_cover))

#### nothinggg w nutrients
Forbstats <- bunchgrassall %>%
  filter(functional_group == "FORB")

modellt = lmer(formula = means ~ N*year_trt + P*year_trt + (1|block), data = Forbstats)
summary(modellt)
anova(modellt)
confint(modellt)
xgN = residuals(modellt)
hist(xgN)
effectsize(modellt, partial = FALSE)


#### signif; n and p w/ time
grassstats <- bunchgrassall %>%
  filter(functional_group == "GRASS")

modellt = lmer(formula = means ~ N*year_trt + P*year_trt + (1|block), data = grassstats)
summary(modellt)
anova(modellt)
confint(modellt)
xgN = residuals(modellt)
hist(xgN)
effectsize(modellt, partial = FALSE)


#### nothingggg w nutrients
legumestats <- bunchgrassall %>%
  filter(functional_group == "LEGUME")

modellt = lmer(formula = means ~ N*year_trt + P*year_trt + (1|block), data = legumestats)
summary(modellt)
anova(modellt)
confint(modellt)
xgN = residuals(modellt)
hist(xgN)
effectsize(modellt, partial = FALSE)


#### nothingggg w time
phloxstats <- bunchgrassall %>%
  filter(functional_group == "PHLOX")

modellt = lmer(formula = means ~ N*year_trt + P*year_trt + (1|block), data = phloxstats)
summary(modellt)
anova(modellt)
confint(modellt)
xgN = residuals(modellt)
hist(xgN)
effectsize(modellt, partial = FALSE)


#### nitrogen * time
gramstats <- bunchgrassall %>%
  filter(functional_group == "GRAMINOID")

modellt = lmer(formula = means ~ N*year_trt + P*year_trt + (1|block), data = gramstats)
summary(modellt)
anova(modellt)
confint(modellt)
xgN = residuals(modellt)
hist(xgN)
effectsize(modellt, partial = FALSE)