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


## SUMMARY: orth ab increased with N


## read in data
rings <- read_csv("OrthopteraDensity_c.csv")

### reorder N and P so that graphs are ordered consistently (nutrients then no nutrients)
rings$N <- factor(rings$N, levels = c("Nitrogen", "Control"))
rings$P <- factor(rings$P, levels = c("Phosphorous", "Control"))


### ring calc: rings are 12" wide (r = 6" = 0.1524m); ring area = pi*0.1524^2 = 0.502051m^2; 8 ring area = 8*0.502051
###          : plot area = 25m^2
###          : 4.0164 * x = 25; MULTIPLY BY A FACTOR OF 6.224


### sum of grasshoppers in rings within a plot, multiplied by 6.224 (see above for calculation) to get plot-wide abundance/density, divided by 25 to get per m^2
ringsplotweekly <- rings %>%
  filter(Year == 1)%>%
  group_by(WeekNo, Block, Plot, Treatment, N, P, K)%>%
  summarize(plotRW = sum(Count) *6.224 / 25)

### for graphs 
ringsseason <- ringsplotweekly %>%
  group_by(Block, Plot, Treatment, N, P, K)%>%
  summarize(plotR = mean(plotRW))


##for LTR per cap 
ringsforltr <- ringsplotweekly %>%
  group_by(Block, Plot, Treatment, N, P, K)%>%
  summarize(plotab = mean(plotRW)*25)

## needed for LTR
write.csv(ringsforltr, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\OrthopteraDenM.csv")

### stats
hopab1 = lmer(formula = plotRW ~ N + P + (1|Block), data = ringsplotweekly)
hopab2 = lmer(formula = plotRW ~ N + P * WeekNo + (1|Block), data = ringsplotweekly)
anova(hopab1, hopab2)
summary(hopab2)
xgN = residuals(hopab2)
hist(xgN)
shapiro.test(xgN)
effectsize(modelgrassN, partial = FALSE)

## non-normal^^^ use glmm's

ringsplotweekly$plotRW <- round(ringsplotweekly$plotRW, digits = 0)

grass <- lmer(plotRW ~ N * P + (1|Block), ringsplotweekly)
grass3 <- lmer(plotRW ~ N + P + (1|Block), ringsplotweekly)
grass4 <- lmer(plotRW ~ N + (1|Block), ringsplotweekly)
grass5 <- lmer(plotRW ~ Treatment + (1|Block), ringsplotweekly)

pm0 = glmmTMB(plotRW~N + (1|Block),  ringsplotweekly, family=poisson)
pm1 = glmmTMB(plotRW~N + P + (1|Block),  ringsplotweekly, family=poisson)
pm2 = glmmTMB(plotRW~N * P + (1|Block),  ringsplotweekly, family=poisson)

## ----Negative Binomial
nbm0 = glmmTMB(plotRW ~ N + (1|Block),  ringsplotweekly,family=nbinom2)
nbm1 = glmmTMB(plotRW~N + P + (1|Block),  ringsplotweekly,  family=nbinom2)
nbm2 = glmmTMB(plotRW~N * P + (1|Block),  ringsplotweekly,  family=nbinom2)

## ----Zero-Inflated Poisson
zipm0 = glmmTMB(plotRW~N + (1|Block), zi=~N,  ringsplotweekly, family=poisson)
zipm1 = glmmTMB(plotRW~N + P + (1|Block), zi=~N + P,  ringsplotweekly, family=poisson)
zipm3 = glmmTMB(plotRW~N * P + (1|Block), zi=~N * P, ringsplotweekly, family=poisson)

## ----Zero-Inflated Negative Binomial
zinbm0 = glmmTMB(plotRW~N +(1|Block), zi=~N,  ringsplotweekly, family=nbinom2)
zinbm1 = glmmTMB(plotRW~N + P + (1|Block), zi=~N + P,  ringsplotweekly, family=nbinom2)
zinbm3 = glmmTMB(plotRW~N * P +(1|Block), zi=~N * P,  ringsplotweekly, family=nbinom2)

## ----Poisson hurdle
hpm0 = glmmTMB(plotRW~N + (1|Block), zi=~N,  ringsplotweekly, family=truncated_poisson)
hpm1 = glmmTMB(plotRW~N + P + (1|Block), zi=~N + P,  ringsplotweekly, 
               family=truncated_poisson)
hpm2 = glmmTMB(plotRW~N * P + (1|Block), zi=~N * P,  ringsplotweekly, 
               family=truncated_poisson)

hnbm0 = glmmTMB(plotRW~N + (1|Block), zi=~N, ringsplotweekly, family=truncated_nbinom2)
hnbm1 = glmmTMB(plotRW~N + P + (1|Block), zi=~N + P,  ringsplotweekly, 
                family=truncated_nbinom2)
hnbm2 = glmmTMB(plotRW~N * P + (1|Block), zi=~N * P,  ringsplotweekly, 
                family=truncated_nbinom2)

whnbm0 = glmmTMB(plotRW~P * WeekNo + (1|Block), zi=~N,  ringsplotweekly, family=truncated_nbinom2)
whnbm1 = glmmTMB(plotRW~N + P * WeekNo + (1|Block), zi=~N + P,  ringsplotweekly, 
                 family=truncated_nbinom2)


## ----Model Comparison - not using any of the grass's b/c residuals non-normal
AICtab(pm0, pm1, pm2,
       nbm0, nbm1, nbm2,
       zipm0, zipm1, zipm3,
       hpm0, hpm1, hpm2,
       hnbm0, hnbm1, hnbm2, 
       whnbm0, whnbm1,
       grass, grass4, grass3, grass5)


summary(hpm0)
res <- residuals(hpm0)
xgN = residuals(hpm0)
confint(hpm0)
hist(xgN)
shapiro.test(xgN)
effectsize(hpm0, partial = FALSE)
