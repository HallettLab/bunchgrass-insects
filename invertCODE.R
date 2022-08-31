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


## read in data
predators <- read_csv("Predators_c.csv")
parasitoids <- read_csv("Parasitoids_c.csv")
herbivores <- read_csv("Herbivores_c.csv")
leafherbs <- read_csv("LeafH_c.csv")
pollenherbs <- read_csv("PollenNectarH_c.csv")
suckherbs <- read_csv("SuckH_c.csv")

# predators
## stats, nothing, although P has a p value of 0.05778
modelpreds = lmer(formula = plotC ~ Nitrogen + Phosphorous + (1|Block), data = predators)
summary(modelpreds)
anova(modelpreds)
confint(modelpreds)
xgN = residuals(modelpreds)
hist(xgN)
shapiro.test(xgN)
effectsize(modelpreds, partial = FALSE)

LDB <- predators

grass <- lmer(plotC ~ Nitrogen * Phosphorous + (1|Block), LDB)
grass1 <- lm(plotC ~ Nitrogen * Phosphorous + (1|Block), LDB)

grass3 <- lmer(plotC ~ Nitrogen + Phosphorous + (1|Block), LDB)
grass4 <- lmer(plotC ~ Nitrogen + Phosphorous + Potassium + (1|Block), LDB)
grass5 <- lmer(plotC ~ Trt + (1|Block), LDB)

LDB <- rename(LDB, N = Nitrogen, P = Phosphorous, K = Potassium)
LDB <- LDB %>%
  mutate(plotC = round(plotC1, digits = 0))

pm0 = glmmTMB(plotC~N + (1|Block), LDB, family=poisson)
pm1 = glmmTMB(plotC~N + P + (1|Block), LDB, family=poisson)
pm2 = glmmTMB(plotC~N * P + (1|Block), LDB, family=poisson)

## ----Negative Binomial
nbm0 = glmmTMB(plotC ~ N + (1|Block), LDB,family=nbinom2)
nbm1 = glmmTMB(plotC~N + P + (1|Block), LDB,  family=nbinom2)
nbm2 = glmmTMB(plotC~N * P + (1|Block), LDB,  family=nbinom2)
wrandom = glmmTMB(plotC~N + P + (1|Block) + (1|WeekNo), LDB,  family=nbinom2)

## ----Zero-Inflated Poisson
zipm0 = glmmTMB(plotC~N + (1|Block), zi=~N, LDB, family=poisson)
zipm1 = glmmTMB(plotC~N + P + (1|Block), zi=~N, LDB, family=poisson)
zipm2 = glmmTMB(plotC~N + P + (1|Block), zi=~N + P, LDB, family=poisson)
zipm3 = glmmTMB(plotC~N * P + (1|Block), zi=~N * P, LDB, family=poisson)

## ----Zero-Inflated Negative Binomial
zinbm0 = glmmTMB(plotC~N +(1|Block), zi=~N, LDB, family=nbinom2)
zinbm1 = glmmTMB(plotC~N + P + (1|Block), zi=~N, LDB, family=nbinom2)
zinbm2 = glmmTMB(plotC~N + P +(1|Block), zi=~N + P, LDB , family=nbinom2)
zinbm3 = glmmTMB(plotC~N * P +(1|Block), zi=~N * P, LDB, family=nbinom2)


## ----Poisson hurdle
hpm0 = glmmTMB(plotC~N + (1|Block), zi=~N, LDB, family=truncated_poisson)
hpm1 = glmmTMB(plotC~N + P + (1|Block), zi=~N + P, LDB, 
               family=truncated_poisson)
hpm2 = glmmTMB(plotC~N * P + (1|Block), zi=~N + P, LDB, 
               family=truncated_poisson)
hnbm0 = glmmTMB(plotC~N + (1|Block), zi=~N, LDB, family=truncated_nbinom2)
hnbm1 = glmmTMB(plotC~N + P + (1|Block), zi=~N + P, LDB, 
                family=truncated_nbinom2)
hnbm2 = glmmTMB(plotC~N * P + (1|Block), zi=~N + P, LDB, 
                family=truncated_nbinom2)

## ----Model Comparison
AICtab(pm0, pm1, pm2,
       nbm0, nbm1, nbm2,
       zipm0, zipm1, zipm2, zipm3,
       hpm0, hpm1, hpm2,
       hnbm0, hnbm1, hnbm2,
       grass, grass3, grass4, grass5)

## use glmm's only because of distribution, interaction between N and P - positive
summary(nbm2)
res <- residuals(nbm2)
hist(res)
shapiro.test(res)
effectsize(nbm2, partial = FALSE)





# parasitoids
## stats, P increases parasitoid ab
modelpreds = lmer(formula = plotC ~ Nitrogen + Phosphorous + (1|Block), data = parasitoids)
summary(modelpreds)
anova(modelpreds)
confint(modelpreds)
xgN = residuals(modelpreds)
hist(xgN)
shapiro.test(xgN)
effectsize(modelpreds, partial = FALSE)



# herbivores, nothing significant
modelpreds = lmer(formula = plotC ~ Nitrogen + Phosphorous + (1|Block), data = herbivores)
summary(modelpreds)
anova(modelpreds)
confint(modelpreds)
xgN = residuals(modelpreds)
hist(xgN)
shapiro.test(xgN)
effectsize(modelpreds, partial = FALSE)



##Herbs by type

##pollen/nectar, N has a negative effect
modelpreds = lmer(formula = plotC ~ Nitrogen + Phosphorous + (1|Block), data = pollenherbs)
summary(modelpreds)
anova(modelpreds)
confint(modelpreds)
xgN = residuals(modelpreds)
hist(xgN)
shapiro.test(xgN)
effectsize(modelpreds, partial = FALSE)


## use glmm's because not very normal
LDB <- pollenherbs 

grass <- lmer(plotC ~ Nitrogen * Phosphorous + (1|Block), LDB)
grass1 <- lm(plotC ~ Nitrogen * Phosphorous + (1|Block), LDB)

grass3 <- lmer(plotC ~ Nitrogen + Phosphorous + (1|Block), LDB)
grass4 <- lmer(plotC ~ Nitrogen + Phosphorous + Potassium + (1|Block), LDB)
grass5 <- lmer(plotC ~ Trt + (1|Block), LDB)

LDB <- rename(LDB, N = Nitrogen, P = Phosphorous, K = Potassium)
LDB <- LDB %>%
  mutate(plotC = round(plotC1, digits = 0))

pm0 = glmmTMB(plotC~N + (1|Block), LDB, family=poisson)
pm1 = glmmTMB(plotC~N + P + (1|Block), LDB, family=poisson)
pm2 = glmmTMB(plotC~N * P + (1|Block), LDB, family=poisson)

## ----Negative Binomial
nbm0 = glmmTMB(plotC ~ N + (1|Block), LDB,family=nbinom2)
nbm1 = glmmTMB(plotC~N + P + (1|Block), LDB,  family=nbinom2)
nbm2 = glmmTMB(plotC~N * P + (1|Block), LDB,  family=nbinom2)
wrandom = glmmTMB(plotC~N + P + (1|Block) + (1|WeekNo), LDB,  family=nbinom2)

## ----Zero-Inflated Poisson
zipm0 = glmmTMB(plotC~N + (1|Block), zi=~N, LDB, family=poisson)
zipm1 = glmmTMB(plotC~N + P + (1|Block), zi=~N, LDB, family=poisson)
zipm2 = glmmTMB(plotC~N + P + (1|Block), zi=~N + P, LDB, family=poisson)
zipm3 = glmmTMB(plotC~N * P + (1|Block), zi=~N * P, LDB, family=poisson)

## ----Zero-Inflated Negative Binomial
zinbm0 = glmmTMB(plotC~N +(1|Block), zi=~N, LDB, family=nbinom2)
zinbm1 = glmmTMB(plotC~N + P + (1|Block), zi=~N, LDB, family=nbinom2)
zinbm2 = glmmTMB(plotC~N + P +(1|Block), zi=~N + P, LDB , family=nbinom2)
zinbm3 = glmmTMB(plotC~N * P +(1|Block), zi=~N * P, LDB, family=nbinom2)


## ----Poisson hurdle
hpm0 = glmmTMB(plotC~N + (1|Block), zi=~N, LDB, family=truncated_poisson)
hpm1 = glmmTMB(plotC~N + P + (1|Block), zi=~N + P, LDB, 
               family=truncated_poisson)
hpm2 = glmmTMB(plotC~N * P + (1|Block), zi=~N + P, LDB, 
               family=truncated_poisson)
hnbm0 = glmmTMB(plotC~N + (1|Block), zi=~N, LDB, family=truncated_nbinom2)
hnbm1 = glmmTMB(plotC~N + P + (1|Block), zi=~N + P, LDB, 
                family=truncated_nbinom2)
hnbm2 = glmmTMB(plotC~N * P + (1|Block), zi=~N + P, LDB, 
                family=truncated_nbinom2)

## ----Model Comparison
AICtab(pm0, pm1, pm2,
       nbm0, nbm1, nbm2,
       zipm0, zipm1, zipm2, zipm3,
       hpm0, hpm1, hpm2,
       hnbm0, hnbm1, hnbm2,
       grass, grass3, grass4, grass5)

## use glmm's only because of distribution - N has a negative effect
summary(nbm2)
res <- residuals(nbm2)
hist(res)
shapiro.test(res)
effectsize(nbm2, partial = FALSE)





##leaf, P significant
modelpreds = lmer(formula = plotC ~ Nitrogen + Phosphorous + (1|Block), data = leafherbs)
summary(modelpreds)
anova(modelpreds)
confint(modelpreds)
xgN = residuals(modelpreds)
hist(xgN)
shapiro.test(xgN)
effectsize(modelpreds, partial = FALSE)

##use glmm's because not very normal
LDB <- leafherbs 

grass <- lmer(plotC ~ Nitrogen * Phosphorous + (1|Block), LDB)
grass1 <- lm(plotC ~ Nitrogen * Phosphorous + (1|Block), LDB)

grass3 <- lmer(plotC ~ Nitrogen + Phosphorous + (1|Block), LDB)
grass4 <- lmer(plotC ~ Nitrogen + Phosphorous + Potassium + (1|Block), LDB)
grass5 <- lmer(plotC ~ Trt + (1|Block), LDB)

LDB <- rename(LDB, N = Nitrogen, P = Phosphorous, K = Potassium)
LDB <- LDB %>%
  mutate(plotC = round(plotC1, digits = 0))

pm0 = glmmTMB(plotC~N + (1|Block), LDB, family=poisson)
pm1 = glmmTMB(plotC~N + P + (1|Block), LDB, family=poisson)
pm2 = glmmTMB(plotC~N * P + (1|Block), LDB, family=poisson)

## ----Negative Binomial
nbm0 = glmmTMB(plotC ~ N + (1|Block), LDB,family=nbinom2)
nbm1 = glmmTMB(plotC~N + P + (1|Block), LDB,  family=nbinom2)
nbm2 = glmmTMB(plotC~N * P + (1|Block), LDB,  family=nbinom2)
wrandom = glmmTMB(plotC~N + P + (1|Block) + (1|WeekNo), LDB,  family=nbinom2)

## ----Zero-Inflated Poisson
zipm0 = glmmTMB(plotC~N + (1|Block), zi=~N, LDB, family=poisson)
zipm1 = glmmTMB(plotC~N + P + (1|Block), zi=~N, LDB, family=poisson)
zipm2 = glmmTMB(plotC~N + P + (1|Block), zi=~N + P, LDB, family=poisson)
zipm3 = glmmTMB(plotC~N * P + (1|Block), zi=~N * P, LDB, family=poisson)

## ----Zero-Inflated Negative Binomial
zinbm0 = glmmTMB(plotC~N +(1|Block), zi=~N, LDB, family=nbinom2)
zinbm1 = glmmTMB(plotC~N + P + (1|Block), zi=~N, LDB, family=nbinom2)
zinbm2 = glmmTMB(plotC~N + P +(1|Block), zi=~N + P, LDB , family=nbinom2)
zinbm3 = glmmTMB(plotC~N * P +(1|Block), zi=~N * P, LDB, family=nbinom2)


## ----Poisson hurdle
hpm0 = glmmTMB(plotC~N + (1|Block), zi=~N, LDB, family=truncated_poisson)
hpm1 = glmmTMB(plotC~N + P + (1|Block), zi=~N + P, LDB, 
               family=truncated_poisson)
hpm2 = glmmTMB(plotC~N * P + (1|Block), zi=~N + P, LDB, 
               family=truncated_poisson)
hnbm0 = glmmTMB(plotC~N + (1|Block), zi=~N, LDB, family=truncated_nbinom2)
hnbm1 = glmmTMB(plotC~N + P + (1|Block), zi=~N + P, LDB, 
                family=truncated_nbinom2)
hnbm2 = glmmTMB(plotC~N * P + (1|Block), zi=~N + P, LDB, 
                family=truncated_nbinom2)

## ----Model Comparison
AICtab(pm0, pm1, pm2,
       nbm0, nbm1, nbm2,
       zipm0, zipm1, zipm2, zipm3,
       hpm0, hpm1, hpm2,
       hnbm0, hnbm1, hnbm2,
       grass, grass3, grass4, grass5)

#used nbm1 because of distribution
summary(nbm1)
res <- residuals(nbm1)
hist(res)
shapiro.test(res)
effectsize(nbm1, partial = FALSE)




## suck, nothing
modelpreds = lmer(formula = plotC ~ Nitrogen + Phosphorous + (1|Block), data = suckherbs)
summary(modelpreds)
anova(modelpreds)
confint(modelpreds)
xgN = residuals(modelpreds)
hist(xgN)
shapiro.test(xgN)
effectsize(modelpreds, partial = FALSE)