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
structure <- read_csv("HabitatVolume_c.csv")

## create dataframe that is mean over season
structurez <- structure %>%
  group_by(WeekNo, Block, Plot, Treatment, N, P, K, Region)%>%
  summarize(meanarea = mean(Volume))


## stats

## total
structureclean <- structurez%>%
  filter(Region == "StrTotal")

structureclean$meanarea <- round(structureclean$meanarea, digits = 0)

# total
grass <- lmer(meanarea ~ N * P + (1|Block), structureclean)
grass3 <- lmer(meanarea ~ N + P + (1|Block), structureclean)
grass4 <- lmer(meanarea ~ N + (1|Block), structureclean)
grass5 <- lmer(meanarea ~ Treatment + (1|Block), structureclean)

pm0 = glmmTMB(meanarea~N + (1|Block),  structureclean, family=poisson)
pm1 = glmmTMB(meanarea~N + P + (1|Block),  structureclean, family=poisson)
pm2 = glmmTMB(meanarea~N * P + (1|Block),  structureclean, family=poisson)

## ----Negative Binomial
nbm0 = glmmTMB(meanarea ~ N + (1|Block),  structureclean,family=nbinom2)
nbm1 = glmmTMB(meanarea~N + P + (1|Block),  structureclean,  family=nbinom2)
nbm2 = glmmTMB(meanarea~N * P + (1|Block),  structureclean,  family=nbinom2)

## ----Zero-Inflated Poisson
zipm0 = glmmTMB(meanarea~N + (1|Block), zi=~N,  structureclean, family=poisson)
zipm1 = glmmTMB(meanarea~N + P + (1|Block), zi=~N + P,  structureclean, family=poisson)
zipm3 = glmmTMB(meanarea~N * P + (1|Block), zi=~N * P, structureclean, family=poisson)

## ----Zero-Inflated Negative Binomial
zinbm0 = glmmTMB(meanarea~N +(1|Block), zi=~N,  structureclean, family=nbinom2)
zinbm1 = glmmTMB(meanarea~N + P + (1|Block), zi=~N + P,  structureclean, family=nbinom2)
zinbm3 = glmmTMB(meanarea~N * P +(1|Block), zi=~N * P,  structureclean, family=nbinom2)

## ----Poisson hurdle
hpm0 = glmmTMB(meanarea~N + (1|Block), zi=~N,  structureclean, family=truncated_poisson)
hpm1 = glmmTMB(meanarea~N + P + (1|Block), zi=~N + P,  structureclean, 
               family=truncated_poisson)
hpm2 = glmmTMB(meanarea~N * P + (1|Block), zi=~N * P,  structureclean, 
               family=truncated_poisson)

hnbm0 = glmmTMB(meanarea~N + (1|Block), zi=~N, structureclean, family=truncated_nbinom2)
hnbm1 = glmmTMB(meanarea~N + P + (1|Block), zi=~N + P,  structureclean, 
                family=truncated_nbinom2)
hnbm2 = glmmTMB(meanarea~N * P + (1|Block), zi=~N * P,  structureclean, 
                family=truncated_nbinom2)

whnbm0 = glmmTMB(meanarea~P * WeekNo + (1|Block), zi=~N,  structureclean, family=truncated_nbinom2)
whnbm1 = glmmTMB(meanarea~N + P * WeekNo + (1|Block), zi=~N + P,  structureclean, 
                 family=truncated_nbinom2)


## ----Model Comparison - not using any of the grass's b/c residuals non-normal
AICtab(pm0, pm1, pm2,
       nbm0, nbm1, nbm2,
       zipm0, zipm1, zipm3,
       hpm0, hpm1, hpm2,
       hnbm0, hnbm1, hnbm2, 
       whnbm0, whnbm1,
       grass, grass4, grass3, grass5)

summary(nbm2)
res <- residuals(nbm2)
xgN = residuals(nbm2)
confint(nbm2)
hist(xgN)
shapiro.test(xgN)
effectsize(nbm2, partial = FALSE)



## top
structureclean <- structurez%>%
  filter(Region == "top")

structureclean$meanarea <- round(structureclean$meanarea, digits = 0)

# top
grass <- lmer(meanarea ~ N * P + (1|Block), structureclean)
grass3 <- lmer(meanarea ~ N + P + (1|Block), structureclean)
grass4 <- lmer(meanarea ~ N + (1|Block), structureclean)
grass5 <- lmer(meanarea ~ Treatment + (1|Block), structureclean)


pm0 = glmmTMB(meanarea~N + (1|Block),  structureclean, family=poisson)
pm1 = glmmTMB(meanarea~N + P + (1|Block),  structureclean, family=poisson)
pm2 = glmmTMB(meanarea~N * P + (1|Block),  structureclean, family=poisson)


## ----Negative Binomial
nbm0 = glmmTMB(meanarea ~ N + (1|Block),  structureclean,family=nbinom2)
nbm1 = glmmTMB(meanarea~N + P + (1|Block),  structureclean,  family=nbinom2)
nbm2 = glmmTMB(meanarea~N * P + (1|Block),  structureclean,  family=nbinom2)


## ----Zero-Inflated Poisson
zipm0 = glmmTMB(meanarea~N + (1|Block), zi=~N,  structureclean, family=poisson)
zipm1 = glmmTMB(meanarea~N + P + (1|Block), zi=~N + P,  structureclean, family=poisson)
zipm3 = glmmTMB(meanarea~N * P + (1|Block), zi=~N * P, structureclean, family=poisson)



## ----Zero-Inflated Negative Binomial
zinbm0 = glmmTMB(meanarea~N +(1|Block), zi=~N,  structureclean, family=nbinom2)
zinbm1 = glmmTMB(meanarea~N + P + (1|Block), zi=~N + P,  structureclean, family=nbinom2)
zinbm3 = glmmTMB(meanarea~N * P +(1|Block), zi=~N * P,  structureclean, family=nbinom2)

## ----Poisson hurdle
hpm0 = glmmTMB(meanarea~N + (1|Block), zi=~N,  structureclean, family=truncated_poisson)
hpm1 = glmmTMB(meanarea~N + P + (1|Block), zi=~N + P,  structureclean, 
               family=truncated_poisson)
hpm2 = glmmTMB(meanarea~N * P + (1|Block), zi=~N * P,  structureclean, 
               family=truncated_poisson)

hnbm0 = glmmTMB(meanarea~N + (1|Block), zi=~N, structureclean, family=truncated_nbinom2)
hnbm1 = glmmTMB(meanarea~N + P + (1|Block), zi=~N + P,  structureclean, 
                family=truncated_nbinom2)
hnbm2 = glmmTMB(meanarea~N * P + (1|Block), zi=~N * P,  structureclean, 
                family=truncated_nbinom2)

whnbm0 = glmmTMB(meanarea~P * WeekNo + (1|Block), zi=~N,  structureclean, family=truncated_nbinom2)
whnbm1 = glmmTMB(meanarea~N + P * WeekNo + (1|Block), zi=~N + P,  structureclean, 
                 family=truncated_nbinom2)


## ----Model Comparison
AICtab(pm0, pm1, pm2,
       nbm0, nbm1, nbm2,
       zipm0, zipm1, zipm3,
       hpm0, hpm1, hpm2,
       hnbm0, hnbm1, hnbm2, 
       whnbm0, whnbm1,
       grass, grass4, grass3, grass5)

summary(whnbm1)
res <- residuals(whnbm1)
xgN = residuals(whnbm1)
confint(whnbm1)
hist(xgN)
shapiro.test(xgN)
effectsize(whnbm1, partial = FALSE)


## bot
structureclean <- structurez%>%
  filter(Region == "bottom")

structureclean$meanarea <- round(structureclean$meanarea, digits = 0)

# bot
grass <- lmer(meanarea ~ N * P + (1|Block), structureclean)
grass3 <- lmer(meanarea ~ N + P + (1|Block), structureclean)
grass4 <- lmer(meanarea ~ N + (1|Block), structureclean)
grass5 <- lmer(meanarea ~ Treatment + (1|Block), structureclean)


pm0 = glmmTMB(meanarea~N + (1|Block),  structureclean, family=poisson)
pm1 = glmmTMB(meanarea~N + P + (1|Block),  structureclean, family=poisson)
pm2 = glmmTMB(meanarea~N * P + (1|Block),  structureclean, family=poisson)


## ----Negative Binomial
nbm0 = glmmTMB(meanarea ~ N + (1|Block),  structureclean,family=nbinom2)
nbm1 = glmmTMB(meanarea~N + P + (1|Block),  structureclean,  family=nbinom2)
nbm2 = glmmTMB(meanarea~N * P + (1|Block),  structureclean,  family=nbinom2)


## ----Zero-Inflated Poisson
zipm0 = glmmTMB(meanarea~N + (1|Block), zi=~N,  structureclean, family=poisson)
zipm1 = glmmTMB(meanarea~N + P + (1|Block), zi=~N + P,  structureclean, family=poisson)
zipm3 = glmmTMB(meanarea~N * P + (1|Block), zi=~N * P, structureclean, family=poisson)



## ----Zero-Inflated Negative Binomial
zinbm0 = glmmTMB(meanarea~N +(1|Block), zi=~N,  structureclean, family=nbinom2)
zinbm1 = glmmTMB(meanarea~N + P + (1|Block), zi=~N + P,  structureclean, family=nbinom2)
zinbm3 = glmmTMB(meanarea~N * P +(1|Block), zi=~N * P,  structureclean, family=nbinom2)

## ----Poisson hurdle
hpm0 = glmmTMB(meanarea~N + (1|Block), zi=~N,  structureclean, family=truncated_poisson)
hpm1 = glmmTMB(meanarea~N + P + (1|Block), zi=~N + P,  structureclean, 
               family=truncated_poisson)
hpm2 = glmmTMB(meanarea~N * P + (1|Block), zi=~N * P,  structureclean, 
               family=truncated_poisson)

hnbm0 = glmmTMB(meanarea~N + (1|Block), zi=~N, structureclean, family=truncated_nbinom2)
hnbm1 = glmmTMB(meanarea~N + P + (1|Block), zi=~N + P,  structureclean, 
                family=truncated_nbinom2)
hnbm2 = glmmTMB(meanarea~N * P + (1|Block), zi=~N * P,  structureclean, 
                family=truncated_nbinom2)
whnbm0 = glmmTMB(meanarea~P * WeekNo + (1|Block), zi=~N,  structureclean, family=truncated_nbinom2)
whnbm1 = glmmTMB(meanarea~N + P * WeekNo + (1|Block), zi=~N + P,  structureclean, 
                 family=truncated_nbinom2)


## ----Model Comparison
AICtab(pm0, pm1, pm2,
       nbm0, nbm1, nbm2,
       zipm0, zipm1, zipm3,
       hpm0, hpm1, hpm2,
       hnbm0, hnbm1, hnbm2, 
       whnbm0, whnbm1,
       grass, grass4, grass3, grass5)

summary(nbm1)
res <- residuals(nbm1)
xgN = residuals(nbm1)
confint(nbm1)
hist(xgN)
shapiro.test(xgN)
effectsize(nbm1, partial = FALSE)


  