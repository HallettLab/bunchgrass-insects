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


## SUMMARY: Bromus is higher in P plots


## read in data
coverBL <- read_csv("CoverBromusLupine_c.csv")


## SE function if needed
calcSE<-function(x){
  x <- x[is.na(x)==F]
  sd(x)/sqrt(length(x))
}


## get plot level mean
plotBL <- coverBL %>%
  group_by(Block, Plot, Treatment, N, P, K, Species)%>%
  summarize(total = sum(PCover)/3)

## need for LTR
write.csv(plotBL, "C:\\Users\\14842\\Documents\\Hallett Lab\\My project\\2021 data\\BLcovm.csv")

## stats
## Bromus - P increases bro cov
brocov2021 <- plotBL%>%
  filter(Species == "Bromus")

modelgrassN = lmer(formula = total ~ N + P + (1|Block), data = brocov2021)
summary(modelgrassN)
anova(modelgrassN)
confint(modelgrassN)
xgN = residuals(modelgrassN)
hist(xgN)
shapiro.test(xgN)
effectsize(modelgrassN, partial = FALSE)

# Fixed effects:
# Estimate Std. Error     df t value Pr(>|t|)    
# (Intercept)     0.750      1.798 10.379   0.417 0.685092    
# NNitrogen       2.389      1.982 19.000   1.205 0.242998    
# PPhosphorous    9.611      1.982 19.000   4.848 0.000112 ***
  
#   > anova(modelgrassN)
# Type III Analysis of Variance Table with Satterthwaite's method
#   Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
# N  34.24   34.24     1    19   1.452 0.2429982    
# P 554.24  554.24     1    19  23.503 0.0001117 ***

# > anova(modelgrassN)
# Type III Analysis of Variance Table with Satterthwaite's method
# Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
# N  34.24   34.24     1    19   1.452 0.2429982    
# P 554.24  554.24     1    19  23.503 0.0001117 ***
  
# effect size
# Parameter    | Std. Coef. |         95% CI
# ------------------------------------------
#   (Intercept)  |      -0.87 | [-1.41, -0.32]
# NNitrogen    |       0.35 | [-0.25,  0.95]
# PPhosphorous |       1.39 | [ 0.79,  1.99]


## lupine - nothinggggg
lupcov2021 <- plotBL%>%
  filter(Species == "Lupine")

modellup = lmer(formula = total ~ N + P + (1|Block), data = lupcov2021)
summary(modellup)
anova(modellup)
confint(modellup)
xgN = residuals(modellup)
hist(xgN)
shapiro.test(xgN)
effectsize(modellup, partial = FALSE)
