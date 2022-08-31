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
library(report)

## read in data
coverclean <- read_csv("CoverAll_c.csv")


## SE function if needed
calcSE<-function(x){
  x <- x[is.na(x)==F]
  sd(x)/sqrt(length(x))
}

## calculate cover over entire plot for each species by dividing by three (this accounts for possible 0 values dropped with dropping NA's)
cover2021means <- coverclean %>%
  group_by(Block, Plot, Treatment, N, P, K, Species, Group)%>%
  summarize(means = sum(PCover)/3)

## cover totals for each functional group - used in stats and plots
covertotals <- cover2021means%>%
  group_by(Block, Plot, Treatment, N, P, K, Group)%>%
  summarize(plotC = (sum(means)))

## reorder N and P so that graphs are ordered consistently (nutrients then no nutrients)
covertotals$Group <- factor(covertotals$Group,levels = c("Grass", "Graminoid", "Phlox", "Forb", "Legume"))
covertotals$N <- factor(covertotals$N,levels = c("Nitrogen", "Control"))
covertotals$P <- factor(covertotals$P,levels = c("Phosphorous", "Control"))

## stats

## Grass - both N and P increase grass
cover2021grass <- covertotals%>%
  filter(Group == "Grass")

modelgrassN = lmer(formula = plotC ~ N + P + (1|Block), data = cover2021grass)
modelgrassN1 = lmer(formula = plotC ~ N*P + (1|Block), data = cover2021grass)
anova(modelgrassN, modelgrassN1) # N1 is a better fit, but no sig diff; going with N since nothing really changes
summary(modelgrassN)
report(modelgrassN)
 a <- anova(modelgrassN)
 report(a)
confint(modelgrassN)
xgN = residuals(modelgrassN)
hist(xgN)
shapiro.test(xgN)
effectsize(modelgrassN, partial = FALSE)

# Fixed effects:
#               Estimate Std. Error      df t value Pr(>|t|)    
#   (Intercept)   46.167      3.071  21.000  15.033 1.03e-12 ***
#   NControl     -13.167      3.546  21.000  -3.713  0.00129 ** 
#   PControl     -24.389      3.546  21.000  -6.878 8.49e-07 ***

# > anova(modelgrassN)
# Type III Analysis of Variance Table with Satterthwaite's method
#   Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
# N 1040.2  1040.2     1    21  13.787  0.001288 ** 
# P 3568.9  3568.9     1    21  47.303 8.493e-07 ***

# Computing profile confidence intervals ...
#                  2.5 %     97.5 %
# .sig01        0.000000   5.087637
# .sigma        6.273947  11.103276
# (Intercept)  40.301292  52.032077
# NControl    -19.937010  -6.396324
# PControl    -31.159232 -17.618546

# effect sizes
# Parameter   | Std. Coef. |         95% CI
# -----------------------------------------
# (Intercept) |       1.14 | [ 0.75,  1.54]
# NControl    |      -0.80 | [-1.25, -0.35]
# PControl    |      -1.49 | [-1.94, -1.03]


## Forb - N decreases forbs
cover2021forb <- covertotals%>%
  filter(Group == "Forb")

modelforb = lmer(formula = plotC ~ N + P + (1|Block), data = cover2021forb)
modelforb1 = lmer(formula = plotC ~ N*P + (1|Block), data = cover2021forb)
modelforb2 = lmer(formula = plotC ~ N + (1|Block), data = cover2021forb)
anova(modelforb, modelforb1, modelforb2) # going with modeforb1; best fit and signif... 
summary(modelforb1)
anova(modelforb1)
confint(modelforb1)
xf = residuals(modelforb1)
hist(xf)
shapiro.test(xf)
effectsize(modelforb1, partial = FALSE)

# Fixed effects:
# Estimate Std. Error      df t value Pr(>|t|)  
# (Intercept)         7.3333     6.8801  2.8340   1.066    0.369  
# NControl            3.5556     4.5174 18.0000   0.787    0.441  
# PControl            0.3333     4.5174 18.0000   0.074    0.942  
# NControl:PControl  13.5556     6.3885 18.0000   2.122    0.048 *

# > anova(modelforb)
# Type III Analysis of Variance Table with Satterthwaite's method
#   Sum Sq Mean Sq NumDF DenDF F value   Pr(>F)   
#  N   640.67  640.67     1    18 10.4650 0.004595 **
#  P   303.41  303.41     1    18  4.9560 0.039012 * 
#  N:P 275.63  275.63     1    18  4.5023 0.047987 *

#  confint(modelforb)
# Computing profile confidence intervals ...
# 2.5 %    97.5 %
#   .sig01             3.927006 26.183210
# .sigma             5.503521 10.139195
# (Intercept)       -7.526135 22.192795
# NControl          -5.031114 12.142225
# PControl          -8.253336  8.920003
# NControl:PControl  1.412171 25.698941

# effect size
# Parameter         | Std. Coef. |        95% CI
# ----------------------------------------------
#   (Intercept)       |      -0.39 | [-1.46, 0.67]
# NControl          |       0.26 | [-0.44, 0.96]
# PControl          |       0.02 | [-0.68, 0.72]
# NControl:PControl |       1.00 | [ 0.01, 1.99]


## Legume - nothing
cover2021legume <- covertotals%>%
  filter(Group == "Legume")

modellegume = lmer(formula = plotC ~ N + P + (1|Block), data = cover2021legume)
summary(modellegume)
anova(modellegume)
confint(modellegume)
xl = residuals(modellegume)
hist(xl)
shapiro.test(xl)
effectsize(modellegume, partial = FALSE)


## Graminoid- N descreases grams
cover2021gram <- covertotals%>%
  filter(Group == "Graminoid")

modelgram = lmer(formula = plotC ~ N + P + (1|Block), data = cover2021gram)
modelgram1 = lmer(formula = plotC ~ N*P + (1|Block), data = cover2021gram)
modelgram2 = lmer(formula = plotC ~ N + (1|Block), data = cover2021gram)
anova(modelgram, modelgram1, modelgram2) ## no sig dif; modelgram 2 best fit but just sticking with modelgram
summary(modelgram)
anova(modelgram)
confint(modelgram)
xgr = residuals(modelgram)
hist(xgr)
shapiro.test(xgr)
effectsize(modelgram, partial = FALSE)

# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)    
# (Intercept)   16.611      4.755  6.062   3.493 0.012719 *  
#   NControl      19.778      4.481 19.000   4.414 0.000298 ***
#   PControl      -3.167      4.481 19.000  -0.707 0.488308

#  anova(modelgram)
# Type III Analysis of Variance Table with Satterthwaite's method
#    Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
# N 2346.96 2346.96     1    19 19.4834 0.0002981 ***
# P   60.17   60.17     1    19  0.4995 0.4883080

# confint(modelgram)
# Computing profile confidence intervals ...
#                 2.5 %    97.5 %
# .sig01        0.00000 14.343818
# .sigma        7.93149 14.607450
# (Intercept)   7.24401 25.978230
# NControl     11.02746 28.528097
# PControl    -11.91699  5.583653

# effect size
# Parameter   | Std. Coef. |        95% CI
# ----------------------------------------
# (Intercept) |      -0.55 | [-1.20, 0.11]
# NControl    |       1.30 | [ 0.69, 1.92]
# PControl    |      -0.21 | [-0.83, 0.41]


## Phlox - P decreases phlox
cover2021phlox <- covertotals%>%
  filter(Group == "Phlox")

modelphlox = lmer(formula = plotC ~ N + P + (1|Block), data = cover2021phlox)
modelphlox1 = lmer(formula = plotC ~ N*P + (1|Block), data = cover2021phlox)
modelphlox2 = lmer(formula = plotC ~ P + (1|Block), data = cover2021phlox)
anova(modelphlox, modelphlox1, modelphlox2) ## nothing sig; sticking with modelphlox
summary(modelphlox)
anova(modelphlox)
confint(modelphlox)
xgro = residuals(modelphlox)
hist(xgro)
shapiro.test(xgro)
effectsize(modelphlox, partial = FALSE)

# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)   
# (Intercept)    2.542      9.864  3.738   0.258  0.81022   
# NControl       5.028      7.301 19.000   0.689  0.49938   
# PControl      24.194      7.301 19.000   3.314  0.00365 **
  
# anova(modelphlox)
# Type III Analysis of Variance Table with Satterthwaite's method
#   Sum Sq Mean Sq NumDF DenDF F value  Pr(>F)   
# N  151.7   151.7     1    19  0.4742 0.49938   
# P 3512.2  3512.2     1    19 10.9812 0.00365 **

#  confint(modelphlox)
# Computing profile confidence intervals ...
#                  2.5 %   97.5 %
# .sig01        0.000000 34.58607
# .sigma       12.924095 23.81018
# (Intercept) -18.014491 23.09787
# NControl     -9.230581 19.28614
# PControl      9.936085 38.45280

# effect size
# Parameter   | Std. Coef. |        95% CI
# ----------------------------------------
# (Intercept) |      -0.61 | [-1.48, 0.25]
# NControl    |       0.21 | [-0.43, 0.85]
# PControl    |       1.01 | [ 0.37, 1.65]

