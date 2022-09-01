library(lavaan)
library(tidyr)
library(readr)
library(effectsize)
library(dplyr)

## read in data
hoppers <- read_csv("hopperSEMdata.csv") 
hoppers = subset(hoppers, select = -c(1))
inverts <- read.csv("invertSEMdata.csv")
inverts = subset(inverts, select = -c(1))

## Hoppers 

# Scale data
hoppers[, c(7:13)] <- apply(hoppers[, c(7:13)], 2, scale)

# Nitrogen - hoppers
model <- '
LUPltr ~ OrthPlotDens + MeanVol + Legume + Grass
BROltr ~ OrthPlotDens + MeanVol + Grass + Legume
OrthPlotDens ~ meanCNr + MeanVol
MeanVol ~ N + Grass + Legume
meanCNr ~ N
Legume ~ N
Grass ~ N
'

fit <- sem(model, data = hoppers)
summary(fit, standardized=TRUE)
fitMeasures(fit, c("cfi", "rmsea", "srmr"))
interpret(fit)


# Phosphorus - hoppers
model2 <- '
LUPltr ~ OrthPlotDens + MeanVol + Legume + Grass
BROltr ~ OrthPlotDens + MeanVol + Grass + Legume
OrthPlotDens ~ MeanVol+ Grass + Legume
MeanVol ~ P + Grass + Legume
Legume ~ P
Grass ~ P
'

fit2 <- sem(model2, data = hoppers)
summary(fit2, standardized=TRUE)
fitMeasures(fit2, c("cfi", "rmsea", "srmr"))
interpret(fit2)


##Phosphorus - inverts
inverts[, c(7:17)] <- apply(inverts[, c(7:17)], 2, scale)

model3 <- '
Pars ~ LeafH
Preds ~ SapH + PollH
SapH ~ Grass + Forb + Phlox + MeanVol
LeafH ~ Grass + MeanVol + Phlox
PollH ~ Forb + Phlox + MeanVol
MeanVol ~ Forb + Graminoid + Phlox
Grass ~ P
Forb ~ P 
Graminoid ~ P
Phlox ~ P
'

fit3 <- sem(model3, data = inverts)
summary(fit3, standardized=TRUE)
fitMeasures(fit3, c("cfi", "rmsea", "srmr"))
interpret(fit3)

