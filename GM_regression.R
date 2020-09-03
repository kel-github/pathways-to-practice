# Written by K. Garner, 2020
# Example code to load two CSV files and bind the datasets, save a csv file, and

rm(list=ls())

# Load packages, source function files and define path variables
library(tidyverse)
library(cowplot)
library(interactions)
source("R_rainclouds.R")
source("KG_data-wrangling.R")
tract_data = 'pathways-to-practice/GM_tract_data.csv'
fpath <- paste(getwd(), 'cleaned-data', sep='/')# path for attaining data
practice.dat <- read.csv(paste(fpath,"practiceGrp-pwr-coeffs.csv", sep="/"))
control.dat <- read.csv(paste(fpath,"controlGrp-pwr-coeffs.csv", sep="/"))
## --------------------------------------------------------------------------


# load behavioural data and relabel
practice.dat <- read.csv(paste(fpath, "practiceGrp-pwr-coeffs.csv", sep="/"))
practice.dat$sub <- as.factor(practice.dat$sub)

control.dat <- read.csv(paste(fpath, "controlGrp-pwr-coeffs.csv", sep="/"))
control.dat$sub <- as.factor(control.dat$sub)


## --------------------------------------------------------------------------
# add in the tract data and tidy up
tract.dat <- read.csv("GM_tract_data.csv") %>% select(-X) 
tract.dat$sub <- as.factor(tract.dat$sub)
reg.dat <- inner_join(practice.dat, tract.dat, by=c('sub')) %>%
  unique() %>% na.omit

reg.dat$sub <- as.factor(reg.dat$sub)
write.csv(reg.dat, paste(fpath, "GM_practice.reg.csv", sep="/"), row.names=FALSE)

reg.dat.con <- inner_join(control.dat, tract.dat, by=c('sub')) %>%
  unique() %>% na.omit

reg.dat.con$sub <- as.factor(reg.dat.con$sub)
write.csv(reg.dat.con, paste(fpath, "GM_control.reg.csv", sep="/"), row.names=FALSE)

## --------------------------------------------------------------------------

# Linear regressions from practice data:
# run a linear regression on the data from the single task condition and parameter a
mod.1 <- lm(formula = a ~ multitasking_network*visual_network, data=reg.dat[reg.dat$cond == "S", ])
# print the output
summary(mod.1)
# Model as a whole insignificant, adjusted R^2 = -0.02405,  F(3, 36) = 0.6947, p = 0.561
# Predictor X response variables insignificant
# Linear regression from first multitasking condition and parameter a
mod.2 <- lm(a ~ multitasking_network*visual_network, data=reg.dat[reg.dat$cond == "FM", ])
summary(mod.2)
# Model as a whole insignificant, adjusted R^2 = -0.04975,  F(3, 36) = 0.3839, p = 0.765
# Predictor X response variables insignificant
# Linear regression from second multitasking condition and parameter a
mod.3 <- lm(a ~ multitasking_network*visual_network, data=reg.dat[reg.dat$cond == "SM", ])
summary(mod.3)
# Model as a whole insignificant, adjusted R^2 = -0.05074,  F(3, 36) = 1.695, p = 0.185
# Predictor X response variables insignificant
# Linear regression from single task condition and parameter b
mod.4 <- lm(b ~ multitasking_network*visual_network, data=reg.dat[reg.dat$cond == "S", ])
summary(mod.4)
# Model as a whole insignificant, adjusted R^2 = -0.04466,  F(3, 36) = 1.608, p = 0.205
# multitasking_network and b relationship significant, t = 2.152, p < .05
# Linear regression from first multitasking condition and parameter b
mod.5 <- lm(b ~ multitasking_network*visual_network, data=reg.dat[reg.dat$cond == "FM", ])
summary(mod.5)
# Model as a whole insignificant, adjusted R^2 = -0.09834,  F(3, 36) = 2.418, p = 0.082
# multitasking_network and b relationship significant, t = 2.604, p < .05
# Linear regression from second multitasking condition and parameter b
mod.6 <- lm(b ~ multitasking_network*visual_network, data=reg.dat[reg.dat$cond == "SM", ])
summary(mod.6)
# Model as a whole insignificant, adjusted R^2 = -0.1135,  F(3, 36) = 2.665, p = 0.062
# multitasking_network and b relationship significant, t = 2.221, p < .05
# Linear regression from single task condition and derivative a
mod.7 <- lm(deriv_a ~ multitasking_network*visual_network, data=reg.dat[reg.dat$cond == "S", ])
summary(mod.7)
# Model as a whole insignificant, adjusted R^2 = -0.05166,  F(3, 36) = 1.708, p = 0.183
# multitasking_network and deriv_a relationship significant, t = -2.122, p < .05
# Linear regression from first multitasking condition and derivative a
mod.8 <- lm(deriv_a ~ multitasking_network*visual_network, data=reg.dat[reg.dat$cond == "FM", ])
summary(mod.8)
# Model as a whole insignificant, adjusted R^2 = -0.05704,  F(3, 36) = 0.2984, p = 0.826
# Predictor X response variables insignificant
# Linear regression from second multitasking condition and derivative a
mod.9 <- lm(deriv_a ~ multitasking_network*visual_network, data=reg.dat[reg.dat$cond == "SM", ])
summary(mod.9)
# Model as a whole insignificant, adjusted R^2 = -0.01078,  F(3, 36) = 1.142, p = 0.345
# Predictor X response variables insignificant
# Linear regression from single task condition and derivative b
mod.10 <- lm(deriv_b ~ multitasking_network*visual_network, data=reg.dat[reg.dat$cond == "S", ])
summary(mod.10)
# Model as a whole insignificant, adjusted R^2 = -0.04466,  F(3, 36) = 1.608, p = 0.205
# multitasking_network and deriv_b relationship significant, t = 2.152, p < .05
# Linear regression from first multitasking condition and derivative b
mod.11 <- lm(deriv_b ~ multitasking_network*visual_network, data=reg.dat[reg.dat$cond == "FM", ])
summary(mod.11)
# Model as a whole insignificant, adjusted R^2 = 0.09834,  F(3, 36) = 2.418, p = 0.082
# multitasking_network and deriv_b relationship significant, t = 2.604, p < .05
# Linear regression from second multitasking condition and derivative b
mod.12 <- lm(deriv_b ~ multitasking_network*visual_network, data=reg.dat[reg.dat$cond == "SM", ])
summary(mod.12)
# Model as a whole insignificant, adjusted R^2 = 0.1135,  F(3, 36) = 2.665, p = 0.062
# multitasking_network and deriv_b relationship significant, t = 2.221, p < .05

# Linear regressions from control data:
# Linear regression from first set size and parameter a
mod.13 <- lm(a ~ multitasking_network*visual_network, data=reg.dat.con[reg.dat.con$cond == "s8", ])
summary(mod.13)
# Model as a whole insignificant, adjusted R^2 = 0.006658,  F(3, 33) = 1.08, p = 0.371
# Predictor X response variables insignificant
# Linear regression from second set size and parameter a
mod.14 <- lm(a ~ multitasking_network*visual_network, data=reg.dat.con[reg.dat.con$cond == "s12", ])
summary(mod.14)
# Model as a whole insignificant, adjusted R^2 = 0.01061,  F(3, 33) = 1.129, p = 0.352
# Predictor X response variables insignificant
# Linear regression from third set size and parameter a
mod.15 <- lm(a ~ multitasking_network*visual_network, data=reg.dat.con[reg.dat.con$cond == "s16", ])
summary(mod.15)
# Model as a whole insignificant, adjusted R^2 = 0.05182,  F(3, 33) = 1.656, p = 0.196
# Predictor X response variables insignificant
# Linear regression from first set size and parameter b
mod.16 <- lm(b ~ multitasking_network*visual_network, data=reg.dat.con[reg.dat.con$cond == "s8", ])
summary(mod.16)
# Model as a whole insignificant, adjusted R^2 = -0.02061,  F(3, 33) = 0.7576, p = 0.526
# Predictor X response variables insignificant
# Linear regression from second set size and parameter b
mod.17 <- lm(b ~ multitasking_network*visual_network, data=reg.dat.con[reg.dat.con$cond == "s12", ])
summary(mod.17)
# Linear regression from third set size and parameter b
mod.18 <- lm(b ~ multitasking_network*visual_network, data=reg.dat.con[reg.dat.con$cond == "s16", ])
summary(mod.18)
# Model as a whole insignificant, adjusted R^2 = -0.05606,  F(3, 33) = 0.363, p = 0.780
# Predictor X response variables insignificant
# Linear regression from first set size and derivative a
mod.19 <- lm(deriv_a ~ multitasking_network*visual_network, data=reg.dat.con[reg.dat.con$cond == "s8", ])
summary(mod.19)
# Model as a whole insignificant, adjusted R^2 = -0.0354,  F(3, 33) = 0.5897, p = 0.626
# Predictor X response variables insignificant
# Linear regression from second set size and derivative a
mod.20 <- lm(deriv_a ~ multitasking_network*visual_network, data=reg.dat.con[reg.dat.con$cond == "s12", ])
summary(mod.20)
# Model as a whole insignificant, adjusted R^2 = -0.005809,  F(3, 33) = 0.9307, p = 0.437
# Predictor X response variables insignificant
# Linear regression from third set size and derivative a
mod.21 <- lm(deriv_a ~ multitasking_network*visual_network, data=reg.dat.con[reg.dat.con$cond == "s16", ])
summary(mod.21)
# Model as a whole insignificant, adjusted R^2 = 0.09028,  F(3, 33) = 2.191, p = 0.108
# visual_network and deriv_a relationship significant, t = 2.094, p < .05
# Linear regression from first set size and derivative b
mod.22 <- lm(deriv_b ~ multitasking_network*visual_network, data=reg.dat.con[reg.dat.con$cond == "s8", ])
summary(mod.22)
# Model as a whole insignificant, adjusted R^2 = -0.02061,  F(3, 33) = 0.7576, p = 0.526
# Predictor X response variables insignificant
# Linear regression from second set size and derivative b
mod.23 <- lm(deriv_b ~ multitasking_network*visual_network, data=reg.dat.con[reg.dat.con$cond == "s12", ])
summary(mod.23)
# Model as a whole insignificant, adjusted R^2 = -0.05606,  F(3, 33) = 0.363, p = 0.780
# Predictor X response variables insignificant
# Linear regression from third set size and derivative b
mod.24 <- lm(deriv_b ~ multitasking_network*visual_network, data=reg.dat.con[reg.dat.con$cond == "s16", ])
summary(mod.24)
# Model as a whole insignificant, adjusted R^2 = -0.05605,  F(3, 33) = 0.3631, p = 0.780
# Predictor X response variables insignificant


## --------------------------------------------------------------------------
# save the new dataframe to a cvs file
write.csv(reg.dat.con, paste(fpath, "GM_reg.dat.con.csv", sep="/"), row.names = FALSE)
write.csv(reg.dat, paste(fpath, "GM_reg.dat.csv, sep="/"), row.names=FALSE)
