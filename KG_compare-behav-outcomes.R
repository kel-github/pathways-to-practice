## Written by K. Garner, 2020
## takes the behavioural measures (CV & learning rate parameters)
## and tests whether learning rates and CVs are associated, and whether
## this is task specific or generalises across tasks

rm(list=ls())

# set working directory to current location
# install.packages("rstudioapi")  # uncomment and run if you don't already have this package installed
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to the location of this file

# load packages and source data-wrangling functions
# --------------------------------------------------------------------------------
# install.packages("tidyverse")  # uncomment and run if you don't have tidyverse 
# installed already
# install.packages("zoo")
library(tidyverse)
library(gridExtra)
source("KG_data-wrangling.R")
source("R_rainclouds.R")
fpath <- paste(getwd(), 'cleaned-data', sep='/')# path for attaining data 

# ------------------------------------------------------------------------------
# load CV and learning rate params and bind together into single dataframe
CV.dat <- read.csv(paste(fpath, "CV-all-subs.csv", sep="/"))
CV.dat$group <- NA
CV.dat$group[CV.dat$sub < 200] = "practice"
CV.dat$group[CV.dat$sub > 199] = "control"
CV.dat$group <- factor(CV.dat$group, levels=c("practice", "control"))
CV.dat$cog_cond <- c("lo", "med", "hi")
CV.dat$cog_cond <- factor(CV.dat$cog_cond, levels=c("lo", "med", "hi"))
CV.dat <- CV.dat %>% select(-mult_cond)
CV.dat$sub <- as.factor(CV.dat$sub)

learn.dat <- rbind( read.csv( paste(fpath, "practiceGrp-pwr-coeffs.csv", sep="/" ) ),
                    read.csv( paste(fpath, "controlGrp-pwr-coeffs.csv", sep="/" ) )) %>%
                   select(-c(model, RMSE, int, slp)) 
learn.dat$cog_cond <- c("lo", "med", "hi")
learn.dat$cog_cond <- factor(learn.dat$cog_cond, levels=c("lo", "med", "hi"))
learn.dat <- learn.dat %>% select(-cond)
learn.dat$sub <- as.factor(learn.dat$sub)

all.dat <- inner_join(CV.dat, learn.dat, by=c("sub", "cog_cond"))
all.dat$sub <- as.factor(all.dat$sub)
# ------------------------------------------------------------------------------
# check correlations of learning rate parameters and first derivatives
with(all.dat[all.dat$group == "practice", ], cor.test(a, deriv_a))
with(all.dat[all.dat$group == "practice", ], cor.test(b, deriv_b))
with(all.dat[all.dat$group == "practice", ], cor.test(a, b))
with(all.dat[all.dat$group == "practice", ], cor.test(deriv_a, deriv_b))
# the two parameters from each function are ok to have in one glm, but not the derivs (makes sense)

#--------------------------------------------------------------------------------
# plot relationship between each CV, and the corresponding learning parameters,
# by group and by condition
all.dat %>% ggplot(aes(x=a, y=CV, colour=group)) +
            geom_point() + facet_wrap(~cog_cond, nrow=2)
all.dat %>% ggplot(aes(x=b, y=CV, colour=group)) +
            geom_point() + facet_wrap(~cog_cond, nrow=2)
all.dat %>% ggplot(aes(x=deriv_a, y=CV, colour=group)) +
            geom_point() + facet_wrap(~cog_cond, nrow=2)
all.dat %>% ggplot(aes(x=deriv_b, y=CV, colour=group)) +
            geom_point() + facet_wrap(~cog_cond, nrow=2)

#--------------------------------------------------------------------------------
# run GLM for each function on CV
summary(glm(CV~a*b*cog_cond*group, family=gaussian, data=all.dat))
summary(glm(CV~deriv_a*deriv_b*cog_cond*group, family=gaussian, data=all.dat))

#-------------------------------------------------------------------------------
with(all.dat, cor.test(CV[cog_cond=="med"],
                       deriv_b[cog_cond=="med"]))

        