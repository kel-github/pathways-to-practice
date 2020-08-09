# Written by K. Garner, 2020
# prep the CV data as regressors (into design matrix) for the NBDT toolbox
rm(list=ls())

# Load packages, source function files and define path variables, and set session
# variables
library(tidyverse)
library(wesanderson)
source("KG_data-wrangling.R")
dat.save.loc = 'cleaned-data'
save.names = c('CV-regressors-multi-first','CV-regressors-multi-second')
save.cols = matrix( c(6, 3, 4, 6, 3, 5), byrow=F, nrow=3 )

## --------------------------------------------------------------------------
# load behavioural data and relabel
source("KG_behaviour-wrangles.R")
levels(dat.recoded$sess) <- c("Pre", "Post")

## --------------------------------------------------------------------------
# Computations
# 1st, compute the mean and var by condition
head(dat.recoded)
CV.dat <- dat.recoded %>% group_by(sub, sess, mult_cond) %>%
  summarise(RTmu = mean(RT, na.rm=T),
            RTsd = sd(RT, na.rm=T)) %>%
  group_by(sub, sess, mult_cond) %>%
  transmute(CV=RTsd/RTmu) %>%
  filter(sess == 'Pre') 

## --------------------------------------------------------------------------
# Put into wideform and check Mahalanobis distance
# for subs 133 and 232 we only have pre single trial data (this could be due to a recoding issue)
CV.Mal.Chk <- CV.dat %>% 
  pivot_wider(names_from = mult_cond, values_from = CV) %>%
  drop_na()

mhl.mat <- CV.Mal.Chk[,c(3:5)]
mhl.cov <- cov(mhl.mat) # here I get the covariance matrix
mhl.dist <- mahalanobis(mhl.mat, colMeans(mhl.mat), mhl.cov) # now calc the M dist
hist(mhl.dist, breaks = 20, col=wes_palette("IsleofDogs1")[1])

sprintf("For a Mahalanobis to be less that .1 per cent likely to have occured by chance, given our degrees of feedom (%f), it has to be a value greater than %f", length(mhl.dist)-1, qchisq(.001, df=length(mhl.dist)-1))

# no exclusions :)

## --------------------------------------------------------------------------

# form pre-data for NBS toolbox
CV.4.save <- CV.dat %>% 
  pivot_wider(names_from = mult_cond, values_from = CV) %>%
  drop_na() 
CV.4.save$int = 1

# ---------------------------------------------------------------------------

# save the data
for (i in 1:length(save.names)) write_csv(CV.4.save[,save.cols[,i]], path=paste(dat.save.loc, '/', save.names[i], '.txt', sep=''), col_names=FALSE)
write_csv(CV.4.save[,1], path=paste(dat.save.loc, '/', 'CV-regressor-subs', '.csv', sep=''), col_names=FALSE)
