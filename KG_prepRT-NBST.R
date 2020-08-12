# Written by K. Garner, 2020
# prep the RT data as regressors (into design matrix) for the NBDT toolbox
rm(list=ls())

# Load packages, source function files and define path variables, and set session
# variables
library(tidyverse)
library(wesanderson)
source("KG_data-wrangling.R")
dat.save.loc = 'cleaned-data'
save.names = c('SkRT-regressors-sFM', 'SkRT-regressors-sSM')
save.cols = matrix( c(6, 3, 4, 6, 3, 5), byrow=F, nrow=3 )
excl.subs <- c("103", "133", "204", "225", "226", "232") # we don't have DTI data for these subs (or pre-data, see KG_prepCV-NBST)
# save coordinates of the atlas used
#co.ords = aal116 %>% select(c("x.mni", "y.mni", "z.mni"))
#write_csv(co.ords, paste("dti-data", "/", "aal116-coords", ".txt", sep=""), col_names=FALSE)
## --------------------------------------------------------------------------
# load behavioural data and relabel
source("KG_behaviour-wrangles.R")
levels(dat.recoded$sess) <- c("Pre", "Post")

## --------------------------------------------------------------------------
# Computations
# 1st, compute the mean and var by condition
head(dat.recoded)

# looked 
RT.dat <- dat.recoded %>% group_by(sub, sess, mult_cond) %>%
            summarise(RT = median(RT, na.rm=T)) %>%
            filter(sess == 'Pre') 
            

## --------------------------------------------------------------------------
# Put into wideform and check Mahalanobis distance
# for subs 133 and 232 we only have pre single trial data (this could be due to a recoding issue)
Mal.Chk <- RT.dat %>% 
            pivot_wider(names_from = mult_cond, values_from = RT) %>%
            drop_na()

mhl.mat <- Mal.Chk[,c(3:5)]
mhl.cov <- cov(mhl.mat) # here I get the covariance matrix
mhl.dist <- mahalanobis(mhl.mat, colMeans(mhl.mat), mhl.cov) # now calc the M dist
hist(mhl.dist, breaks = 20, col=wes_palette("IsleofDogs1")[1])

sprintf("For a Mahalanobis to be less that .1 per cent likely to have occured by chance, given our degrees of feedom (%f), it has to be a value greater than %f", length(mhl.dist)-1, qchisq(.001, df=length(mhl.dist)-1))

# no exclusions :)

## --------------------------------------------------------------------------

# form pre-data for NBS toolbox
'%notin%' <- Negate('%in%')
RT.4.save <- RT.dat %>% 
  pivot_wider(names_from = mult_cond, values_from = RT) %>%
  drop_na() %>%
  filter(sub %notin% excl.subs) 
RT.4.save$int = 1

# ---------------------------------------------------------------------------

# save the data
for (i in 1:length(save.names)) write_csv(RT.4.save[,save.cols[,i]], path=paste(dat.save.loc, '/', save.names[i], '.txt', sep=''), col_names=FALSE)
write_csv(RT.4.save[,1], path=paste(dat.save.loc, '/', 'CV-regressor-subs', '.csv', sep=''), col_names=FALSE)