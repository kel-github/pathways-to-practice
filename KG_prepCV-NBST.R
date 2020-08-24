# Written by K. Garner, 2020
# prep the CV data as regressors (into design matrix) for the NBDT toolbox
rm(list=ls())

# Load packages, source function files and define path variables, and set session
# variables
library(tidyverse)
library(wesanderson)
library(cowplot)
library(brainGraph)
source("R_rainclouds.R")
source("KG_data-wrangling.R")
dat.save.loc = 'cleaned-data'
save.names = c('SkCV-regressors-sFS')
save.cols = matrix( c(6, 3, 4, 5), byrow=F, nrow=4 )
excl.subs <- c("103", "133", "204", "225", "226", "232") # we don't have DTI data for these subs (or pre-data, see below)
# save coordinates of the atlas used
#co.ords = aal116 %>% select(c("x.mni", "y.mni", "z.mni"))
#write_csv(co.ords, paste("dti-data", "/", "aal116-coords", ".txt", sep=""), col_names=FALSE)
## --------------------------------------------------------------------------
# load behavioural data and relabel
source("KG_behaviour-wrangles.R")
levels(dat.recoded$sess) <- c("Pre", "Post")

## --------------------------------------------------------------------------
# Computations
# using https://stats.stackexchange.com/questions/243498/coefficient-of-variation-with-population-median
head(dat.recoded)
CV.dat <- dat.recoded %>% group_by(sub, sess, mult_cond) %>%
  summarise(q25 = quantile(RT, 0.25),
            q50 = quantile(RT, 0.5),
            q75 = quantile(RT, 0.75)) %>%
  group_by(sub, sess, mult_cond) %>%
  transmute(CV=((q75-q25)/q50)*100) %>%
  filter(sess == 'Pre') 
write_csv(CV.dat, path=paste(dat.save.loc, '/', 'CV-all-subs', '.csv', sep=''))

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
# plot values to check distributions
CV.p <- ggplot(CV.dat, aes(x=mult_cond, y=CV, fill = mult_cond, colour = mult_cond)) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust =2, trim =
                     TRUE) +
  geom_point(position=position_jitter(width=.15), size=.25) +
  geom_boxplot(aes(x = mult_cond, y = CV), outlier.shape = NA,
               alpha = 0.3, width = .1, colour = "BLACK") +
  scale_y_continuous(limits=c(0,100)) + coord_flip() +
  ylab('condition') + xlab('CV') + theme_cowplot() + 
  guides(fill = FALSE, colour = FALSE) +
  theme(axis.title.x = element_text(face = "italic")) 


# ---------------------------------------------------------------------------
# form pre-data for NBS toolbox
'%notin%' <- Negate('%in%')
CV.4.save <- CV.dat %>% 
  pivot_wider(names_from = mult_cond, values_from = CV) %>%
  drop_na() %>%
  filter(sub %notin% excl.subs)
CV.4.save$int = 1

# ---------------------------------------------------------------------------

# save the data
for (i in 1:length(save.names)) write_csv(CV.4.save[,save.cols[,i]], path=paste(dat.save.loc, '/', save.names[i], '.txt', sep=''), col_names=FALSE)
write_csv(CV.4.save[,1], path=paste(dat.save.loc, '/', 'CV-regressor-subs', '.csv', sep=''), col_names=FALSE)
