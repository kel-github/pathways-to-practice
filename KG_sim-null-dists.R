# written by K. Garner, 2020
# free to share, code comes as is, please cite responsibly
##################################################################################################
# this code is written to produce a null distribution of the effect sizes and p-values that
# can be expected when one takes a random sample of DTI tracts, perfoms a factor analysis to
# get the two top factors, and then correlates these against the behaviour regressor of interest

rm(list=ls())
##################################################################################################
# load packages and source required files
library(tidyverse)
library(psych)
source("KG_data-wrangling.R")
source("KG_sim-nul-functions.R")
N = 1000 # number of permutations
Ntracts = 16 # number of tracts to select

################################################################################################
fpath <- '~/Dropbox/QBI/pathways-to-practice/dti-data/'
idx <- narrow.tracts(fpath)
# 1580 connections remain
do.select.and.regress <- function(idx){
  # this function will run 1 full
  data <- select.tracts(Ntracts, idx)
  try({reg.dat <- run.EFA(data)
       reg.res <- do.regression(reg.dat)
       reg.res
  })
}

null.dists <- do.call(rbind, replicate(N, do.select.and.regress(idx=idx), simplify=FALSE))

# remove the perms that didn't solve
null.dists <- null.dists %>% drop_na()
# how many perms worked? (960)
tps <- length(null.dists$term)/(length(unique(null.dists$term))*length(unique(null.dists$cog_cond)))
# add perm factor
null.dists$perm <- rep(1:tps, each = length(unique(null.dists$term))*length(unique(null.dists$cog_cond)))

# save the dataframe for plotting
write.csv(null.dists, 'cleaned-data/null-dists.csv', row.names=FALSE)
