# written by K. Garner, 2020
# free to share, code comes as is, please cite responsibly
##################################################################################################
# this code plots the output of KG_sim-null-dists.R

rm(list=ls())
##################################################################################################
# load packages and source required files
library(tidyverse)
library(wesanderson)
library(cowplot)
##################################################################################################
# define previous results for comparison
orig.results <- read.csv('cleaned-data/tract-cv-glm-orig-coefs.csv')
nulls <- read.csv('cleaned-data/null-dists.csv')

# first, select only the terms of interest
terms2inc <- as.character(unique(nulls$term)[c(2,3,7,8,9,10,11,13,14,15,16,17)])
orig.results$term <- nulls$term[1:(17*3)]
nulls <- nulls %>% filter(term %in% terms2inc)
orig.results <- orig.results %>% filter(term %in% terms2inc)
orig.results$sig <- as.factor(orig.results$Pr...t.. < .05)

# define function to make plots
plot.info <- function(cc, dv, data, model.results, model.dv){
  # cc = cog cond
  # dv = dv of interest (either coef or p from data)
  # data = nulls
  # model.results = coefs and ps from original models
  # model.dv = the variable you want from the data frame of the original model results
  
  # first filter model results
  mr <- model.results %>% filter(cog_cond==cc)
  mr$y = .05
  data %>% filter(cog_cond == cc) %>%
    ggplot(aes(x=get(dv), fill=term, colour=term)) +
    geom_density(alpha=0.2) +
    facet_wrap(.~term) +
    theme_cowplot() +
    theme(legend.position = 'none') +
    ggtitle(cc) +
    geom_point(data=mr, 
               mapping=aes(x=get(model.dv), y=y, shape=sig)) +
    facet_wrap(.~term)             
}

# now generate the coefficients plots for s, FM & SM
# lapply(levels(nulls$cog_cond), plot.info, dv='coef', data=nulls, model.results=orig.results, model.dv='Estimate')
# now generate the plots for the p-values
# lapply(levels(nulls$cog_cond), plot.info, dv='p', data=nulls, model.results=orig.results, model.dv='Pr...t..')

