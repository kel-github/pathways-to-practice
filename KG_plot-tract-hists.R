# written by K. Garner and Georgia Marsh, 2020
# this code reads in the DTI data, tidies it, plots boxplot and qqplots to 
# detect outliers and determine normality. We then remove outliers and save the
# remaining data as a csv file.

rm(list=ls())

# set working directory to current location
# install.packages("rstudioapi")  # uncomment and run if you don't already have this package installed
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to the location of this file

# load packages and source data-wrangling functions
# --------------------------------------------------------------------------------
# install.packages("tidyverse")  # uncomment and run if you don't have tidyverse 
# install.packages("cowplot)
# installed already
library(tidyverse)
library(cowplot)
source("KG_data-wrangling.R")
source("R_rainclouds.R")

# define save variables
# --------------------------------------------------------------------------------
save.name <- paste(getwd(), 'plots', 'KG-tract-explore', sep="/") 

# load data
# --------------------------------------------------------------------------------
fpath <- '~/Dropbox/QBI/pathways-to-practice/dti-data/' # filepath to where the data lives
tracts <- list(c("Superior_Frontal_gyrus_dorsolateral_Left", "Anterior_cingulate_and_paracingulate_gyri_Left"), # amend this to contain which tracts you seek
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Anterior_cingulate_and_paracingulate_gyri_Right"), # amend this to contain which tracts you seek
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Anterior_cingulate_and_paracingulate_gyri_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Anterior_cingulate_and_paracingulate_gyri_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Left"),
               c("Caudate_nucleus_Right", "Lenticular_nucleus_putamen_Right"),
               c("Caudate_nucleus_Right", "Lenticular_nucleus_putamen_Left"),
               c("Caudate_nucleus_Left", "Lenticular_nucleus_putamen_Left"),
               c("Caudate_nucleus_Left", "Lenticular_nucleus_putamen_Right"))  # has to be written exactly as is written in the list file
sub.data <- GetDTIData(fpath, tracts)

# 1. name the tracts
# --------------------------------------------------------------------------------
N_tracts = 16
sub.data$tract_name <- rep( c("LDLPFC_LACC", "LDLFPC_LCN", "LDLPFC_LPut", "LDLPFC_RACC", "LDLPFC_RCN", "LDLPFC_RPut",
                              "RDLPFC_RACC", "RDLPFC_RCN", "RDLPFC_RPut", "RDLPFC_LACC", "RDLPFC_LCN", "RDLPFC_LPut",
                              "RCN_RPut", "RCN_LPut", "LCN_LPut", "LCN_RPut"), times = length(sub.data$sub)/N_tracts )

# 2. select session 1 data
# --------------------------------------------------------------------------------
s1.data <- sub.data %>% filter(session == 0)

# 3. plot session 1 data
# --------------------------------------------------------------------------------
p <- ggplot(s1.data, aes(x=tract_name, y=FA, fill = tract_name, colour = tract_name)) +
            geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust =2, trim =
                     TRUE) +
            geom_point(position=position_jitter(width=.15), size=.25) +
            geom_boxplot(aes(x = tract_name, y = FA), outlier.shape = NA,
                          alpha = 0.3, width = .1, colour = "BLACK") +
            scale_y_continuous(limits=c(-0.1,0.7)) + coord_flip() +
            ylab('FA') + xlab('connection') + theme_cowplot() + 
            guides(fill = FALSE, colour = FALSE) +
            theme(axis.title.x = element_text(face = "italic"))
ggsave(paste(save.name, '.pdf', sep=''), plot = p, width=20, height=30, units="cm")
