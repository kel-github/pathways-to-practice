This code is for the pre/post tract correlations to ascertain validity

rm(list=ls())
# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages and source data-wrangling functions
# --------------------------------------------------------------------------------
library(tidyverse)
source("KG_data-wrangling.R")
source("R_rainclouds.R")

# load data
# --------------------------------------------------------------------------------
fpath <- '~/Documents/THESIS/pathways-to-practice/dti-data/'
tracts <- list(c("Superior_Frontal_gyrus_dorsolateral_Left", "Anterior_cingulate_and_paracingulate_gyri_Left"), # amend this to contain which tracts you seek
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Anterior_cingulate_and_paracingulate_gyri_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Anterior_cingulate_and_paracingulate_gyri_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Anterior_cingulate_and_paracingulate_gyri_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Lenticular_nucleus_putamen_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Lenticular_nucleus_putamen_Right"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Lenticular_nucleus_putamen_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Lenticular_nucleus_putamen_Right"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Caudate_nucleus_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Caudate_nucleus_Right"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Caudate_nucleus_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Caudate_nucleus_Right"))

sub.data <- GetDTIData(fpath, tracts)
# relabel factors etc
sub.data$group <- as.factor(sub.data$group)
levels(sub.data$group) <- c("practice", "control")

sub.data$session <- as.factor(sub.data$session)
levels(sub.data$session) <- c("pre-training", "post-training")

sub.data$tract_names <- c("lDLPFC_lACC", "rDLPFC_rACC", "lDLPFC_rACC", "rDLPFC_lACC",
                          "lDLPFC_lCN", "rDLPFC_rCN", "lDLPFC_rCN", "rDLPFC_lCN",
                          "lDLPFC_lLNP", "rDLPFC_rLNP", "lDLPFC_rLNP", "rDLPFC_lLNP",
                          "lACC_lLNP", "rACC_rLNP", "rACC_lLNP", "lACC_rLNP",
                          "lACC_lCN", "rACC_rCN", "rACC_lCN", "lACC_rCN")


# KG note: you need to define tract names as a factor, if not this will lead you to errors later on
sub.data$tract_names <- as.factor(sub.data$tract_names)

sub.data$sub <- as.factor(sub.data$sub)

sub.data <- sub.data %>% unique()

__________________________________________

sub.count <- sub.data %>% group_by(session, tract_names) %>%
  summarise (mean(FA))

getcorrelation <- function(data, tract_name){
  frame_correlate <- data[data$tract_names==tract_name,]
  out <- with(frame_correlate, (cor.test(x=FA[session=='pre-training'], 
                                         y=FA[session=='post-training'])))
  out
}


uniquetracts <- unique(sub.count$tract_names)

cors <- lapply(uniquetracts, getcorrelation, data=sub.count)







