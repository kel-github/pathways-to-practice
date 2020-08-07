# Code that generates s1.data from start to finish
# This code reads in the DTI data, tidies it, plots boxplot and qqplots to 
# detect outliers and determine normality. We then remove outliers and save the
# remaining data as a csv file., and remove any tracts with excessive 0 values:

rm(list=ls())

# set working directory to current location
# install.packages("rstudioapi")  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages and source data-wrangling functions
# --------------------------------------------------------------------------------
# install.packages("tidyverse") 
library(tidyverse)
source("KG_data-wrangling.R")
source("R_rainclouds.R")
# install.packages("corpcor")
library(corpcor)
# install.packages("GPArotation")
library(GPArotation)
# install.packages("psych")
library(psych)
# install.packages("ggcorrplot")
library(ggcorrplot)
# install.packages("cowplot")
library(cowplot)
# install.packages("GGally")
library(GGally)
# install.packages("wesanderson")
library(wesanderson)
# install.packages("nFactors")
library(nFactors)
# install.packages("rmarkdown")
library(rmarkdown)    # You need this library to run this template.
install.packages("eupRate")
devtools::install_github("holtzy/epuRate", force=TRUE)
library(epuRate) 

# install.packages("devtools")
library(devtools)
# install_github("holtzy/epuRate")
library(epuRate)

# load data
# --------------------------------------------------------------------------------

fpath <- 'C:/Git/pathways-to-practice/dti-data/' 

tracts <- list(c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Left"), 
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Superior_parietal_gyrus_left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Superior_parietal_gyrus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Superior_parietal_gyrus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Superior_parietal_gyrus_Left"),
               c("Caudate_nucleus_Left", "Superior_occipital_gyrus_Left"),
               c("Caudate_nucleus_Left", "Superior_occipital_gyrus_Right"),
               c("Caudate_nucleus_Right", "Superior_occipital_gyrus_Right"),
               c("Caudate_nucleus_Right", "Superior_occipital_gyrus_Left"),
               c("Thalamus_Left", "Superior_occipital_gyrus_Left"),
               c("Thalamus_Left", "Superior_occipital_gyrus_Right"),
               c("Thalamus_Right", "Superior_occipital_gyrus_Right"),
               c("Thalamus_Right", "Superior_occipital_gyrus_Left"))  
sub.data <- GetDTIData(fpath, tracts)

# Define factors/tidy data frame:
# --------------------------------------------------------------------------------

sub.data$group <- as.factor(sub.data$group)
levels(sub.data$group) <- c("practice", "control")

sub.data$sub <- as.factor(sub.data$sub)

sub.data$session <- as.factor(sub.data$session)
levels(sub.data$session) <- c("pre-training", "post-training")

sub.data$tract_names <- c("lDLPFC_lCN", "lDLPFC_rCN", "rDLPFC_rCN", "rDLPFC_lCN",
                          "lDLPFC_lLNP", "lDLPFC_rLNP", "rDLPFC_rLNP", "rDLPFC_lLNP",
                          "lDLPFC_lSPG", "lDLPFC_rSPG", "rDLPFC_rSPG", "rDLPFC_lSPG",
                          "lCN_lSOG", "lCN_rSOG", "rCN_rSOG", "rCN_lSOG",
                          "lTHA_lSOG", "lTHA_rSOG", "rTHA_rSOG", "rTHA_lSOG")
sub.data$tract_names <- as.factor(sub.data$tract_names)

# Generate s1.data:
# --------------------------------------------------------------------------------

sub.data <- sub.data %>% unique()

# get s1 data
s1.data <- sub.data %>% filter(session == "pre-training")

# because we lost some session 1 DTI data in the great back up miss of 2014, I am going to work out who does not have session 1 data, and I'll add their session 2 data to this dataframe
missed.subs <- unique(sub.data$sub)[!(unique(sub.data$sub) %in% unique(s1.data$sub))]
s1.data <- rbind(s1.data, sub.data[sub.data$sub %in% missed.subs, ])

# Identify and remove outliers/0 values:
# --------------------------------------------------------------------------------
s1.data <- s1.data %>% filter(FA > 0)
# Retains any FA values above 0

s1.data <- s1.data %>% filter(FA < (mean(FA) + (3*sd(FA))))
# Filters out any FA outliers + 3*sd
s1.data<- s1.data %>% filter(FA > (mean(FA) - (3*sd(FA))))
# Filters out any FA outliers - 3*sd

# Bivariate Correlation Charts:
# --------------------------------------------------------------------------------
# use tidyverse functionality to convert from long to wide, drop people who have an NA value on some measure
# Percentage of 0 values in rTHA_lSOG, rDLPFC_lSPG and lDLPFC_rSPG affecting overall dataset, opted to remove from 
# final s1.data.wide data frame using "drop" function
drop <- c("rTHA_lSOG", "rDLPFC_lSPG", "lDLPFC_rSPG", "lCN_rSOG", "lCN_lSOG", "rCN_lSOG")
s1.data = s1.data[,!(names(s1.data) %in% drop)]


s1.data.wide = s1.data.wide[,!(names(s1.data.wide) %in% drop)]
s1.data.wide <- s1.data %>% select(-c(group, session, tract_start, tract_end)) %>%
  pivot_wider(id_cols=sub, names_from=tract_names, values_from=FA)
s1.data.wide <- na.omit(s1.data.wide) 

# apply the pairs() function to my new dataframe - see https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/pairs for function documentation 
s1.data.wide %>% select(-sub) %>% pairs()
# Data quite busy, difficult to interpret, though no tracts look particularly out of the ordinary

# Test data frame:
df = df[,!(names(df) %in% drop)]
df <- s1.data %>% select(-c(group, session, tract_start, tract_end)) %>%
  pivot_wider(id_cols=sub, names_from=tract_names, values_from=FA) %>%
  drop_na()

# Variance and ratio
# --------------------------------------------------------------------------------
vars <- s1.data %>% select(c(tract_names, FA)) %>%
  group_by(tract_names) %>%
  summarise(Var=var(FA))
# divide the largest by the smallest
sprintf("ratio of largest to smallest variances: %f", max(vars$Var)/min(vars$Var))
# 6.988553
# Low number, good result

# QQ Plots for Normality:
# --------------------------------------------------------------------------------
ggplot(s1.data, aes(sample = FA, colour = factor(tract_names))) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ tract_names, nrow = NULL)
# No major violations of normality, apart from potentially rCN_lSOG, however this is eventually
# removed from the dataset anyway

# Multivariate Outliers
# --------------------------------------------------------------------------------
mhl.mat <- as.matrix(s1.data.wide[,2:length(colnames(s1.data.wide))]) # here I have just turned the data into a matrix so that the subsequent functions I call can work with it
mhl.cov <- cov(mhl.mat) # here I get the covariance matrix
mhl.dist <- mahalanobis(mhl.mat, colMeans(mhl.mat), mhl.cov) # now calc the M dist
hist(mhl.dist, breaks = 20, col=wes_palette("Royal2")[1])

sprintf("For a Mahalanobis to be less that .1 per cent likely to have occured by chance, given our degrees of feedom (74.000000), it has to be a value greater than 42.009654", length(mhl.dist)-1, qchisq(.001, df=length(mhl.dist)-1))

# Multicollinearity and Singularity:
# --------------------------------------------------------------------------------
cor.mat <- cor(mhl.mat)
cor.plt <- ggcorrplot(cor.mat, 
                      hc.order = FALSE,
                      type = "upper",
                      outline.color = "white",
                      ggtheme = ggplot2::theme_gray,
                      colors = c("#6D9EC1", "white", "#E46726")
)
cor.plt
abs(cor.mat) > .9
# No multicollinearity or singularity