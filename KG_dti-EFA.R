# written by K. Garner and Elizabeth Geary, 2020
# this code reads in the DTI data, tidies it, plots boxplot and qqplots to 
# detect outliers and determine normality. We then remove outliers and save the
# remaining data as a csv file.

# written by K. Garner, 2020
# this code reads in the DTI data, tidies it, plots the chosen tracts as a 
# raincloud plot

rm(list=ls())

# set working directory to current location
# install.packages("rstudioapi")  # uncomment and run if you don't already have this package installed
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to the location of this file

# load packages and source data-wrangling functions
# --------------------------------------------------------------------------------
# install.packages("tidyverse")  # uncomment and run if you don't have tidyverse 
# install.packages("cowplot)
# installed already
# library(tidyverse)
# library(cowplot)
# source("KG_data-wrangling.R")
# source("R_rainclouds.R")
source("KG_plot-tract-hists.R")
#install.packages("corpcor")
#install.packages("GPArotation")
#install.packages("ggcorrplot")
install.packages("nFactors")
library(corpcor)
library(GPArotation)
library(psych)
library(ggcorrplot)



# 1. qqplots to examine each of the tracts
# --------------------------------------------------------------------------------
qqp <- ggplot(s1.data, aes(sample=sqrt(FA))) +
              stat_qq() + stat_qq_line() + facet_wrap(.~tract_name)

## The distributions look largely normal, apart from LCN -> RPut, and RCN -> LPut

# 2. Are there any multivariate outliers? (compute Mahalanobis distance)
# --------------------------------------------------------------------------------
s1.data <- s1.data %>% distinct(sub, tract_name, .keep_all = TRUE) 
mhl.dat <- s1.data %>% pivot_wider(id_cols=sub, names_from=tract_name, values_from=FA) %>%
                       drop_na()
mhl.mat <- as.matrix(mhl.dat[,2:length(colnames(mhl.dat))])
mhl.cov <- cov(mhl.mat)
mhl.dist <- mahalanobis(mhl.mat, colMeans(mhl.mat), mhl.cov)
hist(mhl.dist, breaks = 20)
# looks like the participant with a mhl.dist > 30 could be an outlier, will look ar their
# data
poss.outlier.sub <- mhl.dat$sub[mhl.dist > 30]
View(s1.data[s1.data$sub == "150", ])
# None of the values look particularly crazy, so I am hesitant to take them out

# 3. Get EFA dataframe
# --------------------------------------------------------------------------------
efa.dat <- mhl.dat %>% pivot_longer(colnames(mhl.mat), names_to = "tract", values_to="FA")

# 4. Calculate correlation matrix and run basic checks
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

# the plot suggests that the correlations for the CN to Put tracts are too low with the other variables,
# i.e. consistently lower than ~.3 - check if this causes a problem for the EFA by []
# the below formally checks this.
abs(cor.mat)>.3
abs(cor.mat)>.9 # none of the correlations are higer than this

# now perform Bartlett's test to make sure the variances of the variables are not the same
cortest.bartlett(cor.mat)
# $chisq
# [1] 980.4191
# 
# $p.value
# [1] 5.633225e-135
# 
# $df
# [1] 120

# now get the determinant of the correlation matrix, is it > .00001 
det(cor.mat) > 0.00001

# 5. start with a PCA with orthogonal rotation, and look at the number of components 
# -------------------------------------------------------------------------------------------
orth.fit.1 <- principal(mhl.mat, factors=16, rotation="varimax") # how many factors to set here?
plot(orth.fit.1$values, type="lines") # look at the eigenvalues
abline(h=1, lty=2) # data suggests a 3 factor solution
orth.fit.3.fact <- principal(mhl.mat, factors=3, rotation="varimax")
print.psych(orth.fit.3.fact, cut=0.3, sort=TRUE)

# 6. Print hello world
# ----------------------
print('hello world')