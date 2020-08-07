# Written by K. Garner and Georgia Marsh, 2020
# Re-run of factor analysis, etc. with removal of lTHA_rSOG, as per initial loadings
# found within the EFA script
# --------------------------------------------------------------------------------

rm(list=ls())

# set working directory to current location
install.packages("rstudioapi")  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

fpath <- 'C:/Git/pathways-to-practice/dti-data/' 

tracts <- list(c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Left"), 
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Superior_occipital_gyrus_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Superior_occipital_gyrus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Superior_occipital_gyrus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Superior_occipital_gyrus_Left"),
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
drop <- c("rTHA_lSOG", "rDLPFC_lSPG", "lDLPFC_rSPG", "lCN_rSOG", "lCN_lSOG", "rCN_lSOG", "lTHA_rSOG")
s1.data = s1.data[,!(names(s1.data) %in% drop)]


s1.data.wide = s1.data.wide[,!(names(s1.data.wide) %in% drop)]
s1.data.wide <- s1.data %>% select(-c(group, session, tract_start, tract_end)) %>%
  pivot_wider(id_cols=sub, names_from=tract_names, values_from=FA)
s1.data.wide <- na.omit(s1.data.wide) 

# apply the pairs() function to my new dataframe - see https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/pairs for function documentation 
s1.data.wide %>% select(-sub) %>% pairs()
# Data quite busy, difficult to interpret, though no tracts look particularly out of the ordinary

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

#EFA:
# Optimal coordinates:
ev <- eigen(cor.mat) # get the eigenvalues using the correlation matrix
ap <- parallel(subject=nrow(mhl.mat), var=ncol(mhl.mat), rep=1000, quantile=.05, model="factors") # run the parallel analysis
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea, criteria=0) # calculate the 95% values for the parallel analysis, plus the accelaration factor and the optimal coordinates
plotnScree(nS, xlab="Eigenvalues")

# Factor analysis:
factors = c(2,3,4,5,6)
factor_models <- lapply(factors, function(x) factanal(mhl.mat, factors=x)) # run factor analysis with 2, 3, or 4 factors, save output of each to factor_models
factor_models[[1]]

# Sum of square loadings:
lapply(factors, function(x) factor_models[[x-1]]$loadings)

# Testing null hyp:
lapply(factors, function(x) factor_models[[x-1]]$PVAL < .008)

#Two factor solution:
rotations = c("varimax", "promax")
lapply(rotations, function(x) factanal(mhl.mat, factors=2, rotation=x))

#Three factor solution:
lapply(rotations, function(x) factanal(mhl.mat, factors=3, rotation=x))

# Four factor solution:
lapply(rotations, function(x) factanal(mhl.mat, factors=4, rotation=x))

# Five factor solution:
lapply(rotations, function(x) factanal(mhl.mat, factors=5, rotation=x))

# Two factor solution reduces the 14 tracts to two factors that represent the multitasking network and visual
# search network

# Regression-like coefficients for weighting:
# Two Factor Solution
fact.solution <- factanal(mhl.mat, factors=2, rotation="varimax")
nu.data <- factor.scores(mhl.mat, fact.solution$loadings, method="tenBerge")

# now, show the data
nu.data$scores %>% head(5)

regression.dat <- data.frame(sub = s1.data.wide$sub,
                             multitasking_network = nu.data$scores[,'Factor1'],
                             visual_network = nu.data$scores[,'Factor2'])
                            
# now, show the data
regression.dat %>% head(5)
