devtools::install_github("holtzy/epuRate", force=TRUE)


library(corpcor)
library(GPArotation)
library(psych)
library(ggcorrplot)
library(tidyverse)
library(cowplot)
library(GGally)
library(wesanderson)
library(nFactors)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("KG_data-wrangling.R")
source("R_rainclouds.R")
library(rmarkdown)    # You need this library to run this template.
library(epuRate)      # Install with devtools: install_github("holtzy/epuRate", force=TRUE)


# Load Data
# ____________________________________________________________________________________________________________
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
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Lenticular_nucleus_putamen_Right"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Lenticular_nucleus_putamen_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Lenticular_nucleus_putamen_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Lenticular_nucleus_putamen_Right"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Caudate_nucleus_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Caudate_nucleus_Right"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Caudate_nucleus_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Caudate_nucleus_Right"))

sub.data <- GetDTIData(fpath, tracts)

# Defining levels and variables etc.
sub.data$group <- as.factor(sub.data$group)
levels(sub.data$group) <- c("practice", "control")

sub.data$session <- as.factor(sub.data$session)
levels(sub.data$session) <- c("pre-training", "post-training")

sub.data$tract_names <- c("lDLPFC_lACC", "rDLPFC_rACC", "lDLPFC_rACC", "rDLPFC_lACC",
                          "lDLPFC_lCN", "rDLPFC_rCN", "lDLPFC_rCN", "rDLPFC_lCN",
                          "lDLPFC_lLNP", "rDLPFC_rLNP", "lDLPFC_rLNP", "rDLPFC_lLNP", 
                          "rACC_rLNP", "lACC_lLNP", "rACC_lLNP", "lACC_rLNP",
                          "lACC_lCN", "rACC_rCN", "rACC_lCN", "lACC_rCN")
sub.data$tract_names <- as.factor(sub.data$tract_names)
sub.data$sub <- as.factor(sub.data$sub)

#Removing outliers/merging data across sessions
sub.data <- sub.data %>% distinct(sub, tract_names, .keep_all = TRUE)

s1.data <- sub.data %>% filter(session == 0)
s1.data$sub <- as.factor(s1.data$sub)
# because we lost some session 1 DTI data in the great back up miss of 2014, I am going to work out who does not have session 1 data, and I’ll add their session 2 data to this dataframe
missed.subs <- unique(sub.data$sub)[!(unique(sub.data$sub) %in% unique(s1.data$sub))]
s1.data <- rbind(s1.data, sub.data[sub.data$sub %in% missed.subs, ])

s1.data <- s1.data[s1.data$FA > 0,]

s1.data<- s1.data %>% filter(FA > (mean(FA) - (3*sd(FA))))
s1.data <- s1.data %>% filter(FA < (mean(FA) + (3*sd(FA))))

spare<- s1.data
# This is for just incase I make a big error and need another dataframe to work from :)

# _____________________________________________________________________________________________________________

# Densities, data points and boxplots for our FA of interest
ggplot(s1.data, aes(x=tract_names, y=FA, fill = tract_names, colour = tract_names)) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust =2, trim =
                     TRUE) +
  geom_point(position=position_jitter(width=.15), size=.25) +
  geom_boxplot(aes(x = tract_names, y = FA), outlier.shape = NA,
               alpha = 0.3, width = .1, colour = "BLACK") +
  scale_y_continuous(limits=c(0.2,0.6)) + coord_flip() +
  ylab('FA') + xlab('connection') + theme_cowplot() + 
  guides(fill = FALSE, colour = FALSE) +
  theme(axis.title.x = element_text(face = "italic"))
![density/qq/datapoint] ("documents/THESIS/pathways-to-practice/densityqq.png")

#  ACC to cingulate is looking very sparse esp. contralateral...


#Checking for outliers by looking at z-scores more than 3.29
outliers <- s1.data %>% group_by(tract_names) %>%
  filter(((FA - mean(FA))/sd(FA)) > 3.29)
outliers
#There are no outliers it seems

#Bivariate Correlation charts
s1.dat.wide <- s1.data %>% select(-c(group, session, tract_start, tract_end)) %>%
  pivot_wider(id_cols=sub, names_from=tract_names, values_from=FA) %>%
  drop_na()
s1.dat.wide %>% select(-sub) %>% pairs()
![bivariatecorrelation] ("documents/THESIS/pathways-to-practice/bivariatec.png")
#These are kind of hard to interpret bc they're pretty busy. I think it's mostly okay though

#Variance and ratio
vars <- s1.data %>% select(c(tract_names, FA)) %>%
  group_by(tract_names) %>%
  summarise(Var=var(FA))

sprintf("ratio of largest to smallest variances: %f", max(vars$Var)/min(vars$Var))
#8.017283
#Nice and under 10 number

#qq plots for normality
qqp <- ggplot(s1.data, aes(sample=sqrt(FA))) +
  stat_qq() + stat_qq_line() + facet_wrap(.~tract_names)
qqp
![qqplot2] ("documents/THESIS/pathways-to-practice/qqplot2.png")
#Not looking too bad rACC_lCN is a bit wonky and rDLPFC_rACC starts low but I think all is well

mhl.mat <- as.matrix(s1.dat.wide[,2:length(colnames(s1.dat.wide))]) # here I have just turned the data into a matrix so that the subsequent functions I call can work with it
mhl.cov <- cov(mhl.mat) # here I get the covariance matrix
mhl.dist <- mahalanobis(mhl.mat, colMeans(mhl.mat), mhl.cov) # now calc the M dist
hist(mhl.dist, breaks = 20, col=wes_palette("Royal2")[1])
![mahalanobis] ("documents/THESIS/pathways-to-practice/mahalanobis.png")

sprintf("For a Mahalanobis to be less that .1 per cent likely to have occured by chance, given our degrees of feedom (%f), it has to be a value greater than %f", length(mhl.dist)-1, qchisq(.001, df=length(mhl.dist)-1))
#51.084602
#Were good for this one

cor.mat <- cor(mhl.mat)
cor.plt <- ggcorrplot(cor.mat, 
                      hc.order = FALSE,
                      type = "upper",
                      outline.color = "white",
                      ggtheme = ggplot2::theme_gray,
                      colors = c(wes_palette("Royal2")[1], "white", wes_palette("Royal2")[3])
)
cor.plt
![multising] ("documents/THESIS/pathways-to-practice/multising.png")
# Everything looks good for this one

abs(cor.mat) > .9
# nonw have a true result we are looking good here


save.image (file = ExplorationEnvi.RData)

?save.image



