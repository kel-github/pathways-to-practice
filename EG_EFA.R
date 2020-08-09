# This is the code for the actual exploratory factor analysis, for deteriming factor
# structure etc.
# 
# This is to be run after the correlations and exploration code

# written by K. Garner and E. Geary, August 2020

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
library(rmarkdown)   
library(epuRate)

#Data Loading + Cleaning
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

#Other bits

s1.dat.wide <- s1.data %>% select(-c(group, session, tract_start, tract_end)) %>%
  pivot_wider(id_cols=sub, names_from=tract_names, values_from=FA) %>%
  drop_na()

mhl.mat <- as.matrix(s1.dat.wide[,2:length(colnames(s1.dat.wide))]) # here I have just turned the data into a matrix so that the subsequent functions I call can work with it
mhl.cov <- cov(mhl.mat) # here I get the covariance matrix
mhl.dist <- mahalanobis(mhl.mat, colMeans(mhl.mat), mhl.cov) # now calc the M dist

cor.mat <- cor(mhl.mat)

#EFA
#____________________________________________________________________________________________________

ev <- eigen(cor.mat) # get the eigenvalues using the correlation matrix
ap <- parallel(subject=nrow(mhl.mat), var=ncol(mhl.mat), rep=1000, quantile=.05, model="factors") # run the parallel analysis
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea, criteria=0) # calculate the 95% values for the parallel analysis, plus the accelaration factor and the optimal coordinates
plotnScree(nS, xlab="Eigenvalues")
![screeplot] ("documents/THESIS/pathways-to-practice/screeplot.png)

#point of inflection is at 2 factors and the curve seems to sit at around 9 so as such I will look for 
# solutions betweek 2-9 factors

factors = c(2,3,4,5,6,7,8,9)
factor_models <- lapply(factors, function(x) factanal(mhl.mat, factors=x)) # run factor analysis with 2, 3, or 4 factors, save output of each to factor_models
factor_models[[1]]

# Uniquenesses:
# lDLPFC_lACC rDLPFC_rACC lDLPFC_rACC rDLPFC_lACC  lDLPFC_lCN  rDLPFC_rCN  lDLPFC_rCN  rDLPFC_lCN 
#       0.367       0.623       0.341       0.385       0.308       0.425       0.618       0.471 
# lDLPFC_lLNP rDLPFC_rLNP lDLPFC_rLNP rDLPFC_lLNP   rACC_rLNP   lACC_lLNP   rACC_lLNP   lACC_rLNP 
#       0.131       0.169       0.242       0.309       0.381       0.568       0.401       0.488 
#    lACC_lCN    rACC_rCN    rACC_lCN    lACC_rCN 
#       0.701       0.735       0.543       0.847 
# 
# Loadings:
#             Factor1 Factor2
# lDLPFC_lACC  0.430   0.669 
# rDLPFC_rACC  0.120   0.602 
# lDLPFC_rACC  0.355   0.730 
# rDLPFC_lACC  0.397   0.676 
# lDLPFC_lCN   0.696   0.455 
# rDLPFC_rCN   0.692   0.309 
# lDLPFC_rCN   0.338   0.518 
# rDLPFC_lCN   0.313   0.657 
# lDLPFC_lLNP  0.930         
# rDLPFC_rLNP  0.909         
# lDLPFC_rLNP  0.731   0.474 
# rDLPFC_lLNP  0.516   0.652 
# rACC_rLNP    0.719   0.320 
# lACC_lLNP    0.552   0.357 
# rACC_lLNP    0.551   0.544 
# lACC_rLNP    0.559   0.447 
# lACC_lCN     0.232   0.495 
# rACC_rCN     0.181   0.482 
# rACC_lCN             0.675 
# lACC_rCN             0.387 
# 
#                Factor1 Factor2
# SS loadings      5.679   5.266
# Proportion Var   0.284   0.263
# Cumulative Var   0.284   0.547
# 
# Test of the hypothesis that 2 factors are sufficient.
# The chi square statistic is 291.39 on 151 degrees of freedom.
# The p-value is 5.64e-11 

lapply(factors, function(x) factor_models[[x-1]]$loadings)
# After looking at this when we get to 6 factors there begins to be less than 1 eigenvalue loading
#After that the remaining factors dont surpass 1. So I think i'll stick with 5 factors - although
# as you get more than 6 factors the 5th factor becomes less than 1 loading but I think given the 6th 
#iteration we should keep the fifth.

# [[1]]
# 
# Loadings:
#             Factor1 Factor2
# lDLPFC_lACC  0.430   0.669 
# rDLPFC_rACC  0.120   0.602 
# lDLPFC_rACC  0.355   0.730 
# rDLPFC_lACC  0.397   0.676 
# lDLPFC_lCN   0.696   0.455 
# rDLPFC_rCN   0.692   0.309 
# lDLPFC_rCN   0.338   0.518 
# rDLPFC_lCN   0.313   0.657 
# lDLPFC_lLNP  0.930         
# rDLPFC_rLNP  0.909         
# lDLPFC_rLNP  0.731   0.474 
# rDLPFC_lLNP  0.516   0.652 
# rACC_rLNP    0.719   0.320 
# lACC_lLNP    0.552   0.357 
# rACC_lLNP    0.551   0.544 
# lACC_rLNP    0.559   0.447 
# lACC_lCN     0.232   0.495 
# rACC_rCN     0.181   0.482 
# rACC_lCN             0.675 
# lACC_rCN             0.387 
# 
#                Factor1 Factor2
# SS loadings      5.679   5.266
# Proportion Var   0.284   0.263
# Cumulative Var   0.284   0.547
# 
# [[2]]
# 
# Loadings:
#             Factor1 Factor2 Factor3
# lDLPFC_lACC  0.335   0.765   0.185 
# rDLPFC_rACC          0.661   0.145 
# lDLPFC_rACC  0.261   0.740   0.297 
# rDLPFC_lACC  0.314   0.666   0.294 
# lDLPFC_lCN   0.645   0.428   0.309 
# rDLPFC_rCN   0.673   0.234   0.317 
# lDLPFC_rCN   0.312   0.237   0.615 
# rDLPFC_lCN   0.266   0.388   0.629 
# lDLPFC_lLNP  0.913   0.159         
# rDLPFC_rLNP  0.915                 
# lDLPFC_rLNP  0.666   0.576   0.149 
# rDLPFC_lLNP  0.437   0.639   0.313 
# rACC_rLNP    0.680   0.349   0.184 
# lACC_lLNP    0.532   0.224   0.379 
# rACC_lLNP    0.491   0.493   0.331 
# lACC_rLNP    0.504   0.461   0.212 
# lACC_lCN     0.202   0.265   0.507 
# rACC_rCN     0.161   0.181   0.594 
# rACC_lCN             0.355   0.650 
# lACC_rCN                     0.696 
# 
#                Factor1 Factor2 Factor3
# SS loadings      4.941   4.081   3.172
# Proportion Var   0.247   0.204   0.159
# Cumulative Var   0.247   0.451   0.610
# 
# [[3]]
# 
# Loadings:
#             Factor1 Factor2 Factor3 Factor4
# lDLPFC_lACC  0.311   0.765   0.157   0.157 
# rDLPFC_rACC          0.662   0.104   0.129 
# lDLPFC_rACC  0.264   0.757   0.306         
# rDLPFC_lACC  0.287   0.661   0.268   0.203 
# lDLPFC_lCN   0.625   0.437   0.292   0.156 
# rDLPFC_rCN   0.663   0.244   0.315   0.117 
# lDLPFC_rCN   0.300   0.243   0.626   0.111 
# rDLPFC_lCN   0.254   0.402   0.615   0.116 
# lDLPFC_lLNP  0.902   0.166           0.133 
# rDLPFC_rLNP  0.911                   0.106 
# lDLPFC_rLNP  0.646   0.578   0.130   0.169 
# rDLPFC_lLNP  0.430   0.643   0.313         
# rACC_rLNP    0.627   0.349   0.108   0.392 
# lACC_lLNP    0.417   0.175   0.260   0.850 
# rACC_lLNP    0.469   0.499   0.310   0.175 
# lACC_rLNP    0.505   0.467   0.221         
# lACC_lCN     0.129   0.261   0.426   0.430 
# rACC_rCN     0.133   0.197   0.557   0.170 
# rACC_lCN    -0.111   0.368   0.610   0.160 
# lACC_rCN                     0.740         
# 
#                Factor1 Factor2 Factor3 Factor4
# SS loadings      4.570   4.145   2.908   1.360
# Proportion Var   0.228   0.207   0.145   0.068
# Cumulative Var   0.228   0.436   0.581   0.649
# 
# [[4]]
# 
# Loadings:
#             Factor1 Factor2 Factor3 Factor4 Factor5
# lDLPFC_lACC  0.313   0.786   0.151   0.166         
# rDLPFC_rACC          0.686   0.115   0.169         
# lDLPFC_rACC  0.273   0.711   0.227           0.334 
# rDLPFC_lACC  0.303   0.609   0.173   0.194   0.320 
# lDLPFC_lCN   0.620   0.439   0.303   0.179         
# rDLPFC_rCN   0.650   0.273   0.384   0.127         
# lDLPFC_rCN   0.280   0.257   0.630   0.132   0.147 
# rDLPFC_lCN   0.252   0.329   0.491   0.129   0.473 
# lDLPFC_lLNP  0.911   0.126           0.128   0.101 
# rDLPFC_rLNP  0.909                                 
# lDLPFC_rLNP  0.651   0.570   0.133   0.170         
# rDLPFC_lLNP  0.448   0.573   0.192           0.410 
# rACC_rLNP    0.631   0.331   0.102   0.414         
# lACC_lLNP    0.432   0.163   0.207   0.775   0.141 
# rACC_lLNP    0.483   0.421   0.188   0.173   0.402 
# lACC_rLNP    0.509   0.429   0.178           0.201 
# lACC_lCN     0.113   0.256   0.383   0.491   0.165 
# rACC_rCN     0.113   0.215   0.572   0.224         
# rACC_lCN    -0.116   0.249   0.414   0.201   0.653 
# lACC_rCN                     0.749           0.142 
# 
#                Factor1 Factor2 Factor3 Factor4 Factor5
# SS loadings       4.61   3.748   2.378   1.374   1.373
# Proportion Var    0.23   0.187   0.119   0.069   0.069
# Cumulative Var    0.23   0.418   0.537   0.605   0.674
# 
# [[5]]
# 
# Loadings:
#             Factor1 Factor2 Factor3 Factor4 Factor5 Factor6
# lDLPFC_lACC  0.319   0.795   0.143   0.141   0.161         
# rDLPFC_rACC          0.669   0.114           0.177   0.176 
# lDLPFC_rACC  0.281   0.703   0.210   0.407                 
# rDLPFC_lACC  0.267   0.569   0.167   0.325   0.204   0.290 
# lDLPFC_lCN   0.613   0.436   0.298   0.122   0.178   0.110 
# rDLPFC_rCN   0.663   0.274   0.376           0.122         
# lDLPFC_rCN   0.276   0.248   0.622   0.178   0.131         
# rDLPFC_lCN   0.243   0.299   0.464   0.502   0.136   0.112 
# lDLPFC_lLNP  0.898   0.117           0.106   0.130   0.125 
# rDLPFC_rLNP  0.908                                         
# lDLPFC_rLNP  0.638   0.554   0.126   0.123   0.170   0.173 
# rDLPFC_lLNP  0.423   0.536   0.178   0.427           0.230 
# rACC_rLNP    0.610   0.315                   0.421   0.212 
# lACC_lLNP    0.432   0.157   0.202   0.153   0.770         
# rACC_lLNP    0.478   0.395   0.172   0.439   0.170   0.102 
# lACC_rLNP    0.419   0.369   0.183   0.129           0.794 
# lACC_lCN     0.115   0.251   0.373   0.190   0.491         
# rACC_rCN     0.122   0.214   0.565   0.125   0.227         
# rACC_lCN    -0.123   0.214   0.391   0.678   0.209         
# lACC_rCN                     0.774   0.135           0.143 
# 
#                Factor1 Factor2 Factor3 Factor4 Factor5 Factor6
# SS loadings      4.425   3.494   2.311   1.568   1.382   0.965
# Proportion Var   0.221   0.175   0.116   0.078   0.069   0.048
# Cumulative Var   0.221   0.396   0.512   0.590   0.659   0.707
# 
# [[6]]
# 
# Loadings:
#             Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7
# lDLPFC_lACC  0.304   0.826   0.186   0.173                         
# rDLPFC_rACC          0.661   0.101   0.158   0.196           0.171 
# lDLPFC_rACC  0.258   0.744   0.292                   0.256         
# rDLPFC_lACC  0.234   0.610   0.201   0.213   0.298   0.302   0.110 
# lDLPFC_lCN   0.628   0.452   0.367   0.162          -0.101         
# rDLPFC_rCN   0.613   0.279   0.342                           0.644 
# lDLPFC_rCN   0.266   0.251   0.637   0.132                         
# rDLPFC_lCN   0.216   0.359   0.588   0.111   0.103   0.327   0.110 
# lDLPFC_lLNP  0.928   0.139           0.130                         
# rDLPFC_rLNP  0.875                   0.112                   0.191 
# lDLPFC_rLNP  0.622   0.565   0.158   0.186   0.172                 
# rDLPFC_lLNP  0.405   0.593   0.266           0.221   0.345         
# rACC_rLNP    0.600   0.318   0.124   0.412   0.211                 
# lACC_lLNP    0.403   0.162   0.252   0.820           0.109         
# rACC_lLNP    0.459   0.455   0.285   0.169           0.320         
# lACC_rLNP    0.421   0.378   0.214           0.789                 
# lACC_lCN     0.114   0.264   0.451   0.458                         
# rACC_rCN     0.105   0.204   0.562   0.189                   0.173 
# rACC_lCN    -0.108   0.292   0.592   0.156           0.420  -0.136 
# lACC_rCN                     0.728           0.147                 
# 
#                Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7
# SS loadings      4.227   3.884   2.879   1.380   0.949   0.702   0.571
# Proportion Var   0.211   0.194   0.144   0.069   0.047   0.035   0.029
# Cumulative Var   0.211   0.406   0.550   0.619   0.666   0.701   0.730
# 
# [[7]]
# 
# Loadings:
#             Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7 Factor8
# lDLPFC_lACC  0.835   0.272   0.158   0.161   0.110          -0.112         
# rDLPFC_rACC  0.640                   0.217   0.169   0.200           0.219 
# lDLPFC_rACC  0.763   0.220   0.274                           0.175         
# rDLPFC_lACC  0.645   0.222   0.181   0.119   0.115   0.290   0.268   0.116 
# lDLPFC_lCN   0.472   0.598   0.345   0.166   0.117          -0.124         
# rDLPFC_rCN   0.293   0.607   0.347           0.109                   0.636 
# lDLPFC_rCN   0.275   0.266   0.619   0.172                                 
# rDLPFC_lCN   0.405   0.212   0.595                           0.321         
# lDLPFC_lLNP  0.185   0.905                   0.150   0.113                 
# rDLPFC_rLNP          0.893                           0.113           0.176 
# lDLPFC_rLNP  0.588   0.594   0.157           0.200   0.172                 
# rDLPFC_lLNP  0.649   0.395   0.255                   0.222   0.285         
# rACC_rLNP    0.311   0.551   0.119           0.730   0.178                 
# lACC_lLNP    0.210   0.444   0.235   0.393   0.405           0.143         
# rACC_lLNP    0.498   0.453   0.254   0.138                   0.291         
# lACC_rLNP    0.397   0.386   0.216           0.116   0.791                 
# lACC_lCN     0.224   0.130   0.328   0.900                                 
# rACC_rCN     0.196           0.563   0.178   0.258                   0.166 
# rACC_lCN     0.319  -0.116   0.541   0.233                   0.457  -0.120 
# lACC_rCN                     0.724                   0.140                 
# 
#                Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7 Factor8
# SS loadings      4.197   4.038   2.628   1.248   0.964   0.941   0.642   0.558
# Proportion Var   0.210   0.202   0.131   0.062   0.048   0.047   0.032   0.028
# Cumulative Var   0.210   0.412   0.543   0.606   0.654   0.701   0.733   0.761
# 
# [[8]]
# 
# Loadings:
#             Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7 Factor8 Factor9
# lDLPFC_lACC  0.828   0.265   0.145           0.196                                 
# rDLPFC_rACC  0.632                           0.247   0.213   0.142   0.244         
# lDLPFC_rACC  0.764   0.225   0.226   0.254                                         
# rDLPFC_lACC  0.629   0.215   0.159   0.177   0.174   0.299   0.114   0.109   0.249 
# lDLPFC_lCN   0.476   0.603   0.314           0.195   0.112                  -0.191 
# rDLPFC_rCN   0.299   0.610   0.371                                   0.619         
# lDLPFC_rCN   0.282   0.253   0.612   0.110   0.190                                 
# rDLPFC_lCN   0.388   0.222   0.492   0.412           0.101           0.111   0.137 
# lDLPFC_lLNP  0.187   0.908                           0.113   0.135                 
# rDLPFC_rLNP          0.886                           0.109           0.155         
# lDLPFC_rLNP  0.589   0.587   0.160           0.110   0.180   0.186                 
# rDLPFC_lLNP  0.654   0.384   0.246   0.198           0.218                   0.363 
# rACC_rLNP    0.310   0.558   0.115           0.160   0.180   0.713                 
# lACC_lLNP    0.190   0.433   0.223   0.109   0.530           0.367           0.134 
# rACC_lLNP    0.486   0.457   0.195   0.273   0.156   0.107                   0.119 
# lACC_rLNP    0.389   0.386   0.203                   0.797   0.111                 
# lACC_lCN     0.235   0.122   0.314   0.181   0.727                                 
# rACC_rCN     0.208           0.625           0.161           0.259   0.120   0.111 
# rACC_lCN     0.259           0.337   0.869   0.218                                 
# lACC_rCN                     0.717   0.161           0.147                         
# 
#                Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7 Factor8 Factor9
# SS loadings      4.114   4.007   2.299   1.244   1.182   0.977   0.857   0.519   0.322
# Proportion Var   0.206   0.200   0.115   0.062   0.059   0.049   0.043   0.026   0.016
# Cumulative Var   0.206   0.406   0.521   0.583   0.642   0.691   0.734   0.760   0.776

lapply(factors, function(x) factor_models[[x-1]]$PVAL < .006)
#1,2,3 TRUE         4,5,6,7,8 FALSE
#A bit conflicitng it could be only 4 factors that we need but we should do both to investigate

#Two factor
rotations = c("varimax", "promax")
lapply(rotations, function(x) factanal(mhl.mat, factors=2, rotation=x))

#No good, lots of pretty similar loadings with negligible differences
#some convergence re termination of tracts but theres still some opacity, promax offers no help

#Three factor solution
rotations = c("varimax", "promax")
lapply(rotations, function(x) factanal(mhl.mat, factors=3, rotation=x))
#This one is slightly clearer re ACC, caudate and putamen are not fully consistent but there
#is definitely more clarity here

# four factor solution
lapply(rotations, function(x) factanal(mhl.mat, factors=4, rotation=x))
# I like this one, there seems to be fairly solid loading on to (usually) one factor
# And there appears to be some degree of order to it in the varimax solution
# promax was a bit more unclear

#five factor solution
lapply(rotations, function(x) factanal(mhl.mat, factors=5, rotation=x))

#The dlpfc to acc seems to consistently load ont factor 2. The contralateral dlpfc-cn seem to be
#a separate thing from the ipsilateral ones similarly the contralateral dlpfc-put also are the same
# for both sets of tract ipsilateral load onto factor 1 and contralateral load onto factor 2
# acc to CN is fairly erratic

#six factor solution
lapply(rotations, function(x) factanal(mhl.mat, factors=6, rotation=x))
# the addition of a sixth factor is mostly important for one tract it seems - Also
# The bigger I go the more important the first two factors seem and the more the later ones become
#more insignificant factor 6 is ss<1 in the promax solution.

#seven factor solution
lapply(rotations, function(x) factanal(mhl.mat, factors=7, rotation=x))
#the addition of the seventh factor renders the 5,6,and 7th factors useless and mesy and the promax 
#solution just becomes less and less sensical.

#I don't think I'll continue with the rest of the read outs bc they're getting more and more useless.
#But upon looking at the options I do have - I think the four factor solution provides the best explanation
# 

fact.solution <- factanal(mhl.mat, factors=3, rotation="varimax")
nu.data <- factor.scores(mhl.mat, fact.solution$loadings, method="tenBerge")

# now, show the data
nu.data$scores %>% head(87)



