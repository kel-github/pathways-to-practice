# written by K. Garner and Elizabeth Geary, 2020
# this code reads in the DTI data, tidies it, plots boxplot and qqplots to 
# detect outliers and determine normality. We then remove outliers and save the
# remaining data as a csv file.

rm(list=ls())
install.packages("rstudioapi")
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
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Left"))

sub.data <- GetDTIData(fpath, tracts)

sub.data$group <- as.factor(sub.data$group)
levels(sub.data$group) <- c("practice", "control")

sub.data$session <- as.factor(sub.data$session)
levels(sub.data$session) <- c("pre-trial", "post-trial")

sub.data$tract_names <- c("lDLPFC_lACC", "rDLPFC_rACC", "lDLPFC_rACC", "rDLPFC_lACC",
                          "lDLPFC_lCN", "rDLPFC_rCN", "lDLPFC_rCN", "rDLPFC_lCN",
                          "lDLPFC_lLNP", "rDLPFC_rLNP", "lDLPFC_rLNP", "rDLPFC_lLNP")


summary <- sub.data %>% group_by(group, session) %>%
                        summarise(N=length(unique(sub)))
# group    session        N
# <fct>    <fct>      <int>
#   1 practice pre-trial     45
# 2 practice post-trial    39
# 3 control  pre-trial     45
# 4 control  post-trial    46


length(unique(sub.data)) == nrow(sub.data)
#[1] FALSE

#The FALSE result tells me that there is some suplicates

sum(duplicated(sub.data))
#[1] 84

#This tells me that there might be 84 cases of duplicated data

n_occur <- data.frame(table(sub.data$FA))
n_occur[n_occur$Freq > 1,]
sub.data[sub.data$FA %in% n_occur$Var1[n_occur$Freq > 1],]

# Var1 Freq
# 1                    0   10
# 22   0.294687409689091    2
# 56   0.319126152338936    2
# 89   0.330928761839292    2
# 108  0.334245083402268    2
# 133  0.338934297562504    2
# 136  0.339261508950464    2
# 140  0.339878686353904    2
# 157  0.341900443431703    2
# 194  0.346386601762198    2
# 200   0.34686996798348    2
# 209    0.3478902781576    2
# 241   0.35114261915623    2
# 245  0.351472940729645    2
# 290  0.354634877669755    2
# 304  0.355704503211969    2
# 327  0.357484871573354    2
# 345  0.359010034096426    2
# 438  0.364871843893455    2
# 490  0.367045638818777    2
# 502   0.36762254839197    2
# 504  0.367638698020513    2
# 510  0.367806318852869    2
# 554  0.370032716072237    2
# 557  0.370184142133906    2
# 582  0.371434503369797    2
# 617   0.37346577060017    2
# 619  0.373676137384369    2
# 630  0.374368039544495    2
# 634  0.374453752188884    2
# 693  0.377205403453862    2
# 759  0.379828384601795    2
# 760  0.379843819516308    2
# 803  0.383069679773317    2
# 942  0.392025517071244    2
# 1001 0.396688062765659    2
# 1020 0.397936326385558    2
# 1027 0.398558259490168    2
# 1031 0.398830814629291    2
# 1097 0.405652057989945    2
# 1111 0.407153987816762    2
# 1124 0.408624638109586    2
# 1131 0.409158198995793    2
# 1147   0.4104147507453    2
# 1152 0.410646928400696    2
# 1191 0.415612799953343    2
# 1193 0.415824905338595    2
# 1197 0.416725593427504    2
# 1214 0.418515525486122    2
# 1240 0.421297543100199    2
# 1277 0.425459686158222    2
# 1282 0.426154949300281    2
# 1287 0.426949485709255    2
# 1291 0.427278262051836    2
# 1332 0.430635066067435    2
# 1359 0.433835236877001    2
# 1391 0.436798513478138    2
# 1438 0.440584801304782    2
# 1467  0.44246978144299    2
# 1523 0.445680495470972    2
# 1538 0.446397594062048    2
# 1551 0.447258557635795    2
# 1595 0.450485638310828    2
# 1599 0.450642002499889    2
# 1613  0.45137703436052    2
# 1676 0.456891157038046    2
# 1713 0.459935531943978    2
# 1736 0.462170602360079    2
# 1742 0.462557730274313    2
# 1751   0.4631796635937    2
# 1788 0.465991656519092    2
# 1798 0.466630651134397    2
# 1803 0.467061594110318    2
# 1886  0.47517382534185    2
# 1903  0.47725093908046    2
# 1920 0.480610360097435    2
# 1931 0.482227815675373    2
# 1941 0.483139608936265    2
# 1978 0.487878150723026    2
# 1993 0.491164341033289    2
# 2004 0.493153277942636    2
# 2012 0.495680963053558    2
# 2030   0.5003513516166    2
# 2038 0.503080864607216    2
# 2057 0.508493016107755    2

# sub    group    session                               tract_start
# 1331908   105 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 2013700   105 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 2548418   102 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 2916865   101 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 4695557   106 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 5280771   104 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 5903041   101 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 10130116  105 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 10245124  105 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 10761410  102 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 11139075  104 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 11954689  101 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 12535489  101 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 14687237  106 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 19914244  105 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 20163075  104 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 20190916  105 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 20379842  102 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 22462465  101 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 22969537  101 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 24480773  106 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 33634565  106 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 34210564  105 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 34418116  105 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 34616258  102 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 35311875  104 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 37187329  101 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 37800385  101 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 44027330  102 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 46432708  105 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 46607620  105 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 46778113  101 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 47229889  101 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 47810309  106 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 47930115  104 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 54313732  105 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 54438338  102 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 55525633  101 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 55544260  105 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 56484099  104 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 56944901  106 practice  pre-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 57120193  101 practice post-trial  Superior_Frontal_gyrus_dorsolateral_Left
# 62558596  105 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 63305284  105 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 63692354  102 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 63738241  101 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 63756677  106 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 64894851  104 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 65240641  101 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 74204548  105 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 74462788  105 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 75444290  102 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 76628355  104 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 76679233  101 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 76798853  106 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 76923265  101 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 82675588  105 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 83707970  102 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 84228676  105 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 84804485  106 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 85108609  101 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 85929025  101 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 86196099  104 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 86815873  101 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 86972737  101 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 87124613  106 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 87438146  102 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 87677572  105 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 89281348  105 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 89396355  104 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 96978052  105 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 97752386  102 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 98664772  105 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 99203713  101 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 99849025  101 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 99853443  104 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 101470853 106 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 107577988 105 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 108246147 104 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 108338500 105 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 108831554 102 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 110328961 101 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# 110601025 101 practice post-trial Superior_Frontal_gyrus_dorsolateral_Right
# 111167621 106 practice  pre-trial Superior_Frontal_gyrus_dorsolateral_Right
# tract_end                FA tract_names Freq
# 1331908    Anterior_cingulate_and_paracingulate_gyri_Left 0.354634877669755 lDLPFC_lACC    2
# 2013700    Anterior_cingulate_and_paracingulate_gyri_Left 0.364871843893455 lDLPFC_lACC    2
# 2548418    Anterior_cingulate_and_paracingulate_gyri_Left 0.370032716072237 lDLPFC_lACC    2
# 2916865    Anterior_cingulate_and_paracingulate_gyri_Left 0.374453752188884 lDLPFC_lACC    2
# 4695557    Anterior_cingulate_and_paracingulate_gyri_Left 0.397936326385558 lDLPFC_lACC    2
# 5280771    Anterior_cingulate_and_paracingulate_gyri_Left   0.4104147507453 lDLPFC_lACC    2
# 5903041    Anterior_cingulate_and_paracingulate_gyri_Left 0.426154949300281 lDLPFC_lACC    2
# 10130116                             Caudate_nucleus_Left 0.334245083402268  lDLPFC_lCN    2
# 10245124                             Caudate_nucleus_Left 0.338934297562504  lDLPFC_lCN    2
# 10761410                             Caudate_nucleus_Left 0.351472940729645  lDLPFC_lCN    2
# 11139075                             Caudate_nucleus_Left 0.357484871573354  lDLPFC_lCN    2
# 11954689                             Caudate_nucleus_Left 0.367638698020513  lDLPFC_lCN    2
# 12535489                             Caudate_nucleus_Left 0.374368039544495  lDLPFC_lCN    2
# 14687237                             Caudate_nucleus_Left 0.405652057989945  lDLPFC_lCN    2
# 19914244                  Lenticular_nucleus_putamen_Left 0.339878686353904 lDLPFC_lLNP    2
# 20163075                  Lenticular_nucleus_putamen_Left 0.346386601762198 lDLPFC_lLNP    2
# 20190916                  Lenticular_nucleus_putamen_Left  0.34686996798348 lDLPFC_lLNP    2
# 20379842                  Lenticular_nucleus_putamen_Left  0.35114261915623 lDLPFC_lLNP    2
# 22462465                  Lenticular_nucleus_putamen_Left 0.377205403453862 lDLPFC_lLNP    2
# 22969537                  Lenticular_nucleus_putamen_Left 0.383069679773317 lDLPFC_lLNP    2
# 24480773                  Lenticular_nucleus_putamen_Left 0.409158198995793 lDLPFC_lLNP    2
# 33634565  Anterior_cingulate_and_paracingulate_gyri_Right 0.398558259490168 lDLPFC_rACC    2
# 34210564  Anterior_cingulate_and_paracingulate_gyri_Right 0.410646928400696 lDLPFC_rACC    2
# 34418116  Anterior_cingulate_and_paracingulate_gyri_Right 0.416725593427504 lDLPFC_rACC    2
# 34616258  Anterior_cingulate_and_paracingulate_gyri_Right 0.421297543100199 lDLPFC_rACC    2
# 35311875  Anterior_cingulate_and_paracingulate_gyri_Right 0.436798513478138 lDLPFC_rACC    2
# 37187329  Anterior_cingulate_and_paracingulate_gyri_Right 0.466630651134397 lDLPFC_rACC    2
# 37800385  Anterior_cingulate_and_paracingulate_gyri_Right 0.482227815675373 lDLPFC_rACC    2
# 44027330                            Caudate_nucleus_Right 0.415612799953343  lDLPFC_rCN    2
# 46432708                            Caudate_nucleus_Right 0.459935531943978  lDLPFC_rCN    2
# 46607620                            Caudate_nucleus_Right   0.4631796635937  lDLPFC_rCN    2
# 46778113                            Caudate_nucleus_Right 0.465991656519092  lDLPFC_rCN    2
# 47229889                            Caudate_nucleus_Right  0.47517382534185  lDLPFC_rCN    2
# 47810309                            Caudate_nucleus_Right 0.495680963053558  lDLPFC_rCN    2
# 47930115                            Caudate_nucleus_Right 0.503080864607216  lDLPFC_rCN    2
# 54313732                 Lenticular_nucleus_putamen_Right 0.430635066067435 lDLPFC_rLNP    2
# 54438338                 Lenticular_nucleus_putamen_Right 0.433835236877001 lDLPFC_rLNP    2
# 55525633                 Lenticular_nucleus_putamen_Right 0.450485638310828 lDLPFC_rLNP    2
# 55544260                 Lenticular_nucleus_putamen_Right 0.450642002499889 lDLPFC_rLNP    2
# 56484099                 Lenticular_nucleus_putamen_Right 0.467061594110318 lDLPFC_rLNP    2
# 56944901                 Lenticular_nucleus_putamen_Right  0.47725093908046 lDLPFC_rLNP    2
# 57120193                 Lenticular_nucleus_putamen_Right 0.483139608936265 lDLPFC_rLNP    2
# 62558596   Anterior_cingulate_and_paracingulate_gyri_Left 0.398830814629291 rDLPFC_lACC    2
# 63305284   Anterior_cingulate_and_paracingulate_gyri_Left 0.415824905338595 rDLPFC_lACC    2
# 63692354   Anterior_cingulate_and_paracingulate_gyri_Left 0.425459686158222 rDLPFC_lACC    2
# 63738241   Anterior_cingulate_and_paracingulate_gyri_Left 0.426949485709255 rDLPFC_lACC    2
# 63756677   Anterior_cingulate_and_paracingulate_gyri_Left 0.427278262051836 rDLPFC_lACC    2
# 64894851   Anterior_cingulate_and_paracingulate_gyri_Left 0.446397594062048 rDLPFC_lACC    2
# 65240641   Anterior_cingulate_and_paracingulate_gyri_Left  0.45137703436052 rDLPFC_lACC    2
# 74204548                             Caudate_nucleus_Left  0.44246978144299  rDLPFC_lCN    2
# 74462788                             Caudate_nucleus_Left 0.445680495470972  rDLPFC_lCN    2
# 75444290                             Caudate_nucleus_Left 0.462170602360079  rDLPFC_lCN    2
# 76628355                             Caudate_nucleus_Left 0.491164341033289  rDLPFC_lCN    2
# 76679233                             Caudate_nucleus_Left 0.493153277942636  rDLPFC_lCN    2
# 76798853                             Caudate_nucleus_Left   0.5003513516166  rDLPFC_lCN    2
# 76923265                             Caudate_nucleus_Left 0.508493016107755  rDLPFC_lCN    2
# 82675588                  Lenticular_nucleus_putamen_Left 0.418515525486122 rDLPFC_lLNP    2
# 83707970                  Lenticular_nucleus_putamen_Left 0.440584801304782 rDLPFC_lLNP    2
# 84228676                  Lenticular_nucleus_putamen_Left 0.447258557635795 rDLPFC_lLNP    2
# 84804485                  Lenticular_nucleus_putamen_Left 0.456891157038046 rDLPFC_lLNP    2
# 85108609                  Lenticular_nucleus_putamen_Left 0.462557730274313 rDLPFC_lLNP    2
# 85929025                  Lenticular_nucleus_putamen_Left 0.480610360097435 rDLPFC_lLNP    2
# 86196099                  Lenticular_nucleus_putamen_Left 0.487878150723026 rDLPFC_lLNP    2
# 86815873  Anterior_cingulate_and_paracingulate_gyri_Right 0.294687409689091 rDLPFC_rACC    2
# 86972737  Anterior_cingulate_and_paracingulate_gyri_Right 0.319126152338936 rDLPFC_rACC    2
# 87124613  Anterior_cingulate_and_paracingulate_gyri_Right 0.330928761839292 rDLPFC_rACC    2
# 87438146  Anterior_cingulate_and_paracingulate_gyri_Right 0.341900443431703 rDLPFC_rACC    2
# 87677572  Anterior_cingulate_and_paracingulate_gyri_Right   0.3478902781576 rDLPFC_rACC    2
# 89281348  Anterior_cingulate_and_paracingulate_gyri_Right 0.370184142133906 rDLPFC_rACC    2
# 89396355  Anterior_cingulate_and_paracingulate_gyri_Right 0.371434503369797 rDLPFC_rACC    2
# 96978052                            Caudate_nucleus_Right 0.339261508950464  rDLPFC_rCN    2
# 97752386                            Caudate_nucleus_Right 0.355704503211969  rDLPFC_rCN    2
# 98664772                            Caudate_nucleus_Right  0.36762254839197  rDLPFC_rCN    2
# 99203713                            Caudate_nucleus_Right 0.373676137384369  rDLPFC_rCN    2
# 99849025                            Caudate_nucleus_Right 0.379828384601795  rDLPFC_rCN    2
# 99853443                            Caudate_nucleus_Right 0.379843819516308  rDLPFC_rCN    2
# 101470853                           Caudate_nucleus_Right 0.407153987816762  rDLPFC_rCN    2
# 107577988                Lenticular_nucleus_putamen_Right 0.359010034096426 rDLPFC_rLNP    2
# 108246147                Lenticular_nucleus_putamen_Right 0.367045638818777 rDLPFC_rLNP    2
# 108338500                Lenticular_nucleus_putamen_Right 0.367806318852869 rDLPFC_rLNP    2
# 108831554                Lenticular_nucleus_putamen_Right  0.37346577060017 rDLPFC_rLNP    2
# 110328961                Lenticular_nucleus_putamen_Right 0.392025517071244 rDLPFC_rLNP    2
# 110601025                Lenticular_nucleus_putamen_Right 0.396688062765659 rDLPFC_rLNP    2
# 111167621                Lenticular_nucleus_putamen_Right 0.408624638109586 rDLPFC_rLNP    2)

#These are the duplicated bits --> for your reference I used this 
# https://stackoverflow.com/questions/16905425/find-duplicate-values-in-r

sub.data <- sub.data %>% distinct(sub, tract_names, .keep_all = TRUE)

s1.data <- sub.data %>% filter(session == 0)
s1.data$sub <- as.factor(s1.data$sub)
# because we lost some session 1 DTI data in the great back up miss of 2014, I am going to work out who does not have session 1 data, and I’ll add their session 2 data to this dataframe
missed.subs <- unique(sub.data$sub)[!(unique(sub.data$sub) %in% unique(s1.data$sub))]
s1.data <- rbind(s1.data, sub.data[sub.data$sub %in% missed.subs, ])

summary <- sub.data %>% group_by(group) %>%
  summarise(N=length(unique(sub)))

#N: int [1:2] 49, 47

sub.data %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))
#  sub group session tract_start tract_end FA tract_names
#1   0     0       0           0         0  0           0

#No NA FA values - perhaps its under something else? I chose this from one of the links you sent through
# the Sebastian Sauer one :)

which(sub.data$FA == 0.0000, arr.ind=TRUE)?
# [1]  91 355 799 800 912

#This shows me the rows that have 0.0000 as an FA value - given I couldn't see anything for NA I thought 
#perhaps it could be 0.000 - it was I don't think you can have a 0.000 FA value surely? For this I was 
# having a bit of trouble when I was entering the '0.000' it kept giving me error messages so I had a google
# found this and it seemed to work - although I havent split it into groups I can see that theres 3 practice
# and 7 control.
