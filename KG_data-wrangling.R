# Garner, KG. 2020
# This code contains the functions required to wrangle data into
# tidy form for the project 'pathways-to-practice'

# source files and load packages
# ------------------------------------------------------------------
library(tidyverse)

# define functions
# ------------------------------------------------------------------
GetDTIData <- function(fpath, tracts){
  # this function will pull out the FA measures from the participant 
  # level FA matrices
  # the function assumes that within the folder defined by fpath, the data
  # are in two subfolders called 'group-control' and 'group-practice'
  # The function also requires that the data files are named according to the
  # convention 'sub_xxxx_WB_Conn_FAMean.txt'
  # the labels file should also be in fpath
  
  # inputs
  # fpath - the file path to the folder that contains the DTI data
  # tracts - a list, each element of the list is a vector, the vector contains the 
  # names of the two regions that start and end the tract of interest
  
  results <- list()
  # first, read in the file names of the FA data, from where we want to pull data from
  sub_fols <- c( 'group-practice', 'group-control' )
  fnames <- lapply(sub_fols, function(x) list.files(paste(fpath, x, sep=""), pattern="FA") )
  fnames <- c( do.call(cbind, fnames) )
  
  # now, for each file, pull out the required data
  GetSubData <- function(f, tracts){
    sub.num.full <- strtoi(strsplit(f, "_")[[1]][2])
    if (sub.num.full < 2000) {
      group = 1 
    } else {
      group = 2  
    }
    sub.num <- round(sub.num.full*.1)
    pre.post <- 1-(sub.num.full %% 2)
    tmp.dat <- read.table(paste(fpath, sub_fols[group], '/', f, sep="") ,header=FALSE)
    labels <- read.table(paste(fpath, 'aal_labels.txt', sep=""), header = FALSE)
    tract.idxs <- lapply(tracts, function(x) match(x, t(labels)))
    FA.vals <- sapply(tract.idxs, function(x) tmp.dat[x[1],x[2]])
    N = length(FA.vals)
    sub.data <- data.frame(sub = rep(sub.num, each=N),
                           group = rep(group, each=N),
                           session = rep(pre.post, each=N),
                           tract_start = sapply(tracts, function(x) x[1]),
                           tract_end = sapply(tracts, function(x) x[2]),
                           FA = FA.vals)
    sub.data
  }
  
  sub.data <- lapply(fnames, GetSubData, tracts = tracts)
  sub.data <- do.call(rbind, sub.data)
}




 


