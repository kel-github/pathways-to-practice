# define functions

narrow.tracts <- function(fpath){
  # this function identifies for which of the tracts we have > 80% datapoints
  # it then removes the tracts used in the EFA analysis
  # the remaining are kept as an index for permuting a null from unused tracts
  sub_fols <- c( 'group-practice', 'group-control' )
  fnames <- lapply(sub_fols, function(x) list.files(paste(fpath, x, sep=""), pattern="FA") )
  fnames <- c( do.call(cbind, fnames) )
  
  load.data <- function(f){
    sub.num.full <- strtoi(strsplit(f, "_")[[1]][2])
    if (sub.num.full < 2000) {
      fol = "group-practice" 
    } else {
      fol = "group-control"  
    }
    tmp.dat <- read.table(paste(fpath, fol, '/', f, sep="") ,header=FALSE)
    out <- (tmp.dat > 0)*1
    out
  }
  null <- matrix(0, nrow=116, ncol=116)
  for (i in fnames) null = null + load.data(i)
  idx  <-  (null / length(fnames)) > .9 
  tract.names <- as.vector(read.csv(paste(fpath, 'aal_labels.txt', sep=''), header=F))
  used.tracts <- list(c("Superior_Frontal_gyrus_dorsolateral_Left",   "Caudate_nucleus_Left"), 
                      c("Inferior_frontal_gyrus_opercular_part_Left", "Caudate_nucleus_Left"),
                      c("Supplementary_motor_area_Left",              "Caudate_nucleus_Left"),
                      c("Inferior_Parietal_gyrus_Left",               "Caudate_nucleus_Left"),
                      c("Superior_Frontal_gyrus_dorsolateral_Left",   "Lenticular_nucleus_putamen_Left"), 
                      c("Inferior_frontal_gyrus_opercular_part_Left", "Lenticular_nucleus_putamen_Left"),
                      c("Supplementary_motor_area_Left",              "Lenticular_nucleus_putamen_Left"),
                      c("Inferior_Parietal_gyrus_Left",               "Lenticular_nucleus_putamen_Left"),               
                      c("Superior_Frontal_gyrus_dorsolateral_Right",   "Caudate_nucleus_Right"), 
                      c("Inferior_frontal_gyrus_opercular_part_Right", "Caudate_nucleus_Right"),
                      c("Supplementary_motor_area_Right",              "Caudate_nucleus_Right"),
                      c("Inferior_Parietal_gyrus_Right",               "Caudate_nucleus_Right"),
                      c("Superior_Frontal_gyrus_dorsolateral_Right",   "Lenticular_nucleus_putamen_Right"), 
                      c("Inferior_frontal_gyrus_opercular_part_Right", "Lenticular_nucleus_putamen_Right"),
                      c("Supplementary_motor_area_Right",              "Lenticular_nucleus_putamen_Right"),
                      c("Inferior_Parietal_gyrus_Right",               "Lenticular_nucleus_putamen_Right"))
  usd.idxs <- lapply(used.tracts, function(x) c(which(tract.names==x[1]), which(tract.names==x[2])))
  for (i in 1:length(usd.idxs)) idx[usd.idxs[[i]][1],usd.idxs[[i]][2]] <- FALSE
  for (i in 1:length(usd.idxs)) idx[usd.idxs[[i]][2],usd.idxs[[i]][1]] <- FALSE
  diag(idx) <- FALSE
  idx[lower.tri(idx)] <- FALSE
  idx # now I can take all the TRUES of idx and use them to randomly select tracts in the next step
}


select.tracts <- function(N, idx){
  # this function will select N tracts, and shape the subsequent data into a dataframe for
  # use by the EFA functionality
  fpath <- '~/Dropbox/QBI/pathways-to-practice/dti-data/' # filepath to where the data lives
  these.tracts <- sample(which(idx==TRUE), size=N, replace=FALSE)

  # now load subject data from the tracts selected for this permutation
  sub_fols <- c( 'group-practice', 'group-control' )
  fnames <- lapply(sub_fols, function(x) list.files(paste(fpath, x, sep=""), pattern="FA") )
  fnames <- c( do.call(cbind, fnames) )
  
  GetSubData <- function(f, tracts){
    sub.num.full <- strtoi(strsplit(f, "_")[[1]][2])
    if (sub.num.full < 2000) {
      group = 1 
    } else {
      group = 2  
    }
    sub.num <- round(sub.num.full*.1)
    pre.post <- 1-(sub.num.full %% 2)
    tmp.dat <- as.matrix(read.table(paste(fpath, sub_fols[group], '/', f, sep="") ,header=FALSE), nrow=116)[tracts]
    labels <- 1:N
    sub.data <- data.frame(sub = rep(sub.num, each=N),
                           group = rep(group, each=N),
                           session = rep(pre.post, each=N),
                           tract = tracts,
                           FA = tmp.dat)
    sub.data
  }
  
  sub.data <- lapply(fnames, GetSubData, tracts = these.tracts)
  sub.data <- do.call(rbind, sub.data)
  
  # 2. get the data from the 1st session and label the factors
  # --------------------------------------------------------------------------------
  sub.data <- sub.data %>% distinct(sub, tract, .keep_all = TRUE) # just removing some duplicate records
  sub.data$sub <- as.factor(sub.data$sub) 
  # get s1 data
  s1.data <- sub.data %>% filter(session == 0)
  s1.data$sub <- as.factor(s1.data$sub)
  # because we lost some session 1 DTI data in the great back up miss of 2014, I am going to work out who does not have session 1 data, and I'll add their session 2 data to this dataframe
  missed.subs <- unique(sub.data$sub)[!(unique(sub.data$sub) %in% unique(s1.data$sub))]
  s1.data <- rbind(s1.data, sub.data[sub.data$sub %in% missed.subs, ])
  # now I can proceed to label the rest of the factors
  s1.data$group <- as.factor(s1.data$group)
  levels(s1.data$group) <- c("practice", "control")
  s1.data$session <- as.factor(s1.data$session)
  levels(s1.data$session) <- c("pre", "post")
  s1.data$tract_name <- as.factor(s1.data$tract)
  
  # 3. remove known outliers
  # --------------------------------------------------------------------------------
  # as I know tract values should be > 0, I am removing participants who score 0 on a tract
  s1.data <- s1.data %>% filter(FA > 0.01) 
  # now, return the data
  s1.data
}

run.EFA <- function(data){
  # run the EFA on data, return factor loadings
  dat.wide <- data %>% select(-c(group, session)) %>%
              pivot_wider(id_cols=sub, names_from=tract_name, values_from=FA) %>%
              drop_na()
  dat.mat <- as.matrix(dat.wide[,2:length(colnames(dat.wide))])
  efa <- factanal(dat.mat, factors=2, rotations='promax')
  nu.data <- factor.scores(dat.mat, efa$loadings, method='tenBerge')
  reg.dat <- data.frame(sub = dat.wide$sub,
                        factA = nu.data$scores[,'Factor1'],
                        factB = nu.data$scores[,'Factor2'])
  reg.dat
}


do.regression <- function(reg.dat){
  # this function joins the factor scores to the behavioural data, runs the regression model and returns the results
  CV.dat <- read.csv("cleaned-data/CV-all-subs.csv")
  CV.dat$group <- NA
  CV.dat$group[CV.dat$sub < 200] = "practice"
  CV.dat$group[CV.dat$sub > 199] = "control"
  CV.dat$group <- factor(CV.dat$group, levels=c("practice", "control"))
  CV.dat$cog_cond <- c("s", "FM", "SM")
  CV.dat$cog_cond <- factor(CV.dat$cog_cond, levels=c("s", "FM", "SM"))
  CV.dat <- CV.dat %>% select(-mult_cond)
  CV.dat$sub <- as.factor(CV.dat$sub)
  CV.dat$sess <- factor(CV.dat$sess, levels=c("Pre", "Post"))
  
  reg.dat <- inner_join(reg.dat, CV.dat, by=c('sub')) %>%
    unique() %>% na.omit
  reg.dat$sub <- as.factor(reg.dat$sub)
  
  CV.mds <- lapply(unique(reg.dat$cog_cond), function(x) summary(glm(CV ~ factA*factB*group*sess+RT, data=reg.dat[reg.dat$cog_cond == x,])))
  
  results <- do.call(rbind, lapply(CV.mds, function(x) data.frame(term=names(x$coefficients[,1]),
                                                                  coef=x$coefficients[,1],
                                                                  p=x$coefficients[,4])))
  results$cog_cond <- rep(unique(reg.dat$cog_cond), each=length(unique(results$term)))
  results
}