# Written by K. Garner and Georgia Marsh, 2020
# Exploratory Factor Analysis, ran after s1data_code script and Correlations script:

# Optimal coordinates:
ev <- eigen(cor.mat) # get the eigenvalues using the correlation matrix
ap <- parallel(subject=nrow(mhl.mat), var=ncol(mhl.mat), rep=1000, quantile=.05, model="factors") # run the parallel analysis
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea, criteria=0) # calculate the 95% values for the parallel analysis, plus the accelaration factor and the optimal coordinates
plotnScree(nS, xlab="Eigenvalues")
# Scree plot indicates that a two factor solution may be the most optimal

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

# After running all solutions, we identified that the two factor solution was the most optimal for our data.
# Therefore, as the lTHA_rSOG tract did not load strongly onto either factor within this solution, we opted to re-run
# the code to develop a two factor solution without the lTHA_rSOG tract included (see EFAReRun script).
