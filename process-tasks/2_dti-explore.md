# DTI data - exploratory analysis

In this process, you will take some initial steps to coding your expoloratory analysis of the DTI data, and you will go through integrating your code changes to the online Github repository

## step 1. start coding

--To start, open YOURINTITIALS_dti-exploratory.R - this is the file in which you will be developing your code to explore the dti data


1. Read the code, if necessary, uncomment the lines to install packages, and run the code as far as line 18 (end of the 'load packages' section). To run a single line of R code, highlight it and hit cmd + Enter, or ctrl + Enter, dependent on whether you are on a Mac or a PC. Amend the variables in the 'load data; section so that the filepath for the data is correct, and the brain regions/tracks are correctly defined. Run the code in this section to get the 'sub.data' variable.

2. Check and tidy up the dataframe 'sub.data' - i.e. make sure that the relevant variables are labelled as factors. Recode the factors of the data frame with meaningful labels. I recommend adding a factor that contains a meaningful name for each tract.
   [chapter 15](https://r4ds.had.co.nz/factors.html) of Hadley Wickham's book will help you here as will the commands View() and head()
   
3. Pull out some basic frequencies. For example, how many subjects do we have session 1 data for? How many do we have session 1 and session 2 data for? 
   [this will help you as a start](https://rstudio-education.github.io/tidyverse-cookbook/transform-tables.html)

## step 2. integrate changes to Github

--Now you have made some changes, you need to add them to your local Github repository, push them to your online repository, and then request to merge them with mine. 

1. Using terminal, navigate to your repository folder using the cd command

2. Add your changes and push to your online repository using the following commands:

```
git add nameOfYourFile.R 
git commit -m "a useful message - e.g. first DTI explore"
git push

```

3. Now go to your online repository on github. You are now going to make the request to merge your changes with my repository. To the right of the brain menu (the tab saying Branch:master or something similar), click 'New pull request'. 

4. On the Compare page, click 'compare across forks'. The 'base fork' should be the project repository, and the 'head fork' should be yours.

5. Type a title and a description for your pull request.

6. Click 'Create Pull Request'

**Important Note**

Please wait for me to approve the pull request before moving on to the next steps. This applies to all the times that we will update code and integrate with the online repositories in this way. I may request you make chnages to your code before integrating, and I need to approve each step in order, once it is correctly coded, to avoid crazy Github headaches :)


