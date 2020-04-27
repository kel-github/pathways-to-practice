# What is Github and why use it?

We are going to develop our analysis code using [github](https://github.com/). Github is a repository for code. You can think of it as an excellent storage facility for all the code you write. It is excellent because:

1. It allows you to keep track of your code changes using version control, which is basically like time travel. Instead of having many files called things like 'analysis_v1', 'analysis_v2', 'analysis_final', 'analysis_final_final' you have only 1 file. Each time you change your file, github tracks the changes you made to your file (given you give Github a few simple commands). Thus you keep only the latest version of the file. But, if you ever make an error and break your code, you can go back in time and retrieve the previous version from Github!
2. It allows you to share your code really easily. Anyone interested in your work can download your code and reproduce your analysis. Which is very important for open science practices. 
3. It makes it easy to collaborate between multiple coders, as everyone can access the online repository.

# How does it work?

Using Github involves essentially 2 processes. You will install Git on your machine, and use it to track your changes locally. You will also 'push' changes to the online repository on the Github website, thus the online repository is essentially an online copy of your local folder. The online repository differs in that it can also include files you don't have locally, such as those pushed online by other contributors.

# Github exercise 1 - setting up and cloning the project repository

## setting up
1. Create a [Github](https://github.com/) account
2. [install git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) on your machine 

## clone the project repository
1. In your Github account (on the Github website), navigate to [https://github.com/kel-github/pathways-to-practice](https://github.com/kel-github/pathways-to-practice)
2. Hit the 'Fork' button in the right hand corner, select your username. This essentially moves a copy of the repository to your online github account.
3. Open a new terminal window on your own machine. Navigate to the folder where you would like to store the repository by using the command:

```
cd C:/pathfile/to-where-you-want-to-go
```

or if you are on mac:

```
cd ~/Documents/etc

```

4. Having navigated to the folder, enter the command: 

```
git clone https://github.com/kel-github/pathways-to-practice.git

```
You should now see a message telling you that the repository is downloading to your own machine.
