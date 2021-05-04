##### Chapter 3 Navigating the Software
save.image() # save codes and memory with without quitting R
#
history(100) # view the history
#
func(titanic$age)
aavg <- .Last.value # to save the most recent value in the Expression
#
search() # with no args, and present the available-using pkgs
#
?titanic # to know more about a dataset
#
data() # to call all the dataset 
data(fifa, package = "DALEX") # the full format for calling a sepcific data
data(package = "DALEX") # check available but not all data sets in this pkg
#
library() # list all the installed pkgs
#
head(installed.packages())
installed.packages()[, c("Package", "Version")] # check the installed pkgs
#
chooseCRANmirror() # call to choose a CRAN mirrors
#
## set a permnent CRAN for R
options("repos")[[1][1]] # get the url of the CRAN choosen
options(repos = "URL") # set it 
#
$ R --quiet # to hide the start up message
#
source("1.R") # read R text and exectue its contents
source("1.R", echo = TRUE) # also show the codes in the content
#
Sys.getenv() 
# 
?options
#
pkgs <- getOption("defaultPackages")
pkgs <- c(pkgs, "ggplot2")
options(defaultPackages = pkgs) # add ggplot2 to system defult pkgs aviod load it again








