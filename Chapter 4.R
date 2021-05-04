# Chapter 4 Inputs and outputs
scores <- data.frame()
scores <- edit(scores)
####
option(digits = 3) # set digits to change the defualt for digits
####
t
cat("The code here is", a, "\n", file = "1.R") # to save a in the "1.R"
cat(a, file = "1.R", append = TRUE) # the first arg must be list or a vector
#
con <- file("1.R", "w") # connection
cat(b, file = con)
cat(a, file = con) # we need not append, for the "w" has implied this, without "w"
#   the later result will cover the previous result
close(con)
####
sink("2.R") # redirect output to file, will open a blank page automatically
source("3.R", echo = TRUE)
a
t
sink() # resume writing output to R, it only save the output rather than the code
####
list.files() # list files show the contents of the working directory
list.files(recursive = TRUE) # to see all the subdirectories
list.files(all.files = TRUE) # see all the files
####
read.fwf()
read.table() # it will ignore any sentences begin with "#", therefore it is comment line
read.csv() # as.is = TRUE, not as string as factor, also support comment line
write.csv(t, file = "Titanic.csv", row.names = FALSE) # to keep data in csv format,
#           , but it is inflexiable, if we want change something, we can use write table
write.table(t, file = "Titanic_2.csv", row.names = FALSE) # 
###
require("xml2")
require("httr")
url <- 'http://en.wikipedia.org/wiki/World_population'
tbl2 <- htmlParse(rawToChar(GET(url)$content))
rawToChar(GET(url)$content)
tbl <- read_html(url, which = 3)

