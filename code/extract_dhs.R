####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(devtools)
####Run function####
# set our working directory, change this if using on another machine
# wd <- "C:/Users/Alex/Documents/Data/P20/DHS"
wd <- "C:/Users/Alex/Desktop/data/DHSauto/"
setwd(wd)

#Unzip
# zips <- list.files(pattern="*.zip")
# 
# for(i in 1:length(zips)){
#   zip <- zips[i]
#   unzip(zip)
# }

dirs <- list.dirs(wd)

for(dir in dirs){
  if (dir==wd){
    #skip
  }else{
    message(dir)
    # List out all the .dta in our wd, this is where our data is contained
    files <- list.files(dir,"*.dta",ignore.case=TRUE,full.names=TRUE)
    
    for(j in 1:length(files)){
      file <- files[j]
      fileBase <- basename(file)
      fileName <- substr(fileBase,1,nchar(fileBase)-4)
      fileName <- tolower(fileName)
      rdatas <- list.files(dir,"*.RData",ignore.case=TRUE)
      #Comment this out to avoid re-writing
      #       rdatas <- c()
      if(!(paste0(fileName,".RData") %in% rdatas)){
        data <- read.dta(file,convert.factors=FALSE)
        save(data,file=paste0(dir,"/",fileName,".RData"))  
      }
    }
  }
}

# List out all the .dta in our wd, this is where our data is contained
# files <- list.files(wd,"*.dta",ignore.case=TRUE,full.names=TRUE)
# 
# for(j in 1:length(files)){
#   file <- files[j]
#   fileBase <- basename(file)
#   fileName <- substr(fileBase,1,nchar(fileBase)-4)
#   fileName <- tolower(fileName)
#   rdatas <- list.files(wd,"*.RData",ignore.case=TRUE)
#   #Comment this out to avoid re-writing
# #       rdatas <- c()
#   if(!(paste0(fileName,".RData") %in% rdatas)){
#     data <- read.dta(file,convert.factors=FALSE)
#     save(data,file=paste0(wd,"/",fileName,".RData"))  
#   }
# }