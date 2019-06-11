####Function and setup####
list.of.packages <- c("Hmisc","plyr","foreign","data.table","devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
####Run function####
# set our working directory, change this if using on another machine
wd <- "/home/alex/Survey Microdata/DHSauto"
setwd(wd)

output.dir = "/home/alex/git/p20_private_data/project_data/DHS auto/"

#Unzip
# message("Unzipping...")
# zips <- list.files(pattern="*.zip",ignore.case=T,full.names=T)
# pb = txtProgressBar(max=length(zips),style=3)
# for(i in 1:length(zips)){
#   setTxtProgressBar(pb,i)
#   zip <- zips[i]
#   unzip(zip)
# }
# close(pb)

too.big = c("iahr74fl","iahr74fl","iapr74fl","iabr74fl","iair74fl","iakr74fl")

message("Reading and resaving...")
files <- list.files(pattern="*.dta",ignore.case=T,full.names=T,recursive=T)
pb = txtProgressBar(max=length(files),style=3)
for(i in 1:length(files)){
  setTxtProgressBar(pb,i)
  file <- files[i]
  fileBase <- basename(file)
  fileName <- substr(fileBase,1,nchar(fileBase)-4)
  fileName <- tolower(fileName)
  output.filename = paste0(output.dir,"/",fileName,".RData")
  if(!file.exists(output.filename) & !(fileName %in% too.big)){
    data <- read.dta(file,convert.factors=F)
    save(data,file=output.filename)
    rm(data)
  }
}
close(pb)