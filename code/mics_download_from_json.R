list.of.packages <- c("jsonlite","data.table","readr","foreign","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}

wd = paste0(prefix,"/git/p20_private_data")
setwd(wd)

basename.url=function(path){
  path_sep=strsplit(path,split="/")[[1]]
  path_len=length(path_sep)
  return(path_sep[path_len])
}
mics_dat = fromJSON("project_data/mics.json",flatten=T)
mics_dat = subset(mics_dat,dataset.url!="")
urls = mics_dat$dataset.url
nodata=c("https://mics-surveys-prod.s3.amazonaws.com/MICS3/Europe%20and%20Central%20Asia/Macedonia%2C%20The%20Former%20Yugoslav%20Republic%20of/2005-2006/Datasets/Macedonia%202005-06%20MICS_Datasets.zip")

urls= urls[which(!urls %in% nodata)]
# urls = urls[c(184:length(urls))]
uniquesavs=c()
for(url in urls){
  if(exists("ch")){rm(ch)}
  if(exists("hh")){rm(hh)}
  if(exists("hl")){rm(hl)}
  if(exists("wm")){rm(wm)}
  if(exists("uncaptured_list")){rm(uncaptured_list)}
  filename = gsub("%20","_",basename.url(url))
  uniquename = substr(filename,1,nchar(filename)-4)
  message(uniquename)
  tmp = tempfile()
  download.file(url,tmp)
  zip.contents = unzip(tmp,exdir="large.data")
  
  file.remove(tmp)
  
  if("zip" %in% str_sub(zip.contents,-3)){
    message("multiple zips")
     zip.contents=unzip(zip.contents[which(str_sub(zip.contents,-3)=="zip")],exdir="large.data")
  }else{
  zip.contents = zip.contents[which(str_sub(zip.contents,-3)=="sav")]
  }
  all.sav = zip.contents[which(grepl("(.*)sav",tolower(basename(zip.contents))))]
  # uniquesavs=unique(c(uniquesavs,all.sav))
  ch.sav = zip.contents[which(grepl("ch(.*)sav",tolower(basename(zip.contents))))]
  ch.sav2 = zip.contents[which(grepl("under5(.*)sav",tolower(basename(zip.contents))))]
  ch.sav3 = zip.contents[which(grepl("underfive(.*)sav",tolower(basename(zip.contents))))]
  ch.sav=c(ch.sav,ch.sav2,ch.sav3)
  hh.sav = zip.contents[which(grepl("hh(.*)sav",tolower(basename(zip.contents))))]
  hl.sav = zip.contents[which(grepl("hl(.*)sav",tolower(basename(zip.contents))))]
  wm.sav = zip.contents[which(grepl("wm(.*)sav",tolower(basename(zip.contents))))]
  wm.sav2 = zip.contents[which(grepl("woman(.*)sav",tolower(basename(zip.contents))))]
  wm.sav=c(wm.sav,wm.sav2)
  if(length(ch.sav)>0){
    ch = read.spss(ch.sav,use.value.labels=F)
    ch.labs = data.frame(var.name=names(ch),var.lab=attributes(ch)$variable.labels)
    ch$filename = uniquename
  }else{
    ch = NULL
    ch.labs = NULL
  }
  if(length(hh.sav)>0){
    hh = read.spss(hh.sav,use.value.labels=F)
    hh.labs = data.frame(var.name=names(hh),var.lab=attributes(hh)$variable.labels)
    hh$filename = uniquename
  }else{
    hh = NULL
    hh.labs = NULL
  }
  if(length(hl.sav)>0){
    hl = read.spss(hl.sav,use.value.labels=F)
    hl.labs = data.frame(var.name=names(hl),var.lab=attributes(hl)$variable.labels)
    hl$filename = uniquename
  }else{
    hl = NULL
    hl.labs = NULL
  }
  if(length(wm.sav)>0){
    wm = read.spss(wm.sav,use.value.labels=F)
    if(length(attributes(wm)$variable.labels)>0){
    wm.labs = data.frame(var.name=names(wm),var.lab=attributes(wm)$variable.labels)
    }else{
      wm.labs=data.frame(var.name=NA,var.lab=NA)
    }
    wm$filename = uniquename
  }else{
    wm = NULL
    wm.labs = NULL
  }
  uncaptured=all.sav[which(!all.sav %in% c(
      ch.sav
      ,hh.sav
      ,hl.sav
      ,wm.sav
    ))]
  uncaptured_list=list()
  if(length(uncaptured)>0){
    for(uncap in uncaptured){
      data.tmp= read.spss(uncap,use.value.labels=F)
      uncap.labs = data.frame(var.name=names(data.tmp),var.lab=attributes(data.tmp)$variable.labels)
      data.tmp$filename = uniquename
      uncap.list=list("data"=data.tmp,"labs"=uncap.labs)
      uncaptured_list[[basename(uncap)]]=uncap.list
    }
  }
  rdataname = paste0("project_data/MICS auto/",uniquename,".RData")

  save(ch,ch.labs,hh,hh.labs,hl,hl.labs,wm,wm.labs,uncaptured_list,file=rdataname)
}

# ch.sav2 = uniquesavs[which(grepl("ch(.*)sav",tolower(uniquesavs)))]
# ch.sav = uniquesavs[which(grepl("under5(.*)sav",tolower(uniquesavs)))]
# ch.sav3 = uniquesavs[which(grepl("underfive(.*)sav",tolower(uniquesavs)))]
# hh.sav = uniquesavs[which(grepl("hh(.*)sav",tolower(uniquesavs)))]
# hl.sav = uniquesavs[which(grepl("hl(.*)sav",tolower(uniquesavs)))]
# wm.sav = uniquesavs[which(grepl("wm(.*)sav",tolower(uniquesavs)))]
# wm.sav2 = uniquesavs[which(grepl("woman(.*)sav",tolower(uniquesavs)))]
# uncaptured=uniquesavs[which(!uniquesavs %in% c(
#   ch.sav2 
#   ,ch.sav 
#   ,hh.sav 
#   ,hl.sav 
#   ,wm.sav 
#   ,wm.sav2
# ))]

#accessing non-standard data frames 
#load("E:/git/p20_private_data/project_data/MICS auto/Benin_MICS5_Datasets.RData")
#bh.dat=data.frame(uncaptured_list[["bh.sav"]][[1]])
#bh.labs=data.frame(uncaptured_list[["bh.sav"]][[2]])