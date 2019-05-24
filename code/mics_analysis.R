list.of.packages <- c("data.table","readr","plyr")
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

povcalcuts=read.csv("project_data/micsPovcal.csv")
dir=paste0(wd,"/project_data/MICS auto")
rdatas <- list.files(path=dir,pattern="*.RData",ignore.case=T,recursive=T,full.names=TRUE)
rdatas.split=strsplit(rdatas,"/")
rdatafolders=sapply(rdatas.split,`[`,index=6)

subrdatas=rdatas[which(rdatafolders %in% paste0(povcalcuts$filename,".RData"))]


weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}


data.list = list()
#Indonesia 2000 doesn't have 
nowealth=c("E:/git/p20_private_data/project_data/MICS auto/Botswana_2000_MICS_Datasets.RData"
          ,"E:/git/p20_private_data/project_data/MICS auto/Guinea_Bissau_MICS4_Datasets.RData"
          ,"E:/git/p20_private_data/project_data/MICS auto/Indonesia_MICS2_2000_Datasets.RData"
          ,"E:/git/p20_private_data/project_data/MICS auto/Iraq_2000_MICS_Datasets.RData"
          ,"E:/git/p20_private_data/project_data/MICS auto/Kenya_2000_MICS_Datasets.RData"
          ,"E:/git/p20_private_data/project_data/MICS auto/Lesotho_2000_MICS_Datasets.RData"
          ,"E:/git/p20_private_data/project_data/MICS auto/Suriname_2000_MICS_Datasets.RData"
          ,"E:/git/p20_private_data/project_data/MICS auto/Zambia_1999_MICS_Datasets.RData"
          )
subrdatas=subrdatas[which(!subrdatas %in% nowealth)]
for(subrdata in subrdatas){
  povcal_filenam=basename(subrdata)
  povcal_filename=substr(povcal_filenam,1,nchar(povcal_filenam)-6)
  message(povcal_filename)
  load(subrdata)
  hh=data.frame(hh)
  hl=data.frame(hl)
  if(nrow(hh)==0){
    hh=hl
  }else{
    hh=hh
  }
  #Rename cluster/hh var
  names(hh)[which(toupper(names(hh)) %in% c("HI1","HH1","FH",""))] <- "cluster"
  names(hh)[which(toupper(names(hh)) %in% c("HI2","HH2","AH"))] <- "household"
  hh$hhid=NA
  if(povcal_filename=="Botswana_2000_MICS_Datasets"){
    hh$hhid=paste(hh$DISTRICT,hh$DWELLNO,hh$HHNUM,hh$DAYINT,hh$WEIGHT)
    ch$hhid=NA
    hh$HH7=hh$DISTRICT
    ch$hhid=paste(ch$DISTRICT,ch$DWELLNO,ch$HHNUM,ch$DAYINT,ch$WEIGHT)
    hl$hhid=NA
    hl$hhid=paste(hl$DISTRICT,hl$DWELLNO,hl$HHNUM,hl$DAYINT,hl$WEIGHT)
    hl$HL1=hl$P01
    wm$hhid=NA
    wm$hhid=paste(wm$DISTRICT,wm$DWELLNO,wm$HHNUM,wm$DAYINT,wm$WEIGHT)
    }else{
    hh$hhid=paste(hh$cluster,hh$household)
  }
  names(hl)[which(toupper(names(hl)) %in% c("HI1","HH1","FH"))] <- "cluster"
  names(hl)[which(toupper(names(hl)) %in% c("HI2","HH2","AH"))] <- "household"
  hl$hhid=NA
  if(povcal_filename=="Botswana_2000_MICS_Datasets"){
    hl$hhid=paste(hl$DISTRICT,hl$DWELLNO,hl$HHNUM,hl$DAYINT,hl$WEIGHT)
  }else if(povcal_filename=="Guinea_Bissau_2000_MICS_Datasets"){
    hl$hhid=NA
    hh$hhid=NA
    ch$hhid=NA
    wm$hhid=NA
    hl$hhid=hl$AFID
    hh$hhid=hh$AFID
    ch$hhid=ch$AFID
    wm$hhid=wm$AFID
  }else{
  hl$hhid=paste(hl$cluster,hl$household)
  }
  
  
  #Rename sample.weights var
  if(povcal_filename %in% c("Azerbaijan_2000_MICS_Datasets.RData")){
    wi=hl[,c("hhid","WLTHSCOR")]
    hh=join(hh,hl,by=c("hhid"))
  }
  if(exists("wi")){
    wi=as.data.frame(wi)
    names(wi)[which(toupper(names(wi)) %in% c("HI1","HH1","FH"))] <- "cluster"
    names(wi)[which(toupper(names(wi)) %in% c("HI2","HH2","AH"))] <- "household"
    wi$hhid=NA
    wi$hhid=paste(wi$cluster,wi$household)
    names(wi)=c("hhid","wealth")
    hh=join(hh,wi,by=c("hhid"))
  }
  names(hh)=tolower(names(hh))
  names(hh)[which(names(hh) %in% c("hhweight","weight"))] <- "sample.weights"
  
  names(hh)[which(tolower(names(hh))=="wscore")] <- "wealth"
  names(hh)[which(tolower(names(hh))=="wlthscor")] <- "wealth"
  if(sum(is.na(hh$wealth))==nrow(hh) | is.null(hh$wealth)){
    if(nrow(hh)==0) {
      
      message(paste("No hh data"))
      hh=hl
    } else{message("No wealth score")
      no.wealth=T
      hh$wealth=NA}
    
    
  }else {
    hh$wealth=hh$wealth
    no.wealth=F
  }
  if(sum(is.na(hh$sample.weights))==nrow(hh) | is.null(hh$sample.weights)){
    message("No weights score")
    
    
    
  }else{
    hh$sample.weights=hh$sample.weights
    
  }
  #Rename urban var
  urbanvar=tolower(hh.labs$var.name[which(hh.labs$var.lab %in% c(
    "Area"
    ,"Milieu"
    ,"Área"
    ,"Milieu de rÃ©sidence"
    ,"Zona"
    ,"reside"
    ,"VILLAGE CLASSIFICATION"))])
  names(hh)[which(tolower(names(hh))==urbanvar)] <- "urban.rural"
  hh$urban = NA
  hh$urban[which(hh$urban.rural==1)] =1
  hh$urban[which(hh$urban.rural==2)] =0
  names(hl)[which(toupper(names(hl))=="HL1")] <- "line"
  names(hl)[which(toupper(names(hl))=="HH7")] <- "region"
  #povcalcuts
povcalcut <- subset(povcalcuts,filename==povcal_filename)$P20Headcount
extcut <- subset(povcalcuts,filename==povcal_filename)$ExtPovHC
cuts <- c(povcalcut,extcut,.2)
if(no.wealth){
  hh$p20=NA
  hh$ext=NA
  hh$np20=NA
}else{
  povperc <- weighted.percentile(hh$wealth,hh$sample.weights,prob=cuts)
  hh$p20 <- (hh$wealth < povperc[1])
  hh$ext <- (hh$wealth < povperc[2])
  hh$np20<- (hh$wealth < povperc[3])
}


#weights and wealth
wi=hh[,c("hhid","p20","ext","np20","wealth","sample.weights","urban")]
pr=join(hl,wi,by="hhid")



#Rename age
names(pr)[which(tolower(names(pr))=="hl6")] <- "age"


#Region
if(sum(is.na(pr$p20))==length(pr$p20)){
  pr$p20=0
  pr$ext=0
  message("No PovcalNet data")
}
age=data.table(pr)[,.(
  P20HC=weighted.mean(p20, sample.weights, na.rm=TRUE)
  ,ExtremeHC=weighted.mean(ext, sample.weights, na.rm=TRUE)
  ,NP20HC=weighted.mean(np20, sample.weights, na.rm=TRUE)
  ,weights=sum(sample.weights, na.rm=T)
),by=c("age")]

age$filename=povcal_filename
data.list[[povcal_filename]] = age
rm(wi,pr,wm.labs,wm,ch,ch.labs,hh,hh.labs,hl,hl.labs)
}
dat=do.call(rbind,data.list)
save(dat,file="project_data/P20HCByAgeAllMICS.RData")
