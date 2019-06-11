####Function and setup####
list.of.packages <- c("Hmisc","plyr","foreign","data.table","varhandle","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

#Taken from https://raw.githubusercontent.com/akmiller01/alexm-util/master/DevInit/R/P20/2013_tab_data2.R
# wd <- "E:/DHSauto"
wd<- "~/git/p20_indicator_time_trends"
setwd(wd)

source("code/child_mort.R")
povcalcuts <- fread("https://raw.githubusercontent.com/ZChristensen/poverty_trends/master/data/P20incometrends.csv")
dhsmeta<- fread("https://raw.githubusercontent.com/ZChristensen/p20_indicator_time_trends/master/data/dhs_meta_data20190524.csv")
dhsmeta<- subset(dhsmeta, Recode.Structure.!="DHS-I")


dhsmeta$RequestYear=NA
povcalyears=c(1981,1984,1987,1990,1993,1996,1999,2002,2005,2008,2010,2011,2012,2013,2015)
for(i in povcalyears){
  dhsmeta$RequestYear[which(dhsmeta$surveyyr>=i)]=i
  i=i+1
}


dhsmeta$Country.[which(dhsmeta$Country.=="Cape Verde")]<-"Cabo Verde"
dhsmeta$Country.[which(dhsmeta$Country.=="Congo")]<-"Congo, Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Congo Democratic Republic")]<-"Congo, Democratic Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Egypt")]<-"Egypt, Arab Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Gambia")]<-"Gambia, The"
dhsmeta$Country.[which(dhsmeta$Country.=="Yemen")]<-"Yemen, Republic of"
#Afghanistan, Cambodia, Equatorial Guinea and Eritrea have had DHS surveys but don't have PovcalNet data
names(dhsmeta)[which(names(dhsmeta)=="Country.")]="CountryName"

dhsmeta$filename=paste0(dhsmeta$dhs_cc,"HR",dhsmeta$dhs_recode_code,"DT")
dhsmeta=dhsmeta[which(!is.na(dhsmeta$dhs_cc)),]

keep=c("CountryName","RequestYear","filename")
dhsmeta2=dhsmeta[,keep,with=F]

povcalcuts <- join(dhsmeta2,povcalcuts,by=c("CountryName","RequestYear"))

names(povcalcuts)[which(names(povcalcuts)=="RequestYear")] <- "year"
names(povcalcuts)[which(names(povcalcuts)=="CountryCode")] <- "iso3"
povcalcuts$hc<- povcalcuts$P20Headcount/100
povcalcuts$extreme <- povcalcuts$ExtPovHC/100
keep <- c("iso3","year","hc","PovGap","filename","extreme")
povcalcuts <- povcalcuts[,keep, with=F]
# povcalcuts$filename <- NA
# povcalcuts$filename[which(povcalcuts$iso3=="NPL")]<-"NPIR7HFL"
# povcalcuts$filename[which(povcalcuts$iso3=="BEN")]<-"BJIR61FL"

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

psum <- function(...,na.rm=TRUE) { 
  rowSums(do.call(cbind,list(...)),na.rm=na.rm)
} 

####Run function####
# set our working directory, change this if using on another machine
wd <- "~/git/p20_private_data/project_data/"
setwd(wd)

rdatas <- list.files(paste0(wd,"DHS auto"),pattern="*.RData",ignore.case=T,full.names=TRUE)


dataList <- list()
dataIndex <- 1

# Loop through every dir
for(i in 1:length(rdatas)){
  rdata <- rdatas[i]
  # Pull some coded info out of the dir name
  country <- substr(basename(rdata),1,2)
  recode <- substr(basename(rdata),3,4)
  phase <- substr(basename(rdata),5,6)
  subphase <- substr(basename(rdata),5,5)
  povcal_filename <- paste0(country,recode,phase,"dt")
  if(povcal_filename %in% tolower(povcalcuts$filename)){
    message(povcal_filename)
    
    br_patha <- paste0(country,"br",phase)
    br_path <- paste0("DHS auto/",tolower(br_patha),"fl.RData")
    load(br_path)
    br <- data.frame(data)
    remove(data)
    
    pr_patha <- paste0(country,"pr",phase)
    pr_path <- paste0("DHS auto/",tolower(pr_patha),"fl.RData")
    load(pr_path)
    pr <- data.frame(data)
    remove(data)
    
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    names(pr)[which(names(pr)=="hvidx")] <- "line"
    #Rename sample.weights var
    names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
    pr$weights <- pr$sample.weights/1000000
    
    #Urban/rural
    if(phase>1){
      names(pr)[which(names(pr)=="hv025")] <- "urban.rural"
    }else{
      names(pr)[which(names(pr)=="v102")] <- "urban.rural"
    }
    pr$urban <- NA
    pr$urban[which(pr$urban.rural==1)] <- 1
    pr$urban[which(pr$urban.rural==2)] <- 0
    
    # Wealth
    if("hv271" %in% names(pr)){
      pr$hv271 <- pr$hv271/100000
      names(pr)[which(names(pr)=="hv271")] <- "wealth"
    }else{
      wi_patha <- paste0(country,"wi",phase)
      wi_path <- paste0("DHS auto/",tolower(wi_patha),"fl.RData")
      if(file.exists(wi_path)){
        load(wi_path)
        wi <- data.frame(data)
        remove(data)
      }else{
        next;
      }
      names(wi)[which(names(wi)=="whhid")] <-"hhid"
      pr<- join(pr,wi,by="hhid")
      names(pr)[which(names(pr)=="wlthindf")] <-"wealth"
    }
    
    # Poverty
    filename=paste0(country,recode,phase,"dt")
    povcalcuts$filename=tolower(povcalcuts$filename)
    povcalcut <- subset(povcalcuts,filename==povcal_filename)$hc
    extcut <- subset(povcalcuts,filename==povcal_filename)$extreme
    cuts <- c(povcalcut,extcut)
    povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)
    
    pr$p20 <- (pr$wealth < povperc[1])
    pr$ext <- (pr$wealth < povperc[2])
    
    # Education
    if(phase>1){
      names(pr)[which(names(pr)=="hv109")] <- "educ"
    recode.educ <- function(x){
      if(is.na(x)){return(NA)}
      else if(x==8 | x==9){return(NA)}
      else if(x==0 | x==1){return("No education, preschool")}
      else if(x==2 | x==3 ){return("Primary")}
      else if(x==4){return("Secondary")}
      else if(x==5){return("Higher")}
      else{return(NA)}
    }
    pr$educ <- sapply(pr$educ,recode.educ)
    } else{
      names(pr)[which(names(pr)=="v106")] <- "educ"
      recode.educ <- function(x){
        if(is.na(x)){return(NA)}
        else if(x==8 | x==9){return(NA)}
        else if(x==0 ){return("No education, preschool")}
        else if(x==1){return("Primary")}
        else if(x==2){return("Secondary")}
        else if(x==3){return("Higher")}
        else{return(NA)}
      } 
    }
    # Age
    names(pr)[which(names(pr)=="hv105")] <- "age"
    
    # Sex
    names(pr)[which(names(pr)=="hv104")] <- "sex"
    
    # ID vars
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    names(pr)[which(names(pr)=="hv024")] <- "region"
    names(pr)[which(names(pr)=="hvidx")] <- "line"
    names(pr)[which(names(pr)=="hv112")] <- "mother.line"
    pr$mother.line[which(pr$mother.line==99)] <- NA
    
    # Head vars
    names(pr)[which(names(pr)=="hv219")] <- "head.sex"
    names(pr)[which(names(pr)=="hv220")] <- "head.age"
    
    # Birth certificate
    names(pr)[which(names(pr)=="hv140")] <- "birth.cert"
    #0 - neither certificate or registered
    #1 - has certificate
    #2 - registered, no certificate
    #3 - registered, no certificate
    #6 - other
    #8 - dk
    
    # Stunting
    names(pr)[which(names(pr)=="hc70")] <- "child.height.age"
    if(typeof(pr$child.height.age)=="NULL"){
      pr$child.height.age <- NA
    }else{
      pr$child.height.age <- pr$child.height.age/100
    }
    pr$child.height.age[which(pr$child.height.age>80)] <- NA
    pr$stunting <- NA
    pr$stunting[which(pr$child.height.age > (-6) & pr$child.height.age<= (-3))] <- "Severely stunted"
    pr$stunting[which(pr$child.height.age > (-3) & pr$child.height.age<= (-2))] <- "Stunted, but not severely"
    pr$stunting[which(pr$child.height.age > (-2) & pr$child.height.age< (6))] <- "Not stunted"
    
    keep <- c(
      "wealth","weights","urban","region","educ","age","sex","cluster","household","head.sex","head.age","p20","ext","birth.cert","stunting"
    )
    prNames <- names(pr)
    namesDiff <- setdiff(keep,prNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        pr[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    data <- pr[,keep]
    data$filename <- povcal_filename
  }
}

data.total <- rbindlist(dataList)
save(data.total,file="~/git/p20_private_data/project_data/historical_dhs.RData")
