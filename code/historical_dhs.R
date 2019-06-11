####Function and setup####
list.of.packages <- c("Hmisc","plyr","foreign","data.table","varhandle","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

#Taken from https://raw.githubusercontent.com/akmiller01/alexm-util/master/DevInit/R/P20/2013_tab_data2.R
# wd <- "E:/DHSauto"
wd<- "~/git/p20_indicator_time_trends"
setwd(wd)

source("https://raw.githubusercontent.com/akmiller01/alexm-util/master/DevInit/R/P20/wealth_pca.R")
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
    # load(rdata)
    # hr <- data
    # remove(data)
    
    ###Maternal
    ir_patha <- paste0(country,"ir",phase)
    ir_path <- paste0("DHS auto/",tolower(ir_patha),"fl.RData")
    load(ir_path)
    ir <- data.frame(data)
    remove(data)
    
    names(ir)[which(names(ir)=="v001")] <- "cluster"
    names(ir)[which(names(ir)=="v002")] <- "household"
    names(ir)[which(names(ir)=="v003")] <- "line"
    names(ir)[which(names(ir)=="v201")] <- "ceb"
    ir$woman.weights <- ir$v005/1000000

    
    irKeep <- c(
      "cluster"
      ,"household"
      ,"line"
      ,"woman.weights"
    )
    irNames <- names(ir)
    namesDiff <- setdiff(irKeep,irNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        ir[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    ir <- ir[irKeep]
    
    
    pr_patha <- paste0(country,"pr",phase)
    pr_path <- paste0("DHS auto/",tolower(pr_patha),"fl.RData")
    load(pr_path)
    pr <- data.frame(data)
    remove(data)
    
    
    pr$water.time <- pr$hv204
    
  

    
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    names(pr)[which(names(pr)=="hvidx")] <- "line"
    #Rename sample.weights var
    names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
    pr$weights <- pr$sample.weights/1000000
    
    #Rename urban var
    if(phase>1){
      names(pr)[which(names(pr)=="hv025")] <- "urban.rural"
    }else{
      names(pr)[which(names(pr)=="v102")] <- "urban.rural"
    }
    pr$urban <- NA
    pr$urban[which(pr$urban.rural==1)] <- 1
    pr$urban[which(pr$urban.rural==2)] <- 0
    
    #Wealth
    if(any(names(pr)=="hv271")){
    pr$hv271 <- pr$hv271/100000
    names(pr)[which(names(pr)=="hv271")] <- "wealth"}
    if(!any(names(pr)=="wealth")){
      wi_patha <- paste0(country,"wi",phase)
      wi_path <- paste0("DHS auto/",tolower(wi_patha),"fl.RData")
      load(wi_path)
      wi <- data.frame(data)
      remove(data)
      
      names(wi)[which(names(wi)=="whhid")] <-"hhid"
      pr<- join(pr,wi,by="hhid")
      names(pr)[which(names(pr)=="wlthindf")] <-"wealth"
    }
    #   #Make a wealth index
    #   
    #   
    #   catvars = c(
    #     "hv201" #Drinking water
    #     ,"hv202" #Other water
    #     ,"hv205" #Toilet type
    #     ,"hv206" #Has electricity
    #     ,"hv207" #Has radio
    #     ,"hv213" #Floor
    #     ,"hv215" #Roof
    #     ,"hv214" #Wall
    #     ,"hv208" #TV
    #     ,"hv209" #Refrigerator
    #     ,"hv210" #Bicycle
    #     ,"hv211" #motorcycle
    #     ,"hv212" #car
    #   )
    #   
    #   numvars = c(
    #     "hv204" #Time to fetch water
    #     ,"hv216" #sleeping rooms
    #   )
    #   
    #   urbanvar = "urban"
    #   pr <- data.frame(pr)
    #   
    #   pr$missingCount = rowSums(is.na(pr[c(catvars,numvars)]))
    #   pr.wealth <- pr
    #   # hh.wealth = subset(hh,missingCount<length(c(catvars,numvars)))
    #   
    #   pr.wealth <- wealth(pr.wealth,catvars,numvars,urbanvar)
    #   keep <- c("cluster","household","wealth")
    #   pr.wealth <- pr.wealth[keep]
    #   pr.wealth$wealth <- pr.wealth$wealth*-1
    #   pr <- merge(pr,pr.wealth,all.x=TRUE)
    # 
    # }else{
    #   names(pr)[which(names(pr)=="hv271")] <- "wealth"
    #   pr$wealth <- pr$wealth/100000
   
    
    
    #Rename educ var
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
    #Rename age var
      names(pr)[which(names(pr)=="hv105")] <- "age"
    
    #Rename sex var
    names(pr)[which(names(pr)=="hv104")] <- "sex"
    
    #Rename cluster/hh var
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    names(pr)[which(names(pr)=="hv024")] <- "region"
    names(pr)[which(names(pr)=="hvidx")] <- "line"
    names(pr)[which(names(pr)=="hv112")] <- "mother.line"
    pr$mother.line[which(pr$mother.line==99)] <- NA
    
    #Join IR
    pr <- merge(pr,ir,by=c("cluster","household","line"),all.x=TRUE)
    
    
    #Head vars
    names(pr)[which(names(pr)=="hv219")] <- "head.sex"
    names(pr)[which(names(pr)=="hv220")] <- "head.age"
    
    #reg?
    names(pr)[which(names(pr)=="hv140")] <- "birth.cert"
    ###Registration for Togo
    if(country=="tg"){
      pr$birth.cert <- NULL
      kr_path <- paste0("DHS auto/",country,"kr",phase,"fl.RData")
      load(kr_path)
      kr <- data.frame(dat)
      remove(dat)
      kr$line <- kr$s225c
      kr$cluster <- kr$v001
      kr$household <- kr$v002
      kr$birth.cert <- NA
      kr$birth.cert[which(kr$s225e==1)] <- "yes, birth certificate seen"
      kr$birth.cert[which(kr$s225e==2)] <- "yes, birth certificate not seen"
      kr$birth.cert[which(kr$s225e==3)] <- "no"
      kr$birth.cert[which(kr$s225e==8)] <- "no"
      kr$birth.cert[which(is.na(kr$s225e))] <- "no"
      
      kr$birth.reg <- NA
      kr$birth.reg[which(kr$s225f==0)] <- "no"
      kr$birth.reg[which(kr$s225f==1)] <- "yes"
      kr$birth.reg[which(kr$s225f==8)] <- "no"
      kr$birth.reg[which(is.na(kr$s225f))] <- "no"
      #Registered by virtue of having certificate
      kr$birth.reg[which(kr$s225e==1)] <- "yes"
      kr$birth.reg[which(kr$s225e==2)] <- "yes"
      keep <- c("cluster","household","line","birth.reg","birth.cert")
      pr <- merge(pr,kr,by=c("cluster","household","line"),all.x=TRUE)
    }
    
    #nutrition
    names(pr)[which(names(pr)=="ha40")] <- "woman.bmi"
    if(typeof(pr$woman.bmi)!="NULL"){
      pr$woman.bmi <- pr$woman.bmi/100 
    }else{
      pr$woman.bmi <- NA
    }
    names(pr)[which(names(pr)=="hb40")] <- "man.bmi"
    if(typeof(pr$man.bmi)!="NULL"){
      pr$man.bmi <- pr$man.bmi/100 
    }else{
      pr$man.bmi <- NA
    }
    names(pr)[which(names(pr)=="hc1")] <- "age.months"
    names(pr)[which(names(pr)=="hc2")] <- "weight.kg"
    names(pr)[which(names(pr)=="hc3")] <- "height.cm"
    names(pr)[which(names(pr)=="hc15")] <- "standing.lying"
    names(pr)[which(names(pr)=="hc70")] <- "child.height.age"
    if(typeof(pr$child.height.age)=="NULL"){
      pr$child.height.age <- NA
    }else{
      pr$child.height.age <- pr$child.height.age/100
    }
    pr$child.weights <- pr$weights
    
    filename=paste0(country,recode,phase,"dt")
    povcalcuts$filename=tolower(povcalcuts$filename)
    povcalcut <- subset(povcalcuts,filename==povcal_filename)$hc
    extcut <- subset(povcalcuts,filename==povcal_filename)$extreme
    cuts <- c(povcalcut,extcut)
    povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)
    
    pr$p20 <- (pr$wealth < povperc[1])
    pr$ext <- (pr$wealth < povperc[2])
    
    mothers <- unique(pr[c("cluster","household","line","woman.bmi")])
    mothers <- mothers[complete.cases(mothers),]
    names(mothers) <- c("cluster","household","mother.line","mother.bmi")
    pr <- join(
      pr
      ,mothers
      ,by=c("cluster","household","mother.line")
    )
    
    keep <- c("wealth","weights","urban","region","educ","age","sex","cluster","household","head.sex","head.age","p20"
              ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
              ,"woman.bmi","man.bmi","child.weights","mother.bmi","ext","all.births","skilled.births","maternal.deaths","woman.weights","water.time"
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
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1
  }
}

# bjhr41dt
# Error in readChar(con, 5L, useBytes = TRUE) : cannot open the connection
# In addition: Warning message:
#   In readChar(con, 5L, useBytes = TRUE) :
#   cannot open compressed file 'BJIR41DT/bjir41fl.RData', probable reason 'No such file or directory'

varNames <- read.csv("~/git/p20_indicator_time_trends/data/mics_meta_vars_complete.csv",as.is=TRUE,na.strings="")
classes <- read.csv("~/git/p20_indicator_time_trends/data/global_mics_classes.csv",as.is=TRUE,na.strings="NAN")
maternal.varNames <- read.csv("~/git/p20_indicator_time_trends/data/mics_child_vars.csv",as.is=TRUE,na.strings="")

birth.missing <- c("",9,"?","missing","manquant","omidito","non concerne",NA,"NA")

recode.birth.vars <- function(x,skilled){
  str <- trimws(tolower(x))
  
  if(str %in% birth.missing){
    return(FALSE)
  }else if(!skilled){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

code.skilled <- function(skilled.column,skilled=TRUE){
  if(is.factor(skilled.column)){
    skilled.column <- unfactor(skilled.column)
  }
  return(sapply(skilled.column,recode.birth.vars,skilled))
}

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

for(i in 2:length(dirs)){
  dir <- dirs[i]
  hrBase <- basename(dir)
  if(
    (hrBase %in% povcalcuts$filename) 
    & (hrBase %in% unique(varNames$filename))
    & (hrBase %in% unique(classes$filename))
    & (hrBase %in% unique(maternal.varNames$filename))
  ){
    
    message(hrBase)
    if(exists("hh")){rm(hh)}
    if(exists("hl")){rm(hl)}
    if(exists("ch")){rm(ch)}
    if(exists("wm")){rm(wm)}
    load(paste0(dir,"/","hh.RData"))
    load(paste0(dir,"/","hl.RData"))
    load(paste0(dir,"/","ch.RData"))
    load(paste0(dir,"/","wm.RData"))
    hh <- data.frame(hh,as.is=TRUE,check.names=FALSE)
    hl <- data.frame(hl,as.is=TRUE,check.names=FALSE)
    ch <- data.frame(ch,as.is=TRUE,check.names=FALSE)
    wm <- data.frame(wm,as.is=TRUE,check.names=FALSE)
    names(hh) <- tolower(names(hh))
    names(hl) <- tolower(names(hl))
    names(ch) <- tolower(names(ch))
    names(wm) <- tolower(names(wm))
    
    file.varName <- subset(varNames,filename==hrBase)
    
    attendedVar <- subset(file.varName,match=="attended")$varName
    gradeVar <- subset(file.varName,match=="grade")$varName
    schoolVar <- subset(file.varName,match=="school")$varName
    
    ynm.classes <- subset(classes,filename==hrBase & type=="ynm")
    attended.classes <- subset(classes,filename==hrBase & type=="attended")
    urban.rural.classes <- subset(classes,filename==hrBase & type=="urban.rural")
    school.classes <- subset(classes,filename==hrBase & type=="school")
    
    missing.vals <- subset(ynm.classes,is.na(ynm))$value
    no.vals <- subset(ynm.classes,ynm==0)$value
    yes.vals <- subset(ynm.classes,ynm==1)$value
    
    missing.attended <- subset(attended.classes,is.na(attended))$value
    no.attended <- subset(attended.classes,attended==0)$value
    yes.attended <- subset(attended.classes,attended==1)$value
    
    missing.level <- subset(school.classes,is.na(level))$value
    none.level <- subset(school.classes,level=="none")$value
    preschool.level <- subset(school.classes,level=="preschool")$value
    primary.level <- subset(school.classes,level=="primary")$value
    secondary.level <- subset(school.classes,level=="secondary")$value
    higher.level <- subset(school.classes,level=="higher")$value
    
    skilledBirthVars <- subset(maternal.varNames,match=="attendant" &  skilled==1 & filename==hrBase)$var
    unskilledBirthVars <- subset(maternal.varNames,match=="attendant" &  skilled==0 & filename==hrBase)$var
    
    #maternal
    names(wm)[which(names(wm)=="hh1")] <- "cluster"
    names(wm)[which(names(wm)=="hh2")] <- "household"
    names(wm)[which(names(wm)=="ln")] <- "line"
    names(wm)[which(names(wm)=="wmweight")] <- "woman.weights"
    
    wm$skilled.attendant <- NA
    for(var in skilledBirthVars){
      message(var)
      wm$skilled.attendant <- wm$skilled.attendant | code.skilled(wm[,var])
    }
    wm$unskilled.attendant <- NA
    for(var in unskilledBirthVars){
      message(var)
      wm$unskilled.attendant <- wm$unskilled.attendant | code.skilled(wm[,var])
    }
    wm$skilled.births <- NA
    wm$skilled.births[which(wm$skilled.attendant==TRUE)] <- 1
    wm$skilled.births[which(wm$unskilled.attendant==TRUE & is.na(wm$skilled.attendant))] <- 0
    
    wm$all.births <- NA
    wm$all.births[which(wm$skilled.attendant==TRUE)] <- 1
    wm$all.births[which(wm$unskilled.attendant==TRUE)] <- 1
    
    #Rename wealth var
    if(typeof(hh$wlthscor)=="NULL" | typeof(hh$wlthscor)=="logical" | length(hh$wlthscor[which(!is.na(hh$wlthscor))])==0){
      if(typeof(hh$wscore)=="NULL" | typeof(hh$wscore)=="logical" | length(hh$wscore[which(!is.na(hh$wscore))])==0){
        message("Wealth missing!");
        if(hrBase=="Djibouti MICS 2006 SPSS Datasets"){
          message("Building wealth index")
        }else{
          return(NA)
        }
      }else{
        names(hh)[which(names(hh)=="wscore")] <- "wealth"
      }
    }else{
      names(hh)[which(names(hh)=="wlthscor")] <- "wealth"
    }
    
    #Rename sample.weights var
    names(hh)[which(names(hh)=="hhweight")] <- "weights"
    
    #Rename urban var
    names(hh)[which(names(hh)=="hh6")] <- "urban.rural"
    if(typeof(hh$urban.rural)=="NULL"){message("No urban.rural!");hh$urban.rural<-NA;urban.missing<-TRUE}else{urban.missing<-FALSE}
    
    #Rename educ var
    names(hl)[which(names(hl)==attendedVar)] <- "attended"
    names(hl)[which(names(hl)==schoolVar)] <- "school"
    names(hl)[which(names(hl)==gradeVar)] <- "grade"
    
    #Rename age var
    if(max(hl$hl6,na.rm=TRUE)<45){
      names(hl)[which(names(hl)=="hl5")] <- "age"
    }else{
      names(hl)[which(names(hl)=="hl6")] <- "age"
    }
    
    
    #Rename sex var
    names(hl)[which(names(hl)=="hl4")] <- "sex"
    
    #Rename head var
    hl$head <- tolower(substr(hl$hl3,1,4)) %in% c("chef","head")
    
    #Rename child vars
    names(ch)[which(names(ch)=="br1")] <- "birth.cert"
    names(ch)[which(names(ch)=="br2")] <- "birth.reg"
    names(ch)[which(names(ch)=="cage")] <- "age.months"
    names(ch)[which(names(ch)=="chweight")] <- "child.weights"
    names(ch)[which(names(ch)=="an3")] <- "weight.kg"
    names(ch)[which(names(ch)=="an4a")] <- "standing.lying"
    names(ch)[which(names(ch)=="haz2")] <- "child.height.age"
    
    if(hrBase %in% c("Guinea Bissau_MICS5_Datasets","Vanuatu MICS 2007 SPSS Datasets","Sao Tome and Principe_MICS5_Datasets") ){
      ch$birth.reg.coded <- 0
      ch$birth.reg.coded[which(ch$birth.cert %in% c(1,2))] <- 1
      ch$birth.reg.coded[which(ch$birth.reg %in% c(1))] <- 1
      ch$birth.reg <- ch$birth.reg.coded
    }
    if(hrBase=="Somalia MICS 2006 SPSS Datasets"){
      ch$birth.reg <- 0
      # ch$birth.reg[which(is.na(ch$br2a))] <- NA
      ch$birth.reg[which(ch$birth.cert %in% c(1,2))] <- 1
      ch$birth.reg[which(ch$br2a %in% c(1,2,3))] <- 1
    }
    
    #code female bmi
    if(typeof(wm$anw4)!="NULL" & typeof(wm$anw5)!="NULL"){
      wm$anw4[which(wm$anw4==99.9)] <- NA
      wm$anw5[which(wm$anw5==999.9)] <- NA
      wm$anw5 <- wm$anw5/100
      wm$woman.bmi <- wm$anw4/(wm$anw5*wm$anw5)
    }
    
    #Rename cluster/hh var
    names(hl)[which(names(hl)=="hh1")] <- "cluster"
    names(hl)[which(names(hl)=="hh2")] <- "household"
    names(hl)[which(names(hl)=="hl1")] <- "line"
    names(hl)[which(names(hl)=="ln")] <- "line"
    names(hh)[which(names(hh)=="hh1")] <- "cluster"
    names(hh)[which(names(hh)=="hh2")] <- "household"
    names(hh)[which(names(hh)=="hh7")] <- "region"
    names(ch)[which(names(ch)=="hh1")] <- "cluster"
    names(ch)[which(names(ch)=="hh2")] <- "household"
    names(ch)[which(names(ch)=="ln")] <- "line"
    names(ch)[which(names(ch)=="uf6")] <- "mother.line"
    
    recode.educ <- function(attendedV,schoolV,gradeV){
      educV <- c()
      for(i in 1:length(attendedV)){
        attended <- tolower(attendedV[i])
        school <- tolower(schoolV[i])
        if(length(school)<=0){
          school <- NA
        }
        grade <- gradeV[i]
        ###Ignore factor grades for now... We need to code these out in the metavars
        if(is.factor(grade)){
          grade <- NA
        }
        if(!is.na(grade)){
          if(grade>90){grade<-NA}
        }
        if(attended %in% missing.attended){
          if(school %in% missing.level){
            if(is.na(grade)){
              #missing all three
              educ <- NA
            }else{
              #missing attended and level, but not grade
              if(grade>=5 & grade<7){
                educ <- 1
              }else if(grade>=7 & grade<9){
                educ <- 2
              }else if(grade>9){
                educ <- 3
              }else{
                educ <- 0
              }
            }
          }else{
            #missing attended, but not level
            if(is.na(grade)){
              #has level, but not grade
              if(school %in% preschool.level | school %in% none.level){
                educ <- 0
              }else if(school %in% primary.level){
                educ <- 1
              }else if(school %in% secondary.level){
                educ <- 2
              }else if(school %in% higher.level){
                educ <- 3
              }else{
                educ <- NA
              }
            }else{
              #missing attended and level, but not grade
              if(grade>=5 & grade<7){
                educ <- 1
              }else if(grade>=7 & grade<9){
                educ <- 2
              }else if(grade>9){
                educ <- 3
              }else{
                educ <- 0
              }
            }
          }
        }else if(attended %in% no.attended){
          #No education
          educ <- 0
        }else{
          if(school %in% missing.level){
            if(is.na(grade)){
              #has attended, but has no level or grade
              educ <- NA
            }else{
              #has attended, missing level, but not missing grade
              if(grade>=5 & grade<7){
                educ <- 1
              }else if(grade>=7 & grade<9){
                educ <- 2
              }else if(grade>9){
                educ <- 3
              }else{
                educ <- 0
              }
            }
          }else if(school %in% preschool.level | school %in% none.level){
            if(is.na(grade)){
              educ <- 0
            }else if(grade>=5){
              #Complete primary
              educ <- 1
            }else{
              educ <- 0
            }
          } else if(school %in% primary.level){
            if(is.na(grade)){
              educ <- 0
            }else if(grade<5){
              #Incomplete primary
              educ <- 0
            }else if(grade>=5){
              #Complete primary
              educ <- 1
            }else{
              educ <- NA
            }
          } else if(school %in% secondary.level){
            #(in)complete secondary
            educ <- 2
          } else if(school %in% higher.level){
            #(in)complete higher
            educ <- 3
          }else if(grade>=5 & grade<7){
            educ <- 1
          }else if(grade>=7 & grade<9){
            educ <- 2
          }else if(grade>9){
            educ <- 3
          }else if(grade<5){
            #not at least 5 years of some other schooling
            educ <- 0
          } else{
            #missing grade with preschool, primary, or other
            educ <- NA
          }
        }
        educV <- c(educV,educ)
      }
      return(educV)
    }
    
    hl$educ <- recode.educ(hl$attended,hl$school,hl$grade)
    
    head <- subset(hl,head==1)
    names(head)[which(names(head)=="sex")] <- "head.sex"
    names(head)[which(names(head)=="age")] <- "head.age"
    keep <- c("cluster","household","head.sex","head.age")
    head <- head[keep]
    hh <- join(
      hh
      ,head
      ,by=c("cluster","household")
    )
    
    recode.urban.rural <- function(x){
      item <- subset(urban.rural.classes,value==tolower(x))
      if(nrow(item)==0){return(NA)}
      else{item$urban[1]}
    }
    hh$urban <- sapply(hh$urban.rural,recode.urban.rural)
    
    if(hrBase=="Djibouti MICS 2006 SPSS Datasets"){
      #Make a wealth index

      catvars = c(
        "ws1" #Drinking water
        ,"ws2" #Other water
        ,"ws5" #Water treated
        ,"ws8" #Toilet type
        ,"ws9" #Toilet shared
        ,"hc3" #Floor
        ,"hc4" #Roof
        ,"hc5" #Wall
        ,"hc6" #Fuel
        ,"hc9a" #Assets
        ,"hc9b"
        ,"hc9c"
        ,"hc9d"
        ,"hc9e"
        ,"hc9f"
        ,"hc10a"
        ,"hc10b"
        ,"hc10c"
        ,"hc10d"
        ,"hc10e"
        ,"hc10f"
        
      )
      
      numvars = c(
        "ws3" #Time to fetch water
        ,"hc2" #sleeping rooms
      )
      
      urbanvar = "urban"
      hh <- data.frame(hh)
      
      hh$missingCount = rowSums(is.na(hh[c(catvars,numvars)]))
      hh.wealth <- hh
      # hh.wealth = subset(hh,missingCount<length(c(catvars,numvars)))
      
      hh.wealth <- wealth(hh.wealth,catvars,numvars,urbanvar)
      keep <- c("cluster","household","wealth")
      hh.wealth <- hh.wealth[keep]
      hh.wealth$wealth <- hh.wealth$wealth*-1
      hh <- merge(hh,hh.wealth,all.x=TRUE)
    }
    
    povcalcut <- subset(povcalcuts,filename==hrBase)$hc
    extcut <- subset(povcalcuts,filename==hrBase)$extreme
    cuts <- c(povcalcut,extcut)
    povperc <- weighted.percentile(hh$wealth,hh$weights,prob=cuts)
    
    hh$p20 <- (hh$wealth < povperc[1])
    hh$ext <- (hh$wealth < povperc[2])
    
    wmkeep <- c(
      "cluster"
      ,"household"
      ,"line"
      ,"skilled.births"
      ,"all.births"
      ,"ceb"
      ,"cdead"
      ,"woman.bmi"
      ,"woman.weights"
    )
    wmNames <- names(wm)
    namesDiff <- setdiff(wmkeep,wmNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        wm[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      }
    }
    wm <- wm[wmkeep]
    
    hl <- join(
      hl
      ,wm
      ,by=c("cluster","household","line")
    )
    
    names(wm) <- c(
      "cluster"
      ,"household"
      ,"mother.line"
      ,"skilled.births"
      ,"all.births"
      ,"ceb"
      ,"cdead"
      ,"mother.bmi"
      ,"woman.weights"
    )
    
    ch <- join(
      ch
      ,wm
      ,by=c("cluster","household","mother.line")
    )
    
    chkeep <- c("household","cluster","line","birth.cert","birth.reg","age.months","child.weights","weight.kg","standing.lying"
                ,"child.height.age","mother.bmi")
    chNames <- names(ch)
    namesDiff <- setdiff(chkeep,chNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        ch[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      }
    }
    ch <- ch[chkeep]
    
    hl <- join(
      hl
      ,ch
      ,by=c("cluster","household","line")
    )
    
    
    hhkeep <- c("wealth","weights","urban","region","cluster","household","head.sex","head.age","p20","ext")
    hhNames <- names(hh)
    namesDiff <- setdiff(hhkeep,hhNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        hh[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      }
    }
    hh <- hh[hhkeep]
    hl <- join(
      hl
      ,hh
      ,by=c("cluster","household")
    )
    hl <- data.frame(hl,as.is=TRUE,check.names=FALSE)
    keep <- c("wealth","weights","urban","region","educ","age","sex","cluster","household","head.sex","head.age","p20"
              ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
              ,"woman.bmi","man.bmi","child.weights","mother.bmi","ext","all.births","skilled.births","maternal.deaths","woman.weights","water.time"
    )
    hlNames <- names(hl)
    namesDiff <- setdiff(keep,hlNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        hl[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      }
    }
    hl <- hl[keep]
    hl$filename <- hrBase
    dataList[[dataIndex]] <- hl
    dataIndex <- dataIndex + 1
  }
}

data.total <- rbindlist(dataList)

recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(tolower(x)=="dk" | tolower(x)=="don't know"){return(NA)}
  else if(x==0 | tolower(x)=="no education, preschool"){return("No education, preschool")}
  else if(x==1 | tolower(x)=="primary"){return("Primary")}
  else if(x==2 | tolower(x)=="secondary"){return("Secondary")}
  else if(x==3 | tolower(x)=="higher"){return("Higher")}
  else{return(NA)}
}
data.total$educ <- sapply(data.total$educ,recode.educ)
data.total$educ <- factor(data.total$educ
                          ,levels = c("No education, preschool","Primary","Secondary","Higher")
)

codeAgeCat <- function(x){
  startAge <- 0
  ageDiff <- 4
  endAge <- 4
  if(is.na(x)){
    return("missing")
  }
  while(startAge<95){
    endAge <- startAge+ageDiff
    if(x>=startAge & x<=endAge){
      return(
        paste0(startAge,"-",endAge)  
      )
    }
    startAge <- endAge + 1
  }
  if(x>=95){
    return("95+")
  }
  return("missing")
}

data.total$ageCategory <- vapply(data.total$age,codeAgeCat,character(1))
data.total$ageCategory <- factor(data.total$ageCategory,
                                 levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                            ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                            ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                            ,"95+","missing")                          
)

data.total$head.ageCategory <- vapply(data.total$head.age,codeAgeCat,character(1))
data.total$head.ageCategory <- factor(data.total$head.ageCategory,
                                      levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                                 ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                                 ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                                 ,"95+","missing")                          
)

sex.missing = c(NA,"missing",9,"omitido")
sex.male = c(1,"male","masculin","hombre","var???n",3)
sex.female = c(2, "female","feminin","mujer","hembra",4)
if(is.factor(data.total$sex)){data.total$sex<-unfactor(data.total$sex)}
data.total$sex[which(tolower(data.total$sex) %in% sex.missing)] <- NA
data.total$sex[which(tolower(data.total$sex) %in% sex.male)] <- "Male"
data.total$sex[which(tolower(data.total$sex) %in% sex.female)] <- "Female"
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.missing)] <- NA
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.male)] <- "Male"
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.female)] <- "Female"

#0 - neither certificate or registered
#1 - has certificate
#2 - registered
#3 - registered
#6 - other
#8 - dk
# birth.cert.missing <- c(NA,"dk","don't know",6,8,9,"missing","nsp","manquant","no sabe")
# birth.cert.no <- c("registered",0,2,3,"neither certificate or registered","no","non","has only hospital card")
birth.cert.missing <- c(NA)
birth.cert.no <- c("registered",0,2,3,"neither certificate or registered","no","non","has only hospital card","dk","don't know",6,8,9,"missing","nsp","manquant","no sabe")
birth.cert.yes <- setdiff(unique(tolower(data.total$birth.cert)),c(birth.cert.no,birth.cert.missing))

birth.reg.missing <- c(NA,9)
birth.reg.no <- c("no","non",0,"dk","missing","nsp","manquant",8,2)
birth.reg.yes <- c("yes","oui","s???",1)
#count registrations if birth.cert var reveals it to be so
birth.cert.registered <- c(1,2,3,"registered","has only hospital card",birth.cert.yes)
birth.cert.not.registered <- c(0,"neither certificate or registered","no","non","dk","don't know",6,8,9,"missing","nsp","manquant","no sabe")
if(is.factor(data.total$birth.reg)){
  data.total$birth.reg.coded <- unfactor(data.total$birth.reg)
}else{
  data.total$birth.reg.coded <- data.total$birth.reg
}
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.registered)] <- "Yes"
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.not.registered)] <- "No"
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & grepl("visto",data.total$birth.cert))] <- "Yes"

data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.missing)] <- NA
data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.no)] <- 0
data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.yes)] <- 1
data.total$birth.reg.coded[which(substr(data.total$birth.reg.coded,1,1)=="S")] <- 1

data.total$birth.reg <- as.numeric(data.total$birth.reg.coded)

if(is.factor(data.total$birth.cert)){
  data.total$birth.cert <- unfactor(data.total$birth.cert)
}
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.missing)] <- NA
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.no)] <- 0
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.yes)] <- 1
data.total$birth.cert[which(grepl("visto",data.total$birth.cert))] <- 1

data.total$birth.reg[which(data.total$birth.cert==1)] <- 1

data.total$woman.bmi[which(data.total$woman.bmi>80)] <- NA
data.total$woman.bmi.class <- NA
data.total$woman.bmi.class[which(data.total$woman.bmi<16)] <- "Severe thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=16 & data.total$woman.bmi<17)] <- "Moderate thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=17 & data.total$woman.bmi<18.5)] <- "Mild thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=18.5 & data.total$woman.bmi<25)] <- "Normal range"
data.total$woman.bmi.class[which(data.total$woman.bmi>=25 & data.total$woman.bmi<30)] <- "Pre-obese"
data.total$woman.bmi.class[which(data.total$woman.bmi>=30 & data.total$woman.bmi<35)] <- "Obese class I"
data.total$woman.bmi.class[which(data.total$woman.bmi>=35 & data.total$woman.bmi<40)] <- "Obese class II"
data.total$woman.bmi.class[which(data.total$woman.bmi>=40)] <- "Obese class III"

data.total$woman.bmi.class <- factor(data.total$woman.bmi.class
                                     ,levels=c(
                                       "Severe thinness"
                                       ,"Moderate thinness"
                                       ,"Mild thinness"
                                       ,"Normal range"
                                       ,"Pre-obese"
                                       ,"Obese class I"
                                       ,"Obese class II"
                                       ,"Obese class III"
                                     ))

data.total$man.bmi[which(data.total$man.bmi>80)] <- NA
data.total$man.bmi.class <- NA
data.total$man.bmi.class[which(data.total$man.bmi<16)] <- "Severe thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=16 & data.total$man.bmi<17)] <- "Moderate thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=17 & data.total$man.bmi<18.5)] <- "Mild thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=18.5 & data.total$man.bmi<25)] <- "Normal range"
data.total$man.bmi.class[which(data.total$man.bmi>=25 & data.total$man.bmi<30)] <- "Pre-obese"
data.total$man.bmi.class[which(data.total$man.bmi>=30 & data.total$man.bmi<35)] <- "Obese class I"
data.total$man.bmi.class[which(data.total$man.bmi>=35 & data.total$man.bmi<40)] <- "Obese class II"
data.total$man.bmi.class[which(data.total$man.bmi>=40)] <- "Obese class III"

data.total$man.bmi.class <- factor(data.total$man.bmi.class
                                   ,levels=c(
                                     "Severe thinness"
                                     ,"Moderate thinness"
                                     ,"Mild thinness"
                                     ,"Normal range"
                                     ,"Pre-obese"
                                     ,"Obese class I"
                                     ,"Obese class II"
                                     ,"Obese class III"
                                   ))

data.total$mother.bmi[which(data.total$mother.bmi>80)] <- NA
data.total$mother.bmi.class <- NA
data.total$mother.bmi.class[which(data.total$mother.bmi<16)] <- "Severe thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=16 & data.total$mother.bmi<17)] <- "Moderate thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=17 & data.total$mother.bmi<18.5)] <- "Mild thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=18.5 & data.total$mother.bmi<25)] <- "Normal range"
data.total$mother.bmi.class[which(data.total$mother.bmi>=25 & data.total$mother.bmi<30)] <- "Pre-obese"
data.total$mother.bmi.class[which(data.total$mother.bmi>=30 & data.total$mother.bmi<35)] <- "Obese class I"
data.total$mother.bmi.class[which(data.total$mother.bmi>=35 & data.total$mother.bmi<40)] <- "Obese class II"
data.total$mother.bmi.class[which(data.total$mother.bmi>=40)] <- "Obese class III"

data.total$mother.bmi.class <- factor(data.total$mother.bmi.class
                                      ,levels=c(
                                        "Severe thinness"
                                        ,"Moderate thinness"
                                        ,"Mild thinness"
                                        ,"Normal range"
                                        ,"Pre-obese"
                                        ,"Obese class I"
                                        ,"Obese class II"
                                        ,"Obese class III"
                                      ))

data.total$child.height.age[which(data.total$child.height.age>80)] <- NA
data.total$stunting <- NA
# data.total$stunting[which(data.total$child.height.age<= (-6))] <- "Implausibly low"
data.total$stunting[which(data.total$child.height.age > (-6) & data.total$child.height.age<= (-3))] <- "Severely stunted"
data.total$stunting[which(data.total$child.height.age > (-3) & data.total$child.height.age<= (-2))] <- "Stunted, but not severely"
data.total$stunting[which(data.total$child.height.age > (-2) & data.total$child.height.age< (6))] <- "Not stunted"
# data.total$stunting[which(data.total$child.height.age>= (6))] <- "Implausibly high"
# 
# data.total$stunted <- (data.total$child.height.age> -6) & (data.total$child.height.age<= -2)
# data.total$stunted <- data.total$child.height.age<= -2

data.total$stunting <- factor(data.total$stunting
                              ,levels=c(
                                "Implausibly low"
                                ,"Severely stunted"
                                ,"Stunted, but not severely"
                                ,"Not stunted"
                                ,"Implausibly high"
                              ))

load("~/git/p20_private_data/project_data/Other/Dados_20170517.RData")

names(pr)[which(names(pr)=="V0301")] <- "line"
names(pr)[which(names(pr)=="V0102")] <- "cluster"
names(pr)[which(names(pr)=="V0103")] <- "household"

pr$urban = NA
pr$urban[which(pr$V4728 %in% c(1:3))] = 1
pr$urban[which(pr$V4728 %in% c(4:8))] = 0

names(pr)[which(names(pr)=="V4729")] <- "weights"
pr$weights <- pr$weights/1000

pr$sex <- NA
pr$sex[which(pr$V0302==2)] <- "Male"
pr$sex[which(pr$V0302==4)] <- "Female"
names(pr)[which(names(pr)=="V8005")] <- "age"

pr$birth.reg <- NA
pr$birth.reg[which(pr$V0408==2)] = 1
pr$birth.reg[which(pr$V0408==4)] = 0

povcalcut <- subset(povcalcuts,filename=="Brazil")$hc
extcut <- subset(povcalcuts,filename=="Brazil")$ext
povperc <- weighted.percentile(pr$wealth,pr$weights,prob=c(povcalcut,extcut))

pr$p20 <- (pr$wealth < povperc[1])
pr$ext <- (pr$wealth < povperc[2])

codeAgeCat <- function(x){
  startAge <- 0
  ageDiff <- 4
  endAge <- 4
  if(is.na(x)){
    return("missing")
  }
  while(startAge<95){
    endAge <- startAge+ageDiff
    if(x>=startAge & x<=endAge){
      return(
        paste0(startAge,"-",endAge)  
      )
    }
    startAge <- endAge + 1
  }
  if(x>=95){
    return("95+")
  }
  return("missing")
}

pr$ageCategory <- vapply(pr$age,codeAgeCat,character(1))
pr$ageCategory <- factor(pr$ageCategory,
                         levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                    ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                    ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                    ,"95+","missing")                          
)

# * http://stats.uis.unesco.org/unesco/TableViewer/tableView.aspx?ReportId=163
# * Entrance age of primary: 6y
# * Duration of primary: 5y
# * Entrance age of lower secondary: 11y
# * Durantion lower secondary: 4y
# * Entrance age high secondary: 15y
# * Duration high secondary: 3y /* reviewed January 19 2016 */
#   
#   ** 1- DEPRIVED IN EDUCATION **
#   
#   gen yschooling = 0 if (v0602==4 & v0606==4) | v6003==7 | v6003==9 /* never attended school, creche (preschool) and maternal */
pr$yschooling = NA
pr$yschooling[which((pr$V0602==4 & pr$V0606==4) | pr$V6003==7 | pr$V6003==9)] = 0
#   
#   * currently attending so I substract 1 year to get the completed grade *
#   replace yschooling = 0 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==1
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==1)] = 0
# replace yschooling = 1 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==2
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==2)] = 1
# replace yschooling = 2 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==3
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==3)] = 2
# replace yschooling = 3 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==4
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==4)] = 3
# replace yschooling = 4 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==5
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==5)] = 4
# replace yschooling = 5 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==6
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==6)] = 5
# replace yschooling = 6 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==7
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==7)] = 6
# replace yschooling = 7 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==8
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==8)] = 7
# replace yschooling = 8 if pr$V6003==1 & pr$V0605==0
pr$yschooling[which(pr$V6003==1 & pr$V0605==0)] = 8
# 
# replace yschooling = 8 if (pr$V6003==2 | pr$V6003==4) & pr$V0605==1
pr$yschooling[which((pr$V6003==2 | pr$V6003==4) & pr$V0605==1)] = 8
# replace yschooling = 9 if (pr$V6003==2 | pr$V6003==4) & pr$V0605==2
pr$yschooling[which((pr$V6003==2 | pr$V6003==4) & pr$V0605==2)] = 9
# replace yschooling = 10 if (pr$V6003==2 | pr$V6003==4) & pr$V0605==3
pr$yschooling[which((pr$V6003==2 | pr$V6003==4) & pr$V0605==3)] = 10
# replace yschooling = 11 if pr$V6003==2 & pr$V0605==4
pr$yschooling[which(pr$V6003==2 & pr$V0605==4)] = 11
# 
# replace yschooling = 12 if pr$V6003==5 & pr$V0605==1
pr$yschooling[which(pr$V6003==5 & pr$V0605==1)] = 12
# replace yschooling = 13 if pr$V6003==5 & pr$V0605==2
pr$yschooling[which(pr$V6003==5 & pr$V0605==2)] = 13
# replace yschooling = 14 if pr$V6003==5 & pr$V0605==3
pr$yschooling[which(pr$V6003==5 & pr$V0605==3)] = 14
# replace yschooling = 15 if pr$V6003==5 & pr$V0605==4
pr$yschooling[which(pr$V6003==5 & pr$V0605==4)] = 15
# replace yschooling = 16 if pr$V6003==5 & pr$V0605==5
pr$yschooling[which(pr$V6003==5 & pr$V0605==5)] = 16
# replace yschooling = 17 if pr$V6003==5 & pr$V0605==6
pr$yschooling[which(pr$V6003==5 & pr$V0605==6)] = 17
# 
# 
# *attended school in the past but not currently *
#   replace yschooling = 0 if (pr$V6007==1 | pr$V6007==4 | pr$V6007==6) & pr$V0609==3
pr$yschooling[which((pr$V6007==1 | pr$V6007==4 | pr$V6007==6) & pr$V0609==3)] = 0
# replace yschooling = 0 if pr$V6007==11 | pr$V6007==13 /*creche (preschool) and maternal */
pr$yschooling[which(pr$V6007==11 | pr$V6007==13)] = 0
#   
#   replace yschooling = 1 if pr$V6007==1 & pr$V0610==1
pr$yschooling[which(pr$V6007==1 & pr$V0610==1)] = 1
# replace yschooling = 2 if pr$V6007==1 & pr$V0610==2
pr$yschooling[which(pr$V6007==1 & pr$V0610==2)] = 2
# replace yschooling = 3 if pr$V6007==1 & pr$V0610==3
pr$yschooling[which(pr$V6007==1 & pr$V0610==3)] = 3
# replace yschooling = 4 if pr$V6007==1 & pr$V0610==4
pr$yschooling[which(pr$V6007==1 & pr$V0610==4)] = 4
# replace yschooling = 5 if pr$V6007==1 & pr$V0610==5
pr$yschooling[which(pr$V6007==1 & pr$V0610==5)] = 5
# replace yschooling = 6 if pr$V6007==1 & pr$V0610==6
pr$yschooling[which(pr$V6007==1 & pr$V0610==6)] = 6
# 
# replace yschooling = 5 if pr$V6007==2 & pr$V0609==3
pr$yschooling[which(pr$V6007==2 & pr$V0609==3)] = 5
# replace yschooling = 6 if pr$V6007==2 & pr$V0610==1
pr$yschooling[which(pr$V6007==2 & pr$V0610==1)] = 6
# replace yschooling = 7 if pr$V6007==2 & pr$V0610==2
pr$yschooling[which(pr$V6007==2 & pr$V0610==2)] = 7
# replace yschooling = 8 if pr$V6007==2 & pr$V0610==3
pr$yschooling[which(pr$V6007==2 & pr$V0610==3)] = 8
# replace yschooling = 9 if pr$V6007==2 & (pr$V0610==4 | pr$V0610==5)
pr$yschooling[which(pr$V6007==2 & (pr$V0610==4 | pr$V0610==5))] = 9
# 
# replace yschooling = 9 if pr$V6007==3 & pr$V0609==3
pr$yschooling[which(pr$V6007==3 & pr$V0609==3)] = 9
# replace yschooling = 10 if pr$V6007==3 & pr$V0610==1
pr$yschooling[which(pr$V6007==3 & pr$V0610==1)] = 10
# replace yschooling = 11 if pr$V6007==3 & pr$V0610==2
pr$yschooling[which(pr$V6007==3 & pr$V0610==2)] = 11
# replace yschooling = 12 if pr$V6007==3 & pr$V0610==3
pr$yschooling[which(pr$V6007==3 & pr$V0610==3)] = 12
# replace yschooling = 13 if pr$V6007==3 & pr$V0610==4
pr$yschooling[which(pr$V6007==3 & pr$V0610==4)] = 13
# 
# replace yschooling = 1 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==1
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==1)] = 1
# replace yschooling = 2 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==2
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==2)] = 2
# replace yschooling = 3 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==3
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==3)] = 3
# replace yschooling = 4 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==4
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==4)] = 4
# replace yschooling = 5 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==5
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==5)] = 5
# replace yschooling = 6 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==6
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==6)] = 6
# replace yschooling = 7 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==7
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==7)] = 7
# replace yschooling = 8 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==8
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==8)] = 8
# replace yschooling = 9 if pr$V6007==4 & pr$V0610==0
pr$yschooling[which(pr$V6007==4 & pr$V0610==0)] = 9
# 
# replace yschooling = 8 if (pr$V6007==5 | pr$V6007==7) & pr$V0609==3
pr$yschooling[which((pr$V6007==5 | pr$V6007==7) & pr$V0609==3)] = 8
# replace yschooling = 9 if (pr$V6007==5 | pr$V6007==7) & pr$V0610==1
pr$yschooling[which((pr$V6007==5 | pr$V6007==7) & pr$V0610==1)] = 9
# replace yschooling = 10 if (pr$V6007==5 | pr$V6007==7) & pr$V0610==2
pr$yschooling[which((pr$V6007==5 | pr$V6007==7) & pr$V0610==2)] = 10
# replace yschooling = 11 if (pr$V6007==5 | pr$V6007==7) & pr$V0610==3
pr$yschooling[which((pr$V6007==5 | pr$V6007==7) & pr$V0610==3)] = 11
# replace yschooling = 12 if pr$V6007==5 & pr$V0610==4
pr$yschooling[which(pr$V6007==5 & pr$V0610==4)] = 12
# 
# replace yschooling = 12 if pr$V6007==8 & pr$V0609==3
pr$yschooling[which(pr$V6007==8 & pr$V0609==3)] = 12
# replace yschooling = 13 if pr$V6007==8 & pr$V0610==1
pr$yschooling[which(pr$V6007==8 & pr$V0610==1)] = 13
# replace yschooling = 14 if pr$V6007==8 & pr$V0610==2
pr$yschooling[which(pr$V6007==8 & pr$V0610==2)] = 14
# replace yschooling = 15 if pr$V6007==8 & pr$V0610==3
pr$yschooling[which(pr$V6007==8 & pr$V0610==3)] = 15
# replace yschooling = 16 if pr$V6007==8 & pr$V0610==4
pr$yschooling[which(pr$V6007==8 & pr$V0610==4)] = 16
# replace yschooling = 17 if pr$V6007==8 & pr$V0610==5
pr$yschooling[which(pr$V6007==8 & pr$V0610==5)] = 17
# replace yschooling = 18 if pr$V6007==8 & pr$V0610==6
pr$yschooling[which(pr$V6007==8 & pr$V0610==6)] = 18
# 
# 
# replace yschooling=. if age<5
# pr$yschooling[which(pr$age<5)] = NA
recode.educ = function(y){
  if(is.na(y)){return(NA)}
  if(y<5){return("No education, preschool")}
  if(y<9){return("Primary")}
  if(y<12){return("Secondary")}
  return("Higher")
}
pr$educ = sapply(pr$yschooling,recode.educ)
keep <- c("wealth","weights","urban","educ","age","sex","cluster","household","head.sex","head.age","p20"
          ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age","child.weight.age"
          ,"woman.bmi","man.bmi","ageCategory","head.ageCategory","stunting","ext"
)
prNames <- names(pr)
namesDiff <- setdiff(keep,prNames)
if(length(namesDiff)>0){
  for(y in 1:length(namesDiff)){
    pr[namesDiff[y]] <- NA
    message(paste("Missing variable",namesDiff[y]))
  } 
}
data <- pr[keep]
data$filename <- "Brazil"

brazil.data.total <- data
data.total <- rbind(brazil.data.total,data.total,fill=TRUE)

load("~/git/p20_private_data/project_data/Other/CFPS/dat2014.RData")
load("~/git/p20_private_data/project_data/Other/CFPS/wealth.RData")

hr <- dat
ir <- famros
ch <- data.frame(child,as.is=TRUE,check.names=FALSE)

#Rename sample.weights var
names(hr)[which(names(hr)=="fswt_natcs14")] <- "sample.weights"
hr$weights <- hr$sample.weights/100000

#Rename urban var
names(hr)[which(names(hr)=="urban14")] <- "urban.rural"
recode.urban.rural <- function(x){
  if(is.null(x)){return(NA)}
  else if(is.na(x) | x==-9){return(NA)}
  else if(x==1){return(1)}
  else if(x==0){return(0)}
  else{return(NA)}
}
hr$urban <- sapply(hr$urban.rural,recode.urban.rural)

#Rename educ var
names(ir)[which(names(ir)=="tb4_a14_p")] <- "educ"
recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(x<0 | x>8){return(NA)}
  else if(x==1){return("No education, preschool")}
  else if(x==2){return("Primary")}
  else if(x==3 | x==4){return("Secondary")}
  else{return("Higher")}
}
ir$educ <- sapply(ir$educ,recode.educ)
ir$educ <- factor(ir$educ
                  ,levels = c("No education, preschool","Primary","Secondary","Higher")
)

#Rename age/sex var
names(ir)[which(names(ir)=="tb1y_a_p")] <- "birth.year"
ir$birth.year[which(ir$birth.year<=0 | ir$birth.year>=2015)] <- NA
ir$age <- 2014-ir$birth.year
names(ir)[which(names(ir)=="tb2_a_p")] <- "sex"
ir$sex[which(ir$sex<0)] <- NA
ir$sex[which(ir$sex==0)] <- "female"
ir$sex[which(ir$sex==1)] <- "male"


#Registration
names(ir)[which(names(ir)=="qa301_a14_p")] <- "birth.reg.raw"
registered <- c(1,3,79)
nonregistered <- c(5)
ir$birth.reg <- NA
ir$birth.reg[which(ir$birth.reg.raw %in% registered)] <- 1
ir$birth.reg[which(ir$birth.reg.raw %in% nonregistered)] <- 0

#Weight and height
famros.birthdays <- famros[c("pid","fid14","tb1y_a_p","tb1m_a_p")]
ch <- data.frame(child,as.is=TRUE,check.names=FALSE)
ch <- join(
  ch
  ,famros.birthdays
  ,by=c("pid","fid14")
)

code.age.months <- function(cyearV,cmonthV,byearV,bmonthV,ageV){
  age.monthsV <- c()
  for(i in 1:length(cyearV)){
    cyear <- cyearV[i]
    cmonth <- cmonthV[i]
    byear <- byearV[i]
    bmonth <- bmonthV[i]
    age <- ageV[i]
    if(is.na(bmonth)){
      age.months <- age*12
    }
    else if(cmonth==bmonth){
      age.months <- (cyear - byear)*12
    }else if(cmonth>bmonth){
      age.months <- (cyear - byear)*12 + (cmonth-bmonth)
    }else if(cmonth<bmonth){
      age.months <- ((cyear - byear) - 1)*12 + (12 - (bmonth-cmonth))
    }
    if(!is.na(age.months)){
      if(age.months<0){
        age.months <- 0
      } 
    }
    age.monthsV <- c(age.monthsV,age.months)
  }
  return(age.monthsV)
}
ch$tb1m_a_p[which(ch$tb1m_a_p<0)] <- NA
ch$cfps2014_age[which(ch$cfps2014_age<0)] <- NA
ch$age.months <- code.age.months(ch$cyear,ch$cmonth,ch$tb1y_a_p,ch$tb1m_a_p,ch$cfps2014_age)

names(ch)[which(names(ch)=="wa103")] <- "weight.kg"
ch$weight.kg[which(ch$weight.kg<0)] <- NA
ch$weight.kg <- ch$weight.kg/2
names(ch)[which(names(ch)=="wa104")] <- "height.cm"
ch$height.cm[which(ch$height.cm<0)] <- NA
ch <- subset(ch,age.months<=60)
names(ch)[which(names(ch)=="cfps_gender")] <- "sex"
ch$gender<- NA
ch$gender[which(ch$sex==1)] <- 1
ch$gender[which(ch$sex==0)] <- 2
names(ch)[which(names(ch)=="rswt_natcs14")] <- "weights"
ch$weights <- ch$weights/100000
names(ch)[which(names(ch)=="cid14")] <- "cluster"
names(ch)[which(names(ch)=="fid14")] <- "household"
ch <- ch[complete.cases(ch[c("weight.kg","height.cm","age.months","gender","weights")]),]
keep <- c("cluster","household","pid","weight.kg","height.cm","age.months","gender","weights")
ch <- ch[keep]

igu.dir <- "~/git/p20_indicator_time_trends/code/igrowup_R"
weianthro<-read.table(paste0(igu.dir,"/weianthro.txt"),header=T,sep="",skip=0)
lenanthro<-read.table(paste0(igu.dir,"/lenanthro.txt"),header=T,sep="",skip=0)
bmianthro<-read.table(paste0(igu.dir,"/bmianthro.txt"),header=T,sep="",skip=0)
hcanthro<-read.table(paste0(igu.dir,"/hcanthro.txt"),header=T,sep="",skip=0)
acanthro<-read.table(paste0(igu.dir,"/acanthro.txt"),header=T,sep="",skip=0)
ssanthro<-read.table(paste0(igu.dir,"/ssanthro.txt"),header=T,sep="",skip=0)
tsanthro<-read.table(paste0(igu.dir,"/tsanthro.txt"),header=T,sep="",skip=0)
wflanthro<-read.table(paste0(igu.dir,"/wflanthro.txt"),header=T,sep="",skip=0)
wfhanthro<-read.table(paste0(igu.dir,"/wfhanthro.txt"),header=T,sep="",skip=0)
source(paste0(igu.dir,"igrowup_standard.r"))
source(paste0(igu.dir,"igrowup_restricted.r"))
igrowup.restricted(FileLab="ch",FilePath=igu.dir,
                   mydf=ch, sex=gender
                   , age=age.months, age.month=TRUE
                   , weight=weight.kg
                   , lenhei=height.cm
                   , sw=weights)

zscores <- read.csv(paste0(igu.dir,"ch_z_rc.csv"))
zscores$standing.lying <- NA
zscoreKeep <- c("cluster","household","pid","weight.kg","height.cm","age.months","standing.lying","zlen","zwei")
zscores <- zscores[zscoreKeep]
names(zscores)[which(names(zscores)=="zlen")] <- "child.height.age"
names(zscores)[which(names(zscores)=="zwei")] <- "child.weight.age"

#Rename cluster/hh var
# names(hr)[which(names(hr)=="provcd")] <- "province"
# names(hr)[which(names(hr)=="countyid")] <- "county"
names(ir)[which(names(ir)=="fid14")] <- "household"
names(hr)[which(names(hr)=="cid14")] <- "cluster"
names(hr)[which(names(hr)=="fid14")] <- "household"

#Household head
# hh.heads <- unique(ir$tf10pid)
# head <- subset(ir,pid %in% hh.heads)
# names(head)[which(names(head)=="age")] <- "head.age"
# names(head)[which(names(head)=="sex")] <- "head.sex"
# keep <- c("household","head.age","head.sex")
# head <- head[keep]
# 
# ir <- join(
#   ir
#   ,head
#   ,by=c("household")
# )

keep <- c("cluster","household","wealth","weights","urban")
hr <- hr[keep]

ir <- join(
  ir
  ,hr
  ,by=c("household")
)

povcalcut <- subset(povcalcuts,filename=="China")$hc
extcut <- subset(povcalcuts,filename=="China")$extreme
cuts <- c(povcalcut,extcut)
povperc <- weighted.percentile(ir$wealth,ir$weights,prob=cuts)

ir$p20 <- (ir$wealth < povperc[1])
ir$ext <- (ir$wealth < povperc[2])

ir <- join(
  ir
  ,zscores
  ,by=c("cluster","household","pid")
)

keep <- c("wealth","weights","urban","educ","age","sex","cluster","household","head.sex","head.age","p20"
          ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
          ,"woman.bmi","man.bmi","ext"
)
irNames <- names(ir)
namesDiff <- setdiff(keep,irNames)
if(length(namesDiff)>0){
  for(y in 1:length(namesDiff)){
    ir[namesDiff[y]] <- NA
    message(paste("Missing variable",namesDiff[y]))
  } 
}
ir <- ir[keep]

codeAgeCat <- function(x){
  startAge <- 0
  ageDiff <- 4
  endAge <- 4
  if(is.na(x)){
    return("missing")
  }
  while(startAge<95){
    endAge <- startAge+ageDiff
    if(x>=startAge & x<=endAge){
      return(
        paste0(startAge,"-",endAge)  
      )
    }
    startAge <- endAge + 1
  }
  if(x>=95){
    return("95+")
  }
  return("missing")
}

ir$ageCategory <- vapply(ir$age,codeAgeCat,character(1))
ir$ageCategory <- factor(ir$ageCategory,
                         levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                    ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                    ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                    ,"95+","missing")                          
)

# ir$head.ageCategory <- vapply(ir$head.age,codeAgeCat,character(1))
# ir$head.ageCategory <- factor(ir$head.ageCategory,
#                               levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
#                                          ,"35-39","40-44","45-49","50-54","55-59","60-64"
#                                          ,"65-69","70-74","75-79","80-84","85-89","90-94"
#                                          ,"95+","missing")                          
# )

#Not really stunting, do we want to just call this "nutrition"?
ir$stunting <- NA
# ir$stunting[which(ir$child.height.age<= (-6))] <- "Implausibly low"
ir$stunting[which(ir$child.height.age > (-6) & ir$child.height.age<= (-3))] <- "Severely stunted"
ir$stunting[which(ir$child.height.age > (-3) & ir$child.height.age<= (-2))] <- "Stunted, but not severely"
ir$stunting[which(ir$child.height.age > (-2) & ir$child.height.age< (6))] <- "Not stunted"
# ir$stunting[which(ir$child.height.age>= (6))] <- "Implausibly high"

ir$stunting <- factor(ir$stunting
                      ,levels=c(
                        "Implausibly low"
                        ,"Severely stunted"
                        ,"Stunted, but not severely"
                        ,"Not stunted"
                        ,"Implausibly high"
                      ))
ir$filename <- "China"
china.data.total <- ir
data.total <- rbind(china.data.total,data.total,fill=TRUE)

wd <- "~/git/p20_private_data/project_data"
setwd(wd)

save(data.total,file="total_tab_data.RData")