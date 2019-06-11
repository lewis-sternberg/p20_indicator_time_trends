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
    
    #Head vars
    names(pr)[which(names(pr)=="hv219")] <- "head.sex"
    names(pr)[which(names(pr)=="hv220")] <- "head.age"
    
    #reg?
    names(pr)[which(names(pr)=="hv140")] <- "birth.cert"
    #0 - neither certificate or registered
    #1 - has certificate
    #2 - registered
    #3 - registered
    #6 - other
    #8 - dk
    
    names(pr)[which(names(pr)=="hc70")] <- "child.height.age"
    if(typeof(pr$child.height.age)=="NULL"){
      pr$child.height.age <- NA
    }else{
      pr$child.height.age <- pr$child.height.age/100
    }
    
    filename=paste0(country,recode,phase,"dt")
    povcalcuts$filename=tolower(povcalcuts$filename)
    povcalcut <- subset(povcalcuts,filename==povcal_filename)$hc
    extcut <- subset(povcalcuts,filename==povcal_filename)$extreme
    cuts <- c(povcalcut,extcut)
    povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)
    
    pr$p20 <- (pr$wealth < povperc[1])
    pr$ext <- (pr$wealth < povperc[2])
    
    keep <- c("wealth","weights","urban","region","educ","age","sex","cluster","household","head.sex","head.age","p20"
              ,"birth.cert","child.height.age","ext"
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

data.total <- rbindlist(dataList)
save(data.total,file="~/git/p20_private_data/project_data/historical_dhs.RData")


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
