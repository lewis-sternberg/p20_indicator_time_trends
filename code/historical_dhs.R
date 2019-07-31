####Function and setup####
list.of.packages <- c("Hmisc","plyr","foreign","data.table","varhandle","zoo","survey")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

#Taken from https://raw.githubusercontent.com/akmiller01/alexm-util/master/DevInit/R/P20/2013_tab_data2.R

if(Sys.info()[["user"]]=="alex"){
  wd <- "~/git/p20_indicator_time_trends"
  wd2 <- "~/git/p20_private_data/project_data/DHS auto"
}else if(Sys.info()[["user"]]=="dan-w" | Sys.info()[["user"]]=="danw"){
  wd <- "G:/My Drive/Work/GitHub/p20_indicator_time_trends"
  wd2 <- "G:/My Drive/Work/GitHub/p20_indicator_time_trends/data/DHSauto"
}else{
  wd <- "D:/git/p20_indicator_time_trends"
  wd2 <- "D:/DHSauto"
}

setwd(wd)

source("code/child_mort.R")
povcalcuts <- fread("https://raw.githubusercontent.com/ZChristensen/poverty_trends/master/data/P20incometrends.csv")
dhsmeta<- fread("data/dhs_meta_data20190524.csv")
dhsmeta<- subset(dhsmeta, Recode.Structure.!="DHS-I" & WealthIndex == 1)

dhsmeta$Country.[which(dhsmeta$Country.=="Cape Verde")]<-"Cabo Verde"
dhsmeta$Country.[which(dhsmeta$Country.=="Congo")]<-"Congo, Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Congo Democratic Republic")]<-"Congo, Democratic Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Egypt")]<-"Egypt, Arab Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Gambia")]<-"Gambia, The"
dhsmeta$Country.[which(dhsmeta$Country.=="Yemen")]<-"Yemen, Republic of"
#Afghanistan, Cambodia, Equatorial Guinea and Eritrea have had DHS surveys but don't have PovcalNet data
names(dhsmeta)[which(names(dhsmeta)=="Country.")] <- "CountryName"

dhsmeta$filename=paste0(dhsmeta$dhs_cc,"HR",dhsmeta$dhs_recode_code,"DT")
dhsmeta=dhsmeta[which(!is.na(dhsmeta$dhs_cc)),]

dhsmeta2 <- unique(dhsmeta[,c("CountryName","surveyyr","filename")])
povcalyears=c(1990,1993,1996,1999,2002,2005,2008,2010,2011,2012,2013,2015)

variables <- c("birth.registration","u5.mortality","stunting","education")
grid = as.data.table(expand.grid(filename=unique(dhsmeta2$filename), RequestYear=povcalyears, variable = variables))

dhsmeta2 <- merge(grid, dhsmeta2, all=T)

povcalcuts <- join(dhsmeta2,povcalcuts,by=c("CountryName","RequestYear"))

names(povcalcuts)[which(names(povcalcuts)=="CountryCode")] <- "iso3"
povcalcuts$hc<- povcalcuts$P20Headcount
povcalcuts$extreme <- povcalcuts$ExtPovHC
keep <- c("iso3","RequestYear","surveyyr","hc","PovGap","filename","extreme","variable")
povcalcuts <- povcalcuts[,keep, with=F]

povcalcuts = subset(povcalcuts, !is.na(hc))
povcalcuts = povcalcuts[order(povcalcuts$filename,povcalcuts$RequestYear),]
povcalcuts=subset(povcalcuts, filename!="SNHR7IDT")
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
rm(grid,dhsmeta,dhsmeta2)
gc()

####Run function####
setwd(wd2)

missing.br = c(
  "aobr51fl.RData",
  "bfbr70fl.RData",
  "bubr6hfl.RData",
  "ghbr7afl.RData",
  "kebr7afl.RData",
  "lbbr61fl.RData",
  "lbbr71fl.RData",
  "mdbr71fl.RData",
  "mlbr70fl.RData",
  "mwbr6hfl.RData",
  "mwbr7ifl.RData",
  "mzbr51fl.RData",
  "ngbr72fl.RData",
  "rwbr6qfl.RData",
  "rwbr7afl.RData",
  "slbr71fl.RData",
  "snbr50fl.RData",
  "tzbr7ifl.RData", 
  "tzbr4afl.RData",
  "ugbr72fl.RData",
  "ugbr6afl.RData"
)
af=subset(povcalcuts, filename=="ALHR71DT")
af$filename="AFHR70DT"
af$iso3="AFG"
af$hc=0
af$PovGap=0
af$extreme=0

povcalcuts=rbind(af,povcalcuts)
#List of countries with subnational data
subnationalcountries=c("AFG","BGD","BFA","CMR","GHA","KEN","MDG","MWI","MDA","MOZ","MMR","NPL","NGA","RWA","SEN","TZA","UGA")
povcalcuts=subset(povcalcuts, iso3 %in% subnationalcountries)
  
rdatas = list.files(pattern="*.RData",ignore.case=T,recursive=T)
rdatas = substr(rdatas,1,nchar(rdatas)-6)
dataList <- list()
dataIndex <- 1
last_filename <- ""
pb = txtProgressBar(max=nrow(povcalcuts),style=3)
# Loop through every povcalcut
for(i in 1:nrow(povcalcuts)){
  povcal_subset = povcalcuts[i,]
  setTxtProgressBar(pb, i-1)
  # Pull some coded info out of the dir name
  country <- tolower(substr(povcal_subset$filename,1,2))
  recode <- tolower(substr(povcal_subset$filename,3,4))
  phase <- tolower(substr(povcal_subset$filename,5,6))
  subphase <- substr(povcal_subset$filename,5,5)
  rdata_name = paste0(country,recode,phase,"fl")
  variable <- tolower(povcal_subset$variable)
  if(substr(rdata_name,0,6) != last_filename){
    if(!(substr(rdata_name,0,6) != last_filename)){ next; }
    message(paste(rdata_name,povcal_subset$RequestYear))
    if(exists("pr")){rm(pr)}
    print(country)
    pr_patha <- paste0(country,"pr",phase)
    pr_path <- paste0(tolower(pr_patha),"fl.RData")
    load(pr_path)
    pr <- as.data.table(data)
    remove(data)
    keep <- c("hvidx","hhid","hv001","hv002","hv005","hv024","hv025","hv219","hv220","hv271","hv104","hv105","hv109","hv112","hv140","hc70","v106")
    pr <- subset(pr, select= (colnames(pr) %in% keep))
    gc()
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    names(pr)[which(names(pr)=="hvidx")] <- "line"
    if(exists("br")){rm(br)}
    br_patha <- paste0(country,"br",phase)
    br_path <- paste0(tolower(br_patha),"fl.RData")
    if(!(br_path %in% missing.br)){
      load(br_path)
      br <- as.data.table(data)
      remove(data)
      keep <- c("v001","v002","b3","v008","v005","b7","hw5","b4")
      br <- subset(br, select= (colnames(br) %in% keep))
      gc()
      names(br)[which(names(br)=="v001")] <- "cluster"
      names(br)[which(names(br)=="v002")] <- "household"
      names(br)[which(names(br)=="v005")] <- "sample.weights"
      br$weights <- br$sample.weights/1000000
    }else{
      br = data.frame(p20=NA, sex=NA)
    }
    
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
      wi_path <- paste0(tolower(wi_patha),"fl.RData")
      if(file.exists(wi_path)){
        load(wi_path)
        wi <- as.data.table(data)
        remove(data)
      }else{
        wi_patha <- paste0(country,"wi",(as.numeric(phase)-1)) #May be India-specific
        wi_path <- paste0(tolower(wi_patha),"fl.RData")
        if(file.exists(wi_path)){
          load(wi_path)
          wi <- as.data.table(data)
          remove(data)
        } else {
          next;
        }
      }
      setnames(wi,"whhid","hhid")
      pr<- join(pr,wi,by=c("hhid"))
      rm(wi)
      names(pr)[which(names(pr)=="wlthindf")] <-"wealth"
    }
    gc()
    
  # Education
    if(length(names(pr)[which(names(pr)=="hv109")])==1){
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
    pr$educ <- sapply(pr$educ,recode.educ)
    
    # Age
    names(pr)[which(names(pr)=="hv105")] <- "age"
    
    # Sex
    names(pr)[which(names(pr)=="hv104")] <- "sex"
    names(br)[which(names(br)=="b4")] <- "sex"
    
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
    
    #Child height for age
    if(length(names(pr)[which(names(pr)=="hc70")])){
      names(pr)[which(names(pr)=="hc70")] <- "child.height.age"
      pr$child.height.age <- pr$child.height.age/100}
    if(length(names(pr)[which(names(br)=="hw5")])){
      names(br)[which(names(br)=="hw5")] <- "child.height.age"
      br$child.height.age <- br$child.height.age/100}
   
  }
  
  # Poverty
  povcalcut <- povcal_subset$hc
  extcut <- povcal_subset$extreme
  cuts <- c(povcalcut,extcut)
  povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)
  pr$p20 <- (pr$wealth < povperc[1])
  pr$ext <- (pr$wealth < povperc[2])
  
  if(!(br_path %in% missing.br)){
    #Merge br
    pr.pov = pr[,.(p20=mean(p20,na.rm=T)),by=.(cluster,household)]
    pr.pov$p20 <- floor(pr.pov$p20)
    pr.pov$p20 <- as.logical(pr.pov$p20)
    br$p20 = NA
    br = merge(br[,p20:=NULL],pr.pov,by=c("cluster","household"),all.x=T)
  }
  # Birth certificate
  if(variable == "birth.registration"){
    #message("Registration")
    names(pr)[which(names(pr)=="hv140")] <- "birth.cert"
    #0 - neither certificate or registered
    #1 - has certificate
    #2 - registered, no certificate
    #3 - registered, no certificate
    #6 - other
    #8 - dk
    pr$birth.reg = NA
    pr$birth.reg[which(pr$birth.cert %in% c(0,6,8,9))] = 0
    pr$birth.reg[which(pr$birth.cert %in% c(1,2,3))] = 1
    
    dsn = svydesign(
      data=pr
      ,ids=~1
      ,weights=~weights
    )
    
    pov.reg.tab = svytable(~birth.reg+p20+sex,dsn)
    if("TRUE" %in% colnames(pov.reg.tab)){
      if("1" %in% rownames(pov.reg.tab)){
        p20.reg.m = pov.reg.tab["1","TRUE","1"]
        p20.reg.f = pov.reg.tab["1","TRUE","2"]
      }else{
        p20.reg.m = NA
        p20.reg.f = NA
      }
      if("0" %in% rownames(pov.reg.tab)){
        p20.non.reg.m = pov.reg.tab["0","TRUE","1"]
        p20.non.reg.f = pov.reg.tab["0","TRUE","2"]
      }else{
        p20.non.reg.m = NA
        p20.non.reg.f = NA
      }
      p20.reg.m.numerator = p20.reg.m
      p20.reg.m.denominator = sum(p20.reg.m,p20.non.reg.m,na.rm=T)
      p20.reg.m.stat = p20.reg.m.numerator/p20.reg.m.denominator
      p20.reg.f.numerator = p20.reg.f
      p20.reg.f.denominator = sum(p20.reg.f,p20.non.reg.f,na.rm=T)
      p20.reg.f.stat = p20.reg.f.numerator/p20.reg.f.denominator
    }else{
      p20.reg.m.stat = NA
      p20.reg.m.numerator = NA
      p20.reg.m.denominator = NA
      p20.reg.f.stat = NA
      p20.reg.f.numerator = NA
      p20.reg.f.denominator = NA
    }
    if("FALSE" %in% colnames(pov.reg.tab)){
      if("1" %in% rownames(pov.reg.tab)){
        u80.reg.m = pov.reg.tab["1","FALSE","1"]
        u80.reg.f = pov.reg.tab["1","FALSE","2"]
      }else{
        u80.reg.m = NA
        u80.reg.f = NA
      }
      if("0" %in% rownames(pov.reg.tab)){
        u80.non.reg.m = pov.reg.tab["0","FALSE","1"]
        u80.non.reg.f = pov.reg.tab["0","FALSE","2"]
      }else{
        u80.non.reg.m = NA
        u80.non.reg.f = NA
      }
      u80.reg.m.numerator = u80.reg.m
      u80.reg.m.denominator = sum(u80.reg.m,u80.non.reg.m,na.rm=T)
      u80.reg.m.stat = u80.reg.m.numerator/u80.reg.m.denominator
      u80.reg.f.numerator = u80.reg.f
      u80.reg.f.denominator = sum(u80.reg.f,u80.non.reg.f,na.rm=T)
      u80.reg.f.stat = u80.reg.f.numerator/u80.reg.f.denominator
    }else{
      u80.reg.m.stat = NA
      u80.reg.m.numerator = NA
      u80.reg.m.denominator = NA
      u80.reg.f.stat = NA
      u80.reg.f.numerator = NA
      u80.reg.f.denominator = NA
    }
    dat = data.frame(
      p20=c(rep(T,6),rep(F,6)),
      variable=c(rep("registration",12)),
      type=rep(c("statistic","numerator","denominator"),4),
      sex=rep(c(rep("male",3),rep("female",3)),2),
      value=c(p20.reg.m.stat,p20.reg.m.numerator,p20.reg.m.denominator,
              p20.reg.f.stat,p20.reg.f.numerator,p20.reg.f.denominator,
              u80.reg.m.stat,u80.reg.m.numerator,u80.reg.m.denominator,
              u80.reg.f.stat,u80.reg.f.numerator,u80.reg.f.denominator)
    )
  }
  
  # Stunting
  if(variable == "stunting"){
    #message("Stunting")
    if(!(typeof(pr$child.height.age)=="NULL")){
      pr$child.height.age[which(pr$child.height.age>80)] <- NA
      pr$stunting <- NA
      pr$stunting[which(pr$child.height.age > (-6) & pr$child.height.age<= (-3))] <- 1
      pr$stunting[which(pr$child.height.age > (-3) & pr$child.height.age<= (-2))] <- 1
      pr$stunting[which(pr$child.height.age > (-2) & pr$child.height.age< (6))] <- 0
      dsn = svydesign(
        data=pr
        ,ids=~1
        ,weights=~weights
      )
    } else {
      if(!(typeof(br$child.height.age)=="NULL")){
        br$child.height.age[which(br$child.height.age>80)] <- NA
        br$stunting <- NA
        br$stunting[which(br$child.height.age > (-6) & br$child.height.age<= (-3))] <- 1
        br$stunting[which(br$child.height.age > (-3) & br$child.height.age<= (-2))] <- 1
        br$stunting[which(br$child.height.age > (-2) & br$child.height.age< (6))] <- 0
        dsn = svydesign(
          data=br
          ,ids=~1
          ,weights=~weights
        )
      } else {
        br$stunting <- NA
        dsn = svydesign(
          data=rbind(br,br) #Quick and dirty fix for when br is missing
          ,ids=~1
          ,weights = ~1
        )
      }
    }
    pov.stunting.tab = svytable(~stunting+p20+sex,dsn)
    if("TRUE" %in% colnames(pov.stunting.tab)){
      if("1" %in% rownames(pov.stunting.tab)){
        p20.stunting.m = pov.stunting.tab["1","TRUE","1"]
        p20.stunting.f = pov.stunting.tab["1","TRUE","2"]
      }else{
        p20.stunting.m = NA
        p20.stunting.f = NA
      }
      if("0" %in% rownames(pov.stunting.tab)){
        p20.non.stunting.m = pov.stunting.tab["0","TRUE","1"]
        p20.non.stunting.f = pov.stunting.tab["0","TRUE","2"]
      }else{
        p20.non.stunting.m = NA
        p20.non.stunting.f = NA
      }
      p20.stunting.m.numerator = p20.stunting.m
      p20.stunting.m.denominator = sum(p20.stunting.m,p20.non.stunting.m,na.rm=T)
      p20.stunting.m.stat = p20.stunting.m.numerator/p20.stunting.m.denominator
      p20.stunting.f.numerator = p20.stunting.f
      p20.stunting.f.denominator = sum(p20.stunting.f,p20.non.stunting.f,na.rm=T)
      p20.stunting.f.stat = p20.stunting.f.numerator/p20.stunting.f.denominator
    }else{
      p20.stunting.m.stat = NA
      p20.stunting.m.numerator = NA
      p20.stunting.m.denominator = NA
      p20.stunting.f.stat = NA
      p20.stunting.f.numerator = NA
      p20.stunting.f.denominator = NA
    }
    if("FALSE" %in% colnames(pov.stunting.tab)){
      if("1" %in% rownames(pov.stunting.tab)){
        u80.stunting.m = pov.stunting.tab["1","FALSE","1"]
        u80.stunting.f = pov.stunting.tab["1","FALSE","2"]
      }else{
        u80.stunting.m = NA
        u80.stunting.f = NA
      }
      if("0" %in% rownames(pov.stunting.tab)){
        u80.non.stunting.m = pov.stunting.tab["0","FALSE","1"]
        u80.non.stunting.f = pov.stunting.tab["0","FALSE","2"]
      }else{
        u80.non.stunting.m = NA
        u80.non.stunting.f = NA
      }
      u80.stunting.m.numerator = u80.stunting.m
      u80.stunting.m.denominator = sum(u80.stunting.m,u80.non.stunting.m,na.rm=T)
      u80.stunting.m.stat = u80.stunting.m.numerator/u80.stunting.m.denominator
      u80.stunting.f.numerator = u80.stunting.f
      u80.stunting.f.denominator = sum(u80.stunting.f,u80.non.stunting.f,na.rm=T)
      u80.stunting.f.stat = u80.stunting.f.numerator/u80.stunting.f.denominator
    }else{
      u80.stunting.m.stat = NA
      u80.stunting.m.numerator = NA
      u80.stunting.m.denominator = NA
      u80.stunting.f.stat = NA
      u80.stunting.f.numerator = NA
      u80.stunting.f.denominator = NA
    }
    dat = data.frame(
      p20=c(rep(T,6),rep(F,6)),
      variable=c(rep("stunting",12)),
      type=rep(c("statistic","numerator","denominator"),4),
      sex=rep(c(rep("male",3),rep("female",3)),2),
      value=c(p20.stunting.m.stat,p20.stunting.m.numerator,p20.stunting.m.denominator,
              p20.stunting.f.stat,p20.stunting.f.numerator,p20.stunting.f.denominator,
              u80.stunting.m.stat,u80.stunting.m.numerator,u80.stunting.m.denominator,
              u80.stunting.f.stat,u80.stunting.f.numerator,u80.stunting.f.denominator)
    )
  }
  
  #Mortality
  if(variable == "u5.mortality"){
    #message("Mortality")
    br.p20.m = subset(br,p20==T & sex==1)
    br.u80.m = subset(br,p20==F & sex==1)
    br.p20.f = subset(br,p20==T & sex==2)
    br.u80.f = subset(br,p20==F & sex==2)
    gc()
    if(nrow(br.p20.m)>1){
      p20.mort.list.m = mort(br.p20.m)
      p20.mort.m = p20.mort.list.m$mortality
      p20.mort.m.numerator = p20.mort.list.m$total_morts
      p20.mort.m.denominator = p20.mort.list.m$total_survs
    }else{
      p20.mort.m = NA
      p20.mort.m.numerator = NA
      p20.mort.m.denominator = NA
    }
    if(nrow(br.p20.f)>1){
      p20.mort.list.f = mort(br.p20.f)
      p20.mort.f = p20.mort.list.f$mortality
      p20.mort.f.numerator = p20.mort.list.f$total_morts
      p20.mort.f.denominator = p20.mort.list.f$total_survs
    }else{
      p20.mort.f = NA
      p20.mort.f.numerator = NA
      p20.mort.f.denominator = NA
    }
    if(nrow(br.u80.m)>1){
      u80.mort.m.list = mort(br.u80.m)
      u80.mort.m = u80.mort.m.list$mortality
      u80.mort.m.numerator = u80.mort.m.list$total_morts
      u80.mort.m.denominator = u80.mort.m.list$total_survs
    }else{
      u80.mort.m = NA
      u80.mort.m.numerator = NA
      u80.mort.m.denominator = NA
    }
    if(nrow(br.u80.f)>1){
      u80.mort.f.list = mort(br.u80.f)
      u80.mort.f = u80.mort.f.list$mortality
      u80.mort.f.numerator = u80.mort.f.list$total_morts
      u80.mort.f.denominator = u80.mort.f.list$total_survs
    }else{
      u80.mort.f = NA
      u80.mort.f.numerator = NA
      u80.mort.f.denominator = NA
    }
    gc()
    dat = data.frame(
      p20=c(rep(T,6),rep(F,6)),
      variable=c(rep("mortality",12)),
      type=rep(c("statistic","numerator","denominator"),4),
      sex=rep(c(rep("male",3),rep("female",3)),2),
      value=c(p20.mort.m,p20.mort.m.numerator,p20.mort.m.denominator,
              p20.mort.f,p20.mort.f.numerator,p20.mort.f.denominator,
              u80.mort.m,u80.mort.m.numerator,u80.mort.m.denominator,
              u80.mort.f,u80.mort.f.numerator,u80.mort.f.denominator)
    )
  }
  
  #Secondary Education
  if(variable == "education"){
    #message("Education")
    pr$secedu <- NA
    pr$secedu[which(!is.na(pr$educ) & pr$age >= 21)] <- 0
    pr$secedu[which(pr$educ %in% c("Secondary","Higher") & pr$age >= 21)] <- 1
    dsn = svydesign(
      data=pr
      ,ids=~1
      ,weights=~weights
    )
    pov.education.tab = svytable(~secedu+p20+sex,dsn)
    if("TRUE" %in% colnames(pov.education.tab)){
      if("1" %in% rownames(pov.education.tab)){
        p20.education.m = pov.education.tab["1","TRUE","1"]
        p20.education.f = pov.education.tab["1","TRUE","2"]
      }else{
        p20.education.m = NA
        p20.education.f = NA
      }
      if("0" %in% rownames(pov.education.tab)){
        p20.non.education.m = pov.education.tab["0","TRUE","1"]
        p20.non.education.f = pov.education.tab["0","TRUE","2"]
      }else{
        p20.non.education.m = NA
        p20.non.education.f = NA
      }
      p20.education.m.numerator = p20.education.m
      p20.education.m.denominator = sum(p20.education.m,p20.non.education.m,na.rm=T)
      p20.education.m.stat = p20.education.m.numerator/p20.education.m.denominator
      p20.education.f.numerator = p20.education.f
      p20.education.f.denominator = sum(p20.education.f,p20.non.education.f,na.rm=T)
      p20.education.f.stat = p20.education.f.numerator/p20.education.f.denominator
    }else{
      p20.education.m.stat = NA
      p20.education.m.numerator = NA
      p20.education.m.denominator = NA
      p20.education.f.stat = NA
      p20.education.f.numerator = NA
      p20.education.f.denominator = NA
    }
    if("FALSE" %in% colnames(pov.education.tab)){
      if("1" %in% rownames(pov.education.tab)){
        u80.education.m = pov.education.tab["1","FALSE","1"]
        u80.education.f = pov.education.tab["1","FALSE","2"]
      }else{
        u80.education.m = NA
        u80.education.f = NA
      }
      if("0" %in% rownames(pov.education.tab)){
        u80.non.education.m = pov.education.tab["0","FALSE","1"]
        u80.non.education.f = pov.education.tab["0","FALSE","2"]
      }else{
        u80.non.education.m = NA
        u80.non.education.f = NA
      }
      u80.education.m.numerator = u80.education.m
      u80.education.m.denominator = sum(u80.education.m,u80.non.education.m,na.rm=T)
      u80.education.m.stat = u80.education.m.numerator/u80.education.m.denominator
      u80.education.f.numerator = u80.education.f
      u80.education.f.denominator = sum(u80.education.f,u80.non.education.f,na.rm=T)
      u80.education.f.stat = u80.education.f.numerator/u80.education.f.denominator
    }else{
      u80.education.m.stat = NA
      u80.education.m.numerator = NA
      u80.education.m.denominator = NA
      u80.education.f.stat = NA
      u80.education.f.numerator = NA
      u80.education.f.denominator = NA
    }
    dat = data.frame(
      p20=c(rep(T,6),rep(F,6)),
      variable=c(rep("education",12)),
      type=rep(c("statistic","numerator","denominator"),4),
      sex=rep(c(rep("male",3),rep("female",3)),2),
      value=c(p20.education.m.stat,p20.education.m.numerator,p20.education.m.denominator,
              p20.education.f.stat,p20.education.f.numerator,p20.education.f.denominator,
              u80.education.m.stat,u80.education.m.numerator,u80.education.m.denominator,
              u80.education.f.stat,u80.education.f.numerator,u80.education.f.denominator)
    )
  }
  
  dat$iso3 = povcal_subset$iso3
  dat$povcal_year = povcal_subset$RequestYear
  dat$survey_year = povcal_subset$surveyyr
  last_filename = tolower(substr(povcal_subset$filename,0,6))
  dataList[[dataIndex]] <- dat
  dataIndex <- dataIndex + 1
  }


setTxtProgressBar(pb, i)
close(pb)
data.total <- rbindlist(dataList)
save(data.total,file="../historical_allrowssubnational.RData")
#Weightings
data.total$diff = abs(data.total$survey_year - data.total$povcal_year)
data.total$diff[which(is.na(data.total$value))] = NA
data.total$diff.sign = sign(data.total$survey_year - data.total$povcal_year)
pos.data.total = subset(data.total,diff.sign %in% c(0,1))
neg.data.total = subset(data.total,diff.sign %in% c(0,-1))
pos.data.total = data.table(pos.data.total)[,.SD[which.min(.SD$diff),],by=.(iso3,povcal_year, region, variable, p20, type, sex)]
neg.data.total = data.table(neg.data.total)[,.SD[which.min(.SD$diff),],by=.(iso3,povcal_year, region,variable, p20, type, sex)]
neg.data.total = subset(neg.data.total,diff!=0)
data.total = rbind(pos.data.total,neg.data.total)
data.total[,year.weight:=(sum(.SD$diff)-.SD$diff)/sum(.SD$diff),by=.(iso3,povcal_year, variable, region, p20, type, sex)]
data.total$diff = NULL
data.total$diff.sign = NULL
data.total$year.weight[which(data.total$year.weight==0)] = 1
data.total$year.weight[which(is.nan(data.total$year.weight))] = 1

data.total = data.total[,.(
  value=sum(.SD$value*.SD$year.weight,na.rm=T)/sum(.SD$year.weight),
  survey_year=paste(.SD$survey_year,collapse=";")
)
,by=.(p20,variable,type,iso3,povcal_year,region, sex)
]

save(data.total,file="../historical_dhsmf.RData")
fwrite(data.total,"../historical_dhsmf.csv")
