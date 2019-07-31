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

variables <- c("birth.registration","u5.mortality","stunting","education")
grid = as.data.table(expand.grid(filename=unique(dhsmeta2$filename), variable = variables))

dhsmeta2 <- merge(grid, dhsmeta2, all=T)

isos = povcalcuts[,c("CountryName","CountryCode")]
names(isos)[which(names(isos)=="CountryCode")] <- "iso3"
isos=unique(isos)
af=data.frame(CountryName="Afghanistan",iso3="AFG")
isos=rbind(isos,af)
povcalcuts <- join(dhsmeta2,isos,by=c("CountryName"))

keep <- c("iso3","surveyyr","filename","variable")
povcalcuts <- povcalcuts[,keep, with=F]


povcalcuts = povcalcuts[order(povcalcuts$filename,povcalcuts$surveyyr),]
povcalcuts=subset(povcalcuts, filename!="SNHR7IDT")


label.region=function(region.vals,region.labs){
  reg.lab.list=list()
  for(i in 1:length(region.labs)){
    reg.name=names(region.labs)[i]
    reg.lab=as.character(region.labs[i])
    reg.lab.list[[reg.lab]]=reg.name
  } 
  return(unlist(reg.lab.list[as.character(region.vals)]))
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
    names(attributes(data)$label.table)=toupper(names(attributes(data)$label.table))
    reg.labs=attributes(data)$label.table$HV024
    data$region=label.region(data$hv024,reg.labs)
    pr <- as.data.table(data)
    remove(data)
    keep <- c("hvidx","hhid","hv001","hv002","hv005","region","hv025","hv219","hv220","hv271","hv104","hv105","hv109","hv112","hv140","hc70","v106")
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
      names(attributes(data)$label.table)=toupper(names(attributes(data)$label.table))
      data$region=label.region(data$v024,reg.labs)
      br <- as.data.table(data)
      remove(data)
      keep <- c("v001","v002","b3","v008","v005","b7","hw5","b4","region")
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
  
 
  pr.backup=copy(pr)
  br.backup=copy(br)
  regions=unique(pr$region)
  for(this.region in regions){
    pr=copy(pr.backup)
    br=copy(br.backup)
    pr=subset(pr,this.region==region)
    if("region" %in% names(br)){
      br=subset(br,this.region==region)
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
      
      reg.tab = svytable(~birth.reg+sex,dsn)
      
      if("1" %in% rownames(reg.tab)){
        reg.m = reg.tab["1","1"]
        reg.f = reg.tab["1","2"]
      }else{
        reg.m = NA
        reg.f = NA
      }
      if("0" %in% rownames(reg.tab)){
        non.reg.m = reg.tab["0","1"]
        non.reg.f = reg.tab["0","2"]
      }else{
        non.reg.m = NA
        non.reg.f = NA
      }
      reg.m.numerator = reg.m
      reg.m.denominator = sum(reg.m,non.reg.m,na.rm=T)
      reg.m.stat = reg.m.numerator/reg.m.denominator
      reg.f.numerator = reg.f
      reg.f.denominator = sum(reg.f,non.reg.f,na.rm=T)
      reg.f.stat = reg.f.numerator/reg.f.denominator
   
     
      dat = data.frame(
        variable=c(rep("registration",6)),
        type=rep(c("statistic","numerator","denominator"),2),
        sex=rep(c(rep("male",3),rep("female",3)),1),
        value=c(reg.m.stat,reg.m.numerator,reg.m.denominator,
                reg.f.stat,reg.f.numerator,reg.f.denominator)
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
      stunting.tab = svytable(~stunting+sex,dsn)
      if("1" %in% rownames(stunting.tab)){
        stunting.m = stunting.tab["1","1"]
        stunting.f = stunting.tab["1","2"]
      }else{
        stunting.m = NA
        stunting.f = NA
      }
      if("0" %in% rownames(stunting.tab)){
        non.stunting.m = stunting.tab["0","1"]
        non.stunting.f = stunting.tab["0","2"]
      }else{
        non.stunting.m = NA
        non.stunting.f = NA
      }
      stunting.m.numerator = stunting.m
      stunting.m.denominator = sum(stunting.m,non.stunting.m,na.rm=T)
      stunting.m.stat = stunting.m.numerator/stunting.m.denominator
      stunting.f.numerator = stunting.f
      stunting.f.denominator = sum(stunting.f,non.stunting.f,na.rm=T)
      stunting.f.stat = stunting.f.numerator/stunting.f.denominator
      
      
      dat = data.frame(
     
        variable=c(rep("stunting",6)),
        type=rep(c("statistic","numerator","denominator"),2),
        sex=rep(c(rep("male",3),rep("female",3)),1),
        value=c(stunting.m.stat,stunting.m.numerator,stunting.m.denominator,
                stunting.f.stat,stunting.f.numerator,stunting.f.denominator)
      )
    }
    
    #Mortality
    if(variable == "u5.mortality"){
      #message("Mortality")
      br.m = subset(br, sex==1)
      
      br.f = subset(br,sex==2)
      
      gc()
      if(nrow(br.m)>1){
        mort.list.m = mort(br.m)
        mort.m = mort.list.m$mortality
        mort.m.numerator = mort.list.m$total_morts
        mort.m.denominator = mort.list.m$total_survs
      }else{
        mort.m = NA
        mort.m.numerator = NA
        mort.m.denominator = NA
      }
      if(nrow(br.f)>1){
        mort.list.f = mort(br.f)
        mort.f = mort.list.f$mortality
        mort.f.numerator = mort.list.f$total_morts
        mort.f.denominator = mort.list.f$total_survs
      }else{
        mort.f = NA
        mort.f.numerator = NA
        mort.f.denominator = NA
      }
      
      gc()
      dat = data.frame(
        variable=c(rep("mortality",6)),
        type=rep(c("statistic","numerator","denominator"),2),
        sex=rep(c(rep("male",3),rep("female",3)),1),
        value=c(mort.m,mort.m.numerator,mort.m.denominator,
                mort.f,mort.f.numerator,mort.f.denominator)
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
      education.tab = svytable(~secedu+sex,dsn)
        if("1" %in% rownames(education.tab)){
          education.m = education.tab["1","1"]
          education.f = education.tab["1","2"]
        }else{
          education.m = NA
          education.f = NA
        }
        if("0" %in% rownames(education.tab)){
          non.education.m = education.tab["0","1"]
          non.education.f = education.tab["0","2"]
        }else{
          non.education.m = NA
          non.education.f = NA
        }
        education.m.numerator = education.m
        education.m.denominator = sum(education.m,non.education.m,na.rm=T)
        education.m.stat = education.m.numerator/education.m.denominator
        education.f.numerator = education.f
        education.f.denominator = sum(education.f,non.education.f,na.rm=T)
        education.f.stat = education.f.numerator/education.f.denominator
      
      
      dat = data.frame(
        variable=c(rep("education",6)),
        type=rep(c("statistic","numerator","denominator"),2),
        sex=rep(c(rep("male",3),rep("female",3)),1),
        value=c(education.m.stat,education.m.numerator,education.m.denominator,
                education.f.stat,education.f.numerator,education.f.denominator)
      )
    }
    
    dat$iso3 = povcal_subset$iso3
    dat$survey_year = povcal_subset$surveyyr
    dat$region=this.region
    last_filename = tolower(substr(povcal_subset$filename,0,6))
    dataList[[dataIndex]] <- dat
    dataIndex <- dataIndex + 1
  }
  
  }


setTxtProgressBar(pb, i)
close(pb)
data.total <- rbindlist(dataList)
save(data.total,file="../historical_allrowssubnational.RData")

setwd(wd)
save(data.total,file="data/historical_dhssubgender.RData")
fwrite(data.total,"data/historical_dhssubgender.csv")
data.total.num=subset(data.total,type=="numerator")
setnames(data.total.num,"value","numerator")
data.total.den=subset(data.total,type=="denominator")
setnames(data.total.den,"value","denominator")
data.total.wide=join(data.total.num,data.total.den,by=c("variable","sex","iso3","survey_year", "region"))
data.total.wide2=data.table(data.total.wide)[,.(
                                             denominator=sum(denominator)
                                             ,numerator=sum(numerator))
                                             ,by=c("region","variable","iso3","survey_year")]
data.total.wide2$value=data.total.wide2$numerator/data.total.wide2$denominator
fwrite(data.total.wide2,"data/historical_dhs_sub.csv")
