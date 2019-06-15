####Function and setup####
list.of.packages <- c("Hmisc","plyr","foreign","data.table","varhandle","zoo","survey")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

#Taken from https://raw.githubusercontent.com/akmiller01/alexm-util/master/DevInit/R/P20/2013_tab_data2.R

if(Sys.info()[["user"]]=="alex"){
  wd <- "~/git/p20_indicator_time_trends"
  wd2 <- "~/git/p20_private_data/project_data/DHS auto"
}else if(Sys.info()[["user"]]=="dan-w"){
  wd <- "C:/Users/dan-w/Box/Gap Narrative (ITEP), June 2019/git/p20_indicator_time_trends"
  wd2 <- "C:/Users/dan-w/Box/Gap Narrative (ITEP), June 2019/git/p20_indicator_time_trends/data"
}else{
  wd <- "E:/DHSauto"
  wd2 <- "~/git/p20_private_data/project_data/"
}

setwd(wd)

source("code/child_mort.R")
povcalcuts <- fread("https://raw.githubusercontent.com/ZChristensen/poverty_trends/master/data/P20incometrends.csv")
dhsmeta<- fread("C:/Users/dan-w/Box/Gap Narrative (ITEP), June 2019/git/p20_indicator_time_trends/data/dhs_meta_data20190524.csv")
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

dhsmeta2 <- unique(dhsmeta[,c("CountryName","surveyyr","filename","Birth.registration","Women.s.status","Anthropometry")])
povcalyears=c(1981,1984,1987,1990,1993,1996,1999,2002,2005,2008,2010,2011,2012,2013,2015)

variables <- c("Birth.registration","Women.s.status","Anthropometry")
grid = as.data.table(expand.grid(filename=unique(dhsmeta2$filename), surveyyr=unique(dhsmeta2$surveyyr), RequestYear=povcalyears, variable = variables))
dhsmeta2.br <- merge(grid[which(grid$variable == "Birth.registration")], dhsmeta2[which(dhsmeta2$Birth.registration==1)], all=T)
dhsmeta2.ws <- merge(grid[which(grid$variable == "Women.s.status")], dhsmeta2[which(dhsmeta2$Women.s.status==1)], all=T)
dhsmeta2.an <- merge(grid[which(grid$variable == "Anthropometry")], dhsmeta2[which(dhsmeta2$Anthropometry==1)], all=T)
dhsmeta2 <- rbind(dhsmeta2.br,dhsmeta2.an,dhsmeta2.ws)

dhsmeta2$diff = abs(dhsmeta2$surveyyr - dhsmeta2$RequestYear)
dhsmeta2$diff.sign = sign(dhsmeta2$surveyyr - dhsmeta2$RequestYear)
pos.dhsmeta2 = subset(dhsmeta2,diff.sign %in% c(0,1))
neg.dhsmeta2 = subset(dhsmeta2,diff.sign %in% c(0,-1))
pos.dhsmeta2 = data.table(pos.dhsmeta2)[,.SD[which.min(.SD$diff),],by=.(CountryName,RequestYear, variable)]
neg.dhsmeta2 = data.table(neg.dhsmeta2)[,.SD[which.min(.SD$diff),],by=.(CountryName,RequestYear, variable)]
neg.dhsmeta2 = subset(neg.dhsmeta2,diff!=0)
dhsmeta2 = rbind(pos.dhsmeta2,neg.dhsmeta2)
dhsmeta2[,year.weight:=(sum(.SD$diff)-.SD$diff)/sum(.SD$diff),by=.(CountryName,RequestYear, variable)]
dhsmeta2$diff = NULL
dhsmeta2$diff.sign = NULL
dhsmeta2$year.weight[which(dhsmeta2$year.weight==0)] = 1
dhsmeta2$year.weight[which(is.nan(dhsmeta2$year.weight))] = 1

povcalcuts <- join(dhsmeta2,povcalcuts,by=c("CountryName","RequestYear"))

names(povcalcuts)[which(names(povcalcuts)=="CountryCode")] <- "iso3"
povcalcuts$hc<- povcalcuts$P20Headcount/100
povcalcuts$extreme <- povcalcuts$ExtPovHC/100
keep <- c("iso3","RequestYear","surveyyr","hc","PovGap","filename","extreme","year.weight","variable")
povcalcuts <- povcalcuts[,keep, with=F]
povcalcuts = subset(povcalcuts, !is.na(hc))
povcalcuts = povcalcuts[order(povcalcuts$filename,povcalcuts$RequestYear),]

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
rm(grid,dhsmeta2.an,dhsmeta2.br,dhsmeta2.ws,neg.dhsmeta2,pos.dhsmeta2,dhsmeta)
gc()

####Run function####
setwd(wd2)

rdatas = list.files(pattern="*.RData",ignore.case=T)
rdatas = substr(rdatas,1,nchar(rdatas)-6)

dataList <- list()
dataIndex <- 1
#pb = txtProgressBar(max=nrow(povcalcuts),style=3)
# Loop through every povcalcut
for(i in 1:nrow(povcalcuts)){
  #setTxtProgressBar(pb, i)
  povcal_subset = povcalcuts[i,]
  # Pull some coded info out of the dir name
  country <- tolower(substr(povcal_subset$filename,1,2))
  recode <- tolower(substr(povcal_subset$filename,3,4))
  phase <- tolower(substr(povcal_subset$filename,5,6))
  subphase <- substr(povcal_subset$filename,5,5)
  rdata_name = paste0(country,recode,phase,"fl")
  variable <- tolower(povcal_subset$variable)
  if(rdata_name %in% rdatas){
    message(paste(rdata_name,povcal_subset$RequestYear))
    if(exists("pr")){rm(pr)}
    
    pr_patha <- paste0(country,"pr",phase)
    pr_path <- paste0(tolower(pr_patha),"fl.RData")
    load(pr_path)
    pr <- as.data.table(data)
    remove(data)
    keep <- c("hvidx","hv001","hv002","hv005","hv024","hv025","hv219","hv220","hv271","hv104","hv105","hv109","hv112","hv140","hc70")
    pr <- subset(pr, select= (colnames(pr) %in% keep))
    gc()
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    names(pr)[which(names(pr)=="hvidx")] <- "line"
    if(!("hhid" %in% names(pr))){
      pr$hhid <- paste(floor(pr$cluster/1000),pr$line,pr$household)
    }
    #Rename sample.weights var
    names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
    pr$weights <- pr$sample.weights/1000000
    gc()
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
      wi_patha <- paste0(country,"wi",(as.numeric(phase)-1))
      wi_path <- paste0(tolower(wi_patha),"fl.RData")
      if(file.exists(wi_path)){
        load(wi_path)
        wi <- as.data.table(data)
        remove(data)
      }else{
        next;
      }
      names(wi)[which(names(wi)=="whhid")] <-"hhid"
      pr<- join(pr,wi,by="hhid")
      rm(wi)
      names(pr)[which(names(pr)=="wlthindf")] <-"wealth"
    }
    gc()
    # Poverty
    povcalcut <- povcal_subset$hc
    extcut <- povcal_subset$extreme
    cuts <- c(povcalcut,extcut)
    povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)
    
    pr$p20 <- (pr$wealth < povperc[1])
    pr$ext <- (pr$wealth < povperc[2])
    gc()
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
    if(variable == "birth.registration"){
      message("Registration")
      names(pr)[which(names(pr)=="hv140")] <- "birth.cert"
      keep <- c(
        "wealth","weights","urban","region","educ","age","sex","cluster","household","head.sex","head.age","p20","ext","birth.cert"
      )
      pr <- subset(pr, select=(colnames(pr) %in% keep))
      gc()
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
      
      pov.reg.tab = svytable(~birth.reg+p20,dsn)
      if("TRUE" %in% colnames(pov.reg.tab)){
        p20.reg = pov.reg.tab["1","TRUE"]/sum(pov.reg.tab["0","TRUE"],pov.reg.tab["1","TRUE"],na.rm=T)
        p20.reg.numerator = pov.reg.tab["1","TRUE"]
        p20.reg.denominator = sum(pov.reg.tab["0","TRUE"],pov.reg.tab["1","TRUE"],na.rm=T)
      }else{
        p20.reg = NA
        p20.reg.numerator = NA
        p20.reg.denominator = NA
      }
      if("FALSE" %in% colnames(pov.reg.tab)){
        u80.reg = pov.reg.tab["1","FALSE"]/sum(pov.reg.tab["0","FALSE"],pov.reg.tab["1","FALSE"],na.rm=T)
        u80.reg.numerator = pov.reg.tab["1","FALSE"]
        u80.reg.denominator = sum(pov.reg.tab["0","FALSE"],pov.reg.tab["1","FALSE"],na.rm=T)
      }else{
        u80.reg = NA
        u80.reg.numerator = NA
        u80.reg.denominator = NA
      }
      dat = data.frame(
        p20=c(rep(T,3),rep(F,3)),
        variable=c(rep("registration",6)),
        type=rep(c("statistic","numerator","denominator"),2),
        value=c(p20.reg,p20.reg.numerator,p20.reg.denominator,u80.reg,u80.reg.numerator,u80.reg.denominator)
      )
    }
    
    # Stunting
    
    if(variable == "anthropometry"){
      message("Stunting")
      names(pr)[which(names(pr)=="hc70")] <- "child.height.age"
      if(typeof(pr$child.height.age)=="NULL"){
        pr$child.height.age <- NA
      }else{
        pr$child.height.age <- pr$child.height.age/100
      }
      pr$child.height.age[which(pr$child.height.age>80)] <- NA
      pr$stunting <- NA
      pr$stunting[which(pr$child.height.age > (-6) & pr$child.height.age<= (-3))] <- 1
      pr$stunting[which(pr$child.height.age > (-3) & pr$child.height.age<= (-2))] <- 1
      pr$stunting[which(pr$child.height.age > (-2) & pr$child.height.age< (6))] <- 0
      keep <- c(
        "wealth","weights","urban","region","educ","age","sex","cluster","household","head.sex","head.age","p20","ext","birth.reg","stunting"
      )
      pr <- subset(pr, select=(colnames(pr) %in% keep))
      gc()
      dsn = svydesign(
        data=pr
        ,ids=~1
        ,weights=~weights
      )
      pov.stunting.tab = svytable(~stunting+p20,dsn)
      if("TRUE" %in% colnames(pov.stunting.tab)){
        p20.stunting = pov.stunting.tab["1","TRUE"]/sum(pov.stunting.tab["0","TRUE"],pov.stunting.tab["1","TRUE"],na.rm=T)
        p20.stunting.numerator = pov.stunting.tab["1","TRUE"]
        p20.stunting.denominator = sum(pov.stunting.tab["0","TRUE"],pov.stunting.tab["1","TRUE"],na.rm=T)
      }else{
        p20.stunting = NA
        p20.stunting.numerator = NA
        p20.stunting.denominator = NA
      }
      if("FALSE" %in% colnames(pov.stunting.tab)){
        u80.stunting = pov.stunting.tab["1","FALSE"]/sum(pov.stunting.tab["0","FALSE"],pov.stunting.tab["1","FALSE"],na.rm=T)
        u80.stunting.numerator = pov.stunting.tab["1","FALSE"]
        u80.stunting.denominator = sum(pov.stunting.tab["0","FALSE"],pov.stunting.tab["1","FALSE"],na.rm=T)
      }else{
        u80.stunting = NA
        u80.stunting.numerator = NA
        u80.stunting.denominator = NA
      }
      dat = data.frame(
        p20=c(rep(T,3),rep(F,3)),
        variable=c(rep("stunting",6)),
        type=rep(c("statistic","numerator","denominator"),2),
        value=c(p20.stunting,p20.stunting.numerator,p20.stunting.denominator,u80.stunting,u80.stunting.numerator,u80.stunting.denominator)
      )
}
    
    #Mortality    
    if(variable == "women.s.status"){
      message("Mortality")
      keep <- c(
        "wealth","weights","urban","region","educ","age","sex","cluster","household","head.sex","head.age","p20","ext","birth.reg","stunting"
      )
      pr <- subset(pr, select=(colnames(pr) %in% keep))
      gc()
      if(exists("br")){rm(br)}
      br_patha <- paste0(country,"br",phase)
      br_path <- paste0(tolower(br_patha),"fl.RData")
      load(br_path)
      br <- as.data.table(data)
      remove(data)
      keep <- c("v001","v002","b3","v008","v005","b7")
      br <- subset(br, select= (colnames(br) %in% keep))
      gc()
      names(br)[which(names(br)=="v001")] <- "cluster"
      names(br)[which(names(br)=="v002")] <- "household"
      pr.pov = pr[,.(p20=mean(p20,na.rm=T)),by=.(cluster,household)]
      rm(pr, dsn)
      gc()
      br = merge(br,pr.pov,by=c("cluster","household"),all.x=T)
      br.p20 = subset(br,p20==T)
      br.u80 = subset(br,!p20)
      rm(br)
      gc()
      if(nrow(br.p20)>1){
        p20.mort.list = mort(br.p20)
        p20.mort = p20.mort.list$mortality
        p20.mort.numerator = p20.mort.list$total_morts
        p20.mort.denominator = p20.mort.list$total_survs
      }else{
        p20.mort = NA
        p20.mort.numerator = NA
        p20.mort.denominator = NA
      }
      if(nrow(br.u80)>1){
        u80.mort.list = mort(br.u80)
        u80.mort = u80.mort.list$mortality
        u80.mort.numerator = u80.mort.list$total_morts
        u80.mort.denominator = u80.mort.list$total_survs
      }else{
        u80.mort = NA
        u80.mort.numerator = NA
        u80.mort.denominator = NA
      }
      gc()
      dat = data.frame(
        p20=c(rep(T,3),rep(F,3)),
        variable=c(rep("mortality",6)),
        type=rep(c("statistic","numerator","denominator"),2),
        value=c(p20.mort,p20.mort.numerator,p20.mort.denominator,u80.mort,u80.mort.numerator,u80.mort.denominator)
      )
    }
    
    dat$iso3 = povcal_subset$iso3
    dat$povcal_year = povcal_subset$RequestYear
    dat$survey_year = povcal_subset$surveyyr
    dat$filename = povcal_subset$filename
    dat$year.weight = povcal_subset$year.weight

    dataList[[dataIndex]] <- dat
    dataIndex <- dataIndex + 1
  }
}
#close(pb)
data.total <- rbindlist(dataList)
data.total = data.total[,.(
  value=sum(.SD$value*.SD$year.weight,na.rm=T)/sum(.SD$year.weight),
  survey_year=paste(.SD$survey_year,collapse=";")
  )
  ,by=.(p20,variable,type,iso3,povcal_year)
]

save(data.total,file="../historical_dhs.RData")
fwrite(data.total,"../historical_dhs.csv")
