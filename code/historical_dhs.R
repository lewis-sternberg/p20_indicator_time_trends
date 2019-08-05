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

wd <- "C:/"
wd2 <- "C:/DHS data R"
}

setwd(wd)

source("child_mort.r")
dhsmeta <- fread("C:/dhs_meta_data20190524.csv")
povcalcuts<- fread("p20incometrends.csv")
dhsmeta<- subset(dhsmeta, Recode.Structure.="DHS-I")

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
povcalyears=c(1981,1984,1987,1990,1993,1996,1999,2002,2005,2008,2010,2011,2012,2013,2015)
for(year in povcalyears){
  dhsmeta2[, as.character(year)] <- abs(dhsmeta2$surveyyr - year)
}

dhsmeta2 <- melt(dhsmeta2, id.vars = c("filename","CountryName","surveyyr"))
dhsmeta2 <- dhsmeta2[dhsmeta2[, .I[value == min(value)], by=.(CountryName,variable)]$V1]
dhsmeta2 <- dhsmeta2[complete.cases(dhsmeta2)]
dhsmeta2$variable<- as.numeric(levels(dhsmeta2$variable))[dhsmeta2$variable]
names(dhsmeta2)[which(names(dhsmeta2)=="variable")] <- "RequestYear"

povcalcuts <- join(dhsmeta2,povcalcuts,by=c("CountryName","RequestYear"))

names(povcalcuts)[which(names(povcalcuts)=="RequestYear")] <- "year"
names(povcalcuts)[which(names(povcalcuts)=="CountryCode")] <- "iso3"
povcalcuts$hc<- povcalcuts$P20Headcount/100
povcalcuts$extreme <- povcalcuts$ExtPovHC/100
keep <- c("iso3","year","hc","PovGap","filename","extreme")
povcalcuts <- povcalcuts[,keep, with=F]
povcalcuts=subset(povcalcuts, !is.na(hc))
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

setwd(wd2)

rdatas <- list.files(pattern="*.RData",ignore.case=T,full.names=TRUE)


dataList <- list()
dataIndex <- 1




# Loop through every dir
for(i in 1:length(rdatas)){
  rdata <- rdatas[i]
  message('current i:',i) #for testing
  # Pull some coded info out of the dir name
  country <- substr(basename(rdata),1,2)
  recode <- substr(basename(rdata),3,4)
  phase <- substr(basename(rdata),5,6)
  subphase <- substr(basename(rdata),5,5)
  povcal_filename <- paste0(country,recode,phase,"dt")



  if(toupper(povcal_filename) %in% toupper(povcalcuts$filename)){
    message(povcal_filename)
    povcal_subset = subset(povcalcuts,toupper(povcalcuts$filename)==toupper(povcal_filename))
    message(povcal_subset)
    iso3 = povcal_subset$iso3
    survey_year = povcal_subset$year
    for(year in survey_year){
      message(year)
      br_patha <- paste0(country,"br",phase)
      br_path <- paste0(tolower(br_patha),"fl.RData")
      load(br_path)
      br <- data.frame(data)
      remove(data)
      
      pr_patha <- paste0(country,"pr",phase)
      pr_path <- paste0(tolower(pr_patha),"fl.RData")
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
        wi_path <- paste0(tolower(wi_patha),"fl.RData")
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
      povperc <- weighted.percentile(pr$wealth,pr$weights,cuts)
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
      }else{
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
      pr$birth.reg = NA
      pr$birth.reg[which(pr$birth.cert %in% c(0,6,8,9))] = 0
      pr$birth.reg[which(pr$birth.cert %in% c(1,2,3))] = 1
    
      
      # Stunting
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
    
      
      #electrictiy
      names(pr)[which(names(pr)=="hv206")] <- "electricity"
      recode.electricity <- function(x){
        if(is.na(x)){return(NA)}
        else if(x==1) {return(1)}
        else if(x==0 ){return(0)}
        else if("yes"  | "YES" | "Yes" %in% x) {return(1)}
        else if("no"  | "NO" | "No" %in% x) {return(0)}
        else{return(NA)}
      } 

        
      
      #FGM   ##no data?##            
      names(pr)[which(names(pr)=="g102")] <- "FGM"
      if(typeof(pr$FGM)=='NULL'){
        pr$FGM <-NA}
      recode.FGM <- function(x){
        if(is.na(x)){return(NA)}
        else if(x==1) {return(1)}
        else if(x==0 ){return(0)}
        else if("yes"  | "YES" | "Yes" %in% x) {return(1)}
        else if("no"  | "NO" | "No" %in% x) {return(0)}
        else{return(NA)}
      } 
      

    
      
      #Young Marriage - Percentage of women who were married before 18 (17 and under)
      names(pr)[which(names(pr)=="v511")] <- "marriage"
      if(typeof(pr$marriage)=='NULL'){
        pr$marriage <- NA
        recode.electricity <- function(x){
          if(is.na(x)){return(NA)}
          else if(x<18) {return(1)}
          else if(x>=18 ){return(0)}
          else{return(NA)}
        } 
      }


      
      keep <- c(
        "wealth","weights","urban","region","educ","age","sex","cluster","household","head.sex","head.age",
        "p20","ext","birth.reg","stunting", "electricity", "FGM", "marriage"
        )
      prNames <- names(pr)
      namesDiff <- setdiff(keep,prNames)
      if(length(namesDiff)>0){
        for(y in 1:length(namesDiff)){
          pr[namesDiff[y]] <- NA
          message(paste("Missing variable",namesDiff[y]))
        } 
      }
    
      pr <- pr[,keep]
      names(br)[which(names(br)=="v001")] <- "cluster"
      names(br)[which(names(br)=="v002")] <- "household"
      pr.pov = data.table(pr)[,.(p20=mean(p20,na.rm=T)),by=.(cluster,household)]
    
      br <- as.data.table(br)
      br = merge(br,pr.pov,by=c("cluster","household"),all.x=T)
      br.p20 = subset(br,p20==T)
      br.u80 = subset(br,!p20)
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
    
      mort_dat = data.frame(
        p20=c(rep(T,3),rep(F,3)),
        variable=c(rep("mortality",6)),
        type=rep(c("statistic","numerator","denominator"),2),
        value=c(p20.mort,p20.mort.numerator,p20.mort.denominator,u80.mort,u80.mort.numerator,u80.mort.denominator)
      )
      
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
      stunt_dat = data.frame(
        p20=c(rep(T,3),rep(F,3)),
        variable=c(rep("stunting",6)),
        type=rep(c("statistic","numerator","denominator"),2),
        value=c(p20.stunting,p20.stunting.numerator,p20.stunting.denominator,u80.stunting,u80.stunting.numerator,u80.stunting.denominator)
      )
      
      
      pov.elec.tab = svytable(~electricity+p20,dsn)
      if("TRUE" %in% colnames(pov.elec.tab)){
        p20.elec = pov.elec.tab["1","TRUE"]/sum(pov.elec.tab["0","TRUE"],pov.elec.tab["1","TRUE"],na.rm=T)
        p20.elec.numerator = pov.elec.tab["1","TRUE"]
        p20.elec.denominator = sum(pov.elec.tab["0","TRUE"],pov.elec.tab["1","TRUE"],na.rm=T)
      }else{
        p20.elec = NA
        p20.elec.numerator = NA
        p20.elec.denominator = NA
      } 
      if("FALSE" %in% colnames(pov.elec.tab)){
        if("0" %in% colnames(pov.elec.tab)){
        u80.elec = pov.elec.tab["1","FALSE"]/sum(pov.elec.tab["0","FALSE"],pov.elec.tab["1","FALSE"],na.rm=T)
        u80.elec.numerator = pov.elec.tab["1","FALSE"]
        u80.elec.denominator = sum(pov.elec.tab["0","FALSE"],pov.elec.tab["1","FALSE"],na.rm=T)
        }else{
          u80.elec = NA
          u80.elec.numerator = NA
          u80.elec.denominator = NA  
        }
        }else{
        u80.elec = NA
        u80.elec.numerator = NA
        u80.elec.denominator = NA
      }
      elec_dat = data.frame(
        p20=c(rep(T,3),rep(F,3)),
        variable=c(rep("electricity",6)),
        type=rep(c("statistic","numerator","denominator"),2),
        value=c(p20.elec,p20.elec.numerator,p20.elec.denominator,u80.elec,u80.elec.numerator,u80.elec.denominator)
      )      
      
      pov.FGM.tab = svytable(~FGM+p20,dsn)
      if("TRUE" %in% colnames(pov.FGM.tab)){
        p20.FGM = pov.FGM.tab["1","TRUE"]/sum(pov.FGM.tab["0","TRUE"],pov.FGM.tab["1","TRUE"],na.rm=T)
        p20.FGM.numerator = pov.FGM.tab["1","TRUE"]
        p20.FGM.denominator = sum(pov.FGM.tab["0","TRUE"],pov.FGM.tab["1","TRUE"],na.rm=T)
      }else{
        p20.FGM = NA
        p20.FGM.numerator = NA
        p20.FGM.denominator = NA
      } 
      if("FALSE" %in% colnames(pov.FGM.tab)){
        if("0" %in% colnames(pov.FGM.tab)){
          u80.FGM = pov.FGM.tab["1","FALSE"]/sum(pov.FGM.tab["0","FALSE"],pov.FGM.tab["1","FALSE"],na.rm=T)
          u80.FGM.numerator = pov.FGM.tab["1","FALSE"]
          u80.FGM.denominator = sum(pov.FGM.tab["0","FALSE"],pov.FGM.tab["1","FALSE"],na.rm=T)
        }else{
          u80.FGM = NA
          u80.FGM.numerator = NA
          u80.FGM.denominator = NA  
        }
      }else{
        u80.FGM = NA
        u80.FGM.numerator = NA
        u80.FGM.denominator = NA
      }
      FGM_dat = data.frame(
        p20=c(rep(T,3),rep(F,3)),
        variable=c(rep("FGM",6)),
        type=rep(c("statistic","numerator","denominator"),2),
        value=c(p20.FGM,p20.FGM.numerator,p20.FGM.denominator,u80.FGM,u80.FGM.numerator,u80.FGM.denominator)
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
      reg_dat = data.frame(
        p20=c(rep(T,3),rep(F,3)),
        variable=c(rep("registration",6)),
        type=rep(c("statistic","numerator","denominator"),2),
        value=c(p20.reg,p20.reg.numerator,p20.reg.denominator,u80.reg,u80.reg.numerator,u80.reg.denominator)
        
      ) 
      
      
      pov.mar.tab = svytable(~marriage+p20,dsn)
      if("TRUE" %in% colnames(pov.mar.tab)){
        p20.mar = pov.mar.tab["1","TRUE"]/sum(pov.mar.tab["0","TRUE"],pov.mar.tab["1","TRUE"],na.rm=T)
        p20.mar.numerator = pov.mar.tab["1","TRUE"]
        p20.mar.denominator = sum(pov.mar.tab["0","TRUE"],pov.mar.tab["1","TRUE"],na.rm=T)
      }else{
        p20.mar = NA
        p20.mar.numerator = NA
        p20.mar.denominator = NA
      }
      if("FALSE" %in% colnames(pov.mar.tab)){
        u80.mar = pov.mar.tab["1","FALSE"]/sum(pov.mar.tab["0","FALSE"],pov.mar.tab["1","FALSE"],na.rm=T)
        u80.mar.numerator = pov.mar.tab["1","FALSE"]
        u80.mar.denominator = sum(pov.mar.tab["0","FALSE"],pov.mar.tab["1","FALSE"],na.rm=T)
      }else{
        u80.mar = NA
        u80.mar.numerator = NA
        u80.mar.denominator = NA
      }
      mar_dat = data.frame(
        p20=c(rep(T,3),rep(F,3)),
        variable=c(rep("marriage",6)),
        type=rep(c("statistic","numerator","denominator"),2),
        value=c(p20.reg,p20.reg.numerator,p20.reg.denominator,u80.reg,u80.reg.numerator,u80.reg.denominator)
        
      ) 

      

      dat = rbind(mort_dat,stunt_dat,reg_dat,elec_dat,FGM_dat, mar_dat)
      dat$filename <- povcal_filename
      if(length(iso3)>0){
        dat$iso3 = iso3[1:1]
        dat$survey_year = year
      }else{
        dat$iso3 = NA
        dat$survey_year = NA
      }
      
      dataList[[dataIndex]] <- dat
      dataIndex <- dataIndex + 1

    }
  }
}


data.total <- rbindlist(dataList)
save(data.total,file="historical_dhs.RData")


fwrite(data.total,"historical_dhs.csv")
