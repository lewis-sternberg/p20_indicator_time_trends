list.of.packages <- c("data.table","survey")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

mort = function(br){
  #The trick to getting this to work is expressing age as a time period.
  #E.g. someone born 59 months ago would be expressed as -59
  #So we subtract the date of the survey from the birth date
  br$age.months <- br$b3-br$v008
  br$weights <- br$v005/1000000
  # v001 = cluster
  dsn = svydesign(
    data=br
    ,ids=~1
    ,weights=~weights
  )
  
  probs <- c()
  mort_ses <- c()
  surv_ses <- c()
  #Time-lower and time-upper also need to be expressed as time period, with 0 being the survey
  # tl = -59
  tu = 0
  tl = -119
  # tu = -60
  # tl = -179
  # tu = -120
  #Age of death is expressed as a duration... so there's no need for it to be negative. This matches var b7 (age of death in months)
  #Here al stands for age-lower and au stands for age-upper
  segments <- list(
    list("al"=0,"au"=60)
    # list("al"=-1,"au"=0)
    # ,list("al"=1,"au"=2)
    # ,list("al"=3,"au"=5)
    # ,list("al"=6,"au"=11)
    # ,list("al"=12,"au"=23)
    # ,list("al"=24,"au"=35)
    # ,list("al"=36,"au"=47)
    # ,list("al"=48,"au"=60)
  )
  for(i in 1:length(segments)){
    segment = segments[[i]]
    al = segment[["al"]]
    au = segment[["au"]]
    dsn = update(dsn, mort_tmp=(b7>=al & b7<=au))
    dsn = update(dsn, surv_tmp=(is.na(b7) | b7>=al))
    cohortA <- subset(dsn,age.months>=(tl-au) & age.months<(tl-al))
    cohortB <- subset(dsn,age.months>=(tl-al) & age.months<=(tu-au))
    cohortC <- subset(dsn,age.months>(tu-au) & age.months<=(tu-al))
    if(nrow(cohortA)>0){
      Amortalities <- svytotal(~mort_tmp,cohortA,na.rm=T)
      Amort_se = SE(Amortalities)["mort_tmpTRUE"]
    }else{
      Amortalities <- 0
      Amort_se = 0
    }
    if(nrow(cohortB)>0){
      Bmortalities <- svytotal(~mort_tmp,cohortB,na.rm=T)
      Bmort_se = SE(Bmortalities)["mort_tmpTRUE"]
    }else{
      Bmortalities <- 0
      Bmort_se = 0
    }
    if(nrow(cohortC)>0){
      Cmortalities <- svytotal(~mort_tmp,cohortC,na.rm=T)
      Cmort_se = SE(Cmortalities)["mort_tmpTRUE"]
    }else{
      Cmortalities <- 0
      Cmort_se = 0
    }
    if(tu==0){
      mortalities <- Bmortalities+0.5*Amortalities+Cmortalities
      mort_se = Bmort_se+0.5*Amort_se+Cmort_se
    }else{
      mortalities <- Bmortalities+0.5*Amortalities+0.5*Cmortalities
      mort_se = Bmort_se+0.5*Amort_se+0.5*Cmort_se
    }
    
    if(nrow(cohortA)>0){
      Asurvivals <- svytotal(~surv_tmp,cohortA,na.rm=T)
      Asurv_se = SE(Asurvivals)["surv_tmpTRUE"]
    }else{
      Asurvivals <- 0
      Asurv_se = 0
    }
    if(nrow(cohortB)>0){
      Bsurvivals <- svytotal(~surv_tmp,cohortB,na.rm=T)
      Bsurv_se = SE(Bsurvivals)["surv_tmpTRUE"]
    }else{
      Bsurvivals <- 0
      Bsurv_se = 0
    }
    if(nrow(cohortC)>0){
      Csurvivals <- svytotal(~surv_tmp,cohortC,na.rm=T)
      Csurv_se = SE(Csurvivals)["surv_tmpTRUE"]
    }else{
      Csurvivals <- 0
      Csurv_se = 0
    }
    if(tu==0){
      survivals <- Bsurvivals+0.5*Asurvivals+Csurvivals
      surv_se = Bsurv_se+0.5*Asurv_se+Csurv_se
    }else{
      survivals <- Bsurvivals+0.5*Asurvivals+0.5*Csurvivals
      surv_se = Bsurv_se+0.5*Asurv_se+0.5*Csurv_se
    }
    morts = mortalities["mort_tmpTRUE"]
    survs = survivals["surv_tmpTRUE"]
    prob <- 1-(morts/survs)
    if(is.nan(prob)){
      prob <- 1
    }
    probs <- c(probs,prob)
    mort_ses <- c(mort_ses,(mort_se/morts))
    surv_ses <- c(surv_ses,(surv_se/survs))
  }
  mortality <- (1-prod(probs))

  return(list(
    mortality=mortality,
    probs=probs,
    mort_ses=mort_ses,
    surv_ses=surv_ses
  ))
}
