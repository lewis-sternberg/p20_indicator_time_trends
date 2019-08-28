####Function and setup####
list.of.packages <- c("dplyr", "data.table", "MALDIquant", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- "C:/"
setwd(wd)

####agragate data####
load("C:/DHS data R/historical_dhs (new years, combined filenames).RData") #Loading final hisotrical DHS data
povcalcuts<- fread("p20incometrends.csv")

years <-select(data.total_dups,survey_year)
years <-unique(years)
years <-years[[1]]
years <-sort(years)
  
dataList2 <- list()
dataIndex2 <- 1

n=0
dataList <- list()
dataIndex <- 1
while(n-1<length(years)){
  year <- c(years[2+n],years[2+n],years[2+n])
  year_3 <- c(years[n+1], years[n+2], years[n+3])
  years.split <-data.frame(year,year_3)
  names(years.split) <-c("group_year", "year_range")
  dataList[[dataIndex]] <- years.split
  dataIndex <- dataIndex + 1
  n=n+3
  remove(years.split)
}
years.split <- rbindlist(dataList)
remove(dataList)
years <- years[years %in% years.split$group_year]



variables<-c("education", "electricity", "marriage", "mortality", "registration", "stunting")
p20 <-c(TRUE, FALSE)


#for(v in 1:1){
for(v in 1:length(variables)){
  var <- variables[v]
  message(var)
  
  for(p in 1:2){
    pov <-p20[p]
    message(pov)
    
    # for(y in 1:5){
    for(y in 1:length(years)){
      year <- years[y]
      message(year)
      dataList <- list()
      dataIndex <- 1
      years_group <- filter(years.split, group_year==year)
      years_group <- select(years_group,year_range)
      years_group <- years_group[!is.na(years_group$year_range),]
      country_list <- vector()
      
      for(yr in 1:length(years_group)){
        year_group <- years_group[yr]
        message("g: ",year_group)
        print(country_list)
        
        
        dat <-filter(data.total_dups, survey_year==year_group, variable==var, p20==pov)
        rownames(dat) <-NULL
      
        if(length(country_list)>0){
          
          for(c in 1:length(country_list)){
            code <-country_list[c]
            
            dat <- dat[!dat$iso3==code,]
          }}
        dat_num <-filter(dat, type=="numerator")
        dat_num <-dat_num[!is.na(dat_num$value),]
        dat_num <-summarise(dat_num,sum(value))
        
        
        
        dat_den <-filter(dat, type=="denominator")
        dat_den <-dat_den[!is.na(dat_den$value),] 
        dat_den <-summarise(dat_den,sum(value))
        
        dat_stat <-dat_num/dat_den
        country <- dat$iso3[!is.na(dat$value)]
        country <-unique(country)
        
        if(length(country)>0){
          country_list <- append(country_list, country, after=length(country_list))
          country.str <-paste(country, collapse=" ")
          country <-data.frame(country)
          names(country) <-c("CountryCode")
          rownames(country)<-NULL
          
          pop <- merge(povcalcuts, country, by="CountryCode",)
          pop <-pop[order(RequestYear),]
          rownames(pop) <-NULL
          pop_year <-pop$RequestYear[match.closest(year, pop$RequestYear)]
          povcalcuts$pop <-as.numeric(povcalcuts$pop)
          all <-filter(povcalcuts, RequestYear==pop_year)
          total_pop <-summarise(all,sum(pop))
          
          pop <- filter(pop, RequestYear==pop_year)
          countries_pop <- summarise(pop,sum(pop))
          
          percent_pop = countries_pop/total_pop*100
          
          
          data <-data.frame(pov, var, dat_num, dat_den, year, countries_pop, total_pop, country.str)
          dataList[[dataIndex]]<-data
          dataIndex <-dataIndex+1
          remove(data)
        } 

        
      }
      remove(country_list) 
      if(length(dataList)>0){
        aggregate <-rbindlist(dataList)
        names(aggregate) <-c("p20", "variable", "num","den", "group_year", "countries_pop","world_pop", "countries included")
      

      
      num_sum <- summarise(aggregate, sum(num))
      den_sum <-summarise(aggregate,sum(den))
      stat <-num_sum/den_sum
      

      world_pop <-summarise(aggregate, mean(world_pop))
      
      pop_sum <-summarise(aggregate, sum(countries_pop))
      coverage <-pop_sum/world_pop*100
      data_sum <-data.frame(pov, var, year, stat, coverage)
      
      dataList2[[dataIndex2]]<-data_sum
      dataIndex2 <-dataIndex2+1
      
      remove(aggregate)
      remove(data_sum)
      
      
      remove(pop_sum)
      remove(world_pop)
      remove(stat)
      remove(coverage)
      }
      dataList <- list()
      dataIndex <- 1
    }
    
  }
}


aggregate.total2 <-rbindlist(dataList2)
names(aggregate.total2) <-c("p20", "variable", "group_year", "value", "percent of world pop")
#NOTE: population converage includes countries where data is NA#

for (v in 1:length(variables)){
  var <-variables[v]
  
  dat.g <-filter(aggregate.total2, variable==var)
  dat.g <-dat.g[which(!is.na(dat.g$value)),]
  dat.g <-select(dat.g, "p20", "value", "group_year")
  
  
  title = paste("Global", var, "Trends")
  p=ggplot(dat.g, aes(x=group_year, y=value, )) +
    geom_line(aes(color=p20),size =1)+
    geom_point(aes(colour=p20))+
    labs(title=title,y="Percentage",x="Year",colour="")+
    theme_minimal()+
    scale_x_continuous(labels=function(year){sprintf("%.0f",year)})+
    ylim(0,1)
  
  
  ggsave(paste0("p20_u80 output/","Global-aggragate ",var,".png"),p)
  
  
}
