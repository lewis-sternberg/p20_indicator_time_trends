list.of.packages <- c("readr","scrapeR","data.table","jsonlite","RCurl","plyr","zoo","reshape2","XML","httr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "E:/git/p20_indicator_time_trends"
setwd(wd)

base_link = "https://dhsprogram.com"
datasets_page_source = htmlParse(content(GET("http://dhsprogram.com/data/available-datasets.cfm")))
link_elems = getNodeSet(datasets_page_source,"//tr/td/a")
link_suffixes = sapply(link_elems,xmlGetAttr,"href")
link_suffixes <- link_suffixes[grepl("what-we",link_suffixes)]

wealth.ids <- sapply(link_suffixes, function(x) substr(x, gregexpr("-",x)[[1]][4]+1, gregexpr("cfm",x)[[1]]-2))

write_csv(wealth.ids,"Wealth surveys.csv")
