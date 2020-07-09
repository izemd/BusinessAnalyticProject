library(readxl)
library(tidyverse)
library(raster)
library(shiny)
library(shinydashboard)
library(knitr)
library(kableExtra)
library(dplyr)
library(gridExtra)
library(grid)
library(zoo)
library(wesanderson)
library(RCurl)
library(httr)
library(repmis)
library(urltools)
library(h2o)

#Sys.setlocale("LC_ALL", "Polish")
# load stores file

github_link <- "https://github.com/izemd/BusinessAnalyticProject/blob/master/Dataset/Stores.xlsx?raw=true"
temp_file <- tempfile(fileext = ".xlsx")
req <- GET(github_link, 
           # authenticate using GITHUB_PAT
           authenticate(Sys.getenv("GITHUB_PAT"), ""),
           # write result to disk
           write_disk(path = temp_file))
stores <- readxl::read_excel(temp_file)
stores = stores[order(stores$'Store ID'),]
stores

github_link2 <- "https://github.com/izemd/BusinessAnalyticProject/blob/master/Dataset/Parviflora.xlsx?raw=true"
temp_file2 <- tempfile(fileext = ".xlsx")
req2 <- GET(github_link2, 
            authenticate(Sys.getenv("GITHUB_PAT"), ""),
            write_disk(path = temp_file2))
summary.frame <- readxl::read_excel(temp_file2)

# divide daffodil file into sheets
github_link1 <- "https://github.com/izemd/BusinessAnalyticProject/blob/master/Dataset/Daffodils2020.xls?raw=true"
temp_file1 <- tempfile(fileext = ".xls")
req1 <- GET(github_link1, 
           authenticate(Sys.getenv("GITHUB_PAT"), ""),
           write_disk(path = temp_file1))

path = temp_file1
mysheets = excel_sheets(path)
mysheets= mysheets %>% set_names %>% map(read_excel, path = path)
mysheets



daffodil.list = list()
for(i in 1:length(mysheets)){
  breaks = rowSums(is.na(mysheets[[i]])) == ncol(mysheets[[i]])
  daffodil.list[[i]] = split(mysheets[[i]], f = data.table::rleid(breaks))
  daffodil.list[[i]] = Filter(function(x) !all(is.na(x)), daffodil.list[[i]])
}
daf.sheetnames <- names(mysheets)
names(daffodil.list)<-substr(daf.sheetnames, 1, 3)
rm(daf.sheetnames)
rm(mysheets)
rm(breaks)


daf.summary = list()
for(i in 1:length(daffodil.list)){
  for(j in 1:length(daffodil.list[[i]])){
    daf.summary[[names(daffodil.list[i])]] = rbind(daf.summary[[names(daffodil.list[i])]],
                                                   as.data.frame(cbind(daffodil.list[[i]][[j]][1,3],
                                                                       daffodil.list[[i]][[j]][dim(daffodil.list[[i]][[j]])[1],2],
                                                                       daffodil.list[[i]][[j]][dim(daffodil.list[[i]][[j]])[1],3])))
  }
  names(daf.summary[[names(daffodil.list[i])]]) <- c("Store ID", "DAFFODIL", "DAFFODIL COUNT")
  daf.summary[[i]]$`Store ID` = as.numeric(daf.summary[[i]]$`Store ID`)
  daf.summary[[i]]$DAFFODIL = as.numeric(daf.summary[[i]]$DAFFODIL)
  daf.summary[[i]]$`DAFFODIL COUNT` = as.numeric(daf.summary[[i]]$`DAFFODIL COUNT`)
}

daf.summary <- lapply(daf.summary, na.omit)

## load all csv files
#Jan <- read.csv(text=getURL("https://raw.githubusercontent.com/izemd/BusinessAnalyticProject/master/Dataset/Summary%20of%20Sales%20January%202020.csv"))
#Feb <- read.csv(text=getURL("https://raw.githubusercontent.com/izemd/BusinessAnalyticProject/master/Dataset/Summary%20of%20Sales%20February%202020.csv"))
#Mar <- read.csv(text=getURL("https://raw.githubusercontent.com/izemd/BusinessAnalyticProject/master/Dataset/Summary%20of%20Sales%20March%202020.csv"))
#Apr <- read.csv(text=getURL("https://raw.githubusercontent.com/izemd/BusinessAnalyticProject/master/Dataset/Summary%20of%20Sales%20April%202020.csv"))
#
#
#list.all <- list(Jan=Jan, Feb=Feb, Mar=Mar, Apr=Apr)
#list.all[[1]]
#
#
#names(list.all[[1]])[3] <- paste(names(list.all[[1]])[4], "COUNT")
#names(list.all[[2]])[3] <- paste(names(list.all[[2]])[4], "COUNT")
#names(list.all[[3]])[3] <- paste(names(list.all[[3]])[4], "COUNT")
#names(list.all[[4]])[3] <- paste(names(list.all[[4]])[4], "COUNT")
#
#
#names(list.all[[1]])[5] <- paste(names(list.all[[1]])[6], "COUNT")
#names(list.all[[2]])[5] <- paste(names(list.all[[2]])[6], "COUNT")
#names(list.all[[3]])[5] <- paste(names(list.all[[3]])[6], "COUNT")
#names(list.all[[4]])[5] <- paste(names(list.all[[4]])[6], "COUNT")
#
#names(list.all[[1]])[7] <- paste(names(list.all[[1]])[8], "COUNT")
#names(list.all[[2]])[7] <- paste(names(list.all[[2]])[8], "COUNT")
#names(list.all[[3]])[7] <- paste(names(list.all[[3]])[8], "COUNT")
#names(list.all[[4]])[7] <- paste(names(list.all[[4]])[8], "COUNT")
#
#names(list.all[[1]])[11] <- paste(names(list.all[[1]])[12], "COUNT")
#names(list.all[[2]])[11] <- paste(names(list.all[[2]])[12], "COUNT")
#names(list.all[[3]])[11] <- paste(names(list.all[[3]])[12], "COUNT")
#names(list.all[[4]])[11] <- paste(names(list.all[[4]])[12], "COUNT")
#
#
#list.all[[1]] = dplyr::select(list.all[[1]], -c("COUNT.3", "DAFFODIL"))
#list.all[[2]] = dplyr::select(list.all[[2]], -c("COUNT.3", "DAFFODIL"))
#list.all[[3]] = dplyr::select(list.all[[3]], -c("COUNT.3", "DAFFODIL"))
#list.all[[4]] = dplyr::select(list.all[[4]], -c("COUNT.3", "DAFFODIL"))
#
#names(list.all[[1]])[1] <- paste("STORE NAME")
#names(list.all[[2]])[1] <- paste("STORE NAME")
#names(list.all[[3]])[1] <- paste("STORE NAME")
#names(list.all[[4]])[1] <- paste("STORE NAME")


#list.filenames<-list.files(pattern=".csv$")
#list.data<-list()
#for (i in 1:length(list.filenames)){
#  list.data[[i]]<-read.csv(list.filenames[i], check.names = FALSE)
#  #list.data[[i]]$`Store ID`<-list.data[[i]]$`STORE #`  %% 1000
#  names(list.data[[i]])[3] <- paste(names(list.data[[i]])[4], names(list.data[[i]])[3])
#  names(list.data[[i]])[5] <- paste(names(list.data[[i]])[6], names(list.data[[i]])[5])
#  names(list.data[[i]])[7] <- paste(names(list.data[[i]])[8], names(list.data[[i]])[7])
#  names(list.data[[i]])[11] <- paste(names(list.data[[i]])[12], names(list.data[[i]])[11])
#  list.data[[i]] = dplyr::select(list.data[[i]], -c("COUNT", "DAFFODIL"))
#}
#
#names(list.data)<-substr(list.filenames, 18, 20)
#rm(list.filenames)

stores$'Store Name' = toupper(stores$'Store Name')
stores$'Store Name'[!startsWith(stores$'Store Name', 
                                'PARVIFLORA')] = paste('PARVIFLORA', 
                                                       stores$'Store Name'[!startsWith(stores$'Store Name', 
                                                                                       'PARVIFLORA')],
                                                       sep = ' ')



#stores_chars <- stores$`Store Name` %>% paste(collapse="") %>% toupper() %>% as.character() %>% 
#  substring(1:nchar(.), 1:nchar(.)) %>% as.factor() %>% unique() %>% as.data.frame() 
#
#
#
#data_chars <- unlist(lapply(list.all, function(x){
#  x$`STORE NAME` %>% paste(collapse="") %>% toupper() %>% as.character() %>% 
#    substring(1:nchar(.), 1:nchar(.)) 
#})) %>% unique() %>% as.data.frame() 
#
#
#data_chars <- as.character(unlist(list.all[[1]]$`STORE NAME`) %>% paste(collapse=""))
#data_chars %>% substring(1:nchar(.), 1:nchar(.)) 
#
#
#
#
#
#stores_chars = as.character(unlist(stores_chars))
#data_chars = as.character(unlist(data_chars))
#pol_chars = list()
#k = 1
#for(i in 1:length(stores_chars)){
#  if(!(stores_chars[i] %in% data_chars)){
#    pol_chars[[k]] = stores_chars[i]
#    k = k + 1
#  }
#}
#
#
#stores_copy = stores
#for(i in 1:length(pol_chars)){
#  stores_copy$`Store Name` = str_replace_all(stores_copy$`Store Name`, pol_chars[[i]], replacement = '?')
#}
#
#list.all = lapply(list.all, function(x){
#  x$'STORE NAME' = gsub(pattern="RETAIL", replacement="", x=x$'STORE NAME')
#  x$'STORE NAME' = gsub(pattern="OTHER", replacement="", x=x$'STORE NAME')
#  x$'STORE NAME' = gsub(pattern="GROSS", replacement="", x=x$'STORE NAME')
#  x$'STORE NAME' = trim(x$'STORE NAME')
#  x = x %>% group_by(`STORE NAME`) %>% summarise_all(funs(sum))
#  x$'STORE NAME'[!startsWith(x$'STORE NAME', 'PARVIFLORA')] = paste('PARVIFLORA', 
#                                                                    x$'STORE NAME'[!startsWith(x$'STORE NAME', 
#                                                                                               'PARVIFLORA')], 
#                                                                    sep = ' ')
#  full_join(x, stores_copy, by=c("STORE NAME"="Store Name"))
#})
#
#
#
#formatted = lapply(list.all, function(x){
#  full_join(x, stores, by="Store ID")
#})
#
#
#formatted = lapply(formatted, dplyr::select, -c('STORE NAME'))
#
#
#
#summary.list = list()
#for(i in 1:length(names(daf.summary))){
#  summary.list[[i]] = full_join(list.all[names(daf.summary)][[i]], daf.summary[[i]], by = "Store ID")
#}
#names(summary.list) = names(daf.summary)
#
#summary.list = lapply(summary.list, function(x){
#  x = x %>% dplyr::select(-c('TOTAL', 'TOTAL COUNT', 'STORE..'))
#  x = full_join(x, stores, by = 'Store ID') 
#  x$'Store Name'[is.na(x$'Store Name')] = x$'STORE NAME'[is.na(x$'Store Name')]
#  x = x %>% dplyr::select(-'STORE NAME')
#})
#
#summary.list = lapply(summary.list, function(x){
#  x$DAFFODIL[is.na(x$DAFFODIL)] = 0
#  x$`DAFFODIL COUNT`[is.na(x$`DAFFODIL COUNT`)] = 0
#  x$AZALEA[is.na(x$AZALEA)] = 0
#  x$`AZALEA COUNT`[is.na(x$`AZALEA COUNT`)] = 0
#  x$BEGONIA[is.na(x$BEGONIA)] = 0
#  x$`BEGONIA COUNT`[is.na(x$`BEGONIA COUNT`)] = 0
#  x$CARNATION[is.na(x$CARNATION)] = 0
#  x$`CARNATION COUNT`[is.na(x$`CARNATION COUNT`)] = 0
#  x %>% mutate(TOTAL = AZALEA + BEGONIA + CARNATION + DAFFODIL,
#               `TOTAL COUNT` = `AZALEA COUNT` + `BEGONIA COUNT` + `CARNATION COUNT` + `DAFFODIL COUNT`)
#})
#
#summary.frame = data.frame()
#for(i in 1:length(summary.list)){
#  summary.list[[i]]$MONTH = names(summary.list)[i]
#  summary.frame = rbind(summary.frame, summary.list[[i]])
#}
#
#summary.frame$`Store ID` = as.integer(summary.frame$`Store ID`)
#summary.frame = summary.frame %>% dplyr::select('MONTH', "Store ID", "Store Name", "AZALEA COUNT",
#                                                "AZALEA", "BEGONIA COUNT", "BEGONIA", "CARNATION COUNT",
#                                                "CARNATION", "DAFFODIL COUNT", "DAFFODIL", "TOTAL",
#                                                "TOTAL COUNT")
#
azalea_df = summary.frame %>% dplyr::select('MONTH', "Store ID", "Store Name", "AZALEA COUNT", "AZALEA")
begonia_df = summary.frame %>% dplyr::select('MONTH', "Store ID", "Store Name", "BEGONIA COUNT", "BEGONIA")
carnation_df = summary.frame %>% dplyr::select('MONTH', "Store ID", "Store Name", "CARNATION COUNT", "CARNATION")
daffodil_df = summary.frame %>% dplyr::select('MONTH', "Store ID", "Store Name", "DAFFODIL COUNT", "DAFFODIL")

names(azalea_df) =c("Month", "Store ID", "Store Name", "Count" , "Sales")
names(begonia_df) =c("Month", "Store ID", "Store Name", "Count" , "Sales")
names(carnation_df) =c("Month", "Store ID", "Store Name", "Count" , "Sales")
names(daffodil_df) =c("Month", "Store ID", "Store Name", "Count" , "Sales")

azalea_df$`Flower Type` = "AZALEA"
begonia_df$`Flower Type` = "BEGONIA"
carnation_df$`Flower Type` = "CARNATION"
daffodil_df$`Flower Type` = "DAFFODIL"

combined_df = rbind(azalea_df,
                    begonia_df, 
                    carnation_df,
                    daffodil_df)

#combined_df$`Per Transanction` <- combined_df$Sales/combined_df$Count 

# library("writexl")
# write_xlsx(summary.frame, "Parviflora.xlsx")


totalSales <- summary.frame$TOTAL
totalcount <- summary.frame$`TOTAL COUNT`
avgsale <- sum(totalSales)/sum(totalcount)
avg_store_sale <- sum(totalSales)/length(unique(stores$`Store ID`))
stores_unique <- unique(stores$`Store ID`)
avg_trans <- sum(totalcount)/length(unique(stores$`Store ID`))
daff <- summary.frame$DAFFODIL
daff_trans<- summary.frame$`DAFFODIL COUNT`
daff_avg_store <- sum(daff_trans)/length(unique(stores$`Store ID`))
daff_avg <- sum(daff)/length(unique(stores$`Store ID`))
