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
library(urltools)
library(h2o)
library(here)
library(highcharter)
library(flexdashboard)
library(purrr)

Sys.setlocale("LC_ALL", "Polish")

# load stores file
stores <- read_excel("Stores.xlsx")
stores = stores[order(stores$'Store ID'),]
stores

# divide Daffodils file into sheets
path <- "Daffodils2020.xls"
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

# upload all csv files in folder at once
list.filenames <- list.files(pattern = "*.csv")
list.filenames

list.data<-list()
for (i in 1:length(list.filenames)){
  list.data[[i]]<-read.csv(list.filenames[i], check.names = FALSE)
  #list.data[[i]]$`Store ID`<-list.data[[i]]$`STORE #`  %% 1000
  names(list.data[[i]])[3] <- paste(names(list.data[[i]])[4], names(list.data[[i]])[3])
  names(list.data[[i]])[5] <- paste(names(list.data[[i]])[6], names(list.data[[i]])[5])
  names(list.data[[i]])[7] <- paste(names(list.data[[i]])[8], names(list.data[[i]])[7])
  names(list.data[[i]])[11] <- paste(names(list.data[[i]])[12], names(list.data[[i]])[11])
  list.data[[i]] = dplyr::select(list.data[[i]], -c("COUNT", "DAFFODIL"))
}

names(list.data)<-substr(list.filenames, 18, 20)
rm(list.filenames)


stores$'Store Name' = toupper(stores$'Store Name')
stores$'Store Name'[!startsWith(stores$'Store Name', 
                                'PARVIFLORA')] = paste('PARVIFLORA', 
                                                       stores$'Store Name'[!startsWith(stores$'Store Name', 
                                                                                       'PARVIFLORA')],
                                                       sep = ' ')


# find which letters used in stores names
stores_chars <- stores$`Store Name` %>% paste(collapse="") %>% toupper() %>% as.character() %>% 
  substring(1:nchar(.), 1:nchar(.)) %>% as.factor() %>% unique() %>% as.data.frame() 

# find which letters used in list.data
data_chars <- unlist(lapply(list.data, function(x){
  x$`STORE NAME` %>% paste(collapse="") %>% toupper() %>% as.character() %>% 
    substring(1:nchar(.), 1:nchar(.)) 
})) %>% unique() %>% as.data.frame() 


stores_chars = as.character(unlist(stores_chars))
data_chars = as.character(unlist(data_chars))
pol_chars = list()
k = 1
for(i in 1:length(stores_chars)){
  if(!(stores_chars[i] %in% data_chars)){
    pol_chars[[k]] = stores_chars[i]
    k = k + 1
  }
}


stores_copy = stores
for(i in 1:length(pol_chars)){
  stores_copy$`Store Name` = str_replace_all(stores_copy$`Store Name`, pol_chars[[i]], replacement = '?')
}

list.data = lapply(list.data, function(x){
  x$'STORE NAME' = gsub(pattern="RETAIL", replacement="", x=x$'STORE NAME')
  x$'STORE NAME' = gsub(pattern="OTHER", replacement="", x=x$'STORE NAME')
  x$'STORE NAME' = gsub(pattern="GROSS", replacement="", x=x$'STORE NAME')
  x$'STORE NAME' = trim(x$'STORE NAME')
  x = x %>% group_by(`STORE NAME`) %>% summarise_all(funs(sum))
  x$'STORE NAME'[!startsWith(x$'STORE NAME', 'PARVIFLORA')] = paste('PARVIFLORA', 
                                                                    x$'STORE NAME'[!startsWith(x$'STORE NAME', 
                                                                                               'PARVIFLORA')], 
                                                                    sep = ' ')
  full_join(x, stores_copy, by=c("STORE NAME"="Store Name"))
})



formatted = lapply(list.data, function(x){
  full_join(x, stores, by="Store ID")
})


formatted = lapply(formatted, dplyr::select, -c('STORE NAME'))


summary.list = list()
for(i in 1:length(names(daf.summary))){
  summary.list[[i]] = full_join(list.data[names(daf.summary)][[i]], daf.summary[[i]], by = "Store ID")
}
names(summary.list) = names(daf.summary)


summary.list = lapply(summary.list, function(x){
  x = x %>% dplyr::select(-c('TOTAL', 'TOTAL COUNT', 'STORE #'))
  x = full_join(x, stores, by = 'Store ID') 
  x$'Store Name'[is.na(x$'Store Name')] = x$'STORE NAME'[is.na(x$'Store Name')]
  x = x %>% dplyr::select(-'STORE NAME')
})

summary.list = lapply(summary.list, function(x){
  x$DAFFODIL[is.na(x$DAFFODIL)] = 0
  x$`DAFFODIL COUNT`[is.na(x$`DAFFODIL COUNT`)] = 0
  x$AZALEA[is.na(x$AZALEA)] = 0
  x$`AZALEA COUNT`[is.na(x$`AZALEA COUNT`)] = 0
  x$BEGONIA[is.na(x$BEGONIA)] = 0
  x$`BEGONIA COUNT`[is.na(x$`BEGONIA COUNT`)] = 0
  x$CARNATION[is.na(x$CARNATION)] = 0
  x$`CARNATION COUNT`[is.na(x$`CARNATION COUNT`)] = 0
  x %>% mutate(TOTAL = AZALEA + BEGONIA + CARNATION + DAFFODIL,
               `TOTAL COUNT` = `AZALEA COUNT` + `BEGONIA COUNT` + `CARNATION COUNT` + `DAFFODIL COUNT`)
})

summary.frame = data.frame()
for(i in 1:length(summary.list)){
  summary.list[[i]]$MONTH = names(summary.list)[i]
  summary.frame = rbind(summary.frame, summary.list[[i]])
}

summary.frame$`Store ID` = as.integer(summary.frame$`Store ID`)
summary.frame = summary.frame %>% dplyr::select('MONTH', "Store ID", "Store Name", "AZALEA COUNT",
                                                "AZALEA", "BEGONIA COUNT", "BEGONIA", "CARNATION COUNT",
                                                "CARNATION", "DAFFODIL COUNT", "DAFFODIL", "TOTAL",
                                                "TOTAL COUNT")

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


# order by months
summary.frame$MONTH <- factor(summary.frame$MONTH, month.abb, ordered=TRUE)
summary.frame[order(summary.frame$MONTH), ]

combined_df$Month <- factor(combined_df$Month, month.abb, ordered=TRUE)
combined_df[order(combined_df$Month),]

total_sales <- combined_df %>% 
  count(`Store Name`, Sales) %>% 
  glimpse()


total_transaction <- combined_df %>% 
  count(`Store Name`, Count) %>% 
  glimpse()


daf <- summary.frame %>% 
  count(MONTH, DAFFODIL) %>% 
  glimpse()


azal <- summary.frame %>% 
  count(MONTH, AZALEA) %>% 
  glimpse()

bego <- summary.frame %>% 
  count(MONTH, BEGONIA) %>% 
  glimpse()

car <- summary.frame %>% 
  count(MONTH, CARNATION) %>% 
  glimpse()


