# Script for ECON 142 group project
# Kevin Thich, Angel Henriquez, Skanda Shastri

#clear working space
rm(list = ls())

library(doBy)
library(dplyr)
library(stargazer)
library(ISwR)
library(car)
library(maps)
library(maptools)
library(ggplot2)
library(rgdal)
library(tibble)
library(mapproj)
library(ggmap)
library(htmltab)
library(tidyverse)
library(sandwich)

# setting working directory
setwd("~/Desktop/#ECON 142/ECON 142 Project Data")

cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

# turn off scientific notation except for big numbers.
options(scipen = 9)

# list of languages per country
languages <- "https://www.cia.gov/library/publications/the-world-factbook/fields/402.html"
language_data <- htmltab(doc = languages, which = 1)

# removing irrelevant dataset
rm('languages')

language_data$Country <- tolower(language_data$Country)
language_data$Languages <- tolower(language_data$Languages)

# segmenting countries main language
language_data$Languages <- word(language_data$Languages, 1)
# removing commas
language_data$Languages <- as.character(gsub(",","",language_data$Languages))

country_data <- "https://www.worldatlas.com/aatlas/ctycodes.htm"
country_codes <- htmltab(doc = country_data, which = 1)

# removing irrelevant dataset
rm('country_data')

names(country_codes)[1] <- "country"
country_codes$country <- tolower(country_codes$country)
names(country_codes)[3] <- "country_code"
country_codes$country_code <- tolower(country_codes$country_code)

# final language and country dataset
language_country_dataset <- merge(country_codes, language_data, by.x=("country"), by.y=("Country"),  all = TRUE)
language_country_dataset <- language_country_dataset[c(1,3,6)]

# removing irrelevant dataset
rm('country_codes')
rm('language_data')

# missing country codes
language_country_dataset$country_code[2] <- "akr"
language_country_dataset$country_code[36] <- "brn"
language_country_dataset$country_code[40] <- "mmr"
language_country_dataset$country_code[42] <- "cpv"
language_country_dataset$country_code[70] <- "akr"
language_country_dataset$country_code[80] <- "esw"
language_country_dataset$country_code[95] <- "gza"
language_country_dataset$country_code[113] <- "vat"
language_country_dataset$country_code[120] <- "irn"
language_country_dataset$country_code[135] <- "prk"
language_country_dataset$country_code[137] <- "kor"
language_country_dataset$country_code[138] <- "rks"
language_country_dataset$country_code[142] <- "lao"
language_country_dataset$country_code[152] <- "mac"
language_country_dataset$country_code[167] <- "mda"
language_country_dataset$country_code[187] <- "mkd"
language_country_dataset$country_code[200] <- "pcn"
language_country_dataset$country_code[207] <- "rus"
language_country_dataset$country_code[215] <- "maf"
language_country_dataset$country_code[228] <- "sxm"
language_country_dataset$country_code[241] <- "sjm"
language_country_dataset$country_code[246] <- "syr"
language_country_dataset$country_code[250] <- "tza"
language_country_dataset$country_code[275] <- "vnm"
language_country_dataset$country_code[276] <- "vir"
language_country_dataset$country_code[278] <- "pse"

# changing country codes to match other dataset
language_country_dataset$country_code[4] <- "alg"
language_country_dataset$country_code[62] <- "cro"
language_country_dataset$country_code[69] <- "den"
language_country_dataset$country_code[69] <- "den"
language_country_dataset$country_code[97] <- "ger"
language_country_dataset$country_code[89] <- "gui"
language_country_dataset$country_code[179] <- "ned"
language_country_dataset$country_code[202] <- "por"
language_country_dataset$country_code[245] <- "sui"
language_country_dataset$country_code[269] <- "uru"

# renaming countries
language_country_dataset$country[89] <- "guinea"
language_country_dataset$country[135] <- "north korea"
language_country_dataset$country[136] <- "republic of korea"
language_country_dataset$country[137] <- "south korea"
language_country_dataset$country[153] <- "macedonia"
language_country_dataset$country[166] <- "micronesia"
language_country_dataset$country[193] <- "palestine"

# naming languages
language_country_dataset$Languages[9] <- "russian"
language_country_dataset$Languages[17] <- "english"
language_country_dataset$Languages[29] <- "dutch"
language_country_dataset$Languages[32] <- "norwegian"
language_country_dataset$Languages[34] <- "english"
language_country_dataset$Languages[37] <- "malay"
language_country_dataset$Languages[44] <- "french"
language_country_dataset$Languages[46] <- "portuguese"
language_country_dataset$Languages[56] <- "french"
language_country_dataset$Languages[66] <- "czech"
language_country_dataset$Languages[84] <- "english"
language_country_dataset$Languages[89] <- "english"
language_country_dataset$Languages[91] <- "french"
language_country_dataset$Languages[93] <- "english"
language_country_dataset$Languages[103] <- "french"
language_country_dataset$Languages[111] <- "english"
language_country_dataset$Languages[112] <- "italian"
language_country_dataset$Languages[121] <- "arabic"
language_country_dataset$Languages[136] <- "korean"
language_country_dataset$Languages[141] <- "lao"
language_country_dataset$Languages[151] <- "cantonese"
language_country_dataset$Languages[153] <- "macedonian"
language_country_dataset$Languages[161] <- "french"
language_country_dataset$Languages[164] <- "french"
language_country_dataset$Languages[167] <- "moldovan"
language_country_dataset$Languages[175] <- "burmese"
language_country_dataset$Languages[193] <- "arabic"
language_country_dataset$Languages[199] <- "english"
language_country_dataset$Languages[205] <- "french"
language_country_dataset$Languages[211] <- "english"
language_country_dataset$Languages[235] <- "english"
language_country_dataset$Languages[242] <- "norwegian"
language_country_dataset$Languages[243] <- "english"
language_country_dataset$Languages[247] <- "arabic"
language_country_dataset$Languages[268] <- "english"
language_country_dataset$Languages[270] <- "english"
language_country_dataset$Languages[274] <- "english"

# adding missing countries
# england
add_england <- data.frame("england","eng","english")
names(add_england)<-c("country","country_code","Languages")
language_country_dataset <- rbind(add_england, language_country_dataset)
# scotland
add_scotland <- data.frame("scotland","sco","english")
names(add_scotland)<-c("country","country_code","Languages")
language_country_dataset <- rbind(add_scotland, language_country_dataset)
# wales
add_wales <- data.frame("wale","wal","english")
names(add_wales)<-c("country","country_code","Languages")
language_country_dataset <- rbind(add_wales, language_country_dataset)

subset_lc <- language_country_dataset[c(1:20,22:39,41:59,62:70,72:84,87:96,98:123,125:136,138:170,172:202,204:210,212:214,216:218,220:231,233:268,270:275,277:282,284:286),]

# removing irrelevant dataset
rm('add_england')
rm('add_scotland')
rm('add_wales')

#2018-2019 Season
url <- "https://fbref.com/stathead/share.fcgi?id=/Yu2XV&lang=en"
PL1819SZN <- htmltab(doc = url, which = 1)

# removing irrelevant dataset
rm('url')

# country names
PL1819SZN <- extract(PL1819SZN, Nation, c("letter_2", "ccode"), "([^ ]+) (.*)")
# removing unnecessary column
PL1819SZN <- PL1819SZN[c(1:2, 4:32)]
# variable lower case
PL1819SZN$ccode <- tolower(PL1819SZN$ccode)
# merging 2018-2019 data
PL1819SZN$ccode <- ifelse(PL1819SZN$ccode == "chi", "chl", PL1819SZN$ccode)
PL1819SZN <- merge(PL1819SZN, subset_lc, by.x=c("ccode"), by.y=c("country_code"), all = TRUE)
# cleaning data
PL1819SZN <- PL1819SZN[complete.cases(PL1819SZN[ ,2:31]),]
PL1819SZN <- PL1819SZN[complete.cases(PL1819SZN[ ,1]),]

is.character(PL1819SZN$Pos)
PL1819SZN$Pos<- gsub("(.*),.*", "\\1", PL1819SZN$Pos)

PL1819SZN$Pos <- ifelse(PL1819SZN$Pos == "MF" | PL1819SZN$Pos == "FW" | PL1819SZN$Pos == "DM" | PL1819SZN$Pos == "CM" | PL1819SZN$Pos == "WM"
                        | PL1819SZN$Pos == "AM", PL1819SZN$Pos <- PL1819SZN$Pos, PL1819SZN$Pos<-NA)

PL1819SZN <- subset(PL1819SZN, Squad=="Liverpool" | Squad=="Bournemouth" | Squad=="Brighton" | Squad=="Chelsea" | Squad=="Everton"
                    | Squad=="Arsenal" | Squad=="Manchester Utd" | Squad=="Tottenham" | Squad== "Manchester City")

PL1819SZN <- PL1819SZN[complete.cases(PL1819SZN[,4]),]

#2017-2018 Season
url <- "https://fbref.com/stathead/share.fcgi?id=/OZeB4&lang=en"
PL1718SZN <- htmltab(doc = url, which = 1)

# removing irrelevant dataset
rm('url')

# country names
PL1718SZN <- extract(PL1718SZN, Nation, c("2 letters", "country_nickname"), "([^ ]+) (.*)")
# removing unnecessary column
PL1718SZN <- PL1718SZN[-c(3)]
# variable lower case
PL1718SZN$country_nickname <- tolower(PL1718SZN$country_nickname)

PL1718SZN <- merge(PL1718SZN, subset_lc, by.x=c("country_nickname"), by.y=c("country_code"))

PL1718SZN$Pos<- gsub("(.*),.*", "\\1", PL1718SZN$Pos)

PL1718SZN$Pos <- ifelse(PL1718SZN$Pos == "MF" | PL1718SZN$Pos == "FW" | PL1718SZN$Pos == "DM" | PL1718SZN$Pos == "CM" | PL1718SZN$Pos == "WM"
                        | PL1718SZN$Pos == "AM", PL1718SZN$Pos <- PL1718SZN$Pos, PL1718SZN$Pos<-NA)

PL1718SZN <- subset(PL1718SZN, Squad=="Liverpool" | Squad=="Bournemouth" | Squad=="Brighton" | Squad=="Chelsea" | Squad=="Everton"
                    | Squad=="Arsenal" | Squad=="Manchester Utd" | Squad=="Tottenham" | Squad== "Manchester City")

PL1718SZN <- PL1718SZN[complete.cases(PL1718SZN[,4]),]

#2016-2017 Season
url <- "https://fbref.com/stathead/share.fcgi?id=/ofhXN&lang=en"
PL1617SZN <- htmltab(doc = url, which = 1)

# removing irrelevant dataset
rm('url')

# country names
PL1617SZN <- extract(PL1617SZN, Nation, c("2 letters", "country_nickname"), "([^ ]+) (.*)")
# removing unnecessary column
PL1617SZN <- PL1617SZN[-c(3)]
# variable lower case
PL1617SZN$country_nickname <- tolower(PL1617SZN$country_nickname)

PL1617SZN <- merge(PL1617SZN, subset_lc, by.x=c("country_nickname"), by.y=c("country_code"))

PL1617SZN$Pos<- gsub("(.*),.*", "\\1", PL1617SZN$Pos)

PL1617SZN$Pos <- ifelse(PL1617SZN$Pos == "MF" | PL1617SZN$Pos == "FW" | PL1617SZN$Pos == "DM" | PL1617SZN$Pos == "CM" | PL1617SZN$Pos == "WM"
                        | PL1617SZN$Pos == "AM", PL1617SZN$Pos <- PL1617SZN$Pos, PL1617SZN$Pos<-NA)

PL1617SZN <- subset(PL1617SZN, Squad=="Liverpool" | Squad=="Bournemouth" | Squad=="Brighton" | Squad=="Chelsea" | Squad=="Everton"
                    | Squad=="Arsenal" | Squad=="Manchester Utd" | Squad=="Tottenham" | Squad== "Manchester City")

PL1617SZN <- PL1617SZN[complete.cases(PL1617SZN[,4]),]

#2015-2016 Season
url <- "https://fbref.com/stathead/share.fcgi?id=/fd7VT&lang=en"
PL1516SZN <- htmltab(doc = url, which = 1)

# removing irrelevant dataset
rm('url')

# country names
PL1516SZN <- extract(PL1516SZN, Nation, c("2 letters", "country_nickname"), "([^ ]+) (.*)")
# removing unnecessary column
PL1516SZN <- PL1516SZN[-c(3)]
# variable lower case
PL1516SZN$country_nickname <- tolower(PL1516SZN$country_nickname)

PL1516SZN <- merge(PL1516SZN, subset_lc, by.x=c("country_nickname"), by.y=c("country_code"))

is.character(PL1516SZN$Pos)
PL1516SZN$Pos<- gsub("(.*),.*", "\\1", PL1516SZN$Pos)

PL1516SZN$Pos <- ifelse(PL1516SZN$Pos == "MF" | PL1516SZN$Pos == "FW" | PL1516SZN$Pos == "DM" | PL1516SZN$Pos == "CM" | PL1516SZN$Pos == "WM"
                        | PL1516SZN$Pos == "AM", PL1516SZN$Pos <- PL1516SZN$Pos, PL1516SZN$Pos<-NA)

PL1516SZN <- subset(PL1516SZN, Squad=="Liverpool" | Squad=="Bournemouth" | Squad=="Brighton" | Squad=="Chelsea" | Squad=="Everton"
                    | Squad=="Arsenal" | Squad=="Manchester Utd" | Squad=="Tottenham" | Squad== "Manchester City")

PL1516SZN <- PL1516SZN[complete.cases(PL1516SZN[,4]),]

#2014-2015 Season
url <- "https://fbref.com/stathead/share.fcgi?id=/83YtG&lang=en"
PL1415SZN <- htmltab(doc = url, which = 1)

# removing irrelevant dataset
rm('url')

# country names
PL1415SZN <- extract(PL1415SZN, Nation, c("2 letters", "country_nickname"), "([^ ]+) (.*)")
# removing unnecessary column
PL1415SZN <- PL1415SZN[-c(3)]
# variable lower case
PL1415SZN$country_nickname <- tolower(PL1415SZN$country_nickname)

PL1415SZN <- merge(PL1415SZN, subset_lc, by.x=c("country_nickname"), by.y=c("country_code"))

is.character(PL1415SZN$Pos)
PL1415SZN$Pos<- gsub("(.*),.*", "\\1", PL1415SZN$Pos)

PL1415SZN$Pos <- ifelse(PL1415SZN$Pos == "MF" | PL1415SZN$Pos == "FW" | PL1415SZN$Pos == "DM" | PL1415SZN$Pos == "CM" | PL1415SZN$Pos == "WM"
                        | PL1415SZN$Pos == "AM", PL1415SZN$Pos <- PL1415SZN$Pos, PL1415SZN$Pos<-NA)

PL1415SZN <- subset(PL1415SZN, Squad=="Liverpool" | Squad=="Bournemouth" | Squad=="Brighton" | Squad=="Chelsea" | Squad=="Everton"
                    | Squad=="Arsenal" | Squad=="Manchester Utd" | Squad=="Tottenham" | Squad== "Manchester City")

PL1415SZN <- PL1415SZN[complete.cases(PL1415SZN[,4]),]

#2013-2014 Season
url <- "https://fbref.com/stathead/share.fcgi?id=/q42nP&lang=en"
PL1314SZN <- htmltab(doc = url, which = 1)

# removing irrelevant dataset
rm('url')

# country names
PL1314SZN <- extract(PL1314SZN, Nation, c("2 letters", "country_nickname"), "([^ ]+) (.*)")
# removing unnecessary column
PL1314SZN <- PL1314SZN[-c(3)]
# variable lower case
PL1314SZN$country_nickname <- tolower(PL1314SZN$country_nickname)

PL1314SZN <- merge(PL1314SZN, subset_lc, by.x=c("country_nickname"), by.y=c("country_code"))

is.character(PL1314SZN$Pos)
PL1314SZN$Pos<- gsub("(.*),.*", "\\1", PL1314SZN$Pos)

PL1314SZN$Pos <- ifelse(PL1314SZN$Pos == "MF" | PL1314SZN$Pos == "FW" | PL1314SZN$Pos == "DM" | PL1314SZN$Pos == "CM" | PL1314SZN$Pos == "WM"
                        | PL1314SZN$Pos == "AM", PL1314SZN$Pos <- PL1314SZN$Pos, PL1314SZN$Pos<-NA)

PL1314SZN <- subset(PL1314SZN, Squad=="Liverpool" | Squad=="Bournemouth" | Squad=="Brighton" | Squad=="Chelsea" | Squad=="Everton"
                    | Squad=="Arsenal" | Squad=="Manchester Utd" | Squad=="Tottenham" | Squad== "Manchester City")


PL1314SZN <- PL1314SZN[complete.cases(PL1314SZN[,4]),]

#2012-2013 Season
url <- "https://fbref.com/stathead/share.fcgi?id=/SYgN5&lang=en"
PL1213SZN <- htmltab(doc = url, which = 1)

# removing irrelevant dataset
rm('url')

# country names
PL1213SZN <- extract(PL1213SZN, Nation, c("2 letters", "country_nickname"), "([^ ]+) (.*)")
# removing unnecessary column
PL1213SZN <- PL1213SZN[-c(3)]
# variable lower case
PL1213SZN$country_nickname <- tolower(PL1213SZN$country_nickname)

PL1213SZN <- merge(PL1213SZN, subset_lc, by.x=c("country_nickname"), by.y=c("country_code"))

is.character(PL1213SZN$Pos)
PL1213SZN$Pos<- gsub("(.*),.*", "\\1", PL1213SZN$Pos)

PL1213SZN$Pos <- ifelse(PL1213SZN$Pos == "MF" | PL1213SZN$Pos == "FW" | PL1213SZN$Pos == "DM" | PL1213SZN$Pos == "CM" | PL1213SZN$Pos == "WM"
                        | PL1213SZN$Pos == "AM", PL1213SZN$Pos <- PL1213SZN$Pos, PL1213SZN$Pos<-NA)

PL1213SZN <- subset(PL1213SZN, Squad=="Liverpool" | Squad=="Bournemouth" | Squad=="Brighton" | Squad=="Chelsea" | Squad=="Everton"
                    | Squad=="Arsenal" | Squad=="Manchester Utd" | Squad=="Tottenham" | Squad== "Manchester City")

PL1213SZN <- PL1213SZN[complete.cases(PL1213SZN[,4]),]

#2011-2012 Season
url <- "https://fbref.com/stathead/share.fcgi?id=/MiVfH&lang=en"
PL1112SZN <- htmltab(doc = url, which = 1)

# removing irrelevant dataset
rm('url')

# country names
PL1112SZN <- extract(PL1112SZN, Nation, c("2 letters", "country_nickname"), "([^ ]+) (.*)")
# removing unnecessary column
PL1112SZN <- PL1112SZN[-c(3)]
# variable lower case
PL1112SZN$country_nickname <- tolower(PL1112SZN$country_nickname)

PL1112SZN <- merge(PL1112SZN, subset_lc, by.x=c("country_nickname"), by.y=c("country_code"))

is.character(PL1112SZN$Pos)
PL1112SZN$Pos<- gsub("(.*),.*", "\\1", PL1112SZN$Pos)

PL1112SZN$Pos <- ifelse(PL1112SZN$Pos == "MF" | PL1112SZN$Pos == "FW" | PL1112SZN$Pos == "DM" | PL1112SZN$Pos == "CM" | PL1112SZN$Pos == "WM"
                        | PL1112SZN$Pos == "AM", PL1112SZN$Pos <- PL1112SZN$Pos, PL1112SZN$Pos<-NA)

PL1112SZN <- subset(PL1112SZN, Squad=="Liverpool" | Squad=="Bournemouth" | Squad=="Brighton" | Squad=="Chelsea" | Squad=="Everton"
                    | Squad=="Arsenal" | Squad=="Manchester Utd" | Squad=="Tottenham" | Squad== "Manchester City")

PL1112SZN <- PL1112SZN[complete.cases(PL1112SZN[,4]),]

#2010-2011 Season
url <- "https://fbref.com/stathead/share.fcgi?id=/fSUno&lang=en"
PL1011SZN <- htmltab(doc = url, which = 1)

# removing irrelevant dataset
rm('url')

# country names
PL1011SZN <- extract(PL1011SZN, Nation, c("2 letters", "country_nickname"), "([^ ]+) (.*)")
# removing unnecessary column
PL1011SZN <- PL1011SZN[-c(3)]
# variable lower case
PL1011SZN$country_nickname <- tolower(PL1011SZN$country_nickname)

PL1011SZN <- merge(PL1011SZN, subset_lc, by.x=c("country_nickname"), by.y=c("country_code"))

is.character(PL1011SZN$Pos)
PL1011SZN$Pos<- gsub("(.*),.*", "\\1", PL1011SZN$Pos)

PL1011SZN$Pos <- ifelse(PL1011SZN$Pos == "MF" | PL1011SZN$Pos == "FW" | PL1011SZN$Pos == "DM" | PL1011SZN$Pos == "CM" | PL1011SZN$Pos == "WM"
                        | PL1011SZN$Pos == "AM", PL1011SZN$Pos <- PL1011SZN$Pos, PL1011SZN$Pos<-NA)

PL1011SZN <- subset(PL1011SZN, Squad=="Liverpool" | Squad=="Bournemouth" | Squad=="Brighton" | Squad=="Chelsea" | Squad=="Everton"
                    | Squad=="Arsenal" | Squad=="Manchester Utd" | Squad=="Tottenham" | Squad== "Manchester City")

PL1011SZN <- PL1011SZN[complete.cases(PL1011SZN[,4]),]

#2009-2010 Season
url <- "https://fbref.com/stathead/share.fcgi?id=/3W1CU&lang=en"
PL910SZN <- htmltab(doc = url, which = 1)

# removing irrelevant dataset
rm('url')

# country names
PL910SZN <- extract(PL910SZN, Nation, c("2 letters", "country_nickname"), "([^ ]+) (.*)")
# removing unnecessary column
PL910SZN <- PL910SZN[-c(3)]
# variable lower case
PL910SZN$country_nickname <- tolower(PL910SZN$country_nickname)

PL910SZN <- merge(PL910SZN, subset_lc, by.x=c("country_nickname"), by.y=c("country_code"))

is.character(PL910SZN$Pos)
PL910SZN$Pos<- gsub("(.*),.*", "\\1", PL910SZN$Pos)

PL910SZN$Pos <- ifelse(PL910SZN$Pos == "MF" | PL910SZN$Pos == "FW" | PL910SZN$Pos == "DM" | PL910SZN$Pos == "CM" | PL910SZN$Pos == "WM"
                       | PL910SZN$Pos == "AM", PL910SZN$Pos <- PL910SZN$Pos, PL910SZN$Pos<-NA)

PL910SZN <- subset(PL910SZN, Squad=="Liverpool" | Squad=="Bournemouth" | Squad=="Brighton" | Squad=="Chelsea" | Squad=="Everton"
                   | Squad=="Arsenal" | Squad=="Manchester Utd" | Squad=="Tottenham" | Squad== "Manchester City")

PL910SZN <- PL910SZN[complete.cases(PL910SZN[,4]),]

#Creating language variables for each season
PL1819SZN$english <- 0
PL1819SZN$french <- 0
PL1819SZN$portuguese <- 0
PL1819SZN$castilian <- 0
PL1819SZN$dutch <- 0
PL1819SZN$german <- 0
PL1819SZN$spanish <- 0
PL1819SZN$arabic <- 0
PL1819SZN$serbian <- 0
PL1819SZN$korean <- 0
PL1819SZN$croatian <- 0
PL1819SZN$italian <- 0
PL1819SZN$icelandic <- 0
PL1819SZN$hebrew <- 0
PL1819SZN$bokmal <- 0
PL1819SZN$ukrainian <- 0
PL1819SZN$russian <- 0
PL1819SZN$bosnian <- 0
PL1819SZN$czech <- 0
PL1819SZN$danish <- 0
PL1819SZN$armenian <- 0
PL1819SZN$turkish <- 0
PL1819SZN$asante <- 0
PL1819SZN$persian <- 0
PL1819SZN$japanese <- 0
PL1819SZN$maltese <- 0
PL1819SZN$romanian <- 0
PL1819SZN$slovak <- 0
PL1819SZN$swedish <- 0

Arsenal1819Data <- subset(PL1819SZN, Squad =="Arsenal")
Arsenal1819Data$english <- ifelse(Arsenal1819Data$Languages=="english", 1, 0)
Arsenal1819Data$french <- ifelse(Arsenal1819Data$Languages=="french", 1, 0)
Arsenal1819Data$dutch <- ifelse(Arsenal1819Data$Languages=="dutch", 1, 0)
Arsenal1819Data$portuguese <- ifelse(Arsenal1819Data$Languages=="portuguese", 1, 0)
Arsenal1819Data$castilian <- ifelse(Arsenal1819Data$Languages=="castilian", 1, 0)
Arsenal1819Data$spanish <- ifelse(Arsenal1819Data$Languages=="spanish", 1, 0)
Arsenal1819Data$german <- ifelse(Arsenal1819Data$Languages=="german", 1, 0)
Arsenal1819Data$arabic <- ifelse(Arsenal1819Data$Languages=="arabic", 1, 0)
Arsenal1819Data$serbian <- ifelse(Arsenal1819Data$Languages=="serbian", 1, 0)
Arsenal1819Data$korean <- ifelse(Arsenal1819Data$Languages=="korean", 1, 0)
Arsenal1819Data$croatian <- ifelse(Arsenal1819Data$Languages=="croatian", 1, 0)
Arsenal1819Data$italian <- ifelse(Arsenal1819Data$Languages=="italian", 1, 0)
Arsenal1819Data$armenian <- ifelse(Arsenal1819Data$Languages=="armenian", 1, 0)
Arsenal1819Data$icelandic <- ifelse(Arsenal1819Data$Languages=="icelandic", 1, 0)
Arsenal1819Data$hebrew <- ifelse(Arsenal1819Data$Languages=="hebrew", 1, 0)
Arsenal1819Data$bokmal <- ifelse(Arsenal1819Data$Languages=="bokmal", 1, 0)
Arsenal1819Data$ukrainian <- ifelse(Arsenal1819Data$Languages=="ukrainian", 1, 0)
Arsenal1819Data$russian <- ifelse(Arsenal1819Data$Languages=="russian", 1, 0)
Arsenal1819Data$bosnian <- ifelse(Arsenal1819Data$Languages=="bosnian", 1, 0)
Arsenal1819Data$czech <- ifelse(Arsenal1819Data$Languages=="czech", 1, 0)
Arsenal1819Data$danish <- ifelse(Arsenal1819Data$Languages=="danish", 1, 0)
Arsenal1819Data$swedish <- ifelse(Arsenal1819Data$Languages=="swedish", 1, 0)
Arsenal1819Data$japanese <- ifelse(Arsenal1819Data$Languages=="japanese", 1, 0)
Arsenal1819Data$turkish <- ifelse(Arsenal1819Data$Languages=="turkish", 1, 0)
Arsenal1819Data$asante <- ifelse(Arsenal1819Data$Languages=="asante", 1, 0)
Arsenal1819Data$persian <- ifelse(Arsenal1819Data$Languages=="persian", 1, 0)
Arsenal1819Data$maltese <- ifelse(Arsenal1819Data$Languages=="maltese", 1, 0)
Arsenal1819Data$romanian <- ifelse(Arsenal1819Data$Languages=="romanian", 1, 0)
Arsenal1819Data$slovak <- ifelse(Arsenal1819Data$Languages=="slovak", 1, 0)
Arsenal1819Data$PropLanguages <- 0
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "english", sum(Arsenal1819Data$english), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "french", sum(Arsenal1819Data$french), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "dutch", sum(Arsenal1819Data$dutch), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "portuguese", sum(Arsenal1819Data$portuguese), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "castilian", sum(Arsenal1819Data$castilian), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "spanish", sum(Arsenal1819Data$spanish), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "german", sum(Arsenal1819Data$german), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "arabic", sum(Arsenal1819Data$arabic), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "serbian", sum(Arsenal1819Data$serbian), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "korean", sum(Arsenal1819Data$korean), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "croatian", sum(Arsenal1819Data$croatian), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "italian", sum(Arsenal1819Data$italian), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "armenian", sum(Arsenal1819Data$armenian), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "icelandic", sum(Arsenal1819Data$icelandic), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "hebrew", sum(Arsenal1819Data$hebrew), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "bokmal", sum(Arsenal1819Data$bokmal), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "ukrainian", sum(Arsenal1819Data$ukrainian), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "russian", sum(Arsenal1819Data$russian), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "bosnian", sum(Arsenal1819Data$bosnian), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "czech", sum(Arsenal1819Data$czech), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "danish", sum(Arsenal1819Data$danish), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "swedish", sum(Arsenal1819Data$swedish), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "japanese", sum(Arsenal1819Data$japanese), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "turkish", sum(Arsenal1819Data$turkish), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "asante", sum(Arsenal1819Data$asante), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "persian", sum(Arsenal1819Data$persian), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "maltese", sum(Arsenal1819Data$maltese), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "romanian", sum(Arsenal1819Data$romanian), Arsenal1819Data$PropLanguages)
Arsenal1819Data$PropLanguages <- ifelse(Arsenal1819Data$Languages == "slovak", sum(Arsenal1819Data$slovak), Arsenal1819Data$PropLanguages)

Chelsea1819Data <- subset(PL1819SZN, Squad =="Chelsea")
Chelsea1819Data$english <- ifelse(Chelsea1819Data$Languages=="english", 1, 0)
Chelsea1819Data$french <- ifelse(Chelsea1819Data$Languages=="french", 1, 0)
Chelsea1819Data$dutch <- ifelse(Chelsea1819Data$Languages=="dutch", 1, 0)
Chelsea1819Data$portuguese <- ifelse(Chelsea1819Data$Languages=="portuguese", 1, 0)
Chelsea1819Data$castilian <- ifelse(Chelsea1819Data$Languages=="castilian", 1, 0)
Chelsea1819Data$spanish <- ifelse(Chelsea1819Data$Languages=="spanish", 1, 0)
Chelsea1819Data$german <- ifelse(Chelsea1819Data$Languages=="german", 1, 0)
Chelsea1819Data$arabic <- ifelse(Chelsea1819Data$Languages=="arabic", 1, 0)
Chelsea1819Data$serbian <- ifelse(Chelsea1819Data$Languages=="serbian", 1, 0)
Chelsea1819Data$korean <- ifelse(Chelsea1819Data$Languages=="korean", 1, 0)
Chelsea1819Data$croatian <- ifelse(Chelsea1819Data$Languages=="croatian", 1, 0)
Chelsea1819Data$italian <- ifelse(Chelsea1819Data$Languages=="italian", 1, 0)
Chelsea1819Data$armenian <- ifelse(Chelsea1819Data$Languages=="armenian", 1, 0)
Chelsea1819Data$icelandic <- ifelse(Chelsea1819Data$Languages=="icelandic", 1, 0)
Chelsea1819Data$hebrew <- ifelse(Chelsea1819Data$Languages=="hebrew", 1, 0)
Chelsea1819Data$bokmal <- ifelse(Chelsea1819Data$Languages=="bokmal", 1, 0)
Chelsea1819Data$ukrainian <- ifelse(Chelsea1819Data$Languages=="ukrainian", 1, 0)
Chelsea1819Data$russian <- ifelse(Chelsea1819Data$Languages=="russian", 1, 0)
Chelsea1819Data$bosnian <- ifelse(Chelsea1819Data$Languages=="bosnian", 1, 0)
Chelsea1819Data$czech <- ifelse(Chelsea1819Data$Languages=="czech", 1, 0)
Chelsea1819Data$danish <- ifelse(Chelsea1819Data$Languages=="danish", 1, 0)
Chelsea1819Data$swedish <- ifelse(Chelsea1819Data$Languages=="swedish", 1, 0)
Chelsea1819Data$japanese <- ifelse(Chelsea1819Data$Languages=="japanese", 1, 0)
Chelsea1819Data$turkish <- ifelse(Chelsea1819Data$Languages=="turkish", 1, 0)
Chelsea1819Data$asante <- ifelse(Chelsea1819Data$Languages=="asante", 1, 0)
Chelsea1819Data$persian <- ifelse(Chelsea1819Data$Languages=="persian", 1, 0)
Chelsea1819Data$maltese <- ifelse(Chelsea1819Data$Languages=="maltese", 1, 0)
Chelsea1819Data$romanian <- ifelse(Chelsea1819Data$Languages=="romanian", 1, 0)
Chelsea1819Data$slovak <- ifelse(Chelsea1819Data$Languages=="slovak", 1, 0)
Chelsea1819Data$PropLanguages <- 0
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "english", sum(Chelsea1819Data$english), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "french", sum(Chelsea1819Data$french), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "dutch", sum(Chelsea1819Data$dutch), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "portuguese", sum(Chelsea1819Data$portuguese), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "castilian", sum(Chelsea1819Data$castilian), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "spanish", sum(Chelsea1819Data$spanish), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "german", sum(Chelsea1819Data$german), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "arabic", sum(Chelsea1819Data$arabic), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "serbian", sum(Chelsea1819Data$serbian), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "korean", sum(Chelsea1819Data$korean), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "croatian", sum(Chelsea1819Data$croatian), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "italian", sum(Chelsea1819Data$italian), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "armenian", sum(Chelsea1819Data$armenian), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "icelandic", sum(Chelsea1819Data$icelandic), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "hebrew", sum(Chelsea1819Data$hebrew), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "bokmal", sum(Chelsea1819Data$bokmal), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "ukrainian", sum(Chelsea1819Data$ukrainian), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "russian", sum(Chelsea1819Data$russian), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "bosnian", sum(Chelsea1819Data$bosnian), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "czech", sum(Chelsea1819Data$czech), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "danish", sum(Chelsea1819Data$danish), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "swedish", sum(Chelsea1819Data$swedish), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "japanese", sum(Chelsea1819Data$japanese), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "turkish", sum(Chelsea1819Data$turkish), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "asante", sum(Chelsea1819Data$asante), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "persian", sum(Chelsea1819Data$persian), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "maltese", sum(Chelsea1819Data$maltese), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "romanian", sum(Chelsea1819Data$romanian), Chelsea1819Data$PropLanguages)
Chelsea1819Data$PropLanguages <- ifelse(Chelsea1819Data$Languages == "slovak", sum(Chelsea1819Data$slovak), Chelsea1819Data$PropLanguages)

Everton1819Data <- subset(PL1819SZN, Squad =="Everton")
Everton1819Data$english <- ifelse(Everton1819Data$Languages=="english", 1, 0)
Everton1819Data$french <- ifelse(Everton1819Data$Languages=="french", 1, 0)
Everton1819Data$dutch <- ifelse(Everton1819Data$Languages=="dutch", 1, 0)
Everton1819Data$portuguese <- ifelse(Everton1819Data$Languages=="portuguese", 1, 0)
Everton1819Data$castilian <- ifelse(Everton1819Data$Languages=="castilian", 1, 0)
Everton1819Data$spanish <- ifelse(Everton1819Data$Languages=="spanish", 1, 0)
Everton1819Data$german <- ifelse(Everton1819Data$Languages=="german", 1, 0)
Everton1819Data$arabic <- ifelse(Everton1819Data$Languages=="arabic", 1, 0)
Everton1819Data$serbian <- ifelse(Everton1819Data$Languages=="serbian", 1, 0)
Everton1819Data$korean <- ifelse(Everton1819Data$Languages=="korean", 1, 0)
Everton1819Data$croatian <- ifelse(Everton1819Data$Languages=="croatian", 1, 0)
Everton1819Data$italian <- ifelse(Everton1819Data$Languages=="italian", 1, 0)
Everton1819Data$armenian <- ifelse(Everton1819Data$Languages=="armenian", 1, 0)
Everton1819Data$icelandic <- ifelse(Everton1819Data$Languages=="icelandic", 1, 0)
Everton1819Data$hebrew <- ifelse(Everton1819Data$Languages=="hebrew", 1, 0)
Everton1819Data$bokmal <- ifelse(Everton1819Data$Languages=="bokmal", 1, 0)
Everton1819Data$ukrainian <- ifelse(Everton1819Data$Languages=="ukrainian", 1, 0)
Everton1819Data$russian <- ifelse(Everton1819Data$Languages=="russian", 1, 0)
Everton1819Data$bosnian <- ifelse(Everton1819Data$Languages=="bosnian", 1, 0)
Everton1819Data$czech <- ifelse(Everton1819Data$Languages=="czech", 1, 0)
Everton1819Data$danish <- ifelse(Everton1819Data$Languages=="danish", 1, 0)
Everton1819Data$swedish <- ifelse(Everton1819Data$Languages=="swedish", 1, 0)
Everton1819Data$japanese <- ifelse(Everton1819Data$Languages=="japanese", 1, 0)
Everton1819Data$turkish <- ifelse(Everton1819Data$Languages=="turkish", 1, 0)
Everton1819Data$asante <- ifelse(Everton1819Data$Languages=="asante", 1, 0)
Everton1819Data$persian <- ifelse(Everton1819Data$Languages=="persian", 1, 0)
Everton1819Data$maltese <- ifelse(Everton1819Data$Languages=="maltese", 1, 0)
Everton1819Data$romanian <- ifelse(Everton1819Data$Languages=="romanian", 1, 0)
Everton1819Data$slovak <- ifelse(Everton1819Data$Languages=="slovak", 1, 0)
Everton1819Data$PropLanguages <- 0
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "english", sum(Everton1819Data$english), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "french", sum(Everton1819Data$french), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "dutch", sum(Everton1819Data$dutch), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "portuguese", sum(Everton1819Data$portuguese), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "castilian", sum(Everton1819Data$castilian), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "spanish", sum(Everton1819Data$spanish), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "german", sum(Everton1819Data$german), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "arabic", sum(Everton1819Data$arabic), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "serbian", sum(Everton1819Data$serbian), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "korean", sum(Everton1819Data$korean), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "croatian", sum(Everton1819Data$croatian), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "italian", sum(Everton1819Data$italian), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "armenian", sum(Everton1819Data$armenian), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "icelandic", sum(Everton1819Data$icelandic), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "hebrew", sum(Everton1819Data$hebrew), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "bokmal", sum(Everton1819Data$bokmal), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "ukrainian", sum(Everton1819Data$ukrainian), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "russian", sum(Everton1819Data$russian), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "bosnian", sum(Everton1819Data$bosnian), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "czech", sum(Everton1819Data$czech), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "danish", sum(Everton1819Data$danish), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "swedish", sum(Everton1819Data$swedish), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "japanese", sum(Everton1819Data$japanese), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "turkish", sum(Everton1819Data$turkish), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "asante", sum(Everton1819Data$asante), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "persian", sum(Everton1819Data$persian), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "maltese", sum(Everton1819Data$maltese), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "romanian", sum(Everton1819Data$romanian), Everton1819Data$PropLanguages)
Everton1819Data$PropLanguages <- ifelse(Everton1819Data$Languages == "slovak", sum(Everton1819Data$slovak), Everton1819Data$PropLanguages)

Liverpool1819Data <- subset(PL1819SZN, Squad =="Liverpool")
Liverpool1819Data$english <- ifelse(Liverpool1819Data$Languages=="english", 1, 0)
Liverpool1819Data$french <- ifelse(Liverpool1819Data$Languages=="french", 1, 0)
Liverpool1819Data$dutch <- ifelse(Liverpool1819Data$Languages=="dutch", 1, 0)
Liverpool1819Data$portuguese <- ifelse(Liverpool1819Data$Languages=="portuguese", 1, 0)
Liverpool1819Data$castilian <- ifelse(Liverpool1819Data$Languages=="castilian", 1, 0)
Liverpool1819Data$spanish <- ifelse(Liverpool1819Data$Languages=="spanish", 1, 0)
Liverpool1819Data$german <- ifelse(Liverpool1819Data$Languages=="german", 1, 0)
Liverpool1819Data$arabic <- ifelse(Liverpool1819Data$Languages=="arabic", 1, 0)
Liverpool1819Data$serbian <- ifelse(Liverpool1819Data$Languages=="serbian", 1, 0)
Liverpool1819Data$korean <- ifelse(Liverpool1819Data$Languages=="korean", 1, 0)
Liverpool1819Data$croatian <- ifelse(Liverpool1819Data$Languages=="croatian", 1, 0)
Liverpool1819Data$italian <- ifelse(Liverpool1819Data$Languages=="italian", 1, 0)
Liverpool1819Data$armenian <- ifelse(Liverpool1819Data$Languages=="armenian", 1, 0)
Liverpool1819Data$icelandic <- ifelse(Liverpool1819Data$Languages=="icelandic", 1, 0)
Liverpool1819Data$hebrew <- ifelse(Liverpool1819Data$Languages=="hebrew", 1, 0)
Liverpool1819Data$bokmal <- ifelse(Liverpool1819Data$Languages=="bokmal", 1, 0)
Liverpool1819Data$ukrainian <- ifelse(Liverpool1819Data$Languages=="ukrainian", 1, 0)
Liverpool1819Data$russian <- ifelse(Liverpool1819Data$Languages=="russian", 1, 0)
Liverpool1819Data$bosnian <- ifelse(Liverpool1819Data$Languages=="bosnian", 1, 0)
Liverpool1819Data$czech <- ifelse(Liverpool1819Data$Languages=="czech", 1, 0)
Liverpool1819Data$danish <- ifelse(Liverpool1819Data$Languages=="danish", 1, 0)
Liverpool1819Data$swedish <- ifelse(Liverpool1819Data$Languages=="swedish", 1, 0)
Liverpool1819Data$japanese <- ifelse(Liverpool1819Data$Languages=="japanese", 1, 0)
Liverpool1819Data$turkish <- ifelse(Liverpool1819Data$Languages=="turkish", 1, 0)
Liverpool1819Data$asante <- ifelse(Liverpool1819Data$Languages=="asante", 1, 0)
Liverpool1819Data$persian <- ifelse(Liverpool1819Data$Languages=="persian", 1, 0)
Liverpool1819Data$maltese <- ifelse(Liverpool1819Data$Languages=="maltese", 1, 0)
Liverpool1819Data$romanian <- ifelse(Liverpool1819Data$Languages=="romanian", 1, 0)
Liverpool1819Data$slovak <- ifelse(Liverpool1819Data$Languages=="slovak", 1, 0)
Liverpool1819Data$PropLanguages <- 0
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "english", sum(Liverpool1819Data$english), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "french", sum(Liverpool1819Data$french), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "dutch", sum(Liverpool1819Data$dutch), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "portuguese", sum(Liverpool1819Data$portuguese), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "castilian", sum(Liverpool1819Data$castilian), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "spanish", sum(Liverpool1819Data$spanish), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "german", sum(Liverpool1819Data$german), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "arabic", sum(Liverpool1819Data$arabic), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "serbian", sum(Liverpool1819Data$serbian), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "korean", sum(Liverpool1819Data$korean), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "croatian", sum(Liverpool1819Data$croatian), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "italian", sum(Liverpool1819Data$italian), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "armenian", sum(Liverpool1819Data$armenian), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "icelandic", sum(Liverpool1819Data$icelandic), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "hebrew", sum(Liverpool1819Data$hebrew), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "bokmal", sum(Liverpool1819Data$bokmal), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "ukrainian", sum(Liverpool1819Data$ukrainian), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "russian", sum(Liverpool1819Data$russian), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "bosnian", sum(Liverpool1819Data$bosnian), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "czech", sum(Liverpool1819Data$czech), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "danish", sum(Liverpool1819Data$danish), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "swedish", sum(Liverpool1819Data$swedish), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "japanese", sum(Liverpool1819Data$japanese), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "turkish", sum(Liverpool1819Data$turkish), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "asante", sum(Liverpool1819Data$asante), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "persian", sum(Liverpool1819Data$persian), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "maltese", sum(Liverpool1819Data$maltese), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "romanian", sum(Liverpool1819Data$romanian), Liverpool1819Data$PropLanguages)
Liverpool1819Data$PropLanguages <- ifelse(Liverpool1819Data$Languages == "slovak", sum(Liverpool1819Data$slovak), Liverpool1819Data$PropLanguages)

ManchesterCity1819Data <- subset(PL1819SZN, Squad =="Manchester City")
ManchesterCity1819Data$english <- ifelse(ManchesterCity1819Data$Languages=="english", 1, 0)
ManchesterCity1819Data$french <- ifelse(ManchesterCity1819Data$Languages=="french", 1, 0)
ManchesterCity1819Data$dutch <- ifelse(ManchesterCity1819Data$Languages=="dutch", 1, 0)
ManchesterCity1819Data$portuguese <- ifelse(ManchesterCity1819Data$Languages=="portuguese", 1, 0)
ManchesterCity1819Data$castilian <- ifelse(ManchesterCity1819Data$Languages=="castilian", 1, 0)
ManchesterCity1819Data$spanish <- ifelse(ManchesterCity1819Data$Languages=="spanish", 1, 0)
ManchesterCity1819Data$german <- ifelse(ManchesterCity1819Data$Languages=="german", 1, 0)
ManchesterCity1819Data$arabic <- ifelse(ManchesterCity1819Data$Languages=="arabic", 1, 0)
ManchesterCity1819Data$serbian <- ifelse(ManchesterCity1819Data$Languages=="serbian", 1, 0)
ManchesterCity1819Data$korean <- ifelse(ManchesterCity1819Data$Languages=="korean", 1, 0)
ManchesterCity1819Data$croatian <- ifelse(ManchesterCity1819Data$Languages=="croatian", 1, 0)
ManchesterCity1819Data$italian <- ifelse(ManchesterCity1819Data$Languages=="italian", 1, 0)
ManchesterCity1819Data$armenian <- ifelse(ManchesterCity1819Data$Languages=="armenian", 1, 0)
ManchesterCity1819Data$icelandic <- ifelse(ManchesterCity1819Data$Languages=="icelandic", 1, 0)
ManchesterCity1819Data$hebrew <- ifelse(ManchesterCity1819Data$Languages=="hebrew", 1, 0)
ManchesterCity1819Data$bokmal <- ifelse(ManchesterCity1819Data$Languages=="bokmal", 1, 0)
ManchesterCity1819Data$ukrainian <- ifelse(ManchesterCity1819Data$Languages=="ukrainian", 1, 0)
ManchesterCity1819Data$russian <- ifelse(ManchesterCity1819Data$Languages=="russian", 1, 0)
ManchesterCity1819Data$bosnian <- ifelse(ManchesterCity1819Data$Languages=="bosnian", 1, 0)
ManchesterCity1819Data$czech <- ifelse(ManchesterCity1819Data$Languages=="czech", 1, 0)
ManchesterCity1819Data$danish <- ifelse(ManchesterCity1819Data$Languages=="danish", 1, 0)
ManchesterCity1819Data$swedish <- ifelse(ManchesterCity1819Data$Languages=="swedish", 1, 0)
ManchesterCity1819Data$japanese <- ifelse(ManchesterCity1819Data$Languages=="japanese", 1, 0)
ManchesterCity1819Data$turkish <- ifelse(ManchesterCity1819Data$Languages=="turkish", 1, 0)
ManchesterCity1819Data$asante <- ifelse(ManchesterCity1819Data$Languages=="asante", 1, 0)
ManchesterCity1819Data$persian <- ifelse(ManchesterCity1819Data$Languages=="persian", 1, 0)
ManchesterCity1819Data$maltese <- ifelse(ManchesterCity1819Data$Languages=="maltese", 1, 0)
ManchesterCity1819Data$romanian <- ifelse(ManchesterCity1819Data$Languages=="romanian", 1, 0)
ManchesterCity1819Data$slovak <- ifelse(ManchesterCity1819Data$Languages=="slovak", 1, 0)
ManchesterCity1819Data$PropLanguages <- 0
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "english", sum(ManchesterCity1819Data$english), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "french", sum(ManchesterCity1819Data$french), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "dutch", sum(ManchesterCity1819Data$dutch), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "portuguese", sum(ManchesterCity1819Data$portuguese), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "castilian", sum(ManchesterCity1819Data$castilian), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "spanish", sum(ManchesterCity1819Data$spanish), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "german", sum(ManchesterCity1819Data$german), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "arabic", sum(ManchesterCity1819Data$arabic), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "serbian", sum(ManchesterCity1819Data$serbian), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "korean", sum(ManchesterCity1819Data$korean), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "croatian", sum(ManchesterCity1819Data$croatian), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "italian", sum(ManchesterCity1819Data$italian), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "armenian", sum(ManchesterCity1819Data$armenian), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "icelandic", sum(ManchesterCity1819Data$icelandic), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "hebrew", sum(ManchesterCity1819Data$hebrew), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "bokmal", sum(ManchesterCity1819Data$bokmal), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "ukrainian", sum(ManchesterCity1819Data$ukrainian), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "russian", sum(ManchesterCity1819Data$russian), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "bosnian", sum(ManchesterCity1819Data$bosnian), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "czech", sum(ManchesterCity1819Data$czech), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "danish", sum(ManchesterCity1819Data$danish), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "swedish", sum(ManchesterCity1819Data$swedish), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "japanese", sum(ManchesterCity1819Data$japanese), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "turkish", sum(ManchesterCity1819Data$turkish), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "asante", sum(ManchesterCity1819Data$asante), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "persian", sum(ManchesterCity1819Data$persian), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "maltese", sum(ManchesterCity1819Data$maltese), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "romanian", sum(ManchesterCity1819Data$romanian), ManchesterCity1819Data$PropLanguages)
ManchesterCity1819Data$PropLanguages <- ifelse(ManchesterCity1819Data$Languages == "slovak", sum(ManchesterCity1819Data$slovak), ManchesterCity1819Data$PropLanguages)

ManchesterUtd1819Data <- subset(PL1819SZN, Squad =="Manchester Utd")
ManchesterUtd1819Data$english <- ifelse(ManchesterUtd1819Data$Languages=="english", 1, 0)
ManchesterUtd1819Data$french <- ifelse(ManchesterUtd1819Data$Languages=="french", 1, 0)
ManchesterUtd1819Data$dutch <- ifelse(ManchesterUtd1819Data$Languages=="dutch", 1, 0)
ManchesterUtd1819Data$portuguese <- ifelse(ManchesterUtd1819Data$Languages=="portuguese", 1, 0)
ManchesterUtd1819Data$castilian <- ifelse(ManchesterUtd1819Data$Languages=="castilian", 1, 0)
ManchesterUtd1819Data$spanish <- ifelse(ManchesterUtd1819Data$Languages=="spanish", 1, 0)
ManchesterUtd1819Data$german <- ifelse(ManchesterUtd1819Data$Languages=="german", 1, 0)
ManchesterUtd1819Data$arabic <- ifelse(ManchesterUtd1819Data$Languages=="arabic", 1, 0)
ManchesterUtd1819Data$serbian <- ifelse(ManchesterUtd1819Data$Languages=="serbian", 1, 0)
ManchesterUtd1819Data$korean <- ifelse(ManchesterUtd1819Data$Languages=="korean", 1, 0)
ManchesterUtd1819Data$croatian <- ifelse(ManchesterUtd1819Data$Languages=="croatian", 1, 0)
ManchesterUtd1819Data$italian <- ifelse(ManchesterUtd1819Data$Languages=="italian", 1, 0)
ManchesterUtd1819Data$armenian <- ifelse(ManchesterUtd1819Data$Languages=="armenian", 1, 0)
ManchesterUtd1819Data$icelandic <- ifelse(ManchesterUtd1819Data$Languages=="icelandic", 1, 0)
ManchesterUtd1819Data$hebrew <- ifelse(ManchesterUtd1819Data$Languages=="hebrew", 1, 0)
ManchesterUtd1819Data$bokmal <- ifelse(ManchesterUtd1819Data$Languages=="bokmal", 1, 0)
ManchesterUtd1819Data$ukrainian <- ifelse(ManchesterUtd1819Data$Languages=="ukrainian", 1, 0)
ManchesterUtd1819Data$russian <- ifelse(ManchesterUtd1819Data$Languages=="russian", 1, 0)
ManchesterUtd1819Data$bosnian <- ifelse(ManchesterUtd1819Data$Languages=="bosnian", 1, 0)
ManchesterUtd1819Data$czech <- ifelse(ManchesterUtd1819Data$Languages=="czech", 1, 0)
ManchesterUtd1819Data$danish <- ifelse(ManchesterUtd1819Data$Languages=="danish", 1, 0)
ManchesterUtd1819Data$swedish <- ifelse(ManchesterUtd1819Data$Languages=="swedish", 1, 0)
ManchesterUtd1819Data$japanese <- ifelse(ManchesterUtd1819Data$Languages=="japanese", 1, 0)
ManchesterUtd1819Data$turkish <- ifelse(ManchesterUtd1819Data$Languages=="turkish", 1, 0)
ManchesterUtd1819Data$asante <- ifelse(ManchesterUtd1819Data$Languages=="asante", 1, 0)
ManchesterUtd1819Data$persian <- ifelse(ManchesterUtd1819Data$Languages=="persian", 1, 0)
ManchesterUtd1819Data$maltese <- ifelse(ManchesterUtd1819Data$Languages=="maltese", 1, 0)
ManchesterUtd1819Data$romanian <- ifelse(ManchesterUtd1819Data$Languages=="romanian", 1, 0)
ManchesterUtd1819Data$slovak <- ifelse(ManchesterUtd1819Data$Languages=="slovak", 1, 0)
ManchesterUtd1819Data$PropLanguages <- 0
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "english", sum(ManchesterUtd1819Data$english), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "french", sum(ManchesterUtd1819Data$french), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "dutch", sum(ManchesterUtd1819Data$dutch), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "portuguese", sum(ManchesterUtd1819Data$portuguese), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "castilian", sum(ManchesterUtd1819Data$castilian), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "spanish", sum(ManchesterUtd1819Data$spanish), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "german", sum(ManchesterUtd1819Data$german), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "arabic", sum(ManchesterUtd1819Data$arabic), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "serbian", sum(ManchesterUtd1819Data$serbian), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "korean", sum(ManchesterUtd1819Data$korean), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "croatian", sum(ManchesterUtd1819Data$croatian), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "italian", sum(ManchesterUtd1819Data$italian), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "armenian", sum(ManchesterUtd1819Data$armenian), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "icelandic", sum(ManchesterUtd1819Data$icelandic), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "hebrew", sum(ManchesterUtd1819Data$hebrew), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "bokmal", sum(ManchesterUtd1819Data$bokmal), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "ukrainian", sum(ManchesterUtd1819Data$ukrainian), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "russian", sum(ManchesterUtd1819Data$russian), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "bosnian", sum(ManchesterUtd1819Data$bosnian), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "czech", sum(ManchesterUtd1819Data$czech), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "danish", sum(ManchesterUtd1819Data$danish), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "swedish", sum(ManchesterUtd1819Data$swedish), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "japanese", sum(ManchesterUtd1819Data$japanese), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "turkish", sum(ManchesterUtd1819Data$turkish), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "asante", sum(ManchesterUtd1819Data$asante), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "persian", sum(ManchesterUtd1819Data$persian), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "maltese", sum(ManchesterUtd1819Data$maltese), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "romanian", sum(ManchesterUtd1819Data$romanian), ManchesterUtd1819Data$PropLanguages)
ManchesterUtd1819Data$PropLanguages <- ifelse(ManchesterUtd1819Data$Languages == "slovak", sum(ManchesterUtd1819Data$slovak), ManchesterUtd1819Data$PropLanguages)

Tottenham1819Data <- subset(PL1819SZN, Squad =="Tottenham")
Tottenham1819Data$english <- ifelse(Tottenham1819Data$Languages=="english", 1, 0)
Tottenham1819Data$french <- ifelse(Tottenham1819Data$Languages=="french", 1, 0)
Tottenham1819Data$dutch <- ifelse(Tottenham1819Data$Languages=="dutch", 1, 0)
Tottenham1819Data$portuguese <- ifelse(Tottenham1819Data$Languages=="portuguese", 1, 0)
Tottenham1819Data$castilian <- ifelse(Tottenham1819Data$Languages=="castilian", 1, 0)
Tottenham1819Data$spanish <- ifelse(Tottenham1819Data$Languages=="spanish", 1, 0)
Tottenham1819Data$german <- ifelse(Tottenham1819Data$Languages=="german", 1, 0)
Tottenham1819Data$arabic <- ifelse(Tottenham1819Data$Languages=="arabic", 1, 0)
Tottenham1819Data$serbian <- ifelse(Tottenham1819Data$Languages=="serbian", 1, 0)
Tottenham1819Data$korean <- ifelse(Tottenham1819Data$Languages=="korean", 1, 0)
Tottenham1819Data$croatian <- ifelse(Tottenham1819Data$Languages=="croatian", 1, 0)
Tottenham1819Data$italian <- ifelse(Tottenham1819Data$Languages=="italian", 1, 0)
Tottenham1819Data$armenian <- ifelse(Tottenham1819Data$Languages=="armenian", 1, 0)
Tottenham1819Data$icelandic <- ifelse(Tottenham1819Data$Languages=="icelandic", 1, 0)
Tottenham1819Data$hebrew <- ifelse(Tottenham1819Data$Languages=="hebrew", 1, 0)
Tottenham1819Data$bokmal <- ifelse(Tottenham1819Data$Languages=="bokmal", 1, 0)
Tottenham1819Data$ukrainian <- ifelse(Tottenham1819Data$Languages=="ukrainian", 1, 0)
Tottenham1819Data$russian <- ifelse(Tottenham1819Data$Languages=="russian", 1, 0)
Tottenham1819Data$bosnian <- ifelse(Tottenham1819Data$Languages=="bosnian", 1, 0)
Tottenham1819Data$czech <- ifelse(Tottenham1819Data$Languages=="czech", 1, 0)
Tottenham1819Data$danish <- ifelse(Tottenham1819Data$Languages=="danish", 1, 0)
Tottenham1819Data$swedish <- ifelse(Tottenham1819Data$Languages=="swedish", 1, 0)
Tottenham1819Data$japanese <- ifelse(Tottenham1819Data$Languages=="japanese", 1, 0)
Tottenham1819Data$turkish <- ifelse(Tottenham1819Data$Languages=="turkish", 1, 0)
Tottenham1819Data$asante <- ifelse(Tottenham1819Data$Languages=="asante", 1, 0)
Tottenham1819Data$persian <- ifelse(Tottenham1819Data$Languages=="persian", 1, 0)
Tottenham1819Data$maltese <- ifelse(Tottenham1819Data$Languages=="maltese", 1, 0)
Tottenham1819Data$romanian <- ifelse(Tottenham1819Data$Languages=="romanian", 1, 0)
Tottenham1819Data$slovak <- ifelse(Tottenham1819Data$Languages=="slovak", 1, 0)
Tottenham1819Data$PropLanguages <- 0
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "english", sum(Tottenham1819Data$english), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "french", sum(Tottenham1819Data$french), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "dutch", sum(Tottenham1819Data$dutch), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "portuguese", sum(Tottenham1819Data$portuguese), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "castilian", sum(Tottenham1819Data$castilian), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "spanish", sum(Tottenham1819Data$spanish), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "german", sum(Tottenham1819Data$german), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "arabic", sum(Tottenham1819Data$arabic), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "serbian", sum(Tottenham1819Data$serbian), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "korean", sum(Tottenham1819Data$korean), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "croatian", sum(Tottenham1819Data$croatian), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "italian", sum(Tottenham1819Data$italian), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "armenian", sum(Tottenham1819Data$armenian), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "icelandic", sum(Tottenham1819Data$icelandic), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "hebrew", sum(Tottenham1819Data$hebrew), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "bokmal", sum(Tottenham1819Data$bokmal), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "ukrainian", sum(Tottenham1819Data$ukrainian), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "russian", sum(Tottenham1819Data$russian), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "bosnian", sum(Tottenham1819Data$bosnian), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "czech", sum(Tottenham1819Data$czech), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "danish", sum(Tottenham1819Data$danish), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "swedish", sum(Tottenham1819Data$swedish), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "japanese", sum(Tottenham1819Data$japanese), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "turkish", sum(Tottenham1819Data$turkish), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "asante", sum(Tottenham1819Data$asante), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "persian", sum(Tottenham1819Data$persian), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "maltese", sum(Tottenham1819Data$maltese), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "romanian", sum(Tottenham1819Data$romanian), Tottenham1819Data$PropLanguages)
Tottenham1819Data$PropLanguages <- ifelse(Tottenham1819Data$Languages == "slovak", sum(Tottenham1819Data$slovak), Tottenham1819Data$PropLanguages)

PL1819SZN <- rbind(Arsenal1819Data, Chelsea1819Data)
PL1819SZN <- rbind(PL1819SZN, Everton1819Data)
PL1819SZN <- rbind(PL1819SZN, Liverpool1819Data)
PL1819SZN <- rbind(PL1819SZN, ManchesterCity1819Data)
PL1819SZN <- rbind(PL1819SZN, ManchesterUtd1819Data)
PL1819SZN <- rbind(PL1819SZN, Tottenham1819Data)

PL1718SZN$english <- 0
PL1718SZN$french <- 0
PL1718SZN$portuguese <- 0
PL1718SZN$castilian <- 0
PL1718SZN$dutch <- 0
PL1718SZN$german <- 0
PL1718SZN$spanish <- 0
PL1718SZN$arabic <- 0
PL1718SZN$serbian <- 0
PL1718SZN$korean <- 0
PL1718SZN$croatian <- 0
PL1718SZN$italian <- 0
PL1718SZN$icelandic <- 0
PL1718SZN$hebrew <- 0
PL1718SZN$bokmal <- 0
PL1718SZN$ukrainian <- 0
PL1718SZN$russian <- 0
PL1718SZN$bosnian <- 0
PL1718SZN$czech <- 0
PL1718SZN$danish <- 0
PL1718SZN$armenian <- 0
PL1718SZN$turkish <- 0
PL1718SZN$asante <- 0
PL1718SZN$persian <- 0
PL1718SZN$japanese <- 0
PL1718SZN$maltese <- 0
PL1718SZN$romanian <- 0
PL1718SZN$slovak <- 0
PL1718SZN$swedish <- 0

Arsenal1718Data <- subset(PL1718SZN, Squad =="Arsenal")
Arsenal1718Data$english <- ifelse(Arsenal1718Data$Languages=="english", 1, 0)
Arsenal1718Data$french <- ifelse(Arsenal1718Data$Languages=="french", 1, 0)
Arsenal1718Data$dutch <- ifelse(Arsenal1718Data$Languages=="dutch", 1, 0)
Arsenal1718Data$portuguese <- ifelse(Arsenal1718Data$Languages=="portuguese", 1, 0)
Arsenal1718Data$castilian <- ifelse(Arsenal1718Data$Languages=="castilian", 1, 0)
Arsenal1718Data$spanish <- ifelse(Arsenal1718Data$Languages=="spanish", 1, 0)
Arsenal1718Data$german <- ifelse(Arsenal1718Data$Languages=="german", 1, 0)
Arsenal1718Data$arabic <- ifelse(Arsenal1718Data$Languages=="arabic", 1, 0)
Arsenal1718Data$serbian <- ifelse(Arsenal1718Data$Languages=="serbian", 1, 0)
Arsenal1718Data$korean <- ifelse(Arsenal1718Data$Languages=="korean", 1, 0)
Arsenal1718Data$croatian <- ifelse(Arsenal1718Data$Languages=="croatian", 1, 0)
Arsenal1718Data$italian <- ifelse(Arsenal1718Data$Languages=="italian", 1, 0)
Arsenal1718Data$armenian <- ifelse(Arsenal1718Data$Languages=="armenian", 1, 0)
Arsenal1718Data$icelandic <- ifelse(Arsenal1718Data$Languages=="icelandic", 1, 0)
Arsenal1718Data$hebrew <- ifelse(Arsenal1718Data$Languages=="hebrew", 1, 0)
Arsenal1718Data$bokmal <- ifelse(Arsenal1718Data$Languages=="bokmal", 1, 0)
Arsenal1718Data$ukrainian <- ifelse(Arsenal1718Data$Languages=="ukrainian", 1, 0)
Arsenal1718Data$russian <- ifelse(Arsenal1718Data$Languages=="russian", 1, 0)
Arsenal1718Data$bosnian <- ifelse(Arsenal1718Data$Languages=="bosnian", 1, 0)
Arsenal1718Data$czech <- ifelse(Arsenal1718Data$Languages=="czech", 1, 0)
Arsenal1718Data$danish <- ifelse(Arsenal1718Data$Languages=="danish", 1, 0)
Arsenal1718Data$swedish <- ifelse(Arsenal1718Data$Languages=="swedish", 1, 0)
Arsenal1718Data$japanese <- ifelse(Arsenal1718Data$Languages=="japanese", 1, 0)
Arsenal1718Data$turkish <- ifelse(Arsenal1718Data$Languages=="turkish", 1, 0)
Arsenal1718Data$asante <- ifelse(Arsenal1718Data$Languages=="asante", 1, 0)
Arsenal1718Data$persian <- ifelse(Arsenal1718Data$Languages=="persian", 1, 0)
Arsenal1718Data$maltese <- ifelse(Arsenal1718Data$Languages=="maltese", 1, 0)
Arsenal1718Data$romanian <- ifelse(Arsenal1718Data$Languages=="romanian", 1, 0)
Arsenal1718Data$slovak <- ifelse(Arsenal1718Data$Languages=="slovak", 1, 0)
Arsenal1718Data$PropLanguages <- 0
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "english", sum(Arsenal1718Data$english), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "french", sum(Arsenal1718Data$french), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "dutch", sum(Arsenal1718Data$dutch), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "portuguese", sum(Arsenal1718Data$portuguese), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "castilian", sum(Arsenal1718Data$castilian), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "spanish", sum(Arsenal1718Data$spanish), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "german", sum(Arsenal1718Data$german), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "arabic", sum(Arsenal1718Data$arabic), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "serbian", sum(Arsenal1718Data$serbian), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "korean", sum(Arsenal1718Data$korean), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "croatian", sum(Arsenal1718Data$croatian), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "italian", sum(Arsenal1718Data$italian), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "armenian", sum(Arsenal1718Data$armenian), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "icelandic", sum(Arsenal1718Data$icelandic), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "hebrew", sum(Arsenal1718Data$hebrew), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "bokmal", sum(Arsenal1718Data$bokmal), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "ukrainian", sum(Arsenal1718Data$ukrainian), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "russian", sum(Arsenal1718Data$russian), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "bosnian", sum(Arsenal1718Data$bosnian), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "czech", sum(Arsenal1718Data$czech), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "danish", sum(Arsenal1718Data$danish), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "swedish", sum(Arsenal1718Data$swedish), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "japanese", sum(Arsenal1718Data$japanese), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "turkish", sum(Arsenal1718Data$turkish), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "asante", sum(Arsenal1718Data$asante), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "persian", sum(Arsenal1718Data$persian), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "maltese", sum(Arsenal1718Data$maltese), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "romanian", sum(Arsenal1718Data$romanian), Arsenal1718Data$PropLanguages)
Arsenal1718Data$PropLanguages <- ifelse(Arsenal1718Data$Languages == "slovak", sum(Arsenal1718Data$slovak), Arsenal1718Data$PropLanguages)

Chelsea1718Data <- subset(PL1718SZN, Squad =="Chelsea")
Chelsea1718Data$english <- ifelse(Chelsea1718Data$Languages=="english", 1, 0)
Chelsea1718Data$french <- ifelse(Chelsea1718Data$Languages=="french", 1, 0)
Chelsea1718Data$dutch <- ifelse(Chelsea1718Data$Languages=="dutch", 1, 0)
Chelsea1718Data$portuguese <- ifelse(Chelsea1718Data$Languages=="portuguese", 1, 0)
Chelsea1718Data$castilian <- ifelse(Chelsea1718Data$Languages=="castilian", 1, 0)
Chelsea1718Data$spanish <- ifelse(Chelsea1718Data$Languages=="spanish", 1, 0)
Chelsea1718Data$german <- ifelse(Chelsea1718Data$Languages=="german", 1, 0)
Chelsea1718Data$arabic <- ifelse(Chelsea1718Data$Languages=="arabic", 1, 0)
Chelsea1718Data$serbian <- ifelse(Chelsea1718Data$Languages=="serbian", 1, 0)
Chelsea1718Data$korean <- ifelse(Chelsea1718Data$Languages=="korean", 1, 0)
Chelsea1718Data$croatian <- ifelse(Chelsea1718Data$Languages=="croatian", 1, 0)
Chelsea1718Data$italian <- ifelse(Chelsea1718Data$Languages=="italian", 1, 0)
Chelsea1718Data$armenian <- ifelse(Chelsea1718Data$Languages=="armenian", 1, 0)
Chelsea1718Data$icelandic <- ifelse(Chelsea1718Data$Languages=="icelandic", 1, 0)
Chelsea1718Data$hebrew <- ifelse(Chelsea1718Data$Languages=="hebrew", 1, 0)
Chelsea1718Data$bokmal <- ifelse(Chelsea1718Data$Languages=="bokmal", 1, 0)
Chelsea1718Data$ukrainian <- ifelse(Chelsea1718Data$Languages=="ukrainian", 1, 0)
Chelsea1718Data$russian <- ifelse(Chelsea1718Data$Languages=="russian", 1, 0)
Chelsea1718Data$bosnian <- ifelse(Chelsea1718Data$Languages=="bosnian", 1, 0)
Chelsea1718Data$czech <- ifelse(Chelsea1718Data$Languages=="czech", 1, 0)
Chelsea1718Data$danish <- ifelse(Chelsea1718Data$Languages=="danish", 1, 0)
Chelsea1718Data$swedish <- ifelse(Chelsea1718Data$Languages=="swedish", 1, 0)
Chelsea1718Data$japanese <- ifelse(Chelsea1718Data$Languages=="japanese", 1, 0)
Chelsea1718Data$turkish <- ifelse(Chelsea1718Data$Languages=="turkish", 1, 0)
Chelsea1718Data$asante <- ifelse(Chelsea1718Data$Languages=="asante", 1, 0)
Chelsea1718Data$persian <- ifelse(Chelsea1718Data$Languages=="persian", 1, 0)
Chelsea1718Data$maltese <- ifelse(Chelsea1718Data$Languages=="maltese", 1, 0)
Chelsea1718Data$romanian <- ifelse(Chelsea1718Data$Languages=="romanian", 1, 0)
Chelsea1718Data$slovak <- ifelse(Chelsea1718Data$Languages=="slovak", 1, 0)
Chelsea1718Data$PropLanguages <- 0
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "english", sum(Chelsea1718Data$english), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "french", sum(Chelsea1718Data$french), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "dutch", sum(Chelsea1718Data$dutch), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "portuguese", sum(Chelsea1718Data$portuguese), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "castilian", sum(Chelsea1718Data$castilian), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "spanish", sum(Chelsea1718Data$spanish), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "german", sum(Chelsea1718Data$german), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "arabic", sum(Chelsea1718Data$arabic), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "serbian", sum(Chelsea1718Data$serbian), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "korean", sum(Chelsea1718Data$korean), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "croatian", sum(Chelsea1718Data$croatian), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "italian", sum(Chelsea1718Data$italian), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "armenian", sum(Chelsea1718Data$armenian), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "icelandic", sum(Chelsea1718Data$icelandic), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "hebrew", sum(Chelsea1718Data$hebrew), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "bokmal", sum(Chelsea1718Data$bokmal), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "ukrainian", sum(Chelsea1718Data$ukrainian), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "russian", sum(Chelsea1718Data$russian), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "bosnian", sum(Chelsea1718Data$bosnian), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "czech", sum(Chelsea1718Data$czech), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "danish", sum(Chelsea1718Data$danish), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "swedish", sum(Chelsea1718Data$swedish), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "japanese", sum(Chelsea1718Data$japanese), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "turkish", sum(Chelsea1718Data$turkish), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "asante", sum(Chelsea1718Data$asante), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "persian", sum(Chelsea1718Data$persian), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "maltese", sum(Chelsea1718Data$maltese), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "romanian", sum(Chelsea1718Data$romanian), Chelsea1718Data$PropLanguages)
Chelsea1718Data$PropLanguages <- ifelse(Chelsea1718Data$Languages == "slovak", sum(Chelsea1718Data$slovak), Chelsea1718Data$PropLanguages)

Everton1718Data <- subset(PL1718SZN, Squad =="Everton")
Everton1718Data$english <- ifelse(Everton1718Data$Languages=="english", 1, 0)
Everton1718Data$french <- ifelse(Everton1718Data$Languages=="french", 1, 0)
Everton1718Data$dutch <- ifelse(Everton1718Data$Languages=="dutch", 1, 0)
Everton1718Data$portuguese <- ifelse(Everton1718Data$Languages=="portuguese", 1, 0)
Everton1718Data$castilian <- ifelse(Everton1718Data$Languages=="castilian", 1, 0)
Everton1718Data$spanish <- ifelse(Everton1718Data$Languages=="spanish", 1, 0)
Everton1718Data$german <- ifelse(Everton1718Data$Languages=="german", 1, 0)
Everton1718Data$arabic <- ifelse(Everton1718Data$Languages=="arabic", 1, 0)
Everton1718Data$serbian <- ifelse(Everton1718Data$Languages=="serbian", 1, 0)
Everton1718Data$korean <- ifelse(Everton1718Data$Languages=="korean", 1, 0)
Everton1718Data$croatian <- ifelse(Everton1718Data$Languages=="croatian", 1, 0)
Everton1718Data$italian <- ifelse(Everton1718Data$Languages=="italian", 1, 0)
Everton1718Data$armenian <- ifelse(Everton1718Data$Languages=="armenian", 1, 0)
Everton1718Data$icelandic <- ifelse(Everton1718Data$Languages=="icelandic", 1, 0)
Everton1718Data$hebrew <- ifelse(Everton1718Data$Languages=="hebrew", 1, 0)
Everton1718Data$bokmal <- ifelse(Everton1718Data$Languages=="bokmal", 1, 0)
Everton1718Data$ukrainian <- ifelse(Everton1718Data$Languages=="ukrainian", 1, 0)
Everton1718Data$russian <- ifelse(Everton1718Data$Languages=="russian", 1, 0)
Everton1718Data$bosnian <- ifelse(Everton1718Data$Languages=="bosnian", 1, 0)
Everton1718Data$czech <- ifelse(Everton1718Data$Languages=="czech", 1, 0)
Everton1718Data$danish <- ifelse(Everton1718Data$Languages=="danish", 1, 0)
Everton1718Data$swedish <- ifelse(Everton1718Data$Languages=="swedish", 1, 0)
Everton1718Data$japanese <- ifelse(Everton1718Data$Languages=="japanese", 1, 0)
Everton1718Data$turkish <- ifelse(Everton1718Data$Languages=="turkish", 1, 0)
Everton1718Data$asante <- ifelse(Everton1718Data$Languages=="asante", 1, 0)
Everton1718Data$persian <- ifelse(Everton1718Data$Languages=="persian", 1, 0)
Everton1718Data$maltese <- ifelse(Everton1718Data$Languages=="maltese", 1, 0)
Everton1718Data$romanian <- ifelse(Everton1718Data$Languages=="romanian", 1, 0)
Everton1718Data$slovak <- ifelse(Everton1718Data$Languages=="slovak", 1, 0)
Everton1718Data$PropLanguages <- 0
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "english", sum(Everton1718Data$english), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "french", sum(Everton1718Data$french), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "dutch", sum(Everton1718Data$dutch), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "portuguese", sum(Everton1718Data$portuguese), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "castilian", sum(Everton1718Data$castilian), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "spanish", sum(Everton1718Data$spanish), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "german", sum(Everton1718Data$german), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "arabic", sum(Everton1718Data$arabic), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "serbian", sum(Everton1718Data$serbian), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "korean", sum(Everton1718Data$korean), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "croatian", sum(Everton1718Data$croatian), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "italian", sum(Everton1718Data$italian), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "armenian", sum(Everton1718Data$armenian), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "icelandic", sum(Everton1718Data$icelandic), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "hebrew", sum(Everton1718Data$hebrew), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "bokmal", sum(Everton1718Data$bokmal), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "ukrainian", sum(Everton1718Data$ukrainian), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "russian", sum(Everton1718Data$russian), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "bosnian", sum(Everton1718Data$bosnian), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "czech", sum(Everton1718Data$czech), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "danish", sum(Everton1718Data$danish), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "swedish", sum(Everton1718Data$swedish), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "japanese", sum(Everton1718Data$japanese), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "turkish", sum(Everton1718Data$turkish), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "asante", sum(Everton1718Data$asante), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "persian", sum(Everton1718Data$persian), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "maltese", sum(Everton1718Data$maltese), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "romanian", sum(Everton1718Data$romanian), Everton1718Data$PropLanguages)
Everton1718Data$PropLanguages <- ifelse(Everton1718Data$Languages == "slovak", sum(Everton1718Data$slovak), Everton1718Data$PropLanguages)

Liverpool1718Data <- subset(PL1718SZN, Squad =="Liverpool")
Liverpool1718Data$english <- ifelse(Liverpool1718Data$Languages=="english", 1, 0)
Liverpool1718Data$french <- ifelse(Liverpool1718Data$Languages=="french", 1, 0)
Liverpool1718Data$dutch <- ifelse(Liverpool1718Data$Languages=="dutch", 1, 0)
Liverpool1718Data$portuguese <- ifelse(Liverpool1718Data$Languages=="portuguese", 1, 0)
Liverpool1718Data$castilian <- ifelse(Liverpool1718Data$Languages=="castilian", 1, 0)
Liverpool1718Data$spanish <- ifelse(Liverpool1718Data$Languages=="spanish", 1, 0)
Liverpool1718Data$german <- ifelse(Liverpool1718Data$Languages=="german", 1, 0)
Liverpool1718Data$arabic <- ifelse(Liverpool1718Data$Languages=="arabic", 1, 0)
Liverpool1718Data$serbian <- ifelse(Liverpool1718Data$Languages=="serbian", 1, 0)
Liverpool1718Data$korean <- ifelse(Liverpool1718Data$Languages=="korean", 1, 0)
Liverpool1718Data$croatian <- ifelse(Liverpool1718Data$Languages=="croatian", 1, 0)
Liverpool1718Data$italian <- ifelse(Liverpool1718Data$Languages=="italian", 1, 0)
Liverpool1718Data$armenian <- ifelse(Liverpool1718Data$Languages=="armenian", 1, 0)
Liverpool1718Data$icelandic <- ifelse(Liverpool1718Data$Languages=="icelandic", 1, 0)
Liverpool1718Data$hebrew <- ifelse(Liverpool1718Data$Languages=="hebrew", 1, 0)
Liverpool1718Data$bokmal <- ifelse(Liverpool1718Data$Languages=="bokmal", 1, 0)
Liverpool1718Data$ukrainian <- ifelse(Liverpool1718Data$Languages=="ukrainian", 1, 0)
Liverpool1718Data$russian <- ifelse(Liverpool1718Data$Languages=="russian", 1, 0)
Liverpool1718Data$bosnian <- ifelse(Liverpool1718Data$Languages=="bosnian", 1, 0)
Liverpool1718Data$czech <- ifelse(Liverpool1718Data$Languages=="czech", 1, 0)
Liverpool1718Data$danish <- ifelse(Liverpool1718Data$Languages=="danish", 1, 0)
Liverpool1718Data$swedish <- ifelse(Liverpool1718Data$Languages=="swedish", 1, 0)
Liverpool1718Data$japanese <- ifelse(Liverpool1718Data$Languages=="japanese", 1, 0)
Liverpool1718Data$turkish <- ifelse(Liverpool1718Data$Languages=="turkish", 1, 0)
Liverpool1718Data$asante <- ifelse(Liverpool1718Data$Languages=="asante", 1, 0)
Liverpool1718Data$persian <- ifelse(Liverpool1718Data$Languages=="persian", 1, 0)
Liverpool1718Data$maltese <- ifelse(Liverpool1718Data$Languages=="maltese", 1, 0)
Liverpool1718Data$romanian <- ifelse(Liverpool1718Data$Languages=="romanian", 1, 0)
Liverpool1718Data$slovak <- ifelse(Liverpool1718Data$Languages=="slovak", 1, 0)
Liverpool1718Data$PropLanguages <- 0
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "english", sum(Liverpool1718Data$english), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "french", sum(Liverpool1718Data$french), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "dutch", sum(Liverpool1718Data$dutch), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "portuguese", sum(Liverpool1718Data$portuguese), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "castilian", sum(Liverpool1718Data$castilian), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "spanish", sum(Liverpool1718Data$spanish), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "german", sum(Liverpool1718Data$german), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "arabic", sum(Liverpool1718Data$arabic), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "serbian", sum(Liverpool1718Data$serbian), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "korean", sum(Liverpool1718Data$korean), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "croatian", sum(Liverpool1718Data$croatian), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "italian", sum(Liverpool1718Data$italian), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "armenian", sum(Liverpool1718Data$armenian), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "icelandic", sum(Liverpool1718Data$icelandic), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "hebrew", sum(Liverpool1718Data$hebrew), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "bokmal", sum(Liverpool1718Data$bokmal), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "ukrainian", sum(Liverpool1718Data$ukrainian), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "russian", sum(Liverpool1718Data$russian), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "bosnian", sum(Liverpool1718Data$bosnian), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "czech", sum(Liverpool1718Data$czech), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "danish", sum(Liverpool1718Data$danish), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "swedish", sum(Liverpool1718Data$swedish), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "japanese", sum(Liverpool1718Data$japanese), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "turkish", sum(Liverpool1718Data$turkish), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "asante", sum(Liverpool1718Data$asante), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "persian", sum(Liverpool1718Data$persian), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "maltese", sum(Liverpool1718Data$maltese), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "romanian", sum(Liverpool1718Data$romanian), Liverpool1718Data$PropLanguages)
Liverpool1718Data$PropLanguages <- ifelse(Liverpool1718Data$Languages == "slovak", sum(Liverpool1718Data$slovak), Liverpool1718Data$PropLanguages)

ManchesterCity1718Data <- subset(PL1718SZN, Squad =="Manchester City")
ManchesterCity1718Data$english <- ifelse(ManchesterCity1718Data$Languages=="english", 1, 0)
ManchesterCity1718Data$french <- ifelse(ManchesterCity1718Data$Languages=="french", 1, 0)
ManchesterCity1718Data$dutch <- ifelse(ManchesterCity1718Data$Languages=="dutch", 1, 0)
ManchesterCity1718Data$portuguese <- ifelse(ManchesterCity1718Data$Languages=="portuguese", 1, 0)
ManchesterCity1718Data$castilian <- ifelse(ManchesterCity1718Data$Languages=="castilian", 1, 0)
ManchesterCity1718Data$spanish <- ifelse(ManchesterCity1718Data$Languages=="spanish", 1, 0)
ManchesterCity1718Data$german <- ifelse(ManchesterCity1718Data$Languages=="german", 1, 0)
ManchesterCity1718Data$arabic <- ifelse(ManchesterCity1718Data$Languages=="arabic", 1, 0)
ManchesterCity1718Data$serbian <- ifelse(ManchesterCity1718Data$Languages=="serbian", 1, 0)
ManchesterCity1718Data$korean <- ifelse(ManchesterCity1718Data$Languages=="korean", 1, 0)
ManchesterCity1718Data$croatian <- ifelse(ManchesterCity1718Data$Languages=="croatian", 1, 0)
ManchesterCity1718Data$italian <- ifelse(ManchesterCity1718Data$Languages=="italian", 1, 0)
ManchesterCity1718Data$armenian <- ifelse(ManchesterCity1718Data$Languages=="armenian", 1, 0)
ManchesterCity1718Data$icelandic <- ifelse(ManchesterCity1718Data$Languages=="icelandic", 1, 0)
ManchesterCity1718Data$hebrew <- ifelse(ManchesterCity1718Data$Languages=="hebrew", 1, 0)
ManchesterCity1718Data$bokmal <- ifelse(ManchesterCity1718Data$Languages=="bokmal", 1, 0)
ManchesterCity1718Data$ukrainian <- ifelse(ManchesterCity1718Data$Languages=="ukrainian", 1, 0)
ManchesterCity1718Data$russian <- ifelse(ManchesterCity1718Data$Languages=="russian", 1, 0)
ManchesterCity1718Data$bosnian <- ifelse(ManchesterCity1718Data$Languages=="bosnian", 1, 0)
ManchesterCity1718Data$czech <- ifelse(ManchesterCity1718Data$Languages=="czech", 1, 0)
ManchesterCity1718Data$danish <- ifelse(ManchesterCity1718Data$Languages=="danish", 1, 0)
ManchesterCity1718Data$swedish <- ifelse(ManchesterCity1718Data$Languages=="swedish", 1, 0)
ManchesterCity1718Data$japanese <- ifelse(ManchesterCity1718Data$Languages=="japanese", 1, 0)
ManchesterCity1718Data$turkish <- ifelse(ManchesterCity1718Data$Languages=="turkish", 1, 0)
ManchesterCity1718Data$asante <- ifelse(ManchesterCity1718Data$Languages=="asante", 1, 0)
ManchesterCity1718Data$persian <- ifelse(ManchesterCity1718Data$Languages=="persian", 1, 0)
ManchesterCity1718Data$maltese <- ifelse(ManchesterCity1718Data$Languages=="maltese", 1, 0)
ManchesterCity1718Data$romanian <- ifelse(ManchesterCity1718Data$Languages=="romanian", 1, 0)
ManchesterCity1718Data$slovak <- ifelse(ManchesterCity1718Data$Languages=="slovak", 1, 0)
ManchesterCity1718Data$PropLanguages <- 0
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "english", sum(ManchesterCity1718Data$english), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "french", sum(ManchesterCity1718Data$french), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "dutch", sum(ManchesterCity1718Data$dutch), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "portuguese", sum(ManchesterCity1718Data$portuguese), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "castilian", sum(ManchesterCity1718Data$castilian), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "spanish", sum(ManchesterCity1718Data$spanish), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "german", sum(ManchesterCity1718Data$german), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "arabic", sum(ManchesterCity1718Data$arabic), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "serbian", sum(ManchesterCity1718Data$serbian), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "korean", sum(ManchesterCity1718Data$korean), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "croatian", sum(ManchesterCity1718Data$croatian), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "italian", sum(ManchesterCity1718Data$italian), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "armenian", sum(ManchesterCity1718Data$armenian), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "icelandic", sum(ManchesterCity1718Data$icelandic), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "hebrew", sum(ManchesterCity1718Data$hebrew), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "bokmal", sum(ManchesterCity1718Data$bokmal), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "ukrainian", sum(ManchesterCity1718Data$ukrainian), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "russian", sum(ManchesterCity1718Data$russian), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "bosnian", sum(ManchesterCity1718Data$bosnian), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "czech", sum(ManchesterCity1718Data$czech), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "danish", sum(ManchesterCity1718Data$danish), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "swedish", sum(ManchesterCity1718Data$swedish), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "japanese", sum(ManchesterCity1718Data$japanese), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "turkish", sum(ManchesterCity1718Data$turkish), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "asante", sum(ManchesterCity1718Data$asante), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "persian", sum(ManchesterCity1718Data$persian), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "maltese", sum(ManchesterCity1718Data$maltese), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "romanian", sum(ManchesterCity1718Data$romanian), ManchesterCity1718Data$PropLanguages)
ManchesterCity1718Data$PropLanguages <- ifelse(ManchesterCity1718Data$Languages == "slovak", sum(ManchesterCity1718Data$slovak), ManchesterCity1718Data$PropLanguages)

ManchesterUtd1718Data <- subset(PL1718SZN, Squad =="Manchester Utd")
ManchesterUtd1718Data$english <- ifelse(ManchesterUtd1718Data$Languages=="english", 1, 0)
ManchesterUtd1718Data$french <- ifelse(ManchesterUtd1718Data$Languages=="french", 1, 0)
ManchesterUtd1718Data$dutch <- ifelse(ManchesterUtd1718Data$Languages=="dutch", 1, 0)
ManchesterUtd1718Data$portuguese <- ifelse(ManchesterUtd1718Data$Languages=="portuguese", 1, 0)
ManchesterUtd1718Data$castilian <- ifelse(ManchesterUtd1718Data$Languages=="castilian", 1, 0)
ManchesterUtd1718Data$spanish <- ifelse(ManchesterUtd1718Data$Languages=="spanish", 1, 0)
ManchesterUtd1718Data$german <- ifelse(ManchesterUtd1718Data$Languages=="german", 1, 0)
ManchesterUtd1718Data$arabic <- ifelse(ManchesterUtd1718Data$Languages=="arabic", 1, 0)
ManchesterUtd1718Data$serbian <- ifelse(ManchesterUtd1718Data$Languages=="serbian", 1, 0)
ManchesterUtd1718Data$korean <- ifelse(ManchesterUtd1718Data$Languages=="korean", 1, 0)
ManchesterUtd1718Data$croatian <- ifelse(ManchesterUtd1718Data$Languages=="croatian", 1, 0)
ManchesterUtd1718Data$italian <- ifelse(ManchesterUtd1718Data$Languages=="italian", 1, 0)
ManchesterUtd1718Data$armenian <- ifelse(ManchesterUtd1718Data$Languages=="armenian", 1, 0)
ManchesterUtd1718Data$icelandic <- ifelse(ManchesterUtd1718Data$Languages=="icelandic", 1, 0)
ManchesterUtd1718Data$hebrew <- ifelse(ManchesterUtd1718Data$Languages=="hebrew", 1, 0)
ManchesterUtd1718Data$bokmal <- ifelse(ManchesterUtd1718Data$Languages=="bokmal", 1, 0)
ManchesterUtd1718Data$ukrainian <- ifelse(ManchesterUtd1718Data$Languages=="ukrainian", 1, 0)
ManchesterUtd1718Data$russian <- ifelse(ManchesterUtd1718Data$Languages=="russian", 1, 0)
ManchesterUtd1718Data$bosnian <- ifelse(ManchesterUtd1718Data$Languages=="bosnian", 1, 0)
ManchesterUtd1718Data$czech <- ifelse(ManchesterUtd1718Data$Languages=="czech", 1, 0)
ManchesterUtd1718Data$danish <- ifelse(ManchesterUtd1718Data$Languages=="danish", 1, 0)
ManchesterUtd1718Data$swedish <- ifelse(ManchesterUtd1718Data$Languages=="swedish", 1, 0)
ManchesterUtd1718Data$japanese <- ifelse(ManchesterUtd1718Data$Languages=="japanese", 1, 0)
ManchesterUtd1718Data$turkish <- ifelse(ManchesterUtd1718Data$Languages=="turkish", 1, 0)
ManchesterUtd1718Data$asante <- ifelse(ManchesterUtd1718Data$Languages=="asante", 1, 0)
ManchesterUtd1718Data$persian <- ifelse(ManchesterUtd1718Data$Languages=="persian", 1, 0)
ManchesterUtd1718Data$maltese <- ifelse(ManchesterUtd1718Data$Languages=="maltese", 1, 0)
ManchesterUtd1718Data$romanian <- ifelse(ManchesterUtd1718Data$Languages=="romanian", 1, 0)
ManchesterUtd1718Data$slovak <- ifelse(ManchesterUtd1718Data$Languages=="slovak", 1, 0)
ManchesterUtd1718Data$PropLanguages <- 0
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "english", sum(ManchesterUtd1718Data$english), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "french", sum(ManchesterUtd1718Data$french), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "dutch", sum(ManchesterUtd1718Data$dutch), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "portuguese", sum(ManchesterUtd1718Data$portuguese), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "castilian", sum(ManchesterUtd1718Data$castilian), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "spanish", sum(ManchesterUtd1718Data$spanish), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "german", sum(ManchesterUtd1718Data$german), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "arabic", sum(ManchesterUtd1718Data$arabic), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "serbian", sum(ManchesterUtd1718Data$serbian), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "korean", sum(ManchesterUtd1718Data$korean), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "croatian", sum(ManchesterUtd1718Data$croatian), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "italian", sum(ManchesterUtd1718Data$italian), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "armenian", sum(ManchesterUtd1718Data$armenian), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "icelandic", sum(ManchesterUtd1718Data$icelandic), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "hebrew", sum(ManchesterUtd1718Data$hebrew), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "bokmal", sum(ManchesterUtd1718Data$bokmal), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "ukrainian", sum(ManchesterUtd1718Data$ukrainian), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "russian", sum(ManchesterUtd1718Data$russian), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "bosnian", sum(ManchesterUtd1718Data$bosnian), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "czech", sum(ManchesterUtd1718Data$czech), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "danish", sum(ManchesterUtd1718Data$danish), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "swedish", sum(ManchesterUtd1718Data$swedish), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "japanese", sum(ManchesterUtd1718Data$japanese), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "turkish", sum(ManchesterUtd1718Data$turkish), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "asante", sum(ManchesterUtd1718Data$asante), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "persian", sum(ManchesterUtd1718Data$persian), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "maltese", sum(ManchesterUtd1718Data$maltese), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "romanian", sum(ManchesterUtd1718Data$romanian), ManchesterUtd1718Data$PropLanguages)
ManchesterUtd1718Data$PropLanguages <- ifelse(ManchesterUtd1718Data$Languages == "slovak", sum(ManchesterUtd1718Data$slovak), ManchesterUtd1718Data$PropLanguages)

Tottenham1718Data <- subset(PL1718SZN, Squad =="Tottenham")
Tottenham1718Data$english <- ifelse(Tottenham1718Data$Languages=="english", 1, 0)
Tottenham1718Data$french <- ifelse(Tottenham1718Data$Languages=="french", 1, 0)
Tottenham1718Data$dutch <- ifelse(Tottenham1718Data$Languages=="dutch", 1, 0)
Tottenham1718Data$portuguese <- ifelse(Tottenham1718Data$Languages=="portuguese", 1, 0)
Tottenham1718Data$castilian <- ifelse(Tottenham1718Data$Languages=="castilian", 1, 0)
Tottenham1718Data$spanish <- ifelse(Tottenham1718Data$Languages=="spanish", 1, 0)
Tottenham1718Data$german <- ifelse(Tottenham1718Data$Languages=="german", 1, 0)
Tottenham1718Data$arabic <- ifelse(Tottenham1718Data$Languages=="arabic", 1, 0)
Tottenham1718Data$serbian <- ifelse(Tottenham1718Data$Languages=="serbian", 1, 0)
Tottenham1718Data$korean <- ifelse(Tottenham1718Data$Languages=="korean", 1, 0)
Tottenham1718Data$croatian <- ifelse(Tottenham1718Data$Languages=="croatian", 1, 0)
Tottenham1718Data$italian <- ifelse(Tottenham1718Data$Languages=="italian", 1, 0)
Tottenham1718Data$armenian <- ifelse(Tottenham1718Data$Languages=="armenian", 1, 0)
Tottenham1718Data$icelandic <- ifelse(Tottenham1718Data$Languages=="icelandic", 1, 0)
Tottenham1718Data$hebrew <- ifelse(Tottenham1718Data$Languages=="hebrew", 1, 0)
Tottenham1718Data$bokmal <- ifelse(Tottenham1718Data$Languages=="bokmal", 1, 0)
Tottenham1718Data$ukrainian <- ifelse(Tottenham1718Data$Languages=="ukrainian", 1, 0)
Tottenham1718Data$russian <- ifelse(Tottenham1718Data$Languages=="russian", 1, 0)
Tottenham1718Data$bosnian <- ifelse(Tottenham1718Data$Languages=="bosnian", 1, 0)
Tottenham1718Data$czech <- ifelse(Tottenham1718Data$Languages=="czech", 1, 0)
Tottenham1718Data$danish <- ifelse(Tottenham1718Data$Languages=="danish", 1, 0)
Tottenham1718Data$swedish <- ifelse(Tottenham1718Data$Languages=="swedish", 1, 0)
Tottenham1718Data$japanese <- ifelse(Tottenham1718Data$Languages=="japanese", 1, 0)
Tottenham1718Data$turkish <- ifelse(Tottenham1718Data$Languages=="turkish", 1, 0)
Tottenham1718Data$asante <- ifelse(Tottenham1718Data$Languages=="asante", 1, 0)
Tottenham1718Data$persian <- ifelse(Tottenham1718Data$Languages=="persian", 1, 0)
Tottenham1718Data$maltese <- ifelse(Tottenham1718Data$Languages=="maltese", 1, 0)
Tottenham1718Data$romanian <- ifelse(Tottenham1718Data$Languages=="romanian", 1, 0)
Tottenham1718Data$slovak <- ifelse(Tottenham1718Data$Languages=="slovak", 1, 0)
Tottenham1718Data$PropLanguages <- 0
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "english", sum(Tottenham1718Data$english), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "french", sum(Tottenham1718Data$french), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "dutch", sum(Tottenham1718Data$dutch), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "portuguese", sum(Tottenham1718Data$portuguese), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "castilian", sum(Tottenham1718Data$castilian), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "spanish", sum(Tottenham1718Data$spanish), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "german", sum(Tottenham1718Data$german), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "arabic", sum(Tottenham1718Data$arabic), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "serbian", sum(Tottenham1718Data$serbian), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "korean", sum(Tottenham1718Data$korean), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "croatian", sum(Tottenham1718Data$croatian), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "italian", sum(Tottenham1718Data$italian), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "armenian", sum(Tottenham1718Data$armenian), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "icelandic", sum(Tottenham1718Data$icelandic), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "hebrew", sum(Tottenham1718Data$hebrew), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "bokmal", sum(Tottenham1718Data$bokmal), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "ukrainian", sum(Tottenham1718Data$ukrainian), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "russian", sum(Tottenham1718Data$russian), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "bosnian", sum(Tottenham1718Data$bosnian), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "czech", sum(Tottenham1718Data$czech), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "danish", sum(Tottenham1718Data$danish), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "swedish", sum(Tottenham1718Data$swedish), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "japanese", sum(Tottenham1718Data$japanese), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "turkish", sum(Tottenham1718Data$turkish), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "asante", sum(Tottenham1718Data$asante), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "persian", sum(Tottenham1718Data$persian), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "maltese", sum(Tottenham1718Data$maltese), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "romanian", sum(Tottenham1718Data$romanian), Tottenham1718Data$PropLanguages)
Tottenham1718Data$PropLanguages <- ifelse(Tottenham1718Data$Languages == "slovak", sum(Tottenham1718Data$slovak), Tottenham1718Data$PropLanguages)

PL1718SZN <- rbind(Arsenal1718Data, Chelsea1718Data)
PL1718SZN <- rbind(PL1718SZN, Everton1718Data)
PL1718SZN <- rbind(PL1718SZN, Liverpool1718Data)
PL1718SZN <- rbind(PL1718SZN, ManchesterCity1718Data)
PL1718SZN <- rbind(PL1718SZN, ManchesterUtd1718Data)
PL1718SZN <- rbind(PL1718SZN, Tottenham1718Data)

PL1617SZN$english <- 0
PL1617SZN$french <- 0
PL1617SZN$portuguese <- 0
PL1617SZN$castilian <- 0
PL1617SZN$dutch <- 0
PL1617SZN$german <- 0
PL1617SZN$spanish <- 0
PL1617SZN$arabic <- 0
PL1617SZN$serbian <- 0
PL1617SZN$korean <- 0
PL1617SZN$croatian <- 0
PL1617SZN$italian <- 0
PL1617SZN$icelandic <- 0
PL1617SZN$hebrew <- 0
PL1617SZN$bokmal <- 0
PL1617SZN$ukrainian <- 0
PL1617SZN$russian <- 0
PL1617SZN$bosnian <- 0
PL1617SZN$czech <- 0
PL1617SZN$danish <- 0
PL1617SZN$armenian <- 0
PL1617SZN$turkish <- 0
PL1617SZN$asante <- 0
PL1617SZN$persian <- 0
PL1617SZN$japanese <- 0
PL1617SZN$maltese <- 0
PL1617SZN$romanian <- 0
PL1617SZN$slovak <- 0
PL1617SZN$swedish <- 0

Arsenal1617Data <- subset(PL1617SZN, Squad =="Arsenal")
Arsenal1617Data$english <- ifelse(Arsenal1617Data$Languages=="english", 1, 0)
Arsenal1617Data$french <- ifelse(Arsenal1617Data$Languages=="french", 1, 0)
Arsenal1617Data$dutch <- ifelse(Arsenal1617Data$Languages=="dutch", 1, 0)
Arsenal1617Data$portuguese <- ifelse(Arsenal1617Data$Languages=="portuguese", 1, 0)
Arsenal1617Data$castilian <- ifelse(Arsenal1617Data$Languages=="castilian", 1, 0)
Arsenal1617Data$spanish <- ifelse(Arsenal1617Data$Languages=="spanish", 1, 0)
Arsenal1617Data$german <- ifelse(Arsenal1617Data$Languages=="german", 1, 0)
Arsenal1617Data$arabic <- ifelse(Arsenal1617Data$Languages=="arabic", 1, 0)
Arsenal1617Data$serbian <- ifelse(Arsenal1617Data$Languages=="serbian", 1, 0)
Arsenal1617Data$korean <- ifelse(Arsenal1617Data$Languages=="korean", 1, 0)
Arsenal1617Data$croatian <- ifelse(Arsenal1617Data$Languages=="croatian", 1, 0)
Arsenal1617Data$italian <- ifelse(Arsenal1617Data$Languages=="italian", 1, 0)
Arsenal1617Data$armenian <- ifelse(Arsenal1617Data$Languages=="armenian", 1, 0)
Arsenal1617Data$icelandic <- ifelse(Arsenal1617Data$Languages=="icelandic", 1, 0)
Arsenal1617Data$hebrew <- ifelse(Arsenal1617Data$Languages=="hebrew", 1, 0)
Arsenal1617Data$bokmal <- ifelse(Arsenal1617Data$Languages=="bokmal", 1, 0)
Arsenal1617Data$ukrainian <- ifelse(Arsenal1617Data$Languages=="ukrainian", 1, 0)
Arsenal1617Data$russian <- ifelse(Arsenal1617Data$Languages=="russian", 1, 0)
Arsenal1617Data$bosnian <- ifelse(Arsenal1617Data$Languages=="bosnian", 1, 0)
Arsenal1617Data$czech <- ifelse(Arsenal1617Data$Languages=="czech", 1, 0)
Arsenal1617Data$danish <- ifelse(Arsenal1617Data$Languages=="danish", 1, 0)
Arsenal1617Data$swedish <- ifelse(Arsenal1617Data$Languages=="swedish", 1, 0)
Arsenal1617Data$japanese <- ifelse(Arsenal1617Data$Languages=="japanese", 1, 0)
Arsenal1617Data$turkish <- ifelse(Arsenal1617Data$Languages=="turkish", 1, 0)
Arsenal1617Data$asante <- ifelse(Arsenal1617Data$Languages=="asante", 1, 0)
Arsenal1617Data$persian <- ifelse(Arsenal1617Data$Languages=="persian", 1, 0)
Arsenal1617Data$maltese <- ifelse(Arsenal1617Data$Languages=="maltese", 1, 0)
Arsenal1617Data$romanian <- ifelse(Arsenal1617Data$Languages=="romanian", 1, 0)
Arsenal1617Data$slovak <- ifelse(Arsenal1617Data$Languages=="slovak", 1, 0)
Arsenal1617Data$PropLanguages <- 0
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "english", sum(Arsenal1617Data$english), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "french", sum(Arsenal1617Data$french), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "dutch", sum(Arsenal1617Data$dutch), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "portuguese", sum(Arsenal1617Data$portuguese), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "castilian", sum(Arsenal1617Data$castilian), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "spanish", sum(Arsenal1617Data$spanish), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "german", sum(Arsenal1617Data$german), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "arabic", sum(Arsenal1617Data$arabic), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "serbian", sum(Arsenal1617Data$serbian), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "korean", sum(Arsenal1617Data$korean), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "croatian", sum(Arsenal1617Data$croatian), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "italian", sum(Arsenal1617Data$italian), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "armenian", sum(Arsenal1617Data$armenian), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "icelandic", sum(Arsenal1617Data$icelandic), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "hebrew", sum(Arsenal1617Data$hebrew), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "bokmal", sum(Arsenal1617Data$bokmal), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "ukrainian", sum(Arsenal1617Data$ukrainian), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "russian", sum(Arsenal1617Data$russian), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "bosnian", sum(Arsenal1617Data$bosnian), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "czech", sum(Arsenal1617Data$czech), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "danish", sum(Arsenal1617Data$danish), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "swedish", sum(Arsenal1617Data$swedish), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "japanese", sum(Arsenal1617Data$japanese), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "turkish", sum(Arsenal1617Data$turkish), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "asante", sum(Arsenal1617Data$asante), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "persian", sum(Arsenal1617Data$persian), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "maltese", sum(Arsenal1617Data$maltese), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "romanian", sum(Arsenal1617Data$romanian), Arsenal1617Data$PropLanguages)
Arsenal1617Data$PropLanguages <- ifelse(Arsenal1617Data$Languages == "slovak", sum(Arsenal1617Data$slovak), Arsenal1617Data$PropLanguages)

Chelsea1617Data <- subset(PL1617SZN, Squad =="Chelsea")
Chelsea1617Data$english <- ifelse(Chelsea1617Data$Languages=="english", 1, 0)
Chelsea1617Data$french <- ifelse(Chelsea1617Data$Languages=="french", 1, 0)
Chelsea1617Data$dutch <- ifelse(Chelsea1617Data$Languages=="dutch", 1, 0)
Chelsea1617Data$portuguese <- ifelse(Chelsea1617Data$Languages=="portuguese", 1, 0)
Chelsea1617Data$castilian <- ifelse(Chelsea1617Data$Languages=="castilian", 1, 0)
Chelsea1617Data$spanish <- ifelse(Chelsea1617Data$Languages=="spanish", 1, 0)
Chelsea1617Data$german <- ifelse(Chelsea1617Data$Languages=="german", 1, 0)
Chelsea1617Data$arabic <- ifelse(Chelsea1617Data$Languages=="arabic", 1, 0)
Chelsea1617Data$serbian <- ifelse(Chelsea1617Data$Languages=="serbian", 1, 0)
Chelsea1617Data$korean <- ifelse(Chelsea1617Data$Languages=="korean", 1, 0)
Chelsea1617Data$croatian <- ifelse(Chelsea1617Data$Languages=="croatian", 1, 0)
Chelsea1617Data$italian <- ifelse(Chelsea1617Data$Languages=="italian", 1, 0)
Chelsea1617Data$armenian <- ifelse(Chelsea1617Data$Languages=="armenian", 1, 0)
Chelsea1617Data$icelandic <- ifelse(Chelsea1617Data$Languages=="icelandic", 1, 0)
Chelsea1617Data$hebrew <- ifelse(Chelsea1617Data$Languages=="hebrew", 1, 0)
Chelsea1617Data$bokmal <- ifelse(Chelsea1617Data$Languages=="bokmal", 1, 0)
Chelsea1617Data$ukrainian <- ifelse(Chelsea1617Data$Languages=="ukrainian", 1, 0)
Chelsea1617Data$russian <- ifelse(Chelsea1617Data$Languages=="russian", 1, 0)
Chelsea1617Data$bosnian <- ifelse(Chelsea1617Data$Languages=="bosnian", 1, 0)
Chelsea1617Data$czech <- ifelse(Chelsea1617Data$Languages=="czech", 1, 0)
Chelsea1617Data$danish <- ifelse(Chelsea1617Data$Languages=="danish", 1, 0)
Chelsea1617Data$swedish <- ifelse(Chelsea1617Data$Languages=="swedish", 1, 0)
Chelsea1617Data$japanese <- ifelse(Chelsea1617Data$Languages=="japanese", 1, 0)
Chelsea1617Data$turkish <- ifelse(Chelsea1617Data$Languages=="turkish", 1, 0)
Chelsea1617Data$asante <- ifelse(Chelsea1617Data$Languages=="asante", 1, 0)
Chelsea1617Data$persian <- ifelse(Chelsea1617Data$Languages=="persian", 1, 0)
Chelsea1617Data$maltese <- ifelse(Chelsea1617Data$Languages=="maltese", 1, 0)
Chelsea1617Data$romanian <- ifelse(Chelsea1617Data$Languages=="romanian", 1, 0)
Chelsea1617Data$slovak <- ifelse(Chelsea1617Data$Languages=="slovak", 1, 0)
Chelsea1617Data$PropLanguages <- 0
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "english", sum(Chelsea1617Data$english), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "french", sum(Chelsea1617Data$french), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "dutch", sum(Chelsea1617Data$dutch), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "portuguese", sum(Chelsea1617Data$portuguese), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "castilian", sum(Chelsea1617Data$castilian), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "spanish", sum(Chelsea1617Data$spanish), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "german", sum(Chelsea1617Data$german), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "arabic", sum(Chelsea1617Data$arabic), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "serbian", sum(Chelsea1617Data$serbian), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "korean", sum(Chelsea1617Data$korean), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "croatian", sum(Chelsea1617Data$croatian), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "italian", sum(Chelsea1617Data$italian), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "armenian", sum(Chelsea1617Data$armenian), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "icelandic", sum(Chelsea1617Data$icelandic), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "hebrew", sum(Chelsea1617Data$hebrew), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "bokmal", sum(Chelsea1617Data$bokmal), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "ukrainian", sum(Chelsea1617Data$ukrainian), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "russian", sum(Chelsea1617Data$russian), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "bosnian", sum(Chelsea1617Data$bosnian), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "czech", sum(Chelsea1617Data$czech), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "danish", sum(Chelsea1617Data$danish), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "swedish", sum(Chelsea1617Data$swedish), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "japanese", sum(Chelsea1617Data$japanese), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "turkish", sum(Chelsea1617Data$turkish), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "asante", sum(Chelsea1617Data$asante), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "persian", sum(Chelsea1617Data$persian), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "maltese", sum(Chelsea1617Data$maltese), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "romanian", sum(Chelsea1617Data$romanian), Chelsea1617Data$PropLanguages)
Chelsea1617Data$PropLanguages <- ifelse(Chelsea1617Data$Languages == "slovak", sum(Chelsea1617Data$slovak), Chelsea1617Data$PropLanguages)

Everton1617Data <- subset(PL1617SZN, Squad =="Everton")
Everton1617Data$english <- ifelse(Everton1617Data$Languages=="english", 1, 0)
Everton1617Data$french <- ifelse(Everton1617Data$Languages=="french", 1, 0)
Everton1617Data$dutch <- ifelse(Everton1617Data$Languages=="dutch", 1, 0)
Everton1617Data$portuguese <- ifelse(Everton1617Data$Languages=="portuguese", 1, 0)
Everton1617Data$castilian <- ifelse(Everton1617Data$Languages=="castilian", 1, 0)
Everton1617Data$spanish <- ifelse(Everton1617Data$Languages=="spanish", 1, 0)
Everton1617Data$german <- ifelse(Everton1617Data$Languages=="german", 1, 0)
Everton1617Data$arabic <- ifelse(Everton1617Data$Languages=="arabic", 1, 0)
Everton1617Data$serbian <- ifelse(Everton1617Data$Languages=="serbian", 1, 0)
Everton1617Data$korean <- ifelse(Everton1617Data$Languages=="korean", 1, 0)
Everton1617Data$croatian <- ifelse(Everton1617Data$Languages=="croatian", 1, 0)
Everton1617Data$italian <- ifelse(Everton1617Data$Languages=="italian", 1, 0)
Everton1617Data$armenian <- ifelse(Everton1617Data$Languages=="armenian", 1, 0)
Everton1617Data$icelandic <- ifelse(Everton1617Data$Languages=="icelandic", 1, 0)
Everton1617Data$hebrew <- ifelse(Everton1617Data$Languages=="hebrew", 1, 0)
Everton1617Data$bokmal <- ifelse(Everton1617Data$Languages=="bokmal", 1, 0)
Everton1617Data$ukrainian <- ifelse(Everton1617Data$Languages=="ukrainian", 1, 0)
Everton1617Data$russian <- ifelse(Everton1617Data$Languages=="russian", 1, 0)
Everton1617Data$bosnian <- ifelse(Everton1617Data$Languages=="bosnian", 1, 0)
Everton1617Data$czech <- ifelse(Everton1617Data$Languages=="czech", 1, 0)
Everton1617Data$danish <- ifelse(Everton1617Data$Languages=="danish", 1, 0)
Everton1617Data$swedish <- ifelse(Everton1617Data$Languages=="swedish", 1, 0)
Everton1617Data$japanese <- ifelse(Everton1617Data$Languages=="japanese", 1, 0)
Everton1617Data$turkish <- ifelse(Everton1617Data$Languages=="turkish", 1, 0)
Everton1617Data$asante <- ifelse(Everton1617Data$Languages=="asante", 1, 0)
Everton1617Data$persian <- ifelse(Everton1617Data$Languages=="persian", 1, 0)
Everton1617Data$maltese <- ifelse(Everton1617Data$Languages=="maltese", 1, 0)
Everton1617Data$romanian <- ifelse(Everton1617Data$Languages=="romanian", 1, 0)
Everton1617Data$slovak <- ifelse(Everton1617Data$Languages=="slovak", 1, 0)
Everton1617Data$PropLanguages <- 0
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "english", sum(Everton1617Data$english), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "french", sum(Everton1617Data$french), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "dutch", sum(Everton1617Data$dutch), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "portuguese", sum(Everton1617Data$portuguese), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "castilian", sum(Everton1617Data$castilian), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "spanish", sum(Everton1617Data$spanish), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "german", sum(Everton1617Data$german), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "arabic", sum(Everton1617Data$arabic), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "serbian", sum(Everton1617Data$serbian), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "korean", sum(Everton1617Data$korean), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "croatian", sum(Everton1617Data$croatian), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "italian", sum(Everton1617Data$italian), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "armenian", sum(Everton1617Data$armenian), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "icelandic", sum(Everton1617Data$icelandic), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "hebrew", sum(Everton1617Data$hebrew), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "bokmal", sum(Everton1617Data$bokmal), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "ukrainian", sum(Everton1617Data$ukrainian), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "russian", sum(Everton1617Data$russian), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "bosnian", sum(Everton1617Data$bosnian), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "czech", sum(Everton1617Data$czech), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "danish", sum(Everton1617Data$danish), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "swedish", sum(Everton1617Data$swedish), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "japanese", sum(Everton1617Data$japanese), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "turkish", sum(Everton1617Data$turkish), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "asante", sum(Everton1617Data$asante), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "persian", sum(Everton1617Data$persian), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "maltese", sum(Everton1617Data$maltese), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "romanian", sum(Everton1617Data$romanian), Everton1617Data$PropLanguages)
Everton1617Data$PropLanguages <- ifelse(Everton1617Data$Languages == "slovak", sum(Everton1617Data$slovak), Everton1617Data$PropLanguages)

Liverpool1617Data <- subset(PL1617SZN, Squad =="Liverpool")
Liverpool1617Data$english <- ifelse(Liverpool1617Data$Languages=="english", 1, 0)
Liverpool1617Data$french <- ifelse(Liverpool1617Data$Languages=="french", 1, 0)
Liverpool1617Data$dutch <- ifelse(Liverpool1617Data$Languages=="dutch", 1, 0)
Liverpool1617Data$portuguese <- ifelse(Liverpool1617Data$Languages=="portuguese", 1, 0)
Liverpool1617Data$castilian <- ifelse(Liverpool1617Data$Languages=="castilian", 1, 0)
Liverpool1617Data$spanish <- ifelse(Liverpool1617Data$Languages=="spanish", 1, 0)
Liverpool1617Data$german <- ifelse(Liverpool1617Data$Languages=="german", 1, 0)
Liverpool1617Data$arabic <- ifelse(Liverpool1617Data$Languages=="arabic", 1, 0)
Liverpool1617Data$serbian <- ifelse(Liverpool1617Data$Languages=="serbian", 1, 0)
Liverpool1617Data$korean <- ifelse(Liverpool1617Data$Languages=="korean", 1, 0)
Liverpool1617Data$croatian <- ifelse(Liverpool1617Data$Languages=="croatian", 1, 0)
Liverpool1617Data$italian <- ifelse(Liverpool1617Data$Languages=="italian", 1, 0)
Liverpool1617Data$armenian <- ifelse(Liverpool1617Data$Languages=="armenian", 1, 0)
Liverpool1617Data$icelandic <- ifelse(Liverpool1617Data$Languages=="icelandic", 1, 0)
Liverpool1617Data$hebrew <- ifelse(Liverpool1617Data$Languages=="hebrew", 1, 0)
Liverpool1617Data$bokmal <- ifelse(Liverpool1617Data$Languages=="bokmal", 1, 0)
Liverpool1617Data$ukrainian <- ifelse(Liverpool1617Data$Languages=="ukrainian", 1, 0)
Liverpool1617Data$russian <- ifelse(Liverpool1617Data$Languages=="russian", 1, 0)
Liverpool1617Data$bosnian <- ifelse(Liverpool1617Data$Languages=="bosnian", 1, 0)
Liverpool1617Data$czech <- ifelse(Liverpool1617Data$Languages=="czech", 1, 0)
Liverpool1617Data$danish <- ifelse(Liverpool1617Data$Languages=="danish", 1, 0)
Liverpool1617Data$swedish <- ifelse(Liverpool1617Data$Languages=="swedish", 1, 0)
Liverpool1617Data$japanese <- ifelse(Liverpool1617Data$Languages=="japanese", 1, 0)
Liverpool1617Data$turkish <- ifelse(Liverpool1617Data$Languages=="turkish", 1, 0)
Liverpool1617Data$asante <- ifelse(Liverpool1617Data$Languages=="asante", 1, 0)
Liverpool1617Data$persian <- ifelse(Liverpool1617Data$Languages=="persian", 1, 0)
Liverpool1617Data$maltese <- ifelse(Liverpool1617Data$Languages=="maltese", 1, 0)
Liverpool1617Data$romanian <- ifelse(Liverpool1617Data$Languages=="romanian", 1, 0)
Liverpool1617Data$slovak <- ifelse(Liverpool1617Data$Languages=="slovak", 1, 0)
Liverpool1617Data$PropLanguages <- 0
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "english", sum(Liverpool1617Data$english), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "french", sum(Liverpool1617Data$french), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "dutch", sum(Liverpool1617Data$dutch), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "portuguese", sum(Liverpool1617Data$portuguese), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "castilian", sum(Liverpool1617Data$castilian), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "spanish", sum(Liverpool1617Data$spanish), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "german", sum(Liverpool1617Data$german), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "arabic", sum(Liverpool1617Data$arabic), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "serbian", sum(Liverpool1617Data$serbian), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "korean", sum(Liverpool1617Data$korean), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "croatian", sum(Liverpool1617Data$croatian), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "italian", sum(Liverpool1617Data$italian), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "armenian", sum(Liverpool1617Data$armenian), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "icelandic", sum(Liverpool1617Data$icelandic), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "hebrew", sum(Liverpool1617Data$hebrew), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "bokmal", sum(Liverpool1617Data$bokmal), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "ukrainian", sum(Liverpool1617Data$ukrainian), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "russian", sum(Liverpool1617Data$russian), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "bosnian", sum(Liverpool1617Data$bosnian), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "czech", sum(Liverpool1617Data$czech), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "danish", sum(Liverpool1617Data$danish), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "swedish", sum(Liverpool1617Data$swedish), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "japanese", sum(Liverpool1617Data$japanese), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "turkish", sum(Liverpool1617Data$turkish), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "asante", sum(Liverpool1617Data$asante), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "persian", sum(Liverpool1617Data$persian), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "maltese", sum(Liverpool1617Data$maltese), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "romanian", sum(Liverpool1617Data$romanian), Liverpool1617Data$PropLanguages)
Liverpool1617Data$PropLanguages <- ifelse(Liverpool1617Data$Languages == "slovak", sum(Liverpool1617Data$slovak), Liverpool1617Data$PropLanguages)

ManchesterCity1617Data <- subset(PL1617SZN, Squad =="Manchester City")
ManchesterCity1617Data$english <- ifelse(ManchesterCity1617Data$Languages=="english", 1, 0)
ManchesterCity1617Data$french <- ifelse(ManchesterCity1617Data$Languages=="french", 1, 0)
ManchesterCity1617Data$dutch <- ifelse(ManchesterCity1617Data$Languages=="dutch", 1, 0)
ManchesterCity1617Data$portuguese <- ifelse(ManchesterCity1617Data$Languages=="portuguese", 1, 0)
ManchesterCity1617Data$castilian <- ifelse(ManchesterCity1617Data$Languages=="castilian", 1, 0)
ManchesterCity1617Data$spanish <- ifelse(ManchesterCity1617Data$Languages=="spanish", 1, 0)
ManchesterCity1617Data$german <- ifelse(ManchesterCity1617Data$Languages=="german", 1, 0)
ManchesterCity1617Data$arabic <- ifelse(ManchesterCity1617Data$Languages=="arabic", 1, 0)
ManchesterCity1617Data$serbian <- ifelse(ManchesterCity1617Data$Languages=="serbian", 1, 0)
ManchesterCity1617Data$korean <- ifelse(ManchesterCity1617Data$Languages=="korean", 1, 0)
ManchesterCity1617Data$croatian <- ifelse(ManchesterCity1617Data$Languages=="croatian", 1, 0)
ManchesterCity1617Data$italian <- ifelse(ManchesterCity1617Data$Languages=="italian", 1, 0)
ManchesterCity1617Data$armenian <- ifelse(ManchesterCity1617Data$Languages=="armenian", 1, 0)
ManchesterCity1617Data$icelandic <- ifelse(ManchesterCity1617Data$Languages=="icelandic", 1, 0)
ManchesterCity1617Data$hebrew <- ifelse(ManchesterCity1617Data$Languages=="hebrew", 1, 0)
ManchesterCity1617Data$bokmal <- ifelse(ManchesterCity1617Data$Languages=="bokmal", 1, 0)
ManchesterCity1617Data$ukrainian <- ifelse(ManchesterCity1617Data$Languages=="ukrainian", 1, 0)
ManchesterCity1617Data$russian <- ifelse(ManchesterCity1617Data$Languages=="russian", 1, 0)
ManchesterCity1617Data$bosnian <- ifelse(ManchesterCity1617Data$Languages=="bosnian", 1, 0)
ManchesterCity1617Data$czech <- ifelse(ManchesterCity1617Data$Languages=="czech", 1, 0)
ManchesterCity1617Data$danish <- ifelse(ManchesterCity1617Data$Languages=="danish", 1, 0)
ManchesterCity1617Data$swedish <- ifelse(ManchesterCity1617Data$Languages=="swedish", 1, 0)
ManchesterCity1617Data$japanese <- ifelse(ManchesterCity1617Data$Languages=="japanese", 1, 0)
ManchesterCity1617Data$turkish <- ifelse(ManchesterCity1617Data$Languages=="turkish", 1, 0)
ManchesterCity1617Data$asante <- ifelse(ManchesterCity1617Data$Languages=="asante", 1, 0)
ManchesterCity1617Data$persian <- ifelse(ManchesterCity1617Data$Languages=="persian", 1, 0)
ManchesterCity1617Data$maltese <- ifelse(ManchesterCity1617Data$Languages=="maltese", 1, 0)
ManchesterCity1617Data$romanian <- ifelse(ManchesterCity1617Data$Languages=="romanian", 1, 0)
ManchesterCity1617Data$slovak <- ifelse(ManchesterCity1617Data$Languages=="slovak", 1, 0)
ManchesterCity1617Data$PropLanguages <- 0
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "english", sum(ManchesterCity1617Data$english), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "french", sum(ManchesterCity1617Data$french), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "dutch", sum(ManchesterCity1617Data$dutch), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "portuguese", sum(ManchesterCity1617Data$portuguese), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "castilian", sum(ManchesterCity1617Data$castilian), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "spanish", sum(ManchesterCity1617Data$spanish), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "german", sum(ManchesterCity1617Data$german), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "arabic", sum(ManchesterCity1617Data$arabic), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "serbian", sum(ManchesterCity1617Data$serbian), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "korean", sum(ManchesterCity1617Data$korean), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "croatian", sum(ManchesterCity1617Data$croatian), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "italian", sum(ManchesterCity1617Data$italian), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "armenian", sum(ManchesterCity1617Data$armenian), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "icelandic", sum(ManchesterCity1617Data$icelandic), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "hebrew", sum(ManchesterCity1617Data$hebrew), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "bokmal", sum(ManchesterCity1617Data$bokmal), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "ukrainian", sum(ManchesterCity1617Data$ukrainian), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "russian", sum(ManchesterCity1617Data$russian), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "bosnian", sum(ManchesterCity1617Data$bosnian), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "czech", sum(ManchesterCity1617Data$czech), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "danish", sum(ManchesterCity1617Data$danish), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "swedish", sum(ManchesterCity1617Data$swedish), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "japanese", sum(ManchesterCity1617Data$japanese), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "turkish", sum(ManchesterCity1617Data$turkish), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "asante", sum(ManchesterCity1617Data$asante), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "persian", sum(ManchesterCity1617Data$persian), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "maltese", sum(ManchesterCity1617Data$maltese), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "romanian", sum(ManchesterCity1617Data$romanian), ManchesterCity1617Data$PropLanguages)
ManchesterCity1617Data$PropLanguages <- ifelse(ManchesterCity1617Data$Languages == "slovak", sum(ManchesterCity1617Data$slovak), ManchesterCity1617Data$PropLanguages)

ManchesterUtd1617Data <- subset(PL1617SZN, Squad =="Manchester Utd")
ManchesterUtd1617Data$english <- ifelse(ManchesterUtd1617Data$Languages=="english", 1, 0)
ManchesterUtd1617Data$french <- ifelse(ManchesterUtd1617Data$Languages=="french", 1, 0)
ManchesterUtd1617Data$dutch <- ifelse(ManchesterUtd1617Data$Languages=="dutch", 1, 0)
ManchesterUtd1617Data$portuguese <- ifelse(ManchesterUtd1617Data$Languages=="portuguese", 1, 0)
ManchesterUtd1617Data$castilian <- ifelse(ManchesterUtd1617Data$Languages=="castilian", 1, 0)
ManchesterUtd1617Data$spanish <- ifelse(ManchesterUtd1617Data$Languages=="spanish", 1, 0)
ManchesterUtd1617Data$german <- ifelse(ManchesterUtd1617Data$Languages=="german", 1, 0)
ManchesterUtd1617Data$arabic <- ifelse(ManchesterUtd1617Data$Languages=="arabic", 1, 0)
ManchesterUtd1617Data$serbian <- ifelse(ManchesterUtd1617Data$Languages=="serbian", 1, 0)
ManchesterUtd1617Data$korean <- ifelse(ManchesterUtd1617Data$Languages=="korean", 1, 0)
ManchesterUtd1617Data$croatian <- ifelse(ManchesterUtd1617Data$Languages=="croatian", 1, 0)
ManchesterUtd1617Data$italian <- ifelse(ManchesterUtd1617Data$Languages=="italian", 1, 0)
ManchesterUtd1617Data$armenian <- ifelse(ManchesterUtd1617Data$Languages=="armenian", 1, 0)
ManchesterUtd1617Data$icelandic <- ifelse(ManchesterUtd1617Data$Languages=="icelandic", 1, 0)
ManchesterUtd1617Data$hebrew <- ifelse(ManchesterUtd1617Data$Languages=="hebrew", 1, 0)
ManchesterUtd1617Data$bokmal <- ifelse(ManchesterUtd1617Data$Languages=="bokmal", 1, 0)
ManchesterUtd1617Data$ukrainian <- ifelse(ManchesterUtd1617Data$Languages=="ukrainian", 1, 0)
ManchesterUtd1617Data$russian <- ifelse(ManchesterUtd1617Data$Languages=="russian", 1, 0)
ManchesterUtd1617Data$bosnian <- ifelse(ManchesterUtd1617Data$Languages=="bosnian", 1, 0)
ManchesterUtd1617Data$czech <- ifelse(ManchesterUtd1617Data$Languages=="czech", 1, 0)
ManchesterUtd1617Data$danish <- ifelse(ManchesterUtd1617Data$Languages=="danish", 1, 0)
ManchesterUtd1617Data$swedish <- ifelse(ManchesterUtd1617Data$Languages=="swedish", 1, 0)
ManchesterUtd1617Data$japanese <- ifelse(ManchesterUtd1617Data$Languages=="japanese", 1, 0)
ManchesterUtd1617Data$turkish <- ifelse(ManchesterUtd1617Data$Languages=="turkish", 1, 0)
ManchesterUtd1617Data$asante <- ifelse(ManchesterUtd1617Data$Languages=="asante", 1, 0)
ManchesterUtd1617Data$persian <- ifelse(ManchesterUtd1617Data$Languages=="persian", 1, 0)
ManchesterUtd1617Data$maltese <- ifelse(ManchesterUtd1617Data$Languages=="maltese", 1, 0)
ManchesterUtd1617Data$romanian <- ifelse(ManchesterUtd1617Data$Languages=="romanian", 1, 0)
ManchesterUtd1617Data$slovak <- ifelse(ManchesterUtd1617Data$Languages=="slovak", 1, 0)
ManchesterUtd1617Data$PropLanguages <- 0
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "english", sum(ManchesterUtd1617Data$english), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "french", sum(ManchesterUtd1617Data$french), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "dutch", sum(ManchesterUtd1617Data$dutch), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "portuguese", sum(ManchesterUtd1617Data$portuguese), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "castilian", sum(ManchesterUtd1617Data$castilian), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "spanish", sum(ManchesterUtd1617Data$spanish), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "german", sum(ManchesterUtd1617Data$german), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "arabic", sum(ManchesterUtd1617Data$arabic), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "serbian", sum(ManchesterUtd1617Data$serbian), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "korean", sum(ManchesterUtd1617Data$korean), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "croatian", sum(ManchesterUtd1617Data$croatian), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "italian", sum(ManchesterUtd1617Data$italian), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "armenian", sum(ManchesterUtd1617Data$armenian), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "icelandic", sum(ManchesterUtd1617Data$icelandic), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "hebrew", sum(ManchesterUtd1617Data$hebrew), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "bokmal", sum(ManchesterUtd1617Data$bokmal), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "ukrainian", sum(ManchesterUtd1617Data$ukrainian), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "russian", sum(ManchesterUtd1617Data$russian), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "bosnian", sum(ManchesterUtd1617Data$bosnian), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "czech", sum(ManchesterUtd1617Data$czech), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "danish", sum(ManchesterUtd1617Data$danish), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "swedish", sum(ManchesterUtd1617Data$swedish), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "japanese", sum(ManchesterUtd1617Data$japanese), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "turkish", sum(ManchesterUtd1617Data$turkish), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "asante", sum(ManchesterUtd1617Data$asante), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "persian", sum(ManchesterUtd1617Data$persian), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "maltese", sum(ManchesterUtd1617Data$maltese), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "romanian", sum(ManchesterUtd1617Data$romanian), ManchesterUtd1617Data$PropLanguages)
ManchesterUtd1617Data$PropLanguages <- ifelse(ManchesterUtd1617Data$Languages == "slovak", sum(ManchesterUtd1617Data$slovak), ManchesterUtd1617Data$PropLanguages)

Tottenham1617Data <- subset(PL1617SZN, Squad =="Tottenham")
Tottenham1617Data$english <- ifelse(Tottenham1617Data$Languages=="english", 1, 0)
Tottenham1617Data$french <- ifelse(Tottenham1617Data$Languages=="french", 1, 0)
Tottenham1617Data$dutch <- ifelse(Tottenham1617Data$Languages=="dutch", 1, 0)
Tottenham1617Data$portuguese <- ifelse(Tottenham1617Data$Languages=="portuguese", 1, 0)
Tottenham1617Data$castilian <- ifelse(Tottenham1617Data$Languages=="castilian", 1, 0)
Tottenham1617Data$spanish <- ifelse(Tottenham1617Data$Languages=="spanish", 1, 0)
Tottenham1617Data$german <- ifelse(Tottenham1617Data$Languages=="german", 1, 0)
Tottenham1617Data$arabic <- ifelse(Tottenham1617Data$Languages=="arabic", 1, 0)
Tottenham1617Data$serbian <- ifelse(Tottenham1617Data$Languages=="serbian", 1, 0)
Tottenham1617Data$korean <- ifelse(Tottenham1617Data$Languages=="korean", 1, 0)
Tottenham1617Data$croatian <- ifelse(Tottenham1617Data$Languages=="croatian", 1, 0)
Tottenham1617Data$italian <- ifelse(Tottenham1617Data$Languages=="italian", 1, 0)
Tottenham1617Data$armenian <- ifelse(Tottenham1617Data$Languages=="armenian", 1, 0)
Tottenham1617Data$icelandic <- ifelse(Tottenham1617Data$Languages=="icelandic", 1, 0)
Tottenham1617Data$hebrew <- ifelse(Tottenham1617Data$Languages=="hebrew", 1, 0)
Tottenham1617Data$bokmal <- ifelse(Tottenham1617Data$Languages=="bokmal", 1, 0)
Tottenham1617Data$ukrainian <- ifelse(Tottenham1617Data$Languages=="ukrainian", 1, 0)
Tottenham1617Data$russian <- ifelse(Tottenham1617Data$Languages=="russian", 1, 0)
Tottenham1617Data$bosnian <- ifelse(Tottenham1617Data$Languages=="bosnian", 1, 0)
Tottenham1617Data$czech <- ifelse(Tottenham1617Data$Languages=="czech", 1, 0)
Tottenham1617Data$danish <- ifelse(Tottenham1617Data$Languages=="danish", 1, 0)
Tottenham1617Data$swedish <- ifelse(Tottenham1617Data$Languages=="swedish", 1, 0)
Tottenham1617Data$japanese <- ifelse(Tottenham1617Data$Languages=="japanese", 1, 0)
Tottenham1617Data$turkish <- ifelse(Tottenham1617Data$Languages=="turkish", 1, 0)
Tottenham1617Data$asante <- ifelse(Tottenham1617Data$Languages=="asante", 1, 0)
Tottenham1617Data$persian <- ifelse(Tottenham1617Data$Languages=="persian", 1, 0)
Tottenham1617Data$maltese <- ifelse(Tottenham1617Data$Languages=="maltese", 1, 0)
Tottenham1617Data$romanian <- ifelse(Tottenham1617Data$Languages=="romanian", 1, 0)
Tottenham1617Data$slovak <- ifelse(Tottenham1617Data$Languages=="slovak", 1, 0)
Tottenham1617Data$PropLanguages <- 0
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "english", sum(Tottenham1617Data$english), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "french", sum(Tottenham1617Data$french), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "dutch", sum(Tottenham1617Data$dutch), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "portuguese", sum(Tottenham1617Data$portuguese), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "castilian", sum(Tottenham1617Data$castilian), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "spanish", sum(Tottenham1617Data$spanish), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "german", sum(Tottenham1617Data$german), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "arabic", sum(Tottenham1617Data$arabic), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "serbian", sum(Tottenham1617Data$serbian), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "korean", sum(Tottenham1617Data$korean), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "croatian", sum(Tottenham1617Data$croatian), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "italian", sum(Tottenham1617Data$italian), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "armenian", sum(Tottenham1617Data$armenian), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "icelandic", sum(Tottenham1617Data$icelandic), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "hebrew", sum(Tottenham1617Data$hebrew), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "bokmal", sum(Tottenham1617Data$bokmal), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "ukrainian", sum(Tottenham1617Data$ukrainian), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "russian", sum(Tottenham1617Data$russian), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "bosnian", sum(Tottenham1617Data$bosnian), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "czech", sum(Tottenham1617Data$czech), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "danish", sum(Tottenham1617Data$danish), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "swedish", sum(Tottenham1617Data$swedish), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "japanese", sum(Tottenham1617Data$japanese), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "turkish", sum(Tottenham1617Data$turkish), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "asante", sum(Tottenham1617Data$asante), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "persian", sum(Tottenham1617Data$persian), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "maltese", sum(Tottenham1617Data$maltese), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "romanian", sum(Tottenham1617Data$romanian), Tottenham1617Data$PropLanguages)
Tottenham1617Data$PropLanguages <- ifelse(Tottenham1617Data$Languages == "slovak", sum(Tottenham1617Data$slovak), Tottenham1617Data$PropLanguages)

PL1617SZN <- rbind(Arsenal1617Data, Chelsea1617Data)
PL1617SZN <- rbind(PL1617SZN, Everton1617Data)
PL1617SZN <- rbind(PL1617SZN, Liverpool1617Data)
PL1617SZN <- rbind(PL1617SZN, ManchesterCity1617Data)
PL1617SZN <- rbind(PL1617SZN, ManchesterUtd1617Data)
PL1617SZN <- rbind(PL1617SZN, Tottenham1617Data)

PL1516SZN$english <- 0
PL1516SZN$french <- 0
PL1516SZN$portuguese <- 0
PL1516SZN$castilian <- 0
PL1516SZN$dutch <- 0
PL1516SZN$german <- 0
PL1516SZN$spanish <- 0
PL1516SZN$arabic <- 0
PL1516SZN$serbian <- 0
PL1516SZN$korean <- 0
PL1516SZN$croatian <- 0
PL1516SZN$italian <- 0
PL1516SZN$icelandic <- 0
PL1516SZN$hebrew <- 0
PL1516SZN$bokmal <- 0
PL1516SZN$ukrainian <- 0
PL1516SZN$russian <- 0
PL1516SZN$bosnian <- 0
PL1516SZN$czech <- 0
PL1516SZN$danish <- 0
PL1516SZN$armenian <- 0
PL1516SZN$turkish <- 0
PL1516SZN$asante <- 0
PL1516SZN$persian <- 0
PL1516SZN$japanese <- 0
PL1516SZN$maltese <- 0
PL1516SZN$romanian <- 0
PL1516SZN$slovak <- 0
PL1516SZN$swedish <- 0

Arsenal1516Data <- subset(PL1516SZN, Squad =="Arsenal")
Arsenal1516Data$english <- ifelse(Arsenal1516Data$Languages=="english", 1, 0)
Arsenal1516Data$french <- ifelse(Arsenal1516Data$Languages=="french", 1, 0)
Arsenal1516Data$dutch <- ifelse(Arsenal1516Data$Languages=="dutch", 1, 0)
Arsenal1516Data$portuguese <- ifelse(Arsenal1516Data$Languages=="portuguese", 1, 0)
Arsenal1516Data$castilian <- ifelse(Arsenal1516Data$Languages=="castilian", 1, 0)
Arsenal1516Data$spanish <- ifelse(Arsenal1516Data$Languages=="spanish", 1, 0)
Arsenal1516Data$german <- ifelse(Arsenal1516Data$Languages=="german", 1, 0)
Arsenal1516Data$arabic <- ifelse(Arsenal1516Data$Languages=="arabic", 1, 0)
Arsenal1516Data$serbian <- ifelse(Arsenal1516Data$Languages=="serbian", 1, 0)
Arsenal1516Data$korean <- ifelse(Arsenal1516Data$Languages=="korean", 1, 0)
Arsenal1516Data$croatian <- ifelse(Arsenal1516Data$Languages=="croatian", 1, 0)
Arsenal1516Data$italian <- ifelse(Arsenal1516Data$Languages=="italian", 1, 0)
Arsenal1516Data$armenian <- ifelse(Arsenal1516Data$Languages=="armenian", 1, 0)
Arsenal1516Data$icelandic <- ifelse(Arsenal1516Data$Languages=="icelandic", 1, 0)
Arsenal1516Data$hebrew <- ifelse(Arsenal1516Data$Languages=="hebrew", 1, 0)
Arsenal1516Data$bokmal <- ifelse(Arsenal1516Data$Languages=="bokmal", 1, 0)
Arsenal1516Data$ukrainian <- ifelse(Arsenal1516Data$Languages=="ukrainian", 1, 0)
Arsenal1516Data$russian <- ifelse(Arsenal1516Data$Languages=="russian", 1, 0)
Arsenal1516Data$bosnian <- ifelse(Arsenal1516Data$Languages=="bosnian", 1, 0)
Arsenal1516Data$czech <- ifelse(Arsenal1516Data$Languages=="czech", 1, 0)
Arsenal1516Data$danish <- ifelse(Arsenal1516Data$Languages=="danish", 1, 0)
Arsenal1516Data$swedish <- ifelse(Arsenal1516Data$Languages=="swedish", 1, 0)
Arsenal1516Data$japanese <- ifelse(Arsenal1516Data$Languages=="japanese", 1, 0)
Arsenal1516Data$turkish <- ifelse(Arsenal1516Data$Languages=="turkish", 1, 0)
Arsenal1516Data$asante <- ifelse(Arsenal1516Data$Languages=="asante", 1, 0)
Arsenal1516Data$persian <- ifelse(Arsenal1516Data$Languages=="persian", 1, 0)
Arsenal1516Data$maltese <- ifelse(Arsenal1516Data$Languages=="maltese", 1, 0)
Arsenal1516Data$romanian <- ifelse(Arsenal1516Data$Languages=="romanian", 1, 0)
Arsenal1516Data$slovak <- ifelse(Arsenal1516Data$Languages=="slovak", 1, 0)
Arsenal1516Data$PropLanguages <- 0
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "english", sum(Arsenal1516Data$english), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "french", sum(Arsenal1516Data$french), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "dutch", sum(Arsenal1516Data$dutch), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "portuguese", sum(Arsenal1516Data$portuguese), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "castilian", sum(Arsenal1516Data$castilian), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "spanish", sum(Arsenal1516Data$spanish), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "german", sum(Arsenal1516Data$german), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "arabic", sum(Arsenal1516Data$arabic), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "serbian", sum(Arsenal1516Data$serbian), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "korean", sum(Arsenal1516Data$korean), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "croatian", sum(Arsenal1516Data$croatian), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "italian", sum(Arsenal1516Data$italian), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "armenian", sum(Arsenal1516Data$armenian), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "icelandic", sum(Arsenal1516Data$icelandic), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "hebrew", sum(Arsenal1516Data$hebrew), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "bokmal", sum(Arsenal1516Data$bokmal), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "ukrainian", sum(Arsenal1516Data$ukrainian), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "russian", sum(Arsenal1516Data$russian), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "bosnian", sum(Arsenal1516Data$bosnian), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "czech", sum(Arsenal1516Data$czech), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "danish", sum(Arsenal1516Data$danish), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "swedish", sum(Arsenal1516Data$swedish), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "japanese", sum(Arsenal1516Data$japanese), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "turkish", sum(Arsenal1516Data$turkish), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "asante", sum(Arsenal1516Data$asante), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "persian", sum(Arsenal1516Data$persian), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "maltese", sum(Arsenal1516Data$maltese), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "romanian", sum(Arsenal1516Data$romanian), Arsenal1516Data$PropLanguages)
Arsenal1516Data$PropLanguages <- ifelse(Arsenal1516Data$Languages == "slovak", sum(Arsenal1516Data$slovak), Arsenal1516Data$PropLanguages)

Chelsea1516Data <- subset(PL1516SZN, Squad =="Chelsea")
Chelsea1516Data$english <- ifelse(Chelsea1516Data$Languages=="english", 1, 0)
Chelsea1516Data$french <- ifelse(Chelsea1516Data$Languages=="french", 1, 0)
Chelsea1516Data$dutch <- ifelse(Chelsea1516Data$Languages=="dutch", 1, 0)
Chelsea1516Data$portuguese <- ifelse(Chelsea1516Data$Languages=="portuguese", 1, 0)
Chelsea1516Data$castilian <- ifelse(Chelsea1516Data$Languages=="castilian", 1, 0)
Chelsea1516Data$spanish <- ifelse(Chelsea1516Data$Languages=="spanish", 1, 0)
Chelsea1516Data$german <- ifelse(Chelsea1516Data$Languages=="german", 1, 0)
Chelsea1516Data$arabic <- ifelse(Chelsea1516Data$Languages=="arabic", 1, 0)
Chelsea1516Data$serbian <- ifelse(Chelsea1516Data$Languages=="serbian", 1, 0)
Chelsea1516Data$korean <- ifelse(Chelsea1516Data$Languages=="korean", 1, 0)
Chelsea1516Data$croatian <- ifelse(Chelsea1516Data$Languages=="croatian", 1, 0)
Chelsea1516Data$italian <- ifelse(Chelsea1516Data$Languages=="italian", 1, 0)
Chelsea1516Data$armenian <- ifelse(Chelsea1516Data$Languages=="armenian", 1, 0)
Chelsea1516Data$icelandic <- ifelse(Chelsea1516Data$Languages=="icelandic", 1, 0)
Chelsea1516Data$hebrew <- ifelse(Chelsea1516Data$Languages=="hebrew", 1, 0)
Chelsea1516Data$bokmal <- ifelse(Chelsea1516Data$Languages=="bokmal", 1, 0)
Chelsea1516Data$ukrainian <- ifelse(Chelsea1516Data$Languages=="ukrainian", 1, 0)
Chelsea1516Data$russian <- ifelse(Chelsea1516Data$Languages=="russian", 1, 0)
Chelsea1516Data$bosnian <- ifelse(Chelsea1516Data$Languages=="bosnian", 1, 0)
Chelsea1516Data$czech <- ifelse(Chelsea1516Data$Languages=="czech", 1, 0)
Chelsea1516Data$danish <- ifelse(Chelsea1516Data$Languages=="danish", 1, 0)
Chelsea1516Data$swedish <- ifelse(Chelsea1516Data$Languages=="swedish", 1, 0)
Chelsea1516Data$japanese <- ifelse(Chelsea1516Data$Languages=="japanese", 1, 0)
Chelsea1516Data$turkish <- ifelse(Chelsea1516Data$Languages=="turkish", 1, 0)
Chelsea1516Data$asante <- ifelse(Chelsea1516Data$Languages=="asante", 1, 0)
Chelsea1516Data$persian <- ifelse(Chelsea1516Data$Languages=="persian", 1, 0)
Chelsea1516Data$maltese <- ifelse(Chelsea1516Data$Languages=="maltese", 1, 0)
Chelsea1516Data$romanian <- ifelse(Chelsea1516Data$Languages=="romanian", 1, 0)
Chelsea1516Data$slovak <- ifelse(Chelsea1516Data$Languages=="slovak", 1, 0)
Chelsea1516Data$PropLanguages <- 0
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "english", sum(Chelsea1516Data$english), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "french", sum(Chelsea1516Data$french), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "dutch", sum(Chelsea1516Data$dutch), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "portuguese", sum(Chelsea1516Data$portuguese), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "castilian", sum(Chelsea1516Data$castilian), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "spanish", sum(Chelsea1516Data$spanish), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "german", sum(Chelsea1516Data$german), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "arabic", sum(Chelsea1516Data$arabic), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "serbian", sum(Chelsea1516Data$serbian), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "korean", sum(Chelsea1516Data$korean), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "croatian", sum(Chelsea1516Data$croatian), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "italian", sum(Chelsea1516Data$italian), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "armenian", sum(Chelsea1516Data$armenian), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "icelandic", sum(Chelsea1516Data$icelandic), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "hebrew", sum(Chelsea1516Data$hebrew), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "bokmal", sum(Chelsea1516Data$bokmal), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "ukrainian", sum(Chelsea1516Data$ukrainian), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "russian", sum(Chelsea1516Data$russian), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "bosnian", sum(Chelsea1516Data$bosnian), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "czech", sum(Chelsea1516Data$czech), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "danish", sum(Chelsea1516Data$danish), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "swedish", sum(Chelsea1516Data$swedish), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "japanese", sum(Chelsea1516Data$japanese), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "turkish", sum(Chelsea1516Data$turkish), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "asante", sum(Chelsea1516Data$asante), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "persian", sum(Chelsea1516Data$persian), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "maltese", sum(Chelsea1516Data$maltese), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "romanian", sum(Chelsea1516Data$romanian), Chelsea1516Data$PropLanguages)
Chelsea1516Data$PropLanguages <- ifelse(Chelsea1516Data$Languages == "slovak", sum(Chelsea1516Data$slovak), Chelsea1516Data$PropLanguages)

Everton1516Data <- subset(PL1516SZN, Squad =="Everton")
Everton1516Data$english <- ifelse(Everton1516Data$Languages=="english", 1, 0)
Everton1516Data$french <- ifelse(Everton1516Data$Languages=="french", 1, 0)
Everton1516Data$dutch <- ifelse(Everton1516Data$Languages=="dutch", 1, 0)
Everton1516Data$portuguese <- ifelse(Everton1516Data$Languages=="portuguese", 1, 0)
Everton1516Data$castilian <- ifelse(Everton1516Data$Languages=="castilian", 1, 0)
Everton1516Data$spanish <- ifelse(Everton1516Data$Languages=="spanish", 1, 0)
Everton1516Data$german <- ifelse(Everton1516Data$Languages=="german", 1, 0)
Everton1516Data$arabic <- ifelse(Everton1516Data$Languages=="arabic", 1, 0)
Everton1516Data$serbian <- ifelse(Everton1516Data$Languages=="serbian", 1, 0)
Everton1516Data$korean <- ifelse(Everton1516Data$Languages=="korean", 1, 0)
Everton1516Data$croatian <- ifelse(Everton1516Data$Languages=="croatian", 1, 0)
Everton1516Data$italian <- ifelse(Everton1516Data$Languages=="italian", 1, 0)
Everton1516Data$armenian <- ifelse(Everton1516Data$Languages=="armenian", 1, 0)
Everton1516Data$icelandic <- ifelse(Everton1516Data$Languages=="icelandic", 1, 0)
Everton1516Data$hebrew <- ifelse(Everton1516Data$Languages=="hebrew", 1, 0)
Everton1516Data$bokmal <- ifelse(Everton1516Data$Languages=="bokmal", 1, 0)
Everton1516Data$ukrainian <- ifelse(Everton1516Data$Languages=="ukrainian", 1, 0)
Everton1516Data$russian <- ifelse(Everton1516Data$Languages=="russian", 1, 0)
Everton1516Data$bosnian <- ifelse(Everton1516Data$Languages=="bosnian", 1, 0)
Everton1516Data$czech <- ifelse(Everton1516Data$Languages=="czech", 1, 0)
Everton1516Data$danish <- ifelse(Everton1516Data$Languages=="danish", 1, 0)
Everton1516Data$swedish <- ifelse(Everton1516Data$Languages=="swedish", 1, 0)
Everton1516Data$japanese <- ifelse(Everton1516Data$Languages=="japanese", 1, 0)
Everton1516Data$turkish <- ifelse(Everton1516Data$Languages=="turkish", 1, 0)
Everton1516Data$asante <- ifelse(Everton1516Data$Languages=="asante", 1, 0)
Everton1516Data$persian <- ifelse(Everton1516Data$Languages=="persian", 1, 0)
Everton1516Data$maltese <- ifelse(Everton1516Data$Languages=="maltese", 1, 0)
Everton1516Data$romanian <- ifelse(Everton1516Data$Languages=="romanian", 1, 0)
Everton1516Data$slovak <- ifelse(Everton1516Data$Languages=="slovak", 1, 0)
Everton1516Data$PropLanguages <- 0
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "english", sum(Everton1516Data$english), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "french", sum(Everton1516Data$french), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "dutch", sum(Everton1516Data$dutch), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "portuguese", sum(Everton1516Data$portuguese), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "castilian", sum(Everton1516Data$castilian), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "spanish", sum(Everton1516Data$spanish), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "german", sum(Everton1516Data$german), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "arabic", sum(Everton1516Data$arabic), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "serbian", sum(Everton1516Data$serbian), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "korean", sum(Everton1516Data$korean), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "croatian", sum(Everton1516Data$croatian), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "italian", sum(Everton1516Data$italian), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "armenian", sum(Everton1516Data$armenian), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "icelandic", sum(Everton1516Data$icelandic), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "hebrew", sum(Everton1516Data$hebrew), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "bokmal", sum(Everton1516Data$bokmal), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "ukrainian", sum(Everton1516Data$ukrainian), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "russian", sum(Everton1516Data$russian), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "bosnian", sum(Everton1516Data$bosnian), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "czech", sum(Everton1516Data$czech), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "danish", sum(Everton1516Data$danish), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "swedish", sum(Everton1516Data$swedish), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "japanese", sum(Everton1516Data$japanese), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "turkish", sum(Everton1516Data$turkish), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "asante", sum(Everton1516Data$asante), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "persian", sum(Everton1516Data$persian), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "maltese", sum(Everton1516Data$maltese), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "romanian", sum(Everton1516Data$romanian), Everton1516Data$PropLanguages)
Everton1516Data$PropLanguages <- ifelse(Everton1516Data$Languages == "slovak", sum(Everton1516Data$slovak), Everton1516Data$PropLanguages)

Liverpool1516Data <- subset(PL1516SZN, Squad =="Liverpool")
Liverpool1516Data$english <- ifelse(Liverpool1516Data$Languages=="english", 1, 0)
Liverpool1516Data$french <- ifelse(Liverpool1516Data$Languages=="french", 1, 0)
Liverpool1516Data$dutch <- ifelse(Liverpool1516Data$Languages=="dutch", 1, 0)
Liverpool1516Data$portuguese <- ifelse(Liverpool1516Data$Languages=="portuguese", 1, 0)
Liverpool1516Data$castilian <- ifelse(Liverpool1516Data$Languages=="castilian", 1, 0)
Liverpool1516Data$spanish <- ifelse(Liverpool1516Data$Languages=="spanish", 1, 0)
Liverpool1516Data$german <- ifelse(Liverpool1516Data$Languages=="german", 1, 0)
Liverpool1516Data$arabic <- ifelse(Liverpool1516Data$Languages=="arabic", 1, 0)
Liverpool1516Data$serbian <- ifelse(Liverpool1516Data$Languages=="serbian", 1, 0)
Liverpool1516Data$korean <- ifelse(Liverpool1516Data$Languages=="korean", 1, 0)
Liverpool1516Data$croatian <- ifelse(Liverpool1516Data$Languages=="croatian", 1, 0)
Liverpool1516Data$italian <- ifelse(Liverpool1516Data$Languages=="italian", 1, 0)
Liverpool1516Data$armenian <- ifelse(Liverpool1516Data$Languages=="armenian", 1, 0)
Liverpool1516Data$icelandic <- ifelse(Liverpool1516Data$Languages=="icelandic", 1, 0)
Liverpool1516Data$hebrew <- ifelse(Liverpool1516Data$Languages=="hebrew", 1, 0)
Liverpool1516Data$bokmal <- ifelse(Liverpool1516Data$Languages=="bokmal", 1, 0)
Liverpool1516Data$ukrainian <- ifelse(Liverpool1516Data$Languages=="ukrainian", 1, 0)
Liverpool1516Data$russian <- ifelse(Liverpool1516Data$Languages=="russian", 1, 0)
Liverpool1516Data$bosnian <- ifelse(Liverpool1516Data$Languages=="bosnian", 1, 0)
Liverpool1516Data$czech <- ifelse(Liverpool1516Data$Languages=="czech", 1, 0)
Liverpool1516Data$danish <- ifelse(Liverpool1516Data$Languages=="danish", 1, 0)
Liverpool1516Data$swedish <- ifelse(Liverpool1516Data$Languages=="swedish", 1, 0)
Liverpool1516Data$japanese <- ifelse(Liverpool1516Data$Languages=="japanese", 1, 0)
Liverpool1516Data$turkish <- ifelse(Liverpool1516Data$Languages=="turkish", 1, 0)
Liverpool1516Data$asante <- ifelse(Liverpool1516Data$Languages=="asante", 1, 0)
Liverpool1516Data$persian <- ifelse(Liverpool1516Data$Languages=="persian", 1, 0)
Liverpool1516Data$maltese <- ifelse(Liverpool1516Data$Languages=="maltese", 1, 0)
Liverpool1516Data$romanian <- ifelse(Liverpool1516Data$Languages=="romanian", 1, 0)
Liverpool1516Data$slovak <- ifelse(Liverpool1516Data$Languages=="slovak", 1, 0)
Liverpool1516Data$PropLanguages <- 0
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "english", sum(Liverpool1516Data$english), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "french", sum(Liverpool1516Data$french), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "dutch", sum(Liverpool1516Data$dutch), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "portuguese", sum(Liverpool1516Data$portuguese), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "castilian", sum(Liverpool1516Data$castilian), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "spanish", sum(Liverpool1516Data$spanish), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "german", sum(Liverpool1516Data$german), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "arabic", sum(Liverpool1516Data$arabic), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "serbian", sum(Liverpool1516Data$serbian), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "korean", sum(Liverpool1516Data$korean), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "croatian", sum(Liverpool1516Data$croatian), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "italian", sum(Liverpool1516Data$italian), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "armenian", sum(Liverpool1516Data$armenian), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "icelandic", sum(Liverpool1516Data$icelandic), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "hebrew", sum(Liverpool1516Data$hebrew), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "bokmal", sum(Liverpool1516Data$bokmal), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "ukrainian", sum(Liverpool1516Data$ukrainian), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "russian", sum(Liverpool1516Data$russian), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "bosnian", sum(Liverpool1516Data$bosnian), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "czech", sum(Liverpool1516Data$czech), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "danish", sum(Liverpool1516Data$danish), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "swedish", sum(Liverpool1516Data$swedish), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "japanese", sum(Liverpool1516Data$japanese), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "turkish", sum(Liverpool1516Data$turkish), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "asante", sum(Liverpool1516Data$asante), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "persian", sum(Liverpool1516Data$persian), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "maltese", sum(Liverpool1516Data$maltese), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "romanian", sum(Liverpool1516Data$romanian), Liverpool1516Data$PropLanguages)
Liverpool1516Data$PropLanguages <- ifelse(Liverpool1516Data$Languages == "slovak", sum(Liverpool1516Data$slovak), Liverpool1516Data$PropLanguages)

ManchesterCity1516Data <- subset(PL1516SZN, Squad =="Manchester City")
ManchesterCity1516Data$english <- ifelse(ManchesterCity1516Data$Languages=="english", 1, 0)
ManchesterCity1516Data$french <- ifelse(ManchesterCity1516Data$Languages=="french", 1, 0)
ManchesterCity1516Data$dutch <- ifelse(ManchesterCity1516Data$Languages=="dutch", 1, 0)
ManchesterCity1516Data$portuguese <- ifelse(ManchesterCity1516Data$Languages=="portuguese", 1, 0)
ManchesterCity1516Data$castilian <- ifelse(ManchesterCity1516Data$Languages=="castilian", 1, 0)
ManchesterCity1516Data$spanish <- ifelse(ManchesterCity1516Data$Languages=="spanish", 1, 0)
ManchesterCity1516Data$german <- ifelse(ManchesterCity1516Data$Languages=="german", 1, 0)
ManchesterCity1516Data$arabic <- ifelse(ManchesterCity1516Data$Languages=="arabic", 1, 0)
ManchesterCity1516Data$serbian <- ifelse(ManchesterCity1516Data$Languages=="serbian", 1, 0)
ManchesterCity1516Data$korean <- ifelse(ManchesterCity1516Data$Languages=="korean", 1, 0)
ManchesterCity1516Data$croatian <- ifelse(ManchesterCity1516Data$Languages=="croatian", 1, 0)
ManchesterCity1516Data$italian <- ifelse(ManchesterCity1516Data$Languages=="italian", 1, 0)
ManchesterCity1516Data$armenian <- ifelse(ManchesterCity1516Data$Languages=="armenian", 1, 0)
ManchesterCity1516Data$icelandic <- ifelse(ManchesterCity1516Data$Languages=="icelandic", 1, 0)
ManchesterCity1516Data$hebrew <- ifelse(ManchesterCity1516Data$Languages=="hebrew", 1, 0)
ManchesterCity1516Data$bokmal <- ifelse(ManchesterCity1516Data$Languages=="bokmal", 1, 0)
ManchesterCity1516Data$ukrainian <- ifelse(ManchesterCity1516Data$Languages=="ukrainian", 1, 0)
ManchesterCity1516Data$russian <- ifelse(ManchesterCity1516Data$Languages=="russian", 1, 0)
ManchesterCity1516Data$bosnian <- ifelse(ManchesterCity1516Data$Languages=="bosnian", 1, 0)
ManchesterCity1516Data$czech <- ifelse(ManchesterCity1516Data$Languages=="czech", 1, 0)
ManchesterCity1516Data$danish <- ifelse(ManchesterCity1516Data$Languages=="danish", 1, 0)
ManchesterCity1516Data$swedish <- ifelse(ManchesterCity1516Data$Languages=="swedish", 1, 0)
ManchesterCity1516Data$japanese <- ifelse(ManchesterCity1516Data$Languages=="japanese", 1, 0)
ManchesterCity1516Data$turkish <- ifelse(ManchesterCity1516Data$Languages=="turkish", 1, 0)
ManchesterCity1516Data$asante <- ifelse(ManchesterCity1516Data$Languages=="asante", 1, 0)
ManchesterCity1516Data$persian <- ifelse(ManchesterCity1516Data$Languages=="persian", 1, 0)
ManchesterCity1516Data$maltese <- ifelse(ManchesterCity1516Data$Languages=="maltese", 1, 0)
ManchesterCity1516Data$romanian <- ifelse(ManchesterCity1516Data$Languages=="romanian", 1, 0)
ManchesterCity1516Data$slovak <- ifelse(ManchesterCity1516Data$Languages=="slovak", 1, 0)
ManchesterCity1516Data$PropLanguages <- 0
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "english", sum(ManchesterCity1516Data$english), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "french", sum(ManchesterCity1516Data$french), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "dutch", sum(ManchesterCity1516Data$dutch), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "portuguese", sum(ManchesterCity1516Data$portuguese), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "castilian", sum(ManchesterCity1516Data$castilian), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "spanish", sum(ManchesterCity1516Data$spanish), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "german", sum(ManchesterCity1516Data$german), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "arabic", sum(ManchesterCity1516Data$arabic), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "serbian", sum(ManchesterCity1516Data$serbian), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "korean", sum(ManchesterCity1516Data$korean), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "croatian", sum(ManchesterCity1516Data$croatian), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "italian", sum(ManchesterCity1516Data$italian), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "armenian", sum(ManchesterCity1516Data$armenian), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "icelandic", sum(ManchesterCity1516Data$icelandic), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "hebrew", sum(ManchesterCity1516Data$hebrew), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "bokmal", sum(ManchesterCity1516Data$bokmal), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "ukrainian", sum(ManchesterCity1516Data$ukrainian), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "russian", sum(ManchesterCity1516Data$russian), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "bosnian", sum(ManchesterCity1516Data$bosnian), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "czech", sum(ManchesterCity1516Data$czech), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "danish", sum(ManchesterCity1516Data$danish), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "swedish", sum(ManchesterCity1516Data$swedish), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "japanese", sum(ManchesterCity1516Data$japanese), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "turkish", sum(ManchesterCity1516Data$turkish), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "asante", sum(ManchesterCity1516Data$asante), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "persian", sum(ManchesterCity1516Data$persian), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "maltese", sum(ManchesterCity1516Data$maltese), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "romanian", sum(ManchesterCity1516Data$romanian), ManchesterCity1516Data$PropLanguages)
ManchesterCity1516Data$PropLanguages <- ifelse(ManchesterCity1516Data$Languages == "slovak", sum(ManchesterCity1516Data$slovak), ManchesterCity1516Data$PropLanguages)

ManchesterUtd1516Data <- subset(PL1516SZN, Squad =="Manchester Utd")
ManchesterUtd1516Data$english <- ifelse(ManchesterUtd1516Data$Languages=="english", 1, 0)
ManchesterUtd1516Data$french <- ifelse(ManchesterUtd1516Data$Languages=="french", 1, 0)
ManchesterUtd1516Data$dutch <- ifelse(ManchesterUtd1516Data$Languages=="dutch", 1, 0)
ManchesterUtd1516Data$portuguese <- ifelse(ManchesterUtd1516Data$Languages=="portuguese", 1, 0)
ManchesterUtd1516Data$castilian <- ifelse(ManchesterUtd1516Data$Languages=="castilian", 1, 0)
ManchesterUtd1516Data$spanish <- ifelse(ManchesterUtd1516Data$Languages=="spanish", 1, 0)
ManchesterUtd1516Data$german <- ifelse(ManchesterUtd1516Data$Languages=="german", 1, 0)
ManchesterUtd1516Data$arabic <- ifelse(ManchesterUtd1516Data$Languages=="arabic", 1, 0)
ManchesterUtd1516Data$serbian <- ifelse(ManchesterUtd1516Data$Languages=="serbian", 1, 0)
ManchesterUtd1516Data$korean <- ifelse(ManchesterUtd1516Data$Languages=="korean", 1, 0)
ManchesterUtd1516Data$croatian <- ifelse(ManchesterUtd1516Data$Languages=="croatian", 1, 0)
ManchesterUtd1516Data$italian <- ifelse(ManchesterUtd1516Data$Languages=="italian", 1, 0)
ManchesterUtd1516Data$armenian <- ifelse(ManchesterUtd1516Data$Languages=="armenian", 1, 0)
ManchesterUtd1516Data$icelandic <- ifelse(ManchesterUtd1516Data$Languages=="icelandic", 1, 0)
ManchesterUtd1516Data$hebrew <- ifelse(ManchesterUtd1516Data$Languages=="hebrew", 1, 0)
ManchesterUtd1516Data$bokmal <- ifelse(ManchesterUtd1516Data$Languages=="bokmal", 1, 0)
ManchesterUtd1516Data$ukrainian <- ifelse(ManchesterUtd1516Data$Languages=="ukrainian", 1, 0)
ManchesterUtd1516Data$russian <- ifelse(ManchesterUtd1516Data$Languages=="russian", 1, 0)
ManchesterUtd1516Data$bosnian <- ifelse(ManchesterUtd1516Data$Languages=="bosnian", 1, 0)
ManchesterUtd1516Data$czech <- ifelse(ManchesterUtd1516Data$Languages=="czech", 1, 0)
ManchesterUtd1516Data$danish <- ifelse(ManchesterUtd1516Data$Languages=="danish", 1, 0)
ManchesterUtd1516Data$swedish <- ifelse(ManchesterUtd1516Data$Languages=="swedish", 1, 0)
ManchesterUtd1516Data$japanese <- ifelse(ManchesterUtd1516Data$Languages=="japanese", 1, 0)
ManchesterUtd1516Data$turkish <- ifelse(ManchesterUtd1516Data$Languages=="turkish", 1, 0)
ManchesterUtd1516Data$asante <- ifelse(ManchesterUtd1516Data$Languages=="asante", 1, 0)
ManchesterUtd1516Data$persian <- ifelse(ManchesterUtd1516Data$Languages=="persian", 1, 0)
ManchesterUtd1516Data$maltese <- ifelse(ManchesterUtd1516Data$Languages=="maltese", 1, 0)
ManchesterUtd1516Data$romanian <- ifelse(ManchesterUtd1516Data$Languages=="romanian", 1, 0)
ManchesterUtd1516Data$slovak <- ifelse(ManchesterUtd1516Data$Languages=="slovak", 1, 0)
ManchesterUtd1516Data$PropLanguages <- 0
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "english", sum(ManchesterUtd1516Data$english), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "french", sum(ManchesterUtd1516Data$french), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "dutch", sum(ManchesterUtd1516Data$dutch), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "portuguese", sum(ManchesterUtd1516Data$portuguese), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "castilian", sum(ManchesterUtd1516Data$castilian), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "spanish", sum(ManchesterUtd1516Data$spanish), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "german", sum(ManchesterUtd1516Data$german), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "arabic", sum(ManchesterUtd1516Data$arabic), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "serbian", sum(ManchesterUtd1516Data$serbian), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "korean", sum(ManchesterUtd1516Data$korean), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "croatian", sum(ManchesterUtd1516Data$croatian), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "italian", sum(ManchesterUtd1516Data$italian), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "armenian", sum(ManchesterUtd1516Data$armenian), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "icelandic", sum(ManchesterUtd1516Data$icelandic), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "hebrew", sum(ManchesterUtd1516Data$hebrew), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "bokmal", sum(ManchesterUtd1516Data$bokmal), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "ukrainian", sum(ManchesterUtd1516Data$ukrainian), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "russian", sum(ManchesterUtd1516Data$russian), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "bosnian", sum(ManchesterUtd1516Data$bosnian), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "czech", sum(ManchesterUtd1516Data$czech), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "danish", sum(ManchesterUtd1516Data$danish), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "swedish", sum(ManchesterUtd1516Data$swedish), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "japanese", sum(ManchesterUtd1516Data$japanese), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "turkish", sum(ManchesterUtd1516Data$turkish), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "asante", sum(ManchesterUtd1516Data$asante), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "persian", sum(ManchesterUtd1516Data$persian), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "maltese", sum(ManchesterUtd1516Data$maltese), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "romanian", sum(ManchesterUtd1516Data$romanian), ManchesterUtd1516Data$PropLanguages)
ManchesterUtd1516Data$PropLanguages <- ifelse(ManchesterUtd1516Data$Languages == "slovak", sum(ManchesterUtd1516Data$slovak), ManchesterUtd1516Data$PropLanguages)

Tottenham1516Data <- subset(PL1516SZN, Squad =="Tottenham")
Tottenham1516Data$english <- ifelse(Tottenham1516Data$Languages=="english", 1, 0)
Tottenham1516Data$french <- ifelse(Tottenham1516Data$Languages=="french", 1, 0)
Tottenham1516Data$dutch <- ifelse(Tottenham1516Data$Languages=="dutch", 1, 0)
Tottenham1516Data$portuguese <- ifelse(Tottenham1516Data$Languages=="portuguese", 1, 0)
Tottenham1516Data$castilian <- ifelse(Tottenham1516Data$Languages=="castilian", 1, 0)
Tottenham1516Data$spanish <- ifelse(Tottenham1516Data$Languages=="spanish", 1, 0)
Tottenham1516Data$german <- ifelse(Tottenham1516Data$Languages=="german", 1, 0)
Tottenham1516Data$arabic <- ifelse(Tottenham1516Data$Languages=="arabic", 1, 0)
Tottenham1516Data$serbian <- ifelse(Tottenham1516Data$Languages=="serbian", 1, 0)
Tottenham1516Data$korean <- ifelse(Tottenham1516Data$Languages=="korean", 1, 0)
Tottenham1516Data$croatian <- ifelse(Tottenham1516Data$Languages=="croatian", 1, 0)
Tottenham1516Data$italian <- ifelse(Tottenham1516Data$Languages=="italian", 1, 0)
Tottenham1516Data$armenian <- ifelse(Tottenham1516Data$Languages=="armenian", 1, 0)
Tottenham1516Data$icelandic <- ifelse(Tottenham1516Data$Languages=="icelandic", 1, 0)
Tottenham1516Data$hebrew <- ifelse(Tottenham1516Data$Languages=="hebrew", 1, 0)
Tottenham1516Data$bokmal <- ifelse(Tottenham1516Data$Languages=="bokmal", 1, 0)
Tottenham1516Data$ukrainian <- ifelse(Tottenham1516Data$Languages=="ukrainian", 1, 0)
Tottenham1516Data$russian <- ifelse(Tottenham1516Data$Languages=="russian", 1, 0)
Tottenham1516Data$bosnian <- ifelse(Tottenham1516Data$Languages=="bosnian", 1, 0)
Tottenham1516Data$czech <- ifelse(Tottenham1516Data$Languages=="czech", 1, 0)
Tottenham1516Data$danish <- ifelse(Tottenham1516Data$Languages=="danish", 1, 0)
Tottenham1516Data$swedish <- ifelse(Tottenham1516Data$Languages=="swedish", 1, 0)
Tottenham1516Data$japanese <- ifelse(Tottenham1516Data$Languages=="japanese", 1, 0)
Tottenham1516Data$turkish <- ifelse(Tottenham1516Data$Languages=="turkish", 1, 0)
Tottenham1516Data$asante <- ifelse(Tottenham1516Data$Languages=="asante", 1, 0)
Tottenham1516Data$persian <- ifelse(Tottenham1516Data$Languages=="persian", 1, 0)
Tottenham1516Data$maltese <- ifelse(Tottenham1516Data$Languages=="maltese", 1, 0)
Tottenham1516Data$romanian <- ifelse(Tottenham1516Data$Languages=="romanian", 1, 0)
Tottenham1516Data$slovak <- ifelse(Tottenham1516Data$Languages=="slovak", 1, 0)
Tottenham1516Data$PropLanguages <- 0
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "english", sum(Tottenham1516Data$english), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "french", sum(Tottenham1516Data$french), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "dutch", sum(Tottenham1516Data$dutch), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "portuguese", sum(Tottenham1516Data$portuguese), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "castilian", sum(Tottenham1516Data$castilian), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "spanish", sum(Tottenham1516Data$spanish), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "german", sum(Tottenham1516Data$german), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "arabic", sum(Tottenham1516Data$arabic), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "serbian", sum(Tottenham1516Data$serbian), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "korean", sum(Tottenham1516Data$korean), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "croatian", sum(Tottenham1516Data$croatian), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "italian", sum(Tottenham1516Data$italian), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "armenian", sum(Tottenham1516Data$armenian), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "icelandic", sum(Tottenham1516Data$icelandic), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "hebrew", sum(Tottenham1516Data$hebrew), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "bokmal", sum(Tottenham1516Data$bokmal), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "ukrainian", sum(Tottenham1516Data$ukrainian), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "russian", sum(Tottenham1516Data$russian), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "bosnian", sum(Tottenham1516Data$bosnian), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "czech", sum(Tottenham1516Data$czech), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "danish", sum(Tottenham1516Data$danish), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "swedish", sum(Tottenham1516Data$swedish), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "japanese", sum(Tottenham1516Data$japanese), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "turkish", sum(Tottenham1516Data$turkish), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "asante", sum(Tottenham1516Data$asante), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "persian", sum(Tottenham1516Data$persian), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "maltese", sum(Tottenham1516Data$maltese), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "romanian", sum(Tottenham1516Data$romanian), Tottenham1516Data$PropLanguages)
Tottenham1516Data$PropLanguages <- ifelse(Tottenham1516Data$Languages == "slovak", sum(Tottenham1516Data$slovak), Tottenham1516Data$PropLanguages)

PL1516SZN <- rbind(Arsenal1516Data, Chelsea1516Data)
PL1516SZN <- rbind(PL1516SZN, Everton1516Data)
PL1516SZN <- rbind(PL1516SZN, Liverpool1516Data)
PL1516SZN <- rbind(PL1516SZN, ManchesterCity1516Data)
PL1516SZN <- rbind(PL1516SZN, ManchesterUtd1516Data)
PL1516SZN <- rbind(PL1516SZN, Tottenham1516Data)

PL1415SZN$english <- 0
PL1415SZN$french <- 0
PL1415SZN$portuguese <- 0
PL1415SZN$castilian <- 0
PL1415SZN$dutch <- 0
PL1415SZN$german <- 0
PL1415SZN$spanish <- 0
PL1415SZN$arabic <- 0
PL1415SZN$serbian <- 0
PL1415SZN$korean <- 0
PL1415SZN$croatian <- 0
PL1415SZN$italian <- 0
PL1415SZN$icelandic <- 0
PL1415SZN$hebrew <- 0
PL1415SZN$bokmal <- 0
PL1415SZN$ukrainian <- 0
PL1415SZN$russian <- 0
PL1415SZN$bosnian <- 0
PL1415SZN$czech <- 0
PL1415SZN$danish <- 0
PL1415SZN$armenian <- 0
PL1415SZN$turkish <- 0
PL1415SZN$asante <- 0
PL1415SZN$persian <- 0
PL1415SZN$japanese <- 0
PL1415SZN$maltese <- 0
PL1415SZN$romanian <- 0
PL1415SZN$slovak <- 0
PL1415SZN$swedish <- 0

Arsenal1415Data <- subset(PL1415SZN, Squad =="Arsenal")
Arsenal1415Data$english <- ifelse(Arsenal1415Data$Languages=="english", 1, 0)
Arsenal1415Data$french <- ifelse(Arsenal1415Data$Languages=="french", 1, 0)
Arsenal1415Data$dutch <- ifelse(Arsenal1415Data$Languages=="dutch", 1, 0)
Arsenal1415Data$portuguese <- ifelse(Arsenal1415Data$Languages=="portuguese", 1, 0)
Arsenal1415Data$castilian <- ifelse(Arsenal1415Data$Languages=="castilian", 1, 0)
Arsenal1415Data$spanish <- ifelse(Arsenal1415Data$Languages=="spanish", 1, 0)
Arsenal1415Data$german <- ifelse(Arsenal1415Data$Languages=="german", 1, 0)
Arsenal1415Data$arabic <- ifelse(Arsenal1415Data$Languages=="arabic", 1, 0)
Arsenal1415Data$serbian <- ifelse(Arsenal1415Data$Languages=="serbian", 1, 0)
Arsenal1415Data$korean <- ifelse(Arsenal1415Data$Languages=="korean", 1, 0)
Arsenal1415Data$croatian <- ifelse(Arsenal1415Data$Languages=="croatian", 1, 0)
Arsenal1415Data$italian <- ifelse(Arsenal1415Data$Languages=="italian", 1, 0)
Arsenal1415Data$armenian <- ifelse(Arsenal1415Data$Languages=="armenian", 1, 0)
Arsenal1415Data$icelandic <- ifelse(Arsenal1415Data$Languages=="icelandic", 1, 0)
Arsenal1415Data$hebrew <- ifelse(Arsenal1415Data$Languages=="hebrew", 1, 0)
Arsenal1415Data$bokmal <- ifelse(Arsenal1415Data$Languages=="bokmal", 1, 0)
Arsenal1415Data$ukrainian <- ifelse(Arsenal1415Data$Languages=="ukrainian", 1, 0)
Arsenal1415Data$russian <- ifelse(Arsenal1415Data$Languages=="russian", 1, 0)
Arsenal1415Data$bosnian <- ifelse(Arsenal1415Data$Languages=="bosnian", 1, 0)
Arsenal1415Data$czech <- ifelse(Arsenal1415Data$Languages=="czech", 1, 0)
Arsenal1415Data$danish <- ifelse(Arsenal1415Data$Languages=="danish", 1, 0)
Arsenal1415Data$swedish <- ifelse(Arsenal1415Data$Languages=="swedish", 1, 0)
Arsenal1415Data$japanese <- ifelse(Arsenal1415Data$Languages=="japanese", 1, 0)
Arsenal1415Data$turkish <- ifelse(Arsenal1415Data$Languages=="turkish", 1, 0)
Arsenal1415Data$asante <- ifelse(Arsenal1415Data$Languages=="asante", 1, 0)
Arsenal1415Data$persian <- ifelse(Arsenal1415Data$Languages=="persian", 1, 0)
Arsenal1415Data$maltese <- ifelse(Arsenal1415Data$Languages=="maltese", 1, 0)
Arsenal1415Data$romanian <- ifelse(Arsenal1415Data$Languages=="romanian", 1, 0)
Arsenal1415Data$slovak <- ifelse(Arsenal1415Data$Languages=="slovak", 1, 0)
Arsenal1415Data$PropLanguages <- 0
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "english", sum(Arsenal1415Data$english), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "french", sum(Arsenal1415Data$french), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "dutch", sum(Arsenal1415Data$dutch), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "portuguese", sum(Arsenal1415Data$portuguese), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "castilian", sum(Arsenal1415Data$castilian), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "spanish", sum(Arsenal1415Data$spanish), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "german", sum(Arsenal1415Data$german), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "arabic", sum(Arsenal1415Data$arabic), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "serbian", sum(Arsenal1415Data$serbian), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "korean", sum(Arsenal1415Data$korean), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "croatian", sum(Arsenal1415Data$croatian), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "italian", sum(Arsenal1415Data$italian), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "armenian", sum(Arsenal1415Data$armenian), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "icelandic", sum(Arsenal1415Data$icelandic), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "hebrew", sum(Arsenal1415Data$hebrew), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "bokmal", sum(Arsenal1415Data$bokmal), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "ukrainian", sum(Arsenal1415Data$ukrainian), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "russian", sum(Arsenal1415Data$russian), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "bosnian", sum(Arsenal1415Data$bosnian), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "czech", sum(Arsenal1415Data$czech), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "danish", sum(Arsenal1415Data$danish), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "swedish", sum(Arsenal1415Data$swedish), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "japanese", sum(Arsenal1415Data$japanese), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "turkish", sum(Arsenal1415Data$turkish), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "asante", sum(Arsenal1415Data$asante), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "persian", sum(Arsenal1415Data$persian), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "maltese", sum(Arsenal1415Data$maltese), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "romanian", sum(Arsenal1415Data$romanian), Arsenal1415Data$PropLanguages)
Arsenal1415Data$PropLanguages <- ifelse(Arsenal1415Data$Languages == "slovak", sum(Arsenal1415Data$slovak), Arsenal1415Data$PropLanguages)

Chelsea1415Data <- subset(PL1415SZN, Squad =="Chelsea")
Chelsea1415Data$english <- ifelse(Chelsea1415Data$Languages=="english", 1, 0)
Chelsea1415Data$french <- ifelse(Chelsea1415Data$Languages=="french", 1, 0)
Chelsea1415Data$dutch <- ifelse(Chelsea1415Data$Languages=="dutch", 1, 0)
Chelsea1415Data$portuguese <- ifelse(Chelsea1415Data$Languages=="portuguese", 1, 0)
Chelsea1415Data$castilian <- ifelse(Chelsea1415Data$Languages=="castilian", 1, 0)
Chelsea1415Data$spanish <- ifelse(Chelsea1415Data$Languages=="spanish", 1, 0)
Chelsea1415Data$german <- ifelse(Chelsea1415Data$Languages=="german", 1, 0)
Chelsea1415Data$arabic <- ifelse(Chelsea1415Data$Languages=="arabic", 1, 0)
Chelsea1415Data$serbian <- ifelse(Chelsea1415Data$Languages=="serbian", 1, 0)
Chelsea1415Data$korean <- ifelse(Chelsea1415Data$Languages=="korean", 1, 0)
Chelsea1415Data$croatian <- ifelse(Chelsea1415Data$Languages=="croatian", 1, 0)
Chelsea1415Data$italian <- ifelse(Chelsea1415Data$Languages=="italian", 1, 0)
Chelsea1415Data$armenian <- ifelse(Chelsea1415Data$Languages=="armenian", 1, 0)
Chelsea1415Data$icelandic <- ifelse(Chelsea1415Data$Languages=="icelandic", 1, 0)
Chelsea1415Data$hebrew <- ifelse(Chelsea1415Data$Languages=="hebrew", 1, 0)
Chelsea1415Data$bokmal <- ifelse(Chelsea1415Data$Languages=="bokmal", 1, 0)
Chelsea1415Data$ukrainian <- ifelse(Chelsea1415Data$Languages=="ukrainian", 1, 0)
Chelsea1415Data$russian <- ifelse(Chelsea1415Data$Languages=="russian", 1, 0)
Chelsea1415Data$bosnian <- ifelse(Chelsea1415Data$Languages=="bosnian", 1, 0)
Chelsea1415Data$czech <- ifelse(Chelsea1415Data$Languages=="czech", 1, 0)
Chelsea1415Data$danish <- ifelse(Chelsea1415Data$Languages=="danish", 1, 0)
Chelsea1415Data$swedish <- ifelse(Chelsea1415Data$Languages=="swedish", 1, 0)
Chelsea1415Data$japanese <- ifelse(Chelsea1415Data$Languages=="japanese", 1, 0)
Chelsea1415Data$turkish <- ifelse(Chelsea1415Data$Languages=="turkish", 1, 0)
Chelsea1415Data$asante <- ifelse(Chelsea1415Data$Languages=="asante", 1, 0)
Chelsea1415Data$persian <- ifelse(Chelsea1415Data$Languages=="persian", 1, 0)
Chelsea1415Data$maltese <- ifelse(Chelsea1415Data$Languages=="maltese", 1, 0)
Chelsea1415Data$romanian <- ifelse(Chelsea1415Data$Languages=="romanian", 1, 0)
Chelsea1415Data$slovak <- ifelse(Chelsea1415Data$Languages=="slovak", 1, 0)
Chelsea1415Data$PropLanguages <- 0
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "english", sum(Chelsea1415Data$english), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "french", sum(Chelsea1415Data$french), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "dutch", sum(Chelsea1415Data$dutch), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "portuguese", sum(Chelsea1415Data$portuguese), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "castilian", sum(Chelsea1415Data$castilian), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "spanish", sum(Chelsea1415Data$spanish), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "german", sum(Chelsea1415Data$german), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "arabic", sum(Chelsea1415Data$arabic), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "serbian", sum(Chelsea1415Data$serbian), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "korean", sum(Chelsea1415Data$korean), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "croatian", sum(Chelsea1415Data$croatian), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "italian", sum(Chelsea1415Data$italian), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "armenian", sum(Chelsea1415Data$armenian), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "icelandic", sum(Chelsea1415Data$icelandic), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "hebrew", sum(Chelsea1415Data$hebrew), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "bokmal", sum(Chelsea1415Data$bokmal), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "ukrainian", sum(Chelsea1415Data$ukrainian), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "russian", sum(Chelsea1415Data$russian), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "bosnian", sum(Chelsea1415Data$bosnian), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "czech", sum(Chelsea1415Data$czech), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "danish", sum(Chelsea1415Data$danish), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "swedish", sum(Chelsea1415Data$swedish), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "japanese", sum(Chelsea1415Data$japanese), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "turkish", sum(Chelsea1415Data$turkish), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "asante", sum(Chelsea1415Data$asante), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "persian", sum(Chelsea1415Data$persian), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "maltese", sum(Chelsea1415Data$maltese), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "romanian", sum(Chelsea1415Data$romanian), Chelsea1415Data$PropLanguages)
Chelsea1415Data$PropLanguages <- ifelse(Chelsea1415Data$Languages == "slovak", sum(Chelsea1415Data$slovak), Chelsea1415Data$PropLanguages)

Everton1415Data <- subset(PL1415SZN, Squad =="Everton")
Everton1415Data$english <- ifelse(Everton1415Data$Languages=="english", 1, 0)
Everton1415Data$french <- ifelse(Everton1415Data$Languages=="french", 1, 0)
Everton1415Data$dutch <- ifelse(Everton1415Data$Languages=="dutch", 1, 0)
Everton1415Data$portuguese <- ifelse(Everton1415Data$Languages=="portuguese", 1, 0)
Everton1415Data$castilian <- ifelse(Everton1415Data$Languages=="castilian", 1, 0)
Everton1415Data$spanish <- ifelse(Everton1415Data$Languages=="spanish", 1, 0)
Everton1415Data$german <- ifelse(Everton1415Data$Languages=="german", 1, 0)
Everton1415Data$arabic <- ifelse(Everton1415Data$Languages=="arabic", 1, 0)
Everton1415Data$serbian <- ifelse(Everton1415Data$Languages=="serbian", 1, 0)
Everton1415Data$korean <- ifelse(Everton1415Data$Languages=="korean", 1, 0)
Everton1415Data$croatian <- ifelse(Everton1415Data$Languages=="croatian", 1, 0)
Everton1415Data$italian <- ifelse(Everton1415Data$Languages=="italian", 1, 0)
Everton1415Data$armenian <- ifelse(Everton1415Data$Languages=="armenian", 1, 0)
Everton1415Data$icelandic <- ifelse(Everton1415Data$Languages=="icelandic", 1, 0)
Everton1415Data$hebrew <- ifelse(Everton1415Data$Languages=="hebrew", 1, 0)
Everton1415Data$bokmal <- ifelse(Everton1415Data$Languages=="bokmal", 1, 0)
Everton1415Data$ukrainian <- ifelse(Everton1415Data$Languages=="ukrainian", 1, 0)
Everton1415Data$russian <- ifelse(Everton1415Data$Languages=="russian", 1, 0)
Everton1415Data$bosnian <- ifelse(Everton1415Data$Languages=="bosnian", 1, 0)
Everton1415Data$czech <- ifelse(Everton1415Data$Languages=="czech", 1, 0)
Everton1415Data$danish <- ifelse(Everton1415Data$Languages=="danish", 1, 0)
Everton1415Data$swedish <- ifelse(Everton1415Data$Languages=="swedish", 1, 0)
Everton1415Data$japanese <- ifelse(Everton1415Data$Languages=="japanese", 1, 0)
Everton1415Data$turkish <- ifelse(Everton1415Data$Languages=="turkish", 1, 0)
Everton1415Data$asante <- ifelse(Everton1415Data$Languages=="asante", 1, 0)
Everton1415Data$persian <- ifelse(Everton1415Data$Languages=="persian", 1, 0)
Everton1415Data$maltese <- ifelse(Everton1415Data$Languages=="maltese", 1, 0)
Everton1415Data$romanian <- ifelse(Everton1415Data$Languages=="romanian", 1, 0)
Everton1415Data$slovak <- ifelse(Everton1415Data$Languages=="slovak", 1, 0)
Everton1415Data$PropLanguages <- 0
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "english", sum(Everton1415Data$english), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "french", sum(Everton1415Data$french), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "dutch", sum(Everton1415Data$dutch), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "portuguese", sum(Everton1415Data$portuguese), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "castilian", sum(Everton1415Data$castilian), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "spanish", sum(Everton1415Data$spanish), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "german", sum(Everton1415Data$german), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "arabic", sum(Everton1415Data$arabic), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "serbian", sum(Everton1415Data$serbian), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "korean", sum(Everton1415Data$korean), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "croatian", sum(Everton1415Data$croatian), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "italian", sum(Everton1415Data$italian), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "armenian", sum(Everton1415Data$armenian), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "icelandic", sum(Everton1415Data$icelandic), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "hebrew", sum(Everton1415Data$hebrew), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "bokmal", sum(Everton1415Data$bokmal), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "ukrainian", sum(Everton1415Data$ukrainian), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "russian", sum(Everton1415Data$russian), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "bosnian", sum(Everton1415Data$bosnian), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "czech", sum(Everton1415Data$czech), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "danish", sum(Everton1415Data$danish), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "swedish", sum(Everton1415Data$swedish), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "japanese", sum(Everton1415Data$japanese), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "turkish", sum(Everton1415Data$turkish), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "asante", sum(Everton1415Data$asante), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "persian", sum(Everton1415Data$persian), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "maltese", sum(Everton1415Data$maltese), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "romanian", sum(Everton1415Data$romanian), Everton1415Data$PropLanguages)
Everton1415Data$PropLanguages <- ifelse(Everton1415Data$Languages == "slovak", sum(Everton1415Data$slovak), Everton1415Data$PropLanguages)

Liverpool1415Data <- subset(PL1415SZN, Squad =="Liverpool")
Liverpool1415Data$english <- ifelse(Liverpool1415Data$Languages=="english", 1, 0)
Liverpool1415Data$french <- ifelse(Liverpool1415Data$Languages=="french", 1, 0)
Liverpool1415Data$dutch <- ifelse(Liverpool1415Data$Languages=="dutch", 1, 0)
Liverpool1415Data$portuguese <- ifelse(Liverpool1415Data$Languages=="portuguese", 1, 0)
Liverpool1415Data$castilian <- ifelse(Liverpool1415Data$Languages=="castilian", 1, 0)
Liverpool1415Data$spanish <- ifelse(Liverpool1415Data$Languages=="spanish", 1, 0)
Liverpool1415Data$german <- ifelse(Liverpool1415Data$Languages=="german", 1, 0)
Liverpool1415Data$arabic <- ifelse(Liverpool1415Data$Languages=="arabic", 1, 0)
Liverpool1415Data$serbian <- ifelse(Liverpool1415Data$Languages=="serbian", 1, 0)
Liverpool1415Data$korean <- ifelse(Liverpool1415Data$Languages=="korean", 1, 0)
Liverpool1415Data$croatian <- ifelse(Liverpool1415Data$Languages=="croatian", 1, 0)
Liverpool1415Data$italian <- ifelse(Liverpool1415Data$Languages=="italian", 1, 0)
Liverpool1415Data$armenian <- ifelse(Liverpool1415Data$Languages=="armenian", 1, 0)
Liverpool1415Data$icelandic <- ifelse(Liverpool1415Data$Languages=="icelandic", 1, 0)
Liverpool1415Data$hebrew <- ifelse(Liverpool1415Data$Languages=="hebrew", 1, 0)
Liverpool1415Data$bokmal <- ifelse(Liverpool1415Data$Languages=="bokmal", 1, 0)
Liverpool1415Data$ukrainian <- ifelse(Liverpool1415Data$Languages=="ukrainian", 1, 0)
Liverpool1415Data$russian <- ifelse(Liverpool1415Data$Languages=="russian", 1, 0)
Liverpool1415Data$bosnian <- ifelse(Liverpool1415Data$Languages=="bosnian", 1, 0)
Liverpool1415Data$czech <- ifelse(Liverpool1415Data$Languages=="czech", 1, 0)
Liverpool1415Data$danish <- ifelse(Liverpool1415Data$Languages=="danish", 1, 0)
Liverpool1415Data$swedish <- ifelse(Liverpool1415Data$Languages=="swedish", 1, 0)
Liverpool1415Data$japanese <- ifelse(Liverpool1415Data$Languages=="japanese", 1, 0)
Liverpool1415Data$turkish <- ifelse(Liverpool1415Data$Languages=="turkish", 1, 0)
Liverpool1415Data$asante <- ifelse(Liverpool1415Data$Languages=="asante", 1, 0)
Liverpool1415Data$persian <- ifelse(Liverpool1415Data$Languages=="persian", 1, 0)
Liverpool1415Data$maltese <- ifelse(Liverpool1415Data$Languages=="maltese", 1, 0)
Liverpool1415Data$romanian <- ifelse(Liverpool1415Data$Languages=="romanian", 1, 0)
Liverpool1415Data$slovak <- ifelse(Liverpool1415Data$Languages=="slovak", 1, 0)
Liverpool1415Data$PropLanguages <- 0
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "english", sum(Liverpool1415Data$english), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "french", sum(Liverpool1415Data$french), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "dutch", sum(Liverpool1415Data$dutch), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "portuguese", sum(Liverpool1415Data$portuguese), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "castilian", sum(Liverpool1415Data$castilian), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "spanish", sum(Liverpool1415Data$spanish), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "german", sum(Liverpool1415Data$german), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "arabic", sum(Liverpool1415Data$arabic), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "serbian", sum(Liverpool1415Data$serbian), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "korean", sum(Liverpool1415Data$korean), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "croatian", sum(Liverpool1415Data$croatian), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "italian", sum(Liverpool1415Data$italian), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "armenian", sum(Liverpool1415Data$armenian), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "icelandic", sum(Liverpool1415Data$icelandic), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "hebrew", sum(Liverpool1415Data$hebrew), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "bokmal", sum(Liverpool1415Data$bokmal), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "ukrainian", sum(Liverpool1415Data$ukrainian), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "russian", sum(Liverpool1415Data$russian), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "bosnian", sum(Liverpool1415Data$bosnian), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "czech", sum(Liverpool1415Data$czech), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "danish", sum(Liverpool1415Data$danish), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "swedish", sum(Liverpool1415Data$swedish), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "japanese", sum(Liverpool1415Data$japanese), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "turkish", sum(Liverpool1415Data$turkish), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "asante", sum(Liverpool1415Data$asante), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "persian", sum(Liverpool1415Data$persian), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "maltese", sum(Liverpool1415Data$maltese), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "romanian", sum(Liverpool1415Data$romanian), Liverpool1415Data$PropLanguages)
Liverpool1415Data$PropLanguages <- ifelse(Liverpool1415Data$Languages == "slovak", sum(Liverpool1415Data$slovak), Liverpool1415Data$PropLanguages)

ManchesterCity1415Data <- subset(PL1415SZN, Squad =="Manchester City")
ManchesterCity1415Data$english <- ifelse(ManchesterCity1415Data$Languages=="english", 1, 0)
ManchesterCity1415Data$french <- ifelse(ManchesterCity1415Data$Languages=="french", 1, 0)
ManchesterCity1415Data$dutch <- ifelse(ManchesterCity1415Data$Languages=="dutch", 1, 0)
ManchesterCity1415Data$portuguese <- ifelse(ManchesterCity1415Data$Languages=="portuguese", 1, 0)
ManchesterCity1415Data$castilian <- ifelse(ManchesterCity1415Data$Languages=="castilian", 1, 0)
ManchesterCity1415Data$spanish <- ifelse(ManchesterCity1415Data$Languages=="spanish", 1, 0)
ManchesterCity1415Data$german <- ifelse(ManchesterCity1415Data$Languages=="german", 1, 0)
ManchesterCity1415Data$arabic <- ifelse(ManchesterCity1415Data$Languages=="arabic", 1, 0)
ManchesterCity1415Data$serbian <- ifelse(ManchesterCity1415Data$Languages=="serbian", 1, 0)
ManchesterCity1415Data$korean <- ifelse(ManchesterCity1415Data$Languages=="korean", 1, 0)
ManchesterCity1415Data$croatian <- ifelse(ManchesterCity1415Data$Languages=="croatian", 1, 0)
ManchesterCity1415Data$italian <- ifelse(ManchesterCity1415Data$Languages=="italian", 1, 0)
ManchesterCity1415Data$armenian <- ifelse(ManchesterCity1415Data$Languages=="armenian", 1, 0)
ManchesterCity1415Data$icelandic <- ifelse(ManchesterCity1415Data$Languages=="icelandic", 1, 0)
ManchesterCity1415Data$hebrew <- ifelse(ManchesterCity1415Data$Languages=="hebrew", 1, 0)
ManchesterCity1415Data$bokmal <- ifelse(ManchesterCity1415Data$Languages=="bokmal", 1, 0)
ManchesterCity1415Data$ukrainian <- ifelse(ManchesterCity1415Data$Languages=="ukrainian", 1, 0)
ManchesterCity1415Data$russian <- ifelse(ManchesterCity1415Data$Languages=="russian", 1, 0)
ManchesterCity1415Data$bosnian <- ifelse(ManchesterCity1415Data$Languages=="bosnian", 1, 0)
ManchesterCity1415Data$czech <- ifelse(ManchesterCity1415Data$Languages=="czech", 1, 0)
ManchesterCity1415Data$danish <- ifelse(ManchesterCity1415Data$Languages=="danish", 1, 0)
ManchesterCity1415Data$swedish <- ifelse(ManchesterCity1415Data$Languages=="swedish", 1, 0)
ManchesterCity1415Data$japanese <- ifelse(ManchesterCity1415Data$Languages=="japanese", 1, 0)
ManchesterCity1415Data$turkish <- ifelse(ManchesterCity1415Data$Languages=="turkish", 1, 0)
ManchesterCity1415Data$asante <- ifelse(ManchesterCity1415Data$Languages=="asante", 1, 0)
ManchesterCity1415Data$persian <- ifelse(ManchesterCity1415Data$Languages=="persian", 1, 0)
ManchesterCity1415Data$maltese <- ifelse(ManchesterCity1415Data$Languages=="maltese", 1, 0)
ManchesterCity1415Data$romanian <- ifelse(ManchesterCity1415Data$Languages=="romanian", 1, 0)
ManchesterCity1415Data$slovak <- ifelse(ManchesterCity1415Data$Languages=="slovak", 1, 0)
ManchesterCity1415Data$PropLanguages <- 0
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "english", sum(ManchesterCity1415Data$english), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "french", sum(ManchesterCity1415Data$french), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "dutch", sum(ManchesterCity1415Data$dutch), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "portuguese", sum(ManchesterCity1415Data$portuguese), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "castilian", sum(ManchesterCity1415Data$castilian), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "spanish", sum(ManchesterCity1415Data$spanish), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "german", sum(ManchesterCity1415Data$german), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "arabic", sum(ManchesterCity1415Data$arabic), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "serbian", sum(ManchesterCity1415Data$serbian), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "korean", sum(ManchesterCity1415Data$korean), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "croatian", sum(ManchesterCity1415Data$croatian), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "italian", sum(ManchesterCity1415Data$italian), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "armenian", sum(ManchesterCity1415Data$armenian), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "icelandic", sum(ManchesterCity1415Data$icelandic), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "hebrew", sum(ManchesterCity1415Data$hebrew), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "bokmal", sum(ManchesterCity1415Data$bokmal), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "ukrainian", sum(ManchesterCity1415Data$ukrainian), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "russian", sum(ManchesterCity1415Data$russian), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "bosnian", sum(ManchesterCity1415Data$bosnian), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "czech", sum(ManchesterCity1415Data$czech), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "danish", sum(ManchesterCity1415Data$danish), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "swedish", sum(ManchesterCity1415Data$swedish), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "japanese", sum(ManchesterCity1415Data$japanese), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "turkish", sum(ManchesterCity1415Data$turkish), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "asante", sum(ManchesterCity1415Data$asante), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "persian", sum(ManchesterCity1415Data$persian), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "maltese", sum(ManchesterCity1415Data$maltese), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "romanian", sum(ManchesterCity1415Data$romanian), ManchesterCity1415Data$PropLanguages)
ManchesterCity1415Data$PropLanguages <- ifelse(ManchesterCity1415Data$Languages == "slovak", sum(ManchesterCity1415Data$slovak), ManchesterCity1415Data$PropLanguages)

ManchesterUtd1415Data <- subset(PL1415SZN, Squad =="Manchester Utd")
ManchesterUtd1415Data$english <- ifelse(ManchesterUtd1415Data$Languages=="english", 1, 0)
ManchesterUtd1415Data$french <- ifelse(ManchesterUtd1415Data$Languages=="french", 1, 0)
ManchesterUtd1415Data$dutch <- ifelse(ManchesterUtd1415Data$Languages=="dutch", 1, 0)
ManchesterUtd1415Data$portuguese <- ifelse(ManchesterUtd1415Data$Languages=="portuguese", 1, 0)
ManchesterUtd1415Data$castilian <- ifelse(ManchesterUtd1415Data$Languages=="castilian", 1, 0)
ManchesterUtd1415Data$spanish <- ifelse(ManchesterUtd1415Data$Languages=="spanish", 1, 0)
ManchesterUtd1415Data$german <- ifelse(ManchesterUtd1415Data$Languages=="german", 1, 0)
ManchesterUtd1415Data$arabic <- ifelse(ManchesterUtd1415Data$Languages=="arabic", 1, 0)
ManchesterUtd1415Data$serbian <- ifelse(ManchesterUtd1415Data$Languages=="serbian", 1, 0)
ManchesterUtd1415Data$korean <- ifelse(ManchesterUtd1415Data$Languages=="korean", 1, 0)
ManchesterUtd1415Data$croatian <- ifelse(ManchesterUtd1415Data$Languages=="croatian", 1, 0)
ManchesterUtd1415Data$italian <- ifelse(ManchesterUtd1415Data$Languages=="italian", 1, 0)
ManchesterUtd1415Data$armenian <- ifelse(ManchesterUtd1415Data$Languages=="armenian", 1, 0)
ManchesterUtd1415Data$icelandic <- ifelse(ManchesterUtd1415Data$Languages=="icelandic", 1, 0)
ManchesterUtd1415Data$hebrew <- ifelse(ManchesterUtd1415Data$Languages=="hebrew", 1, 0)
ManchesterUtd1415Data$bokmal <- ifelse(ManchesterUtd1415Data$Languages=="bokmal", 1, 0)
ManchesterUtd1415Data$ukrainian <- ifelse(ManchesterUtd1415Data$Languages=="ukrainian", 1, 0)
ManchesterUtd1415Data$russian <- ifelse(ManchesterUtd1415Data$Languages=="russian", 1, 0)
ManchesterUtd1415Data$bosnian <- ifelse(ManchesterUtd1415Data$Languages=="bosnian", 1, 0)
ManchesterUtd1415Data$czech <- ifelse(ManchesterUtd1415Data$Languages=="czech", 1, 0)
ManchesterUtd1415Data$danish <- ifelse(ManchesterUtd1415Data$Languages=="danish", 1, 0)
ManchesterUtd1415Data$swedish <- ifelse(ManchesterUtd1415Data$Languages=="swedish", 1, 0)
ManchesterUtd1415Data$japanese <- ifelse(ManchesterUtd1415Data$Languages=="japanese", 1, 0)
ManchesterUtd1415Data$turkish <- ifelse(ManchesterUtd1415Data$Languages=="turkish", 1, 0)
ManchesterUtd1415Data$asante <- ifelse(ManchesterUtd1415Data$Languages=="asante", 1, 0)
ManchesterUtd1415Data$persian <- ifelse(ManchesterUtd1415Data$Languages=="persian", 1, 0)
ManchesterUtd1415Data$maltese <- ifelse(ManchesterUtd1415Data$Languages=="maltese", 1, 0)
ManchesterUtd1415Data$romanian <- ifelse(ManchesterUtd1415Data$Languages=="romanian", 1, 0)
ManchesterUtd1415Data$slovak <- ifelse(ManchesterUtd1415Data$Languages=="slovak", 1, 0)
ManchesterUtd1415Data$PropLanguages <- 0
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "english", sum(ManchesterUtd1415Data$english), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "french", sum(ManchesterUtd1415Data$french), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "dutch", sum(ManchesterUtd1415Data$dutch), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "portuguese", sum(ManchesterUtd1415Data$portuguese), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "castilian", sum(ManchesterUtd1415Data$castilian), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "spanish", sum(ManchesterUtd1415Data$spanish), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "german", sum(ManchesterUtd1415Data$german), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "arabic", sum(ManchesterUtd1415Data$arabic), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "serbian", sum(ManchesterUtd1415Data$serbian), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "korean", sum(ManchesterUtd1415Data$korean), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "croatian", sum(ManchesterUtd1415Data$croatian), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "italian", sum(ManchesterUtd1415Data$italian), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "armenian", sum(ManchesterUtd1415Data$armenian), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "icelandic", sum(ManchesterUtd1415Data$icelandic), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "hebrew", sum(ManchesterUtd1415Data$hebrew), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "bokmal", sum(ManchesterUtd1415Data$bokmal), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "ukrainian", sum(ManchesterUtd1415Data$ukrainian), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "russian", sum(ManchesterUtd1415Data$russian), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "bosnian", sum(ManchesterUtd1415Data$bosnian), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "czech", sum(ManchesterUtd1415Data$czech), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "danish", sum(ManchesterUtd1415Data$danish), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "swedish", sum(ManchesterUtd1415Data$swedish), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "japanese", sum(ManchesterUtd1415Data$japanese), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "turkish", sum(ManchesterUtd1415Data$turkish), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "asante", sum(ManchesterUtd1415Data$asante), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "persian", sum(ManchesterUtd1415Data$persian), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "maltese", sum(ManchesterUtd1415Data$maltese), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "romanian", sum(ManchesterUtd1415Data$romanian), ManchesterUtd1415Data$PropLanguages)
ManchesterUtd1415Data$PropLanguages <- ifelse(ManchesterUtd1415Data$Languages == "slovak", sum(ManchesterUtd1415Data$slovak), ManchesterUtd1415Data$PropLanguages)

Tottenham1415Data <- subset(PL1415SZN, Squad =="Tottenham")
Tottenham1415Data$english <- ifelse(Tottenham1415Data$Languages=="english", 1, 0)
Tottenham1415Data$french <- ifelse(Tottenham1415Data$Languages=="french", 1, 0)
Tottenham1415Data$dutch <- ifelse(Tottenham1415Data$Languages=="dutch", 1, 0)
Tottenham1415Data$portuguese <- ifelse(Tottenham1415Data$Languages=="portuguese", 1, 0)
Tottenham1415Data$castilian <- ifelse(Tottenham1415Data$Languages=="castilian", 1, 0)
Tottenham1415Data$spanish <- ifelse(Tottenham1415Data$Languages=="spanish", 1, 0)
Tottenham1415Data$german <- ifelse(Tottenham1415Data$Languages=="german", 1, 0)
Tottenham1415Data$arabic <- ifelse(Tottenham1415Data$Languages=="arabic", 1, 0)
Tottenham1415Data$serbian <- ifelse(Tottenham1415Data$Languages=="serbian", 1, 0)
Tottenham1415Data$korean <- ifelse(Tottenham1415Data$Languages=="korean", 1, 0)
Tottenham1415Data$croatian <- ifelse(Tottenham1415Data$Languages=="croatian", 1, 0)
Tottenham1415Data$italian <- ifelse(Tottenham1415Data$Languages=="italian", 1, 0)
Tottenham1415Data$armenian <- ifelse(Tottenham1415Data$Languages=="armenian", 1, 0)
Tottenham1415Data$icelandic <- ifelse(Tottenham1415Data$Languages=="icelandic", 1, 0)
Tottenham1415Data$hebrew <- ifelse(Tottenham1415Data$Languages=="hebrew", 1, 0)
Tottenham1415Data$bokmal <- ifelse(Tottenham1415Data$Languages=="bokmal", 1, 0)
Tottenham1415Data$ukrainian <- ifelse(Tottenham1415Data$Languages=="ukrainian", 1, 0)
Tottenham1415Data$russian <- ifelse(Tottenham1415Data$Languages=="russian", 1, 0)
Tottenham1415Data$bosnian <- ifelse(Tottenham1415Data$Languages=="bosnian", 1, 0)
Tottenham1415Data$czech <- ifelse(Tottenham1415Data$Languages=="czech", 1, 0)
Tottenham1415Data$danish <- ifelse(Tottenham1415Data$Languages=="danish", 1, 0)
Tottenham1415Data$swedish <- ifelse(Tottenham1415Data$Languages=="swedish", 1, 0)
Tottenham1415Data$japanese <- ifelse(Tottenham1415Data$Languages=="japanese", 1, 0)
Tottenham1415Data$turkish <- ifelse(Tottenham1415Data$Languages=="turkish", 1, 0)
Tottenham1415Data$asante <- ifelse(Tottenham1415Data$Languages=="asante", 1, 0)
Tottenham1415Data$persian <- ifelse(Tottenham1415Data$Languages=="persian", 1, 0)
Tottenham1415Data$maltese <- ifelse(Tottenham1415Data$Languages=="maltese", 1, 0)
Tottenham1415Data$romanian <- ifelse(Tottenham1415Data$Languages=="romanian", 1, 0)
Tottenham1415Data$slovak <- ifelse(Tottenham1415Data$Languages=="slovak", 1, 0)
Tottenham1415Data$PropLanguages <- 0
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "english", sum(Tottenham1415Data$english), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "french", sum(Tottenham1415Data$french), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "dutch", sum(Tottenham1415Data$dutch), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "portuguese", sum(Tottenham1415Data$portuguese), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "castilian", sum(Tottenham1415Data$castilian), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "spanish", sum(Tottenham1415Data$spanish), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "german", sum(Tottenham1415Data$german), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "arabic", sum(Tottenham1415Data$arabic), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "serbian", sum(Tottenham1415Data$serbian), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "korean", sum(Tottenham1415Data$korean), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "croatian", sum(Tottenham1415Data$croatian), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "italian", sum(Tottenham1415Data$italian), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "armenian", sum(Tottenham1415Data$armenian), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "icelandic", sum(Tottenham1415Data$icelandic), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "hebrew", sum(Tottenham1415Data$hebrew), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "bokmal", sum(Tottenham1415Data$bokmal), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "ukrainian", sum(Tottenham1415Data$ukrainian), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "russian", sum(Tottenham1415Data$russian), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "bosnian", sum(Tottenham1415Data$bosnian), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "czech", sum(Tottenham1415Data$czech), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "danish", sum(Tottenham1415Data$danish), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "swedish", sum(Tottenham1415Data$swedish), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "japanese", sum(Tottenham1415Data$japanese), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "turkish", sum(Tottenham1415Data$turkish), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "asante", sum(Tottenham1415Data$asante), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "persian", sum(Tottenham1415Data$persian), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "maltese", sum(Tottenham1415Data$maltese), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "romanian", sum(Tottenham1415Data$romanian), Tottenham1415Data$PropLanguages)
Tottenham1415Data$PropLanguages <- ifelse(Tottenham1415Data$Languages == "slovak", sum(Tottenham1415Data$slovak), Tottenham1415Data$PropLanguages)

PL1415SZN <- rbind(Arsenal1415Data, Chelsea1415Data)
PL1415SZN <- rbind(PL1415SZN, Everton1415Data)
PL1415SZN <- rbind(PL1415SZN, Liverpool1415Data)
PL1415SZN <- rbind(PL1415SZN, ManchesterCity1415Data)
PL1415SZN <- rbind(PL1415SZN, ManchesterUtd1415Data)
PL1415SZN <- rbind(PL1415SZN, Tottenham1415Data)

PL1314SZN$english <- 0
PL1314SZN$french <- 0
PL1314SZN$portuguese <- 0
PL1314SZN$castilian <- 0
PL1314SZN$dutch <- 0
PL1314SZN$german <- 0
PL1314SZN$spanish <- 0
PL1314SZN$arabic <- 0
PL1314SZN$serbian <- 0
PL1314SZN$korean <- 0
PL1314SZN$croatian <- 0
PL1314SZN$italian <- 0
PL1314SZN$icelandic <- 0
PL1314SZN$hebrew <- 0
PL1314SZN$bokmal <- 0
PL1314SZN$ukrainian <- 0
PL1314SZN$russian <- 0
PL1314SZN$bosnian <- 0
PL1314SZN$czech <- 0
PL1314SZN$danish <- 0
PL1314SZN$armenian <- 0
PL1314SZN$turkish <- 0
PL1314SZN$asante <- 0
PL1314SZN$persian <- 0
PL1314SZN$japanese <- 0
PL1314SZN$maltese <- 0
PL1314SZN$romanian <- 0
PL1314SZN$slovak <- 0
PL1314SZN$swedish <- 0

Arsenal1314Data <- subset(PL1314SZN, Squad =="Arsenal")
Arsenal1314Data$english <- ifelse(Arsenal1314Data$Languages=="english", 1, 0)
Arsenal1314Data$french <- ifelse(Arsenal1314Data$Languages=="french", 1, 0)
Arsenal1314Data$dutch <- ifelse(Arsenal1314Data$Languages=="dutch", 1, 0)
Arsenal1314Data$portuguese <- ifelse(Arsenal1314Data$Languages=="portuguese", 1, 0)
Arsenal1314Data$castilian <- ifelse(Arsenal1314Data$Languages=="castilian", 1, 0)
Arsenal1314Data$spanish <- ifelse(Arsenal1314Data$Languages=="spanish", 1, 0)
Arsenal1314Data$german <- ifelse(Arsenal1314Data$Languages=="german", 1, 0)
Arsenal1314Data$arabic <- ifelse(Arsenal1314Data$Languages=="arabic", 1, 0)
Arsenal1314Data$serbian <- ifelse(Arsenal1314Data$Languages=="serbian", 1, 0)
Arsenal1314Data$korean <- ifelse(Arsenal1314Data$Languages=="korean", 1, 0)
Arsenal1314Data$croatian <- ifelse(Arsenal1314Data$Languages=="croatian", 1, 0)
Arsenal1314Data$italian <- ifelse(Arsenal1314Data$Languages=="italian", 1, 0)
Arsenal1314Data$armenian <- ifelse(Arsenal1314Data$Languages=="armenian", 1, 0)
Arsenal1314Data$icelandic <- ifelse(Arsenal1314Data$Languages=="icelandic", 1, 0)
Arsenal1314Data$hebrew <- ifelse(Arsenal1314Data$Languages=="hebrew", 1, 0)
Arsenal1314Data$bokmal <- ifelse(Arsenal1314Data$Languages=="bokmal", 1, 0)
Arsenal1314Data$ukrainian <- ifelse(Arsenal1314Data$Languages=="ukrainian", 1, 0)
Arsenal1314Data$russian <- ifelse(Arsenal1314Data$Languages=="russian", 1, 0)
Arsenal1314Data$bosnian <- ifelse(Arsenal1314Data$Languages=="bosnian", 1, 0)
Arsenal1314Data$czech <- ifelse(Arsenal1314Data$Languages=="czech", 1, 0)
Arsenal1314Data$danish <- ifelse(Arsenal1314Data$Languages=="danish", 1, 0)
Arsenal1314Data$swedish <- ifelse(Arsenal1314Data$Languages=="swedish", 1, 0)
Arsenal1314Data$japanese <- ifelse(Arsenal1314Data$Languages=="japanese", 1, 0)
Arsenal1314Data$turkish <- ifelse(Arsenal1314Data$Languages=="turkish", 1, 0)
Arsenal1314Data$asante <- ifelse(Arsenal1314Data$Languages=="asante", 1, 0)
Arsenal1314Data$persian <- ifelse(Arsenal1314Data$Languages=="persian", 1, 0)
Arsenal1314Data$maltese <- ifelse(Arsenal1314Data$Languages=="maltese", 1, 0)
Arsenal1314Data$romanian <- ifelse(Arsenal1314Data$Languages=="romanian", 1, 0)
Arsenal1314Data$slovak <- ifelse(Arsenal1314Data$Languages=="slovak", 1, 0)
Arsenal1314Data$PropLanguages <- 0
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "english", sum(Arsenal1314Data$english), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "french", sum(Arsenal1314Data$french), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "dutch", sum(Arsenal1314Data$dutch), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "portuguese", sum(Arsenal1314Data$portuguese), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "castilian", sum(Arsenal1314Data$castilian), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "spanish", sum(Arsenal1314Data$spanish), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "german", sum(Arsenal1314Data$german), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "arabic", sum(Arsenal1314Data$arabic), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "serbian", sum(Arsenal1314Data$serbian), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "korean", sum(Arsenal1314Data$korean), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "croatian", sum(Arsenal1314Data$croatian), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "italian", sum(Arsenal1314Data$italian), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "armenian", sum(Arsenal1314Data$armenian), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "icelandic", sum(Arsenal1314Data$icelandic), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "hebrew", sum(Arsenal1314Data$hebrew), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "bokmal", sum(Arsenal1314Data$bokmal), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "ukrainian", sum(Arsenal1314Data$ukrainian), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "russian", sum(Arsenal1314Data$russian), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "bosnian", sum(Arsenal1314Data$bosnian), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "czech", sum(Arsenal1314Data$czech), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "danish", sum(Arsenal1314Data$danish), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "swedish", sum(Arsenal1314Data$swedish), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "japanese", sum(Arsenal1314Data$japanese), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "turkish", sum(Arsenal1314Data$turkish), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "asante", sum(Arsenal1314Data$asante), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "persian", sum(Arsenal1314Data$persian), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "maltese", sum(Arsenal1314Data$maltese), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "romanian", sum(Arsenal1314Data$romanian), Arsenal1314Data$PropLanguages)
Arsenal1314Data$PropLanguages <- ifelse(Arsenal1314Data$Languages == "slovak", sum(Arsenal1314Data$slovak), Arsenal1314Data$PropLanguages)

Chelsea1314Data <- subset(PL1314SZN, Squad =="Chelsea")
Chelsea1314Data$english <- ifelse(Chelsea1314Data$Languages=="english", 1, 0)
Chelsea1314Data$french <- ifelse(Chelsea1314Data$Languages=="french", 1, 0)
Chelsea1314Data$dutch <- ifelse(Chelsea1314Data$Languages=="dutch", 1, 0)
Chelsea1314Data$portuguese <- ifelse(Chelsea1314Data$Languages=="portuguese", 1, 0)
Chelsea1314Data$castilian <- ifelse(Chelsea1314Data$Languages=="castilian", 1, 0)
Chelsea1314Data$spanish <- ifelse(Chelsea1314Data$Languages=="spanish", 1, 0)
Chelsea1314Data$german <- ifelse(Chelsea1314Data$Languages=="german", 1, 0)
Chelsea1314Data$arabic <- ifelse(Chelsea1314Data$Languages=="arabic", 1, 0)
Chelsea1314Data$serbian <- ifelse(Chelsea1314Data$Languages=="serbian", 1, 0)
Chelsea1314Data$korean <- ifelse(Chelsea1314Data$Languages=="korean", 1, 0)
Chelsea1314Data$croatian <- ifelse(Chelsea1314Data$Languages=="croatian", 1, 0)
Chelsea1314Data$italian <- ifelse(Chelsea1314Data$Languages=="italian", 1, 0)
Chelsea1314Data$armenian <- ifelse(Chelsea1314Data$Languages=="armenian", 1, 0)
Chelsea1314Data$icelandic <- ifelse(Chelsea1314Data$Languages=="icelandic", 1, 0)
Chelsea1314Data$hebrew <- ifelse(Chelsea1314Data$Languages=="hebrew", 1, 0)
Chelsea1314Data$bokmal <- ifelse(Chelsea1314Data$Languages=="bokmal", 1, 0)
Chelsea1314Data$ukrainian <- ifelse(Chelsea1314Data$Languages=="ukrainian", 1, 0)
Chelsea1314Data$russian <- ifelse(Chelsea1314Data$Languages=="russian", 1, 0)
Chelsea1314Data$bosnian <- ifelse(Chelsea1314Data$Languages=="bosnian", 1, 0)
Chelsea1314Data$czech <- ifelse(Chelsea1314Data$Languages=="czech", 1, 0)
Chelsea1314Data$danish <- ifelse(Chelsea1314Data$Languages=="danish", 1, 0)
Chelsea1314Data$swedish <- ifelse(Chelsea1314Data$Languages=="swedish", 1, 0)
Chelsea1314Data$japanese <- ifelse(Chelsea1314Data$Languages=="japanese", 1, 0)
Chelsea1314Data$turkish <- ifelse(Chelsea1314Data$Languages=="turkish", 1, 0)
Chelsea1314Data$asante <- ifelse(Chelsea1314Data$Languages=="asante", 1, 0)
Chelsea1314Data$persian <- ifelse(Chelsea1314Data$Languages=="persian", 1, 0)
Chelsea1314Data$maltese <- ifelse(Chelsea1314Data$Languages=="maltese", 1, 0)
Chelsea1314Data$romanian <- ifelse(Chelsea1314Data$Languages=="romanian", 1, 0)
Chelsea1314Data$slovak <- ifelse(Chelsea1314Data$Languages=="slovak", 1, 0)
Chelsea1314Data$PropLanguages <- 0
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "english", sum(Chelsea1314Data$english), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "french", sum(Chelsea1314Data$french), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "dutch", sum(Chelsea1314Data$dutch), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "portuguese", sum(Chelsea1314Data$portuguese), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "castilian", sum(Chelsea1314Data$castilian), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "spanish", sum(Chelsea1314Data$spanish), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "german", sum(Chelsea1314Data$german), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "arabic", sum(Chelsea1314Data$arabic), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "serbian", sum(Chelsea1314Data$serbian), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "korean", sum(Chelsea1314Data$korean), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "croatian", sum(Chelsea1314Data$croatian), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "italian", sum(Chelsea1314Data$italian), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "armenian", sum(Chelsea1314Data$armenian), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "icelandic", sum(Chelsea1314Data$icelandic), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "hebrew", sum(Chelsea1314Data$hebrew), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "bokmal", sum(Chelsea1314Data$bokmal), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "ukrainian", sum(Chelsea1314Data$ukrainian), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "russian", sum(Chelsea1314Data$russian), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "bosnian", sum(Chelsea1314Data$bosnian), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "czech", sum(Chelsea1314Data$czech), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "danish", sum(Chelsea1314Data$danish), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "swedish", sum(Chelsea1314Data$swedish), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "japanese", sum(Chelsea1314Data$japanese), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "turkish", sum(Chelsea1314Data$turkish), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "asante", sum(Chelsea1314Data$asante), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "persian", sum(Chelsea1314Data$persian), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "maltese", sum(Chelsea1314Data$maltese), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "romanian", sum(Chelsea1314Data$romanian), Chelsea1314Data$PropLanguages)
Chelsea1314Data$PropLanguages <- ifelse(Chelsea1314Data$Languages == "slovak", sum(Chelsea1314Data$slovak), Chelsea1314Data$PropLanguages)

Everton1314Data <- subset(PL1314SZN, Squad =="Everton")
Everton1314Data$english <- ifelse(Everton1314Data$Languages=="english", 1, 0)
Everton1314Data$french <- ifelse(Everton1314Data$Languages=="french", 1, 0)
Everton1314Data$dutch <- ifelse(Everton1314Data$Languages=="dutch", 1, 0)
Everton1314Data$portuguese <- ifelse(Everton1314Data$Languages=="portuguese", 1, 0)
Everton1314Data$castilian <- ifelse(Everton1314Data$Languages=="castilian", 1, 0)
Everton1314Data$spanish <- ifelse(Everton1314Data$Languages=="spanish", 1, 0)
Everton1314Data$german <- ifelse(Everton1314Data$Languages=="german", 1, 0)
Everton1314Data$arabic <- ifelse(Everton1314Data$Languages=="arabic", 1, 0)
Everton1314Data$serbian <- ifelse(Everton1314Data$Languages=="serbian", 1, 0)
Everton1314Data$korean <- ifelse(Everton1314Data$Languages=="korean", 1, 0)
Everton1314Data$croatian <- ifelse(Everton1314Data$Languages=="croatian", 1, 0)
Everton1314Data$italian <- ifelse(Everton1314Data$Languages=="italian", 1, 0)
Everton1314Data$armenian <- ifelse(Everton1314Data$Languages=="armenian", 1, 0)
Everton1314Data$icelandic <- ifelse(Everton1314Data$Languages=="icelandic", 1, 0)
Everton1314Data$hebrew <- ifelse(Everton1314Data$Languages=="hebrew", 1, 0)
Everton1314Data$bokmal <- ifelse(Everton1314Data$Languages=="bokmal", 1, 0)
Everton1314Data$ukrainian <- ifelse(Everton1314Data$Languages=="ukrainian", 1, 0)
Everton1314Data$russian <- ifelse(Everton1314Data$Languages=="russian", 1, 0)
Everton1314Data$bosnian <- ifelse(Everton1314Data$Languages=="bosnian", 1, 0)
Everton1314Data$czech <- ifelse(Everton1314Data$Languages=="czech", 1, 0)
Everton1314Data$danish <- ifelse(Everton1314Data$Languages=="danish", 1, 0)
Everton1314Data$swedish <- ifelse(Everton1314Data$Languages=="swedish", 1, 0)
Everton1314Data$japanese <- ifelse(Everton1314Data$Languages=="japanese", 1, 0)
Everton1314Data$turkish <- ifelse(Everton1314Data$Languages=="turkish", 1, 0)
Everton1314Data$asante <- ifelse(Everton1314Data$Languages=="asante", 1, 0)
Everton1314Data$persian <- ifelse(Everton1314Data$Languages=="persian", 1, 0)
Everton1314Data$maltese <- ifelse(Everton1314Data$Languages=="maltese", 1, 0)
Everton1314Data$romanian <- ifelse(Everton1314Data$Languages=="romanian", 1, 0)
Everton1314Data$slovak <- ifelse(Everton1314Data$Languages=="slovak", 1, 0)
Everton1314Data$PropLanguages <- 0
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "english", sum(Everton1314Data$english), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "french", sum(Everton1314Data$french), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "dutch", sum(Everton1314Data$dutch), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "portuguese", sum(Everton1314Data$portuguese), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "castilian", sum(Everton1314Data$castilian), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "spanish", sum(Everton1314Data$spanish), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "german", sum(Everton1314Data$german), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "arabic", sum(Everton1314Data$arabic), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "serbian", sum(Everton1314Data$serbian), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "korean", sum(Everton1314Data$korean), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "croatian", sum(Everton1314Data$croatian), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "italian", sum(Everton1314Data$italian), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "armenian", sum(Everton1314Data$armenian), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "icelandic", sum(Everton1314Data$icelandic), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "hebrew", sum(Everton1314Data$hebrew), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "bokmal", sum(Everton1314Data$bokmal), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "ukrainian", sum(Everton1314Data$ukrainian), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "russian", sum(Everton1314Data$russian), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "bosnian", sum(Everton1314Data$bosnian), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "czech", sum(Everton1314Data$czech), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "danish", sum(Everton1314Data$danish), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "swedish", sum(Everton1314Data$swedish), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "japanese", sum(Everton1314Data$japanese), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "turkish", sum(Everton1314Data$turkish), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "asante", sum(Everton1314Data$asante), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "persian", sum(Everton1314Data$persian), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "maltese", sum(Everton1314Data$maltese), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "romanian", sum(Everton1314Data$romanian), Everton1314Data$PropLanguages)
Everton1314Data$PropLanguages <- ifelse(Everton1314Data$Languages == "slovak", sum(Everton1314Data$slovak), Everton1314Data$PropLanguages)

Liverpool1314Data <- subset(PL1314SZN, Squad =="Liverpool")
Liverpool1314Data$english <- ifelse(Liverpool1314Data$Languages=="english", 1, 0)
Liverpool1314Data$french <- ifelse(Liverpool1314Data$Languages=="french", 1, 0)
Liverpool1314Data$dutch <- ifelse(Liverpool1314Data$Languages=="dutch", 1, 0)
Liverpool1314Data$portuguese <- ifelse(Liverpool1314Data$Languages=="portuguese", 1, 0)
Liverpool1314Data$castilian <- ifelse(Liverpool1314Data$Languages=="castilian", 1, 0)
Liverpool1314Data$spanish <- ifelse(Liverpool1314Data$Languages=="spanish", 1, 0)
Liverpool1314Data$german <- ifelse(Liverpool1314Data$Languages=="german", 1, 0)
Liverpool1314Data$arabic <- ifelse(Liverpool1314Data$Languages=="arabic", 1, 0)
Liverpool1314Data$serbian <- ifelse(Liverpool1314Data$Languages=="serbian", 1, 0)
Liverpool1314Data$korean <- ifelse(Liverpool1314Data$Languages=="korean", 1, 0)
Liverpool1314Data$croatian <- ifelse(Liverpool1314Data$Languages=="croatian", 1, 0)
Liverpool1314Data$italian <- ifelse(Liverpool1314Data$Languages=="italian", 1, 0)
Liverpool1314Data$armenian <- ifelse(Liverpool1314Data$Languages=="armenian", 1, 0)
Liverpool1314Data$icelandic <- ifelse(Liverpool1314Data$Languages=="icelandic", 1, 0)
Liverpool1314Data$hebrew <- ifelse(Liverpool1314Data$Languages=="hebrew", 1, 0)
Liverpool1314Data$bokmal <- ifelse(Liverpool1314Data$Languages=="bokmal", 1, 0)
Liverpool1314Data$ukrainian <- ifelse(Liverpool1314Data$Languages=="ukrainian", 1, 0)
Liverpool1314Data$russian <- ifelse(Liverpool1314Data$Languages=="russian", 1, 0)
Liverpool1314Data$bosnian <- ifelse(Liverpool1314Data$Languages=="bosnian", 1, 0)
Liverpool1314Data$czech <- ifelse(Liverpool1314Data$Languages=="czech", 1, 0)
Liverpool1314Data$danish <- ifelse(Liverpool1314Data$Languages=="danish", 1, 0)
Liverpool1314Data$swedish <- ifelse(Liverpool1314Data$Languages=="swedish", 1, 0)
Liverpool1314Data$japanese <- ifelse(Liverpool1314Data$Languages=="japanese", 1, 0)
Liverpool1314Data$turkish <- ifelse(Liverpool1314Data$Languages=="turkish", 1, 0)
Liverpool1314Data$asante <- ifelse(Liverpool1314Data$Languages=="asante", 1, 0)
Liverpool1314Data$persian <- ifelse(Liverpool1314Data$Languages=="persian", 1, 0)
Liverpool1314Data$maltese <- ifelse(Liverpool1314Data$Languages=="maltese", 1, 0)
Liverpool1314Data$romanian <- ifelse(Liverpool1314Data$Languages=="romanian", 1, 0)
Liverpool1314Data$slovak <- ifelse(Liverpool1314Data$Languages=="slovak", 1, 0)
Liverpool1314Data$PropLanguages <- 0
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "english", sum(Liverpool1314Data$english), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "french", sum(Liverpool1314Data$french), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "dutch", sum(Liverpool1314Data$dutch), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "portuguese", sum(Liverpool1314Data$portuguese), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "castilian", sum(Liverpool1314Data$castilian), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "spanish", sum(Liverpool1314Data$spanish), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "german", sum(Liverpool1314Data$german), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "arabic", sum(Liverpool1314Data$arabic), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "serbian", sum(Liverpool1314Data$serbian), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "korean", sum(Liverpool1314Data$korean), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "croatian", sum(Liverpool1314Data$croatian), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "italian", sum(Liverpool1314Data$italian), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "armenian", sum(Liverpool1314Data$armenian), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "icelandic", sum(Liverpool1314Data$icelandic), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "hebrew", sum(Liverpool1314Data$hebrew), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "bokmal", sum(Liverpool1314Data$bokmal), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "ukrainian", sum(Liverpool1314Data$ukrainian), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "russian", sum(Liverpool1314Data$russian), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "bosnian", sum(Liverpool1314Data$bosnian), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "czech", sum(Liverpool1314Data$czech), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "danish", sum(Liverpool1314Data$danish), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "swedish", sum(Liverpool1314Data$swedish), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "japanese", sum(Liverpool1314Data$japanese), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "turkish", sum(Liverpool1314Data$turkish), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "asante", sum(Liverpool1314Data$asante), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "persian", sum(Liverpool1314Data$persian), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "maltese", sum(Liverpool1314Data$maltese), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "romanian", sum(Liverpool1314Data$romanian), Liverpool1314Data$PropLanguages)
Liverpool1314Data$PropLanguages <- ifelse(Liverpool1314Data$Languages == "slovak", sum(Liverpool1314Data$slovak), Liverpool1314Data$PropLanguages)

ManchesterCity1314Data <- subset(PL1314SZN, Squad =="Manchester City")
ManchesterCity1314Data$english <- ifelse(ManchesterCity1314Data$Languages=="english", 1, 0)
ManchesterCity1314Data$french <- ifelse(ManchesterCity1314Data$Languages=="french", 1, 0)
ManchesterCity1314Data$dutch <- ifelse(ManchesterCity1314Data$Languages=="dutch", 1, 0)
ManchesterCity1314Data$portuguese <- ifelse(ManchesterCity1314Data$Languages=="portuguese", 1, 0)
ManchesterCity1314Data$castilian <- ifelse(ManchesterCity1314Data$Languages=="castilian", 1, 0)
ManchesterCity1314Data$spanish <- ifelse(ManchesterCity1314Data$Languages=="spanish", 1, 0)
ManchesterCity1314Data$german <- ifelse(ManchesterCity1314Data$Languages=="german", 1, 0)
ManchesterCity1314Data$arabic <- ifelse(ManchesterCity1314Data$Languages=="arabic", 1, 0)
ManchesterCity1314Data$serbian <- ifelse(ManchesterCity1314Data$Languages=="serbian", 1, 0)
ManchesterCity1314Data$korean <- ifelse(ManchesterCity1314Data$Languages=="korean", 1, 0)
ManchesterCity1314Data$croatian <- ifelse(ManchesterCity1314Data$Languages=="croatian", 1, 0)
ManchesterCity1314Data$italian <- ifelse(ManchesterCity1314Data$Languages=="italian", 1, 0)
ManchesterCity1314Data$armenian <- ifelse(ManchesterCity1314Data$Languages=="armenian", 1, 0)
ManchesterCity1314Data$icelandic <- ifelse(ManchesterCity1314Data$Languages=="icelandic", 1, 0)
ManchesterCity1314Data$hebrew <- ifelse(ManchesterCity1314Data$Languages=="hebrew", 1, 0)
ManchesterCity1314Data$bokmal <- ifelse(ManchesterCity1314Data$Languages=="bokmal", 1, 0)
ManchesterCity1314Data$ukrainian <- ifelse(ManchesterCity1314Data$Languages=="ukrainian", 1, 0)
ManchesterCity1314Data$russian <- ifelse(ManchesterCity1314Data$Languages=="russian", 1, 0)
ManchesterCity1314Data$bosnian <- ifelse(ManchesterCity1314Data$Languages=="bosnian", 1, 0)
ManchesterCity1314Data$czech <- ifelse(ManchesterCity1314Data$Languages=="czech", 1, 0)
ManchesterCity1314Data$danish <- ifelse(ManchesterCity1314Data$Languages=="danish", 1, 0)
ManchesterCity1314Data$swedish <- ifelse(ManchesterCity1314Data$Languages=="swedish", 1, 0)
ManchesterCity1314Data$japanese <- ifelse(ManchesterCity1314Data$Languages=="japanese", 1, 0)
ManchesterCity1314Data$turkish <- ifelse(ManchesterCity1314Data$Languages=="turkish", 1, 0)
ManchesterCity1314Data$asante <- ifelse(ManchesterCity1314Data$Languages=="asante", 1, 0)
ManchesterCity1314Data$persian <- ifelse(ManchesterCity1314Data$Languages=="persian", 1, 0)
ManchesterCity1314Data$maltese <- ifelse(ManchesterCity1314Data$Languages=="maltese", 1, 0)
ManchesterCity1314Data$romanian <- ifelse(ManchesterCity1314Data$Languages=="romanian", 1, 0)
ManchesterCity1314Data$slovak <- ifelse(ManchesterCity1314Data$Languages=="slovak", 1, 0)
ManchesterCity1314Data$PropLanguages <- 0
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "english", sum(ManchesterCity1314Data$english), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "french", sum(ManchesterCity1314Data$french), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "dutch", sum(ManchesterCity1314Data$dutch), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "portuguese", sum(ManchesterCity1314Data$portuguese), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "castilian", sum(ManchesterCity1314Data$castilian), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "spanish", sum(ManchesterCity1314Data$spanish), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "german", sum(ManchesterCity1314Data$german), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "arabic", sum(ManchesterCity1314Data$arabic), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "serbian", sum(ManchesterCity1314Data$serbian), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "korean", sum(ManchesterCity1314Data$korean), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "croatian", sum(ManchesterCity1314Data$croatian), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "italian", sum(ManchesterCity1314Data$italian), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "armenian", sum(ManchesterCity1314Data$armenian), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "icelandic", sum(ManchesterCity1314Data$icelandic), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "hebrew", sum(ManchesterCity1314Data$hebrew), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "bokmal", sum(ManchesterCity1314Data$bokmal), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "ukrainian", sum(ManchesterCity1314Data$ukrainian), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "russian", sum(ManchesterCity1314Data$russian), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "bosnian", sum(ManchesterCity1314Data$bosnian), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "czech", sum(ManchesterCity1314Data$czech), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "danish", sum(ManchesterCity1314Data$danish), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "swedish", sum(ManchesterCity1314Data$swedish), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "japanese", sum(ManchesterCity1314Data$japanese), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "turkish", sum(ManchesterCity1314Data$turkish), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "asante", sum(ManchesterCity1314Data$asante), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "persian", sum(ManchesterCity1314Data$persian), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "maltese", sum(ManchesterCity1314Data$maltese), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "romanian", sum(ManchesterCity1314Data$romanian), ManchesterCity1314Data$PropLanguages)
ManchesterCity1314Data$PropLanguages <- ifelse(ManchesterCity1314Data$Languages == "slovak", sum(ManchesterCity1314Data$slovak), ManchesterCity1314Data$PropLanguages)

ManchesterUtd1314Data <- subset(PL1314SZN, Squad =="Manchester Utd")
ManchesterUtd1314Data$english <- ifelse(ManchesterUtd1314Data$Languages=="english", 1, 0)
ManchesterUtd1314Data$french <- ifelse(ManchesterUtd1314Data$Languages=="french", 1, 0)
ManchesterUtd1314Data$dutch <- ifelse(ManchesterUtd1314Data$Languages=="dutch", 1, 0)
ManchesterUtd1314Data$portuguese <- ifelse(ManchesterUtd1314Data$Languages=="portuguese", 1, 0)
ManchesterUtd1314Data$castilian <- ifelse(ManchesterUtd1314Data$Languages=="castilian", 1, 0)
ManchesterUtd1314Data$spanish <- ifelse(ManchesterUtd1314Data$Languages=="spanish", 1, 0)
ManchesterUtd1314Data$german <- ifelse(ManchesterUtd1314Data$Languages=="german", 1, 0)
ManchesterUtd1314Data$arabic <- ifelse(ManchesterUtd1314Data$Languages=="arabic", 1, 0)
ManchesterUtd1314Data$serbian <- ifelse(ManchesterUtd1314Data$Languages=="serbian", 1, 0)
ManchesterUtd1314Data$korean <- ifelse(ManchesterUtd1314Data$Languages=="korean", 1, 0)
ManchesterUtd1314Data$croatian <- ifelse(ManchesterUtd1314Data$Languages=="croatian", 1, 0)
ManchesterUtd1314Data$italian <- ifelse(ManchesterUtd1314Data$Languages=="italian", 1, 0)
ManchesterUtd1314Data$armenian <- ifelse(ManchesterUtd1314Data$Languages=="armenian", 1, 0)
ManchesterUtd1314Data$icelandic <- ifelse(ManchesterUtd1314Data$Languages=="icelandic", 1, 0)
ManchesterUtd1314Data$hebrew <- ifelse(ManchesterUtd1314Data$Languages=="hebrew", 1, 0)
ManchesterUtd1314Data$bokmal <- ifelse(ManchesterUtd1314Data$Languages=="bokmal", 1, 0)
ManchesterUtd1314Data$ukrainian <- ifelse(ManchesterUtd1314Data$Languages=="ukrainian", 1, 0)
ManchesterUtd1314Data$russian <- ifelse(ManchesterUtd1314Data$Languages=="russian", 1, 0)
ManchesterUtd1314Data$bosnian <- ifelse(ManchesterUtd1314Data$Languages=="bosnian", 1, 0)
ManchesterUtd1314Data$czech <- ifelse(ManchesterUtd1314Data$Languages=="czech", 1, 0)
ManchesterUtd1314Data$danish <- ifelse(ManchesterUtd1314Data$Languages=="danish", 1, 0)
ManchesterUtd1314Data$swedish <- ifelse(ManchesterUtd1314Data$Languages=="swedish", 1, 0)
ManchesterUtd1314Data$japanese <- ifelse(ManchesterUtd1314Data$Languages=="japanese", 1, 0)
ManchesterUtd1314Data$turkish <- ifelse(ManchesterUtd1314Data$Languages=="turkish", 1, 0)
ManchesterUtd1314Data$asante <- ifelse(ManchesterUtd1314Data$Languages=="asante", 1, 0)
ManchesterUtd1314Data$persian <- ifelse(ManchesterUtd1314Data$Languages=="persian", 1, 0)
ManchesterUtd1314Data$maltese <- ifelse(ManchesterUtd1314Data$Languages=="maltese", 1, 0)
ManchesterUtd1314Data$romanian <- ifelse(ManchesterUtd1314Data$Languages=="romanian", 1, 0)
ManchesterUtd1314Data$slovak <- ifelse(ManchesterUtd1314Data$Languages=="slovak", 1, 0)
ManchesterUtd1314Data$PropLanguages <- 0
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "english", sum(ManchesterUtd1314Data$english), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "french", sum(ManchesterUtd1314Data$french), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "dutch", sum(ManchesterUtd1314Data$dutch), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "portuguese", sum(ManchesterUtd1314Data$portuguese), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "castilian", sum(ManchesterUtd1314Data$castilian), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "spanish", sum(ManchesterUtd1314Data$spanish), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "german", sum(ManchesterUtd1314Data$german), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "arabic", sum(ManchesterUtd1314Data$arabic), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "serbian", sum(ManchesterUtd1314Data$serbian), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "korean", sum(ManchesterUtd1314Data$korean), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "croatian", sum(ManchesterUtd1314Data$croatian), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "italian", sum(ManchesterUtd1314Data$italian), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "armenian", sum(ManchesterUtd1314Data$armenian), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "icelandic", sum(ManchesterUtd1314Data$icelandic), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "hebrew", sum(ManchesterUtd1314Data$hebrew), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "bokmal", sum(ManchesterUtd1314Data$bokmal), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "ukrainian", sum(ManchesterUtd1314Data$ukrainian), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "russian", sum(ManchesterUtd1314Data$russian), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "bosnian", sum(ManchesterUtd1314Data$bosnian), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "czech", sum(ManchesterUtd1314Data$czech), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "danish", sum(ManchesterUtd1314Data$danish), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "swedish", sum(ManchesterUtd1314Data$swedish), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "japanese", sum(ManchesterUtd1314Data$japanese), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "turkish", sum(ManchesterUtd1314Data$turkish), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "asante", sum(ManchesterUtd1314Data$asante), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "persian", sum(ManchesterUtd1314Data$persian), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "maltese", sum(ManchesterUtd1314Data$maltese), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "romanian", sum(ManchesterUtd1314Data$romanian), ManchesterUtd1314Data$PropLanguages)
ManchesterUtd1314Data$PropLanguages <- ifelse(ManchesterUtd1314Data$Languages == "slovak", sum(ManchesterUtd1314Data$slovak), ManchesterUtd1314Data$PropLanguages)

Tottenham1314Data <- subset(PL1314SZN, Squad =="Tottenham")
Tottenham1314Data$english <- ifelse(Tottenham1314Data$Languages=="english", 1, 0)
Tottenham1314Data$french <- ifelse(Tottenham1314Data$Languages=="french", 1, 0)
Tottenham1314Data$dutch <- ifelse(Tottenham1314Data$Languages=="dutch", 1, 0)
Tottenham1314Data$portuguese <- ifelse(Tottenham1314Data$Languages=="portuguese", 1, 0)
Tottenham1314Data$castilian <- ifelse(Tottenham1314Data$Languages=="castilian", 1, 0)
Tottenham1314Data$spanish <- ifelse(Tottenham1314Data$Languages=="spanish", 1, 0)
Tottenham1314Data$german <- ifelse(Tottenham1314Data$Languages=="german", 1, 0)
Tottenham1314Data$arabic <- ifelse(Tottenham1314Data$Languages=="arabic", 1, 0)
Tottenham1314Data$serbian <- ifelse(Tottenham1314Data$Languages=="serbian", 1, 0)
Tottenham1314Data$korean <- ifelse(Tottenham1314Data$Languages=="korean", 1, 0)
Tottenham1314Data$croatian <- ifelse(Tottenham1314Data$Languages=="croatian", 1, 0)
Tottenham1314Data$italian <- ifelse(Tottenham1314Data$Languages=="italian", 1, 0)
Tottenham1314Data$armenian <- ifelse(Tottenham1314Data$Languages=="armenian", 1, 0)
Tottenham1314Data$icelandic <- ifelse(Tottenham1314Data$Languages=="icelandic", 1, 0)
Tottenham1314Data$hebrew <- ifelse(Tottenham1314Data$Languages=="hebrew", 1, 0)
Tottenham1314Data$bokmal <- ifelse(Tottenham1314Data$Languages=="bokmal", 1, 0)
Tottenham1314Data$ukrainian <- ifelse(Tottenham1314Data$Languages=="ukrainian", 1, 0)
Tottenham1314Data$russian <- ifelse(Tottenham1314Data$Languages=="russian", 1, 0)
Tottenham1314Data$bosnian <- ifelse(Tottenham1314Data$Languages=="bosnian", 1, 0)
Tottenham1314Data$czech <- ifelse(Tottenham1314Data$Languages=="czech", 1, 0)
Tottenham1314Data$danish <- ifelse(Tottenham1314Data$Languages=="danish", 1, 0)
Tottenham1314Data$swedish <- ifelse(Tottenham1314Data$Languages=="swedish", 1, 0)
Tottenham1314Data$japanese <- ifelse(Tottenham1314Data$Languages=="japanese", 1, 0)
Tottenham1314Data$turkish <- ifelse(Tottenham1314Data$Languages=="turkish", 1, 0)
Tottenham1314Data$asante <- ifelse(Tottenham1314Data$Languages=="asante", 1, 0)
Tottenham1314Data$persian <- ifelse(Tottenham1314Data$Languages=="persian", 1, 0)
Tottenham1314Data$maltese <- ifelse(Tottenham1314Data$Languages=="maltese", 1, 0)
Tottenham1314Data$romanian <- ifelse(Tottenham1314Data$Languages=="romanian", 1, 0)
Tottenham1314Data$slovak <- ifelse(Tottenham1314Data$Languages=="slovak", 1, 0)
Tottenham1314Data$PropLanguages <- 0
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "english", sum(Tottenham1314Data$english), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "french", sum(Tottenham1314Data$french), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "dutch", sum(Tottenham1314Data$dutch), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "portuguese", sum(Tottenham1314Data$portuguese), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "castilian", sum(Tottenham1314Data$castilian), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "spanish", sum(Tottenham1314Data$spanish), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "german", sum(Tottenham1314Data$german), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "arabic", sum(Tottenham1314Data$arabic), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "serbian", sum(Tottenham1314Data$serbian), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "korean", sum(Tottenham1314Data$korean), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "croatian", sum(Tottenham1314Data$croatian), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "italian", sum(Tottenham1314Data$italian), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "armenian", sum(Tottenham1314Data$armenian), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "icelandic", sum(Tottenham1314Data$icelandic), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "hebrew", sum(Tottenham1314Data$hebrew), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "bokmal", sum(Tottenham1314Data$bokmal), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "ukrainian", sum(Tottenham1314Data$ukrainian), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "russian", sum(Tottenham1314Data$russian), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "bosnian", sum(Tottenham1314Data$bosnian), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "czech", sum(Tottenham1314Data$czech), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "danish", sum(Tottenham1314Data$danish), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "swedish", sum(Tottenham1314Data$swedish), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "japanese", sum(Tottenham1314Data$japanese), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "turkish", sum(Tottenham1314Data$turkish), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "asante", sum(Tottenham1314Data$asante), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "persian", sum(Tottenham1314Data$persian), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "maltese", sum(Tottenham1314Data$maltese), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "romanian", sum(Tottenham1314Data$romanian), Tottenham1314Data$PropLanguages)
Tottenham1314Data$PropLanguages <- ifelse(Tottenham1314Data$Languages == "slovak", sum(Tottenham1314Data$slovak), Tottenham1314Data$PropLanguages)

PL1314SZN <- rbind(Arsenal1314Data, Chelsea1314Data)
PL1314SZN <- rbind(PL1314SZN, Everton1314Data)
PL1314SZN <- rbind(PL1314SZN, Liverpool1314Data)
PL1314SZN <- rbind(PL1314SZN, ManchesterCity1314Data)
PL1314SZN <- rbind(PL1314SZN, ManchesterUtd1314Data)
PL1314SZN <- rbind(PL1314SZN, Tottenham1314Data)

PL1213SZN$english <- 0
PL1213SZN$french <- 0
PL1213SZN$portuguese <- 0
PL1213SZN$castilian <- 0
PL1213SZN$dutch <- 0
PL1213SZN$german <- 0
PL1213SZN$spanish <- 0
PL1213SZN$arabic <- 0
PL1213SZN$serbian <- 0
PL1213SZN$korean <- 0
PL1213SZN$croatian <- 0
PL1213SZN$italian <- 0
PL1213SZN$icelandic <- 0
PL1213SZN$hebrew <- 0
PL1213SZN$bokmal <- 0
PL1213SZN$ukrainian <- 0
PL1213SZN$russian <- 0
PL1213SZN$bosnian <- 0
PL1213SZN$czech <- 0
PL1213SZN$danish <- 0
PL1213SZN$armenian <- 0
PL1213SZN$turkish <- 0
PL1213SZN$asante <- 0
PL1213SZN$persian <- 0
PL1213SZN$japanese <- 0
PL1213SZN$maltese <- 0
PL1213SZN$romanian <- 0
PL1213SZN$slovak <- 0
PL1213SZN$swedish <- 0

Arsenal1213Data <- subset(PL1213SZN, Squad =="Arsenal")
Arsenal1213Data$english <- ifelse(Arsenal1213Data$Languages=="english", 1, 0)
Arsenal1213Data$french <- ifelse(Arsenal1213Data$Languages=="french", 1, 0)
Arsenal1213Data$dutch <- ifelse(Arsenal1213Data$Languages=="dutch", 1, 0)
Arsenal1213Data$portuguese <- ifelse(Arsenal1213Data$Languages=="portuguese", 1, 0)
Arsenal1213Data$castilian <- ifelse(Arsenal1213Data$Languages=="castilian", 1, 0)
Arsenal1213Data$spanish <- ifelse(Arsenal1213Data$Languages=="spanish", 1, 0)
Arsenal1213Data$german <- ifelse(Arsenal1213Data$Languages=="german", 1, 0)
Arsenal1213Data$arabic <- ifelse(Arsenal1213Data$Languages=="arabic", 1, 0)
Arsenal1213Data$serbian <- ifelse(Arsenal1213Data$Languages=="serbian", 1, 0)
Arsenal1213Data$korean <- ifelse(Arsenal1213Data$Languages=="korean", 1, 0)
Arsenal1213Data$croatian <- ifelse(Arsenal1213Data$Languages=="croatian", 1, 0)
Arsenal1213Data$italian <- ifelse(Arsenal1213Data$Languages=="italian", 1, 0)
Arsenal1213Data$armenian <- ifelse(Arsenal1213Data$Languages=="armenian", 1, 0)
Arsenal1213Data$icelandic <- ifelse(Arsenal1213Data$Languages=="icelandic", 1, 0)
Arsenal1213Data$hebrew <- ifelse(Arsenal1213Data$Languages=="hebrew", 1, 0)
Arsenal1213Data$bokmal <- ifelse(Arsenal1213Data$Languages=="bokmal", 1, 0)
Arsenal1213Data$ukrainian <- ifelse(Arsenal1213Data$Languages=="ukrainian", 1, 0)
Arsenal1213Data$russian <- ifelse(Arsenal1213Data$Languages=="russian", 1, 0)
Arsenal1213Data$bosnian <- ifelse(Arsenal1213Data$Languages=="bosnian", 1, 0)
Arsenal1213Data$czech <- ifelse(Arsenal1213Data$Languages=="czech", 1, 0)
Arsenal1213Data$danish <- ifelse(Arsenal1213Data$Languages=="danish", 1, 0)
Arsenal1213Data$swedish <- ifelse(Arsenal1213Data$Languages=="swedish", 1, 0)
Arsenal1213Data$japanese <- ifelse(Arsenal1213Data$Languages=="japanese", 1, 0)
Arsenal1213Data$turkish <- ifelse(Arsenal1213Data$Languages=="turkish", 1, 0)
Arsenal1213Data$asante <- ifelse(Arsenal1213Data$Languages=="asante", 1, 0)
Arsenal1213Data$persian <- ifelse(Arsenal1213Data$Languages=="persian", 1, 0)
Arsenal1213Data$maltese <- ifelse(Arsenal1213Data$Languages=="maltese", 1, 0)
Arsenal1213Data$romanian <- ifelse(Arsenal1213Data$Languages=="romanian", 1, 0)
Arsenal1213Data$slovak <- ifelse(Arsenal1213Data$Languages=="slovak", 1, 0)
Arsenal1213Data$PropLanguages <- 0
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "english", sum(Arsenal1213Data$english), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "french", sum(Arsenal1213Data$french), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "dutch", sum(Arsenal1213Data$dutch), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "portuguese", sum(Arsenal1213Data$portuguese), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "castilian", sum(Arsenal1213Data$castilian), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "spanish", sum(Arsenal1213Data$spanish), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "german", sum(Arsenal1213Data$german), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "arabic", sum(Arsenal1213Data$arabic), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "serbian", sum(Arsenal1213Data$serbian), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "korean", sum(Arsenal1213Data$korean), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "croatian", sum(Arsenal1213Data$croatian), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "italian", sum(Arsenal1213Data$italian), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "armenian", sum(Arsenal1213Data$armenian), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "icelandic", sum(Arsenal1213Data$icelandic), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "hebrew", sum(Arsenal1213Data$hebrew), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "bokmal", sum(Arsenal1213Data$bokmal), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "ukrainian", sum(Arsenal1213Data$ukrainian), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "russian", sum(Arsenal1213Data$russian), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "bosnian", sum(Arsenal1213Data$bosnian), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "czech", sum(Arsenal1213Data$czech), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "danish", sum(Arsenal1213Data$danish), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "swedish", sum(Arsenal1213Data$swedish), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "japanese", sum(Arsenal1213Data$japanese), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "turkish", sum(Arsenal1213Data$turkish), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "asante", sum(Arsenal1213Data$asante), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "persian", sum(Arsenal1213Data$persian), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "maltese", sum(Arsenal1213Data$maltese), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "romanian", sum(Arsenal1213Data$romanian), Arsenal1213Data$PropLanguages)
Arsenal1213Data$PropLanguages <- ifelse(Arsenal1213Data$Languages == "slovak", sum(Arsenal1213Data$slovak), Arsenal1213Data$PropLanguages)

Chelsea1213Data <- subset(PL1213SZN, Squad =="Chelsea")
Chelsea1213Data$english <- ifelse(Chelsea1213Data$Languages=="english", 1, 0)
Chelsea1213Data$french <- ifelse(Chelsea1213Data$Languages=="french", 1, 0)
Chelsea1213Data$dutch <- ifelse(Chelsea1213Data$Languages=="dutch", 1, 0)
Chelsea1213Data$portuguese <- ifelse(Chelsea1213Data$Languages=="portuguese", 1, 0)
Chelsea1213Data$castilian <- ifelse(Chelsea1213Data$Languages=="castilian", 1, 0)
Chelsea1213Data$spanish <- ifelse(Chelsea1213Data$Languages=="spanish", 1, 0)
Chelsea1213Data$german <- ifelse(Chelsea1213Data$Languages=="german", 1, 0)
Chelsea1213Data$arabic <- ifelse(Chelsea1213Data$Languages=="arabic", 1, 0)
Chelsea1213Data$serbian <- ifelse(Chelsea1213Data$Languages=="serbian", 1, 0)
Chelsea1213Data$korean <- ifelse(Chelsea1213Data$Languages=="korean", 1, 0)
Chelsea1213Data$croatian <- ifelse(Chelsea1213Data$Languages=="croatian", 1, 0)
Chelsea1213Data$italian <- ifelse(Chelsea1213Data$Languages=="italian", 1, 0)
Chelsea1213Data$armenian <- ifelse(Chelsea1213Data$Languages=="armenian", 1, 0)
Chelsea1213Data$icelandic <- ifelse(Chelsea1213Data$Languages=="icelandic", 1, 0)
Chelsea1213Data$hebrew <- ifelse(Chelsea1213Data$Languages=="hebrew", 1, 0)
Chelsea1213Data$bokmal <- ifelse(Chelsea1213Data$Languages=="bokmal", 1, 0)
Chelsea1213Data$ukrainian <- ifelse(Chelsea1213Data$Languages=="ukrainian", 1, 0)
Chelsea1213Data$russian <- ifelse(Chelsea1213Data$Languages=="russian", 1, 0)
Chelsea1213Data$bosnian <- ifelse(Chelsea1213Data$Languages=="bosnian", 1, 0)
Chelsea1213Data$czech <- ifelse(Chelsea1213Data$Languages=="czech", 1, 0)
Chelsea1213Data$danish <- ifelse(Chelsea1213Data$Languages=="danish", 1, 0)
Chelsea1213Data$swedish <- ifelse(Chelsea1213Data$Languages=="swedish", 1, 0)
Chelsea1213Data$japanese <- ifelse(Chelsea1213Data$Languages=="japanese", 1, 0)
Chelsea1213Data$turkish <- ifelse(Chelsea1213Data$Languages=="turkish", 1, 0)
Chelsea1213Data$asante <- ifelse(Chelsea1213Data$Languages=="asante", 1, 0)
Chelsea1213Data$persian <- ifelse(Chelsea1213Data$Languages=="persian", 1, 0)
Chelsea1213Data$maltese <- ifelse(Chelsea1213Data$Languages=="maltese", 1, 0)
Chelsea1213Data$romanian <- ifelse(Chelsea1213Data$Languages=="romanian", 1, 0)
Chelsea1213Data$slovak <- ifelse(Chelsea1213Data$Languages=="slovak", 1, 0)
Chelsea1213Data$PropLanguages <- 0
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "english", sum(Chelsea1213Data$english), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "french", sum(Chelsea1213Data$french), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "dutch", sum(Chelsea1213Data$dutch), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "portuguese", sum(Chelsea1213Data$portuguese), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "castilian", sum(Chelsea1213Data$castilian), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "spanish", sum(Chelsea1213Data$spanish), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "german", sum(Chelsea1213Data$german), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "arabic", sum(Chelsea1213Data$arabic), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "serbian", sum(Chelsea1213Data$serbian), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "korean", sum(Chelsea1213Data$korean), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "croatian", sum(Chelsea1213Data$croatian), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "italian", sum(Chelsea1213Data$italian), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "armenian", sum(Chelsea1213Data$armenian), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "icelandic", sum(Chelsea1213Data$icelandic), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "hebrew", sum(Chelsea1213Data$hebrew), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "bokmal", sum(Chelsea1213Data$bokmal), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "ukrainian", sum(Chelsea1213Data$ukrainian), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "russian", sum(Chelsea1213Data$russian), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "bosnian", sum(Chelsea1213Data$bosnian), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "czech", sum(Chelsea1213Data$czech), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "danish", sum(Chelsea1213Data$danish), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "swedish", sum(Chelsea1213Data$swedish), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "japanese", sum(Chelsea1213Data$japanese), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "turkish", sum(Chelsea1213Data$turkish), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "asante", sum(Chelsea1213Data$asante), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "persian", sum(Chelsea1213Data$persian), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "maltese", sum(Chelsea1213Data$maltese), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "romanian", sum(Chelsea1213Data$romanian), Chelsea1213Data$PropLanguages)
Chelsea1213Data$PropLanguages <- ifelse(Chelsea1213Data$Languages == "slovak", sum(Chelsea1213Data$slovak), Chelsea1213Data$PropLanguages)

Everton1213Data <- subset(PL1213SZN, Squad =="Everton")
Everton1213Data$english <- ifelse(Everton1213Data$Languages=="english", 1, 0)
Everton1213Data$french <- ifelse(Everton1213Data$Languages=="french", 1, 0)
Everton1213Data$dutch <- ifelse(Everton1213Data$Languages=="dutch", 1, 0)
Everton1213Data$portuguese <- ifelse(Everton1213Data$Languages=="portuguese", 1, 0)
Everton1213Data$castilian <- ifelse(Everton1213Data$Languages=="castilian", 1, 0)
Everton1213Data$spanish <- ifelse(Everton1213Data$Languages=="spanish", 1, 0)
Everton1213Data$german <- ifelse(Everton1213Data$Languages=="german", 1, 0)
Everton1213Data$arabic <- ifelse(Everton1213Data$Languages=="arabic", 1, 0)
Everton1213Data$serbian <- ifelse(Everton1213Data$Languages=="serbian", 1, 0)
Everton1213Data$korean <- ifelse(Everton1213Data$Languages=="korean", 1, 0)
Everton1213Data$croatian <- ifelse(Everton1213Data$Languages=="croatian", 1, 0)
Everton1213Data$italian <- ifelse(Everton1213Data$Languages=="italian", 1, 0)
Everton1213Data$armenian <- ifelse(Everton1213Data$Languages=="armenian", 1, 0)
Everton1213Data$icelandic <- ifelse(Everton1213Data$Languages=="icelandic", 1, 0)
Everton1213Data$hebrew <- ifelse(Everton1213Data$Languages=="hebrew", 1, 0)
Everton1213Data$bokmal <- ifelse(Everton1213Data$Languages=="bokmal", 1, 0)
Everton1213Data$ukrainian <- ifelse(Everton1213Data$Languages=="ukrainian", 1, 0)
Everton1213Data$russian <- ifelse(Everton1213Data$Languages=="russian", 1, 0)
Everton1213Data$bosnian <- ifelse(Everton1213Data$Languages=="bosnian", 1, 0)
Everton1213Data$czech <- ifelse(Everton1213Data$Languages=="czech", 1, 0)
Everton1213Data$danish <- ifelse(Everton1213Data$Languages=="danish", 1, 0)
Everton1213Data$swedish <- ifelse(Everton1213Data$Languages=="swedish", 1, 0)
Everton1213Data$japanese <- ifelse(Everton1213Data$Languages=="japanese", 1, 0)
Everton1213Data$turkish <- ifelse(Everton1213Data$Languages=="turkish", 1, 0)
Everton1213Data$asante <- ifelse(Everton1213Data$Languages=="asante", 1, 0)
Everton1213Data$persian <- ifelse(Everton1213Data$Languages=="persian", 1, 0)
Everton1213Data$maltese <- ifelse(Everton1213Data$Languages=="maltese", 1, 0)
Everton1213Data$romanian <- ifelse(Everton1213Data$Languages=="romanian", 1, 0)
Everton1213Data$slovak <- ifelse(Everton1213Data$Languages=="slovak", 1, 0)
Everton1213Data$PropLanguages <- 0
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "english", sum(Everton1213Data$english), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "french", sum(Everton1213Data$french), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "dutch", sum(Everton1213Data$dutch), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "portuguese", sum(Everton1213Data$portuguese), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "castilian", sum(Everton1213Data$castilian), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "spanish", sum(Everton1213Data$spanish), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "german", sum(Everton1213Data$german), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "arabic", sum(Everton1213Data$arabic), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "serbian", sum(Everton1213Data$serbian), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "korean", sum(Everton1213Data$korean), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "croatian", sum(Everton1213Data$croatian), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "italian", sum(Everton1213Data$italian), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "armenian", sum(Everton1213Data$armenian), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "icelandic", sum(Everton1213Data$icelandic), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "hebrew", sum(Everton1213Data$hebrew), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "bokmal", sum(Everton1213Data$bokmal), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "ukrainian", sum(Everton1213Data$ukrainian), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "russian", sum(Everton1213Data$russian), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "bosnian", sum(Everton1213Data$bosnian), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "czech", sum(Everton1213Data$czech), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "danish", sum(Everton1213Data$danish), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "swedish", sum(Everton1213Data$swedish), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "japanese", sum(Everton1213Data$japanese), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "turkish", sum(Everton1213Data$turkish), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "asante", sum(Everton1213Data$asante), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "persian", sum(Everton1213Data$persian), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "maltese", sum(Everton1213Data$maltese), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "romanian", sum(Everton1213Data$romanian), Everton1213Data$PropLanguages)
Everton1213Data$PropLanguages <- ifelse(Everton1213Data$Languages == "slovak", sum(Everton1213Data$slovak), Everton1213Data$PropLanguages)

Liverpool1213Data <- subset(PL1213SZN, Squad =="Liverpool")
Liverpool1213Data$english <- ifelse(Liverpool1213Data$Languages=="english", 1, 0)
Liverpool1213Data$french <- ifelse(Liverpool1213Data$Languages=="french", 1, 0)
Liverpool1213Data$dutch <- ifelse(Liverpool1213Data$Languages=="dutch", 1, 0)
Liverpool1213Data$portuguese <- ifelse(Liverpool1213Data$Languages=="portuguese", 1, 0)
Liverpool1213Data$castilian <- ifelse(Liverpool1213Data$Languages=="castilian", 1, 0)
Liverpool1213Data$spanish <- ifelse(Liverpool1213Data$Languages=="spanish", 1, 0)
Liverpool1213Data$german <- ifelse(Liverpool1213Data$Languages=="german", 1, 0)
Liverpool1213Data$arabic <- ifelse(Liverpool1213Data$Languages=="arabic", 1, 0)
Liverpool1213Data$serbian <- ifelse(Liverpool1213Data$Languages=="serbian", 1, 0)
Liverpool1213Data$korean <- ifelse(Liverpool1213Data$Languages=="korean", 1, 0)
Liverpool1213Data$croatian <- ifelse(Liverpool1213Data$Languages=="croatian", 1, 0)
Liverpool1213Data$italian <- ifelse(Liverpool1213Data$Languages=="italian", 1, 0)
Liverpool1213Data$armenian <- ifelse(Liverpool1213Data$Languages=="armenian", 1, 0)
Liverpool1213Data$icelandic <- ifelse(Liverpool1213Data$Languages=="icelandic", 1, 0)
Liverpool1213Data$hebrew <- ifelse(Liverpool1213Data$Languages=="hebrew", 1, 0)
Liverpool1213Data$bokmal <- ifelse(Liverpool1213Data$Languages=="bokmal", 1, 0)
Liverpool1213Data$ukrainian <- ifelse(Liverpool1213Data$Languages=="ukrainian", 1, 0)
Liverpool1213Data$russian <- ifelse(Liverpool1213Data$Languages=="russian", 1, 0)
Liverpool1213Data$bosnian <- ifelse(Liverpool1213Data$Languages=="bosnian", 1, 0)
Liverpool1213Data$czech <- ifelse(Liverpool1213Data$Languages=="czech", 1, 0)
Liverpool1213Data$danish <- ifelse(Liverpool1213Data$Languages=="danish", 1, 0)
Liverpool1213Data$swedish <- ifelse(Liverpool1213Data$Languages=="swedish", 1, 0)
Liverpool1213Data$japanese <- ifelse(Liverpool1213Data$Languages=="japanese", 1, 0)
Liverpool1213Data$turkish <- ifelse(Liverpool1213Data$Languages=="turkish", 1, 0)
Liverpool1213Data$asante <- ifelse(Liverpool1213Data$Languages=="asante", 1, 0)
Liverpool1213Data$persian <- ifelse(Liverpool1213Data$Languages=="persian", 1, 0)
Liverpool1213Data$maltese <- ifelse(Liverpool1213Data$Languages=="maltese", 1, 0)
Liverpool1213Data$romanian <- ifelse(Liverpool1213Data$Languages=="romanian", 1, 0)
Liverpool1213Data$slovak <- ifelse(Liverpool1213Data$Languages=="slovak", 1, 0)
Liverpool1213Data$PropLanguages <- 0
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "english", sum(Liverpool1213Data$english), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "french", sum(Liverpool1213Data$french), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "dutch", sum(Liverpool1213Data$dutch), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "portuguese", sum(Liverpool1213Data$portuguese), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "castilian", sum(Liverpool1213Data$castilian), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "spanish", sum(Liverpool1213Data$spanish), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "german", sum(Liverpool1213Data$german), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "arabic", sum(Liverpool1213Data$arabic), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "serbian", sum(Liverpool1213Data$serbian), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "korean", sum(Liverpool1213Data$korean), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "croatian", sum(Liverpool1213Data$croatian), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "italian", sum(Liverpool1213Data$italian), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "armenian", sum(Liverpool1213Data$armenian), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "icelandic", sum(Liverpool1213Data$icelandic), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "hebrew", sum(Liverpool1213Data$hebrew), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "bokmal", sum(Liverpool1213Data$bokmal), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "ukrainian", sum(Liverpool1213Data$ukrainian), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "russian", sum(Liverpool1213Data$russian), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "bosnian", sum(Liverpool1213Data$bosnian), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "czech", sum(Liverpool1213Data$czech), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "danish", sum(Liverpool1213Data$danish), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "swedish", sum(Liverpool1213Data$swedish), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "japanese", sum(Liverpool1213Data$japanese), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "turkish", sum(Liverpool1213Data$turkish), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "asante", sum(Liverpool1213Data$asante), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "persian", sum(Liverpool1213Data$persian), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "maltese", sum(Liverpool1213Data$maltese), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "romanian", sum(Liverpool1213Data$romanian), Liverpool1213Data$PropLanguages)
Liverpool1213Data$PropLanguages <- ifelse(Liverpool1213Data$Languages == "slovak", sum(Liverpool1213Data$slovak), Liverpool1213Data$PropLanguages)

ManchesterCity1213Data <- subset(PL1213SZN, Squad =="Manchester City")
ManchesterCity1213Data$english <- ifelse(ManchesterCity1213Data$Languages=="english", 1, 0)
ManchesterCity1213Data$french <- ifelse(ManchesterCity1213Data$Languages=="french", 1, 0)
ManchesterCity1213Data$dutch <- ifelse(ManchesterCity1213Data$Languages=="dutch", 1, 0)
ManchesterCity1213Data$portuguese <- ifelse(ManchesterCity1213Data$Languages=="portuguese", 1, 0)
ManchesterCity1213Data$castilian <- ifelse(ManchesterCity1213Data$Languages=="castilian", 1, 0)
ManchesterCity1213Data$spanish <- ifelse(ManchesterCity1213Data$Languages=="spanish", 1, 0)
ManchesterCity1213Data$german <- ifelse(ManchesterCity1213Data$Languages=="german", 1, 0)
ManchesterCity1213Data$arabic <- ifelse(ManchesterCity1213Data$Languages=="arabic", 1, 0)
ManchesterCity1213Data$serbian <- ifelse(ManchesterCity1213Data$Languages=="serbian", 1, 0)
ManchesterCity1213Data$korean <- ifelse(ManchesterCity1213Data$Languages=="korean", 1, 0)
ManchesterCity1213Data$croatian <- ifelse(ManchesterCity1213Data$Languages=="croatian", 1, 0)
ManchesterCity1213Data$italian <- ifelse(ManchesterCity1213Data$Languages=="italian", 1, 0)
ManchesterCity1213Data$armenian <- ifelse(ManchesterCity1213Data$Languages=="armenian", 1, 0)
ManchesterCity1213Data$icelandic <- ifelse(ManchesterCity1213Data$Languages=="icelandic", 1, 0)
ManchesterCity1213Data$hebrew <- ifelse(ManchesterCity1213Data$Languages=="hebrew", 1, 0)
ManchesterCity1213Data$bokmal <- ifelse(ManchesterCity1213Data$Languages=="bokmal", 1, 0)
ManchesterCity1213Data$ukrainian <- ifelse(ManchesterCity1213Data$Languages=="ukrainian", 1, 0)
ManchesterCity1213Data$russian <- ifelse(ManchesterCity1213Data$Languages=="russian", 1, 0)
ManchesterCity1213Data$bosnian <- ifelse(ManchesterCity1213Data$Languages=="bosnian", 1, 0)
ManchesterCity1213Data$czech <- ifelse(ManchesterCity1213Data$Languages=="czech", 1, 0)
ManchesterCity1213Data$danish <- ifelse(ManchesterCity1213Data$Languages=="danish", 1, 0)
ManchesterCity1213Data$swedish <- ifelse(ManchesterCity1213Data$Languages=="swedish", 1, 0)
ManchesterCity1213Data$japanese <- ifelse(ManchesterCity1213Data$Languages=="japanese", 1, 0)
ManchesterCity1213Data$turkish <- ifelse(ManchesterCity1213Data$Languages=="turkish", 1, 0)
ManchesterCity1213Data$asante <- ifelse(ManchesterCity1213Data$Languages=="asante", 1, 0)
ManchesterCity1213Data$persian <- ifelse(ManchesterCity1213Data$Languages=="persian", 1, 0)
ManchesterCity1213Data$maltese <- ifelse(ManchesterCity1213Data$Languages=="maltese", 1, 0)
ManchesterCity1213Data$romanian <- ifelse(ManchesterCity1213Data$Languages=="romanian", 1, 0)
ManchesterCity1213Data$slovak <- ifelse(ManchesterCity1213Data$Languages=="slovak", 1, 0)
ManchesterCity1213Data$PropLanguages <- 0
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "english", sum(ManchesterCity1213Data$english), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "french", sum(ManchesterCity1213Data$french), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "dutch", sum(ManchesterCity1213Data$dutch), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "portuguese", sum(ManchesterCity1213Data$portuguese), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "castilian", sum(ManchesterCity1213Data$castilian), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "spanish", sum(ManchesterCity1213Data$spanish), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "german", sum(ManchesterCity1213Data$german), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "arabic", sum(ManchesterCity1213Data$arabic), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "serbian", sum(ManchesterCity1213Data$serbian), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "korean", sum(ManchesterCity1213Data$korean), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "croatian", sum(ManchesterCity1213Data$croatian), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "italian", sum(ManchesterCity1213Data$italian), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "armenian", sum(ManchesterCity1213Data$armenian), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "icelandic", sum(ManchesterCity1213Data$icelandic), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "hebrew", sum(ManchesterCity1213Data$hebrew), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "bokmal", sum(ManchesterCity1213Data$bokmal), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "ukrainian", sum(ManchesterCity1213Data$ukrainian), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "russian", sum(ManchesterCity1213Data$russian), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "bosnian", sum(ManchesterCity1213Data$bosnian), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "czech", sum(ManchesterCity1213Data$czech), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "danish", sum(ManchesterCity1213Data$danish), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "swedish", sum(ManchesterCity1213Data$swedish), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "japanese", sum(ManchesterCity1213Data$japanese), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "turkish", sum(ManchesterCity1213Data$turkish), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "asante", sum(ManchesterCity1213Data$asante), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "persian", sum(ManchesterCity1213Data$persian), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "maltese", sum(ManchesterCity1213Data$maltese), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "romanian", sum(ManchesterCity1213Data$romanian), ManchesterCity1213Data$PropLanguages)
ManchesterCity1213Data$PropLanguages <- ifelse(ManchesterCity1213Data$Languages == "slovak", sum(ManchesterCity1213Data$slovak), ManchesterCity1213Data$PropLanguages)

ManchesterUtd1213Data <- subset(PL1213SZN, Squad =="Manchester Utd")
ManchesterUtd1213Data$english <- ifelse(ManchesterUtd1213Data$Languages=="english", 1, 0)
ManchesterUtd1213Data$french <- ifelse(ManchesterUtd1213Data$Languages=="french", 1, 0)
ManchesterUtd1213Data$dutch <- ifelse(ManchesterUtd1213Data$Languages=="dutch", 1, 0)
ManchesterUtd1213Data$portuguese <- ifelse(ManchesterUtd1213Data$Languages=="portuguese", 1, 0)
ManchesterUtd1213Data$castilian <- ifelse(ManchesterUtd1213Data$Languages=="castilian", 1, 0)
ManchesterUtd1213Data$spanish <- ifelse(ManchesterUtd1213Data$Languages=="spanish", 1, 0)
ManchesterUtd1213Data$german <- ifelse(ManchesterUtd1213Data$Languages=="german", 1, 0)
ManchesterUtd1213Data$arabic <- ifelse(ManchesterUtd1213Data$Languages=="arabic", 1, 0)
ManchesterUtd1213Data$serbian <- ifelse(ManchesterUtd1213Data$Languages=="serbian", 1, 0)
ManchesterUtd1213Data$korean <- ifelse(ManchesterUtd1213Data$Languages=="korean", 1, 0)
ManchesterUtd1213Data$croatian <- ifelse(ManchesterUtd1213Data$Languages=="croatian", 1, 0)
ManchesterUtd1213Data$italian <- ifelse(ManchesterUtd1213Data$Languages=="italian", 1, 0)
ManchesterUtd1213Data$armenian <- ifelse(ManchesterUtd1213Data$Languages=="armenian", 1, 0)
ManchesterUtd1213Data$icelandic <- ifelse(ManchesterUtd1213Data$Languages=="icelandic", 1, 0)
ManchesterUtd1213Data$hebrew <- ifelse(ManchesterUtd1213Data$Languages=="hebrew", 1, 0)
ManchesterUtd1213Data$bokmal <- ifelse(ManchesterUtd1213Data$Languages=="bokmal", 1, 0)
ManchesterUtd1213Data$ukrainian <- ifelse(ManchesterUtd1213Data$Languages=="ukrainian", 1, 0)
ManchesterUtd1213Data$russian <- ifelse(ManchesterUtd1213Data$Languages=="russian", 1, 0)
ManchesterUtd1213Data$bosnian <- ifelse(ManchesterUtd1213Data$Languages=="bosnian", 1, 0)
ManchesterUtd1213Data$czech <- ifelse(ManchesterUtd1213Data$Languages=="czech", 1, 0)
ManchesterUtd1213Data$danish <- ifelse(ManchesterUtd1213Data$Languages=="danish", 1, 0)
ManchesterUtd1213Data$swedish <- ifelse(ManchesterUtd1213Data$Languages=="swedish", 1, 0)
ManchesterUtd1213Data$japanese <- ifelse(ManchesterUtd1213Data$Languages=="japanese", 1, 0)
ManchesterUtd1213Data$turkish <- ifelse(ManchesterUtd1213Data$Languages=="turkish", 1, 0)
ManchesterUtd1213Data$asante <- ifelse(ManchesterUtd1213Data$Languages=="asante", 1, 0)
ManchesterUtd1213Data$persian <- ifelse(ManchesterUtd1213Data$Languages=="persian", 1, 0)
ManchesterUtd1213Data$maltese <- ifelse(ManchesterUtd1213Data$Languages=="maltese", 1, 0)
ManchesterUtd1213Data$romanian <- ifelse(ManchesterUtd1213Data$Languages=="romanian", 1, 0)
ManchesterUtd1213Data$slovak <- ifelse(ManchesterUtd1213Data$Languages=="slovak", 1, 0)
ManchesterUtd1213Data$PropLanguages <- 0
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "english", sum(ManchesterUtd1213Data$english), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "french", sum(ManchesterUtd1213Data$french), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "dutch", sum(ManchesterUtd1213Data$dutch), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "portuguese", sum(ManchesterUtd1213Data$portuguese), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "castilian", sum(ManchesterUtd1213Data$castilian), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "spanish", sum(ManchesterUtd1213Data$spanish), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "german", sum(ManchesterUtd1213Data$german), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "arabic", sum(ManchesterUtd1213Data$arabic), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "serbian", sum(ManchesterUtd1213Data$serbian), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "korean", sum(ManchesterUtd1213Data$korean), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "croatian", sum(ManchesterUtd1213Data$croatian), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "italian", sum(ManchesterUtd1213Data$italian), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "armenian", sum(ManchesterUtd1213Data$armenian), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "icelandic", sum(ManchesterUtd1213Data$icelandic), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "hebrew", sum(ManchesterUtd1213Data$hebrew), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "bokmal", sum(ManchesterUtd1213Data$bokmal), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "ukrainian", sum(ManchesterUtd1213Data$ukrainian), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "russian", sum(ManchesterUtd1213Data$russian), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "bosnian", sum(ManchesterUtd1213Data$bosnian), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "czech", sum(ManchesterUtd1213Data$czech), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "danish", sum(ManchesterUtd1213Data$danish), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "swedish", sum(ManchesterUtd1213Data$swedish), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "japanese", sum(ManchesterUtd1213Data$japanese), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "turkish", sum(ManchesterUtd1213Data$turkish), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "asante", sum(ManchesterUtd1213Data$asante), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "persian", sum(ManchesterUtd1213Data$persian), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "maltese", sum(ManchesterUtd1213Data$maltese), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "romanian", sum(ManchesterUtd1213Data$romanian), ManchesterUtd1213Data$PropLanguages)
ManchesterUtd1213Data$PropLanguages <- ifelse(ManchesterUtd1213Data$Languages == "slovak", sum(ManchesterUtd1213Data$slovak), ManchesterUtd1213Data$PropLanguages)

Tottenham1213Data <- subset(PL1213SZN, Squad =="Tottenham")
Tottenham1213Data$english <- ifelse(Tottenham1213Data$Languages=="english", 1, 0)
Tottenham1213Data$french <- ifelse(Tottenham1213Data$Languages=="french", 1, 0)
Tottenham1213Data$dutch <- ifelse(Tottenham1213Data$Languages=="dutch", 1, 0)
Tottenham1213Data$portuguese <- ifelse(Tottenham1213Data$Languages=="portuguese", 1, 0)
Tottenham1213Data$castilian <- ifelse(Tottenham1213Data$Languages=="castilian", 1, 0)
Tottenham1213Data$spanish <- ifelse(Tottenham1213Data$Languages=="spanish", 1, 0)
Tottenham1213Data$german <- ifelse(Tottenham1213Data$Languages=="german", 1, 0)
Tottenham1213Data$arabic <- ifelse(Tottenham1213Data$Languages=="arabic", 1, 0)
Tottenham1213Data$serbian <- ifelse(Tottenham1213Data$Languages=="serbian", 1, 0)
Tottenham1213Data$korean <- ifelse(Tottenham1213Data$Languages=="korean", 1, 0)
Tottenham1213Data$croatian <- ifelse(Tottenham1213Data$Languages=="croatian", 1, 0)
Tottenham1213Data$italian <- ifelse(Tottenham1213Data$Languages=="italian", 1, 0)
Tottenham1213Data$armenian <- ifelse(Tottenham1213Data$Languages=="armenian", 1, 0)
Tottenham1213Data$icelandic <- ifelse(Tottenham1213Data$Languages=="icelandic", 1, 0)
Tottenham1213Data$hebrew <- ifelse(Tottenham1213Data$Languages=="hebrew", 1, 0)
Tottenham1213Data$bokmal <- ifelse(Tottenham1213Data$Languages=="bokmal", 1, 0)
Tottenham1213Data$ukrainian <- ifelse(Tottenham1213Data$Languages=="ukrainian", 1, 0)
Tottenham1213Data$russian <- ifelse(Tottenham1213Data$Languages=="russian", 1, 0)
Tottenham1213Data$bosnian <- ifelse(Tottenham1213Data$Languages=="bosnian", 1, 0)
Tottenham1213Data$czech <- ifelse(Tottenham1213Data$Languages=="czech", 1, 0)
Tottenham1213Data$danish <- ifelse(Tottenham1213Data$Languages=="danish", 1, 0)
Tottenham1213Data$swedish <- ifelse(Tottenham1213Data$Languages=="swedish", 1, 0)
Tottenham1213Data$japanese <- ifelse(Tottenham1213Data$Languages=="japanese", 1, 0)
Tottenham1213Data$turkish <- ifelse(Tottenham1213Data$Languages=="turkish", 1, 0)
Tottenham1213Data$asante <- ifelse(Tottenham1213Data$Languages=="asante", 1, 0)
Tottenham1213Data$persian <- ifelse(Tottenham1213Data$Languages=="persian", 1, 0)
Tottenham1213Data$maltese <- ifelse(Tottenham1213Data$Languages=="maltese", 1, 0)
Tottenham1213Data$romanian <- ifelse(Tottenham1213Data$Languages=="romanian", 1, 0)
Tottenham1213Data$slovak <- ifelse(Tottenham1213Data$Languages=="slovak", 1, 0)
Tottenham1213Data$PropLanguages <- 0
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "english", sum(Tottenham1213Data$english), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "french", sum(Tottenham1213Data$french), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "dutch", sum(Tottenham1213Data$dutch), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "portuguese", sum(Tottenham1213Data$portuguese), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "castilian", sum(Tottenham1213Data$castilian), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "spanish", sum(Tottenham1213Data$spanish), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "german", sum(Tottenham1213Data$german), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "arabic", sum(Tottenham1213Data$arabic), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "serbian", sum(Tottenham1213Data$serbian), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "korean", sum(Tottenham1213Data$korean), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "croatian", sum(Tottenham1213Data$croatian), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "italian", sum(Tottenham1213Data$italian), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "armenian", sum(Tottenham1213Data$armenian), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "icelandic", sum(Tottenham1213Data$icelandic), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "hebrew", sum(Tottenham1213Data$hebrew), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "bokmal", sum(Tottenham1213Data$bokmal), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "ukrainian", sum(Tottenham1213Data$ukrainian), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "russian", sum(Tottenham1213Data$russian), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "bosnian", sum(Tottenham1213Data$bosnian), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "czech", sum(Tottenham1213Data$czech), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "danish", sum(Tottenham1213Data$danish), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "swedish", sum(Tottenham1213Data$swedish), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "japanese", sum(Tottenham1213Data$japanese), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "turkish", sum(Tottenham1213Data$turkish), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "asante", sum(Tottenham1213Data$asante), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "persian", sum(Tottenham1213Data$persian), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "maltese", sum(Tottenham1213Data$maltese), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "romanian", sum(Tottenham1213Data$romanian), Tottenham1213Data$PropLanguages)
Tottenham1213Data$PropLanguages <- ifelse(Tottenham1213Data$Languages == "slovak", sum(Tottenham1213Data$slovak), Tottenham1213Data$PropLanguages)

PL1213SZN <- rbind(Arsenal1213Data, Chelsea1213Data)
PL1213SZN <- rbind(PL1213SZN, Everton1213Data)
PL1213SZN <- rbind(PL1213SZN, Liverpool1213Data)
PL1213SZN <- rbind(PL1213SZN, ManchesterCity1213Data)
PL1213SZN <- rbind(PL1213SZN, ManchesterUtd1213Data)
PL1213SZN <- rbind(PL1213SZN, Tottenham1213Data)

PL1112SZN$english <- 0
PL1112SZN$french <- 0
PL1112SZN$portuguese <- 0
PL1112SZN$castilian <- 0
PL1112SZN$dutch <- 0
PL1112SZN$german <- 0
PL1112SZN$spanish <- 0
PL1112SZN$arabic <- 0
PL1112SZN$serbian <- 0
PL1112SZN$korean <- 0
PL1112SZN$croatian <- 0
PL1112SZN$italian <- 0
PL1112SZN$icelandic <- 0
PL1112SZN$hebrew <- 0
PL1112SZN$bokmal <- 0
PL1112SZN$ukrainian <- 0
PL1112SZN$russian <- 0
PL1112SZN$bosnian <- 0
PL1112SZN$czech <- 0
PL1112SZN$danish <- 0
PL1112SZN$armenian <- 0
PL1112SZN$turkish <- 0
PL1112SZN$asante <- 0
PL1112SZN$persian <- 0
PL1112SZN$japanese <- 0
PL1112SZN$maltese <- 0
PL1112SZN$romanian <- 0
PL1112SZN$slovak <- 0
PL1112SZN$swedish <- 0

Arsenal1112Data <- subset(PL1112SZN, Squad =="Arsenal")
Arsenal1112Data$english <- ifelse(Arsenal1112Data$Languages=="english", 1, 0)
Arsenal1112Data$french <- ifelse(Arsenal1112Data$Languages=="french", 1, 0)
Arsenal1112Data$dutch <- ifelse(Arsenal1112Data$Languages=="dutch", 1, 0)
Arsenal1112Data$portuguese <- ifelse(Arsenal1112Data$Languages=="portuguese", 1, 0)
Arsenal1112Data$castilian <- ifelse(Arsenal1112Data$Languages=="castilian", 1, 0)
Arsenal1112Data$spanish <- ifelse(Arsenal1112Data$Languages=="spanish", 1, 0)
Arsenal1112Data$german <- ifelse(Arsenal1112Data$Languages=="german", 1, 0)
Arsenal1112Data$arabic <- ifelse(Arsenal1112Data$Languages=="arabic", 1, 0)
Arsenal1112Data$serbian <- ifelse(Arsenal1112Data$Languages=="serbian", 1, 0)
Arsenal1112Data$korean <- ifelse(Arsenal1112Data$Languages=="korean", 1, 0)
Arsenal1112Data$croatian <- ifelse(Arsenal1112Data$Languages=="croatian", 1, 0)
Arsenal1112Data$italian <- ifelse(Arsenal1112Data$Languages=="italian", 1, 0)
Arsenal1112Data$armenian <- ifelse(Arsenal1112Data$Languages=="armenian", 1, 0)
Arsenal1112Data$icelandic <- ifelse(Arsenal1112Data$Languages=="icelandic", 1, 0)
Arsenal1112Data$hebrew <- ifelse(Arsenal1112Data$Languages=="hebrew", 1, 0)
Arsenal1112Data$bokmal <- ifelse(Arsenal1112Data$Languages=="bokmal", 1, 0)
Arsenal1112Data$ukrainian <- ifelse(Arsenal1112Data$Languages=="ukrainian", 1, 0)
Arsenal1112Data$russian <- ifelse(Arsenal1112Data$Languages=="russian", 1, 0)
Arsenal1112Data$bosnian <- ifelse(Arsenal1112Data$Languages=="bosnian", 1, 0)
Arsenal1112Data$czech <- ifelse(Arsenal1112Data$Languages=="czech", 1, 0)
Arsenal1112Data$danish <- ifelse(Arsenal1112Data$Languages=="danish", 1, 0)
Arsenal1112Data$swedish <- ifelse(Arsenal1112Data$Languages=="swedish", 1, 0)
Arsenal1112Data$japanese <- ifelse(Arsenal1112Data$Languages=="japanese", 1, 0)
Arsenal1112Data$turkish <- ifelse(Arsenal1112Data$Languages=="turkish", 1, 0)
Arsenal1112Data$asante <- ifelse(Arsenal1112Data$Languages=="asante", 1, 0)
Arsenal1112Data$persian <- ifelse(Arsenal1112Data$Languages=="persian", 1, 0)
Arsenal1112Data$maltese <- ifelse(Arsenal1112Data$Languages=="maltese", 1, 0)
Arsenal1112Data$romanian <- ifelse(Arsenal1112Data$Languages=="romanian", 1, 0)
Arsenal1112Data$slovak <- ifelse(Arsenal1112Data$Languages=="slovak", 1, 0)
Arsenal1112Data$PropLanguages <- 0
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "english", sum(Arsenal1112Data$english), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "french", sum(Arsenal1112Data$french), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "dutch", sum(Arsenal1112Data$dutch), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "portuguese", sum(Arsenal1112Data$portuguese), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "castilian", sum(Arsenal1112Data$castilian), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "spanish", sum(Arsenal1112Data$spanish), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "german", sum(Arsenal1112Data$german), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "arabic", sum(Arsenal1112Data$arabic), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "serbian", sum(Arsenal1112Data$serbian), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "korean", sum(Arsenal1112Data$korean), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "croatian", sum(Arsenal1112Data$croatian), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "italian", sum(Arsenal1112Data$italian), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "armenian", sum(Arsenal1112Data$armenian), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "icelandic", sum(Arsenal1112Data$icelandic), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "hebrew", sum(Arsenal1112Data$hebrew), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "bokmal", sum(Arsenal1112Data$bokmal), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "ukrainian", sum(Arsenal1112Data$ukrainian), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "russian", sum(Arsenal1112Data$russian), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "bosnian", sum(Arsenal1112Data$bosnian), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "czech", sum(Arsenal1112Data$czech), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "danish", sum(Arsenal1112Data$danish), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "swedish", sum(Arsenal1112Data$swedish), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "japanese", sum(Arsenal1112Data$japanese), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "turkish", sum(Arsenal1112Data$turkish), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "asante", sum(Arsenal1112Data$asante), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "persian", sum(Arsenal1112Data$persian), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "maltese", sum(Arsenal1112Data$maltese), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "romanian", sum(Arsenal1112Data$romanian), Arsenal1112Data$PropLanguages)
Arsenal1112Data$PropLanguages <- ifelse(Arsenal1112Data$Languages == "slovak", sum(Arsenal1112Data$slovak), Arsenal1112Data$PropLanguages)

Chelsea1112Data <- subset(PL1112SZN, Squad =="Chelsea")
Chelsea1112Data$english <- ifelse(Chelsea1112Data$Languages=="english", 1, 0)
Chelsea1112Data$french <- ifelse(Chelsea1112Data$Languages=="french", 1, 0)
Chelsea1112Data$dutch <- ifelse(Chelsea1112Data$Languages=="dutch", 1, 0)
Chelsea1112Data$portuguese <- ifelse(Chelsea1112Data$Languages=="portuguese", 1, 0)
Chelsea1112Data$castilian <- ifelse(Chelsea1112Data$Languages=="castilian", 1, 0)
Chelsea1112Data$spanish <- ifelse(Chelsea1112Data$Languages=="spanish", 1, 0)
Chelsea1112Data$german <- ifelse(Chelsea1112Data$Languages=="german", 1, 0)
Chelsea1112Data$arabic <- ifelse(Chelsea1112Data$Languages=="arabic", 1, 0)
Chelsea1112Data$serbian <- ifelse(Chelsea1112Data$Languages=="serbian", 1, 0)
Chelsea1112Data$korean <- ifelse(Chelsea1112Data$Languages=="korean", 1, 0)
Chelsea1112Data$croatian <- ifelse(Chelsea1112Data$Languages=="croatian", 1, 0)
Chelsea1112Data$italian <- ifelse(Chelsea1112Data$Languages=="italian", 1, 0)
Chelsea1112Data$armenian <- ifelse(Chelsea1112Data$Languages=="armenian", 1, 0)
Chelsea1112Data$icelandic <- ifelse(Chelsea1112Data$Languages=="icelandic", 1, 0)
Chelsea1112Data$hebrew <- ifelse(Chelsea1112Data$Languages=="hebrew", 1, 0)
Chelsea1112Data$bokmal <- ifelse(Chelsea1112Data$Languages=="bokmal", 1, 0)
Chelsea1112Data$ukrainian <- ifelse(Chelsea1112Data$Languages=="ukrainian", 1, 0)
Chelsea1112Data$russian <- ifelse(Chelsea1112Data$Languages=="russian", 1, 0)
Chelsea1112Data$bosnian <- ifelse(Chelsea1112Data$Languages=="bosnian", 1, 0)
Chelsea1112Data$czech <- ifelse(Chelsea1112Data$Languages=="czech", 1, 0)
Chelsea1112Data$danish <- ifelse(Chelsea1112Data$Languages=="danish", 1, 0)
Chelsea1112Data$swedish <- ifelse(Chelsea1112Data$Languages=="swedish", 1, 0)
Chelsea1112Data$japanese <- ifelse(Chelsea1112Data$Languages=="japanese", 1, 0)
Chelsea1112Data$turkish <- ifelse(Chelsea1112Data$Languages=="turkish", 1, 0)
Chelsea1112Data$asante <- ifelse(Chelsea1112Data$Languages=="asante", 1, 0)
Chelsea1112Data$persian <- ifelse(Chelsea1112Data$Languages=="persian", 1, 0)
Chelsea1112Data$maltese <- ifelse(Chelsea1112Data$Languages=="maltese", 1, 0)
Chelsea1112Data$romanian <- ifelse(Chelsea1112Data$Languages=="romanian", 1, 0)
Chelsea1112Data$slovak <- ifelse(Chelsea1112Data$Languages=="slovak", 1, 0)
Chelsea1112Data$PropLanguages <- 0
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "english", sum(Chelsea1112Data$english), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "french", sum(Chelsea1112Data$french), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "dutch", sum(Chelsea1112Data$dutch), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "portuguese", sum(Chelsea1112Data$portuguese), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "castilian", sum(Chelsea1112Data$castilian), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "spanish", sum(Chelsea1112Data$spanish), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "german", sum(Chelsea1112Data$german), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "arabic", sum(Chelsea1112Data$arabic), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "serbian", sum(Chelsea1112Data$serbian), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "korean", sum(Chelsea1112Data$korean), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "croatian", sum(Chelsea1112Data$croatian), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "italian", sum(Chelsea1112Data$italian), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "armenian", sum(Chelsea1112Data$armenian), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "icelandic", sum(Chelsea1112Data$icelandic), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "hebrew", sum(Chelsea1112Data$hebrew), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "bokmal", sum(Chelsea1112Data$bokmal), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "ukrainian", sum(Chelsea1112Data$ukrainian), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "russian", sum(Chelsea1112Data$russian), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "bosnian", sum(Chelsea1112Data$bosnian), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "czech", sum(Chelsea1112Data$czech), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "danish", sum(Chelsea1112Data$danish), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "swedish", sum(Chelsea1112Data$swedish), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "japanese", sum(Chelsea1112Data$japanese), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "turkish", sum(Chelsea1112Data$turkish), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "asante", sum(Chelsea1112Data$asante), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "persian", sum(Chelsea1112Data$persian), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "maltese", sum(Chelsea1112Data$maltese), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "romanian", sum(Chelsea1112Data$romanian), Chelsea1112Data$PropLanguages)
Chelsea1112Data$PropLanguages <- ifelse(Chelsea1112Data$Languages == "slovak", sum(Chelsea1112Data$slovak), Chelsea1112Data$PropLanguages)

Everton1112Data <- subset(PL1112SZN, Squad =="Everton")
Everton1112Data$english <- ifelse(Everton1112Data$Languages=="english", 1, 0)
Everton1112Data$french <- ifelse(Everton1112Data$Languages=="french", 1, 0)
Everton1112Data$dutch <- ifelse(Everton1112Data$Languages=="dutch", 1, 0)
Everton1112Data$portuguese <- ifelse(Everton1112Data$Languages=="portuguese", 1, 0)
Everton1112Data$castilian <- ifelse(Everton1112Data$Languages=="castilian", 1, 0)
Everton1112Data$spanish <- ifelse(Everton1112Data$Languages=="spanish", 1, 0)
Everton1112Data$german <- ifelse(Everton1112Data$Languages=="german", 1, 0)
Everton1112Data$arabic <- ifelse(Everton1112Data$Languages=="arabic", 1, 0)
Everton1112Data$serbian <- ifelse(Everton1112Data$Languages=="serbian", 1, 0)
Everton1112Data$korean <- ifelse(Everton1112Data$Languages=="korean", 1, 0)
Everton1112Data$croatian <- ifelse(Everton1112Data$Languages=="croatian", 1, 0)
Everton1112Data$italian <- ifelse(Everton1112Data$Languages=="italian", 1, 0)
Everton1112Data$armenian <- ifelse(Everton1112Data$Languages=="armenian", 1, 0)
Everton1112Data$icelandic <- ifelse(Everton1112Data$Languages=="icelandic", 1, 0)
Everton1112Data$hebrew <- ifelse(Everton1112Data$Languages=="hebrew", 1, 0)
Everton1112Data$bokmal <- ifelse(Everton1112Data$Languages=="bokmal", 1, 0)
Everton1112Data$ukrainian <- ifelse(Everton1112Data$Languages=="ukrainian", 1, 0)
Everton1112Data$russian <- ifelse(Everton1112Data$Languages=="russian", 1, 0)
Everton1112Data$bosnian <- ifelse(Everton1112Data$Languages=="bosnian", 1, 0)
Everton1112Data$czech <- ifelse(Everton1112Data$Languages=="czech", 1, 0)
Everton1112Data$danish <- ifelse(Everton1112Data$Languages=="danish", 1, 0)
Everton1112Data$swedish <- ifelse(Everton1112Data$Languages=="swedish", 1, 0)
Everton1112Data$japanese <- ifelse(Everton1112Data$Languages=="japanese", 1, 0)
Everton1112Data$turkish <- ifelse(Everton1112Data$Languages=="turkish", 1, 0)
Everton1112Data$asante <- ifelse(Everton1112Data$Languages=="asante", 1, 0)
Everton1112Data$persian <- ifelse(Everton1112Data$Languages=="persian", 1, 0)
Everton1112Data$maltese <- ifelse(Everton1112Data$Languages=="maltese", 1, 0)
Everton1112Data$romanian <- ifelse(Everton1112Data$Languages=="romanian", 1, 0)
Everton1112Data$slovak <- ifelse(Everton1112Data$Languages=="slovak", 1, 0)
Everton1112Data$PropLanguages <- 0
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "english", sum(Everton1112Data$english), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "french", sum(Everton1112Data$french), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "dutch", sum(Everton1112Data$dutch), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "portuguese", sum(Everton1112Data$portuguese), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "castilian", sum(Everton1112Data$castilian), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "spanish", sum(Everton1112Data$spanish), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "german", sum(Everton1112Data$german), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "arabic", sum(Everton1112Data$arabic), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "serbian", sum(Everton1112Data$serbian), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "korean", sum(Everton1112Data$korean), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "croatian", sum(Everton1112Data$croatian), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "italian", sum(Everton1112Data$italian), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "armenian", sum(Everton1112Data$armenian), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "icelandic", sum(Everton1112Data$icelandic), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "hebrew", sum(Everton1112Data$hebrew), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "bokmal", sum(Everton1112Data$bokmal), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "ukrainian", sum(Everton1112Data$ukrainian), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "russian", sum(Everton1112Data$russian), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "bosnian", sum(Everton1112Data$bosnian), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "czech", sum(Everton1112Data$czech), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "danish", sum(Everton1112Data$danish), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "swedish", sum(Everton1112Data$swedish), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "japanese", sum(Everton1112Data$japanese), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "turkish", sum(Everton1112Data$turkish), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "asante", sum(Everton1112Data$asante), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "persian", sum(Everton1112Data$persian), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "maltese", sum(Everton1112Data$maltese), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "romanian", sum(Everton1112Data$romanian), Everton1112Data$PropLanguages)
Everton1112Data$PropLanguages <- ifelse(Everton1112Data$Languages == "slovak", sum(Everton1112Data$slovak), Everton1112Data$PropLanguages)

Liverpool1112Data <- subset(PL1112SZN, Squad =="Liverpool")
Liverpool1112Data$english <- ifelse(Liverpool1112Data$Languages=="english", 1, 0)
Liverpool1112Data$french <- ifelse(Liverpool1112Data$Languages=="french", 1, 0)
Liverpool1112Data$dutch <- ifelse(Liverpool1112Data$Languages=="dutch", 1, 0)
Liverpool1112Data$portuguese <- ifelse(Liverpool1112Data$Languages=="portuguese", 1, 0)
Liverpool1112Data$castilian <- ifelse(Liverpool1112Data$Languages=="castilian", 1, 0)
Liverpool1112Data$spanish <- ifelse(Liverpool1112Data$Languages=="spanish", 1, 0)
Liverpool1112Data$german <- ifelse(Liverpool1112Data$Languages=="german", 1, 0)
Liverpool1112Data$arabic <- ifelse(Liverpool1112Data$Languages=="arabic", 1, 0)
Liverpool1112Data$serbian <- ifelse(Liverpool1112Data$Languages=="serbian", 1, 0)
Liverpool1112Data$korean <- ifelse(Liverpool1112Data$Languages=="korean", 1, 0)
Liverpool1112Data$croatian <- ifelse(Liverpool1112Data$Languages=="croatian", 1, 0)
Liverpool1112Data$italian <- ifelse(Liverpool1112Data$Languages=="italian", 1, 0)
Liverpool1112Data$armenian <- ifelse(Liverpool1112Data$Languages=="armenian", 1, 0)
Liverpool1112Data$icelandic <- ifelse(Liverpool1112Data$Languages=="icelandic", 1, 0)
Liverpool1112Data$hebrew <- ifelse(Liverpool1112Data$Languages=="hebrew", 1, 0)
Liverpool1112Data$bokmal <- ifelse(Liverpool1112Data$Languages=="bokmal", 1, 0)
Liverpool1112Data$ukrainian <- ifelse(Liverpool1112Data$Languages=="ukrainian", 1, 0)
Liverpool1112Data$russian <- ifelse(Liverpool1112Data$Languages=="russian", 1, 0)
Liverpool1112Data$bosnian <- ifelse(Liverpool1112Data$Languages=="bosnian", 1, 0)
Liverpool1112Data$czech <- ifelse(Liverpool1112Data$Languages=="czech", 1, 0)
Liverpool1112Data$danish <- ifelse(Liverpool1112Data$Languages=="danish", 1, 0)
Liverpool1112Data$swedish <- ifelse(Liverpool1112Data$Languages=="swedish", 1, 0)
Liverpool1112Data$japanese <- ifelse(Liverpool1112Data$Languages=="japanese", 1, 0)
Liverpool1112Data$turkish <- ifelse(Liverpool1112Data$Languages=="turkish", 1, 0)
Liverpool1112Data$asante <- ifelse(Liverpool1112Data$Languages=="asante", 1, 0)
Liverpool1112Data$persian <- ifelse(Liverpool1112Data$Languages=="persian", 1, 0)
Liverpool1112Data$maltese <- ifelse(Liverpool1112Data$Languages=="maltese", 1, 0)
Liverpool1112Data$romanian <- ifelse(Liverpool1112Data$Languages=="romanian", 1, 0)
Liverpool1112Data$slovak <- ifelse(Liverpool1112Data$Languages=="slovak", 1, 0)
Liverpool1112Data$PropLanguages <- 0
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "english", sum(Liverpool1112Data$english), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "french", sum(Liverpool1112Data$french), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "dutch", sum(Liverpool1112Data$dutch), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "portuguese", sum(Liverpool1112Data$portuguese), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "castilian", sum(Liverpool1112Data$castilian), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "spanish", sum(Liverpool1112Data$spanish), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "german", sum(Liverpool1112Data$german), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "arabic", sum(Liverpool1112Data$arabic), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "serbian", sum(Liverpool1112Data$serbian), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "korean", sum(Liverpool1112Data$korean), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "croatian", sum(Liverpool1112Data$croatian), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "italian", sum(Liverpool1112Data$italian), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "armenian", sum(Liverpool1112Data$armenian), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "icelandic", sum(Liverpool1112Data$icelandic), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "hebrew", sum(Liverpool1112Data$hebrew), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "bokmal", sum(Liverpool1112Data$bokmal), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "ukrainian", sum(Liverpool1112Data$ukrainian), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "russian", sum(Liverpool1112Data$russian), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "bosnian", sum(Liverpool1112Data$bosnian), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "czech", sum(Liverpool1112Data$czech), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "danish", sum(Liverpool1112Data$danish), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "swedish", sum(Liverpool1112Data$swedish), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "japanese", sum(Liverpool1112Data$japanese), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "turkish", sum(Liverpool1112Data$turkish), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "asante", sum(Liverpool1112Data$asante), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "persian", sum(Liverpool1112Data$persian), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "maltese", sum(Liverpool1112Data$maltese), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "romanian", sum(Liverpool1112Data$romanian), Liverpool1112Data$PropLanguages)
Liverpool1112Data$PropLanguages <- ifelse(Liverpool1112Data$Languages == "slovak", sum(Liverpool1112Data$slovak), Liverpool1112Data$PropLanguages)

ManchesterCity1112Data <- subset(PL1112SZN, Squad =="Manchester City")
ManchesterCity1112Data$english <- ifelse(ManchesterCity1112Data$Languages=="english", 1, 0)
ManchesterCity1112Data$french <- ifelse(ManchesterCity1112Data$Languages=="french", 1, 0)
ManchesterCity1112Data$dutch <- ifelse(ManchesterCity1112Data$Languages=="dutch", 1, 0)
ManchesterCity1112Data$portuguese <- ifelse(ManchesterCity1112Data$Languages=="portuguese", 1, 0)
ManchesterCity1112Data$castilian <- ifelse(ManchesterCity1112Data$Languages=="castilian", 1, 0)
ManchesterCity1112Data$spanish <- ifelse(ManchesterCity1112Data$Languages=="spanish", 1, 0)
ManchesterCity1112Data$german <- ifelse(ManchesterCity1112Data$Languages=="german", 1, 0)
ManchesterCity1112Data$arabic <- ifelse(ManchesterCity1112Data$Languages=="arabic", 1, 0)
ManchesterCity1112Data$serbian <- ifelse(ManchesterCity1112Data$Languages=="serbian", 1, 0)
ManchesterCity1112Data$korean <- ifelse(ManchesterCity1112Data$Languages=="korean", 1, 0)
ManchesterCity1112Data$croatian <- ifelse(ManchesterCity1112Data$Languages=="croatian", 1, 0)
ManchesterCity1112Data$italian <- ifelse(ManchesterCity1112Data$Languages=="italian", 1, 0)
ManchesterCity1112Data$armenian <- ifelse(ManchesterCity1112Data$Languages=="armenian", 1, 0)
ManchesterCity1112Data$icelandic <- ifelse(ManchesterCity1112Data$Languages=="icelandic", 1, 0)
ManchesterCity1112Data$hebrew <- ifelse(ManchesterCity1112Data$Languages=="hebrew", 1, 0)
ManchesterCity1112Data$bokmal <- ifelse(ManchesterCity1112Data$Languages=="bokmal", 1, 0)
ManchesterCity1112Data$ukrainian <- ifelse(ManchesterCity1112Data$Languages=="ukrainian", 1, 0)
ManchesterCity1112Data$russian <- ifelse(ManchesterCity1112Data$Languages=="russian", 1, 0)
ManchesterCity1112Data$bosnian <- ifelse(ManchesterCity1112Data$Languages=="bosnian", 1, 0)
ManchesterCity1112Data$czech <- ifelse(ManchesterCity1112Data$Languages=="czech", 1, 0)
ManchesterCity1112Data$danish <- ifelse(ManchesterCity1112Data$Languages=="danish", 1, 0)
ManchesterCity1112Data$swedish <- ifelse(ManchesterCity1112Data$Languages=="swedish", 1, 0)
ManchesterCity1112Data$japanese <- ifelse(ManchesterCity1112Data$Languages=="japanese", 1, 0)
ManchesterCity1112Data$turkish <- ifelse(ManchesterCity1112Data$Languages=="turkish", 1, 0)
ManchesterCity1112Data$asante <- ifelse(ManchesterCity1112Data$Languages=="asante", 1, 0)
ManchesterCity1112Data$persian <- ifelse(ManchesterCity1112Data$Languages=="persian", 1, 0)
ManchesterCity1112Data$maltese <- ifelse(ManchesterCity1112Data$Languages=="maltese", 1, 0)
ManchesterCity1112Data$romanian <- ifelse(ManchesterCity1112Data$Languages=="romanian", 1, 0)
ManchesterCity1112Data$slovak <- ifelse(ManchesterCity1112Data$Languages=="slovak", 1, 0)
ManchesterCity1112Data$PropLanguages <- 0
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "english", sum(ManchesterCity1112Data$english), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "french", sum(ManchesterCity1112Data$french), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "dutch", sum(ManchesterCity1112Data$dutch), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "portuguese", sum(ManchesterCity1112Data$portuguese), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "castilian", sum(ManchesterCity1112Data$castilian), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "spanish", sum(ManchesterCity1112Data$spanish), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "german", sum(ManchesterCity1112Data$german), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "arabic", sum(ManchesterCity1112Data$arabic), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "serbian", sum(ManchesterCity1112Data$serbian), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "korean", sum(ManchesterCity1112Data$korean), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "croatian", sum(ManchesterCity1112Data$croatian), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "italian", sum(ManchesterCity1112Data$italian), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "armenian", sum(ManchesterCity1112Data$armenian), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "icelandic", sum(ManchesterCity1112Data$icelandic), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "hebrew", sum(ManchesterCity1112Data$hebrew), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "bokmal", sum(ManchesterCity1112Data$bokmal), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "ukrainian", sum(ManchesterCity1112Data$ukrainian), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "russian", sum(ManchesterCity1112Data$russian), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "bosnian", sum(ManchesterCity1112Data$bosnian), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "czech", sum(ManchesterCity1112Data$czech), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "danish", sum(ManchesterCity1112Data$danish), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "swedish", sum(ManchesterCity1112Data$swedish), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "japanese", sum(ManchesterCity1112Data$japanese), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "turkish", sum(ManchesterCity1112Data$turkish), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "asante", sum(ManchesterCity1112Data$asante), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "persian", sum(ManchesterCity1112Data$persian), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "maltese", sum(ManchesterCity1112Data$maltese), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "romanian", sum(ManchesterCity1112Data$romanian), ManchesterCity1112Data$PropLanguages)
ManchesterCity1112Data$PropLanguages <- ifelse(ManchesterCity1112Data$Languages == "slovak", sum(ManchesterCity1112Data$slovak), ManchesterCity1112Data$PropLanguages)

ManchesterUtd1112Data <- subset(PL1112SZN, Squad =="Manchester Utd")
ManchesterUtd1112Data$english <- ifelse(ManchesterUtd1112Data$Languages=="english", 1, 0)
ManchesterUtd1112Data$french <- ifelse(ManchesterUtd1112Data$Languages=="french", 1, 0)
ManchesterUtd1112Data$dutch <- ifelse(ManchesterUtd1112Data$Languages=="dutch", 1, 0)
ManchesterUtd1112Data$portuguese <- ifelse(ManchesterUtd1112Data$Languages=="portuguese", 1, 0)
ManchesterUtd1112Data$castilian <- ifelse(ManchesterUtd1112Data$Languages=="castilian", 1, 0)
ManchesterUtd1112Data$spanish <- ifelse(ManchesterUtd1112Data$Languages=="spanish", 1, 0)
ManchesterUtd1112Data$german <- ifelse(ManchesterUtd1112Data$Languages=="german", 1, 0)
ManchesterUtd1112Data$arabic <- ifelse(ManchesterUtd1112Data$Languages=="arabic", 1, 0)
ManchesterUtd1112Data$serbian <- ifelse(ManchesterUtd1112Data$Languages=="serbian", 1, 0)
ManchesterUtd1112Data$korean <- ifelse(ManchesterUtd1112Data$Languages=="korean", 1, 0)
ManchesterUtd1112Data$croatian <- ifelse(ManchesterUtd1112Data$Languages=="croatian", 1, 0)
ManchesterUtd1112Data$italian <- ifelse(ManchesterUtd1112Data$Languages=="italian", 1, 0)
ManchesterUtd1112Data$armenian <- ifelse(ManchesterUtd1112Data$Languages=="armenian", 1, 0)
ManchesterUtd1112Data$icelandic <- ifelse(ManchesterUtd1112Data$Languages=="icelandic", 1, 0)
ManchesterUtd1112Data$hebrew <- ifelse(ManchesterUtd1112Data$Languages=="hebrew", 1, 0)
ManchesterUtd1112Data$bokmal <- ifelse(ManchesterUtd1112Data$Languages=="bokmal", 1, 0)
ManchesterUtd1112Data$ukrainian <- ifelse(ManchesterUtd1112Data$Languages=="ukrainian", 1, 0)
ManchesterUtd1112Data$russian <- ifelse(ManchesterUtd1112Data$Languages=="russian", 1, 0)
ManchesterUtd1112Data$bosnian <- ifelse(ManchesterUtd1112Data$Languages=="bosnian", 1, 0)
ManchesterUtd1112Data$czech <- ifelse(ManchesterUtd1112Data$Languages=="czech", 1, 0)
ManchesterUtd1112Data$danish <- ifelse(ManchesterUtd1112Data$Languages=="danish", 1, 0)
ManchesterUtd1112Data$swedish <- ifelse(ManchesterUtd1112Data$Languages=="swedish", 1, 0)
ManchesterUtd1112Data$japanese <- ifelse(ManchesterUtd1112Data$Languages=="japanese", 1, 0)
ManchesterUtd1112Data$turkish <- ifelse(ManchesterUtd1112Data$Languages=="turkish", 1, 0)
ManchesterUtd1112Data$asante <- ifelse(ManchesterUtd1112Data$Languages=="asante", 1, 0)
ManchesterUtd1112Data$persian <- ifelse(ManchesterUtd1112Data$Languages=="persian", 1, 0)
ManchesterUtd1112Data$maltese <- ifelse(ManchesterUtd1112Data$Languages=="maltese", 1, 0)
ManchesterUtd1112Data$romanian <- ifelse(ManchesterUtd1112Data$Languages=="romanian", 1, 0)
ManchesterUtd1112Data$slovak <- ifelse(ManchesterUtd1112Data$Languages=="slovak", 1, 0)
ManchesterUtd1112Data$PropLanguages <- 0
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "english", sum(ManchesterUtd1112Data$english), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "french", sum(ManchesterUtd1112Data$french), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "dutch", sum(ManchesterUtd1112Data$dutch), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "portuguese", sum(ManchesterUtd1112Data$portuguese), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "castilian", sum(ManchesterUtd1112Data$castilian), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "spanish", sum(ManchesterUtd1112Data$spanish), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "german", sum(ManchesterUtd1112Data$german), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "arabic", sum(ManchesterUtd1112Data$arabic), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "serbian", sum(ManchesterUtd1112Data$serbian), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "korean", sum(ManchesterUtd1112Data$korean), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "croatian", sum(ManchesterUtd1112Data$croatian), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "italian", sum(ManchesterUtd1112Data$italian), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "armenian", sum(ManchesterUtd1112Data$armenian), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "icelandic", sum(ManchesterUtd1112Data$icelandic), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "hebrew", sum(ManchesterUtd1112Data$hebrew), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "bokmal", sum(ManchesterUtd1112Data$bokmal), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "ukrainian", sum(ManchesterUtd1112Data$ukrainian), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "russian", sum(ManchesterUtd1112Data$russian), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "bosnian", sum(ManchesterUtd1112Data$bosnian), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "czech", sum(ManchesterUtd1112Data$czech), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "danish", sum(ManchesterUtd1112Data$danish), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "swedish", sum(ManchesterUtd1112Data$swedish), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "japanese", sum(ManchesterUtd1112Data$japanese), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "turkish", sum(ManchesterUtd1112Data$turkish), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "asante", sum(ManchesterUtd1112Data$asante), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "persian", sum(ManchesterUtd1112Data$persian), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "maltese", sum(ManchesterUtd1112Data$maltese), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "romanian", sum(ManchesterUtd1112Data$romanian), ManchesterUtd1112Data$PropLanguages)
ManchesterUtd1112Data$PropLanguages <- ifelse(ManchesterUtd1112Data$Languages == "slovak", sum(ManchesterUtd1112Data$slovak), ManchesterUtd1112Data$PropLanguages)

Tottenham1112Data <- subset(PL1112SZN, Squad =="Tottenham")
Tottenham1112Data$english <- ifelse(Tottenham1112Data$Languages=="english", 1, 0)
Tottenham1112Data$french <- ifelse(Tottenham1112Data$Languages=="french", 1, 0)
Tottenham1112Data$dutch <- ifelse(Tottenham1112Data$Languages=="dutch", 1, 0)
Tottenham1112Data$portuguese <- ifelse(Tottenham1112Data$Languages=="portuguese", 1, 0)
Tottenham1112Data$castilian <- ifelse(Tottenham1112Data$Languages=="castilian", 1, 0)
Tottenham1112Data$spanish <- ifelse(Tottenham1112Data$Languages=="spanish", 1, 0)
Tottenham1112Data$german <- ifelse(Tottenham1112Data$Languages=="german", 1, 0)
Tottenham1112Data$arabic <- ifelse(Tottenham1112Data$Languages=="arabic", 1, 0)
Tottenham1112Data$serbian <- ifelse(Tottenham1112Data$Languages=="serbian", 1, 0)
Tottenham1112Data$korean <- ifelse(Tottenham1112Data$Languages=="korean", 1, 0)
Tottenham1112Data$croatian <- ifelse(Tottenham1112Data$Languages=="croatian", 1, 0)
Tottenham1112Data$italian <- ifelse(Tottenham1112Data$Languages=="italian", 1, 0)
Tottenham1112Data$armenian <- ifelse(Tottenham1112Data$Languages=="armenian", 1, 0)
Tottenham1112Data$icelandic <- ifelse(Tottenham1112Data$Languages=="icelandic", 1, 0)
Tottenham1112Data$hebrew <- ifelse(Tottenham1112Data$Languages=="hebrew", 1, 0)
Tottenham1112Data$bokmal <- ifelse(Tottenham1112Data$Languages=="bokmal", 1, 0)
Tottenham1112Data$ukrainian <- ifelse(Tottenham1112Data$Languages=="ukrainian", 1, 0)
Tottenham1112Data$russian <- ifelse(Tottenham1112Data$Languages=="russian", 1, 0)
Tottenham1112Data$bosnian <- ifelse(Tottenham1112Data$Languages=="bosnian", 1, 0)
Tottenham1112Data$czech <- ifelse(Tottenham1112Data$Languages=="czech", 1, 0)
Tottenham1112Data$danish <- ifelse(Tottenham1112Data$Languages=="danish", 1, 0)
Tottenham1112Data$swedish <- ifelse(Tottenham1112Data$Languages=="swedish", 1, 0)
Tottenham1112Data$japanese <- ifelse(Tottenham1112Data$Languages=="japanese", 1, 0)
Tottenham1112Data$turkish <- ifelse(Tottenham1112Data$Languages=="turkish", 1, 0)
Tottenham1112Data$asante <- ifelse(Tottenham1112Data$Languages=="asante", 1, 0)
Tottenham1112Data$persian <- ifelse(Tottenham1112Data$Languages=="persian", 1, 0)
Tottenham1112Data$maltese <- ifelse(Tottenham1112Data$Languages=="maltese", 1, 0)
Tottenham1112Data$romanian <- ifelse(Tottenham1112Data$Languages=="romanian", 1, 0)
Tottenham1112Data$slovak <- ifelse(Tottenham1112Data$Languages=="slovak", 1, 0)
Tottenham1112Data$PropLanguages <- 0
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "english", sum(Tottenham1112Data$english), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "french", sum(Tottenham1112Data$french), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "dutch", sum(Tottenham1112Data$dutch), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "portuguese", sum(Tottenham1112Data$portuguese), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "castilian", sum(Tottenham1112Data$castilian), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "spanish", sum(Tottenham1112Data$spanish), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "german", sum(Tottenham1112Data$german), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "arabic", sum(Tottenham1112Data$arabic), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "serbian", sum(Tottenham1112Data$serbian), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "korean", sum(Tottenham1112Data$korean), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "croatian", sum(Tottenham1112Data$croatian), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "italian", sum(Tottenham1112Data$italian), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "armenian", sum(Tottenham1112Data$armenian), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "icelandic", sum(Tottenham1112Data$icelandic), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "hebrew", sum(Tottenham1112Data$hebrew), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "bokmal", sum(Tottenham1112Data$bokmal), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "ukrainian", sum(Tottenham1112Data$ukrainian), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "russian", sum(Tottenham1112Data$russian), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "bosnian", sum(Tottenham1112Data$bosnian), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "czech", sum(Tottenham1112Data$czech), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "danish", sum(Tottenham1112Data$danish), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "swedish", sum(Tottenham1112Data$swedish), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "japanese", sum(Tottenham1112Data$japanese), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "turkish", sum(Tottenham1112Data$turkish), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "asante", sum(Tottenham1112Data$asante), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "persian", sum(Tottenham1112Data$persian), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "maltese", sum(Tottenham1112Data$maltese), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "romanian", sum(Tottenham1112Data$romanian), Tottenham1112Data$PropLanguages)
Tottenham1112Data$PropLanguages <- ifelse(Tottenham1112Data$Languages == "slovak", sum(Tottenham1112Data$slovak), Tottenham1112Data$PropLanguages)

PL1112SZN <- rbind(Arsenal1112Data, Chelsea1112Data)
PL1112SZN <- rbind(PL1112SZN, Everton1112Data)
PL1112SZN <- rbind(PL1112SZN, Liverpool1112Data)
PL1112SZN <- rbind(PL1112SZN, ManchesterCity1112Data)
PL1112SZN <- rbind(PL1112SZN, ManchesterUtd1112Data)
PL1112SZN <- rbind(PL1112SZN, Tottenham1112Data)

PL1011SZN$english <- 0
PL1011SZN$french <- 0
PL1011SZN$portuguese <- 0
PL1011SZN$castilian <- 0
PL1011SZN$dutch <- 0
PL1011SZN$german <- 0
PL1011SZN$spanish <- 0
PL1011SZN$arabic <- 0
PL1011SZN$serbian <- 0
PL1011SZN$korean <- 0
PL1011SZN$croatian <- 0
PL1011SZN$italian <- 0
PL1011SZN$icelandic <- 0
PL1011SZN$hebrew <- 0
PL1011SZN$bokmal <- 0
PL1011SZN$ukrainian <- 0
PL1011SZN$russian <- 0
PL1011SZN$bosnian <- 0
PL1011SZN$czech <- 0
PL1011SZN$danish <- 0
PL1011SZN$armenian <- 0
PL1011SZN$turkish <- 0
PL1011SZN$asante <- 0
PL1011SZN$persian <- 0
PL1011SZN$japanese <- 0
PL1011SZN$maltese <- 0
PL1011SZN$romanian <- 0
PL1011SZN$slovak <- 0
PL1011SZN$swedish <- 0

Arsenal1011Data <- subset(PL1011SZN, Squad =="Arsenal")
Arsenal1011Data$english <- ifelse(Arsenal1011Data$Languages=="english", 1, 0)
Arsenal1011Data$french <- ifelse(Arsenal1011Data$Languages=="french", 1, 0)
Arsenal1011Data$dutch <- ifelse(Arsenal1011Data$Languages=="dutch", 1, 0)
Arsenal1011Data$portuguese <- ifelse(Arsenal1011Data$Languages=="portuguese", 1, 0)
Arsenal1011Data$castilian <- ifelse(Arsenal1011Data$Languages=="castilian", 1, 0)
Arsenal1011Data$spanish <- ifelse(Arsenal1011Data$Languages=="spanish", 1, 0)
Arsenal1011Data$german <- ifelse(Arsenal1011Data$Languages=="german", 1, 0)
Arsenal1011Data$arabic <- ifelse(Arsenal1011Data$Languages=="arabic", 1, 0)
Arsenal1011Data$serbian <- ifelse(Arsenal1011Data$Languages=="serbian", 1, 0)
Arsenal1011Data$korean <- ifelse(Arsenal1011Data$Languages=="korean", 1, 0)
Arsenal1011Data$croatian <- ifelse(Arsenal1011Data$Languages=="croatian", 1, 0)
Arsenal1011Data$italian <- ifelse(Arsenal1011Data$Languages=="italian", 1, 0)
Arsenal1011Data$armenian <- ifelse(Arsenal1011Data$Languages=="armenian", 1, 0)
Arsenal1011Data$icelandic <- ifelse(Arsenal1011Data$Languages=="icelandic", 1, 0)
Arsenal1011Data$hebrew <- ifelse(Arsenal1011Data$Languages=="hebrew", 1, 0)
Arsenal1011Data$bokmal <- ifelse(Arsenal1011Data$Languages=="bokmal", 1, 0)
Arsenal1011Data$ukrainian <- ifelse(Arsenal1011Data$Languages=="ukrainian", 1, 0)
Arsenal1011Data$russian <- ifelse(Arsenal1011Data$Languages=="russian", 1, 0)
Arsenal1011Data$bosnian <- ifelse(Arsenal1011Data$Languages=="bosnian", 1, 0)
Arsenal1011Data$czech <- ifelse(Arsenal1011Data$Languages=="czech", 1, 0)
Arsenal1011Data$danish <- ifelse(Arsenal1011Data$Languages=="danish", 1, 0)
Arsenal1011Data$swedish <- ifelse(Arsenal1011Data$Languages=="swedish", 1, 0)
Arsenal1011Data$japanese <- ifelse(Arsenal1011Data$Languages=="japanese", 1, 0)
Arsenal1011Data$turkish <- ifelse(Arsenal1011Data$Languages=="turkish", 1, 0)
Arsenal1011Data$asante <- ifelse(Arsenal1011Data$Languages=="asante", 1, 0)
Arsenal1011Data$persian <- ifelse(Arsenal1011Data$Languages=="persian", 1, 0)
Arsenal1011Data$maltese <- ifelse(Arsenal1011Data$Languages=="maltese", 1, 0)
Arsenal1011Data$romanian <- ifelse(Arsenal1011Data$Languages=="romanian", 1, 0)
Arsenal1011Data$slovak <- ifelse(Arsenal1011Data$Languages=="slovak", 1, 0)
Arsenal1011Data$PropLanguages <- 0
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "english", sum(Arsenal1011Data$english), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "french", sum(Arsenal1011Data$french), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "dutch", sum(Arsenal1011Data$dutch), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "portuguese", sum(Arsenal1011Data$portuguese), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "castilian", sum(Arsenal1011Data$castilian), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "spanish", sum(Arsenal1011Data$spanish), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "german", sum(Arsenal1011Data$german), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "arabic", sum(Arsenal1011Data$arabic), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "serbian", sum(Arsenal1011Data$serbian), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "korean", sum(Arsenal1011Data$korean), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "croatian", sum(Arsenal1011Data$croatian), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "italian", sum(Arsenal1011Data$italian), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "armenian", sum(Arsenal1011Data$armenian), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "icelandic", sum(Arsenal1011Data$icelandic), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "hebrew", sum(Arsenal1011Data$hebrew), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "bokmal", sum(Arsenal1011Data$bokmal), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "ukrainian", sum(Arsenal1011Data$ukrainian), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "russian", sum(Arsenal1011Data$russian), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "bosnian", sum(Arsenal1011Data$bosnian), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "czech", sum(Arsenal1011Data$czech), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "danish", sum(Arsenal1011Data$danish), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "swedish", sum(Arsenal1011Data$swedish), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "japanese", sum(Arsenal1011Data$japanese), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "turkish", sum(Arsenal1011Data$turkish), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "asante", sum(Arsenal1011Data$asante), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "persian", sum(Arsenal1011Data$persian), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "maltese", sum(Arsenal1011Data$maltese), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "romanian", sum(Arsenal1011Data$romanian), Arsenal1011Data$PropLanguages)
Arsenal1011Data$PropLanguages <- ifelse(Arsenal1011Data$Languages == "slovak", sum(Arsenal1011Data$slovak), Arsenal1011Data$PropLanguages)

Chelsea1011Data <- subset(PL1011SZN, Squad =="Chelsea")
Chelsea1011Data$english <- ifelse(Chelsea1011Data$Languages=="english", 1, 0)
Chelsea1011Data$french <- ifelse(Chelsea1011Data$Languages=="french", 1, 0)
Chelsea1011Data$dutch <- ifelse(Chelsea1011Data$Languages=="dutch", 1, 0)
Chelsea1011Data$portuguese <- ifelse(Chelsea1011Data$Languages=="portuguese", 1, 0)
Chelsea1011Data$castilian <- ifelse(Chelsea1011Data$Languages=="castilian", 1, 0)
Chelsea1011Data$spanish <- ifelse(Chelsea1011Data$Languages=="spanish", 1, 0)
Chelsea1011Data$german <- ifelse(Chelsea1011Data$Languages=="german", 1, 0)
Chelsea1011Data$arabic <- ifelse(Chelsea1011Data$Languages=="arabic", 1, 0)
Chelsea1011Data$serbian <- ifelse(Chelsea1011Data$Languages=="serbian", 1, 0)
Chelsea1011Data$korean <- ifelse(Chelsea1011Data$Languages=="korean", 1, 0)
Chelsea1011Data$croatian <- ifelse(Chelsea1011Data$Languages=="croatian", 1, 0)
Chelsea1011Data$italian <- ifelse(Chelsea1011Data$Languages=="italian", 1, 0)
Chelsea1011Data$armenian <- ifelse(Chelsea1011Data$Languages=="armenian", 1, 0)
Chelsea1011Data$icelandic <- ifelse(Chelsea1011Data$Languages=="icelandic", 1, 0)
Chelsea1011Data$hebrew <- ifelse(Chelsea1011Data$Languages=="hebrew", 1, 0)
Chelsea1011Data$bokmal <- ifelse(Chelsea1011Data$Languages=="bokmal", 1, 0)
Chelsea1011Data$ukrainian <- ifelse(Chelsea1011Data$Languages=="ukrainian", 1, 0)
Chelsea1011Data$russian <- ifelse(Chelsea1011Data$Languages=="russian", 1, 0)
Chelsea1011Data$bosnian <- ifelse(Chelsea1011Data$Languages=="bosnian", 1, 0)
Chelsea1011Data$czech <- ifelse(Chelsea1011Data$Languages=="czech", 1, 0)
Chelsea1011Data$danish <- ifelse(Chelsea1011Data$Languages=="danish", 1, 0)
Chelsea1011Data$swedish <- ifelse(Chelsea1011Data$Languages=="swedish", 1, 0)
Chelsea1011Data$japanese <- ifelse(Chelsea1011Data$Languages=="japanese", 1, 0)
Chelsea1011Data$turkish <- ifelse(Chelsea1011Data$Languages=="turkish", 1, 0)
Chelsea1011Data$asante <- ifelse(Chelsea1011Data$Languages=="asante", 1, 0)
Chelsea1011Data$persian <- ifelse(Chelsea1011Data$Languages=="persian", 1, 0)
Chelsea1011Data$maltese <- ifelse(Chelsea1011Data$Languages=="maltese", 1, 0)
Chelsea1011Data$romanian <- ifelse(Chelsea1011Data$Languages=="romanian", 1, 0)
Chelsea1011Data$slovak <- ifelse(Chelsea1011Data$Languages=="slovak", 1, 0)
Chelsea1011Data$PropLanguages <- 0
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "english", sum(Chelsea1011Data$english), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "french", sum(Chelsea1011Data$french), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "dutch", sum(Chelsea1011Data$dutch), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "portuguese", sum(Chelsea1011Data$portuguese), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "castilian", sum(Chelsea1011Data$castilian), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "spanish", sum(Chelsea1011Data$spanish), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "german", sum(Chelsea1011Data$german), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "arabic", sum(Chelsea1011Data$arabic), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "serbian", sum(Chelsea1011Data$serbian), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "korean", sum(Chelsea1011Data$korean), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "croatian", sum(Chelsea1011Data$croatian), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "italian", sum(Chelsea1011Data$italian), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "armenian", sum(Chelsea1011Data$armenian), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "icelandic", sum(Chelsea1011Data$icelandic), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "hebrew", sum(Chelsea1011Data$hebrew), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "bokmal", sum(Chelsea1011Data$bokmal), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "ukrainian", sum(Chelsea1011Data$ukrainian), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "russian", sum(Chelsea1011Data$russian), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "bosnian", sum(Chelsea1011Data$bosnian), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "czech", sum(Chelsea1011Data$czech), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "danish", sum(Chelsea1011Data$danish), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "swedish", sum(Chelsea1011Data$swedish), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "japanese", sum(Chelsea1011Data$japanese), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "turkish", sum(Chelsea1011Data$turkish), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "asante", sum(Chelsea1011Data$asante), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "persian", sum(Chelsea1011Data$persian), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "maltese", sum(Chelsea1011Data$maltese), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "romanian", sum(Chelsea1011Data$romanian), Chelsea1011Data$PropLanguages)
Chelsea1011Data$PropLanguages <- ifelse(Chelsea1011Data$Languages == "slovak", sum(Chelsea1011Data$slovak), Chelsea1011Data$PropLanguages)

Everton1011Data <- subset(PL1011SZN, Squad =="Everton")
Everton1011Data$english <- ifelse(Everton1011Data$Languages=="english", 1, 0)
Everton1011Data$french <- ifelse(Everton1011Data$Languages=="french", 1, 0)
Everton1011Data$dutch <- ifelse(Everton1011Data$Languages=="dutch", 1, 0)
Everton1011Data$portuguese <- ifelse(Everton1011Data$Languages=="portuguese", 1, 0)
Everton1011Data$castilian <- ifelse(Everton1011Data$Languages=="castilian", 1, 0)
Everton1011Data$spanish <- ifelse(Everton1011Data$Languages=="spanish", 1, 0)
Everton1011Data$german <- ifelse(Everton1011Data$Languages=="german", 1, 0)
Everton1011Data$arabic <- ifelse(Everton1011Data$Languages=="arabic", 1, 0)
Everton1011Data$serbian <- ifelse(Everton1011Data$Languages=="serbian", 1, 0)
Everton1011Data$korean <- ifelse(Everton1011Data$Languages=="korean", 1, 0)
Everton1011Data$croatian <- ifelse(Everton1011Data$Languages=="croatian", 1, 0)
Everton1011Data$italian <- ifelse(Everton1011Data$Languages=="italian", 1, 0)
Everton1011Data$armenian <- ifelse(Everton1011Data$Languages=="armenian", 1, 0)
Everton1011Data$icelandic <- ifelse(Everton1011Data$Languages=="icelandic", 1, 0)
Everton1011Data$hebrew <- ifelse(Everton1011Data$Languages=="hebrew", 1, 0)
Everton1011Data$bokmal <- ifelse(Everton1011Data$Languages=="bokmal", 1, 0)
Everton1011Data$ukrainian <- ifelse(Everton1011Data$Languages=="ukrainian", 1, 0)
Everton1011Data$russian <- ifelse(Everton1011Data$Languages=="russian", 1, 0)
Everton1011Data$bosnian <- ifelse(Everton1011Data$Languages=="bosnian", 1, 0)
Everton1011Data$czech <- ifelse(Everton1011Data$Languages=="czech", 1, 0)
Everton1011Data$danish <- ifelse(Everton1011Data$Languages=="danish", 1, 0)
Everton1011Data$swedish <- ifelse(Everton1011Data$Languages=="swedish", 1, 0)
Everton1011Data$japanese <- ifelse(Everton1011Data$Languages=="japanese", 1, 0)
Everton1011Data$turkish <- ifelse(Everton1011Data$Languages=="turkish", 1, 0)
Everton1011Data$asante <- ifelse(Everton1011Data$Languages=="asante", 1, 0)
Everton1011Data$persian <- ifelse(Everton1011Data$Languages=="persian", 1, 0)
Everton1011Data$maltese <- ifelse(Everton1011Data$Languages=="maltese", 1, 0)
Everton1011Data$romanian <- ifelse(Everton1011Data$Languages=="romanian", 1, 0)
Everton1011Data$slovak <- ifelse(Everton1011Data$Languages=="slovak", 1, 0)
Everton1011Data$PropLanguages <- 0
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "english", sum(Everton1011Data$english), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "french", sum(Everton1011Data$french), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "dutch", sum(Everton1011Data$dutch), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "portuguese", sum(Everton1011Data$portuguese), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "castilian", sum(Everton1011Data$castilian), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "spanish", sum(Everton1011Data$spanish), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "german", sum(Everton1011Data$german), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "arabic", sum(Everton1011Data$arabic), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "serbian", sum(Everton1011Data$serbian), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "korean", sum(Everton1011Data$korean), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "croatian", sum(Everton1011Data$croatian), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "italian", sum(Everton1011Data$italian), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "armenian", sum(Everton1011Data$armenian), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "icelandic", sum(Everton1011Data$icelandic), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "hebrew", sum(Everton1011Data$hebrew), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "bokmal", sum(Everton1011Data$bokmal), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "ukrainian", sum(Everton1011Data$ukrainian), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "russian", sum(Everton1011Data$russian), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "bosnian", sum(Everton1011Data$bosnian), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "czech", sum(Everton1011Data$czech), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "danish", sum(Everton1011Data$danish), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "swedish", sum(Everton1011Data$swedish), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "japanese", sum(Everton1011Data$japanese), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "turkish", sum(Everton1011Data$turkish), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "asante", sum(Everton1011Data$asante), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "persian", sum(Everton1011Data$persian), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "maltese", sum(Everton1011Data$maltese), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "romanian", sum(Everton1011Data$romanian), Everton1011Data$PropLanguages)
Everton1011Data$PropLanguages <- ifelse(Everton1011Data$Languages == "slovak", sum(Everton1011Data$slovak), Everton1011Data$PropLanguages)

Liverpool1011Data <- subset(PL1011SZN, Squad =="Liverpool")
Liverpool1011Data$english <- ifelse(Liverpool1011Data$Languages=="english", 1, 0)
Liverpool1011Data$french <- ifelse(Liverpool1011Data$Languages=="french", 1, 0)
Liverpool1011Data$dutch <- ifelse(Liverpool1011Data$Languages=="dutch", 1, 0)
Liverpool1011Data$portuguese <- ifelse(Liverpool1011Data$Languages=="portuguese", 1, 0)
Liverpool1011Data$castilian <- ifelse(Liverpool1011Data$Languages=="castilian", 1, 0)
Liverpool1011Data$spanish <- ifelse(Liverpool1011Data$Languages=="spanish", 1, 0)
Liverpool1011Data$german <- ifelse(Liverpool1011Data$Languages=="german", 1, 0)
Liverpool1011Data$arabic <- ifelse(Liverpool1011Data$Languages=="arabic", 1, 0)
Liverpool1011Data$serbian <- ifelse(Liverpool1011Data$Languages=="serbian", 1, 0)
Liverpool1011Data$korean <- ifelse(Liverpool1011Data$Languages=="korean", 1, 0)
Liverpool1011Data$croatian <- ifelse(Liverpool1011Data$Languages=="croatian", 1, 0)
Liverpool1011Data$italian <- ifelse(Liverpool1011Data$Languages=="italian", 1, 0)
Liverpool1011Data$armenian <- ifelse(Liverpool1011Data$Languages=="armenian", 1, 0)
Liverpool1011Data$icelandic <- ifelse(Liverpool1011Data$Languages=="icelandic", 1, 0)
Liverpool1011Data$hebrew <- ifelse(Liverpool1011Data$Languages=="hebrew", 1, 0)
Liverpool1011Data$bokmal <- ifelse(Liverpool1011Data$Languages=="bokmal", 1, 0)
Liverpool1011Data$ukrainian <- ifelse(Liverpool1011Data$Languages=="ukrainian", 1, 0)
Liverpool1011Data$russian <- ifelse(Liverpool1011Data$Languages=="russian", 1, 0)
Liverpool1011Data$bosnian <- ifelse(Liverpool1011Data$Languages=="bosnian", 1, 0)
Liverpool1011Data$czech <- ifelse(Liverpool1011Data$Languages=="czech", 1, 0)
Liverpool1011Data$danish <- ifelse(Liverpool1011Data$Languages=="danish", 1, 0)
Liverpool1011Data$swedish <- ifelse(Liverpool1011Data$Languages=="swedish", 1, 0)
Liverpool1011Data$japanese <- ifelse(Liverpool1011Data$Languages=="japanese", 1, 0)
Liverpool1011Data$turkish <- ifelse(Liverpool1011Data$Languages=="turkish", 1, 0)
Liverpool1011Data$asante <- ifelse(Liverpool1011Data$Languages=="asante", 1, 0)
Liverpool1011Data$persian <- ifelse(Liverpool1011Data$Languages=="persian", 1, 0)
Liverpool1011Data$maltese <- ifelse(Liverpool1011Data$Languages=="maltese", 1, 0)
Liverpool1011Data$romanian <- ifelse(Liverpool1011Data$Languages=="romanian", 1, 0)
Liverpool1011Data$slovak <- ifelse(Liverpool1011Data$Languages=="slovak", 1, 0)
Liverpool1011Data$PropLanguages <- 0
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "english", sum(Liverpool1011Data$english), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "french", sum(Liverpool1011Data$french), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "dutch", sum(Liverpool1011Data$dutch), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "portuguese", sum(Liverpool1011Data$portuguese), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "castilian", sum(Liverpool1011Data$castilian), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "spanish", sum(Liverpool1011Data$spanish), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "german", sum(Liverpool1011Data$german), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "arabic", sum(Liverpool1011Data$arabic), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "serbian", sum(Liverpool1011Data$serbian), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "korean", sum(Liverpool1011Data$korean), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "croatian", sum(Liverpool1011Data$croatian), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "italian", sum(Liverpool1011Data$italian), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "armenian", sum(Liverpool1011Data$armenian), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "icelandic", sum(Liverpool1011Data$icelandic), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "hebrew", sum(Liverpool1011Data$hebrew), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "bokmal", sum(Liverpool1011Data$bokmal), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "ukrainian", sum(Liverpool1011Data$ukrainian), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "russian", sum(Liverpool1011Data$russian), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "bosnian", sum(Liverpool1011Data$bosnian), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "czech", sum(Liverpool1011Data$czech), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "danish", sum(Liverpool1011Data$danish), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "swedish", sum(Liverpool1011Data$swedish), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "japanese", sum(Liverpool1011Data$japanese), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "turkish", sum(Liverpool1011Data$turkish), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "asante", sum(Liverpool1011Data$asante), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "persian", sum(Liverpool1011Data$persian), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "maltese", sum(Liverpool1011Data$maltese), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "romanian", sum(Liverpool1011Data$romanian), Liverpool1011Data$PropLanguages)
Liverpool1011Data$PropLanguages <- ifelse(Liverpool1011Data$Languages == "slovak", sum(Liverpool1011Data$slovak), Liverpool1011Data$PropLanguages)

ManchesterCity1011Data <- subset(PL1011SZN, Squad =="Manchester City")
ManchesterCity1011Data$english <- ifelse(ManchesterCity1011Data$Languages=="english", 1, 0)
ManchesterCity1011Data$french <- ifelse(ManchesterCity1011Data$Languages=="french", 1, 0)
ManchesterCity1011Data$dutch <- ifelse(ManchesterCity1011Data$Languages=="dutch", 1, 0)
ManchesterCity1011Data$portuguese <- ifelse(ManchesterCity1011Data$Languages=="portuguese", 1, 0)
ManchesterCity1011Data$castilian <- ifelse(ManchesterCity1011Data$Languages=="castilian", 1, 0)
ManchesterCity1011Data$spanish <- ifelse(ManchesterCity1011Data$Languages=="spanish", 1, 0)
ManchesterCity1011Data$german <- ifelse(ManchesterCity1011Data$Languages=="german", 1, 0)
ManchesterCity1011Data$arabic <- ifelse(ManchesterCity1011Data$Languages=="arabic", 1, 0)
ManchesterCity1011Data$serbian <- ifelse(ManchesterCity1011Data$Languages=="serbian", 1, 0)
ManchesterCity1011Data$korean <- ifelse(ManchesterCity1011Data$Languages=="korean", 1, 0)
ManchesterCity1011Data$croatian <- ifelse(ManchesterCity1011Data$Languages=="croatian", 1, 0)
ManchesterCity1011Data$italian <- ifelse(ManchesterCity1011Data$Languages=="italian", 1, 0)
ManchesterCity1011Data$armenian <- ifelse(ManchesterCity1011Data$Languages=="armenian", 1, 0)
ManchesterCity1011Data$icelandic <- ifelse(ManchesterCity1011Data$Languages=="icelandic", 1, 0)
ManchesterCity1011Data$hebrew <- ifelse(ManchesterCity1011Data$Languages=="hebrew", 1, 0)
ManchesterCity1011Data$bokmal <- ifelse(ManchesterCity1011Data$Languages=="bokmal", 1, 0)
ManchesterCity1011Data$ukrainian <- ifelse(ManchesterCity1011Data$Languages=="ukrainian", 1, 0)
ManchesterCity1011Data$russian <- ifelse(ManchesterCity1011Data$Languages=="russian", 1, 0)
ManchesterCity1011Data$bosnian <- ifelse(ManchesterCity1011Data$Languages=="bosnian", 1, 0)
ManchesterCity1011Data$czech <- ifelse(ManchesterCity1011Data$Languages=="czech", 1, 0)
ManchesterCity1011Data$danish <- ifelse(ManchesterCity1011Data$Languages=="danish", 1, 0)
ManchesterCity1011Data$swedish <- ifelse(ManchesterCity1011Data$Languages=="swedish", 1, 0)
ManchesterCity1011Data$japanese <- ifelse(ManchesterCity1011Data$Languages=="japanese", 1, 0)
ManchesterCity1011Data$turkish <- ifelse(ManchesterCity1011Data$Languages=="turkish", 1, 0)
ManchesterCity1011Data$asante <- ifelse(ManchesterCity1011Data$Languages=="asante", 1, 0)
ManchesterCity1011Data$persian <- ifelse(ManchesterCity1011Data$Languages=="persian", 1, 0)
ManchesterCity1011Data$maltese <- ifelse(ManchesterCity1011Data$Languages=="maltese", 1, 0)
ManchesterCity1011Data$romanian <- ifelse(ManchesterCity1011Data$Languages=="romanian", 1, 0)
ManchesterCity1011Data$slovak <- ifelse(ManchesterCity1011Data$Languages=="slovak", 1, 0)
ManchesterCity1011Data$PropLanguages <- 0
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "english", sum(ManchesterCity1011Data$english), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "french", sum(ManchesterCity1011Data$french), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "dutch", sum(ManchesterCity1011Data$dutch), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "portuguese", sum(ManchesterCity1011Data$portuguese), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "castilian", sum(ManchesterCity1011Data$castilian), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "spanish", sum(ManchesterCity1011Data$spanish), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "german", sum(ManchesterCity1011Data$german), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "arabic", sum(ManchesterCity1011Data$arabic), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "serbian", sum(ManchesterCity1011Data$serbian), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "korean", sum(ManchesterCity1011Data$korean), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "croatian", sum(ManchesterCity1011Data$croatian), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "italian", sum(ManchesterCity1011Data$italian), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "armenian", sum(ManchesterCity1011Data$armenian), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "icelandic", sum(ManchesterCity1011Data$icelandic), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "hebrew", sum(ManchesterCity1011Data$hebrew), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "bokmal", sum(ManchesterCity1011Data$bokmal), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "ukrainian", sum(ManchesterCity1011Data$ukrainian), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "russian", sum(ManchesterCity1011Data$russian), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "bosnian", sum(ManchesterCity1011Data$bosnian), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "czech", sum(ManchesterCity1011Data$czech), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "danish", sum(ManchesterCity1011Data$danish), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "swedish", sum(ManchesterCity1011Data$swedish), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "japanese", sum(ManchesterCity1011Data$japanese), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "turkish", sum(ManchesterCity1011Data$turkish), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "asante", sum(ManchesterCity1011Data$asante), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "persian", sum(ManchesterCity1011Data$persian), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "maltese", sum(ManchesterCity1011Data$maltese), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "romanian", sum(ManchesterCity1011Data$romanian), ManchesterCity1011Data$PropLanguages)
ManchesterCity1011Data$PropLanguages <- ifelse(ManchesterCity1011Data$Languages == "slovak", sum(ManchesterCity1011Data$slovak), ManchesterCity1011Data$PropLanguages)

ManchesterUtd1011Data <- subset(PL1011SZN, Squad =="Manchester Utd")
ManchesterUtd1011Data$english <- ifelse(ManchesterUtd1011Data$Languages=="english", 1, 0)
ManchesterUtd1011Data$french <- ifelse(ManchesterUtd1011Data$Languages=="french", 1, 0)
ManchesterUtd1011Data$dutch <- ifelse(ManchesterUtd1011Data$Languages=="dutch", 1, 0)
ManchesterUtd1011Data$portuguese <- ifelse(ManchesterUtd1011Data$Languages=="portuguese", 1, 0)
ManchesterUtd1011Data$castilian <- ifelse(ManchesterUtd1011Data$Languages=="castilian", 1, 0)
ManchesterUtd1011Data$spanish <- ifelse(ManchesterUtd1011Data$Languages=="spanish", 1, 0)
ManchesterUtd1011Data$german <- ifelse(ManchesterUtd1011Data$Languages=="german", 1, 0)
ManchesterUtd1011Data$arabic <- ifelse(ManchesterUtd1011Data$Languages=="arabic", 1, 0)
ManchesterUtd1011Data$serbian <- ifelse(ManchesterUtd1011Data$Languages=="serbian", 1, 0)
ManchesterUtd1011Data$korean <- ifelse(ManchesterUtd1011Data$Languages=="korean", 1, 0)
ManchesterUtd1011Data$croatian <- ifelse(ManchesterUtd1011Data$Languages=="croatian", 1, 0)
ManchesterUtd1011Data$italian <- ifelse(ManchesterUtd1011Data$Languages=="italian", 1, 0)
ManchesterUtd1011Data$armenian <- ifelse(ManchesterUtd1011Data$Languages=="armenian", 1, 0)
ManchesterUtd1011Data$icelandic <- ifelse(ManchesterUtd1011Data$Languages=="icelandic", 1, 0)
ManchesterUtd1011Data$hebrew <- ifelse(ManchesterUtd1011Data$Languages=="hebrew", 1, 0)
ManchesterUtd1011Data$bokmal <- ifelse(ManchesterUtd1011Data$Languages=="bokmal", 1, 0)
ManchesterUtd1011Data$ukrainian <- ifelse(ManchesterUtd1011Data$Languages=="ukrainian", 1, 0)
ManchesterUtd1011Data$russian <- ifelse(ManchesterUtd1011Data$Languages=="russian", 1, 0)
ManchesterUtd1011Data$bosnian <- ifelse(ManchesterUtd1011Data$Languages=="bosnian", 1, 0)
ManchesterUtd1011Data$czech <- ifelse(ManchesterUtd1011Data$Languages=="czech", 1, 0)
ManchesterUtd1011Data$danish <- ifelse(ManchesterUtd1011Data$Languages=="danish", 1, 0)
ManchesterUtd1011Data$swedish <- ifelse(ManchesterUtd1011Data$Languages=="swedish", 1, 0)
ManchesterUtd1011Data$japanese <- ifelse(ManchesterUtd1011Data$Languages=="japanese", 1, 0)
ManchesterUtd1011Data$turkish <- ifelse(ManchesterUtd1011Data$Languages=="turkish", 1, 0)
ManchesterUtd1011Data$asante <- ifelse(ManchesterUtd1011Data$Languages=="asante", 1, 0)
ManchesterUtd1011Data$persian <- ifelse(ManchesterUtd1011Data$Languages=="persian", 1, 0)
ManchesterUtd1011Data$maltese <- ifelse(ManchesterUtd1011Data$Languages=="maltese", 1, 0)
ManchesterUtd1011Data$romanian <- ifelse(ManchesterUtd1011Data$Languages=="romanian", 1, 0)
ManchesterUtd1011Data$slovak <- ifelse(ManchesterUtd1011Data$Languages=="slovak", 1, 0)
ManchesterUtd1011Data$PropLanguages <- 0
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "english", sum(ManchesterUtd1011Data$english), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "french", sum(ManchesterUtd1011Data$french), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "dutch", sum(ManchesterUtd1011Data$dutch), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "portuguese", sum(ManchesterUtd1011Data$portuguese), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "castilian", sum(ManchesterUtd1011Data$castilian), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "spanish", sum(ManchesterUtd1011Data$spanish), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "german", sum(ManchesterUtd1011Data$german), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "arabic", sum(ManchesterUtd1011Data$arabic), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "serbian", sum(ManchesterUtd1011Data$serbian), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "korean", sum(ManchesterUtd1011Data$korean), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "croatian", sum(ManchesterUtd1011Data$croatian), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "italian", sum(ManchesterUtd1011Data$italian), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "armenian", sum(ManchesterUtd1011Data$armenian), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "icelandic", sum(ManchesterUtd1011Data$icelandic), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "hebrew", sum(ManchesterUtd1011Data$hebrew), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "bokmal", sum(ManchesterUtd1011Data$bokmal), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "ukrainian", sum(ManchesterUtd1011Data$ukrainian), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "russian", sum(ManchesterUtd1011Data$russian), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "bosnian", sum(ManchesterUtd1011Data$bosnian), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "czech", sum(ManchesterUtd1011Data$czech), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "danish", sum(ManchesterUtd1011Data$danish), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "swedish", sum(ManchesterUtd1011Data$swedish), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "japanese", sum(ManchesterUtd1011Data$japanese), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "turkish", sum(ManchesterUtd1011Data$turkish), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "asante", sum(ManchesterUtd1011Data$asante), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "persian", sum(ManchesterUtd1011Data$persian), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "maltese", sum(ManchesterUtd1011Data$maltese), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "romanian", sum(ManchesterUtd1011Data$romanian), ManchesterUtd1011Data$PropLanguages)
ManchesterUtd1011Data$PropLanguages <- ifelse(ManchesterUtd1011Data$Languages == "slovak", sum(ManchesterUtd1011Data$slovak), ManchesterUtd1011Data$PropLanguages)

Tottenham1011Data <- subset(PL1011SZN, Squad =="Tottenham")
Tottenham1011Data$english <- ifelse(Tottenham1011Data$Languages=="english", 1, 0)
Tottenham1011Data$french <- ifelse(Tottenham1011Data$Languages=="french", 1, 0)
Tottenham1011Data$dutch <- ifelse(Tottenham1011Data$Languages=="dutch", 1, 0)
Tottenham1011Data$portuguese <- ifelse(Tottenham1011Data$Languages=="portuguese", 1, 0)
Tottenham1011Data$castilian <- ifelse(Tottenham1011Data$Languages=="castilian", 1, 0)
Tottenham1011Data$spanish <- ifelse(Tottenham1011Data$Languages=="spanish", 1, 0)
Tottenham1011Data$german <- ifelse(Tottenham1011Data$Languages=="german", 1, 0)
Tottenham1011Data$arabic <- ifelse(Tottenham1011Data$Languages=="arabic", 1, 0)
Tottenham1011Data$serbian <- ifelse(Tottenham1011Data$Languages=="serbian", 1, 0)
Tottenham1011Data$korean <- ifelse(Tottenham1011Data$Languages=="korean", 1, 0)
Tottenham1011Data$croatian <- ifelse(Tottenham1011Data$Languages=="croatian", 1, 0)
Tottenham1011Data$italian <- ifelse(Tottenham1011Data$Languages=="italian", 1, 0)
Tottenham1011Data$armenian <- ifelse(Tottenham1011Data$Languages=="armenian", 1, 0)
Tottenham1011Data$icelandic <- ifelse(Tottenham1011Data$Languages=="icelandic", 1, 0)
Tottenham1011Data$hebrew <- ifelse(Tottenham1011Data$Languages=="hebrew", 1, 0)
Tottenham1011Data$bokmal <- ifelse(Tottenham1011Data$Languages=="bokmal", 1, 0)
Tottenham1011Data$ukrainian <- ifelse(Tottenham1011Data$Languages=="ukrainian", 1, 0)
Tottenham1011Data$russian <- ifelse(Tottenham1011Data$Languages=="russian", 1, 0)
Tottenham1011Data$bosnian <- ifelse(Tottenham1011Data$Languages=="bosnian", 1, 0)
Tottenham1011Data$czech <- ifelse(Tottenham1011Data$Languages=="czech", 1, 0)
Tottenham1011Data$danish <- ifelse(Tottenham1011Data$Languages=="danish", 1, 0)
Tottenham1011Data$swedish <- ifelse(Tottenham1011Data$Languages=="swedish", 1, 0)
Tottenham1011Data$japanese <- ifelse(Tottenham1011Data$Languages=="japanese", 1, 0)
Tottenham1011Data$turkish <- ifelse(Tottenham1011Data$Languages=="turkish", 1, 0)
Tottenham1011Data$asante <- ifelse(Tottenham1011Data$Languages=="asante", 1, 0)
Tottenham1011Data$persian <- ifelse(Tottenham1011Data$Languages=="persian", 1, 0)
Tottenham1011Data$maltese <- ifelse(Tottenham1011Data$Languages=="maltese", 1, 0)
Tottenham1011Data$romanian <- ifelse(Tottenham1011Data$Languages=="romanian", 1, 0)
Tottenham1011Data$slovak <- ifelse(Tottenham1011Data$Languages=="slovak", 1, 0)
Tottenham1011Data$PropLanguages <- 0
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "english", sum(Tottenham1011Data$english), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "french", sum(Tottenham1011Data$french), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "dutch", sum(Tottenham1011Data$dutch), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "portuguese", sum(Tottenham1011Data$portuguese), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "castilian", sum(Tottenham1011Data$castilian), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "spanish", sum(Tottenham1011Data$spanish), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "german", sum(Tottenham1011Data$german), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "arabic", sum(Tottenham1011Data$arabic), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "serbian", sum(Tottenham1011Data$serbian), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "korean", sum(Tottenham1011Data$korean), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "croatian", sum(Tottenham1011Data$croatian), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "italian", sum(Tottenham1011Data$italian), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "armenian", sum(Tottenham1011Data$armenian), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "icelandic", sum(Tottenham1011Data$icelandic), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "hebrew", sum(Tottenham1011Data$hebrew), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "bokmal", sum(Tottenham1011Data$bokmal), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "ukrainian", sum(Tottenham1011Data$ukrainian), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "russian", sum(Tottenham1011Data$russian), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "bosnian", sum(Tottenham1011Data$bosnian), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "czech", sum(Tottenham1011Data$czech), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "danish", sum(Tottenham1011Data$danish), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "swedish", sum(Tottenham1011Data$swedish), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "japanese", sum(Tottenham1011Data$japanese), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "turkish", sum(Tottenham1011Data$turkish), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "asante", sum(Tottenham1011Data$asante), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "persian", sum(Tottenham1011Data$persian), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "maltese", sum(Tottenham1011Data$maltese), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "romanian", sum(Tottenham1011Data$romanian), Tottenham1011Data$PropLanguages)
Tottenham1011Data$PropLanguages <- ifelse(Tottenham1011Data$Languages == "slovak", sum(Tottenham1011Data$slovak), Tottenham1011Data$PropLanguages)

PL1011SZN <- rbind(Arsenal1011Data, Chelsea1011Data)
PL1011SZN <- rbind(PL1011SZN, Everton1011Data)
PL1011SZN <- rbind(PL1011SZN, Liverpool1011Data)
PL1011SZN <- rbind(PL1011SZN, ManchesterCity1011Data)
PL1011SZN <- rbind(PL1011SZN, ManchesterUtd1011Data)
PL1011SZN <- rbind(PL1011SZN, Tottenham1011Data)

PL910SZN$english <- 0
PL910SZN$french <- 0
PL910SZN$portuguese <- 0
PL910SZN$castilian <- 0
PL910SZN$dutch <- 0
PL910SZN$german <- 0
PL910SZN$spanish <- 0
PL910SZN$arabic <- 0
PL910SZN$serbian <- 0
PL910SZN$korean <- 0
PL910SZN$croatian <- 0
PL910SZN$italian <- 0
PL910SZN$icelandic <- 0
PL910SZN$hebrew <- 0
PL910SZN$bokmal <- 0
PL910SZN$ukrainian <- 0
PL910SZN$russian <- 0
PL910SZN$bosnian <- 0
PL910SZN$czech <- 0
PL910SZN$danish <- 0
PL910SZN$armenian <- 0
PL910SZN$turkish <- 0
PL910SZN$asante <- 0
PL910SZN$persian <- 0
PL910SZN$japanese <- 0
PL910SZN$maltese <- 0
PL910SZN$romanian <- 0
PL910SZN$slovak <- 0
PL910SZN$swedish <- 0

Arsenal910Data <- subset(PL910SZN, Squad =="Arsenal")
Arsenal910Data$english <- ifelse(Arsenal910Data$Languages=="english", 1, 0)
Arsenal910Data$french <- ifelse(Arsenal910Data$Languages=="french", 1, 0)
Arsenal910Data$dutch <- ifelse(Arsenal910Data$Languages=="dutch", 1, 0)
Arsenal910Data$portuguese <- ifelse(Arsenal910Data$Languages=="portuguese", 1, 0)
Arsenal910Data$castilian <- ifelse(Arsenal910Data$Languages=="castilian", 1, 0)
Arsenal910Data$spanish <- ifelse(Arsenal910Data$Languages=="spanish", 1, 0)
Arsenal910Data$german <- ifelse(Arsenal910Data$Languages=="german", 1, 0)
Arsenal910Data$arabic <- ifelse(Arsenal910Data$Languages=="arabic", 1, 0)
Arsenal910Data$serbian <- ifelse(Arsenal910Data$Languages=="serbian", 1, 0)
Arsenal910Data$korean <- ifelse(Arsenal910Data$Languages=="korean", 1, 0)
Arsenal910Data$croatian <- ifelse(Arsenal910Data$Languages=="croatian", 1, 0)
Arsenal910Data$italian <- ifelse(Arsenal910Data$Languages=="italian", 1, 0)
Arsenal910Data$armenian <- ifelse(Arsenal910Data$Languages=="armenian", 1, 0)
Arsenal910Data$icelandic <- ifelse(Arsenal910Data$Languages=="icelandic", 1, 0)
Arsenal910Data$hebrew <- ifelse(Arsenal910Data$Languages=="hebrew", 1, 0)
Arsenal910Data$bokmal <- ifelse(Arsenal910Data$Languages=="bokmal", 1, 0)
Arsenal910Data$ukrainian <- ifelse(Arsenal910Data$Languages=="ukrainian", 1, 0)
Arsenal910Data$russian <- ifelse(Arsenal910Data$Languages=="russian", 1, 0)
Arsenal910Data$bosnian <- ifelse(Arsenal910Data$Languages=="bosnian", 1, 0)
Arsenal910Data$czech <- ifelse(Arsenal910Data$Languages=="czech", 1, 0)
Arsenal910Data$danish <- ifelse(Arsenal910Data$Languages=="danish", 1, 0)
Arsenal910Data$swedish <- ifelse(Arsenal910Data$Languages=="swedish", 1, 0)
Arsenal910Data$japanese <- ifelse(Arsenal910Data$Languages=="japanese", 1, 0)
Arsenal910Data$turkish <- ifelse(Arsenal910Data$Languages=="turkish", 1, 0)
Arsenal910Data$asante <- ifelse(Arsenal910Data$Languages=="asante", 1, 0)
Arsenal910Data$persian <- ifelse(Arsenal910Data$Languages=="persian", 1, 0)
Arsenal910Data$maltese <- ifelse(Arsenal910Data$Languages=="maltese", 1, 0)
Arsenal910Data$romanian <- ifelse(Arsenal910Data$Languages=="romanian", 1, 0)
Arsenal910Data$slovak <- ifelse(Arsenal910Data$Languages=="slovak", 1, 0)
Arsenal910Data$PropLanguages <- 0
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "english", sum(Arsenal910Data$english), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "french", sum(Arsenal910Data$french), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "dutch", sum(Arsenal910Data$dutch), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "portuguese", sum(Arsenal910Data$portuguese), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "castilian", sum(Arsenal910Data$castilian), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "spanish", sum(Arsenal910Data$spanish), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "german", sum(Arsenal910Data$german), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "arabic", sum(Arsenal910Data$arabic), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "serbian", sum(Arsenal910Data$serbian), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "korean", sum(Arsenal910Data$korean), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "croatian", sum(Arsenal910Data$croatian), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "italian", sum(Arsenal910Data$italian), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "armenian", sum(Arsenal910Data$armenian), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "icelandic", sum(Arsenal910Data$icelandic), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "hebrew", sum(Arsenal910Data$hebrew), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "bokmal", sum(Arsenal910Data$bokmal), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "ukrainian", sum(Arsenal910Data$ukrainian), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "russian", sum(Arsenal910Data$russian), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "bosnian", sum(Arsenal910Data$bosnian), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "czech", sum(Arsenal910Data$czech), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "danish", sum(Arsenal910Data$danish), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "swedish", sum(Arsenal910Data$swedish), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "japanese", sum(Arsenal910Data$japanese), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "turkish", sum(Arsenal910Data$turkish), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "asante", sum(Arsenal910Data$asante), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "persian", sum(Arsenal910Data$persian), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "maltese", sum(Arsenal910Data$maltese), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "romanian", sum(Arsenal910Data$romanian), Arsenal910Data$PropLanguages)
Arsenal910Data$PropLanguages <- ifelse(Arsenal910Data$Languages == "slovak", sum(Arsenal910Data$slovak), Arsenal910Data$PropLanguages)

Chelsea910Data <- subset(PL910SZN, Squad =="Chelsea")
Chelsea910Data$english <- ifelse(Chelsea910Data$Languages=="english", 1, 0)
Chelsea910Data$french <- ifelse(Chelsea910Data$Languages=="french", 1, 0)
Chelsea910Data$dutch <- ifelse(Chelsea910Data$Languages=="dutch", 1, 0)
Chelsea910Data$portuguese <- ifelse(Chelsea910Data$Languages=="portuguese", 1, 0)
Chelsea910Data$castilian <- ifelse(Chelsea910Data$Languages=="castilian", 1, 0)
Chelsea910Data$spanish <- ifelse(Chelsea910Data$Languages=="spanish", 1, 0)
Chelsea910Data$german <- ifelse(Chelsea910Data$Languages=="german", 1, 0)
Chelsea910Data$arabic <- ifelse(Chelsea910Data$Languages=="arabic", 1, 0)
Chelsea910Data$serbian <- ifelse(Chelsea910Data$Languages=="serbian", 1, 0)
Chelsea910Data$korean <- ifelse(Chelsea910Data$Languages=="korean", 1, 0)
Chelsea910Data$croatian <- ifelse(Chelsea910Data$Languages=="croatian", 1, 0)
Chelsea910Data$italian <- ifelse(Chelsea910Data$Languages=="italian", 1, 0)
Chelsea910Data$armenian <- ifelse(Chelsea910Data$Languages=="armenian", 1, 0)
Chelsea910Data$icelandic <- ifelse(Chelsea910Data$Languages=="icelandic", 1, 0)
Chelsea910Data$hebrew <- ifelse(Chelsea910Data$Languages=="hebrew", 1, 0)
Chelsea910Data$bokmal <- ifelse(Chelsea910Data$Languages=="bokmal", 1, 0)
Chelsea910Data$ukrainian <- ifelse(Chelsea910Data$Languages=="ukrainian", 1, 0)
Chelsea910Data$russian <- ifelse(Chelsea910Data$Languages=="russian", 1, 0)
Chelsea910Data$bosnian <- ifelse(Chelsea910Data$Languages=="bosnian", 1, 0)
Chelsea910Data$czech <- ifelse(Chelsea910Data$Languages=="czech", 1, 0)
Chelsea910Data$danish <- ifelse(Chelsea910Data$Languages=="danish", 1, 0)
Chelsea910Data$swedish <- ifelse(Chelsea910Data$Languages=="swedish", 1, 0)
Chelsea910Data$japanese <- ifelse(Chelsea910Data$Languages=="japanese", 1, 0)
Chelsea910Data$turkish <- ifelse(Chelsea910Data$Languages=="turkish", 1, 0)
Chelsea910Data$asante <- ifelse(Chelsea910Data$Languages=="asante", 1, 0)
Chelsea910Data$persian <- ifelse(Chelsea910Data$Languages=="persian", 1, 0)
Chelsea910Data$maltese <- ifelse(Chelsea910Data$Languages=="maltese", 1, 0)
Chelsea910Data$romanian <- ifelse(Chelsea910Data$Languages=="romanian", 1, 0)
Chelsea910Data$slovak <- ifelse(Chelsea910Data$Languages=="slovak", 1, 0)
Chelsea910Data$PropLanguages <- 0
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "english", sum(Chelsea910Data$english), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "french", sum(Chelsea910Data$french), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "dutch", sum(Chelsea910Data$dutch), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "portuguese", sum(Chelsea910Data$portuguese), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "castilian", sum(Chelsea910Data$castilian), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "spanish", sum(Chelsea910Data$spanish), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "german", sum(Chelsea910Data$german), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "arabic", sum(Chelsea910Data$arabic), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "serbian", sum(Chelsea910Data$serbian), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "korean", sum(Chelsea910Data$korean), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "croatian", sum(Chelsea910Data$croatian), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "italian", sum(Chelsea910Data$italian), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "armenian", sum(Chelsea910Data$armenian), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "icelandic", sum(Chelsea910Data$icelandic), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "hebrew", sum(Chelsea910Data$hebrew), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "bokmal", sum(Chelsea910Data$bokmal), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "ukrainian", sum(Chelsea910Data$ukrainian), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "russian", sum(Chelsea910Data$russian), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "bosnian", sum(Chelsea910Data$bosnian), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "czech", sum(Chelsea910Data$czech), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "danish", sum(Chelsea910Data$danish), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "swedish", sum(Chelsea910Data$swedish), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "japanese", sum(Chelsea910Data$japanese), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "turkish", sum(Chelsea910Data$turkish), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "asante", sum(Chelsea910Data$asante), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "persian", sum(Chelsea910Data$persian), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "maltese", sum(Chelsea910Data$maltese), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "romanian", sum(Chelsea910Data$romanian), Chelsea910Data$PropLanguages)
Chelsea910Data$PropLanguages <- ifelse(Chelsea910Data$Languages == "slovak", sum(Chelsea910Data$slovak), Chelsea910Data$PropLanguages)

Everton910Data <- subset(PL910SZN, Squad =="Everton")
Everton910Data$english <- ifelse(Everton910Data$Languages=="english", 1, 0)
Everton910Data$french <- ifelse(Everton910Data$Languages=="french", 1, 0)
Everton910Data$dutch <- ifelse(Everton910Data$Languages=="dutch", 1, 0)
Everton910Data$portuguese <- ifelse(Everton910Data$Languages=="portuguese", 1, 0)
Everton910Data$castilian <- ifelse(Everton910Data$Languages=="castilian", 1, 0)
Everton910Data$spanish <- ifelse(Everton910Data$Languages=="spanish", 1, 0)
Everton910Data$german <- ifelse(Everton910Data$Languages=="german", 1, 0)
Everton910Data$arabic <- ifelse(Everton910Data$Languages=="arabic", 1, 0)
Everton910Data$serbian <- ifelse(Everton910Data$Languages=="serbian", 1, 0)
Everton910Data$korean <- ifelse(Everton910Data$Languages=="korean", 1, 0)
Everton910Data$croatian <- ifelse(Everton910Data$Languages=="croatian", 1, 0)
Everton910Data$italian <- ifelse(Everton910Data$Languages=="italian", 1, 0)
Everton910Data$armenian <- ifelse(Everton910Data$Languages=="armenian", 1, 0)
Everton910Data$icelandic <- ifelse(Everton910Data$Languages=="icelandic", 1, 0)
Everton910Data$hebrew <- ifelse(Everton910Data$Languages=="hebrew", 1, 0)
Everton910Data$bokmal <- ifelse(Everton910Data$Languages=="bokmal", 1, 0)
Everton910Data$ukrainian <- ifelse(Everton910Data$Languages=="ukrainian", 1, 0)
Everton910Data$russian <- ifelse(Everton910Data$Languages=="russian", 1, 0)
Everton910Data$bosnian <- ifelse(Everton910Data$Languages=="bosnian", 1, 0)
Everton910Data$czech <- ifelse(Everton910Data$Languages=="czech", 1, 0)
Everton910Data$danish <- ifelse(Everton910Data$Languages=="danish", 1, 0)
Everton910Data$swedish <- ifelse(Everton910Data$Languages=="swedish", 1, 0)
Everton910Data$japanese <- ifelse(Everton910Data$Languages=="japanese", 1, 0)
Everton910Data$turkish <- ifelse(Everton910Data$Languages=="turkish", 1, 0)
Everton910Data$asante <- ifelse(Everton910Data$Languages=="asante", 1, 0)
Everton910Data$persian <- ifelse(Everton910Data$Languages=="persian", 1, 0)
Everton910Data$maltese <- ifelse(Everton910Data$Languages=="maltese", 1, 0)
Everton910Data$romanian <- ifelse(Everton910Data$Languages=="romanian", 1, 0)
Everton910Data$slovak <- ifelse(Everton910Data$Languages=="slovak", 1, 0)
Everton910Data$PropLanguages <- 0
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "english", sum(Everton910Data$english), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "french", sum(Everton910Data$french), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "dutch", sum(Everton910Data$dutch), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "portuguese", sum(Everton910Data$portuguese), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "castilian", sum(Everton910Data$castilian), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "spanish", sum(Everton910Data$spanish), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "german", sum(Everton910Data$german), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "arabic", sum(Everton910Data$arabic), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "serbian", sum(Everton910Data$serbian), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "korean", sum(Everton910Data$korean), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "croatian", sum(Everton910Data$croatian), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "italian", sum(Everton910Data$italian), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "armenian", sum(Everton910Data$armenian), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "icelandic", sum(Everton910Data$icelandic), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "hebrew", sum(Everton910Data$hebrew), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "bokmal", sum(Everton910Data$bokmal), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "ukrainian", sum(Everton910Data$ukrainian), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "russian", sum(Everton910Data$russian), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "bosnian", sum(Everton910Data$bosnian), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "czech", sum(Everton910Data$czech), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "danish", sum(Everton910Data$danish), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "swedish", sum(Everton910Data$swedish), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "japanese", sum(Everton910Data$japanese), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "turkish", sum(Everton910Data$turkish), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "asante", sum(Everton910Data$asante), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "persian", sum(Everton910Data$persian), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "maltese", sum(Everton910Data$maltese), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "romanian", sum(Everton910Data$romanian), Everton910Data$PropLanguages)
Everton910Data$PropLanguages <- ifelse(Everton910Data$Languages == "slovak", sum(Everton910Data$slovak), Everton910Data$PropLanguages)

Liverpool910Data <- subset(PL910SZN, Squad =="Liverpool")
Liverpool910Data$english <- ifelse(Liverpool910Data$Languages=="english", 1, 0)
Liverpool910Data$french <- ifelse(Liverpool910Data$Languages=="french", 1, 0)
Liverpool910Data$dutch <- ifelse(Liverpool910Data$Languages=="dutch", 1, 0)
Liverpool910Data$portuguese <- ifelse(Liverpool910Data$Languages=="portuguese", 1, 0)
Liverpool910Data$castilian <- ifelse(Liverpool910Data$Languages=="castilian", 1, 0)
Liverpool910Data$spanish <- ifelse(Liverpool910Data$Languages=="spanish", 1, 0)
Liverpool910Data$german <- ifelse(Liverpool910Data$Languages=="german", 1, 0)
Liverpool910Data$arabic <- ifelse(Liverpool910Data$Languages=="arabic", 1, 0)
Liverpool910Data$serbian <- ifelse(Liverpool910Data$Languages=="serbian", 1, 0)
Liverpool910Data$korean <- ifelse(Liverpool910Data$Languages=="korean", 1, 0)
Liverpool910Data$croatian <- ifelse(Liverpool910Data$Languages=="croatian", 1, 0)
Liverpool910Data$italian <- ifelse(Liverpool910Data$Languages=="italian", 1, 0)
Liverpool910Data$armenian <- ifelse(Liverpool910Data$Languages=="armenian", 1, 0)
Liverpool910Data$icelandic <- ifelse(Liverpool910Data$Languages=="icelandic", 1, 0)
Liverpool910Data$hebrew <- ifelse(Liverpool910Data$Languages=="hebrew", 1, 0)
Liverpool910Data$bokmal <- ifelse(Liverpool910Data$Languages=="bokmal", 1, 0)
Liverpool910Data$ukrainian <- ifelse(Liverpool910Data$Languages=="ukrainian", 1, 0)
Liverpool910Data$russian <- ifelse(Liverpool910Data$Languages=="russian", 1, 0)
Liverpool910Data$bosnian <- ifelse(Liverpool910Data$Languages=="bosnian", 1, 0)
Liverpool910Data$czech <- ifelse(Liverpool910Data$Languages=="czech", 1, 0)
Liverpool910Data$danish <- ifelse(Liverpool910Data$Languages=="danish", 1, 0)
Liverpool910Data$swedish <- ifelse(Liverpool910Data$Languages=="swedish", 1, 0)
Liverpool910Data$japanese <- ifelse(Liverpool910Data$Languages=="japanese", 1, 0)
Liverpool910Data$turkish <- ifelse(Liverpool910Data$Languages=="turkish", 1, 0)
Liverpool910Data$asante <- ifelse(Liverpool910Data$Languages=="asante", 1, 0)
Liverpool910Data$persian <- ifelse(Liverpool910Data$Languages=="persian", 1, 0)
Liverpool910Data$maltese <- ifelse(Liverpool910Data$Languages=="maltese", 1, 0)
Liverpool910Data$romanian <- ifelse(Liverpool910Data$Languages=="romanian", 1, 0)
Liverpool910Data$slovak <- ifelse(Liverpool910Data$Languages=="slovak", 1, 0)
Liverpool910Data$PropLanguages <- 0
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "english", sum(Liverpool910Data$english), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "french", sum(Liverpool910Data$french), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "dutch", sum(Liverpool910Data$dutch), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "portuguese", sum(Liverpool910Data$portuguese), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "castilian", sum(Liverpool910Data$castilian), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "spanish", sum(Liverpool910Data$spanish), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "german", sum(Liverpool910Data$german), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "arabic", sum(Liverpool910Data$arabic), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "serbian", sum(Liverpool910Data$serbian), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "korean", sum(Liverpool910Data$korean), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "croatian", sum(Liverpool910Data$croatian), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "italian", sum(Liverpool910Data$italian), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "armenian", sum(Liverpool910Data$armenian), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "icelandic", sum(Liverpool910Data$icelandic), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "hebrew", sum(Liverpool910Data$hebrew), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "bokmal", sum(Liverpool910Data$bokmal), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "ukrainian", sum(Liverpool910Data$ukrainian), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "russian", sum(Liverpool910Data$russian), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "bosnian", sum(Liverpool910Data$bosnian), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "czech", sum(Liverpool910Data$czech), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "danish", sum(Liverpool910Data$danish), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "swedish", sum(Liverpool910Data$swedish), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "japanese", sum(Liverpool910Data$japanese), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "turkish", sum(Liverpool910Data$turkish), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "asante", sum(Liverpool910Data$asante), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "persian", sum(Liverpool910Data$persian), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "maltese", sum(Liverpool910Data$maltese), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "romanian", sum(Liverpool910Data$romanian), Liverpool910Data$PropLanguages)
Liverpool910Data$PropLanguages <- ifelse(Liverpool910Data$Languages == "slovak", sum(Liverpool910Data$slovak), Liverpool910Data$PropLanguages)

ManchesterCity910Data <- subset(PL910SZN, Squad =="Manchester City")
ManchesterCity910Data$english <- ifelse(ManchesterCity910Data$Languages=="english", 1, 0)
ManchesterCity910Data$french <- ifelse(ManchesterCity910Data$Languages=="french", 1, 0)
ManchesterCity910Data$dutch <- ifelse(ManchesterCity910Data$Languages=="dutch", 1, 0)
ManchesterCity910Data$portuguese <- ifelse(ManchesterCity910Data$Languages=="portuguese", 1, 0)
ManchesterCity910Data$castilian <- ifelse(ManchesterCity910Data$Languages=="castilian", 1, 0)
ManchesterCity910Data$spanish <- ifelse(ManchesterCity910Data$Languages=="spanish", 1, 0)
ManchesterCity910Data$german <- ifelse(ManchesterCity910Data$Languages=="german", 1, 0)
ManchesterCity910Data$arabic <- ifelse(ManchesterCity910Data$Languages=="arabic", 1, 0)
ManchesterCity910Data$serbian <- ifelse(ManchesterCity910Data$Languages=="serbian", 1, 0)
ManchesterCity910Data$korean <- ifelse(ManchesterCity910Data$Languages=="korean", 1, 0)
ManchesterCity910Data$croatian <- ifelse(ManchesterCity910Data$Languages=="croatian", 1, 0)
ManchesterCity910Data$italian <- ifelse(ManchesterCity910Data$Languages=="italian", 1, 0)
ManchesterCity910Data$armenian <- ifelse(ManchesterCity910Data$Languages=="armenian", 1, 0)
ManchesterCity910Data$icelandic <- ifelse(ManchesterCity910Data$Languages=="icelandic", 1, 0)
ManchesterCity910Data$hebrew <- ifelse(ManchesterCity910Data$Languages=="hebrew", 1, 0)
ManchesterCity910Data$bokmal <- ifelse(ManchesterCity910Data$Languages=="bokmal", 1, 0)
ManchesterCity910Data$ukrainian <- ifelse(ManchesterCity910Data$Languages=="ukrainian", 1, 0)
ManchesterCity910Data$russian <- ifelse(ManchesterCity910Data$Languages=="russian", 1, 0)
ManchesterCity910Data$bosnian <- ifelse(ManchesterCity910Data$Languages=="bosnian", 1, 0)
ManchesterCity910Data$czech <- ifelse(ManchesterCity910Data$Languages=="czech", 1, 0)
ManchesterCity910Data$danish <- ifelse(ManchesterCity910Data$Languages=="danish", 1, 0)
ManchesterCity910Data$swedish <- ifelse(ManchesterCity910Data$Languages=="swedish", 1, 0)
ManchesterCity910Data$japanese <- ifelse(ManchesterCity910Data$Languages=="japanese", 1, 0)
ManchesterCity910Data$turkish <- ifelse(ManchesterCity910Data$Languages=="turkish", 1, 0)
ManchesterCity910Data$asante <- ifelse(ManchesterCity910Data$Languages=="asante", 1, 0)
ManchesterCity910Data$persian <- ifelse(ManchesterCity910Data$Languages=="persian", 1, 0)
ManchesterCity910Data$maltese <- ifelse(ManchesterCity910Data$Languages=="maltese", 1, 0)
ManchesterCity910Data$romanian <- ifelse(ManchesterCity910Data$Languages=="romanian", 1, 0)
ManchesterCity910Data$slovak <- ifelse(ManchesterCity910Data$Languages=="slovak", 1, 0)
ManchesterCity910Data$PropLanguages <- 0
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "english", sum(ManchesterCity910Data$english), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "french", sum(ManchesterCity910Data$french), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "dutch", sum(ManchesterCity910Data$dutch), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "portuguese", sum(ManchesterCity910Data$portuguese), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "castilian", sum(ManchesterCity910Data$castilian), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "spanish", sum(ManchesterCity910Data$spanish), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "german", sum(ManchesterCity910Data$german), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "arabic", sum(ManchesterCity910Data$arabic), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "serbian", sum(ManchesterCity910Data$serbian), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "korean", sum(ManchesterCity910Data$korean), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "croatian", sum(ManchesterCity910Data$croatian), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "italian", sum(ManchesterCity910Data$italian), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "armenian", sum(ManchesterCity910Data$armenian), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "icelandic", sum(ManchesterCity910Data$icelandic), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "hebrew", sum(ManchesterCity910Data$hebrew), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "bokmal", sum(ManchesterCity910Data$bokmal), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "ukrainian", sum(ManchesterCity910Data$ukrainian), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "russian", sum(ManchesterCity910Data$russian), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "bosnian", sum(ManchesterCity910Data$bosnian), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "czech", sum(ManchesterCity910Data$czech), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "danish", sum(ManchesterCity910Data$danish), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "swedish", sum(ManchesterCity910Data$swedish), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "japanese", sum(ManchesterCity910Data$japanese), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "turkish", sum(ManchesterCity910Data$turkish), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "asante", sum(ManchesterCity910Data$asante), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "persian", sum(ManchesterCity910Data$persian), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "maltese", sum(ManchesterCity910Data$maltese), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "romanian", sum(ManchesterCity910Data$romanian), ManchesterCity910Data$PropLanguages)
ManchesterCity910Data$PropLanguages <- ifelse(ManchesterCity910Data$Languages == "slovak", sum(ManchesterCity910Data$slovak), ManchesterCity910Data$PropLanguages)

ManchesterUtd910Data <- subset(PL910SZN, Squad =="Manchester Utd")
ManchesterUtd910Data$english <- ifelse(ManchesterUtd910Data$Languages=="english", 1, 0)
ManchesterUtd910Data$french <- ifelse(ManchesterUtd910Data$Languages=="french", 1, 0)
ManchesterUtd910Data$dutch <- ifelse(ManchesterUtd910Data$Languages=="dutch", 1, 0)
ManchesterUtd910Data$portuguese <- ifelse(ManchesterUtd910Data$Languages=="portuguese", 1, 0)
ManchesterUtd910Data$castilian <- ifelse(ManchesterUtd910Data$Languages=="castilian", 1, 0)
ManchesterUtd910Data$spanish <- ifelse(ManchesterUtd910Data$Languages=="spanish", 1, 0)
ManchesterUtd910Data$german <- ifelse(ManchesterUtd910Data$Languages=="german", 1, 0)
ManchesterUtd910Data$arabic <- ifelse(ManchesterUtd910Data$Languages=="arabic", 1, 0)
ManchesterUtd910Data$serbian <- ifelse(ManchesterUtd910Data$Languages=="serbian", 1, 0)
ManchesterUtd910Data$korean <- ifelse(ManchesterUtd910Data$Languages=="korean", 1, 0)
ManchesterUtd910Data$croatian <- ifelse(ManchesterUtd910Data$Languages=="croatian", 1, 0)
ManchesterUtd910Data$italian <- ifelse(ManchesterUtd910Data$Languages=="italian", 1, 0)
ManchesterUtd910Data$armenian <- ifelse(ManchesterUtd910Data$Languages=="armenian", 1, 0)
ManchesterUtd910Data$icelandic <- ifelse(ManchesterUtd910Data$Languages=="icelandic", 1, 0)
ManchesterUtd910Data$hebrew <- ifelse(ManchesterUtd910Data$Languages=="hebrew", 1, 0)
ManchesterUtd910Data$bokmal <- ifelse(ManchesterUtd910Data$Languages=="bokmal", 1, 0)
ManchesterUtd910Data$ukrainian <- ifelse(ManchesterUtd910Data$Languages=="ukrainian", 1, 0)
ManchesterUtd910Data$russian <- ifelse(ManchesterUtd910Data$Languages=="russian", 1, 0)
ManchesterUtd910Data$bosnian <- ifelse(ManchesterUtd910Data$Languages=="bosnian", 1, 0)
ManchesterUtd910Data$czech <- ifelse(ManchesterUtd910Data$Languages=="czech", 1, 0)
ManchesterUtd910Data$danish <- ifelse(ManchesterUtd910Data$Languages=="danish", 1, 0)
ManchesterUtd910Data$swedish <- ifelse(ManchesterUtd910Data$Languages=="swedish", 1, 0)
ManchesterUtd910Data$japanese <- ifelse(ManchesterUtd910Data$Languages=="japanese", 1, 0)
ManchesterUtd910Data$turkish <- ifelse(ManchesterUtd910Data$Languages=="turkish", 1, 0)
ManchesterUtd910Data$asante <- ifelse(ManchesterUtd910Data$Languages=="asante", 1, 0)
ManchesterUtd910Data$persian <- ifelse(ManchesterUtd910Data$Languages=="persian", 1, 0)
ManchesterUtd910Data$maltese <- ifelse(ManchesterUtd910Data$Languages=="maltese", 1, 0)
ManchesterUtd910Data$romanian <- ifelse(ManchesterUtd910Data$Languages=="romanian", 1, 0)
ManchesterUtd910Data$slovak <- ifelse(ManchesterUtd910Data$Languages=="slovak", 1, 0)
ManchesterUtd910Data$PropLanguages <- 0
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "english", sum(ManchesterUtd910Data$english), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "french", sum(ManchesterUtd910Data$french), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "dutch", sum(ManchesterUtd910Data$dutch), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "portuguese", sum(ManchesterUtd910Data$portuguese), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "castilian", sum(ManchesterUtd910Data$castilian), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "spanish", sum(ManchesterUtd910Data$spanish), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "german", sum(ManchesterUtd910Data$german), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "arabic", sum(ManchesterUtd910Data$arabic), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "serbian", sum(ManchesterUtd910Data$serbian), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "korean", sum(ManchesterUtd910Data$korean), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "croatian", sum(ManchesterUtd910Data$croatian), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "italian", sum(ManchesterUtd910Data$italian), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "armenian", sum(ManchesterUtd910Data$armenian), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "icelandic", sum(ManchesterUtd910Data$icelandic), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "hebrew", sum(ManchesterUtd910Data$hebrew), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "bokmal", sum(ManchesterUtd910Data$bokmal), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "ukrainian", sum(ManchesterUtd910Data$ukrainian), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "russian", sum(ManchesterUtd910Data$russian), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "bosnian", sum(ManchesterUtd910Data$bosnian), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "czech", sum(ManchesterUtd910Data$czech), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "danish", sum(ManchesterUtd910Data$danish), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "swedish", sum(ManchesterUtd910Data$swedish), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "japanese", sum(ManchesterUtd910Data$japanese), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "turkish", sum(ManchesterUtd910Data$turkish), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "asante", sum(ManchesterUtd910Data$asante), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "persian", sum(ManchesterUtd910Data$persian), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "maltese", sum(ManchesterUtd910Data$maltese), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "romanian", sum(ManchesterUtd910Data$romanian), ManchesterUtd910Data$PropLanguages)
ManchesterUtd910Data$PropLanguages <- ifelse(ManchesterUtd910Data$Languages == "slovak", sum(ManchesterUtd910Data$slovak), ManchesterUtd910Data$PropLanguages)

Tottenham910Data <- subset(PL910SZN, Squad =="Tottenham")
Tottenham910Data$english <- ifelse(Tottenham910Data$Languages=="english", 1, 0)
Tottenham910Data$french <- ifelse(Tottenham910Data$Languages=="french", 1, 0)
Tottenham910Data$dutch <- ifelse(Tottenham910Data$Languages=="dutch", 1, 0)
Tottenham910Data$portuguese <- ifelse(Tottenham910Data$Languages=="portuguese", 1, 0)
Tottenham910Data$castilian <- ifelse(Tottenham910Data$Languages=="castilian", 1, 0)
Tottenham910Data$spanish <- ifelse(Tottenham910Data$Languages=="spanish", 1, 0)
Tottenham910Data$german <- ifelse(Tottenham910Data$Languages=="german", 1, 0)
Tottenham910Data$arabic <- ifelse(Tottenham910Data$Languages=="arabic", 1, 0)
Tottenham910Data$serbian <- ifelse(Tottenham910Data$Languages=="serbian", 1, 0)
Tottenham910Data$korean <- ifelse(Tottenham910Data$Languages=="korean", 1, 0)
Tottenham910Data$croatian <- ifelse(Tottenham910Data$Languages=="croatian", 1, 0)
Tottenham910Data$italian <- ifelse(Tottenham910Data$Languages=="italian", 1, 0)
Tottenham910Data$armenian <- ifelse(Tottenham910Data$Languages=="armenian", 1, 0)
Tottenham910Data$icelandic <- ifelse(Tottenham910Data$Languages=="icelandic", 1, 0)
Tottenham910Data$hebrew <- ifelse(Tottenham910Data$Languages=="hebrew", 1, 0)
Tottenham910Data$bokmal <- ifelse(Tottenham910Data$Languages=="bokmal", 1, 0)
Tottenham910Data$ukrainian <- ifelse(Tottenham910Data$Languages=="ukrainian", 1, 0)
Tottenham910Data$russian <- ifelse(Tottenham910Data$Languages=="russian", 1, 0)
Tottenham910Data$bosnian <- ifelse(Tottenham910Data$Languages=="bosnian", 1, 0)
Tottenham910Data$czech <- ifelse(Tottenham910Data$Languages=="czech", 1, 0)
Tottenham910Data$danish <- ifelse(Tottenham910Data$Languages=="danish", 1, 0)
Tottenham910Data$swedish <- ifelse(Tottenham910Data$Languages=="swedish", 1, 0)
Tottenham910Data$japanese <- ifelse(Tottenham910Data$Languages=="japanese", 1, 0)
Tottenham910Data$turkish <- ifelse(Tottenham910Data$Languages=="turkish", 1, 0)
Tottenham910Data$asante <- ifelse(Tottenham910Data$Languages=="asante", 1, 0)
Tottenham910Data$persian <- ifelse(Tottenham910Data$Languages=="persian", 1, 0)
Tottenham910Data$maltese <- ifelse(Tottenham910Data$Languages=="maltese", 1, 0)
Tottenham910Data$romanian <- ifelse(Tottenham910Data$Languages=="romanian", 1, 0)
Tottenham910Data$slovak <- ifelse(Tottenham910Data$Languages=="slovak", 1, 0)
Tottenham910Data$PropLanguages <- 0
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "english", sum(Tottenham910Data$english), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "french", sum(Tottenham910Data$french), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "dutch", sum(Tottenham910Data$dutch), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "portuguese", sum(Tottenham910Data$portuguese), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "castilian", sum(Tottenham910Data$castilian), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "spanish", sum(Tottenham910Data$spanish), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "german", sum(Tottenham910Data$german), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "arabic", sum(Tottenham910Data$arabic), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "serbian", sum(Tottenham910Data$serbian), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "korean", sum(Tottenham910Data$korean), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "croatian", sum(Tottenham910Data$croatian), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "italian", sum(Tottenham910Data$italian), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "armenian", sum(Tottenham910Data$armenian), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "icelandic", sum(Tottenham910Data$icelandic), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "hebrew", sum(Tottenham910Data$hebrew), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "bokmal", sum(Tottenham910Data$bokmal), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "ukrainian", sum(Tottenham910Data$ukrainian), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "russian", sum(Tottenham910Data$russian), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "bosnian", sum(Tottenham910Data$bosnian), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "czech", sum(Tottenham910Data$czech), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "danish", sum(Tottenham910Data$danish), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "swedish", sum(Tottenham910Data$swedish), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "japanese", sum(Tottenham910Data$japanese), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "turkish", sum(Tottenham910Data$turkish), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "asante", sum(Tottenham910Data$asante), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "persian", sum(Tottenham910Data$persian), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "maltese", sum(Tottenham910Data$maltese), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "romanian", sum(Tottenham910Data$romanian), Tottenham910Data$PropLanguages)
Tottenham910Data$PropLanguages <- ifelse(Tottenham910Data$Languages == "slovak", sum(Tottenham910Data$slovak), Tottenham910Data$PropLanguages)

PL910SZN <- rbind(Arsenal910Data, Chelsea910Data)
PL910SZN <- rbind(PL910SZN, Everton910Data)
PL910SZN <- rbind(PL910SZN, Liverpool910Data)
PL910SZN <- rbind(PL910SZN, ManchesterCity910Data)
PL910SZN <- rbind(PL910SZN, ManchesterUtd910Data)
PL910SZN <- rbind(PL910SZN, Tottenham910Data)

# removing irrelevant data
rm('Arsenal910Data')
rm('Arsenal1011Data')
rm('Arsenal1112Data')
rm('Arsenal1213Data')
rm('Arsenal1314Data')
rm('Arsenal1415Data')
rm('Arsenal1516Data')
rm('Arsenal1617Data')
rm('Arsenal1718Data')
rm('Arsenal1819Data')
rm('Chelsea910Data')
rm('Chelsea1011Data')
rm('Chelsea1112Data')
rm('Chelsea1213Data')
rm('Chelsea1314Data')
rm('Chelsea1415Data')
rm('Chelsea1516Data')
rm('Chelsea1617Data')
rm('Chelsea1718Data')
rm('Chelsea1819Data')
rm('Everton910Data')
rm('Everton1011Data')
rm('Everton1112Data')
rm('Everton1213Data')
rm('Everton1314Data')
rm('Everton1415Data')
rm('Everton1516Data')
rm('Everton1617Data')
rm('Everton1718Data')
rm('Everton1819Data')
rm('Liverpool910Data')
rm('Liverpool1011Data')
rm('Liverpool1112Data')
rm('Liverpool1213Data')
rm('Liverpool1314Data')
rm('Liverpool1415Data')
rm('Liverpool1516Data')
rm('Liverpool1617Data')
rm('Liverpool1718Data')
rm('Liverpool1819Data')
rm('ManchesterCity910Data')
rm('ManchesterCity1011Data')
rm('ManchesterCity1112Data')
rm('ManchesterCity1213Data')
rm('ManchesterCity1314Data')
rm('ManchesterCity1415Data')
rm('ManchesterCity1516Data')
rm('ManchesterCity1617Data')
rm('ManchesterCity1718Data')
rm('ManchesterCity1819Data')
rm('ManchesterUtd910Data')
rm('ManchesterUtd1011Data')
rm('ManchesterUtd1112Data')
rm('ManchesterUtd1213Data')
rm('ManchesterUtd1314Data')
rm('ManchesterUtd1415Data')
rm('ManchesterUtd1516Data')
rm('ManchesterUtd1617Data')
rm('ManchesterUtd1718Data')
rm('ManchesterUtd1819Data')
rm('Tottenham910Data')
rm('Tottenham1011Data')
rm('Tottenham1112Data')
rm('Tottenham1213Data')
rm('Tottenham1314Data')
rm('Tottenham1415Data')
rm('Tottenham1516Data')
rm('Tottenham1617Data')
rm('Tottenham1718Data')
rm('Tottenham1819Data')

#I will have to merge the nationality with official language here

#Then, create new binary variables for each individual language

#Then, create a new variable to demonstrate the prop of their team that speaks their language

#------------------------------------------

#Time to append all these datasets together. Do this last after creating the proportion of language variable.

#------------------------------------------

#2018-2019 Season Compared to 2017-2018 Season

Merged_1819 <- merge(PL1819SZN, PL1718SZN, by.x=c("Player"), by.y=c("Player"))

Merged_1819 <- Merged_1819[, -c(14:32)]

Merged_1819 <- Merged_1819[, -c(2:4)]

Merged_1819 <- Merged_1819[, -c(12:40)]

Merged_1819 <- Merged_1819[, -c(13:15)]

Merged_1819 <- Merged_1819[, -c(14:15)]

Merged_1819 <- Merged_1819[, -c(19:50)]

Merged_1819 <- Merged_1819[, -c(19:34)]

Merged_1819$`Performance >> Gls.x` <- as.numeric(Merged_1819$`Performance >> Gls.x`)

Merged_1819$`Performance >> Ast.x` <- as.numeric(Merged_1819$`Performance >> Ast.x`)

Merged_1819$PerformanceCurr <- (Merged_1819$`Performance >> Gls.x`+ Merged_1819$`Performance >> Ast.x`)

Merged_1819$`Performance >> Gls.y` <- as.numeric(Merged_1819$`Performance >> Gls.y`)

Merged_1819$`Performance >> Ast.y` <- as.numeric(Merged_1819$`Performance >> Ast.y`)

Merged_1819$PerformancePrev <- (Merged_1819$`Performance >> Gls.y`+ Merged_1819$`Performance >> Ast.y`)

Merged_1819$ProportionTeammateChange <- (Merged_1819$PropLanguages.x - Merged_1819$PropLanguages.y)

Merged_1819$PerformanceChange <- (Merged_1819$PerformanceCurr - Merged_1819$PerformancePrev)

Merged_1819$`Playing Time >> Mn/Ap` <- NULL

colnames(Merged_1819)[colnames(Merged_1819) == "Playing Time >> Apps"] <- "Playing Time >> MP.x"

colnames(Merged_1819)[colnames(Merged_1819) == "Playing Time >> MP"] <- "Playing Time >> MP.y"


Reg1 <- lm(PerformanceChange ~ ProportionTeammateChange, data=Merged_1819)

stargazer(Reg1,
          se=list(cse(Reg1)),
          title="Effect of Language on Performance", type="text",
          df=FALSE, digits=2)

ggplot(Merged_1819, aes(x=ProportionTeammateChange, y=PerformanceChange)) +
  labs(y = "Player Performance", x = "Proportion of Same Language" , title = "Player Performance and Same-speaking Teammates") +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=F)

ggplot(Merged_1819, aes(x=ProportionTeammateChange, y=PerformanceChange)) + geom_point(size=1.75)+
  geom_smooth(method=lm, se=F, aes(colour=Squad.x))

#---------------------------------------------

#2017-2018 Season Compared to 2016-2017 Season


Merged_1718 <- merge(PL1718SZN, PL1617SZN, by.x=c("Player"), by.y=c("Player"))

Merged_1718 <- Merged_1718[, -c(2:4)]

Merged_1718 <- Merged_1718[, -c(10:27)]

Merged_1718 <- Merged_1718[, -c(11:39)]

Merged_1718 <- Merged_1718[, -c(12:14)]

Merged_1718 <- Merged_1718[, -c(13:14)]

Merged_1718 <- Merged_1718[, -c(18:52)]

Merged_1718 <- Merged_1718[, -c(18:22)]

Merged_1718$`Performance >> Gls.x` <- as.numeric(Merged_1718$`Performance >> Gls.x`)

Merged_1718$`Performance >> Ast.x` <- as.numeric(Merged_1718$`Performance >> Ast.x`)

Merged_1718$PerformanceCurr <- (Merged_1718$`Performance >> Gls.x`+ Merged_1718$`Performance >> Ast.x`)

Merged_1718$`Performance >> Gls.y` <- as.numeric(Merged_1718$`Performance >> Gls.y`)

Merged_1718$`Performance >> Ast.y` <- as.numeric(Merged_1718$`Performance >> Ast.y`)

Merged_1718$PerformancePrev <- (Merged_1718$`Performance >> Gls.y`+ Merged_1718$`Performance >> Ast.y`)

Merged_1718$ProportionTeammateChange <- (Merged_1718$PropLanguages.x - Merged_1718$PropLanguages.y)

Merged_1718$PerformanceChange <- (Merged_1718$PerformanceCurr - Merged_1718$PerformancePrev)

Reg2 <- lm(PerformanceChange ~ ProportionTeammateChange, data=Merged_1718)

stargazer(Reg2,
          se=list(cse(Reg2)),
          title="Effect of Language on Performance", type="text",
          df=FALSE, digits=2)

ggplot(Merged_1718, aes(x=ProportionTeammateChange, y=PerformanceChange)) +
  labs(y = "Player Performance", x = "Proportion of Same Language" , title = "Player Performance and Same-speaking Teammates") +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=F)

ggplot(Merged_1718, aes(x=ProportionTeammateChange, y=PerformanceChange)) + geom_point(size=1.75)+
  geom_smooth(method=lm, se=F, aes(colour=Squad.x))

#---------------------------------------------

#2016-2017 Season Compared to 2015-2016 Season

Merged_1617 <- merge(PL1617SZN, PL1516SZN, by.x=c("Player"), by.y=c("Player"))

Merged_1617 <- Merged_1617[, -c(2:4)]

Merged_1617 <- Merged_1617[, -c(10:19)]

Merged_1617 <- Merged_1617[, -c(11:39)]

Merged_1617 <- Merged_1617[, -c(12:14)]

Merged_1617 <- Merged_1617[, -c(13:14)]

Merged_1617 <- Merged_1617[, -c(18:52)]

Merged_1617 <- Merged_1617[, -c(18:22)]

Merged_1617$`Performance >> Gls.x` <- as.numeric(Merged_1617$`Performance >> Gls.x`)

Merged_1617$`Performance >> Ast.x` <- as.numeric(Merged_1617$`Performance >> Ast.x`)

Merged_1617$PerformanceCurr <- (Merged_1617$`Performance >> Gls.x`+ Merged_1617$`Performance >> Ast.x`)

Merged_1617$`Performance >> Gls.y` <- as.numeric(Merged_1617$`Performance >> Gls.y`)

Merged_1617$`Performance >> Ast.y` <- as.numeric(Merged_1617$`Performance >> Ast.y`)

Merged_1617$PerformancePrev <- (Merged_1617$`Performance >> Gls.y`+ Merged_1617$`Performance >> Ast.y`)

Merged_1617$ProportionTeammateChange <- (Merged_1617$PropLanguages.x - Merged_1617$PropLanguages.y)

Merged_1617$PerformanceChange <- (Merged_1617$PerformanceCurr - Merged_1617$PerformancePrev)

Reg2 <- lm(PerformanceChange ~ ProportionTeammateChange, data=Merged_1617)

stargazer(Reg2,
          se=list(cse(Reg2)),
          title="Effect of Language on Performance", type="text",
          df=FALSE, digits=2)

ggplot(Merged_1617, aes(x=ProportionTeammateChange, y=PerformanceChange)) +
  labs(y = "Player Performance", x = "Proportion of Same Language" , title = "Player Performance and Same-speaking Teammates") +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=F)

ggplot(Merged_1617, aes(x=ProportionTeammateChange, y=PerformanceChange)) + geom_point(size=1.75)+
  geom_smooth(method=lm, se=F, aes(colour=Squad.x))

#---------------------------------------------

#2015-2016 Season Compared to 2014-2015 Season

Merged_1516 <- merge(PL1516SZN, PL1415SZN, by.x=c("Player"), by.y=c("Player"))

Merged_1516 <- Merged_1516[, -c(2:4)]

Merged_1516 <- Merged_1516[, -c(10:19)]

Merged_1516 <- Merged_1516[, -c(11:39)]

Merged_1516 <- Merged_1516[, -c(12:14)]

Merged_1516 <- Merged_1516[, -c(13:14)]

Merged_1516 <- Merged_1516[, -c(18:52)]

Merged_1516 <- Merged_1516[, -c(18:22)]

Merged_1516$`Performance >> Gls.x` <- as.numeric(Merged_1516$`Performance >> Gls.x`)

Merged_1516$`Performance >> Ast.x` <- as.numeric(Merged_1516$`Performance >> Ast.x`)

Merged_1516$PerformanceCurr <- (Merged_1516$`Performance >> Gls.x`+ Merged_1516$`Performance >> Ast.x`)

Merged_1516$`Performance >> Gls.y` <- as.numeric(Merged_1516$`Performance >> Gls.y`)

Merged_1516$`Performance >> Ast.y` <- as.numeric(Merged_1516$`Performance >> Ast.y`)

Merged_1516$PerformancePrev <- (Merged_1516$`Performance >> Gls.y`+ Merged_1516$`Performance >> Ast.y`)

Merged_1516$ProportionTeammateChange <- (Merged_1516$PropLanguages.x - Merged_1516$PropLanguages.y)

Merged_1516$PerformanceChange <- (Merged_1516$PerformanceCurr - Merged_1516$PerformancePrev)

Reg2 <- lm(PerformanceChange ~ ProportionTeammateChange, data=Merged_1516)

stargazer(Reg2,
          se=list(cse(Reg2)),
          title="Effect of Language on Performance", type="text",
          df=FALSE, digits=2)

ggplot(Merged_1516, aes(x=ProportionTeammateChange, y=PerformanceChange)) +
  labs(y = "Player Performance", x = "Proportion of Same Language" , title = "Player Performance and Same-speaking Teammates") +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=F)

ggplot(Merged_1516, aes(x=ProportionTeammateChange, y=PerformanceChange)) + geom_point(size=1.75)+
  geom_smooth(method=lm, se=F, aes(colour=Squad.x))

#---------------------------------------------

#2014-2015 Season Compared to 2013-2014 Season

Merged_1415 <- merge(PL1415SZN, PL1314SZN, by.x=c("Player"), by.y=c("Player"))

Merged_1415 <- Merged_1415[, -c(2:4)]

Merged_1415 <- Merged_1415[, -c(10:19)]

Merged_1415 <- Merged_1415[, -c(11:39)]

Merged_1415 <- Merged_1415[, -c(12:14)]

Merged_1415 <- Merged_1415[, -c(13:14)]

Merged_1415 <- Merged_1415[, -c(18:52)]

Merged_1415 <- Merged_1415[, -c(18:22)]

Merged_1415$`Performance >> Gls.x` <- as.numeric(Merged_1415$`Performance >> Gls.x`)

Merged_1415$`Performance >> Ast.x` <- as.numeric(Merged_1415$`Performance >> Ast.x`)

Merged_1415$PerformanceCurr <- (Merged_1415$`Performance >> Gls.x`+ Merged_1415$`Performance >> Ast.x`)

Merged_1415$`Performance >> Gls.y` <- as.numeric(Merged_1415$`Performance >> Gls.y`)

Merged_1415$`Performance >> Ast.y` <- as.numeric(Merged_1415$`Performance >> Ast.y`)

Merged_1415$PerformancePrev <- (Merged_1415$`Performance >> Gls.y`+ Merged_1415$`Performance >> Ast.y`)

Merged_1415$ProportionTeammateChange <- (Merged_1415$PropLanguages.x - Merged_1415$PropLanguages.y)

Merged_1415$PerformanceChange <- (Merged_1415$PerformanceCurr - Merged_1415$PerformancePrev)

Reg2 <- lm(PerformanceChange ~ ProportionTeammateChange, data=Merged_1415)

stargazer(Reg2,
          se=list(cse(Reg2)),
          title="Effect of Language on Performance", type="text",
          df=FALSE, digits=2)

ggplot(Merged_1415, aes(x=ProportionTeammateChange, y=PerformanceChange)) +
  labs(y = "Player Performance", x = "Proportion of Same Language" , title = "Player Performance and Same-speaking Teammates") +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=F)

ggplot(Merged_1415, aes(x=ProportionTeammateChange, y=PerformanceChange)) + geom_point(size=1.75)+
  geom_smooth(method=lm, se=F, aes(colour=Squad.x))

#---------------------------------------------

#2013-2014 Season Compared to 2012-2013 Season

Merged_1314 <- merge(PL1314SZN, PL1213SZN, by.x=c("Player"), by.y=c("Player"))

Merged_1314 <- Merged_1314[, -c(2:4)]

Merged_1314 <- Merged_1314[, -c(10:19)]

Merged_1314 <- Merged_1314[, -c(11:39)]

Merged_1314 <- Merged_1314[, -c(12:14)]

Merged_1314 <- Merged_1314[, -c(13:14)]

Merged_1314 <- Merged_1314[, -c(18:52)]

Merged_1314 <- Merged_1314[, -c(18:22)]

Merged_1314$`Performance >> Gls.x` <- as.numeric(Merged_1314$`Performance >> Gls.x`)

Merged_1314$`Performance >> Ast.x` <- as.numeric(Merged_1314$`Performance >> Ast.x`)

Merged_1314$PerformanceCurr <- (Merged_1314$`Performance >> Gls.x`+ Merged_1314$`Performance >> Ast.x`)

Merged_1314$`Performance >> Gls.y` <- as.numeric(Merged_1314$`Performance >> Gls.y`)

Merged_1314$`Performance >> Ast.y` <- as.numeric(Merged_1314$`Performance >> Ast.y`)

Merged_1314$PerformancePrev <- (Merged_1314$`Performance >> Gls.y`+ Merged_1314$`Performance >> Ast.y`)

Merged_1314$ProportionTeammateChange <- (Merged_1314$PropLanguages.x - Merged_1314$PropLanguages.y)

Merged_1314$PerformanceChange <- (Merged_1314$PerformanceCurr - Merged_1314$PerformancePrev)

Reg2 <- lm(PerformanceChange ~ ProportionTeammateChange, data=Merged_1314)

stargazer(Reg2,
          se=list(cse(Reg2)),
          title="Effect of Language on Performance", type="text",
          df=FALSE, digits=2)

ggplot(Merged_1314, aes(x=ProportionTeammateChange, y=PerformanceChange)) +
  labs(y = "Player Performance", x = "Proportion of Same Language" , title = "Player Performance and Same-speaking Teammates") +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=F)

ggplot(Merged_1314, aes(x=ProportionTeammateChange, y=PerformanceChange)) + geom_point(size=1.75)+
  geom_smooth(method=lm, se=F, aes(colour=Squad.x))

#---------------------------------------------

#2012-2013 Season Compared to 2011-2012 Season

Merged_1213 <- merge(PL1213SZN, PL1112SZN, by.x=c("Player"), by.y=c("Player"))

Merged_1213 <- Merged_1213[, -c(2:4)]

Merged_1213 <- Merged_1213[, -c(10:19)]

Merged_1213 <- Merged_1213[, -c(11:39)]

Merged_1213 <- Merged_1213[, -c(12:14)]

Merged_1213 <- Merged_1213[, -c(13:14)]

Merged_1213 <- Merged_1213[, -c(18:52)]

Merged_1213 <- Merged_1213[, -c(18:22)]

Merged_1213$`Performance >> Gls.x` <- as.numeric(Merged_1213$`Performance >> Gls.x`)

Merged_1213$`Performance >> Ast.x` <- as.numeric(Merged_1213$`Performance >> Ast.x`)

Merged_1213$PerformanceCurr <- (Merged_1213$`Performance >> Gls.x`+ Merged_1213$`Performance >> Ast.x`)

Merged_1213$`Performance >> Gls.y` <- as.numeric(Merged_1213$`Performance >> Gls.y`)

Merged_1213$`Performance >> Ast.y` <- as.numeric(Merged_1213$`Performance >> Ast.y`)

Merged_1213$PerformancePrev <- (Merged_1213$`Performance >> Gls.y`+ Merged_1213$`Performance >> Ast.y`)

Merged_1213$ProportionTeammateChange <- (Merged_1213$PropLanguages.x - Merged_1213$PropLanguages.y)

Merged_1213$PerformanceChange <- (Merged_1213$PerformanceCurr - Merged_1213$PerformancePrev)

Reg2 <- lm(PerformanceChange ~ ProportionTeammateChange, data=Merged_1213)

stargazer(Reg2,
          se=list(cse(Reg2)),
          title="Effect of Language on Performance", type="text",
          df=FALSE, digits=2)

ggplot(Merged_1213, aes(x=ProportionTeammateChange, y=PerformanceChange)) +
  labs(y = "Player Performance", x = "Proportion of Same Language" , title = "Player Performance and Same-speaking Teammates") +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=F)

ggplot(Merged_1213, aes(x=ProportionTeammateChange, y=PerformanceChange)) + geom_point(size=1.75)+
  geom_smooth(method=lm, se=F, aes(colour=Squad.x))

#---------------------------------------------

#2011-2012 Season Compared to 2010-2011 Season

Merged_1112 <- merge(PL1112SZN, PL1011SZN, by.x=c("Player"), by.y=c("Player"))

Merged_1112 <- Merged_1112[, -c(2:4)]

Merged_1112 <- Merged_1112[, -c(10:19)]

Merged_1112 <- Merged_1112[, -c(11:39)]

Merged_1112 <- Merged_1112[, -c(12:14)]

Merged_1112 <- Merged_1112[, -c(13:14)]

Merged_1112 <- Merged_1112[, -c(18:52)]

Merged_1112 <- Merged_1112[, -c(18:22)]

Merged_1112$`Performance >> Gls.x` <- as.numeric(Merged_1112$`Performance >> Gls.x`)

Merged_1112$`Performance >> Ast.x` <- as.numeric(Merged_1112$`Performance >> Ast.x`)

Merged_1112$PerformanceCurr <- (Merged_1112$`Performance >> Gls.x`+ Merged_1112$`Performance >> Ast.x`)

Merged_1112$`Performance >> Gls.y` <- as.numeric(Merged_1112$`Performance >> Gls.y`)

Merged_1112$`Performance >> Ast.y` <- as.numeric(Merged_1112$`Performance >> Ast.y`)

Merged_1112$PerformancePrev <- (Merged_1112$`Performance >> Gls.y`+ Merged_1112$`Performance >> Ast.y`)

Merged_1112$ProportionTeammateChange <- (Merged_1112$PropLanguages.x - Merged_1112$PropLanguages.y)

Merged_1112$PerformanceChange <- (Merged_1112$PerformanceCurr - Merged_1112$PerformancePrev)

Reg2 <- lm(PerformanceChange ~ ProportionTeammateChange, data=Merged_1112)

stargazer(Reg2,
          se=list(cse(Reg2)),
          title="Effect of Language on Performance", type="text",
          df=FALSE, digits=2)

ggplot(Merged_1112, aes(x=ProportionTeammateChange, y=PerformanceChange)) +
  labs(y = "Player Performance", x = "Proportion of Same Language" , title = "Player Performance and Same-speaking Teammates") +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=F)

ggplot(Merged_1112, aes(x=ProportionTeammateChange, y=PerformanceChange)) + geom_point(size=1.75)+
  geom_smooth(method=lm, se=F, aes(colour=Squad.x))

#---------------------------------------------

#2010-2011 Season Compared to 2009-2010 Season

Merged_1011 <- merge(PL1011SZN, PL910SZN, by.x=c("Player"), by.y=c("Player"))

Merged_1011 <- Merged_1011[, -c(2:4)]

Merged_1011 <- Merged_1011[, -c(10:19)]

Merged_1011 <- Merged_1011[, -c(11:39)]

Merged_1011 <- Merged_1011[, -c(12:14)]

Merged_1011 <- Merged_1011[, -c(13:14)]

Merged_1011 <- Merged_1011[, -c(18:52)]

Merged_1011 <- Merged_1011[, -c(18:22)]

Merged_1011$`Performance >> Gls.x` <- as.numeric(Merged_1011$`Performance >> Gls.x`)

Merged_1011$`Performance >> Ast.x` <- as.numeric(Merged_1011$`Performance >> Ast.x`)

Merged_1011$PerformanceCurr <- (Merged_1011$`Performance >> Gls.x`+ Merged_1011$`Performance >> Ast.x`)

Merged_1011$`Performance >> Gls.y` <- as.numeric(Merged_1011$`Performance >> Gls.y`)

Merged_1011$`Performance >> Ast.y` <- as.numeric(Merged_1011$`Performance >> Ast.y`)

Merged_1011$PerformancePrev <- (Merged_1011$`Performance >> Gls.y`+ Merged_1011$`Performance >> Ast.y`)

Merged_1011$ProportionTeammateChange <- (Merged_1011$PropLanguages.x - Merged_1011$PropLanguages.y)

Merged_1011$PerformanceChange <- (Merged_1011$PerformanceCurr - Merged_1011$PerformancePrev)

Reg2 <- lm(PerformanceChange ~ ProportionTeammateChange, data=Merged_1011)

stargazer(Reg2,
          se=list(cse(Reg2)),
          title="Effect of Language on Performance", type="text",
          df=FALSE, digits=2)

ggplot(Merged_1011, aes(x=ProportionTeammateChange, y=PerformanceChange)) +
  labs(y = "Player Performance", x = "Proportion of Same Language" , title = "Player Performance and Same-speaking Teammates") +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=F)

ggplot(Merged_1011, aes(x=ProportionTeammateChange, y=PerformanceChange)) + geom_point(size=1.75)+
  geom_smooth(method=lm, se=F, aes(colour=Squad.x))

#appending the merged datasets

appended_data <- rbind(Merged_1819, Merged_1718)

appended_data <- rbind(appended_data, Merged_1617)  

appended_data <- rbind(appended_data, Merged_1516)

appended_data <- rbind(appended_data, Merged_1415)

appended_data <- rbind(appended_data, Merged_1314)

appended_data <- rbind(appended_data, Merged_1213)

appended_data <- rbind(appended_data, Merged_1112)

appended_data <- rbind(appended_data, Merged_1011)

appended_data$`Playing Time >> Starts.x` <- as.numeric(appended_data$`Playing Time >> Starts.x`)

appended_data$`Playing Time >> Starts.y` <- as.numeric(appended_data$`Playing Time >> Starts.y`)

appended_data$`Playing Time >> Min.x` <- as.numeric(gsub(",","",appended_data$`Playing Time >> Min.x`))

appended_data$`Playing Time >> Min.y` <- as.numeric(gsub(",","",appended_data$`Playing Time >> Min.y`))

appended_data$`Playing Time >> MP.x` <- as.numeric(appended_data$`Playing Time >> MP.x`)

appended_data$`Playing Time >> MP.y` <- as.numeric(appended_data$`Playing Time >> MP.y`)

appended_data$Age.x <- as.numeric(appended_data$Age.x)

appended_data$PlayedMatchDifference <- (appended_data$`Playing Time >> MP.x` - appended_data$`Playing Time >> MP.y`)

appended_data$MinuteDifference <- (appended_data$`Playing Time >> Min.x` - appended_data$`Playing Time >> Min.y`)

appended_data$StartDifference <- (appended_data$`Playing Time >> Starts.x` - appended_data$`Playing Time >> Starts.y`)

Reg1 <- lm(PerformanceChange ~ ProportionTeammateChange + MinuteDifference, data=appended_data)

Reg2 <- lm(PerformanceChange ~ ProportionTeammateChange + MinuteDifference + StartDifference +
             PlayedMatchDifference, data=appended_data)

Reg3 <- lm(PerformanceChange ~ ProportionTeammateChange + MinuteDifference + StartDifference +
             PlayedMatchDifference + Age.x, data=appended_data)

Reg4 <- lm(PerformanceChange ~ ProportionTeammateChange + MinuteDifference + StartDifference +
             PlayedMatchDifference + Age.x + I(Age.x^2) +
             (ProportionTeammateChange * MinuteDifference), data=appended_data)

stargazer(Reg1, Reg2, Reg3, Reg4,
          se=list(cse(Reg1),cse(Reg2), cse(Reg3), cse(Reg4)),
          title="Effect of Language on Performance", type="text",
          covariate.labels = c(("Change in number of teammates with their langauge"),
                               "Difference in minutes played per season",
                               "Difference in starts per season",
                               "Difference in matches played per season",
                               "Age of the player",
                               "Age of the player squared",
                               "Interaction on teammates and minutes played",
                               "Intercept"),
          df=FALSE, digits=2)

ggplot(appended_data, aes(x=ProportionTeammateChange, y=PerformanceChange)) +
  labs(y = "Player Performance", x = "Proportion of Same Language" , title = "Player Performance and Same-speaking Teammates") +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=F)

ggplot(appended_data, aes(x=ProportionTeammateChange, y=PerformanceChange)) + geom_point(size=1.75)+
  geom_smooth(method=lm, se=F, aes(colour=Squad.x))

stargazer(appended_data[c("PerformanceChange","ProportionTeammateChange", "MinuteDifference", "StartDifference", "PlayedMatchDifference", "Age.x")], type="text",
          digits=2, title="Descriptive Statistics ")

