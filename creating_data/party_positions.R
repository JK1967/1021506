# Project: MPhil dissertation in Politcs -- Oxford
# Date: April 2019
# Author: ANONYMOUS
# description: this file creates the positions of parties and merges all results into the final database. 





library(sjlabelled)
library(dplyr)
library(tidyr)
library(Hmisc)
library(readxl)
library(dplyr)
library(haven)
library(lubridate)
library(manifestoR)


#Code creates the database with party positions from CHES and CMP CHES for positions and blurring, CMP for salience. 
#using CMP for salience because CHES 2014 doesn't have salience for immigration related dimensions. 

ches_trend <- read_dta("1999-2014_CHES_dataset_means-3.dta")
ches_2014_expert <- read_dta("2014_CHES_dataset_expert-level.dta")
ches_2010_expert <- read_dta("2010_CHES_dataset_expert-level.dta")
ches_trend <- remove_all_labels(ches_trend) #turn off to see labels



#####################ches: Adding expert SDs for 2014 data############################


##First for 2014 expert level data
###Adding expert SD for  Left Right
ches_trend$lrecon_sd <- NA
for (i in ches_trend$party_id ) {
  ches_trend$lrecon_sd[ches_trend$party_id==i & ches_trend$year == 2014] = sd(ches_2014_expert$lrecon[ches_2014_expert$party_id==i],na.rm=T)}

###Adding expert SD for galtan
ches_trend$galtan_sd <- NA
for (i in ches_trend$party_id ) {
  ches_trend$galtan_sd[ches_trend$party_id==i & ches_trend$year == 2014] = sd(ches_2014_expert$galtan[ches_2014_expert$party_id==i],na.rm=T)}

###Adding expert SD for EU position
ches_trend$eu_dimension_sd <- NA
for (i in ches_trend$party_id ) {
  ches_trend$eu_dimension_sd[ches_trend$party_id==i & ches_trend$year == 2014] = sd(ches_2014_expert$eu_position[ches_2014_expert$party_id==i],na.rm=T)}

###Adding expert SD for immigration position
ches_trend$immigration_dimension_sd <- NA
for (i in ches_trend$party_id ) {
  ches_trend$immigration_dimension_sd[ches_trend$party_id==i & ches_trend$year == 2014] = sd(ches_2014_expert$immigrate_policy[ches_2014_expert$party_id==i],na.rm=T)}


######now for 2010
###Adding expert SD for  Left Right
for (i in ches_trend$party_id ) {
  ches_trend$lrecon_sd[ches_trend$party_id==i & ches_trend$year == 2010] = sd(ches_2010_expert$lrecon[ches_2010_expert$party_id==i],na.rm=T)}

###Adding expert SD for galtan
for (i in ches_trend$party_id ) {
  ches_trend$galtan_sd[ches_trend$party_id==i & ches_trend$year == 2010] = sd(ches_2010_expert$galtan[ches_2010_expert$party_id==i],na.rm=T)}

###Adding expert SD for EU position
for (i in ches_trend$party_id ) {
  ches_trend$eu_dimension_sd[ches_trend$party_id==i & ches_trend$year == 2010] = sd(ches_2010_expert$position[ches_2010_expert$party_id==i],na.rm=T)}

###Adding expert SD for immigration position
for (i in ches_trend$party_id ) {
  ches_trend$immigration_dimension_sd[ches_trend$party_id==i & ches_trend$year == 2010] = sd(ches_2010_expert$immigrate_policy[ches_2010_expert$party_id==i],na.rm=T)}





#####################ches: Creating new variables in the CHES dataset########################

#reversing order of variables on immigration 10 now pro globalization. 
ches_trend$immigration_dimension <- ches_trend$immigrate_policy #runs from 0 to 10
cols <- c("immigration_dimension")
ches_trend[,cols] <- 10 - ches_trend[,cols]

#EU dimension
ches_trend$eu_dimension <- ches_trend$position #runs from 1 to 7

#note that both dimensions are not the same length, thus before combining I need to equalize, happens below

#combining all EU and all immigration scores into one big dimensions (convert all to 0-10 and then add up)
#EU variables (10p scale) # normalized = (x-min(x))/(max(x)-min(x))
ches_trend$eu_dimension <- ches_trend$eu_dimension -1
ches_trend$eu_dimension <- ches_trend$eu_dimension /6 * 10 
ches_trend$globalization <- NA
ches_trend$globalization <- (ches_trend$eu_dimension + ches_trend$immigration_dimension) /2 

#library(psych)
#x <- data.frame(as.numeric(ches_trend$eu_dimension[ches_trend$eastwest == 1 & ches_trend$year == 2014 | ches_trend$year == 2010 ]),  as.numeric(ches_trend$immigration_dimension[ches_trend$eastwest == 1 & ches_trend$year == 2014 | ches_trend$year == 2010 ]))
#psych::alpha(x) #Alpha is really low, so shouldn't use these as one globalization dimension
#detach("package:psych", unload=TRUE)
#rm(x)

##########Combining blurring (SD) scores for summarized new dimension
#globalization dimension: both above dimensions.
ches_trend$globalization_sd <- NA
ches_trend$globalization_sd <- (ches_trend$immigration_dimension_sd + ches_trend$eu_dimension_sd) /2




#####################ches: embedding/position scores##########################
###########Party distance from mean
##EU dimension
#creating variable for mean distance in a country
x <- aggregate(eu_dimension ~ country + year, data = ches_trend, mean) #aggregate because it groups together nicely
colnames(x)[which(names(x) == "eu_dimension")] <- "mean_eu_dimension_country"
ches_trend <- left_join(ches_trend, x, by= c("country","year")) #add back to dataframe

#function to make mean difference
ches_trend$eu_dimension_distance_mean <- NA
for (i in ches_trend$party_id){
  ches_trend$eu_dimension_distance_mean[ches_trend$party_id==i] <- (ches_trend$eu_dimension[ches_trend$party_id==i] - ches_trend$mean_eu_dimension_country[ches_trend$party_id==i])
}
#remove negative numbers by *-1 to get absolute distance from the country mean for each variable
ches_trend$eu_dimension_distance_mean <- ifelse(ches_trend$eu_dimension_distance_mean < 0, ches_trend$eu_dimension_distance_mean *-1, ches_trend$eu_dimension_distance_mean)


##immigration dimension
#creating variable for mean distance in a country
x <- aggregate(immigration_dimension ~ country + year, data = ches_trend, mean)
colnames(x)[which(names(x) == "immigration_dimension")] <- "mean_immigration_dimension_country"
ches_trend <- left_join(ches_trend, x, by= c("country","year"))

#function to make mean difference
ches_trend$immigration_dimension_distance_mean <- NA
for (i in ches_trend$party_id){
  ches_trend$immigration_dimension_distance_mean[ches_trend$party_id==i] <- (ches_trend$immigration_dimension[ches_trend$party_id==i] - ches_trend$mean_immigration_dimension_country[ches_trend$party_id==i])
}
#remove negative numbers by *-1 to get absolute distance from the country mean for each variable
ches_trend$immigration_dimension_distance_mean <- ifelse(ches_trend$immigration_dimension_distance_mean < 0, ches_trend$immigration_dimension_distance_mean *-1, ches_trend$immigration_dimension_distance_mean)


##globalization dimension
#creating variable for mean distance in a country
x <- aggregate(globalization ~ country + year, data = ches_trend, mean)
colnames(x)[which(names(x) == "globalization")] <- "mean_globalization_country"
ches_trend <- left_join(ches_trend, x, by= c("country","year"))

#function to make mean difference
ches_trend$globalization_distance_mean <- NA
for (i in ches_trend$party_id){
  ches_trend$globalization_distance_mean[ches_trend$party_id==i] <- (ches_trend$globalization[ches_trend$party_id==i] - ches_trend$mean_globalization_country[ches_trend$party_id==i])
}
#remove negative numbers by *-1 to get absolute distance from the country mean for each variable
ches_trend$globalization_distance_mean <- ifelse(ches_trend$globalization_distance_mean < 0, ches_trend$globalization_distance_mean *-1, ches_trend$globalization_distance_mean)

rm(x)
















#####################cmp: salience for all variables#################
#mp_setapikey("manifesto_apikey.txt")
#cmp <- mp_maindataset()
#saveRDS(cmp, file = "cmp.rds")
cmp <- readRDS("cmp.rds")

#creating the salience scores by combining the percentage of statements. 
#salience eu
x <- aggregate((per108+per110) ~ party+edate, data = cmp, sum) #aggregates per party year combination. 
colnames(x) <- c("cmp_id", "year", "eu_salience")           
x <- subset(x, year > "2005-01-01") #takes only years relevant in relation to CHES
x$year <- format(as.Date(x$year, format="%Y/%m/%d"),"%Y") #changes year to CHES format

#Salience immigration 
y <- aggregate((per608+per607+per602+per601) ~ party+edate, data = cmp, sum)
colnames(y) <- c("cmp_id", "year", "immi_salience")  
y <- subset(y, year > "2005-01-01")
y$year <- format(as.Date(y$year, format="%Y/%m/%d"),"%Y")

#salience globalization
z <- aggregate((per108+per110+per608+per607+per602+per601) ~ party+edate, data = cmp, sum)
colnames(z) <- c("cmp_id", "year", "glob_salience")    
z <- subset(z, year > "2005-01-01")
z$year <- format(as.Date(z$year, format="%Y/%m/%d"),"%Y")

#merge three frames into one:
cmp_party_pos <- inner_join(x, y)
cmp_party_pos <- inner_join(cmp_party_pos, z)
rm(x,y,z)

#group years into the same years as used for the CHES data
cmp_party_pos$year <- ifelse(cmp_party_pos$year > 2011, 2014, 2009) 

#create the mean score for each party over the elections in the timerange. 
x <- aggregate(eu_salience ~ cmp_id + year, data = cmp_party_pos, mean)
y <- aggregate(immi_salience ~ cmp_id + year, data = cmp_party_pos, mean)
z <- aggregate(glob_salience ~ cmp_id + year, data = cmp_party_pos, mean)

#join together
cmp_party_pos <- full_join(x, y)
cmp_party_pos <- full_join(cmp_party_pos, z)







#####################merge: Merging everything######################
#taking only useful columns from ches_trend.
master_file <- subset(ches_trend, ches_trend$year > 2006 & ches_trend$eastwest == 1)
master_file <- subset(master_file, select = -c(eumember, epvote, eu_fiscal, eu_employ, eu_budgets, eu_environ, eu_agri, eu_asylum, eu_turkey, cosmo, cosmo_salience, us, us_salience))

#changing name of ID variable for merge and match up years: changing EES 2009 year to CHES 2010. 
names(party_vot_var_14_09)[names(party_vot_var_14_09) == 'party_ches'] <- 'party_id'
party_vot_var_14_09$year <- ifelse(party_vot_var_14_09$year == 2009, 2010, party_vot_var_14_09$year)

#join the data from EES in there. inner_join because I need all data from both files to be there. 
master_file <- inner_join(master_file, party_vot_var_14_09) 

#join the data from CMP in there. left_join because I want to keep all rows in the master file and only rows with corresponding values in CMP
cmp_party_pos$year <- ifelse(cmp_party_pos$year == 2009, 2010, cmp_party_pos$year)
names(cmp_party_pos)[names(cmp_party_pos) == 'eu_salience'] <- 'cmp_eu_salience'
names(cmp_party_pos)[names(cmp_party_pos) == 'immi_salience'] <- 'cmp_immi_salience'
names(cmp_party_pos)[names(cmp_party_pos) == 'glob_salience'] <- 'cmp_glob_salience'




master_file <- left_join(master_file, cmp_party_pos)


###########Add variables to master file: controls, merges from variables within file, controls, etc #####################
#who the mainstream parties are
master_file$mainstream <- ifelse(master_file$family == 2 | master_file$family == 3 | master_file$family == 4 | master_file$family == 5, 1, 0)
#1       rad right
#2            cons
#3         liberal
#4       christdem
#5       socialist
#6        rad left
#7           green
#8     regionalist
#9       no family
#10    confessional
#11 agrarian/centre

##issue entrepeneurship (Hobolt & De Vries 2015): salience X distance party position and average all parties 
#made with both ches and cmp salience. Ches is what Vries Hobolt do. Yet, immi salience only exists in 2010 for Ches
master_file$issue_entrep_chessal_eu <- NA
master_file$issue_entrep_cmpsal_eu <- NA
master_file$issue_entrep_chessal_immi <- NA
master_file$issue_entrep_cmpsal_immi <- NA
master_file$issue_entrep_chessal_glob <- NA
master_file$issue_entrep_cmpsal_glob <- NA
master_file$issue_entrep_chessal_eu <- master_file$eu_dimension_distance_mean * master_file$eu_salience
master_file$issue_entrep_cmpsal_eu <- master_file$eu_dimension_distance_mean * master_file$cmp_eu_salience
master_file$issue_entrep_chessal_immi <- master_file$immigration_dimension_distance_mean * master_file$immigra_salience
master_file$issue_entrep_cmpsal_immi <- master_file$immigration_dimension_distance_mean * master_file$cmp_immi_salience
master_file$issue_entrep_chessal_glob <- (master_file$issue_entrep_chessal_eu + master_file$issue_entrep_chessal_immi) /2
master_file$issue_entrep_cmpsal_glob <- (master_file$issue_entrep_cmpsal_eu + master_file$issue_entrep_cmpsal_immi) /2

##Government participation (as control variable)
#recode 0.5 to 1: 0.5 is only in govn part of the year
master_file$govt[master_file$govt == 0.5] <- 1




##country with names
master_file$country_name <- master_file$country
master_file$country_name[master_file$country_name == 1 ] <- "Belgium"
master_file$country_name[master_file$country_name == 2 ] <- "Denmark"
master_file$country_name[master_file$country_name == 3 ] <- "Germany"
master_file$country_name[master_file$country_name == 5 ] <- "Spain"
master_file$country_name[master_file$country_name == 6 ] <- "France"
master_file$country_name[master_file$country_name == 7 ] <- "Ireland"
master_file$country_name[master_file$country_name == 8 ] <- "Italy"
master_file$country_name[master_file$country_name == 10 ] <- "Netherlands"
master_file$country_name[master_file$country_name == 11 ] <- "UK"
master_file$country_name[master_file$country_name == 12 ] <- "Portugal"
master_file$country_name[master_file$country_name == 13 ] <- "Austria"
master_file$country_name[master_file$country_name == 14 ] <- "Finland"
master_file$country_name[master_file$country_name == 16 ] <- "Sweden"
master_file$country_name[master_file$country_name == 38 ] <- "Luxembourg"


##Correct vot_overl for maximum number of parties in a country

master_file$vot_overl_cor <- master_file$vot_overl
master_file$vot_overl_cor[master_file$country_name == "Belgium" & master_file$year == 2014] <- master_file$vot_overl_cor / 11
master_file$vot_overl_cor[master_file$country_name == "Denmark" & master_file$year == 2014] <- master_file$vot_overl_cor / 7
master_file$vot_overl_cor[master_file$country_name == "Germany" & master_file$year == 2014] <- master_file$vot_overl_cor / 7
master_file$vot_overl_cor[master_file$country_name == "Spain" & master_file$year == 2014] <- master_file$vot_overl_cor / 11
master_file$vot_overl_cor[master_file$country_name == "France" & master_file$year == 2014] <- master_file$vot_overl_cor / 6
master_file$vot_overl_cor[master_file$country_name == "Ireland" & master_file$year == 2014] <- master_file$vot_overl_cor / 6
master_file$vot_overl_cor[master_file$country_name == "Italy" & master_file$year == 2014] <- master_file$vot_overl_cor / 9
master_file$vot_overl_cor[master_file$country_name == "Netherlands" & master_file$year == 2014] <- master_file$vot_overl_cor / 9
master_file$vot_overl_cor[master_file$country_name == "UK" & master_file$year == 2014] <- master_file$vot_overl_cor / 7
master_file$vot_overl_cor[master_file$country_name == "Portugal" & master_file$year == 2014] <- master_file$vot_overl_cor / 6
master_file$vot_overl_cor[master_file$country_name == "Austria" & master_file$year == 2014] <- master_file$vot_overl_cor / 6
master_file$vot_overl_cor[master_file$country_name == "Finland" & master_file$year == 2014] <- master_file$vot_overl_cor / 8
master_file$vot_overl_cor[master_file$country_name == "Sweden" & master_file$year == 2014] <- master_file$vot_overl_cor / 10
master_file$vot_overl_cor[master_file$country_name == "Luxembourg" & master_file$year == 2014] <- master_file$vot_overl_cor / 6

master_file$vot_overl_cor[master_file$country_name == "Belgium" & master_file$year == 2010] <- master_file$vot_overl_cor / 11
master_file$vot_overl_cor[master_file$country_name == "Denmark" & master_file$year == 2010] <- master_file$vot_overl_cor / 7
master_file$vot_overl_cor[master_file$country_name == "Germany" & master_file$year == 2010] <- master_file$vot_overl_cor / 7
master_file$vot_overl_cor[master_file$country_name == "Spain" & master_file$year == 2010] <- master_file$vot_overl_cor / 11
master_file$vot_overl_cor[master_file$country_name == "France" & master_file$year == 2010] <- master_file$vot_overl_cor / 6
master_file$vot_overl_cor[master_file$country_name == "Ireland" & master_file$year == 2010] <- master_file$vot_overl_cor / 6
master_file$vot_overl_cor[master_file$country_name == "Italy" & master_file$year == 2010] <- master_file$vot_overl_cor / 9
master_file$vot_overl_cor[master_file$country_name == "Netherlands" & master_file$year == 2010] <- master_file$vot_overl_cor / 9
master_file$vot_overl_cor[master_file$country_name == "UK" & master_file$year == 2010] <- master_file$vot_overl_cor / 7
master_file$vot_overl_cor[master_file$country_name == "Portugal" & master_file$year == 2010] <- master_file$vot_overl_cor / 6
master_file$vot_overl_cor[master_file$country_name == "Austria" & master_file$year == 2010] <- master_file$vot_overl_cor / 6
master_file$vot_overl_cor[master_file$country_name == "Finland" & master_file$year == 2010] <- master_file$vot_overl_cor / 8
master_file$vot_overl_cor[master_file$country_name == "Sweden" & master_file$year == 2010] <- master_file$vot_overl_cor / 10
master_file$vot_overl_cor[master_file$country_name == "Luxembourg" & master_file$year == 2010] <- master_file$vot_overl_cor / 6

master_file$vot_overl_cor <- master_file$vot_overl_cor * 100


#add effective threshold for countries
master_file$elec_thres <- NA

#electoral thresholds are effectives ones for those countries that have them and legal ones for the other countries. 
master_file$elec_thres[master_file$country_name == "Belgium"] <- 1.54
master_file$elec_thres[master_file$country_name == "Denmark"] <- 1.64
master_file$elec_thres[master_file$country_name == "Germany"] <- 5
master_file$elec_thres[master_file$country_name == "Spain"] <- 1.35
master_file$elec_thres[master_file$country_name == "France"] <- NA
master_file$elec_thres[master_file$country_name == "Ireland"] <- 2.35
master_file$elec_thres[master_file$country_name == "Italy"] <- 0.4
master_file$elec_thres[master_file$country_name == "Netherlands"] <- 0.67
master_file$elec_thres[master_file$country_name == "UK"] <- NA
master_file$elec_thres[master_file$country_name == "Portugal"] <- 1.36
master_file$elec_thres[master_file$country_name == "Austria"] <- 4
master_file$elec_thres[master_file$country_name == "Finland"] <- 1.35
master_file$elec_thres[master_file$country_name == "Sweden"] <- 4
master_file$elec_thres[master_file$country_name == "Luxembourg"] <- 2.95

#note that France and the UK get an NA value because The validity of the effective threshold has been questioned regarding its use in the context of pluralitymajoritarian systems.70 Following this criticism, plurality-majoritarian as well as mixed-member
# majoritarian systems71. See Abou Chadi (2018, p. 9)



########Save master file#############################
#master_file <- readRDS("master_file.rds")
#saveRDS(master_file, file = "master_file_v2.rds")
#saveRDS(master_file, file = "master_file_backup.rds")
#write.csv(master_file , "master_file.csv", row.names=FALSE)
#write.csv(master_file , "master_file_v2.csv", row.names=FALSE)































