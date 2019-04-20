# Project: MPhil dissertation in Politcs -- Oxford
# Date: April 2019
# description: this code creates the Dyad ratios plot.


#### - packages ####
library(dplyr)
library(tidyr)
library(Hmisc)
library(readxl)
library(readxl)
library(dplyr)
library(haven)
library(manifestoR)
library(lubridate)
source( 'stimson.R' )


#### - CHES 1999-2014 ####
#import data
ches <-
  read.csv("1999-2014-chess-means.csv", stringsAsFactors = F) #check in excel whether some values for dimensions are written in words. Needs to be changed to right values if downloading a fresh dataset

#remove parties with seat share less than 1 and parties not in Western-Europe
#ches <- subset(ches, seat >= 1)
ches <-
  subset(ches, eastwest == "west")  #drop all not in EU 15. p2 on Ches handbook for party list

#turn into 0 to 100.
ches$position_ratio <-
  ches$position - 1 # -1 because scale starts at 1 and 1/7 is not 0.
ches$position_ratio <- ches$position_ratio / 6 * 100

positions_ches <- aggregate(position_ratio ~ year, data = ches, mean)
positions_ches$house <- "CHES"

#adding the N for each year
describe(ches$position_ratio[ches$year == 2002])
positions_ches$nyear <- c(142, 98, 115, 125, 143)


#### -Chess 2017 ####

ches17 <-
  read.csv("CHES_means_2017.csv", stringsAsFactors = F) #check in excel whether some values for dimensions are written in words. Needs to be changed to right values if downloading a fresh dataset
#ches17 <- subset(ches17, seat >= 1) #drop all less than one seat

#add east west variable (9 west countries) and drop not west
ches17$eastwest <-
  ifelse(
    ches17$country == "ger" |
      ches17$country == "esp" |
      ches17$country == "fr" |
      ches17$country == "gr" |
      ches17$country == "it" |
      ches17$country == "nl" |
      ches17$country == "por" |
      ches17$country == "swe" | ches17$country == "uk",
    "west",
    "east"
  )
ches17 <-
  subset(ches17, eastwest == "west")  #drop all not in EU 15. p2 on Ches handbook for party list

#add a ratio measure for position for every single party position.
ches17$position_ratio <- ches17$position - 1
ches17$position_ratio <- ches17$position_ratio / 6 * 100

#add position
positions_ches17 <-
  aggregate(position_ratio ~ year, data = ches17, mean)
positions_ches17$house <- "CHES"


#add N
#describe(ches17$position_ratio[ches17$year == 2017])
positions_ches17$nyear <- c(90)





#### - R&M ####

##Now to add positions for the RayMarks data
raymar <-
  read.csv("1984-1999_dataset_means.csv", stringsAsFactors = F)
raymar <- subset(raymar, eastwest == 1) #all are west

#add ratio for all parties
raymar$position_ratio <- raymar$position - 1
raymar$position_ratio <- raymar$position_ratio / 6 * 100

positions_rm <- aggregate(position_ratio ~ year, data = raymar, mean)
positions_rm$house <- "raymar"


#add N
#describe(raymar$position_ratio[raymar$year == 1984])
positions_rm$nyear <- c(126, 135, 138, 144, 141)






#### -R&W ####
WR_0813 <- read_dta("WR_2008_2013_recoded.dta")
#View(WR_0813)
#add East West variable. Note 14 countries in East, no Luxembourg
WR_0813$eastwest <-
  ifelse(
    WR_0813$country == 15 |
      WR_0813$country == 16 |
      WR_0813$country == 19 |
      WR_0813$country == 20 |
      WR_0813$country == 25 |
      WR_0813$country == 18 |
      WR_0813$country == 21 |
      WR_0813$country == 22 |
      WR_0813$country == 23 |
      WR_0813$country == 27 |
      WR_0813$country == 24 |
      WR_0813$country == 14 |
      WR_0813$country == 17 | WR_0813$country == 26,
    "west",
    "east"
  )
WR_0813 <- subset(WR_0813, WR_0813$eastwest == "west")

#create a new variable combining three different EU measures, like in the paper
WR_0813$position_q121315 <-
  (WR_0813$q12 + WR_0813$q13 + WR_0813$q15) / 3
describe(WR_0813$position_q121315)

#create new variable with the 0 to 1 scale for the European question (q13)
WR_0813$q12_ratio <- WR_0813$q12 - 1
WR_0813$q12_ratio <- WR_0813$q12_ratio / 6 * 100

WR_0813$position_q121315_ratio <- WR_0813$position_q121315 - 1
WR_0813$position_q121315_ratio <-
  WR_0813$position_q121315_ratio / 6 * 100
describe(WR_0813$position_q121315_ratio)
describe(WR_0813$partyname)


#add in data frame
positions_wr <-
  aggregate(position_q121315_ratio ~ year, data = WR_0813, mean)
positions_wr$house <- "RW"

#add N
describe(WR_0813$q12_ratio[WR_0813$year == 2008])
positions_wr$nyear <- c(114, 108)

colnames(positions_wr)[2] <- "position_ratio"



#### - CMP ####
mp_setapikey("manifesto_apikey.txt")
CMP <- mp_maindataset()

###doing basic preparations
#creating new variable with the year
CMP$year <- format(as.Date(CMP$edate, format = "%Y/%m/%d"), "%Y")
CMP$year <- as.numeric(CMP$year)

#creating a new variable for eastwest and dropping rest of dataset:
CMP$eastwest <-
  ifelse(
    CMP$country == 21 |
      CMP$country == 13 |
      CMP$country == 41 |
      CMP$country == 34 |
      CMP$country == 33 |
      CMP$country == 31 |
      CMP$country == 53 |
      CMP$country == 32 |
      CMP$country == 22 |
      CMP$country == 51 |
      CMP$country == 35 |
      CMP$country == 42 |
      CMP$country == 14 |
      CMP$country == 11 | CMP$country == 38  ,
    "west",
    "eastorelse"
  )
CMP <- subset(CMP, eastwest == "west") #keeping EU15


#adding EU position and ratio for every single party.
CMP$position <- CMP$per108 - CMP$per110 #position
calculation <-
  CMP$per108 + CMP$per110 #variable to use for calculation
CMP$ratio <- CMP$position / calculation #ratio position
rm(calculation) #cleaning up
describe(CMP$ratio) #Ratio is from -1 to 1. Want it from 0 to 1
CMP$ratio_01 <- CMP$ratio / 2 + 0.5  #rescale to make from 0 to 1
CMP$ratio_01 <-
  CMP$ratio_01 * 100 #make bigger to make same as other variables
describe(CMP$ratio_01)
#adding who the mainstream parties are
#CMP$mainstream <- ifelse(CMP$parfam == 20 | CMP$parfam == 30 | CMP$parfam == 40 | CMP$parfam == 50 | CMP$parfam == 60, 1, 0) #adding value to indicate mainstream party

positions_cmp <- aggregate(ratio_01 ~ year, data = CMP, mean)
positions_cmp$house <- "CMP"


#Add N with function. To change for different party families change the mainstream condition. Change years by choosing different input vector
ncmp <- function(yearr) {
  x <-
    describe(CMP$ratio_01[CMP$year == yearr]) #year is input from function
  y <- (x[[4]][1]) #indexes item one in the 4th list. Which is N
  z <- as.numeric(y) #turn into numeric
  return(z)
}

positions_cmp$nyear <- as.numeric(lapply(positions_cmp$year, ncmp))


colnames(positions_cmp)[2] <- "position_ratio"





#### - Positions Benoit & Laver #####
library(haven)
PPMD_summary_data <-
  read_dta(
    "PPMD_summary_data.dta"
  )
#View(PPMD_summary_data)

PPMD_summary_data$eastwest <-
  ifelse(
    PPMD_summary_data$Country == 52 |
      PPMD_summary_data$Country == 53 |
      PPMD_summary_data$Country == 54 |
      PPMD_summary_data$Country == 57 |
      PPMD_summary_data$Country == 58 |
      PPMD_summary_data$Country == 59 |
      PPMD_summary_data$Country == 60 |
      PPMD_summary_data$Country == 65 |
      PPMD_summary_data$Country == 68 |
      PPMD_summary_data$Country == 73 |
      PPMD_summary_data$Country == 74 |
      PPMD_summary_data$Country == 72 |
      PPMD_summary_data$Country == 61 |
      PPMD_summary_data$Country == 63 |
      PPMD_summary_data$Country == 66,
    "west",
    "east"
  )
PPMD_summary_data <-
  subset(PPMD_summary_data, PPMD_summary_data$eastwest == "west")
PPMD_summary_data$year <- 2003

PPMD_summary_data_eu <-
  subset(PPMD_summary_data, PPMD_summary_data$Dimension == 24) #only keep dimension that is for EU
positions_benlav <-
  aggregate(Mean ~ year, data = PPMD_summary_data_eu, mean) #getting mean


#rescale variable and make in right direction. One in favour.normalized = (x-min(x))/(max(x)-min(x))
positions_benlav$Mean <- ((positions_benlav$Mean - 1) / (20 - 1)) * 100

#add house n and change name
positions_benlav$house <- "benlav"
positions_benlav$nyear <- 196
colnames(positions_benlav)[2] <- "position_ratio"




#### - merge into one database ##### 
#merge into one datbase
positions_eu <- bind_rows(positions_ches, positions_ches17)
positions_eu <- bind_rows(positions_eu, positions_rm)
positions_eu <- bind_rows(positions_eu, positions_wr)
positions_eu <- bind_rows(positions_eu, positions_benlav)
positions_eu <- bind_rows(positions_eu, positions_cmp)

positions_eu$year <-
  as.Date(ISOdate(positions_eu$year, 1, 1))  # beginning of year


####changing order of the columns. Right order: house, year, mean, n. Thus house to front.
positions_eu <- positions_eu[, c(3, 1, 2, 4)]





#### - Same code for migration ##### 

#### - RW ####
#create new variable with the 0 to 1 scale for the European question (q13)
WR_0813$immigration <- WR_0813$q18
WR_0813$immigration <- WR_0813$immigration - 1
WR_0813$immigration <- WR_0813$immigration / 6 * 100

#describe(WR_0813$immigration[WR_0813$year==2008])

#add in data frame
positions_wr_im <-
  aggregate(WR_0813$immigration ~ WR_0813$year, data = WR_0813, mean)
positions_wr_im$house <- "RW"

#add N
#describe(WR_0813$immigration[WR_0813$year == 2013])
positions_wr_im$nyear <- c(114, 108)

colnames(positions_wr_im)[2] <- "position_ratio_im"
colnames(positions_wr_im)[1] <- "year"




#### - CHES#### 
ches$immigration <- ches$immigrate_policy * 10
#describe(ches$immigration)

positions_ches_im <- aggregate(immigration ~ year, data = ches, mean)
positions_ches_im$house <- "CHES"

#adding the N for each year
positions_ches_im$nyear <- c(112, 125, 142)

colnames(positions_ches_im)[2] <- "position_ratio_im"




#### - CHES17####
ches17$immigration <- ches17$immigrate_policy * 10

#add position
positions_ches17_im <-
  aggregate(immigration ~ year, data = ches17, mean)
positions_ches17_im$house <- "CHES"


#add N
#describe(ches17$immigration[ches17$year == 2017])
positions_ches17_im$nyear <- c(89)

colnames(positions_ches17_im)[2] <- "position_ratio_im"

#### - R&M #### 
#no data on migration



#### - Benoit & Laver####
#describe(PPMD_summary_data_eu$Mean[PPMD_summary_data_eu$Dimension == 19]) #19 is immigration dimension

PPMD_summary_data_im <-
  subset(PPMD_summary_data, PPMD_summary_data$Dimension == 19) #only keep dimension that is for EU
positions_benlav_im <-
  aggregate(Mean ~ year, data = PPMD_summary_data_im, mean) #getting mean



#rescale variable and make in right direction. One in favour.normalized = (x-min(x))/(max(x)-min(x))
positions_benlav_im$Mean <-
  ((positions_benlav_im$Mean - 1) / (20 - 1)) * 100
positions_benlav_im$Mean <- 101 - positions_benlav_im$Mean

describe(PPMD_summary_data$Mean[PPMD_summary_data$Dimension == 19])
#add house n and change name
positions_benlav_im$house <- "benlav"
positions_benlav_im$nyear <- 226
colnames(positions_benlav_im)[2] <- "position_ratio_im"




#standardization proposed by CMP project
#some stuff proposed by CMP European Integration (Position ratio): (per108 - per110)/(per108 + per110) (visualizationt tab)
CMP$immi_pos_rat <-
  ((CMP$per607 + CMP$per601) - (CMP$per608 + CMP$per602)) / (CMP$per607 +
                                                               CMP$per601 + CMP$per608 + CMP$per602)
CMP$immi_pos_rat <- CMP$immi_pos_rat / 2 + 0.5
CMP$immi_pos_rat <- CMP$immi_pos_rat * 100
describe(CMP$immi_pos_rat)

positions_cmp_im <- aggregate(immi_pos_rat ~ year, data = CMP, mean)
positions_cmp_im$house <- "CMP"

#Add N with function. To change for different party families change the mainstream condition. Change years by choosing different input vector
ncmp <- function(yearr) {
  x <-
    describe(CMP$immi_pos_rat[CMP$year == yearr]) #year is input from function
  y <- (x[[4]][1]) #indexes item one in the 4th list. Which is N
  z <- as.numeric(y) #turn into numeric
  return(z)
}

positions_cmp_im$nyear <-
  as.numeric(lapply(positions_cmp_im$year, ncmp))


colnames(positions_cmp_im)[2] <- "position_ratio_im"




#merge into one datbase
positions_im <- bind_rows(positions_ches_im, positions_ches17_im)
positions_im <- bind_rows(positions_im, positions_wr_im)
positions_im <- bind_rows(positions_im, positions_benlav_im)
positions_im <- bind_rows(positions_im, positions_cmp_im)

positions_im$year <-
  as.Date(ISOdate(positions_im$year, 1, 1))  # beginning of year


#changing order of the columns. Right order: house, year, mean, n. Thus house to front.
positions_im <- positions_im[, c(3, 1, 2, 4)]







#### - applying dyad: EU ####
output <-
  extract(
    varname = positions_eu$house,
    date = positions_eu$year,
    index = positions_eu$position_ratio,
    ncases = positions_eu$nyear
  )



#plotting dyad
year <- as.numeric(output$period)
latent <- as.numeric(output$latent1)
eu_pos <- as.data.frame(year)
eu_pos$position <- latent

library(ggplot2)
des_pos_eu <-
  ggplot(data = eu_pos, aes(x = year, y = position)) + geom_line(size = 1) +
  theme_bw() + coord_cartesian(ylim = c(55, 75)) +
  scale_x_continuous(minor_breaks = 1920:2030, breaks = (
    c(
      1946,
      1950,
      1955,
      1960,
      1965,
      1970,
      1975,
      1980,
      1985,
      1990,
      1995,
      2000,
      2005,
      2010,
      2017
    )
  )) +
  xlab("Year") + ylab("EU Position") + ggtitle("Position on EU in Western-Europe")
des_pos_eu







####the problem: aggregating the datasets
des_pos_eu_allmeas <-
  ggplot(data = positions_cmp, aes(x = year, y = position_ratio, colour =
                                     "grey")) +
  geom_line() +
  geom_line(data = positions_ches, aes(x = year, y = position_ratio, colour =
                                         "red")) +
  geom_point(data = positions_ches17, aes(x = year, y = position_ratio, colour =
                                            "blue")) +
  geom_line(data = positions_rm, aes(x = year, y = position_ratio, colour =
                                       "green")) +
  geom_line(data = positions_wr, aes(x = year, y = position_ratio, colour =
                                       "yellow")) +
  geom_point(data = positions_benlav, aes(x = year, y = position_ratio, colour =
                                            "orange")) +
  theme_bw() + xlab("Year") + ylab("EU Position") + ggtitle("Different measures of EU position") +
  scale_color_identity(
    name = "Measures",
    labels = c("CHES17", "Ray-Marks", "CMP", "Ben-Lav", "CHES", "RW"),
    guide = "legend"
  ) +
  scale_x_continuous(minor_breaks = 1920:2030, breaks = (
    c(
      1946,
      1955,
      1960,
      1965,
      1970,
      1975,
      1980,
      1985,
      1990,
      1995,
      2000,
      2005,
      2010,
      2017
    )
  ))

des_pos_eu_allmeas #saved as 700x350









#### - applying dyad: immigration ####


output_im <-
  extract(
    varname = positions_im$house,
    date = positions_im$year,
    index = positions_im$position_ratio,
    ncases = positions_im$nyear
  )



#plotting dyad
year <- as.numeric(output_im$period)
latent <- as.numeric(output_im$latent1)
im_pos <- as.data.frame(year)
im_pos$position <- latent

library(ggplot2)
des_pos_im <-
  ggplot(data = im_pos, aes(x = year, y = position)) + geom_line(size = 1) +
  theme_bw() + coord_cartesian(ylim = c(46, 73)) +
  scale_x_continuous(minor_breaks = 1920:2030, breaks = (
    c(
      1946,
      1950,
      1955,
      1960,
      1965,
      1970,
      1975,
      1980,
      1985,
      1990,
      1995,
      2000,
      2005,
      2010,
      2017
    )
  )) +
  xlab("Year") + ylab("Immigration Position") + ggtitle("Position on immigration in Western-Europe")
des_pos_im





#### - Plots combining both position and immigration in a single plot ####


im_pos$type <- "Immigration"
eu_pos$type <- "EU"

library(dplyr)
eu_immi_pos <- full_join(im_pos, eu_pos)

des_pos_imeu <-
  ggplot(data = eu_immi_pos, aes(x = year, y = position, color = type)) + geom_line(size =
                                                                                      1) +
  theme_bw() + coord_cartesian(ylim = c(46, 73)) +
  scale_x_continuous(minor_breaks = 1920:2030, breaks = (
    c(
      1946,
      1950,
      1955,
      1960,
      1965,
      1970,
      1975,
      1980,
      1985,
      1990,
      1995,
      2000,
      2005,
      2010,
      2017
    )
  )) +
  labs(x = "Year", y = "Position", color = "") +
  scale_color_manual(values = c("EU" = "blue", "Immigration" = "black")) +
  theme(legend.position = "bottom")


des_pos_imeu

#saved as 700X350





#### - the problem: aggregating the datasets ####
des_pos_im_allmeas <-
  ggplot(data = positions_cmp_im, aes(x = year, y = position_ratio_im, colour =
                                        "grey")) +
  geom_line() +
  geom_line(data = positions_ches, aes(x = year, y = position_ratio, colour =
                                         "red")) +
  geom_point(data = positions_ches17, aes(x = year, y = position_ratio, colour =
                                            "blue")) +
  geom_line(data = positions_wr, aes(x = year, y = position_ratio, colour =
                                       "yellow")) +
  geom_point(data = positions_benlav, aes(x = year, y = position_ratio, colour =
                                            "orange")) +
  theme_bw() + xlab("Year") + ylab("Immigration Position") + ggtitle("Different measures of immigration position") +
  scale_color_identity(
    name = "Measures",
    labels = c("CHES17", "CMP", "Ben-Lav", "CHES", "RW"),
    guide = "legend"
  ) +
  scale_x_continuous(minor_breaks = 1920:2030, breaks = (
    c(
      1946,
      1955,
      1960,
      1965,
      1970,
      1975,
      1980,
      1985,
      1990,
      1995,
      2000,
      2005,
      2010,
      2017
    )
  ))

des_pos_im_allmeas #saved as 700x350




