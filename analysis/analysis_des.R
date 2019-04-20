# Project: MPhil dissertation in Politcs -- Oxford
# Date: April 2019
# description: this data file creates the results for the descriptive chapter. 










#### - loading packages ####
library(sjlabelled)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Hmisc)
library(readxl)
library(readxl)
library(dplyr)
library(haven)
library(manifestoR)



#### - subsetting ####
mp_setapikey("manifesto_apikey.txt")
CMP <- mp_maindataset()

#only keeping parties in my countries. 
CMP$eastwest <- ifelse(CMP$country == 21 | CMP$country == 13 | CMP$country == 41 | CMP$country == 34 | CMP$country == 33 | CMP$country == 31 | CMP$country == 53 | CMP$country == 32 | CMP$country == 22 | CMP$country == 51 | CMP$country == 35 | CMP$country == 42 | CMP$country == 14 | CMP$country == 11 | CMP$country == 38  , "west", "eastorelse")
CMP <- subset(CMP, eastwest == "west") #keeping EU15

#creating new variable with the year
library(lubridate)
CMP$year <- format(as.Date(CMP$edate, format="%Y/%m/%d"),"%Y")
CMP$year <- as.numeric(CMP$year)




######## - analysis over time###########


#### - CMP salience#############
###European Integration salience#
#saliences for issues
CMP$position <- CMP$per108+CMP$per110 #variable is called position because that is the name in CHES. 
CMP$immi <- CMP$per601 + CMP$per602 + CMP$per607 + CMP$per608 




####EU and immi salience combined
emp_eu_immi <- ggplot(data=CMP, aes(x=edate, y=position, colour="EU")) +
  geom_smooth(method="loess", span = 0.1, show.legend = TRUE) + theme_bw() + coord_cartesian(ylim=c(0, 7)) + 
  scale_x_date(date_minor_breaks = "1 year", date_labels = "%Y", breaks=as.Date(c("1945-01-01", "1950-01-0", "1955-01-01",
                                                                                  "1960-01-01", "1965-01-01", "1970-01-01",
                                                                                  "1975-01-01", "1980-01-01", "1985-01-01", 
                                                                                  "1990-01-01", "1995-01-01", "2000-01-01",
                                                                                  "2005-01-01", "2010-01-01", "2017-01-01"))) +
  xlab("Election Date") + ylab("Emphasis") + 
  geom_smooth(method="loess", span = 0.1, data=CMP, aes(x=edate, y=immi, colour="Immigration"),
              show.legend = TRUE, linetype=6) +
  scale_colour_manual(name="", values=c("blue", "black")) + theme(legend.position="bottom")

emp_eu_immi #saved as 700 X 350








#### - CMP positions calculation#################
#creating variable for mean EU salience in a country
x <- aggregate(position ~ country + edate, data = CMP, mean) #aggregate because it groups together nicely
colnames(x)[which(names(x) == "position")] <- "mean_eu_salience_country" #change column name
CMP <- left_join(CMP, x, by= c("country","edate")) #add back to dataframe

#creating variable for mean immi salience in a country
x <- aggregate(immi ~ country + edate, data = CMP, mean) #aggregate because it groups together nicely
colnames(x)[which(names(x) == "immi")] <- "mean_immi_salience_country" #change column name
CMP <- left_join(CMP, x, by= c("country","edate")) #add back to dataframe



#Create position variables for party positions by subtracting negative from positive (i.e 608 is negative)
CMP$position_pos <- NA
CMP$immi_pos <- NA
CMP$position_pos <- (CMP$per108 - CMP$per110) / (CMP$per108 + CMP$per110)
CMP$immi_pos <- ((CMP$per607+CMP$per601) - (CMP$per608+CMP$per602)) / (CMP$per607+CMP$per601+CMP$per608+CMP$per602)
CMP$position_pos <- CMP$position_pos /2 + 0.5
CMP$position_pos <- CMP$position_pos * 100
CMP$immi_pos <- CMP$immi_pos /2 + 0.5
CMP$immi_pos <- CMP$immi_pos * 100
describe(CMP$position_pos)
describe(CMP$immi_pos)
#creating variable for mean EU position in a country
x <- aggregate(position_pos ~ country + edate, data = CMP, mean) #aggregate because it groups together nicely
colnames(x)[which(names(x) == "position_pos")] <- "mean_eu_country" #change column name
CMP <- left_join(CMP, x, by= c("country","edate")) #add back to dataframe

#creating variable for mean immigration position in a country
x <- aggregate(immi_pos ~ country + edate, data = CMP, mean) #aggregate because it groups together nicely
colnames(x)[which(names(x) == "immi_pos")] <- "mean_immi_country" #change column name
CMP <- left_join(CMP, x, by= c("country","edate")) #add back to dataframe
rm(x)




#take distance from country mean for a party on EU
CMP$eu_dimension_distance_mean <- NA
for (i in CMP$party){
  CMP$eu_dimension_distance_mean[CMP$party==i] <- (CMP$position_pos[CMP$party==i] - CMP$mean_eu_country[CMP$party==i])
}
#remove negative numbers by taking absolute distance
CMP$eu_dimension_distance_mean <- abs(CMP$eu_dimension_distance_mean)

describe(CMP$eu_dimension_distance_mean)


#take distance from country mean for a party on immigration
CMP$immi_dimension_distance_mean <- NA
for (i in CMP$party){
  CMP$immi_dimension_distance_mean[CMP$party==i] <- (CMP$immi_pos[CMP$party==i] - CMP$mean_immi_country[CMP$party==i])
}
#remove negative numbers by *-1 to get absolute distance from the country mean for each variable
CMP$immi_dimension_distance_mean <- abs(CMP$immi_dimension_distance_mean)





#### - CMP entrepreneurship################
#multiply distance from country with salience to get entrepreneurship score for EU and immi
CMP$entr_immi <- NA
CMP$entr_eu <- NA
CMP$entr_immi <- CMP$immi_dimension_distance_mean * CMP$immi
CMP$entr_eu <- CMP$eu_dimension_distance_mean * CMP$position



#plot for EU Entrepreneurship both in same plot
des_ent_eu_immi <- ggplot(data=CMP, aes(x=edate, y=entr_eu, colour="EU")) +
  geom_smooth(method="loess", span = 0.1, show.legend = TRUE) + theme_bw() + coord_cartesian(ylim=c(0, 201)) + 
  scale_x_date(date_minor_breaks = "1 year", date_labels = "%Y", breaks=as.Date(c("1945-01-01", "1950-01-01", "1955-01-01",
                                                                                  "1960-01-01", "1965-01-01", "1970-01-01",
                                                                                  "1975-01-01", "1980-01-01", "1985-01-01", 
                                                                                  "1990-01-01", "1995-01-01", "2000-01-01",
                                                                                  "2005-01-01", "2010-01-01", "2017-01-01"))) +
  xlab("Election Date") + ylab("EU Entrepreneurship") + geom_smooth(method="loess", span = 0.1, data=CMP, 
                                                                    aes(x=edate, y=entr_immi, colour="Immigration"),
                                                                    show.legend = TRUE, linetype=6) +
  scale_colour_manual(name="", values=c("blue", "black")) + theme(legend.position="bottom")
  
  
  
des_ent_eu_immi #saved as 700 X350







#### - CMP Division among parties#################
#for both using the variable made before: the absolute distance from the country mean for parties smoothened out
#####division plot two in one
div_euim <- ggplot(data=CMP, aes(x=edate, y=eu_dimension_distance_mean, colour="EU")) +
  geom_smooth(method="loess", span = 0.1, show.legend = TRUE) + theme_bw() + coord_cartesian(ylim=c(0, 40)) + 
  scale_x_date(date_minor_breaks = "1 year", date_labels = "%Y", breaks=as.Date(c("1945-01-01", "1950-01-0", "1955-01-01",
                                                                                  "1960-01-01", "1965-01-01", "1970-01-01",
                                                                                  "1975-01-01", "1980-01-01", "1985-01-01", 
                                                                                  "1990-01-01", "1995-01-01", "2000-01-01",
                                                                                  "2005-01-01", "2010-01-01", "2017-01-01"))) +
  xlab("Election Date") + ylab("Aver dist from cntry mean") + 
  geom_smooth(method="loess", span = 0.1, data=CMP, aes(x=edate, y=immi_dimension_distance_mean, colour="Immigration"),
              show.legend = TRUE, linetype=6) +
  scale_colour_manual(name="", values=c("blue", "black")) + theme(legend.position="bottom")

div_euim #saved as 700 X350


#### - CMP blurring#################
#empty variables
CMP$position_blur <- NA
CMP$immi_blur <- NA
#make score
CMP$position_blur <- (abs((CMP$per108 - CMP$per110))) / (CMP$per108 + CMP$per110)
CMP$immi_blur <- (abs(((CMP$per607+CMP$per601) - (CMP$per608+CMP$per602)))) / (CMP$per608+CMP$per602+CMP$per607+CMP$per601)
#reverse to make interpretation intuitive
CMP$position_blur <- 1 - CMP$position_blur
CMP$immi_blur <- 1 - CMP$immi_blur
#no position taking is not blurring
CMP$position_blur[CMP$per108== 0 & CMP$per110 ==0] <- 0
CMP$immi_blur[CMP$per607==0 & CMP$per601==0 & CMP$per608==0 & CMP$per602==0] <- 0


##plot
blur_euimmi <- ggplot(data=CMP, aes(x=edate, y=position_blur, colour="EU")) +
  geom_smooth(method="loess", span = 0.1, show.legend = TRUE) + theme_bw() + coord_cartesian(ylim=c(0, 0.55)) + 
  scale_x_date(date_minor_breaks = "1 year", date_labels = "%Y", breaks=as.Date(c("1945-01-01", "1950-01-0", "1955-01-01",
                                                                                  "1960-01-01", "1965-01-01", "1970-01-01",
                                                                                  "1975-01-01", "1980-01-01", "1985-01-01", 
                                                                                  "1990-01-01", "1995-01-01", "2000-01-01",
                                                                                  "2005-01-01", "2010-01-01", "2017-01-01"))) +
  xlab("Election Date") + ylab("Blurring") + 
  geom_smooth(method="loess", span = 0.1, data=CMP, aes(x=edate, y=immi_blur, colour="Immigration"),
              show.legend = TRUE, linetype=6) +
  scale_colour_manual(name="", values=c("blue", "black")) + theme(legend.position="bottom")

blur_euimmi #saved as 700X350



#### - CMP Embedding######
#creating position variables. _bas (basic) because I don't adjust them to be from 0 to 100. 
CMP$position_pos_bas <- NA # 1is pro EU (i.e. 108 is positive)
CMP$immi_pos_bas <- NA #1 is pro migration (only saying positive things. )
CMP$position_pos_bas <- (CMP$per108 - CMP$per110) / (CMP$per108 + CMP$per110)
CMP$immi_pos_bas <- ((CMP$per607+CMP$per601) - (CMP$per608+CMP$per602)) / (CMP$per607+CMP$per601+CMP$per608+CMP$per602)



##creating GALTAN variable
#gal=Green-Alternative-Libertarian and Traditional-Authoritarian-Nationalist
CMP$galtan_pos_bas <- NA  #1 is more progressive. 
CMP$galtan_pos_bas <- ((CMP$per106 + CMP$per107  + CMP$per501 + CMP$per502 + CMP$per604 + CMP$per606) - 
  (CMP$per603 + CMP$per605 + CMP$per109)) / (CMP$per106 + CMP$per107  + CMP$per501 + CMP$per502 + CMP$per604 + 
                                               CMP$per606 + CMP$per603 + CMP$per605 + CMP$per109)


##creating a left-right variable
CMP$lefrig2 <- NA #higher values are more right
CMP$lefrig2 <- ((CMP$per401 + CMP$per402 + CMP$per407 + CMP$per414 + CMP$per505 + CMP$per702) -
  (CMP$per403 + CMP$per404 + CMP$per405 + CMP$per406 + CMP$per409 + CMP$per412 + CMP$per415 + CMP$per504 + CMP$per701)) /
  (CMP$per401 + CMP$per402 + CMP$per407 + CMP$per414 + CMP$per505 + CMP$per702 + CMP$per403 + CMP$per404 + CMP$per405 + 
     CMP$per406 + CMP$per409 + CMP$per412 + CMP$per415 + CMP$per504 + CMP$per701)


#get rid of NA cases so lm error doesn't kill loop
CMP2 <- subset(CMP, !is.na(position_pos_bas) & !is.na(galtan_pos_bas) & !is.na(immi_pos_bas) & !is.na(lefrig2))




###Code
library("broom")
eugaltan <- CMP2 %>% group_by(country, date) %>% do(model = lm(position_pos_bas~galtan_pos_bas, data = .)) %>% glance(model) %>% select(country, date, `r.squared`)
eulefrig <- CMP2 %>% group_by(country, date) %>% do(model = lm(position_pos_bas~lefrig2, data = .)) %>% glance(model) %>% select(country, date, `r.squared`)

immigaltan <- CMP2 %>% group_by(country, date) %>% do(model = lm(immi_pos_bas~galtan_pos_bas, data = .)) %>% glance(model) %>% select(country, date, `r.squared`)
immilefrig <- CMP2 %>% group_by(country, date) %>% do(model = lm(immi_pos_bas~lefrig2, data = .)) %>% glance(model) %>% select(country, date, `r.squared`)

#to check how integrated left right and galtan are. 
galtanlefr <- CMP2 %>% group_by(country, date) %>% do(model = lm(lefrig2~galtan_pos_bas, data = .)) %>% glance(model) %>% select(country, date, `r.squared`)

colnames(eugaltan)[3] <- "eugaltan"
colnames(eulefrig)[3] <- "eulefrig"
colnames(immigaltan)[3] <- "immigaltan"
colnames(immilefrig)[3] <- "immilefrig"

all_embedding <- full_join(eugaltan, eulefrig, by=c("country", "date"))
all_embedding <- full_join(all_embedding, immigaltan, by=c("country", "date"))
all_embedding <- full_join(all_embedding, immilefrig, by=c("country", "date"))

#formula to check if we get the right values
#summary(lm(position_pos_bas~lefrig2, data = subset(CMP2, country == 31 & date == 196703)))

#change all nans to NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

all_embedding[is.nan(all_embedding)] <- NA



#change date variable to actual date so it is easier and prettier to plot. 
x <- CMP2 %>% select(country, edate, date)
x  <- x %>% unique()

all_embedding <- left_join(all_embedding, x, by=c("country", "date"))

rm(x, eugaltan, eulefrig, immigaltan, immilefrig) 





#making the plot for the EU
embed_eu <- ggplot(data=all_embedding, aes(x=edate, y=eugaltan, colour = "GAL-TAN")) + 
  geom_smooth(method="loess", span = 0.2, show.legend = TRUE, se=FALSE) + coord_cartesian(ylim=c(0, 0.61)) + theme_bw() +
  geom_smooth(method="loess", span = 0.2, data= all_embedding, se=FALSE, aes(x=edate, y=eulefrig, 
                                                                             colour = "Left Right"), show.legend = TRUE) +
  scale_colour_manual(name="", values=c("black", "grey")) + theme(legend.position="bottom") +
  scale_x_date(date_minor_breaks = "1 year", date_labels = "%Y", breaks=as.Date(c("1945-01-01", "1950-01-0", "1955-01-01",
                                                                                  "1960-01-01", "1965-01-01", "1970-01-01",
                                                                                  "1975-01-01", "1980-01-01", "1985-01-01", 
                                                                                  "1990-01-01", "1995-01-01", "2000-01-01",
                                                                                  "2005-01-01", "2010-01-01", "2017-01-01"))) +
  ylab("Embedded") + ggtitle("Embedding of EU") + xlab("Election Date")

embed_eu




#making the plot for immigration
embed_immi <- ggplot(data=all_embedding, aes(x=edate, y=immigaltan, colour = "GAL-TAN")) + 
  geom_smooth(method="loess", span = 0.2, show.legend = TRUE, se=FALSE) + coord_cartesian(ylim=c(0, 0.61)) + theme_bw() +
  geom_smooth(method="loess", span = 0.2, data= all_embedding, se=FALSE, aes(x=edate, y=immilefrig, 
                                                                             colour = "Left Right"), show.legend = TRUE) +
  scale_colour_manual(name="", values=c("black", "grey")) + theme(legend.position="bottom") +
  scale_x_date(date_minor_breaks = "1 year", date_labels = "%Y", breaks=as.Date(c("1945-01-01", "1950-01-0", "1955-01-01",
                                                                                  "1960-01-01", "1965-01-01", "1970-01-01",
                                                                                  "1975-01-01", "1980-01-01", "1985-01-01", 
                                                                                  "1990-01-01", "1995-01-01", "2000-01-01",
                                                                                  "2005-01-01", "2010-01-01", "2017-01-01"))) +
  ylab("Embedded") + ggtitle("Embedding of immigration") + xlab("Election Date")

embed_immi



embedding_temp <- cowplot::plot_grid(embed_eu, embed_immi, ncol = 1, align = 'h')
embedding_temp #saved as 700 X 550





#plotting party names. 
#ggplot(data=subset(CMP, countryname == "Netherlands" & year == 2017), aes(y=position_pos_bas, x=lefrig2))  + geom_text(aes(label=partyabbrev)) + theme_bw()



















######### - analysis cross-sectional###########
###############################3
#######################Cross-sectional contemporary
###############################3

####Using the same CMP data. This is the mean per country for all elections after 2011 till 2017 (thus past 6 years)
setwd()
master_file <- readRDS("master_file_v2.rds") #note i'm taking v2 now
master_file <- subset(master_file, year == 2014)



##########taking means per country
######Salience
##Immigration
mean_sal_country_im <- aggregate(cmp_immi_salience ~ country_name, data = master_file, mean)
mean_sal_country_im$salience_type <- "Immigration"
colnames(mean_sal_country_im)[2] <- "Emphasis"

##EU
mean_sal_country_eu <- aggregate(cmp_eu_salience ~ country_name, data = master_file, mean)
mean_sal_country_eu$salience_type <- "EU"
colnames(mean_sal_country_eu)[2] <- "Emphasis"

mean_sal_eu_imm <- full_join(mean_sal_country_eu, mean_sal_country_im)




#plot it. Saved as #550 X 391
ggplot(data=mean_sal_eu_imm, mapping = aes(y = Emphasis , x = country_name)) +
  geom_col(aes(fill = salience_type), position = "dodge", width=0.8) + 
  theme_bw() + labs(x = "Country", fill = "Dimension") +
  scale_fill_manual(values = c("EU" = "blue", "Immigration" = "black")) +
  theme(axis.text=element_text(size=8, angle = 45, hjust = 1))
  



######Blurring
####using CHES data here.
##Immigration
mean_blur_country_im <- aggregate(immigration_dimension_sd ~ country_name, data = master_file, mean)
mean_blur_country_im$blur_type <- "Immigration"
colnames(mean_blur_country_im)[2] <- "Blurring"


##EU
mean_blur_country_eu <- aggregate(eu_dimension_sd ~ country_name, data = master_file, mean)
mean_blur_country_eu$blur_type <- "EU"
colnames(mean_blur_country_eu)[2] <- "Blurring"


mean_blur_eu_im <- full_join(mean_blur_country_im, mean_blur_country_eu)
rm(mean_blur_country_im, mean_blur_country_eu)


#plot it. Saved as #550 X 391
ggplot(data=mean_blur_eu_im, mapping = aes(y = Blurring , x = country_name)) +
  geom_col(aes(fill = blur_type), position = "dodge", width=0.8) + 
  theme_bw() + labs(x = "Country", fill = "Dimension") +
  scale_fill_manual(values = c("EU" = "blue", "Immigration" = "black")) +
  theme(axis.text=element_text(size=8, angle = 45, hjust = 1)) 









######Embedding. CHES data
#Regress in each country. Make dataframe with those scores per country. For immmi and EU. both LR and GalTan
#lr EU
be_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "Belgium")))$adj.r.squared
dn_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "Denmark")))$adj.r.squared
gr_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "Germany")))$adj.r.squared
sp_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "Spain")))$adj.r.squared
fr_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "France")))$adj.r.squared
ir_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "Ireland")))$adj.r.squared
it_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "Italy")))$adj.r.squared
nl_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "Netherlands")))$adj.r.squared
uk_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "UK")))$adj.r.squared
pr_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "Portugal")))$adj.r.squared
au_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "Austria")))$adj.r.squared
fi_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "Finland")))$adj.r.squared
sw_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "Sweden")))$adj.r.squared
lu_lr_eu <- summary(lm(lrecon~eu_dimension, data = subset(master_file, country_name == "Luxembourg")))$adj.r.squared


#lr immi
be_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "Belgium")))$adj.r.squared
dn_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "Denmark")))$adj.r.squared
gr_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "Germany")))$adj.r.squared
sp_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "Spain")))$adj.r.squared
fr_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "France")))$adj.r.squared
ir_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "Ireland")))$adj.r.squared
it_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "Italy")))$adj.r.squared
nl_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "Netherlands")))$adj.r.squared
uk_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "UK")))$adj.r.squared
pr_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "Portugal")))$adj.r.squared
au_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "Austria")))$adj.r.squared
fi_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "Finland")))$adj.r.squared
sw_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "Sweden")))$adj.r.squared
lu_lr_immi <- summary(lm(lrecon~immigrate_policy, data = subset(master_file, country_name == "Luxembourg")))$adj.r.squared




#galtan immi
be_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "Belgium")))$adj.r.squared
dn_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "Denmark")))$adj.r.squared
gr_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "Germany")))$adj.r.squared
sp_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "Spain")))$adj.r.squared
fr_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "France")))$adj.r.squared
ir_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "Ireland")))$adj.r.squared
it_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "Italy")))$adj.r.squared
nl_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "Netherlands")))$adj.r.squared
uk_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "UK")))$adj.r.squared
pr_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "Portugal")))$adj.r.squared
au_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "Austria")))$adj.r.squared
fi_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "Finland")))$adj.r.squared
sw_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "Sweden")))$adj.r.squared
lu_galtan_immi <- summary(lm(galtan~immigrate_policy, data = subset(master_file, country_name == "Luxembourg")))$adj.r.squared


#galtan eu
be_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "Belgium")))$adj.r.squared
dn_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "Denmark")))$adj.r.squared
gr_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "Germany")))$adj.r.squared
sp_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "Spain")))$adj.r.squared
fr_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "France")))$adj.r.squared
ir_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "Ireland")))$adj.r.squared
it_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "Italy")))$adj.r.squared
nl_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "Netherlands")))$adj.r.squared
uk_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "UK")))$adj.r.squared
pr_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "Portugal")))$adj.r.squared
au_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "Austria")))$adj.r.squared
fi_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "Finland")))$adj.r.squared
sw_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "Sweden")))$adj.r.squared
lu_galtan_eu <- summary(lm(galtan~eu_dimension, data = subset(master_file, country_name == "Luxembourg")))$adj.r.squared



#creating embedding score for each country: alll those scores in a long data frame for easy ggplot plotting
eu <- vector(mode= "character", length = 14)
eu[1:14] <- "EU"
immi <- vector(mode= "character", length = 14)
immi[1:14] <- "Immigration"

embedd_score_lr <- data.frame("country" = c(unique(master_file$country_name), unique(master_file$country_name)),
                           "lr_embed" = c(be_lr_eu, dn_lr_eu, gr_lr_eu, sp_lr_eu, fr_lr_eu, ir_lr_eu,
                                       it_lr_eu, nl_lr_eu, uk_lr_eu, pr_lr_eu, au_lr_eu, fi_lr_eu,
                                       sw_lr_eu, lu_lr_eu, be_lr_immi, dn_lr_immi, gr_lr_immi, sp_lr_immi, fr_lr_immi, ir_lr_immi,
                                       it_lr_immi, nl_lr_immi, uk_lr_immi, pr_lr_immi, au_lr_immi, fi_lr_immi,
                                       sw_lr_immi, lu_lr_immi),
                           "type" = c(eu, immi))

#all R squareds lower than 0 become 0 cause lower is meaningless. 
embedd_score_lr$lr_embed[embedd_score_lr$lr_embed<0] <- 0


embedd_score_galtan <- data.frame("country" = c(unique(master_file$country_name), unique(master_file$country_name)),
                           "galtan_embed" = c(be_galtan_eu, dn_galtan_eu, gr_galtan_eu, sp_galtan_eu, fr_galtan_eu, ir_galtan_eu,
                                             it_galtan_eu, nl_galtan_eu, uk_galtan_eu, pr_galtan_eu, au_galtan_eu, fi_galtan_eu,
                                             sw_galtan_eu, lu_galtan_eu, be_galtan_immi, dn_galtan_immi, gr_galtan_immi, sp_galtan_immi, fr_galtan_immi, ir_galtan_immi,
                                             it_galtan_immi, nl_galtan_immi, uk_galtan_immi, pr_galtan_immi, au_galtan_immi, fi_galtan_immi,
                                             sw_galtan_immi, lu_galtan_immi),
                           "type" = c(eu, immi))

embedd_score_galtan$galtan_embed[embedd_score_galtan$galtan_embed<0] <- 0



###plots
#embedding in left right
emd_lr <- ggplot(data=embedd_score_lr, mapping = aes(y = lr_embed , x = country)) +
  geom_col(aes(fill = type), position = "dodge", width=0.8) + 
  theme_bw() + labs(x = "Country", y= "Embedded", fill = "Dimension") +
  scale_fill_manual(values = c("EU" = "blue", "Immigration" = "black")) +
  theme(axis.text=element_text(size=8, angle = 45, hjust = 1)) + ylim(0, 1) +
  ggtitle("Embedded in Left Right")

#embedding in galtan
emd_galtan <- ggplot(data=embedd_score_galtan, mapping = aes(y = galtan_embed , x = country)) +
  geom_col(aes(fill = type), position = "dodge", width=0.8) + 
  theme_bw() + labs(x = "Country", y= "Embedded", fill = "Dimension") +
  scale_fill_manual(values = c("EU" = "blue", "Immigration" = "black")) +
  theme(axis.text=element_text(size=8, angle = 45, hjust = 1)) + ylim(0, 1) +
  ggtitle("Embedded in GAL-TAN")


library(cowplot)
embedding_plts <- cowplot::plot_grid(emd_galtan, emd_lr, ncol = 1, align = 'h')
embedding_plts #saved as 550 X 550





######positions of parties 
mean_pos_im <- aggregate(immigrate_policy ~ country_name, data = master_file, mean)


ggplot(data=mean_pos_im, mapping = aes(y = reorder(country_name, immigrate_policy), x = immigrate_policy)) +
  geom_point(size=2) + geom_vline (xintercept = 5, color = "gray30" ) + 
  scale_x_continuous(breaks = c(2, 2.5, 3, 3.5 , 4, 4.5, 5, 5.5, 6, 6.5, 7), 
                     labels = c("2", "2.5", "3\n (Open)", "3.5" , "4", "4.5", "5", "5.5", "6\n (Strict)", "6.5", "7")) +
  theme(axis.text=element_text(size=9)) + labs(y="", x="Migration policy stance") + theme_bw()



mean_pos_eu <- aggregate(position ~ country_name, data = master_file, mean)

#positions on a normal plot
ggplot(data=mean_pos_eu, mapping = aes(y = reorder(country_name, position), x = position)) +
  geom_point(size=2) + geom_vline (xintercept = 5, color = "gray30" )  +
  scale_x_continuous( breaks = c(1, 1.5, 2, 2.5, 3, 3.5 , 4, 4.5, 5, 5.5, 6, 6.5, 7), 
                     labels = c("1", "1.5", "2", "2.5", "3", "3.5" , "4\n (Opposed)", "4.5", "5", "5.5\n (Favour)", "6", "6.5", "7")) +
  theme(axis.text=element_text(size=9)) + labs(y="", x="EU Integration stance") + theme_bw()




######Division of parties
sd_pos_eu <- aggregate(position ~ country_name, data = master_file, sd)

#positions on a normal plot
ggplot(data=sd_pos_eu, mapping = aes(y = reorder(country_name, position), x = position)) +
  geom_point(size=2) +
  scale_x_continuous( breaks = c(1, 1.5, 2, 2.5, 3, 3.5 , 4, 4.5, 5, 5.5, 6, 6.5, 7), 
                      labels = c("1", "1.5", "2", "2.5", "3", "3.5" , "4\n (Opposed)", "4.5", "5", "5.5\n (Favour)", "6", "6.5", "7")) +
  theme(axis.text=element_text(size=9)) + labs(y="", x="SD of party positions on EU Integration") + theme_bw()













#### - Maps for on the cover ####
library(maptools)
library(ggplot2)
library(ggmap)
library(maps)
library(dplyr)






## explanation of how the map works: https://egallic.fr/en/european-map-using-r/
library(ggplot2)
library(grid)
library(rworldmap)


worldMap <- getMap()

# Member States of the European Union in my analysis
europeanUnion <- c("Austria","Belgium", "Denmark","Finland","France",
                   "Germany","Greece","Ireland","Italy", "Luxembourg","Netherlands",
                   "Portugal","Spain", "Sweden", "United Kingdom")


# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)

# Extract longitude and latitude border's coordinates of members states of E.U. 
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)


##prepare the datafiles (need a columns with country name and a columns with the values).
#total salience 
eu <- mean_sal_eu_imm[1:11,] #very brute way to get the dataframes back in one piece. 
immi <- mean_sal_eu_imm[12:22,]
glob_salience_country <- full_join(eu, immi, by="country_name")
glob_salience_country$salience <- as.numeric(glob_salience_country$Emphasis.x) + 
  as.numeric(glob_salience_country$Emphasis.y) #sum up for total salience
colnames(glob_salience_country)[1] <- "region" #change name to region which fits the map command later down
glob_salience_country <- subset(glob_salience_country, select = c(region, salience)) #drop useless columns. 
master_file2010 <- readRDS("master_file_v2.rds") #also load 2010 masterfile for the 3 countries that have missing data
glob_sal <- aggregate(cmp_glob_salience~country_name, data = master_file2010, mean)
glob_sal <- glob_sal[2:4,]
colnames(glob_sal)[1] <- "region"
colnames(glob_sal)[2] <- "salience"
glob_salience_country <- bind_rows(glob_salience_country, glob_sal)
glob_salience_country[11,1] <- "United Kingdom"
europeCoords_im <- inner_join(europeCoords, glob_salience_country) #join back into polygon dataframe
rm(eu, immi, glob_salience_country, master_file2010, glob_sal) #clean up. 

#total blurring
eu <- mean_blur_eu_im[1:14,] #very brute way to get the dataframes back in one piece. 
immi <- mean_blur_eu_im[15:28,]
glob_blur_country <- full_join(eu, immi, by="country_name")
glob_blur_country$blurring <- (glob_blur_country$Blurring.x + 
  glob_blur_country$Blurring.y) / 2 #sum up for total salience. Divide because it is average blurring
colnames(glob_blur_country)[1] <- "region" #change name to region which fits the map command later down
glob_blur_country <- subset(glob_blur_country, select = c(region, blurring)) #drop useless columns. 
glob_blur_country[14,1] <- "United Kingdom"
europeCoords_blur <- inner_join(europeCoords, glob_blur_country) #join back into polygon dataframe
rm(eu, immi, glob_blur_country) #clean up. 

#total for embedding
glob_embed <- cbind(embedd_score_lr, embedd_score_galtan)
eu <- glob_embed[1:14,]
immi <- glob_embed[15:28,]
glob_embed <- cbind(eu, immi)
glob_embed$embedding <- (glob_embed[,2] + glob_embed[,5] + glob_embed[,8] + glob_embed[,11]) /4
colnames(glob_embed)[1] <- "region" #change name to region which fits the map command later down
glob_embed <- subset(glob_embed, select = c(region, embedding)) #drop useless columns. 
glob_embed$region <- as.character(glob_embed$region)
glob_embed[9,1] <- "United Kingdom"
europeCoords_emb <- inner_join(europeCoords, glob_embed) #join back into polygon dataframe
rm(eu, immi, glob_embed)

#total scores for entrepreneurship
master_file2010 <- readRDS("master_file_v2.rds") #also load 2010 masterfile for the 3 countries that have missing data
glob_entr <- aggregate(issue_entrep_cmpsal_glob ~country_name, data = master_file, mean)
glob_entr2010 <- aggregate(issue_entrep_cmpsal_glob ~country_name, data = master_file2010, mean)
anti <- glob_entr2010[2:4,]
glob_entr <- bind_rows(glob_entr, anti)
colnames(glob_entr)[1] <- "region"
glob_entr$logent <- log(glob_entr$issue_entrep_cmpsal_glob)
glob_entr[11,1] <- "United Kingdom"
europeCoords_ent <- inner_join(europeCoords, glob_entr) #join back into polygon dataframe
rm(anti, master_file2010, glob_entr2010, glob_entr)

#map for salience
salience_glob <- ggplot() + geom_polygon(data = europeCoords_im, aes(x = long, y = lat, group = region, fill = salience),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

salience_glob <- salience_glob + scale_fill_gradient(name = "", low = "grey91", high = "grey15", na.value = "white")

salience_glob <- salience_glob + theme( axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  axis.line=element_blank(),
  panel.background = element_blank(), legend.position = "right",
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"), 
  text=element_text(size=10,  family="serif"),
  plot.title = element_text(hjust = 0.5, size=12)) +
  ggtitle("Salience of globalisation")


ggsave("salience_glob.png", plot= salience_glob, dpi=600) #this saves in much higher resolution. 



#map for blurring
blur_glob <- ggplot() + geom_polygon(data = europeCoords_blur, aes(x = long, y = lat, group = region, fill = blurring),
                                         colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

blur_glob <- blur_glob + scale_fill_gradient(name = "", low = "grey91", high = "grey15", na.value = "white")

blur_glob <- blur_glob + theme( axis.text.x = element_blank(),
                                        axis.text.y = element_blank(), axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank(), axis.title = element_blank(),
                                        axis.line=element_blank(),
                                        panel.background = element_blank(), legend.position = "right",
                                        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"), 
                                        text=element_text(family="serif", size=10),
                                plot.title = element_text(hjust = 0.5, size=12)) +
  ggtitle("Blurring on globalisation")

blur_glob 
ggsave("blur_glob.png", plot= blur_glob, dpi=600)


#map for embedding
emb_glob <- ggplot() + geom_polygon(data = europeCoords_emb, aes(x = long, y = lat, group = region, fill = embedding),
                                     colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

emb_glob <- emb_glob + scale_fill_gradient(name = "", low = "grey91", high = "grey15", na.value = "white")

emb_glob <- emb_glob + theme( axis.text.x = element_blank(),
                                axis.text.y = element_blank(), axis.ticks.x = element_blank(),
                                axis.ticks.y = element_blank(), axis.title = element_blank(),
                                axis.line=element_blank(),
                                panel.background = element_blank(), legend.position = "right",
                                plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"), 
                                text=element_text(size=10,  family="serif"),
                              plot.title = element_text(hjust = 0.5, size=12)) + 
  ggtitle("Embedding of globalisation")

emb_glob 
ggsave("emb_glob.png", plot= emb_glob, dpi=600)

#map for entrepreneurship
ent_glob <- ggplot() + geom_polygon(data = europeCoords_ent, aes(x = long, y = lat, group = region, fill = issue_entrep_cmpsal_glob),
                                    colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

ent_glob <- ent_glob + scale_fill_gradient(name = "", low = "grey91", high = "grey15", na.value = "white")

ent_glob <- ent_glob + theme( axis.text.x = element_blank(),
                              axis.text.y = element_blank(), 
                              axis.ticks.x = element_blank(),
                              axis.ticks.y = element_blank(), 
                              axis.title = element_blank(),
                              axis.line=element_blank(),
                              panel.background = element_blank(),
                              legend.position = "right",
                              plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"),
                              text=element_text(size=10,  family="serif"),
                              plot.title = element_text(hjust = 0.5, size=12)) + 
  ggtitle("Entrepreneurship on globalisation") 

ent_glob 
ggsave("ent_glob.png", plot= ent_glob, dpi=600)

#plotting all maps together in one big plot. 
cowplot::plot_grid(salience_glob, blur_glob, emb_glob, ent_glob, ncol = 2, align = 'h')















