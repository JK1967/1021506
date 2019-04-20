# Project: MPhil dissertation in Politcs -- Oxford
# Date: April 2019
# Author: ANONYMOUS
# Based on: Abou-Chadi & Wagner 2018. Substantial parts of their code have been used
# Data: CMP Project and Abou-Chadi & Wagner (2019).
# Steps: (i) Creating the required variables using CMP and merge with Wagner. 
#   (ii) Do the analyis
#   (iii) Run robustness checks


####- packages
library(dplyr)
library(Hmisc)
library(rdrobust)
library(rdd)



#### - Creating database####


#loading all data
setwd("")
library(manifestoR)
mp_setapikey("manifesto_apikey.txt")
CMP <- mp_maindataset()
#saveRDS(CMP, file="CMPdata")
rrp_rdd <- read.delim("rrp_rdd.tab")

#add east west to CMP data
CMP$eastwest <- ifelse(CMP$country == 42 | CMP$country == 13 | CMP$country == 14 |
                         CMP$country == 41 | CMP$country == 34 | CMP$country == 53 |
                         CMP$country == 32 | CMP$country == 23 | CMP$country == 22 |
                         CMP$country ==  12| CMP$country == 35 | CMP$country == 11 |
                         CMP$country == 33 | CMP$country == 43, "west", "eastorelse")



###creating variables in CMP: salience
#saliences for issues
CMP$eu_sal <- CMP$per108+CMP$per110 #variable is called position because that is the name in CHES. 
CMP$immi_sal <- CMP$per601 + CMP$per602 + CMP$per607 + CMP$per608 
#taking fd
CMP <- CMP %>% group_by(party) %>% mutate(eu_sal_fd = eu_sal-lag(eu_sal, order_by=date)) %>% ungroup
CMP <- CMP %>% group_by(party) %>% mutate(immi_sal_fd = immi_sal-lag(immi_sal, order_by=date)) %>% ungroup



###creating variables in CMP: Entrepeneurship
#Create position variables for party positions by subtracting negative from positive (i.e 608 is negative)
CMP$eu_pos <- NA
CMP$immi_pos <- NA
CMP$eu_pos <- (CMP$per108 - CMP$per110) / (CMP$per108 + CMP$per110)
CMP$immi_pos <- ((CMP$per607+CMP$per601) - (CMP$per608+CMP$per602)) / (CMP$per607+CMP$per601+CMP$per608+CMP$per602)

#creating variable for mean EU position in a country
x <- aggregate(eu_pos ~ country + edate, data = CMP, mean, na.rm=TRUE, na.action="na.omit") #aggregate because it groups together nicely
colnames(x)[which(names(x) == "eu_pos")] <- "mean_eu_country" #change column name
CMP <- left_join(CMP, x, by= c("country","edate")) #add back to dataframe

#creating variable for mean immigration position in a country
x <- aggregate(immi_pos ~ country + edate, data = CMP, mean, na.rm=TRUE, na.action="na.omit") #aggregate because it groups together nicely
colnames(x)[which(names(x) == "immi_pos")] <- "mean_immi_country" #change column name
CMP <- left_join(CMP, x, by= c("country","edate")) #add back to dataframe
rm(x)

#take distance from country mean for a party on EU
CMP$eu_dimension_distance_mean <- NA
for (i in CMP$party){
  CMP$eu_dimension_distance_mean[CMP$party == i] <- (CMP$eu_pos[CMP$party == i] - 
                                                       CMP$mean_eu_country[CMP$party == i])
}

#remove negative values
CMP$eu_dimension_distance_mean <- abs(CMP$eu_dimension_distance_mean)

#take distance from country mean for a party on immigration
CMP$immi_dimension_distance_mean <- NA
for (i in CMP$party){
  CMP$immi_dimension_distance_mean[CMP$party == i] <- (CMP$immi_pos[CMP$party == i] - 
                                                       CMP$mean_immi_country[CMP$party == i])
}
#remove negative values
CMP$immi_dimension_distance_mean <- abs(CMP$immi_dimension_distance_mean)

#multiply distance from country with salience to get entrepreneurship score for EU and immi
#check whether all variables look OK
CMP$entr_immi <- NA
CMP$entr_eu <- NA
CMP$entr_immi <- CMP$immi_dimension_distance_mean * CMP$immi_sal
CMP$entr_eu <- CMP$eu_dimension_distance_mean * CMP$eu_sal
CMP$entr_immi<- ifelse(is.nan(CMP$entr_immi) == TRUE, 0, CMP$entr_immi) #nan means no entrepreneurship
CMP$entr_eu<- ifelse(is.nan(CMP$entr_eu) == TRUE, 0, CMP$entr_eu)

#taking fd
CMP <- CMP %>% group_by(party) %>% mutate(eu_ent_fd = entr_eu - lag(entr_eu, order_by = date)) %>% ungroup
CMP <- CMP %>% group_by(party) %>% mutate(immi_ent_fd = entr_immi - lag(entr_immi, order_by = date)) %>% ungroup



###creating variables in CMP: Blurring
CMP$eu_blur <- NA
CMP$immi_blur <- NA
CMP$eu_blur <- (abs((CMP$per108 - CMP$per110))) / (CMP$per108 + CMP$per110)
CMP$immi_blur <- (abs(((CMP$per607 + CMP$per601) - (CMP$per608 + CMP$per602)))) /
  (CMP$per607 + CMP$per601 + CMP$per608 + CMP$per602)

#make all nan into 1 because nans are caused by a math error when dividing by 0.
#this means that all nan's have no blurring cause no position. Thus they should be set to 1
#because that indicates no blurring (i.e. 5-5=0 -> 0*10=0. Thus completly blurred is 0)
CMP$eu_blur<- ifelse(is.nan(CMP$eu_blur) == TRUE, 1, CMP$eu_blur)
CMP$immi_blur<- ifelse(is.nan(CMP$immi_blur) == TRUE, 1, CMP$immi_blur)
CMP$eu_blur <- 1 - CMP$eu_blur #reverse variables: (min+max) - value. This makes interpretation more sensible. Higher == more blur
CMP$immi_blur <- 1 - CMP$immi_blur 

CMP <- CMP %>% group_by(party) %>% mutate(eu_blur_fd = eu_blur - lag(eu_blur, order_by = date)) %>% ungroup
CMP <- CMP %>% group_by(party) %>% mutate(immi_blur_fd = immi_blur - lag(immi_blur, order_by = date)) %>% ungroup



###Add CMP alternative variables that have nothing to do with RR for robustness check
CMP$environ_sal <- CMP$per501
CMP <- CMP %>% group_by(party) %>% mutate(environ_sal_fd = environ_sal - lag(environ_sal, order_by = date)) %>% ungroup



###add variables from Wagner
chadi_usefull <- select(rrp_rdd, one_of(c("er.v.c", "er.v.c_l", "er.in", "er.in_l", "thrs", "thrs_l", "edate", "iso2c"))) #only keep columns I want plus columns to merge with
chadi_usefull$country[chadi_usefull$iso2c == "AT"] <- 42 #add CMP country codes to Wagner data for merge
chadi_usefull$country[chadi_usefull$iso2c == "BG"] <- 80
chadi_usefull$country[chadi_usefull$iso2c == "CH"] <- 43
chadi_usefull$country[chadi_usefull$iso2c == "CZ"] <- 82
chadi_usefull$country[chadi_usefull$iso2c == "DE"] <- 41
chadi_usefull$country[chadi_usefull$iso2c == "DK"] <- 13
chadi_usefull$country[chadi_usefull$iso2c == "EE"] <- 83
chadi_usefull$country[chadi_usefull$iso2c == "ES"] <- 33
chadi_usefull$country[chadi_usefull$iso2c == "FI"] <- 14
chadi_usefull$country[chadi_usefull$iso2c == "GR"] <- 34
chadi_usefull$country[chadi_usefull$iso2c == "HR"] <- 81
chadi_usefull$country[chadi_usefull$iso2c == "IE"] <- 53
chadi_usefull$country[chadi_usefull$iso2c == "IT"] <- 32
chadi_usefull$country[chadi_usefull$iso2c == "LU"] <- 23
chadi_usefull$country[chadi_usefull$iso2c == "LV"] <- 87
chadi_usefull$country[chadi_usefull$iso2c == "NL"] <- 22
chadi_usefull$country[chadi_usefull$iso2c == "NO"] <- 12
chadi_usefull$country[chadi_usefull$iso2c == "PL"] <- 92
chadi_usefull$country[chadi_usefull$iso2c == "PT"] <- 35
chadi_usefull$country[chadi_usefull$iso2c == "RO"] <- 93
chadi_usefull$country[chadi_usefull$iso2c == "SE"] <- 11
chadi_usefull$country[chadi_usefull$iso2c == "SI"] <- 97
chadi_usefull$country[chadi_usefull$iso2c == "SK"] <- 96

CMP$edate <- as.factor(CMP$edate) #change so I can merge

chadi_usefull <- chadi_usefull %>% distinct(edate, country, .keep_all = TRUE) #only keep unique country edate combinations. 
CMP <- inner_join(CMP, chadi_usefull, by=c("edate", "country" ) ) 
rm(chadi_usefull, rrp_rdd)

#exclude all parties that are RR themselves (use same coding as Chadi, which is same as Mudde. Note that not all parties are here because not all are in CMP!)
CMP <- CMP[!(CMP$party==42420 | CMP$party==43810 | CMP$party== 82710 | 
                CMP$party== 41953 | CMP$party== 13720 | CMP$party== 83611 | 
                CMP$party== 34710 | CMP$party== 81713 | CMP$party==32720 | 
                CMP$party== 32710 | CMP$party== 87723 | CMP$party== 87710 |
                CMP$party== 87721 | CMP$party== 22711 | CMP$party== 22720 | 
                CMP$party== 22722 | CMP$party== 92621 | CMP$party== 92713 | 
                CMP$party== 93712 | CMP$party== 11710 | 
                CMP$party== 97710 | CMP$party== 96710), ]

#change label so I don't have to do that in ggplot. 
CMP$er.in_l <- ifelse(CMP$er.in_l == 1, "RRP w seat(s)", "RRP w/o seat(s)")

#variable for percentage of seat share in every election.
CMP$absseat.per <- (100 * CMP$absseat / CMP$totseats)

#get mean seat share value across all years for each party. This thus only goes well if you already have the right number of years.
CMP$absseat.per.mean <- NA
for (i in CMP$party){
  CMP$absseat.per.mean[CMP$party == i] <- mean(CMP$absseat.per[CMP$party == i]) }
rm(i)

#change all nan in the whole dataset to NA. This is less error prone in packages
CMP[ is.na(CMP) ] <- NA

rm(i)




#### - Analysis ####

library(rdrobust)
library(rdd)
source( 'rrp_rdd_functions_vme.R' )


#subsetting data to only have Western-European countries and no too small parties. 
ds <- CMP #load for all parties
ds <- subset(CMP, absseat.per.mean > 7.5 & eastwest == "west" ) #remove parties with less than 7.5 seats on average and not in western EU. 




#plots blurring
rdd_bl_immi <- jump.plot.blur( data = subset( ds , er.v.c_l <= 10 ) 
                               , force.var = 'er.v.c_l' 
                               , yvar = 'immi_blur_fd' 
                               , seat.identifier = 'er.in_l' 
                               , polynomial = 3 
                               , ylabel = expression(Delta* "Immigration Blurring")
) 

rdd_bl_eu <- jump.plot.blur( data = subset( ds , er.v.c_l <= 10 ) 
                             , force.var = 'er.v.c_l' 
                             , yvar = 'eu_blur_fd' 
                             , seat.identifier = 'er.in_l' 
                             , polynomial = 3 
                             , ylabel = expression(Delta*" EU Blurring")
) 

rdd_bl <- cowplot::plot_grid(rdd_bl_immi, rdd_bl_eu, ncol = 2, align = 'l')
rdd_bl #saved as 700 X 320
rm(rdd_bl_immi, rdd_bl_eu)



#plots emphasizing
rdd_emp_immi <- jump.plot.emp( data = subset( ds , er.v.c_l <= 10 ) 
                               , force.var = 'er.v.c_l' 
                               , yvar = 'immi_sal_fd' 
                               , seat.identifier = 'er.in_l' 
                               , ylabel = expression(Delta*" Immigration Emphasising")
) 

rdd_emp_eu <- jump.plot.emp( data = subset( ds , er.v.c_l <= 10 ) 
                             , force.var = 'er.v.c_l' 
                             , yvar = 'eu_sal_fd' 
                             , seat.identifier = 'er.in_l' 
                             , polynomial = 4 
                             , ylabel = expression(Delta*" EU Emphasising")
) 

rdd_emp <- cowplot::plot_grid(rdd_emp_immi, rdd_emp_eu, ncol = 2, align = 'l')
rdd_emp
rm(rdd_emp_eu, rdd_emp_immi)



#plots entrepeneurship
rdd_ent_immi <- jump.plot.ent( data = subset( ds , er.v.c_l <= 10 ) 
                               , force.var = 'er.v.c_l' 
                               , yvar = 'immi_ent_fd' 
                               , seat.identifier = 'er.in_l' 
                               , polynomial = 3 
                               , ylabel = expression(Delta*" Immigration Entrepreneurship")
) 

rdd_ent_eu <- jump.plot.ent( data = subset( ds , er.v.c_l <= 10 ) 
                             , force.var = 'er.v.c_l' 
                             , yvar = 'eu_ent_fd' 
                             , seat.identifier = 'er.in_l' 
                             , polynomial = 3 
                             , ylabel = expression(Delta*" EU Entrepreneurship")
) 

rdd_ent <- cowplot::plot_grid(rdd_ent_immi, rdd_ent_eu, ncol = 2, align = 'l')
rdd_ent
rm(rdd_ent_immi, rdd_ent_eu)




#emphasising results
x <- IKbandwidth(ds$er.v.c_l, ds$immi_sal_fd) 
summary(rdrobust(y = ds$immi_sal_fd, x = ds$er.v.c_l, cluster=ds$iso2c, all = TRUE, h = c(x)))

x <- IKbandwidth(ds$er.v.c_l, ds$eu_sal_fd) 
summary(rdrobust(ds$eu_sal_fd, ds$er.v.c_l, cluster = ds$iso2c, all=TRUE, h=c(x)))



#Blurring results
x <- IKbandwidth(ds$er.v.c_l, ds$immi_blur_fd) 
summary(rdrobust(ds$immi_blur_fd, ds$er.v.c_l, cluster=ds$iso2c, all = TRUE, h = c(x))) #

x <- IKbandwidth(ds$er.v.c_l, ds$eu_blur_fd) 
summary(rdrobust(ds$eu_blur_fd, ds$er.v.c_l, cluster=ds$iso2c, all = TRUE, h = c(1.9)))



#entrepreneurship results
x <- IKbandwidth(ds$er.v.c_l, ds$immi_ent_fd) 
summary(rdrobust(ds$immi_ent_fd, ds$er.v.c_l, cluster=ds$iso2c, all = TRUE, h = c(x)))

x <- IKbandwidth(ds$er.v.c_l, ds$eu_ent_fd) 
summary(rdrobust(ds$eu_ent_fd, ds$er.v.c_l, cluster=ds$iso2c, all = TRUE, h = c(x)))
rm(x, i)





##Results for left and right parties independently. Not in thesis. 
ds %<>%
  group_by( iso2c ) %>%
  mutate( country.mean.rile = mean( rile , na.rm = TRUE )) %>%
  ungroup( ) %>%
  group_by( party ) %>%
  mutate( mean.rile = mean ( rile , na.rm = TRUE )) %>%
  ungroup( )

ds_right <- subset(ds, mean.rile > country.mean.rile)
ds_left <- subset(ds, mean.rile < country.mean.rile)

#emphasising results right
x <- IKbandwidth(ds_right$er.v.c_l, ds_right$immi_sal_fd) #doing this separate to avoid error. 
summary(rdrobust(y=ds_right$immi_sal_fd, x=ds_right$er.v.c_l, cluster=ds_right$iso2c, all=TRUE, h=c(x)))
x <- IKbandwidth(ds_right$er.v.c_l, ds_right$eu_sal_fd) 
summary(rdrobust(ds_right$eu_sal_fd, ds_right$er.v.c_l, cluster=ds_right$iso2c, all=TRUE, h=c(x)))

#emphasising results left
x <- IKbandwidth(ds_left$er.v.c_l, ds_left$immi_sal_fd) #doing this separate to avoid error. 
summary(rdrobust(y=ds_left$immi_sal_fd, x=ds_left$er.v.c_l, cluster=ds_left$iso2c, all=TRUE, h=c(x)))
x <- IKbandwidth(ds_left$er.v.c_l, ds_left$eu_sal_fd) 
summary(rdrobust(ds_left$eu_sal_fd, ds_left$er.v.c_l, cluster=ds_left$iso2c, all=TRUE, h=c(x)))


#Blurring results right
x <- IKbandwidth(ds_right$er.v.c_l, ds_right$immi_blur_fd) 
summary(rdrobust(ds_right$immi_blur_fd, ds_right$er.v.c_l, cluster=ds_right$iso2c, all=TRUE, h=c(x)))
x <- IKbandwidth(ds_right$er.v.c_l, ds_right$eu_blur_fd) 
summary(rdrobust(ds_right$eu_blur_fd, ds_right$er.v.c_l, cluster=ds_right$iso2c, all=TRUE, h=c(1.6)))

#lurring results left
x <- IKbandwidth(ds_left$er.v.c_l, ds_left$immi_blur_fd) 
summary(rdrobust(ds_left$immi_blur_fd, ds_left$er.v.c_l, cluster=ds_left$iso2c, all=TRUE, h=c(x)))
x <- IKbandwidth(ds_left$er.v.c_l, ds_left$eu_blur_fd) 
summary(rdrobust(ds_left$eu_blur_fd, ds_left$er.v.c_l, cluster=ds_left$iso2c, all=TRUE, h=c(1.14)))


#entrepreneurship results right
x <- IKbandwidth(ds_right$er.v.c_l, ds_right$immi_ent_fd) 
summary(rdrobust(ds_right$immi_ent_fd, ds_right$er.v.c_l, cluster=ds_right$iso2c, all=TRUE, h=c(x)))

x <- IKbandwidth(ds_right$er.v.c_l, ds_right$eu_ent_fd) 
summary(rdrobust(ds_right$eu_ent_fd, ds_right$er.v.c_l, cluster=ds_right$iso2c, all=TRUE, h=c(x)))


#entrepreneurship results left
x <- IKbandwidth(ds_left$er.v.c_l, ds_left$immi_ent_fd) 
summary(rdrobust(ds_left$immi_ent_fd, ds_left$er.v.c_l, cluster=ds_left$iso2c, all=TRUE, h=c(1.8)))

x <- IKbandwidth(ds_left$er.v.c_l, ds_left$eu_ent_fd) 
summary(rdrobust(ds_left$eu_ent_fd, ds_left$er.v.c_l, cluster=ds_left$iso2c, all=TRUE, h=c(x)))
rm(x, i)











#### - Tests####
##with meaningless issue. 
x <- IKbandwidth(ds$er.v.c_l, ds$environ_sal_fd) #doing this separate to avoid weird error. 
summary(rdrobust(y=ds$environ_sal_fd, x=ds$er.v.c_l, cluster=ds$iso2c, all=TRUE, h=c(x)))








### - jacknife analysis 
c.list <- unique(ds$iso2c)

#adding an error catch term in the function because the package otherwise gives the same strange error as above.
  #unfortunately I haven't been able to get those errors out. Still doesn't work properly, so don't add. 

#for use in paper because prints LaTeX tables
for (i in c.list){
  tryCatch({print(i)
    ds_i <- subset(ds, ds$iso2c != i)
    x <- IKbandwidth(ds_i$er.v.c_l, ds_i$immi_blur_fd)
    y <- rdrobust(ds_i$immi_blur_fd, ds_i$er.v.c_l, cluster=ds_i$iso2c, all=TRUE, h=c(x))
    z <- as.data.frame(y$coef)
    w <- as.data.frame(y$pv)
    q <- cbind(z, w)
    q$bw <- x
    print( xtable::xtable( q ) , comment = FALSE , include.rownames = TRUE , size = 'footnotesize')
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#use to create .txt files by copying output. 
for (i in c.list){
  tryCatch({print(i)
    ds_i <- subset(ds, ds$iso2c != i)
    x <- IKbandwidth(ds_i$er.v.c_l, ds_i$immi_blur_fd)
    y <- rdrobust(ds_i$immi_blur_fd, ds_i$er.v.c_l, cluster=ds_i$iso2c, all=TRUE, h=c(x))
    z <- as.data.frame(y$coef)
    w <- as.data.frame(y$pv)
    q <- cbind(z, w)
    q$bw <- x
    print(q)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}



##different cutoffs
#subsetting data to only have Western-European countries and no too small parties. 
ds <- CMP #load for all parties
ds.10 <- subset(CMP, absseat.per.mean > 10 & eastwest == "west" ) #remove parties with less than 7.5 seats on average and not in western EU. 

#emphasising results
x <- IKbandwidth(ds.10$er.v.c_l, ds.10$immi_sal_fd) 
summary(rdrobust(y=ds.10$immi_sal_fd, x=ds.10$er.v.c_l, cluster=ds.10$iso2c, all=TRUE, h=c(x)))

x <- IKbandwidth(ds.10$er.v.c_l, ds.10$eu_sal_fd) 
summary(rdrobust(ds.10$eu_sal_fd, ds.10$er.v.c_l, cluster=ds.10$iso2c, all=TRUE, h=c(x)))

#Blurring results
x <- IKbandwidth(ds.10$er.v.c_l, ds.10$immi_blur_fd) 
summary(rdrobust(ds.10$immi_blur_fd, ds.10$er.v.c_l, cluster=ds.10$iso2c, all=TRUE, h=c(1.9))) 

x <- IKbandwidth(ds.10$er.v.c_l, ds.10$eu_blur_fd) 
summary(rdrobust(ds.10$eu_blur_fd, ds.10$er.v.c_l, cluster=ds.10$iso2c, all=TRUE, h=c(1.7)))

#entrepreneurship results
x <- IKbandwidth(ds.10$er.v.c_l, ds.10$immi_ent_fd) 
summary(rdrobust(ds.10$immi_ent_fd, ds.10$er.v.c_l, cluster=ds.10$iso2c, all=TRUE, h=c(x)))

x <- IKbandwidth(ds.10$er.v.c_l, ds.10$eu_ent_fd) 
summary(rdrobust(ds.10$eu_ent_fd, ds.10$er.v.c_l, cluster=ds.10$iso2c, all=TRUE, h=c(x)))
rm(x, i)


ds <- CMP #load for all parties
ds.5 <- subset(CMP, absseat.per.mean > 5 & eastwest == "west" ) #remove parties with less than 7.5 seats on average and not in western EU. 

#emphasising results
x <- IKbandwidth(ds.5$er.v.c_l, ds.5$immi_sal_fd) 
summary(rdrobust(y=ds.5$immi_sal_fd, x=ds.5$er.v.c_l, cluster=ds.5$iso2c, all=TRUE, h=c(x)))

x <- IKbandwidth(ds.5$er.v.c_l, ds.5$eu_sal_fd) 
summary(rdrobust(ds.5$eu_sal_fd, ds.5$er.v.c_l, cluster=ds.5$iso2c, all=TRUE, h=c(x)))

#Blurring results
x <- IKbandwidth(ds.5$er.v.c_l, ds.5$immi_blur_fd) 
summary(rdrobust(ds.5$immi_blur_fd, ds.5$er.v.c_l, cluster=ds.5$iso2c, all=TRUE, h=c(1.9))) 

x <- IKbandwidth(ds.5$er.v.c_l, ds.5$eu_blur_fd) 
summary(rdrobust(ds.5$eu_blur_fd, ds.5$er.v.c_l, cluster=ds.5$iso2c, all=TRUE, h=c(1.2)))

#entrepreneurship results
x <- IKbandwidth(ds.5$er.v.c_l, ds.5$immi_ent_fd) 
summary(rdrobust(ds.5$immi_ent_fd, ds.5$er.v.c_l, cluster=ds.5$iso2c, all=TRUE, h=c(x)))

x <- IKbandwidth(ds.5$er.v.c_l, ds.5$eu_ent_fd) 
summary(rdrobust(ds.5$eu_ent_fd, ds.5$er.v.c_l, cluster=ds.5$iso2c, all=TRUE, h=c(x)))
rm(x, i)










#only fixed threshold countries. Go back and add Eastern European countries too. 
ds <- CMP #load for all parties
ds <- subset(CMP, absseat.per.mean > 7.5) #remove parties with less than 7.5 seats. Add Eastern Europe back in so there
  #are enough countries with a fixed threshold. 


c.list <- c( 'AT' , 'BG' , 'CZ' , 'DE' , 'EE' , 'GR' , 'LV' , 'NL' , 'PL' , 'RO' , 'SE' , 'SI')

ds %>% 
  filter( iso2c %in% c.list ) %>%  
  filter( !( iso2c == 'AT' & edate < '1994-01-01' )) %>%
  filter( !( iso2c == 'GR' & edate < '1993-01-01' )) %>%
  filter( !( iso2c == 'SI' & edate < '2000-01-01' )) -> ds.fixed


#emphasising
x <- IKbandwidth(ds.fixed$er.v.c_l, ds.fixed$immi_sal_fd) #doing this separate to avoid error. 
summary(rdrobust(y=ds.fixed$immi_sal_fd, x=ds.fixed$er.v.c_l, cluster=ds.fixed$iso2c, all=TRUE, h=c(x)))

x <- IKbandwidth(ds.fixed$er.v.c_l, ds.fixed$eu_sal_fd) 
summary(rdrobust(ds.fixed$eu_sal_fd, ds.fixed$er.v.c_l, cluster=ds.fixed$iso2c, all=TRUE, h=c(x)))

#entrepreneurship
x <- IKbandwidth(ds.fixed$er.v.c_l, ds.fixed$immi_ent_fd) 
summary(rdrobust(ds.fixed$immi_ent_fd, ds.fixed$er.v.c_l, cluster=ds.fixed$iso2c, all=TRUE, h=c(x)))

x <- IKbandwidth(ds.fixed$er.v.c_l, ds.fixed$eu_ent_fd) 
summary(rdrobust(ds.fixed$eu_ent_fd, ds.fixed$er.v.c_l, cluster=ds.fixed$iso2c, all=TRUE, h=c(x)))

#Blurring
x <- IKbandwidth(ds.fixed$er.v.c_l, ds.fixed$immi_blur_fd) 
summary(rdrobust(ds.fixed$immi_blur_fd, ds.fixed$er.v.c_l, cluster=ds.fixed$iso2c, all=TRUE, h=c(x)))

x <- IKbandwidth(ds.fixed$er.v.c_l, ds.fixed$eu_blur_fd) 
summary(rdrobust(ds.fixed$eu_blur_fd, ds.fixed$er.v.c_l, cluster=ds.fixed$iso2c, all=TRUE, h=c(x)))




#### - median cut-off before and after thresholds ####
ds %>% filter( er.v.c_l < 0 ) -> left
left.median <- median( left$er.v.c_l )

ds %>% filter( er.v.c_l >= 0 ) -> right
right.median <- median( right$er.v.c_l )

# blurring #immigration blurring gives weird error. Manually estimate those estimates using RDrobust. 
rob_blur_immi <- rd.placebo( data = ds 
                             , force.var = 'er.v.c_l' 
                             , yvar = 'immi_blur_fd'
                             , seat.identifier = 'er.in_l' 
                             , fixed.effects = 'iso2c'
                             , clust1 = 'party' 
                             , clust2 = 'edate'
                             , polynomials = c( 1 , 2 , 3 , 4 )
                             , cut.ps = c( left.median , 0 , right.median )
                             , bws = NULL
)
rob_blur_immi
print(xtable::xtable(rob_blur_immi) , comment = FALSE , include.rownames = FALSE )

x <- IKbandwidth(ds$er.v.c_l, ds$immi_blur_fd) #change formula for those error values. 
summary(rdrobust(ds$immi_blur_fd, ds$er.v.c_l, cluster=ds$iso2c, all=TRUE, p=2, c=6, h=c(3.25)))




rob_blur_eu <- rd.placebo( data = ds 
                           , force.var = 'er.v.c_l' 
                           , yvar = 'eu_blur_fd'
                           , seat.identifier = 'er.in_l' 
                           , fixed.effects = 'iso2c'
                           , clust1 = 'party' 
                           , clust2 = 'edate'
                           , polynomials = c( 1 , 2 , 3 , 4 )
                           , cut.ps = c( left.median , 0 , right.median )
                           , bws = NULL
)
rob_blur_eu
print(xtable::xtable(rob_blur_eu) , comment = FALSE , include.rownames = FALSE )

#emphasising
rob_emp_immi <- rd.placebo( data = ds 
                            , force.var = 'er.v.c_l' 
                            , yvar = 'immi_sal_fd'
                            , seat.identifier = 'er.in_l' 
                            , fixed.effects = 'iso2c'
                            , clust1 = 'party' 
                            , clust2 = 'edate'
                            , polynomials = c( 1 , 2 , 3 , 4 )
                            , cut.ps = c( left.median , 0 , right.median )
                            , bws = NULL
)
rob_emp_immi
print(xtable::xtable(rob_emp_immi) , comment = FALSE , include.rownames = FALSE )

rob_emp_eu <- rd.placebo( data = ds 
                          , force.var = 'er.v.c_l' 
                          , yvar = 'eu_sal_fd'
                          , seat.identifier = 'er.in_l' 
                          , fixed.effects = 'iso2c'
                          , clust1 = 'party' 
                          , clust2 = 'edate'
                          , polynomials = c( 1 , 2 , 3 , 4 )
                          , cut.ps = c( left.median , 0 , right.median )
                          , bws = NULL
)
rob_emp_eu
print(xtable::xtable(rob_emp_eu) , comment = FALSE , include.rownames = FALSE )

#Entrepreneurship
rob_ent_immi <- rd.placebo( data = ds 
                            , force.var = 'er.v.c_l' 
                            , yvar = 'immi_ent_fd'
                            , seat.identifier = 'er.in_l' 
                            , fixed.effects = 'iso2c'
                            , clust1 = 'party' 
                            , clust2 = 'edate'
                            , polynomials = c( 1 , 2 , 3 , 4 )
                            , cut.ps = c( left.median , 0 , right.median )
                            , bws = NULL
)
rob_ent_immi
print(xtable::xtable(rob_ent_immi) , comment = FALSE , include.rownames = FALSE )

rob_ent_eu <- rd.placebo( data = ds 
                          , force.var = 'er.v.c_l' 
                          , yvar = 'eu_ent_fd'
                          , seat.identifier = 'er.in_l' 
                          , fixed.effects = 'iso2c'
                          , clust1 = 'party' 
                          , clust2 = 'edate'
                          , polynomials = c( 1 , 2 , 3 , 4 )
                          , cut.ps = c( left.median , 0 , right.median )
                          , bws = NULL
)
rob_ent_eu
print(xtable::xtable(rob_ent_eu) , comment = FALSE , include.rownames = FALSE )





















