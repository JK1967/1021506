##################################3
#   Project: MPhil thesis         #
#   Name: ...                     #
#   Description: analysis on the  #
#     NL for final chapter.       #
##################################3

#### - packages ####
library(sjlabelled)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Hmisc)
library(readxl)
library(readxl)
library(dplyr)
library(haven)

#### - loading data ####
master_file <- readRDS("master_file_v2.rds")
master_file_nl <- subset(master_file, country == 10 )
master_file_nl_main <- master_file_nl[(master_file_nl$party == "CDA" |
                                         master_file_nl$party == "VVD" | 
                                         master_file_nl$party == "PvdA" |
                                         master_file_nl$party == "PVV"), ] 

#### - Analyis ####

## Plots for immigration
#salience and position plot
nl_sal_pos <- ggplot(data = subset(master_file_nl_main, year == 2010),
                     mapping = aes(x = immigrate_policy,  y = immigra_salience)) + 
  geom_text(mapping = aes(label = party), size = 4) +
  theme(axis.text = element_text(size = 9)) + theme_bw()+ 
  labs(x = "Immigration position", y= "Salience") + 
  xlim(0, 10) + ylim(0, 10) + geom_vline(xintercept = 5) + 
  geom_hline(yintercept = 5)
nl_sal_pos #saved as 445 X 320


#independent variables plot 
nl_pot <- ggplot(data = subset(master_file_nl_main, year == 2010),
                 mapping = aes(y = perext_immi_lv,  x = sd_immi_lv)) + 
  geom_text(mapping = aes(label = party), size = 4) +
  theme(axis.text = element_text(size = 9)) + theme_bw() + 
  labs(y = "Electoral potential", x = "Dividing potential")
nl_pot #saved as 445 X 320


#blurring by parties
nl_blur <- ggplot(data = subset(master_file_nl_main, year == 2010), 
                  mapping = aes(y = reorder(party, immigration_dimension_sd),
                                x = immigration_dimension_sd)) +
  geom_point(size = 3) + theme_bw() + labs(x = "Blurring", y = "Party")
nl_blur #saved as 300X215




## Plots for the EU
#salience and position
nl_sal_pos_eu <- ggplot(data = subset(master_file_nl_main, year == 2014),
                        mapping = aes(x = (11 - position),  y = eu_salience)) + 
  geom_text(mapping = aes(label = party), size = 4) +
  theme(axis.text=element_text(size = 9)) + theme_bw() + 
  labs(x = "Immigration position", y= "Salience") + 
  xlim(0, 10) + ylim(0, 10) + geom_vline(xintercept = 5) +
  geom_hline(yintercept = 5)
nl_sal_pos_eu #saved as 445 X 320


#independent variables. 
nl_pot_eu <- ggplot(data = subset(master_file_nl_main, year == 2014),
                    mapping = aes(y = perext_eu_lv,  x = sd_eu_lv)) + 
  geom_text(mapping = aes(label = party), size = 4) +
  theme(axis.text=element_text(size = 9)) + theme_bw() + 
  labs(y = "Electoral potential", x = "Dividing potential")
nl_pot_eu


#blurring by parties
nl_blur_eu <- ggplot(data=subset(master_file_nl_main, year == 2014),
                     mapping = aes(y = reorder(party, eu_dimension_sd),
                                   x = eu_dimension_sd)) +
  geom_point(size = 3) + theme_bw() + labs(x = "Blurring", y = "Party")
nl_blur_eu #300X215


