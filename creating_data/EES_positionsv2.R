# Project: MPhil dissertation in Politcs -- Oxford
# Date: April 2019
# Author: ANONYMOUS
# description: this file creates the database for the characteristics of likely voters. 
# approximate running time: 10 minutes. 



library(sjlabelled)
library(dplyr)
library(tidyr)
library(Hmisc)
library(readxl)
library(dplyr)
library(haven)
#some packages come later down, make sure all is installed and works otherwise NAs will ensure. 
#running all at once doesn't always work. Better to do bit by bit (especially overlap between parties)


EES_2014 <- read_dta("EES 2014 ZA5160_v4-0-0.dta")



###############################EES_2014##
#############EES_2014: Running the code for EES_2014$qpp6_ees & qpp5_ees# This adds the CHES scores in the cells instead of EES scores#################
##i.e. party a voter voted for or who she would consider voting for. 

#add empty variables to fill. These are the party codes for likely to vote on in the cells.
EES_2014$qpp5_ches <- NA #which party did you vote for?
EES_2014$qpp6_ches <- NA #which party would vote for?
EES_2014$qpp21_ches <- NA #Close to a particular party

#get_label(EES_2014$qpp6)
#get_labels(EES_2014$qpp8_1)

#Austria
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1040520] <- 1302 # AT OVP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1040302] <- 1301 # AT SPO 
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1040423] <- 1309 # AT NEOS 
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1040110] <- 1304 # AT Grune 
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1040420] <- 1303 # AT FPO 
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1040600] <- 1307 # AT BZO

# Belgium (Flanders & Wallonia)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1056325] <- 119 ##  BE PVDA
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1056521] <- 109 ##  BE CD&V
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1056327] <- 103 ##  BE SPA
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1056421] <- 107 ##  BE VLD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1056913] <- 110 ##  BE NVA
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1056112] <- 105 ##  BE Groen
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1056711] <- 112 ##  BE VB
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1056522] <- 108 #   BE cdH
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1056322] <- 102 #   BE PS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1056427] <- 106 #   BE MR
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1056111] <- 104 #   BE ECOLO

# Bulgaria (1100300, 1100601, 1100400 missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1100600] <- 2010 # BG GERB
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1100900] <- 2004 # BG DPS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1100700] <- 2007 # BG Attack
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1100602] <- 2015 # BG BBT
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1103001] <- 2016 # BG ABV
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1100001] <- 2002 # BG SDS

# Cyrpus (1196002 is missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1196711] <- 4001 # CY DISY
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1196422] <- 4004 # CY DIKO
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1196322] <- 4005 # CY EDEK
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1196321] <- 4003 # CY AKEL
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1196110] <- 4006 # CY KOP

# Czech Republic (1203321 1203110 is missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1203523] <- 2104 #  CZ KDU-CSL
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1203530] <- 2109 #  CZ TOP09
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1203320] <- 2101 #  CZ CSSD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1203413] <- 2102 #  CZ ODS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1203220] <- 2103 #  CZ KSCM
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1203413] <- 2111 #  CZ ANO2011
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1203953] <- 2113 #  CZ SVOBODNI

# Germany
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1276521] <- 301 # GE CDU
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1276320] <- 302 # GE SPD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1276420] <- 303 # GE FDP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1276113] <- 304 # GE Grunen
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1276321] <- 306 # GE Linke
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1276621] <- 310 # GE AfD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1276951] <- 311 # GE Piraten

# Denmark (FolkB is missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1208320] <- 201 # DK SD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1208420] <- 211 # DK V
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1208330] <- 206 # DK SF
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1208720] <- 215 # DK DF
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1208410] <- 202 # DK RV
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1208421] <- 218 # DK LA
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1208620] <- 203 # DK KF

# Estonia (1233003 is missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1233613] <- 2201 # EE IRL
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1233410] <- 2204 # EE SDE
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1233430] <- 2203 # EE ER
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1233411] <- 2202 # EE EK
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1233100] <- 2207 # EE EER

# Greece (1300116 is missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1300511] <- 402 # GR ND
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1300215] <- 403 # GR SYRIZA
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1300313] <- 401 # GR PASOK
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1300611] <- 401 # GR ANEL
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1300710] <- 415 # GR XA
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1300225] <- 414 # GR DIMAR
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1300210] <- 404 # GR KKE
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1300323] <- 413 # GR Potami
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1300703] <- 410 # GR LAOS

# Spain 
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1724610] <- 502 # ESP PP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1724320] <- 501 # ESP PSOE
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1724220] <- 504 # ESP IU
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1724010] <- 523 # ESP UPyd
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1724905] <- 511 # ESP ERC
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1724310] <- 526 # ESP Cs
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1724230] <- 525 # ESP Podemos
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1724007] <- 505 # ESP CiU
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1724902] <- 506 # ESP EAJ-PNV
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1724908] <- 513 # ESP BNG
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1724907] <- 517 # ESP CC

# Finland
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1246620] <- 1402 # FI KOK
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1246520] <- 1409 # FI KD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1246320] <- 1401 # FI SDP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1246810] <- 1403 # FI KESK
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1246901] <- 1406 # FI RKP/SFP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1246110] <- 1408 # FI VIHR
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1246223] <- 1404 # FI VAS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1246820] <- 1405 # FI PS

# France (Please check for 1250223. 1250636, 1250233 are missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1250626] <- 609 # FR UMP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1250320] <- 602 # FR PS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1250720] <- 610 # FR FN
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1250110] <- 605 # FR EELV
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1250223] <- 601 # FR PCF
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1250336] <- 613 # FR MODEM

# Hungary
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1348700] <- 2308 # HU JOBBIK
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1348110] <- 2309 # HU LMP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1348421] <- 2302 # HU Fidesz
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1348220] <- 2301 # HU MSzP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1348120] <- 2310 # HU E14
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1348330] <- 2311 # HU DK

# Ireland
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1372520] <- 702 # IE FG
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1372320] <- 703 # IE Lab
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1372620] <- 701 # IE FF
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1372110] <- 705 # IE GP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1372951] <- 707 # IE SF
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1372220] <- 708 # IE SP

# Italy (1380902, 1380630 are missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1380331] <- 837 # IT PD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1380610] <- 815 # IT FI
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1380720] <- 811 # IT LN
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1380956] <- 845 # IT M5S
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1380523] <- 814 # IT UDC
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1380007] <- 838 # IT SEl
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1380633] <- 848 # IT NCD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1380631] <- 844 # IT FDL
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1380958] <- 827 # IT SVP

# Lithuania (1440420 is missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1440620] <- 2506 # LT TS-LKD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1440320] <- 2501 # LT LSDP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1440421] <- 2518 # LT LRLS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1440322] <- 2516 # LT DP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1440621] <- 2515 # LT TT
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1440952] <- 2511 # LT LLRA
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1440524] <- 2507 # LT LVZS

# Luxembourg (1442220 is missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1442520] <- 3801 # LU CSV
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1442320] <- 3804 # LU LSAP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1442420] <- 3803 # LU DP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1442113] <- 3802 # LU Greng
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1442222] <- 3806 # LU DL
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1442951] <- 3805 # LU ADR

# Latvia (1428620 1428422 1428424 are missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1428610] <- 2412 # LV V
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1428317] <- 2410 # LV SDPS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1428723] <- 2406 # LV NA
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1428110] <- 2405 # LV ZZS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1428901] <- 2402 # LV LKS 

# Malta (1470100 is missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1470300] <- 3701 # MT PL
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1470500] <- 3702 # MT PN

# Netherlands (1528528 - please check. EES codes as coalition of 2 parties)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1528420] <- 1003 # NL VVD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1528320] <- 1002 # NL PvdA
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1528600] <- 1017 # NL PVV
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1528220] <- 1014 # NL SP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1528521] <- 1001 # NL CDA
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1528330] <- 1004 # NL D66
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1528526] <- 1016 # NL CU
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1528110] <- 1005 # NL GL
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1528951] <- 1018 # NL PvdD

# Poland
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1616435] <- 2603 # PL PO
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1616811] <- 2606 # PL PSL
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1616210] <- 2601 # PL SLD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1616436] <- 2605 # PL PiS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1616310] <- 2613 # PL RP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1616001] <- 2614 # PL KNP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1616002] <- 2616 # PL SP

# Portugal (1620314 - please check. EES codes as coalition of 2 parties)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1620313] <- 1206 # PT PSD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1620520] <- 1202 # PT PP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1620311] <- 1205 # PT PS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1620229] <- 1201 # PT CDU
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1620211] <- 1208 # PT BE
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1620110] <- 1209 # PT MPT

# Romania (1642700, 1642800 are missing. 1642502 is a coalition)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1642300] <- 2701 # RO PSD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1642401] <- 2705 # RO PNL
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1642400] <- 2704 # RO PDL
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1642981] <- 2710 # RO PP-DD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1642900] <- 2706 # RO UDMR
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1642503] <- 2711 # RO PMP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1642600] <- 2702 # RO PC

# Sweden
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1752320] <- 1602 # SE SAP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1752620] <- 1605 # SE M
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1752110] <- 1607 # SE MP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1752420] <- 1604 # SE FP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1752810] <- 1603 # SE C
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1752700] <- 1610 # SE SD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1752520] <- 1606 # SE KD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1752220] <- 1601 # SE V
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1752953] <- 1612 # SE FI
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1752000] <- 1611 # SE PP

# Slovenia (1705450, 1705421, 1705324, 1705710, 1705952 are missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1705340] <- 2914 # SI PS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1705320] <- 2902 # SI SDS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1705323] <- 2903 # SI SD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1705951] <- 2906 # SI DeSUS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1705522] <- 2905 # SI NSI
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1705521] <- 2904 # SI SLS

# Slovakia (1703222 is missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1703521] <- 2805 # SK KDH
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1703523] <- 2802 # SK SDKU-DS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1703954] <- 2804 # SK SMK-MKP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1703423] <- 2803 # SK Smer-SD
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1703610] <- 2815 # SK NOVA
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1703440] <- 2812 # SK SaS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1703620] <- 2814 # SK OLaNO
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1703955] <- 2813 # SK MH
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1703710] <- 2809 # SK SNS

# UK (1826210, 1826903, 1826724, 1826720 are missing)
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1826620] <- 1101 # UK Cons
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1826320] <- 1102 # UK Lab
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1826421] <- 1104 # UK LibDems
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1826110] <- 1107 # UK Green
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1826951] <- 1108 # UK UKIP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1826902] <- 1105 # UK SNP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1826901] <- 1106 # UK Plaid

# Croatia
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1191320] <- 3102 # HR SDP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1191412] <- 3105 # HR HNS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1191511] <- 3101 # HR HDZ
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1191613] <- 3109 # HR HSP
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1191410] <- 3104 # HR HSLS
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1191952] <- 3107 # HR HDSSB
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1191330] <- 3112 # HR HL-SR
EES_2014$qpp6_ches[EES_2014$qpp6_ees==1191110] <- 3114 # HR ORaH


#############Do the same code but for all the 5 questions#################

#Austria
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1040520] <- 1302 # AT OVP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1040302] <- 1301 # AT SPO 
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1040423] <- 1309 # AT NEOS 
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1040110] <- 1304 # AT Grune 
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1040420] <- 1303 # AT FPO 
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1040600] <- 1307 # AT BZO

# Belgium (Flanders & Wallonia)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1056325] <- 119 ##  BE PVDA
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1056521] <- 109 ##  BE CD&V
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1056327] <- 103 ##  BE SPA
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1056421] <- 107 ##  BE VLD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1056913] <- 110 ##  BE NVA
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1056112] <- 105 ##  BE Groen
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1056711] <- 112 ##  BE VB
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1056522] <- 108 #   BE cdH
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1056322] <- 102 #   BE PS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1056427] <- 106 #   BE MR
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1056111] <- 104 #   BE ECOLO

# Bulgaria (1100300, 1100601, 1100400 missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1100600] <- 2010 # BG GERB
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1100900] <- 2004 # BG DPS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1100700] <- 2007 # BG Attack
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1100602] <- 2015 # BG BBT
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1103001] <- 2016 # BG ABV
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1100001] <- 2002 # BG SDS

# Cyrpus (1196002 is missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1196711] <- 4001 # CY DISY
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1196422] <- 4004 # CY DIKO
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1196322] <- 4005 # CY EDEK
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1196321] <- 4003 # CY AKEL
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1196110] <- 4006 # CY KOP

# Czech Republic (1203321 1203110 is missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1203523] <- 2104 #  CZ KDU-CSL
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1203530] <- 2109 #  CZ TOP09
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1203320] <- 2101 #  CZ CSSD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1203413] <- 2102 #  CZ ODS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1203220] <- 2103 #  CZ KSCM
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1203413] <- 2111 #  CZ ANO2011
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1203953] <- 2113 #  CZ SVOBODNI

# Germany
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1276521] <- 301 # GE CDU
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1276320] <- 302 # GE SPD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1276420] <- 303 # GE FDP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1276113] <- 304 # GE Grunen
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1276321] <- 306 # GE Linke
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1276621] <- 310 # GE AfD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1276951] <- 311 # GE Piraten

# Denmark (FolkB is missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1208320] <- 201 # DK SD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1208420] <- 211 # DK V
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1208330] <- 206 # DK SF
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1208720] <- 215 # DK DF
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1208410] <- 202 # DK RV
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1208421] <- 218 # DK LA
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1208620] <- 203 # DK KF

# Estonia (1233003 is missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1233613] <- 2201 # EE IRL
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1233410] <- 2204 # EE SDE
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1233430] <- 2203 # EE ER
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1233411] <- 2202 # EE EK
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1233100] <- 2207 # EE EER

# Greece (1300116 is missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1300511] <- 402 # GR ND
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1300215] <- 403 # GR SYRIZA
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1300313] <- 401 # GR PASOK
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1300611] <- 401 # GR ANEL
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1300710] <- 415 # GR XA
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1300225] <- 414 # GR DIMAR
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1300210] <- 404 # GR KKE
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1300323] <- 413 # GR Potami
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1300703] <- 410 # GR LAOS

# Spain 
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1724610] <- 502 # ESP PP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1724320] <- 501 # ESP PSOE
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1724220] <- 504 # ESP IU
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1724010] <- 523 # ESP UPyd
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1724905] <- 511 # ESP ERC
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1724310] <- 526 # ESP Cs
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1724230] <- 525 # ESP Podemos
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1724007] <- 505 # ESP CiU
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1724902] <- 506 # ESP EAJ-PNV
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1724908] <- 513 # ESP BNG
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1724907] <- 517 # ESP CC

# Finland
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1246620] <- 1402 # FI KOK
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1246520] <- 1409 # FI KD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1246320] <- 1401 # FI SDP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1246810] <- 1403 # FI KESK
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1246901] <- 1406 # FI RKP/SFP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1246110] <- 1408 # FI VIHR
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1246223] <- 1404 # FI VAS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1246820] <- 1405 # FI PS

# France (Please check for 1250223. 1250636, 1250233 are missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1250626] <- 609 # FR UMP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1250320] <- 602 # FR PS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1250720] <- 610 # FR FN
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1250110] <- 605 # FR EELV
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1250223] <- 601 # FR PCF
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1250336] <- 613 # FR MODEM

# Hungary
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1348700] <- 2308 # HU JOBBIK
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1348110] <- 2309 # HU LMP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1348421] <- 2302 # HU Fidesz
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1348220] <- 2301 # HU MSzP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1348120] <- 2310 # HU E14
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1348330] <- 2311 # HU DK

# Ireland
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1372520] <- 702 # IE FG
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1372320] <- 703 # IE Lab
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1372620] <- 701 # IE FF
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1372110] <- 705 # IE GP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1372951] <- 707 # IE SF
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1372220] <- 708 # IE SP

# Italy (1380902, 1380630 are missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1380331] <- 837 # IT PD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1380610] <- 815 # IT FI
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1380720] <- 811 # IT LN
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1380956] <- 845 # IT M5S
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1380523] <- 814 # IT UDC
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1380007] <- 838 # IT SEl
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1380633] <- 848 # IT NCD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1380631] <- 844 # IT FDL
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1380958] <- 827 # IT SVP

# Lithuania (1440420 is missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1440620] <- 2506 # LT TS-LKD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1440320] <- 2501 # LT LSDP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1440421] <- 2518 # LT LRLS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1440322] <- 2516 # LT DP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1440621] <- 2515 # LT TT
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1440952] <- 2511 # LT LLRA
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1440524] <- 2507 # LT LVZS

# Luxembourg (1442220 is missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1442520] <- 3801 # LU CSV
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1442320] <- 3804 # LU LSAP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1442420] <- 3803 # LU DP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1442113] <- 3802 # LU Greng
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1442222] <- 3806 # LU DL
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1442951] <- 3805 # LU ADR

# Latvia (1428620 1428422 1428424 are missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1428610] <- 2412 # LV V
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1428317] <- 2410 # LV SDPS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1428723] <- 2406 # LV NA
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1428110] <- 2405 # LV ZZS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1428901] <- 2402 # LV LKS 

# Malta (1470100 is missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1470300] <- 3701 # MT PL
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1470500] <- 3702 # MT PN

# Netherlands (1528528 - please check. EES codes as coalition of 2 parties)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1528420] <- 1003 # NL VVD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1528320] <- 1002 # NL PvdA
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1528600] <- 1017 # NL PVV
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1528220] <- 1014 # NL SP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1528521] <- 1001 # NL CDA
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1528330] <- 1004 # NL D66
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1528526] <- 1016 # NL CU
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1528110] <- 1005 # NL GL
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1528951] <- 1018 # NL PvdD

# Poland
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1616435] <- 2603 # PL PO
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1616811] <- 2606 # PL PSL
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1616210] <- 2601 # PL SLD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1616436] <- 2605 # PL PiS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1616310] <- 2613 # PL RP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1616001] <- 2614 # PL KNP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1616002] <- 2616 # PL SP

# Portugal (1620314 - please check. EES codes as coalition of 2 parties)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1620313] <- 1206 # PT PSD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1620520] <- 1202 # PT PP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1620311] <- 1205 # PT PS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1620229] <- 1201 # PT CDU
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1620211] <- 1208 # PT BE
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1620110] <- 1209 # PT MPT

# Romania (1642700, 1642800 are missing. 1642502 is a coalition)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1642300] <- 2701 # RO PSD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1642401] <- 2705 # RO PNL
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1642400] <- 2704 # RO PDL
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1642981] <- 2710 # RO PP-DD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1642900] <- 2706 # RO UDMR
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1642503] <- 2711 # RO PMP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1642600] <- 2702 # RO PC

# Sweden
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1752320] <- 1602 # SE SAP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1752620] <- 1605 # SE M
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1752110] <- 1607 # SE MP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1752420] <- 1604 # SE FP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1752810] <- 1603 # SE C
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1752700] <- 1610 # SE SD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1752520] <- 1606 # SE KD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1752220] <- 1601 # SE V
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1752953] <- 1612 # SE FI
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1752000] <- 1611 # SE PP

# Slovenia (1705450, 1705421, 1705324, 1705710, 1705952 are missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1705340] <- 2914 # SI PS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1705320] <- 2902 # SI SDS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1705323] <- 2903 # SI SD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1705951] <- 2906 # SI DeSUS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1705522] <- 2905 # SI NSI
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1705521] <- 2904 # SI SLS

# Slovakia (1703222 is missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1703521] <- 2805 # SK KDH
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1703523] <- 2802 # SK SDKU-DS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1703954] <- 2804 # SK SMK-MKP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1703423] <- 2803 # SK Smer-SD
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1703610] <- 2815 # SK NOVA
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1703440] <- 2812 # SK SaS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1703620] <- 2814 # SK OLaNO
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1703955] <- 2813 # SK MH
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1703710] <- 2809 # SK SNS

# UK (1826210, 1826903, 1826724, 1826720 are missing)
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1826620] <- 1101 # UK Cons
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1826320] <- 1102 # UK Lab
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1826421] <- 1104 # UK LibDems
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1826110] <- 1107 # UK Green
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1826951] <- 1108 # UK UKIP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1826902] <- 1105 # UK SNP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1826901] <- 1106 # UK Plaid

# Croatia
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1191320] <- 3102 # HR SDP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1191412] <- 3105 # HR HNS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1191511] <- 3101 # HR HDZ
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1191613] <- 3109 # HR HSP
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1191410] <- 3104 # HR HSLS
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1191952] <- 3107 # HR HDSSB
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1191330] <- 3112 # HR HL-SR
EES_2014$qpp5_ches[EES_2014$qpp5_ees==1191110] <- 3114 # HR ORaH




#############Same again but for 21 Question###################################

#Austria
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1040520] <- 1302 # AT OVP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1040302] <- 1301 # AT SPO 
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1040423] <- 1309 # AT NEOS 
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1040110] <- 1304 # AT Grune 
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1040420] <- 1303 # AT FPO 
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1040600] <- 1307 # AT BZO

# Belgium (Flanders & Wallonia)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1056325] <- 119 ##  BE PVDA
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1056521] <- 109 ##  BE CD&V
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1056327] <- 103 ##  BE SPA
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1056421] <- 107 ##  BE VLD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1056913] <- 110 ##  BE NVA
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1056112] <- 105 ##  BE Groen
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1056711] <- 112 ##  BE VB
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1056522] <- 108 #   BE cdH
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1056322] <- 102 #   BE PS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1056427] <- 106 #   BE MR
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1056111] <- 104 #   BE ECOLO

# Bulgaria (1100300, 1100601, 1100400 missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1100600] <- 2010 # BG GERB
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1100900] <- 2004 # BG DPS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1100700] <- 2007 # BG Attack
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1100602] <- 2015 # BG BBT
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1103001] <- 2016 # BG ABV
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1100001] <- 2002 # BG SDS

# Cyrpus (1196002 is missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1196711] <- 4001 # CY DISY
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1196422] <- 4004 # CY DIKO
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1196322] <- 4005 # CY EDEK
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1196321] <- 4003 # CY AKEL
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1196110] <- 4006 # CY KOP

# Czech Republic (1203321 1203110 is missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1203523] <- 2104 #  CZ KDU-CSL
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1203530] <- 2109 #  CZ TOP09
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1203320] <- 2101 #  CZ CSSD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1203413] <- 2102 #  CZ ODS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1203220] <- 2103 #  CZ KSCM
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1203413] <- 2111 #  CZ ANO2011
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1203953] <- 2113 #  CZ SVOBODNI

# Germany
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1276521] <- 301 # GE CDU
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1276320] <- 302 # GE SPD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1276420] <- 303 # GE FDP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1276113] <- 304 # GE Grunen
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1276321] <- 306 # GE Linke
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1276621] <- 310 # GE AfD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1276951] <- 311 # GE Piraten

# Denmark (FolkB is missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1208320] <- 201 # DK SD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1208420] <- 211 # DK V
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1208330] <- 206 # DK SF
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1208720] <- 215 # DK DF
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1208410] <- 202 # DK RV
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1208421] <- 218 # DK LA
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1208620] <- 203 # DK KF

# Estonia (1233003 is missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1233613] <- 2201 # EE IRL
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1233410] <- 2204 # EE SDE
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1233430] <- 2203 # EE ER
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1233411] <- 2202 # EE EK
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1233100] <- 2207 # EE EER

# Greece (1300116 is missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1300511] <- 402 # GR ND
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1300215] <- 403 # GR SYRIZA
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1300313] <- 401 # GR PASOK
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1300611] <- 401 # GR ANEL
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1300710] <- 415 # GR XA
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1300225] <- 414 # GR DIMAR
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1300210] <- 404 # GR KKE
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1300323] <- 413 # GR Potami
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1300703] <- 410 # GR LAOS

# Spain 
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1724610] <- 502 # ESP PP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1724320] <- 501 # ESP PSOE
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1724220] <- 504 # ESP IU
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1724010] <- 523 # ESP UPyd
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1724905] <- 511 # ESP ERC
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1724310] <- 526 # ESP Cs
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1724230] <- 525 # ESP Podemos
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1724007] <- 505 # ESP CiU
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1724902] <- 506 # ESP EAJ-PNV
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1724908] <- 513 # ESP BNG
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1724907] <- 517 # ESP CC

# Finland
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1246620] <- 1402 # FI KOK
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1246520] <- 1409 # FI KD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1246320] <- 1401 # FI SDP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1246810] <- 1403 # FI KESK
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1246901] <- 1406 # FI RKP/SFP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1246110] <- 1408 # FI VIHR
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1246223] <- 1404 # FI VAS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1246820] <- 1405 # FI PS

# France (Please check for 1250223. 1250636, 1250233 are missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1250626] <- 609 # FR UMP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1250320] <- 602 # FR PS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1250720] <- 610 # FR FN
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1250110] <- 605 # FR EELV
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1250223] <- 601 # FR PCF
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1250336] <- 613 # FR MODEM

# Hungary
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1348700] <- 2308 # HU JOBBIK
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1348110] <- 2309 # HU LMP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1348421] <- 2302 # HU Fidesz
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1348220] <- 2301 # HU MSzP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1348120] <- 2310 # HU E14
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1348330] <- 2311 # HU DK

# Ireland
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1372520] <- 702 # IE FG
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1372320] <- 703 # IE Lab
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1372620] <- 701 # IE FF
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1372110] <- 705 # IE GP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1372951] <- 707 # IE SF
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1372220] <- 708 # IE SP

# Italy (1380902, 1380630 are missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1380331] <- 837 # IT PD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1380610] <- 815 # IT FI
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1380720] <- 811 # IT LN
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1380956] <- 845 # IT M5S
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1380523] <- 814 # IT UDC
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1380007] <- 838 # IT SEl
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1380633] <- 848 # IT NCD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1380631] <- 844 # IT FDL
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1380958] <- 827 # IT SVP

# Lithuania (1440420 is missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1440620] <- 2506 # LT TS-LKD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1440320] <- 2501 # LT LSDP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1440421] <- 2518 # LT LRLS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1440322] <- 2516 # LT DP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1440621] <- 2515 # LT TT
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1440952] <- 2511 # LT LLRA
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1440524] <- 2507 # LT LVZS

# Luxembourg (1442220 is missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1442520] <- 3801 # LU CSV
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1442320] <- 3804 # LU LSAP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1442420] <- 3803 # LU DP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1442113] <- 3802 # LU Greng
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1442222] <- 3806 # LU DL
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1442951] <- 3805 # LU ADR

# Latvia (1428620 1428422 1428424 are missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1428610] <- 2412 # LV V
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1428317] <- 2410 # LV SDPS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1428723] <- 2406 # LV NA
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1428110] <- 2405 # LV ZZS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1428901] <- 2402 # LV LKS 

# Malta (1470100 is missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1470300] <- 3701 # MT PL
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1470500] <- 3702 # MT PN

# Netherlands (1528528 - please check. EES codes as coalition of 2 parties)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1528420] <- 1003 # NL VVD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1528320] <- 1002 # NL PvdA
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1528600] <- 1017 # NL PVV
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1528220] <- 1014 # NL SP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1528521] <- 1001 # NL CDA
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1528330] <- 1004 # NL D66
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1528526] <- 1016 # NL CU
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1528110] <- 1005 # NL GL
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1528951] <- 1018 # NL PvdD

# Poland
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1616435] <- 2603 # PL PO
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1616811] <- 2606 # PL PSL
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1616210] <- 2601 # PL SLD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1616436] <- 2605 # PL PiS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1616310] <- 2613 # PL RP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1616001] <- 2614 # PL KNP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1616002] <- 2616 # PL SP

# Portugal (1620314 - please check. EES codes as coalition of 2 parties)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1620313] <- 1206 # PT PSD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1620520] <- 1202 # PT PP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1620311] <- 1205 # PT PS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1620229] <- 1201 # PT CDU
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1620211] <- 1208 # PT BE
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1620110] <- 1209 # PT MPT

# Romania (1642700, 1642800 are missing. 1642502 is a coalition)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1642300] <- 2701 # RO PSD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1642401] <- 2705 # RO PNL
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1642400] <- 2704 # RO PDL
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1642981] <- 2710 # RO PP-DD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1642900] <- 2706 # RO UDMR
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1642503] <- 2711 # RO PMP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1642600] <- 2702 # RO PC

# Sweden
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1752320] <- 1602 # SE SAP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1752620] <- 1605 # SE M
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1752110] <- 1607 # SE MP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1752420] <- 1604 # SE FP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1752810] <- 1603 # SE C
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1752700] <- 1610 # SE SD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1752520] <- 1606 # SE KD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1752220] <- 1601 # SE V
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1752953] <- 1612 # SE FI
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1752000] <- 1611 # SE PP

# Slovenia (1705450, 1705421, 1705324, 1705710, 1705952 are missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1705340] <- 2914 # SI PS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1705320] <- 2902 # SI SDS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1705323] <- 2903 # SI SD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1705951] <- 2906 # SI DeSUS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1705522] <- 2905 # SI NSI
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1705521] <- 2904 # SI SLS

# Slovakia (1703222 is missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1703521] <- 2805 # SK KDH
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1703523] <- 2802 # SK SDKU-DS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1703954] <- 2804 # SK SMK-MKP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1703423] <- 2803 # SK Smer-SD
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1703610] <- 2815 # SK NOVA
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1703440] <- 2812 # SK SaS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1703620] <- 2814 # SK OLaNO
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1703955] <- 2813 # SK MH
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1703710] <- 2809 # SK SNS

# UK (1826210, 1826903, 1826724, 1826720 are missing)
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1826620] <- 1101 # UK Cons
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1826320] <- 1102 # UK Lab
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1826421] <- 1104 # UK LibDems
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1826110] <- 1107 # UK Green
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1826951] <- 1108 # UK UKIP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1826902] <- 1105 # UK SNP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1826901] <- 1106 # UK Plaid

# Croatia
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1191320] <- 3102 # HR SDP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1191412] <- 3105 # HR HNS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1191511] <- 3101 # HR HDZ
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1191613] <- 3109 # HR HSP
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1191410] <- 3104 # HR HSLS
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1191952] <- 3107 # HR HDSSB
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1191330] <- 3112 # HR HL-SR
EES_2014$qpp21_ches[EES_2014$qpp21_ees==1191110] <- 3114 # HR ORaH



#############check succes#############################

Hmisc::describe(EES_2014$qpp5_ches)
describe(EES_2014$qpp6_ches)
sum(EES_2014$qpp5_ees > 100)
sum(EES_2014$qpp6_ees == 1191320)
sum(EES_2014$qpp6_ees == 20)
sum(EES_2014$qpp5 > 0)






#############Adding the CHES codes for question 8 (same labels as for Q14) on likely to vote on, also add lv vote columns#############################
###This adds dummies for whether a voter is a likely voter for a specific party. 

##add new dummy columns for all unique parties in questions 5, 6, and 21. 
#unique ches codes in questions 5, 6, and 21 in one dataframe with ches_CODE. Note that the above code is already for 28 countries. 
#Thus, if you would want to add Eastern Europe you do not need to change the whole code because the unique 194 parties stay the same!
#the 194 are 194 because those are the parties identified by the EES team as the parties that pop up in the EES 2014 survey and have a corresponding CHES code.
x <- data.frame(ches = sort(unique(EES_2014$qpp5_ches))) #get unique values, thus parties, for all three questions
y <- data.frame(ches = sort(unique(EES_2014$qpp6_ches))) 
z <- data.frame(ches = sort(unique(EES_2014$qpp21_ches))) 
x <- bind_rows(x, y) #add to one big variable
x <- bind_rows(x, z)
unique_parties <- sort(unique(x$ches)) #take only unique values from one big variable. 194 unique parties
x <- data.frame(chescode = unique_parties) #add in dataframe with character
x$charac <- "lv_ches"  #add likely voter chess plus code
x <- unite(x, ches_code, 2:1) #unite with 2 first to put the lv_ches in front
lv_column_names_matrix <- x
y <- x$ches_code #turn into simple character string
#lv_column_names_string <- y  #need later when I calculate scores per party. 
EES_2014[y] <- NA #add unique values (i.e. party codes) to dataframe as new columns. 
rm(x, y, z) #clean up working space


######now fill all the dummies up with a 1 is a voter is a likely voter for a party
attach(EES_2014) #attach database so I don't have to use $

#see the labels
get_label(EES_2014$qpp8_1)
get_labels(EES_2014$qpp8_2)
#how this works: the qpp8_x are sorted per country. The 8 indicates the likely score to vote. For different voters change this score.
#right now I only have Western-European countries (same as in WRR, OECD 15-1). If you want more just add the countries because the empty columns are already there. 

#get rid of the NAs in the 8 question
EES_2014$qpp8_1[EES_2014$qpp8_1 == -99 | EES_2014$qpp8_1 == -9 | EES_2014$qpp8_1  == -8 | EES_2014$qpp8_1 == -7] <- NA
EES_2014$qpp8_2[EES_2014$qpp8_2 == -99 | EES_2014$qpp8_1 == -9 | EES_2014$qpp8_1  == -8 | EES_2014$qpp8_1 == -7] <- NA
EES_2014$qpp8_3[EES_2014$qpp8_3 == -99 | EES_2014$qpp8_1 == -9 | EES_2014$qpp8_1  == -8 | EES_2014$qpp8_1 == -7] <- NA
EES_2014$qpp8_4[EES_2014$qpp8_4 == -99 | EES_2014$qpp8_1 == -9 | EES_2014$qpp8_1  == -8 | EES_2014$qpp8_1 == -7] <- NA
EES_2014$qpp8_5[EES_2014$qpp8_5 == -99 | EES_2014$qpp8_1 == -9 | EES_2014$qpp8_1  == -8 | EES_2014$qpp8_1 == -7] <- NA
EES_2014$qpp8_6[EES_2014$qpp8_6 == -99 | EES_2014$qpp8_1 == -9 | EES_2014$qpp8_1  == -8 | EES_2014$qpp8_1 == -7] <- NA
EES_2014$qpp8_7[EES_2014$qpp8_7 == -99 | EES_2014$qpp8_1 == -9 | EES_2014$qpp8_1  == -8 | EES_2014$qpp8_1 == -7] <- NA
EES_2014$qpp8_8[EES_2014$qpp8_8 == -99 | EES_2014$qpp8_1 == -9 | EES_2014$qpp8_1  == -8 | EES_2014$qpp8_1 == -7] <- NA
describe(EES_2014$qpp8_1) #check if worked

###Add the likely voters for the different countries. Only doing Western Europe here. If adding easter Europe then add here. 
#Austria
EES_2014$lv_ches_1302 <- ifelse(countrycode==1040 & qpp8_1 > 6 | qpp5_ches == 1302 | qpp6_ches == 1302 | qpp21_ches == 1302, 1, NA)
EES_2014$lv_ches_1301 <- ifelse(countrycode==1040 & qpp8_2 > 6 | qpp5_ches == 1301 | qpp6_ches == 1301 | qpp21_ches == 1301, 1, NA)
EES_2014$lv_ches_1309 <- ifelse(countrycode==1040 & qpp8_3 > 6 | qpp5_ches == 1309 | qpp6_ches == 1309 | qpp21_ches == 1309, 1, NA)
EES_2014$lv_ches_1304 <- ifelse(countrycode==1040 & qpp8_4 > 6 | qpp5_ches == 1304 | qpp6_ches == 1304 | qpp21_ches == 1304, 1, NA)
EES_2014$lv_ches_1303 <- ifelse(countrycode==1040 & qpp8_5 > 6 | qpp5_ches == 1303 | qpp6_ches == 1303 | qpp21_ches == 1303, 1, NA)
EES_2014$lv_ches_1307 <- ifelse(countrycode==1040 & qpp8_6 > 6 | qpp5_ches == 1307 | qpp6_ches == 1307 | qpp21_ches == 1307, 1, NA)
describe(EES_2014$qpp8_1)
#Belgium (French)
EES_2014$lv_ches_108 <- ifelse(countrycode==1056 & p13_intlang==8 & qpp8_1 > 6 | qpp5_ches == 108 | qpp6_ches == 108 | qpp21_ches == 108, 1, NA)
EES_2014$lv_ches_102 <- ifelse(countrycode==1056 & p13_intlang==8 & qpp8_2 > 6 | qpp5_ches == 102 | qpp6_ches == 102 | qpp21_ches == 102, 1, NA)
EES_2014$lv_ches_106 <- ifelse(countrycode==1056 & p13_intlang==8 & qpp8_3 > 6 | qpp5_ches == 106 | qpp6_ches == 106 | qpp21_ches == 106, 1, NA)
EES_2014$lv_ches_104 <- ifelse(countrycode==1056 & p13_intlang==8 & qpp8_4 > 6 | qpp5_ches == 104 | qpp6_ches == 104 | qpp21_ches == 104, 1, NA)
EES_2014$lv_ches_120 <- ifelse(countrycode==1056 & p13_intlang==8 & qpp8_5 > 6 | qpp5_ches == 120 | qpp6_ches == 120 | qpp21_ches == 120, 1, NA)
EES_2014$lv_ches_119 <- ifelse(countrycode==1056 & p13_intlang==8 & qpp8_6 > 6 | qpp5_ches == 119 | qpp6_ches == 119 | qpp21_ches == 119, 1, NA)

#Belgium (Flanders)
EES_2014$lv_ches_119 <- ifelse(countrycode==1056 & qpp8_1 > 6 | qpp5_ches == 119 | qpp6_ches == 119 | qpp21_ches == 119, 1, EES_2014$lv_ches_119) #note not NA because double variable
EES_2014$lv_ches_109 <- ifelse(countrycode==1056 & qpp8_2 > 6 | qpp5_ches == 109 | qpp6_ches == 109 | qpp21_ches == 109, 1, NA)
EES_2014$lv_ches_103 <- ifelse(countrycode==1056 & qpp8_3 > 6 | qpp5_ches == 103 | qpp6_ches == 103 | qpp21_ches == 103, 1, NA)
EES_2014$lv_ches_107 <- ifelse(countrycode==1056 & qpp8_4 > 6 | qpp5_ches == 107 | qpp6_ches == 107 | qpp21_ches == 107, 1, NA)
EES_2014$lv_ches_110 <- ifelse(countrycode==1056 & qpp8_5 > 6 | qpp5_ches == 110 | qpp6_ches == 110 | qpp21_ches == 110, 1, NA)
EES_2014$lv_ches_105 <- ifelse(countrycode==1056 & qpp8_6 > 6 | qpp5_ches == 105 | qpp6_ches == 105 | qpp21_ches == 105, 1, NA)
EES_2014$lv_ches_112 <- ifelse(countrycode==1056 & qpp8_7 > 6 | qpp5_ches == 112 | qpp6_ches == 112 | qpp21_ches == 112, 1, NA)

#Germany
EES_2014$lv_ches_301 <- ifelse(countrycode==1276 & qpp8_1 > 6 | qpp5_ches == 301 | qpp6_ches == 301 | qpp21_ches == 301, 1, NA)
EES_2014$lv_ches_302 <- ifelse(countrycode==1276 & qpp8_2 > 6 | qpp5_ches == 302 | qpp6_ches == 302 | qpp21_ches == 302, 1, NA)
EES_2014$lv_ches_303 <- ifelse(countrycode==1276 & qpp8_3 > 6 | qpp5_ches == 303 | qpp6_ches == 303 | qpp21_ches == 303, 1, NA)
EES_2014$lv_ches_304 <- ifelse(countrycode==1276 & qpp8_4 > 6 | qpp5_ches == 304 | qpp6_ches == 304 | qpp21_ches == 304, 1, NA)
EES_2014$lv_ches_306 <- ifelse(countrycode==1276 & qpp8_5 > 6 | qpp5_ches == 306 | qpp6_ches == 306 | qpp21_ches == 306, 1, NA)
EES_2014$lv_ches_310 <- ifelse(countrycode==1276 & qpp8_6 > 6 | qpp5_ches == 310 | qpp6_ches == 310 | qpp21_ches == 310, 1, NA)
EES_2014$lv_ches_311 <- ifelse(countrycode==1276 & qpp8_7 > 6 | qpp5_ches == 311 | qpp6_ches == 311 | qpp21_ches == 311, 1, NA)

# Denmark
EES_2014$lv_ches_201 <- ifelse(countrycode==1208 & qpp8_1 > 6 | qpp5_ches == 201 | qpp6_ches == 201 | qpp21_ches == 201, 1, NA)
EES_2014$lv_ches_211 <- ifelse(countrycode==1208 & qpp8_2 > 6 | qpp5_ches == 211 | qpp6_ches == 211 | qpp21_ches == 211, 1, NA)
EES_2014$lv_ches_206 <- ifelse(countrycode==1208 & qpp8_3 > 6 | qpp5_ches == 206 | qpp6_ches == 206 | qpp21_ches == 206, 1, NA)
EES_2014$lv_ches_215 <- ifelse(countrycode==1208 & qpp8_4 > 6 | qpp5_ches == 215 | qpp6_ches == 215 | qpp21_ches == 215, 1, NA)
EES_2014$lv_ches_202 <- ifelse(countrycode==1208 & qpp8_5 > 6 | qpp5_ches == 202 | qpp6_ches == 202 | qpp21_ches == 202, 1, NA)
EES_2014$lv_ches_218 <- ifelse(countrycode==1208 & qpp8_6 > 6 | qpp5_ches == 218 | qpp6_ches == 218 | qpp21_ches == 218, 1, NA)
EES_2014$lv_ches_203 <- ifelse(countrycode==1208 & qpp8_7 > 6 | qpp5_ches == 203 | qpp6_ches == 203 | qpp21_ches == 203, 1, NA)
EES_2014$lv_ches_217 <- ifelse(countrycode==1208 & qpp8_8 > 6 | qpp5_ches == 217 | qpp6_ches == 217 | qpp21_ches == 217, 1, NA)

#Spain
EES_2014$lv_ches_502 <- ifelse(countrycode==1724 & qpp8_1 > 6 | qpp5_ches == 502 | qpp6_ches == 502 | qpp21_ches == 502, 1, NA)
EES_2014$lv_ches_501 <- ifelse(countrycode==1724 & qpp8_2 > 6 | qpp5_ches == 501 | qpp6_ches == 501 | qpp21_ches == 501, 1, NA)
EES_2014$lv_ches_504 <- ifelse(countrycode==1724 & qpp8_3 > 6 | qpp5_ches == 504 | qpp6_ches == 504 | qpp21_ches == 504, 1, NA)
EES_2014$lv_ches_523 <- ifelse(countrycode==1724 & qpp8_4 > 6 | qpp5_ches == 523 | qpp6_ches == 523 | qpp21_ches == 523, 1, NA)
EES_2014$lv_ches_505 <- ifelse(countrycode==1724 & qpp8_5 > 6 | qpp5_ches == 505 | qpp6_ches == 505 | qpp21_ches == 505, 1, NA) #is also 506 and 517
EES_2014$lv_ches_511 <- ifelse(countrycode==1724 & qpp8_6 > 6 | qpp5_ches == 511 | qpp6_ches == 511 | qpp21_ches == 511, 1, NA)
EES_2014$lv_ches_525 <- ifelse(countrycode==1724 & qpp8_7 > 6 | qpp5_ches == 526 | qpp6_ches == 526 | qpp21_ches == 526, 1, NA)
EES_2014$lv_ches_525 <- ifelse(countrycode==1724 & qpp8_8 > 6 | qpp5_ches == 525 | qpp6_ches == 525 | qpp21_ches == 525, 1, NA)

#Finland
EES_2014$lv_ches_1402 <- ifelse(countrycode==1246 & qpp8_1 > 6 | qpp5_ches == 1402 | qpp6_ches == 1402 | qpp21_ches == 1402, 1, NA)
EES_2014$lv_ches_1409 <- ifelse(countrycode==1246 & qpp8_2 > 6 | qpp5_ches == 1409 | qpp6_ches == 1409 | qpp21_ches == 1409, 1, NA)
EES_2014$lv_ches_1401 <- ifelse(countrycode==1246 & qpp8_3 > 6 | qpp5_ches == 1401 | qpp6_ches == 1401 | qpp21_ches == 1401, 1, NA)
EES_2014$lv_ches_1403 <- ifelse(countrycode==1246 & qpp8_4 > 6 | qpp5_ches == 1403 | qpp6_ches == 1403 | qpp21_ches == 1403, 1, NA)
EES_2014$lv_ches_1406 <- ifelse(countrycode==1246 & qpp8_5 > 6 | qpp5_ches == 1406 | qpp6_ches == 1406 | qpp21_ches == 1406, 1, NA)
EES_2014$lv_ches_1408 <- ifelse(countrycode==1246 & qpp8_6 > 6 | qpp5_ches == 1408 | qpp6_ches == 1408 | qpp21_ches == 1408, 1, NA)
EES_2014$lv_ches_1404 <- ifelse(countrycode==1246 & qpp8_7 > 6 | qpp5_ches == 1404 | qpp6_ches == 1404 | qpp21_ches == 1404, 1, NA)
EES_2014$lv_ches_1405 <- ifelse(countrycode==1246 & qpp8_8 > 6 | qpp5_ches == 1405 | qpp6_ches == 1405 | qpp21_ches == 1405, 1, NA)

#France
EES_2014$lv_ches_609 <- ifelse(countrycode==1250 & qpp8_1 > 6 | qpp5_ches == 609 | qpp6_ches == 609 | qpp21_ches == 609, 1, NA)
EES_2014$lv_ches_602 <- ifelse(countrycode==1250 & qpp8_2 > 6 | qpp5_ches == 602 | qpp6_ches == 602 | qpp21_ches == 602, 1, NA)
EES_2014$lv_ches_610 <- ifelse(countrycode==1250 & qpp8_3 > 6 | qpp5_ches == 610 | qpp6_ches == 610 | qpp21_ches == 610, 1, NA)
EES_2014$lv_ches_605 <- ifelse(countrycode==1250 & qpp8_4 > 6 | qpp5_ches == 605 | qpp6_ches == 605 | qpp21_ches == 605, 1, NA)
EES_2014$lv_ches_601 <- ifelse(countrycode==1250 & qpp8_5 > 6 | qpp5_ches == 601 | qpp6_ches == 601 | qpp21_ches == 601, 1, NA)
EES_2014$lv_ches_613 <- ifelse(countrycode==1250 & qpp8_6 > 6 | qpp5_ches == 613 | qpp6_ches == 613 | qpp21_ches == 613, 1, NA)

#Ireland
EES_2014$lv_ches_702 <- ifelse(countrycode==1372 & qpp8_1 > 6 | qpp5_ches == 702 | qpp6_ches == 702 | qpp21_ches == 702, 1, NA)
EES_2014$lv_ches_703 <- ifelse(countrycode==1372 & qpp8_2 > 6 | qpp5_ches == 703 | qpp6_ches == 703 | qpp21_ches == 703, 1, NA)
EES_2014$lv_ches_701 <- ifelse(countrycode==1372 & qpp8_3 > 6 | qpp5_ches == 701 | qpp6_ches == 701 | qpp21_ches == 701, 1, NA)
EES_2014$lv_ches_705 <- ifelse(countrycode==1372 & qpp8_4 > 6 | qpp5_ches == 705 | qpp6_ches == 705 | qpp21_ches == 705, 1, NA)
EES_2014$lv_ches_707 <- ifelse(countrycode==1372 & qpp8_5 > 6 | qpp5_ches == 707 | qpp6_ches == 707 | qpp21_ches == 707, 1, NA)
EES_2014$lv_ches_708 <- ifelse(countrycode==1372 & qpp8_6 > 6 | qpp5_ches == 708 | qpp6_ches == 708 | qpp21_ches == 708, 1, NA)

#Italy
EES_2014$lv_ches_837 <- ifelse(countrycode==1380 & qpp8_1 > 6 | qpp5_ches == 837 | qpp6_ches == 837 | qpp21_ches == 837, 1, NA)
EES_2014$lv_ches_815 <- ifelse(countrycode==1380 & qpp8_2 > 6 | qpp5_ches == 815 | qpp6_ches == 815 | qpp21_ches == 815, 1, NA)
EES_2014$lv_ches_811 <- ifelse(countrycode==1380 & qpp8_3 > 6 | qpp5_ches == 811 | qpp6_ches == 811 | qpp21_ches == 811, 1, NA)
EES_2014$lv_ches_845 <- ifelse(countrycode==1380 & qpp8_4 > 6 | qpp5_ches == 845 | qpp6_ches == 845 | qpp21_ches == 845, 1, NA)
EES_2014$lv_ches_814 <- ifelse(countrycode==1380 & qpp8_5 > 6 | qpp5_ches == 814 | qpp6_ches == 814 | qpp21_ches == 814, 1, NA)
EES_2014$lv_ches_838 <- ifelse(countrycode==1380 & qpp8_6 > 6 | qpp5_ches == 838 | qpp6_ches == 838 | qpp21_ches == 838, 1, NA)
EES_2014$lv_ches_848 <- ifelse(countrycode==1380 & qpp8_7 > 6 | qpp5_ches == 848 | qpp6_ches == 848 | qpp21_ches == 848, 1, NA)
EES_2014$lv_ches_844 <- ifelse(countrycode==1380 & qpp8_8 > 6 | qpp5_ches == 844 | qpp6_ches == 844 | qpp21_ches == 844, 1, NA)

#Luxembourg
EES_2014$lv_ches_3801 <- ifelse(countrycode==1442 & qpp8_1 > 6 | qpp5_ches == 3801 | qpp6_ches == 3801 | qpp21_ches == 3801, 1, NA)
EES_2014$lv_ches_3804 <- ifelse(countrycode==1442 & qpp8_2 > 6 | qpp5_ches == 3804 | qpp6_ches == 3804 | qpp21_ches == 3804, 1, NA)
EES_2014$lv_ches_3803 <- ifelse(countrycode==1442 & qpp8_3 > 6 | qpp5_ches == 3803 | qpp6_ches == 3803 | qpp21_ches == 3803, 1, NA)
EES_2014$lv_ches_3802 <- ifelse(countrycode==1442 & qpp8_4 > 6 | qpp5_ches == 3802 | qpp6_ches == 3802 | qpp21_ches == 3802, 1, NA)
EES_2014$lv_ches_3806 <- ifelse(countrycode==1442 & qpp8_5 > 6 | qpp5_ches == 3806 | qpp6_ches == 3806 | qpp21_ches == 3806, 1, NA)
EES_2014$lv_ches_3805 <- ifelse(countrycode==1442 & qpp8_6 > 6 | qpp5_ches == 3805 | qpp6_ches == 3805 | qpp21_ches == 3805, 1, NA)

#Netherlands
EES_2014$lv_ches_1003 <- ifelse(countrycode==1528 & qpp8_1 > 6 | qpp5_ches == 1003 | qpp6_ches == 1003 | qpp21_ches == 1003, 1, NA)
EES_2014$lv_ches_1002 <- ifelse(countrycode==1528 & qpp8_2 > 6 | qpp5_ches == 1002 | qpp6_ches == 1002 | qpp21_ches == 1002, 1, NA)
EES_2014$lv_ches_1017 <- ifelse(countrycode==1528 & qpp8_3 > 6 | qpp5_ches == 1017 | qpp6_ches == 1017 | qpp21_ches == 1017, 1, NA)
EES_2014$lv_ches_1014 <- ifelse(countrycode==1528 & qpp8_4 > 6 | qpp5_ches == 1014 | qpp6_ches == 1014 | qpp21_ches == 1014, 1, NA)
EES_2014$lv_ches_1001 <- ifelse(countrycode==1528 & qpp8_5 > 6 | qpp5_ches == 1001 | qpp6_ches == 1001 | qpp21_ches == 1001, 1, NA)
EES_2014$lv_ches_1004 <- ifelse(countrycode==1528 & qpp8_6 > 6 | qpp5_ches == 1004 | qpp6_ches == 1004 | qpp21_ches == 1004, 1, NA)
EES_2014$lv_ches_1016 <- ifelse(countrycode==1528 & qpp8_7 > 6 | qpp5_ches == 1016 | qpp6_ches == 1016 | qpp21_ches == 1016, 1, NA)
EES_2014$lv_ches_1005 <- ifelse(countrycode==1528 & qpp8_8 > 6 | qpp5_ches == 1005 | qpp6_ches == 1005 | qpp21_ches == 1005, 1, NA)

#Portugal
EES_2014$lv_ches_1206 <- ifelse(countrycode==1620 & qpp8_1 > 6 | qpp5_ches == 1206 | qpp6_ches == 1206 | qpp21_ches == 1206, 1, NA)
EES_2014$lv_ches_1202 <- ifelse(countrycode==1620 & qpp8_2 > 6 | qpp5_ches == 1202 | qpp6_ches == 1202 | qpp21_ches == 1202, 1, NA)
EES_2014$lv_ches_1205 <- ifelse(countrycode==1620 & qpp8_3 > 6 | qpp5_ches == 1205 | qpp6_ches == 1205 | qpp21_ches == 1205, 1, NA)
EES_2014$lv_ches_1201 <- ifelse(countrycode==1620 & qpp8_4 > 6 | qpp5_ches == 1201 | qpp6_ches == 1201 | qpp21_ches == 1201, 1, NA)
EES_2014$lv_ches_1208 <- ifelse(countrycode==1620 & qpp8_5 > 6 | qpp5_ches == 1208 | qpp6_ches == 1208 | qpp21_ches == 1208, 1, NA)
EES_2014$lv_ches_1209 <- ifelse(countrycode==1620 & qpp8_6 > 6 | qpp5_ches == 1209 | qpp6_ches == 1209 | qpp21_ches == 1209, 1, NA)

#Sweden
EES_2014$lv_ches_1602 <- ifelse(countrycode==1752 & qpp8_1 > 6 | qpp5_ches == 1602 | qpp6_ches == 1602 | qpp21_ches == 1602, 1, NA)
EES_2014$lv_ches_1605 <- ifelse(countrycode==1752 & qpp8_2 > 6 | qpp5_ches == 1605 | qpp6_ches == 1605 | qpp21_ches == 1605, 1, NA)
EES_2014$lv_ches_1607 <- ifelse(countrycode==1752 & qpp8_3 > 6 | qpp5_ches == 1607 | qpp6_ches == 1607 | qpp21_ches == 1607, 1, NA)
EES_2014$lv_ches_1604 <- ifelse(countrycode==1752 & qpp8_4 > 6 | qpp5_ches == 1604 | qpp6_ches == 1604 | qpp21_ches == 1604, 1, NA)
EES_2014$lv_ches_1603 <- ifelse(countrycode==1752 & qpp8_5 > 6 | qpp5_ches == 1603 | qpp6_ches == 1603 | qpp21_ches == 1603, 1, NA)
EES_2014$lv_ches_1610 <- ifelse(countrycode==1752 & qpp8_6 > 6 | qpp5_ches == 1610 | qpp6_ches == 1610 | qpp21_ches == 1610, 1, NA)
EES_2014$lv_ches_1606 <- ifelse(countrycode==1752 & qpp8_7 > 6 | qpp5_ches == 1606 | qpp6_ches == 1606 | qpp21_ches == 1606, 1, NA)
EES_2014$lv_ches_1601 <- ifelse(countrycode==1752 & qpp8_8 > 6 | qpp5_ches == 1601 | qpp6_ches == 1601 | qpp21_ches == 1601, 1, NA)


#UK
EES_2014$lv_ches_1101 <- ifelse(countrycode==1826 & qpp8_1 > 6 | qpp5_ches == 1101 | qpp6_ches == 1101 | qpp21_ches == 1101, 1, NA)
EES_2014$lv_ches_1102 <- ifelse(countrycode==1826 & qpp8_2 > 6 | qpp5_ches == 1102 | qpp6_ches == 1102 | qpp21_ches == 1102, 1, NA)
EES_2014$lv_ches_1104 <- ifelse(countrycode==1826 & qpp8_3 > 6 | qpp5_ches == 1104 | qpp6_ches == 1104 | qpp21_ches == 1104, 1, NA)
EES_2014$lv_ches_1107 <- ifelse(countrycode==1826 & qpp8_4 > 6 | qpp5_ches == 1107 | qpp6_ches == 1107 | qpp21_ches == 1107, 1, NA)
EES_2014$lv_ches_1108 <- ifelse(countrycode==1826 & qpp8_5 > 6 | qpp5_ches == 1108 | qpp6_ches == 1108 | qpp21_ches == 1108, 1, NA)
EES_2014$lv_ches_1105 <- ifelse(countrycode==1826 & qpp8_6 > 6 | qpp5_ches == 1105 | qpp6_ches == 1105 | qpp21_ches == 1105, 1, NA)
EES_2014$lv_ches_1106 <- ifelse(countrycode==1826 & qpp8_7 > 6 | qpp5_ches == 1106 | qpp6_ches == 1106 | qpp21_ches == 1106, 1, NA)








#############Prepare variables of interest (postitions on globalization issues: glob_pos)#################

#see the labels
get_label(EES_2014$qpp18)
get_labels(EES_2014$qpp17_6)
#get_labels(EES_2014$qpp17_7) #dropped because not in earlier EES rounds. 

#remove NAs
EES_2014$qpp18[EES_2014$qpp18 == -99 | EES_2014$qpp18 == -9 | EES_2014$qpp18  == -8 | EES_2014$qpp18 == -7] <- NA
EES_2014$qpp17_6[EES_2014$qpp8_2 == -99 | EES_2014$qpp17_6 == -9 | EES_2014$qpp17_6  == -8 | EES_2014$qpp17_6 == -7] <- NA
EES_2014$qpp17_7[EES_2014$qpp8_3 == -99 | EES_2014$qpp17_7 == -9 | EES_2014$qpp17_7  == -8 | EES_2014$qpp17_7 == -7] <- NA

#reverse order of 17_7 to align with other questions
class(qpp17_7)
library(car)
EES_2014$qpp17_7_rev <- EES_2014$qpp17_7
EES_2014$qpp17_7_rev <- as.numeric(EES_2014$qpp17_7_rev)
EES_2014$qpp17_7_rev <- car::recode(EES_2014$qpp17_7_rev, "11=1; 10=2; 9=3; 8=4; 7=5; 6=6; 5=7; 4=8; 3=9; 2=10; 1=11")
Hmisc::describe(EES_2014$qpp17_7_rev) #check if worked
detach("package:car", unload=TRUE) #only needed for recode function

#Create the new variable combine Q 18 (EU unification), 17_7 (EU integration: authority membersyyaye budget), and 17_6 (Immigration)
x <- data.frame(EES_2014$qpp17_6, EES_2014$qpp18)
library(psych)
psych::alpha(x) #using psych package to get cronbach alpha score.  0.39 so not high enough.. but no problem: SD still valid and for extreme scores I can look at individual variables. am not looking at voter positions or congruence so all good!
detach("package:psych", unload=TRUE) #detach because clashes with misc

#merge variables
EES_2014$glob_pos <- NA
EES_2014$glob_pos <- (as.numeric(EES_2014$qpp17_6) + as.numeric(EES_2014$qpp18))  / 2 #creating new variable.
Hmisc::describe(EES_2014$glob_pos) #seems all good














#############Globalization dimension: SD likely voters#########

##using SD and not var or MAD: does the least with variables. Apply to glob dimension: cause easiest to calculate, plus closer to what parties do (and differences bigger). Plus, important, cause it takes into consideration if voters all think 10 on one and 1 on the other
#for using MAD
# library(DescTools) #using this function to get MAD https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/MeanAD
#MeanAD(glob_pos, na.rm = TRUE)
#MeanAD(glob_pos[lv_ches_1017 == 1], na.rm = TRUE)  #SD for the PVV

###Check whether everything works with a country I know
attach(EES_2014)
mean(glob_pos[lv_ches_1001 == 1], na.rm = TRUE) #CDA
mean(glob_pos[lv_ches_1002 == 1], na.rm = TRUE) #PvdA
mean(glob_pos[lv_ches_1003 == 1], na.rm = TRUE) #VVD
mean(glob_pos[lv_ches_1004 == 1], na.rm = TRUE) #D66
mean(glob_pos[lv_ches_1005 == 1], na.rm = TRUE) #GL
mean(glob_pos[lv_ches_1017 == 1], na.rm = TRUE) #PVV

sd(glob_pos[lv_ches_1001 == 1], na.rm = TRUE) #CDA
sd(glob_pos[lv_ches_1002 == 1], na.rm = TRUE) #PvdA
sd(glob_pos[lv_ches_1003 == 1], na.rm = TRUE) #VVD
sd(glob_pos[lv_ches_1004 == 1], na.rm = TRUE) #D66
sd(glob_pos[lv_ches_1005 == 1], na.rm = TRUE) #GL
sd(glob_pos[lv_ches_1017 == 1], na.rm = TRUE) #PVV


#SD of voters on glob dimension. 
party_vot_var <- data.frame(party_ches = unique_parties, year = 2014) #really important that the unique parties stays in the right order! See bottom file for what used now. 
party_vot_var$sd_glob_lv <- c(sd(glob_pos[lv_ches_102 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_103 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_104 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_105 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_106 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_107 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_108 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_109 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_110 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_112 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_119 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_201 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_202 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_203 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_206 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_211 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_215 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_218 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_301 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_302 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_303 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_304 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_306 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_310 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_311 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_401 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_402 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_403 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_404 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_410 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_413 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_414 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_415 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_501 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_502 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_504 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_505 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_506 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_511 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_513 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_517 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_523 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_525 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_601 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_602 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_605 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_609 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_610 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_613 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_701 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_702 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_703 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_705 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_707 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_708 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_811 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_814 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_815 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_827 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_837 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_838 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_844 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_845 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_848 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1001 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1002 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1003 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1004 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1005 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1014 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1016 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1017 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1018 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1101 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1102 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1104 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1105 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1106 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1107 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1108 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1201 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1202 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1205 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1206 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1208 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1209 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1302 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1303 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1304 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1307 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1309 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1401 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1402 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1403 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1404 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1405 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1406 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1408 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1409 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1601 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1602 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1603 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1604 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1605 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1606 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1607 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1610 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1611 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_1612 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2002 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2004 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2007 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2010 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2015 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2016 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2101 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2103 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2104 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2109 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2111 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2113 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2201 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2202 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2203 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2204 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2207 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2301 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2302 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2308 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2309 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2310 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2311 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2402 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2405 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2406 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2410 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2412 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2501 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2506 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2507 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2511 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2515 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2516 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2518 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2601 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2603 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2605 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2606 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2613 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2614 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2616 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2701 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2702 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2704 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2705 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2706 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2710 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2711 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2802 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2803 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2804 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2805 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2809 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2812 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2813 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2814 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2815 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2902 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2903 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2904 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2905 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2906 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_2914 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3101 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3102 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3104 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3105 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3107 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3109 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3112 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3114 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3701 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3702 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3801 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3802 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3803 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3804 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3805 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_3806 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_4001 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_4003 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_4004 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_4005 == 1], na.rm = TRUE),
                              sd(glob_pos[lv_ches_4006 == 1], na.rm = TRUE))




#############Globalization dimension: MAD############

#for using MAD
library(DescTools) #using this function to get MAD https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/MeanAD
MeanAD(glob_pos, na.rm = TRUE)
MeanAD(glob_pos[lv_ches_1017 == 1], na.rm = TRUE)  #SD for the PVV

#MAD of voters on glob dimension. 
party_vot_var$mad_glob_lv <- c(MeanAD(glob_pos[lv_ches_102 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_103 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_104 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_105 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_106 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_107 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_108 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_109 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_110 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_112 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_119 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_201 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_202 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_203 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_206 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_211 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_215 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_218 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_301 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_302 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_303 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_304 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_306 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_310 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_311 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_401 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_402 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_403 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_404 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_410 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_413 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_414 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_415 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_501 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_502 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_504 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_505 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_506 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_511 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_513 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_517 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_523 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_525 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_601 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_602 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_605 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_609 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_610 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_613 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_701 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_702 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_703 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_705 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_707 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_708 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_811 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_814 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_815 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_827 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_837 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_838 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_844 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_845 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_848 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1001 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1002 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1003 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1004 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1005 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1014 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1016 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1017 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1018 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1101 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1102 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1104 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1105 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1106 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1107 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1108 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1201 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1202 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1205 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1206 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1208 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1209 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1302 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1303 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1304 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1307 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1309 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1401 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1402 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1403 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1404 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1405 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1406 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1408 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1409 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1601 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1602 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1603 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1604 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1605 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1606 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1607 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1610 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1611 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_1612 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2002 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2004 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2007 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2010 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2015 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2016 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2101 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2103 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2104 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2109 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2111 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2113 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2201 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2202 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2203 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2204 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2207 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2301 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2302 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2308 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2309 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2310 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2311 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2402 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2405 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2406 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2410 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2412 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2501 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2506 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2507 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2511 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2515 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2516 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2518 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2601 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2603 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2605 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2606 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2613 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2614 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2616 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2701 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2702 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2704 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2705 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2706 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2710 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2711 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2802 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2803 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2804 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2805 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2809 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2812 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2813 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2814 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2815 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2902 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2903 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2904 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2905 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2906 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_2914 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3101 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3102 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3104 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3105 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3107 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3109 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3112 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3114 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3701 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3702 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3801 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3802 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3803 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3804 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3805 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_3806 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_4001 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_4003 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_4004 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_4005 == 1], na.rm = TRUE),
                               MeanAD(glob_pos[lv_ches_4006 == 1], na.rm = TRUE))

detach("package:DescTools", unload=TRUE)

party_vot_var$mad_glob_lv <- ifelse(is.nan(party_vot_var$mad_glob_lv) == TRUE, NA, party_vot_var$mad_glob_lv)




#############Globalization dimension: mean positions for likely voters#############
party_vot_var$mean_glob_lv <- c(mean(glob_pos[lv_ches_102 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_103 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_104 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_105 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_106 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_107 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_108 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_109 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_110 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_112 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_119 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_201 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_202 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_203 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_206 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_211 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_215 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_218 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_301 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_302 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_303 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_304 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_306 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_310 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_311 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_401 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_402 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_403 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_404 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_410 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_413 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_414 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_415 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_501 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_502 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_504 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_505 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_506 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_511 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_513 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_517 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_523 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_525 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_601 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_602 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_605 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_609 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_610 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_613 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_701 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_702 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_703 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_705 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_707 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_708 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_811 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_814 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_815 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_827 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_837 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_838 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_844 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_845 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_848 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1001 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1002 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1003 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1004 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1005 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1014 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1016 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1017 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1018 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1101 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1102 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1104 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1105 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1106 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1107 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1108 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1201 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1202 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1205 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1206 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1208 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1209 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1302 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1303 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1304 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1307 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1309 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1401 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1402 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1403 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1404 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1405 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1406 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1408 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1409 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1601 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1602 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1603 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1604 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1605 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1606 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1607 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1610 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1611 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_1612 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2002 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2004 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2007 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2010 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2015 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2016 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2101 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2103 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2104 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2109 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2111 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2113 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2201 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2202 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2203 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2204 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2207 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2301 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2302 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2308 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2309 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2310 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2311 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2402 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2405 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2406 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2410 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2412 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2501 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2506 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2507 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2511 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2515 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2516 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2518 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2601 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2603 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2605 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2606 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2613 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2614 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2616 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2701 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2702 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2704 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2705 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2706 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2710 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2711 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2802 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2803 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2804 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2805 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2809 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2812 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2813 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2814 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2815 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2902 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2903 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2904 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2905 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2906 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_2914 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3101 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3102 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3104 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3105 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3107 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3109 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3112 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3114 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3701 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3702 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3801 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3802 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3803 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3804 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3805 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_3806 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_4001 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_4003 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_4004 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_4005 == 1], na.rm = TRUE),
                                mean(glob_pos[lv_ches_4006 == 1], na.rm = TRUE))

party_vot_var$mean_glob_lv <- ifelse(is.nan(party_vot_var$mean_glob_lv) == TRUE, NA, party_vot_var$mean_glob_lv)










#############Globalization dimension: percentage extreme voters##############################


attach(EES_2014)

#Function for percentage extreme. 
#take the sum of all not NA values for a question. This is the total n for that question. Then take all extreme values
#for that same question. Do this three times for both and calculate percentage extreme scores.
#note that not all people answering all three questions is taken into account by taking separate Ns for each question
#the lvches is the name for columns with dummies for likely voters. 
per_ext_voters <- function(lvches)
{
  n1 = sum(!is.na((as.numeric(qpp17_6[lvches == 1]))))
  t1 = sum(!is.na((as.numeric(qpp17_6[lvches == 1 & (qpp17_6 == 11 | qpp17_6 == 1) ]))))
  n3 = sum(!is.na((as.numeric(qpp18[lvches == 1]))))
  t3 = sum(!is.na((as.numeric(qpp18[lvches == 1 & (qpp18 == 11 | qpp18 == 1) ]))))
  pext <- (t1 + t3)/(n1 + n3) * 100
  return(pext)
}


####################Adding the code for all parties. Note that, just like above, all NAs are for Eastern European countries
party_vot_var$perext_glob_lv <- c(per_ext_voters(lv_ches_102),
                                  per_ext_voters(lv_ches_103),
                                  per_ext_voters(lv_ches_104),
                                  per_ext_voters(lv_ches_105),
                                  per_ext_voters(lv_ches_106),
                                  per_ext_voters(lv_ches_107),
                                  per_ext_voters(lv_ches_108),
                                  per_ext_voters(lv_ches_109),
                                  per_ext_voters(lv_ches_110),
                                  per_ext_voters(lv_ches_112),
                                  per_ext_voters(lv_ches_119),
                                  per_ext_voters(lv_ches_201),
                                  per_ext_voters(lv_ches_202),
                                  per_ext_voters(lv_ches_203),
                                  per_ext_voters(lv_ches_206),
                                  per_ext_voters(lv_ches_211),
                                  per_ext_voters(lv_ches_215),
                                  per_ext_voters(lv_ches_218),
                                  per_ext_voters(lv_ches_301),
                                  per_ext_voters(lv_ches_302),
                                  per_ext_voters(lv_ches_303),
                                  per_ext_voters(lv_ches_304),
                                  per_ext_voters(lv_ches_306),
                                  per_ext_voters(lv_ches_310),
                                  per_ext_voters(lv_ches_311),
                                  per_ext_voters(lv_ches_401),
                                  per_ext_voters(lv_ches_402),
                                  per_ext_voters(lv_ches_403),
                                  per_ext_voters(lv_ches_404),
                                  per_ext_voters(lv_ches_410),
                                  per_ext_voters(lv_ches_413),
                                  per_ext_voters(lv_ches_414),
                                  per_ext_voters(lv_ches_415),
                                  per_ext_voters(lv_ches_501),
                                  per_ext_voters(lv_ches_502),
                                  per_ext_voters(lv_ches_504),
                                  per_ext_voters(lv_ches_505),
                                  per_ext_voters(lv_ches_506),
                                  per_ext_voters(lv_ches_511),
                                  per_ext_voters(lv_ches_513),
                                  per_ext_voters(lv_ches_517),
                                  per_ext_voters(lv_ches_523),
                                  per_ext_voters(lv_ches_525),
                                  per_ext_voters(lv_ches_601),
                                  per_ext_voters(lv_ches_602),
                                  per_ext_voters(lv_ches_605),
                                  per_ext_voters(lv_ches_609),
                                  per_ext_voters(lv_ches_610),
                                  per_ext_voters(lv_ches_613),
                                  per_ext_voters(lv_ches_701),
                                  per_ext_voters(lv_ches_702),
                                  per_ext_voters(lv_ches_703),
                                  per_ext_voters(lv_ches_705),
                                  per_ext_voters(lv_ches_707),
                                  per_ext_voters(lv_ches_708),
                                  per_ext_voters(lv_ches_811),
                                  per_ext_voters(lv_ches_814),
                                  per_ext_voters(lv_ches_815),
                                  per_ext_voters(lv_ches_827),
                                  per_ext_voters(lv_ches_837),
                                  per_ext_voters(lv_ches_838),
                                  per_ext_voters(lv_ches_844),
                                  per_ext_voters(lv_ches_845),
                                  per_ext_voters(lv_ches_848),
                                  per_ext_voters(lv_ches_1001),
                                  per_ext_voters(lv_ches_1002),
                                  per_ext_voters(lv_ches_1003),
                                  per_ext_voters(lv_ches_1004),
                                  per_ext_voters(lv_ches_1005),
                                  per_ext_voters(lv_ches_1014),
                                  per_ext_voters(lv_ches_1016),
                                  per_ext_voters(lv_ches_1017),
                                  per_ext_voters(lv_ches_1018),
                                  per_ext_voters(lv_ches_1101),
                                  per_ext_voters(lv_ches_1102),
                                  per_ext_voters(lv_ches_1104),
                                  per_ext_voters(lv_ches_1105),
                                  per_ext_voters(lv_ches_1106),
                                  per_ext_voters(lv_ches_1107),
                                  per_ext_voters(lv_ches_1108),
                                  per_ext_voters(lv_ches_1201),
                                  per_ext_voters(lv_ches_1202),
                                  per_ext_voters(lv_ches_1205),
                                  per_ext_voters(lv_ches_1206),
                                  per_ext_voters(lv_ches_1208),
                                  per_ext_voters(lv_ches_1209),
                                  per_ext_voters(lv_ches_1302),
                                  per_ext_voters(lv_ches_1303),
                                  per_ext_voters(lv_ches_1304),
                                  per_ext_voters(lv_ches_1307),
                                  per_ext_voters(lv_ches_1309),
                                  per_ext_voters(lv_ches_1401),
                                  per_ext_voters(lv_ches_1402),
                                  per_ext_voters(lv_ches_1403),
                                  per_ext_voters(lv_ches_1404),
                                  per_ext_voters(lv_ches_1405),
                                  per_ext_voters(lv_ches_1406),
                                  per_ext_voters(lv_ches_1408),
                                  per_ext_voters(lv_ches_1409),
                                  per_ext_voters(lv_ches_1601),
                                  per_ext_voters(lv_ches_1602),
                                  per_ext_voters(lv_ches_1603),
                                  per_ext_voters(lv_ches_1604),
                                  per_ext_voters(lv_ches_1605),
                                  per_ext_voters(lv_ches_1606),
                                  per_ext_voters(lv_ches_1607),
                                  per_ext_voters(lv_ches_1610),
                                  per_ext_voters(lv_ches_1611),
                                  per_ext_voters(lv_ches_1612),
                                  per_ext_voters(lv_ches_2002),
                                  per_ext_voters(lv_ches_2004),
                                  per_ext_voters(lv_ches_2007),
                                  per_ext_voters(lv_ches_2010),
                                  per_ext_voters(lv_ches_2015),
                                  per_ext_voters(lv_ches_2016),
                                  per_ext_voters(lv_ches_2101),
                                  per_ext_voters(lv_ches_2103),
                                  per_ext_voters(lv_ches_2104),
                                  per_ext_voters(lv_ches_2109),
                                  per_ext_voters(lv_ches_2111),
                                  per_ext_voters(lv_ches_2113),
                                  per_ext_voters(lv_ches_2201),
                                  per_ext_voters(lv_ches_2202),
                                  per_ext_voters(lv_ches_2203),
                                  per_ext_voters(lv_ches_2204),
                                  per_ext_voters(lv_ches_2207),
                                  per_ext_voters(lv_ches_2301),
                                  per_ext_voters(lv_ches_2302),
                                  per_ext_voters(lv_ches_2308),
                                  per_ext_voters(lv_ches_2309),
                                  per_ext_voters(lv_ches_2310),
                                  per_ext_voters(lv_ches_2311),
                                  per_ext_voters(lv_ches_2402),
                                  per_ext_voters(lv_ches_2405),
                                  per_ext_voters(lv_ches_2406),
                                  per_ext_voters(lv_ches_2410),
                                  per_ext_voters(lv_ches_2412),
                                  per_ext_voters(lv_ches_2501),
                                  per_ext_voters(lv_ches_2506),
                                  per_ext_voters(lv_ches_2507),
                                  per_ext_voters(lv_ches_2511),
                                  per_ext_voters(lv_ches_2515),
                                  per_ext_voters(lv_ches_2516),
                                  per_ext_voters(lv_ches_2518),
                                  per_ext_voters(lv_ches_2601),
                                  per_ext_voters(lv_ches_2603),
                                  per_ext_voters(lv_ches_2605),
                                  per_ext_voters(lv_ches_2606),
                                  per_ext_voters(lv_ches_2613),
                                  per_ext_voters(lv_ches_2614),
                                  per_ext_voters(lv_ches_2616),
                                  per_ext_voters(lv_ches_2701),
                                  per_ext_voters(lv_ches_2702),
                                  per_ext_voters(lv_ches_2704),
                                  per_ext_voters(lv_ches_2705),
                                  per_ext_voters(lv_ches_2706),
                                  per_ext_voters(lv_ches_2710),
                                  per_ext_voters(lv_ches_2711),
                                  per_ext_voters(lv_ches_2802),
                                  per_ext_voters(lv_ches_2803),
                                  per_ext_voters(lv_ches_2804),
                                  per_ext_voters(lv_ches_2805),
                                  per_ext_voters(lv_ches_2809),
                                  per_ext_voters(lv_ches_2812),
                                  per_ext_voters(lv_ches_2813),
                                  per_ext_voters(lv_ches_2814),
                                  per_ext_voters(lv_ches_2815),
                                  per_ext_voters(lv_ches_2902),
                                  per_ext_voters(lv_ches_2903),
                                  per_ext_voters(lv_ches_2904),
                                  per_ext_voters(lv_ches_2905),
                                  per_ext_voters(lv_ches_2906),
                                  per_ext_voters(lv_ches_2914),
                                  per_ext_voters(lv_ches_3101),
                                  per_ext_voters(lv_ches_3102),
                                  per_ext_voters(lv_ches_3104),
                                  per_ext_voters(lv_ches_3105),
                                  per_ext_voters(lv_ches_3107),
                                  per_ext_voters(lv_ches_3109),
                                  per_ext_voters(lv_ches_3112),
                                  per_ext_voters(lv_ches_3114),
                                  per_ext_voters(lv_ches_3701),
                                  per_ext_voters(lv_ches_3702),
                                  per_ext_voters(lv_ches_3801),
                                  per_ext_voters(lv_ches_3802),
                                  per_ext_voters(lv_ches_3803),
                                  per_ext_voters(lv_ches_3804),
                                  per_ext_voters(lv_ches_3805),
                                  per_ext_voters(lv_ches_3806),
                                  per_ext_voters(lv_ches_4001),
                                  per_ext_voters(lv_ches_4003),
                                  per_ext_voters(lv_ches_4004),
                                  per_ext_voters(lv_ches_4005),
                                  per_ext_voters(lv_ches_4006))



party_vot_var$perext_glob_lv <- ifelse(is.nan(party_vot_var$perext_glob_lv) == TRUE, NA, party_vot_var$perext_glob_lv)










#############Globalization dimension: Average distance from country mean##########################################
##Can function as an add-on to percentage of extreme scores. 


library(DescTools) #using this function to get MAD https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/MeanAD

party_mad <- function(party, var, country) #MAD for likely voters of a party, on a variable, in a country
{
  country_mean <-  function(var, country) #country_mean calculates the mean on a variable in a country
  {
    x <- mean(var[countrycode == country], na.rm=TRUE)
    return(x)
  }
  y <- MeanAD(var[party == 1], FUN=country_mean(var, country), na.rm=TRUE) #MeanAD calculates the mean distance on a variable for likely voters in a country where the midpoint is the return from above, i.e. country mean. 
  return(y) 
}

party_mad(lv_ches_1017, glob_pos, 1528) #PVV far away from country mean
party_mad(lv_ches_1003, glob_pos, 1056) #VVD closer to country mean


party_vot_var$mad_glob_lv_countrymean <- c(party_mad(lv_ches_102, glob_pos, 1056),
                                           party_mad(lv_ches_103, glob_pos, 1056),
                                           party_mad(lv_ches_104, glob_pos, 1056),
                                           party_mad(lv_ches_105, glob_pos, 1056),
                                           party_mad(lv_ches_106, glob_pos, 1056),
                                           party_mad(lv_ches_107, glob_pos, 1056),
                                           party_mad(lv_ches_108, glob_pos, 1056),
                                           party_mad(lv_ches_109, glob_pos, 1056),
                                           party_mad(lv_ches_110, glob_pos, 1056),
                                           party_mad(lv_ches_112, glob_pos, 1056),
                                           party_mad(lv_ches_119, glob_pos, 1056),
                                           party_mad(lv_ches_201, glob_pos, 1208),
                                           party_mad(lv_ches_202, glob_pos, 1208),
                                           party_mad(lv_ches_203, glob_pos, 1208),
                                           party_mad(lv_ches_206, glob_pos, 1208),
                                           party_mad(lv_ches_211, glob_pos, 1208),
                                           party_mad(lv_ches_215, glob_pos, 1208),
                                           party_mad(lv_ches_218, glob_pos, 1208),
                                           party_mad(lv_ches_301, glob_pos, 1276),
                                           party_mad(lv_ches_302, glob_pos, 1276),
                                           party_mad(lv_ches_303, glob_pos, 1276),
                                           party_mad(lv_ches_304, glob_pos, 1276),
                                           party_mad(lv_ches_306, glob_pos, 1276),
                                           party_mad(lv_ches_310, glob_pos, 1276),
                                           party_mad(lv_ches_311, glob_pos, 1276),
                                           party_mad(lv_ches_401, glob_pos, 1300),
                                           party_mad(lv_ches_402, glob_pos, 1300),
                                           party_mad(lv_ches_403, glob_pos, 1300),
                                           party_mad(lv_ches_404, glob_pos, 1300),
                                           party_mad(lv_ches_410, glob_pos, 1300),
                                           party_mad(lv_ches_413, glob_pos, 1300),
                                           party_mad(lv_ches_414, glob_pos, 1300),
                                           party_mad(lv_ches_415, glob_pos, 1300),
                                           party_mad(lv_ches_501, glob_pos, 1724),
                                           party_mad(lv_ches_502, glob_pos, 1724),
                                           party_mad(lv_ches_504, glob_pos, 1724),
                                           party_mad(lv_ches_505, glob_pos, 1724),
                                           party_mad(lv_ches_506, glob_pos, 1724),
                                           party_mad(lv_ches_511, glob_pos, 1724),
                                           party_mad(lv_ches_513, glob_pos, 1724),
                                           party_mad(lv_ches_517, glob_pos, 1724),
                                           party_mad(lv_ches_523, glob_pos, 1724),
                                           party_mad(lv_ches_525, glob_pos, 1724),
                                           party_mad(lv_ches_601, glob_pos, 1250),
                                           party_mad(lv_ches_602, glob_pos, 1250),
                                           party_mad(lv_ches_605, glob_pos, 1250),
                                           party_mad(lv_ches_609, glob_pos, 1250),
                                           party_mad(lv_ches_610, glob_pos, 1250),
                                           party_mad(lv_ches_613, glob_pos, 1250),
                                           party_mad(lv_ches_701, glob_pos, 1372),
                                           party_mad(lv_ches_702, glob_pos, 1372),
                                           party_mad(lv_ches_703, glob_pos, 1372),
                                           party_mad(lv_ches_705, glob_pos, 1372),
                                           party_mad(lv_ches_707, glob_pos, 1372),
                                           party_mad(lv_ches_708, glob_pos, 1372),
                                           party_mad(lv_ches_811, glob_pos, 1380),
                                           party_mad(lv_ches_814, glob_pos, 1380),
                                           party_mad(lv_ches_815, glob_pos, 1380),
                                           party_mad(lv_ches_827, glob_pos, 1380),
                                           party_mad(lv_ches_837, glob_pos, 1380),
                                           party_mad(lv_ches_838, glob_pos, 1380),
                                           party_mad(lv_ches_844, glob_pos, 1380),
                                           party_mad(lv_ches_845, glob_pos, 1380),
                                           party_mad(lv_ches_848, glob_pos, 1380),
                                           party_mad(lv_ches_1001, glob_pos, 1528),
                                           party_mad(lv_ches_1002, glob_pos, 1528),
                                           party_mad(lv_ches_1003, glob_pos, 1528),
                                           party_mad(lv_ches_1004, glob_pos, 1528),
                                           party_mad(lv_ches_1005, glob_pos, 1528),
                                           party_mad(lv_ches_1014, glob_pos, 1528),
                                           party_mad(lv_ches_1016, glob_pos, 1528),
                                           party_mad(lv_ches_1017, glob_pos, 1528),
                                           party_mad(lv_ches_1018, glob_pos, 1528),
                                           party_mad(lv_ches_1101, glob_pos, 1826),
                                           party_mad(lv_ches_1102, glob_pos, 1826),
                                           party_mad(lv_ches_1104, glob_pos, 1826),
                                           party_mad(lv_ches_1105, glob_pos, 1826),
                                           party_mad(lv_ches_1106, glob_pos, 1826),
                                           party_mad(lv_ches_1107, glob_pos, 1826),
                                           party_mad(lv_ches_1108, glob_pos, 1826),
                                           party_mad(lv_ches_1201, glob_pos, 1620),
                                           party_mad(lv_ches_1202, glob_pos, 1620),
                                           party_mad(lv_ches_1205, glob_pos, 1620),
                                           party_mad(lv_ches_1206, glob_pos, 1620),
                                           party_mad(lv_ches_1208, glob_pos, 1620),
                                           party_mad(lv_ches_1209, glob_pos, 1620),
                                           party_mad(lv_ches_1302, glob_pos, 1040),
                                           party_mad(lv_ches_1303, glob_pos, 1040),
                                           party_mad(lv_ches_1304, glob_pos, 1040),
                                           party_mad(lv_ches_1307, glob_pos, 1040),
                                           party_mad(lv_ches_1309, glob_pos, 1040),
                                           party_mad(lv_ches_1401, glob_pos, 1246),
                                           party_mad(lv_ches_1402, glob_pos, 1246),
                                           party_mad(lv_ches_1403, glob_pos, 1246),
                                           party_mad(lv_ches_1404, glob_pos, 1246),
                                           party_mad(lv_ches_1405, glob_pos, 1246),
                                           party_mad(lv_ches_1406, glob_pos, 1246),
                                           party_mad(lv_ches_1408, glob_pos, 1246),
                                           party_mad(lv_ches_1409, glob_pos, 1246),
                                           party_mad(lv_ches_1601, glob_pos, 1752),
                                           party_mad(lv_ches_1602, glob_pos, 1752),
                                           party_mad(lv_ches_1603, glob_pos, 1752),
                                           party_mad(lv_ches_1604, glob_pos, 1752),
                                           party_mad(lv_ches_1605, glob_pos, 1752),
                                           party_mad(lv_ches_1606, glob_pos, 1752),
                                           party_mad(lv_ches_1607, glob_pos, 1752),
                                           party_mad(lv_ches_1610, glob_pos, 1752),
                                           party_mad(lv_ches_1611, glob_pos, 1752),
                                           party_mad(lv_ches_1612, glob_pos, 1752),
                                           party_mad(lv_ches_2002, glob_pos, 1100),
                                           party_mad(lv_ches_2004, glob_pos, 1100),
                                           party_mad(lv_ches_2007, glob_pos, 1100),
                                           party_mad(lv_ches_2010, glob_pos, 1100),
                                           party_mad(lv_ches_2015, glob_pos, 1100),
                                           party_mad(lv_ches_2016, glob_pos, 1100),
                                           party_mad(lv_ches_2101, glob_pos, 1203),
                                           party_mad(lv_ches_2103, glob_pos, 1203),
                                           party_mad(lv_ches_2104, glob_pos, 1203),
                                           party_mad(lv_ches_2109, glob_pos, 1203),
                                           party_mad(lv_ches_2111, glob_pos, 1203),
                                           party_mad(lv_ches_2113, glob_pos, 1203),
                                           party_mad(lv_ches_2201, glob_pos, 1233),
                                           party_mad(lv_ches_2202, glob_pos, 1233),
                                           party_mad(lv_ches_2203, glob_pos, 1233),
                                           party_mad(lv_ches_2204, glob_pos, 1233),
                                           party_mad(lv_ches_2207, glob_pos, 1233),
                                           party_mad(lv_ches_2301, glob_pos, 1348),
                                           party_mad(lv_ches_2302, glob_pos, 1348),
                                           party_mad(lv_ches_2308, glob_pos, 1348),
                                           party_mad(lv_ches_2309, glob_pos, 1348),
                                           party_mad(lv_ches_2310, glob_pos, 1348),
                                           party_mad(lv_ches_2311, glob_pos, 1348),
                                           party_mad(lv_ches_2402, glob_pos, 1428),
                                           party_mad(lv_ches_2405, glob_pos, 1428),
                                           party_mad(lv_ches_2406, glob_pos, 1428),
                                           party_mad(lv_ches_2410, glob_pos, 1428),
                                           party_mad(lv_ches_2412, glob_pos, 1428),
                                           party_mad(lv_ches_2501, glob_pos, 1440),
                                           party_mad(lv_ches_2506, glob_pos, 1440),
                                           party_mad(lv_ches_2507, glob_pos, 1440),
                                           party_mad(lv_ches_2511, glob_pos, 1440),
                                           party_mad(lv_ches_2515, glob_pos, 1440),
                                           party_mad(lv_ches_2516, glob_pos, 1440),
                                           party_mad(lv_ches_2518, glob_pos, 1440),
                                           party_mad(lv_ches_2601, glob_pos, 1616),
                                           party_mad(lv_ches_2603, glob_pos, 1616),
                                           party_mad(lv_ches_2605, glob_pos, 1616),
                                           party_mad(lv_ches_2606, glob_pos, 1616),
                                           party_mad(lv_ches_2613, glob_pos, 1616),
                                           party_mad(lv_ches_2614, glob_pos, 1616),
                                           party_mad(lv_ches_2616, glob_pos, 1616),
                                           party_mad(lv_ches_2701, glob_pos, 1642),
                                           party_mad(lv_ches_2702, glob_pos, 1642),
                                           party_mad(lv_ches_2704, glob_pos, 1642),
                                           party_mad(lv_ches_2705, glob_pos, 1642),
                                           party_mad(lv_ches_2706, glob_pos, 1642),
                                           party_mad(lv_ches_2710, glob_pos, 1642),
                                           party_mad(lv_ches_2711, glob_pos, 1642),
                                           party_mad(lv_ches_2802, glob_pos, 1703),
                                           party_mad(lv_ches_2803, glob_pos, 1703),
                                           party_mad(lv_ches_2804, glob_pos, 1703),
                                           party_mad(lv_ches_2805, glob_pos, 1703),
                                           party_mad(lv_ches_2809, glob_pos, 1703),
                                           party_mad(lv_ches_2812, glob_pos, 1703),
                                           party_mad(lv_ches_2813, glob_pos, 1703),
                                           party_mad(lv_ches_2814, glob_pos, 1703),
                                           party_mad(lv_ches_2815, glob_pos, 1703),
                                           party_mad(lv_ches_2902, glob_pos, 1705),
                                           party_mad(lv_ches_2903, glob_pos, 1705),
                                           party_mad(lv_ches_2904, glob_pos, 1705),
                                           party_mad(lv_ches_2905, glob_pos, 1705),
                                           party_mad(lv_ches_2906, glob_pos, 1705),
                                           party_mad(lv_ches_2914, glob_pos, 1705),
                                           party_mad(lv_ches_3101, glob_pos, 1191),
                                           party_mad(lv_ches_3102, glob_pos, 1191),
                                           party_mad(lv_ches_3104, glob_pos, 1191),
                                           party_mad(lv_ches_3105, glob_pos, 1191),
                                           party_mad(lv_ches_3107, glob_pos, 1191),
                                           party_mad(lv_ches_3109, glob_pos, 1191),
                                           party_mad(lv_ches_3112, glob_pos, 1191),
                                           party_mad(lv_ches_3114, glob_pos, 1191),
                                           party_mad(lv_ches_3701, glob_pos, 1470),
                                           party_mad(lv_ches_3702, glob_pos, 1470),
                                           party_mad(lv_ches_3801, glob_pos, 1442),
                                           party_mad(lv_ches_3802, glob_pos, 1442),
                                           party_mad(lv_ches_3803, glob_pos, 1442),
                                           party_mad(lv_ches_3804, glob_pos, 1442),
                                           party_mad(lv_ches_3805, glob_pos, 1442),
                                           party_mad(lv_ches_3806, glob_pos, 1442),
                                           party_mad(lv_ches_4001, glob_pos, 1196),
                                           party_mad(lv_ches_4003, glob_pos, 1196),
                                           party_mad(lv_ches_4004, glob_pos, 1196),
                                           party_mad(lv_ches_4005, glob_pos, 1196),
                                           party_mad(lv_ches_4006, glob_pos, 1196))

detach("package:DescTools", unload=TRUE)

party_vot_var$mad_glob_lv_countrymean <- ifelse(is.nan(party_vot_var$mad_glob_lv_countrymean) == TRUE, NA, party_vot_var$mad_glob_lv_countrymean)

summary(lm(party_vot_var$sd_glob_lv~party_vot_var$mad_glob_lv_countrymean))







#############Immigration dimension: SD likely voters#########
describe(EES_2014$qpp17_6)

#SD of voters on Immi dimension. 
party_vot_var$sd_immi_lv <- c(sd(qpp17_6[lv_ches_102 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_103 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_104 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_105 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_106 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_107 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_108 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_109 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_110 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_112 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_119 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_201 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_202 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_203 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_206 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_211 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_215 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_218 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_301 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_302 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_303 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_304 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_306 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_310 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_311 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_401 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_402 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_403 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_404 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_410 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_413 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_414 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_415 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_501 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_502 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_504 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_505 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_506 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_511 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_513 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_517 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_523 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_525 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_601 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_602 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_605 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_609 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_610 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_613 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_701 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_702 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_703 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_705 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_707 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_708 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_811 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_814 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_815 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_827 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_837 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_838 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_844 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_845 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_848 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1001 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1002 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1003 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1004 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1005 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1014 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1016 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1017 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1018 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1101 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1102 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1104 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1105 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1106 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1107 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1108 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1201 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1202 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1205 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1206 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1208 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1209 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1302 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1303 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1304 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1307 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1309 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1401 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1402 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1403 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1404 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1405 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1406 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1408 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1409 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1601 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1602 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1603 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1604 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1605 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1606 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1607 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1610 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1611 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_1612 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2002 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2004 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2007 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2010 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2015 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2016 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2101 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2103 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2104 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2109 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2111 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2113 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2201 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2202 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2203 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2204 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2207 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2301 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2302 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2308 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2309 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2310 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2311 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2402 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2405 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2406 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2410 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2412 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2501 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2506 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2507 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2511 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2515 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2516 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2518 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2601 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2603 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2605 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2606 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2613 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2614 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2616 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2701 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2702 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2704 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2705 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2706 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2710 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2711 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2802 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2803 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2804 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2805 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2809 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2812 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2813 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2814 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2815 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2902 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2903 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2904 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2905 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2906 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_2914 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3101 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3102 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3104 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3105 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3107 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3109 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3112 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3114 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3701 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3702 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3801 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3802 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3803 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3804 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3805 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_3806 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_4001 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_4003 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_4004 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_4005 == 1], na.rm = TRUE),
                              sd(qpp17_6[lv_ches_4006 == 1], na.rm = TRUE))




#############Immigration dimension: MAD#########

library(DescTools)
#MAD of voters on glob dimension. 
party_vot_var$mad_immi_lv <- c(MeanAD(qpp17_6[lv_ches_102 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_103 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_104 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_105 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_106 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_107 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_108 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_109 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_110 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_112 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_119 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_201 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_202 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_203 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_206 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_211 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_215 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_218 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_301 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_302 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_303 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_304 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_306 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_310 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_311 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_401 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_402 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_403 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_404 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_410 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_413 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_414 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_415 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_501 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_502 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_504 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_505 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_506 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_511 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_513 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_517 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_523 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_525 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_601 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_602 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_605 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_609 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_610 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_613 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_701 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_702 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_703 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_705 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_707 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_708 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_811 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_814 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_815 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_827 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_837 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_838 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_844 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_845 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_848 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1001 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1002 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1003 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1004 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1005 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1014 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1016 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1017 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1018 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1101 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1102 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1104 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1105 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1106 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1107 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1108 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1201 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1202 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1205 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1206 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1208 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1209 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1302 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1303 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1304 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1307 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1309 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1401 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1402 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1403 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1404 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1405 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1406 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1408 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1409 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1601 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1602 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1603 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1604 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1605 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1606 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1607 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1610 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1611 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_1612 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2002 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2004 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2007 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2010 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2015 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2016 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2101 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2103 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2104 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2109 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2111 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2113 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2201 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2202 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2203 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2204 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2207 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2301 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2302 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2308 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2309 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2310 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2311 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2402 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2405 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2406 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2410 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2412 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2501 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2506 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2507 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2511 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2515 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2516 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2518 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2601 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2603 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2605 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2606 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2613 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2614 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2616 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2701 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2702 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2704 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2705 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2706 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2710 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2711 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2802 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2803 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2804 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2805 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2809 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2812 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2813 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2814 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2815 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2902 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2903 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2904 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2905 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2906 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_2914 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3101 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3102 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3104 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3105 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3107 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3109 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3112 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3114 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3701 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3702 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3801 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3802 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3803 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3804 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3805 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_3806 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_4001 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_4003 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_4004 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_4005 == 1], na.rm = TRUE),
                               MeanAD(qpp17_6[lv_ches_4006 == 1], na.rm = TRUE))

detach("package:DescTools", unload=TRUE)
#change nan to na
party_vot_var$mad_immi_lv <- ifelse(is.nan(party_vot_var$mad_immi_lv) == TRUE, NA, party_vot_var$mad_immi_lv)




#############Immigration dimension: mean positions for likely voters#############
party_vot_var$mean_immi_lv <- NA
party_vot_var$mean_immi_lv <- c(mean(qpp17_6[lv_ches_102 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_103 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_104 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_105 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_106 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_107 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_108 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_109 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_110 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_112 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_119 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_201 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_202 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_203 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_206 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_211 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_215 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_218 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_301 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_302 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_303 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_304 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_306 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_310 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_311 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_401 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_402 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_403 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_404 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_410 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_413 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_414 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_415 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_501 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_502 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_504 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_505 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_506 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_511 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_513 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_517 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_523 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_525 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_601 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_602 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_605 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_609 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_610 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_613 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_701 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_702 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_703 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_705 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_707 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_708 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_811 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_814 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_815 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_827 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_837 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_838 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_844 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_845 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_848 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1001 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1002 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1003 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1004 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1005 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1014 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1016 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1017 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1018 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1101 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1102 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1104 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1105 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1106 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1107 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1108 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1201 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1202 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1205 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1206 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1208 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1209 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1302 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1303 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1304 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1307 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1309 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1401 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1402 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1403 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1404 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1405 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1406 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1408 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1409 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1601 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1602 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1603 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1604 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1605 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1606 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1607 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1610 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1611 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_1612 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2002 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2004 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2007 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2010 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2015 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2016 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2101 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2103 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2104 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2109 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2111 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2113 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2201 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2202 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2203 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2204 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2207 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2301 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2302 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2308 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2309 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2310 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2311 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2402 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2405 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2406 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2410 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2412 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2501 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2506 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2507 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2511 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2515 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2516 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2518 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2601 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2603 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2605 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2606 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2613 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2614 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2616 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2701 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2702 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2704 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2705 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2706 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2710 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2711 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2802 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2803 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2804 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2805 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2809 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2812 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2813 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2814 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2815 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2902 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2903 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2904 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2905 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2906 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_2914 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3101 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3102 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3104 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3105 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3107 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3109 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3112 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3114 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3701 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3702 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3801 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3802 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3803 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3804 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3805 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_3806 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_4001 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_4003 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_4004 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_4005 == 1], na.rm = TRUE),
                                mean(qpp17_6[lv_ches_4006 == 1], na.rm = TRUE))

party_vot_var$mean_immi_lv <- ifelse(is.nan(party_vot_var$mean_immi_lv) == TRUE, NA, party_vot_var$mean_immi_lv)










#############Immigration dimension: percentage extreme voters##############################


attach(EES_2014)

#trying to make a function for percentage extreme. 
#take the sum of all not NA values for a question. This is the total n for that question. Then take all extreme values
#for that same question. Do this three times for both and calculate percentage extreme scores.
#note that not all people answering all three questions is taken into account by taking separate Ns for each question
#the lvches is the name for columns with dummies for likely voters. 
per_ext_voters <- function(lvches)
{
  n1 = sum(!is.na((as.numeric(qpp17_6[lvches == 1]))))
  t1 = sum(!is.na((as.numeric(qpp17_6[lvches == 1 & (qpp17_6 == 11 | qpp17_6 == 1) ]))))
  pext <- (t1/n1) * 100
  return(pext)
}

####################Adding the code for all parties. Note that, just like above, all NAs are for Eastern European countries
party_vot_var$perext_immi_lv <- c(per_ext_voters(lv_ches_102),
                                  per_ext_voters(lv_ches_103),
                                  per_ext_voters(lv_ches_104),
                                  per_ext_voters(lv_ches_105),
                                  per_ext_voters(lv_ches_106),
                                  per_ext_voters(lv_ches_107),
                                  per_ext_voters(lv_ches_108),
                                  per_ext_voters(lv_ches_109),
                                  per_ext_voters(lv_ches_110),
                                  per_ext_voters(lv_ches_112),
                                  per_ext_voters(lv_ches_119),
                                  per_ext_voters(lv_ches_201),
                                  per_ext_voters(lv_ches_202),
                                  per_ext_voters(lv_ches_203),
                                  per_ext_voters(lv_ches_206),
                                  per_ext_voters(lv_ches_211),
                                  per_ext_voters(lv_ches_215),
                                  per_ext_voters(lv_ches_218),
                                  per_ext_voters(lv_ches_301),
                                  per_ext_voters(lv_ches_302),
                                  per_ext_voters(lv_ches_303),
                                  per_ext_voters(lv_ches_304),
                                  per_ext_voters(lv_ches_306),
                                  per_ext_voters(lv_ches_310),
                                  per_ext_voters(lv_ches_311),
                                  per_ext_voters(lv_ches_401),
                                  per_ext_voters(lv_ches_402),
                                  per_ext_voters(lv_ches_403),
                                  per_ext_voters(lv_ches_404),
                                  per_ext_voters(lv_ches_410),
                                  per_ext_voters(lv_ches_413),
                                  per_ext_voters(lv_ches_414),
                                  per_ext_voters(lv_ches_415),
                                  per_ext_voters(lv_ches_501),
                                  per_ext_voters(lv_ches_502),
                                  per_ext_voters(lv_ches_504),
                                  per_ext_voters(lv_ches_505),
                                  per_ext_voters(lv_ches_506),
                                  per_ext_voters(lv_ches_511),
                                  per_ext_voters(lv_ches_513),
                                  per_ext_voters(lv_ches_517),
                                  per_ext_voters(lv_ches_523),
                                  per_ext_voters(lv_ches_525),
                                  per_ext_voters(lv_ches_601),
                                  per_ext_voters(lv_ches_602),
                                  per_ext_voters(lv_ches_605),
                                  per_ext_voters(lv_ches_609),
                                  per_ext_voters(lv_ches_610),
                                  per_ext_voters(lv_ches_613),
                                  per_ext_voters(lv_ches_701),
                                  per_ext_voters(lv_ches_702),
                                  per_ext_voters(lv_ches_703),
                                  per_ext_voters(lv_ches_705),
                                  per_ext_voters(lv_ches_707),
                                  per_ext_voters(lv_ches_708),
                                  per_ext_voters(lv_ches_811),
                                  per_ext_voters(lv_ches_814),
                                  per_ext_voters(lv_ches_815),
                                  per_ext_voters(lv_ches_827),
                                  per_ext_voters(lv_ches_837),
                                  per_ext_voters(lv_ches_838),
                                  per_ext_voters(lv_ches_844),
                                  per_ext_voters(lv_ches_845),
                                  per_ext_voters(lv_ches_848),
                                  per_ext_voters(lv_ches_1001),
                                  per_ext_voters(lv_ches_1002),
                                  per_ext_voters(lv_ches_1003),
                                  per_ext_voters(lv_ches_1004),
                                  per_ext_voters(lv_ches_1005),
                                  per_ext_voters(lv_ches_1014),
                                  per_ext_voters(lv_ches_1016),
                                  per_ext_voters(lv_ches_1017),
                                  per_ext_voters(lv_ches_1018),
                                  per_ext_voters(lv_ches_1101),
                                  per_ext_voters(lv_ches_1102),
                                  per_ext_voters(lv_ches_1104),
                                  per_ext_voters(lv_ches_1105),
                                  per_ext_voters(lv_ches_1106),
                                  per_ext_voters(lv_ches_1107),
                                  per_ext_voters(lv_ches_1108),
                                  per_ext_voters(lv_ches_1201),
                                  per_ext_voters(lv_ches_1202),
                                  per_ext_voters(lv_ches_1205),
                                  per_ext_voters(lv_ches_1206),
                                  per_ext_voters(lv_ches_1208),
                                  per_ext_voters(lv_ches_1209),
                                  per_ext_voters(lv_ches_1302),
                                  per_ext_voters(lv_ches_1303),
                                  per_ext_voters(lv_ches_1304),
                                  per_ext_voters(lv_ches_1307),
                                  per_ext_voters(lv_ches_1309),
                                  per_ext_voters(lv_ches_1401),
                                  per_ext_voters(lv_ches_1402),
                                  per_ext_voters(lv_ches_1403),
                                  per_ext_voters(lv_ches_1404),
                                  per_ext_voters(lv_ches_1405),
                                  per_ext_voters(lv_ches_1406),
                                  per_ext_voters(lv_ches_1408),
                                  per_ext_voters(lv_ches_1409),
                                  per_ext_voters(lv_ches_1601),
                                  per_ext_voters(lv_ches_1602),
                                  per_ext_voters(lv_ches_1603),
                                  per_ext_voters(lv_ches_1604),
                                  per_ext_voters(lv_ches_1605),
                                  per_ext_voters(lv_ches_1606),
                                  per_ext_voters(lv_ches_1607),
                                  per_ext_voters(lv_ches_1610),
                                  per_ext_voters(lv_ches_1611),
                                  per_ext_voters(lv_ches_1612),
                                  per_ext_voters(lv_ches_2002),
                                  per_ext_voters(lv_ches_2004),
                                  per_ext_voters(lv_ches_2007),
                                  per_ext_voters(lv_ches_2010),
                                  per_ext_voters(lv_ches_2015),
                                  per_ext_voters(lv_ches_2016),
                                  per_ext_voters(lv_ches_2101),
                                  per_ext_voters(lv_ches_2103),
                                  per_ext_voters(lv_ches_2104),
                                  per_ext_voters(lv_ches_2109),
                                  per_ext_voters(lv_ches_2111),
                                  per_ext_voters(lv_ches_2113),
                                  per_ext_voters(lv_ches_2201),
                                  per_ext_voters(lv_ches_2202),
                                  per_ext_voters(lv_ches_2203),
                                  per_ext_voters(lv_ches_2204),
                                  per_ext_voters(lv_ches_2207),
                                  per_ext_voters(lv_ches_2301),
                                  per_ext_voters(lv_ches_2302),
                                  per_ext_voters(lv_ches_2308),
                                  per_ext_voters(lv_ches_2309),
                                  per_ext_voters(lv_ches_2310),
                                  per_ext_voters(lv_ches_2311),
                                  per_ext_voters(lv_ches_2402),
                                  per_ext_voters(lv_ches_2405),
                                  per_ext_voters(lv_ches_2406),
                                  per_ext_voters(lv_ches_2410),
                                  per_ext_voters(lv_ches_2412),
                                  per_ext_voters(lv_ches_2501),
                                  per_ext_voters(lv_ches_2506),
                                  per_ext_voters(lv_ches_2507),
                                  per_ext_voters(lv_ches_2511),
                                  per_ext_voters(lv_ches_2515),
                                  per_ext_voters(lv_ches_2516),
                                  per_ext_voters(lv_ches_2518),
                                  per_ext_voters(lv_ches_2601),
                                  per_ext_voters(lv_ches_2603),
                                  per_ext_voters(lv_ches_2605),
                                  per_ext_voters(lv_ches_2606),
                                  per_ext_voters(lv_ches_2613),
                                  per_ext_voters(lv_ches_2614),
                                  per_ext_voters(lv_ches_2616),
                                  per_ext_voters(lv_ches_2701),
                                  per_ext_voters(lv_ches_2702),
                                  per_ext_voters(lv_ches_2704),
                                  per_ext_voters(lv_ches_2705),
                                  per_ext_voters(lv_ches_2706),
                                  per_ext_voters(lv_ches_2710),
                                  per_ext_voters(lv_ches_2711),
                                  per_ext_voters(lv_ches_2802),
                                  per_ext_voters(lv_ches_2803),
                                  per_ext_voters(lv_ches_2804),
                                  per_ext_voters(lv_ches_2805),
                                  per_ext_voters(lv_ches_2809),
                                  per_ext_voters(lv_ches_2812),
                                  per_ext_voters(lv_ches_2813),
                                  per_ext_voters(lv_ches_2814),
                                  per_ext_voters(lv_ches_2815),
                                  per_ext_voters(lv_ches_2902),
                                  per_ext_voters(lv_ches_2903),
                                  per_ext_voters(lv_ches_2904),
                                  per_ext_voters(lv_ches_2905),
                                  per_ext_voters(lv_ches_2906),
                                  per_ext_voters(lv_ches_2914),
                                  per_ext_voters(lv_ches_3101),
                                  per_ext_voters(lv_ches_3102),
                                  per_ext_voters(lv_ches_3104),
                                  per_ext_voters(lv_ches_3105),
                                  per_ext_voters(lv_ches_3107),
                                  per_ext_voters(lv_ches_3109),
                                  per_ext_voters(lv_ches_3112),
                                  per_ext_voters(lv_ches_3114),
                                  per_ext_voters(lv_ches_3701),
                                  per_ext_voters(lv_ches_3702),
                                  per_ext_voters(lv_ches_3801),
                                  per_ext_voters(lv_ches_3802),
                                  per_ext_voters(lv_ches_3803),
                                  per_ext_voters(lv_ches_3804),
                                  per_ext_voters(lv_ches_3805),
                                  per_ext_voters(lv_ches_3806),
                                  per_ext_voters(lv_ches_4001),
                                  per_ext_voters(lv_ches_4003),
                                  per_ext_voters(lv_ches_4004),
                                  per_ext_voters(lv_ches_4005),
                                  per_ext_voters(lv_ches_4006))



party_vot_var$perext_immi_lv <- ifelse(is.nan(party_vot_var$perext_immi_lv) == TRUE, NA, party_vot_var$perext_immi_lv)










#############Immigration dimension: Average distance from country mean##########################################
##Can function as an add-on to percentage of extreme scores. 


library(DescTools) #using this function to get MAD https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/MeanAD

party_mad <- function(party, var, country) #MAD for likely voters of a party, on a variable, in a country
{
  country_mean <-  function(var, country) #country_mean calculates the mean on a variable in a country
  {
    x <- mean(var[countrycode == country], na.rm=TRUE)
    return(x)
  }
  y <- MeanAD(var[party == 1], FUN=country_mean(var, country), na.rm=TRUE) #MeanAD calculates the mean distance on a variable for likely voters in a country where the midpoint is the return from above, i.e. country mean. 
  return(y) 
}

party_mad(lv_ches_1017, qpp17_6, 1528) #PVV far away from country mean
party_mad(lv_ches_1003, qpp17_6, 1056) #VVD closer to country mean



party_vot_var$mad_immi_lv_countrymean <- c(party_mad(lv_ches_102, qpp17_6, 1056),
                                           party_mad(lv_ches_103, qpp17_6, 1056),
                                           party_mad(lv_ches_104, qpp17_6, 1056),
                                           party_mad(lv_ches_105, qpp17_6, 1056),
                                           party_mad(lv_ches_106, qpp17_6, 1056),
                                           party_mad(lv_ches_107, qpp17_6, 1056),
                                           party_mad(lv_ches_108, qpp17_6, 1056),
                                           party_mad(lv_ches_109, qpp17_6, 1056),
                                           party_mad(lv_ches_110, qpp17_6, 1056),
                                           party_mad(lv_ches_112, qpp17_6, 1056),
                                           party_mad(lv_ches_119, qpp17_6, 1056),
                                           party_mad(lv_ches_201, qpp17_6, 1208),
                                           party_mad(lv_ches_202, qpp17_6, 1208),
                                           party_mad(lv_ches_203, qpp17_6, 1208),
                                           party_mad(lv_ches_206, qpp17_6, 1208),
                                           party_mad(lv_ches_211, qpp17_6, 1208),
                                           party_mad(lv_ches_215, qpp17_6, 1208),
                                           party_mad(lv_ches_218, qpp17_6, 1208),
                                           party_mad(lv_ches_301, qpp17_6, 1276),
                                           party_mad(lv_ches_302, qpp17_6, 1276),
                                           party_mad(lv_ches_303, qpp17_6, 1276),
                                           party_mad(lv_ches_304, qpp17_6, 1276),
                                           party_mad(lv_ches_306, qpp17_6, 1276),
                                           party_mad(lv_ches_310, qpp17_6, 1276),
                                           party_mad(lv_ches_311, qpp17_6, 1276),
                                           party_mad(lv_ches_401, qpp17_6, 1300),
                                           party_mad(lv_ches_402, qpp17_6, 1300),
                                           party_mad(lv_ches_403, qpp17_6, 1300),
                                           party_mad(lv_ches_404, qpp17_6, 1300),
                                           party_mad(lv_ches_410, qpp17_6, 1300),
                                           party_mad(lv_ches_413, qpp17_6, 1300),
                                           party_mad(lv_ches_414, qpp17_6, 1300),
                                           party_mad(lv_ches_415, qpp17_6, 1300),
                                           party_mad(lv_ches_501, qpp17_6, 1724),
                                           party_mad(lv_ches_502, qpp17_6, 1724),
                                           party_mad(lv_ches_504, qpp17_6, 1724),
                                           party_mad(lv_ches_505, qpp17_6, 1724),
                                           party_mad(lv_ches_506, qpp17_6, 1724),
                                           party_mad(lv_ches_511, qpp17_6, 1724),
                                           party_mad(lv_ches_513, qpp17_6, 1724),
                                           party_mad(lv_ches_517, qpp17_6, 1724),
                                           party_mad(lv_ches_523, qpp17_6, 1724),
                                           party_mad(lv_ches_525, qpp17_6, 1724),
                                           party_mad(lv_ches_601, qpp17_6, 1250),
                                           party_mad(lv_ches_602, qpp17_6, 1250),
                                           party_mad(lv_ches_605, qpp17_6, 1250),
                                           party_mad(lv_ches_609, qpp17_6, 1250),
                                           party_mad(lv_ches_610, qpp17_6, 1250),
                                           party_mad(lv_ches_613, qpp17_6, 1250),
                                           party_mad(lv_ches_701, qpp17_6, 1372),
                                           party_mad(lv_ches_702, qpp17_6, 1372),
                                           party_mad(lv_ches_703, qpp17_6, 1372),
                                           party_mad(lv_ches_705, qpp17_6, 1372),
                                           party_mad(lv_ches_707, qpp17_6, 1372),
                                           party_mad(lv_ches_708, qpp17_6, 1372),
                                           party_mad(lv_ches_811, qpp17_6, 1380),
                                           party_mad(lv_ches_814, qpp17_6, 1380),
                                           party_mad(lv_ches_815, qpp17_6, 1380),
                                           party_mad(lv_ches_827, qpp17_6, 1380),
                                           party_mad(lv_ches_837, qpp17_6, 1380),
                                           party_mad(lv_ches_838, qpp17_6, 1380),
                                           party_mad(lv_ches_844, qpp17_6, 1380),
                                           party_mad(lv_ches_845, qpp17_6, 1380),
                                           party_mad(lv_ches_848, qpp17_6, 1380),
                                           party_mad(lv_ches_1001, qpp17_6, 1528),
                                           party_mad(lv_ches_1002, qpp17_6, 1528),
                                           party_mad(lv_ches_1003, qpp17_6, 1528),
                                           party_mad(lv_ches_1004, qpp17_6, 1528),
                                           party_mad(lv_ches_1005, qpp17_6, 1528),
                                           party_mad(lv_ches_1014, qpp17_6, 1528),
                                           party_mad(lv_ches_1016, qpp17_6, 1528),
                                           party_mad(lv_ches_1017, qpp17_6, 1528),
                                           party_mad(lv_ches_1018, qpp17_6, 1528),
                                           party_mad(lv_ches_1101, qpp17_6, 1826),
                                           party_mad(lv_ches_1102, qpp17_6, 1826),
                                           party_mad(lv_ches_1104, qpp17_6, 1826),
                                           party_mad(lv_ches_1105, qpp17_6, 1826),
                                           party_mad(lv_ches_1106, qpp17_6, 1826),
                                           party_mad(lv_ches_1107, qpp17_6, 1826),
                                           party_mad(lv_ches_1108, qpp17_6, 1826),
                                           party_mad(lv_ches_1201, qpp17_6, 1620),
                                           party_mad(lv_ches_1202, qpp17_6, 1620),
                                           party_mad(lv_ches_1205, qpp17_6, 1620),
                                           party_mad(lv_ches_1206, qpp17_6, 1620),
                                           party_mad(lv_ches_1208, qpp17_6, 1620),
                                           party_mad(lv_ches_1209, qpp17_6, 1620),
                                           party_mad(lv_ches_1302, qpp17_6, 1040),
                                           party_mad(lv_ches_1303, qpp17_6, 1040),
                                           party_mad(lv_ches_1304, qpp17_6, 1040),
                                           party_mad(lv_ches_1307, qpp17_6, 1040),
                                           party_mad(lv_ches_1309, qpp17_6, 1040),
                                           party_mad(lv_ches_1401, qpp17_6, 1246),
                                           party_mad(lv_ches_1402, qpp17_6, 1246),
                                           party_mad(lv_ches_1403, qpp17_6, 1246),
                                           party_mad(lv_ches_1404, qpp17_6, 1246),
                                           party_mad(lv_ches_1405, qpp17_6, 1246),
                                           party_mad(lv_ches_1406, qpp17_6, 1246),
                                           party_mad(lv_ches_1408, qpp17_6, 1246),
                                           party_mad(lv_ches_1409, qpp17_6, 1246),
                                           party_mad(lv_ches_1601, qpp17_6, 1752),
                                           party_mad(lv_ches_1602, qpp17_6, 1752),
                                           party_mad(lv_ches_1603, qpp17_6, 1752),
                                           party_mad(lv_ches_1604, qpp17_6, 1752),
                                           party_mad(lv_ches_1605, qpp17_6, 1752),
                                           party_mad(lv_ches_1606, qpp17_6, 1752),
                                           party_mad(lv_ches_1607, qpp17_6, 1752),
                                           party_mad(lv_ches_1610, qpp17_6, 1752),
                                           party_mad(lv_ches_1611, qpp17_6, 1752),
                                           party_mad(lv_ches_1612, qpp17_6, 1752),
                                           party_mad(lv_ches_2002, qpp17_6, 1100),
                                           party_mad(lv_ches_2004, qpp17_6, 1100),
                                           party_mad(lv_ches_2007, qpp17_6, 1100),
                                           party_mad(lv_ches_2010, qpp17_6, 1100),
                                           party_mad(lv_ches_2015, qpp17_6, 1100),
                                           party_mad(lv_ches_2016, qpp17_6, 1100),
                                           party_mad(lv_ches_2101, qpp17_6, 1203),
                                           party_mad(lv_ches_2103, qpp17_6, 1203),
                                           party_mad(lv_ches_2104, qpp17_6, 1203),
                                           party_mad(lv_ches_2109, qpp17_6, 1203),
                                           party_mad(lv_ches_2111, qpp17_6, 1203),
                                           party_mad(lv_ches_2113, qpp17_6, 1203),
                                           party_mad(lv_ches_2201, qpp17_6, 1233),
                                           party_mad(lv_ches_2202, qpp17_6, 1233),
                                           party_mad(lv_ches_2203, qpp17_6, 1233),
                                           party_mad(lv_ches_2204, qpp17_6, 1233),
                                           party_mad(lv_ches_2207, qpp17_6, 1233),
                                           party_mad(lv_ches_2301, qpp17_6, 1348),
                                           party_mad(lv_ches_2302, qpp17_6, 1348),
                                           party_mad(lv_ches_2308, qpp17_6, 1348),
                                           party_mad(lv_ches_2309, qpp17_6, 1348),
                                           party_mad(lv_ches_2310, qpp17_6, 1348),
                                           party_mad(lv_ches_2311, qpp17_6, 1348),
                                           party_mad(lv_ches_2402, qpp17_6, 1428),
                                           party_mad(lv_ches_2405, qpp17_6, 1428),
                                           party_mad(lv_ches_2406, qpp17_6, 1428),
                                           party_mad(lv_ches_2410, qpp17_6, 1428),
                                           party_mad(lv_ches_2412, qpp17_6, 1428),
                                           party_mad(lv_ches_2501, qpp17_6, 1440),
                                           party_mad(lv_ches_2506, qpp17_6, 1440),
                                           party_mad(lv_ches_2507, qpp17_6, 1440),
                                           party_mad(lv_ches_2511, qpp17_6, 1440),
                                           party_mad(lv_ches_2515, qpp17_6, 1440),
                                           party_mad(lv_ches_2516, qpp17_6, 1440),
                                           party_mad(lv_ches_2518, qpp17_6, 1440),
                                           party_mad(lv_ches_2601, qpp17_6, 1616),
                                           party_mad(lv_ches_2603, qpp17_6, 1616),
                                           party_mad(lv_ches_2605, qpp17_6, 1616),
                                           party_mad(lv_ches_2606, qpp17_6, 1616),
                                           party_mad(lv_ches_2613, qpp17_6, 1616),
                                           party_mad(lv_ches_2614, qpp17_6, 1616),
                                           party_mad(lv_ches_2616, qpp17_6, 1616),
                                           party_mad(lv_ches_2701, qpp17_6, 1642),
                                           party_mad(lv_ches_2702, qpp17_6, 1642),
                                           party_mad(lv_ches_2704, qpp17_6, 1642),
                                           party_mad(lv_ches_2705, qpp17_6, 1642),
                                           party_mad(lv_ches_2706, qpp17_6, 1642),
                                           party_mad(lv_ches_2710, qpp17_6, 1642),
                                           party_mad(lv_ches_2711, qpp17_6, 1642),
                                           party_mad(lv_ches_2802, qpp17_6, 1703),
                                           party_mad(lv_ches_2803, qpp17_6, 1703),
                                           party_mad(lv_ches_2804, qpp17_6, 1703),
                                           party_mad(lv_ches_2805, qpp17_6, 1703),
                                           party_mad(lv_ches_2809, qpp17_6, 1703),
                                           party_mad(lv_ches_2812, qpp17_6, 1703),
                                           party_mad(lv_ches_2813, qpp17_6, 1703),
                                           party_mad(lv_ches_2814, qpp17_6, 1703),
                                           party_mad(lv_ches_2815, qpp17_6, 1703),
                                           party_mad(lv_ches_2902, qpp17_6, 1705),
                                           party_mad(lv_ches_2903, qpp17_6, 1705),
                                           party_mad(lv_ches_2904, qpp17_6, 1705),
                                           party_mad(lv_ches_2905, qpp17_6, 1705),
                                           party_mad(lv_ches_2906, qpp17_6, 1705),
                                           party_mad(lv_ches_2914, qpp17_6, 1705),
                                           party_mad(lv_ches_3101, qpp17_6, 1191),
                                           party_mad(lv_ches_3102, qpp17_6, 1191),
                                           party_mad(lv_ches_3104, qpp17_6, 1191),
                                           party_mad(lv_ches_3105, qpp17_6, 1191),
                                           party_mad(lv_ches_3107, qpp17_6, 1191),
                                           party_mad(lv_ches_3109, qpp17_6, 1191),
                                           party_mad(lv_ches_3112, qpp17_6, 1191),
                                           party_mad(lv_ches_3114, qpp17_6, 1191),
                                           party_mad(lv_ches_3701, qpp17_6, 1470),
                                           party_mad(lv_ches_3702, qpp17_6, 1470),
                                           party_mad(lv_ches_3801, qpp17_6, 1442),
                                           party_mad(lv_ches_3802, qpp17_6, 1442),
                                           party_mad(lv_ches_3803, qpp17_6, 1442),
                                           party_mad(lv_ches_3804, qpp17_6, 1442),
                                           party_mad(lv_ches_3805, qpp17_6, 1442),
                                           party_mad(lv_ches_3806, qpp17_6, 1442),
                                           party_mad(lv_ches_4001, qpp17_6, 1196),
                                           party_mad(lv_ches_4003, qpp17_6, 1196),
                                           party_mad(lv_ches_4004, qpp17_6, 1196),
                                           party_mad(lv_ches_4005, qpp17_6, 1196),
                                           party_mad(lv_ches_4006, qpp17_6, 1196))

detach("package:DescTools", unload=TRUE)

party_vot_var$mad_immi_lv_countrymean <- ifelse(is.nan(party_vot_var$mad_immi_lv_countrymean) == TRUE, NA, party_vot_var$mad_immi_lv_countrymean)












#############EU dimension: SD likely voters#########

#SD of voters on EU dimension. 
party_vot_var$sd_eu_lv <- c(sd(qpp18[lv_ches_102 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_103 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_104 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_105 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_106 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_107 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_108 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_109 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_110 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_112 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_119 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_201 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_202 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_203 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_206 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_211 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_215 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_218 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_301 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_302 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_303 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_304 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_306 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_310 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_311 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_401 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_402 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_403 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_404 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_410 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_413 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_414 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_415 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_501 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_502 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_504 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_505 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_506 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_511 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_513 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_517 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_523 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_525 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_601 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_602 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_605 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_609 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_610 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_613 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_701 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_702 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_703 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_705 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_707 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_708 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_811 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_814 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_815 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_827 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_837 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_838 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_844 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_845 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_848 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1001 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1002 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1003 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1004 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1005 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1014 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1016 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1017 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1018 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1101 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1102 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1104 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1105 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1106 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1107 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1108 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1201 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1202 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1205 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1206 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1208 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1209 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1302 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1303 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1304 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1307 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1309 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1401 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1402 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1403 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1404 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1405 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1406 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1408 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1409 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1601 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1602 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1603 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1604 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1605 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1606 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1607 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1610 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1611 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_1612 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2002 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2004 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2007 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2010 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2015 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2016 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2101 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2103 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2104 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2109 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2111 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2113 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2201 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2202 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2203 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2204 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2207 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2301 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2302 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2308 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2309 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2310 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2311 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2402 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2405 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2406 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2410 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2412 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2501 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2506 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2507 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2511 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2515 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2516 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2518 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2601 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2603 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2605 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2606 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2613 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2614 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2616 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2701 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2702 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2704 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2705 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2706 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2710 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2711 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2802 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2803 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2804 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2805 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2809 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2812 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2813 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2814 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2815 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2902 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2903 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2904 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2905 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2906 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_2914 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3101 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3102 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3104 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3105 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3107 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3109 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3112 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3114 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3701 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3702 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3801 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3802 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3803 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3804 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3805 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_3806 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_4001 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_4003 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_4004 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_4005 == 1], na.rm = TRUE),
                            sd(qpp18[lv_ches_4006 == 1], na.rm = TRUE))




#############EU dimension: MAD#########
library(DescTools)
#MAD of voters on glob dimension. 
party_vot_var$mad_eu_lv <- c(MeanAD(qpp18[lv_ches_102 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_103 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_104 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_105 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_106 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_107 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_108 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_109 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_110 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_112 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_119 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_201 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_202 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_203 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_206 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_211 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_215 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_218 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_301 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_302 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_303 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_304 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_306 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_310 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_311 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_401 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_402 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_403 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_404 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_410 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_413 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_414 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_415 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_501 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_502 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_504 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_505 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_506 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_511 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_513 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_517 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_523 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_525 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_601 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_602 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_605 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_609 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_610 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_613 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_701 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_702 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_703 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_705 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_707 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_708 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_811 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_814 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_815 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_827 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_837 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_838 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_844 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_845 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_848 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1001 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1002 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1003 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1004 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1005 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1014 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1016 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1017 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1018 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1101 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1102 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1104 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1105 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1106 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1107 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1108 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1201 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1202 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1205 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1206 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1208 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1209 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1302 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1303 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1304 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1307 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1309 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1401 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1402 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1403 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1404 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1405 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1406 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1408 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1409 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1601 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1602 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1603 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1604 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1605 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1606 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1607 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1610 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1611 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_1612 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2002 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2004 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2007 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2010 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2015 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2016 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2101 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2103 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2104 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2109 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2111 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2113 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2201 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2202 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2203 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2204 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2207 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2301 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2302 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2308 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2309 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2310 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2311 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2402 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2405 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2406 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2410 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2412 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2501 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2506 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2507 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2511 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2515 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2516 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2518 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2601 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2603 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2605 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2606 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2613 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2614 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2616 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2701 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2702 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2704 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2705 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2706 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2710 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2711 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2802 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2803 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2804 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2805 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2809 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2812 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2813 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2814 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2815 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2902 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2903 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2904 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2905 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2906 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_2914 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3101 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3102 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3104 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3105 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3107 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3109 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3112 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3114 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3701 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3702 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3801 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3802 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3803 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3804 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3805 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_3806 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_4001 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_4003 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_4004 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_4005 == 1], na.rm = TRUE),
                             MeanAD(qpp18[lv_ches_4006 == 1], na.rm = TRUE))

detach("package:DescTools", unload=TRUE)

party_vot_var$mad_eu_lv <- ifelse(is.nan(party_vot_var$mad_eu_lv) == TRUE, NA, party_vot_var$mad_eu_lv)




#############EU dimension: mean positions for likely voters#############
party_vot_var$mean_eu_lv <- c(mean(qpp18[lv_ches_102 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_103 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_104 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_105 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_106 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_107 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_108 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_109 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_110 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_112 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_119 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_201 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_202 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_203 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_206 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_211 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_215 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_218 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_301 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_302 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_303 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_304 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_306 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_310 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_311 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_401 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_402 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_403 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_404 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_410 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_413 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_414 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_415 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_501 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_502 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_504 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_505 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_506 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_511 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_513 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_517 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_523 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_525 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_601 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_602 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_605 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_609 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_610 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_613 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_701 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_702 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_703 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_705 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_707 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_708 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_811 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_814 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_815 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_827 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_837 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_838 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_844 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_845 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_848 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1001 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1002 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1003 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1004 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1005 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1014 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1016 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1017 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1018 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1101 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1102 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1104 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1105 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1106 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1107 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1108 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1201 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1202 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1205 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1206 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1208 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1209 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1302 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1303 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1304 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1307 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1309 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1401 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1402 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1403 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1404 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1405 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1406 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1408 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1409 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1601 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1602 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1603 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1604 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1605 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1606 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1607 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1610 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1611 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_1612 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2002 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2004 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2007 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2010 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2015 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2016 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2101 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2103 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2104 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2109 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2111 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2113 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2201 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2202 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2203 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2204 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2207 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2301 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2302 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2308 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2309 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2310 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2311 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2402 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2405 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2406 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2410 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2412 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2501 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2506 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2507 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2511 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2515 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2516 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2518 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2601 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2603 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2605 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2606 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2613 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2614 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2616 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2701 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2702 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2704 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2705 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2706 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2710 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2711 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2802 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2803 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2804 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2805 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2809 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2812 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2813 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2814 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2815 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2902 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2903 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2904 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2905 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2906 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_2914 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3101 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3102 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3104 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3105 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3107 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3109 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3112 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3114 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3701 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3702 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3801 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3802 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3803 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3804 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3805 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_3806 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_4001 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_4003 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_4004 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_4005 == 1], na.rm = TRUE),
                              mean(qpp18[lv_ches_4006 == 1], na.rm = TRUE))

party_vot_var$mean_eu_lv <- ifelse(is.nan(party_vot_var$mean_eu_lv) == TRUE, NA, party_vot_var$mean_eu_lv)










#############EU dimension: percentage extreme voters##############################


per_ext_voters <- function(lvches)
{
  n1 = sum(!is.na((as.numeric(qpp18[lvches == 1]))))
  t1 = sum(!is.na((as.numeric(qpp18[lvches == 1 & (qpp18 == 11 | qpp18 == 1) ]))))
  pext <- (t1/n1) * 100
  return(pext)
}


####################Adding the code for all parties. Note that, just like above, all NAs are for Eastern European countries
party_vot_var$perext_eu_lv <- c(per_ext_voters(lv_ches_102),
                                per_ext_voters(lv_ches_103),
                                per_ext_voters(lv_ches_104),
                                per_ext_voters(lv_ches_105),
                                per_ext_voters(lv_ches_106),
                                per_ext_voters(lv_ches_107),
                                per_ext_voters(lv_ches_108),
                                per_ext_voters(lv_ches_109),
                                per_ext_voters(lv_ches_110),
                                per_ext_voters(lv_ches_112),
                                per_ext_voters(lv_ches_119),
                                per_ext_voters(lv_ches_201),
                                per_ext_voters(lv_ches_202),
                                per_ext_voters(lv_ches_203),
                                per_ext_voters(lv_ches_206),
                                per_ext_voters(lv_ches_211),
                                per_ext_voters(lv_ches_215),
                                per_ext_voters(lv_ches_218),
                                per_ext_voters(lv_ches_301),
                                per_ext_voters(lv_ches_302),
                                per_ext_voters(lv_ches_303),
                                per_ext_voters(lv_ches_304),
                                per_ext_voters(lv_ches_306),
                                per_ext_voters(lv_ches_310),
                                per_ext_voters(lv_ches_311),
                                per_ext_voters(lv_ches_401),
                                per_ext_voters(lv_ches_402),
                                per_ext_voters(lv_ches_403),
                                per_ext_voters(lv_ches_404),
                                per_ext_voters(lv_ches_410),
                                per_ext_voters(lv_ches_413),
                                per_ext_voters(lv_ches_414),
                                per_ext_voters(lv_ches_415),
                                per_ext_voters(lv_ches_501),
                                per_ext_voters(lv_ches_502),
                                per_ext_voters(lv_ches_504),
                                per_ext_voters(lv_ches_505),
                                per_ext_voters(lv_ches_506),
                                per_ext_voters(lv_ches_511),
                                per_ext_voters(lv_ches_513),
                                per_ext_voters(lv_ches_517),
                                per_ext_voters(lv_ches_523),
                                per_ext_voters(lv_ches_525),
                                per_ext_voters(lv_ches_601),
                                per_ext_voters(lv_ches_602),
                                per_ext_voters(lv_ches_605),
                                per_ext_voters(lv_ches_609),
                                per_ext_voters(lv_ches_610),
                                per_ext_voters(lv_ches_613),
                                per_ext_voters(lv_ches_701),
                                per_ext_voters(lv_ches_702),
                                per_ext_voters(lv_ches_703),
                                per_ext_voters(lv_ches_705),
                                per_ext_voters(lv_ches_707),
                                per_ext_voters(lv_ches_708),
                                per_ext_voters(lv_ches_811),
                                per_ext_voters(lv_ches_814),
                                per_ext_voters(lv_ches_815),
                                per_ext_voters(lv_ches_827),
                                per_ext_voters(lv_ches_837),
                                per_ext_voters(lv_ches_838),
                                per_ext_voters(lv_ches_844),
                                per_ext_voters(lv_ches_845),
                                per_ext_voters(lv_ches_848),
                                per_ext_voters(lv_ches_1001),
                                per_ext_voters(lv_ches_1002),
                                per_ext_voters(lv_ches_1003),
                                per_ext_voters(lv_ches_1004),
                                per_ext_voters(lv_ches_1005),
                                per_ext_voters(lv_ches_1014),
                                per_ext_voters(lv_ches_1016),
                                per_ext_voters(lv_ches_1017),
                                per_ext_voters(lv_ches_1018),
                                per_ext_voters(lv_ches_1101),
                                per_ext_voters(lv_ches_1102),
                                per_ext_voters(lv_ches_1104),
                                per_ext_voters(lv_ches_1105),
                                per_ext_voters(lv_ches_1106),
                                per_ext_voters(lv_ches_1107),
                                per_ext_voters(lv_ches_1108),
                                per_ext_voters(lv_ches_1201),
                                per_ext_voters(lv_ches_1202),
                                per_ext_voters(lv_ches_1205),
                                per_ext_voters(lv_ches_1206),
                                per_ext_voters(lv_ches_1208),
                                per_ext_voters(lv_ches_1209),
                                per_ext_voters(lv_ches_1302),
                                per_ext_voters(lv_ches_1303),
                                per_ext_voters(lv_ches_1304),
                                per_ext_voters(lv_ches_1307),
                                per_ext_voters(lv_ches_1309),
                                per_ext_voters(lv_ches_1401),
                                per_ext_voters(lv_ches_1402),
                                per_ext_voters(lv_ches_1403),
                                per_ext_voters(lv_ches_1404),
                                per_ext_voters(lv_ches_1405),
                                per_ext_voters(lv_ches_1406),
                                per_ext_voters(lv_ches_1408),
                                per_ext_voters(lv_ches_1409),
                                per_ext_voters(lv_ches_1601),
                                per_ext_voters(lv_ches_1602),
                                per_ext_voters(lv_ches_1603),
                                per_ext_voters(lv_ches_1604),
                                per_ext_voters(lv_ches_1605),
                                per_ext_voters(lv_ches_1606),
                                per_ext_voters(lv_ches_1607),
                                per_ext_voters(lv_ches_1610),
                                per_ext_voters(lv_ches_1611),
                                per_ext_voters(lv_ches_1612),
                                per_ext_voters(lv_ches_2002),
                                per_ext_voters(lv_ches_2004),
                                per_ext_voters(lv_ches_2007),
                                per_ext_voters(lv_ches_2010),
                                per_ext_voters(lv_ches_2015),
                                per_ext_voters(lv_ches_2016),
                                per_ext_voters(lv_ches_2101),
                                per_ext_voters(lv_ches_2103),
                                per_ext_voters(lv_ches_2104),
                                per_ext_voters(lv_ches_2109),
                                per_ext_voters(lv_ches_2111),
                                per_ext_voters(lv_ches_2113),
                                per_ext_voters(lv_ches_2201),
                                per_ext_voters(lv_ches_2202),
                                per_ext_voters(lv_ches_2203),
                                per_ext_voters(lv_ches_2204),
                                per_ext_voters(lv_ches_2207),
                                per_ext_voters(lv_ches_2301),
                                per_ext_voters(lv_ches_2302),
                                per_ext_voters(lv_ches_2308),
                                per_ext_voters(lv_ches_2309),
                                per_ext_voters(lv_ches_2310),
                                per_ext_voters(lv_ches_2311),
                                per_ext_voters(lv_ches_2402),
                                per_ext_voters(lv_ches_2405),
                                per_ext_voters(lv_ches_2406),
                                per_ext_voters(lv_ches_2410),
                                per_ext_voters(lv_ches_2412),
                                per_ext_voters(lv_ches_2501),
                                per_ext_voters(lv_ches_2506),
                                per_ext_voters(lv_ches_2507),
                                per_ext_voters(lv_ches_2511),
                                per_ext_voters(lv_ches_2515),
                                per_ext_voters(lv_ches_2516),
                                per_ext_voters(lv_ches_2518),
                                per_ext_voters(lv_ches_2601),
                                per_ext_voters(lv_ches_2603),
                                per_ext_voters(lv_ches_2605),
                                per_ext_voters(lv_ches_2606),
                                per_ext_voters(lv_ches_2613),
                                per_ext_voters(lv_ches_2614),
                                per_ext_voters(lv_ches_2616),
                                per_ext_voters(lv_ches_2701),
                                per_ext_voters(lv_ches_2702),
                                per_ext_voters(lv_ches_2704),
                                per_ext_voters(lv_ches_2705),
                                per_ext_voters(lv_ches_2706),
                                per_ext_voters(lv_ches_2710),
                                per_ext_voters(lv_ches_2711),
                                per_ext_voters(lv_ches_2802),
                                per_ext_voters(lv_ches_2803),
                                per_ext_voters(lv_ches_2804),
                                per_ext_voters(lv_ches_2805),
                                per_ext_voters(lv_ches_2809),
                                per_ext_voters(lv_ches_2812),
                                per_ext_voters(lv_ches_2813),
                                per_ext_voters(lv_ches_2814),
                                per_ext_voters(lv_ches_2815),
                                per_ext_voters(lv_ches_2902),
                                per_ext_voters(lv_ches_2903),
                                per_ext_voters(lv_ches_2904),
                                per_ext_voters(lv_ches_2905),
                                per_ext_voters(lv_ches_2906),
                                per_ext_voters(lv_ches_2914),
                                per_ext_voters(lv_ches_3101),
                                per_ext_voters(lv_ches_3102),
                                per_ext_voters(lv_ches_3104),
                                per_ext_voters(lv_ches_3105),
                                per_ext_voters(lv_ches_3107),
                                per_ext_voters(lv_ches_3109),
                                per_ext_voters(lv_ches_3112),
                                per_ext_voters(lv_ches_3114),
                                per_ext_voters(lv_ches_3701),
                                per_ext_voters(lv_ches_3702),
                                per_ext_voters(lv_ches_3801),
                                per_ext_voters(lv_ches_3802),
                                per_ext_voters(lv_ches_3803),
                                per_ext_voters(lv_ches_3804),
                                per_ext_voters(lv_ches_3805),
                                per_ext_voters(lv_ches_3806),
                                per_ext_voters(lv_ches_4001),
                                per_ext_voters(lv_ches_4003),
                                per_ext_voters(lv_ches_4004),
                                per_ext_voters(lv_ches_4005),
                                per_ext_voters(lv_ches_4006))



party_vot_var$perext_eu_lv <- ifelse(is.nan(party_vot_var$perext_eu_lv) == TRUE, NA, party_vot_var$perext_eu_lv)










#############EU dimension: Average distance from country mean##########################################



library(DescTools) #using this function to get MAD https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/MeanAD

party_mad <- function(party, var, country) #MAD for likely voters of a party, on a variable, in a country
{
  country_mean <-  function(var, country) #country_mean calculates the mean on a variable in a country
  {
    x <- mean(var[countrycode == country], na.rm=TRUE)
    return(x)
  }
  y <- MeanAD(var[party == 1], FUN=country_mean(var, country), na.rm=TRUE) #MeanAD calculates the mean distance on a variable for likely voters in a country where the midpoint is the return from above, i.e. country mean. 
  return(y) 
}


party_vot_var$mad_eu_lv_countrymean <- c(party_mad(lv_ches_102, qpp18, 1056),
                                         party_mad(lv_ches_103, qpp18, 1056),
                                         party_mad(lv_ches_104, qpp18, 1056),
                                         party_mad(lv_ches_105, qpp18, 1056),
                                         party_mad(lv_ches_106, qpp18, 1056),
                                         party_mad(lv_ches_107, qpp18, 1056),
                                         party_mad(lv_ches_108, qpp18, 1056),
                                         party_mad(lv_ches_109, qpp18, 1056),
                                         party_mad(lv_ches_110, qpp18, 1056),
                                         party_mad(lv_ches_112, qpp18, 1056),
                                         party_mad(lv_ches_119, qpp18, 1056),
                                         party_mad(lv_ches_201, qpp18, 1208),
                                         party_mad(lv_ches_202, qpp18, 1208),
                                         party_mad(lv_ches_203, qpp18, 1208),
                                         party_mad(lv_ches_206, qpp18, 1208),
                                         party_mad(lv_ches_211, qpp18, 1208),
                                         party_mad(lv_ches_215, qpp18, 1208),
                                         party_mad(lv_ches_218, qpp18, 1208),
                                         party_mad(lv_ches_301, qpp18, 1276),
                                         party_mad(lv_ches_302, qpp18, 1276),
                                         party_mad(lv_ches_303, qpp18, 1276),
                                         party_mad(lv_ches_304, qpp18, 1276),
                                         party_mad(lv_ches_306, qpp18, 1276),
                                         party_mad(lv_ches_310, qpp18, 1276),
                                         party_mad(lv_ches_311, qpp18, 1276),
                                         party_mad(lv_ches_401, qpp18, 1300),
                                         party_mad(lv_ches_402, qpp18, 1300),
                                         party_mad(lv_ches_403, qpp18, 1300),
                                         party_mad(lv_ches_404, qpp18, 1300),
                                         party_mad(lv_ches_410, qpp18, 1300),
                                         party_mad(lv_ches_413, qpp18, 1300),
                                         party_mad(lv_ches_414, qpp18, 1300),
                                         party_mad(lv_ches_415, qpp18, 1300),
                                         party_mad(lv_ches_501, qpp18, 1724),
                                         party_mad(lv_ches_502, qpp18, 1724),
                                         party_mad(lv_ches_504, qpp18, 1724),
                                         party_mad(lv_ches_505, qpp18, 1724),
                                         party_mad(lv_ches_506, qpp18, 1724),
                                         party_mad(lv_ches_511, qpp18, 1724),
                                         party_mad(lv_ches_513, qpp18, 1724),
                                         party_mad(lv_ches_517, qpp18, 1724),
                                         party_mad(lv_ches_523, qpp18, 1724),
                                         party_mad(lv_ches_525, qpp18, 1724),
                                         party_mad(lv_ches_601, qpp18, 1250),
                                         party_mad(lv_ches_602, qpp18, 1250),
                                         party_mad(lv_ches_605, qpp18, 1250),
                                         party_mad(lv_ches_609, qpp18, 1250),
                                         party_mad(lv_ches_610, qpp18, 1250),
                                         party_mad(lv_ches_613, qpp18, 1250),
                                         party_mad(lv_ches_701, qpp18, 1372),
                                         party_mad(lv_ches_702, qpp18, 1372),
                                         party_mad(lv_ches_703, qpp18, 1372),
                                         party_mad(lv_ches_705, qpp18, 1372),
                                         party_mad(lv_ches_707, qpp18, 1372),
                                         party_mad(lv_ches_708, qpp18, 1372),
                                         party_mad(lv_ches_811, qpp18, 1380),
                                         party_mad(lv_ches_814, qpp18, 1380),
                                         party_mad(lv_ches_815, qpp18, 1380),
                                         party_mad(lv_ches_827, qpp18, 1380),
                                         party_mad(lv_ches_837, qpp18, 1380),
                                         party_mad(lv_ches_838, qpp18, 1380),
                                         party_mad(lv_ches_844, qpp18, 1380),
                                         party_mad(lv_ches_845, qpp18, 1380),
                                         party_mad(lv_ches_848, qpp18, 1380),
                                         party_mad(lv_ches_1001, qpp18, 1528),
                                         party_mad(lv_ches_1002, qpp18, 1528),
                                         party_mad(lv_ches_1003, qpp18, 1528),
                                         party_mad(lv_ches_1004, qpp18, 1528),
                                         party_mad(lv_ches_1005, qpp18, 1528),
                                         party_mad(lv_ches_1014, qpp18, 1528),
                                         party_mad(lv_ches_1016, qpp18, 1528),
                                         party_mad(lv_ches_1017, qpp18, 1528),
                                         party_mad(lv_ches_1018, qpp18, 1528),
                                         party_mad(lv_ches_1101, qpp18, 1826),
                                         party_mad(lv_ches_1102, qpp18, 1826),
                                         party_mad(lv_ches_1104, qpp18, 1826),
                                         party_mad(lv_ches_1105, qpp18, 1826),
                                         party_mad(lv_ches_1106, qpp18, 1826),
                                         party_mad(lv_ches_1107, qpp18, 1826),
                                         party_mad(lv_ches_1108, qpp18, 1826),
                                         party_mad(lv_ches_1201, qpp18, 1620),
                                         party_mad(lv_ches_1202, qpp18, 1620),
                                         party_mad(lv_ches_1205, qpp18, 1620),
                                         party_mad(lv_ches_1206, qpp18, 1620),
                                         party_mad(lv_ches_1208, qpp18, 1620),
                                         party_mad(lv_ches_1209, qpp18, 1620),
                                         party_mad(lv_ches_1302, qpp18, 1040),
                                         party_mad(lv_ches_1303, qpp18, 1040),
                                         party_mad(lv_ches_1304, qpp18, 1040),
                                         party_mad(lv_ches_1307, qpp18, 1040),
                                         party_mad(lv_ches_1309, qpp18, 1040),
                                         party_mad(lv_ches_1401, qpp18, 1246),
                                         party_mad(lv_ches_1402, qpp18, 1246),
                                         party_mad(lv_ches_1403, qpp18, 1246),
                                         party_mad(lv_ches_1404, qpp18, 1246),
                                         party_mad(lv_ches_1405, qpp18, 1246),
                                         party_mad(lv_ches_1406, qpp18, 1246),
                                         party_mad(lv_ches_1408, qpp18, 1246),
                                         party_mad(lv_ches_1409, qpp18, 1246),
                                         party_mad(lv_ches_1601, qpp18, 1752),
                                         party_mad(lv_ches_1602, qpp18, 1752),
                                         party_mad(lv_ches_1603, qpp18, 1752),
                                         party_mad(lv_ches_1604, qpp18, 1752),
                                         party_mad(lv_ches_1605, qpp18, 1752),
                                         party_mad(lv_ches_1606, qpp18, 1752),
                                         party_mad(lv_ches_1607, qpp18, 1752),
                                         party_mad(lv_ches_1610, qpp18, 1752),
                                         party_mad(lv_ches_1611, qpp18, 1752),
                                         party_mad(lv_ches_1612, qpp18, 1752),
                                         party_mad(lv_ches_2002, qpp18, 1100),
                                         party_mad(lv_ches_2004, qpp18, 1100),
                                         party_mad(lv_ches_2007, qpp18, 1100),
                                         party_mad(lv_ches_2010, qpp18, 1100),
                                         party_mad(lv_ches_2015, qpp18, 1100),
                                         party_mad(lv_ches_2016, qpp18, 1100),
                                         party_mad(lv_ches_2101, qpp18, 1203),
                                         party_mad(lv_ches_2103, qpp18, 1203),
                                         party_mad(lv_ches_2104, qpp18, 1203),
                                         party_mad(lv_ches_2109, qpp18, 1203),
                                         party_mad(lv_ches_2111, qpp18, 1203),
                                         party_mad(lv_ches_2113, qpp18, 1203),
                                         party_mad(lv_ches_2201, qpp18, 1233),
                                         party_mad(lv_ches_2202, qpp18, 1233),
                                         party_mad(lv_ches_2203, qpp18, 1233),
                                         party_mad(lv_ches_2204, qpp18, 1233),
                                         party_mad(lv_ches_2207, qpp18, 1233),
                                         party_mad(lv_ches_2301, qpp18, 1348),
                                         party_mad(lv_ches_2302, qpp18, 1348),
                                         party_mad(lv_ches_2308, qpp18, 1348),
                                         party_mad(lv_ches_2309, qpp18, 1348),
                                         party_mad(lv_ches_2310, qpp18, 1348),
                                         party_mad(lv_ches_2311, qpp18, 1348),
                                         party_mad(lv_ches_2402, qpp18, 1428),
                                         party_mad(lv_ches_2405, qpp18, 1428),
                                         party_mad(lv_ches_2406, qpp18, 1428),
                                         party_mad(lv_ches_2410, qpp18, 1428),
                                         party_mad(lv_ches_2412, qpp18, 1428),
                                         party_mad(lv_ches_2501, qpp18, 1440),
                                         party_mad(lv_ches_2506, qpp18, 1440),
                                         party_mad(lv_ches_2507, qpp18, 1440),
                                         party_mad(lv_ches_2511, qpp18, 1440),
                                         party_mad(lv_ches_2515, qpp18, 1440),
                                         party_mad(lv_ches_2516, qpp18, 1440),
                                         party_mad(lv_ches_2518, qpp18, 1440),
                                         party_mad(lv_ches_2601, qpp18, 1616),
                                         party_mad(lv_ches_2603, qpp18, 1616),
                                         party_mad(lv_ches_2605, qpp18, 1616),
                                         party_mad(lv_ches_2606, qpp18, 1616),
                                         party_mad(lv_ches_2613, qpp18, 1616),
                                         party_mad(lv_ches_2614, qpp18, 1616),
                                         party_mad(lv_ches_2616, qpp18, 1616),
                                         party_mad(lv_ches_2701, qpp18, 1642),
                                         party_mad(lv_ches_2702, qpp18, 1642),
                                         party_mad(lv_ches_2704, qpp18, 1642),
                                         party_mad(lv_ches_2705, qpp18, 1642),
                                         party_mad(lv_ches_2706, qpp18, 1642),
                                         party_mad(lv_ches_2710, qpp18, 1642),
                                         party_mad(lv_ches_2711, qpp18, 1642),
                                         party_mad(lv_ches_2802, qpp18, 1703),
                                         party_mad(lv_ches_2803, qpp18, 1703),
                                         party_mad(lv_ches_2804, qpp18, 1703),
                                         party_mad(lv_ches_2805, qpp18, 1703),
                                         party_mad(lv_ches_2809, qpp18, 1703),
                                         party_mad(lv_ches_2812, qpp18, 1703),
                                         party_mad(lv_ches_2813, qpp18, 1703),
                                         party_mad(lv_ches_2814, qpp18, 1703),
                                         party_mad(lv_ches_2815, qpp18, 1703),
                                         party_mad(lv_ches_2902, qpp18, 1705),
                                         party_mad(lv_ches_2903, qpp18, 1705),
                                         party_mad(lv_ches_2904, qpp18, 1705),
                                         party_mad(lv_ches_2905, qpp18, 1705),
                                         party_mad(lv_ches_2906, qpp18, 1705),
                                         party_mad(lv_ches_2914, qpp18, 1705),
                                         party_mad(lv_ches_3101, qpp18, 1191),
                                         party_mad(lv_ches_3102, qpp18, 1191),
                                         party_mad(lv_ches_3104, qpp18, 1191),
                                         party_mad(lv_ches_3105, qpp18, 1191),
                                         party_mad(lv_ches_3107, qpp18, 1191),
                                         party_mad(lv_ches_3109, qpp18, 1191),
                                         party_mad(lv_ches_3112, qpp18, 1191),
                                         party_mad(lv_ches_3114, qpp18, 1191),
                                         party_mad(lv_ches_3701, qpp18, 1470),
                                         party_mad(lv_ches_3702, qpp18, 1470),
                                         party_mad(lv_ches_3801, qpp18, 1442),
                                         party_mad(lv_ches_3802, qpp18, 1442),
                                         party_mad(lv_ches_3803, qpp18, 1442),
                                         party_mad(lv_ches_3804, qpp18, 1442),
                                         party_mad(lv_ches_3805, qpp18, 1442),
                                         party_mad(lv_ches_3806, qpp18, 1442),
                                         party_mad(lv_ches_4001, qpp18, 1196),
                                         party_mad(lv_ches_4003, qpp18, 1196),
                                         party_mad(lv_ches_4004, qpp18, 1196),
                                         party_mad(lv_ches_4005, qpp18, 1196),
                                         party_mad(lv_ches_4006, qpp18, 1196))

detach("package:DescTools", unload=TRUE)

party_vot_var$mad_eu_lv_countrymean <- ifelse(is.nan(party_vot_var$mad_eu_lv_countrymean) == TRUE, NA, party_vot_var$mad_eu_lv_countrymean)






#############Other variables: overlap######################
#which(colnames(EES_2014)=="lv_ches_102") #seeing first and last column numbers
#which(colnames(EES_2014)=="lv_ches_4006")
attach(EES_2014)

EES_2014$vot_overl <- NA
EES_2014$vot_overl <- rowSums(EES_2014[426:622], na.rm=TRUE)
describe(EES_2014$vot_overl)
#EES_2014$vot_overl <- apply(EES_2014[,426:622],na.rm=TRUE, 1, sum) same but with apply. 

attach(EES_2014)
#describe(EES_2014$vot_overl)
party_vot_var$vot_overl <- NA
party_vot_var$vot_overl <- c(mean(vot_overl[lv_ches_102 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_103 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_104 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_105 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_106 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_107 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_108 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_109 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_110 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_112 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_119 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_201 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_202 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_203 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_206 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_211 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_215 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_218 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_301 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_302 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_303 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_304 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_306 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_310 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_311 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_401 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_402 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_403 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_404 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_410 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_413 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_414 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_415 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_501 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_502 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_504 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_505 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_506 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_511 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_513 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_517 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_523 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_525 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_601 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_602 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_605 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_609 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_610 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_613 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_701 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_702 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_703 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_705 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_707 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_708 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_811 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_814 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_815 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_827 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_837 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_838 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_844 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_845 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_848 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1001 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1002 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1003 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1004 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1005 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1014 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1016 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1017 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1018 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1101 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1102 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1104 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1105 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1106 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1107 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1108 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1201 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1202 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1205 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1206 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1208 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1209 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1302 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1303 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1304 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1307 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1309 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1401 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1402 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1403 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1404 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1405 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1406 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1408 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1409 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1601 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1602 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1603 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1604 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1605 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1606 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1607 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1610 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1611 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_1612 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2002 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2004 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2007 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2010 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2015 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2016 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2101 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2103 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2104 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2109 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2111 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2113 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2201 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2202 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2203 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2204 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2207 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2301 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2302 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2308 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2309 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2310 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2311 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2402 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2405 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2406 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2410 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2412 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2501 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2506 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2507 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2511 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2515 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2516 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2518 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2601 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2603 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2605 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2606 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2613 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2614 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2616 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2701 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2702 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2704 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2705 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2706 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2710 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2711 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2802 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2803 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2804 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2805 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2809 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2812 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2813 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2814 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2815 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2902 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2903 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2904 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2905 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2906 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_2914 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3101 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3102 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3104 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3105 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3107 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3109 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3112 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3114 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3701 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3702 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3801 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3802 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3803 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3804 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3805 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_3806 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_4001 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_4003 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_4004 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_4005 == 1], na.rm = TRUE),
                              mean(vot_overl[lv_ches_4006 == 1], na.rm = TRUE))

party_vot_var$vot_overl <- ifelse(is.nan(party_vot_var$vot_overl) == TRUE, NA, party_vot_var$vot_overl)



#check if works
#mean(vot_overl[lv_ches_1001 == 1], na.rm = TRUE) #CDA
#mean(vot_overl[lv_ches_1002 == 1], na.rm = TRUE) #PvdA
#mean(vot_overl[lv_ches_1003 == 1], na.rm = TRUE) #VVD
#mean(vot_overl[lv_ches_1004 == 1], na.rm = TRUE) #D66
#mean(vot_overl[lv_ches_1005 == 1], na.rm = TRUE) #GL
#mean(vot_overl[lv_ches_1017 == 1], na.rm = TRUE) #PVV














#############Add labels to columns###########################

#var.labels = c(sd_glob_lv="Division of a party's voters on globalization measured by SD of likely voter position", mad_glob_lv="Division of a party's voters on globalization measured by MAD of likely voter position", mean_glob_lv="mean position on globalization of the likely voters of a party", perext_glob_lv="percentage of voters of a party that selected a 0 or 10 on globalization", mad_glob_lv_countrymean="mad on globalization compared to country mean, i.e. average distance from country mean for likely voters")
#label(party_vot_var) = as.list(var.labels[match(names(party_vot_var), names(var.labels))])
#label(party_vot_var)
#rm(x, lv_column_names_matrix, lv_column_names_string, var.labels)






#############Unique parties (should be 194)#######
#   [1]  102  103  104  105  106  107  108  109  110  112  119  201  202  203  206  211  215  218  301  302  303  304
#[23]  306  310  311  401  402  403  404  410  413  414  415  501  502  504  505  506  511  513  517  523  525  601
#[45]  602  605  609  610  613  701  702  703  705  707  708  811  814  815  827  837  838  844  845  848 1001 1002
#[67] 1003 1004 1005 1014 1016 1017 1018 1101 1102 1104 1105 1106 1107 1108 1201 1202 1205 1206 1208 1209 1302 1303
#[89] 1304 1307 1309 1401 1402 1403 1404 1405 1406 1408 1409 1601 1602 1603 1604 1605 1606 1607 1610 1611 1612 2002
#[111] 2004 2007 2010 2015 2016 2101 2103 2104 2109 2111 2113 2201 2202 2203 2204 2207 2301 2302 2308 2309 2310 2311
#[133] 2402 2405 2406 2410 2412 2501 2506 2507 2511 2515 2516 2518 2601 2603 2605 2606 2613 2614 2616 2701 2702 2704
#[155] 2705 2706 2710 2711 2802 2803 2804 2805 2809 2812 2813 2814 2815 2902 2903 2904 2905 2906 2914 3101 3102 3104
#[177] 3105 3107 3109 3112 3114 3701 3702 3801 3802 3803 3804 3805 3806 4001 4003 4004 4005 4006





####Only way to make the long lists shorter:
#create a subset of the database with only likely voter for a party and then do the different variables for each party.
#wont be that much more efficient. 













#############Most important issue################
EES_2014$mii_immi <- 0
EES_2014$mii_immi[EES_2014$qpp1aO_EES== 5 | EES_2014$qpp1aO_EES== 3 | EES_2014$qpp1aO_EES== 72 |
                  EES_2014$qpp1aO_EES== 73 | EES_2014$qpp1aO_EES== 99 | EES_2014$qpp1aO_EES== 104 |
                  EES_2014$qpp1aO_EES== 105] <- 1

EES_2014$mii_eu <- 0
EES_2014$mii_eu[EES_2014$qpp1aO_EES== 1 | EES_2014$qpp1aO_EES== 3 | EES_2014$qpp1aO_EES== 39 |
                  EES_2014$qpp1aO_EES== 42 | EES_2014$qpp1aO_EES== 43 | EES_2014$qpp1aO_EES== 44 |
                  EES_2014$qpp1aO_EES== 45 | EES_2014$qpp1aO_EES== 46 | EES_2014$qpp1aO_EES== 47 |
                  EES_2014$qpp1aO_EES== 48 | EES_2014$qpp1aO_EES== 49 | EES_2014$qpp1aO_EES== 50 |
                  EES_2014$qpp1aO_EES== 84] <- 1

###second most important issue. 
EES_2014$mii_immi[EES_2014$qpp1bO_EES== 5 | EES_2014$qpp1bO_EES== 3 | EES_2014$qpp1bO_EES== 72 |
                    EES_2014$qpp1bO_EES== 73 | EES_2014$qpp1bO_EES== 99 | EES_2014$qpp1bO_EES== 104 |
                    EES_2014$qpp1bO_EES== 105] <- 1

EES_2014$mii_eu <- 0
EES_2014$mii_eu[EES_2014$qpp1bO_EES== 1 | EES_2014$qpp1bO_EES== 3 | EES_2014$qpp1bO_EES== 39 |
                  EES_2014$qpp1bO_EES== 42 | EES_2014$qpp1bO_EES== 43 | EES_2014$qpp1bO_EES== 44 |
                  EES_2014$qpp1bO_EES== 45 | EES_2014$qpp1bO_EES== 46 | EES_2014$qpp1bO_EES== 47 |
                  EES_2014$qpp1bO_EES== 48 | EES_2014$qpp1bO_EES== 49 | EES_2014$qpp1bO_EES== 50 |
                  EES_2014$qpp1bO_EES== 84] <- 1





#function for the percentage of likely voters of a party that have either immigration or European integration as MII
#take the sum of all not NA values for a question. This is the total n for that question. Then take all extreme values
#for that same question. Do this three times for both and calculate percentage extreme scores.
#note that not all people answering all three questions is taken into account by taking separate Ns for each question
#the lvches is the name for columns with dummies for likely voters.
per_mii_immi <- function(lvches)
{
  n1 = sum(!is.na((as.numeric(mii_immi[lvches == 1]))))
  t1 = sum(!is.na((as.numeric(mii_immi[lvches == 1 & (mii_immi == 1) ]))))
  pext <- (t1/n1) * 100
  return(pext)
}


per_mii_eu <- function(lvches)
{
  n1 = sum(!is.na((as.numeric(mii_eu[lvches == 1]))))
  t1 = sum(!is.na((as.numeric(mii_eu[lvches == 1 & (mii_eu == 1) ]))))
  pext <- (t1/n1) * 100
  return(pext)
}


attach(EES_2014)


####################Adding the code for all parties. Note that, just like above, all NAs are for Eastern European countries
party_vot_var$mii_immi_lv <- c(per_mii_immi(lv_ches_102),
                                  per_mii_immi(lv_ches_103),
                                  per_mii_immi(lv_ches_104),
                                  per_mii_immi(lv_ches_105),
                                  per_mii_immi(lv_ches_106),
                                  per_mii_immi(lv_ches_107),
                                  per_mii_immi(lv_ches_108),
                                  per_mii_immi(lv_ches_109),
                                  per_mii_immi(lv_ches_110),
                                  per_mii_immi(lv_ches_112),
                                  per_mii_immi(lv_ches_119),
                                  per_mii_immi(lv_ches_201),
                                  per_mii_immi(lv_ches_202),
                                  per_mii_immi(lv_ches_203),
                                  per_mii_immi(lv_ches_206),
                                  per_mii_immi(lv_ches_211),
                                  per_mii_immi(lv_ches_215),
                                  per_mii_immi(lv_ches_218),
                                  per_mii_immi(lv_ches_301),
                                  per_mii_immi(lv_ches_302),
                                  per_mii_immi(lv_ches_303),
                                  per_mii_immi(lv_ches_304),
                                  per_mii_immi(lv_ches_306),
                                  per_mii_immi(lv_ches_310),
                                  per_mii_immi(lv_ches_311),
                                  per_mii_immi(lv_ches_401),
                                  per_mii_immi(lv_ches_402),
                                  per_mii_immi(lv_ches_403),
                                  per_mii_immi(lv_ches_404),
                                  per_mii_immi(lv_ches_410),
                                  per_mii_immi(lv_ches_413),
                                  per_mii_immi(lv_ches_414),
                                  per_mii_immi(lv_ches_415),
                                  per_mii_immi(lv_ches_501),
                                  per_mii_immi(lv_ches_502),
                                  per_mii_immi(lv_ches_504),
                                  per_mii_immi(lv_ches_505),
                                  per_mii_immi(lv_ches_506),
                                  per_mii_immi(lv_ches_511),
                                  per_mii_immi(lv_ches_513),
                                  per_mii_immi(lv_ches_517),
                                  per_mii_immi(lv_ches_523),
                                  per_mii_immi(lv_ches_525),
                                  per_mii_immi(lv_ches_601),
                                  per_mii_immi(lv_ches_602),
                                  per_mii_immi(lv_ches_605),
                                  per_mii_immi(lv_ches_609),
                                  per_mii_immi(lv_ches_610),
                                  per_mii_immi(lv_ches_613),
                                  per_mii_immi(lv_ches_701),
                                  per_mii_immi(lv_ches_702),
                                  per_mii_immi(lv_ches_703),
                                  per_mii_immi(lv_ches_705),
                                  per_mii_immi(lv_ches_707),
                                  per_mii_immi(lv_ches_708),
                                  per_mii_immi(lv_ches_811),
                                  per_mii_immi(lv_ches_814),
                                  per_mii_immi(lv_ches_815),
                                  per_mii_immi(lv_ches_827),
                                  per_mii_immi(lv_ches_837),
                                  per_mii_immi(lv_ches_838),
                                  per_mii_immi(lv_ches_844),
                                  per_mii_immi(lv_ches_845),
                                  per_mii_immi(lv_ches_848),
                                  per_mii_immi(lv_ches_1001),
                                  per_mii_immi(lv_ches_1002),
                                  per_mii_immi(lv_ches_1003),
                                  per_mii_immi(lv_ches_1004),
                                  per_mii_immi(lv_ches_1005),
                                  per_mii_immi(lv_ches_1014),
                                  per_mii_immi(lv_ches_1016),
                                  per_mii_immi(lv_ches_1017),
                                  per_mii_immi(lv_ches_1018),
                                  per_mii_immi(lv_ches_1101),
                                  per_mii_immi(lv_ches_1102),
                                  per_mii_immi(lv_ches_1104),
                                  per_mii_immi(lv_ches_1105),
                                  per_mii_immi(lv_ches_1106),
                                  per_mii_immi(lv_ches_1107),
                                  per_mii_immi(lv_ches_1108),
                                  per_mii_immi(lv_ches_1201),
                                  per_mii_immi(lv_ches_1202),
                                  per_mii_immi(lv_ches_1205),
                                  per_mii_immi(lv_ches_1206),
                                  per_mii_immi(lv_ches_1208),
                                  per_mii_immi(lv_ches_1209),
                                  per_mii_immi(lv_ches_1302),
                                  per_mii_immi(lv_ches_1303),
                                  per_mii_immi(lv_ches_1304),
                                  per_mii_immi(lv_ches_1307),
                                  per_mii_immi(lv_ches_1309),
                                  per_mii_immi(lv_ches_1401),
                                  per_mii_immi(lv_ches_1402),
                                  per_mii_immi(lv_ches_1403),
                                  per_mii_immi(lv_ches_1404),
                                  per_mii_immi(lv_ches_1405),
                                  per_mii_immi(lv_ches_1406),
                                  per_mii_immi(lv_ches_1408),
                                  per_mii_immi(lv_ches_1409),
                                  per_mii_immi(lv_ches_1601),
                                  per_mii_immi(lv_ches_1602),
                                  per_mii_immi(lv_ches_1603),
                                  per_mii_immi(lv_ches_1604),
                                  per_mii_immi(lv_ches_1605),
                                  per_mii_immi(lv_ches_1606),
                                  per_mii_immi(lv_ches_1607),
                                  per_mii_immi(lv_ches_1610),
                                  per_mii_immi(lv_ches_1611),
                                  per_mii_immi(lv_ches_1612),
                                  per_mii_immi(lv_ches_2002),
                                  per_mii_immi(lv_ches_2004),
                                  per_mii_immi(lv_ches_2007),
                                  per_mii_immi(lv_ches_2010),
                                  per_mii_immi(lv_ches_2015),
                                  per_mii_immi(lv_ches_2016),
                                  per_mii_immi(lv_ches_2101),
                                  per_mii_immi(lv_ches_2103),
                                  per_mii_immi(lv_ches_2104),
                                  per_mii_immi(lv_ches_2109),
                                  per_mii_immi(lv_ches_2111),
                                  per_mii_immi(lv_ches_2113),
                                  per_mii_immi(lv_ches_2201),
                                  per_mii_immi(lv_ches_2202),
                                  per_mii_immi(lv_ches_2203),
                                  per_mii_immi(lv_ches_2204),
                                  per_mii_immi(lv_ches_2207),
                                  per_mii_immi(lv_ches_2301),
                                  per_mii_immi(lv_ches_2302),
                                  per_mii_immi(lv_ches_2308),
                                  per_mii_immi(lv_ches_2309),
                                  per_mii_immi(lv_ches_2310),
                                  per_mii_immi(lv_ches_2311),
                                  per_mii_immi(lv_ches_2402),
                                  per_mii_immi(lv_ches_2405),
                                  per_mii_immi(lv_ches_2406),
                                  per_mii_immi(lv_ches_2410),
                                  per_mii_immi(lv_ches_2412),
                                  per_mii_immi(lv_ches_2501),
                                  per_mii_immi(lv_ches_2506),
                                  per_mii_immi(lv_ches_2507),
                                  per_mii_immi(lv_ches_2511),
                                  per_mii_immi(lv_ches_2515),
                                  per_mii_immi(lv_ches_2516),
                                  per_mii_immi(lv_ches_2518),
                                  per_mii_immi(lv_ches_2601),
                                  per_mii_immi(lv_ches_2603),
                                  per_mii_immi(lv_ches_2605),
                                  per_mii_immi(lv_ches_2606),
                                  per_mii_immi(lv_ches_2613),
                                  per_mii_immi(lv_ches_2614),
                                  per_mii_immi(lv_ches_2616),
                                  per_mii_immi(lv_ches_2701),
                                  per_mii_immi(lv_ches_2702),
                                  per_mii_immi(lv_ches_2704),
                                  per_mii_immi(lv_ches_2705),
                                  per_mii_immi(lv_ches_2706),
                                  per_mii_immi(lv_ches_2710),
                                  per_mii_immi(lv_ches_2711),
                                  per_mii_immi(lv_ches_2802),
                                  per_mii_immi(lv_ches_2803),
                                  per_mii_immi(lv_ches_2804),
                                  per_mii_immi(lv_ches_2805),
                                  per_mii_immi(lv_ches_2809),
                                  per_mii_immi(lv_ches_2812),
                                  per_mii_immi(lv_ches_2813),
                                  per_mii_immi(lv_ches_2814),
                                  per_mii_immi(lv_ches_2815),
                                  per_mii_immi(lv_ches_2902),
                                  per_mii_immi(lv_ches_2903),
                                  per_mii_immi(lv_ches_2904),
                                  per_mii_immi(lv_ches_2905),
                                  per_mii_immi(lv_ches_2906),
                                  per_mii_immi(lv_ches_2914),
                                  per_mii_immi(lv_ches_3101),
                                  per_mii_immi(lv_ches_3102),
                                  per_mii_immi(lv_ches_3104),
                                  per_mii_immi(lv_ches_3105),
                                  per_mii_immi(lv_ches_3107),
                                  per_mii_immi(lv_ches_3109),
                                  per_mii_immi(lv_ches_3112),
                                  per_mii_immi(lv_ches_3114),
                                  per_mii_immi(lv_ches_3701),
                                  per_mii_immi(lv_ches_3702),
                                  per_mii_immi(lv_ches_3801),
                                  per_mii_immi(lv_ches_3802),
                                  per_mii_immi(lv_ches_3803),
                                  per_mii_immi(lv_ches_3804),
                                  per_mii_immi(lv_ches_3805),
                                  per_mii_immi(lv_ches_3806),
                                  per_mii_immi(lv_ches_4001),
                                  per_mii_immi(lv_ches_4003),
                                  per_mii_immi(lv_ches_4004),
                                  per_mii_immi(lv_ches_4005),
                                  per_mii_immi(lv_ches_4006))



party_vot_var$mii_immi_lv <- ifelse(is.nan(party_vot_var$mii_immi_lv) == TRUE, NA, party_vot_var$mii_immi_lv)



###same for EU
party_vot_var$mii_eu_lv <- c(per_mii_eu(lv_ches_102),
                               per_mii_eu(lv_ches_103),
                               per_mii_eu(lv_ches_104),
                               per_mii_eu(lv_ches_105),
                               per_mii_eu(lv_ches_106),
                               per_mii_eu(lv_ches_107),
                               per_mii_eu(lv_ches_108),
                               per_mii_eu(lv_ches_109),
                               per_mii_eu(lv_ches_110),
                               per_mii_eu(lv_ches_112),
                               per_mii_eu(lv_ches_119),
                               per_mii_eu(lv_ches_201),
                               per_mii_eu(lv_ches_202),
                               per_mii_eu(lv_ches_203),
                               per_mii_eu(lv_ches_206),
                               per_mii_eu(lv_ches_211),
                               per_mii_eu(lv_ches_215),
                               per_mii_eu(lv_ches_218),
                               per_mii_eu(lv_ches_301),
                               per_mii_eu(lv_ches_302),
                               per_mii_eu(lv_ches_303),
                               per_mii_eu(lv_ches_304),
                               per_mii_eu(lv_ches_306),
                               per_mii_eu(lv_ches_310),
                               per_mii_eu(lv_ches_311),
                               per_mii_eu(lv_ches_401),
                               per_mii_eu(lv_ches_402),
                               per_mii_eu(lv_ches_403),
                               per_mii_eu(lv_ches_404),
                               per_mii_eu(lv_ches_410),
                               per_mii_eu(lv_ches_413),
                               per_mii_eu(lv_ches_414),
                               per_mii_eu(lv_ches_415),
                               per_mii_eu(lv_ches_501),
                               per_mii_eu(lv_ches_502),
                               per_mii_eu(lv_ches_504),
                               per_mii_eu(lv_ches_505),
                               per_mii_eu(lv_ches_506),
                               per_mii_eu(lv_ches_511),
                               per_mii_eu(lv_ches_513),
                               per_mii_eu(lv_ches_517),
                               per_mii_eu(lv_ches_523),
                               per_mii_eu(lv_ches_525),
                               per_mii_eu(lv_ches_601),
                               per_mii_eu(lv_ches_602),
                               per_mii_eu(lv_ches_605),
                               per_mii_eu(lv_ches_609),
                               per_mii_eu(lv_ches_610),
                               per_mii_eu(lv_ches_613),
                               per_mii_eu(lv_ches_701),
                               per_mii_eu(lv_ches_702),
                               per_mii_eu(lv_ches_703),
                               per_mii_eu(lv_ches_705),
                               per_mii_eu(lv_ches_707),
                               per_mii_eu(lv_ches_708),
                               per_mii_eu(lv_ches_811),
                               per_mii_eu(lv_ches_814),
                               per_mii_eu(lv_ches_815),
                               per_mii_eu(lv_ches_827),
                               per_mii_eu(lv_ches_837),
                               per_mii_eu(lv_ches_838),
                               per_mii_eu(lv_ches_844),
                               per_mii_eu(lv_ches_845),
                               per_mii_eu(lv_ches_848),
                               per_mii_eu(lv_ches_1001),
                               per_mii_eu(lv_ches_1002),
                               per_mii_eu(lv_ches_1003),
                               per_mii_eu(lv_ches_1004),
                               per_mii_eu(lv_ches_1005),
                               per_mii_eu(lv_ches_1014),
                               per_mii_eu(lv_ches_1016),
                               per_mii_eu(lv_ches_1017),
                               per_mii_eu(lv_ches_1018),
                               per_mii_eu(lv_ches_1101),
                               per_mii_eu(lv_ches_1102),
                               per_mii_eu(lv_ches_1104),
                               per_mii_eu(lv_ches_1105),
                               per_mii_eu(lv_ches_1106),
                               per_mii_eu(lv_ches_1107),
                               per_mii_eu(lv_ches_1108),
                               per_mii_eu(lv_ches_1201),
                               per_mii_eu(lv_ches_1202),
                               per_mii_eu(lv_ches_1205),
                               per_mii_eu(lv_ches_1206),
                               per_mii_eu(lv_ches_1208),
                               per_mii_eu(lv_ches_1209),
                               per_mii_eu(lv_ches_1302),
                               per_mii_eu(lv_ches_1303),
                               per_mii_eu(lv_ches_1304),
                               per_mii_eu(lv_ches_1307),
                               per_mii_eu(lv_ches_1309),
                               per_mii_eu(lv_ches_1401),
                               per_mii_eu(lv_ches_1402),
                               per_mii_eu(lv_ches_1403),
                               per_mii_eu(lv_ches_1404),
                               per_mii_eu(lv_ches_1405),
                               per_mii_eu(lv_ches_1406),
                               per_mii_eu(lv_ches_1408),
                               per_mii_eu(lv_ches_1409),
                               per_mii_eu(lv_ches_1601),
                               per_mii_eu(lv_ches_1602),
                               per_mii_eu(lv_ches_1603),
                               per_mii_eu(lv_ches_1604),
                               per_mii_eu(lv_ches_1605),
                               per_mii_eu(lv_ches_1606),
                               per_mii_eu(lv_ches_1607),
                               per_mii_eu(lv_ches_1610),
                               per_mii_eu(lv_ches_1611),
                               per_mii_eu(lv_ches_1612),
                               per_mii_eu(lv_ches_2002),
                               per_mii_eu(lv_ches_2004),
                               per_mii_eu(lv_ches_2007),
                               per_mii_eu(lv_ches_2010),
                               per_mii_eu(lv_ches_2015),
                               per_mii_eu(lv_ches_2016),
                               per_mii_eu(lv_ches_2101),
                               per_mii_eu(lv_ches_2103),
                               per_mii_eu(lv_ches_2104),
                               per_mii_eu(lv_ches_2109),
                               per_mii_eu(lv_ches_2111),
                               per_mii_eu(lv_ches_2113),
                               per_mii_eu(lv_ches_2201),
                               per_mii_eu(lv_ches_2202),
                               per_mii_eu(lv_ches_2203),
                               per_mii_eu(lv_ches_2204),
                               per_mii_eu(lv_ches_2207),
                               per_mii_eu(lv_ches_2301),
                               per_mii_eu(lv_ches_2302),
                               per_mii_eu(lv_ches_2308),
                               per_mii_eu(lv_ches_2309),
                               per_mii_eu(lv_ches_2310),
                               per_mii_eu(lv_ches_2311),
                               per_mii_eu(lv_ches_2402),
                               per_mii_eu(lv_ches_2405),
                               per_mii_eu(lv_ches_2406),
                               per_mii_eu(lv_ches_2410),
                               per_mii_eu(lv_ches_2412),
                               per_mii_eu(lv_ches_2501),
                               per_mii_eu(lv_ches_2506),
                               per_mii_eu(lv_ches_2507),
                               per_mii_eu(lv_ches_2511),
                               per_mii_eu(lv_ches_2515),
                               per_mii_eu(lv_ches_2516),
                               per_mii_eu(lv_ches_2518),
                               per_mii_eu(lv_ches_2601),
                               per_mii_eu(lv_ches_2603),
                               per_mii_eu(lv_ches_2605),
                               per_mii_eu(lv_ches_2606),
                               per_mii_eu(lv_ches_2613),
                               per_mii_eu(lv_ches_2614),
                               per_mii_eu(lv_ches_2616),
                               per_mii_eu(lv_ches_2701),
                               per_mii_eu(lv_ches_2702),
                               per_mii_eu(lv_ches_2704),
                               per_mii_eu(lv_ches_2705),
                               per_mii_eu(lv_ches_2706),
                               per_mii_eu(lv_ches_2710),
                               per_mii_eu(lv_ches_2711),
                               per_mii_eu(lv_ches_2802),
                               per_mii_eu(lv_ches_2803),
                               per_mii_eu(lv_ches_2804),
                               per_mii_eu(lv_ches_2805),
                               per_mii_eu(lv_ches_2809),
                               per_mii_eu(lv_ches_2812),
                               per_mii_eu(lv_ches_2813),
                               per_mii_eu(lv_ches_2814),
                               per_mii_eu(lv_ches_2815),
                               per_mii_eu(lv_ches_2902),
                               per_mii_eu(lv_ches_2903),
                               per_mii_eu(lv_ches_2904),
                               per_mii_eu(lv_ches_2905),
                               per_mii_eu(lv_ches_2906),
                               per_mii_eu(lv_ches_2914),
                               per_mii_eu(lv_ches_3101),
                               per_mii_eu(lv_ches_3102),
                               per_mii_eu(lv_ches_3104),
                               per_mii_eu(lv_ches_3105),
                               per_mii_eu(lv_ches_3107),
                               per_mii_eu(lv_ches_3109),
                               per_mii_eu(lv_ches_3112),
                               per_mii_eu(lv_ches_3114),
                               per_mii_eu(lv_ches_3701),
                               per_mii_eu(lv_ches_3702),
                               per_mii_eu(lv_ches_3801),
                               per_mii_eu(lv_ches_3802),
                               per_mii_eu(lv_ches_3803),
                               per_mii_eu(lv_ches_3804),
                               per_mii_eu(lv_ches_3805),
                               per_mii_eu(lv_ches_3806),
                               per_mii_eu(lv_ches_4001),
                               per_mii_eu(lv_ches_4003),
                               per_mii_eu(lv_ches_4004),
                               per_mii_eu(lv_ches_4005),
                               per_mii_eu(lv_ches_4006))



party_vot_var$mii_eu_lv <- ifelse(is.nan(party_vot_var$mii_eu_lv) == TRUE, NA, party_vot_var$mii_eu_lv)


















#############Testing likely voter assumption########



###for Left Right Dimension
#removing -99 and -8 from question
EES_2014$qpp14_1[EES_2014$qpp14_1 == -99 | EES_2014$qpp14_1 == -8] <- NA
EES_2014$qpp14_2[EES_2014$qpp14_2 == -99 | EES_2014$qpp14_2 == -8] <- NA
EES_2014$qpp14_3[EES_2014$qpp14_3 == -99 | EES_2014$qpp14_3 == -8] <- NA
EES_2014$qpp14_4[EES_2014$qpp14_4 == -99 | EES_2014$qpp14_4 == -8 | EES_2014$qpp14_4 == -7] <- NA
EES_2014$qpp14_5[EES_2014$qpp14_5 == -99 | EES_2014$qpp14_5 == -8 | EES_2014$qpp14_5 == -7] <- NA
EES_2014$qpp14_6[EES_2014$qpp14_6 == -99 | EES_2014$qpp14_6 == -8 | EES_2014$qpp14_6 == -7] <- NA
EES_2014$qpp14_7[EES_2014$qpp14_7 == -99 | EES_2014$qpp14_7 == -8 | EES_2014$qpp14_7 == -7 | EES_2014$qpp14_7 == -9] <- NA
EES_2014$qpp14_8[EES_2014$qpp14_8 == -99 | EES_2014$qpp14_8 == -8 | EES_2014$qpp14_8 == -7 | EES_2014$qpp14_8 == -9] <- NA



####adding sd of assessment of party positions on the left right dimensions of the likely voters of a party
attach(EES_2014)
party_vot_var$sd_lv_lefrig_ass <- c(sd(qpp14_2[lv_ches_102 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_103 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_104 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_105 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_106 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_107 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_108 == 1], na.rm = TRUE),
                              sd(qpp14_2[lv_ches_109 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_110 == 1], na.rm = TRUE),
                              sd(qpp14_7[lv_ches_112 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_119 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_201 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_202 == 1], na.rm = TRUE),
                              sd(qpp14_7[lv_ches_203 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_206 == 1], na.rm = TRUE),
                              sd(qpp14_2[lv_ches_211 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_215 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_218 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_301 == 1], na.rm = TRUE),
                              sd(qpp14_2[lv_ches_302 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_303 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_304 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_306 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_310 == 1], na.rm = TRUE),
                              sd(qpp14_7[lv_ches_311 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_401 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_402 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_403 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_404 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_410 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_413 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_414 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_415 == 1], na.rm = TRUE),
                              sd(qpp14_2[lv_ches_501 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_502 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_504 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_505 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_506 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_511 == 1], na.rm = TRUE),
                              NA,
                              sd(qpp14_5[lv_ches_517 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_523 == 1], na.rm = TRUE),
                              sd(qpp14_8[lv_ches_525 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_601 == 1], na.rm = TRUE),
                              sd(qpp14_2[lv_ches_602 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_605 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_609 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_610 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_613 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_701 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_702 == 1], na.rm = TRUE),
                              sd(qpp14_2[lv_ches_703 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_705 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_707 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_708 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_811 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_814 == 1], na.rm = TRUE),
                              sd(qpp14_2[lv_ches_815 == 1], na.rm = TRUE),
                              NA,
                              sd(qpp14_1[lv_ches_837 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_838 == 1], na.rm = TRUE),
                              sd(qpp14_8[lv_ches_844 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_845 == 1], na.rm = TRUE),
                              sd(qpp14_7[lv_ches_848 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_1001 == 1], na.rm = TRUE),
                              sd(qpp14_2[lv_ches_1002 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_1003 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_1004 == 1], na.rm = TRUE),
                              sd(qpp14_8[lv_ches_1005 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_1014 == 1], na.rm = TRUE),
                              sd(qpp14_7[lv_ches_1016 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_1017 == 1], na.rm = TRUE),
                              NA,
                              sd(qpp14_1[lv_ches_1101 == 1], na.rm = TRUE),
                              sd(qpp14_2[lv_ches_1102 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_1104 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_1105 == 1], na.rm = TRUE),
                              sd(qpp14_7[lv_ches_1106 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_1107 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_1108 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_1201 == 1], na.rm = TRUE),
                              sd(qpp14_2[lv_ches_1202 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_1205 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_1206 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_1208 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_1209 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_1302 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_1303 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_1304 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_1307 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_1309 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_1401 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_1402 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_1403 == 1], na.rm = TRUE),
                              sd(qpp14_7[lv_ches_1404 == 1], na.rm = TRUE),
                              sd(qpp14_8[lv_ches_1405 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_1406 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_1408 == 1], na.rm = TRUE),
                              sd(qpp14_2[lv_ches_1409 == 1], na.rm = TRUE),
                              sd(qpp14_8[lv_ches_1601 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_1602 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_1603 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_1604 == 1], na.rm = TRUE),
                              sd(qpp14_2[lv_ches_1605 == 1], na.rm = TRUE),
                              sd(qpp14_7[lv_ches_1606 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_1607 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_1610 == 1], na.rm = TRUE),
                              NA,
                              NA,
                              sd(qpp14_1[lv_ches_2002 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2004 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2007 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2010 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2015 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2016 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2101 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2103 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2104 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2109 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2111 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2113 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2201 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2202 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2203 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2204 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2207 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2301 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2302 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2308 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2309 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2310 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2311 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2402 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2405 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2406 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2410 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2412 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2501 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2506 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2507 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2511 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2515 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2516 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2518 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2601 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2603 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2605 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2606 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2613 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2614 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2616 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2701 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2702 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2704 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2705 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2706 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2710 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2711 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2802 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2803 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2804 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2805 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2809 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2812 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2813 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2814 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2815 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2902 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2903 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2904 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2905 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2906 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_2914 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_3101 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_3102 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_3104 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_3105 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_3107 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_3109 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_3112 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_3114 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_3701 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_3702 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_3801 == 1], na.rm = TRUE),
                              sd(qpp14_4[lv_ches_3802 == 1], na.rm = TRUE),
                              sd(qpp14_3[lv_ches_3803 == 1], na.rm = TRUE),
                              sd(qpp14_2[lv_ches_3804 == 1], na.rm = TRUE),
                              sd(qpp14_6[lv_ches_3805 == 1], na.rm = TRUE),
                              sd(qpp14_5[lv_ches_3806 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_4001 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_4003 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_4004 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_4005 == 1], na.rm = TRUE),
                              sd(qpp14_1[lv_ches_4006 == 1], na.rm = TRUE))




#now for those voters that are not likely voters. Note that we will get NAs for those places without a country code (Eastern Europe)
party_vot_var$sd_non_lv_lefrig_ass <- c(sd(qpp14_2[is.na(lv_ches_102) & countrycode == 1056 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_103) & countrycode == 1056 ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_104) & countrycode == 1056 ], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_105) & countrycode == 1056 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_106) & countrycode == 1056 ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_107) & countrycode == 1056 ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_108) & countrycode == 1056 ], na.rm = TRUE),
                                    sd(qpp14_2[is.na(lv_ches_109) & countrycode == 1056 ], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_110) & countrycode == 1056 ], na.rm = TRUE),
                                    sd(qpp14_7[is.na(lv_ches_112) & countrycode == 1056 ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_119) & countrycode == 1056 ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_201) & countrycode == 1208 ], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_202) & countrycode == 1208 ], na.rm = TRUE),
                                    sd(qpp14_7[is.na(lv_ches_203) & countrycode == 1208 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_206) & countrycode == 1208 ], na.rm = TRUE),
                                    sd(qpp14_2[is.na(lv_ches_211) & countrycode == 1208 ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_215) & countrycode == 1208 ], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_218) & countrycode == 1208 ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_301) & countrycode == 1276 ], na.rm = TRUE),
                                    sd(qpp14_2[is.na(lv_ches_302) & countrycode == 1276 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_303) & countrycode == 1276 ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_304) & countrycode == 1276 ], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_306) & countrycode == 1276 ], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_310) & countrycode == 1276 ], na.rm = TRUE),
                                    sd(qpp14_7[is.na(lv_ches_311) & countrycode == 1276 ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_401) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_402) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_403) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_404) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_410) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_413) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_414) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_415) ], na.rm = TRUE),
                                    sd(qpp14_2[is.na(lv_ches_501) & countrycode ==  1724], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_502) & countrycode ==  1724], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_504) & countrycode ==  1724], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_505) & countrycode ==  1724], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_506) & countrycode ==  1724], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_511) & countrycode ==  1724], na.rm = TRUE),
                                    NA,
                                    sd(qpp14_5[is.na(lv_ches_517) & countrycode ==  1724   ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_523) & countrycode ==  1724   ], na.rm = TRUE),
                                    sd(qpp14_8[is.na(lv_ches_525) & countrycode ==  1724   ], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_601) & countrycode ==  1250 ], na.rm = TRUE),
                                    sd(qpp14_2[is.na(lv_ches_602) & countrycode ==  1250 ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_605) & countrycode ==  1250 ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_609) & countrycode ==  1250 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_610) & countrycode ==  1250 ], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_613) & countrycode ==  1250 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_701) & countrycode ==  1372 ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_702) & countrycode ==  1372 ], na.rm = TRUE),
                                    sd(qpp14_2[is.na(lv_ches_703) & countrycode ==  1372 ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_705) & countrycode ==  1372 ], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_707) & countrycode ==  1372 ], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_708) & countrycode ==  1372 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_811) & countrycode ==  1380 ], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_814) & countrycode ==  1380 ], na.rm = TRUE),
                                    sd(qpp14_2[is.na(lv_ches_815) & countrycode ==  1380 ], na.rm = TRUE),
                                    NA,
                                    sd(qpp14_1[is.na(lv_ches_837) & countrycode ==  1380 ], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_838) & countrycode ==  1380 ], na.rm = TRUE),
                                    sd(qpp14_8[is.na(lv_ches_844) & countrycode ==  1380 ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_845) & countrycode ==  1380 ], na.rm = TRUE),
                                    sd(qpp14_7[is.na(lv_ches_848) & countrycode ==  1380 ], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_1001) & countrycode ==  1528 ], na.rm = TRUE),
                                    sd(qpp14_2[is.na(lv_ches_1002) & countrycode ==  1528 ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_1003) & countrycode ==  1528 ], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_1004) & countrycode ==  1528 ], na.rm = TRUE),
                                    sd(qpp14_8[is.na(lv_ches_1005) & countrycode ==  1528 ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_1014) & countrycode ==  1528 ], na.rm = TRUE),
                                    sd(qpp14_7[is.na(lv_ches_1016) & countrycode ==  1528 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_1017) & countrycode ==  1528 ], na.rm = TRUE),
                                    NA,
                                    sd(qpp14_1[is.na(lv_ches_1101) & countrycode ==  1826 ], na.rm = TRUE),
                                    sd(qpp14_2[is.na(lv_ches_1102) & countrycode ==  1826 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_1104) & countrycode ==  1826 ], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_1105) & countrycode ==  1826 ], na.rm = TRUE),
                                    sd(qpp14_7[is.na(lv_ches_1106) & countrycode ==  1826 ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_1107) & countrycode ==  1826 ], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_1108) & countrycode ==  1826 ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_1201) & countrycode ==  1620 ], na.rm = TRUE),
                                    sd(qpp14_2[is.na(lv_ches_1202) & countrycode ==  1620 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_1205) & countrycode ==  1620 ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_1206) & countrycode ==  1620 ], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_1208) & countrycode ==  1620 ], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_1209) & countrycode ==  1620 ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_1302) & countrycode ==  1040 ], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_1303) & countrycode ==  1040 ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_1304) & countrycode ==  1040 ], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_1307) & countrycode ==  1040 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_1309) & countrycode ==  1040 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_1401) & countrycode ==  1246 ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_1402) & countrycode ==  1246 ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_1403) & countrycode ==  1246 ], na.rm = TRUE),
                                    sd(qpp14_7[is.na(lv_ches_1404) & countrycode ==  1246 ], na.rm = TRUE),
                                    sd(qpp14_8[is.na(lv_ches_1405) & countrycode ==  1246 ], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_1406) & countrycode ==  1246 ], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_1408) & countrycode ==  1246 ], na.rm = TRUE),
                                    sd(qpp14_2[is.na(lv_ches_1409) & countrycode ==  1246 ], na.rm = TRUE),
                                    sd(qpp14_8[is.na(lv_ches_1601) & countrycode ==  1752 ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_1602) & countrycode ==  1752 ], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_1603) & countrycode ==  1752 ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_1604) & countrycode ==  1752 ], na.rm = TRUE),
                                    sd(qpp14_2[is.na(lv_ches_1605) & countrycode ==  1752 ], na.rm = TRUE),
                                    sd(qpp14_7[is.na(lv_ches_1606) & countrycode ==  1752 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_1607) & countrycode ==  1752 ], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_1610) & countrycode ==  1752 ], na.rm = TRUE),
                                    NA,
                                    NA,
                                    sd(qpp14_1[is.na(lv_ches_2002) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2004) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2007) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2010) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2015) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2016) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2101) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2103) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2104) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2109) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2111) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2113) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2201) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2202) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2203) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2204) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2207) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2301) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2302) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2308) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2309) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2310) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2311) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2402) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2405) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2406) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2410) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2412) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2501) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2506) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2507) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2511) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2515) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2516) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2518) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2601) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2603) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2605) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2606) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2613) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2614) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2616) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2701) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2702) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2704) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2705) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2706) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2710) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2711) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2802) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2803) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2804) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2805) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2809) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2812) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2813) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2814) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2815) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2902) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2903) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2904) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2905) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2906) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_2914) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_3101) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_3102) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_3104) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_3105) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_3107) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_3109) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_3112) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_3114) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_3701) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_3702) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_3801) ], na.rm = TRUE),
                                    sd(qpp14_4[is.na(lv_ches_3802) & countrycode ==  1442 ], na.rm = TRUE),
                                    sd(qpp14_3[is.na(lv_ches_3803) & countrycode ==  1442 ], na.rm = TRUE),
                                    sd(qpp14_2[is.na(lv_ches_3804) & countrycode ==  1442 ], na.rm = TRUE),
                                    sd(qpp14_6[is.na(lv_ches_3805) & countrycode ==  1442 ], na.rm = TRUE),
                                    sd(qpp14_5[is.na(lv_ches_3806) & countrycode ==  1442 ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_4001) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_4003) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_4004) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_4005) ], na.rm = TRUE),
                                    sd(qpp14_1[is.na(lv_ches_4006) ], na.rm = TRUE))



#####for European integration dimension
#removing -99 and -8 from question
EES_2014$qpp19_1[EES_2014$qpp19_1 == -99 | EES_2014$qpp19_1 == -7 | EES_2014$qpp19_1 == -8 | EES_2014$qpp19_1 == -9 | EES_2014$qpp19_1 == -999] <- NA
EES_2014$qpp19_2[EES_2014$qpp19_2 == -99 | EES_2014$qpp19_2 == -7 | EES_2014$qpp19_2 == -8 | EES_2014$qpp19_2 == -9 | EES_2014$qpp19_2 == -999] <- NA
EES_2014$qpp19_3[EES_2014$qpp19_3 == -99 | EES_2014$qpp19_3 == -7 | EES_2014$qpp19_3 == -8 | EES_2014$qpp19_3 == -9 | EES_2014$qpp19_3 == -999] <- NA
EES_2014$qpp19_4[EES_2014$qpp19_4 == -99 | EES_2014$qpp19_4 == -7 | EES_2014$qpp19_4 == -8 | EES_2014$qpp19_4 == -9 | EES_2014$qpp19_4 == -999] <- NA
EES_2014$qpp19_5[EES_2014$qpp19_5 == -99 | EES_2014$qpp19_5 == -7 | EES_2014$qpp19_5 == -8 | EES_2014$qpp19_5 == -9 | EES_2014$qpp19_5 == -999] <- NA
EES_2014$qpp19_6[EES_2014$qpp19_6 == -99 | EES_2014$qpp19_6 == -7 | EES_2014$qpp19_6 == -8 | EES_2014$qpp19_6 == -9 | EES_2014$qpp19_6 == -999] <- NA
EES_2014$qpp19_7[EES_2014$qpp19_7 == -99 | EES_2014$qpp19_7 == -7 | EES_2014$qpp19_7 == -8 | EES_2014$qpp19_7 == -9 | EES_2014$qpp19_7 == -999] <- NA
EES_2014$qpp19_8[EES_2014$qpp19_8 == -99 | EES_2014$qpp19_8 == -7 | EES_2014$qpp19_8 == -8 | EES_2014$qpp19_8 == -9 | EES_2014$qpp19_8 == -999] <- NA



#adding sd of likely voter assessment of a party on left right
attach(EES_2014)

party_vot_var$sd_lv_eu_ass <- c(sd(qpp19_2[lv_ches_102 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_103 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_104 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_105 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_106 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_107 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_108 == 1], na.rm = TRUE),
                                    sd(qpp19_2[lv_ches_109 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_110 == 1], na.rm = TRUE),
                                    sd(qpp19_7[lv_ches_112 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_119 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_201 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_202 == 1], na.rm = TRUE),
                                    sd(qpp19_7[lv_ches_203 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_206 == 1], na.rm = TRUE),
                                    sd(qpp19_2[lv_ches_211 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_215 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_218 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_301 == 1], na.rm = TRUE),
                                    sd(qpp19_2[lv_ches_302 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_303 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_304 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_306 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_310 == 1], na.rm = TRUE),
                                    sd(qpp19_7[lv_ches_311 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_401 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_402 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_403 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_404 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_410 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_413 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_414 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_415 == 1], na.rm = TRUE),
                                    sd(qpp19_2[lv_ches_501 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_502 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_504 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_505 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_506 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_511 == 1], na.rm = TRUE),
                                    NA,
                                    sd(qpp19_5[lv_ches_517 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_523 == 1], na.rm = TRUE),
                                    sd(qpp19_8[lv_ches_525 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_601 == 1], na.rm = TRUE),
                                    sd(qpp19_2[lv_ches_602 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_605 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_609 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_610 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_613 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_701 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_702 == 1], na.rm = TRUE),
                                    sd(qpp19_2[lv_ches_703 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_705 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_707 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_708 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_811 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_814 == 1], na.rm = TRUE),
                                    sd(qpp19_2[lv_ches_815 == 1], na.rm = TRUE),
                                    NA,
                                    sd(qpp19_1[lv_ches_837 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_838 == 1], na.rm = TRUE),
                                    sd(qpp19_8[lv_ches_844 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_845 == 1], na.rm = TRUE),
                                    sd(qpp19_7[lv_ches_848 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_1001 == 1], na.rm = TRUE),
                                    sd(qpp19_2[lv_ches_1002 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_1003 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_1004 == 1], na.rm = TRUE),
                                    sd(qpp19_8[lv_ches_1005 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_1014 == 1], na.rm = TRUE),
                                    sd(qpp19_7[lv_ches_1016 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_1017 == 1], na.rm = TRUE),
                                    NA,
                                    sd(qpp19_1[lv_ches_1101 == 1], na.rm = TRUE),
                                    sd(qpp19_2[lv_ches_1102 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_1104 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_1105 == 1], na.rm = TRUE),
                                    sd(qpp19_7[lv_ches_1106 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_1107 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_1108 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_1201 == 1], na.rm = TRUE),
                                    sd(qpp19_2[lv_ches_1202 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_1205 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_1206 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_1208 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_1209 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_1302 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_1303 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_1304 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_1307 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_1309 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_1401 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_1402 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_1403 == 1], na.rm = TRUE),
                                    sd(qpp19_7[lv_ches_1404 == 1], na.rm = TRUE),
                                    sd(qpp19_8[lv_ches_1405 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_1406 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_1408 == 1], na.rm = TRUE),
                                    sd(qpp19_2[lv_ches_1409 == 1], na.rm = TRUE),
                                    sd(qpp19_8[lv_ches_1601 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_1602 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_1603 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_1604 == 1], na.rm = TRUE),
                                    sd(qpp19_2[lv_ches_1605 == 1], na.rm = TRUE),
                                    sd(qpp19_7[lv_ches_1606 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_1607 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_1610 == 1], na.rm = TRUE),
                                    NA,
                                    NA,
                                    sd(qpp19_1[lv_ches_2002 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2004 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2007 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2010 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2015 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2016 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2101 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2103 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2104 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2109 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2111 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2113 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2201 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2202 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2203 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2204 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2207 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2301 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2302 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2308 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2309 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2310 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2311 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2402 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2405 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2406 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2410 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2412 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2501 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2506 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2507 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2511 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2515 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2516 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2518 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2601 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2603 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2605 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2606 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2613 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2614 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2616 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2701 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2702 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2704 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2705 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2706 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2710 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2711 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2802 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2803 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2804 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2805 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2809 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2812 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2813 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2814 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2815 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2902 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2903 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2904 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2905 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2906 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_2914 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_3101 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_3102 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_3104 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_3105 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_3107 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_3109 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_3112 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_3114 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_3701 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_3702 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_3801 == 1], na.rm = TRUE),
                                    sd(qpp19_4[lv_ches_3802 == 1], na.rm = TRUE),
                                    sd(qpp19_3[lv_ches_3803 == 1], na.rm = TRUE),
                                    sd(qpp19_2[lv_ches_3804 == 1], na.rm = TRUE),
                                    sd(qpp19_6[lv_ches_3805 == 1], na.rm = TRUE),
                                    sd(qpp19_5[lv_ches_3806 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_4001 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_4003 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_4004 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_4005 == 1], na.rm = TRUE),
                                    sd(qpp19_1[lv_ches_4006 == 1], na.rm = TRUE))


sd(qpp19_5[is.na(lv_ches_110) & countrycode == 1056], na.rm = TRUE)



party_vot_var$sd_non_lv_eu_ass <- c(sd(qpp19_2[is.na(lv_ches_102) & countrycode == 1056 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_103) & countrycode == 1056 ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_104) & countrycode == 1056 ], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_105) & countrycode == 1056 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_106) & countrycode == 1056 ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_107) & countrycode == 1056 ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_108) & countrycode == 1056 ], na.rm = TRUE),
                                        sd(qpp19_2[is.na(lv_ches_109) & countrycode == 1056 ], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_110) & countrycode == 1056 ], na.rm = TRUE),
                                        sd(qpp19_7[is.na(lv_ches_112) & countrycode == 1056 ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_119) & countrycode == 1056 ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_201) & countrycode == 1208 ], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_202) & countrycode == 1208 ], na.rm = TRUE),
                                        sd(qpp19_7[is.na(lv_ches_203) & countrycode == 1208 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_206) & countrycode == 1208 ], na.rm = TRUE),
                                        sd(qpp19_2[is.na(lv_ches_211) & countrycode == 1208 ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_215) & countrycode == 1208 ], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_218) & countrycode == 1208 ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_301) & countrycode == 1276 ], na.rm = TRUE),
                                        sd(qpp19_2[is.na(lv_ches_302) & countrycode == 1276 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_303) & countrycode == 1276 ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_304) & countrycode == 1276 ], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_306) & countrycode == 1276 ], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_310) & countrycode == 1276 ], na.rm = TRUE),
                                        sd(qpp19_7[is.na(lv_ches_311) & countrycode == 1276 ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_401) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_402) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_403) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_404) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_410) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_413) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_414) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_415) ], na.rm = TRUE),
                                        sd(qpp19_2[is.na(lv_ches_501) & countrycode ==  1724], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_502) & countrycode ==  1724], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_504) & countrycode ==  1724], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_505) & countrycode ==  1724], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_506) & countrycode ==  1724], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_511) & countrycode ==  1724], na.rm = TRUE),
                                        NA,
                                        sd(qpp19_5[is.na(lv_ches_517) & countrycode ==  1724   ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_523) & countrycode ==  1724   ], na.rm = TRUE),
                                        sd(qpp19_8[is.na(lv_ches_525) & countrycode ==  1724   ], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_601) & countrycode ==  1250 ], na.rm = TRUE),
                                        sd(qpp19_2[is.na(lv_ches_602) & countrycode ==  1250 ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_605) & countrycode ==  1250 ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_609) & countrycode ==  1250 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_610) & countrycode ==  1250 ], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_613) & countrycode ==  1250 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_701) & countrycode ==  1372 ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_702) & countrycode ==  1372 ], na.rm = TRUE),
                                        sd(qpp19_2[is.na(lv_ches_703) & countrycode ==  1372 ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_705) & countrycode ==  1372 ], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_707) & countrycode ==  1372 ], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_708) & countrycode ==  1372 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_811) & countrycode ==  1380 ], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_814) & countrycode ==  1380 ], na.rm = TRUE),
                                        sd(qpp19_2[is.na(lv_ches_815) & countrycode ==  1380 ], na.rm = TRUE),
                                        NA,
                                        sd(qpp19_1[is.na(lv_ches_837) & countrycode ==  1380 ], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_838) & countrycode ==  1380 ], na.rm = TRUE),
                                        sd(qpp19_8[is.na(lv_ches_844) & countrycode ==  1380 ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_845) & countrycode ==  1380 ], na.rm = TRUE),
                                        sd(qpp19_7[is.na(lv_ches_848) & countrycode ==  1380 ], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_1001) & countrycode ==  1528 ], na.rm = TRUE),
                                        sd(qpp19_2[is.na(lv_ches_1002) & countrycode ==  1528 ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_1003) & countrycode ==  1528 ], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_1004) & countrycode ==  1528 ], na.rm = TRUE),
                                        sd(qpp19_8[is.na(lv_ches_1005) & countrycode ==  1528 ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_1014) & countrycode ==  1528 ], na.rm = TRUE),
                                        sd(qpp19_7[is.na(lv_ches_1016) & countrycode ==  1528 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_1017) & countrycode ==  1528 ], na.rm = TRUE),
                                        NA,
                                        sd(qpp19_1[is.na(lv_ches_1101) & countrycode ==  1826 ], na.rm = TRUE),
                                        sd(qpp19_2[is.na(lv_ches_1102) & countrycode ==  1826 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_1104) & countrycode ==  1826 ], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_1105) & countrycode ==  1826 ], na.rm = TRUE),
                                        sd(qpp19_7[is.na(lv_ches_1106) & countrycode ==  1826 ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_1107) & countrycode ==  1826 ], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_1108) & countrycode ==  1826 ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_1201) & countrycode ==  1620 ], na.rm = TRUE),
                                        sd(qpp19_2[is.na(lv_ches_1202) & countrycode ==  1620 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_1205) & countrycode ==  1620 ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_1206) & countrycode ==  1620 ], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_1208) & countrycode ==  1620 ], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_1209) & countrycode ==  1620 ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_1302) & countrycode ==  1040 ], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_1303) & countrycode ==  1040 ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_1304) & countrycode ==  1040 ], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_1307) & countrycode ==  1040 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_1309) & countrycode ==  1040 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_1401) & countrycode ==  1246 ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_1402) & countrycode ==  1246 ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_1403) & countrycode ==  1246 ], na.rm = TRUE),
                                        sd(qpp19_7[is.na(lv_ches_1404) & countrycode ==  1246 ], na.rm = TRUE),
                                        sd(qpp19_8[is.na(lv_ches_1405) & countrycode ==  1246 ], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_1406) & countrycode ==  1246 ], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_1408) & countrycode ==  1246 ], na.rm = TRUE),
                                        sd(qpp19_2[is.na(lv_ches_1409) & countrycode ==  1246 ], na.rm = TRUE),
                                        sd(qpp19_8[is.na(lv_ches_1601) & countrycode ==  1752 ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_1602) & countrycode ==  1752 ], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_1603) & countrycode ==  1752 ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_1604) & countrycode ==  1752 ], na.rm = TRUE),
                                        sd(qpp19_2[is.na(lv_ches_1605) & countrycode ==  1752 ], na.rm = TRUE),
                                        sd(qpp19_7[is.na(lv_ches_1606) & countrycode ==  1752 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_1607) & countrycode ==  1752 ], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_1610) & countrycode ==  1752 ], na.rm = TRUE),
                                        NA,
                                        NA,
                                        sd(qpp19_1[is.na(lv_ches_2002) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2004) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2007) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2010) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2015) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2016) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2101) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2103) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2104) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2109) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2111) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2113) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2201) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2202) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2203) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2204) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2207) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2301) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2302) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2308) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2309) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2310) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2311) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2402) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2405) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2406) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2410) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2412) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2501) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2506) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2507) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2511) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2515) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2516) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2518) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2601) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2603) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2605) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2606) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2613) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2614) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2616) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2701) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2702) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2704) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2705) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2706) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2710) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2711) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2802) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2803) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2804) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2805) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2809) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2812) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2813) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2814) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2815) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2902) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2903) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2904) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2905) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2906) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_2914) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_3101) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_3102) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_3104) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_3105) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_3107) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_3109) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_3112) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_3114) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_3701) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_3702) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_3801) ], na.rm = TRUE),
                                        sd(qpp19_4[is.na(lv_ches_3802) & countrycode ==  1442 ], na.rm = TRUE),
                                        sd(qpp19_3[is.na(lv_ches_3803) & countrycode ==  1442 ], na.rm = TRUE),
                                        sd(qpp19_2[is.na(lv_ches_3804) & countrycode ==  1442 ], na.rm = TRUE),
                                        sd(qpp19_6[is.na(lv_ches_3805) & countrycode ==  1442 ], na.rm = TRUE),
                                        sd(qpp19_5[is.na(lv_ches_3806) & countrycode ==  1442 ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_4001) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_4003) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_4004) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_4005) ], na.rm = TRUE),
                                        sd(qpp19_1[is.na(lv_ches_4006) ], na.rm = TRUE))









#############EES_2009: Running the code for EES_2009$q87, q28, q27 This adds the CHES scores in the cells instead of EES scores#################
###############################EES_2009##
EES_2009 <- read_dta("ZA5055_v1-1-1_stata14.dta")

##i.e. party a voter voted for or who she would consider voting for. 

#add empty variables to fill. These are the party codes for likely to vote on in the cells.
EES_2009$q87_ches <- NA #Close to
EES_2009$q28_ches <- NA #which party would vote for?
EES_2009$q27_ches <- NA #Did vote for?

#voting tomorrow (q28_ches)
#Austria
EES_2009$q28_ches[EES_2009$q28==1040520] <- 1302 # AT OVP
EES_2009$q28_ches[EES_2009$q28==1040302] <- 1301 # AT SPO 
EES_2009$q28_ches[EES_2009$q28==1040423] <- 1309 # AT NEOS 
EES_2009$q28_ches[EES_2009$q28==1040110] <- 1304 # AT Grune 
EES_2009$q28_ches[EES_2009$q28==1040420] <- 1303 # AT FPO 
EES_2009$q28_ches[EES_2009$q28==1040600] <- 1307 # AT BZO

# Belgium (Flanders & Wallonia)
EES_2009$q28_ches[EES_2009$q28==1056325] <- 119 ##  BE PVDA
EES_2009$q28_ches[EES_2009$q28==1056521] <- 109 ##  BE CD&V
EES_2009$q28_ches[EES_2009$q28==1056327] <- 103 ##  BE SPA
EES_2009$q28_ches[EES_2009$q28==1056421] <- 107 ##  BE VLD
EES_2009$q28_ches[EES_2009$q28==1056913] <- 110 ##  BE NVA
EES_2009$q28_ches[EES_2009$q28==1056112] <- 105 ##  BE Groen
EES_2009$q28_ches[EES_2009$q28==1056711] <- 112 ##  BE VB
EES_2009$q28_ches[EES_2009$q28==1056522] <- 108 #   BE cdH
EES_2009$q28_ches[EES_2009$q28==1056322] <- 102 #   BE PS
EES_2009$q28_ches[EES_2009$q28==1056427] <- 106 #   BE MR
EES_2009$q28_ches[EES_2009$q28==1056111] <- 104 #   BE ECOLO

# Bulgaria (1100300, 1100601, 1100400 missing)
EES_2009$q28_ches[EES_2009$q28==1100600] <- 2010 # BG GERB
EES_2009$q28_ches[EES_2009$q28==1100900] <- 2004 # BG DPS
EES_2009$q28_ches[EES_2009$q28==1100700] <- 2007 # BG Attack
EES_2009$q28_ches[EES_2009$q28==1100602] <- 2015 # BG BBT
EES_2009$q28_ches[EES_2009$q28==1103001] <- 2016 # BG ABV
EES_2009$q28_ches[EES_2009$q28==1100001] <- 2002 # BG SDS

# Cyrpus (1196002 is missing)
EES_2009$q28_ches[EES_2009$q28==1196711] <- 4001 # CY DISY
EES_2009$q28_ches[EES_2009$q28==1196422] <- 4004 # CY DIKO
EES_2009$q28_ches[EES_2009$q28==1196322] <- 4005 # CY EDEK
EES_2009$q28_ches[EES_2009$q28==1196321] <- 4003 # CY AKEL
EES_2009$q28_ches[EES_2009$q28==1196110] <- 4006 # CY KOP

# Czech Republic (1203321 1203110 is missing)
EES_2009$q28_ches[EES_2009$q28==1203523] <- 2104 #  CZ KDU-CSL
EES_2009$q28_ches[EES_2009$q28==1203530] <- 2109 #  CZ TOP09
EES_2009$q28_ches[EES_2009$q28==1203320] <- 2101 #  CZ CSSD
EES_2009$q28_ches[EES_2009$q28==1203413] <- 2102 #  CZ ODS
EES_2009$q28_ches[EES_2009$q28==1203220] <- 2103 #  CZ KSCM
EES_2009$q28_ches[EES_2009$q28==1203413] <- 2111 #  CZ ANO2011
EES_2009$q28_ches[EES_2009$q28==1203953] <- 2113 #  CZ SVOBODNI

# Germany
EES_2009$q28_ches[EES_2009$q28==1276521] <- 301 # GE CDU
EES_2009$q28_ches[EES_2009$q28==1276320] <- 302 # GE SPD
EES_2009$q28_ches[EES_2009$q28==1276420] <- 303 # GE FDP
EES_2009$q28_ches[EES_2009$q28==1276113] <- 304 # GE Grunen
EES_2009$q28_ches[EES_2009$q28==1276321] <- 306 # GE Linke
EES_2009$q28_ches[EES_2009$q28==1276621] <- 310 # GE AfD
EES_2009$q28_ches[EES_2009$q28==1276951] <- 311 # GE Piraten

# Denmark (FolkB is missing)
EES_2009$q28_ches[EES_2009$q28==1208320] <- 201 # DK SD
EES_2009$q28_ches[EES_2009$q28==1208420] <- 211 # DK V
EES_2009$q28_ches[EES_2009$q28==1208330] <- 206 # DK SF
EES_2009$q28_ches[EES_2009$q28==1208720] <- 215 # DK DF
EES_2009$q28_ches[EES_2009$q28==1208410] <- 202 # DK RV
EES_2009$q28_ches[EES_2009$q28==1208421] <- 218 # DK LA
EES_2009$q28_ches[EES_2009$q28==1208620] <- 203 # DK KF

# Estonia (1233003 is missing)
EES_2009$q28_ches[EES_2009$q28==1233613] <- 2201 # EE IRL
EES_2009$q28_ches[EES_2009$q28==1233410] <- 2204 # EE SDE
EES_2009$q28_ches[EES_2009$q28==1233430] <- 2203 # EE ER
EES_2009$q28_ches[EES_2009$q28==1233411] <- 2202 # EE EK
EES_2009$q28_ches[EES_2009$q28==1233100] <- 2207 # EE EER

# Greece (1300116 is missing)
EES_2009$q28_ches[EES_2009$q28==1300511] <- 402 # GR ND
EES_2009$q28_ches[EES_2009$q28==1300215] <- 403 # GR SYRIZA
EES_2009$q28_ches[EES_2009$q28==1300313] <- 401 # GR PASOK
EES_2009$q28_ches[EES_2009$q28==1300611] <- 401 # GR ANEL
EES_2009$q28_ches[EES_2009$q28==1300710] <- 415 # GR XA
EES_2009$q28_ches[EES_2009$q28==1300225] <- 414 # GR DIMAR
EES_2009$q28_ches[EES_2009$q28==1300210] <- 404 # GR KKE
EES_2009$q28_ches[EES_2009$q28==1300323] <- 413 # GR Potami
EES_2009$q28_ches[EES_2009$q28==1300703] <- 410 # GR LAOS

# Spain 
EES_2009$q28_ches[EES_2009$q28==1724610] <- 502 # ESP PP
EES_2009$q28_ches[EES_2009$q28==1724320] <- 501 # ESP PSOE
EES_2009$q28_ches[EES_2009$q28==1724220] <- 504 # ESP IU
EES_2009$q28_ches[EES_2009$q28==1724010] <- 523 # ESP UPyd
EES_2009$q28_ches[EES_2009$q28==1724905] <- 511 # ESP ERC
EES_2009$q28_ches[EES_2009$q28==1724310] <- 526 # ESP Cs
EES_2009$q28_ches[EES_2009$q28==1724230] <- 525 # ESP Podemos
EES_2009$q28_ches[EES_2009$q28==1724007] <- 505 # ESP CiU
EES_2009$q28_ches[EES_2009$q28==1724902] <- 506 # ESP EAJ-PNV
EES_2009$q28_ches[EES_2009$q28==1724908] <- 513 # ESP BNG
EES_2009$q28_ches[EES_2009$q28==1724907] <- 517 # ESP CC

# Finland
EES_2009$q28_ches[EES_2009$q28==1246620] <- 1402 # FI KOK
EES_2009$q28_ches[EES_2009$q28==1246520] <- 1409 # FI KD
EES_2009$q28_ches[EES_2009$q28==1246320] <- 1401 # FI SDP
EES_2009$q28_ches[EES_2009$q28==1246810] <- 1403 # FI KESK
EES_2009$q28_ches[EES_2009$q28==1246901] <- 1406 # FI RKP/SFP
EES_2009$q28_ches[EES_2009$q28==1246110] <- 1408 # FI VIHR
EES_2009$q28_ches[EES_2009$q28==1246223] <- 1404 # FI VAS
EES_2009$q28_ches[EES_2009$q28==1246820] <- 1405 # FI PS

# France (Please check for 1250223. 1250636, 1250233 are missing)
EES_2009$q28_ches[EES_2009$q28==1250626] <- 609 # FR UMP
EES_2009$q28_ches[EES_2009$q28==1250320] <- 602 # FR PS
EES_2009$q28_ches[EES_2009$q28==1250720] <- 610 # FR FN
EES_2009$q28_ches[EES_2009$q28==1250110] <- 605 # FR EELV
EES_2009$q28_ches[EES_2009$q28==1250223] <- 601 # FR PCF
EES_2009$q28_ches[EES_2009$q28==1250336] <- 613 # FR MODEM

# Hungary
EES_2009$q28_ches[EES_2009$q28==1348700] <- 2308 # HU JOBBIK
EES_2009$q28_ches[EES_2009$q28==1348110] <- 2309 # HU LMP
EES_2009$q28_ches[EES_2009$q28==1348421] <- 2302 # HU Fidesz
EES_2009$q28_ches[EES_2009$q28==1348220] <- 2301 # HU MSzP
EES_2009$q28_ches[EES_2009$q28==1348120] <- 2310 # HU E14
EES_2009$q28_ches[EES_2009$q28==1348330] <- 2311 # HU DK

# Ireland
EES_2009$q28_ches[EES_2009$q28==1372520] <- 702 # IE FG
EES_2009$q28_ches[EES_2009$q28==1372320] <- 703 # IE Lab
EES_2009$q28_ches[EES_2009$q28==1372620] <- 701 # IE FF
EES_2009$q28_ches[EES_2009$q28==1372110] <- 705 # IE GP
EES_2009$q28_ches[EES_2009$q28==1372951] <- 707 # IE SF
EES_2009$q28_ches[EES_2009$q28==1372220] <- 708 # IE SP

# Italy (1380902, 1380630 are missing)
EES_2009$q28_ches[EES_2009$q28==1380331] <- 837 # IT PD
EES_2009$q28_ches[EES_2009$q28==1380610] <- 815 # IT FI
EES_2009$q28_ches[EES_2009$q28==1380720] <- 811 # IT LN
EES_2009$q28_ches[EES_2009$q28==1380956] <- 845 # IT M5S
EES_2009$q28_ches[EES_2009$q28==1380523] <- 814 # IT UDC
EES_2009$q28_ches[EES_2009$q28==1380007] <- 838 # IT SEl
EES_2009$q28_ches[EES_2009$q28==1380633] <- 848 # IT NCD
EES_2009$q28_ches[EES_2009$q28==1380631] <- 844 # IT FDL
EES_2009$q28_ches[EES_2009$q28==1380958] <- 827 # IT SVP

# Lithuania (1440420 is missing)
EES_2009$q28_ches[EES_2009$q28==1440620] <- 2506 # LT TS-LKD
EES_2009$q28_ches[EES_2009$q28==1440320] <- 2501 # LT LSDP
EES_2009$q28_ches[EES_2009$q28==1440421] <- 2518 # LT LRLS
EES_2009$q28_ches[EES_2009$q28==1440322] <- 2516 # LT DP
EES_2009$q28_ches[EES_2009$q28==1440621] <- 2515 # LT TT
EES_2009$q28_ches[EES_2009$q28==1440952] <- 2511 # LT LLRA
EES_2009$q28_ches[EES_2009$q28==1440524] <- 2507 # LT LVZS

# Luxembourg (1442220 is missing)
EES_2009$q28_ches[EES_2009$q28==1442520] <- 3801 # LU CSV
EES_2009$q28_ches[EES_2009$q28==1442320] <- 3804 # LU LSAP
EES_2009$q28_ches[EES_2009$q28==1442420] <- 3803 # LU DP
EES_2009$q28_ches[EES_2009$q28==1442113] <- 3802 # LU Greng
EES_2009$q28_ches[EES_2009$q28==1442222] <- 3806 # LU DL
EES_2009$q28_ches[EES_2009$q28==1442951] <- 3805 # LU ADR

# Latvia (1428620 1428422 1428424 are missing)
EES_2009$q28_ches[EES_2009$q28==1428610] <- 2412 # LV V
EES_2009$q28_ches[EES_2009$q28==1428317] <- 2410 # LV SDPS
EES_2009$q28_ches[EES_2009$q28==1428723] <- 2406 # LV NA
EES_2009$q28_ches[EES_2009$q28==1428110] <- 2405 # LV ZZS
EES_2009$q28_ches[EES_2009$q28==1428901] <- 2402 # LV LKS 

# Malta (1470100 is missing)
EES_2009$q28_ches[EES_2009$q28==1470300] <- 3701 # MT PL
EES_2009$q28_ches[EES_2009$q28==1470500] <- 3702 # MT PN

# Netherlands (1528528 - please check. EES codes as coalition of 2 parties)
EES_2009$q28_ches[EES_2009$q28==1528420] <- 1003 # NL VVD
EES_2009$q28_ches[EES_2009$q28==1528320] <- 1002 # NL PvdA
EES_2009$q28_ches[EES_2009$q28==1528600] <- 1017 # NL PVV
EES_2009$q28_ches[EES_2009$q28==1528220] <- 1014 # NL SP
EES_2009$q28_ches[EES_2009$q28==1528521] <- 1001 # NL CDA
EES_2009$q28_ches[EES_2009$q28==1528330] <- 1004 # NL D66
EES_2009$q28_ches[EES_2009$q28==1528526] <- 1016 # NL CU
EES_2009$q28_ches[EES_2009$q28==1528110] <- 1005 # NL GL
EES_2009$q28_ches[EES_2009$q28==1528951] <- 1018 # NL PvdD

# Poland
EES_2009$q28_ches[EES_2009$q28==1616435] <- 2603 # PL PO
EES_2009$q28_ches[EES_2009$q28==1616811] <- 2606 # PL PSL
EES_2009$q28_ches[EES_2009$q28==1616210] <- 2601 # PL SLD
EES_2009$q28_ches[EES_2009$q28==1616436] <- 2605 # PL PiS
EES_2009$q28_ches[EES_2009$q28==1616310] <- 2613 # PL RP
EES_2009$q28_ches[EES_2009$q28==1616001] <- 2614 # PL KNP
EES_2009$q28_ches[EES_2009$q28==1616002] <- 2616 # PL SP

# Portugal (1620314 - please check. EES codes as coalition of 2 parties)
EES_2009$q28_ches[EES_2009$q28==1620313] <- 1206 # PT PSD
EES_2009$q28_ches[EES_2009$q28==1620520] <- 1202 # PT PP
EES_2009$q28_ches[EES_2009$q28==1620311] <- 1205 # PT PS
EES_2009$q28_ches[EES_2009$q28==1620229] <- 1201 # PT CDU
EES_2009$q28_ches[EES_2009$q28==1620211] <- 1208 # PT BE
EES_2009$q28_ches[EES_2009$q28==1620110] <- 1209 # PT MPT

# Romania (1642700, 1642800 are missing. 1642502 is a coalition)
EES_2009$q28_ches[EES_2009$q28==1642300] <- 2701 # RO PSD
EES_2009$q28_ches[EES_2009$q28==1642401] <- 2705 # RO PNL
EES_2009$q28_ches[EES_2009$q28==1642400] <- 2704 # RO PDL
EES_2009$q28_ches[EES_2009$q28==1642981] <- 2710 # RO PP-DD
EES_2009$q28_ches[EES_2009$q28==1642900] <- 2706 # RO UDMR
EES_2009$q28_ches[EES_2009$q28==1642503] <- 2711 # RO PMP
EES_2009$q28_ches[EES_2009$q28==1642600] <- 2702 # RO PC

# Sweden
EES_2009$q28_ches[EES_2009$q28==1752320] <- 1602 # SE SAP
EES_2009$q28_ches[EES_2009$q28==1752620] <- 1605 # SE M
EES_2009$q28_ches[EES_2009$q28==1752110] <- 1607 # SE MP
EES_2009$q28_ches[EES_2009$q28==1752420] <- 1604 # SE FP
EES_2009$q28_ches[EES_2009$q28==1752810] <- 1603 # SE C
EES_2009$q28_ches[EES_2009$q28==1752700] <- 1610 # SE SD
EES_2009$q28_ches[EES_2009$q28==1752520] <- 1606 # SE KD
EES_2009$q28_ches[EES_2009$q28==1752220] <- 1601 # SE V
EES_2009$q28_ches[EES_2009$q28==1752953] <- 1612 # SE FI
EES_2009$q28_ches[EES_2009$q28==1752000] <- 1611 # SE PP

# Slovenia (1705450, 1705421, 1705324, 1705710, 1705952 are missing)
EES_2009$q28_ches[EES_2009$q28==1705340] <- 2914 # SI PS
EES_2009$q28_ches[EES_2009$q28==1705320] <- 2902 # SI SDS
EES_2009$q28_ches[EES_2009$q28==1705323] <- 2903 # SI SD
EES_2009$q28_ches[EES_2009$q28==1705951] <- 2906 # SI DeSUS
EES_2009$q28_ches[EES_2009$q28==1705522] <- 2905 # SI NSI
EES_2009$q28_ches[EES_2009$q28==1705521] <- 2904 # SI SLS

# Slovakia (1703222 is missing)
EES_2009$q28_ches[EES_2009$q28==1703521] <- 2805 # SK KDH
EES_2009$q28_ches[EES_2009$q28==1703523] <- 2802 # SK SDKU-DS
EES_2009$q28_ches[EES_2009$q28==1703954] <- 2804 # SK SMK-MKP
EES_2009$q28_ches[EES_2009$q28==1703423] <- 2803 # SK Smer-SD
EES_2009$q28_ches[EES_2009$q28==1703610] <- 2815 # SK NOVA
EES_2009$q28_ches[EES_2009$q28==1703440] <- 2812 # SK SaS
EES_2009$q28_ches[EES_2009$q28==1703620] <- 2814 # SK OLaNO
EES_2009$q28_ches[EES_2009$q28==1703955] <- 2813 # SK MH
EES_2009$q28_ches[EES_2009$q28==1703710] <- 2809 # SK SNS

# UK (1826210, 1826903, 1826724, 1826720 are missing)
EES_2009$q28_ches[EES_2009$q28==1826620] <- 1101 # UK Cons
EES_2009$q28_ches[EES_2009$q28==1826320] <- 1102 # UK Lab
EES_2009$q28_ches[EES_2009$q28==1826421] <- 1104 # UK LibDems
EES_2009$q28_ches[EES_2009$q28==1826110] <- 1107 # UK Green
EES_2009$q28_ches[EES_2009$q28==1826951] <- 1108 # UK UKIP
EES_2009$q28_ches[EES_2009$q28==1826902] <- 1105 # UK SNP
EES_2009$q28_ches[EES_2009$q28==1826901] <- 1106 # UK Plaid

# Croatia
EES_2009$q28_ches[EES_2009$q28==1191320] <- 3102 # HR SDP
EES_2009$q28_ches[EES_2009$q28==1191412] <- 3105 # HR HNS
EES_2009$q28_ches[EES_2009$q28==1191511] <- 3101 # HR HDZ
EES_2009$q28_ches[EES_2009$q28==1191613] <- 3109 # HR HSP
EES_2009$q28_ches[EES_2009$q28==1191410] <- 3104 # HR HSLS
EES_2009$q28_ches[EES_2009$q28==1191952] <- 3107 # HR HDSSB
EES_2009$q28_ches[EES_2009$q28==1191330] <- 3112 # HR HL-SR
EES_2009$q28_ches[EES_2009$q28==1191110] <- 3114 # HR ORaH







##############same but for other question

#fDid vote for(q27_ches)
#Austria
EES_2009$q27_ches[EES_2009$q27==1040520] <- 1302 # AT OVP
EES_2009$q27_ches[EES_2009$q27==1040302] <- 1301 # AT SPO 
EES_2009$q27_ches[EES_2009$q27==1040423] <- 1309 # AT NEOS 
EES_2009$q27_ches[EES_2009$q27==1040110] <- 1304 # AT Grune 
EES_2009$q27_ches[EES_2009$q27==1040420] <- 1303 # AT FPO 
EES_2009$q27_ches[EES_2009$q27==1040600] <- 1307 # AT BZO

# Belgium (Flanders & Wallonia)
EES_2009$q27_ches[EES_2009$q27==1056325] <- 119 ##  BE PVDA
EES_2009$q27_ches[EES_2009$q27==1056521] <- 109 ##  BE CD&V
EES_2009$q27_ches[EES_2009$q27==1056327] <- 103 ##  BE SPA
EES_2009$q27_ches[EES_2009$q27==1056421] <- 107 ##  BE VLD
EES_2009$q27_ches[EES_2009$q27==1056913] <- 110 ##  BE NVA
EES_2009$q27_ches[EES_2009$q27==1056112] <- 105 ##  BE Groen
EES_2009$q27_ches[EES_2009$q27==1056711] <- 112 ##  BE VB
EES_2009$q27_ches[EES_2009$q27==1056522] <- 108 #   BE cdH
EES_2009$q27_ches[EES_2009$q27==1056322] <- 102 #   BE PS
EES_2009$q27_ches[EES_2009$q27==1056427] <- 106 #   BE MR
EES_2009$q27_ches[EES_2009$q27==1056111] <- 104 #   BE ECOLO

# Bulgaria (1100300, 1100601, 1100400 missing)
EES_2009$q27_ches[EES_2009$q27==1100600] <- 2010 # BG GERB
EES_2009$q27_ches[EES_2009$q27==1100900] <- 2004 # BG DPS
EES_2009$q27_ches[EES_2009$q27==1100700] <- 2007 # BG Attack
EES_2009$q27_ches[EES_2009$q27==1100602] <- 2015 # BG BBT
EES_2009$q27_ches[EES_2009$q27==1103001] <- 2016 # BG ABV
EES_2009$q27_ches[EES_2009$q27==1100001] <- 2002 # BG SDS

# Cyrpus (1196002 is missing)
EES_2009$q27_ches[EES_2009$q27==1196711] <- 4001 # CY DISY
EES_2009$q27_ches[EES_2009$q27==1196422] <- 4004 # CY DIKO
EES_2009$q27_ches[EES_2009$q27==1196322] <- 4005 # CY EDEK
EES_2009$q27_ches[EES_2009$q27==1196321] <- 4003 # CY AKEL
EES_2009$q27_ches[EES_2009$q27==1196110] <- 4006 # CY KOP

# Czech Republic (1203321 1203110 is missing)
EES_2009$q27_ches[EES_2009$q27==1203523] <- 2104 #  CZ KDU-CSL
EES_2009$q27_ches[EES_2009$q27==1203530] <- 2109 #  CZ TOP09
EES_2009$q27_ches[EES_2009$q27==1203320] <- 2101 #  CZ CSSD
EES_2009$q27_ches[EES_2009$q27==1203413] <- 2102 #  CZ ODS
EES_2009$q27_ches[EES_2009$q27==1203220] <- 2103 #  CZ KSCM
EES_2009$q27_ches[EES_2009$q27==1203413] <- 2111 #  CZ ANO2011
EES_2009$q27_ches[EES_2009$q27==1203953] <- 2113 #  CZ SVOBODNI

# Germany
EES_2009$q27_ches[EES_2009$q27==1276521] <- 301 # GE CDU
EES_2009$q27_ches[EES_2009$q27==1276320] <- 302 # GE SPD
EES_2009$q27_ches[EES_2009$q27==1276420] <- 303 # GE FDP
EES_2009$q27_ches[EES_2009$q27==1276113] <- 304 # GE Grunen
EES_2009$q27_ches[EES_2009$q27==1276321] <- 306 # GE Linke
EES_2009$q27_ches[EES_2009$q27==1276621] <- 310 # GE AfD
EES_2009$q27_ches[EES_2009$q27==1276951] <- 311 # GE Piraten

# Denmark (FolkB is missing)
EES_2009$q27_ches[EES_2009$q27==1208320] <- 201 # DK SD
EES_2009$q27_ches[EES_2009$q27==1208420] <- 211 # DK V
EES_2009$q27_ches[EES_2009$q27==1208330] <- 206 # DK SF
EES_2009$q27_ches[EES_2009$q27==1208720] <- 215 # DK DF
EES_2009$q27_ches[EES_2009$q27==1208410] <- 202 # DK RV
EES_2009$q27_ches[EES_2009$q27==1208421] <- 218 # DK LA
EES_2009$q27_ches[EES_2009$q27==1208620] <- 203 # DK KF

# Estonia (1233003 is missing)
EES_2009$q27_ches[EES_2009$q27==1233613] <- 2201 # EE IRL
EES_2009$q27_ches[EES_2009$q27==1233410] <- 2204 # EE SDE
EES_2009$q27_ches[EES_2009$q27==1233430] <- 2203 # EE ER
EES_2009$q27_ches[EES_2009$q27==1233411] <- 2202 # EE EK
EES_2009$q27_ches[EES_2009$q27==1233100] <- 2207 # EE EER

# Greece (1300116 is missing)
EES_2009$q27_ches[EES_2009$q27==1300511] <- 402 # GR ND
EES_2009$q27_ches[EES_2009$q27==1300215] <- 403 # GR SYRIZA
EES_2009$q27_ches[EES_2009$q27==1300313] <- 401 # GR PASOK
EES_2009$q27_ches[EES_2009$q27==1300611] <- 401 # GR ANEL
EES_2009$q27_ches[EES_2009$q27==1300710] <- 415 # GR XA
EES_2009$q27_ches[EES_2009$q27==1300225] <- 414 # GR DIMAR
EES_2009$q27_ches[EES_2009$q27==1300210] <- 404 # GR KKE
EES_2009$q27_ches[EES_2009$q27==1300323] <- 413 # GR Potami
EES_2009$q27_ches[EES_2009$q27==1300703] <- 410 # GR LAOS

# Spain 
EES_2009$q27_ches[EES_2009$q27==1724610] <- 502 # ESP PP
EES_2009$q27_ches[EES_2009$q27==1724320] <- 501 # ESP PSOE
EES_2009$q27_ches[EES_2009$q27==1724220] <- 504 # ESP IU
EES_2009$q27_ches[EES_2009$q27==1724010] <- 523 # ESP UPyd
EES_2009$q27_ches[EES_2009$q27==1724905] <- 511 # ESP ERC
EES_2009$q27_ches[EES_2009$q27==1724310] <- 526 # ESP Cs
EES_2009$q27_ches[EES_2009$q27==1724230] <- 525 # ESP Podemos
EES_2009$q27_ches[EES_2009$q27==1724007] <- 505 # ESP CiU
EES_2009$q27_ches[EES_2009$q27==1724902] <- 506 # ESP EAJ-PNV
EES_2009$q27_ches[EES_2009$q27==1724908] <- 513 # ESP BNG
EES_2009$q27_ches[EES_2009$q27==1724907] <- 517 # ESP CC

# Finland
EES_2009$q27_ches[EES_2009$q27==1246620] <- 1402 # FI KOK
EES_2009$q27_ches[EES_2009$q27==1246520] <- 1409 # FI KD
EES_2009$q27_ches[EES_2009$q27==1246320] <- 1401 # FI SDP
EES_2009$q27_ches[EES_2009$q27==1246810] <- 1403 # FI KESK
EES_2009$q27_ches[EES_2009$q27==1246901] <- 1406 # FI RKP/SFP
EES_2009$q27_ches[EES_2009$q27==1246110] <- 1408 # FI VIHR
EES_2009$q27_ches[EES_2009$q27==1246223] <- 1404 # FI VAS
EES_2009$q27_ches[EES_2009$q27==1246820] <- 1405 # FI PS

# France (Please check for 1250223. 1250636, 1250233 are missing)
EES_2009$q27_ches[EES_2009$q27==1250626] <- 609 # FR UMP
EES_2009$q27_ches[EES_2009$q27==1250320] <- 602 # FR PS
EES_2009$q27_ches[EES_2009$q27==1250720] <- 610 # FR FN
EES_2009$q27_ches[EES_2009$q27==1250110] <- 605 # FR EELV
EES_2009$q27_ches[EES_2009$q27==1250223] <- 601 # FR PCF
EES_2009$q27_ches[EES_2009$q27==1250336] <- 613 # FR MODEM

# Hungary
EES_2009$q27_ches[EES_2009$q27==1348700] <- 2308 # HU JOBBIK
EES_2009$q27_ches[EES_2009$q27==1348110] <- 2309 # HU LMP
EES_2009$q27_ches[EES_2009$q27==1348421] <- 2302 # HU Fidesz
EES_2009$q27_ches[EES_2009$q27==1348220] <- 2301 # HU MSzP
EES_2009$q27_ches[EES_2009$q27==1348120] <- 2310 # HU E14
EES_2009$q27_ches[EES_2009$q27==1348330] <- 2311 # HU DK

# Ireland
EES_2009$q27_ches[EES_2009$q27==1372520] <- 702 # IE FG
EES_2009$q27_ches[EES_2009$q27==1372320] <- 703 # IE Lab
EES_2009$q27_ches[EES_2009$q27==1372620] <- 701 # IE FF
EES_2009$q27_ches[EES_2009$q27==1372110] <- 705 # IE GP
EES_2009$q27_ches[EES_2009$q27==1372951] <- 707 # IE SF
EES_2009$q27_ches[EES_2009$q27==1372220] <- 708 # IE SP

# Italy (1380902, 1380630 are missing)
EES_2009$q27_ches[EES_2009$q27==1380331] <- 837 # IT PD
EES_2009$q27_ches[EES_2009$q27==1380610] <- 815 # IT FI
EES_2009$q27_ches[EES_2009$q27==1380720] <- 811 # IT LN
EES_2009$q27_ches[EES_2009$q27==1380956] <- 845 # IT M5S
EES_2009$q27_ches[EES_2009$q27==1380523] <- 814 # IT UDC
EES_2009$q27_ches[EES_2009$q27==1380007] <- 838 # IT SEl
EES_2009$q27_ches[EES_2009$q27==1380633] <- 848 # IT NCD
EES_2009$q27_ches[EES_2009$q27==1380631] <- 844 # IT FDL
EES_2009$q27_ches[EES_2009$q27==1380958] <- 827 # IT SVP

# Lithuania (1440420 is missing)
EES_2009$q27_ches[EES_2009$q27==1440620] <- 2506 # LT TS-LKD
EES_2009$q27_ches[EES_2009$q27==1440320] <- 2501 # LT LSDP
EES_2009$q27_ches[EES_2009$q27==1440421] <- 2518 # LT LRLS
EES_2009$q27_ches[EES_2009$q27==1440322] <- 2516 # LT DP
EES_2009$q27_ches[EES_2009$q27==1440621] <- 2515 # LT TT
EES_2009$q27_ches[EES_2009$q27==1440952] <- 2511 # LT LLRA
EES_2009$q27_ches[EES_2009$q27==1440524] <- 2507 # LT LVZS

# Luxembourg (1442220 is missing)
EES_2009$q27_ches[EES_2009$q27==1442520] <- 3801 # LU CSV
EES_2009$q27_ches[EES_2009$q27==1442320] <- 3804 # LU LSAP
EES_2009$q27_ches[EES_2009$q27==1442420] <- 3803 # LU DP
EES_2009$q27_ches[EES_2009$q27==1442113] <- 3802 # LU Greng
EES_2009$q27_ches[EES_2009$q27==1442222] <- 3806 # LU DL
EES_2009$q27_ches[EES_2009$q27==1442951] <- 3805 # LU ADR

# Latvia (1428620 1428422 1428424 are missing)
EES_2009$q27_ches[EES_2009$q27==1428610] <- 2412 # LV V
EES_2009$q27_ches[EES_2009$q27==1428317] <- 2410 # LV SDPS
EES_2009$q27_ches[EES_2009$q27==1428723] <- 2406 # LV NA
EES_2009$q27_ches[EES_2009$q27==1428110] <- 2405 # LV ZZS
EES_2009$q27_ches[EES_2009$q27==1428901] <- 2402 # LV LKS 

# Malta (1470100 is missing)
EES_2009$q27_ches[EES_2009$q27==1470300] <- 3701 # MT PL
EES_2009$q27_ches[EES_2009$q27==1470500] <- 3702 # MT PN

# Netherlands (1528528 - please check. EES codes as coalition of 2 parties)
EES_2009$q27_ches[EES_2009$q27==1528420] <- 1003 # NL VVD
EES_2009$q27_ches[EES_2009$q27==1528320] <- 1002 # NL PvdA
EES_2009$q27_ches[EES_2009$q27==1528600] <- 1017 # NL PVV
EES_2009$q27_ches[EES_2009$q27==1528220] <- 1014 # NL SP
EES_2009$q27_ches[EES_2009$q27==1528521] <- 1001 # NL CDA
EES_2009$q27_ches[EES_2009$q27==1528330] <- 1004 # NL D66
EES_2009$q27_ches[EES_2009$q27==1528526] <- 1016 # NL CU
EES_2009$q27_ches[EES_2009$q27==1528110] <- 1005 # NL GL
EES_2009$q27_ches[EES_2009$q27==1528951] <- 1018 # NL PvdD

# Poland
EES_2009$q27_ches[EES_2009$q27==1616435] <- 2603 # PL PO
EES_2009$q27_ches[EES_2009$q27==1616811] <- 2606 # PL PSL
EES_2009$q27_ches[EES_2009$q27==1616210] <- 2601 # PL SLD
EES_2009$q27_ches[EES_2009$q27==1616436] <- 2605 # PL PiS
EES_2009$q27_ches[EES_2009$q27==1616310] <- 2613 # PL RP
EES_2009$q27_ches[EES_2009$q27==1616001] <- 2614 # PL KNP
EES_2009$q27_ches[EES_2009$q27==1616002] <- 2616 # PL SP

# Portugal (1620314 - please check. EES codes as coalition of 2 parties)
EES_2009$q27_ches[EES_2009$q27==1620313] <- 1206 # PT PSD
EES_2009$q27_ches[EES_2009$q27==1620520] <- 1202 # PT PP
EES_2009$q27_ches[EES_2009$q27==1620311] <- 1205 # PT PS
EES_2009$q27_ches[EES_2009$q27==1620229] <- 1201 # PT CDU
EES_2009$q27_ches[EES_2009$q27==1620211] <- 1208 # PT BE
EES_2009$q27_ches[EES_2009$q27==1620110] <- 1209 # PT MPT

# Romania (1642700, 1642800 are missing. 1642502 is a coalition)
EES_2009$q27_ches[EES_2009$q27==1642300] <- 2701 # RO PSD
EES_2009$q27_ches[EES_2009$q27==1642401] <- 2705 # RO PNL
EES_2009$q27_ches[EES_2009$q27==1642400] <- 2704 # RO PDL
EES_2009$q27_ches[EES_2009$q27==1642981] <- 2710 # RO PP-DD
EES_2009$q27_ches[EES_2009$q27==1642900] <- 2706 # RO UDMR
EES_2009$q27_ches[EES_2009$q27==1642503] <- 2711 # RO PMP
EES_2009$q27_ches[EES_2009$q27==1642600] <- 2702 # RO PC

# Sweden
EES_2009$q27_ches[EES_2009$q27==1752320] <- 1602 # SE SAP
EES_2009$q27_ches[EES_2009$q27==1752620] <- 1605 # SE M
EES_2009$q27_ches[EES_2009$q27==1752110] <- 1607 # SE MP
EES_2009$q27_ches[EES_2009$q27==1752420] <- 1604 # SE FP
EES_2009$q27_ches[EES_2009$q27==1752810] <- 1603 # SE C
EES_2009$q27_ches[EES_2009$q27==1752700] <- 1610 # SE SD
EES_2009$q27_ches[EES_2009$q27==1752520] <- 1606 # SE KD
EES_2009$q27_ches[EES_2009$q27==1752220] <- 1601 # SE V
EES_2009$q27_ches[EES_2009$q27==1752953] <- 1612 # SE FI
EES_2009$q27_ches[EES_2009$q27==1752000] <- 1611 # SE PP

# Slovenia (1705450, 1705421, 1705324, 1705710, 1705952 are missing)
EES_2009$q27_ches[EES_2009$q27==1705340] <- 2914 # SI PS
EES_2009$q27_ches[EES_2009$q27==1705320] <- 2902 # SI SDS
EES_2009$q27_ches[EES_2009$q27==1705323] <- 2903 # SI SD
EES_2009$q27_ches[EES_2009$q27==1705951] <- 2906 # SI DeSUS
EES_2009$q27_ches[EES_2009$q27==1705522] <- 2905 # SI NSI
EES_2009$q27_ches[EES_2009$q27==1705521] <- 2904 # SI SLS

# Slovakia (1703222 is missing)
EES_2009$q27_ches[EES_2009$q27==1703521] <- 2805 # SK KDH
EES_2009$q27_ches[EES_2009$q27==1703523] <- 2802 # SK SDKU-DS
EES_2009$q27_ches[EES_2009$q27==1703954] <- 2804 # SK SMK-MKP
EES_2009$q27_ches[EES_2009$q27==1703423] <- 2803 # SK Smer-SD
EES_2009$q27_ches[EES_2009$q27==1703610] <- 2815 # SK NOVA
EES_2009$q27_ches[EES_2009$q27==1703440] <- 2812 # SK SaS
EES_2009$q27_ches[EES_2009$q27==1703620] <- 2814 # SK OLaNO
EES_2009$q27_ches[EES_2009$q27==1703955] <- 2813 # SK MH
EES_2009$q27_ches[EES_2009$q27==1703710] <- 2809 # SK SNS

# UK (1826210, 1826903, 1826724, 1826720 are missing)
EES_2009$q27_ches[EES_2009$q27==1826620] <- 1101 # UK Cons
EES_2009$q27_ches[EES_2009$q27==1826320] <- 1102 # UK Lab
EES_2009$q27_ches[EES_2009$q27==1826421] <- 1104 # UK LibDems
EES_2009$q27_ches[EES_2009$q27==1826110] <- 1107 # UK Green
EES_2009$q27_ches[EES_2009$q27==1826951] <- 1108 # UK UKIP
EES_2009$q27_ches[EES_2009$q27==1826902] <- 1105 # UK SNP
EES_2009$q27_ches[EES_2009$q27==1826901] <- 1106 # UK Plaid

# Croatia
EES_2009$q27_ches[EES_2009$q27==1191320] <- 3102 # HR SDP
EES_2009$q27_ches[EES_2009$q27==1191412] <- 3105 # HR HNS
EES_2009$q27_ches[EES_2009$q27==1191511] <- 3101 # HR HDZ
EES_2009$q27_ches[EES_2009$q27==1191613] <- 3109 # HR HSP
EES_2009$q27_ches[EES_2009$q27==1191410] <- 3104 # HR HSLS
EES_2009$q27_ches[EES_2009$q27==1191952] <- 3107 # HR HDSSB
EES_2009$q27_ches[EES_2009$q27==1191330] <- 3112 # HR HL-SR
EES_2009$q27_ches[EES_2009$q27==1191110] <- 3114 # HR ORaH





##############same but for other questions

#Close to(q87_ches)
#Austria
EES_2009$q87_ches[EES_2009$q87==1040520] <- 1302 # AT OVP
EES_2009$q87_ches[EES_2009$q87==1040302] <- 1301 # AT SPO 
EES_2009$q87_ches[EES_2009$q87==1040423] <- 1309 # AT NEOS 
EES_2009$q87_ches[EES_2009$q87==1040110] <- 1304 # AT Grune 
EES_2009$q87_ches[EES_2009$q87==1040420] <- 1303 # AT FPO 
EES_2009$q87_ches[EES_2009$q87==1040600] <- 1307 # AT BZO

# Belgium (Flanders & Wallonia)
EES_2009$q87_ches[EES_2009$q87==1056325] <- 119 ##  BE PVDA
EES_2009$q87_ches[EES_2009$q87==1056521] <- 109 ##  BE CD&V
EES_2009$q87_ches[EES_2009$q87==1056327] <- 103 ##  BE SPA
EES_2009$q87_ches[EES_2009$q87==1056421] <- 107 ##  BE VLD
EES_2009$q87_ches[EES_2009$q87==1056913] <- 110 ##  BE NVA
EES_2009$q87_ches[EES_2009$q87==1056112] <- 105 ##  BE Groen
EES_2009$q87_ches[EES_2009$q87==1056711] <- 112 ##  BE VB
EES_2009$q87_ches[EES_2009$q87==1056522] <- 108 #   BE cdH
EES_2009$q87_ches[EES_2009$q87==1056322] <- 102 #   BE PS
EES_2009$q87_ches[EES_2009$q87==1056427] <- 106 #   BE MR
EES_2009$q87_ches[EES_2009$q87==1056111] <- 104 #   BE ECOLO

# Bulgaria (1100300, 1100601, 1100400 missing)
EES_2009$q87_ches[EES_2009$q87==1100600] <- 2010 # BG GERB
EES_2009$q87_ches[EES_2009$q87==1100900] <- 2004 # BG DPS
EES_2009$q87_ches[EES_2009$q87==1100700] <- 2007 # BG Attack
EES_2009$q87_ches[EES_2009$q87==1100602] <- 2015 # BG BBT
EES_2009$q87_ches[EES_2009$q87==1103001] <- 2016 # BG ABV
EES_2009$q87_ches[EES_2009$q87==1100001] <- 2002 # BG SDS

# Cyrpus (1196002 is missing)
EES_2009$q87_ches[EES_2009$q87==1196711] <- 4001 # CY DISY
EES_2009$q87_ches[EES_2009$q87==1196422] <- 4004 # CY DIKO
EES_2009$q87_ches[EES_2009$q87==1196322] <- 4005 # CY EDEK
EES_2009$q87_ches[EES_2009$q87==1196321] <- 4003 # CY AKEL
EES_2009$q87_ches[EES_2009$q87==1196110] <- 4006 # CY KOP

# Czech Republic (1203321 1203110 is missing)
EES_2009$q87_ches[EES_2009$q87==1203523] <- 2104 #  CZ KDU-CSL
EES_2009$q87_ches[EES_2009$q87==1203530] <- 2109 #  CZ TOP09
EES_2009$q87_ches[EES_2009$q87==1203320] <- 2101 #  CZ CSSD
EES_2009$q87_ches[EES_2009$q87==1203413] <- 2102 #  CZ ODS
EES_2009$q87_ches[EES_2009$q87==1203220] <- 2103 #  CZ KSCM
EES_2009$q87_ches[EES_2009$q87==1203413] <- 2111 #  CZ ANO2011
EES_2009$q87_ches[EES_2009$q87==1203953] <- 2113 #  CZ SVOBODNI

# Germany
EES_2009$q87_ches[EES_2009$q87==1276521] <- 301 # GE CDU
EES_2009$q87_ches[EES_2009$q87==1276320] <- 302 # GE SPD
EES_2009$q87_ches[EES_2009$q87==1276420] <- 303 # GE FDP
EES_2009$q87_ches[EES_2009$q87==1276113] <- 304 # GE Grunen
EES_2009$q87_ches[EES_2009$q87==1276321] <- 306 # GE Linke
EES_2009$q87_ches[EES_2009$q87==1276621] <- 310 # GE AfD
EES_2009$q87_ches[EES_2009$q87==1276951] <- 311 # GE Piraten

# Denmark (FolkB is missing)
EES_2009$q87_ches[EES_2009$q87==1208320] <- 201 # DK SD
EES_2009$q87_ches[EES_2009$q87==1208420] <- 211 # DK V
EES_2009$q87_ches[EES_2009$q87==1208330] <- 206 # DK SF
EES_2009$q87_ches[EES_2009$q87==1208720] <- 215 # DK DF
EES_2009$q87_ches[EES_2009$q87==1208410] <- 202 # DK RV
EES_2009$q87_ches[EES_2009$q87==1208421] <- 218 # DK LA
EES_2009$q87_ches[EES_2009$q87==1208620] <- 203 # DK KF

# Estonia (1233003 is missing)
EES_2009$q87_ches[EES_2009$q87==1233613] <- 2201 # EE IRL
EES_2009$q87_ches[EES_2009$q87==1233410] <- 2204 # EE SDE
EES_2009$q87_ches[EES_2009$q87==1233430] <- 2203 # EE ER
EES_2009$q87_ches[EES_2009$q87==1233411] <- 2202 # EE EK
EES_2009$q87_ches[EES_2009$q87==1233100] <- 2207 # EE EER

# Greece (1300116 is missing)
EES_2009$q87_ches[EES_2009$q87==1300511] <- 402 # GR ND
EES_2009$q87_ches[EES_2009$q87==1300215] <- 403 # GR SYRIZA
EES_2009$q87_ches[EES_2009$q87==1300313] <- 401 # GR PASOK
EES_2009$q87_ches[EES_2009$q87==1300611] <- 401 # GR ANEL
EES_2009$q87_ches[EES_2009$q87==1300710] <- 415 # GR XA
EES_2009$q87_ches[EES_2009$q87==1300225] <- 414 # GR DIMAR
EES_2009$q87_ches[EES_2009$q87==1300210] <- 404 # GR KKE
EES_2009$q87_ches[EES_2009$q87==1300323] <- 413 # GR Potami
EES_2009$q87_ches[EES_2009$q87==1300703] <- 410 # GR LAOS

# Spain 
EES_2009$q87_ches[EES_2009$q87==1724610] <- 502 # ESP PP
EES_2009$q87_ches[EES_2009$q87==1724320] <- 501 # ESP PSOE
EES_2009$q87_ches[EES_2009$q87==1724220] <- 504 # ESP IU
EES_2009$q87_ches[EES_2009$q87==1724010] <- 523 # ESP UPyd
EES_2009$q87_ches[EES_2009$q87==1724905] <- 511 # ESP ERC
EES_2009$q87_ches[EES_2009$q87==1724310] <- 526 # ESP Cs
EES_2009$q87_ches[EES_2009$q87==1724230] <- 525 # ESP Podemos
EES_2009$q87_ches[EES_2009$q87==1724007] <- 505 # ESP CiU
EES_2009$q87_ches[EES_2009$q87==1724902] <- 506 # ESP EAJ-PNV
EES_2009$q87_ches[EES_2009$q87==1724908] <- 513 # ESP BNG
EES_2009$q87_ches[EES_2009$q87==1724907] <- 517 # ESP CC

# Finland
EES_2009$q87_ches[EES_2009$q87==1246620] <- 1402 # FI KOK
EES_2009$q87_ches[EES_2009$q87==1246520] <- 1409 # FI KD
EES_2009$q87_ches[EES_2009$q87==1246320] <- 1401 # FI SDP
EES_2009$q87_ches[EES_2009$q87==1246810] <- 1403 # FI KESK
EES_2009$q87_ches[EES_2009$q87==1246901] <- 1406 # FI RKP/SFP
EES_2009$q87_ches[EES_2009$q87==1246110] <- 1408 # FI VIHR
EES_2009$q87_ches[EES_2009$q87==1246223] <- 1404 # FI VAS
EES_2009$q87_ches[EES_2009$q87==1246820] <- 1405 # FI PS

# France (Please check for 1250223. 1250636, 1250233 are missing)
EES_2009$q87_ches[EES_2009$q87==1250626] <- 609 # FR UMP
EES_2009$q87_ches[EES_2009$q87==1250320] <- 602 # FR PS
EES_2009$q87_ches[EES_2009$q87==1250720] <- 610 # FR FN
EES_2009$q87_ches[EES_2009$q87==1250110] <- 605 # FR EELV
EES_2009$q87_ches[EES_2009$q87==1250223] <- 601 # FR PCF
EES_2009$q87_ches[EES_2009$q87==1250336] <- 613 # FR MODEM

# Hungary
EES_2009$q87_ches[EES_2009$q87==1348700] <- 2308 # HU JOBBIK
EES_2009$q87_ches[EES_2009$q87==1348110] <- 2309 # HU LMP
EES_2009$q87_ches[EES_2009$q87==1348421] <- 2302 # HU Fidesz
EES_2009$q87_ches[EES_2009$q87==1348220] <- 2301 # HU MSzP
EES_2009$q87_ches[EES_2009$q87==1348120] <- 2310 # HU E14
EES_2009$q87_ches[EES_2009$q87==1348330] <- 2311 # HU DK

# Ireland
EES_2009$q87_ches[EES_2009$q87==1372520] <- 702 # IE FG
EES_2009$q87_ches[EES_2009$q87==1372320] <- 703 # IE Lab
EES_2009$q87_ches[EES_2009$q87==1372620] <- 701 # IE FF
EES_2009$q87_ches[EES_2009$q87==1372110] <- 705 # IE GP
EES_2009$q87_ches[EES_2009$q87==1372951] <- 707 # IE SF
EES_2009$q87_ches[EES_2009$q87==1372220] <- 708 # IE SP

# Italy (1380902, 1380630 are missing)
EES_2009$q87_ches[EES_2009$q87==1380331] <- 837 # IT PD
EES_2009$q87_ches[EES_2009$q87==1380610] <- 815 # IT FI
EES_2009$q87_ches[EES_2009$q87==1380720] <- 811 # IT LN
EES_2009$q87_ches[EES_2009$q87==1380956] <- 845 # IT M5S
EES_2009$q87_ches[EES_2009$q87==1380523] <- 814 # IT UDC
EES_2009$q87_ches[EES_2009$q87==1380007] <- 838 # IT SEl
EES_2009$q87_ches[EES_2009$q87==1380633] <- 848 # IT NCD
EES_2009$q87_ches[EES_2009$q87==1380631] <- 844 # IT FDL
EES_2009$q87_ches[EES_2009$q87==1380958] <- 827 # IT SVP

# Lithuania (1440420 is missing)
EES_2009$q87_ches[EES_2009$q87==1440620] <- 2506 # LT TS-LKD
EES_2009$q87_ches[EES_2009$q87==1440320] <- 2501 # LT LSDP
EES_2009$q87_ches[EES_2009$q87==1440421] <- 2518 # LT LRLS
EES_2009$q87_ches[EES_2009$q87==1440322] <- 2516 # LT DP
EES_2009$q87_ches[EES_2009$q87==1440621] <- 2515 # LT TT
EES_2009$q87_ches[EES_2009$q87==1440952] <- 2511 # LT LLRA
EES_2009$q87_ches[EES_2009$q87==1440524] <- 2507 # LT LVZS

# Luxembourg (1442220 is missing)
EES_2009$q87_ches[EES_2009$q87==1442520] <- 3801 # LU CSV
EES_2009$q87_ches[EES_2009$q87==1442320] <- 3804 # LU LSAP
EES_2009$q87_ches[EES_2009$q87==1442420] <- 3803 # LU DP
EES_2009$q87_ches[EES_2009$q87==1442113] <- 3802 # LU Greng
EES_2009$q87_ches[EES_2009$q87==1442222] <- 3806 # LU DL
EES_2009$q87_ches[EES_2009$q87==1442951] <- 3805 # LU ADR

# Latvia (1428620 1428422 1428424 are missing)
EES_2009$q87_ches[EES_2009$q87==1428610] <- 2412 # LV V
EES_2009$q87_ches[EES_2009$q87==1428317] <- 2410 # LV SDPS
EES_2009$q87_ches[EES_2009$q87==1428723] <- 2406 # LV NA
EES_2009$q87_ches[EES_2009$q87==1428110] <- 2405 # LV ZZS
EES_2009$q87_ches[EES_2009$q87==1428901] <- 2402 # LV LKS 

# Malta (1470100 is missing)
EES_2009$q87_ches[EES_2009$q87==1470300] <- 3701 # MT PL
EES_2009$q87_ches[EES_2009$q87==1470500] <- 3702 # MT PN

# Netherlands (1528528 - please check. EES codes as coalition of 2 parties)
EES_2009$q87_ches[EES_2009$q87==1528420] <- 1003 # NL VVD
EES_2009$q87_ches[EES_2009$q87==1528320] <- 1002 # NL PvdA
EES_2009$q87_ches[EES_2009$q87==1528600] <- 1017 # NL PVV
EES_2009$q87_ches[EES_2009$q87==1528220] <- 1014 # NL SP
EES_2009$q87_ches[EES_2009$q87==1528521] <- 1001 # NL CDA
EES_2009$q87_ches[EES_2009$q87==1528330] <- 1004 # NL D66
EES_2009$q87_ches[EES_2009$q87==1528526] <- 1016 # NL CU
EES_2009$q87_ches[EES_2009$q87==1528110] <- 1005 # NL GL
EES_2009$q87_ches[EES_2009$q87==1528951] <- 1018 # NL PvdD

# Poland
EES_2009$q87_ches[EES_2009$q87==1616435] <- 2603 # PL PO
EES_2009$q87_ches[EES_2009$q87==1616811] <- 2606 # PL PSL
EES_2009$q87_ches[EES_2009$q87==1616210] <- 2601 # PL SLD
EES_2009$q87_ches[EES_2009$q87==1616436] <- 2605 # PL PiS
EES_2009$q87_ches[EES_2009$q87==1616310] <- 2613 # PL RP
EES_2009$q87_ches[EES_2009$q87==1616001] <- 2614 # PL KNP
EES_2009$q87_ches[EES_2009$q87==1616002] <- 2616 # PL SP

# Portugal (1620314 - please check. EES codes as coalition of 2 parties)
EES_2009$q87_ches[EES_2009$q87==1620313] <- 1206 # PT PSD
EES_2009$q87_ches[EES_2009$q87==1620520] <- 1202 # PT PP
EES_2009$q87_ches[EES_2009$q87==1620311] <- 1205 # PT PS
EES_2009$q87_ches[EES_2009$q87==1620229] <- 1201 # PT CDU
EES_2009$q87_ches[EES_2009$q87==1620211] <- 1208 # PT BE
EES_2009$q87_ches[EES_2009$q87==1620110] <- 1209 # PT MPT

# Romania (1642700, 1642800 are missing. 1642502 is a coalition)
EES_2009$q87_ches[EES_2009$q87==1642300] <- 2701 # RO PSD
EES_2009$q87_ches[EES_2009$q87==1642401] <- 2705 # RO PNL
EES_2009$q87_ches[EES_2009$q87==1642400] <- 2704 # RO PDL
EES_2009$q87_ches[EES_2009$q87==1642981] <- 2710 # RO PP-DD
EES_2009$q87_ches[EES_2009$q87==1642900] <- 2706 # RO UDMR
EES_2009$q87_ches[EES_2009$q87==1642503] <- 2711 # RO PMP
EES_2009$q87_ches[EES_2009$q87==1642600] <- 2702 # RO PC

# Sweden
EES_2009$q87_ches[EES_2009$q87==1752320] <- 1602 # SE SAP
EES_2009$q87_ches[EES_2009$q87==1752620] <- 1605 # SE M
EES_2009$q87_ches[EES_2009$q87==1752110] <- 1607 # SE MP
EES_2009$q87_ches[EES_2009$q87==1752420] <- 1604 # SE FP
EES_2009$q87_ches[EES_2009$q87==1752810] <- 1603 # SE C
EES_2009$q87_ches[EES_2009$q87==1752700] <- 1610 # SE SD
EES_2009$q87_ches[EES_2009$q87==1752520] <- 1606 # SE KD
EES_2009$q87_ches[EES_2009$q87==1752220] <- 1601 # SE V
EES_2009$q87_ches[EES_2009$q87==1752953] <- 1612 # SE FI
EES_2009$q87_ches[EES_2009$q87==1752000] <- 1611 # SE PP

# Slovenia (1705450, 1705421, 1705324, 1705710, 1705952 are missing)
EES_2009$q87_ches[EES_2009$q87==1705340] <- 2914 # SI PS
EES_2009$q87_ches[EES_2009$q87==1705320] <- 2902 # SI SDS
EES_2009$q87_ches[EES_2009$q87==1705323] <- 2903 # SI SD
EES_2009$q87_ches[EES_2009$q87==1705951] <- 2906 # SI DeSUS
EES_2009$q87_ches[EES_2009$q87==1705522] <- 2905 # SI NSI
EES_2009$q87_ches[EES_2009$q87==1705521] <- 2904 # SI SLS

# Slovakia (1703222 is missing)
EES_2009$q87_ches[EES_2009$q87==1703521] <- 2805 # SK KDH
EES_2009$q87_ches[EES_2009$q87==1703523] <- 2802 # SK SDKU-DS
EES_2009$q87_ches[EES_2009$q87==1703954] <- 2804 # SK SMK-MKP
EES_2009$q87_ches[EES_2009$q87==1703423] <- 2803 # SK Smer-SD
EES_2009$q87_ches[EES_2009$q87==1703610] <- 2815 # SK NOVA
EES_2009$q87_ches[EES_2009$q87==1703440] <- 2812 # SK SaS
EES_2009$q87_ches[EES_2009$q87==1703620] <- 2814 # SK OLaNO
EES_2009$q87_ches[EES_2009$q87==1703955] <- 2813 # SK MH
EES_2009$q87_ches[EES_2009$q87==1703710] <- 2809 # SK SNS

# UK (1826210, 1826903, 1826724, 1826720 are missing)
EES_2009$q87_ches[EES_2009$q87==1826620] <- 1101 # UK Cons
EES_2009$q87_ches[EES_2009$q87==1826320] <- 1102 # UK Lab
EES_2009$q87_ches[EES_2009$q87==1826421] <- 1104 # UK LibDems
EES_2009$q87_ches[EES_2009$q87==1826110] <- 1107 # UK Green
EES_2009$q87_ches[EES_2009$q87==1826951] <- 1108 # UK UKIP
EES_2009$q87_ches[EES_2009$q87==1826902] <- 1105 # UK SNP
EES_2009$q87_ches[EES_2009$q87==1826901] <- 1106 # UK Plaid

# Croatia
EES_2009$q87_ches[EES_2009$q87==1191320] <- 3102 # HR SDP
EES_2009$q87_ches[EES_2009$q87==1191412] <- 3105 # HR HNS
EES_2009$q87_ches[EES_2009$q87==1191511] <- 3101 # HR HDZ
EES_2009$q87_ches[EES_2009$q87==1191613] <- 3109 # HR HSP
EES_2009$q87_ches[EES_2009$q87==1191410] <- 3104 # HR HSLS
EES_2009$q87_ches[EES_2009$q87==1191952] <- 3107 # HR HDSSB
EES_2009$q87_ches[EES_2009$q87==1191330] <- 3112 # HR HL-SR
EES_2009$q87_ches[EES_2009$q87==1191110] <- 3114 # HR ORaH





#############Adding the CHES codes for question 39 on likely to vote on, also add lv vote columns#############################
###This adds dummies for whether a voter is a likely voter for a specific party. 
x <- data.frame(chescode = unique_parties) #Take unique parties from EES_2014 code! Add some manually later on. 
x$charac <- "lv_ches"  #add likely voter chess plus code
x <- unite(x, ches_code, 2:1) #unite with 2 first to put the lv_ches in front
z <- data.frame(ches_code = c("lv_ches_1301", "lv_ches_1308", "lv_ches_117", "lv_ches_118","lv_ches_115", "lv_ches_216","lv_ches_507", "lv_ches_624","lv_ches_828", "lv_ches_803","lv_ches_1006", "lv_ches_1109")) #add party codes not in the unique parties vector from EES2014
x <- bind_rows(x, z) #bind to create one big dataframe
lv_column_names_matrix <- x
y <- x$ches_code #turn into simple character string
#lv_column_names_string <- y  #need later when I calculate scores per party. 
EES_2009[y] <- NA #add unique values (i.e. party codes) to dataframe as new columns. 
rm(x, y, z) #clean up working space

######now fill all the dummies up with a 1 is a voter is a likely voter for a party
attach(EES_2009) #attach database so I don't have to use $


#see the labels
describe(EES_2009$q39_p1)
get_labels(EES_2009$q39_2)
#how this works: the q39_x are sorted per country. The 8 indicates the likely score to vote. For different voters change this score.
#right now I only have Western-European countries (same as in WRR, OECD 15-1). If you want more just add the countries because the empty columns are already there. 

#get rid of the NAs in the 8 question
EES_2009$q39_p1[EES_2009$q39_p1 == 99 | EES_2009$q39_p1 == 89 | EES_2009$q39_p1  == 88 | EES_2009$q39_p1 == 77] <- NA
EES_2009$q39_p2[EES_2009$q39_p2 == 99 | EES_2009$q39_p2 == 89 | EES_2009$q39_p2  == 88 | EES_2009$q39_p2 == 77] <- NA
EES_2009$q39_p3[EES_2009$q39_p3 == 99 | EES_2009$q39_p3 == 89 | EES_2009$q39_p3  == 88 | EES_2009$q39_p3 == 77] <- NA
EES_2009$q39_p4[EES_2009$q39_p4 == 99 | EES_2009$q39_p4 == 89 | EES_2009$q39_p4  == 88 | EES_2009$q39_p4 == 77] <- NA
EES_2009$q39_p5[EES_2009$q39_p5 == 99 | EES_2009$q39_p5 == 89 | EES_2009$q39_p5  == 88 | EES_2009$q39_p5 == 77] <- NA
EES_2009$q39_p6[EES_2009$q39_p6 == 99 | EES_2009$q39_p6 == 89 | EES_2009$q39_p6  == 88 | EES_2009$q39_p6 == 77] <- NA
EES_2009$q39_p7[EES_2009$q39_p7 == 99 | EES_2009$q39_p7 == 89 | EES_2009$q39_p7  == 88 | EES_2009$q39_p7 == 77] <- NA
EES_2009$q39_p8[EES_2009$q39_p8 == 99 | EES_2009$q39_p8 == 89 | EES_2009$q39_p8  == 88 | EES_2009$q39_p8 == 77] <- NA
EES_2009$q39_p9[EES_2009$q39_p9 == 99 | EES_2009$q39_p9 == 89 | EES_2009$q39_p9  == 88 | EES_2009$q39_p9 == 77] <- NA
EES_2009$q39_p10[EES_2009$q39_p10 == 99 | EES_2009$q39_p10 == 89 | EES_2009$q39_p10  == 88 | EES_2009$q39_p10 == 77] <- NA
EES_2009$q39_p11[EES_2009$q39_p11 == 99 | EES_2009$q39_p11 == 89 | EES_2009$q39_p11  == 88 | EES_2009$q39_p11 == 77] <- NA
EES_2009$q39_p12[EES_2009$q39_p12 == 99 | EES_2009$q39_p12 == 89 | EES_2009$q39_p12  == 88 | EES_2009$q39_p12 == 77] <- NA
EES_2009$q39_p13[EES_2009$q39_p13 == 99 | EES_2009$q39_p13 == 89 | EES_2009$q39_p13  == 88 | EES_2009$q39_p13 == 77] <- NA
EES_2009$q39_p14[EES_2009$q39_p14 == 99 | EES_2009$q39_p14 == 89 | EES_2009$q39_p14  == 88 | EES_2009$q39_p14 == 77] <- NA
EES_2009$q39_p15[EES_2009$q39_p15 == 99 | EES_2009$q39_p15 == 89 | EES_2009$q39_p15  == 88 | EES_2009$q39_p15 == 77] <- NA

#Austria
EES_2009$lv_ches_1301 <- ifelse(t102==1040 & q39_p1 > 6 | q27_ches == 1301 | q28_ches == 1301 | q87_ches == 1301, 1, NA)
EES_2009$lv_ches_1302 <- ifelse(t102==1040 & q39_p2 > 6 | q27_ches == 1302 | q28_ches == 1302 | q87_ches == 1302, 1, NA)
EES_2009$lv_ches_1303 <- ifelse(t102==1040 & q39_p3 > 6 | q27_ches == 1303 | q28_ches == 1303 | q87_ches == 1303, 1, NA)
EES_2009$lv_ches_1307 <- ifelse(t102==1040 & q39_p4 > 6 | q27_ches == 1307 | q28_ches == 1307 | q87_ches == 1307, 1, NA)
EES_2009$lv_ches_1304 <- ifelse(t102==1040 & q39_p5 > 6 | q27_ches == 1304 | q28_ches == 1304 | q87_ches == 1304, 1, NA)
EES_2009$lv_ches_1308 <- ifelse(t102==1040 & q39_p6 > 6 | q27_ches == 1308 | q28_ches == 1308 | q87_ches == 1308, 1, NA)

#Belgium 
EES_2009$lv_ches_109 <- ifelse(t102==1056 & q39_p1 > 6 | q27_ches == 109 | q28_ches == 109 | q87_ches == 109, 1, NA)
EES_2009$lv_ches_107 <- ifelse(t102==1056 & q39_p2 > 6 | q27_ches == 107 | q28_ches == 107 | q87_ches == 107, 1, NA)
EES_2009$lv_ches_103 <- ifelse(t102==1056 & q39_p3 > 6 | q27_ches == 103 | q28_ches == 103 | q87_ches == 103, 1, NA)
EES_2009$lv_ches_112 <- ifelse(t102==1056 & q39_p4 > 6 | q27_ches == 112 | q28_ches == 112 | q87_ches == 112, 1, NA)
EES_2009$lv_ches_105 <- ifelse(t102==1056 & q39_p5 > 6 | q27_ches == 105 | q28_ches == 105 | q87_ches == 105, 1, NA)
EES_2009$lv_ches_110 <- ifelse(t102==1056 & q39_p6 > 6 | q27_ches == 110 | q28_ches == 110 | q87_ches == 110, 1, NA)
EES_2009$lv_ches_117 <- ifelse(t102==1056 & q39_p7 > 6 | q27_ches == 117 | q28_ches == 117 | q87_ches == 117, 1, EES_2009$lv_ches_119) #note not NA because double variable
EES_2009$lv_ches_118 <- ifelse(t102==1056 & q39_p8 > 6 | q27_ches == 118 | q28_ches == 118 | q87_ches == 118, 1, NA)
EES_2009$lv_ches_119 <- ifelse(t102==1056 & q39_p9 > 6 | q27_ches == 119 | q28_ches == 119 | q87_ches == 119, 1, NA)
EES_2009$lv_ches_108 <- ifelse(t102==1056 & q39_p10 > 6 | q27_ches == 108 | q28_ches == 108 | q87_ches == 108, 1, NA)
EES_2009$lv_ches_102 <- ifelse(t102==1056 & q39_p12 > 6 | q27_ches == 102 | q28_ches == 102 | q87_ches == 102, 1, NA)
EES_2009$lv_ches_115 <- ifelse(t102==1056 & q39_p13 > 6 | q27_ches == 115 | q28_ches == 115 | q87_ches == 115, 1, NA)

#Germany
EES_2009$lv_ches_301 <- ifelse(t102==1276 & q39_p1 > 6 | q27_ches == 301 | q28_ches == 301 | q87_ches == 301, 1, NA)
EES_2009$lv_ches_302 <- ifelse(t102==1276 & q39_p2 > 6 | q27_ches == 302 | q28_ches == 302 | q87_ches == 302, 1, NA)
EES_2009$lv_ches_304 <- ifelse(t102==1276 & q39_p3 > 6 | q27_ches == 304 | q28_ches == 304 | q87_ches == 304, 1, NA)
EES_2009$lv_ches_306 <- ifelse(t102==1276 & q39_p4 > 6 | q27_ches == 306 | q28_ches == 306 | q87_ches == 306, 1, NA)
EES_2009$lv_ches_303 <- ifelse(t102==1276 & q39_p5 > 6 | q27_ches == 303 | q28_ches == 303 | q87_ches == 303, 1, NA)

# Denmark
EES_2009$lv_ches_201 <- ifelse(t102==1208 & q39_p1 > 6 | q27_ches == 201 | q28_ches == 201 | q87_ches == 201, 1, NA)
EES_2009$lv_ches_202 <- ifelse(t102==1208 & q39_p2 > 6 | q27_ches == 202 | q28_ches == 202 | q87_ches == 202, 1, NA)
EES_2009$lv_ches_203 <- ifelse(t102==1208 & q39_p3 > 6 | q27_ches == 203 | q28_ches == 203 | q87_ches == 203, 1, NA)
EES_2009$lv_ches_206 <- ifelse(t102==1208 & q39_p4 > 6 | q27_ches == 206 | q28_ches == 206 | q87_ches == 206, 1, NA)
EES_2009$lv_ches_215 <- ifelse(t102==1208 & q39_p5 > 6 | q27_ches == 215 | q28_ches == 215 | q87_ches == 215, 1, NA)
EES_2009$lv_ches_211 <- ifelse(t102==1208 & q39_p6 > 6 | q27_ches == 211 | q28_ches == 211 | q87_ches == 211, 1, NA)
EES_2009$lv_ches_218 <- ifelse(t102==1208 & q39_p7 > 6 | q27_ches == 218 | q28_ches == 218 | q87_ches == 218, 1, NA)
EES_2009$lv_ches_216 <- ifelse(t102==1208 & q39_p8 > 6 | q27_ches == 216 | q28_ches == 216 | q87_ches == 216, 1, NA)

#Spain
EES_2009$lv_ches_502 <- ifelse(t102==1724 & q39_p1 > 6 | q27_ches == 502 | q28_ches == 502 | q87_ches == 502, 1, NA)
EES_2009$lv_ches_501 <- ifelse(t102==1724 & q39_p2 > 6 | q27_ches == 501 | q28_ches == 501 | q87_ches == 501, 1, NA)
EES_2009$lv_ches_504 <- ifelse(t102==1724 & q39_p3 > 6 | q27_ches == 504 | q28_ches == 504 | q87_ches == 504, 1, NA)
EES_2009$lv_ches_523 <- ifelse(t102==1724 & q39_p5 > 6 | q27_ches == 523 | q28_ches == 523 | q87_ches == 523, 1, NA) #is also 506 and 517
EES_2009$lv_ches_505 <- ifelse(t102==1724 & q39_p6 > 6 | q27_ches == 505 | q28_ches == 505 | q87_ches == 505, 1, NA)
EES_2009$lv_ches_511 <- ifelse(t102==1724 & q39_p7 > 6 | q27_ches == 511 | q28_ches == 511 | q87_ches == 511, 1, NA)
EES_2009$lv_ches_506 <- ifelse(t102==1724 & q39_p8 > 6 | q27_ches == 506 | q28_ches == 506 | q87_ches == 506, 1, NA)
EES_2009$lv_ches_513 <- ifelse(t102==1724 & q39_p9 > 6 | q27_ches == 513 | q28_ches == 513 | q87_ches == 513, 1, NA)
EES_2009$lv_ches_517 <- ifelse(t102==1724 & q39_p10 > 6 | q27_ches == 517 | q28_ches == 517 | q87_ches == 517, 1, NA)
EES_2009$lv_ches_507 <- ifelse(t102==1724 & q39_p12 > 6 | q27_ches == 507 | q28_ches == 507 | q87_ches == 507, 1, NA)

#Finland
EES_2009$lv_ches_1401 <- ifelse(t102==1246 & q39_p1 > 6 | q27_ches == 1401 | q28_ches == 1401 | q87_ches == 1401, 1, NA)
EES_2009$lv_ches_1403 <- ifelse(t102==1246 & q39_p2 > 6 | q27_ches == 1403 | q28_ches == 1403 | q87_ches == 1403, 1, NA)
EES_2009$lv_ches_1402 <- ifelse(t102==1246 & q39_p3 > 6 | q27_ches == 1402 | q28_ches == 1402 | q87_ches == 1402, 1, NA)
EES_2009$lv_ches_1404 <- ifelse(t102==1246 & q39_p4 > 6 | q27_ches == 1404 | q28_ches == 1404 | q87_ches == 1404, 1, NA)
EES_2009$lv_ches_1408 <- ifelse(t102==1246 & q39_p5 > 6 | q27_ches == 1408 | q28_ches == 1408 | q87_ches == 1408, 1, NA)
EES_2009$lv_ches_1406 <- ifelse(t102==1246 & q39_p6 > 6 | q27_ches == 1406 | q28_ches == 1406 | q87_ches == 1406, 1, NA)
EES_2009$lv_ches_1409 <- ifelse(t102==1246 & q39_p7 > 6 | q27_ches == 1409 | q28_ches == 1409 | q87_ches == 1409, 1, NA)
EES_2009$lv_ches_1405 <- ifelse(t102==1246 & q39_p8 > 6 | q27_ches == 1405 | q28_ches == 1405 | q87_ches == 1405, 1, NA)

#France
EES_2009$lv_ches_601 <- ifelse(t102==1250 & q39_p2 > 6 | q27_ches == 601 | q28_ches == 601 | q87_ches == 601, 1, NA)
EES_2009$lv_ches_602 <- ifelse(t102==1250 & q39_p3 > 6 | q27_ches == 602 | q28_ches == 602 | q87_ches == 602, 1, NA)
EES_2009$lv_ches_605 <- ifelse(t102==1250 & q39_p4 > 6 | q27_ches == 605 | q28_ches == 605 | q87_ches == 605, 1, NA)
EES_2009$lv_ches_613 <- ifelse(t102==1250 & q39_p5 > 6 | q27_ches == 613 | q28_ches == 613 | q87_ches == 613, 1, NA)
EES_2009$lv_ches_609 <- ifelse(t102==1250 & q39_p6 > 6 | q27_ches == 609 | q28_ches == 609 | q87_ches == 609, 1, NA)
EES_2009$lv_ches_610 <- ifelse(t102==1250 & q39_p7 > 6 | q27_ches == 610 | q28_ches == 610 | q87_ches == 610, 1, NA)
EES_2009$lv_ches_624 <- ifelse(t102==1250 & q39_p9 > 6 | q27_ches == 624 | q28_ches == 624 | q87_ches == 624, 1, NA)

#Ireland
EES_2009$lv_ches_701 <- ifelse(t102==1372 & q39_p1 > 6 | q27_ches == 701 | q28_ches == 701 | q87_ches == 701, 1, NA)
EES_2009$lv_ches_702 <- ifelse(t102==1372 & q39_p2 > 6 | q27_ches == 702 | q28_ches == 702 | q87_ches == 702, 1, NA)
EES_2009$lv_ches_705 <- ifelse(t102==1372 & q39_p3 > 6 | q27_ches == 705 | q28_ches == 705 | q87_ches == 705, 1, NA)
EES_2009$lv_ches_703 <- ifelse(t102==1372 & q39_p4 > 6 | q27_ches == 703 | q28_ches == 703 | q87_ches == 703, 1, NA)
EES_2009$lv_ches_707 <- ifelse(t102==1372 & q39_p5 > 6 | q27_ches == 707 | q28_ches == 707 | q87_ches == 707, 1, NA)

#Italy
EES_2009$lv_ches_811 <- ifelse(t102==1380 & q39_p2 > 6 | q27_ches == 811 | q28_ches == 811 | q87_ches == 811, 1, NA)
EES_2009$lv_ches_837 <- ifelse(t102==1380 & q39_p3 > 6 | q27_ches == 837 | q28_ches == 837 | q87_ches == 837, 1, NA)
EES_2009$lv_ches_828 <- ifelse(t102==1380 & q39_p4 > 6 | q27_ches == 828 | q28_ches == 828 | q87_ches == 828, 1, NA)
EES_2009$lv_ches_814 <- ifelse(t102==1380 & q39_p5 > 6 | q27_ches == 814 | q28_ches == 814 | q87_ches == 814, 1, NA)
EES_2009$lv_ches_803 <- ifelse(t102==1380 & q39_p6 > 6 | q27_ches == 803 | q28_ches == 803 | q87_ches == 803, 1, NA)
EES_2009$lv_ches_838 <- ifelse(t102==1380 & q39_p7 > 6 | q27_ches == 838 | q28_ches == 838 | q87_ches == 838, 1, NA)

#Luxembourg
EES_2009$lv_ches_3802 <- ifelse(t102==1442 & q39_p1 > 6 | q27_ches == 3802 | q28_ches == 3802 | q87_ches == 3802, 1, NA)
EES_2009$lv_ches_3804 <- ifelse(t102==1442 & q39_p2 > 6 | q27_ches == 3804 | q28_ches == 3804 | q87_ches == 3804, 1, NA)
EES_2009$lv_ches_3803 <- ifelse(t102==1442 & q39_p3 > 6 | q27_ches == 3803 | q28_ches == 3803 | q87_ches == 3803, 1, NA)
EES_2009$lv_ches_3801 <- ifelse(t102==1442 & q39_p4 > 6 | q27_ches == 3801 | q28_ches == 3801 | q87_ches == 3801, 1, NA)
EES_2009$lv_ches_3805 <- ifelse(t102==1442 & q39_p5 > 6 | q27_ches == 3805 | q28_ches == 3805 | q87_ches == 3805, 1, NA)
EES_2009$lv_ches_3806 <- ifelse(t102==1442 & q39_p6 > 6 | q27_ches == 3806 | q28_ches == 3806 | q87_ches == 3806, 1, NA)

#Netherlands
EES_2009$lv_ches_1002 <- ifelse(t102==1528 & q39_p1 > 6 | q27_ches == 1002 | q28_ches == 1002 | q87_ches == 1002, 1, NA)
EES_2009$lv_ches_1001 <- ifelse(t102==1528 & q39_p2 > 6 | q27_ches == 1001 | q28_ches == 1001 | q87_ches == 1001, 1, NA)
EES_2009$lv_ches_1003 <- ifelse(t102==1528 & q39_p3 > 6 | q27_ches == 1003 | q28_ches == 1003 | q87_ches == 1003, 1, NA)
EES_2009$lv_ches_1004 <- ifelse(t102==1528 & q39_p4 > 6 | q27_ches == 1004 | q28_ches == 1004 | q87_ches == 1004, 1, NA)
EES_2009$lv_ches_1005 <- ifelse(t102==1528 & q39_p5 > 6 | q27_ches == 1005 | q28_ches == 1005 | q87_ches == 1005, 1, NA)
EES_2009$lv_ches_1018 <- ifelse(t102==1528 & q39_p6 > 6 | q27_ches == 1018 | q28_ches == 1018 | q87_ches == 1018, 1, NA)
EES_2009$lv_ches_1016 <- ifelse(t102==1528 & q39_p7 > 6 | q27_ches == 1016 | q28_ches == 1016 | q87_ches == 1016, 1, NA)
EES_2009$lv_ches_1006 <- ifelse(t102==1528 & q39_p8 > 6 | q27_ches == 1006 | q28_ches == 1006 | q87_ches == 1006, 1, NA)
EES_2009$lv_ches_1014 <- ifelse(t102==1528 & q39_p9 > 6 | q27_ches == 1014 | q28_ches == 1014 | q87_ches == 1014, 1, NA)
EES_2009$lv_ches_1017 <- ifelse(t102==1528 & q39_p10 > 6 | q27_ches == 1017 | q28_ches == 1017 | q87_ches == 1017, 1, NA)

#Portugal
EES_2009$lv_ches_1208 <- ifelse(t102==1620 & q39_p1 > 6 | q27_ches == 1208 | q28_ches == 1208 | q87_ches == 1208, 1, NA)
EES_2009$lv_ches_1202 <- ifelse(t102==1620 & q39_p2 > 6 | q27_ches == 1202 | q28_ches == 1202 | q87_ches == 1202, 1, NA)
EES_2009$lv_ches_1201 <- ifelse(t102==1620 & q39_p3 > 6 | q27_ches == 1201 | q28_ches == 1201 | q87_ches == 1201, 1, NA)
EES_2009$lv_ches_1205 <- ifelse(t102==1620 & q39_p4 > 6 | q27_ches == 1205 | q28_ches == 1205 | q87_ches == 1205, 1, NA)
EES_2009$lv_ches_1206 <- ifelse(t102==1620 & q39_p5 > 6 | q27_ches == 1206 | q28_ches == 1206 | q87_ches == 1206, 1, NA)

#Sweden
EES_2009$lv_ches_1601 <- ifelse(t102==1752 & q39_p1 > 6 | q27_ches == 1601 | q28_ches == 1601 | q87_ches == 1601, 1, NA)
EES_2009$lv_ches_1602 <- ifelse(t102==1752 & q39_p2 > 6 | q27_ches == 1602 | q28_ches == 1602 | q87_ches == 1602, 1, NA)
EES_2009$lv_ches_1603 <- ifelse(t102==1752 & q39_p3 > 6 | q27_ches == 1603 | q28_ches == 1603 | q87_ches == 1603, 1, NA)
EES_2009$lv_ches_1604 <- ifelse(t102==1752 & q39_p4 > 6 | q27_ches == 1604 | q28_ches == 1604 | q87_ches == 1604, 1, NA)
EES_2009$lv_ches_1605 <- ifelse(t102==1752 & q39_p5 > 6 | q27_ches == 1605 | q28_ches == 1605 | q87_ches == 1605, 1, NA)
EES_2009$lv_ches_1606 <- ifelse(t102==1752 & q39_p6 > 6 | q27_ches == 1606 | q28_ches == 1606 | q87_ches == 1606, 1, NA)
EES_2009$lv_ches_1607 <- ifelse(t102==1752 & q39_p7 > 6 | q27_ches == 1607 | q28_ches == 1607 | q87_ches == 1607, 1, NA)
EES_2009$lv_ches_1610 <- ifelse(t102==1752 & q39_p8 > 6 | q27_ches == 1610 | q28_ches == 1610 | q87_ches == 1610, 1, NA)
EES_2009$lv_ches_1612 <- ifelse(t102==1752 & q39_p9 > 6 | q27_ches == 1612 | q28_ches == 1612 | q87_ches == 1612, 1, NA)

#UK
EES_2009$lv_ches_1102 <- ifelse(t102==1826 & q39_p1 > 6 | q27_ches == 1102 | q28_ches == 1102 | q87_ches == 1102, 1, NA)
EES_2009$lv_ches_1101 <- ifelse(t102==1826 & q39_p2 > 6 | q27_ches == 1101 | q28_ches == 1101 | q87_ches == 1101, 1, NA)
EES_2009$lv_ches_1104 <- ifelse(t102==1826 & q39_p3 > 6 | q27_ches == 1104 | q28_ches == 1104 | q87_ches == 1104, 1, NA)
EES_2009$lv_ches_1105 <- ifelse(t102==1826 & q39_p4 > 6 | q27_ches == 1105 | q28_ches == 1105 | q87_ches == 1105, 1, NA)
EES_2009$lv_ches_1106 <- ifelse(t102==1826 & q39_p5 > 6 | q27_ches == 1106 | q28_ches == 1106 | q87_ches == 1106, 1, NA)
EES_2009$lv_ches_1108 <- ifelse(t102==1826 & q39_p6 > 6 | q27_ches == 1108 | q28_ches == 1108 | q87_ches == 1108, 1, NA)
EES_2009$lv_ches_1109 <- ifelse(t102==1826 & q39_p7 > 6 | q27_ches == 1109 | q28_ches == 1109 | q87_ches == 1109, 1, NA)
EES_2009$lv_ches_1107 <- ifelse(t102==1826 & q39_p8 > 6 | q27_ches == 1107 | q28_ches == 1107 | q87_ches == 1107, 1, NA)







#############Prepare variables of interest (postitions on globalization issues: glob_pos)#################

#see the labels
describe(EES_2009$q80) #unification pushed further 10 is pro globalization
describe(EES_2009$q67) #immigration  5 is pro globalization

#remove NAs
EES_2009$q80[EES_2009$q80 == 77 | EES_2009$q80 == 88] <- NA
EES_2009$q67[EES_2009$q67 == 8 | EES_2009$q67 == 7] <- NA

# Variable is from 1 to 5. Needs to be from 0 to 10. normalized = (x-min(x))/(max(x)-min(x))
EES_2009$q67_rec <- 1 + (((EES_2009$q67 - 1) / (5-1)) * 10) #transfer to from 1 to 11 to get 0 out and line up with EES2014

#make EES q80 from 1 ro 11
EES_2009$q80 <- EES_2009$q80 + 1


#creating globaliation variable
x <- data.frame(EES_2009$q67_rec, EES_2009$q80)
library(psych)
psych::alpha(x) #using psych package to get cronbach alpha score.  0.29 so not high enough.. but no problem: SD still valid and for extreme scores I can look at individual variables. am not looking at voter positions or congruence so all good!
detach("package:psych", unload=TRUE) #detach because clashes with misc

#merge variables
EES_2009$glob_pos <- NA
EES_2009$glob_pos <- (as.numeric(EES_2009$q67_rec) + as.numeric(EES_2009$q80))  / 2 #creating new variable.
Hmisc::describe(EES_2009$glob_pos) #seems all good

attach(EES)

#############Globalization dimension: SD likely voters#########
attach(EES_2009)

#create database to dump everything in. 
party_vot_var_09 <- data.frame(party_ches = unique_parties, year = 2009) #really important that the unique parties stays in the right order! Used order from 2014 now.  
z <- data.frame(party_ches = c(1301, 1308, 117, 118, 115, 216, 507, 624, 828, 803, 1006, 1109), year = 2009) #add extra parties that were present in 09 dataset. Also add at the bottom of all code. 
party_vot_var_09 <- bind_rows(party_vot_var_09, z)

#do SD on glob pos for likley voters of a party. 
party_vot_var_09$sd_glob_lv <- c(sd(glob_pos[lv_ches_102 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_103 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_104 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_105 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_106 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_107 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_108 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_109 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_110 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_112 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_119 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_201 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_202 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_203 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_206 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_211 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_215 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_218 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_301 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_302 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_303 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_304 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_306 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_310 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_311 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_401 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_402 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_403 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_404 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_410 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_413 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_414 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_415 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_501 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_502 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_504 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_505 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_506 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_511 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_513 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_517 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_523 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_525 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_601 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_602 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_605 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_609 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_610 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_613 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_701 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_702 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_703 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_705 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_707 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_708 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_811 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_814 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_815 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_827 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_837 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_838 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_844 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_845 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_848 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1001 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1002 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1003 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1004 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1005 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1014 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1016 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1017 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1018 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1101 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1102 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1104 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1105 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1106 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1107 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1108 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1201 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1202 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1205 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1206 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1208 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1209 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1302 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1303 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1304 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1307 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1309 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1401 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1402 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1403 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1404 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1405 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1406 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1408 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1409 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1601 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1602 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1603 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1604 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1605 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1606 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1607 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1610 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1611 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1612 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2002 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2004 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2007 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2010 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2015 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2016 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2101 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2103 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2104 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2109 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2111 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2113 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2201 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2202 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2203 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2204 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2207 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2301 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2302 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2308 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2309 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2310 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2311 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2402 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2405 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2406 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2410 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2412 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2501 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2506 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2507 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2511 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2515 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2516 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2518 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2601 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2603 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2605 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2606 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2613 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2614 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2616 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2701 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2702 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2704 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2705 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2706 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2710 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2711 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2802 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2803 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2804 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2805 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2809 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2812 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2813 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2814 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2815 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2902 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2903 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2904 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2905 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2906 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_2914 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3101 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3102 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3104 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3105 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3107 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3109 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3112 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3114 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3701 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3702 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3801 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3802 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3803 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3804 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3805 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_3806 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_4001 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_4003 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_4004 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_4005 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_4006 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1301 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1308 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_117 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_118 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_115 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_216 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_507 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_624 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_828 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_803 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1006 == 1], na.rm = TRUE),
                                 sd(glob_pos[lv_ches_1109 == 1], na.rm = TRUE))



#############Globalization dimension: MAD############
#for using MAD
library(DescTools) #using this function to get MAD https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/MeanAD
MeanAD(glob_pos, na.rm = TRUE)
MeanAD(glob_pos[lv_ches_1017 == 1], na.rm = TRUE)  #SD for the PVV

#MAD of voters on glob dimension. 
party_vot_var_09$mad_glob_lv <- c(MeanAD(glob_pos[lv_ches_102 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_103 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_104 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_105 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_106 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_107 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_108 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_109 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_110 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_112 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_119 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_201 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_202 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_203 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_206 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_211 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_215 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_218 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_301 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_302 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_303 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_304 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_306 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_310 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_311 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_401 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_402 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_403 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_404 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_410 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_413 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_414 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_415 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_501 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_502 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_504 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_505 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_506 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_511 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_513 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_517 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_523 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_525 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_601 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_602 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_605 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_609 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_610 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_613 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_701 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_702 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_703 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_705 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_707 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_708 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_811 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_814 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_815 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_827 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_837 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_838 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_844 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_845 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_848 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1001 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1002 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1003 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1004 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1005 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1014 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1016 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1017 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1018 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1101 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1102 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1104 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1105 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1106 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1107 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1108 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1201 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1202 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1205 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1206 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1208 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1209 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1302 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1303 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1304 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1307 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1309 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1401 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1402 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1403 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1404 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1405 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1406 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1408 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1409 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1601 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1602 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1603 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1604 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1605 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1606 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1607 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1610 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1611 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1612 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2002 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2004 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2007 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2010 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2015 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2016 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2101 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2103 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2104 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2109 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2111 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2113 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2201 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2202 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2203 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2204 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2207 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2301 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2302 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2308 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2309 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2310 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2311 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2402 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2405 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2406 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2410 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2412 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2501 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2506 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2507 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2511 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2515 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2516 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2518 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2601 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2603 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2605 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2606 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2613 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2614 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2616 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2701 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2702 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2704 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2705 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2706 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2710 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2711 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2802 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2803 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2804 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2805 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2809 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2812 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2813 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2814 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2815 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2902 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2903 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2904 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2905 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2906 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_2914 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3101 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3102 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3104 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3105 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3107 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3109 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3112 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3114 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3701 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3702 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3801 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3802 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3803 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3804 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3805 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_3806 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_4001 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_4003 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_4004 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_4005 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_4006 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1301 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1308 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_117 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_118 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_115 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_216 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_507 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_624 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_828 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_803 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1006 == 1], na.rm = TRUE),
                                  MeanAD(glob_pos[lv_ches_1109 == 1], na.rm = TRUE))

detach("package:DescTools", unload=TRUE)

party_vot_var_09$mad_glob_lv <- ifelse(is.nan(party_vot_var_09$mad_glob_lv) == TRUE, NA, party_vot_var_09$mad_glob_lv)




#############Globalization dimension: mean positions for likely voters#############
party_vot_var_09$mean_glob_lv <- c(mean(glob_pos[lv_ches_102 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_103 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_104 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_105 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_106 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_107 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_108 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_109 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_110 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_112 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_119 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_201 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_202 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_203 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_206 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_211 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_215 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_218 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_301 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_302 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_303 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_304 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_306 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_310 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_311 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_401 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_402 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_403 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_404 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_410 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_413 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_414 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_415 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_501 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_502 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_504 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_505 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_506 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_511 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_513 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_517 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_523 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_525 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_601 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_602 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_605 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_609 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_610 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_613 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_701 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_702 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_703 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_705 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_707 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_708 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_811 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_814 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_815 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_827 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_837 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_838 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_844 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_845 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_848 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1001 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1002 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1003 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1004 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1005 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1014 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1016 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1017 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1018 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1101 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1102 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1104 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1105 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1106 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1107 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1108 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1201 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1202 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1205 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1206 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1208 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1209 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1302 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1303 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1304 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1307 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1309 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1401 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1402 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1403 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1404 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1405 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1406 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1408 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1409 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1601 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1602 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1603 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1604 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1605 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1606 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1607 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1610 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1611 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1612 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2002 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2004 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2007 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2010 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2015 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2016 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2101 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2103 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2104 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2109 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2111 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2113 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2201 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2202 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2203 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2204 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2207 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2301 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2302 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2308 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2309 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2310 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2311 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2402 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2405 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2406 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2410 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2412 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2501 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2506 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2507 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2511 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2515 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2516 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2518 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2601 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2603 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2605 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2606 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2613 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2614 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2616 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2701 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2702 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2704 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2705 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2706 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2710 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2711 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2802 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2803 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2804 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2805 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2809 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2812 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2813 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2814 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2815 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2902 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2903 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2904 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2905 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2906 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_2914 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3101 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3102 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3104 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3105 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3107 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3109 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3112 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3114 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3701 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3702 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3801 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3802 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3803 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3804 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3805 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_3806 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_4001 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_4003 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_4004 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_4005 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_4006 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1301 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1308 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_117 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_118 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_115 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_216 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_507 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_624 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_828 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_803 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1006 == 1], na.rm = TRUE),
                                   mean(glob_pos[lv_ches_1109 == 1], na.rm = TRUE))

party_vot_var_09$mean_glob_lv <- ifelse(is.nan(party_vot_var_09$mean_glob_lv) == TRUE, NA, party_vot_var_09$mean_glob_lv)





#############Globalization dimension: percentage extreme voters##############################
#Function for percentage extreme. 
#take the sum of all not NA values for a question. This is the total n for that question. Then take all extreme values
#for that same question. Do this three times for both and calculate percentage extreme scores.
#note that not all people answering all three questions is taken into account by taking separate Ns for each question
#the lvches is the name for columns with dummies for likely voters. 
attach(EES_2009)

per_ext_voters <- function(lvches)
{
  n1 = sum(!is.na((as.numeric(q80[lvches == 1]))))
  t1 = sum(!is.na((as.numeric(q80[lvches == 1 & (q80 == 11 | q80 == 1) ]))))
  n3 = sum(!is.na((as.numeric(q67_rec[lvches == 1]))))
  t3 = sum(!is.na((as.numeric(q67_rec[lvches == 1 & (q67_rec == 11 | q67_rec == 1) ]))))
  pext <- (t1 + t3)/(n1 + n3) * 100
  return(pext)
}


####################Adding the code for all parties. Note that, just like above, all NAs are for Eastern European countries
party_vot_var_09$perext_glob_lv <- c(per_ext_voters(lv_ches_102),
                                     per_ext_voters(lv_ches_103),
                                     per_ext_voters(lv_ches_104),
                                     per_ext_voters(lv_ches_105),
                                     per_ext_voters(lv_ches_106),
                                     per_ext_voters(lv_ches_107),
                                     per_ext_voters(lv_ches_108),
                                     per_ext_voters(lv_ches_109),
                                     per_ext_voters(lv_ches_110),
                                     per_ext_voters(lv_ches_112),
                                     per_ext_voters(lv_ches_119),
                                     per_ext_voters(lv_ches_201),
                                     per_ext_voters(lv_ches_202),
                                     per_ext_voters(lv_ches_203),
                                     per_ext_voters(lv_ches_206),
                                     per_ext_voters(lv_ches_211),
                                     per_ext_voters(lv_ches_215),
                                     per_ext_voters(lv_ches_218),
                                     per_ext_voters(lv_ches_301),
                                     per_ext_voters(lv_ches_302),
                                     per_ext_voters(lv_ches_303),
                                     per_ext_voters(lv_ches_304),
                                     per_ext_voters(lv_ches_306),
                                     per_ext_voters(lv_ches_310),
                                     per_ext_voters(lv_ches_311),
                                     per_ext_voters(lv_ches_401),
                                     per_ext_voters(lv_ches_402),
                                     per_ext_voters(lv_ches_403),
                                     per_ext_voters(lv_ches_404),
                                     per_ext_voters(lv_ches_410),
                                     per_ext_voters(lv_ches_413),
                                     per_ext_voters(lv_ches_414),
                                     per_ext_voters(lv_ches_415),
                                     per_ext_voters(lv_ches_501),
                                     per_ext_voters(lv_ches_502),
                                     per_ext_voters(lv_ches_504),
                                     per_ext_voters(lv_ches_505),
                                     per_ext_voters(lv_ches_506),
                                     per_ext_voters(lv_ches_511),
                                     per_ext_voters(lv_ches_513),
                                     per_ext_voters(lv_ches_517),
                                     per_ext_voters(lv_ches_523),
                                     per_ext_voters(lv_ches_525),
                                     per_ext_voters(lv_ches_601),
                                     per_ext_voters(lv_ches_602),
                                     per_ext_voters(lv_ches_605),
                                     per_ext_voters(lv_ches_609),
                                     per_ext_voters(lv_ches_610),
                                     per_ext_voters(lv_ches_613),
                                     per_ext_voters(lv_ches_701),
                                     per_ext_voters(lv_ches_702),
                                     per_ext_voters(lv_ches_703),
                                     per_ext_voters(lv_ches_705),
                                     per_ext_voters(lv_ches_707),
                                     per_ext_voters(lv_ches_708),
                                     per_ext_voters(lv_ches_811),
                                     per_ext_voters(lv_ches_814),
                                     per_ext_voters(lv_ches_815),
                                     per_ext_voters(lv_ches_827),
                                     per_ext_voters(lv_ches_837),
                                     per_ext_voters(lv_ches_838),
                                     per_ext_voters(lv_ches_844),
                                     per_ext_voters(lv_ches_845),
                                     per_ext_voters(lv_ches_848),
                                     per_ext_voters(lv_ches_1001),
                                     per_ext_voters(lv_ches_1002),
                                     per_ext_voters(lv_ches_1003),
                                     per_ext_voters(lv_ches_1004),
                                     per_ext_voters(lv_ches_1005),
                                     per_ext_voters(lv_ches_1014),
                                     per_ext_voters(lv_ches_1016),
                                     per_ext_voters(lv_ches_1017),
                                     per_ext_voters(lv_ches_1018),
                                     per_ext_voters(lv_ches_1101),
                                     per_ext_voters(lv_ches_1102),
                                     per_ext_voters(lv_ches_1104),
                                     per_ext_voters(lv_ches_1105),
                                     per_ext_voters(lv_ches_1106),
                                     per_ext_voters(lv_ches_1107),
                                     per_ext_voters(lv_ches_1108),
                                     per_ext_voters(lv_ches_1201),
                                     per_ext_voters(lv_ches_1202),
                                     per_ext_voters(lv_ches_1205),
                                     per_ext_voters(lv_ches_1206),
                                     per_ext_voters(lv_ches_1208),
                                     per_ext_voters(lv_ches_1209),
                                     per_ext_voters(lv_ches_1302),
                                     per_ext_voters(lv_ches_1303),
                                     per_ext_voters(lv_ches_1304),
                                     per_ext_voters(lv_ches_1307),
                                     per_ext_voters(lv_ches_1309),
                                     per_ext_voters(lv_ches_1401),
                                     per_ext_voters(lv_ches_1402),
                                     per_ext_voters(lv_ches_1403),
                                     per_ext_voters(lv_ches_1404),
                                     per_ext_voters(lv_ches_1405),
                                     per_ext_voters(lv_ches_1406),
                                     per_ext_voters(lv_ches_1408),
                                     per_ext_voters(lv_ches_1409),
                                     per_ext_voters(lv_ches_1601),
                                     per_ext_voters(lv_ches_1602),
                                     per_ext_voters(lv_ches_1603),
                                     per_ext_voters(lv_ches_1604),
                                     per_ext_voters(lv_ches_1605),
                                     per_ext_voters(lv_ches_1606),
                                     per_ext_voters(lv_ches_1607),
                                     per_ext_voters(lv_ches_1610),
                                     per_ext_voters(lv_ches_1611),
                                     per_ext_voters(lv_ches_1612),
                                     per_ext_voters(lv_ches_2002),
                                     per_ext_voters(lv_ches_2004),
                                     per_ext_voters(lv_ches_2007),
                                     per_ext_voters(lv_ches_2010),
                                     per_ext_voters(lv_ches_2015),
                                     per_ext_voters(lv_ches_2016),
                                     per_ext_voters(lv_ches_2101),
                                     per_ext_voters(lv_ches_2103),
                                     per_ext_voters(lv_ches_2104),
                                     per_ext_voters(lv_ches_2109),
                                     per_ext_voters(lv_ches_2111),
                                     per_ext_voters(lv_ches_2113),
                                     per_ext_voters(lv_ches_2201),
                                     per_ext_voters(lv_ches_2202),
                                     per_ext_voters(lv_ches_2203),
                                     per_ext_voters(lv_ches_2204),
                                     per_ext_voters(lv_ches_2207),
                                     per_ext_voters(lv_ches_2301),
                                     per_ext_voters(lv_ches_2302),
                                     per_ext_voters(lv_ches_2308),
                                     per_ext_voters(lv_ches_2309),
                                     per_ext_voters(lv_ches_2310),
                                     per_ext_voters(lv_ches_2311),
                                     per_ext_voters(lv_ches_2402),
                                     per_ext_voters(lv_ches_2405),
                                     per_ext_voters(lv_ches_2406),
                                     per_ext_voters(lv_ches_2410),
                                     per_ext_voters(lv_ches_2412),
                                     per_ext_voters(lv_ches_2501),
                                     per_ext_voters(lv_ches_2506),
                                     per_ext_voters(lv_ches_2507),
                                     per_ext_voters(lv_ches_2511),
                                     per_ext_voters(lv_ches_2515),
                                     per_ext_voters(lv_ches_2516),
                                     per_ext_voters(lv_ches_2518),
                                     per_ext_voters(lv_ches_2601),
                                     per_ext_voters(lv_ches_2603),
                                     per_ext_voters(lv_ches_2605),
                                     per_ext_voters(lv_ches_2606),
                                     per_ext_voters(lv_ches_2613),
                                     per_ext_voters(lv_ches_2614),
                                     per_ext_voters(lv_ches_2616),
                                     per_ext_voters(lv_ches_2701),
                                     per_ext_voters(lv_ches_2702),
                                     per_ext_voters(lv_ches_2704),
                                     per_ext_voters(lv_ches_2705),
                                     per_ext_voters(lv_ches_2706),
                                     per_ext_voters(lv_ches_2710),
                                     per_ext_voters(lv_ches_2711),
                                     per_ext_voters(lv_ches_2802),
                                     per_ext_voters(lv_ches_2803),
                                     per_ext_voters(lv_ches_2804),
                                     per_ext_voters(lv_ches_2805),
                                     per_ext_voters(lv_ches_2809),
                                     per_ext_voters(lv_ches_2812),
                                     per_ext_voters(lv_ches_2813),
                                     per_ext_voters(lv_ches_2814),
                                     per_ext_voters(lv_ches_2815),
                                     per_ext_voters(lv_ches_2902),
                                     per_ext_voters(lv_ches_2903),
                                     per_ext_voters(lv_ches_2904),
                                     per_ext_voters(lv_ches_2905),
                                     per_ext_voters(lv_ches_2906),
                                     per_ext_voters(lv_ches_2914),
                                     per_ext_voters(lv_ches_3101),
                                     per_ext_voters(lv_ches_3102),
                                     per_ext_voters(lv_ches_3104),
                                     per_ext_voters(lv_ches_3105),
                                     per_ext_voters(lv_ches_3107),
                                     per_ext_voters(lv_ches_3109),
                                     per_ext_voters(lv_ches_3112),
                                     per_ext_voters(lv_ches_3114),
                                     per_ext_voters(lv_ches_3701),
                                     per_ext_voters(lv_ches_3702),
                                     per_ext_voters(lv_ches_3801),
                                     per_ext_voters(lv_ches_3802),
                                     per_ext_voters(lv_ches_3803),
                                     per_ext_voters(lv_ches_3804),
                                     per_ext_voters(lv_ches_3805),
                                     per_ext_voters(lv_ches_3806),
                                     per_ext_voters(lv_ches_4001),
                                     per_ext_voters(lv_ches_4003),
                                     per_ext_voters(lv_ches_4004),
                                     per_ext_voters(lv_ches_4005),
                                     per_ext_voters(lv_ches_4006),
                                     per_ext_voters(lv_ches_1301),
                                     per_ext_voters(lv_ches_1308),
                                     per_ext_voters(lv_ches_117),
                                     per_ext_voters(lv_ches_118),
                                     per_ext_voters(lv_ches_115),
                                     per_ext_voters(lv_ches_216),
                                     per_ext_voters(lv_ches_507),
                                     per_ext_voters(lv_ches_624),
                                     per_ext_voters(lv_ches_828),
                                     per_ext_voters(lv_ches_803),
                                     per_ext_voters(lv_ches_1006),
                                     per_ext_voters(lv_ches_1109))



party_vot_var_09$perext_glob_lv <- ifelse(is.nan(party_vot_var_09$perext_glob_lv) == TRUE, NA, party_vot_var_09$perext_glob_lv)



#############Globalization dimension: Average distance from country mean##########################################
##Can function as an add-on to percentage of extreme scores. 


library(DescTools) #using this function to get MAD https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/MeanAD

party_mad <- function(party, var, country) #MAD for likely voters of a party, on a variable, in a country
{
  country_mean <-  function(var, country) #country_mean calculates the mean on a variable in a country
  {
    x <- mean(var[t102 == country], na.rm=TRUE)
    return(x)
  }
  y <- MeanAD(var[party == 1], FUN=country_mean(var, country), na.rm=TRUE) #MeanAD calculates the mean distance on a variable for likely voters in a country where the midpoint is the return from above, i.e. country mean. 
  return(y) 
}

party_mad(lv_ches_1017, glob_pos, 1528) #PVV far away from country mean
party_mad(lv_ches_1003, glob_pos, 1056) #VVD closer to country mean


party_vot_var_09$mad_glob_lv_countrymean <- c(party_mad(lv_ches_102, glob_pos, 1056),
                                              party_mad(lv_ches_103, glob_pos, 1056),
                                              party_mad(lv_ches_104, glob_pos, 1056),
                                              party_mad(lv_ches_105, glob_pos, 1056),
                                              party_mad(lv_ches_106, glob_pos, 1056),
                                              party_mad(lv_ches_107, glob_pos, 1056),
                                              party_mad(lv_ches_108, glob_pos, 1056),
                                              party_mad(lv_ches_109, glob_pos, 1056),
                                              party_mad(lv_ches_110, glob_pos, 1056),
                                              party_mad(lv_ches_112, glob_pos, 1056),
                                              party_mad(lv_ches_119, glob_pos, 1056),
                                              party_mad(lv_ches_201, glob_pos, 1208),
                                              party_mad(lv_ches_202, glob_pos, 1208),
                                              party_mad(lv_ches_203, glob_pos, 1208),
                                              party_mad(lv_ches_206, glob_pos, 1208),
                                              party_mad(lv_ches_211, glob_pos, 1208),
                                              party_mad(lv_ches_215, glob_pos, 1208),
                                              party_mad(lv_ches_218, glob_pos, 1208),
                                              party_mad(lv_ches_301, glob_pos, 1276),
                                              party_mad(lv_ches_302, glob_pos, 1276),
                                              party_mad(lv_ches_303, glob_pos, 1276),
                                              party_mad(lv_ches_304, glob_pos, 1276),
                                              party_mad(lv_ches_306, glob_pos, 1276),
                                              party_mad(lv_ches_310, glob_pos, 1276),
                                              party_mad(lv_ches_311, glob_pos, 1276),
                                              party_mad(lv_ches_401, glob_pos, 1300),
                                              party_mad(lv_ches_402, glob_pos, 1300),
                                              party_mad(lv_ches_403, glob_pos, 1300),
                                              party_mad(lv_ches_404, glob_pos, 1300),
                                              party_mad(lv_ches_410, glob_pos, 1300),
                                              party_mad(lv_ches_413, glob_pos, 1300),
                                              party_mad(lv_ches_414, glob_pos, 1300),
                                              party_mad(lv_ches_415, glob_pos, 1300),
                                              party_mad(lv_ches_501, glob_pos, 1724),
                                              party_mad(lv_ches_502, glob_pos, 1724),
                                              party_mad(lv_ches_504, glob_pos, 1724),
                                              party_mad(lv_ches_505, glob_pos, 1724),
                                              party_mad(lv_ches_506, glob_pos, 1724),
                                              party_mad(lv_ches_511, glob_pos, 1724),
                                              party_mad(lv_ches_513, glob_pos, 1724),
                                              party_mad(lv_ches_517, glob_pos, 1724),
                                              party_mad(lv_ches_523, glob_pos, 1724),
                                              party_mad(lv_ches_525, glob_pos, 1724),
                                              party_mad(lv_ches_601, glob_pos, 1250),
                                              party_mad(lv_ches_602, glob_pos, 1250),
                                              party_mad(lv_ches_605, glob_pos, 1250),
                                              party_mad(lv_ches_609, glob_pos, 1250),
                                              party_mad(lv_ches_610, glob_pos, 1250),
                                              party_mad(lv_ches_613, glob_pos, 1250),
                                              party_mad(lv_ches_701, glob_pos, 1372),
                                              party_mad(lv_ches_702, glob_pos, 1372),
                                              party_mad(lv_ches_703, glob_pos, 1372),
                                              party_mad(lv_ches_705, glob_pos, 1372),
                                              party_mad(lv_ches_707, glob_pos, 1372),
                                              party_mad(lv_ches_708, glob_pos, 1372),
                                              party_mad(lv_ches_811, glob_pos, 1380),
                                              party_mad(lv_ches_814, glob_pos, 1380),
                                              party_mad(lv_ches_815, glob_pos, 1380),
                                              party_mad(lv_ches_827, glob_pos, 1380),
                                              party_mad(lv_ches_837, glob_pos, 1380),
                                              party_mad(lv_ches_838, glob_pos, 1380),
                                              party_mad(lv_ches_844, glob_pos, 1380),
                                              party_mad(lv_ches_845, glob_pos, 1380),
                                              party_mad(lv_ches_848, glob_pos, 1380),
                                              party_mad(lv_ches_1001, glob_pos, 1528),
                                              party_mad(lv_ches_1002, glob_pos, 1528),
                                              party_mad(lv_ches_1003, glob_pos, 1528),
                                              party_mad(lv_ches_1004, glob_pos, 1528),
                                              party_mad(lv_ches_1005, glob_pos, 1528),
                                              party_mad(lv_ches_1014, glob_pos, 1528),
                                              party_mad(lv_ches_1016, glob_pos, 1528),
                                              party_mad(lv_ches_1017, glob_pos, 1528),
                                              party_mad(lv_ches_1018, glob_pos, 1528),
                                              party_mad(lv_ches_1101, glob_pos, 1826),
                                              party_mad(lv_ches_1102, glob_pos, 1826),
                                              party_mad(lv_ches_1104, glob_pos, 1826),
                                              party_mad(lv_ches_1105, glob_pos, 1826),
                                              party_mad(lv_ches_1106, glob_pos, 1826),
                                              party_mad(lv_ches_1107, glob_pos, 1826),
                                              party_mad(lv_ches_1108, glob_pos, 1826),
                                              party_mad(lv_ches_1201, glob_pos, 1620),
                                              party_mad(lv_ches_1202, glob_pos, 1620),
                                              party_mad(lv_ches_1205, glob_pos, 1620),
                                              party_mad(lv_ches_1206, glob_pos, 1620),
                                              party_mad(lv_ches_1208, glob_pos, 1620),
                                              party_mad(lv_ches_1209, glob_pos, 1620),
                                              party_mad(lv_ches_1302, glob_pos, 1040),
                                              party_mad(lv_ches_1303, glob_pos, 1040),
                                              party_mad(lv_ches_1304, glob_pos, 1040),
                                              party_mad(lv_ches_1307, glob_pos, 1040),
                                              party_mad(lv_ches_1309, glob_pos, 1040),
                                              party_mad(lv_ches_1401, glob_pos, 1246),
                                              party_mad(lv_ches_1402, glob_pos, 1246),
                                              party_mad(lv_ches_1403, glob_pos, 1246),
                                              party_mad(lv_ches_1404, glob_pos, 1246),
                                              party_mad(lv_ches_1405, glob_pos, 1246),
                                              party_mad(lv_ches_1406, glob_pos, 1246),
                                              party_mad(lv_ches_1408, glob_pos, 1246),
                                              party_mad(lv_ches_1409, glob_pos, 1246),
                                              party_mad(lv_ches_1601, glob_pos, 1752),
                                              party_mad(lv_ches_1602, glob_pos, 1752),
                                              party_mad(lv_ches_1603, glob_pos, 1752),
                                              party_mad(lv_ches_1604, glob_pos, 1752),
                                              party_mad(lv_ches_1605, glob_pos, 1752),
                                              party_mad(lv_ches_1606, glob_pos, 1752),
                                              party_mad(lv_ches_1607, glob_pos, 1752),
                                              party_mad(lv_ches_1610, glob_pos, 1752),
                                              party_mad(lv_ches_1611, glob_pos, 1752),
                                              party_mad(lv_ches_1612, glob_pos, 1752),
                                              party_mad(lv_ches_2002, glob_pos, 1100),
                                              party_mad(lv_ches_2004, glob_pos, 1100),
                                              party_mad(lv_ches_2007, glob_pos, 1100),
                                              party_mad(lv_ches_2010, glob_pos, 1100),
                                              party_mad(lv_ches_2015, glob_pos, 1100),
                                              party_mad(lv_ches_2016, glob_pos, 1100),
                                              party_mad(lv_ches_2101, glob_pos, 1203),
                                              party_mad(lv_ches_2103, glob_pos, 1203),
                                              party_mad(lv_ches_2104, glob_pos, 1203),
                                              party_mad(lv_ches_2109, glob_pos, 1203),
                                              party_mad(lv_ches_2111, glob_pos, 1203),
                                              party_mad(lv_ches_2113, glob_pos, 1203),
                                              party_mad(lv_ches_2201, glob_pos, 1233),
                                              party_mad(lv_ches_2202, glob_pos, 1233),
                                              party_mad(lv_ches_2203, glob_pos, 1233),
                                              party_mad(lv_ches_2204, glob_pos, 1233),
                                              party_mad(lv_ches_2207, glob_pos, 1233),
                                              party_mad(lv_ches_2301, glob_pos, 1348),
                                              party_mad(lv_ches_2302, glob_pos, 1348),
                                              party_mad(lv_ches_2308, glob_pos, 1348),
                                              party_mad(lv_ches_2309, glob_pos, 1348),
                                              party_mad(lv_ches_2310, glob_pos, 1348),
                                              party_mad(lv_ches_2311, glob_pos, 1348),
                                              party_mad(lv_ches_2402, glob_pos, 1428),
                                              party_mad(lv_ches_2405, glob_pos, 1428),
                                              party_mad(lv_ches_2406, glob_pos, 1428),
                                              party_mad(lv_ches_2410, glob_pos, 1428),
                                              party_mad(lv_ches_2412, glob_pos, 1428),
                                              party_mad(lv_ches_2501, glob_pos, 1440),
                                              party_mad(lv_ches_2506, glob_pos, 1440),
                                              party_mad(lv_ches_2507, glob_pos, 1440),
                                              party_mad(lv_ches_2511, glob_pos, 1440),
                                              party_mad(lv_ches_2515, glob_pos, 1440),
                                              party_mad(lv_ches_2516, glob_pos, 1440),
                                              party_mad(lv_ches_2518, glob_pos, 1440),
                                              party_mad(lv_ches_2601, glob_pos, 1616),
                                              party_mad(lv_ches_2603, glob_pos, 1616),
                                              party_mad(lv_ches_2605, glob_pos, 1616),
                                              party_mad(lv_ches_2606, glob_pos, 1616),
                                              party_mad(lv_ches_2613, glob_pos, 1616),
                                              party_mad(lv_ches_2614, glob_pos, 1616),
                                              party_mad(lv_ches_2616, glob_pos, 1616),
                                              party_mad(lv_ches_2701, glob_pos, 1642),
                                              party_mad(lv_ches_2702, glob_pos, 1642),
                                              party_mad(lv_ches_2704, glob_pos, 1642),
                                              party_mad(lv_ches_2705, glob_pos, 1642),
                                              party_mad(lv_ches_2706, glob_pos, 1642),
                                              party_mad(lv_ches_2710, glob_pos, 1642),
                                              party_mad(lv_ches_2711, glob_pos, 1642),
                                              party_mad(lv_ches_2802, glob_pos, 1703),
                                              party_mad(lv_ches_2803, glob_pos, 1703),
                                              party_mad(lv_ches_2804, glob_pos, 1703),
                                              party_mad(lv_ches_2805, glob_pos, 1703),
                                              party_mad(lv_ches_2809, glob_pos, 1703),
                                              party_mad(lv_ches_2812, glob_pos, 1703),
                                              party_mad(lv_ches_2813, glob_pos, 1703),
                                              party_mad(lv_ches_2814, glob_pos, 1703),
                                              party_mad(lv_ches_2815, glob_pos, 1703),
                                              party_mad(lv_ches_2902, glob_pos, 1705),
                                              party_mad(lv_ches_2903, glob_pos, 1705),
                                              party_mad(lv_ches_2904, glob_pos, 1705),
                                              party_mad(lv_ches_2905, glob_pos, 1705),
                                              party_mad(lv_ches_2906, glob_pos, 1705),
                                              party_mad(lv_ches_2914, glob_pos, 1705),
                                              party_mad(lv_ches_3101, glob_pos, 1191),
                                              party_mad(lv_ches_3102, glob_pos, 1191),
                                              party_mad(lv_ches_3104, glob_pos, 1191),
                                              party_mad(lv_ches_3105, glob_pos, 1191),
                                              party_mad(lv_ches_3107, glob_pos, 1191),
                                              party_mad(lv_ches_3109, glob_pos, 1191),
                                              party_mad(lv_ches_3112, glob_pos, 1191),
                                              party_mad(lv_ches_3114, glob_pos, 1191),
                                              party_mad(lv_ches_3701, glob_pos, 1470),
                                              party_mad(lv_ches_3702, glob_pos, 1470),
                                              party_mad(lv_ches_3801, glob_pos, 1442),
                                              party_mad(lv_ches_3802, glob_pos, 1442),
                                              party_mad(lv_ches_3803, glob_pos, 1442),
                                              party_mad(lv_ches_3804, glob_pos, 1442),
                                              party_mad(lv_ches_3805, glob_pos, 1442),
                                              party_mad(lv_ches_3806, glob_pos, 1442),
                                              party_mad(lv_ches_4001, glob_pos, 1196),
                                              party_mad(lv_ches_4003, glob_pos, 1196),
                                              party_mad(lv_ches_4004, glob_pos, 1196),
                                              party_mad(lv_ches_4005, glob_pos, 1196),
                                              party_mad(lv_ches_4006, glob_pos, 1196),
                                              party_mad(lv_ches_1301, glob_pos, 1040),
                                              party_mad(lv_ches_1308, glob_pos, 1040),
                                              party_mad(lv_ches_117, glob_pos, 1056),
                                              party_mad(lv_ches_118, glob_pos, 1056),
                                              party_mad(lv_ches_115, glob_pos, 1056),
                                              party_mad(lv_ches_216, glob_pos, 1208),
                                              party_mad(lv_ches_507, glob_pos, 1724),
                                              party_mad(lv_ches_624, glob_pos, 1250),
                                              party_mad(lv_ches_828, glob_pos, 1380),
                                              party_mad(lv_ches_803, glob_pos, 1380),
                                              party_mad(lv_ches_1006, glob_pos, 1528),
                                              party_mad(lv_ches_1109, glob_pos, 1826))


detach("package:DescTools", unload=TRUE)

party_vot_var_09$mad_glob_lv_countrymean <- ifelse(is.nan(party_vot_var_09$mad_glob_lv_countrymean) == TRUE, NA, party_vot_var_09$mad_glob_lv_countrymean)

summary(lm(party_vot_var_09$sd_glob_lv~party_vot_var_09$mad_glob_lv_countrymean), na.action(na.omit))








#############Immigration dimension: SD likely voters#########
attach(EES_2009)


#do SD on glob pos for likley voters of a party. 
party_vot_var_09$sd_immi_lv <- c(sd(q67_rec[lv_ches_102 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_103 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_104 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_105 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_106 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_107 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_108 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_109 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_110 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_112 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_119 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_201 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_202 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_203 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_206 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_211 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_215 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_218 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_301 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_302 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_303 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_304 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_306 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_310 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_311 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_401 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_402 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_403 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_404 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_410 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_413 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_414 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_415 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_501 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_502 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_504 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_505 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_506 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_511 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_513 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_517 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_523 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_525 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_601 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_602 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_605 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_609 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_610 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_613 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_701 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_702 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_703 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_705 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_707 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_708 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_811 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_814 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_815 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_827 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_837 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_838 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_844 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_845 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_848 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1001 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1002 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1003 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1004 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1005 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1014 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1016 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1017 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1018 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1101 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1102 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1104 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1105 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1106 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1107 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1108 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1201 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1202 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1205 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1206 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1208 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1209 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1302 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1303 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1304 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1307 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1309 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1401 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1402 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1403 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1404 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1405 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1406 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1408 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1409 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1601 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1602 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1603 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1604 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1605 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1606 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1607 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1610 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1611 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1612 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2002 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2004 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2007 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2010 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2015 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2016 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2101 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2103 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2104 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2109 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2111 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2113 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2201 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2202 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2203 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2204 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2207 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2301 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2302 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2308 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2309 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2310 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2311 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2402 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2405 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2406 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2410 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2412 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2501 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2506 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2507 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2511 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2515 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2516 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2518 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2601 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2603 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2605 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2606 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2613 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2614 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2616 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2701 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2702 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2704 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2705 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2706 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2710 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2711 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2802 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2803 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2804 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2805 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2809 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2812 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2813 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2814 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2815 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2902 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2903 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2904 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2905 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2906 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_2914 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3101 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3102 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3104 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3105 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3107 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3109 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3112 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3114 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3701 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3702 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3801 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3802 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3803 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3804 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3805 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_3806 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_4001 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_4003 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_4004 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_4005 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_4006 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1301 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1308 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_117 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_118 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_115 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_216 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_507 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_624 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_828 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_803 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1006 == 1], na.rm = TRUE),
                                 sd(q67_rec[lv_ches_1109 == 1], na.rm = TRUE))



#############Immigration dimension: MAD############
#for using MAD
library(DescTools) #using this function to get MAD https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/MeanAD
MeanAD(q67_rec, na.rm = TRUE)
MeanAD(q67_rec[lv_ches_1017 == 1], na.rm = TRUE)  #SD for the PVV

#MAD of voters on glob dimension. 
party_vot_var_09$mad_immi_lv <- c(MeanAD(q67_rec[lv_ches_102 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_103 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_104 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_105 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_106 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_107 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_108 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_109 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_110 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_112 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_119 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_201 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_202 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_203 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_206 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_211 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_215 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_218 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_301 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_302 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_303 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_304 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_306 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_310 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_311 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_401 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_402 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_403 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_404 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_410 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_413 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_414 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_415 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_501 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_502 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_504 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_505 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_506 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_511 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_513 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_517 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_523 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_525 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_601 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_602 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_605 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_609 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_610 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_613 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_701 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_702 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_703 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_705 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_707 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_708 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_811 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_814 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_815 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_827 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_837 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_838 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_844 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_845 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_848 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1001 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1002 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1003 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1004 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1005 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1014 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1016 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1017 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1018 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1101 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1102 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1104 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1105 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1106 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1107 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1108 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1201 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1202 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1205 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1206 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1208 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1209 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1302 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1303 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1304 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1307 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1309 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1401 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1402 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1403 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1404 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1405 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1406 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1408 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1409 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1601 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1602 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1603 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1604 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1605 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1606 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1607 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1610 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1611 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1612 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2002 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2004 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2007 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2010 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2015 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2016 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2101 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2103 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2104 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2109 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2111 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2113 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2201 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2202 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2203 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2204 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2207 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2301 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2302 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2308 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2309 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2310 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2311 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2402 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2405 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2406 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2410 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2412 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2501 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2506 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2507 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2511 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2515 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2516 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2518 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2601 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2603 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2605 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2606 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2613 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2614 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2616 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2701 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2702 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2704 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2705 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2706 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2710 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2711 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2802 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2803 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2804 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2805 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2809 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2812 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2813 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2814 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2815 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2902 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2903 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2904 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2905 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2906 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_2914 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3101 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3102 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3104 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3105 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3107 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3109 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3112 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3114 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3701 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3702 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3801 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3802 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3803 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3804 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3805 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_3806 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_4001 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_4003 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_4004 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_4005 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_4006 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1301 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1308 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_117 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_118 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_115 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_216 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_507 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_624 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_828 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_803 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1006 == 1], na.rm = TRUE),
                                  MeanAD(q67_rec[lv_ches_1109 == 1], na.rm = TRUE))

detach("package:DescTools", unload=TRUE)

party_vot_var_09$mad_immi_lv <- ifelse(is.nan(party_vot_var_09$mad_immi_lv) == TRUE, NA, party_vot_var_09$mad_immi_lv)




#############Immigration dimension: mean positions for likely voters#############
party_vot_var_09$mean_immi_lv <- c(mean(q67_rec[lv_ches_102 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_103 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_104 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_105 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_106 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_107 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_108 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_109 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_110 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_112 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_119 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_201 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_202 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_203 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_206 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_211 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_215 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_218 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_301 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_302 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_303 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_304 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_306 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_310 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_311 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_401 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_402 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_403 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_404 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_410 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_413 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_414 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_415 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_501 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_502 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_504 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_505 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_506 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_511 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_513 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_517 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_523 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_525 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_601 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_602 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_605 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_609 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_610 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_613 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_701 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_702 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_703 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_705 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_707 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_708 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_811 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_814 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_815 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_827 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_837 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_838 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_844 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_845 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_848 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1001 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1002 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1003 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1004 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1005 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1014 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1016 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1017 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1018 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1101 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1102 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1104 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1105 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1106 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1107 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1108 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1201 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1202 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1205 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1206 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1208 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1209 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1302 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1303 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1304 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1307 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1309 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1401 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1402 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1403 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1404 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1405 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1406 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1408 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1409 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1601 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1602 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1603 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1604 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1605 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1606 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1607 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1610 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1611 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1612 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2002 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2004 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2007 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2010 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2015 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2016 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2101 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2103 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2104 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2109 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2111 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2113 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2201 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2202 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2203 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2204 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2207 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2301 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2302 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2308 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2309 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2310 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2311 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2402 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2405 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2406 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2410 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2412 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2501 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2506 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2507 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2511 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2515 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2516 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2518 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2601 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2603 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2605 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2606 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2613 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2614 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2616 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2701 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2702 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2704 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2705 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2706 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2710 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2711 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2802 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2803 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2804 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2805 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2809 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2812 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2813 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2814 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2815 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2902 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2903 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2904 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2905 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2906 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_2914 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3101 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3102 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3104 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3105 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3107 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3109 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3112 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3114 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3701 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3702 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3801 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3802 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3803 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3804 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3805 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_3806 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_4001 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_4003 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_4004 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_4005 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_4006 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1301 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1308 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_117 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_118 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_115 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_216 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_507 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_624 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_828 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_803 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1006 == 1], na.rm = TRUE),
                                   mean(q67_rec[lv_ches_1109 == 1], na.rm = TRUE))

party_vot_var_09$mean_immi_lv <- ifelse(is.nan(party_vot_var_09$mean_immi_lv) == TRUE, NA, party_vot_var_09$mean_immi_lv)





#############Immigration dimension: percentage extreme voters##############################
#Function for percentage extreme. 
#take the sum of all not NA values for a question. This is the total n for that question. Then take all extreme values
#for that same question. Do this three times for both and calculate percentage extreme scores.
#note that not all people answering all three questions is taken into account by taking separate Ns for each question
#the lvches is the name for columns with dummies for likely voters. 
attach(EES_2009)

per_ext_voters <- function(lvches)
{
  n1 = sum(!is.na((as.numeric(q67_rec[lvches == 1]))))
  t1 = sum(!is.na((as.numeric(q67_rec[lvches == 1 & (q67_rec == 11 | q67_rec == 1) ]))))
  pext <- (t1/n1) * 100
  return(pext)
}


####################Adding the code for all parties. Note that, just like above, all NAs are for Eastern European countries
party_vot_var_09$perext_immi_lv <- c(per_ext_voters(lv_ches_102),
                                     per_ext_voters(lv_ches_103),
                                     per_ext_voters(lv_ches_104),
                                     per_ext_voters(lv_ches_105),
                                     per_ext_voters(lv_ches_106),
                                     per_ext_voters(lv_ches_107),
                                     per_ext_voters(lv_ches_108),
                                     per_ext_voters(lv_ches_109),
                                     per_ext_voters(lv_ches_110),
                                     per_ext_voters(lv_ches_112),
                                     per_ext_voters(lv_ches_119),
                                     per_ext_voters(lv_ches_201),
                                     per_ext_voters(lv_ches_202),
                                     per_ext_voters(lv_ches_203),
                                     per_ext_voters(lv_ches_206),
                                     per_ext_voters(lv_ches_211),
                                     per_ext_voters(lv_ches_215),
                                     per_ext_voters(lv_ches_218),
                                     per_ext_voters(lv_ches_301),
                                     per_ext_voters(lv_ches_302),
                                     per_ext_voters(lv_ches_303),
                                     per_ext_voters(lv_ches_304),
                                     per_ext_voters(lv_ches_306),
                                     per_ext_voters(lv_ches_310),
                                     per_ext_voters(lv_ches_311),
                                     per_ext_voters(lv_ches_401),
                                     per_ext_voters(lv_ches_402),
                                     per_ext_voters(lv_ches_403),
                                     per_ext_voters(lv_ches_404),
                                     per_ext_voters(lv_ches_410),
                                     per_ext_voters(lv_ches_413),
                                     per_ext_voters(lv_ches_414),
                                     per_ext_voters(lv_ches_415),
                                     per_ext_voters(lv_ches_501),
                                     per_ext_voters(lv_ches_502),
                                     per_ext_voters(lv_ches_504),
                                     per_ext_voters(lv_ches_505),
                                     per_ext_voters(lv_ches_506),
                                     per_ext_voters(lv_ches_511),
                                     per_ext_voters(lv_ches_513),
                                     per_ext_voters(lv_ches_517),
                                     per_ext_voters(lv_ches_523),
                                     per_ext_voters(lv_ches_525),
                                     per_ext_voters(lv_ches_601),
                                     per_ext_voters(lv_ches_602),
                                     per_ext_voters(lv_ches_605),
                                     per_ext_voters(lv_ches_609),
                                     per_ext_voters(lv_ches_610),
                                     per_ext_voters(lv_ches_613),
                                     per_ext_voters(lv_ches_701),
                                     per_ext_voters(lv_ches_702),
                                     per_ext_voters(lv_ches_703),
                                     per_ext_voters(lv_ches_705),
                                     per_ext_voters(lv_ches_707),
                                     per_ext_voters(lv_ches_708),
                                     per_ext_voters(lv_ches_811),
                                     per_ext_voters(lv_ches_814),
                                     per_ext_voters(lv_ches_815),
                                     per_ext_voters(lv_ches_827),
                                     per_ext_voters(lv_ches_837),
                                     per_ext_voters(lv_ches_838),
                                     per_ext_voters(lv_ches_844),
                                     per_ext_voters(lv_ches_845),
                                     per_ext_voters(lv_ches_848),
                                     per_ext_voters(lv_ches_1001),
                                     per_ext_voters(lv_ches_1002),
                                     per_ext_voters(lv_ches_1003),
                                     per_ext_voters(lv_ches_1004),
                                     per_ext_voters(lv_ches_1005),
                                     per_ext_voters(lv_ches_1014),
                                     per_ext_voters(lv_ches_1016),
                                     per_ext_voters(lv_ches_1017),
                                     per_ext_voters(lv_ches_1018),
                                     per_ext_voters(lv_ches_1101),
                                     per_ext_voters(lv_ches_1102),
                                     per_ext_voters(lv_ches_1104),
                                     per_ext_voters(lv_ches_1105),
                                     per_ext_voters(lv_ches_1106),
                                     per_ext_voters(lv_ches_1107),
                                     per_ext_voters(lv_ches_1108),
                                     per_ext_voters(lv_ches_1201),
                                     per_ext_voters(lv_ches_1202),
                                     per_ext_voters(lv_ches_1205),
                                     per_ext_voters(lv_ches_1206),
                                     per_ext_voters(lv_ches_1208),
                                     per_ext_voters(lv_ches_1209),
                                     per_ext_voters(lv_ches_1302),
                                     per_ext_voters(lv_ches_1303),
                                     per_ext_voters(lv_ches_1304),
                                     per_ext_voters(lv_ches_1307),
                                     per_ext_voters(lv_ches_1309),
                                     per_ext_voters(lv_ches_1401),
                                     per_ext_voters(lv_ches_1402),
                                     per_ext_voters(lv_ches_1403),
                                     per_ext_voters(lv_ches_1404),
                                     per_ext_voters(lv_ches_1405),
                                     per_ext_voters(lv_ches_1406),
                                     per_ext_voters(lv_ches_1408),
                                     per_ext_voters(lv_ches_1409),
                                     per_ext_voters(lv_ches_1601),
                                     per_ext_voters(lv_ches_1602),
                                     per_ext_voters(lv_ches_1603),
                                     per_ext_voters(lv_ches_1604),
                                     per_ext_voters(lv_ches_1605),
                                     per_ext_voters(lv_ches_1606),
                                     per_ext_voters(lv_ches_1607),
                                     per_ext_voters(lv_ches_1610),
                                     per_ext_voters(lv_ches_1611),
                                     per_ext_voters(lv_ches_1612),
                                     per_ext_voters(lv_ches_2002),
                                     per_ext_voters(lv_ches_2004),
                                     per_ext_voters(lv_ches_2007),
                                     per_ext_voters(lv_ches_2010),
                                     per_ext_voters(lv_ches_2015),
                                     per_ext_voters(lv_ches_2016),
                                     per_ext_voters(lv_ches_2101),
                                     per_ext_voters(lv_ches_2103),
                                     per_ext_voters(lv_ches_2104),
                                     per_ext_voters(lv_ches_2109),
                                     per_ext_voters(lv_ches_2111),
                                     per_ext_voters(lv_ches_2113),
                                     per_ext_voters(lv_ches_2201),
                                     per_ext_voters(lv_ches_2202),
                                     per_ext_voters(lv_ches_2203),
                                     per_ext_voters(lv_ches_2204),
                                     per_ext_voters(lv_ches_2207),
                                     per_ext_voters(lv_ches_2301),
                                     per_ext_voters(lv_ches_2302),
                                     per_ext_voters(lv_ches_2308),
                                     per_ext_voters(lv_ches_2309),
                                     per_ext_voters(lv_ches_2310),
                                     per_ext_voters(lv_ches_2311),
                                     per_ext_voters(lv_ches_2402),
                                     per_ext_voters(lv_ches_2405),
                                     per_ext_voters(lv_ches_2406),
                                     per_ext_voters(lv_ches_2410),
                                     per_ext_voters(lv_ches_2412),
                                     per_ext_voters(lv_ches_2501),
                                     per_ext_voters(lv_ches_2506),
                                     per_ext_voters(lv_ches_2507),
                                     per_ext_voters(lv_ches_2511),
                                     per_ext_voters(lv_ches_2515),
                                     per_ext_voters(lv_ches_2516),
                                     per_ext_voters(lv_ches_2518),
                                     per_ext_voters(lv_ches_2601),
                                     per_ext_voters(lv_ches_2603),
                                     per_ext_voters(lv_ches_2605),
                                     per_ext_voters(lv_ches_2606),
                                     per_ext_voters(lv_ches_2613),
                                     per_ext_voters(lv_ches_2614),
                                     per_ext_voters(lv_ches_2616),
                                     per_ext_voters(lv_ches_2701),
                                     per_ext_voters(lv_ches_2702),
                                     per_ext_voters(lv_ches_2704),
                                     per_ext_voters(lv_ches_2705),
                                     per_ext_voters(lv_ches_2706),
                                     per_ext_voters(lv_ches_2710),
                                     per_ext_voters(lv_ches_2711),
                                     per_ext_voters(lv_ches_2802),
                                     per_ext_voters(lv_ches_2803),
                                     per_ext_voters(lv_ches_2804),
                                     per_ext_voters(lv_ches_2805),
                                     per_ext_voters(lv_ches_2809),
                                     per_ext_voters(lv_ches_2812),
                                     per_ext_voters(lv_ches_2813),
                                     per_ext_voters(lv_ches_2814),
                                     per_ext_voters(lv_ches_2815),
                                     per_ext_voters(lv_ches_2902),
                                     per_ext_voters(lv_ches_2903),
                                     per_ext_voters(lv_ches_2904),
                                     per_ext_voters(lv_ches_2905),
                                     per_ext_voters(lv_ches_2906),
                                     per_ext_voters(lv_ches_2914),
                                     per_ext_voters(lv_ches_3101),
                                     per_ext_voters(lv_ches_3102),
                                     per_ext_voters(lv_ches_3104),
                                     per_ext_voters(lv_ches_3105),
                                     per_ext_voters(lv_ches_3107),
                                     per_ext_voters(lv_ches_3109),
                                     per_ext_voters(lv_ches_3112),
                                     per_ext_voters(lv_ches_3114),
                                     per_ext_voters(lv_ches_3701),
                                     per_ext_voters(lv_ches_3702),
                                     per_ext_voters(lv_ches_3801),
                                     per_ext_voters(lv_ches_3802),
                                     per_ext_voters(lv_ches_3803),
                                     per_ext_voters(lv_ches_3804),
                                     per_ext_voters(lv_ches_3805),
                                     per_ext_voters(lv_ches_3806),
                                     per_ext_voters(lv_ches_4001),
                                     per_ext_voters(lv_ches_4003),
                                     per_ext_voters(lv_ches_4004),
                                     per_ext_voters(lv_ches_4005),
                                     per_ext_voters(lv_ches_4006),
                                     per_ext_voters(lv_ches_1301),
                                     per_ext_voters(lv_ches_1308),
                                     per_ext_voters(lv_ches_117),
                                     per_ext_voters(lv_ches_118),
                                     per_ext_voters(lv_ches_115),
                                     per_ext_voters(lv_ches_216),
                                     per_ext_voters(lv_ches_507),
                                     per_ext_voters(lv_ches_624),
                                     per_ext_voters(lv_ches_828),
                                     per_ext_voters(lv_ches_803),
                                     per_ext_voters(lv_ches_1006),
                                     per_ext_voters(lv_ches_1109))



party_vot_var_09$perext_immi_lv <- ifelse(is.nan(party_vot_var_09$perext_immi_lv) == TRUE, NA, party_vot_var_09$perext_immi_lv)



#############Immigration dimension: Average distance from country mean##########################################
##Can function as an add-on to percentage of extreme scores. 

library(DescTools) #using this function to get MAD https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/MeanAD

party_mad <- function(party, var, country) #MAD for likely voters of a party, on a variable, in a country
{
  country_mean <-  function(var, country) #country_mean calculates the mean on a variable in a country
  {
    x <- mean(var[t102 == country], na.rm=TRUE)
    return(x)
  }
  y <- MeanAD(var[party == 1], FUN=country_mean(var, country), na.rm=TRUE) #MeanAD calculates the mean distance on a variable for likely voters in a country where the midpoint is the return from above, i.e. country mean. 
  return(y) 
}

#check if function works. It does. 
#party_mad(lv_ches_1017, q67_rec, 1528) #PVV far away from country mean
#MeanAD(q67_rec[lv_ches_1017== 1], FUN=mean(q67_rec[t102==1528], na.rm = TRUE), na.rm = TRUE)
#party_mad(lv_ches_1003, q67_rec, 1056) #VVD closer to country mean


party_vot_var_09$mad_immi_lv_countrymean <- c(party_mad(lv_ches_102, q67_rec, 1056),
                                              party_mad(lv_ches_103, q67_rec, 1056),
                                              party_mad(lv_ches_104, q67_rec, 1056),
                                              party_mad(lv_ches_105, q67_rec, 1056),
                                              party_mad(lv_ches_106, q67_rec, 1056),
                                              party_mad(lv_ches_107, q67_rec, 1056),
                                              party_mad(lv_ches_108, q67_rec, 1056),
                                              party_mad(lv_ches_109, q67_rec, 1056),
                                              party_mad(lv_ches_110, q67_rec, 1056),
                                              party_mad(lv_ches_112, q67_rec, 1056),
                                              party_mad(lv_ches_119, q67_rec, 1056),
                                              party_mad(lv_ches_201, q67_rec, 1208),
                                              party_mad(lv_ches_202, q67_rec, 1208),
                                              party_mad(lv_ches_203, q67_rec, 1208),
                                              party_mad(lv_ches_206, q67_rec, 1208),
                                              party_mad(lv_ches_211, q67_rec, 1208),
                                              party_mad(lv_ches_215, q67_rec, 1208),
                                              party_mad(lv_ches_218, q67_rec, 1208),
                                              party_mad(lv_ches_301, q67_rec, 1276),
                                              party_mad(lv_ches_302, q67_rec, 1276),
                                              party_mad(lv_ches_303, q67_rec, 1276),
                                              party_mad(lv_ches_304, q67_rec, 1276),
                                              party_mad(lv_ches_306, q67_rec, 1276),
                                              party_mad(lv_ches_310, q67_rec, 1276),
                                              party_mad(lv_ches_311, q67_rec, 1276),
                                              party_mad(lv_ches_401, q67_rec, 1300),
                                              party_mad(lv_ches_402, q67_rec, 1300),
                                              party_mad(lv_ches_403, q67_rec, 1300),
                                              party_mad(lv_ches_404, q67_rec, 1300),
                                              party_mad(lv_ches_410, q67_rec, 1300),
                                              party_mad(lv_ches_413, q67_rec, 1300),
                                              party_mad(lv_ches_414, q67_rec, 1300),
                                              party_mad(lv_ches_415, q67_rec, 1300),
                                              party_mad(lv_ches_501, q67_rec, 1724),
                                              party_mad(lv_ches_502, q67_rec, 1724),
                                              party_mad(lv_ches_504, q67_rec, 1724),
                                              party_mad(lv_ches_505, q67_rec, 1724),
                                              party_mad(lv_ches_506, q67_rec, 1724),
                                              party_mad(lv_ches_511, q67_rec, 1724),
                                              party_mad(lv_ches_513, q67_rec, 1724),
                                              party_mad(lv_ches_517, q67_rec, 1724),
                                              party_mad(lv_ches_523, q67_rec, 1724),
                                              party_mad(lv_ches_525, q67_rec, 1724),
                                              party_mad(lv_ches_601, q67_rec, 1250),
                                              party_mad(lv_ches_602, q67_rec, 1250),
                                              party_mad(lv_ches_605, q67_rec, 1250),
                                              party_mad(lv_ches_609, q67_rec, 1250),
                                              party_mad(lv_ches_610, q67_rec, 1250),
                                              party_mad(lv_ches_613, q67_rec, 1250),
                                              party_mad(lv_ches_701, q67_rec, 1372),
                                              party_mad(lv_ches_702, q67_rec, 1372),
                                              party_mad(lv_ches_703, q67_rec, 1372),
                                              party_mad(lv_ches_705, q67_rec, 1372),
                                              party_mad(lv_ches_707, q67_rec, 1372),
                                              party_mad(lv_ches_708, q67_rec, 1372),
                                              party_mad(lv_ches_811, q67_rec, 1380),
                                              party_mad(lv_ches_814, q67_rec, 1380),
                                              party_mad(lv_ches_815, q67_rec, 1380),
                                              party_mad(lv_ches_827, q67_rec, 1380),
                                              party_mad(lv_ches_837, q67_rec, 1380),
                                              party_mad(lv_ches_838, q67_rec, 1380),
                                              party_mad(lv_ches_844, q67_rec, 1380),
                                              party_mad(lv_ches_845, q67_rec, 1380),
                                              party_mad(lv_ches_848, q67_rec, 1380),
                                              party_mad(lv_ches_1001, q67_rec, 1528),
                                              party_mad(lv_ches_1002, q67_rec, 1528),
                                              party_mad(lv_ches_1003, q67_rec, 1528),
                                              party_mad(lv_ches_1004, q67_rec, 1528),
                                              party_mad(lv_ches_1005, q67_rec, 1528),
                                              party_mad(lv_ches_1014, q67_rec, 1528),
                                              party_mad(lv_ches_1016, q67_rec, 1528),
                                              party_mad(lv_ches_1017, q67_rec, 1528),
                                              party_mad(lv_ches_1018, q67_rec, 1528),
                                              party_mad(lv_ches_1101, q67_rec, 1826),
                                              party_mad(lv_ches_1102, q67_rec, 1826),
                                              party_mad(lv_ches_1104, q67_rec, 1826),
                                              party_mad(lv_ches_1105, q67_rec, 1826),
                                              party_mad(lv_ches_1106, q67_rec, 1826),
                                              party_mad(lv_ches_1107, q67_rec, 1826),
                                              party_mad(lv_ches_1108, q67_rec, 1826),
                                              party_mad(lv_ches_1201, q67_rec, 1620),
                                              party_mad(lv_ches_1202, q67_rec, 1620),
                                              party_mad(lv_ches_1205, q67_rec, 1620),
                                              party_mad(lv_ches_1206, q67_rec, 1620),
                                              party_mad(lv_ches_1208, q67_rec, 1620),
                                              party_mad(lv_ches_1209, q67_rec, 1620),
                                              party_mad(lv_ches_1302, q67_rec, 1040),
                                              party_mad(lv_ches_1303, q67_rec, 1040),
                                              party_mad(lv_ches_1304, q67_rec, 1040),
                                              party_mad(lv_ches_1307, q67_rec, 1040),
                                              party_mad(lv_ches_1309, q67_rec, 1040),
                                              party_mad(lv_ches_1401, q67_rec, 1246),
                                              party_mad(lv_ches_1402, q67_rec, 1246),
                                              party_mad(lv_ches_1403, q67_rec, 1246),
                                              party_mad(lv_ches_1404, q67_rec, 1246),
                                              party_mad(lv_ches_1405, q67_rec, 1246),
                                              party_mad(lv_ches_1406, q67_rec, 1246),
                                              party_mad(lv_ches_1408, q67_rec, 1246),
                                              party_mad(lv_ches_1409, q67_rec, 1246),
                                              party_mad(lv_ches_1601, q67_rec, 1752),
                                              party_mad(lv_ches_1602, q67_rec, 1752),
                                              party_mad(lv_ches_1603, q67_rec, 1752),
                                              party_mad(lv_ches_1604, q67_rec, 1752),
                                              party_mad(lv_ches_1605, q67_rec, 1752),
                                              party_mad(lv_ches_1606, q67_rec, 1752),
                                              party_mad(lv_ches_1607, q67_rec, 1752),
                                              party_mad(lv_ches_1610, q67_rec, 1752),
                                              party_mad(lv_ches_1611, q67_rec, 1752),
                                              party_mad(lv_ches_1612, q67_rec, 1752),
                                              party_mad(lv_ches_2002, q67_rec, 1100),
                                              party_mad(lv_ches_2004, q67_rec, 1100),
                                              party_mad(lv_ches_2007, q67_rec, 1100),
                                              party_mad(lv_ches_2010, q67_rec, 1100),
                                              party_mad(lv_ches_2015, q67_rec, 1100),
                                              party_mad(lv_ches_2016, q67_rec, 1100),
                                              party_mad(lv_ches_2101, q67_rec, 1203),
                                              party_mad(lv_ches_2103, q67_rec, 1203),
                                              party_mad(lv_ches_2104, q67_rec, 1203),
                                              party_mad(lv_ches_2109, q67_rec, 1203),
                                              party_mad(lv_ches_2111, q67_rec, 1203),
                                              party_mad(lv_ches_2113, q67_rec, 1203),
                                              party_mad(lv_ches_2201, q67_rec, 1233),
                                              party_mad(lv_ches_2202, q67_rec, 1233),
                                              party_mad(lv_ches_2203, q67_rec, 1233),
                                              party_mad(lv_ches_2204, q67_rec, 1233),
                                              party_mad(lv_ches_2207, q67_rec, 1233),
                                              party_mad(lv_ches_2301, q67_rec, 1348),
                                              party_mad(lv_ches_2302, q67_rec, 1348),
                                              party_mad(lv_ches_2308, q67_rec, 1348),
                                              party_mad(lv_ches_2309, q67_rec, 1348),
                                              party_mad(lv_ches_2310, q67_rec, 1348),
                                              party_mad(lv_ches_2311, q67_rec, 1348),
                                              party_mad(lv_ches_2402, q67_rec, 1428),
                                              party_mad(lv_ches_2405, q67_rec, 1428),
                                              party_mad(lv_ches_2406, q67_rec, 1428),
                                              party_mad(lv_ches_2410, q67_rec, 1428),
                                              party_mad(lv_ches_2412, q67_rec, 1428),
                                              party_mad(lv_ches_2501, q67_rec, 1440),
                                              party_mad(lv_ches_2506, q67_rec, 1440),
                                              party_mad(lv_ches_2507, q67_rec, 1440),
                                              party_mad(lv_ches_2511, q67_rec, 1440),
                                              party_mad(lv_ches_2515, q67_rec, 1440),
                                              party_mad(lv_ches_2516, q67_rec, 1440),
                                              party_mad(lv_ches_2518, q67_rec, 1440),
                                              party_mad(lv_ches_2601, q67_rec, 1616),
                                              party_mad(lv_ches_2603, q67_rec, 1616),
                                              party_mad(lv_ches_2605, q67_rec, 1616),
                                              party_mad(lv_ches_2606, q67_rec, 1616),
                                              party_mad(lv_ches_2613, q67_rec, 1616),
                                              party_mad(lv_ches_2614, q67_rec, 1616),
                                              party_mad(lv_ches_2616, q67_rec, 1616),
                                              party_mad(lv_ches_2701, q67_rec, 1642),
                                              party_mad(lv_ches_2702, q67_rec, 1642),
                                              party_mad(lv_ches_2704, q67_rec, 1642),
                                              party_mad(lv_ches_2705, q67_rec, 1642),
                                              party_mad(lv_ches_2706, q67_rec, 1642),
                                              party_mad(lv_ches_2710, q67_rec, 1642),
                                              party_mad(lv_ches_2711, q67_rec, 1642),
                                              party_mad(lv_ches_2802, q67_rec, 1703),
                                              party_mad(lv_ches_2803, q67_rec, 1703),
                                              party_mad(lv_ches_2804, q67_rec, 1703),
                                              party_mad(lv_ches_2805, q67_rec, 1703),
                                              party_mad(lv_ches_2809, q67_rec, 1703),
                                              party_mad(lv_ches_2812, q67_rec, 1703),
                                              party_mad(lv_ches_2813, q67_rec, 1703),
                                              party_mad(lv_ches_2814, q67_rec, 1703),
                                              party_mad(lv_ches_2815, q67_rec, 1703),
                                              party_mad(lv_ches_2902, q67_rec, 1705),
                                              party_mad(lv_ches_2903, q67_rec, 1705),
                                              party_mad(lv_ches_2904, q67_rec, 1705),
                                              party_mad(lv_ches_2905, q67_rec, 1705),
                                              party_mad(lv_ches_2906, q67_rec, 1705),
                                              party_mad(lv_ches_2914, q67_rec, 1705),
                                              party_mad(lv_ches_3101, q67_rec, 1191),
                                              party_mad(lv_ches_3102, q67_rec, 1191),
                                              party_mad(lv_ches_3104, q67_rec, 1191),
                                              party_mad(lv_ches_3105, q67_rec, 1191),
                                              party_mad(lv_ches_3107, q67_rec, 1191),
                                              party_mad(lv_ches_3109, q67_rec, 1191),
                                              party_mad(lv_ches_3112, q67_rec, 1191),
                                              party_mad(lv_ches_3114, q67_rec, 1191),
                                              party_mad(lv_ches_3701, q67_rec, 1470),
                                              party_mad(lv_ches_3702, q67_rec, 1470),
                                              party_mad(lv_ches_3801, q67_rec, 1442),
                                              party_mad(lv_ches_3802, q67_rec, 1442),
                                              party_mad(lv_ches_3803, q67_rec, 1442),
                                              party_mad(lv_ches_3804, q67_rec, 1442),
                                              party_mad(lv_ches_3805, q67_rec, 1442),
                                              party_mad(lv_ches_3806, q67_rec, 1442),
                                              party_mad(lv_ches_4001, q67_rec, 1196),
                                              party_mad(lv_ches_4003, q67_rec, 1196),
                                              party_mad(lv_ches_4004, q67_rec, 1196),
                                              party_mad(lv_ches_4005, q67_rec, 1196),
                                              party_mad(lv_ches_4006, q67_rec, 1196),
                                              party_mad(lv_ches_1301, q67_rec, 1040),
                                              party_mad(lv_ches_1308, q67_rec, 1040),
                                              party_mad(lv_ches_117, q67_rec, 1056),
                                              party_mad(lv_ches_118, q67_rec, 1056),
                                              party_mad(lv_ches_115, q67_rec, 1056),
                                              party_mad(lv_ches_216, q67_rec, 1208),
                                              party_mad(lv_ches_507, q67_rec, 1724),
                                              party_mad(lv_ches_624, q67_rec, 1250),
                                              party_mad(lv_ches_828, q67_rec, 1380),
                                              party_mad(lv_ches_803, q67_rec, 1380),
                                              party_mad(lv_ches_1006, q67_rec, 1528),
                                              party_mad(lv_ches_1109, q67_rec, 1826))


detach("package:DescTools", unload=TRUE)

party_vot_var_09$mad_immi_lv_countrymean <- ifelse(is.nan(party_vot_var_09$mad_immi_lv_countrymean) == TRUE, NA, party_vot_var_09$mad_immi_lv_countrymean)









#############EU dimension: SD likely voters#########
attach(EES_2009)

#do SD on glob pos for likley voters of a party. 
party_vot_var_09$sd_eu_lv <- c(sd(q80[lv_ches_102 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_103 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_104 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_105 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_106 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_107 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_108 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_109 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_110 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_112 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_119 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_201 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_202 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_203 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_206 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_211 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_215 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_218 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_301 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_302 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_303 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_304 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_306 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_310 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_311 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_401 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_402 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_403 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_404 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_410 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_413 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_414 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_415 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_501 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_502 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_504 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_505 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_506 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_511 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_513 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_517 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_523 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_525 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_601 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_602 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_605 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_609 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_610 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_613 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_701 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_702 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_703 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_705 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_707 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_708 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_811 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_814 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_815 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_827 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_837 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_838 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_844 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_845 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_848 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1001 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1002 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1003 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1004 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1005 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1014 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1016 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1017 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1018 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1101 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1102 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1104 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1105 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1106 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1107 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1108 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1201 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1202 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1205 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1206 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1208 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1209 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1302 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1303 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1304 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1307 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1309 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1401 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1402 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1403 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1404 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1405 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1406 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1408 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1409 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1601 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1602 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1603 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1604 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1605 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1606 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1607 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1610 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1611 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1612 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2002 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2004 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2007 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2010 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2015 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2016 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2101 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2103 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2104 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2109 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2111 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2113 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2201 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2202 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2203 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2204 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2207 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2301 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2302 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2308 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2309 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2310 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2311 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2402 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2405 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2406 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2410 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2412 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2501 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2506 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2507 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2511 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2515 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2516 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2518 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2601 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2603 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2605 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2606 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2613 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2614 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2616 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2701 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2702 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2704 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2705 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2706 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2710 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2711 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2802 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2803 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2804 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2805 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2809 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2812 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2813 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2814 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2815 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2902 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2903 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2904 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2905 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2906 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_2914 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3101 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3102 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3104 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3105 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3107 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3109 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3112 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3114 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3701 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3702 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3801 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3802 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3803 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3804 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3805 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_3806 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_4001 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_4003 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_4004 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_4005 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_4006 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1301 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1308 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_117 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_118 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_115 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_216 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_507 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_624 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_828 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_803 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1006 == 1], na.rm = TRUE),
                               sd(q80[lv_ches_1109 == 1], na.rm = TRUE))



#############EU dimension: MAD############
#for using MAD
library(DescTools) #using this function to get MAD https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/MeanAD
MeanAD(q80, na.rm = TRUE)
MeanAD(q80[lv_ches_1017 == 1], na.rm = TRUE)  #SD for the PVV

#MAD of voters on glob dimension. 
party_vot_var_09$mad_eu_lv <- c(MeanAD(q80[lv_ches_102 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_103 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_104 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_105 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_106 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_107 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_108 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_109 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_110 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_112 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_119 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_201 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_202 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_203 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_206 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_211 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_215 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_218 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_301 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_302 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_303 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_304 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_306 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_310 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_311 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_401 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_402 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_403 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_404 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_410 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_413 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_414 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_415 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_501 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_502 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_504 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_505 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_506 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_511 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_513 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_517 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_523 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_525 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_601 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_602 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_605 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_609 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_610 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_613 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_701 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_702 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_703 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_705 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_707 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_708 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_811 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_814 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_815 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_827 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_837 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_838 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_844 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_845 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_848 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1001 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1002 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1003 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1004 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1005 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1014 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1016 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1017 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1018 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1101 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1102 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1104 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1105 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1106 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1107 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1108 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1201 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1202 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1205 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1206 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1208 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1209 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1302 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1303 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1304 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1307 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1309 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1401 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1402 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1403 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1404 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1405 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1406 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1408 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1409 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1601 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1602 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1603 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1604 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1605 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1606 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1607 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1610 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1611 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1612 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2002 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2004 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2007 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2010 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2015 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2016 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2101 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2103 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2104 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2109 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2111 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2113 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2201 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2202 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2203 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2204 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2207 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2301 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2302 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2308 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2309 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2310 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2311 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2402 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2405 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2406 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2410 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2412 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2501 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2506 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2507 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2511 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2515 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2516 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2518 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2601 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2603 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2605 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2606 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2613 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2614 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2616 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2701 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2702 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2704 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2705 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2706 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2710 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2711 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2802 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2803 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2804 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2805 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2809 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2812 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2813 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2814 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2815 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2902 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2903 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2904 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2905 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2906 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_2914 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3101 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3102 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3104 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3105 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3107 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3109 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3112 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3114 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3701 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3702 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3801 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3802 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3803 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3804 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3805 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_3806 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_4001 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_4003 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_4004 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_4005 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_4006 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1301 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1308 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_117 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_118 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_115 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_216 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_507 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_624 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_828 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_803 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1006 == 1], na.rm = TRUE),
                                MeanAD(q80[lv_ches_1109 == 1], na.rm = TRUE))

detach("package:DescTools", unload=TRUE)

party_vot_var_09$mad_eu_lv <- ifelse(is.nan(party_vot_var_09$mad_eu_lv) == TRUE, NA, party_vot_var_09$mad_eu_lv)




#############EU dimension: mean positions for likely voters#############
party_vot_var_09$mean_eu_lv <- c(mean(q80[lv_ches_102 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_103 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_104 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_105 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_106 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_107 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_108 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_109 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_110 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_112 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_119 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_201 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_202 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_203 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_206 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_211 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_215 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_218 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_301 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_302 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_303 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_304 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_306 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_310 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_311 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_401 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_402 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_403 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_404 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_410 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_413 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_414 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_415 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_501 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_502 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_504 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_505 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_506 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_511 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_513 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_517 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_523 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_525 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_601 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_602 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_605 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_609 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_610 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_613 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_701 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_702 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_703 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_705 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_707 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_708 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_811 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_814 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_815 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_827 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_837 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_838 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_844 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_845 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_848 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1001 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1002 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1003 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1004 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1005 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1014 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1016 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1017 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1018 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1101 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1102 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1104 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1105 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1106 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1107 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1108 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1201 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1202 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1205 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1206 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1208 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1209 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1302 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1303 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1304 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1307 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1309 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1401 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1402 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1403 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1404 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1405 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1406 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1408 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1409 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1601 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1602 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1603 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1604 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1605 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1606 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1607 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1610 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1611 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1612 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2002 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2004 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2007 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2010 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2015 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2016 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2101 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2103 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2104 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2109 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2111 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2113 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2201 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2202 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2203 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2204 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2207 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2301 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2302 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2308 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2309 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2310 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2311 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2402 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2405 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2406 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2410 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2412 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2501 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2506 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2507 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2511 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2515 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2516 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2518 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2601 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2603 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2605 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2606 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2613 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2614 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2616 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2701 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2702 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2704 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2705 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2706 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2710 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2711 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2802 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2803 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2804 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2805 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2809 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2812 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2813 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2814 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2815 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2902 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2903 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2904 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2905 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2906 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_2914 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3101 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3102 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3104 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3105 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3107 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3109 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3112 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3114 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3701 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3702 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3801 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3802 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3803 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3804 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3805 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_3806 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_4001 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_4003 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_4004 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_4005 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_4006 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1301 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1308 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_117 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_118 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_115 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_216 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_507 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_624 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_828 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_803 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1006 == 1], na.rm = TRUE),
                                 mean(q80[lv_ches_1109 == 1], na.rm = TRUE))

party_vot_var_09$mean_eu_lv <- ifelse(is.nan(party_vot_var_09$mean_eu_lv) == TRUE, NA, party_vot_var_09$mean_eu_lv)





#############EU dimension: percentage extreme voters##############################
#Function for percentage extreme. 
#take the sum of all not NA values for a question. This is the total n for that question. Then take all extreme values
#for that same question. Do this three times for both and calculate percentage extreme scores.
#note that not all people answering all three questions is taken into account by taking separate Ns for each question
#the lvches is the name for columns with dummies for likely voters. 
attach(EES_2009)

per_ext_voters <- function(lvches)
{
  n1 = sum(!is.na((as.numeric(q80[lvches == 1]))))
  t1 = sum(!is.na((as.numeric(q80[lvches == 1 & (q80 == 11 | q80 == 1) ]))))
  pext <- (t1/n1) * 100
  return(pext)
}


####################Adding the code for all parties. Note that, just like above, all NAs are for Eastern European countries
party_vot_var_09$perext_eu_lv <- c(per_ext_voters(lv_ches_102),
                                   per_ext_voters(lv_ches_103),
                                   per_ext_voters(lv_ches_104),
                                   per_ext_voters(lv_ches_105),
                                   per_ext_voters(lv_ches_106),
                                   per_ext_voters(lv_ches_107),
                                   per_ext_voters(lv_ches_108),
                                   per_ext_voters(lv_ches_109),
                                   per_ext_voters(lv_ches_110),
                                   per_ext_voters(lv_ches_112),
                                   per_ext_voters(lv_ches_119),
                                   per_ext_voters(lv_ches_201),
                                   per_ext_voters(lv_ches_202),
                                   per_ext_voters(lv_ches_203),
                                   per_ext_voters(lv_ches_206),
                                   per_ext_voters(lv_ches_211),
                                   per_ext_voters(lv_ches_215),
                                   per_ext_voters(lv_ches_218),
                                   per_ext_voters(lv_ches_301),
                                   per_ext_voters(lv_ches_302),
                                   per_ext_voters(lv_ches_303),
                                   per_ext_voters(lv_ches_304),
                                   per_ext_voters(lv_ches_306),
                                   per_ext_voters(lv_ches_310),
                                   per_ext_voters(lv_ches_311),
                                   per_ext_voters(lv_ches_401),
                                   per_ext_voters(lv_ches_402),
                                   per_ext_voters(lv_ches_403),
                                   per_ext_voters(lv_ches_404),
                                   per_ext_voters(lv_ches_410),
                                   per_ext_voters(lv_ches_413),
                                   per_ext_voters(lv_ches_414),
                                   per_ext_voters(lv_ches_415),
                                   per_ext_voters(lv_ches_501),
                                   per_ext_voters(lv_ches_502),
                                   per_ext_voters(lv_ches_504),
                                   per_ext_voters(lv_ches_505),
                                   per_ext_voters(lv_ches_506),
                                   per_ext_voters(lv_ches_511),
                                   per_ext_voters(lv_ches_513),
                                   per_ext_voters(lv_ches_517),
                                   per_ext_voters(lv_ches_523),
                                   per_ext_voters(lv_ches_525),
                                   per_ext_voters(lv_ches_601),
                                   per_ext_voters(lv_ches_602),
                                   per_ext_voters(lv_ches_605),
                                   per_ext_voters(lv_ches_609),
                                   per_ext_voters(lv_ches_610),
                                   per_ext_voters(lv_ches_613),
                                   per_ext_voters(lv_ches_701),
                                   per_ext_voters(lv_ches_702),
                                   per_ext_voters(lv_ches_703),
                                   per_ext_voters(lv_ches_705),
                                   per_ext_voters(lv_ches_707),
                                   per_ext_voters(lv_ches_708),
                                   per_ext_voters(lv_ches_811),
                                   per_ext_voters(lv_ches_814),
                                   per_ext_voters(lv_ches_815),
                                   per_ext_voters(lv_ches_827),
                                   per_ext_voters(lv_ches_837),
                                   per_ext_voters(lv_ches_838),
                                   per_ext_voters(lv_ches_844),
                                   per_ext_voters(lv_ches_845),
                                   per_ext_voters(lv_ches_848),
                                   per_ext_voters(lv_ches_1001),
                                   per_ext_voters(lv_ches_1002),
                                   per_ext_voters(lv_ches_1003),
                                   per_ext_voters(lv_ches_1004),
                                   per_ext_voters(lv_ches_1005),
                                   per_ext_voters(lv_ches_1014),
                                   per_ext_voters(lv_ches_1016),
                                   per_ext_voters(lv_ches_1017),
                                   per_ext_voters(lv_ches_1018),
                                   per_ext_voters(lv_ches_1101),
                                   per_ext_voters(lv_ches_1102),
                                   per_ext_voters(lv_ches_1104),
                                   per_ext_voters(lv_ches_1105),
                                   per_ext_voters(lv_ches_1106),
                                   per_ext_voters(lv_ches_1107),
                                   per_ext_voters(lv_ches_1108),
                                   per_ext_voters(lv_ches_1201),
                                   per_ext_voters(lv_ches_1202),
                                   per_ext_voters(lv_ches_1205),
                                   per_ext_voters(lv_ches_1206),
                                   per_ext_voters(lv_ches_1208),
                                   per_ext_voters(lv_ches_1209),
                                   per_ext_voters(lv_ches_1302),
                                   per_ext_voters(lv_ches_1303),
                                   per_ext_voters(lv_ches_1304),
                                   per_ext_voters(lv_ches_1307),
                                   per_ext_voters(lv_ches_1309),
                                   per_ext_voters(lv_ches_1401),
                                   per_ext_voters(lv_ches_1402),
                                   per_ext_voters(lv_ches_1403),
                                   per_ext_voters(lv_ches_1404),
                                   per_ext_voters(lv_ches_1405),
                                   per_ext_voters(lv_ches_1406),
                                   per_ext_voters(lv_ches_1408),
                                   per_ext_voters(lv_ches_1409),
                                   per_ext_voters(lv_ches_1601),
                                   per_ext_voters(lv_ches_1602),
                                   per_ext_voters(lv_ches_1603),
                                   per_ext_voters(lv_ches_1604),
                                   per_ext_voters(lv_ches_1605),
                                   per_ext_voters(lv_ches_1606),
                                   per_ext_voters(lv_ches_1607),
                                   per_ext_voters(lv_ches_1610),
                                   per_ext_voters(lv_ches_1611),
                                   per_ext_voters(lv_ches_1612),
                                   per_ext_voters(lv_ches_2002),
                                   per_ext_voters(lv_ches_2004),
                                   per_ext_voters(lv_ches_2007),
                                   per_ext_voters(lv_ches_2010),
                                   per_ext_voters(lv_ches_2015),
                                   per_ext_voters(lv_ches_2016),
                                   per_ext_voters(lv_ches_2101),
                                   per_ext_voters(lv_ches_2103),
                                   per_ext_voters(lv_ches_2104),
                                   per_ext_voters(lv_ches_2109),
                                   per_ext_voters(lv_ches_2111),
                                   per_ext_voters(lv_ches_2113),
                                   per_ext_voters(lv_ches_2201),
                                   per_ext_voters(lv_ches_2202),
                                   per_ext_voters(lv_ches_2203),
                                   per_ext_voters(lv_ches_2204),
                                   per_ext_voters(lv_ches_2207),
                                   per_ext_voters(lv_ches_2301),
                                   per_ext_voters(lv_ches_2302),
                                   per_ext_voters(lv_ches_2308),
                                   per_ext_voters(lv_ches_2309),
                                   per_ext_voters(lv_ches_2310),
                                   per_ext_voters(lv_ches_2311),
                                   per_ext_voters(lv_ches_2402),
                                   per_ext_voters(lv_ches_2405),
                                   per_ext_voters(lv_ches_2406),
                                   per_ext_voters(lv_ches_2410),
                                   per_ext_voters(lv_ches_2412),
                                   per_ext_voters(lv_ches_2501),
                                   per_ext_voters(lv_ches_2506),
                                   per_ext_voters(lv_ches_2507),
                                   per_ext_voters(lv_ches_2511),
                                   per_ext_voters(lv_ches_2515),
                                   per_ext_voters(lv_ches_2516),
                                   per_ext_voters(lv_ches_2518),
                                   per_ext_voters(lv_ches_2601),
                                   per_ext_voters(lv_ches_2603),
                                   per_ext_voters(lv_ches_2605),
                                   per_ext_voters(lv_ches_2606),
                                   per_ext_voters(lv_ches_2613),
                                   per_ext_voters(lv_ches_2614),
                                   per_ext_voters(lv_ches_2616),
                                   per_ext_voters(lv_ches_2701),
                                   per_ext_voters(lv_ches_2702),
                                   per_ext_voters(lv_ches_2704),
                                   per_ext_voters(lv_ches_2705),
                                   per_ext_voters(lv_ches_2706),
                                   per_ext_voters(lv_ches_2710),
                                   per_ext_voters(lv_ches_2711),
                                   per_ext_voters(lv_ches_2802),
                                   per_ext_voters(lv_ches_2803),
                                   per_ext_voters(lv_ches_2804),
                                   per_ext_voters(lv_ches_2805),
                                   per_ext_voters(lv_ches_2809),
                                   per_ext_voters(lv_ches_2812),
                                   per_ext_voters(lv_ches_2813),
                                   per_ext_voters(lv_ches_2814),
                                   per_ext_voters(lv_ches_2815),
                                   per_ext_voters(lv_ches_2902),
                                   per_ext_voters(lv_ches_2903),
                                   per_ext_voters(lv_ches_2904),
                                   per_ext_voters(lv_ches_2905),
                                   per_ext_voters(lv_ches_2906),
                                   per_ext_voters(lv_ches_2914),
                                   per_ext_voters(lv_ches_3101),
                                   per_ext_voters(lv_ches_3102),
                                   per_ext_voters(lv_ches_3104),
                                   per_ext_voters(lv_ches_3105),
                                   per_ext_voters(lv_ches_3107),
                                   per_ext_voters(lv_ches_3109),
                                   per_ext_voters(lv_ches_3112),
                                   per_ext_voters(lv_ches_3114),
                                   per_ext_voters(lv_ches_3701),
                                   per_ext_voters(lv_ches_3702),
                                   per_ext_voters(lv_ches_3801),
                                   per_ext_voters(lv_ches_3802),
                                   per_ext_voters(lv_ches_3803),
                                   per_ext_voters(lv_ches_3804),
                                   per_ext_voters(lv_ches_3805),
                                   per_ext_voters(lv_ches_3806),
                                   per_ext_voters(lv_ches_4001),
                                   per_ext_voters(lv_ches_4003),
                                   per_ext_voters(lv_ches_4004),
                                   per_ext_voters(lv_ches_4005),
                                   per_ext_voters(lv_ches_4006),
                                   per_ext_voters(lv_ches_1301),
                                   per_ext_voters(lv_ches_1308),
                                   per_ext_voters(lv_ches_117),
                                   per_ext_voters(lv_ches_118),
                                   per_ext_voters(lv_ches_115),
                                   per_ext_voters(lv_ches_216),
                                   per_ext_voters(lv_ches_507),
                                   per_ext_voters(lv_ches_624),
                                   per_ext_voters(lv_ches_828),
                                   per_ext_voters(lv_ches_803),
                                   per_ext_voters(lv_ches_1006),
                                   per_ext_voters(lv_ches_1109))



party_vot_var_09$perext_eu_lv <- ifelse(is.nan(party_vot_var_09$perext_eu_lv) == TRUE, NA, party_vot_var_09$perext_eu_lv)



#############EU dimension: Average distance from country mean##########################################
##Can function as an add-on to percentage of extreme scores. 


library(DescTools) #using this function to get MAD https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/MeanAD

party_mad <- function(party, var, country) #MAD for likely voters of a party, on a variable, in a country
{
  country_mean <-  function(var, country) #country_mean calculates the mean on a variable in a country
  {
    x <- mean(var[t102 == country], na.rm=TRUE)
    return(x)
  }
  y <- MeanAD(var[party == 1], FUN=country_mean(var, country), na.rm=TRUE) #MeanAD calculates the mean distance on a variable for likely voters in a country where the midpoint is the return from above, i.e. country mean. 
  return(y) 
}

party_mad(lv_ches_1017, q80, 1528) #PVV far away from country mean
party_mad(lv_ches_1003, q80, 1056) #VVD closer to country mean


party_vot_var_09$mad_eu_lv_countrymean <- c(party_mad(lv_ches_102, q80, 1056),
                                            party_mad(lv_ches_103, q80, 1056),
                                            party_mad(lv_ches_104, q80, 1056),
                                            party_mad(lv_ches_105, q80, 1056),
                                            party_mad(lv_ches_106, q80, 1056),
                                            party_mad(lv_ches_107, q80, 1056),
                                            party_mad(lv_ches_108, q80, 1056),
                                            party_mad(lv_ches_109, q80, 1056),
                                            party_mad(lv_ches_110, q80, 1056),
                                            party_mad(lv_ches_112, q80, 1056),
                                            party_mad(lv_ches_119, q80, 1056),
                                            party_mad(lv_ches_201, q80, 1208),
                                            party_mad(lv_ches_202, q80, 1208),
                                            party_mad(lv_ches_203, q80, 1208),
                                            party_mad(lv_ches_206, q80, 1208),
                                            party_mad(lv_ches_211, q80, 1208),
                                            party_mad(lv_ches_215, q80, 1208),
                                            party_mad(lv_ches_218, q80, 1208),
                                            party_mad(lv_ches_301, q80, 1276),
                                            party_mad(lv_ches_302, q80, 1276),
                                            party_mad(lv_ches_303, q80, 1276),
                                            party_mad(lv_ches_304, q80, 1276),
                                            party_mad(lv_ches_306, q80, 1276),
                                            party_mad(lv_ches_310, q80, 1276),
                                            party_mad(lv_ches_311, q80, 1276),
                                            party_mad(lv_ches_401, q80, 1300),
                                            party_mad(lv_ches_402, q80, 1300),
                                            party_mad(lv_ches_403, q80, 1300),
                                            party_mad(lv_ches_404, q80, 1300),
                                            party_mad(lv_ches_410, q80, 1300),
                                            party_mad(lv_ches_413, q80, 1300),
                                            party_mad(lv_ches_414, q80, 1300),
                                            party_mad(lv_ches_415, q80, 1300),
                                            party_mad(lv_ches_501, q80, 1724),
                                            party_mad(lv_ches_502, q80, 1724),
                                            party_mad(lv_ches_504, q80, 1724),
                                            party_mad(lv_ches_505, q80, 1724),
                                            party_mad(lv_ches_506, q80, 1724),
                                            party_mad(lv_ches_511, q80, 1724),
                                            party_mad(lv_ches_513, q80, 1724),
                                            party_mad(lv_ches_517, q80, 1724),
                                            party_mad(lv_ches_523, q80, 1724),
                                            party_mad(lv_ches_525, q80, 1724),
                                            party_mad(lv_ches_601, q80, 1250),
                                            party_mad(lv_ches_602, q80, 1250),
                                            party_mad(lv_ches_605, q80, 1250),
                                            party_mad(lv_ches_609, q80, 1250),
                                            party_mad(lv_ches_610, q80, 1250),
                                            party_mad(lv_ches_613, q80, 1250),
                                            party_mad(lv_ches_701, q80, 1372),
                                            party_mad(lv_ches_702, q80, 1372),
                                            party_mad(lv_ches_703, q80, 1372),
                                            party_mad(lv_ches_705, q80, 1372),
                                            party_mad(lv_ches_707, q80, 1372),
                                            party_mad(lv_ches_708, q80, 1372),
                                            party_mad(lv_ches_811, q80, 1380),
                                            party_mad(lv_ches_814, q80, 1380),
                                            party_mad(lv_ches_815, q80, 1380),
                                            party_mad(lv_ches_827, q80, 1380),
                                            party_mad(lv_ches_837, q80, 1380),
                                            party_mad(lv_ches_838, q80, 1380),
                                            party_mad(lv_ches_844, q80, 1380),
                                            party_mad(lv_ches_845, q80, 1380),
                                            party_mad(lv_ches_848, q80, 1380),
                                            party_mad(lv_ches_1001, q80, 1528),
                                            party_mad(lv_ches_1002, q80, 1528),
                                            party_mad(lv_ches_1003, q80, 1528),
                                            party_mad(lv_ches_1004, q80, 1528),
                                            party_mad(lv_ches_1005, q80, 1528),
                                            party_mad(lv_ches_1014, q80, 1528),
                                            party_mad(lv_ches_1016, q80, 1528),
                                            party_mad(lv_ches_1017, q80, 1528),
                                            party_mad(lv_ches_1018, q80, 1528),
                                            party_mad(lv_ches_1101, q80, 1826),
                                            party_mad(lv_ches_1102, q80, 1826),
                                            party_mad(lv_ches_1104, q80, 1826),
                                            party_mad(lv_ches_1105, q80, 1826),
                                            party_mad(lv_ches_1106, q80, 1826),
                                            party_mad(lv_ches_1107, q80, 1826),
                                            party_mad(lv_ches_1108, q80, 1826),
                                            party_mad(lv_ches_1201, q80, 1620),
                                            party_mad(lv_ches_1202, q80, 1620),
                                            party_mad(lv_ches_1205, q80, 1620),
                                            party_mad(lv_ches_1206, q80, 1620),
                                            party_mad(lv_ches_1208, q80, 1620),
                                            party_mad(lv_ches_1209, q80, 1620),
                                            party_mad(lv_ches_1302, q80, 1040),
                                            party_mad(lv_ches_1303, q80, 1040),
                                            party_mad(lv_ches_1304, q80, 1040),
                                            party_mad(lv_ches_1307, q80, 1040),
                                            party_mad(lv_ches_1309, q80, 1040),
                                            party_mad(lv_ches_1401, q80, 1246),
                                            party_mad(lv_ches_1402, q80, 1246),
                                            party_mad(lv_ches_1403, q80, 1246),
                                            party_mad(lv_ches_1404, q80, 1246),
                                            party_mad(lv_ches_1405, q80, 1246),
                                            party_mad(lv_ches_1406, q80, 1246),
                                            party_mad(lv_ches_1408, q80, 1246),
                                            party_mad(lv_ches_1409, q80, 1246),
                                            party_mad(lv_ches_1601, q80, 1752),
                                            party_mad(lv_ches_1602, q80, 1752),
                                            party_mad(lv_ches_1603, q80, 1752),
                                            party_mad(lv_ches_1604, q80, 1752),
                                            party_mad(lv_ches_1605, q80, 1752),
                                            party_mad(lv_ches_1606, q80, 1752),
                                            party_mad(lv_ches_1607, q80, 1752),
                                            party_mad(lv_ches_1610, q80, 1752),
                                            party_mad(lv_ches_1611, q80, 1752),
                                            party_mad(lv_ches_1612, q80, 1752),
                                            party_mad(lv_ches_2002, q80, 1100),
                                            party_mad(lv_ches_2004, q80, 1100),
                                            party_mad(lv_ches_2007, q80, 1100),
                                            party_mad(lv_ches_2010, q80, 1100),
                                            party_mad(lv_ches_2015, q80, 1100),
                                            party_mad(lv_ches_2016, q80, 1100),
                                            party_mad(lv_ches_2101, q80, 1203),
                                            party_mad(lv_ches_2103, q80, 1203),
                                            party_mad(lv_ches_2104, q80, 1203),
                                            party_mad(lv_ches_2109, q80, 1203),
                                            party_mad(lv_ches_2111, q80, 1203),
                                            party_mad(lv_ches_2113, q80, 1203),
                                            party_mad(lv_ches_2201, q80, 1233),
                                            party_mad(lv_ches_2202, q80, 1233),
                                            party_mad(lv_ches_2203, q80, 1233),
                                            party_mad(lv_ches_2204, q80, 1233),
                                            party_mad(lv_ches_2207, q80, 1233),
                                            party_mad(lv_ches_2301, q80, 1348),
                                            party_mad(lv_ches_2302, q80, 1348),
                                            party_mad(lv_ches_2308, q80, 1348),
                                            party_mad(lv_ches_2309, q80, 1348),
                                            party_mad(lv_ches_2310, q80, 1348),
                                            party_mad(lv_ches_2311, q80, 1348),
                                            party_mad(lv_ches_2402, q80, 1428),
                                            party_mad(lv_ches_2405, q80, 1428),
                                            party_mad(lv_ches_2406, q80, 1428),
                                            party_mad(lv_ches_2410, q80, 1428),
                                            party_mad(lv_ches_2412, q80, 1428),
                                            party_mad(lv_ches_2501, q80, 1440),
                                            party_mad(lv_ches_2506, q80, 1440),
                                            party_mad(lv_ches_2507, q80, 1440),
                                            party_mad(lv_ches_2511, q80, 1440),
                                            party_mad(lv_ches_2515, q80, 1440),
                                            party_mad(lv_ches_2516, q80, 1440),
                                            party_mad(lv_ches_2518, q80, 1440),
                                            party_mad(lv_ches_2601, q80, 1616),
                                            party_mad(lv_ches_2603, q80, 1616),
                                            party_mad(lv_ches_2605, q80, 1616),
                                            party_mad(lv_ches_2606, q80, 1616),
                                            party_mad(lv_ches_2613, q80, 1616),
                                            party_mad(lv_ches_2614, q80, 1616),
                                            party_mad(lv_ches_2616, q80, 1616),
                                            party_mad(lv_ches_2701, q80, 1642),
                                            party_mad(lv_ches_2702, q80, 1642),
                                            party_mad(lv_ches_2704, q80, 1642),
                                            party_mad(lv_ches_2705, q80, 1642),
                                            party_mad(lv_ches_2706, q80, 1642),
                                            party_mad(lv_ches_2710, q80, 1642),
                                            party_mad(lv_ches_2711, q80, 1642),
                                            party_mad(lv_ches_2802, q80, 1703),
                                            party_mad(lv_ches_2803, q80, 1703),
                                            party_mad(lv_ches_2804, q80, 1703),
                                            party_mad(lv_ches_2805, q80, 1703),
                                            party_mad(lv_ches_2809, q80, 1703),
                                            party_mad(lv_ches_2812, q80, 1703),
                                            party_mad(lv_ches_2813, q80, 1703),
                                            party_mad(lv_ches_2814, q80, 1703),
                                            party_mad(lv_ches_2815, q80, 1703),
                                            party_mad(lv_ches_2902, q80, 1705),
                                            party_mad(lv_ches_2903, q80, 1705),
                                            party_mad(lv_ches_2904, q80, 1705),
                                            party_mad(lv_ches_2905, q80, 1705),
                                            party_mad(lv_ches_2906, q80, 1705),
                                            party_mad(lv_ches_2914, q80, 1705),
                                            party_mad(lv_ches_3101, q80, 1191),
                                            party_mad(lv_ches_3102, q80, 1191),
                                            party_mad(lv_ches_3104, q80, 1191),
                                            party_mad(lv_ches_3105, q80, 1191),
                                            party_mad(lv_ches_3107, q80, 1191),
                                            party_mad(lv_ches_3109, q80, 1191),
                                            party_mad(lv_ches_3112, q80, 1191),
                                            party_mad(lv_ches_3114, q80, 1191),
                                            party_mad(lv_ches_3701, q80, 1470),
                                            party_mad(lv_ches_3702, q80, 1470),
                                            party_mad(lv_ches_3801, q80, 1442),
                                            party_mad(lv_ches_3802, q80, 1442),
                                            party_mad(lv_ches_3803, q80, 1442),
                                            party_mad(lv_ches_3804, q80, 1442),
                                            party_mad(lv_ches_3805, q80, 1442),
                                            party_mad(lv_ches_3806, q80, 1442),
                                            party_mad(lv_ches_4001, q80, 1196),
                                            party_mad(lv_ches_4003, q80, 1196),
                                            party_mad(lv_ches_4004, q80, 1196),
                                            party_mad(lv_ches_4005, q80, 1196),
                                            party_mad(lv_ches_4006, q80, 1196),
                                            party_mad(lv_ches_1301, q80, 1040),
                                            party_mad(lv_ches_1308, q80, 1040),
                                            party_mad(lv_ches_117, q80, 1056),
                                            party_mad(lv_ches_118, q80, 1056),
                                            party_mad(lv_ches_115, q80, 1056),
                                            party_mad(lv_ches_216, q80, 1208),
                                            party_mad(lv_ches_507, q80, 1724),
                                            party_mad(lv_ches_624, q80, 1250),
                                            party_mad(lv_ches_828, q80, 1380),
                                            party_mad(lv_ches_803, q80, 1380),
                                            party_mad(lv_ches_1006, q80, 1528),
                                            party_mad(lv_ches_1109, q80, 1826))


detach("package:DescTools", unload=TRUE)

party_vot_var_09$mad_eu_lv_countrymean <- ifelse(is.nan(party_vot_var_09$mad_eu_lv_countrymean) == TRUE, NA, party_vot_var_09$mad_eu_lv_countrymean)








#############Education and other variables###################
#overlap voters
#which(colnames(EES_2009)=="lv_ches_102") #seeing first and last column numbers
#which(colnames(EES_2009)=="lv_ches_1109")
attach(EES_2009)
EES_2009$vot_overl <- NA
EES_2009$vot_overl <- rowSums(EES_2009[279:484], na.rm=TRUE)

#mean(vot_overl[lv_ches_1108 == 1], na.rm = TRUE)

#describe(EES_2009$lv_ches_1605)

#describe(EES_2009$vot_overl[EES_2009$lv_ches_1606 == 1])
#mean(EES_2009$vot_overl[EES_2009$lv_ches_1606 == 1], na.rm=TRUE)
#mean(vot_overl[lv_ches_1606 == 1], na.rm = TRUE)

attach(EES_2009)
party_vot_var_09$vot_overl <- NA
party_vot_var_09$vot_overl <- c(mean(vot_overl[lv_ches_102 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_103 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_104 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_105 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_106 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_107 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_108 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_109 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_110 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_112 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_119 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_201 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_202 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_203 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_206 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_211 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_215 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_218 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_301 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_302 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_303 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_304 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_306 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_310 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_311 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_401 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_402 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_403 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_404 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_410 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_413 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_414 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_415 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_501 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_502 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_504 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_505 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_506 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_511 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_513 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_517 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_523 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_525 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_601 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_602 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_605 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_609 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_610 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_613 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_701 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_702 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_703 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_705 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_707 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_708 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_811 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_814 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_815 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_827 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_837 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_838 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_844 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_845 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_848 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1001 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1002 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1003 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1004 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1005 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1014 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1016 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1017 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1018 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1101 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1102 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1104 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1105 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1106 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1107 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1108 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1201 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1202 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1205 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1206 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1208 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1209 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1302 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1303 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1304 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1307 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1309 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1401 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1402 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1403 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1404 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1405 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1406 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1408 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1409 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1601 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1602 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1603 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1604 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1605 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1606 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1607 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1610 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1611 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1612 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2002 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2004 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2007 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2010 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2015 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2016 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2101 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2103 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2104 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2109 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2111 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2113 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2201 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2202 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2203 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2204 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2207 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2301 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2302 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2308 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2309 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2310 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2311 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2402 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2405 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2406 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2410 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2412 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2501 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2506 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2507 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2511 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2515 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2516 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2518 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2601 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2603 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2605 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2606 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2613 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2614 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2616 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2701 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2702 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2704 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2705 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2706 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2710 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2711 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2802 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2803 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2804 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2805 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2809 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2812 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2813 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2814 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2815 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2902 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2903 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2904 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2905 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2906 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_2914 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3101 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3102 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3104 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3105 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3107 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3109 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3112 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3114 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3701 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3702 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3801 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3802 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3803 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3804 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3805 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_3806 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_4001 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_4003 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_4004 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_4005 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_4006 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1301 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1308 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_117 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_118 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_115 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_216 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_507 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_624 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_828 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_803 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1006 == 1], na.rm = TRUE),
                                 mean(vot_overl[lv_ches_1109 == 1], na.rm = TRUE))


party_vot_var_09$vot_overl <- ifelse(is.nan(party_vot_var_09$vot_overl) == TRUE, NA, party_vot_var_09$vot_overl)


#education
describe(EES_2009$v200)
EES_2009$v200[EES_2009$v200 == 71 | EES_2009$v200 == 77 | EES_2009$v200 == 88 | EES_2009$v200 == 98 | EES_2009$v200 == 99 ] <- NA
attach(EES_2009)

party_vot_var_09$sd_edu_lv <- c(sd(v200[lv_ches_102 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_103 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_104 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_105 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_106 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_107 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_108 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_109 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_110 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_112 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_119 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_201 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_202 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_203 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_206 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_211 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_215 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_218 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_301 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_302 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_303 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_304 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_306 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_310 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_311 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_401 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_402 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_403 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_404 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_410 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_413 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_414 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_415 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_501 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_502 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_504 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_505 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_506 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_511 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_513 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_517 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_523 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_525 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_601 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_602 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_605 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_609 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_610 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_613 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_701 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_702 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_703 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_705 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_707 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_708 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_811 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_814 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_815 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_827 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_837 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_838 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_844 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_845 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_848 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1001 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1002 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1003 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1004 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1005 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1014 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1016 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1017 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1018 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1101 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1102 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1104 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1105 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1106 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1107 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1108 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1201 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1202 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1205 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1206 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1208 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1209 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1302 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1303 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1304 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1307 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1309 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1401 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1402 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1403 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1404 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1405 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1406 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1408 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1409 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1601 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1602 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1603 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1604 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1605 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1606 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1607 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1610 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1611 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1612 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2002 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2004 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2007 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2010 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2015 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2016 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2101 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2103 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2104 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2109 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2111 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2113 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2201 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2202 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2203 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2204 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2207 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2301 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2302 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2308 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2309 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2310 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2311 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2402 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2405 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2406 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2410 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2412 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2501 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2506 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2507 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2511 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2515 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2516 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2518 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2601 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2603 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2605 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2606 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2613 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2614 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2616 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2701 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2702 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2704 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2705 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2706 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2710 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2711 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2802 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2803 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2804 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2805 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2809 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2812 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2813 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2814 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2815 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2902 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2903 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2904 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2905 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2906 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_2914 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3101 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3102 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3104 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3105 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3107 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3109 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3112 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3114 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3701 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3702 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3801 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3802 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3803 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3804 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3805 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_3806 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_4001 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_4003 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_4004 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_4005 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_4006 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1301 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1308 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_117 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_118 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_115 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_216 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_507 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_624 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_828 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_803 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1006 == 1], na.rm = TRUE),
                                sd(v200[lv_ches_1109 == 1], na.rm = TRUE))


library(DescTools) #using this function to get MAD https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/MeanAD
MeanAD(v200, na.rm = TRUE)
MeanAD(v200[lv_ches_1017 == 1], na.rm = TRUE)  #SD for the PVV


#MAD of voters' Education 
party_vot_var_09$mad_edu_lv <- c(MeanAD(v200[lv_ches_102 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_103 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_104 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_105 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_106 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_107 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_108 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_109 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_110 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_112 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_119 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_201 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_202 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_203 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_206 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_211 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_215 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_218 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_301 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_302 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_303 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_304 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_306 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_310 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_311 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_401 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_402 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_403 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_404 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_410 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_413 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_414 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_415 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_501 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_502 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_504 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_505 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_506 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_511 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_513 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_517 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_523 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_525 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_601 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_602 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_605 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_609 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_610 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_613 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_701 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_702 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_703 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_705 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_707 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_708 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_811 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_814 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_815 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_827 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_837 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_838 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_844 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_845 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_848 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1001 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1002 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1003 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1004 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1005 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1014 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1016 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1017 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1018 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1101 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1102 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1104 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1105 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1106 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1107 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1108 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1201 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1202 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1205 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1206 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1208 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1209 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1302 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1303 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1304 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1307 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1309 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1401 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1402 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1403 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1404 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1405 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1406 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1408 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1409 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1601 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1602 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1603 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1604 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1605 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1606 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1607 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1610 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1611 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1612 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2002 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2004 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2007 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2010 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2015 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2016 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2101 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2103 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2104 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2109 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2111 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2113 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2201 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2202 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2203 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2204 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2207 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2301 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2302 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2308 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2309 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2310 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2311 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2402 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2405 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2406 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2410 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2412 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2501 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2506 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2507 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2511 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2515 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2516 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2518 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2601 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2603 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2605 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2606 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2613 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2614 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2616 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2701 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2702 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2704 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2705 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2706 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2710 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2711 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2802 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2803 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2804 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2805 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2809 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2812 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2813 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2814 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2815 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2902 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2903 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2904 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2905 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2906 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_2914 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3101 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3102 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3104 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3105 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3107 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3109 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3112 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3114 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3701 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3702 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3801 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3802 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3803 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3804 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3805 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_3806 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_4001 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_4003 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_4004 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_4005 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_4006 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1301 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1308 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_117 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_118 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_115 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_216 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_507 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_624 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_828 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_803 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1006 == 1], na.rm = TRUE),
                                 MeanAD(v200[lv_ches_1109 == 1], na.rm = TRUE))

detach("package:DescTools", unload=TRUE)

party_vot_var_09$mad_edu_lv <- ifelse(is.nan(party_vot_var_09$mad_edu_lv) == TRUE, NA, party_vot_var_09$mad_edu_lv)


party_vot_var_09$mean_edu_lv <- c(mean(v200[lv_ches_102 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_103 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_104 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_105 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_106 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_107 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_108 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_109 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_110 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_112 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_119 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_201 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_202 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_203 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_206 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_211 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_215 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_218 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_301 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_302 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_303 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_304 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_306 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_310 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_311 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_401 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_402 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_403 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_404 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_410 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_413 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_414 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_415 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_501 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_502 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_504 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_505 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_506 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_511 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_513 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_517 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_523 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_525 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_601 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_602 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_605 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_609 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_610 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_613 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_701 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_702 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_703 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_705 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_707 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_708 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_811 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_814 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_815 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_827 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_837 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_838 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_844 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_845 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_848 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1001 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1002 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1003 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1004 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1005 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1014 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1016 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1017 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1018 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1101 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1102 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1104 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1105 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1106 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1107 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1108 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1201 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1202 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1205 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1206 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1208 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1209 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1302 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1303 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1304 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1307 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1309 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1401 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1402 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1403 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1404 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1405 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1406 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1408 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1409 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1601 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1602 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1603 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1604 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1605 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1606 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1607 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1610 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1611 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1612 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2002 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2004 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2007 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2010 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2015 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2016 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2101 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2103 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2104 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2109 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2111 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2113 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2201 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2202 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2203 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2204 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2207 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2301 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2302 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2308 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2309 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2310 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2311 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2402 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2405 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2406 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2410 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2412 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2501 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2506 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2507 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2511 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2515 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2516 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2518 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2601 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2603 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2605 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2606 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2613 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2614 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2616 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2701 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2702 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2704 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2705 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2706 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2710 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2711 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2802 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2803 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2804 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2805 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2809 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2812 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2813 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2814 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2815 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2902 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2903 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2904 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2905 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2906 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_2914 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3101 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3102 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3104 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3105 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3107 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3109 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3112 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3114 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3701 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3702 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3801 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3802 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3803 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3804 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3805 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_3806 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_4001 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_4003 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_4004 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_4005 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_4006 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1301 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1308 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_117 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_118 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_115 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_216 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_507 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_624 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_828 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_803 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1006 == 1], na.rm = TRUE),
                                  mean(v200[lv_ches_1109 == 1], na.rm = TRUE))

party_vot_var_09$mean_edu_lv <- ifelse(is.nan(party_vot_var_09$mean_edu_lv) == TRUE, NA, party_vot_var_09$mean_edu_lv)


library(DescTools)
party_vot_var_09$mad_edu_lv_countrymean <- c(party_mad(lv_ches_102, v200, 1056),
                                             party_mad(lv_ches_103, v200, 1056),
                                             party_mad(lv_ches_104, v200, 1056),
                                             party_mad(lv_ches_105, v200, 1056),
                                             party_mad(lv_ches_106, v200, 1056),
                                             party_mad(lv_ches_107, v200, 1056),
                                             party_mad(lv_ches_108, v200, 1056),
                                             party_mad(lv_ches_109, v200, 1056),
                                             party_mad(lv_ches_110, v200, 1056),
                                             party_mad(lv_ches_112, v200, 1056),
                                             party_mad(lv_ches_119, v200, 1056),
                                             party_mad(lv_ches_201, v200, 1208),
                                             party_mad(lv_ches_202, v200, 1208),
                                             party_mad(lv_ches_203, v200, 1208),
                                             party_mad(lv_ches_206, v200, 1208),
                                             party_mad(lv_ches_211, v200, 1208),
                                             party_mad(lv_ches_215, v200, 1208),
                                             party_mad(lv_ches_218, v200, 1208),
                                             party_mad(lv_ches_301, v200, 1276),
                                             party_mad(lv_ches_302, v200, 1276),
                                             party_mad(lv_ches_303, v200, 1276),
                                             party_mad(lv_ches_304, v200, 1276),
                                             party_mad(lv_ches_306, v200, 1276),
                                             party_mad(lv_ches_310, v200, 1276),
                                             party_mad(lv_ches_311, v200, 1276),
                                             party_mad(lv_ches_401, v200, 1300),
                                             party_mad(lv_ches_402, v200, 1300),
                                             party_mad(lv_ches_403, v200, 1300),
                                             party_mad(lv_ches_404, v200, 1300),
                                             party_mad(lv_ches_410, v200, 1300),
                                             party_mad(lv_ches_413, v200, 1300),
                                             party_mad(lv_ches_414, v200, 1300),
                                             party_mad(lv_ches_415, v200, 1300),
                                             party_mad(lv_ches_501, v200, 1724),
                                             party_mad(lv_ches_502, v200, 1724),
                                             party_mad(lv_ches_504, v200, 1724),
                                             party_mad(lv_ches_505, v200, 1724),
                                             party_mad(lv_ches_506, v200, 1724),
                                             party_mad(lv_ches_511, v200, 1724),
                                             party_mad(lv_ches_513, v200, 1724),
                                             party_mad(lv_ches_517, v200, 1724),
                                             party_mad(lv_ches_523, v200, 1724),
                                             party_mad(lv_ches_525, v200, 1724),
                                             party_mad(lv_ches_601, v200, 1250),
                                             party_mad(lv_ches_602, v200, 1250),
                                             party_mad(lv_ches_605, v200, 1250),
                                             party_mad(lv_ches_609, v200, 1250),
                                             party_mad(lv_ches_610, v200, 1250),
                                             party_mad(lv_ches_613, v200, 1250),
                                             party_mad(lv_ches_701, v200, 1372),
                                             party_mad(lv_ches_702, v200, 1372),
                                             party_mad(lv_ches_703, v200, 1372),
                                             party_mad(lv_ches_705, v200, 1372),
                                             party_mad(lv_ches_707, v200, 1372),
                                             party_mad(lv_ches_708, v200, 1372),
                                             party_mad(lv_ches_811, v200, 1380),
                                             party_mad(lv_ches_814, v200, 1380),
                                             party_mad(lv_ches_815, v200, 1380),
                                             party_mad(lv_ches_827, v200, 1380),
                                             party_mad(lv_ches_837, v200, 1380),
                                             party_mad(lv_ches_838, v200, 1380),
                                             party_mad(lv_ches_844, v200, 1380),
                                             party_mad(lv_ches_845, v200, 1380),
                                             party_mad(lv_ches_848, v200, 1380),
                                             party_mad(lv_ches_1001, v200, 1528),
                                             party_mad(lv_ches_1002, v200, 1528),
                                             party_mad(lv_ches_1003, v200, 1528),
                                             party_mad(lv_ches_1004, v200, 1528),
                                             party_mad(lv_ches_1005, v200, 1528),
                                             party_mad(lv_ches_1014, v200, 1528),
                                             party_mad(lv_ches_1016, v200, 1528),
                                             party_mad(lv_ches_1017, v200, 1528),
                                             party_mad(lv_ches_1018, v200, 1528),
                                             party_mad(lv_ches_1101, v200, 1826),
                                             party_mad(lv_ches_1102, v200, 1826),
                                             party_mad(lv_ches_1104, v200, 1826),
                                             party_mad(lv_ches_1105, v200, 1826),
                                             party_mad(lv_ches_1106, v200, 1826),
                                             party_mad(lv_ches_1107, v200, 1826),
                                             party_mad(lv_ches_1108, v200, 1826),
                                             party_mad(lv_ches_1201, v200, 1620),
                                             party_mad(lv_ches_1202, v200, 1620),
                                             party_mad(lv_ches_1205, v200, 1620),
                                             party_mad(lv_ches_1206, v200, 1620),
                                             party_mad(lv_ches_1208, v200, 1620),
                                             party_mad(lv_ches_1209, v200, 1620),
                                             party_mad(lv_ches_1302, v200, 1040),
                                             party_mad(lv_ches_1303, v200, 1040),
                                             party_mad(lv_ches_1304, v200, 1040),
                                             party_mad(lv_ches_1307, v200, 1040),
                                             party_mad(lv_ches_1309, v200, 1040),
                                             party_mad(lv_ches_1401, v200, 1246),
                                             party_mad(lv_ches_1402, v200, 1246),
                                             party_mad(lv_ches_1403, v200, 1246),
                                             party_mad(lv_ches_1404, v200, 1246),
                                             party_mad(lv_ches_1405, v200, 1246),
                                             party_mad(lv_ches_1406, v200, 1246),
                                             party_mad(lv_ches_1408, v200, 1246),
                                             party_mad(lv_ches_1409, v200, 1246),
                                             party_mad(lv_ches_1601, v200, 1752),
                                             party_mad(lv_ches_1602, v200, 1752),
                                             party_mad(lv_ches_1603, v200, 1752),
                                             party_mad(lv_ches_1604, v200, 1752),
                                             party_mad(lv_ches_1605, v200, 1752),
                                             party_mad(lv_ches_1606, v200, 1752),
                                             party_mad(lv_ches_1607, v200, 1752),
                                             party_mad(lv_ches_1610, v200, 1752),
                                             party_mad(lv_ches_1611, v200, 1752),
                                             party_mad(lv_ches_1612, v200, 1752),
                                             party_mad(lv_ches_2002, v200, 1100),
                                             party_mad(lv_ches_2004, v200, 1100),
                                             party_mad(lv_ches_2007, v200, 1100),
                                             party_mad(lv_ches_2010, v200, 1100),
                                             party_mad(lv_ches_2015, v200, 1100),
                                             party_mad(lv_ches_2016, v200, 1100),
                                             party_mad(lv_ches_2101, v200, 1203),
                                             party_mad(lv_ches_2103, v200, 1203),
                                             party_mad(lv_ches_2104, v200, 1203),
                                             party_mad(lv_ches_2109, v200, 1203),
                                             party_mad(lv_ches_2111, v200, 1203),
                                             party_mad(lv_ches_2113, v200, 1203),
                                             party_mad(lv_ches_2201, v200, 1233),
                                             party_mad(lv_ches_2202, v200, 1233),
                                             party_mad(lv_ches_2203, v200, 1233),
                                             party_mad(lv_ches_2204, v200, 1233),
                                             party_mad(lv_ches_2207, v200, 1233),
                                             party_mad(lv_ches_2301, v200, 1348),
                                             party_mad(lv_ches_2302, v200, 1348),
                                             party_mad(lv_ches_2308, v200, 1348),
                                             party_mad(lv_ches_2309, v200, 1348),
                                             party_mad(lv_ches_2310, v200, 1348),
                                             party_mad(lv_ches_2311, v200, 1348),
                                             party_mad(lv_ches_2402, v200, 1428),
                                             party_mad(lv_ches_2405, v200, 1428),
                                             party_mad(lv_ches_2406, v200, 1428),
                                             party_mad(lv_ches_2410, v200, 1428),
                                             party_mad(lv_ches_2412, v200, 1428),
                                             party_mad(lv_ches_2501, v200, 1440),
                                             party_mad(lv_ches_2506, v200, 1440),
                                             party_mad(lv_ches_2507, v200, 1440),
                                             party_mad(lv_ches_2511, v200, 1440),
                                             party_mad(lv_ches_2515, v200, 1440),
                                             party_mad(lv_ches_2516, v200, 1440),
                                             party_mad(lv_ches_2518, v200, 1440),
                                             party_mad(lv_ches_2601, v200, 1616),
                                             party_mad(lv_ches_2603, v200, 1616),
                                             party_mad(lv_ches_2605, v200, 1616),
                                             party_mad(lv_ches_2606, v200, 1616),
                                             party_mad(lv_ches_2613, v200, 1616),
                                             party_mad(lv_ches_2614, v200, 1616),
                                             party_mad(lv_ches_2616, v200, 1616),
                                             party_mad(lv_ches_2701, v200, 1642),
                                             party_mad(lv_ches_2702, v200, 1642),
                                             party_mad(lv_ches_2704, v200, 1642),
                                             party_mad(lv_ches_2705, v200, 1642),
                                             party_mad(lv_ches_2706, v200, 1642),
                                             party_mad(lv_ches_2710, v200, 1642),
                                             party_mad(lv_ches_2711, v200, 1642),
                                             party_mad(lv_ches_2802, v200, 1703),
                                             party_mad(lv_ches_2803, v200, 1703),
                                             party_mad(lv_ches_2804, v200, 1703),
                                             party_mad(lv_ches_2805, v200, 1703),
                                             party_mad(lv_ches_2809, v200, 1703),
                                             party_mad(lv_ches_2812, v200, 1703),
                                             party_mad(lv_ches_2813, v200, 1703),
                                             party_mad(lv_ches_2814, v200, 1703),
                                             party_mad(lv_ches_2815, v200, 1703),
                                             party_mad(lv_ches_2902, v200, 1705),
                                             party_mad(lv_ches_2903, v200, 1705),
                                             party_mad(lv_ches_2904, v200, 1705),
                                             party_mad(lv_ches_2905, v200, 1705),
                                             party_mad(lv_ches_2906, v200, 1705),
                                             party_mad(lv_ches_2914, v200, 1705),
                                             party_mad(lv_ches_3101, v200, 1191),
                                             party_mad(lv_ches_3102, v200, 1191),
                                             party_mad(lv_ches_3104, v200, 1191),
                                             party_mad(lv_ches_3105, v200, 1191),
                                             party_mad(lv_ches_3107, v200, 1191),
                                             party_mad(lv_ches_3109, v200, 1191),
                                             party_mad(lv_ches_3112, v200, 1191),
                                             party_mad(lv_ches_3114, v200, 1191),
                                             party_mad(lv_ches_3701, v200, 1470),
                                             party_mad(lv_ches_3702, v200, 1470),
                                             party_mad(lv_ches_3801, v200, 1442),
                                             party_mad(lv_ches_3802, v200, 1442),
                                             party_mad(lv_ches_3803, v200, 1442),
                                             party_mad(lv_ches_3804, v200, 1442),
                                             party_mad(lv_ches_3805, v200, 1442),
                                             party_mad(lv_ches_3806, v200, 1442),
                                             party_mad(lv_ches_4001, v200, 1196),
                                             party_mad(lv_ches_4003, v200, 1196),
                                             party_mad(lv_ches_4004, v200, 1196),
                                             party_mad(lv_ches_4005, v200, 1196),
                                             party_mad(lv_ches_4006, v200, 1196),
                                             party_mad(lv_ches_1301, v200, 1040),
                                             party_mad(lv_ches_1308, v200, 1040),
                                             party_mad(lv_ches_117, v200, 1056),
                                             party_mad(lv_ches_118, v200, 1056),
                                             party_mad(lv_ches_115, v200, 1056),
                                             party_mad(lv_ches_216, v200, 1208),
                                             party_mad(lv_ches_507, v200, 1724),
                                             party_mad(lv_ches_624, v200, 1250),
                                             party_mad(lv_ches_828, v200, 1380),
                                             party_mad(lv_ches_803, v200, 1380),
                                             party_mad(lv_ches_1006, v200, 1528),
                                             party_mad(lv_ches_1109, v200, 1826))


detach("package:DescTools", unload=TRUE)

party_vot_var_09$mad_edu_lv_countrymean <- ifelse(is.nan(party_vot_var_09$mad_edu_lv_countrymean) == TRUE, NA, party_vot_var_09$mad_edu_lv_countrymean)









#############Most important issue################
EES_2009$mii_immi <- 0
EES_2009$mii_immi[EES_2009$q1== 5 | EES_2009$q1== 3 | EES_2009$q1== 72 |
                    EES_2009$q1== 73 | EES_2009$q1== 99 | EES_2009$q1== 104 |
                    EES_2009$q1== 105] <- 1

EES_2009$mii_eu <- 0
EES_2009$mii_eu[EES_2009$q1== 1 | EES_2009$q1== 3 | EES_2009$q1== 39 |
                  EES_2009$q1== 42 | EES_2009$q1== 43 | EES_2009$q1== 44 |
                  EES_2009$q1== 45 | EES_2009$q1== 46 | EES_2009$q1== 47 |
                  EES_2009$q1== 48 | EES_2009$q1== 49 | EES_2009$q1== 50 |
                  EES_2009$q1== 84] <- 1

###second most important issue. 
EES_2009$mii_immi[EES_2009$q2== 5 | EES_2009$q2== 3 | EES_2009$q2== 72 |
                    EES_2009$q2== 73 | EES_2009$q2== 99 | EES_2009$q2== 104 |
                    EES_2009$q2== 105] <- 1

EES_2009$mii_eu <- 0
EES_2009$mii_eu[EES_2009$q2== 1 | EES_2009$q2== 3 | EES_2009$q2== 39 |
                  EES_2009$q2== 42 | EES_2009$q2== 43 | EES_2009$q2== 44 |
                  EES_2009$q2== 45 | EES_2009$q2== 46 | EES_2009$q2== 47 |
                  EES_2009$q2== 48 | EES_2009$q2== 49 | EES_2009$q2== 50 |
                  EES_2009$q2== 84] <- 1






#function for the percentage of likely voters of a party that have either immigration or European integration as MII
#take the sum of all not NA values for a question. This is the total n for that question. Then take all extreme values
#for that same question. Do this three times for both and calculate percentage extreme scores.
#note that not all people answering all three questions is taken into account by taking separate Ns for each question
#the lvches is the name for columns with dummies for likely voters.
per_mii_immi <- function(lvches)
{
  n1 = sum(!is.na((as.numeric(mii_immi[lvches == 1]))))
  t1 = sum(!is.na((as.numeric(mii_immi[lvches == 1 & (mii_immi == 1) ]))))
  pext <- (t1/n1) * 100
  return(pext)
}


per_mii_eu <- function(lvches)
{
  n1 = sum(!is.na((as.numeric(mii_eu[lvches == 1]))))
  t1 = sum(!is.na((as.numeric(mii_eu[lvches == 1 & (mii_eu == 1) ]))))
  pext <- (t1/n1) * 100
  return(pext)
}


attach(EES_2009)


####################Adding the code for all parties. Note that, just like above, all NAs are for Eastern European countries
party_vot_var_09$mii_immi_lv <- c(per_mii_immi(lv_ches_102),
                                   per_mii_immi(lv_ches_103),
                                   per_mii_immi(lv_ches_104),
                                   per_mii_immi(lv_ches_105),
                                   per_mii_immi(lv_ches_106),
                                   per_mii_immi(lv_ches_107),
                                   per_mii_immi(lv_ches_108),
                                   per_mii_immi(lv_ches_109),
                                   per_mii_immi(lv_ches_110),
                                   per_mii_immi(lv_ches_112),
                                   per_mii_immi(lv_ches_119),
                                   per_mii_immi(lv_ches_201),
                                   per_mii_immi(lv_ches_202),
                                   per_mii_immi(lv_ches_203),
                                   per_mii_immi(lv_ches_206),
                                   per_mii_immi(lv_ches_211),
                                   per_mii_immi(lv_ches_215),
                                   per_mii_immi(lv_ches_218),
                                   per_mii_immi(lv_ches_301),
                                   per_mii_immi(lv_ches_302),
                                   per_mii_immi(lv_ches_303),
                                   per_mii_immi(lv_ches_304),
                                   per_mii_immi(lv_ches_306),
                                   per_mii_immi(lv_ches_310),
                                   per_mii_immi(lv_ches_311),
                                   per_mii_immi(lv_ches_401),
                                   per_mii_immi(lv_ches_402),
                                   per_mii_immi(lv_ches_403),
                                   per_mii_immi(lv_ches_404),
                                   per_mii_immi(lv_ches_410),
                                   per_mii_immi(lv_ches_413),
                                   per_mii_immi(lv_ches_414),
                                   per_mii_immi(lv_ches_415),
                                   per_mii_immi(lv_ches_501),
                                   per_mii_immi(lv_ches_502),
                                   per_mii_immi(lv_ches_504),
                                   per_mii_immi(lv_ches_505),
                                   per_mii_immi(lv_ches_506),
                                   per_mii_immi(lv_ches_511),
                                   per_mii_immi(lv_ches_513),
                                   per_mii_immi(lv_ches_517),
                                   per_mii_immi(lv_ches_523),
                                   per_mii_immi(lv_ches_525),
                                   per_mii_immi(lv_ches_601),
                                   per_mii_immi(lv_ches_602),
                                   per_mii_immi(lv_ches_605),
                                   per_mii_immi(lv_ches_609),
                                   per_mii_immi(lv_ches_610),
                                   per_mii_immi(lv_ches_613),
                                   per_mii_immi(lv_ches_701),
                                   per_mii_immi(lv_ches_702),
                                   per_mii_immi(lv_ches_703),
                                   per_mii_immi(lv_ches_705),
                                   per_mii_immi(lv_ches_707),
                                   per_mii_immi(lv_ches_708),
                                   per_mii_immi(lv_ches_811),
                                   per_mii_immi(lv_ches_814),
                                   per_mii_immi(lv_ches_815),
                                   per_mii_immi(lv_ches_827),
                                   per_mii_immi(lv_ches_837),
                                   per_mii_immi(lv_ches_838),
                                   per_mii_immi(lv_ches_844),
                                   per_mii_immi(lv_ches_845),
                                   per_mii_immi(lv_ches_848),
                                   per_mii_immi(lv_ches_1001),
                                   per_mii_immi(lv_ches_1002),
                                   per_mii_immi(lv_ches_1003),
                                   per_mii_immi(lv_ches_1004),
                                   per_mii_immi(lv_ches_1005),
                                   per_mii_immi(lv_ches_1014),
                                   per_mii_immi(lv_ches_1016),
                                   per_mii_immi(lv_ches_1017),
                                   per_mii_immi(lv_ches_1018),
                                   per_mii_immi(lv_ches_1101),
                                   per_mii_immi(lv_ches_1102),
                                   per_mii_immi(lv_ches_1104),
                                   per_mii_immi(lv_ches_1105),
                                   per_mii_immi(lv_ches_1106),
                                   per_mii_immi(lv_ches_1107),
                                   per_mii_immi(lv_ches_1108),
                                   per_mii_immi(lv_ches_1201),
                                   per_mii_immi(lv_ches_1202),
                                   per_mii_immi(lv_ches_1205),
                                   per_mii_immi(lv_ches_1206),
                                   per_mii_immi(lv_ches_1208),
                                   per_mii_immi(lv_ches_1209),
                                   per_mii_immi(lv_ches_1302),
                                   per_mii_immi(lv_ches_1303),
                                   per_mii_immi(lv_ches_1304),
                                   per_mii_immi(lv_ches_1307),
                                   per_mii_immi(lv_ches_1309),
                                   per_mii_immi(lv_ches_1401),
                                   per_mii_immi(lv_ches_1402),
                                   per_mii_immi(lv_ches_1403),
                                   per_mii_immi(lv_ches_1404),
                                   per_mii_immi(lv_ches_1405),
                                   per_mii_immi(lv_ches_1406),
                                   per_mii_immi(lv_ches_1408),
                                   per_mii_immi(lv_ches_1409),
                                   per_mii_immi(lv_ches_1601),
                                   per_mii_immi(lv_ches_1602),
                                   per_mii_immi(lv_ches_1603),
                                   per_mii_immi(lv_ches_1604),
                                   per_mii_immi(lv_ches_1605),
                                   per_mii_immi(lv_ches_1606),
                                   per_mii_immi(lv_ches_1607),
                                   per_mii_immi(lv_ches_1610),
                                   per_mii_immi(lv_ches_1611),
                                   per_mii_immi(lv_ches_1612),
                                   per_mii_immi(lv_ches_2002),
                                   per_mii_immi(lv_ches_2004),
                                   per_mii_immi(lv_ches_2007),
                                   per_mii_immi(lv_ches_2010),
                                   per_mii_immi(lv_ches_2015),
                                   per_mii_immi(lv_ches_2016),
                                   per_mii_immi(lv_ches_2101),
                                   per_mii_immi(lv_ches_2103),
                                   per_mii_immi(lv_ches_2104),
                                   per_mii_immi(lv_ches_2109),
                                   per_mii_immi(lv_ches_2111),
                                   per_mii_immi(lv_ches_2113),
                                   per_mii_immi(lv_ches_2201),
                                   per_mii_immi(lv_ches_2202),
                                   per_mii_immi(lv_ches_2203),
                                   per_mii_immi(lv_ches_2204),
                                   per_mii_immi(lv_ches_2207),
                                   per_mii_immi(lv_ches_2301),
                                   per_mii_immi(lv_ches_2302),
                                   per_mii_immi(lv_ches_2308),
                                   per_mii_immi(lv_ches_2309),
                                   per_mii_immi(lv_ches_2310),
                                   per_mii_immi(lv_ches_2311),
                                   per_mii_immi(lv_ches_2402),
                                   per_mii_immi(lv_ches_2405),
                                   per_mii_immi(lv_ches_2406),
                                   per_mii_immi(lv_ches_2410),
                                   per_mii_immi(lv_ches_2412),
                                   per_mii_immi(lv_ches_2501),
                                   per_mii_immi(lv_ches_2506),
                                   per_mii_immi(lv_ches_2507),
                                   per_mii_immi(lv_ches_2511),
                                   per_mii_immi(lv_ches_2515),
                                   per_mii_immi(lv_ches_2516),
                                   per_mii_immi(lv_ches_2518),
                                   per_mii_immi(lv_ches_2601),
                                   per_mii_immi(lv_ches_2603),
                                   per_mii_immi(lv_ches_2605),
                                   per_mii_immi(lv_ches_2606),
                                   per_mii_immi(lv_ches_2613),
                                   per_mii_immi(lv_ches_2614),
                                   per_mii_immi(lv_ches_2616),
                                   per_mii_immi(lv_ches_2701),
                                   per_mii_immi(lv_ches_2702),
                                   per_mii_immi(lv_ches_2704),
                                   per_mii_immi(lv_ches_2705),
                                   per_mii_immi(lv_ches_2706),
                                   per_mii_immi(lv_ches_2710),
                                   per_mii_immi(lv_ches_2711),
                                   per_mii_immi(lv_ches_2802),
                                   per_mii_immi(lv_ches_2803),
                                   per_mii_immi(lv_ches_2804),
                                   per_mii_immi(lv_ches_2805),
                                   per_mii_immi(lv_ches_2809),
                                   per_mii_immi(lv_ches_2812),
                                   per_mii_immi(lv_ches_2813),
                                   per_mii_immi(lv_ches_2814),
                                   per_mii_immi(lv_ches_2815),
                                   per_mii_immi(lv_ches_2902),
                                   per_mii_immi(lv_ches_2903),
                                   per_mii_immi(lv_ches_2904),
                                   per_mii_immi(lv_ches_2905),
                                   per_mii_immi(lv_ches_2906),
                                   per_mii_immi(lv_ches_2914),
                                   per_mii_immi(lv_ches_3101),
                                   per_mii_immi(lv_ches_3102),
                                   per_mii_immi(lv_ches_3104),
                                   per_mii_immi(lv_ches_3105),
                                   per_mii_immi(lv_ches_3107),
                                   per_mii_immi(lv_ches_3109),
                                   per_mii_immi(lv_ches_3112),
                                   per_mii_immi(lv_ches_3114),
                                   per_mii_immi(lv_ches_3701),
                                   per_mii_immi(lv_ches_3702),
                                   per_mii_immi(lv_ches_3801),
                                   per_mii_immi(lv_ches_3802),
                                   per_mii_immi(lv_ches_3803),
                                   per_mii_immi(lv_ches_3804),
                                   per_mii_immi(lv_ches_3805),
                                   per_mii_immi(lv_ches_3806),
                                   per_mii_immi(lv_ches_4001),
                                   per_mii_immi(lv_ches_4003),
                                   per_mii_immi(lv_ches_4004),
                                   per_mii_immi(lv_ches_4005),
                                   per_mii_immi(lv_ches_4006),
                                   per_mii_immi(lv_ches_1301),
                                   per_mii_immi(lv_ches_1308),
                                   per_mii_immi(lv_ches_117),
                                   per_mii_immi(lv_ches_118),
                                   per_mii_immi(lv_ches_115),
                                   per_mii_immi(lv_ches_216),
                                   per_mii_immi(lv_ches_507),
                                   per_mii_immi(lv_ches_624),
                                   per_mii_immi(lv_ches_828),
                                   per_mii_immi(lv_ches_803),
                                   per_mii_immi(lv_ches_1006),
                                   per_mii_immi(lv_ches_1109))



party_vot_var_09$mii_immi_lv <- ifelse(is.nan(party_vot_var_09$mii_immi_lv) == TRUE, NA, party_vot_var_09$mii_immi_lv)



###same for EU
party_vot_var_09$mii_eu_lv <- c(per_mii_eu(lv_ches_102),
                                   per_mii_eu(lv_ches_103),
                                   per_mii_eu(lv_ches_104),
                                   per_mii_eu(lv_ches_105),
                                   per_mii_eu(lv_ches_106),
                                   per_mii_eu(lv_ches_107),
                                   per_mii_eu(lv_ches_108),
                                   per_mii_eu(lv_ches_109),
                                   per_mii_eu(lv_ches_110),
                                   per_mii_eu(lv_ches_112),
                                   per_mii_eu(lv_ches_119),
                                   per_mii_eu(lv_ches_201),
                                   per_mii_eu(lv_ches_202),
                                   per_mii_eu(lv_ches_203),
                                   per_mii_eu(lv_ches_206),
                                   per_mii_eu(lv_ches_211),
                                   per_mii_eu(lv_ches_215),
                                   per_mii_eu(lv_ches_218),
                                   per_mii_eu(lv_ches_301),
                                   per_mii_eu(lv_ches_302),
                                   per_mii_eu(lv_ches_303),
                                   per_mii_eu(lv_ches_304),
                                   per_mii_eu(lv_ches_306),
                                   per_mii_eu(lv_ches_310),
                                   per_mii_eu(lv_ches_311),
                                   per_mii_eu(lv_ches_401),
                                   per_mii_eu(lv_ches_402),
                                   per_mii_eu(lv_ches_403),
                                   per_mii_eu(lv_ches_404),
                                   per_mii_eu(lv_ches_410),
                                   per_mii_eu(lv_ches_413),
                                   per_mii_eu(lv_ches_414),
                                   per_mii_eu(lv_ches_415),
                                   per_mii_eu(lv_ches_501),
                                   per_mii_eu(lv_ches_502),
                                   per_mii_eu(lv_ches_504),
                                   per_mii_eu(lv_ches_505),
                                   per_mii_eu(lv_ches_506),
                                   per_mii_eu(lv_ches_511),
                                   per_mii_eu(lv_ches_513),
                                   per_mii_eu(lv_ches_517),
                                   per_mii_eu(lv_ches_523),
                                   per_mii_eu(lv_ches_525),
                                   per_mii_eu(lv_ches_601),
                                   per_mii_eu(lv_ches_602),
                                   per_mii_eu(lv_ches_605),
                                   per_mii_eu(lv_ches_609),
                                   per_mii_eu(lv_ches_610),
                                   per_mii_eu(lv_ches_613),
                                   per_mii_eu(lv_ches_701),
                                   per_mii_eu(lv_ches_702),
                                   per_mii_eu(lv_ches_703),
                                   per_mii_eu(lv_ches_705),
                                   per_mii_eu(lv_ches_707),
                                   per_mii_eu(lv_ches_708),
                                   per_mii_eu(lv_ches_811),
                                   per_mii_eu(lv_ches_814),
                                   per_mii_eu(lv_ches_815),
                                   per_mii_eu(lv_ches_827),
                                   per_mii_eu(lv_ches_837),
                                   per_mii_eu(lv_ches_838),
                                   per_mii_eu(lv_ches_844),
                                   per_mii_eu(lv_ches_845),
                                   per_mii_eu(lv_ches_848),
                                   per_mii_eu(lv_ches_1001),
                                   per_mii_eu(lv_ches_1002),
                                   per_mii_eu(lv_ches_1003),
                                   per_mii_eu(lv_ches_1004),
                                   per_mii_eu(lv_ches_1005),
                                   per_mii_eu(lv_ches_1014),
                                   per_mii_eu(lv_ches_1016),
                                   per_mii_eu(lv_ches_1017),
                                   per_mii_eu(lv_ches_1018),
                                   per_mii_eu(lv_ches_1101),
                                   per_mii_eu(lv_ches_1102),
                                   per_mii_eu(lv_ches_1104),
                                   per_mii_eu(lv_ches_1105),
                                   per_mii_eu(lv_ches_1106),
                                   per_mii_eu(lv_ches_1107),
                                   per_mii_eu(lv_ches_1108),
                                   per_mii_eu(lv_ches_1201),
                                   per_mii_eu(lv_ches_1202),
                                   per_mii_eu(lv_ches_1205),
                                   per_mii_eu(lv_ches_1206),
                                   per_mii_eu(lv_ches_1208),
                                   per_mii_eu(lv_ches_1209),
                                   per_mii_eu(lv_ches_1302),
                                   per_mii_eu(lv_ches_1303),
                                   per_mii_eu(lv_ches_1304),
                                   per_mii_eu(lv_ches_1307),
                                   per_mii_eu(lv_ches_1309),
                                   per_mii_eu(lv_ches_1401),
                                   per_mii_eu(lv_ches_1402),
                                   per_mii_eu(lv_ches_1403),
                                   per_mii_eu(lv_ches_1404),
                                   per_mii_eu(lv_ches_1405),
                                   per_mii_eu(lv_ches_1406),
                                   per_mii_eu(lv_ches_1408),
                                   per_mii_eu(lv_ches_1409),
                                   per_mii_eu(lv_ches_1601),
                                   per_mii_eu(lv_ches_1602),
                                   per_mii_eu(lv_ches_1603),
                                   per_mii_eu(lv_ches_1604),
                                   per_mii_eu(lv_ches_1605),
                                   per_mii_eu(lv_ches_1606),
                                   per_mii_eu(lv_ches_1607),
                                   per_mii_eu(lv_ches_1610),
                                   per_mii_eu(lv_ches_1611),
                                   per_mii_eu(lv_ches_1612),
                                   per_mii_eu(lv_ches_2002),
                                   per_mii_eu(lv_ches_2004),
                                   per_mii_eu(lv_ches_2007),
                                   per_mii_eu(lv_ches_2010),
                                   per_mii_eu(lv_ches_2015),
                                   per_mii_eu(lv_ches_2016),
                                   per_mii_eu(lv_ches_2101),
                                   per_mii_eu(lv_ches_2103),
                                   per_mii_eu(lv_ches_2104),
                                   per_mii_eu(lv_ches_2109),
                                   per_mii_eu(lv_ches_2111),
                                   per_mii_eu(lv_ches_2113),
                                   per_mii_eu(lv_ches_2201),
                                   per_mii_eu(lv_ches_2202),
                                   per_mii_eu(lv_ches_2203),
                                   per_mii_eu(lv_ches_2204),
                                   per_mii_eu(lv_ches_2207),
                                   per_mii_eu(lv_ches_2301),
                                   per_mii_eu(lv_ches_2302),
                                   per_mii_eu(lv_ches_2308),
                                   per_mii_eu(lv_ches_2309),
                                   per_mii_eu(lv_ches_2310),
                                   per_mii_eu(lv_ches_2311),
                                   per_mii_eu(lv_ches_2402),
                                   per_mii_eu(lv_ches_2405),
                                   per_mii_eu(lv_ches_2406),
                                   per_mii_eu(lv_ches_2410),
                                   per_mii_eu(lv_ches_2412),
                                   per_mii_eu(lv_ches_2501),
                                   per_mii_eu(lv_ches_2506),
                                   per_mii_eu(lv_ches_2507),
                                   per_mii_eu(lv_ches_2511),
                                   per_mii_eu(lv_ches_2515),
                                   per_mii_eu(lv_ches_2516),
                                   per_mii_eu(lv_ches_2518),
                                   per_mii_eu(lv_ches_2601),
                                   per_mii_eu(lv_ches_2603),
                                   per_mii_eu(lv_ches_2605),
                                   per_mii_eu(lv_ches_2606),
                                   per_mii_eu(lv_ches_2613),
                                   per_mii_eu(lv_ches_2614),
                                   per_mii_eu(lv_ches_2616),
                                   per_mii_eu(lv_ches_2701),
                                   per_mii_eu(lv_ches_2702),
                                   per_mii_eu(lv_ches_2704),
                                   per_mii_eu(lv_ches_2705),
                                   per_mii_eu(lv_ches_2706),
                                   per_mii_eu(lv_ches_2710),
                                   per_mii_eu(lv_ches_2711),
                                   per_mii_eu(lv_ches_2802),
                                   per_mii_eu(lv_ches_2803),
                                   per_mii_eu(lv_ches_2804),
                                   per_mii_eu(lv_ches_2805),
                                   per_mii_eu(lv_ches_2809),
                                   per_mii_eu(lv_ches_2812),
                                   per_mii_eu(lv_ches_2813),
                                   per_mii_eu(lv_ches_2814),
                                   per_mii_eu(lv_ches_2815),
                                   per_mii_eu(lv_ches_2902),
                                   per_mii_eu(lv_ches_2903),
                                   per_mii_eu(lv_ches_2904),
                                   per_mii_eu(lv_ches_2905),
                                   per_mii_eu(lv_ches_2906),
                                   per_mii_eu(lv_ches_2914),
                                   per_mii_eu(lv_ches_3101),
                                   per_mii_eu(lv_ches_3102),
                                   per_mii_eu(lv_ches_3104),
                                   per_mii_eu(lv_ches_3105),
                                   per_mii_eu(lv_ches_3107),
                                   per_mii_eu(lv_ches_3109),
                                   per_mii_eu(lv_ches_3112),
                                   per_mii_eu(lv_ches_3114),
                                   per_mii_eu(lv_ches_3701),
                                   per_mii_eu(lv_ches_3702),
                                   per_mii_eu(lv_ches_3801),
                                   per_mii_eu(lv_ches_3802),
                                   per_mii_eu(lv_ches_3803),
                                   per_mii_eu(lv_ches_3804),
                                   per_mii_eu(lv_ches_3805),
                                   per_mii_eu(lv_ches_3806),
                                   per_mii_eu(lv_ches_4001),
                                   per_mii_eu(lv_ches_4003),
                                   per_mii_eu(lv_ches_4004),
                                   per_mii_eu(lv_ches_4005),
                                   per_mii_eu(lv_ches_4006),
                                   per_mii_eu(lv_ches_1301),
                                   per_mii_eu(lv_ches_1308),
                                   per_mii_eu(lv_ches_117),
                                   per_mii_eu(lv_ches_118),
                                   per_mii_eu(lv_ches_115),
                                   per_mii_eu(lv_ches_216),
                                   per_mii_eu(lv_ches_507),
                                   per_mii_eu(lv_ches_624),
                                   per_mii_eu(lv_ches_828),
                                   per_mii_eu(lv_ches_803),
                                   per_mii_eu(lv_ches_1006),
                                   per_mii_eu(lv_ches_1109))



party_vot_var_09$mii_eu_lv <- ifelse(is.nan(party_vot_var_09$mii_eu_lv) == TRUE, NA, party_vot_var_09$mii_eu_lv)







########Merging party_vot_var and party_vot_var_09 (EES 12 and 09 party information)#########
#before merging, manually repeat the overlap variables. Somehow those don't work otherwise

party_vot_var_14_09 <- full_join(party_vot_var, party_vot_var_09)

#remove all rows with NA values (those correspond to Eastern European countries)
party_vot_var_14_09 <- party_vot_var_14_09[complete.cases(party_vot_var_14_09[ , 3:4]),]

all_unique_parties <- unique(party_vot_var_14_09$party_ches) #save all unique CHES parties I have data for. 




























