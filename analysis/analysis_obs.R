##################################3
#   Project: MPhil thesis         #
#   Name: ...                     #
#   Description: analysis for the #
#     obervational data chapter   #
##################################3

#### - packages ####
library(stargazer)
library(sjPlot)
library(lme4)
library(ggiraph)
library(ggiraphExtra)
library(plyr)
library(ggplot2)
library(ggeffects)
library(Hmisc)
library(cowplot)#clashes with gg plot and sjplot on some commands. Better at plotting grids
library(foreign) 
library(plm)
library(lmtest)
library(pcse)
library(clusterSEs)
library(margins)
library(texreg)

#### - loading data ####
setwd()
master_file <- readRDS("master_file_v2.rds") #note i'm taking v2 now
attach(master_file)


#### - Basic models ####
#blurring
bl_immi <- lm(immigration_dimension_sd ~ perext_immi_lv + sd_immi_lv, na.action = na.omit)
bl_eu <- lm(eu_dimension_sd ~ perext_eu_lv + sd_eu_lv, na.action = na.omit)

#ignoring
ig_immi <- lm(cmp_immi_salience ~ perext_immi_lv + sd_immi_lv, na.action = na.omit)
ig_eu <- lm(cmp_eu_salience ~ perext_eu_lv + sd_eu_lv, na.action = na.omit)

#entrepreneurship
en_immi <- lm(issue_entrep_cmpsal_immi~perext_immi_lv+sd_immi_lv, na.action = na.omit)
en_eu <- lm(issue_entrep_cmpsal_eu~perext_eu_lv+sd_eu_lv, na.action = na.omit)


##ith alternative specification for electoral potential
#blurring
bl_immi_di <- lm(immigration_dimension_sd~mad_immi_lv_countrymean+mii_immi_lv+sd_immi_lv, na.action = na.omit)
bl_eu_di <- lm(eu_dimension_sd~mad_eu_lv_countrymean+mii_eu_lv+sd_eu_lv, na.action = na.omit)

#ignoring
ig_immi_di <- lm(cmp_immi_salience~mad_immi_lv_countrymean+mii_immi_lv+sd_immi_lv, na.action = na.omit)
ig_eu_di <- lm(cmp_eu_salience~mad_eu_lv_countrymean+mii_eu_lv+sd_eu_lv, na.action = na.omit)

#entrepreneurship
en_immi_di <- lm(issue_entrep_cmpsal_immi~mad_immi_lv_countrymean+mii_immi_lv+sd_immi_lv, na.action = na.omit)
en_eu_di <- lm(issue_entrep_cmpsal_eu~mad_eu_lv_countrymean+mii_eu_lv+sd_eu_lv, na.action = na.omit)




#### - basic models with fixed effects ####
attach(master_file)
#blurring
bl_immi_fe <- lm(immigration_dimension_sd~perext_immi_lv+sd_immi_lv + seat + govt + country_name +as.factor(year), na.action = na.omit)
bl_eu_fe <- lm(eu_dimension_sd~perext_eu_lv+ sd_eu_lv+ seat + govt + country_name +as.factor(year), na.action = na.omit)
bl_glob_fe <- lm(globalization_sd~perext_glob_lv+sd_glob_lv + seat + govt + country_name +as.factor(year), na.action = na.omit)

#ignoring
ig_immi_fe <- lm(cmp_immi_salience~perext_immi_lv+sd_immi_lv + seat + govt + country_name +as.factor(year), na.action = na.omit)
ig_eu_fe <- lm(cmp_eu_salience~perext_eu_lv+sd_eu_lv + seat + govt + country_name +as.factor(year), na.action = na.omit)
ig_glob_fe <- lm(cmp_glob_salience~perext_glob_lv+sd_glob_lv + seat + govt + country_name +as.factor(year), na.action = na.omit)

#entrepreneurship
en_immi_fe <- lm(issue_entrep_cmpsal_immi~perext_immi_lv + sd_immi_lv + seat + govt + country_name +as.factor(year), na.action = na.omit)
en_eu_fe <- lm(issue_entrep_cmpsal_eu~perext_eu_lv + sd_eu_lv + seat + govt + country_name +as.factor(year), na.action = na.omit)
en_glob_fe <- lm(issue_entrep_cmpsal_glob~perext_glob_lv+sd_glob_lv+ seat + govt + country_name +as.factor(year), na.action = na.omit)





#### - Marginal effects plots of LSDV models ####
##run LSDV models without clustred SE because otherwise margins package doesn't recognize the type
bl_immi_fe <- lm(immigration_dimension_sd~perext_immi_lv+sd_immi_lv + seat + govt + vot_overl_cor + country_name +as.factor(year), na.action = na.omit)
bl_eu_fe <- lm(eu_dimension_sd~perext_eu_lv+ sd_eu_lv+ seat + govt + vot_overl_cor + country_name +as.factor(year), na.action = na.omit)

ig_immi_fe <-lm(cmp_immi_salience~perext_immi_lv+sd_immi_lv + seat + govt + vot_overl_cor + country_name + as.factor(year), na.action = na.omit)
ig_eu_fe <- lm(cmp_eu_salience~perext_eu_lv+sd_eu_lv + seat + govt + vot_overl_cor + country_name + as.factor(year), na.action = na.omit)

en_immi_fe <- lm(issue_entrep_cmpsal_immi~perext_immi_lv+sd_immi_lv + seat + govt + vot_overl_cor + country_name +as.factor(year), na.action = na.omit)
en_eu_fe <- lm(issue_entrep_cmpsal_eu~perext_eu_lv+sd_eu_lv + seat + govt + vot_overl_cor +country_name +as.factor(year), na.action = na.omit)

summary(bl_immi_fe)
bl_eu_fe
ig_immi_fe
ig_eu_fe
en_immi_fe
en_eu_fe





#### - LSDV models ####
#models plus coeftest for clustered SEs. 
bl_immi_fe <- lm(immigration_dimension_sd~perext_immi_lv+sd_immi_lv + seat + govt + vot_overl_cor + country_name +as.factor(year), na.action = na.omit)
bl_immi_fe <- coeftest(bl_immi_fe, vcov=vcovHC(bl_immi_fe,type="HC0",cluster="group"))
bl_eu_fe <- lm(eu_dimension_sd~perext_eu_lv+ sd_eu_lv+ seat + govt + vot_overl_cor + country_name +as.factor(year), na.action = na.omit)
bl_eu_fe <- coeftest(bl_eu_fe, vcov=vcovHC(bl_eu_fe,type="HC0",cluster="group"))

ig_immi_fe <-lm(cmp_immi_salience~perext_immi_lv+sd_immi_lv + seat + govt + vot_overl_cor + country_name + as.factor(year), na.action = na.omit)
ig_immi_fe <- coeftest(ig_immi_fe, vcov=vcovHC(ig_immi_fe,type="HC0",cluster="group"))
ig_eu_fe <- lm(cmp_eu_salience~perext_eu_lv+sd_eu_lv + seat + govt + vot_overl_cor + country_name + as.factor(year), na.action = na.omit)
ig_eu_fe <- coeftest(ig_eu_fe, vcov=vcovHC(ig_eu_fe,type="HC0",cluster="group"))

en_immi_fe <- lm(issue_entrep_cmpsal_immi~perext_immi_lv+sd_immi_lv + seat + govt + vot_overl_cor + country_name +as.factor(year), na.action = na.omit)
en_immi_fe <- coeftest(en_immi_fe, vcov=vcovHC(en_immi_fe,type="HC0",cluster="group"))
en_eu_fe <- lm(issue_entrep_cmpsal_eu~perext_eu_lv+sd_eu_lv + seat + govt + vot_overl_cor +country_name +as.factor(year), na.action = na.omit)
en_eu_fe <- coeftest(en_eu_fe, vcov=vcovHC(en_eu_fe,type="HC0",cluster="group"))

bl_immi_fe
bl_eu_fe
ig_immi_fe
ig_eu_fe
en_immi_fe
en_eu_fe







#### - test how correlated electoral potential is with MII.####
cor(master_file$perext_immi_lv, master_file$mii_immi_lv,  method = c("pearson"))
cor(master_file$perext_eu_lv, master_file$mii_eu_lv,  method = c("pearson"))

cor(master_file$perext_immi_lv, master_file$mad_immi_lv_countrymean,  method = c("pearson"))
cor(master_file$perext_eu_lv, master_file$mad_eu_lv_countrymean,  method = c("pearson"))






#### - LSDV models with direct distance and salience measures ####
bl_immi_fe_di <- lm(immigration_dimension_sd~mad_immi_lv_countrymean+mii_immi_lv+sd_immi_lv + seat + govt + vot_overl_cor + country_name + as.factor(year), na.action = na.omit)
bl_immi_fe_di <- coeftest(bl_immi_fe_di, vcov=vcovHC(bl_immi_fe_di,type="HC0",cluster="group"))
bl_eu_fe_di <- lm(eu_dimension_sd~mad_eu_lv_countrymean+mii_eu_lv+ sd_eu_lv+ seat + govt + vot_overl_cor + country_name +as.factor(year), na.action = na.omit)
bl_eu_fe_di <- coeftest(bl_eu_fe_di, vcov=vcovHC(bl_eu_fe_di,type="HC0",cluster="group"))

bl_immi_fe_di
bl_eu_fe_di


ig_immi_fe_di <-lm(cmp_immi_salience~mad_immi_lv_countrymean+mii_immi_lv+sd_immi_lv + seat + govt + vot_overl_cor + country_name + as.factor(year), na.action = na.omit)
ig_immi_fe_di <- coeftest(ig_immi_fe_di, vcov=vcovHC(ig_immi_fe_di,type="HC0",cluster="group"))
ig_eu_fe_di <- lm(cmp_eu_salience~mad_eu_lv_countrymean+mii_eu_lv+sd_eu_lv + seat + govt + vot_overl_cor + country_name + as.factor(year), na.action = na.omit)
ig_eu_fe_di <- coeftest(ig_eu_fe_di, vcov=vcovHC(ig_eu_fe_di,type="HC0",cluster="group"))

ig_immi_fe_di
ig_eu_fe_di


en_immi_fe_di <- lm(issue_entrep_cmpsal_immi~mad_immi_lv_countrymean+mii_immi_lv+sd_immi_lv + seat + govt + vot_overl_cor + country_name +as.factor(year), na.action = na.omit)
en_immi_fe_di <- coeftest(en_immi_fe_di, vcov=vcovHC(en_immi_fe_di,type="HC0",cluster="group"))
en_eu_fe_di <- lm(issue_entrep_cmpsal_eu~mad_eu_lv_countrymean+mii_eu_lv+sd_eu_lv + seat + govt + vot_overl_cor +country_name +as.factor(year), na.action = na.omit)
en_eu_fe_di <- coeftest(en_eu_fe_di, vcov=vcovHC(en_eu_fe_di,type="HC0",cluster="group"))




#### - test whether CHES salience measure for entrepreneurship gives the same results (presented in Appendix). Note: no year factor in immigration due to missing data
en_immi_fe_ches <- lm(issue_entrep_chessal_immi~perext_immi_lv+sd_immi_lv + seat + govt + vot_overl_cor + country_name, na.action = na.omit)
en_immi_fe_ches <- coeftest(en_immi_fe_ches, vcov=vcovHC(en_immi_fe_ches,type="HC0",cluster="group"))
en_eu_fe_ches <- lm(issue_entrep_chessal_eu~perext_eu_lv+sd_eu_lv + seat + govt + vot_overl_cor +country_name +as.factor(year), na.action = na.omit)
en_eu_fe_ches <- coeftest(en_eu_fe_ches, vcov=vcovHC(en_eu_fe_ches,type="HC0",cluster="group"))
en_immi_fe_di_ches <- lm(issue_entrep_chessal_immi~mad_immi_lv_countrymean+mii_immi_lv+sd_immi_lv + seat + govt + vot_overl_cor + country_name, na.action = na.omit)
en_immi_fe_di_ches <- coeftest(en_immi_fe_di_ches, vcov=vcovHC(en_immi_fe_di_ches,type="HC0",cluster="group"))
en_eu_fe_di_ches <- lm(issue_entrep_chessal_eu~mad_eu_lv_countrymean+mii_eu_lv+sd_eu_lv + seat + govt + vot_overl_cor +country_name +as.factor(year), na.action = na.omit)
en_eu_fe_di_ches <- coeftest(en_eu_fe_di_ches, vcov=vcovHC(en_eu_fe_di_ches,type="HC0",cluster="group"))


en_eu_fe_ches
en_immi_fe_ches
en_immi_fe_di_ches
en_eu_fe_di_ches






#### - Running tests for panel data ####
fixed <- plm(formula = cmp_immi_salience ~ perext_immi_lv + sd_immi_lv, data = master_file, model = "within", index = c("country"))
fixed.time <- plm(formula = cmp_immi_salience ~ perext_immi_lv + sd_immi_lv + as.factor(year), data = master_file, model = "within", index = c("country"))
random <- plm(formula = cmp_immi_salience ~ perext_immi_lv + sd_immi_lv, data = master_file, model = "random", index = c("country"))
phtest(fixed, random) #test for fixed or random effects. Needs to be below 0.05 for fixed. See princeton slides
pFtest(fixed.time, fixed) #test for time effect needed. 
pbgtest(fixed)
library(lmtest)
bptest(cmp_immi_salience ~ perext_immi_lv + sd_immi_lv + as.factor(country), data = master_file, studentize=F)

summary(lm(immigration_dimension_sd~perext_immi_lv))




#### - truely fixed effects models with party dummies ####
attach(master_file)
bl_immi_fepd <- lm(immigration_dimension_sd~perext_immi_lv+sd_immi_lv + seat + govt + vot_overl_cor + country_name +as.factor(year) + as.factor(party), na.action = na.omit)
bl_immi_fepd <- coeftest(bl_immi_fepd, vcov=vcovHC(bl_immi_fepd,type="HC0",cluster="group"))
bl_eu_fepd <- lm(eu_dimension_sd~perext_eu_lv+ sd_eu_lv + seat + govt + vot_overl_cor + country_name +as.factor(year) + as.factor(party), na.action = na.omit)
bl_eu_fepd <- coeftest(bl_eu_fepd, vcov=vcovHC(bl_eu_fepd,type="HC0",cluster="group"))

ig_immi_fepd <-lm(cmp_immi_salience~perext_immi_lv+sd_immi_lv  + seat + govt + vot_overl_cor + country_name + as.factor(year) + as.factor(party), na.action = na.omit)
ig_immi_fepd <- coeftest(ig_immi_fepd, vcov=vcovHC(ig_immi_fepd,type="HC0",cluster="group"))
ig_eu_fepd <- lm(cmp_eu_salience~perext_eu_lv+sd_eu_lv  + seat + govt + vot_overl_cor + country_name + as.factor(year) + as.factor(party), na.action = na.omit)
ig_eu_fepd <- coeftest(ig_eu_fepd, vcov=vcovHC(ig_eu_fepd,type="HC0",cluster="group"))

en_immi_fepd <- lm(issue_entrep_cmpsal_immi~perext_immi_lv+sd_immi_lv  + seat + govt + vot_overl_cor + country_name +as.factor(year) + as.factor(party), na.action = na.omit)
en_immi_fepd <- coeftest(en_immi_fepd, vcov=vcovHC(en_immi_fepd,type="HC0",cluster="group"))
en_eu_fepd <- lm(issue_entrep_cmpsal_eu~perext_eu_lv+sd_eu_lv  + seat + govt + vot_overl_cor +country_name +as.factor(year) + as.factor(party), na.action = na.omit)
en_eu_fepd <- coeftest(en_eu_fepd, vcov=vcovHC(en_eu_fepd,type="HC0",cluster="group"))


bl_immi_fepd 
bl_eu_fepd 
ig_immi_fepd 
ig_eu_fepd 
en_immi_fepd 
en_eu_fepd 

summary(bl_immi_fepd)
summary(bl_eu_fepd)
summary(ig_immi_fepd)
summary(ig_eu_fepd)
summary(en_immi_fepd)
summary(en_eu_fepd)



#### - truely fixed effects models with different specifications of electoral potential. #### 
ig_immi_fepd_ep <-lm(cmp_immi_salience~mad_immi_lv_countrymean+mii_immi_lv+sd_immi_lv  + seat + govt + vot_overl_cor + country_name + as.factor(year) + as.factor(party), na.action = na.omit)
ig_immi_fepd_ep <- coeftest(ig_immi_fepd_ep, vcov=vcovHC(ig_immi_fepd_ep,type="HC0",cluster="group"))
ig_eu_fepd_ep <- lm(cmp_eu_salience~mad_eu_lv_countrymean+mii_eu_lv+sd_eu_lv  + seat + govt + vot_overl_cor + country_name + as.factor(year) + as.factor(party), na.action = na.omit)
ig_eu_fepd_ep <- coeftest(ig_eu_fepd_ep, vcov=vcovHC(ig_eu_fepd_ep,type="HC0",cluster="group"))

bl_immi_fepd_ep <-lm(immigration_dimension_sd~mad_immi_lv_countrymean+mii_immi_lv+sd_immi_lv  + seat + govt + vot_overl_cor + country_name + as.factor(year) + as.factor(party), na.action = na.omit)
bl_immi_fepd_ep <- coeftest(bl_immi_fepd_ep, vcov=vcovHC(bl_immi_fepd_ep,type="HC0",cluster="group"))
bl_eu_fepd_ep <- lm(eu_dimension_sd~mad_eu_lv_countrymean+mii_eu_lv+sd_eu_lv  + seat + govt + vot_overl_cor + country_name + as.factor(year) + as.factor(party), na.action = na.omit)
bl_eu_fepd_ep <- coeftest(bl_eu_fepd_ep, vcov=vcovHC(bl_eu_fepd_ep,type="HC0",cluster="group"))

en_immi_fepd_ep <- lm(issue_entrep_cmpsal_immi~mad_immi_lv_countrymean+mii_immi_lv+sd_immi_lv  + seat + govt + vot_overl_cor + country_name +as.factor(year) + as.factor(party), na.action = na.omit)
en_immi_fepd_ep <- coeftest(en_immi_fepd_ep, vcov=vcovHC(en_immi_fepd_ep,type="HC0",cluster="group"))
en_eu_fepd_ep <- lm(issue_entrep_cmpsal_eu~mad_eu_lv_countrymean+mii_eu_lv+sd_eu_lv  + seat + govt + vot_overl_cor +country_name +as.factor(year) + as.factor(party), na.action = na.omit)
en_eu_fepd_ep <- coeftest(en_eu_fepd_ep, vcov=vcovHC(en_eu_fepd_ep,type="HC0",cluster="group"))


ig_immi_fepd_ep
ig_eu_fepd_ep
bl_immi_fepd_ep
bl_eu_fepd_ep
en_immi_fepd_ep
en_eu_fepd_ep







#### - Multilevel models ####
#UK and France are automatically removed from these models because they have an NA on electoral threshold.
#due to different electoral system. 

#centring variables.
master_file_mean <- master_file #creating a new master file where variables are centered. 
attach(master_file_mean)
#dependent variables first
master_file_mean$immigration_dimension_sd <- immigration_dimension_sd-mean(immigration_dimension_sd, na.rm = TRUE)
master_file_mean$eu_dimension_sd <- eu_dimension_sd-mean(eu_dimension_sd, na.rm = TRUE)
master_file_mean$cmp_immi_salience <- cmp_immi_salience-mean(cmp_immi_salience, na.rm = TRUE)
master_file_mean$cmp_eu_salience <- cmp_eu_salience-mean(cmp_eu_salience, na.rm = TRUE)
master_file_mean$issue_entrep_cmpsal_immi <- issue_entrep_cmpsal_immi-mean(issue_entrep_cmpsal_immi, na.rm = TRUE)
master_file_mean$issue_entrep_cmpsal_eu <- issue_entrep_cmpsal_eu-mean(issue_entrep_cmpsal_eu, na.rm = TRUE)

#now control variables (except dummies)
master_file_mean$seat <- seat-mean(seat, na.rm = TRUE)
master_file_mean$vot_overl_cor <- vot_overl_cor-mean(vot_overl_cor, na.rm = TRUE)
master_file_mean$elec_thres <- elec_thres-mean(elec_thres, na.rm = TRUE)

#Basic Multilevel models
library(lme4)
bl_immi_ml <- lmer(immigration_dimension_sd~perext_immi_lv + sd_immi_lv  + seat + govt + vot_overl_cor + elec_thres + as.factor(year) + (1|country), data = master_file_mean)
bl_eu_ml <- lmer(eu_dimension_sd~perext_eu_lv + sd_eu_lv  + seat + govt + vot_overl_cor + elec_thres + as.factor(year) + (1|country), data = master_file_mean)
ig_immi_ml <- lmer(cmp_immi_salience~perext_immi_lv + sd_immi_lv  + seat + govt + vot_overl_cor + elec_thres + as.factor(year) + (1|country), data = master_file_mean)
ig_eu_ml <- lmer(cmp_eu_salience~perext_eu_lv + sd_eu_lv  + seat + govt + vot_overl_cor + elec_thres + as.factor(year) + (1|country), data = master_file_mean)
en_immi_ml <- lmer(issue_entrep_cmpsal_immi~perext_immi_lv + sd_immi_lv  + seat + govt + vot_overl_cor + elec_thres + as.factor(year)  + (1|country), data = master_file_mean)
en_eu_ml <- lmer(issue_entrep_cmpsal_eu~perext_eu_lv + sd_eu_lv  + seat + govt + vot_overl_cor + elec_thres + as.factor(year) + (1|country), data = master_file_mean)




summary(bl_immi_ml)
summary(bl_eu_ml)
summary(ig_immi_ml)
summary(ig_eu_ml)
summary(en_immi_ml)
summary(en_eu_ml)


#multilevel model with bayes estimator for in the paper. Better because of level 2 N of 14. 
library(rstan)
library(brms)
library(nmle)
library(lme4)


#without random slopes
bl_immi_ml_by <- brm(immigration_dimension_sd~perext_immi_lv + sd_immi_lv +  seat + govt + vot_overl_cor + elec_thres + as.factor(year) + (1|country), data = master_file_mean)
bl_eu_ml_by <- brm(eu_dimension_sd~perext_eu_lv + sd_eu_lv  + seat + govt + vot_overl_cor + elec_thres + as.factor(year)   + (1|country), data = master_file_mean)
ig_immi_ml_by <- brm(cmp_immi_salience~perext_immi_lv + sd_immi_lv  + seat + govt + vot_overl_cor + elec_thres + as.factor(year)   + (1|country), data = master_file_mean)
ig_eu_ml_by <- brm(cmp_eu_salience~perext_eu_lv + sd_eu_lv + seat + govt + vot_overl_cor + elec_thres + as.factor(year)   + (1|country), data = master_file_mean)
en_immi_ml_by <- brm(issue_entrep_cmpsal_immi~perext_immi_lv + sd_immi_lv  + seat + govt + vot_overl_cor + elec_thres + as.factor(year)   + (1|country), data = master_file_mean)
en_eu_ml_by <- brm(issue_entrep_cmpsal_eu~perext_eu_lv + sd_eu_lv + seat + govt + vot_overl_cor + elec_thres + as.factor(year) + (1|country), data = master_file_mean)


summary(bl_immi_ml_by)
summary(bl_eu_ml_by)
summary(ig_immi_ml_by)
summary(ig_eu_ml_by)
summary(en_immi_ml_by)
summary(en_eu_ml_by)




#with random slopes. Can do with and without country level elec thres
bl_immi_ml_by_rs <- brm(immigration_dimension_sd~perext_immi_lv + sd_immi_lv +  seat + govt + vot_overl_cor   + as.factor(year) + (elec_thres|country), data = master_file_mean)
bl_eu_ml_by_rs <- brm(eu_dimension_sd~perext_eu_lv + sd_eu_lv  + seat + govt + vot_overl_cor   + as.factor(year) + (elec_thres|country), data = master_file_mean)
ig_immi_ml_by_rs <- brm(cmp_immi_salience~perext_immi_lv + sd_immi_lv  + seat + govt + vot_overl_cor   + as.factor(year)   + (elec_thres|country), data = master_file_mean)
ig_eu_ml_by_rs <- brm(cmp_eu_salience~perext_eu_lv + sd_eu_lv + seat + govt + vot_overl_cor   + as.factor(year)  + (elec_thres|country), data = master_file_mean)
en_immi_ml_by_rs <- brm(issue_entrep_cmpsal_immi~perext_immi_lv + sd_immi_lv  + seat + govt + vot_overl_cor   +  as.factor(year)   + (elec_thres|country), data = master_file_mean)
en_eu_ml_by_rs <- brm(issue_entrep_cmpsal_eu~perext_eu_lv + sd_eu_lv + seat + govt + vot_overl_cor   + as.factor(year) + (elec_thres|country), data = master_file_mean)


summary(bl_immi_ml_by_rs)
summary(bl_eu_ml_by_rs)
summary(ig_immi_ml_by_rs)
summary(ig_eu_ml_by_rs)
summary(en_immi_ml_by_rs)
summary(en_eu_ml_by_rs)
















#### - Checking Likely voter assumption ####

#putting the mean of the standard deviation in a dataframe.
likely_vot_ass <- data.frame("sd_lv_ass" = c(mean(master_file$sd_lv_lefrig_ass, na.rm = TRUE), 
                                             mean(master_file$sd_non_lv_lefrig_ass, na.rm = TRUE),
                                             mean(master_file$sd_lv_eu_ass, na.rm = TRUE),
                                             mean(master_file$sd_non_lv_eu_ass, na.rm = TRUE)),
                             "ass" = c("lr lv ", "lr non lv", "eu lv", "eu non lv"), 
                             "eu_lr" = c("Left Right", "Left Right", "EU", "EU" ))

#turning it into a plot
ggplot(data=likely_vot_ass, aes(y = likely_vot_ass$sd_lv_ass, x = likely_vot_ass$ass, fill = likely_vot_ass$eu_lr)) + 
  geom_col()  + coord_cartesian(ylim=c(1.75, 2.75)) +
  ylab("Mean SD of position assessment") + labs(fill = "Dimension") +
  scale_x_discrete(name = "Voter groups", labels=c("likely voters", "non-lv", "llikely voters", "non-lv")) +
  scale_fill_grey() + theme_bw()
  







#### - Stargazer: normal models ####
#####normal models
##blurring all parties
stargazer(bl_immi, bl_eu, bl_immi_fe, bl_eu_fe,  
          title="Explaining Party Blurring", align=TRUE,
          font.size = "footnotesize", column.sep.width = "10pt",
          keep.stat=c("n"), covariate.labels=c("Electoral Potential", "Dividing Potential", "", "", 
                                               "Seat Share", "Government Party", "Competition Exposure", "2014"),
          out.header = FALSE, dep.var.caption=c(""), 
          dep.var.labels=c("1: Immi", "2: EU", "3: Immi", "4: EU"),
          omit=c("country_nameUK", "country_nameSweden", "country_nameSpain", "country_namePortugal",
                 "country_nameNetherlands", "country_nameLuxembourg", "country_nameItaly",
                 "country_nameIreland", "country_nameGermany", "country_nameFrance", "country_nameFinland", 
                 "country_nameDenmark", "country_nameBelgium"), 
          add.lines = list(c("Country FE", "", "", "X", "X")),
          model.numbers=FALSE)

##Ign all parties
stargazer(ig_immi, ig_eu, ig_immi_fe, ig_eu_fe,  
          title="Explaining Party Ignoring", align=TRUE,
          font.size = "footnotesize", column.sep.width = "10pt",
          keep.stat=c("n"), covariate.labels=c("Electoral Potential", "Dividing Potential", "", "", 
                                               "Seat Share", "Government Party", "Competition Exposure", "2014"),
          out.header = FALSE, dep.var.caption=c(""), 
          dep.var.labels=c("1: Immi", "2: EU", "3: Immi", "4: EU"),
          omit=c("country_nameUK", "country_nameSweden", "country_nameSpain", "country_namePortugal",
                 "country_nameNetherlands", "country_nameLuxembourg", "country_nameItaly",
                 "country_nameIreland", "country_nameGermany", "country_nameFrance", "country_nameFinland", 
                 "country_nameDenmark", "country_nameBelgium"), 
          add.lines = list(c("Country FE", "", "", "X", "X")),
          model.numbers=FALSE)

##Entrepreneurship all parties
stargazer(en_immi, en_eu, en_immi_fe, en_eu_fe,  
          title="Explaining Party Entrepreneurship", align=TRUE,
          font.size = "footnotesize", column.sep.width = "10pt",
          keep.stat=c("n"), covariate.labels=c("Electoral Potential", "Dividing Potential", "", "", 
                                               "Seat Share", "Government Party", "Competition Exposure", "2014"),
          out.header = FALSE, dep.var.caption=c(""), 
          dep.var.labels=c("1: Immi", "2: EU", "3: Immi", "4: EU"),
          omit=c("country_nameUK", "country_nameSweden", "country_nameSpain", "country_namePortugal",
                 "country_nameNetherlands", "country_nameLuxembourg", "country_nameItaly",
                 "country_nameIreland", "country_nameGermany", "country_nameFrance", "country_nameFinland", 
                 "country_nameDenmark", "country_nameBelgium"), 
          add.lines = list(c("Country FE", "", "", "X", "X")),
          model.numbers=FALSE)



#### - Stargazer: Alternative specifications for electoral potential. ####
##blurring all parties
stargazer(bl_immi_di, bl_eu_di, bl_immi_fe_di, bl_eu_fe_di,  
          title="Alternative specification electoral potential: blurring", align=TRUE,
          font.size = "footnotesize", column.sep.width = "10pt",
          keep.stat=c("n"), covariate.labels=c("MAD country mean", "MII", "Dividing Potential", "", "", "", 
                                               "Seat Share", "Government Party", "Competition Exposure", "2014"),
          out.header = FALSE, dep.var.caption=c(""), 
          dep.var.labels=c("1: Immi", "2: EU", "3: Immi", "4: EU"),
          omit=c("country_nameUK", "country_nameSweden", "country_nameSpain", "country_namePortugal",
                 "country_nameNetherlands", "country_nameLuxembourg", "country_nameItaly",
                 "country_nameIreland", "country_nameGermany", "country_nameFrance", "country_nameFinland", 
                 "country_nameDenmark", "country_nameBelgium"), 
          add.lines = list(c("Country FE", "", "", "X", "X")),
          model.numbers=FALSE)

##Ign all parties
stargazer(ig_immi_di, ig_eu_di, ig_immi_fe_di, ig_eu_fe_di,  
          title="Alternative specification electoral potential: positioning", align=TRUE,
          font.size = "footnotesize", column.sep.width = "10pt",
          keep.stat=c("n"), covariate.labels=c("MAD country mean", "MII", "Dividing Potential", "", "", "", 
                                               "Seat Share", "Government Party", "Competition Exposure", "2014"),
          out.header = FALSE, dep.var.caption=c(""), 
          dep.var.labels=c("1: Immi", "2: EU", "3: Immi", "4: EU"),
          omit=c("country_nameUK", "country_nameSweden", "country_nameSpain", "country_namePortugal",
                 "country_nameNetherlands", "country_nameLuxembourg", "country_nameItaly",
                 "country_nameIreland", "country_nameGermany", "country_nameFrance", "country_nameFinland", 
                 "country_nameDenmark", "country_nameBelgium"), 
          add.lines = list(c("Country FE", "", "", "X", "X")),
          model.numbers=FALSE)

##Entrepreneurship all parties
stargazer(en_immi_di, en_eu_di, en_immi_fe_di, en_eu_fe_di,  
          title="Alternative specification electoral potential: entrepreneurship", align=TRUE,
          font.size = "footnotesize", column.sep.width = "10pt",
          keep.stat=c("n"), covariate.labels=c("MAD country mean", "MII", "Dividing Potential", "", "", "", 
                                               "Seat Share", "Government Party", "Competition Exposure", "2014"),
          out.header = FALSE, dep.var.caption=c(""), 
          dep.var.labels=c("1: Immi", "2: EU", "3: Immi", "4: EU"),
          omit=c("country_nameUK", "country_nameSweden", "country_nameSpain", "country_namePortugal",
                 "country_nameNetherlands", "country_nameLuxembourg", "country_nameItaly",
                 "country_nameIreland", "country_nameGermany", "country_nameFrance", "country_nameFinland", 
                 "country_nameDenmark", "country_nameBelgium"), 
          add.lines = list(c("Country FE", "", "", "X", "X")),
          model.numbers=FALSE)


#### - Stargazer: Multilevel models. Created LaTeX tables with online tool that come with the package. ####
#blurring
launch_shinystan(bl_immi_ml_by_rs)
launch_shinystan(bl_eu_ml_by_rs)

#ignoring
launch_shinystan(ig_immi_ml_by_rs)
launch_shinystan(ig_eu_ml_by_rs)

#entrepeneurship
launch_shinystan(en_immi_ml_by_rs)
launch_shinystan(en_eu_ml_by_rs)













#### - Stargazer: True fixed effects models #####
# Adjustes tables in Latex. 
## normal specification of electoral potential
stargazer(bl_immi_fepd, bl_eu_fepd, ig_immi_fepd, ig_eu_fepd, en_immi_fepd, en_eu_fepd, 
          title="Party strategies modelled with party dummies", align=TRUE, keep = 
            c("perext_immi_lv", "sd_immi_lv", "perext_eu_lv", "sd_eu_lv", "seat", "govt", "vot_overl_cor"),
          font.size = "footnotesize", column.sep.width = "10pt", keep.stat=c("n"), 
          covariate.labels=c("Electoral Potential", "Dividing Potential", "", "", "Seat Share", "Government Party", 
                             "Competition Exposure"),
          out.header = FALSE, dep.var.caption=c(""), 
          dep.var.labels=c("1: Immi", "2: EU", "3: Immi", "4: EU", "5: Immi", "6: EU"),
          column.labels = c("Good", "Better", "Good", "Better", "Good", "Better"),
          add.lines = list(c("Country FE", "X", "X", "X", "X", "X", "X")),
          model.numbers=FALSE)


## alternative specification of electoral potential
ig_immi_fepd_ep
ig_eu_fepd_ep
bl_immi_fepd_ep
bl_eu_fepd_ep
en_immi_fepd_ep
en_eu_fepd_ep


stargazer(bl_immi_fepd_ep, bl_eu_fepd_ep, ig_immi_fepd_ep, ig_eu_fepd_ep, en_immi_fepd_ep, en_eu_fepd_ep, 
          title="Party strategies modelled with party dummies", align=TRUE, keep = 
            c("mii_immi_lv", "mad_immi_lv_countrymean", "sd_immi_lv", "mii_eu_lv", "mad_eu_lv_countrymean",
              "sd_eu_lv", "seat", "govt", "vot_overl_cor"),
          font.size = "footnotesize", column.sep.width = "10pt", keep.stat=c("n"), 
          covariate.labels=c("MAD country mean", "MII", "Dividing Potential", "", "", "", "Seat Share", "Government Party", 
                             "Competition Exposure"),
          out.header = FALSE, dep.var.caption=c(""), 
          dep.var.labels=c("1: Immi", "2: EU", "3: Immi", "4: EU", "5: Immi", "6: EU"),
          column.labels = c("Good", "Better", "Good", "Better", "Good", "Better"),
          add.lines = list(c("Country FE", "X", "X", "X", "X", "X", "X")),
          model.numbers=FALSE)





#### - Stargazer: Alternative operationalisation of issue entrepreneurship using CHES #####

en_eu_fe_ches
en_immi_fe_ches
en_immi_fe_di_ches
en_eu_fe_di_ches

stargazer(en_eu_fe_ches, en_immi_fe_ches, en_eu_fe_di_ches, en_immi_fe_di_ches, 
          title="Entrepreneurship modelled with CHES salience", align=TRUE, keep = 
            c("perext_eu_lv", "mii_immi_lv", "mad_immi_lv_countrymean", "sd_immi_lv", "mii_eu_lv", "mad_eu_lv_countrymean",
              "sd_eu_lv", "perext_immi_lv", "seat", "govt", "vot_overl_cor"),
          font.size = "footnotesize", column.sep.width = "10pt", keep.stat=c("n"), 
          out.header = FALSE, dep.var.caption=c(""), 
          dep.var.labels=c("1: Immi", "2: EU", "3: Immi", "4: EU"),
          column.labels = c("Good", "Better", "Good", "Better"),
          add.lines = list(c("Country FE", "X", "X", "X", "X")),
          model.numbers=FALSE)




stargazer(en_eu_fe_cmp, en_immi_fe_cmp, en_immi_fe_di_cmp, en_eu_fe_di_cmp)









#### - Explaining party strategies as a function of party family. Used in conclusion ####
library(nnet)
library(sjPlot)

#create a factor object with labels attatched to it. (labels work weird with factors)
familt_crdem <- master_file$family
family_crdem <- factor(master_file$family, labels = c("rad righ", "conservative", "liberal","christdem", 
                                                      "socialists", "rad left", "green","regionalist",
                                                      "no family", "confessional", "agrarian"))

family_crdem <- relevel(family_crdem, ref="christdem") #christen democratics as the baseline 


pos_lr <- lm(master_file$lrecon~family_crdem)
pos_immi <- lm(master_file$immigration_dimension~family_crdem)
pos_eu <- lm(master_file$position~family_crdem)

sal_lr <- lm(master_file$lrecon_salience~family_crdem)
sal_immi <- lm(master_file$immigra_salience~family_crdem)
sal_eu <- lm(master_file$eu_salience~family_crdem)
entr_eu <- lm(master_file$issue_entrep_chessal_eu~family_crdem)
blur_eu <- lm(master_file$eu_dimension_sd~family_crdem)

pos_lr_pl <- plot_model(pos_lr, sort.est = FALSE, title="Left Right Position", colors = "gs")
sal_lr_pl <- plot_model(sal_lr, sort.est = FALSE, title="Salience Left Right", colors = "gs")
pos_immi_pl <- plot_model(pos_immi, sort.est = FALSE, title="Immigration Position", colors = "gs")
pos_eu_pl <- plot_model(pos_eu, sort.est = FALSE, title="EU Position", colors = "gs")

sal_immi_pl <- plot_model(sal_immi, sort.est = FALSE, title="Salience Immigration", colors = "gs")
sal_eu_pl <- plot_model(sal_eu, sort.est = FALSE, title="Salience EU", colors = "gs")
entr_eu_pl <- plot_model(entr_eu, sort.est = FALSE, title="Entrepeneurship EU", colors = "gs")
blur_eu_pl <- plot_model(blur_eu, sort.est = FALSE, title="Blurring EU", colors = "gs")


#saved as 700 X 507. 
fam_pl_pos <- cowplot::plot_grid(pos_lr_pl, sal_lr_pl, pos_immi_pl, pos_eu_pl, align = 'h')
fam_pl_str <- cowplot::plot_grid(sal_immi_pl, sal_eu_pl, entr_eu_pl, blur_eu_pl, align = 'h')
















