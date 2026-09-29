
###############################################################################
# Title:    code for Figure 4
# Author:   [Yiwen Pan]
#
# Description:
#   This script reproduces the analyses underlying Figure 4:
#     (a) Model selection for the NPP-total piecewise SEM (global, 20 km)
#     (b) Model selection for the NPP-stability piecewise SEM (global, 30 km)
#     (c) Direct and indirect effect plot for the "Amount" pathway
#     (d) Direct and indirect effect plot for the "FFI" pathway
#   Panels a and b were rendered manually in PowerPoint from the model
#   selection tables produced below; panels c and d are generated directly.
#
# Input:
#   `variables`                 — data frame of predictors and responses
#   `weights_matrix_dist_20`    — spatial weights matrix (20 km)
#   `weights_matrix_dist_30`    — spatial weights matrix (30 km)
#   `data`                      — coefficient table for panels c and d, with
#                                 columns: Predictor, Coefficient, Pathway,
#                                 Response
###############################################################################


# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------

# Install packages if missing (uncomment if needed)
# install.packages(c("piecewiseSEM", "spatialreg", "dplyr", "ggplot2",
#                    "ggbreak"))

library(piecewiseSEM)   # psem(), coefs()
library(spatialreg)     # errorsarlm()
library(dplyr)
library(ggplot2)
library(ggbreak)        # scale_x_break()

# Set a consistent theme for all figures
theme_set(theme_classic(base_family = "serif"))


# ===========================================================================
# Figure 4a: Model selection for NPP total (global, 20 km)
# ===========================================================================

# --- 4a.1 Full model (all paths) -------------------------------------------
Npp_total20_sem <- psem(
  errorsarlm(temp_annual ~ amount20 + FFI20 + dem_mean +
               aspect_S_proportion + interact20,
             data = variables, listw = weights_matrix_dist_20,
             zero.policy = TRUE, method = "MC"),
  errorsarlm(precip_sum_annual ~ amount20 + FFI20 + dem_mean +
               aspect_S_proportion + interact20,
             data = variables, listw = weights_matrix_dist_20,
             zero.policy = TRUE, method = "MC"),
  errorsarlm(diversity_SR1ha_mean ~ amount20 + FFI20 + temp_annual +
               precip_sum_annual + dem_mean + aspect_S_proportion + interact20,
             data = variables, listw = weights_matrix_dist_20,
             zero.policy = TRUE, method = "MC"),
  errorsarlm(GSL_mean20 ~ amount20 + FFI20 + temp_annual +
               precip_sum_annual + diversity_SR1ha_mean + dem_mean +
               aspect_S_proportion + interact20,
             data = variables, listw = weights_matrix_dist_20,
             zero.policy = TRUE, method = "MC"),
  errorsarlm(Npp_total20 ~ amount20 + FFI20 + GSL_mean20 + temp_annual +
               precip_sum_annual + diversity_SR1ha_mean + dem_mean +
               aspect_S_proportion + interact20,
             data = variables, listw = weights_matrix_dist_20,
             zero.policy = TRUE, method = "MC"),
  amount20 %~~% FFI20,
  temp_annual %~~% precip_sum_annual,
  data = variables
)

summary(Npp_total20_sem)

# --- 4a.2 Candidate (reduced) models ---------------------------------------
Npp_total20_sem_select1 <- psem(
  errorsarlm(temp_annual ~ amount20+FFI20+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(precip_sum_annual ~ amount20+FFI20+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(diversity_SR1ha_mean ~ amount20+FFI20+temp_annual+precip_sum_annual+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(GSL_mean20 ~ amount20+FFI20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(Npp_total20 ~ amount20+FFI20+GSL_mean20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  amount20 %~~% FFI20,
  temp_annual %~~% precip_sum_annual,
  data = variables)

result1<-summary(Npp_total20_sem_select1)

result_nppstotal20_select1<-coefs(Npp_total20_sem_select1)
result_nppstotal20_select1$AIC<-AIC(Npp_total20_sem_select1)$AIC
result_nppstotal20_select1$modelname <- "Npp_total20_sem_select1"
result_nppstotal20_select1$inverse_distance <- "20"

result_nppstotal20_select1$FisherCvalue<-as.numeric(result1$Cstat['Fisher.C'])
result_nppstotal20_select1$FisherCP<-as.numeric(result1$Cstat['P.Value'])



Npp_total20_sem_select2 <- psem(
  errorsarlm(temp_annual ~ amount20+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(precip_sum_annual ~ amount20+FFI20+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(diversity_SR1ha_mean ~ amount20+FFI20+temp_annual+precip_sum_annual+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(GSL_mean20 ~ amount20+FFI20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(Npp_total20 ~ amount20+FFI20+GSL_mean20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  amount20 %~~% FFI20,
  temp_annual %~~% precip_sum_annual,
  data = variables)

result2<-summary(Npp_total20_sem_select2)

result_nppstotal20_select2<-coefs(Npp_total20_sem_select2)
result_nppstotal20_select2$AIC<-AIC(Npp_total20_sem_select2)$AIC
result_nppstotal20_select2$modelname <- "Npp_total20_sem_select2"
result_nppstotal20_select2$inverse_distance <- "20"

result_nppstotal20_select2$FisherCvalue<-as.numeric(result2$Cstat['Fisher.C'])
result_nppstotal20_select2$FisherCP<-as.numeric(result2$Cstat['P.Value'])



Npp_total20_sem_select3 <- psem(
  errorsarlm(temp_annual ~ amount20+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(precip_sum_annual ~ amount20+FFI20+dem_mean+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(diversity_SR1ha_mean ~ amount20+FFI20+temp_annual+precip_sum_annual+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(GSL_mean20 ~ amount20+FFI20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(Npp_total20 ~ amount20+FFI20+GSL_mean20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  amount20 %~~% FFI20,
  temp_annual %~~% precip_sum_annual,
  data = variables)

result3<-summary(Npp_total20_sem_select3)

result_nppstotal20_select3<-coefs(Npp_total20_sem_select3)
result_nppstotal20_select3$AIC<-AIC(Npp_total20_sem_select3)$AIC
result_nppstotal20_select3$modelname <- "Npp_total20_sem_select3"
result_nppstotal20_select3$inverse_distance <- "20"

result_nppstotal20_select3$FisherCvalue<-as.numeric(result3$Cstat['Fisher.C'])
result_nppstotal20_select3$FisherCP<-as.numeric(result3$Cstat['P.Value'])


Npp_total20_sem_select4 <- psem(
  errorsarlm(temp_annual ~ dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(precip_sum_annual ~ amount20+FFI20+dem_mean+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(diversity_SR1ha_mean ~ amount20+FFI20+temp_annual+precip_sum_annual+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(GSL_mean20 ~ amount20+FFI20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(Npp_total20 ~ amount20+FFI20+GSL_mean20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  amount20 %~~% FFI20,
  temp_annual %~~% precip_sum_annual,
  data = variables)

result4<-summary(Npp_total20_sem_select4)

result_nppstotal20_select4<-coefs(Npp_total20_sem_select4)
result_nppstotal20_select4$AIC<-AIC(Npp_total20_sem_select4)$AIC
result_nppstotal20_select4$modelname <- "Npp_total20_sem_select4"
result_nppstotal20_select4$inverse_distance <- "20"

result_nppstotal20_select4$FisherCvalue<-as.numeric(result4$Cstat['Fisher.C'])
result_nppstotal20_select4$FisherCP<-as.numeric(result4$Cstat['P.Value'])



Npp_total20_sem_select5 <- psem(
  errorsarlm(temp_annual ~ dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(precip_sum_annual ~ amount20+dem_mean+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(diversity_SR1ha_mean ~ amount20+FFI20+temp_annual+precip_sum_annual+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(GSL_mean20 ~ amount20+FFI20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(Npp_total20 ~ amount20+FFI20+GSL_mean20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  amount20 %~~% FFI20,
  temp_annual %~~% precip_sum_annual,
  data = variables)

result5<-summary(Npp_total20_sem_select5)

result_nppstotal20_select5<-coefs(Npp_total20_sem_select5)
result_nppstotal20_select5$AIC<-AIC(Npp_total20_sem_select5)$AIC
result_nppstotal20_select5$modelname <- "Npp_total20_sem_select5"
result_nppstotal20_select5$inverse_distance <- "20"

result_nppstotal20_select5$FisherCvalue<-as.numeric(result5$Cstat['Fisher.C'])
result_nppstotal20_select5$FisherCP<-as.numeric(result5$Cstat['P.Value'])



Npp_total20_sem_select6 <- psem(
  errorsarlm(temp_annual ~ dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(precip_sum_annual ~ amount20+dem_mean+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(diversity_SR1ha_mean ~ amount20+FFI20+temp_annual+precip_sum_annual+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(GSL_mean20 ~ amount20+FFI20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(Npp_total20 ~ amount20+FFI20+GSL_mean20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  amount20 %~~% FFI20,
  temp_annual %~~% precip_sum_annual,
  data = variables)

result6<-summary(Npp_total20_sem_select6)

result_nppstotal20_select6<-coefs(Npp_total20_sem_select6)
result_nppstotal20_select6$AIC<-AIC(Npp_total20_sem_select6)$AIC
result_nppstotal20_select6$modelname <- "Npp_total20_sem_select6"
result_nppstotal20_select6$inverse_distance <- "20"

result_nppstotal20_select6$FisherCvalue<-as.numeric(result6$Cstat['Fisher.C'])
result_nppstotal20_select6$FisherCP<-as.numeric(result6$Cstat['P.Value'])


Npp_total20_sem_select7 <- psem(
  errorsarlm(temp_annual ~ dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(precip_sum_annual ~ amount20+dem_mean+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(diversity_SR1ha_mean ~ amount20+FFI20+temp_annual+precip_sum_annual+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(GSL_mean20 ~ amount20+FFI20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(Npp_total20 ~ amount20+FFI20+GSL_mean20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  amount20 %~~% FFI20,
  temp_annual %~~% precip_sum_annual,
  data = variables)

result7<-summary(Npp_total20_sem_select7)

result_nppstotal20_select7<-coefs(Npp_total20_sem_select7)
result_nppstotal20_select7$AIC<-AIC(Npp_total20_sem_select7)$AIC
result_nppstotal20_select7$modelname <- "Npp_total20_sem_select7"
result_nppstotal20_select7$inverse_distance <- "20"

result_nppstotal20_select7$FisherCvalue<-as.numeric(result7$Cstat['Fisher.C'])
result_nppstotal20_select7$FisherCP<-as.numeric(result7$Cstat['P.Value'])


Npp_total20_sem_select8 <- psem(
  errorsarlm(temp_annual ~ dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(precip_sum_annual ~ amount20+dem_mean, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(diversity_SR1ha_mean ~ amount20+FFI20+temp_annual+precip_sum_annual+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(GSL_mean20 ~ amount20+FFI20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  errorsarlm(Npp_total20 ~ amount20+FFI20+GSL_mean20+temp_annual+precip_sum_annual+diversity_SR1ha_mean+dem_mean+aspect_S_proportion+interact20, data = variables,
             listw = weights_matrix_dist_20, zero.policy = T, method = "MC"),
  amount20 %~~% FFI20,
  temp_annual %~~% precip_sum_annual,
  data = variables)

result8<-summary(Npp_total20_sem_select8)

result_nppstotal20_select8<-coefs(Npp_total20_sem_select8)
result_nppstotal20_select8$AIC<-AIC(Npp_total20_sem_select8)$AIC
result_nppstotal20_select8$modelname <- "Npp_total20_sem_select8"
result_nppstotal20_select8$inverse_distance <- "20"

result_nppstotal20_select8$FisherCvalue<-as.numeric(result8$Cstat['Fisher.C'])
result_nppstotal20_select8$FisherCP<-as.numeric(result8$Cstat['P.Value'])



result_npptotal20<-rbind(result_nppstotal20_select1,result_nppstotal20_select2,result_nppstotal20_select3,result_nppstotal20_select4,result_nppstotal20_select5,result_nppstotal20_select6,result_nppstotal20_select7,
                         result_nppstotal20_select8)

result_npptotal20 <- result_npptotal20 %>%
  distinct(AIC, modelname, .keep_all = TRUE)

#AIC of select3 is lowest


# ===========================================================================
# Figure 4b: Model selection for NPP stability (global, 30 km)
# ===========================================================================

# --- 4b.1 Full model --------------------------------------------------------
Npp_stability20_00_sem <- psem(
  errorsarlm(temp_mean20_01 ~ amount_mean20_00 + FFI_mean20_00 + interact +
               dem_mean + aspect_S_proportion,
             data = variables, listw = weights_matrix_dist_30,
             zero.policy = TRUE, method = "MC"),
  errorsarlm(precip_mean20_01 ~ amount_mean20_00 + FFI_mean20_00 + interact +
               dem_mean + aspect_S_proportion,
             data = variables, listw = weights_matrix_dist_30,
             zero.policy = TRUE, method = "MC"),
  errorsarlm(diversity_SR1ha_mean ~ amount_mean20_00 + FFI_mean20_00 +
               interact + temp_mean20_01 + precip_mean20_01 + dem_mean +
               aspect_S_proportion,
             data = variables, listw = weights_matrix_dist_30,
             zero.policy = TRUE, method = "MC"),
  errorsarlm(GSL_stability20_00 ~ amount_mean20_00 + FFI_mean20_00 +
               interact + temp_mean20_01 + precip_mean20_01 +
               diversity_SR1ha_mean + dem_mean + aspect_S_proportion,
             data = variables, listw = weights_matrix_dist_30,
             zero.policy = TRUE, method = "MC"),
  errorsarlm(NppTotal_stability ~ amount_mean20_00 + FFI_mean20_00 +
               interact + temp_mean20_01 + precip_mean20_01 +
               diversity_SR1ha_mean + GSL_stability20_00 + dem_mean +
               aspect_S_proportion,
             data = variables, listw = weights_matrix_dist_30,
             zero.policy = TRUE, method = "MC"),
  amount_mean20_00 %~~% FFI_mean20_00,
  temp_mean20_01 %~~% precip_mean20_01,
  data = variables
)

summary(Npp_stability20_00_sem)

# --- 4b.2 Candidate (reduced) models ---------------------------------------
Npp_stability20_00_sem_select1<-psem(
  errorsarlm(temp_mean20_01 ~ amount_mean20_00+FFI_mean20_00+interact+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  errorsarlm(precip_mean20_01 ~ amount_mean20_00+FFI_mean20_00+interact+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  errorsarlm(diversity_SR1ha_mean~ amount_mean20_00+FFI_mean20_00+interact+temp_mean20_01+precip_mean20_01+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  errorsarlm(GSL_stability20_00~ amount_mean20_00+FFI_mean20_00+interact+temp_mean20_01+diversity_SR1ha_mean+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  errorsarlm(NppTotal_stability~amount_mean20_00+FFI_mean20_00+interact+temp_mean20_01+precip_mean20_01+diversity_SR1ha_mean+GSL_stability20_00+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  amount_mean20_00 %~~% FFI_mean20_00,
  temp_mean20_01 %~~% precip_mean20_01,
  data = variables)

result1<-summary(Npp_stability20_00_sem_select1)

result_nppstability_select1<-coefs(Npp_stability20_00_sem_select1)
result_nppstability_select1$AIC<-AIC(Npp_stability20_00_sem_select1)$AIC
result_nppstability_select1$modelname <- "Npp_stability20_00_sem_select1"
result_nppstability_select1$inverse_distance <- "30"

result_nppstability_select1$FisherCvalue<-as.numeric(result1$Cstat['Fisher.C'])
result_nppstability_select1$FisherCP<-as.numeric(result1$Cstat['P.Value'])



Npp_stability20_00_sem_select2<-psem(
  errorsarlm(temp_mean20_01 ~ amount_mean20_00+FFI_mean20_00+interact+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  errorsarlm(precip_mean20_01 ~ amount_mean20_00+FFI_mean20_00+interact+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  errorsarlm(diversity_SR1ha_mean~ amount_mean20_00+FFI_mean20_00+interact+temp_mean20_01+precip_mean20_01+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  errorsarlm(GSL_stability20_00~ amount_mean20_00+FFI_mean20_00+interact+temp_mean20_01+diversity_SR1ha_mean+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  errorsarlm(NppTotal_stability~amount_mean20_00+FFI_mean20_00+interact+precip_mean20_01+diversity_SR1ha_mean+GSL_stability20_00+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  amount_mean20_00 %~~% FFI_mean20_00,
  temp_mean20_01 %~~% precip_mean20_01,
  data = variables)

result2<-summary(Npp_stability20_00_sem_select2)

result_nppstability_select2<-coefs(Npp_stability20_00_sem_select2)
result_nppstability_select2$AIC<-AIC(Npp_stability20_00_sem_select2)$AIC
result_nppstability_select2$modelname <- "Npp_stability20_00_sem_select2"
result_nppstability_select2$inverse_distance <- "30"

result_nppstability_select2$FisherCvalue<-as.numeric(result2$Cstat['Fisher.C'])
result_nppstability_select2$FisherCP<-as.numeric(result2$Cstat['P.Value'])



Npp_stability20_00_sem_select3<-psem(
  errorsarlm(temp_mean20_01 ~ amount_mean20_00+FFI_mean20_00+interact+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  errorsarlm(precip_mean20_01 ~ amount_mean20_00+FFI_mean20_00+interact+dem_mean,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  errorsarlm(diversity_SR1ha_mean~ amount_mean20_00+FFI_mean20_00+interact+temp_mean20_01+precip_mean20_01+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  errorsarlm(GSL_stability20_00~ amount_mean20_00+FFI_mean20_00+interact+temp_mean20_01+diversity_SR1ha_mean+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  errorsarlm(NppTotal_stability~amount_mean20_00+FFI_mean20_00+interact+precip_mean20_01+diversity_SR1ha_mean+GSL_stability20_00+dem_mean+aspect_S_proportion,data = variables,
             listw = weights_matrix_dist_30, zero.policy = T,
             method = "MC"),
  amount_mean20_00 %~~% FFI_mean20_00,
  temp_mean20_01 %~~% precip_mean20_01,
  data = variables)

result3<-summary(Npp_stability20_00_sem_select3)

result_nppstability_select3<-coefs(Npp_stability20_00_sem_select3)
result_nppstability_select3$AIC<-AIC(Npp_stability20_00_sem_select3)$AIC
result_nppstability_select3$modelname <- "Npp_stability20_00_sem_select3"
result_nppstability_select3$inverse_distance <- "30"

result_nppstability_select3$FisherCvalue<-as.numeric(result3$Cstat['Fisher.C'])
result_nppstability_select3$FisherCP<-as.numeric(result3$Cstat['P.Value'])


result_nppstability <- bind_rows(
  result_nppstability_select1,
  result_nppstability_select2,
  result_nppstability_select3
)


result_nppstability <- result_nppstability  %>%
  distinct(AIC, modelname, .keep_all = TRUE)


#AIC of select3 is lowest


# ===========================================================================
# Figure 4c: Direct and indirect effect plot for the "Amount" pathway
# ===========================================================================

select_amount <- data %>% filter(Predictor == "Amount")

plot_4c <- ggplot(select_amount,
                  aes(x = Coefficient, y = Pathway, fill = Response)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Standardized coefficient", fill = "response") +
  scale_fill_manual(values = c("#339900", "#0066CC")) +
  scale_x_break(c(0.004, 0.1), scales = "free", space = 0.2) +
  scale_x_continuous(
    breaks = c(0, seq(0.001, 0.004, by = 0.002),
               seq(0.1, 0.6, by = 0.2)),
    labels = function(x) ifelse(x == 0, "0", as.character(x)),
    sec.axis = dup_axis()
  ) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  theme_classic() +
  theme(
    text               = element_text(family = "serif"),
    strip.text         = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.y        = element_text(size = 15, color = "black"),
    axis.text.x        = element_text(size = 15, color = "black"),
    axis.line.x.top    = element_blank(),
    axis.text.x.top    = element_blank(),
    axis.ticks.x.top   = element_blank(),
    axis.title.x       = element_text(size = 15, face = "bold"),
    axis.title.x.top   = element_blank(),
    axis.title.y       = element_blank(),
    legend.text        = element_text(size = 15),
    legend.title       = element_blank(),
    legend.position    = "top"
  )

plot_4c


# ===========================================================================
# Figure 4d: Direct and indirect effect plot for the "FFI" pathway
# ===========================================================================

select_ffi <- data %>% filter(Predictor == "FFI")

plot_4d <- ggplot(select_ffi,
                  aes(x = Coefficient, y = Pathway, fill = Response)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Standardized coefficient", fill = "response") +
  scale_fill_manual(values = c("#339900", "#0066CC")) +
  scale_x_break(c(-0.01, -0.001), scales = "free", space = 0.2) +
  scale_x_continuous(
    breaks = c(seq(-0.4, -0.01, by = 0.1), 0, 0.005,
               seq(0.05, 0.6, by = 0.1)),
    labels = function(x) ifelse(x == 0, "0", as.character(x)),
    sec.axis = dup_axis()
  ) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  theme_classic() +
  theme(
    text               = element_text(family = "serif"),
    strip.text         = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.y        = element_text(size = 15, color = "black"),
    axis.text.x        = element_text(size = 15, color = "black"),
    axis.line.x.top    = element_blank(),
    axis.text.x.top    = element_blank(),
    axis.ticks.x.top   = element_blank(),
    axis.title.x       = element_text(size = 15, face = "bold"),
    axis.title.x.top   = element_blank(),
    axis.title.y       = element_blank(),
    legend.text        = element_text(size = 15),
    legend.title       = element_blank(),
    legend.position    = "top"
  )

plot_4d


###############################################################################
# End of script
###############################################################################


