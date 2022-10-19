# Modeling wrapper test
install.packages("readxl")
install.packages("tidyr")
install.packages("reshape2")
install.packages("splitstackshape")
install.packages("doBy")
install.packages("WriteXLS")
install.packages("Writexl")
install.packages('Rcpp')
install.packages("ggplot2")
install.packages("ggplot")
install.packages("dplyr")
install.packages("rlist")

install.packages("fitdistrplus")
install.packages("MASS")
install.packages("psych")
install.packages("rgl")
install.packages("copula")
install.packages("VineCopula")

install.packages("scales")
install.packages("univariateML")
install.packages("xlsx")
install.packages("writexl")
install.packages("logspline")
install.packages("xtable")
install.packages("devtools")
install.packages("plyr")
install.packages("rJava")
install.packages("readr")
install.packages("Mvt")

library(psych)
library(rgl)
library(copula)
library(VineCopula)
library(readxl)
library(scales)
library(univariateML)
library(xlsx)
library(fitdistrplus)
library(logspline)
library(plyr)
library(readr)
library(mvtnorm)

library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(splitstackshape)
library(doBy)
library(WriteXLS)
library(rlist)
library(xtable)
library(MASS)
library(stats)



##Estimate the parameters of the calibration year catch series'
source("calibration year copula params.R")


n_drawz<-1000

# Start the clock!
ptm <- proc.time()


state_output = data.frame()
state_cal_output = data.frame()
state_pred_output = data.frame()
year_output = data.frame()


#for (x in 1:30){
  ##########
  # Estimate the catch-per-trip copulas so we don't re-estimate every time
  #source("calc_catch_per_trip_copulas.R")
  ##########
  
  
  
  ########## 
  # 1) Run the calibration files
  
  source("HCR calibration MA.R")
  source("HCR calibration RI.R")
  source("HCR calibration CT.R")
  source("HCR calibration NY.R")
  source("HCR calibration NJ.R")
  source("HCR calibration DE.R")
  source("HCR calibration MD.R")
  source("HCR calibration VA.R")
  source("HCR calibration NC.R")
  
  
  # Combine the results
  calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                         pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                         pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))
  
  
  
  calibration_output_by_period[is.na(calibration_output_by_period)] = 0
  
  
  # MRIP_data_sf <- subset(data.frame( read.csv("total AB1B2 2021 by state.csv")),  species=="SUMMER FLOUNDER")                                                                          
  # MRIP_data_bsb <- subset(data.frame( read.csv("total AB1B2 2021 by state.csv")), species=="BLACK SEA BASS")                                                                          
  # MRIP_data_scup <- subset(data.frame( read.csv("total AB1B2 2021 by state.csv")), species=="SCUP")                                                                          
  # 
  # sum(calibration_output_by_period$tot_sf_catch)
  # sum(MRIP_data_sf$tot_catch)
  # (sum(MRIP_data_sf$tot_catch)-sum(calibration_output_by_period$tot_sf_catch))/sum(MRIP_data_sf$tot_catch)
  # 
  # sum(calibration_output_by_period$tot_keep_sf)
  # sum(MRIP_data_sf$tot_harvest)
  # (sum(MRIP_data_sf$tot_harvest)-sum(calibration_output_by_period$tot_keep_sf))/sum(MRIP_data_sf$tot_harvest)
  # 
  # sum(calibration_output_by_period$tot_rel_sf)
  # sum(MRIP_data_sf$tot_rel)
  # (sum(MRIP_data_sf$tot_rel)-sum(calibration_output_by_period$tot_rel_sf))/sum(MRIP_data_sf$tot_rel)
  # 
  # #########
  # 
  # sum(calibration_output_by_period$tot_bsb_catch)
  # sum(MRIP_data_bsb$tot_catch)
  # (sum(MRIP_data_bsb$tot_catch)-sum(calibration_output_by_period$tot_bsb_catch))/sum(MRIP_data_bsb$tot_catch)
  # 
  # sum(calibration_output_by_period$tot_keep_bsb)
  # sum(MRIP_data_bsb$tot_harvest)
  # (sum(MRIP_data_bsb$tot_harvest)-sum(calibration_output_by_period$tot_keep_bsb))/sum(MRIP_data_bsb$tot_harvest)
  # 
  # sum(calibration_output_by_period$tot_rel_bsb)
  # sum(MRIP_data_bsb$tot_rel)
  # (sum(MRIP_data_bsb$tot_rel)-sum(calibration_output_by_period$tot_rel_bsb))/sum(MRIP_data_bsb$tot_rel)
  # 
  # #########
  # 
  # sum(calibration_output_by_period$tot_scup_catch)
  # sum(MRIP_data_scup$tot_catch)
  # (sum(MRIP_data_scup$tot_catch)-sum(calibration_output_by_period$tot_scup_catch))/sum(MRIP_data_scup$tot_catch)
  # 
  # sum(calibration_output_by_period$tot_keep_scup)
  # sum(MRIP_data_scup$tot_harvest)
  # (sum(MRIP_data_scup$tot_harvest)-sum(calibration_output_by_period$tot_keep_scup))/sum(MRIP_data_scup$tot_harvest)
  # 
  # sum(calibration_output_by_period$tot_rel_scup)
  # sum(MRIP_data_scup$tot_rel)
  # (sum(MRIP_data_scup$tot_rel)-sum(calibration_output_by_period$tot_rel_scup))/sum(MRIP_data_scup$tot_rel)
  
  
  calibration_output_by_period$draw = 1
  
  
  write_xlsx(calibration_output_by_period,"calibration_output_by_period.xlsx")
  saveRDS(calibration_output_by_period, file = "calibration_output_by_period.rds")
  
  #For multiple draws, stack these in state_cal_output
  #state_cal_output =rbind.fill(state_cal_output, calibration_output_by_period)
  
#}
#write_xlsx(state_cal_output,"calibration_output_by_period.xlsx")
  
  
  state_pred_output = data.frame()
  years<-c("2021", "2023")
  # regs <- c("minus1")
  
  for (x in 1:50){
    for (y in years){
     year <- y
      #x<-1
      # 
      #for (x in 1:30){
      #regulation="2015"
      
      ##########  
      # Input new population numbers-at-age distribution (numbers_at_age_YYYY) in the following script to create population adjusted 
      # catch-at-length and catch-per-trip for summer flounder
      #source("CAL given stock structure - assessment coastwide - prediction.R")
      #source("catch at length given stock structure - prediction.R")
      
      # THIS IS WHERE TO IMPORT THE ALKS FOR EACH SPECIES
      # Import the fluke ALK (in centimeters) provided by M. Terceiro
      fluke_ALK <- data.frame(read_csv("fluke_ALK_2018_adj.csv", show_col_types = FALSE))
      #bsb_ALK <- data.frame(read_csv("", show_col_types = FALSE))
      scup_ALK <- data.frame(read_csv("scup_ALK_2015_2018_adj.csv", show_col_types = FALSE))
      
      
      
      # THIS IS WHERE TO IMPORT THE NUMBERS AT AGE FOR EACH SPECIES BASED ON THE YEAR(S) OF INTEREST
      # Import the fluke MCMC draws
      fluke_numbers_at_age = data.frame(read_csv(paste0("fluke_MCMC_100_", year,".csv"), show_col_types = FALSE))
      fluke_numbers_at_age = subset(fluke_numbers_at_age, fluke_numbers_at_age$draw==x)
      
      # Import the bsb MCMC draws
      #bsb_numbers_at_age = data.frame(read_csv("bsb_MCMC_100_2021.csv", show_col_types = FALSE))
      #bsb_numbers_at_age = subset(bsb_numbers_at_age, bsb_numbers_at_age$draw==1)
      
      # Import the scup MCMC draws
      scup_numbers_at_age = data.frame(read_csv(paste0("scup_MCMC_100_", year,".csv"), show_col_types = FALSE))
      scup_numbers_at_age = subset(scup_numbers_at_age, scup_numbers_at_age$draw==x)
      
    
      source("CAL given stock structure.R")
      
      ##########  
      
      
      
      ##########  
      # run the simulation code under the new set of regulations (regulation file is directed_trips_region - alternative regs test.xlsx)
      
      directed_trip_alt_regs=data.frame(read_csv(paste0("directed trips and regulations ", year,".csv"), show_col_types = FALSE))
      directed_trip_alt_regs$dtrip_2019=round(directed_trip_alt_regs$dtrip)
      
      
      source("HCR prediction MA.R")
      source("HCR prediction RI.R")
      source("HCR prediction CT.R")
      source("HCR prediction NY.R")
      source("HCR prediction NJ.R")
      source("HCR prediction DE.R")
      source("HCR prediction MD.R")
      source("HCR prediction VA.R")
      source("HCR prediction NC.R")
      
      
      prediction_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                            pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                            pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))
      
      
      prediction_output_by_period[is.na(prediction_output_by_period)] = 0
      write_xlsx(prediction_output_by_period,"prediction_output_by_period.xlsx")
      
      
      
      state_prediction_output=prediction_output_by_period
      
      
      # state_prediction_output$state1=with(state_prediction_output, match(state, unique(state)))
      # state_prediction_output1= subset(state_prediction_output, select=-c(state, period))
      # state_prediction_output1=aggregate(state_prediction_output1, by=list(state_prediction_output1$state1),FUN=sum, na.rm=TRUE)
      # state_prediction_output1= subset(state_prediction_output1, select=-c(state1))
      # names(state_prediction_output1)[names(state_prediction_output1) == "Group.1"] = "state1"
      # 
      # 
      # state_names=subset(state_prediction_output, select=c(state, state1))
      # state_names = state_names[!duplicated(state_names), ]
      # state_prediction_output1 =  merge(state_prediction_output1,state_names,by="state1", all.x=TRUE, all.y=TRUE)
      
      state_prediction_output$draw <- x
      state_prediction_output$year <- y
      
      
      state_pred_output <- rbind.fill(state_pred_output, state_prediction_output)
      
      #year_output <- rbind.fill(year_output, state_pred_output)
      
    }
  
  
  }
  write_xlsx(state_pred_output,"state_output.xlsx")
  
  write_xlsx(state_pred_output,"state_output_new_pop_ns_15_21.xlsx")
  #write_xlsx(state_pred_output,"state_output_new_pop_ns_15_21_avg_selectivity.xlsx")
  
  #write_xlsx(state_pred_output,paste0("state_output_new_pop_ns",year,".xlsx"))
  
  
  
  # Stop the clock
  proc.time() - ptm
  
  
  
  
  