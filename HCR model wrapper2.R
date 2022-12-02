

args = commandArgs(trailingOnly=TRUE)

# This is the modeling wrapper 

# Steps in the process

#2) Simulate the fishery under alternative regulations and a new catch-at-length distribution for summer flounder
# a) Create new catch-at-length/catch-per-trip distributions for summer flounder based on population numbers at length. 
# a) Calcualte angler welfare/fishing effort changes and changes in catch
# Modeling wrapper test
#profvis::profvis({
#load needed packages and install if not currently installed.
pkgs_to_use <- c("tidyr",
                 "magrittr",
                 "tidyverse",
                 "reshape2",
                 "splitstackshape",
                 "doBy",
                 "WriteXLS",
                 "Rcpp",
                 "ggplot2",
                 "dplyr",
                 "rlist",
                 "fitdistrplus",
                 "MASS",
                 "psych",
                 "rgl",
                 "copula",
                 "VineCopula",
                 "scales",
                 "univariateML",
                 "logspline",
                 "readr",
                 "data.table",
                 "conflicted", 
                 "readxl", 
                 "writexl", 
                 "plyr")
#install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
# library(readxl)
# library(writexl)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("count", "dplyr")



###Run the calibration model 
source("calibration year copula params - two species.R")


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
calibration_output_by_period$draw = 1

write_xlsx(calibration_output_by_period,"calibration_output_by_period.xlsx")
saveRDS(calibration_output_by_period, file = "calibration_output_by_period.rds")

calibration_data_table <- readRDS("calibration_output_by_period.rds")
calibration_data_table_base <- split(calibration_data_table, calibration_data_table$state)

costs_new_all_MA$state<-"MA"
costs_new_all_RI$state<-"RI"
costs_new_all_CT$state<-"CT"
costs_new_all_NY$state<-"NY"
costs_new_all_NJ$state<-"NJ"
costs_new_all_DE$state<-"DE"
costs_new_all_MD$state<-"MD"
costs_new_all_VA$state<-"VA"
costs_new_all_NC$state<-"NC"
 
#2) Run the prediction model 

##Run the catch function 
source("catch_function.R")

# Start the clock!
ptm <- proc.time()



predictions = list()
predictions_all = list()

#years<-c( "2018", "2019", "2020", "2022")
#years<-c( "2018", "2019", "2020", "2022", "2023.1", "2023.2", "2023.3", "2023.4", "2023.5","2023.6", "2023.7", "2024", "2025" )
#years<-c( "2018",  "2023.4" )
years<-c( "2022", "2023.1", "2023.4", "2023.5","2023.6", "2023.7", "2025" )

for (x in 1:100){
  for (y in years){
  year <- y
  
  # THIS IS WHERE TO IMPORT THE ALKS FOR EACH SPECIES
  # Import the fluke ALK (in centimeters) provided by M. Terceiro
  fluke_ALK <- data.frame(read_csv("fluke_ALK_2018_adj.csv", show_col_types = FALSE))
  #bsb_ALK <- data.frame(read_csv("", show_col_types = FALSE))
  scup_ALK <- data.frame(read_csv("scup_ALK_2015_2018_adj.csv", show_col_types = FALSE))
  
  
  if (year %in% c("2018", "2019", "2020", "2022")){
  # THIS IS WHERE TO IMPORT THE NUMBERS AT AGE FOR EACH SPECIES BASED ON THE YEAR(S) OF INTEREST
  # Import the fluke MCMC draws
  fluke_numbers_at_age = data.frame(read_csv(paste0("fluke_MCMC_100_", year,".csv"), show_col_types = FALSE))
  #fluke_numbers_at_age = data.frame(read_csv("fluke_MCMC_100_2023.csv", show_col_types = FALSE))
  fluke_numbers_at_age = subset(fluke_numbers_at_age, fluke_numbers_at_age$draw==x)
  
  #this is the check dataset with median values of the 2021 stock
  #fluke_numbers_at_age = data.frame(read_csv(paste0("fluke_MCMC_median_", year,".csv"), show_col_types = FALSE))
  #fluke_numbers_at_age = subset(fluke_numbers_at_age, fluke_numbers_at_age$draw==1)
  
  # Import the bsb MCMC draws
  #bsb_numbers_at_age = data.frame(read_csv("bsb_MCMC_100_2021.csv", show_col_types = FALSE))
  #bsb_numbers_at_age = subset(bsb_numbers_at_age, bsb_numbers_at_age$draw==1)
  
  # Import the scup MCMC draws
  scup_numbers_at_age = data.frame(read_csv(paste0("scup_MCMC_100_", year,".csv"), show_col_types = FALSE))
  scup_numbers_at_age = subset(scup_numbers_at_age, scup_numbers_at_age$draw==x)
  #scup_numbers_at_age = data.frame(read_csv("scup_MCMC_100_2023.csv", show_col_types = FALSE))
  
  #this is the check dataset with median values of the 2021 stock
  #scup_numbers_at_age = data.frame(read_csv(paste0("scup_MCMC_median_", year,".csv"), show_col_types = FALSE))
  #scup_numbers_at_age = subset(scup_numbers_at_age, scup_numbers_at_age$draw==1)
  }
  
  #Choose 2023 stock distirbution for 2023 runs
  if (year %in% c("2023.1", "2023.2", "2023.3", "2023.4", "2023.5","2023.6", "2023.7", "2024", "2025")){
    fluke_numbers_at_age = data.frame(read_csv("fluke_MCMC_100_2023.csv", show_col_types = FALSE))
    fluke_numbers_at_age = subset(fluke_numbers_at_age, fluke_numbers_at_age$draw==x)

    scup_numbers_at_age = data.frame(read_csv("scup_MCMC_100_2023.csv", show_col_types = FALSE))
    scup_numbers_at_age = subset(scup_numbers_at_age, scup_numbers_at_age$draw==x)

  }
  
  source("CAL given stock structure.R")
  
  ##########
  
  
  
  ##########
  # run the simulation code under the new set of regulations (regulation file is directed trips and regulations XXXX.xlsx)
  directed_trips_table=data.frame(read_csv(paste0("directed trips and regulations ", year,".csv"), show_col_types = FALSE))
  directed_trips_table_base <- split(directed_trips_table, directed_trips_table$state)
  
  #directed_trip_alt_regs$dtrip_2019=round(directed_trip_alt_regs$dtrip)

  
  # params <- list(state1 = c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"),
  #                calibration_data_table = c(list(calibration_data_table_base[[3]]),list(calibration_data_table_base[[8]]), list(calibration_data_table_base[[1]]),
  #                                           list(calibration_data_table_base[[7]]), list(calibration_data_table_base[[6]]), list(calibration_data_table_base[[2]]),
  #                                           list(calibration_data_table_base[[4]]), list(calibration_data_table_base[[9]]), list(calibration_data_table_base[[5]])),
  #                directed_trips_table = c(list(directed_trips_table[[3]]),list(directed_trips_table[[8]]), list(directed_trips_table[[1]]),
  #                                         list(directed_trips_table[[7]]), list(directed_trips_table[[6]]), list(directed_trips_table[[2]]),
  #                                         list(directed_trips_table[[4]]), list(directed_trips_table[[9]]), list(directed_trips_table[[5]])),
  #                sf_size_data_read <- c(list(sf_size_data_read_base[[3]]),list(sf_size_data_read_base[[8]]), list(sf_size_data_read_base[[1]]),
  #                                       list(sf_size_data_read_base[[7]]), list(sf_size_data_read_base[[6]]), list(sf_size_data_read_base[[2]]),
  #                                       list(sf_size_data_read_base[[4]]), list(sf_size_data_read_base[[9]]), list(sf_size_data_read_base[[5]])),
  #                bsb_size_data_read <- c(list(bsb_size_data_read_base[[3]]),list(bsb_size_data_read_base[[8]]), list(bsb_size_data_read_base[[1]]),
  #                                        list(bsb_size_data_read_base[[7]]), list(bsb_size_data_read_base[[6]]), list(bsb_size_data_read_base[[2]]),
  #                                        list(bsb_size_data_read_base[[4]]), list(bsb_size_data_read_base[[9]]), list(bsb_size_data_read_base[[5]])),
  #                scup_size_data_read <- c(list(scup_size_data_read_base[[3]]),list(scup_size_data_read_base[[8]]), list(scup_size_data_read_base[[1]]),
  #                                         list(scup_size_data_read_base[[7]]), list(scup_size_data_read_base[[6]]), list(scup_size_data_read_base[[2]]),
  #                                         list(scup_size_data_read_base[[4]]), list(scup_size_data_read_base[[9]]), list(scup_size_data_read_base[[5]])),
  #                param_draws_MA = c(list(param_draws_MA), list(param_draws_RI), list(param_draws_CT),
  #                                   list(param_draws_NY), list(param_draws_NJ), list(param_draws_DE),
  #                                   list(param_draws_MD), list(param_draws_VA), list(param_draws_NC)),
  #                costs_new_all_MA = c(list(costs_new_all_MA), list(costs_new_all_RI), list(costs_new_all_CT),
  #                                     list(costs_new_all_NY), list(costs_new_all_NJ), list(costs_new_all_DE),
  #                                     list(costs_new_all_MD), list(costs_new_all_VA), list(costs_new_all_NC)),
  #                sf_catch_data_all = c(list(sf_catch_data_ma),list(sf_catch_data_ri),
  #                                      list(sf_catch_data_ct),list(sf_catch_data_ny),
  #                                      list(sf_catch_data_nj),list(sf_catch_data_de),
  #                                      list(sf_catch_data_md),list(sf_catch_data_va), list(sf_catch_data_nc)))
  #  safe_predict_rec_catch <- purrr::safely(predict_rec_catch, otherwise = NA_real_)
  #  xx <-  purrr::pmap(params, predict_rec_catch)
  #  prediction_output_by_period <- purrr::map(xx, 1)
  
  MA_pred <- predict_rec_catch(state1 <- "MA",
                               calibration_data_table <- calibration_data_table_base[[3]],
                               directed_trips_table <- directed_trips_table_base[[3]],
                               sf_size_data_read <- sf_size_data_read_base[[3]],
                               bsb_size_data_read <- bsb_size_data_read_base[[3]],
                               scup_size_data_read <- scup_size_data_read_base[[3]],
                               param_draws_MA <- param_draws_MA,
                               costs_new_all <- costs_new_all_MA,
                               sf_catch_data_all <- sf_catch_data_ma)

  RI_pred <- predict_rec_catch(state1 <- "RI",
                               calibration_data_table <- calibration_data_table_base[[8]],
                               directed_trips_table <- directed_trips_table_base[[8]],
                               sf_size_data_read <- sf_size_data_read_base[[8]],
                               bsb_size_data_read <- bsb_size_data_read_base[[8]],
                               scup_size_data_read <- scup_size_data_read_base[[8]],
                               param_draws_MA <- param_draws_RI,
                               costs_new_all <- costs_new_all_RI,
                               sf_catch_data_all <- sf_catch_data_ri)

  CT_pred <- predict_rec_catch(state1 <- "CT",
                               calibration_data_table <- calibration_data_table_base[[1]],
                               directed_trips_table <- directed_trips_table_base[[1]],
                               sf_size_data_read <- sf_size_data_read_base[[1]],
                               bsb_size_data_read <- bsb_size_data_read_base[[1]],
                               scup_size_data_read <- scup_size_data_read_base[[1]],
                               param_draws_MA <- param_draws_CT,
                               costs_new_all <- costs_new_all_CT,
                               sf_catch_data_all <- sf_catch_data_ct)


  NY_pred <- predict_rec_catch(state1 <- "NY",
                               calibration_data_table <- calibration_data_table_base[[7]],
                               directed_trips_table <- directed_trips_table_base[[7]],
                               sf_size_data_read <- sf_size_data_read_base[[7]],
                               bsb_size_data_read <- bsb_size_data_read_base[[7]],
                               scup_size_data_read <- scup_size_data_read_base[[7]],
                               param_draws_MA <- param_draws_NY,
                               costs_new_all <- costs_new_all_NY,
                               sf_catch_data_all <- sf_catch_data_ny)

  NJ_pred <- predict_rec_catch(state1 <- "NJ",
                               calibration_data_table <- calibration_data_table_base[[6]],
                               directed_trips_table <- directed_trips_table_base[[6]],
                               sf_size_data_read <- sf_size_data_read_base[[6]],
                               bsb_size_data_read <- bsb_size_data_read_base[[6]],
                               scup_size_data_read <- scup_size_data_read_base[[6]],
                               param_draws_MA <- param_draws_NJ,
                               costs_new_all <- costs_new_all_NJ,
                               sf_catch_data_all <- sf_catch_data_nj)


  DE_pred <- predict_rec_catch(state1 <- "DE",
                               calibration_data_table <- calibration_data_table_base[[2]],
                               directed_trips_table <- directed_trips_table_base[[2]],
                               sf_size_data_read <- sf_size_data_read_base[[2]],
                               bsb_size_data_read <- bsb_size_data_read_base[[2]],
                               scup_size_data_read <- scup_size_data_read_base[[2]],
                               param_draws_MA <- param_draws_DE,
                               costs_new_all <- costs_new_all_DE,
                               sf_catch_data_all <- sf_catch_data_de)


  MD_pred <- predict_rec_catch(state1 <- "MD",
                               calibration_data_table <- calibration_data_table_base[[4]],
                               directed_trips_table <- directed_trips_table_base[[4]],
                               sf_size_data_read <- sf_size_data_read_base[[4]],
                               bsb_size_data_read <- bsb_size_data_read_base[[4]],
                               scup_size_data_read <- scup_size_data_read_base[[4]],
                               param_draws_MA <- param_draws_MD,
                               costs_new_all <- costs_new_all_MD,
                               sf_catch_data_all <- sf_catch_data_md)

  VA_pred <- predict_rec_catch(state1 <- "VA",
                               calibration_data_table <- calibration_data_table_base[[9]],
                               directed_trips_table <- directed_trips_table_base[[9]],
                               sf_size_data_read <- sf_size_data_read_base[[9]],
                               bsb_size_data_read <- bsb_size_data_read_base[[9]],
                               scup_size_data_read <- scup_size_data_read_base[[9]],
                               param_draws_MA <- param_draws_VA,
                               costs_new_all <- costs_new_all_VA,
                               sf_catch_data_all <- sf_catch_data_va)

  NC_pred <- predict_rec_catch(state1 <- "NC",
                               calibration_data_table <- calibration_data_table_base[[5]],
                               directed_trips_table <- directed_trips_table_base[[5]],
                               sf_size_data_read <- sf_size_data_read_base[[5]],
                               bsb_size_data_read <- bsb_size_data_read_base[[5]],
                               scup_size_data_read <- scup_size_data_read_base[[5]],
                               param_draws_MA <- param_draws_NC,
                               costs_new_all <- costs_new_all_NC,
                               sf_catch_data_all <- sf_catch_data_nc)
  
  predictions[[y]]<- dplyr::bind_rows(MA_pred, RI_pred, CT_pred, NY_pred, NJ_pred, DE_pred, MD_pred, VA_pred, NC_pred)
  predictions[[y]]$draw<-x
  predictions[[y]]$year<-year
  
  
}

predictions_all[[x]]= list.stack(predictions, fill=TRUE)
predictions_all[is.na(predictions_all)] = 0


}

predictions_full= list.stack(predictions_all, fill=TRUE)
predictions_full[is.na(predictions_full)] = 0

# Stop the clock
proc.time() - ptm


#write_xlsx(predictions_full,"out_of_sample_projections_new2.xlsx")
#write_xlsx(predictions_full,"out_of_sample_projections_check.xlsx")
#write_xlsx(predictions_full,"out_of_sample_and_2023_projections_11_30.xlsx")
write_xlsx(predictions_full,"out_of_sample_and_2023_projections_11_30_redo_slot.xlsx")

