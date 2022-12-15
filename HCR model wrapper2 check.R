

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
                 "plyr" , "furr", "profvis")
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

observed_catch_2018_2022_44 <- read_csv("observed_catch_2018_2022_44.csv",   show_col_types = FALSE)
saveRDS(observed_catch_2018_2022_44, "observed_catch_2018_2022_44.rds")

observed_catch_2018_2022_9 <- read_csv("observed_catch_2018_2022_9.csv",  show_col_types = FALSE)
saveRDS(observed_catch_2018_2022_9, "observed_catch_2018_2022_9.rds")

observed_catch_2018_2022_10 <- read_csv("observed_catch_2018_2022_10.csv",  show_col_types = FALSE)
saveRDS(observed_catch_2018_2022_10, "observed_catch_2018_2022_10.rds")

observed_catch_2018_2022_24 <- read_csv("observed_catch_2018_2022_24.csv",  show_col_types = FALSE)
saveRDS(observed_catch_2018_2022_24, "observed_catch_2018_2022_24.rds")

observed_catch_2018_2022_25 <- read_csv("observed_catch_2018_2022_25.csv",  show_col_types = FALSE)
saveRDS(observed_catch_2018_2022_25, "observed_catch_2018_2022_25.rds")

observed_catch_2018_2022_34 <- read_csv("observed_catch_2018_2022_34.csv",  show_col_types = FALSE)
saveRDS(observed_catch_2018_2022_34, "observed_catch_2018_2022_34.rds")

observed_catch_2018_2022_36 <- read_csv("observed_catch_2018_2022_36.csv",  show_col_types = FALSE)
saveRDS(observed_catch_2018_2022_36, "observed_catch_2018_2022_36.rds")

observed_catch_2018_2022_37 <- read_csv("observed_catch_2018_2022_37.csv",  show_col_types = FALSE)
saveRDS(observed_catch_2018_2022_37, "observed_catch_2018_2022_37.rds")

observed_catch_2018_2022_51 <- read_csv("observed_catch_2018_2022_51.csv",  show_col_types = FALSE)
saveRDS(observed_catch_2018_2022_51, "observed_catch_2018_2022_51.rds")

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
source("catch_function check.R")



parallelly::availableCores()
future::plan(multisession, workers=8)

# Start the clock!
ptm <- proc.time()



predictions = list()
predictions_all = list()


  
#years<-c( "2018", "2019", "2020", "2022")
#years<-c( "2018", "2019", "2020", "2022", "2023.1", "2023.2", "2023.3", "2023.4", "2023.5","2023.6", "2023.7", "2024", "2025" )
#years<-c( "2018",  "2023.4" )
#years<-c( "2022", "2023.1", "2023.4", "2023.5","2023.6", "2023.7", "2025" )
years<-c( "2023.1")
for (x in 1:1){
  for (y in years){
    
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
    if (year %in% c("2023.1", "2023.2", "2023.3", "2023.4", "2023.5","2023.6", "2023.7", "2024", "2025", 
                    "2023.8", "2023.9", "2025.1")){
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
    
    

    
    params <- list(state1 = c("CT", "DE", "MA", "MD", "NC", "NJ","NY", "RI", "VA"),
                   calibration_data_table = calibration_data_table_base,
                   directed_trips_table = directed_trips_table_base,
                   sf_size_data_read = sf_size_data_read_base,
                   bsb_size_data_read = bsb_size_data_read_base,
                   scup_size_data_read = scup_size_data_read_base,
                   param_draws_MA = c(list(param_draws_CT), list(param_draws_DE), list(param_draws_MA),
                                      list(param_draws_MD), list(param_draws_NC), list(param_draws_NJ),
                                      list(param_draws_NY), list(param_draws_RI), list(param_draws_VA)),
                   costs_new_all = c(list(costs_new_all_CT), list(costs_new_all_DE), list(costs_new_all_MA),
                                     list(costs_new_all_MD), list(costs_new_all_NC), list(costs_new_all_NJ),
                                     list(costs_new_all_NY), list(costs_new_all_RI), list(costs_new_all_VA)),
                   sf_catch_data_all = c(list(sf_catch_data_ct),list(sf_catch_data_de),list(sf_catch_data_ma),
                                         list(sf_catch_data_md),list(sf_catch_data_nc),list(sf_catch_data_nj),
                                         list(sf_catch_data_ny),list(sf_catch_data_ri), list(sf_catch_data_va)))
    
    safe_predict_rec_catch <- purrr::safely(predict_rec_catch, otherwise = NA_real_)
    

    xx_check <-  future_pmap(params, safe_predict_rec_catch, .options = furrr_options(seed = 32190))

    prediction_output_by_period1 <- future_map(xx_check, 1)
    
    
    
    predictions[[y]]<- list.stack(prediction_output_by_period1, fill=TRUE)
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
#write_xlsx(predictions_full,"out_of_sample_and_2023_projections_11_30_redo_slot.xlsx")
write_xlsx(predictions_full,"test_vals_simultaneous.xlsx")

