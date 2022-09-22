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


n_drawz<-1000

# Start the clock!
ptm <- proc.time()


state_output = data.frame()
state_cal_output = data.frame()
state_pred_output = data.frame()
year_output = data.frame()


for (x in 1:30){
  ##########
  # Estimate the catch-per-trip copulas so we don't re-estimate every time
  source("calc_catch_per_trip_copulas.R")
  ##########
  
  
  
  ########## 
  # 1) Run the calibration files
  
  source("calibration4 MA.R")
  source("calibration4 RI.R")
  source("calibration4 CT.R")
  source("calibration4 NY.R")
  source("calibration4 NJ.R")
  source("calibration4 DE.R")
  source("calibration4 MD.R")
  source("calibration4 VA.R")
  source("calibration4 NC.R")
  
  
  # Combine the results
  calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                         pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                         pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))
  
  
  
  calibration_output_by_period[is.na(calibration_output_by_period)] = 0
  
  # write_xlsx(calibration_output_by_period,"calibration_output_by_period_lb.xlsx")
  # saveRDS(calibration_output_by_period, file = "calibration_output_by_period_lb.rds")
  
  
  calibration_output_by_period$draw = x
  
  write_xlsx(calibration_output_by_period,"calibration_output_by_period.xlsx")
  saveRDS(calibration_output_by_period, file = "calibration_output_by_period.rds")
  
  rm(utilites_MA, utilites_RI, utilites_CT, utilites_NY, utilites_NJ, utilites_DE, utilites_MD, utilites_VA, utilites_NC )
  
  #aggregate_calibration_output= subset(calibration_output_by_period, select=-c(state, alt_regs, period))
  #aggregate_calibration_output = aggregate(aggregate_calibration_output, by=list(calibration_output_by_period$sim),FUN=sum, na.rm=TRUE)
  
  #write_xlsx(aggregate_calibration_output,"aggregate_calibration_output_lb.xlsx")
  #write_xlsx(aggregate_calibration_output,"aggregate_calibration_output.xlsx")
  state_cal_output =rbind.fill(state_cal_output, calibration_output_by_period)
  
}
write_xlsx(state_cal_output,"calibration_output_by_period.xlsx")