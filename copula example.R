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


####################
#Massachusetts

observed_catch_data <- subset(read.csv("observed_catch_2021_25.csv"), select=c(sf_tot_cat, bsb_tot_cat, scup_tot_cat))

sf_cat <- observed_catch_data$sf_tot_cat
bsb_cat <- observed_catch_data$bsb_tot_cat
scup_cat <- observed_catch_data$scup_tot_cat


#SF 
nbfit_sf = fitdistr(sf_cat, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_MA_21.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_size <- nbfit_sf$estimate['size']


#BSB 
nbfit_bsb <- fitdistr(bsb_cat, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_MA_21.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_size <- nbfit_bsb$estimate['size']


#Scup 
nbfit_scup <- fitdistr(scup_cat, "Negative Binomial")
saveRDS(nbfit_scup, "nb_catch_parameters_scup_MA_21.rds")

scup_mu <- nbfit_scup$estimate['mu']
scup_size <- nbfit_scup$estimate['size']


#Two species
set.seed(500)
t.cop <- tCopula(dim=2,dispstr = "un")
observed_catch_data<- subset(observed_catch_data, select=c(sf_tot_cat, bsb_tot_cat))
m <- pobs(as.matrix(observed_catch_data))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)


#saveRDS(fit, "catch_copula_MA_21.rds")
#fit <- readRDS("catch_copula_MA_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
df <- coef(fit)[2]


#You can change the parameters of the distirbtion in the paramMargins list
copula_dist <- mvdc(copula=tCopula(rho1, dim=2, df=df,dispstr = "un"),  margins=c("nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim <- rMvdc(10000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
state="MA"

pred_catch_data_MA=data.frame(sf_pred_cat, bsb_pred_cat,  state)
pred_catch_data_MA[is.na(pred_catch_data_MA)] <- 0



#Simulated and observed data should be similar in mean catch-per-trip and correlation
mean(pred_catch_data_MA$sf_pred_cat)
mean(observed_catch_data$sf_tot_cat)

mean(pred_catch_data_MA$bsb_pred_cat)
mean(observed_catch_data$bsb_tot_cat)

cor(observed_catch_data$sf_tot_cat, observed_catch_data$bsb_tot_cat, method = c("spearman"))
cor(pred_catch_data_MA$sf_pred_cat, pred_catch_data_MA$bsb_pred_cat, method = c("spearman"))

