

#The following creates an catch-per-trip dataset adjusted to reflect:
# 1) Uncertainty in MRIP catch-per-trip estimates, as we draw from different sets of NB parameters. 
#    The file nb_params.csv contains a distribution of parameters based on uncertainty in catch per-trip in 2021
# 2) the population size. This is reflected in the species_catch_expansion_factor_ST scalars 
#    computed in "CAL given stock structure.R" 



########################
###Massachusetts
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==25 & draw==x)
sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size



###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_MA_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
df <- coef(fit)[2]

copula_dist <- mvdc(copula=tCopula(dim=2, rho1, df=df),  margins=c("nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim <- rMvdc(30000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
state="MA"

pred_catch_data_MA=data.frame(sf_pred_cat, bsb_pred_cat, state)
pred_catch_data_MA[is.na(pred_catch_data_MA)] <- 0


#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2018_2022_25.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_MA<- subset(pred_catch_data_MA, pred_catch_data_MA$sf_pred_cat<=max_sf &
                              pred_catch_data_MA$bsb_pred_cat<=max_bsb)

mean(pred_catch_data_MA$sf_pred_cat)
mean(observed_21$sf_tot_cat)

saveRDS(pred_catch_data_MA, "pred_catch_data_MA.rds")
########################






########################
###Rhode Island 
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==44 & draw==x)
sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size


#Obtain the copula fit 
fit <- readRDS("catch_copula_RI_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
df <- coef(fit)[2]

copula_dist <- mvdc(copula=tCopula(dim=2, rho1, df=df),  margins=c("nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim <- rMvdc(30000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
state="RI"

pred_catch_data_RI=data.frame(sf_pred_cat, bsb_pred_cat, state)
pred_catch_data_RI[is.na(pred_catch_data_RI)] <- 0


#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2018_2022_44.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_RI<- subset(pred_catch_data_RI, pred_catch_data_RI$sf_pred_cat<=max_sf &
                              pred_catch_data_RI$bsb_pred_cat<=max_bsb)

mean(pred_catch_data_RI$sf_pred_cat)
mean(observed_21$sf_tot_cat)

saveRDS(pred_catch_data_RI, "pred_catch_data_RI.rds")
########################



########################
###Connecticut
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==9 & draw==x)
sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size



#Obtain the copula fit 
fit <- readRDS("catch_copula_CT_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
df <- coef(fit)[2]

copula_dist <- mvdc(copula=tCopula(dim=2, rho1, df=df),  margins=c("nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim <- rMvdc(30000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
state="CT"

pred_catch_data_CT=data.frame(sf_pred_cat, bsb_pred_cat, state)
pred_catch_data_CT[is.na(pred_catch_data_CT)] <- 0


#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2018_2022_9.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_CT<- subset(pred_catch_data_CT, pred_catch_data_CT$sf_pred_cat<=max_sf &
                              pred_catch_data_CT$bsb_pred_cat<=max_bsb)

mean(pred_catch_data_CT$sf_pred_cat)
mean(observed_21$sf_tot_cat)

saveRDS(pred_catch_data_CT, "pred_catch_data_CT.rds")
########################




########################
###New York 
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==36 & draw==x)
sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size



###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_NY_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
df <- coef(fit)[2]

copula_dist <- mvdc(copula=tCopula(dim=2, rho1, df=df),  margins=c("nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim <- rMvdc(30000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
state="NY"

pred_catch_data_NY=data.frame(sf_pred_cat, bsb_pred_cat, state)
pred_catch_data_NY[is.na(pred_catch_data_NY)] <- 0


#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2018_2022_36.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_NY<- subset(pred_catch_data_NY, pred_catch_data_NY$sf_pred_cat<=max_sf &
                              pred_catch_data_NY$bsb_pred_cat<=max_bsb)

mean(pred_catch_data_NY$sf_pred_cat)
mean(observed_21$sf_tot_cat)

saveRDS(pred_catch_data_NY, "pred_catch_data_NY.rds")
########################






########################
###New Jersey 
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==34 & draw==x)
sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size



###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_NJ_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
df <- coef(fit)[2]

copula_dist <- mvdc(copula=tCopula(dim=2, rho1, df=df),  margins=c("nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim <- rMvdc(30000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
state="NJ"

pred_catch_data_NJ=data.frame(sf_pred_cat, bsb_pred_cat, state)
pred_catch_data_NJ[is.na(pred_catch_data_NJ)] <- 0


#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2018_2022_34.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_NJ<- subset(pred_catch_data_NJ, pred_catch_data_NJ$sf_pred_cat<=max_sf &
                              pred_catch_data_NJ$bsb_pred_cat<=max_bsb)

mean(pred_catch_data_NJ$sf_pred_cat)
mean(observed_21$sf_tot_cat)

saveRDS(pred_catch_data_NJ, "pred_catch_data_NJ.rds")
########################



########################
###Delaware
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==10 & draw==x)
sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size

###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_DE_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
df <- coef(fit)[2]

copula_dist <- mvdc(copula=tCopula(dim=2, rho1, df=df),  margins=c("nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim <- rMvdc(30000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
state="DE"

pred_catch_data_DE=data.frame(sf_pred_cat, bsb_pred_cat, state)
pred_catch_data_DE[is.na(pred_catch_data_DE)] <- 0


#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2018_2022_10.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_DE<- subset(pred_catch_data_DE, pred_catch_data_DE$sf_pred_cat<=max_sf &
                              pred_catch_data_DE$bsb_pred_cat<=max_bsb)

mean(pred_catch_data_DE$sf_pred_cat)
mean(observed_21$sf_tot_cat)

saveRDS(pred_catch_data_DE, "pred_catch_data_DE.rds")
########################






########################
###Marlyand
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==24 & draw==x)
sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size



#####################
#####OMIT SCUP 
#####################

###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_MD_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
df <- coef(fit)[2]

copula_dist <- mvdc(copula=tCopula(dim=2, rho1, df=df),  margins=c("nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim <- rMvdc(30000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
state="MD"

pred_catch_data_MD=data.frame(sf_pred_cat, bsb_pred_cat, state)
pred_catch_data_MD[is.na(pred_catch_data_MD)] <- 0


#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2018_2022_24.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_MD<- subset(pred_catch_data_MD, pred_catch_data_MD$sf_pred_cat<=max_sf &
                              pred_catch_data_MD$bsb_pred_cat<=max_bsb)

mean(pred_catch_data_MD$sf_pred_cat)
mean(observed_21$sf_tot_cat)

saveRDS(pred_catch_data_MD, "pred_catch_data_MD.rds")
########################





########################
###Virgina 
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==51 & draw==x)
sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size


###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_VA_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
df <- coef(fit)[2]

copula_dist <- mvdc(copula=tCopula(dim=2, rho1, df=df),  margins=c("nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim <- rMvdc(30000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
state="VA"

pred_catch_data_VA=data.frame(sf_pred_cat, bsb_pred_cat, state)
pred_catch_data_VA[is.na(pred_catch_data_VA)] <- 0


#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2018_2022_51.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_VA<- subset(pred_catch_data_VA, pred_catch_data_VA$sf_pred_cat<=max_sf &
                              pred_catch_data_VA$bsb_pred_cat<=max_bsb)

mean(pred_catch_data_VA$sf_pred_cat)
mean(observed_21$sf_tot_cat)

saveRDS(pred_catch_data_VA, "pred_catch_data_VA.rds")
########################






########################
###North Carolina
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==37 & draw==x)
sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size




#####################
#####OMIT SCUP 
#####################

###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_NC_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
df <- coef(fit)[2]

copula_dist <- mvdc(copula=tCopula(dim=2, rho1, df=df),  margins=c("nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim <- rMvdc(30000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
state="NC"

pred_catch_data_NC=data.frame(sf_pred_cat, bsb_pred_cat, state)
pred_catch_data_NC[is.na(pred_catch_data_NC)] <- 0


#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2018_2022_37.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_NC<- subset(pred_catch_data_NC, pred_catch_data_NC$sf_pred_cat<=max_sf &
                              pred_catch_data_NC$bsb_pred_cat<=max_bsb)

mean(pred_catch_data_NC$sf_pred_cat)
mean(observed_21$sf_tot_cat)

saveRDS(pred_catch_data_NC, "pred_catch_data_NC.rds")
########################


