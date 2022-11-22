

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

# nb_params<- readRDS("nb_catch_parameters_sf_MA_21.rds")
# sf_mu <- nb_params[["estimate"]][["mu"]]
# sf_size <- nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_bsb_MA_21.rds")
# bsb_mu <-nb_params[["estimate"]][["mu"]]
# bsb_size <- nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_scup_MA_21.rds")
# scup_mu <- nb_params[["estimate"]][["mu"]]
# scup_size <- nb_params[["estimate"]][["size"]]


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year (unceratin estimates)

#####################
#####Summer flounder
#####################

var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*fluke_catch_expansion_factor_MA

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


#####################
#####Sea bass
#####################
var_bsb=bsb_mu+(bsb_mu^2)/bsb_size
cv_bsb_base = sqrt(var_bsb)/bsb_mu
cv_bsb_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
bsb_mu_new=bsb_mu*bsb_catch_expansion_factor_MA

#solve for new size parameter 
bsb_size_new=(bsb_mu_new^2)/(((cv_bsb_base*bsb_mu_new)^2)-bsb_mu_new)

#new variance and CV
var_bsb_new=bsb_mu_new+(bsb_mu_new^2)/bsb_size_new
cv_bsb_new = sqrt(var_bsb_new)/bsb_mu_new

#Check that CV old and CV new are the same
cv_bsb_base
cv_bsb_new



#####################
#####Scup
#####################
var_scup=scup_mu+(scup_mu^2)/scup_size
cv_scup_base = sqrt(var_scup)/scup_mu
cv_scup_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
scup_mu_new=scup_mu*scup_catch_expansion_factor_MA

#solve for new size parameter 
scup_size_new=(scup_mu_new^2)/(((cv_scup_base*scup_mu_new)^2)-scup_mu_new)

#new variance and CV
var_scup_new=scup_mu_new+(scup_mu_new^2)/scup_size_new
cv_scup_new = sqrt(var_scup_new)/scup_mu_new

#Check that CV old and CV new are the same
cv_scup_base
cv_scup_new


###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_MA_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
rho2 <- coef(fit)[2]
rho3 <- coef(fit)[3]
df <- coef(fit)[4]

copula_dist <- mvdc(copula=tCopula(param=c(rho1, rho2, rho3),dim=3,df=df,dispstr = "un"),  margins=c("nbinom", "nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu_new, size=bsb_size_new), 
                                      list(mu=scup_mu_new, size=scup_size_new)))

sim <- rMvdc(10000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
scup_pred_cat=sim[,3]
state="MA"

pred_catch_data_MA=data.frame(sf_pred_cat, bsb_pred_cat, scup_pred_cat, state)
pred_catch_data_MA[is.na(pred_catch_data_MA)] <- 0


#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2021_25.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_MA<- subset(pred_catch_data_MA, pred_catch_data_MA$sf_pred_cat<=max_sf &
                                                pred_catch_data_MA$bsb_pred_cat<=max_bsb &
                                                pred_catch_data_MA$scup_pred_cat<=max_scup)


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

# nb_params<- readRDS("nb_catch_parameters_sf_RI_21.rds")
# sf_mu <- nb_params[["estimate"]][["mu"]]
# sf_size <- nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_bsb_RI_21.rds")
# bsb_mu <- nb_params[["estimate"]][["mu"]]
# bsb_size <-nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_scup_RI_21.rds")
# scup_mu <- nb_params[["estimate"]][["mu"]]
# scup_size <- nb_params[["estimate"]][["size"]]


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year (unceratin estimates)

#####################
#####Summer flounder
#####################

var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*fluke_catch_expansion_factor_RI

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


#####################
#####Sea bass
#####################
var_bsb=bsb_mu+(bsb_mu^2)/bsb_size
cv_bsb_base = sqrt(var_bsb)/bsb_mu
cv_bsb_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
bsb_mu_new=bsb_mu*bsb_catch_expansion_factor_RI

#solve for new size parameter 
bsb_size_new=(bsb_mu_new^2)/(((cv_bsb_base*bsb_mu_new)^2)-bsb_mu_new)

#new variance and CV
var_bsb_new=bsb_mu_new+(bsb_mu_new^2)/bsb_size_new
cv_bsb_new = sqrt(var_bsb_new)/bsb_mu_new

#Check that CV old and CV new are the same
cv_bsb_base
cv_bsb_new



#####################
#####Scup
#####################
var_scup=scup_mu+(scup_mu^2)/scup_size
cv_scup_base = sqrt(var_scup)/scup_mu
cv_scup_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
scup_mu_new=scup_mu*scup_catch_expansion_factor_RI

#solve for new size parameter 
scup_size_new=(scup_mu_new^2)/(((cv_scup_base*scup_mu_new)^2)-scup_mu_new)

#new variance and CV
var_scup_new=scup_mu_new+(scup_mu_new^2)/scup_size_new
cv_scup_new = sqrt(var_scup_new)/scup_mu_new

#Check that CV old and CV new are the same
cv_scup_base
cv_scup_new


###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_RI_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
rho2 <- coef(fit)[2]
rho3 <- coef(fit)[3]
df <- coef(fit)[4]

copula_dist <- mvdc(copula=tCopula(param=c(rho1, rho2, rho3),dim=3,df=df,dispstr = "un"),  margins=c("nbinom", "nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu_new, size=bsb_size_new), 
                                      list(mu=scup_mu_new, size=scup_size_new)))

sim <- rMvdc(10000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
scup_pred_cat=sim[,3]
state="RI"

pred_catch_data_RI=data.frame(sf_pred_cat, bsb_pred_cat, scup_pred_cat, state)
pred_catch_data_RI[is.na(pred_catch_data_RI)] <- 0




#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2021_44.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_RI<- subset(pred_catch_data_RI, pred_catch_data_RI$sf_pred_cat<=max_sf & 
                              pred_catch_data_RI$bsb_pred_cat<=max_bsb &
                              pred_catch_data_RI$scup_pred_cat<=max_scup)

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

# nb_params<- readRDS("nb_catch_parameters_sf_CT_21.rds")
# sf_mu <- nb_params[["estimate"]][["mu"]]
# sf_size <-nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_bsb_CT_21.rds")
# bsb_mu <- nb_params[["estimate"]][["mu"]]
# bsb_size <- nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_scup_CT_21.rds")
# scup_mu <- nb_params[["estimate"]][["mu"]]
# scup_size <- nb_params[["estimate"]][["size"]]


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year (unceratin estimates)

#####################
#####Summer flounder
#####################

var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*fluke_catch_expansion_factor_CT

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


#####################
#####Sea bass
#####################
var_bsb=bsb_mu+(bsb_mu^2)/bsb_size
cv_bsb_base = sqrt(var_bsb)/bsb_mu
cv_bsb_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
bsb_mu_new=bsb_mu*bsb_catch_expansion_factor_CT

#solve for new size parameter 
bsb_size_new=(bsb_mu_new^2)/(((cv_bsb_base*bsb_mu_new)^2)-bsb_mu_new)

#new variance and CV
var_bsb_new=bsb_mu_new+(bsb_mu_new^2)/bsb_size_new
cv_bsb_new = sqrt(var_bsb_new)/bsb_mu_new

#Check that CV old and CV new are the same
cv_bsb_base
cv_bsb_new



#####################
#####Scup
#####################
var_scup=scup_mu+(scup_mu^2)/scup_size
cv_scup_base = sqrt(var_scup)/scup_mu
cv_scup_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
scup_mu_new=scup_mu*scup_catch_expansion_factor_CT

#solve for new size parameter 
scup_size_new=(scup_mu_new^2)/(((cv_scup_base*scup_mu_new)^2)-scup_mu_new)

#new variance and CV
var_scup_new=scup_mu_new+(scup_mu_new^2)/scup_size_new
cv_scup_new = sqrt(var_scup_new)/scup_mu_new

#Check that CV old and CV new are the same
cv_scup_base
cv_scup_new


###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_CT_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
rho2 <- coef(fit)[2]
rho3 <- coef(fit)[3]
df <- coef(fit)[4]

copula_dist <- mvdc(copula=tCopula(param=c(rho1, rho2, rho3),dim=3,df=df,dispstr = "un"),  margins=c("nbinom", "nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu_new, size=bsb_size_new), 
                                      list(mu=scup_mu_new, size=scup_size_new)))

sim <- rMvdc(10000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
scup_pred_cat=sim[,3]
state="CT"

pred_catch_data_CT=data.frame(sf_pred_cat, bsb_pred_cat, scup_pred_cat, state)
pred_catch_data_CT[is.na(pred_catch_data_CT)] <- 0


#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2021_9.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_CT<- subset(pred_catch_data_CT, pred_catch_data_CT$sf_pred_cat<=max_sf & 
                              pred_catch_data_CT$bsb_pred_cat<=max_bsb &
                              pred_catch_data_CT$scup_pred_cat<=max_scup)


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

# nb_params<- readRDS("nb_catch_parameters_sf_NY_21.rds")
# sf_mu <-nb_params[["estimate"]][["mu"]]
# sf_size <- nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_bsb_NY_21.rds")
# bsb_mu <- nb_params[["estimate"]][["mu"]]
# bsb_size <- nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_scup_NY_21.rds")
# scup_mu <- nb_params[["estimate"]][["mu"]]
# scup_size <- nb_params[["estimate"]][["size"]]


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year (unceratin estimates)

#####################
#####Summer flounder
#####################

var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*fluke_catch_expansion_factor_NY

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


#####################
#####Sea bass
#####################
var_bsb=bsb_mu+(bsb_mu^2)/bsb_size
cv_bsb_base = sqrt(var_bsb)/bsb_mu
cv_bsb_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
bsb_mu_new=bsb_mu*bsb_catch_expansion_factor_NY

#solve for new size parameter 
bsb_size_new=(bsb_mu_new^2)/(((cv_bsb_base*bsb_mu_new)^2)-bsb_mu_new)

#new variance and CV
var_bsb_new=bsb_mu_new+(bsb_mu_new^2)/bsb_size_new
cv_bsb_new = sqrt(var_bsb_new)/bsb_mu_new

#Check that CV old and CV new are the same
cv_bsb_base
cv_bsb_new



#####################
#####Scup
#####################
var_scup=scup_mu+(scup_mu^2)/scup_size
cv_scup_base = sqrt(var_scup)/scup_mu
cv_scup_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
scup_mu_new=scup_mu*scup_catch_expansion_factor_NY

#solve for new size parameter 
scup_size_new=(scup_mu_new^2)/(((cv_scup_base*scup_mu_new)^2)-scup_mu_new)

#new variance and CV
var_scup_new=scup_mu_new+(scup_mu_new^2)/scup_size_new
cv_scup_new = sqrt(var_scup_new)/scup_mu_new

#Check that CV old and CV new are the same
cv_scup_base
cv_scup_new


###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_NY_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
rho2 <- coef(fit)[2]
rho3 <- coef(fit)[3]
df <- coef(fit)[4]

copula_dist <- mvdc(copula=tCopula(param=c(rho1, rho2, rho3),dim=3,df=df,dispstr = "un"),  margins=c("nbinom", "nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu_new, size=bsb_size_new), 
                                      list(mu=scup_mu_new, size=scup_size_new)))

sim <- rMvdc(10000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
scup_pred_cat=sim[,3]
state="NY"

pred_catch_data_NY=data.frame(sf_pred_cat, bsb_pred_cat, scup_pred_cat, state)
pred_catch_data_NY[is.na(pred_catch_data_NY)] <- 0


#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2021_36.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_NY<- subset(pred_catch_data_NY, pred_catch_data_NY$sf_pred_cat<=max_sf & 
                              pred_catch_data_NY$bsb_pred_cat<=max_bsb &
                              pred_catch_data_NY$scup_pred_cat<=max_scup)

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

# nb_params<- readRDS("nb_catch_parameters_sf_NJ_21.rds")
# sf_mu <- nb_params[["estimate"]][["mu"]]
# sf_size <- nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_bsb_NJ_21.rds")
# bsb_mu <- nb_params[["estimate"]][["mu"]]
# bsb_size <- nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_scup_NJ_21.rds")
# scup_mu <- nb_params[["estimate"]][["mu"]]
# scup_size <- nb_params[["estimate"]][["size"]]


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year (unceratin estimates)

#####################
#####Summer flounder
#####################

var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*fluke_catch_expansion_factor_NJ

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


#####################
#####Sea bass
#####################
var_bsb=bsb_mu+(bsb_mu^2)/bsb_size
cv_bsb_base = sqrt(var_bsb)/bsb_mu
cv_bsb_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
bsb_mu_new=bsb_mu*bsb_catch_expansion_factor_NJ

#solve for new size parameter 
bsb_size_new=(bsb_mu_new^2)/(((cv_bsb_base*bsb_mu_new)^2)-bsb_mu_new)

#new variance and CV
var_bsb_new=bsb_mu_new+(bsb_mu_new^2)/bsb_size_new
cv_bsb_new = sqrt(var_bsb_new)/bsb_mu_new

#Check that CV old and CV new are the same
cv_bsb_base
cv_bsb_new



#####################
#####Scup
#####################
var_scup=scup_mu+(scup_mu^2)/scup_size
cv_scup_base = sqrt(var_scup)/scup_mu
cv_scup_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
scup_mu_new=scup_mu*scup_catch_expansion_factor_NJ

#solve for new size parameter 
scup_size_new=(scup_mu_new^2)/(((cv_scup_base*scup_mu_new)^2)-scup_mu_new)

#new variance and CV
var_scup_new=scup_mu_new+(scup_mu_new^2)/scup_size_new
cv_scup_new = sqrt(var_scup_new)/scup_mu_new

#Check that CV old and CV new are the same
cv_scup_base
cv_scup_new


###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_NJ_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
rho2 <- coef(fit)[2]
rho3 <- coef(fit)[3]
df <- coef(fit)[4]

copula_dist <- mvdc(copula=tCopula(param=c(rho1, rho2, rho3),dim=3,df=df,dispstr = "un"),  margins=c("nbinom", "nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu_new, size=bsb_size_new), 
                                      list(mu=scup_mu_new, size=scup_size_new)))

sim <- rMvdc(10000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
scup_pred_cat=sim[,3]
state="NJ"

pred_catch_data_NJ=data.frame(sf_pred_cat, bsb_pred_cat, scup_pred_cat, state)
pred_catch_data_NJ[is.na(pred_catch_data_NJ)] <- 0

#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2021_34.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_NJ<- subset(pred_catch_data_NJ, pred_catch_data_NJ$sf_pred_cat<=max_sf & 
                              pred_catch_data_NJ$bsb_pred_cat<=max_bsb &
                              pred_catch_data_NJ$scup_pred_cat<=max_scup)

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
# 
# nb_params<- readRDS("nb_catch_parameters_sf_DE_21.rds")
# sf_mu <- nb_params[["estimate"]][["mu"]]
# sf_size <- nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_bsb_DE_21.rds")
# bsb_mu <- nb_params[["estimate"]][["mu"]]
# bsb_size <- nb_params[["estimate"]][["size"]]




# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year (unceratin estimates)

#####################
#####Summer flounder
#####################

var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*fluke_catch_expansion_factor_DE

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


#####################
#####Sea bass
#####################
var_bsb=bsb_mu+(bsb_mu^2)/bsb_size
cv_bsb_base = sqrt(var_bsb)/bsb_mu
cv_bsb_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
bsb_mu_new=bsb_mu*bsb_catch_expansion_factor_DE

#solve for new size parameter 
bsb_size_new=(bsb_mu_new^2)/(((cv_bsb_base*bsb_mu_new)^2)-bsb_mu_new)

#new variance and CV
var_bsb_new=bsb_mu_new+(bsb_mu_new^2)/bsb_size_new
cv_bsb_new = sqrt(var_bsb_new)/bsb_mu_new

#Check that CV old and CV new are the same
cv_bsb_base
cv_bsb_new



#####################
#####OMIT SCUP 
#####################

###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_DE_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
df <- coef(fit)[2]

copula_dist <- mvdc(copula=tCopula(param=rho1,dim=2,df=df,dispstr = "un"),  margins=c("nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu_new, size=bsb_size_new)))

sim <- rMvdc(10000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
state="DE"

pred_catch_data_DE=data.frame(sf_pred_cat, bsb_pred_cat, state)
pred_catch_data_DE[is.na(pred_catch_data_DE)] <- 0

#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2021_10.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_DE<- subset(pred_catch_data_DE, pred_catch_data_DE$sf_pred_cat<=max_sf & 
                              pred_catch_data_DE$bsb_pred_cat<=max_bsb)


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

# nb_params<- readRDS("nb_catch_parameters_sf_MD_21.rds")
# sf_mu <-  nb_params[["estimate"]][["mu"]]
# sf_size <-  nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_bsb_MD_21.rds")
# bsb_mu <-  nb_params[["estimate"]][["mu"]]
# bsb_size <-  nb_params[["estimate"]][["size"]]



# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year (unceratin estimates)

#####################
#####Summer flounder
#####################

var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*fluke_catch_expansion_factor_MD

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


#####################
#####Sea bass
#####################
var_bsb=bsb_mu+(bsb_mu^2)/bsb_size
cv_bsb_base = sqrt(var_bsb)/bsb_mu
cv_bsb_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
bsb_mu_new=bsb_mu*bsb_catch_expansion_factor_MD

#solve for new size parameter 
bsb_size_new=(bsb_mu_new^2)/(((cv_bsb_base*bsb_mu_new)^2)-bsb_mu_new)

#new variance and CV
var_bsb_new=bsb_mu_new+(bsb_mu_new^2)/bsb_size_new
cv_bsb_new = sqrt(var_bsb_new)/bsb_mu_new

#Check that CV old and CV new are the same
cv_bsb_base
cv_bsb_new



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

copula_dist <- mvdc(copula=tCopula(param=rho1,dim=2,df=df,dispstr = "un"),  margins=c("nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu_new, size=bsb_size_new)))

sim <- rMvdc(10000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
state="MD"

pred_catch_data_MD=data.frame(sf_pred_cat, bsb_pred_cat, state)
pred_catch_data_MD[is.na(pred_catch_data_MD)] <- 0

#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2021_24.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_MD<- subset(pred_catch_data_MD, pred_catch_data_MD$sf_pred_cat<=max_sf & 
                              pred_catch_data_MD$bsb_pred_cat<=max_bsb)

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

# nb_params<- readRDS("nb_catch_parameters_sf_VA_21.rds")
# sf_mu <- nb_params[["estimate"]][["mu"]]
# sf_size <- nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_bsb_VA_21.rds")
# bsb_mu <- nb_params[["estimate"]][["mu"]]
# bsb_size <- nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_scup_VA_21.rds")
# scup_mu <- nb_params[["estimate"]][["mu"]]
# scup_size <- nb_params[["estimate"]][["size"]]


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year (unceratin estimates)

#####################
#####Summer flounder
#####################

var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*fluke_catch_expansion_factor_VA

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


#####################
#####Sea bass
#####################
var_bsb=bsb_mu+(bsb_mu^2)/bsb_size
cv_bsb_base = sqrt(var_bsb)/bsb_mu
cv_bsb_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
bsb_mu_new=bsb_mu*bsb_catch_expansion_factor_VA

#solve for new size parameter 
bsb_size_new=(bsb_mu_new^2)/(((cv_bsb_base*bsb_mu_new)^2)-bsb_mu_new)

#new variance and CV
var_bsb_new=bsb_mu_new+(bsb_mu_new^2)/bsb_size_new
cv_bsb_new = sqrt(var_bsb_new)/bsb_mu_new

#Check that CV old and CV new are the same
cv_bsb_base
cv_bsb_new



#####################
#####Scup
#####################
var_scup=scup_mu+(scup_mu^2)/scup_size
cv_scup_base = sqrt(var_scup)/scup_mu
cv_scup_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
scup_mu_new=scup_mu*scup_catch_expansion_factor_VA

#solve for new size parameter 
scup_size_new=(scup_mu_new^2)/(((cv_scup_base*scup_mu_new)^2)-scup_mu_new)

#new variance and CV
var_scup_new=scup_mu_new+(scup_mu_new^2)/scup_size_new
cv_scup_new = sqrt(var_scup_new)/scup_mu_new

#Check that CV old and CV new are the same
cv_scup_base
cv_scup_new


###########
###########


#Obtain the copula fit 
fit <- readRDS("catch_copula_VA_21.rds")

# Set the parameters
rho1 <- coef(fit)[1]
rho2 <- coef(fit)[2]
rho3 <- coef(fit)[3]
df <- coef(fit)[4]

copula_dist <- mvdc(copula=tCopula(param=c(rho1, rho2, rho3),dim=3,df=df,dispstr = "un"),  margins=c("nbinom", "nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu_new, size=bsb_size_new), 
                                      list(mu=scup_mu_new, size=scup_size_new)))

sim <- rMvdc(10000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
scup_pred_cat=sim[,3]
state="VA"

pred_catch_data_VA=data.frame(sf_pred_cat, bsb_pred_cat, scup_pred_cat, state)
pred_catch_data_VA[is.na(pred_catch_data_VA)] <- 0

#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2021_51.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_bsb<- max(observed_21$bsb_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_VA<- subset(pred_catch_data_VA, pred_catch_data_VA$sf_pred_cat<=max_sf & 
                              pred_catch_data_VA$bsb_pred_cat<=max_bsb &
                              pred_catch_data_VA$scup_pred_cat<=max_scup)


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

# nb_params<- readRDS("nb_catch_parameters_sf_NC_21.rds")
# sf_mu <-  nb_params[["estimate"]][["mu"]]
# sf_size <-  nb_params[["estimate"]][["size"]]
# 
# nb_params<- readRDS("nb_catch_parameters_bsb_NC_21.rds")
# bsb_mu <-  nb_params[["estimate"]][["mu"]]
# bsb_size <- nb_params[["estimate"]][["size"]]



# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year (unceratin estimates)

#####################
#####Summer flounder
#####################

var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*fluke_catch_expansion_factor_NC

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


#####################
#####Sea bass
#####################
var_bsb=bsb_mu+(bsb_mu^2)/bsb_size
cv_bsb_base = sqrt(var_bsb)/bsb_mu
cv_bsb_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
bsb_mu_new=bsb_mu*bsb_catch_expansion_factor_NC

#solve for new size parameter 
bsb_size_new=(bsb_mu_new^2)/(((cv_bsb_base*bsb_mu_new)^2)-bsb_mu_new)

#new variance and CV
var_bsb_new=bsb_mu_new+(bsb_mu_new^2)/bsb_size_new
cv_bsb_new = sqrt(var_bsb_new)/bsb_mu_new

#Check that CV old and CV new are the same
cv_bsb_base
cv_bsb_new



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

copula_dist <- mvdc(copula=tCopula(param=rho1,dim=2,df=df,dispstr = "un"),  margins=c("nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu_new, size=bsb_size_new)))

sim <- rMvdc(10000, copula_dist )

sf_pred_cat=sim[,1]
bsb_pred_cat=sim[,2]
state="NC"

pred_catch_data_NC=data.frame(sf_pred_cat, bsb_pred_cat, state)
pred_catch_data_NC[is.na(pred_catch_data_NC)] <- 0



#Remove outliers of scup, bsb, sf catch
observed_21<- read_csv("observed_catch_2021_37.csv",  show_col_types = FALSE)
max_sf<- max(observed_21$sf_tot_cat)
max_scup<- max(observed_21$scup_tot_cat)



pred_catch_data_NC<- subset(pred_catch_data_NC, pred_catch_data_NC$sf_pred_cat<=max_sf & 
                              pred_catch_data_NC$bsb_pred_cat<=max_bsb)


saveRDS(pred_catch_data_NC, "pred_catch_data_NC.rds")
########################


