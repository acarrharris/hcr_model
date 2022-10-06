

#The following creates an catch-per-trip dataset adjusted to reflect:
 # 1) Uncertainty in MRIP catch-per-trip estimates, as we draw from different sets of NB parameters
 # 2) the population size



########################
###Massachusetts
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==25 & draw==1)

sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size


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
saveRDS(pred_catch_data_MA, "pred_catch_data_MA.rds")
########################






########################
###Rhode Island 
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==44 & draw==1)

sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size


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
saveRDS(pred_catch_data_RI, "pred_catch_data_RI.rds")
########################



########################
###Connecticut
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==9 & draw==1)

sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size


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
saveRDS(pred_catch_data_CT, "pred_catch_data_CT.rds")
########################




########################
###New York 
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==36 & draw==1)

sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size


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
saveRDS(pred_catch_data_NY, "pred_catch_data_NY.rds")
########################






########################
###New Jersey 
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==34 & draw==1)

sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size


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
saveRDS(pred_catch_data_NJ, "pred_catch_data_NJ.rds")
########################



########################
###Delaware
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==10 & draw==1)

sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size


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
saveRDS(pred_catch_data_DE, "pred_catch_data_DE.rds")
########################






########################
###Marlyand
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==24 & draw==1)

sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size


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
saveRDS(pred_catch_data_MD, "pred_catch_data_MD.rds")
########################





########################
###Virgina 
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==51 & draw==1)

sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size


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
saveRDS(pred_catch_data_VA, "pred_catch_data_VA.rds")
########################






########################
###North Carolina
nb_params<- subset(read_csv("nb_params.csv",  show_col_types = FALSE), state==37 & draw==1)

sf_mu <- nb_params$sf_mu
sf_size <- nb_params$sf_size

bsb_mu <- nb_params$bsb_mu
bsb_size <- nb_params$bsb_size

scup_mu <- nb_params$scup_mu
scup_size <- nb_params$scup_size


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
saveRDS(pred_catch_data_NC, "pred_catch_data_NC.rds")
########################

