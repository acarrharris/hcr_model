
#This code calculates the calibration year copula parameters by state


####################
#Massachusetts

observed_catch_data <- subset(read.csv("observed_catch_2021_25.csv"), select=c(sf_tot_cat, bsb_tot_cat, scup_tot_cat))
sf_cat <- observed_catch_data$sf_tot_cat
bsb_cat <- observed_catch_data$bsb_tot_cat
scup_cat <- observed_catch_data$scup_tot_cat

nbfit_sf = fitdistr(sf_cat, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_MA_21.rds")

sf_mu <- nbfit_sf$estimate['mu']

sf_mu
mean(sf_cat)


sf_size <- nbfit_sf$estimate['size']
sf_size


#BSB 
nbfit_bsb <- fitdistr(bsb_cat, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_MA_21.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
mean(bsb_cat)

bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#Scup 
nbfit_scup <- fitdistr(scup_cat, "Negative Binomial")
saveRDS(nbfit_scup, "nb_catch_parameters_scup_MA_21.rds")

scup_mu <- nbfit_scup$estimate['mu']
scup_mu
mean(scup_cat)

scup_size <- nbfit_scup$estimate['size']
scup_size

set.seed(500)
t.cop <- tCopula(dim=3,dispstr = "un")
m <- pobs(as.matrix(observed_catch_data))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)


saveRDS(fit, "catch_copula_MA_21.rds")
####################




####################
#Rhode Island 

observed_catch_data <- subset(read.csv("observed_catch_2021_44.csv"), select=c(sf_tot_cat, bsb_tot_cat, scup_tot_cat))
sf_cat <- observed_catch_data$sf_tot_cat
bsb_cat <- observed_catch_data$bsb_tot_cat
scup_cat <- observed_catch_data$scup_tot_cat

nbfit_sf = fitdistr(sf_cat, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_RI_21.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu
mean(sf_cat)


sf_size <- nbfit_sf$estimate['size']
sf_size


#BSB 
nbfit_bsb <- fitdistr(bsb_cat, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_RI_21.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
mean(bsb_cat)

bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#Scup 
nbfit_scup <- fitdistr(scup_cat, "Negative Binomial")
saveRDS(nbfit_scup, "nb_catch_parameters_scup_RI_21.rds")

scup_mu <- nbfit_scup$estimate['mu']
scup_mu
mean(scup_cat)

scup_size <- nbfit_scup$estimate['size']
scup_size

set.seed(500)
t.cop <- tCopula(dim=3,dispstr = "un")
m <- pobs(as.matrix(observed_catch_data))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)
saveRDS(fit, "catch_copula_RI_21.rds")
####################






####################
#Connecticut 

observed_catch_data <- subset(read.csv("observed_catch_2021_9.csv"), select=c(sf_tot_cat, bsb_tot_cat, scup_tot_cat))
sf_cat <- observed_catch_data$sf_tot_cat
bsb_cat <- observed_catch_data$bsb_tot_cat
scup_cat <- observed_catch_data$scup_tot_cat

nbfit_sf = fitdistr(sf_cat, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_CT_21.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu
mean(sf_cat)


sf_size <- nbfit_sf$estimate['size']
sf_size


#BSB 
nbfit_bsb <- fitdistr(bsb_cat, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_CT_21.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
mean(bsb_cat)

bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#Scup 
nbfit_scup <- fitdistr(scup_cat, "Negative Binomial")
saveRDS(nbfit_scup, "nb_catch_parameters_scup_CT_21.rds")

scup_mu <- nbfit_scup$estimate['mu']
scup_mu
mean(scup_cat)

scup_size <- nbfit_scup$estimate['size']
scup_size


set.seed(500)
t.cop <- tCopula(dim=3,dispstr = "un")
m <- pobs(as.matrix(observed_catch_data))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)
saveRDS(fit, "catch_copula_CT_21.rds")
####################




####################
#New York 

observed_catch_data <- subset(read.csv("observed_catch_2021_36.csv"), select=c(sf_tot_cat, bsb_tot_cat, scup_tot_cat))
sf_cat <- observed_catch_data$sf_tot_cat
bsb_cat <- observed_catch_data$bsb_tot_cat
scup_cat <- observed_catch_data$scup_tot_cat

nbfit_sf = fitdistr(sf_cat, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_NY_21.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu
mean(sf_cat)


sf_size <- nbfit_sf$estimate['size']
sf_size


#BSB 
nbfit_bsb <- fitdistr(bsb_cat, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_NY_21.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
mean(bsb_cat)

bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#Scup 
nbfit_scup <- fitdistr(scup_cat, "Negative Binomial")
saveRDS(nbfit_scup, "nb_catch_parameters_scup_NY_21.rds")

scup_mu <- nbfit_scup$estimate['mu']
scup_mu
mean(scup_cat)

scup_size <- nbfit_scup$estimate['size']
scup_size


set.seed(500)
t.cop <- tCopula(dim=3,dispstr = "un")
m <- pobs(as.matrix(observed_catch_data))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)
saveRDS(fit, "catch_copula_NY_21.rds")
####################



####################
#New Jersey  

observed_catch_data <- subset(read.csv("observed_catch_2021_34.csv"), select=c(sf_tot_cat, bsb_tot_cat, scup_tot_cat))
sf_cat <- observed_catch_data$sf_tot_cat
bsb_cat <- observed_catch_data$bsb_tot_cat
scup_cat <- observed_catch_data$scup_tot_cat

nbfit_sf = fitdistr(sf_cat, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_NJ_21.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu
mean(sf_cat)


sf_size <- nbfit_sf$estimate['size']
sf_size


#BSB 
nbfit_bsb <- fitdistr(bsb_cat, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_NJ_21.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
mean(bsb_cat)

bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#Scup 
nbfit_scup <- fitdistr(scup_cat, "Negative Binomial")
saveRDS(nbfit_scup, "nb_catch_parameters_scup_NJ_21.rds")

scup_mu <- nbfit_scup$estimate['mu']
scup_mu
mean(scup_cat)

scup_size <- nbfit_scup$estimate['size']
scup_size

set.seed(500)
t.cop <- tCopula(dim=3,dispstr = "un")
m <- pobs(as.matrix(observed_catch_data))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)
saveRDS(fit, "catch_copula_NJ_21.rds")
####################





####################
#Delaware
observed_catch_data <- subset(read.csv("observed_catch_2021_10.csv"), select=c(sf_tot_cat, bsb_tot_cat))
sf_cat <- observed_catch_data$sf_tot_cat
bsb_cat <- observed_catch_data$bsb_tot_cat

nbfit_sf = fitdistr(sf_cat, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_DE_21.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu
mean(sf_cat)


sf_size <- nbfit_sf$estimate['size']
sf_size


#BSB 
nbfit_bsb <- fitdistr(bsb_cat, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_DE_21.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
mean(bsb_cat)

bsb_size <- nbfit_bsb$estimate['size']
bsb_size



#Scup 
###omit scup for Delaware


set.seed(500)
t.cop <- tCopula(dim=2,dispstr = "un")
m <- pobs(as.matrix(observed_catch_data))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)
saveRDS(fit, "catch_copula_DE_21.rds")
####################



####################
#Maryland

observed_catch_data <- subset(read.csv("observed_catch_2021_24.csv"), select=c(sf_tot_cat, bsb_tot_cat))
sf_cat <- observed_catch_data$sf_tot_cat
bsb_cat <- observed_catch_data$bsb_tot_cat

nbfit_sf = fitdistr(sf_cat, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_MD_21.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu
mean(sf_cat)


sf_size <- nbfit_sf$estimate['size']
sf_size


#BSB 
nbfit_bsb <- fitdistr(bsb_cat, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_MD_21.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
mean(bsb_cat)

bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#Scup 
###omit scup for MD


set.seed(500)
t.cop <- tCopula(dim=2,dispstr = "un")
m <- pobs(as.matrix(observed_catch_data))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)
saveRDS(fit, "catch_copula_MD_21.rds")
####################





####################
#Virginia  

observed_catch_data <- subset(read.csv("observed_catch_2021_51.csv"), select=c(sf_tot_cat, bsb_tot_cat, scup_tot_cat))
sf_cat <- observed_catch_data$sf_tot_cat
bsb_cat <- observed_catch_data$bsb_tot_cat
scup_cat <- observed_catch_data$scup_tot_cat

nbfit_sf = fitdistr(sf_cat, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_VA_21.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu
mean(sf_cat)


sf_size <- nbfit_sf$estimate['size']
sf_size


#BSB 
nbfit_bsb <- fitdistr(bsb_cat, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_VA_21.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
mean(bsb_cat)

bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#Scup 
nbfit_scup <- fitdistr(scup_cat, "Negative Binomial")
saveRDS(nbfit_scup, "nb_catch_parameters_scup_VA_21.rds")

scup_mu <- nbfit_scup$estimate['mu']
scup_mu
mean(scup_cat)

scup_size <- nbfit_scup$estimate['size']
scup_size

set.seed(500)
t.cop <- tCopula(dim=3,dispstr = "un")
m <- pobs(as.matrix(observed_catch_data))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)
saveRDS(fit, "catch_copula_VA_21.rds")
####################


####################
#North Carolina
observed_catch_data <- subset(read.csv("observed_catch_2021_37.csv"), select=c(sf_tot_cat, bsb_tot_cat))
sf_cat <- observed_catch_data$sf_tot_cat
bsb_cat <- observed_catch_data$bsb_tot_cat

nbfit_sf = fitdistr(sf_cat, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_NC_21.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu
mean(sf_cat)


sf_size <- nbfit_sf$estimate['size']
sf_size


#BSB 
nbfit_bsb <- fitdistr(bsb_cat, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_NC_21.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
mean(bsb_cat)

bsb_size <- nbfit_bsb$estimate['size']
bsb_size



#Scup 
###omit scup for NC


set.seed(500)
t.cop <- tCopula(dim=2,dispstr = "un")
m <- pobs(as.matrix(observed_catch_data))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)
saveRDS(fit, "catch_copula_NC_21.rds")
####################


