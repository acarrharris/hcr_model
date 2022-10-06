

###Northern states
catch_data <- read.csv("catch_draws_uncertainty.csv")

#For each state and draw from 1-100, calculate the NB mu and size parameters. 
#params <-c()

statez=as.factor(catch_data$state)
levels(statez)

params_st=list()
params <-list()

s<-10
catch_data_new <- subset(catch_data, state==s)


  for(i in 1:10){
    
  sf_cat <- catch_data_new[,paste0('sf_cat',i)]
  bsb_cat <- catch_data_new[,paste0('bsb_cat',i)]
  scup_cat <- catch_data_new[,paste0('scup_cat',i)]

  ##############
  #estimate the nb parameters
  ##############

  #Fluke 
  nbfit_sf = fitdistr(sf_cat, "Negative Binomial")

  sf_mu <- nbfit_sf$estimate['mu']
  sf_mu

  sf_size <- nbfit_sf$estimate['size']
  sf_size

  
  #BSB 
  nbfit_bsb <- fitdistr(bsb_cat, "Negative Binomial")

  bsb_mu <- nbfit_bsb$estimate['mu']
  bsb_mu

  bsb_size <- nbfit_bsb$estimate['size']
  bsb_size


  #Scup 
  nbfit_scup <- fitdistr(scup_cat, "Negative Binomial")

  scup_mu <- nbfit_scup$estimate['mu']
  scup_mu
  scup_size <- nbfit_scup$estimate['size']
  scup_size

  draw <- i
  state <- s
  
  #params <- rbind(params, c(sf_mu, sf_size, bsb_mu, bsb_size, scup_mu, scup_size, state, draw))
  params[[i]] <- as.data.table(cbind(sf_mu, sf_size, bsb_mu, bsb_size, scup_mu, scup_size, draw, state))
  

  }

params_9 <- list.stack(params, fill=TRUE)
 










params_state_new <- list.stack(params_state, fill=TRUE)



#Create the t-copula with observed correlation structure 
#Generate 1000 draws based on the alternative catch-per-trip parameters
#Insert ACTUAL baseline-year correlation structure here
#t copula

####
observed_catch_data <- subset(read.csv("observed_catch_2021_44.csv"), select=c(sf_tot_cat, bsb_tot_cat, scup_tot_cat))
sf_cat <- observed_catch_data$sf_tot_cat
bsb_cat <- observed_catch_data$bsb_tot_cat
scup_cat <- observed_catch_data$scup_tot_cat

nbfit_sf = fitdistr(sf_cat, "Negative Binomial")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size


#BSB 
nbfit_bsb <- fitdistr(bsb_cat, "Negative Binomial")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu

bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#Scup 
nbfit_scup <- fitdistr(scup_cat, "Negative Binomial")

scup_mu <- nbfit_scup$estimate['mu']
scup_mu
scup_size <- nbfit_scup$estimate['size']
scup_size




set.seed(500)
t.cop <- tCopula(dim=3,dispstr = "un")
m <- pobs(as.matrix(observed_catch_data))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)

rho1 <- coef(fit)[1]
rho2 <- coef(fit)[2]
rho3 <- coef(fit)[3]

rho1
rho2
rho3

df <- coef(fit)[4]

#persp(tCopula(dim=2,rho,df=df),dCopula)

copula_dist <- mvdc(copula=tCopula(param=c(rho1, rho2, rho3),dim=3,df=df,dispstr = "un"),  margins=c("nbinom", "nbinom", "nbinom"),
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size), 
                                      list(mu=scup_mu, size=scup_size)))

sim <- rMvdc(10000, copula_dist )
cor(sim,method="kendall")
cor(observed_catch_data,method="kendall")






state="MA"
catch_data_sim=data.frame(state, sf_cat_sim, bsb_cat_sim, scup_cat_sim)



t_cop_model <- tCopula(dim = 3)



t_cop_model <- tCopula(dim = 2)
m <- pobs(as.matrix(cbind(sf,bsb, scup)))
fit <- fitCopula(t_cop_model, m, method = 'ml')
fit
coef(fit)
dispstr='un'
#saveRDS(fit, "catch_copula_NO_19.rds")


rho <- coef(fit)[1]
df <- coef(fit)[2]



t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

region="NO"
catch_data_sim=data.frame( sf_t_nb, bsb_t_nb, region)
mean(catch_data_sim$sf_t_nb)
mean(catch_data$sf_tot_cat)

#cor(sf_t_nb, bsb_t_nb, method = c("kendall"))


write_xlsx(catch_data_sim, "NO_catch_data_sim1.xlsx") 





###New Jersey
catch_data <- read_excel("observed_catch_NJ_19.xlsx")
#catch_data <- readRDS("observed_catch_NJ_19.rds")


sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat

#cor(sf, bsb, method = c("kendall"))

#estimate the nb parameters
nbfit_sf = fitdistr(sf, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_NJ_19.rds")


sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size

nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_NJ_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size

#t copula
t_cop_model <- tCopula(dim = 2)
m <- pobs(as.matrix(cbind(sf,bsb)))
fit <- fitCopula(t_cop_model, m, method = 'ml')
fit
coef(fit)

saveRDS(fit, "catch_copula_NJ_19.rds")


rho <- coef(fit)[1]
df <- coef(fit)[2]



t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

region="NJ"
catch_data_sim=data.frame( sf_t_nb, bsb_t_nb, region)
mean(catch_data_sim$sf_t_nb)
mean(catch_data$sf_tot_cat)

#cor(sf_t_nb, bsb_t_nb, method = c("kendall"))

write_xlsx(catch_data_sim, "NJ_catch_data_sim1.xlsx") 


###Southern states
catch_data <- read_excel("observed_catch_SO_19.xlsx")
#catch_data <- readRDS("observed_catch_SO_19.rds")

sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
nbfit_sf = fitdistr(sf, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_SO_19.rds")


sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size

nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_SO_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size

#t copula
t_cop_model <- tCopula(dim = 2)
m <- pobs(as.matrix(cbind(sf,bsb)))
fit <- fitCopula(t_cop_model, m, method = 'ml')
fit
coef(fit)

saveRDS(fit, "catch_copula_SO_19.rds")


rho <- coef(fit)[1]
df <- coef(fit)[2]



t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

region="SO"
catch_data_sim=data.frame( sf_t_nb, bsb_t_nb, region)
mean(catch_data_sim$sf_t_nb)
mean(catch_data$sf_tot_cat)


write_xlsx(catch_data_sim, "SO_catch_data_sim1.xlsx") 
