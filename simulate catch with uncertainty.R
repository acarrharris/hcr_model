

###Northern states
catch_data <- read.csv("catch_draws_uncertainty.csv")

#For each state and draw from 1-100, calculate the NB mu and size parameters. 
params <-list()


catch_data <- subset(catch_data, state==25)

#for(i in 2:2){
i<-2
sf_cat <- catch_data[,paste0('sf_cat',i)]
bsb_cat <- catch_data[,paste0('bsb_cat',i)]
scup_cat <- catch_data[,paste0('scup_cat',i)]


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
state <- "MA"

df[[i]] <- cbind(state, draw, sf_mu, sf_size, bsb_mu, bsb_size, scup_mu, scup_size)

}
params=list.stack(df, fill=TRUE)


#Create the t-copula with observed correlation structure 
#Generate 1000 draws based on the alternative catch-per-trip parameters
#Insert ACTUAL baseline-year correlation structure here

myCop <- mvdc(copula=tCopula(param=c(0.4,0.2,-0.8),dim=3,dispstr = "un"), margins=c("nbinom", "nbinom", "nbinom"),
              paramMargins=list(list(mu=sf_mu, size=sf_size),
                                list(mu=bsb_mu, size=bsb_size), 
                                list(mu=scup_mu, size=scup_size)))


myCop <- normalCopula(param=c(0.4,0.2,-0.8), dim = 3, dispstr = "un")
myMvd <- mvdc(copula=myCop, margins=c("nbinom", "nbinom", "nbinom"),
              paramMargins=list(list(mu=sf_mu, size=sf_size),
                                list(mu=bsb_mu, size=bsb_size), 
                                list(mu=scup_mu, size=scup_size)))
sim_t_cop_nb <- rMvdc(20000, myMvd )
pairs.panels(sim_t_cop_nb)
cor(sim_t_cop_nb,method = c("spearman"))

sf_cat_sim=sim_t_cop_nb[,1]
bsb_cat_sim=sim_t_cop_nb[,2]
scup_cat_sim=sim_t_cop_nb[,3]
df=cbind(sf_cat_sim,bsb_cat_sim, scup_cat_sim)

cor

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
