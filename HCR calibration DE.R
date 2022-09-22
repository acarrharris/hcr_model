
###Notes:
# There are several pieces of information that vary by state or regions:
#     By state: trip costs, percent of trips taken by each mode (used for assigning trip costs), 
#               and regulations (which also vary within a a season for a given state)
#     By state or region (MA-NY, NJ, DE-NC): sf catch-per-trip and catch-at-length distributions
#     By region (MA-NY, NJ, DE-MD, VA-NC): utility parameters and target species 

# This code requires the following data:
# 1) Directed summer flounder by regulatory period in the baseline year: directed_trips_region_XX.xlsx
# 2) Distribution of trip costs by mode in each state: trip_costs_NE.xlsx
# 3) Catch-per-trip of SF and BSB from the copula model: XX_catch_data_sim1.xlsx
# 4) Catch-per-trip of species other than SF and BSB: other_species_fitted_catch.xlsx

state1="DE"
state_no=10

p_star_sf <- .80
p_star_bsb<- 0.715
#p_star_scup<-.46

######################################
##   Begin simulating trip outcomes ##
######################################

#Import directed trips file - gives directed trips by regulatory period in 2021
directed_trips <- data.frame( read.csv("directed trips and regulations 2021.csv"))                                                                            

directed_trips$dtrip=round(directed_trips$dtrip)
directed_trips= subset(directed_trips, state == state1)

min_period=min(directed_trips$period)
max_period=max(directed_trips$period)


n_drawz<-2000

# Set up an output file for the separately simulated within-season regulatory periods  
pds = list()

periodz=as.factor(directed_trips$period)
levels(periodz)

for(p in levels(periodz)){
  directed_trips_p = subset(directed_trips, period == p)
  n_trips = mean(directed_trips_p$dtrip)
  #n_draws = min(1000,n_trips*2.5 )
  n_draws = n_drawz
  
  
  fluke_bag = mean(directed_trips_p$fluke_bag)
  fluke_min = mean(directed_trips_p$fluke_min)
  bsb_bag = mean(directed_trips_p$bsb_bag)
  bsb_min = mean(directed_trips_p$bsb_min)
  scup_bag = mean(directed_trips_p$scup_bag)
  scup_min = mean(directed_trips_p$scup_min)
  
  
  # Set up an output file for catch draw files 
  dfs = list()
  
  #Run the catch loop X times to give X draws of catch for each species
  for(i in 1:10) {
    # Input catch-per-trip numbers 
    sf_catch_data = data.frame(read.csv(paste0('observed_catch_2021_',state_no, '.csv')))                                                                           
    tot_sf_catch = sf_catch_data$sf_tot_cat
    tot_bsb_catch = sf_catch_data$bsb_tot_cat
    tot_scup_catch = sf_catch_data$scup_tot_cat
    sf_catch_data = data.frame(tot_sf_catch,tot_bsb_catch,tot_scup_catch)
    
    # random draw of fluke and bsb catch
    sf_catch_data = as.data.frame(sf_catch_data[sample(1:nrow(sf_catch_data), n_draws), ])
    sf_catch_data$tripid = 1:nrow(sf_catch_data)
    sf_bsb_catch_data = sf_catch_data
    
    
    # subset trips with zero catch, as no size draws are required
    sf_zero_catch = subset(sf_catch_data, tot_sf_catch == 0)
    
    
    #remove trips with zero summer flounder catch, will add them on later
    sf_catch_data=sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
    
    
    #expand the sf_catch_data so that each row represents a fish
    row_inds = seq_len(nrow(sf_catch_data))
    sf_catch_data = sf_catch_data[c(rep(row_inds, sf_catch_data$tot_sf_catch)), ]
    rownames(sf_catch_data) = NULL
    sf_catch_data$fishid = 1:nrow(sf_catch_data)
    
    
    
    #Execute the following code if the seasonal period has a positive bag limit 
    if(fluke_bag>0){
      
      sf_catch_data1= as.data.frame(sf_catch_data)  
      sf_catch_data1$uniform=runif(nrow(sf_catch_data1))
      sf_catch_data1$keep = ifelse(sf_catch_data1$uniform>=p_star_sf, 1,0)
      
      sf_catch_data1$csum_keep <- ave(sf_catch_data1$keep, sf_catch_data1$tripid, FUN=cumsum)
      sf_catch_data1$keep_adj = ifelse(sf_catch_data1$csum_keep>fluke_bag, 0,sf_catch_data1$keep)
      
      # #Add the following lines to end the trip once the bag limit is reached (rather than continuing to discard)
      # ###
      # sf_catch_data1$post_bag_fish=ifelse(sf_catch_data1$csum_keep>fluke_bag, 1,0)
      # sf_catch_data1= subset(sf_catch_data1,post_bag_fish==0 )
      # sf_catch_data1 <- subset(sf_catch_data1, select=-c(post_bag_fish ))
      # ###
      
      sf_catch_data1 <- subset(sf_catch_data1, select=-c(keep, csum_keep))
      names(sf_catch_data1)[names(sf_catch_data1) == "keep_adj"] = "keep"
      
      sf_catch_data1$release = ifelse(sf_catch_data1$keep==0, 1,0) 
      sf_catch_data1=subset(sf_catch_data1, select=c(tripid, keep, release))
      sf_catch_data1 <-aggregate(sf_catch_data1, by=list(sf_catch_data1$tripid),FUN=sum, na.rm=TRUE)
      sf_catch_data1 <-subset(sf_catch_data1, select=c(Group.1, keep, release))
      names(sf_catch_data1)[names(sf_catch_data1) == "Group.1"] = "tripid"
      names(sf_catch_data1)[names(sf_catch_data1) == "keep"] = "tot_keep_sf"
      names(sf_catch_data1)[names(sf_catch_data1) == "release"] = "tot_rel_sf"
      
    }
    
    if(fluke_bag==0){
      
      sf_catch_data1= as.data.frame(sf_catch_data)  
      sf_catch_data1$keep = 0
      sf_catch_data1$release = 1
      
      sf_catch_data1=subset(sf_catch_data1, select=c(tripid, keep, release))
      sf_catch_data1 <-aggregate(sf_catch_data1, by=list(sf_catch_data1$tripid),FUN=sum, na.rm=TRUE)
      sf_catch_data1 <-subset(sf_catch_data1, select=c(Group.1, keep, release))
      names(sf_catch_data1)[names(sf_catch_data1) == "Group.1"] = "tripid"
      names(sf_catch_data1)[names(sf_catch_data1) == "keep"] = "tot_keep_sf"
      names(sf_catch_data1)[names(sf_catch_data1) == "release"] = "tot_rel_sf"
      
    }
    
    
    
    trip_data =  as.data.frame(sf_catch_data1)
    
    #add the zero catch trips 
    trip_data = bind_rows(trip_data, sf_zero_catch)
    
    #quick sort and cleanup 
    trip_data = trip_data[order(trip_data$tripid),]
    rownames(trip_data) <- NULL
    
    trip_data[is.na(trip_data)] = 0
    trip_data$tot_sf_catch = trip_data$tot_keep+trip_data$tot_rel
    trip_data[is.na(trip_data)] = 0
    
    
    
    #########################
    ###  Black sea bass  ####
    #########################
    
    
    #draw sizes for black sea bass catch
    bsb_catch_data =subset(sf_bsb_catch_data, select=c(tripid, tot_bsb_catch))
    bsb_catch_data = bsb_catch_data[!duplicated(bsb_catch_data), ]
    
    #subset trips with zero bsb catch 
    bsb_zero_catch = subset(bsb_catch_data, tot_bsb_catch == 0, select=c(tripid, tot_bsb_catch))
    
    
    #remove trips with zero bsb catch, will add them on later
    bsb_catch_data=bsb_catch_data[bsb_catch_data$tot_bsb_catch!=0, ]
    rownames(bsb_catch_data) = NULL
    
    
    #expand the bsb_catch_data so that each row represents a fish
    row_inds = seq_len(nrow(bsb_catch_data))
    bsb_catch_data[is.na(bsb_catch_data)] = 0
    bsb_catch_data = bsb_catch_data[c(rep(row_inds, bsb_catch_data$tot_bsb_catch)), ]
    rownames(bsb_catch_data) = NULL
    bsb_catch_data$fishid = 1:nrow(bsb_catch_data)
    
    
    
    #Execute the following code if the seasonal period has a positive bag limit 
    if(bsb_bag>0){
      
      bsb_catch_data1= as.data.frame(bsb_catch_data)  
      bsb_catch_data1$uniform=runif(nrow(bsb_catch_data1))
      bsb_catch_data1$keep = ifelse(bsb_catch_data1$uniform>=p_star_bsb, 1,0) 
      
      bsb_catch_data1$csum_keep <- ave(bsb_catch_data1$keep, bsb_catch_data1$tripid, FUN=cumsum)
      bsb_catch_data1$keep_adj = ifelse(bsb_catch_data1$csum_keep>bsb_bag, 0,bsb_catch_data1$keep)
      bsb_catch_data1 <- subset(bsb_catch_data1, select=-c(keep, csum_keep))
      names(bsb_catch_data1)[names(bsb_catch_data1) == "keep_adj"] = "keep"
      
      
      bsb_catch_data1$release = ifelse(bsb_catch_data1$keep==0, 1,0) 
      
      bsb_catch_data1=subset(bsb_catch_data1, select=c(tripid, keep, release))
      bsb_catch_data1 <-aggregate(bsb_catch_data1, by=list(bsb_catch_data$tripid),FUN=sum, na.rm=TRUE)
      bsb_catch_data1 <-subset(bsb_catch_data1, select=c(Group.1, keep, release))
      names(bsb_catch_data1)[names(bsb_catch_data1) == "Group.1"] = "tripid"
      names(bsb_catch_data1)[names(bsb_catch_data1) == "keep"] = "tot_keep_bsb"
      names(bsb_catch_data1)[names(bsb_catch_data1) == "release"] = "tot_rel_bsb"
      
    }
    
    if(bsb_bag==0){
      
      bsb_catch_data1= as.data.frame(bsb_catch_data)  
      bsb_catch_data1$keep = 0
      bsb_catch_data1$release = 1
      
      bsb_catch_data1=subset(bsb_catch_data1, select=c(tripid, keep, release))
      bsb_catch_data1 <-aggregate(bsb_catch_data1, by=list(bsb_catch_data1$tripid),FUN=sum, na.rm=TRUE)
      bsb_catch_data1 <-subset(bsb_catch_data1, select=c(Group.1, keep, release))
      names(bsb_catch_data1)[names(bsb_catch_data1) == "Group.1"] = "tripid"
      names(bsb_catch_data1)[names(bsb_catch_data1) == "keep"] = "tot_keep_bsb"
      names(bsb_catch_data1)[names(bsb_catch_data1) == "release"] = "tot_rel_bsb"
      
    }
    
    
    #add the zero catch trips 
    bsb_catch_data1 = bind_rows(bsb_catch_data1, bsb_zero_catch)
    bsb_catch_data1 = subset(bsb_catch_data1, select=-c(tot_bsb_catch))
    
    #quick sort and cleanup 
    bsb_catch_data1 = bsb_catch_data1[order(bsb_catch_data1$tripid),]
    rownames(bsb_catch_data1) <- NULL
    
    bsb_catch_data1[is.na(bsb_catch_data1)] = 0
    
    
    # merge the trip data (summer flounder catch, lengths, and cost) with the bsb data (numbers kept and released))
    trip_data =  merge(trip_data,bsb_catch_data1,by="tripid")
    trip_data[is.na(trip_data)] = 0
    
    
    
    
    #########################
    ###       Scup       ####
    #########################
    #Essentially zero scup catch for DE in 2021, so just add this manually 
    
    trip_data$tot_keep_scup <- 0
    trip_data$tot_rel_scup <- 0
    trip_data$tot_scup_catch <- 0
    
  
    
    # #draw sizes for scup catch
    # scup_catch_data =subset(sf_bsb_catch_data, select=c(tripid, tot_scup_catch))
    # scup_catch_data = scup_catch_data[!duplicated(scup_catch_data), ]
    # 
    # #subset trips with zero scup catch 
    # scup_zero_catch = subset(scup_catch_data, tot_scup_catch == 0, select=c(tripid, tot_scup_catch))
    # 
    # 
    # #remove trips with zero scup catch, will add them on later
    # scup_catch_data=scup_catch_data[scup_catch_data$tot_scup_catch!=0, ]
    # rownames(scup_catch_data) = NULL
    # 
    # 
    # #expand the scup_catch_data so that each row represents a fish
    # row_inds = seq_len(nrow(scup_catch_data))
    # scup_catch_data[is.na(scup_catch_data)] = 0
    # scup_catch_data = scup_catch_data[c(rep(row_inds, scup_catch_data$tot_scup_catch)), ]
    # rownames(scup_catch_data) = NULL
    # scup_catch_data$fishid = 1:nrow(scup_catch_data)
    # 
    # 
    # 
    # #Execute the following code if the seasonal period has a positive bag limit 
    # if(scup_bag>0){
    #   
    #   scup_catch_data1= as.data.frame(scup_catch_data)  
    #   scup_catch_data1$uniform=runif(nrow(scup_catch_data1))
    #   scup_catch_data1$keep = ifelse(scup_catch_data1$uniform>=p_star_scup, 1,0) 
    #   
    #   scup_catch_data1$csum_keep <- ave(scup_catch_data1$keep, scup_catch_data1$tripid, FUN=cumsum)
    #   scup_catch_data1$keep_adj = ifelse(scup_catch_data1$csum_keep>scup_bag, 0,scup_catch_data1$keep)
    #   scup_catch_data1 <- subset(scup_catch_data1, select=-c(keep, csum_keep))
    #   names(scup_catch_data1)[names(scup_catch_data1) == "keep_adj"] = "keep"
    #   
    #   
    #   scup_catch_data1$release = ifelse(scup_catch_data1$keep==0, 1,0) 
    #   
    #   scup_catch_data1=subset(scup_catch_data1, select=c(tripid, keep, release))
    #   scup_catch_data1 <-aggregate(scup_catch_data1, by=list(scup_catch_data$tripid),FUN=sum, na.rm=TRUE)
    #   scup_catch_data1 <-subset(scup_catch_data1, select=c(Group.1, keep, release))
    #   names(scup_catch_data1)[names(scup_catch_data1) == "Group.1"] = "tripid"
    #   names(scup_catch_data1)[names(scup_catch_data1) == "keep"] = "tot_keep_scup"
    #   names(scup_catch_data1)[names(scup_catch_data1) == "release"] = "tot_rel_scup"
    #   
    # }
    # 
    # if(scup_bag==0){
    #   
    #   scup_catch_data1= as.data.frame(scup_catch_data)  
    #   scup_catch_data1$keep = 0
    #   scup_catch_data1$release = 1
    #   
    #   scup_catch_data1=subset(scup_catch_data1, select=c(tripid, keep, release))
    #   scup_catch_data1 <-aggregate(scup_catch_data1, by=list(scup_catch_data1$tripid),FUN=sum, na.rm=TRUE)
    #   scup_catch_data1 <-subset(scup_catch_data1, select=c(Group.1, keep, release))
    #   names(scup_catch_data1)[names(scup_catch_data1) == "Group.1"] = "tripid"
    #   names(scup_catch_data1)[names(scup_catch_data1) == "keep"] = "tot_keep_scup"
    #   names(scup_catch_data1)[names(scup_catch_data1) == "release"] = "tot_rel_scup"
    #   
    # }
    # 
    # 
    # #add the zero catch trips 
    # scup_catch_data1 = bind_rows(scup_catch_data1, scup_zero_catch)
    # scup_catch_data1 = subset(scup_catch_data1, select=-c(tot_scup_catch))
    # 
    # #quick sort and cleanup 
    # scup_catch_data1 = scup_catch_data1[order(scup_catch_data1$tripid),]
    # rownames(scup_catch_data1) <- NULL
    # 
    # scup_catch_data1[is.na(scup_catch_data1)] = 0
    # 
    # 
    # # merge the trip data (summer flounder catch, lengths, and cost) with the bsb/fluke data (numbers kept and released))
    # trip_data =  merge(trip_data,scup_catch_data1,by="tripid")
    # trip_data[is.na(trip_data)] = 0
    
    
    trip_data$catch_draw=i
    dfs[[i]]=trip_data
    
  }
  
  #combine all the catch draw files 
  dfs_all = as.data.frame(bind_rows(dfs[[1]], dfs[[2]],dfs[[3]],dfs[[4]],dfs[[5]],
                                    dfs[[6]], dfs[[7]],dfs[[8]],dfs[[9]],dfs[[10]]))
  
  dfs_all[is.na(dfs_all)] = 0
  dfs_all <- dfs_all[order(dfs_all$tripid),]
  rownames(dfs_all) = NULL
  
  dfs_all$period=p
  pds[[p]] = dfs_all
}

######################################
##   End simulating trip outcomes   ##
######################################


pds_all= list.stack(pds, fill=TRUE)
pds_all[is.na(pds_all)] = 0

pds_all$tot_bsb_catch=pds_all$tot_keep_bsb+pds_all$tot_rel_bsb
pds_all$tot_sf_catch=pds_all$tot_keep_sf+pds_all$tot_rel_sf
pds_all$tot_scup_catch=pds_all$tot_keep_scup+pds_all$tot_rel_scup

rm(pds)


#Create random draws of preference parameters based on the estimated means and SD from the choice model
param_draws_DE = as.data.frame(1:n_drawz)
names(param_draws_DE)[1] <- 'tripid'
param_draws_DE$beta_sqrt_sf_keep = rnorm(n_drawz, mean = 0.7553103, sd = 1.304717)
param_draws_DE$beta_sqrt_sf_release = rnorm(n_drawz, mean = 0.063097 , sd = 0.2389134 )
param_draws_DE$beta_sqrt_bsb_keep = rnorm(n_drawz, mean = 0.2796952 , sd = 0.3044652 )
param_draws_DE$beta_sqrt_bsb_release = rnorm(n_drawz, mean = 0.069036 , sd = 0 )
param_draws_DE$beta_sqrt_scup_catch = rnorm(n_drawz, mean = 0.0182875 , sd = 0)
param_draws_DE$beta_opt_out = rnorm(n_drawz, mean = -1.822518, sd = 2.040809 )
param_draws_DE$beta_cost = rnorm(n_drawz, mean = -0.0114263, sd = 0)
#param_draws_DE$parameter_draw=d






# Now calculate trip probabilities and utilities based on the multiple catch draws for each choice occasion
costs_new_DE = list()
pds_new = list()
for(p in levels(periodz)){
  #p <- 10
  directed_trips_p = subset(directed_trips, period == p)
  n_trips = mean(directed_trips_p$dtrip)  
  
  # Add trip costs. These are mean and sd estimates from over all modes from the expenditure survey
  pds=subset(pds_all, period==p)
  
  trip_costs=data.frame(read_csv("trip_costs_state_summary.csv", show_col_types = FALSE))                
  mean_cost=trip_costs$mean_cost[trip_costs$state==state1]
  sd_cost=trip_costs$sd_cost[trip_costs$state==state1]
  trip_data=pds
  trip_data$cost=rnorm(nrow(trip_data), mean=mean_cost,sd= sd_cost)
  trip_data[is.na(trip_data)] = 0
  
  
  # Costs_new_state data sets will retain raw trip outcomes from the baseline scenario. 
  # We will merge these data to the prediction year outcomes to calculate changes in CS. 
  costs_new_DE[[p]] = subset(trip_data, select=c(tripid, cost, catch_draw, tot_keep_sf, tot_rel_sf,
                                                 tot_keep_bsb,tot_rel_bsb,tot_scup_catch  ))
  
  names(costs_new_DE[[p]])[names(costs_new_DE[[p]]) == "tot_keep"] = "tot_keep_sf_base"
  names(costs_new_DE[[p]])[names(costs_new_DE[[p]]) == "tot_rel"] = "tot_rel_sf_base"
  names(costs_new_DE[[p]])[names(costs_new_DE[[p]]) == "tot_keep_bsb"] = "tot_keep_bsb_base"
  names(costs_new_DE[[p]])[names(costs_new_DE[[p]]) == "tot_rel_bsb"] = "tot_rel_bsb_base"
  names(costs_new_DE[[p]])[names(costs_new_DE[[p]]) == "tot_scup_catch"] = "tot_cat_scup_base"
  
  costs_new_DE[[p]]$period = p
  
  
  
  trip_data =  merge(param_draws_DE,trip_data,by="tripid")
  
  
  #Expected utility
  trip_data$vA = 
    trip_data$beta_sqrt_sf_keep*sqrt(trip_data$tot_keep_sf) +
    trip_data$beta_sqrt_sf_release*sqrt(trip_data$tot_rel_sf) +  
    trip_data$beta_sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb) +
    trip_data$beta_sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb) +  
    trip_data$beta_sqrt_scup_catch*sqrt(trip_data$tot_scup_catch) +
    trip_data$beta_cost*trip_data$cost 
  
  trip_data$period=as.numeric(trip_data$period)
  
  # Collapse data from the X catch draws so that each row contains mean values
  mean_trip_data <-aggregate(trip_data, by=list(trip_data$tripid),FUN=mean, na.rm=TRUE)
  
  
  # Now expand the data to create three alternatives, representing the alternatives available in choice survey
  mean_trip_data <- expandRows(mean_trip_data, 2, count.is.col = FALSE)
  
  #Alt 1, 2
  mean_trip_data$alt <- sequence(tabulate(mean_trip_data$tripid))
  
  #Alt 2 is the opt_out 
  mean_trip_data$opt_out = ifelse(mean_trip_data$alt!=1, 1,0) 
  
  #Caluculate the expected utility of alts 2 parameters of the utility function
  mean_trip_data$vA_optout= mean_trip_data$beta_opt_out*mean_trip_data$opt_out 
  
  
  
  #Now put the two values in the same column, exponentiate, and caluculate their sum (vA_col_sum)
  mean_trip_data$expon_V <- case_when(mean_trip_data$alt==1 ~ exp(mean_trip_data$vA), 
                                      mean_trip_data$alt==2 ~ exp(mean_trip_data$vA_optout)) 
  
  mean_trip_data$expon_V_col_sum = ave(mean_trip_data$expon_V, mean_trip_data$tripid, FUN = sum)
  
  
  # Caluculate the probability of a respondent selected each alternative based on 
  # exponentiated expected utility of the altenrative [exp(expected utility, alt=i] 
  # and the sum of exponentiated expected utility across the two altenratives.
  mean_trip_data$prob = mean_trip_data$expon_V/mean_trip_data$expon_V_col_sum
  
  
  # Get rid of things we don't need. 
  mean_trip_data = subset(mean_trip_data, alt==1, select=-c(alt, opt_out, vA_optout, Group.1, catch_draw,                                                              vA, vA_optout,beta_cost, beta_opt_out, beta_sqrt_scup_catch,beta_sqrt_bsb_release, 
                                                            beta_sqrt_bsb_keep, beta_sqrt_sf_release, beta_sqrt_sf_keep))
  
  
  
  # Multiply the trip probability by each of the catch and cost variables (not the variable below) to get probability-weighted catch
  list_names = c("tot_bsb_catch","tot_keep_bsb","tot_keep_scup", "tot_keep_sf","tot_rel_bsb", "tot_rel_scup",
                 "tot_rel_sf","tot_scup_catch" , "tot_sf_catch", "cost")
  
  for (l in list_names){
    mean_trip_data[,l] = mean_trip_data[,l]*mean_trip_data$prob
  }
  
  
  # Multiply each choice occasion's trip outcomes (catch, cost, trip probabilities) in mean_trip_pool 
  # by the expansion factor (expand), so that each choice occasion represents a certain number of choice occasions
  
  mean_prob=mean(mean_trip_data$prob)
  observed_trips=n_trips
  sims = round(observed_trips/mean_prob)
  ndraws = nrow(mean_trip_data)
  expand=sims/ndraws
  mean_trip_data$n_choice_occasions=1
  
  list_names = c("tot_bsb_catch","tot_keep_bsb","tot_keep_scup", "tot_keep_sf","tot_rel_bsb", "tot_rel_scup",
                 "tot_rel_sf","tot_scup_catch" , "tot_sf_catch", "cost", "prob","n_choice_occasions" )
  
  for (l in list_names){
    mean_trip_data[,l] = mean_trip_data[,l]*expand
  }
  
  
  
  mean_trip_data$sim=1
  
  #sum probability weighted catch over all choice occasions
  aggregate_trip_data <-aggregate(mean_trip_data, by=list(mean_trip_data$sim),FUN=sum, na.rm=TRUE)
  
  
  aggregate_trip_data = subset(aggregate_trip_data, select=-c(Group.1, tripid, period, expon_V, expon_V_col_sum,sim))
  names(aggregate_trip_data)[names(aggregate_trip_data) == "prob"] = "observed_trips"
  
  
  aggregate_trip_data$sim =1
  
  
  aggregate_trip_data$period=p
  pds_new[[p]]=aggregate_trip_data
  
}


pds_new_all_DE=list.stack(pds_new, fill=TRUE)

pds_new_all_DE[is.na(pds_new_all_DE)] = 0
pds_new_all_DE$state = state1
rm(pds_new)

# costs_new_all contain trip outcomes for the baseline period. Will use to calculate welfare changes, 
# and assign catch-per-trip in the prediction years. 

costs_new_all_DE=list.stack(costs_new_DE, fill=TRUE)
costs_new_all_DE[is.na(costs_new_all_DE)] = 0
rm(costs_new_DE)



###Compare calibration model output with MRIP 

MRIP_data_sf <- subset(data.frame( read.csv("total AB1B2 2021 by state.csv")), state=="DELAWARE" & species=="SUMMER FLOUNDER")                                                                          
MRIP_data_bsb <- subset(data.frame( read.csv("total AB1B2 2021 by state.csv")), state=="DELAWARE" & species=="BLACK SEA BASS")                                                                          
MRIP_data_scup <- subset(data.frame( read.csv("total AB1B2 2021 by state.csv")), state=="DELAWARE" & species=="SCUP")                                                                          


##SF
sum(pds_new_all_DE$tot_keep_sf)
sum(MRIP_data_sf$tot_harvest)
((sum(MRIP_data_sf$tot_harvest)-sum(pds_new_all_DE$tot_keep_sf))/sum(MRIP_data_sf$tot_harvest))*100

sum(pds_new_all_DE$tot_rel_sf)
sum(MRIP_data_sf$tot_rel)
((sum(MRIP_data_sf$tot_rel)-sum(pds_new_all_DE$tot_rel_sf))/sum(MRIP_data_sf$tot_rel))*100

sum(pds_new_all_DE$tot_sf_catch)
sum(MRIP_data_sf$tot_catch)
((sum(MRIP_data_sf$tot_catch)-sum(pds_new_all_DE$tot_sf_catch))/sum(MRIP_data_sf$tot_catch))*100

##BSB
sum(pds_new_all_DE$tot_keep_bsb)
sum(MRIP_data_bsb$tot_harvest)
((sum(MRIP_data_bsb$tot_harvest)-sum(pds_new_all_DE$tot_keep_bsb))/sum(MRIP_data_bsb$tot_harvest))*100

sum(pds_new_all_DE$tot_rel_bsb)
sum(MRIP_data_bsb$tot_rel)
((sum(MRIP_data_bsb$tot_rel)-sum(pds_new_all_DE$tot_rel_bsb))/sum(MRIP_data_bsb$tot_rel))*100

sum(pds_new_all_DE$tot_bsb_catch)
sum(MRIP_data_bsb$tot_catch)
((sum(MRIP_data_bsb$tot_catch)-sum(pds_new_all_DE$tot_bsb_catch))/sum(MRIP_data_bsb$tot_catch))*100


##scup
sum(pds_new_all_DE$tot_keep_scup)
sum(MRIP_data_scup$tot_harvest)
((sum(MRIP_data_scup$tot_harvest)-sum(pds_new_all_DE$tot_keep_scup))/sum(MRIP_data_scup$tot_harvest))*100

sum(pds_new_all_DE$tot_rel_scup)
sum(MRIP_data_scup$tot_rel)
((sum(MRIP_data_scup$tot_rel)-sum(pds_new_all_DE$tot_rel_scup))/sum(MRIP_data_scup$tot_rel))*100

sum(pds_new_all_DE$tot_scup_catch)
sum(MRIP_data_scup$tot_catch)
((sum(MRIP_data_scup$tot_catch)-sum(pds_new_all_DE$tot_scup_catch))/sum(MRIP_data_scup$tot_catch))*100



sum(pds_new_all_DE$observed_trips)
