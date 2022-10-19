

###Notes:
# There are several pieces of information that vary by state or regions:
#     By state: trip costs, percent of trips taken by each mode (used for assigning trip costs), 
#               and regulations (which also vary within a a season for a given state)
#     By state: catch-per-trip and catch-at-length distributions
#     By region (MA-NY, NJ, DE-MD, VA-NC): utility parameters and target species 


# This code requires the following data:
# 1) The output from the calibration: calibration_output_by_period.xlsx
# 2) Dataset containing the alternative regulations to be imposed
# 3) Abundance adjusted catch-at-length for summer flounder. 
#    This consists of two output files from the "catch at length given stock structure" script:
#       a) "predicted_catch_(STATE)" which gives catch-per-trip based on population abundance
#       b)  "SPECIES_fitted_sizes_y2plus.xlsx" which gives a new size distribution based on historical recreational 
#           selectivity and projected population abundances at length. 



state1 <- "VA"


# Input the calibration output which contains the number of choice occasions needed to simulate
#calibration_data = data.frame(read_excel("calibration_output_by_period.xlsx"))
#calibration_data = subset(calibration_data, state == state1, select=c(period, sim, state, n_choice_occasions))

calibration_data <- data.frame(readRDS("calibration_output_by_period.RDS")) %>%
  filter(state == state1 & draw==1) %>%
  select(period, sim, state, n_choice_occasions)


# Input the data set containing alterntative regulations and directed trips
directed_trips <- subset(directed_trip_alt_regs, state == state1)

min_period <- min(directed_trips$period)
max_period <- max(directed_trips$period)


######################################
##   Begin simulating trip outcomes ##
######################################

# Set up an output file for the separately simulated within-season regulatory periods  
pds <- list()

periodz <- as.factor(directed_trips$period)
levels(periodz)

for(p in levels(periodz)){
  directed_trips_p <- subset(directed_trips, period == p)
  n_trips <- mean(directed_trips_p$dtrip_2019)
  #n_draws = min(1000,n_trips*2.5 )
  n_draws = n_drawz
  
  fluke_bag <- mean(directed_trips_p$fluke_bag)
  fluke_min <- mean(directed_trips_p$fluke_min)
  fluke_max <- mean(directed_trips_p$fluke_max)
  bsb_bag <- mean(directed_trips_p$bsb_bag)
  bsb_min <- mean(directed_trips_p$bsb_min)
  scup_bag <- mean(directed_trips_p$scup_bag)
  scup_min <- mean(directed_trips_p$scup_min)
  
  
  # Set up an output file for the separate catch draw files 
  dfs <- list()
  
  for(i in 1:10) {
    
    # Input catch-per-trip numbers 
    sf_catch_data <- data.frame(readRDS("pred_catch_data_VA.rds"))                                                                            
    tot_sf_catch <- sf_catch_data$sf_pred_cat
    tot_bsb_catch <- sf_catch_data$bsb_pred_cat
    tot_scup_catch <- sf_catch_data$scup_pred_cat
    sf_catch_data <- data.frame(tot_sf_catch,tot_bsb_catch,tot_scup_catch)
    
    # random draw of fluke and bsb catch
    sf_catch_data <- as.data.frame(sf_catch_data[sample(1:nrow(sf_catch_data), n_draws), ])
    sf_catch_data$tripid <- 1:nrow(sf_catch_data)
    sf_bsb_scup_catch_data <- sf_catch_data
    
    
    
    # subset trips with zero catch, as no size draws are required
    sf_zero_catch <- subset(sf_catch_data, tot_sf_catch == 0)
    
    
    
    #remove trips with zero summer flounder catch
    sf_catch_data<-sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
    
    
    
    #expand the sf_catch_data so that each row represents a fish
    row_inds <- seq_len(nrow(sf_catch_data))
    sf_catch_data <- sf_catch_data[c(rep(row_inds, sf_catch_data$tot_sf_catch)), ]
    rownames(sf_catch_data) <- NULL
    sf_catch_data$fishid <- 1:nrow(sf_catch_data)
    
    
    #Start Gavin code insert
    size_data_read <- readRDS("sf_fitted_sizes_y2plus.rds") %>% tibble()
    size_data <- size_data_read %>% filter(state == state1)
    
    # generate lengths for each fish
    catch_size_data <- sf_catch_data %>% 
      mutate(fitted_length = sample(size_data$length,
                                    nrow(.),
                                    prob = size_data$fitted_prob,
                                    replace = TRUE)) #%>%
    
    
    ##I()
    
    # Impose regulations, calculate keep and release per trip
    # For summer flounder, retain keep- and release-at-length
    
    
    
    catch_size_data <- catch_size_data %>% 
      #left_join(regs, by = "period") %>% 
      mutate(posskeep = ifelse(fitted_length>=fluke_min & fitted_length<=fluke_max,1,0)) %>% 
      group_by(tripid) %>% 
      # keep = case_when(
      # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      # TRUE ~ 0),
      mutate(csum_keep = cumsum(posskeep)) %>% 
      ungroup() %>% 
      mutate(
        keep_adj = case_when(
          fluke_bag > 0 ~ ifelse(csum_keep<=fluke_bag & posskeep==1,1,0),
          TRUE ~ 0),
        # keep_adj = case_when(
        #   csum_keep<=bag & keep==1 ~ 1,
        #   TRUE ~ 0),
        release = case_when(
          fluke_bag > 0 ~ ifelse(posskeep==0 | (posskeep==1 & csum_keep>fluke_bag ), 1,0)))
    
    
    catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_adj, release)) %>% 
      rename(keep = keep_adj)
    
    
    new_size_data <- catch_size_data %>% 
      group_by( tripid, fitted_length) %>% 
      summarize(keep = sum(keep),
                release = sum(release), .groups = "drop") #%>% 
    
    # generate sum of number of kept and released fish by tripid
    summed_catch_data <- catch_size_data %>% 
      group_by( tripid) %>% 
      summarize(tot_keep_sf = sum(keep),
                tot_rel_sf = sum(release),
                .groups = "drop") #%>% 
    
    
    keep_size_data <- new_size_data %>%
      ungroup() %>%
      dplyr::select(-release) %>% 
      pivot_wider(names_from = fitted_length, #_length,
                  names_glue = "keep_length_sf_{fitted_length}",
                  names_sort = TRUE,
                  values_from = keep, 
                  values_fill = 0)# %>% 
    #I()
    #keep_size_data
    
    release_size_data <- new_size_data %>%
      ungroup() %>% 
      dplyr::select(-keep) %>% 
      pivot_wider(names_from = fitted_length, #_length,
                  names_glue = "release_length_sf_{fitted_length}",
                  names_sort = TRUE,
                  values_from = release, 
                  values_fill = 0) #%>% 
    
    trip_data <- summed_catch_data %>% 
      left_join(keep_size_data, by = c( "tripid")) %>% 
      left_join(release_size_data, by = c( "tripid")) #%>% 
    #I()
    #trip_data
    
    #add the zero catch trips 
    trip_data <- bind_rows(trip_data, sf_zero_catch) %>% 
      #arrange(period, catch_draw, tripid) %>% 
      mutate_if(is.numeric, replace_na, replace = 0) %>% 
      mutate_if(is.character, replace_na, replace = state1) #%>%
    #trip_data$tot_sf_catch <- trip_data$tot_keep_sf+trip_data$tot_rel_sf
    
    
    
    
    
    #########################
    ###  Black sea bass  ####
    #########################
    
    
    #draw sizes for black sea bass catch
    bsb_catch_data <-subset(sf_bsb_scup_catch_data, select=c(tripid, tot_bsb_catch))
    bsb_catch_data <- bsb_catch_data[!duplicated(bsb_catch_data), ]
    
    
    
    ##############
    
    # subset trips with zero catch, as no size draws are required
    bsb_zero_catch <- subset(bsb_catch_data, tot_bsb_catch == 0)
    
    
    
    #remove trips with zero summer flounder catch
    bsb_catch_data<-bsb_catch_data[bsb_catch_data$tot_bsb_catch!=0, ]
    
    
    
    #expand the bsb_catch_data so that each row represents a fish
    row_inds <- seq_len(nrow(bsb_catch_data))
    bsb_catch_data <- bsb_catch_data[c(rep(row_inds, bsb_catch_data$tot_bsb_catch)), ]
    rownames(bsb_catch_data) <- NULL
    bsb_catch_data$fishid <- 1:nrow(bsb_catch_data)
    
    
    #Start Gavin code insert
    size_data_read <- readRDS("bsb_fitted_sizes_y2plus.rds") %>% tibble()
    size_data <- size_data_read %>% filter(state == state1)
    
    # generate lengths for each fish
    catch_size_data <- bsb_catch_data %>% 
      mutate(fitted_length = sample(size_data$length,
                                    nrow(.),
                                    prob = size_data$fitted_prob,
                                    replace = TRUE)) #%>%
    
    
    ##I()
    
    # Impose regulations, calculate keep and release per trip
    # For summer flounder, retain keep- and release-at-length
    
    
    
    catch_size_data <- catch_size_data %>% 
      #left_join(regs, by = "period") %>% 
      mutate(posskeep = ifelse(fitted_length>=bsb_min ,1,0)) %>% 
      group_by(tripid) %>% 
      # keep = case_when(
      # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      # TRUE ~ 0),
      mutate(csum_keep = cumsum(posskeep)) %>% 
      ungroup() %>% 
      mutate(
        keep_adj = case_when(
          bsb_bag > 0 ~ ifelse(csum_keep<=bsb_bag & posskeep==1,1,0),
          TRUE ~ 0),
        # keep_adj = case_when(
        #   csum_keep<=bag & keep==1 ~ 1,
        #   TRUE ~ 0),
        release = case_when(
          bsb_bag > 0 ~ ifelse(posskeep==0 | (posskeep==1 & csum_keep>bsb_bag ), 1,0)))
    
    
    catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_adj, release)) %>% 
      rename(keep = keep_adj)
    
    
    new_size_data <- catch_size_data %>% 
      group_by( tripid, fitted_length) %>% 
      summarize(keep = sum(keep),
                release = sum(release), .groups = "drop") #%>% 
    
    # generate sum of number of kept and released fish by tripid
    summed_catch_data <- catch_size_data %>% 
      group_by( tripid) %>% 
      summarize(tot_keep_bsb = sum(keep),
                tot_rel_bsb = sum(release),
                .groups = "drop") #%>% 
    
    
    keep_size_data <- new_size_data %>%
      ungroup() %>%
      dplyr::select(-release) %>% 
      pivot_wider(names_from = fitted_length, #_length,
                  names_glue = "keep_length_bsb_{fitted_length}",
                  names_sort = TRUE,
                  values_from = keep, 
                  values_fill = 0)# %>% 
    #I()
    #keep_size_data
    
    release_size_data <- new_size_data %>%
      ungroup() %>% 
      dplyr::select(-keep) %>% 
      pivot_wider(names_from = fitted_length, #_length,
                  names_glue = "release_length_bsb_{fitted_length}",
                  names_sort = TRUE,
                  values_from = release, 
                  values_fill = 0) #%>% 
    
    trip_data_bsb <- summed_catch_data %>% 
      left_join(keep_size_data, by = c( "tripid")) %>% 
      left_join(release_size_data, by = c( "tripid")) #%>% 
    #I()
    #trip_data
    
    #add the zero catch trips 
    trip_data_bsb <- bind_rows(trip_data_bsb, bsb_zero_catch) %>% 
      #arrange(period, catch_draw, tripid) %>% 
      mutate_if(is.numeric, replace_na, replace = 0) %>% 
      mutate_if(is.character, replace_na, replace = state1) #%>%
    #trip_data_bsb$tot_bsb_catch <- trip_data_bsb$tot_keep_bsb+trip_data_bsb$tot_rel_bsb
    
    # merge the bsb trip data with the rest of the trip data 
    trip_data <-  merge(trip_data,trip_data_bsb,by="tripid")
    trip_data[is.na(trip_data)] <- 0    
    
    
    
    
    #########################
    ###  Scup  ####
    #########################
    
    
    #draw sizes for scup catch
    scup_catch_data <-subset(sf_bsb_scup_catch_data, select=c(tripid, tot_scup_catch))
    scup_catch_data <- scup_catch_data[!duplicated(scup_catch_data), ]
    
    
    
    ##############
    
    # subset trips with zero catch, as no size draws are required
    scup_zero_catch <- subset(scup_catch_data, tot_scup_catch == 0)
    
    
    
    #remove trips with zero summer flounder catch
    scup_catch_data<-scup_catch_data[scup_catch_data$tot_scup_catch!=0, ]
    
    
    
    #expand the scup_catch_data so that each row represents a fish
    row_inds <- seq_len(nrow(scup_catch_data))
    scup_catch_data <- scup_catch_data[c(rep(row_inds, scup_catch_data$tot_scup_catch)), ]
    rownames(scup_catch_data) <- NULL
    scup_catch_data$fishid <- 1:nrow(scup_catch_data)
    
    
    #Start Gavin code insert
    size_data_read <- readRDS("scup_fitted_sizes_y2plus.rds") %>% tibble()
    size_data <- size_data_read %>% filter(state == state1)
    
    # generate lengths for each fish
    catch_size_data <- scup_catch_data %>% 
      mutate(fitted_length = sample(size_data$length,
                                    nrow(.),
                                    prob = size_data$fitted_prob,
                                    replace = TRUE)) #%>%
    
    
    ##I()
    
    # Impose regulations, calculate keep and release per trip
    # For summer flounder, retain keep- and release-at-length
    
    
    
    catch_size_data <- catch_size_data %>% 
      #left_join(regs, by = "period") %>% 
      mutate(posskeep = ifelse(fitted_length>=scup_min ,1,0)) %>% 
      group_by(tripid) %>% 
      # keep = case_when(
      # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      # TRUE ~ 0),
      mutate(csum_keep = cumsum(posskeep)) %>% 
      ungroup() %>% 
      mutate(
        keep_adj = case_when(
          scup_bag > 0 ~ ifelse(csum_keep<=scup_bag & posskeep==1,1,0),
          TRUE ~ 0),
        # keep_adj = case_when(
        #   csum_keep<=bag & keep==1 ~ 1,
        #   TRUE ~ 0),
        release = case_when(
          scup_bag > 0 ~ ifelse(posskeep==0 | (posskeep==1 & csum_keep>scup_bag ), 1,0)))
    
    
    catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_adj, release)) %>% 
      rename(keep = keep_adj)
    
    
    new_size_data <- catch_size_data %>% 
      group_by( tripid, fitted_length) %>% 
      summarize(keep = sum(keep),
                release = sum(release), .groups = "drop") #%>% 
    
    # generate sum of number of kept and released fish by tripid
    summed_catch_data <- catch_size_data %>% 
      group_by( tripid) %>% 
      summarize(tot_keep_scup = sum(keep),
                tot_rel_scup = sum(release),
                .groups = "drop") #%>% 
    
    
    keep_size_data <- new_size_data %>%
      ungroup() %>%
      dplyr::select(-release) %>% 
      pivot_wider(names_from = fitted_length, #_length,
                  names_glue = "keep_length_scup_{fitted_length}",
                  names_sort = TRUE,
                  values_from = keep, 
                  values_fill = 0)# %>% 
    #I()
    #keep_size_data
    
    release_size_data <- new_size_data %>%
      ungroup() %>% 
      dplyr::select(-keep) %>% 
      pivot_wider(names_from = fitted_length, #_length,
                  names_glue = "release_length_scup_{fitted_length}",
                  names_sort = TRUE,
                  values_from = release, 
                  values_fill = 0) #%>% 
    
    trip_data_scup <- summed_catch_data %>% 
      left_join(keep_size_data, by = c( "tripid")) %>% 
      left_join(release_size_data, by = c( "tripid")) #%>% 
    #I()
    #trip_data
    
    #add the zero catch trips 
    trip_data_scup <- bind_rows(trip_data_scup, scup_zero_catch) %>% 
      #arrange(period, catch_draw, tripid) %>% 
      mutate_if(is.numeric, replace_na, replace = 0) %>% 
      mutate_if(is.character, replace_na, replace = state1) #%>%
    #trip_data_scup$tot_scup_catch <- trip_data_scup$tot_keep_scup+trip_data_scup$tot_rel_scup
    
    # merge the scup trip data with the rest of the trip data 
    trip_data <-  merge(trip_data,trip_data_scup,by="tripid")
    
    trip_data<- subset(trip_data, select=-c(tot_bsb_catch.x, tot_bsb_catch.y, tot_scup_catch.x, tot_scup_catch.y, tot_sf_catch))  
    trip_data$tot_sf_catch <- trip_data$tot_keep_sf+trip_data$tot_rel_sf
    trip_data$tot_bsb_catch <- trip_data$tot_keep_bsb+trip_data$tot_rel_bsb
    trip_data$tot_scup_catch <- trip_data$tot_keep_scup+trip_data$tot_rel_scup
    trip_data[is.na(trip_data)] <- 0      
    
    
    trip_data$catch_draw <- i
    dfs[[i]] <- trip_data
    
    
  }
  
  dfs_all <- as.data.frame(bind_rows(dfs[[1]], dfs[[2]],dfs[[3]],dfs[[4]],dfs[[5]],
                                     dfs[[6]], dfs[[7]],dfs[[8]],dfs[[9]],dfs[[10]]))
  
  dfs_all[is.na(dfs_all)] <- 0
  dfs_all <- dfs_all[order(dfs_all$tripid),]
  rownames(dfs_all) <- NULL
  
  dfs_all$period<-p
  pds[[p]] <- dfs_all
}

pds_all<- list.stack(pds, fill=TRUE)
pds_all[is.na(pds_all)] <- 0
rm(pds)

######################################
##   End simulating trip outcomes   ##
######################################

# Now calculate trip probabilities and utilities based on the multiple catch draws for each choice occasion

pds_new <- list()
for(p in levels(periodz)){
  
  # Merge the prediction year data to the calibration data
  pds <- subset(pds_all, period==p)
  
  cost_data <- subset(costs_new_all_VA, period == p, select=-c(period))
  trip_data <-  merge(pds,cost_data,by=c("tripid", "catch_draw"))
  trip_data[is.na(trip_data)] <- 0
  
  
  #set up an output file for each draw of utility parameters. For now, only taking one draw. 
  parameter_draws <- list()
  
  for(d in 1:1) {
    
    # Use the previously drawn set of utility parameters to calculate expected utility, welfare, and effort in the prediction year
    
    #param_draws_VA_prediction <- subset(param_draws_VA, parameter_draw==1)
    trip_data <-  merge(param_draws_VA,trip_data,by="tripid")
    
    
    # Expected utility (prediction year)
    trip_data$vA = 
      trip_data$beta_sqrt_sf_keep*sqrt(trip_data$tot_keep_sf) +
      trip_data$beta_sqrt_sf_release*sqrt(trip_data$tot_rel_sf) +  
      trip_data$beta_sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb) +
      trip_data$beta_sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb) +  
      trip_data$beta_sqrt_scup_catch*sqrt(trip_data$tot_scup_catch) +
      trip_data$beta_cost*trip_data$cost 
    
    # Expected utility (base year)
    trip_data$v0 = 
      trip_data$beta_sqrt_sf_keep*sqrt(trip_data$tot_keep_sf_base) +
      trip_data$beta_sqrt_sf_release*sqrt(trip_data$tot_rel_sf_base) +  
      trip_data$beta_sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb_base) +
      trip_data$beta_sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb_base) +  
      trip_data$beta_sqrt_scup_catch*sqrt(trip_data$tot_cat_scup_base) +
      trip_data$beta_cost*trip_data$cost 
    
    trip_data$period <- as.numeric(trip_data$period)
    
    ########
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
    mean_trip_data$v0_optout= mean_trip_data$beta_opt_out*mean_trip_data$opt_out 
    
    
    
    #Now put the two values in the same column, exponentiate, and caluculate their sum (vA_col_sum)
    mean_trip_data$expon_vA <- case_when(mean_trip_data$alt==1 ~ exp(mean_trip_data$vA), 
                                         mean_trip_data$alt==2 ~ exp(mean_trip_data$vA_optout)) 
    
    mean_trip_data$expon_v0 <- case_when(mean_trip_data$alt==1 ~ exp(mean_trip_data$v0), 
                                         mean_trip_data$alt==2 ~ exp(mean_trip_data$v0_optout)) 
    
    mean_trip_data$expon_vA_col_sum = ave(mean_trip_data$expon_vA, mean_trip_data$tripid, FUN = sum)
    mean_trip_data$expon_v0_col_sum = ave(mean_trip_data$expon_v0, mean_trip_data$tripid, FUN = sum)
    
    
    # Caluculate the probability of a respondent selected each alternative based on 
    # exponentiated expected utility of the altenrative [exp(expected utility, alt=i] 
    # and the sum of exponentiated expected utility across the two altenratives.
    mean_trip_data$probA = mean_trip_data$expon_vA/mean_trip_data$expon_vA_col_sum
    mean_trip_data$prob0 = mean_trip_data$expon_v0/mean_trip_data$expon_v0_col_sum
    
    #change in Consmer surplus between prediction year and baseline year 
    mean_trip_data$change_CS <- (1/mean_trip_data$beta_cost)*(log(mean_trip_data$expon_vA_col_sum) - log(mean_trip_data$expon_v0_col_sum))
    
    
    # Get rid of things we don't need. 
    mean_trip_data <- subset(mean_trip_data, alt==1,select=-c(alt, beta_cost,beta_opt_out, beta_sqrt_bsb_keep, beta_sqrt_bsb_release, beta_sqrt_scup_catch, 
                                                              beta_sqrt_sf_keep ,beta_sqrt_sf_release, catch_draw, expon_v0 ,expon_v0_col_sum, expon_vA, 
                                                              Group.1, opt_out, v0, v0_optout, vA, vA_optout, expon_vA_col_sum, tot_keep_bsb_base, tot_cat_scup_base, 
                                                              tot_rel_bsb_base, tot_rel_sf_base , tot_keep_sf_base))                                                        
    
    
    # Multiply the average trip probability by each of the catch variables (not the variable below) to get probability-weighted values
    list_names <- colnames(mean_trip_data)[colnames(mean_trip_data) !="tripid" 
                                           &  colnames(mean_trip_data) !="period" 
                                           & colnames(mean_trip_data) !="probA"
                                           & colnames(mean_trip_data) !="prob0" 
                                           & colnames(mean_trip_data) !="change_CS"  ]
    
    for (l in list_names){
      mean_trip_data[,l] <- mean_trip_data[,l]*mean_trip_data$probA
    }
    
    #Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in 
    #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
    sims <- subset(calibration_data, period==p & sim==1)
    n_choice_occasions <- mean(sims$n_choice_occasions)
    ndraws <- nrow(mean_trip_data)
    expand <- n_choice_occasions/ndraws
    
    mean_trip_data$n_choice_occasions <- 1
    
    list_names <- colnames(mean_trip_data)[ colnames(mean_trip_data) !="tripid" 
                                            & colnames(mean_trip_data) !="period"]
    
    for (l in list_names){
      mean_trip_data[,l] <- mean_trip_data[,l]*expand
    }
    
    
    mean_trip_data$sim <- 1
    
    #sum probability weighted catch over all choice occasions
    aggregate_trip_data <-aggregate(mean_trip_data, by=list(mean_trip_data$sim),FUN=sum, na.rm=TRUE)
    
    
    aggregate_trip_data <- subset(aggregate_trip_data, select=-c(Group.1, tripid, prob0, sim))
    names(aggregate_trip_data)[names(aggregate_trip_data) == "probA"] = "observed_trips"
    
    
    aggregate_trip_data$sim <- d
    
    parameter_draws[[d]] <- aggregate_trip_data
    #parameter_draws=aggregate_trip_data
    
  }
  
  parameter_draws_all <- as.data.frame(bind_rows(parameter_draws[[1]]))
  #parameter_draws_all = list.stack(parameter_draws,fill=TRUE)
  
  parameter_draws_all[is.na(parameter_draws_all)] <- 0
  rownames(parameter_draws_all) <- NULL
  
  parameter_draws_all$period <- p
  pds_new[[p]] <- parameter_draws_all
  
}


pds_new_all_VA <-list.stack(pds_new, fill=TRUE)
pds_new_all_VA[is.na(pds_new_all_VA)] = 0
pds_new_all_VA$state = state1






