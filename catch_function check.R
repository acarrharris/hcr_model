


# costs_new_all_MA$state<-"MA"
# costs_new_all_RI$state<-"RI"
# costs_new_all_CT$state<-"CT"
# costs_new_all_NY$state<-"NY"
# costs_new_all_NJ$state<-"NJ"
# costs_new_all_DE$state<-"DE"
# costs_new_all_MD$state<-"MD"
# costs_new_all_VA$state<-"VA"
# costs_new_all_NC$state<-"NC"
# 
# costs_all = dplyr::bind_rows(costs_new_all_MA, costs_new_all_RI,costs_new_all_CT,costs_new_all_NY,costs_new_all_NJ,
#                              costs_new_all_DE, costs_new_all_MD, costs_new_all_VA, costs_new_all_NC)
# costs_all_base <- split(costs_all, costs_all$state)
# 
# # Input the calibration output which contains the number of choice occasions needed to simulate
# calibration_data_table <- readRDS("calibration_output_by_period.rds")
# calibration_data_table_base <- split(calibration_data_table, calibration_data_table$state)
# 
# ##Regulations file 
# directed_trips_table=data.frame(read_csv(paste0("directed trips and regulations ", year,".csv"), show_col_types = FALSE))
# directed_trips_table_base <- split(directed_trips_table, directed_trips_table$state)
# 
# ##Sizes 
# sf_size_data_read <- readRDS("sf_fitted_sizes_y2plus.rds") %>% tibble()
# sf_size_data_read_base <- split(sf_size_data_read, sf_size_data_read$state)
# 
# 
# bsb_size_data_read <- readRDS("bsb_fitted_sizes_y2plus.rds") %>% tibble()
# bsb_size_data_read_base <- split(bsb_size_data_read, bsb_size_data_read$state)
# 
# 
# scup_size_data_read <- readRDS("scup_fitted_sizes_y2plus.rds") %>% tibble()
# scup_size_data_read_base <- split(scup_size_data_read, scup_size_data_read$state)
# 



# 
predict_rec_catch <- function(state1,
                              calibration_data_table,
                              directed_trips_table,
                              sf_size_data_read,
                              bsb_size_data_read,
                              scup_size_data_read,
                              param_draws_MA,
                              costs_new_all,
                              sf_catch_data_all){
  # 
  # state1 <- "CT"
  # calibration_data_table <- calibration_data_table_base[[1]]
  # directed_trips_table <- directed_trips_table_base[[1]]
  # sf_size_data_read <- sf_size_data_read_base[[1]]
  # bsb_size_data_read <- bsb_size_data_read_base[[1]]
  # scup_size_data_read <- scup_size_data_read_base[[1]]
  # param_draws_MA <- param_draws_CT
  # costs_new_all <- costs_new_all_CT
  # sf_catch_data_all <- sf_catch_data_ct
  
  
  # state1 <- "MA"
  # calibration_data_table <- calibration_data_table_base[[3]]
  # directed_trips_table <- directed_trips_table_base[[3]]
  # sf_size_data_read <- sf_size_data_read_base[[3]]
  # bsb_size_data_read <- bsb_size_data_read_base[[3]]
  # scup_size_data_read <- scup_size_data_read_base[[3]]
  # param_draws_MA <- param_draws_MA
  # costs_new_all <- costs_new_all_MA
  # sf_catch_data_all <- sf_catch_data_ma
  
  
  #profvis::profvis({
  #if (state1 %in% c("MA", "RI", "CT", "NY", "NJ", "VA")) {
  scup_nbs<- subset(read_csv("nb_params.csv",  show_col_types = FALSE))
  scup_nbs <- scup_nbs %>% tibble() %>% dplyr::filter(state == state1) %>% dplyr::filter(draw == x)
  scup_mu_param <- scup_nbs$scup_mu
  scup_size_param <- scup_nbs$scup_size
  
  ###
  if (scup_mu_param!=0){
    sf_catch_data_all <- sf_catch_data_all  %>% 
      mutate(tot_scup_catch = rnbinom(1:nrow(sf_catch_data_all), mu = scup_mu_param, size = scup_size_param))
    
    #sf_catch_data_all$tot_scup_catch <- rnbinom(1:nrow(sf_catch_data_all), mu = scup_mu_param, size = scup_size_param)
  }
  
  if (scup_mu_param==0){
    sf_catch_data_all <- sf_catch_data_all  %>% 
      mutate(tot_scup_catch = 0 )
    #sf_catch_data_all$tot_scup_catch <- 0
  }
  
  # Input the calibration output which contains the number of choice occasions needed to simulate
  calibration_data <- calibration_data_table #%>% tibble() %>% dplyr::filter(state == state1) 
  
  # Input regul
  directed_trips <- directed_trips_table #%>% tibble() %>% dplyr::filter(state == state1)
  sf_size_data <- sf_size_data_read %>% rename(fitted_length = length) #%>% filter(state == state1) 
  bsb_size_data <- bsb_size_data_read  %>% rename(fitted_length = length) #%>% filter(state == state1)
  scup_size_data <- scup_size_data_read  %>% rename(fitted_length = length) #%>% filter(state == state1 
  
  
  # Input the data set containing alterntative regulations and directed trips
  #directed_trips$dtrip <- round(directed_trips$dtrip)
  #directed_trips <- subset(directed_trips, state == state1)
  
  #min_period <- min(directed_trips$period)
  #max_period <- max(directed_trips$period)
  
  
  ######################################
  ##   Begin simulating trip outcomes ##
  ######################################
  
  # Set up an output file for the separately simulated within-season regulatory periods  
  directed_trips_p <- directed_trips %>% #subset(directed_trips, period == p)
    mutate(period = as.character(period)) %>% 
    #group_by(period) %>% 
    mutate(#n_trips = floor(mean(dtrip_2019)),
      n_trips = floor(dtrip),
      n_draws = floor(min(1000,n_trips*2.5)))# %>% 
  #ungroup()
  #n_draws = floor(min(30000,n_trips*2.5)))
  nsamp = 10
  niter <- nsamp*sum(directed_trips_p$n_draws)
  
  period_vec <- directed_trips_p %>% 
    dplyr::select(period, n_draws) %>% 
    uncount(n_draws)
  
  regs <- directed_trips_p %>% 
    dplyr::select(period, 
                  fluke_bag1, fluke_min1, fluke_max1,
                  fluke_bag2, fluke_min2, fluke_max2,
                  bsb_bag,
                  bsb_min,
                  scup_bag,
                  scup_min)
  
  
  sf_catch_data <- sf_catch_data_all %>% 
    slice_sample(n = niter, replace = TRUE) %>%
    mutate(period = rep(period_vec$period, each = nsamp),
           catch_draw = rep(1:nsamp, length.out = niter),
           tripid = rep(unlist(purrr::map(directed_trips_p$n_draws, seq_len)), each = nsamp)) #%>% 
  
  sf_bsb_catch_data <- sf_catch_data
  #tibble::rowid_to_column("tripid") %>%
  #I()
  
  # subset trips with zero catch, as no size draws are required
  sf_zero_catch <- filter(sf_catch_data, tot_sf_catch == 0)
  
  #remove trips with zero summer flounder catch
  #sf_catch_data=sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
  sf_catch_data <- filter(sf_catch_data, tot_sf_catch > 0) 
  
  #expand the sf_catch_data so that each row represents a fish
  row_inds <- seq_len(nrow(sf_catch_data))
  sf_catch_data <- sf_catch_data[c(rep(row_inds, sf_catch_data$tot_sf_catch)), ]
  rownames(sf_catch_data) <- NULL
  sf_catch_data$fishid <- 1:nrow(sf_catch_data)
  
  
  # generate lengths for each fish
  catch_size_data <- sf_catch_data %>% 
    mutate(fitted_length = sample(sf_size_data$fitted_length,
                                  nrow(.),
                                  prob = sf_size_data$fitted_prob,
                                  replace = TRUE)) #%>%
  ##I()
  
  # Impose regulations, calculate keep and release per trip
  # For summer flounder, retain keep- and release-at-length
  
  catch_size_data <- catch_size_data %>%
    left_join(regs, by = "period") %>%
    mutate(posskeep = ifelse(fitted_length>=fluke_min1 & fitted_length<fluke_max1,1,0)) %>%
    group_by(tripid, period, catch_draw) %>%
    # keep = case_when(
    # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
    # TRUE ~ 0),
    mutate(csum_keep = cumsum(posskeep)) %>%
    ungroup() %>%
    mutate(
      keep_adj = case_when(
        fluke_bag1 > 0 ~ ifelse(csum_keep<=fluke_bag1 & posskeep==1,1,0),
        TRUE ~ 0)) %>%
    
    mutate(posskeep2 = ifelse(fitted_length>=fluke_min2 & fitted_length<fluke_max2,1,0)) %>%
    group_by(tripid, period, catch_draw) %>%
    # keep = case_when(
    # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
    # TRUE ~ 0),
    mutate(csum_keep2 = cumsum(posskeep2)) %>%
    ungroup() %>%
    mutate(
      keep_adj2 = case_when(
        fluke_bag2 > 0 ~ ifelse(csum_keep2<=fluke_bag2 & posskeep2==1,1,0)))
  
  #catch_size_data[is.na(catch_size_data)] <- 0
  catch_size_data <- catch_size_data %>%
   mutate_if(is.numeric, replace_na, replace = 0)
    
  catch_size_data <- catch_size_data %>%
    mutate(keep_tot = keep_adj+keep_adj2,
           release = ifelse(keep_adj==0 & keep_adj2==0,1,0))  
  
  #catch_size_data$release<-ifelse((catch_size_data$keep_adj==0 & catch_size_data$keep_adj2==0), 1,0)
  #catch_size_data$keep_tot<-catch_size_data$keep_adj+catch_size_data$keep_adj2
  
  
  
  catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_tot, release, period, catch_draw)) %>% 
    rename(keep = keep_tot)
  
  new_size_data <- catch_size_data %>%
    group_by(period, catch_draw, tripid, fitted_length) %>%
    summarize(keep = sum(keep),
              release = sum(release), .groups = "drop") #%>%
  
  summed_catch_data <- catch_size_data %>%
    group_by(period, catch_draw, tripid) %>%
    summarize(tot_keep_sf = sum(keep),
              tot_rel_sf = sum(release),
              .groups = "drop") #%>%
  
  
  
  keep_size_data <- new_size_data %>%
    #ungroup() %>%
    dplyr::select(-release) %>% 
    pivot_wider(names_from = fitted_length, #_length,
                names_glue = "keep_length_sf_{fitted_length}",
                names_sort = TRUE,
                values_from = keep, 
                values_fill = 0) # %>% 
  #I()
  #keep_size_data
  
  release_size_data <- new_size_data %>%
    #ungroup() %>% 
    dplyr::select(-keep) %>% 
    pivot_wider(names_from = fitted_length, #_length,
                names_glue = "release_length_sf_{fitted_length}",
                names_sort = TRUE,
                values_from = release, 
                values_fill = 0) #%>% 
  
  
  trip_data <- summed_catch_data %>% 
    left_join(keep_size_data, by = c("period", "catch_draw","tripid")) %>% 
    left_join(release_size_data, by = c("period", "catch_draw","tripid")) #%>% 
  
  #add the zero catch trips 
  trip_data <- bind_rows(trip_data, sf_zero_catch) %>% 
    #arrange(period, catch_draw, tripid) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(state = state1,
           tot_sf_catch = tot_keep_sf + tot_rel_sf)
  
  
  
  #######Black Sea Bass
  
  
  
  # subset trips with zero catch, as no size draws are required
  bsb_zero_catch <- filter(sf_bsb_catch_data, tot_bsb_catch == 0)
  
  #remove trips with zero summer flounder catch
  #sf_catch_data=sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
  bsb_catch_data <- filter(sf_bsb_catch_data, tot_bsb_catch > 0) 
  
  #expand the sf_catch_data so that each row represents a fish
  row_inds <- seq_len(nrow(bsb_catch_data))
  bsb_catch_data <- bsb_catch_data[c(rep(row_inds, bsb_catch_data$tot_bsb_catch)), ]
  rownames(bsb_catch_data) <- NULL
  bsb_catch_data$fishid <- 1:nrow(bsb_catch_data)
  
  
  # generate lengths for each fish
  catch_size_data <- bsb_catch_data %>% 
    mutate(fitted_length = sample(bsb_size_data$fitted_length,
                                  nrow(.),
                                  prob = bsb_size_data$fitted_prob,
                                  replace = TRUE)) #%>%
  
  
  catch_size_data <- catch_size_data %>% 
    left_join(regs, by = "period") %>% 
    mutate(posskeep = ifelse(fitted_length>=bsb_min ,1,0)) %>% 
    group_by(tripid, period, catch_draw) %>% 
    # keep = case_when(
    # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
    # TRUE ~ 0),
    mutate(csum_keep = cumsum(posskeep)) %>% 
    ungroup() %>% 
    mutate(
      keep_adj = case_when(
        bsb_bag > 0 ~ ifelse(csum_keep<=bsb_bag & posskeep==1,1,0),
        TRUE ~ 0))
  #,
  # keep_adj = case_when(
  #   csum_keep<=bag & keep==1 ~ 1,
  #   TRUE ~ 0),
  #release = case_when(
  # bsb_bag > 0 ~ ifelse(posskeep==0 | (posskeep==1 & csum_keep>bsb_bag ), 1,0)))
  
  #catch_size_data[is.na(catch_size_data)] <- 0
  catch_size_data <- catch_size_data %>%
    mutate_if(is.numeric, replace_na, replace = 0)
  
  catch_size_data <- catch_size_data %>%
    mutate(release = ifelse(keep_adj==0,1,0))  
  
  #catch_size_data$release<-ifelse((catch_size_data$keep_adj==0), 1,0)
  
  catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_adj, release, period, catch_draw)) %>% 
    rename(keep = keep_adj)
  
  
  new_size_data <- catch_size_data %>%
    group_by(period, catch_draw, tripid, fitted_length) %>%
    summarize(keep = sum(keep),
              release = sum(release), .groups = "drop") #%>%
  
  summed_catch_data <- catch_size_data %>%
    group_by(period, catch_draw, tripid) %>%
    summarize(tot_keep_bsb = sum(keep),
              tot_rel_bsb = sum(release),
              .groups = "drop") #%>%
  
  
  
  keep_size_data <- new_size_data %>%
    #ungroup() %>%
    dplyr::select(-release) %>% 
    pivot_wider(names_from = fitted_length, #_length,
                names_glue = "keep_length_bsb_{fitted_length}",
                names_sort = TRUE,
                values_from = keep, 
                values_fill = 0) # %>% 
  #I()
  #keep_size_data
  
  release_size_data <- new_size_data %>%
    #ungroup() %>% 
    dplyr::select(-keep) %>% 
    pivot_wider(names_from = fitted_length, #_length,
                names_glue = "release_length_bsb_{fitted_length}",
                names_sort = TRUE,
                values_from = release, 
                values_fill = 0) #%>% 
  
  
  trip_data_bsb <- summed_catch_data %>% 
    left_join(keep_size_data, by = c("period", "catch_draw","tripid")) %>% 
    left_join(release_size_data, by = c("period", "catch_draw","tripid")) #%>% 
  
  #add the zero catch trips 
  trip_data_bsb <- bind_rows(trip_data_bsb, bsb_zero_catch) %>% 
    #arrange(period, catch_draw, tripid) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(state = state1,
           tot_bsb_catch = tot_keep_bsb + tot_rel_bsb)
  
  
  # merge the bsb trip data with the rest of the trip data 
  trip_data <-  merge(trip_data,trip_data_bsb,by=c("period", "catch_draw", "tripid", "state"))
  
  
  
  ##########Scup
  # subset trips with zero catch, as no size draws are required
  
  if (scup_mu_param!=0){
    
    
    scup_zero_catch <- filter(sf_bsb_catch_data, tot_scup_catch == 0)
    
    #remove trips with zero summer flounder catch
    #sf_catch_data=sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
    scup_catch_data <- filter(sf_bsb_catch_data, tot_scup_catch > 0) 
    
    #expand the sf_catch_data so that each row represents a fish
    row_inds <- seq_len(nrow(scup_catch_data))
    scup_catch_data <- scup_catch_data[c(rep(row_inds, scup_catch_data$tot_scup_catch)), ]
    rownames(scup_catch_data) <- NULL
    scup_catch_data$fishid <- 1:nrow(scup_catch_data)
    
    
    # generate lengths for each fish
    catch_size_data <- scup_catch_data %>% 
      mutate(fitted_length = sample(scup_size_data$fitted_length,
                                    nrow(.),
                                    prob = scup_size_data$fitted_prob,
                                    replace = TRUE)) #%>%
    
    
    catch_size_data <- catch_size_data %>% 
      left_join(regs, by = "period") %>% 
      mutate(posskeep = ifelse(fitted_length>=scup_min ,1,0)) %>% 
      group_by(tripid, period, catch_draw) %>% 
      # keep = case_when(
      # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      # TRUE ~ 0),
      mutate(csum_keep = cumsum(posskeep)) %>% 
      ungroup() %>% 
      mutate(
        keep_adj = case_when(
          scup_bag > 0 ~ ifelse(csum_keep<=scup_bag & posskeep==1,1,0),
          TRUE ~ 0))
    #,
    # keep_adj = case_when(
    #   csum_keep<=bag & keep==1 ~ 1,
    #   TRUE ~ 0),
    #release = case_when(
    # scup_bag > 0 ~ ifelse(posskeep==0 | (posskeep==1 & csum_keep>scup_bag ), 1,0)))
    
    #catch_size_data[is.na(catch_size_data)] <- 0
    catch_size_data <- catch_size_data %>%
      mutate_if(is.numeric, replace_na, replace = 0)
    
    catch_size_data <- catch_size_data %>%
      mutate(release = ifelse(keep_adj==0,1,0))  
    
    #catch_size_data$release<-ifelse((catch_size_data$keep_adj==0), 1,0)
    
    catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_adj, release, period, catch_draw)) %>% 
      rename(keep = keep_adj)
    
    
    new_size_data <- catch_size_data %>%
      group_by(period, catch_draw, tripid, fitted_length) %>%
      summarize(keep = sum(keep),
                release = sum(release), .groups = "drop") #%>%
    
    summed_catch_data <- catch_size_data %>%
      group_by(period, catch_draw, tripid) %>%
      summarize(tot_keep_scup = sum(keep),
                tot_rel_scup = sum(release),
                .groups = "drop") #%>%
    
    
    
    keep_size_data <- new_size_data %>%
      #ungroup() %>%
      dplyr::select(-release) %>% 
      pivot_wider(names_from = fitted_length, #_length,
                  names_glue = "keep_length_scup_{fitted_length}",
                  names_sort = TRUE,
                  values_from = keep, 
                  values_fill = 0) # %>% 
    #I()
    #keep_size_data
    
    release_size_data <- new_size_data %>%
      #ungroup() %>% 
      dplyr::select(-keep) %>% 
      pivot_wider(names_from = fitted_length, #_length,
                  names_glue = "release_length_scup_{fitted_length}",
                  names_sort = TRUE,
                  values_from = release, 
                  values_fill = 0) #%>% 
    
    
    trip_data_scup <- summed_catch_data %>% 
      left_join(keep_size_data, by = c("period", "catch_draw","tripid")) %>% 
      left_join(release_size_data, by = c("period", "catch_draw","tripid")) #%>% 
    
    #add the zero catch trips 
    trip_data_scup <- bind_rows(trip_data_scup, scup_zero_catch) %>% 
      #arrange(period, catch_draw, tripid) %>% 
      mutate_if(is.numeric, replace_na, replace = 0) %>% 
      mutate(state = state1,
             tot_scup_catch = tot_keep_scup + tot_rel_scup)
    
    
    # merge the bsb trip data with the rest of the trip data 
    trip_data <-  merge(trip_data,trip_data_scup,by=c("period", "catch_draw", "tripid", "state"))
    
    trip_data<- subset(trip_data, select=-c(tot_bsb_catch, tot_bsb_catch.x, tot_bsb_catch.y,
                                            tot_sf_catch, tot_sf_catch.x, tot_sf_catch.y,
                                            tot_scup_catch, tot_scup_catch.x, tot_scup_catch.y))
    
    trip_data$tot_bsb_catch<-trip_data$tot_keep_bsb+trip_data$tot_rel_bsb
    trip_data$tot_sf_catch<-trip_data$tot_keep_sf+trip_data$tot_rel_sf
    trip_data$tot_scup_catch<-trip_data$tot_keep_scup+trip_data$tot_rel_scup
    
  }
  
  
  ##Exclude scup from DE, MD, and NC
  if (scup_mu_param==0){
    trip_data$tot_scup_catch<-0
    trip_data$tot_keep_scup<-0
    trip_data$tot_rel_scup<-0
  }
  
  # merge catch information for other species. Assume per-trip catch outcomes for these species are the same as the calibration. 
  # This info is contained in the costs_new_all_state datasets
  #bsb_sc_data <- subset(costs_new_all_MA, catch_draw<=nsamp, select=c(period, catch_draw, tripid,tot_keep_scup_base, tot_rel_scup_base)) %>%
  #  tibble()
  base_data <- costs_new_all %>% #tibble() %>% 
    filter(catch_draw<=nsamp) 
  
  
  
  # merge the trip data (summer flounder catch + lengths) with the other species data (numbers kept and released))
  #trip_data <-  merge(trip_data,bsb_sc_data,by="tripid") %>% 
  trip_data <- trip_data %>% 
    #ungroup() %>% 
    left_join(base_data, by = c("period","catch_draw","tripid", "state")) 
  # %>% 
  #   # zap <- bsb_sc_data %>% data.table()
  #   # zip <- trip_data %>% ungroup() %>% data.table()
  #   # zop <- zap[zip,on=c("period","catch_draw","tripid")]
  #   #mutate_if(is.numeric, replace_na, replace = 0) %>% 
  #   left_join(regs %>% dplyr::select(period, bsb_bag), by = c("period")) %>% 
  #   mutate(region = region1,
  #          keep_bsb = rbinom(nrow(.), tot_bsb_catch, prop_bsb_keep),   # GF adding BSB catch from draws to retain correlation
  #          tot_keep_bsb = ifelse(keep_bsb <= bsb_bag, keep_bsb, bsb_bag),
  #          tot_rel_bsb = tot_bsb_catch - tot_keep_bsb) %>%
  #   dplyr::select(-keep_bsb,-bsb_bag) %>% 
  #   dplyr::select(period, catch_draw, everything()) #%>% 
  
  
  # 
  # cost_data <- costs_new_all_MA %>% dplyr::select(-tot_sf_catch)
  # trip_data <- left_join(dfs,cost_data,by=c("period", "tripid", "catch_draw")) #%>% 
  #mutate_if(is.numeric, replace_na, replace = 0) #%>% 
  #I()
  #  trip_data[is.na(trip_data)] = 0
  
  
  #set up an output file for each draw of utility parameters. For now, only taking one draw. 
  # parameter_draws = list()
  # 
  # #for(d in 1:1) {
  # d <- as.integer(1)
  #dchoose = 1  
  # Use the previously drawn set of utility parameters to calculate expected utility, welfare, and effort in the prediction year
  #param_draws_MA_prediction = subset(param_draws_MA, parameter_draw=i)
  param_draws_MA_prediction = param_draws_MA  #%>% tibble() %>% filter(parameter_draw==dchoose)
  #trip_data =  merge(param_draws_MA_prediction,trip_data,by="tripid")
  trip_data <- right_join(param_draws_MA_prediction,trip_data,by=c("tripid"))
  #trip_data <- na.omit(trip_data)
  
  
  # Expected utility (prediction year)
  trip_data <-trip_data %>%
  mutate(
    vA = beta_sqrt_sf_keep*sqrt(tot_keep_sf) +
     beta_sqrt_sf_release*sqrt(tot_rel_sf) +  
     beta_sqrt_bsb_keep*sqrt(tot_keep_bsb) +
     beta_sqrt_bsb_release*sqrt(tot_rel_bsb) +  
     beta_sqrt_scup_catch*sqrt(tot_scup_catch) +
     beta_cost*cost,
  # Expected utility (base year)
   v0 = beta_sqrt_sf_keep*sqrt(tot_keep_sf_base) +
     beta_sqrt_sf_release*sqrt(tot_rel_sf_base) +  
     beta_sqrt_bsb_keep*sqrt(tot_keep_bsb_base) +
     beta_sqrt_bsb_release*sqrt(tot_rel_bsb_base) +  
     beta_sqrt_scup_catch*sqrt(tot_cat_scup_base) +
     beta_cost*cost) 
  
  trip_data <- trip_data %>% 
    mutate(period = as.numeric(period))
  
  # Collapse data from the X catch draws so that each row contains mean values
  #mean_trip_data <-aggregate(trip_data, by=list(trip_data$tripid),FUN=mean, na.rm=TRUE)
  mean_trip_data <- trip_data %>% 
    dplyr::select(-c("state")) %>% 
    data.table()
  mean_trip_data <- mean_trip_data[, lapply(.SD, mean), by=list(period,tripid)] %>% 
    tibble() #%>% 
  
  
  # nkeep <- trip_data %>%
  #   group_by(period, tripid) %>%
  #   summarise(keep_one = length(which(tot_keep>0))/length(tot_keep), #n(),
  #             .groups = "drop")
  # mean_trip_data <- left_join(mean_trip_data, nkeep, by = c("period", "tripid"))
  
  
  # Now expand the data to create three alternatives, representing the alternatives available in choice survey
  #mean_trip_data <- expandRows(mean_trip_data, 3, count.is.col = FALSE)
  mean_trip_data <- mean_trip_data %>% 
    mutate(n_alt = rep(2,nrow(.))) %>% 
    uncount(n_alt) %>% 
    mutate(alt = rep(1:2,nrow(.)/2),
           opt_out = ifelse(alt == 2, 1, 0))
  
  
  
  #Caluculate the expected utility of alts 2 parameters of the utility function
  mean_trip_data$vA_optout= mean_trip_data$beta_opt_out*mean_trip_data$opt_out 
  mean_trip_data$v0_optout= mean_trip_data$beta_opt_out*mean_trip_data$opt_out 
  
  
  #Now put the two values in the same column, exponentiate, and caluculate their sum (vA_col_sum)
  mean_trip_data$expon_vA <- case_when(mean_trip_data$alt==1 ~ exp(mean_trip_data$vA), 
                                       mean_trip_data$alt==2 ~ exp(mean_trip_data$vA_optout)) 
  
  mean_trip_data$expon_v0 <- case_when(mean_trip_data$alt==1 ~ exp(mean_trip_data$v0), 
                                       mean_trip_data$alt==2 ~ exp(mean_trip_data$v0_optout)) 
  
  mean_trip_data <- mean_trip_data %>% 
    group_by(period, tripid) %>% 
    mutate(vA_col_sum = sum(expon_vA),
           v0_col_sum = sum(expon_v0)) %>% 
    ungroup()
  
  mean_trip_data <- mean_trip_data %>% 
    mutate(change_CS = (1/beta_cost)*(log(vA_col_sum) - log(v0_col_sum))) %>% 
    mutate(probA = expon_vA/vA_col_sum,
           prob0 = expon_v0/v0_col_sum)
  
  
  
  # Get rid of things we don't need. 
  mean_trip_data <- subset(mean_trip_data, alt==1,select=-c(alt, beta_cost,beta_opt_out, beta_sqrt_bsb_keep, beta_sqrt_bsb_release, beta_sqrt_scup_catch, 
                                                            beta_sqrt_sf_keep ,beta_sqrt_sf_release, catch_draw, expon_v0 ,v0_col_sum, expon_vA, 
                                                            opt_out, v0, v0_optout, vA, vA_optout, vA_col_sum, tot_keep_bsb_base, tot_cat_scup_base, 
                                                            tot_rel_bsb_base, tot_rel_sf_base , tot_keep_sf_base)) 
  
  # Multiply the average trip probability by each of the catch variables (not the variables below) to get probability-weighted catch
  list_names <- colnames(mean_trip_data)[colnames(mean_trip_data) !="tripid" 
                                         & colnames(mean_trip_data) !="period" 
                                         & colnames(mean_trip_data) !="probA"
                                         & colnames(mean_trip_data) !="prob0" 
                                         & colnames(mean_trip_data) !="change_CS"]    
  
  # for (l in list_names){
  #   mean_trip_data[,l] <- mean_trip_data[,l]*mean_trip_data$probA
  # }
  
  
  mean_trip_data <- mean_trip_data %>% 
    mutate(across(.cols = all_of(list_names),.fns=function(x) probA*x))
  
  
  mean_trip_data <- mean_trip_data %>%
    mutate( n_choice_occasions_alt = rep(1,nrow(.)))
  
  
  #Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in 
  #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
  sims <- calibration_data %>% filter(sim==1) %>% 
    dplyr::select(c(n_choice_occasions, period)) %>% 
    left_join(mean_trip_data %>% count(period, name = "ndraws") %>% mutate(period = as.character(period)), by = "period") %>% 
    mutate(expand = n_choice_occasions/ndraws)
  #n_choice_occasions = mean(sims$n_choice_occasions)
  #ndraws = nrow(mean_trip_data)
  #expand=n_choice_occasions/ndraws
  
  
  #mean_trip_data$sim=1
  mean_trip_data <- mean_trip_data %>%
    mutate(sim = rep(1,nrow(.))) 
  
  #Here add other trip quality statistics
  #keep_one = ifelse(tot_keep>0,1,0))
  
  #sum probability weighted catch over all choice occasions
  #aggregate_trip_data <-aggregate(mean_trip_data, by=list(mean_trip_data$sim),FUN=sum, na.rm=TRUE)
  aggregate_trip_data <- mean_trip_data %>% 
    group_by(period, sim) %>% 
    summarize_all(sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    left_join(sims %>% mutate(period = as.numeric(period)), by = c("period"))
  # right_join(sims %>% mutate(period = as.numeric(period)) %>% dplyr::select(-n_choice_occasions),
  #            by = c("period","sim"))
  
  
  
  ls(aggregate_trip_data)
  list_names = colnames(aggregate_trip_data)[ colnames(aggregate_trip_data) !="tripid" 
                                              & colnames(aggregate_trip_data) !="catch_draw" & colnames(aggregate_trip_data) !="period"
                                              & colnames(aggregate_trip_data) !="vA" & colnames(aggregate_trip_data) !="v0"
                                              & colnames(aggregate_trip_data) != "state" 
                                              & colnames(aggregate_trip_data) != "ndraws" 
                                              & colnames(aggregate_trip_data) != "expand" & colnames(aggregate_trip_data) != "n_choice_occasions"
                                              & colnames(aggregate_trip_data) != "parameter_draw" & colnames(aggregate_trip_data) != "prob0"]
  #& colnames(mean_trip_data) !="cost" & colnames(mean_trip_data) !="vA" & colnames(mean_trip_data) !="v0"
  
  
  # for (l in list_names){
  #   mean_trip_data[,l] = mean_trip_data[,l]*expand
  # }
  
  
  #aggregate_trip_data[,list_names] <- aggregate_trip_data$expand*aggregate_trip_data[,list_names]
  
  aggregate_trip_data <- aggregate_trip_data %>% 
    mutate(across(.cols = all_of(list_names),.fns=function(x) expand*x))
  
  
  aggregate_trip_data = subset(aggregate_trip_data, select=-c(tripid, sim, prob0, expand, n_choice_occasions))
  names(aggregate_trip_data)[names(aggregate_trip_data) == "probA"] = "observed_trips"
  
  
  pds_new_all_MA <- aggregate_trip_data %>%  #list.stack(pds_new, fill=TRUE) %>% 
    #mutate_at(vars(contains("length")), replace_na, replace = 0) %>% 
    #pds_new_all_MA[is.na(pds_new_all_MA)] = 0
    mutate(state = state1)
  
  #})
  # write_xlsx(pds_new_all_MA,"MA_prediction_output_check.xlsx")
  return(pds_new_all_MA)
  
  #end function
}



# predictions = list()
# years<-c("2022", "2020", "2019", "2018")
# 
# for (x in 1:3){
#   #for (y in years){
#     year <- 2019
# 
# # THIS IS WHERE TO IMPORT THE ALKS FOR EACH SPECIES
# # Import the fluke ALK (in centimeters) provided by M. Terceiro
# fluke_ALK <- data.frame(read_csv("fluke_ALK_2018_adj.csv", show_col_types = FALSE))
# #bsb_ALK <- data.frame(read_csv("", show_col_types = FALSE))
# scup_ALK <- data.frame(read_csv("scup_ALK_2015_2018_adj.csv", show_col_types = FALSE))
# 
# 
# 
# # THIS IS WHERE TO IMPORT THE NUMBERS AT AGE FOR EACH SPECIES BASED ON THE YEAR(S) OF INTEREST
# # Import the fluke MCMC draws
# fluke_numbers_at_age = data.frame(read_csv(paste0("fluke_MCMC_100_", year,".csv"), show_col_types = FALSE))
# #fluke_numbers_at_age = data.frame(read_csv("fluke_MCMC_100_2023.csv", show_col_types = FALSE))
# fluke_numbers_at_age = subset(fluke_numbers_at_age, fluke_numbers_at_age$draw==x)
# 
# #this is the check dataset with median values of the 2021 stock
# #fluke_numbers_at_age = data.frame(read_csv(paste0("fluke_MCMC_median_", year,".csv"), show_col_types = FALSE))
# 
# #fluke_numbers_at_age = subset(fluke_numbers_at_age, fluke_numbers_at_age$draw==1)
# 
# # Import the bsb MCMC draws
# #bsb_numbers_at_age = data.frame(read_csv("bsb_MCMC_100_2021.csv", show_col_types = FALSE))
# #bsb_numbers_at_age = subset(bsb_numbers_at_age, bsb_numbers_at_age$draw==1)
# 
# # Import the scup MCMC draws
# scup_numbers_at_age = data.frame(read_csv(paste0("scup_MCMC_100_", year,".csv"), show_col_types = FALSE))
# scup_numbers_at_age = subset(scup_numbers_at_age, scup_numbers_at_age$draw==x)
# 
# #scup_numbers_at_age = data.frame(read_csv("scup_MCMC_100_2023.csv", show_col_types = FALSE))
# 
# #this is the check dataset with median values of the 2021 stock
# #scup_numbers_at_age = data.frame(read_csv(paste0("scup_MCMC_median_", year,".csv"), show_col_types = FALSE))
# 
# #scup_numbers_at_age = subset(scup_numbers_at_age, scup_numbers_at_age$draw==1)
# 
# 
# source("CAL given stock structure.R")
# 
# ##########
# 
# 
# 
# ##########
# # run the simulation code under the new set of regulations (regulation file is directed_trips_region - alternative regs test.xlsx)
# 
# directed_trip_alt_regs=data.frame(read_csv(paste0("directed trips and regulations ", year,".csv"), show_col_types = FALSE))
# #directed_trip_alt_regs=data.frame(read_csv("directed trips and regulations 2023.1.csv", show_col_types = FALSE))
# 
# directed_trip_alt_regs$dtrip_2019=round(directed_trip_alt_regs$dtrip)
# 
# 
# 
# MA_pred <- predict_rec_catch(state1 <- "MA",
#                              calibration_data_table <- calibration_data_table_base[[3]],
#                              directed_trips_table <- directed_trips_table_base[[3]],
#                              sf_size_data_read <- sf_size_data_read_base[[3]],
#                              bsb_size_data_read <- bsb_size_data_read_base[[3]],
#                              scup_size_data_read <- scup_size_data_read_base[[3]],
#                              param_draws_MA <- param_draws_MA,
#                              costs_new_all <- costs_new_all_MA,
#                              sf_catch_data_all <- sf_catch_data_ma)
# 
# RI_pred <- predict_rec_catch(state1 <- "RI",
#                              calibration_data_table <- calibration_data_table_base[[8]],
#                              directed_trips_table <- directed_trips_table_base[[8]],
#                              sf_size_data_read <- sf_size_data_read_base[[8]],
#                              bsb_size_data_read <- bsb_size_data_read_base[[8]],
#                              scup_size_data_read <- scup_size_data_read_base[[8]],
#                              param_draws_MA <- param_draws_RI,
#                              costs_new_all <- costs_new_all_RI,
#                              sf_catch_data_all <- sf_catch_data_ri)
# 
# CT_pred <- predict_rec_catch(state1 <- "CT",
#                              calibration_data_table <- calibration_data_table_base[[1]],
#                              directed_trips_table <- directed_trips_table_base[[1]],
#                              sf_size_data_read <- sf_size_data_read_base[[1]],
#                              bsb_size_data_read <- bsb_size_data_read_base[[1]],
#                              scup_size_data_read <- scup_size_data_read_base[[1]],
#                              param_draws_MA <- param_draws_CT,
#                              costs_new_all <- costs_new_all_CT,
#                              sf_catch_data_all <- sf_catch_data_ct)
# 
# 
# NY_pred <- predict_rec_catch(state1 <- "NY",
#                              calibration_data_table <- calibration_data_table_base[[7]],
#                              directed_trips_table <- directed_trips_table_base[[7]],
#                              sf_size_data_read <- sf_size_data_read_base[[7]],
#                              bsb_size_data_read <- bsb_size_data_read_base[[7]],
#                              scup_size_data_read <- scup_size_data_read_base[[7]],
#                              param_draws_MA <- param_draws_NY,
#                              costs_new_all <- costs_new_all_NY,
#                              sf_catch_data_all <- sf_catch_data_ny)
# 
# NJ_pred <- predict_rec_catch(state1 <- "NJ",
#                              calibration_data_table <- calibration_data_table_base[[6]],
#                              directed_trips_table <- directed_trips_table_base[[6]],
#                              sf_size_data_read <- sf_size_data_read_base[[6]],
#                              bsb_size_data_read <- bsb_size_data_read_base[[6]],
#                              scup_size_data_read <- scup_size_data_read_base[[6]],
#                              param_draws_MA <- param_draws_NJ,
#                              costs_new_all <- costs_new_all_NJ,
#                              sf_catch_data_all <- sf_catch_data_nj)
# 
# 
# DE_pred <- predict_rec_catch(state1 <- "DE",
#                              calibration_data_table <- calibration_data_table_base[[2]],
#                              directed_trips_table <- directed_trips_table_base[[2]],
#                              sf_size_data_read <- sf_size_data_read_base[[2]],
#                              bsb_size_data_read <- bsb_size_data_read_base[[2]],
#                              scup_size_data_read <- scup_size_data_read_base[[2]],
#                              param_draws_MA <- param_draws_DE,
#                              costs_new_all <- costs_new_all_DE,
#                              sf_catch_data_all <- sf_catch_data_de)
# 
# 
# MD_pred <- predict_rec_catch(state1 <- "MD",
#                              calibration_data_table <- calibration_data_table_base[[4]],
#                              directed_trips_table <- directed_trips_table_base[[4]],
#                              sf_size_data_read <- sf_size_data_read_base[[4]],
#                              bsb_size_data_read <- bsb_size_data_read_base[[4]],
#                              scup_size_data_read <- scup_size_data_read_base[[4]],
#                              param_draws_MA <- param_draws_MD,
#                              costs_new_all <- costs_new_all_MD,
#                              sf_catch_data_all <- sf_catch_data_md)
# 
# VA_pred <- predict_rec_catch(state1 <- "VA",
#                              calibration_data_table <- calibration_data_table_base[[9]],
#                              directed_trips_table <- directed_trips_table_base[[9]],
#                              sf_size_data_read <- sf_size_data_read_base[[9]],
#                              bsb_size_data_read <- bsb_size_data_read_base[[9]],
#                              scup_size_data_read <- scup_size_data_read_base[[9]],
#                              param_draws_MA <- param_draws_VA,
#                              costs_new_all <- costs_new_all_VA,
#                              sf_catch_data_all <- sf_catch_data_va)
# 
# NC_pred <- predict_rec_catch(state1 <- "NC",
#                              calibration_data_table <- calibration_data_table_base[[5]],
#                              directed_trips_table <- directed_trips_table_base[[5]],
#                              sf_size_data_read <- sf_size_data_read_base[[5]],
#                              bsb_size_data_read <- bsb_size_data_read_base[[5]],
#                              scup_size_data_read <- scup_size_data_read_base[[5]],
#                              param_draws_MA <- param_draws_NC,
#                              costs_new_all <- costs_new_all_NC,
#                              sf_catch_data_all <- sf_catch_data_nc)
# 
# 
# predictions[[x]]<- dplyr::bind_rows(MA_pred, RI_pred, CT_pred, NY_pred, NJ_pred, DE_pred, MD_pred, VA_pred, NC_pred)
# }
# 
#   predictions_all= list.stack(predictions, fill=TRUE)
#   predictions_all[is.na(predictions_all)] = 0
# 
