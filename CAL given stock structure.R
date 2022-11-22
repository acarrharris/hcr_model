

# This code uses  recreational selectivity (q) in the baseline year to determine catch-at-length given any size structure of the stock

# Code requires the following data: 
# AgE length key (for now using 2018 commercial survey ALK ): com_sv_len_age_adj_2018.xlsx
# Numbers at age file, in 1000s of fish: numbers_at_age_2018.csv <-- This will be the output from the operating model
# Baseline recreational selectivity: rec_selectivity.xlsx


######################################### 
########## BEGIN FLUKE ##################
######################################### 


# Merge the ALK and Na datasets and create population numbers-at-length (inches)
fluke_numbers_at_length <-  merge(fluke_ALK,fluke_numbers_at_age,by="age", all.x=TRUE, all.y=TRUE)
fluke_numbers_at_length$N_l <- fluke_numbers_at_length$proportion*fluke_numbers_at_length$Na

fluke_numbers_at_length <- aggregate(fluke_numbers_at_length, by=list(fluke_numbers_at_length$l_in_bin),FUN=sum, na.rm=TRUE)
fluke_numbers_at_length <-subset(fluke_numbers_at_length, select=c(Group.1,N_l))
names(fluke_numbers_at_length)[names(fluke_numbers_at_length) == "Group.1"] <- "length"
sum(fluke_numbers_at_length$N_l)

# Translate cms's to inches 
fluke_numbers_at_length$length <- round(fluke_numbers_at_length$length/2.54)
fluke_numbers_at_length <- aggregate(fluke_numbers_at_length, by=list(fluke_numbers_at_length$length),FUN=sum, na.rm=TRUE)
fluke_numbers_at_length <-subset(fluke_numbers_at_length, select=c(Group.1,N_l))
names(fluke_numbers_at_length)[names(fluke_numbers_at_length) == "Group.1"] <- "length"


# Import and merge the selectivity data to this file 
fluke_selectivity <- data.frame(read_csv("avg_rec_selectivity_fluke.csv", show_col_types = FALSE))
fluke_numbers_at_length_new <-  merge(fluke_selectivity,fluke_numbers_at_length,by=c("length"),  all.x=TRUE, all.y=TRUE)
fluke_numbers_at_length_new <- subset(fluke_numbers_at_length_new, N_l!=0 & state!=0)


# Create catch-at-length based on the new numbers-at-length
fluke_numbers_at_length_new$q <- as.numeric(fluke_numbers_at_length_new$q)

#numbers_at_length_new$C_l_new <- (numbers_at_length_new$q)*(numbers_at_length_new$N_l)*(numbers_at_length_new$E)
fluke_numbers_at_length_new$C_l_new <- (fluke_numbers_at_length_new$q)*(fluke_numbers_at_length_new$N_l)

sum(fluke_numbers_at_length_new$C_l_new)
sum(fluke_numbers_at_length_new$tot_cat)


###########


# subset the catch-at-length new datstet by region
fluke_Cl_new_MA <-subset(fluke_numbers_at_length_new, state=="MA", select=c(length, C_l_new))
fluke_Cl_new_RI <-subset(fluke_numbers_at_length_new, state=="RI", select=c(length, C_l_new))
fluke_Cl_new_CT <-subset(fluke_numbers_at_length_new, state=="CT", select=c(length, C_l_new))
fluke_Cl_new_NY <-subset(fluke_numbers_at_length_new, state=="NY", select=c(length, C_l_new))
fluke_Cl_new_NJ <-subset(fluke_numbers_at_length_new, state=="NJ", select=c(length, C_l_new))
fluke_Cl_new_DE <-subset(fluke_numbers_at_length_new, state=="DE", select=c(length, C_l_new))
fluke_Cl_new_MD <-subset(fluke_numbers_at_length_new, state=="MD", select=c(length, C_l_new))
fluke_Cl_new_VA <-subset(fluke_numbers_at_length_new, state=="VA", select=c(length, C_l_new))
fluke_Cl_new_NC <-subset(fluke_numbers_at_length_new, state=="NC", select=c(length, C_l_new))

fluke_Cl_new_MA$C_l_new=round(fluke_Cl_new_MA$C_l_new)
fluke_Cl_new_RI$C_l_new=round(fluke_Cl_new_RI$C_l_new)
fluke_Cl_new_CT$C_l_new=round(fluke_Cl_new_CT$C_l_new)
fluke_Cl_new_NY$C_l_new=round(fluke_Cl_new_NY$C_l_new)
fluke_Cl_new_NJ$C_l_new=round(fluke_Cl_new_NJ$C_l_new)
fluke_Cl_new_DE$C_l_new=round(fluke_Cl_new_DE$C_l_new)
fluke_Cl_new_MD$C_l_new=round(fluke_Cl_new_MD$C_l_new)
fluke_Cl_new_VA$C_l_new=round(fluke_Cl_new_VA$C_l_new)
fluke_Cl_new_NC$C_l_new=round(fluke_Cl_new_NC$C_l_new)

tot_pred_fluke_cat_MA=sum(fluke_Cl_new_MA$C_l_new)
tot_pred_fluke_cat_RI=sum(fluke_Cl_new_RI$C_l_new)
tot_pred_fluke_cat_CT=sum(fluke_Cl_new_CT$C_l_new)
tot_pred_fluke_cat_NY=sum(fluke_Cl_new_NY$C_l_new)
tot_pred_fluke_cat_NJ=sum(fluke_Cl_new_NJ$C_l_new)
tot_pred_fluke_cat_DE=sum(fluke_Cl_new_DE$C_l_new)
tot_pred_fluke_cat_MD=sum(fluke_Cl_new_MD$C_l_new)
tot_pred_fluke_cat_VA=sum(fluke_Cl_new_VA$C_l_new)
tot_pred_fluke_cat_NC=sum(fluke_Cl_new_NC$C_l_new)

tot_cat_MA_base=sum(subset(fluke_selectivity, state == "MA")$tot_cat)
tot_cat_RI_base=sum(subset(fluke_selectivity, state == "RI")$tot_cat)
tot_cat_CT_base=sum(subset(fluke_selectivity, state == "CT")$tot_cat)
tot_cat_NY_base=sum(subset(fluke_selectivity, state == "NY")$tot_cat)
tot_cat_NJ_base=sum(subset(fluke_selectivity, state == "NJ")$tot_cat)
tot_cat_DE_base=sum(subset(fluke_selectivity, state == "DE")$tot_cat)
tot_cat_MD_base=sum(subset(fluke_selectivity, state == "MD")$tot_cat)
tot_cat_VA_base=sum(subset(fluke_selectivity, state == "VA")$tot_cat)
tot_cat_NC_base=sum(subset(fluke_selectivity, state == "NC")$tot_cat)

#Create a factor that expands total catch in the prediction year
fluke_catch_expansion_factor_MA=round(tot_pred_fluke_cat_MA/tot_cat_MA_base, digits=4)
fluke_catch_expansion_factor_RI=round(tot_pred_fluke_cat_RI/tot_cat_RI_base, digits=4)
fluke_catch_expansion_factor_CT=round(tot_pred_fluke_cat_CT/tot_cat_CT_base, digits=4)
fluke_catch_expansion_factor_NY=round(tot_pred_fluke_cat_NY/tot_cat_NY_base, digits=4)
fluke_catch_expansion_factor_NJ=round(tot_pred_fluke_cat_NJ/tot_cat_NJ_base, digits=4)
fluke_catch_expansion_factor_DE=round(tot_pred_fluke_cat_DE/tot_cat_DE_base, digits=4)
fluke_catch_expansion_factor_MD=round(tot_pred_fluke_cat_MD/tot_cat_MD_base, digits=4)
fluke_catch_expansion_factor_VA=round(tot_pred_fluke_cat_VA/tot_cat_VA_base, digits=4)
fluke_catch_expansion_factor_NC=round(tot_pred_fluke_cat_NC/tot_cat_NC_base, digits=4)

fluke_catch_expansion_factor_MA=1
fluke_catch_expansion_factor_RI=1
fluke_catch_expansion_factor_CT=1
fluke_catch_expansion_factor_NY=1
fluke_catch_expansion_factor_NJ=1
fluke_catch_expansion_factor_DE=1
fluke_catch_expansion_factor_MD=1
fluke_catch_expansion_factor_VA=1
fluke_catch_expansion_factor_NC=1


######################################### 
############ END FLUKE ##################
######################################### 




######################################### 
########## BEGIN BSB ##################
######################################### 


# # Merge the ALK and Na datasets and create population numbers-at-length (inches)
# bsb_numbers_at_length <-  merge(bsb_ALK,bsb_numbers_at_age,by="age", all.x=TRUE, all.y=TRUE)
# bsb_numbers_at_length$N_l <- bsb_numbers_at_length$proportion*bsb_numbers_at_length$Na
# 
# bsb_numbers_at_length <- aggregate(bsb_numbers_at_length, by=list(bsb_numbers_at_length$l_in_bin),FUN=sum, na.rm=TRUE)
# bsb_numbers_at_length <-subset(bsb_numbers_at_length, select=c(Group.1,N_l))
# names(bsb_numbers_at_length)[names(bsb_numbers_at_length) == "Group.1"] <- "length"
# sum(bsb_numbers_at_length$N_l)
# 
# # Translate cms's to inches 
# bsb_numbers_at_length$length <- round(bsb_numbers_at_length$length/2.54)
# bsb_numbers_at_length <- aggregate(bsb_numbers_at_length, by=list(bsb_numbers_at_length$length),FUN=sum, na.rm=TRUE)
# bsb_numbers_at_length <-subset(bsb_numbers_at_length, select=c(Group.1,N_l))
# names(bsb_numbers_at_length)[names(bsb_numbers_at_length) == "Group.1"] <- "length"
# 
# 
# # Import and merge the selectivity data to this file 
# bsb_selectivity <- data.frame(read_csv("rec_selectivity_bsb_2021.csv", show_col_types = FALSE))
# bsb_numbers_at_length_new <-  merge(bsb_selectivity,bsb_numbers_at_length,by=c("length"),  all.x=TRUE, all.y=TRUE)
# bsb_numbers_at_length_new <- subset(bsb_numbers_at_length_new, N_l!=0 & state!=0)
# 
# 
# # Create catch-at-length based on the new numbers-at-length
# bsb_numbers_at_length_new$q <- as.numeric(bsb_numbers_at_length_new$q)
# 
# #numbers_at_length_new$C_l_new <- (numbers_at_length_new$q)*(numbers_at_length_new$N_l)*(numbers_at_length_new$E)
# bsb_numbers_at_length_new$C_l_new <- (bsb_numbers_at_length_new$q)*(bsb_numbers_at_length_new$N_l)
# 
# sum(bsb_numbers_at_length_new$C_l_new)
# sum(bsb_numbers_at_length_new$tot_cat)
# 
# 
# ###########
# 
# 
# # subset the catch-at-length new datstet by region
# bsb_Cl_new_MA <-subset(bsb_numbers_at_length_new, state=="MA", select=c(length, C_l_new))
# bsb_Cl_new_RI <-subset(bsb_numbers_at_length_new, state=="RI", select=c(length, C_l_new))
# bsb_Cl_new_CT <-subset(bsb_numbers_at_length_new, state=="CT", select=c(length, C_l_new))
# bsb_Cl_new_NY <-subset(bsb_numbers_at_length_new, state=="NY", select=c(length, C_l_new))
# bsb_Cl_new_NJ <-subset(bsb_numbers_at_length_new, state=="NJ", select=c(length, C_l_new))
# bsb_Cl_new_DE <-subset(bsb_numbers_at_length_new, state=="DE", select=c(length, C_l_new))
# bsb_Cl_new_MD <-subset(bsb_numbers_at_length_new, state=="MD", select=c(length, C_l_new))
# bsb_Cl_new_VA <-subset(bsb_numbers_at_length_new, state=="VA", select=c(length, C_l_new))
# bsb_Cl_new_NC <-subset(bsb_numbers_at_length_new, state=="NC", select=c(length, C_l_new))
# 
# bsb_Cl_new_MA$C_l_new=round(bsb_Cl_new_MA$C_l_new)
# bsb_Cl_new_RI$C_l_new=round(bsb_Cl_new_RI$C_l_new)
# bsb_Cl_new_CT$C_l_new=round(bsb_Cl_new_CT$C_l_new)
# bsb_Cl_new_NY$C_l_new=round(bsb_Cl_new_NY$C_l_new)
# bsb_Cl_new_NJ$C_l_new=round(bsb_Cl_new_NJ$C_l_new)
# bsb_Cl_new_DE$C_l_new=round(bsb_Cl_new_DE$C_l_new)
# bsb_Cl_new_MD$C_l_new=round(bsb_Cl_new_MD$C_l_new)
# bsb_Cl_new_VA$C_l_new=round(bsb_Cl_new_VA$C_l_new)
# bsb_Cl_new_NC$C_l_new=round(bsb_Cl_new_NC$C_l_new)
# 
# tot_pred_bsb_cat_MA=sum(bsb_Cl_new_MA$C_l_new)
# tot_pred_bsb_cat_RI=sum(bsb_Cl_new_RI$C_l_new)
# tot_pred_bsb_cat_CT=sum(bsb_Cl_new_CT$C_l_new)
# tot_pred_bsb_cat_NY=sum(bsb_Cl_new_NY$C_l_new)
# tot_pred_bsb_cat_NJ=sum(bsb_Cl_new_NJ$C_l_new)
# tot_pred_bsb_cat_DE=sum(bsb_Cl_new_DE$C_l_new)
# tot_pred_bsb_cat_MD=sum(bsb_Cl_new_MD$C_l_new)
# tot_pred_bsb_cat_VA=sum(bsb_Cl_new_VA$C_l_new)
# tot_pred_bsb_cat_NC=sum(bsb_Cl_new_NC$C_l_new)
# 
# tot_cat_MA_base=sum(subset(bsb_selectivity, state == "MA")$tot_cat)
# tot_cat_RI_base=sum(subset(bsb_selectivity, state == "RI")$tot_cat)
# tot_cat_CT_base=sum(subset(bsb_selectivity, state == "CT")$tot_cat)
# tot_cat_NY_base=sum(subset(bsb_selectivity, state == "NY")$tot_cat)
# tot_cat_NJ_base=sum(subset(bsb_selectivity, state == "NJ")$tot_cat)
# tot_cat_DE_base=sum(subset(bsb_selectivity, state == "DE")$tot_cat)
# tot_cat_MD_base=sum(subset(bsb_selectivity, state == "MD")$tot_cat)
# tot_cat_VA_base=sum(subset(bsb_selectivity, state == "VA")$tot_cat)
# tot_cat_NC_base=sum(subset(bsb_selectivity, state == "NC")$tot_cat)
# 
# #Create a factor that expands total catch in the prediction year
# bsb_catch_expansion_factor_MA=round(tot_pred_bsb_cat_MA/tot_cat_MA_base, digits=4)
# bsb_catch_expansion_factor_RI=round(tot_pred_bsb_cat_RI/tot_cat_RI_base, digits=4)
# bsb_catch_expansion_factor_CT=round(tot_pred_bsb_cat_CT/tot_cat_CT_base, digits=4)
# bsb_catch_expansion_factor_NY=round(tot_pred_bsb_cat_NY/tot_cat_NY_base, digits=4)
# bsb_catch_expansion_factor_NJ=round(tot_pred_bsb_cat_NJ/tot_cat_NJ_base, digits=4)
# bsb_catch_expansion_factor_DE=round(tot_pred_bsb_cat_DE/tot_cat_DE_base, digits=4)
# bsb_catch_expansion_factor_MD=round(tot_pred_bsb_cat_MD/tot_cat_MD_base, digits=4)
# bsb_catch_expansion_factor_VA=round(tot_pred_bsb_cat_VA/tot_cat_VA_base, digits=4)
# bsb_catch_expansion_factor_NC=round(tot_pred_bsb_cat_NC/tot_cat_NC_base, digits=4)



###########Place holder for BSB

####Sizes for next year, for now, will be equal to baseline year
bsb_selectivity <- data.frame(read_csv("rec_selectivity_bsb_2021.csv", show_col_types = FALSE))
bsb_Cl_new_MA <-subset(bsb_selectivity, state=="MA", select=c(length, prob_star))
bsb_Cl_new_RI <-subset(bsb_selectivity, state=="RI", select=c(length, prob_star))
bsb_Cl_new_CT <-subset(bsb_selectivity, state=="CT", select=c(length, prob_star))
bsb_Cl_new_NY <-subset(bsb_selectivity, state=="NY", select=c(length, prob_star))
bsb_Cl_new_NJ <-subset(bsb_selectivity, state=="NJ", select=c(length, prob_star))
bsb_Cl_new_DE <-subset(bsb_selectivity, state=="DE", select=c(length, prob_star))
bsb_Cl_new_MD <-subset(bsb_selectivity, state=="MD", select=c(length, prob_star))
bsb_Cl_new_VA <-subset(bsb_selectivity, state=="VA", select=c(length, prob_star))
bsb_Cl_new_NC <-subset(bsb_selectivity, state=="NC", select=c(length, prob_star))

 bsb_catch_expansion_factor_MA=1
 bsb_catch_expansion_factor_RI=1
 bsb_catch_expansion_factor_CT=1
 bsb_catch_expansion_factor_NY=1
 bsb_catch_expansion_factor_NJ=1
 bsb_catch_expansion_factor_DE=1
 bsb_catch_expansion_factor_MD=1
 bsb_catch_expansion_factor_VA=1
 bsb_catch_expansion_factor_NC=1

######################################### 
############ END BSB ##################
######################################### 




######################################### 
########## BEGIN SCUP ##################
######################################### 


# Merge the ALK and Na datasets and create population numbers-at-length (inches)
scup_numbers_at_length <-  merge(scup_ALK,scup_numbers_at_age,by="age", all.x=TRUE, all.y=TRUE)
scup_numbers_at_length$N_l <- scup_numbers_at_length$proportion*scup_numbers_at_length$Na

scup_numbers_at_length <- aggregate(scup_numbers_at_length, by=list(scup_numbers_at_length$l_in_bin),FUN=sum, na.rm=TRUE)
scup_numbers_at_length <-subset(scup_numbers_at_length, select=c(Group.1,N_l))
names(scup_numbers_at_length)[names(scup_numbers_at_length) == "Group.1"] <- "length"
sum(scup_numbers_at_length$N_l)

# Translate cms's to inches 
scup_numbers_at_length$length <- round(scup_numbers_at_length$length/2.54)
scup_numbers_at_length <- aggregate(scup_numbers_at_length, by=list(scup_numbers_at_length$length),FUN=sum, na.rm=TRUE)
scup_numbers_at_length <-subset(scup_numbers_at_length, select=c(Group.1,N_l))
names(scup_numbers_at_length)[names(scup_numbers_at_length) == "Group.1"] <- "length"


# Import and merge the selectivity data to this file 
scup_selectivity <- data.frame(read_csv("avg_rec_selectivity_scup.csv", show_col_types = FALSE))
scup_numbers_at_length_new <-  merge(scup_selectivity,scup_numbers_at_length,by=c("length"),  all.x=TRUE, all.y=TRUE)
scup_numbers_at_length_new <- subset(scup_numbers_at_length_new, N_l!=0 & state!=0)


# Create catch-at-length based on the new numbers-at-length
scup_numbers_at_length_new$q <- as.numeric(scup_numbers_at_length_new$q)

#numbers_at_length_new$C_l_new <- (numbers_at_length_new$q)*(numbers_at_length_new$N_l)*(numbers_at_length_new$E)
scup_numbers_at_length_new$C_l_new <- (scup_numbers_at_length_new$q)*(scup_numbers_at_length_new$N_l)

sum(scup_numbers_at_length_new$C_l_new)
sum(scup_numbers_at_length_new$tot_cat)


###########


# subset the catch-at-length new datstet by region
scup_Cl_new_MA <-subset(scup_numbers_at_length_new, state=="MA", select=c(length, C_l_new))
scup_Cl_new_RI <-subset(scup_numbers_at_length_new, state=="RI", select=c(length, C_l_new))
scup_Cl_new_CT <-subset(scup_numbers_at_length_new, state=="CT", select=c(length, C_l_new))
scup_Cl_new_NY <-subset(scup_numbers_at_length_new, state=="NY", select=c(length, C_l_new))
scup_Cl_new_NJ <-subset(scup_numbers_at_length_new, state=="NJ", select=c(length, C_l_new))
scup_Cl_new_DE <-subset(scup_numbers_at_length_new, state=="DE", select=c(length, C_l_new))
scup_Cl_new_MD <-subset(scup_numbers_at_length_new, state=="MD", select=c(length, C_l_new))
scup_Cl_new_VA <-subset(scup_numbers_at_length_new, state=="VA", select=c(length, C_l_new))
scup_Cl_new_NC <-subset(scup_numbers_at_length_new, state=="NC", select=c(length, C_l_new))

scup_Cl_new_MA$C_l_new=round(scup_Cl_new_MA$C_l_new)
scup_Cl_new_RI$C_l_new=round(scup_Cl_new_RI$C_l_new)
scup_Cl_new_CT$C_l_new=round(scup_Cl_new_CT$C_l_new)
scup_Cl_new_NY$C_l_new=round(scup_Cl_new_NY$C_l_new)
scup_Cl_new_NJ$C_l_new=round(scup_Cl_new_NJ$C_l_new)
scup_Cl_new_DE$C_l_new=round(scup_Cl_new_DE$C_l_new)
scup_Cl_new_MD$C_l_new=round(scup_Cl_new_MD$C_l_new)
scup_Cl_new_VA$C_l_new=round(scup_Cl_new_VA$C_l_new)
scup_Cl_new_NC$C_l_new=round(scup_Cl_new_NC$C_l_new)

tot_pred_scup_cat_MA=sum(scup_Cl_new_MA$C_l_new)
tot_pred_scup_cat_RI=sum(scup_Cl_new_RI$C_l_new)
tot_pred_scup_cat_CT=sum(scup_Cl_new_CT$C_l_new)
tot_pred_scup_cat_NY=sum(scup_Cl_new_NY$C_l_new)
tot_pred_scup_cat_NJ=sum(scup_Cl_new_NJ$C_l_new)
tot_pred_scup_cat_DE=sum(scup_Cl_new_DE$C_l_new)
tot_pred_scup_cat_MD=sum(scup_Cl_new_MD$C_l_new)
tot_pred_scup_cat_VA=sum(scup_Cl_new_VA$C_l_new)
tot_pred_scup_cat_NC=sum(scup_Cl_new_NC$C_l_new)

tot_cat_MA_base=sum(subset(scup_selectivity, state == "MA")$tot_cat)
tot_cat_RI_base=sum(subset(scup_selectivity, state == "RI")$tot_cat)
tot_cat_CT_base=sum(subset(scup_selectivity, state == "CT")$tot_cat)
tot_cat_NY_base=sum(subset(scup_selectivity, state == "NY")$tot_cat)
tot_cat_NJ_base=sum(subset(scup_selectivity, state == "NJ")$tot_cat)
tot_cat_DE_base=sum(subset(scup_selectivity, state == "DE")$tot_cat)
tot_cat_MD_base=sum(subset(scup_selectivity, state == "MD")$tot_cat)
tot_cat_VA_base=sum(subset(scup_selectivity, state == "VA")$tot_cat)
tot_cat_NC_base=sum(subset(scup_selectivity, state == "NC")$tot_cat)

#Create a factor that expands total catch in the prediction year
scup_catch_expansion_factor_MA=round(tot_pred_scup_cat_MA/tot_cat_MA_base, digits=4)
scup_catch_expansion_factor_RI=round(tot_pred_scup_cat_RI/tot_cat_RI_base, digits=4)
scup_catch_expansion_factor_CT=round(tot_pred_scup_cat_CT/tot_cat_CT_base, digits=4)
scup_catch_expansion_factor_NY=round(tot_pred_scup_cat_NY/tot_cat_NY_base, digits=4)
scup_catch_expansion_factor_NJ=round(tot_pred_scup_cat_NJ/tot_cat_NJ_base, digits=4)
scup_catch_expansion_factor_DE=round(tot_pred_scup_cat_DE/tot_cat_DE_base, digits=4)
scup_catch_expansion_factor_MD=round(tot_pred_scup_cat_MD/tot_cat_MD_base, digits=4)
scup_catch_expansion_factor_VA=round(tot_pred_scup_cat_VA/tot_cat_VA_base, digits=4)
scup_catch_expansion_factor_NC=round(tot_pred_scup_cat_NC/tot_cat_NC_base, digits=4)

scup_catch_expansion_factor_MA=1
scup_catch_expansion_factor_RI=1
scup_catch_expansion_factor_CT=1
scup_catch_expansion_factor_NY=1
scup_catch_expansion_factor_NJ=1
scup_catch_expansion_factor_DE=1
scup_catch_expansion_factor_MD=1
scup_catch_expansion_factor_VA=1
scup_catch_expansion_factor_NC=1


######################################### 
############ END SCUP ##################
######################################### 







##########
# Here, execute the catch-per trip file. 
# This file adjusts the expected number of catch per trip by population abundances. 
#source("predicted catch per trip by state.R")
#source("predicted catch per trip by state-check.R")
source("predicted catch per trip by state - two species.R")


#####



#Make the new catch-at length prob. distirbution
##########FLUKE

fluke_Cl_new_MA <- fluke_Cl_new_MA %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("MA",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

fluke_Cl_new_RI <- fluke_Cl_new_RI %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("RI",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

fluke_Cl_new_CT <- fluke_Cl_new_CT %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("CT",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

fluke_Cl_new_NY <- fluke_Cl_new_NY %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("NY",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

fluke_Cl_new_NJ <- fluke_Cl_new_NJ %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("NJ",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

fluke_Cl_new_DE <- fluke_Cl_new_DE %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("DE",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

fluke_Cl_new_MD <- fluke_Cl_new_MD %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("MD",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

fluke_Cl_new_VA <- fluke_Cl_new_VA %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("VA",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()


fluke_Cl_new_NC <- fluke_Cl_new_NC %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("NC",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()




#combine the datasets
fluke_sizes_all_y2 = bind_rows(fluke_Cl_new_MA, fluke_Cl_new_RI,
                              fluke_Cl_new_CT, fluke_Cl_new_NY,
                              fluke_Cl_new_NJ, fluke_Cl_new_DE, 
                              fluke_Cl_new_MD, fluke_Cl_new_VA, fluke_Cl_new_NC )



fluke_sizes_all_y2 = subset(fluke_sizes_all_y2, select=c(fitted_prob, length, state, year))
saveRDS(fluke_sizes_all_y2,file = "sf_fitted_sizes_y2plus.rds")



##########BSB
bsb_Cl_new_MA <- bsb_Cl_new_MA %>%
  rename(fitted_prob = prob_star) %>%
  mutate(state = rep("MA",nrow(.)),
         year = rep("y2", nrow(.))) %>%
  I()

bsb_Cl_new_RI <- bsb_Cl_new_RI %>%
  rename(fitted_prob = prob_star) %>%
  mutate(state = rep("RI",nrow(.)),
         year = rep("y2", nrow(.))) %>%
  I()

bsb_Cl_new_CT <- bsb_Cl_new_CT %>%
  rename(fitted_prob = prob_star) %>%
  mutate(state = rep("CT",nrow(.)),
         year = rep("y2", nrow(.))) %>%
  I()

bsb_Cl_new_NY <- bsb_Cl_new_NY %>%
  rename(fitted_prob = prob_star) %>%
  mutate(state = rep("NY",nrow(.)),
         year = rep("y2", nrow(.))) %>%
  I()

bsb_Cl_new_NJ <- bsb_Cl_new_NJ %>%
  rename(fitted_prob = prob_star) %>%
  mutate(state = rep("NJ",nrow(.)),
         year = rep("y2", nrow(.))) %>%
  I()

bsb_Cl_new_DE <- bsb_Cl_new_DE %>%
  rename(fitted_prob = prob_star) %>%
  mutate(state = rep("DE",nrow(.)),
         year = rep("y2", nrow(.))) %>%
  I()

bsb_Cl_new_MD <- bsb_Cl_new_MD %>%
  rename(fitted_prob = prob_star) %>%
  mutate(state = rep("MD",nrow(.)),
         year = rep("y2", nrow(.))) %>%
  I()

bsb_Cl_new_VA <- bsb_Cl_new_VA %>%
  rename(fitted_prob = prob_star) %>%
  mutate(state = rep("VA",nrow(.)),
         year = rep("y2", nrow(.))) %>%
  I()

bsb_Cl_new_NC <- bsb_Cl_new_NC %>%
  rename(fitted_prob = prob_star) %>%
  mutate(state = rep("NC",nrow(.)),
         year = rep("y2", nrow(.))) %>%
  I()




# bsb_Cl_new_MA <- bsb_Cl_new_MA %>% 
#   rename(nfish = C_l_new) %>% 
#   mutate(fitted_prob = nfish/sum(.$nfish),
#          state = rep("MA",nrow(.)),
#          year = rep("y2", nrow(.))) %>% 
#   I()
# 
# bsb_Cl_new_RI <- bsb_Cl_new_RI %>% 
#   rename(nfish = C_l_new) %>% 
#   mutate(fitted_prob = nfish/sum(.$nfish),
#          state = rep("RI",nrow(.)),
#          year = rep("y2", nrow(.))) %>% 
#   I()
# 
# bsb_Cl_new_CT <- bsb_Cl_new_CT %>% 
#   rename(nfish = C_l_new) %>% 
#   mutate(fitted_prob = nfish/sum(.$nfish),
#          state = rep("CT",nrow(.)),
#          year = rep("y2", nrow(.))) %>% 
#   I()
# 
# bsb_Cl_new_NY <- bsb_Cl_new_NY %>% 
#   rename(nfish = C_l_new) %>% 
#   mutate(fitted_prob = nfish/sum(.$nfish),
#          state = rep("NY",nrow(.)),
#          year = rep("y2", nrow(.))) %>% 
#   I()
# 
# bsb_Cl_new_NJ <- bsb_Cl_new_NJ %>% 
#   rename(nfish = C_l_new) %>% 
#   mutate(fitted_prob = nfish/sum(.$nfish),
#          state = rep("NJ",nrow(.)),
#          year = rep("y2", nrow(.))) %>% 
#   I()
# 
# bsb_Cl_new_DE <- bsb_Cl_new_DE %>% 
#   rename(nfish = C_l_new) %>% 
#   mutate(fitted_prob = nfish/sum(.$nfish),
#          state = rep("DE",nrow(.)),
#          year = rep("y2", nrow(.))) %>% 
#   I()
# 
# bsb_Cl_new_MD <- bsb_Cl_new_MD %>% 
#   rename(nfish = C_l_new) %>% 
#   mutate(fitted_prob = nfish/sum(.$nfish),
#          state = rep("MD",nrow(.)),
#          year = rep("y2", nrow(.))) %>% 
#   I()
# 
# bsb_Cl_new_VA <- bsb_Cl_new_VA %>% 
#   rename(nfish = C_l_new) %>% 
#   mutate(fitted_prob = nfish/sum(.$nfish),
#          state = rep("VA",nrow(.)),
#          year = rep("y2", nrow(.))) %>% 
#   I()
# 
# 
# bsb_Cl_new_NC <- bsb_Cl_new_NC %>% 
#   rename(nfish = C_l_new) %>% 
#   mutate(fitted_prob = nfish/sum(.$nfish),
#          state = rep("NC",nrow(.)),
#          year = rep("y2", nrow(.))) %>% 
#   I()
# 
# 
# 
# 
#combine the datasets
bsb_sizes_all_y2 = bind_rows(bsb_Cl_new_MA, bsb_Cl_new_RI,
                              bsb_Cl_new_CT, bsb_Cl_new_NY,
                              bsb_Cl_new_NJ, bsb_Cl_new_DE,
                              bsb_Cl_new_MD, bsb_Cl_new_VA, bsb_Cl_new_NC )



bsb_sizes_region_all_y2 = subset(bsb_sizes_all_y2, select=c(fitted_prob, length, state, year))
saveRDS(bsb_sizes_region_all_y2,file = "bsb_fitted_sizes_y2plus.rds")







##########SCUP
scup_Cl_new_MA <- scup_Cl_new_MA %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("MA",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

scup_Cl_new_RI <- scup_Cl_new_RI %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("RI",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

scup_Cl_new_CT <- scup_Cl_new_CT %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("CT",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

scup_Cl_new_NY <- scup_Cl_new_NY %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("NY",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

scup_Cl_new_NJ <- scup_Cl_new_NJ %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("NJ",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

scup_Cl_new_DE <- scup_Cl_new_DE %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("DE",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

scup_Cl_new_MD <- scup_Cl_new_MD %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("MD",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

scup_Cl_new_VA <- scup_Cl_new_VA %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("VA",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()


scup_Cl_new_NC <- scup_Cl_new_NC %>% 
  rename(nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         state = rep("NC",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()




#combine the datasets
scup_sizes_all_y2 = bind_rows(scup_Cl_new_MA, scup_Cl_new_RI,
                             scup_Cl_new_CT, scup_Cl_new_NY,
                             scup_Cl_new_NJ, scup_Cl_new_DE, 
                             scup_Cl_new_MD, scup_Cl_new_VA, scup_Cl_new_NC )



scup_sizes_region_all_y2 = subset(scup_sizes_all_y2, select=c(fitted_prob, length, state, year))
saveRDS(scup_sizes_region_all_y2,file = "scup_fitted_sizes_y2plus.rds")


