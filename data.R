library(dplyr)
library(magrittr)
library(readr)
setwd("~/Documents/GitHub/tods")

files <- list.files("NCDB data", pattern = "*.csv",
                    full.names = TRUE)

#create columns
station <- character()
pop_70 <- numeric()
white_70 <- numeric()
black_70 <- numeric()
hispanic_70 <- numeric()
family_income_70 <- numeric()
avg_hh_70 <- numeric()

pop_80 <- numeric()
white_80 <- numeric()
black_80 <- numeric()
hispanic_80 <- numeric()
family_income_80 <- numeric()
avg_hh_80 <- numeric()
pub_trans_80 <- numeric()
avg_med_rent_80 <- numeric()

pop_90 <- numeric()
white_90 <- numeric()
black_90 <- numeric()
hispanic_90 <- numeric()
family_income_90 <- numeric()
avg_hh_90 <- numeric()
pub_trans_90 <- numeric()
avg_med_rent_90 <- numeric()

pop_00 <- numeric()
white_00 <- numeric()
black_00 <- numeric()
hispanic_00 <- numeric()
family_income_00 <- numeric()
avg_hh_00 <- numeric()
pub_trans_00 <- numeric()
avg_med_rent_00 <- numeric()

pop_10 <- numeric()
white_10 <- numeric()
black_10 <- numeric()
hispanic_10 <- numeric()
family_income_10 <- numeric()
avg_hh_10 <- numeric()
pub_trans_10 <- numeric()
avg_med_rent_10 <- numeric()
#

k = 1
for (i in seq_along(files)) {
  the_data <- read.csv(files[i], header = TRUE, stringsAsFactors=FALSE)
  for (j in 1:nrow(the_data)) {
    station[k] <- files[i]
    pop_70[k] <- the_data$TRCTPOP7[j]
    pop_80[k] <- the_data$TRCTPOP8[j]
    pop_90[k] <- the_data$TRCTPOP9[j]
    pop_00[k] <- the_data$TRCTPOP0[j]
    pop_10[k] <- the_data$TRCTPOP1[j]
    
    white_70[k] <- the_data$SHRWHT7[j] * 100 
    black_70[k] <- the_data$SHRBLK7[j] * 100 
    hispanic_70[k] <- the_data$SHRHSP7[j] * 100 
    family_income_70[k] <- the_data$FAVINC7[j]
    avg_hh_70[k] <- the_data$AVHHIN7[j]
    
    white_80[k] <- the_data$SHRWHT8[j] * 100 
    black_80[k] <- the_data$SHRBLK8[j] * 100 
    hispanic_80[k] <- the_data$SHRHSP8[j] * 100 
    family_income_80[k] <- the_data$FAVINC8[j]
    avg_hh_80[k] <- the_data$AVHHIN8[j]
    avg_med_rent_80[k] <- the_data$MDGRENT8[j]
    pub_trans_80[k] <- the_data$TRVLPB8N[j]/the_data$TRVLPB8D[j] * 100 
    
    white_90[k] <- the_data$SHRWHT9[j] * 100 
    black_90[k] <- the_data$SHRBLK9[j] * 100 
    hispanic_90[k] <- the_data$SHRHSP9[j]
    family_income_90[k] <- the_data$FAVINC9[j]
    avg_hh_90[k] <- the_data$AVHHIN9[j]
    avg_med_rent_90[k] <- the_data$MDGRENT9[j]
    pub_trans_90[k] <- the_data$TRVLPB9N[j]/the_data$TRVLPB9D[j] * 100 
    
    white_00[k] <- the_data$SHRWHT0[j] * 100 
    black_00[k] <- the_data$SHRBLK0[j] * 100
    hispanic_00[k] <- the_data$SHRHSP0[j] * 100 
    family_income_00[k] <- the_data$FAVINC0[j]
    avg_hh_00[k] <- the_data$AVHHIN0[j]
    avg_med_rent_00[k] <- the_data$MDGRENT0[j]
    pub_trans_00[k] <- the_data$TRVLPB0N[j]/the_data$TRVLPB0D[j] * 100
    
    white_10[k] <- the_data$SHRWHT1[j] * 100 
    black_10[k] <- the_data$SHRBLK1[j] * 100
    hispanic_10[k] <- the_data$SHRHSP1[j] * 100
    family_income_10[k] <- the_data$FAVINC1A[j]
    avg_hh_10[k] <- the_data$AVHHIN1A[j]
    avg_med_rent_10[k] <- the_data$MDGRENT1A[j]
    pub_trans_10[k] <- the_data$TRVLPB1A[j] * 100
    
    k = k + 1  
  }
}

#create table
all_data <- cbind.data.frame(station, pop_70, white_70, black_70, hispanic_70, family_income_70, avg_hh_70, 
                             pop_80, white_80, black_80, hispanic_80, family_income_80, avg_hh_80, avg_med_rent_80, pub_trans_80,
                             pop_90, white_90, black_90, hispanic_90, family_income_90, avg_hh_90, avg_med_rent_90, pub_trans_90,
                             pop_00, white_00, black_00, hispanic_00, family_income_00, avg_hh_00, avg_med_rent_00, pub_trans_00,
                             pop_10, white_10, black_10, hispanic_10, family_income_10, avg_hh_10, avg_med_rent_10, pub_trans_10)

#add metadata
metadata <- read.csv("metadata.csv")
final_unclean <- full_join(metadata, all_data, how="outer")

#clean
final <- final_unclean %>% replace(final_unclean < 0, NA)  
final$family_income_10[!final$family_income_10 > 0] <- NA           
final$avg_med_rent_10[final$avg_med_rent_10 == 0] <- NA
