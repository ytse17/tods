library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(gridExtra)
setwd("~/Documents/GitHub/tods")

source('~/Documents/GitHub/tods/data.R')

final_bool <- final %>% mutate(treatment_90 = as.integer(year/100) == 19) %>%
  mutate(treatment_00 = as.integer(year/100) == 20)

is_1990 <- final_bool %>% filter(treatment_90 == TRUE)
is_2000 <- final_bool %>% filter(treatment_00 == TRUE)

years <- seq(as.Date("1970-01-01"), by=3653, len=5)

#population
pop_treatment <- c(mean(is_1990$pop_70),
                   mean(is_1990$pop_80),
                   mean(is_1990$pop_90),
                   mean(is_1990$pop_00),
                   mean(is_1990$pop_10))

pop_control <- c(mean(is_2000$pop_70),
                 mean(is_2000$pop_80),
                 mean(is_2000$pop_90),
                 mean(is_2000$pop_00),
                 mean(is_2000$pop_10))

white_treatment <- c(mean(is_1990$white_70),
                     mean(is_1990$white_80),
                     mean(is_1990$white_90),
                     mean(is_1990$white_00),
                     mean(is_1990$white_10))

white_control <- c(mean(is_2000$white_70),
                   mean(is_2000$white_80),
                   mean(is_2000$white_90),
                   mean(is_2000$white_00),
                   mean(is_2000$white_10))

black_treatment <- c(mean(is_1990$black_70),
                     mean(is_1990$black_80),
                     mean(is_1990$black_90),
                     mean(is_1990$black_00),
                     mean(is_1990$black_10))

black_control <- c(mean(is_2000$black_70),
                   mean(is_2000$black_80),
                   mean(is_2000$black_90),
                   mean(is_2000$black_00),
                   mean(is_2000$black_10))

hispanic_treatment <- c(mean(is_1990$hispanic_70),
                        mean(is_1990$hispanic_80),
                        mean(is_1990$hispanic_90),
                        mean(is_1990$hispanic_00),
                        mean(is_1990$hispanic_10))

hispanic_control <- c(mean(is_2000$hispanic_70),
                      mean(is_2000$hispanic_80),
                      mean(is_2000$hispanic_90),
                      mean(is_2000$hispanic_00),
                      mean(is_2000$hispanic_10))

family_income_treatment <- c(mean(is_1990$family_income_70),
                             mean(is_1990$family_income_80),
                             mean(is_1990$family_income_90),
                             mean(is_1990$family_income_00),
                             mean(is_1990$family_income_10, na.rm=TRUE))

family_income_control <- c(mean(is_2000$family_income_70),
                           mean(is_2000$family_income_80),
                           mean(is_2000$family_income_90),
                           mean(is_2000$family_income_00),
                           mean(is_2000$family_income_10, na.rm=TRUE))

avg_hh_treatment <- c(mean(is_1990$avg_hh_70),
                      mean(is_1990$avg_hh_80),
                      mean(is_1990$avg_hh_90),
                      mean(is_1990$avg_hh_00),
                      mean(is_1990$avg_hh_10, na.rm=TRUE))

avg_hh_control <- c(mean(is_2000$avg_hh_70),
                    mean(is_2000$avg_hh_80),
                    mean(is_2000$avg_hh_90),
                    mean(is_2000$avg_hh_00),
                    mean(is_2000$avg_hh_10, na.rm=TRUE))

avg_med_rent_treatment <- c(NA, mean(is_1990$avg_med_rent_80),
                            mean(is_1990$avg_med_rent_90),
                            mean(is_1990$avg_med_rent_00),
                            mean(is_1990$avg_med_rent_10, na.rm=TRUE))

avg_med_rent_control <- c(NA, mean(is_2000$avg_med_rent_80),
                          mean(is_2000$avg_med_rent_90),
                          mean(is_2000$avg_med_rent_00),
                          mean(is_2000$avg_med_rent_10, na.rm=TRUE))

pub_trans_treatment <- c(NA, mean(is_1990$pub_trans_80),
                         mean(is_1990$pub_trans_90),
                         mean(is_1990$pub_trans_00),
                         mean(is_1990$pub_trans_10, na.rm=TRUE))

pub_trans_control <- c(NA, mean(is_2000$pub_trans_80),
                       mean(is_2000$pub_trans_90),
                       mean(is_2000$pub_trans_00),
                       mean(is_2000$pub_trans_10, na.rm=TRUE))

pretrend_data <- cbind.data.frame(years, pop_treatment, pop_control, white_treatment, white_control,
                                  black_treatment, black_control, hispanic_treatment, hispanic_control,
                                  family_income_treatment, family_income_control, avg_hh_treatment,
                                  avg_hh_control, avg_med_rent_treatment, avg_med_rent_control,
                                  pub_trans_treatment, pub_trans_control)

pretrend_long <- reshape(pretrend_data, direction="long", 
                         varying=list(c("pop_treatment","pop_control"), 
                                      c("white_treatment", "white_control"), 
                                      c("black_treatment","black_control"),
                                      c("hispanic_treatment", "hispanic_control"),
                                      c("family_income_treatment", "family_income_control"),
                                      c("avg_hh_treatment", "avg_hh_control"),
                                      c("avg_med_rent_treatment", "avg_med_rent_control"),
                                      c("pub_trans_treatment", "pub_trans_control")), 
                         v.names=c("pop","white","black", "hispanic", "family_income", "avg_hh", "avg_med_rent", "pub_trans"))

pretrend_long$time[pretrend_long$time == 1] <- "Treatment"
pretrend_long$time[pretrend_long$time == 2] <- "Control"

#graph trends
pop_trend <- ggplot(pretrend_long, aes(x=years, y=pop, colour=time)) + 
  geom_line() + labs(x = "Year", y = "Population", colour = "Group") +
  geom_vline(xintercept = as.numeric(as.Date("1996-01-01")), linetype="dotted")

white_trend <- ggplot(pretrend_long, aes(x=years, y=white, colour=time)) + 
  geom_line() + labs(x = "Year", y = "% White", colour = "Group") +
  geom_vline(xintercept = as.numeric(as.Date("1996-01-01")), linetype="dotted")

black_trend <- ggplot(pretrend_long, aes(x=years, y=black, colour=time)) + 
  geom_line() + labs(x = "Year", y = "% Black", colour = "Group") +
  geom_vline(xintercept = as.numeric(as.Date("1996-01-01")), linetype="dotted")  

hispanic_trend <- ggplot(pretrend_long, aes(x=years, y=hispanic, colour=time)) + 
  geom_line() + labs(x = "Year", y = "% Hispanic", colour = "Group") +
  geom_vline(xintercept = as.numeric(as.Date("1996-01-01")), linetype="dotted")

family_income_trend <- ggplot(pretrend_long, aes(x=years, y=family_income, colour=time)) + 
  geom_line() + labs(x = "Year", y = "Family Income", colour = "Group") +
  geom_vline(xintercept = as.numeric(as.Date("1996-01-01")), linetype="dotted")

avg_hh_trend <- ggplot(pretrend_long, aes(x=years, y=avg_hh, colour=time)) + 
  geom_line() + labs(x = "Year", y = "Household Income", colour = "Group") + 
  theme(axis.title.y = element_text(size = 8)) +
  geom_vline(xintercept = as.numeric(as.Date("1996-01-01")), linetype="dotted")

avg_med_rent_trend <- ggplot(pretrend_long, aes(x=years, y=avg_med_rent, colour=time)) + 
  geom_line() + labs(x = "Year", y = "Median Rent", colour = "Group") +
  geom_vline(xintercept = as.numeric(as.Date("1996-01-01")), linetype="dotted")

pub_trans_trend <- ggplot(pretrend_long, aes(x=years, y=pub_trans, colour=time)) + 
  geom_line() + labs(x = "Year", y = "Transit Ridership", colour = "Group") +
  theme(axis.title.y = element_text(size = 8)) +
  geom_vline(xintercept = as.numeric(as.Date("1996-01-01")), linetype="dotted")

grid.arrange(pop_trend, white_trend, black_trend, hispanic_trend,
             avg_hh_trend, avg_med_rent_trend, ncol=2)


