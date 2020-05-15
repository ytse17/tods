library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(gridExtra)
library(stargazer)
setwd("~/Documents/GitHub/tods")

source('~/Documents/GitHub/tods/data.R')

final_wide <- final
final_wide$avg_med_rent_70 <- NA
final_wide$pub_trans_70 <- NA

final_long <- reshape(final_wide, direction="long", 
                      varying=list(c("pop_70","pop_80", "pop_90", "pop_00", "pop_10"), 
                                   c("white_70","white_80", "white_90", "white_00", "white_10"), 
                                   c("black_70","black_80", "black_90", "black_00", "black_10"),
                                   c("hispanic_70","hispanic_80", "hispanic_90", "hispanic_00", "hispanic_10"),
                                   c("family_income_70","family_income_80", "family_income_90", "family_income_00", "family_income_10"),
                                   c("avg_hh_70","avg_hh_80", "avg_hh_90", "avg_hh_00", "avg_hh_10"),
                                   c("avg_med_rent_70", "avg_med_rent_80", "avg_med_rent_90", "avg_med_rent_00", "avg_med_rent_10"),
                                   c("pub_trans_70", "pub_trans_80", "pub_trans_90", "pub_trans_00", "pub_trans_10")), 
                      v.names=c("pop","white","black", "hispanic", "family_income", "avg_hh", "avg_med_rent", "pub_trans"),
                      times=c(1970, 1980, 1990, 2000, 2010))

final_long$post = ifelse(final_long$time >= 1998, 1, 0)
final_long$treated = ifelse(as.integer(final_long$year/100) == 19, 1, 0)

# density plot for income distribution
control_long <- final_long %>% filter(treated == 0)
treatment_long <- final_long %>% filter(treated == 1)

control_income_plot <- ggplot(control_long, aes(x=avg_hh, group=time, fill=time, colour=time)) + 
  geom_density(alpha=0.4) + 
  labs(x = "Average Household Income (Control)", y = "Density", fill = "Year", colour = "Year") +
  coord_cartesian(xlim = c(0, 150000), ylim = c(0, 0.00025)) +
  theme(legend.position="top")
treatment_income_plot <-ggplot(treatment_long, aes(x=avg_hh, group=time, fill=time, colour=time)) + 
  geom_density(alpha=0.4) +
  labs(x = "Average Household Income (Treatment)", y = "Density", fill = "Year", colour = "Year") +
  coord_cartesian(xlim = c(0, 150000), ylim = c(0, 0.00025)) +
  theme(legend.position="top")

grid.arrange(control_income_plot, treatment_income_plot, ncol=2)

#regressions
mod_pop = lm(pop ~ treated * post, data=final_long)
mod_pop_race = lm(pop ~ treated * post + hispanic + black, data=final_long)
mod_pop_income = lm(pop ~ treated * post * family_income, data=final_long)

mod_white = lm(white ~ treated * post, data=final_long)
mod_white_income = lm(white ~ treated * post * avg_hh, data=final_long)
mod_white_rent = lm(white ~ treated * post + avg_med_rent, data=final_long)

mod_black = lm(black ~ treated * post, data=final_long)
mod_black_income = lm(black ~ treated * post * avg_hh, data=final_long)
mod_black_rent = lm(black ~ treated * post + avg_med_rent, data=final_long)

mod_hispanic = lm(hispanic ~ treated * post, data=final_long)
mod_hispanic_income = lm(hispanic ~ treated * post * avg_hh, data=final_long)
mod_hispanic_rent = lm(hispanic ~ treated * post + avg_med_rent, data=final_long)

mod_family_income = lm(family_income ~ treated * post, data=final_long)

mod_avg_hh = lm(avg_hh ~ treated * post, data=final_long)
mod_avg_hh_race = lm(avg_hh ~ treated * post + hispanic + black, data=final_long)

#remove outliers
outliers <- boxplot(final_long$avg_hh, plot=FALSE)$out
final_no_outliers <- final_long %>% filter(!avg_hh %in% outliers)
mod_avg_hh_no_outliers = lm(avg_hh ~ treated * post, data=final_no_outliers)
mod_avg_hh_no_outliers_race = lm(avg_hh ~ treated * post + hispanic + black, data=final_no_outliers)

mod_avg_med_rent = lm(avg_med_rent ~ treated * post, data=final_long)
mod_avg_med_rent_race = lm(avg_med_rent ~ treated * post + hispanic + black, data=final_long)

#remove outliers
outliers <- boxplot(final_long$avg_med_rent, plot=FALSE)$out
final_no_outliers <- final_long %>% filter(!avg_med_rent %in% outliers)
mod_avg_med_rent_no_outliers = lm(avg_med_rent ~ treated * post, data=final_no_outliers)
mod_avg_med_rent_no_outliers_race = lm(avg_med_rent ~ treated * post + hispanic + black, data=final_no_outliers)

pop_code <- stargazer(mod_pop, mod_pop_race, align=TRUE, title='Results: Population')
white_code <- stargazer(mod_white, mod_white_income, align=TRUE, title='Results: Share of White Population')
black_code <- stargazer(mod_black, mod_black_income, align=TRUE, title='Results: Share of Black Population')
hispanic_code <- stargazer(mod_hispanic, mod_hispanic_income, align=TRUE, title='Results: Share of Hispanic Population')
race_code <- stargazer(mod_white, mod_black, mod_hispanic, align=TRUE, title='Results: Race')
avg_hh_code <- stargazer(mod_avg_hh, mod_avg_hh_race, mod_avg_hh_no_outliers, mod_avg_hh_no_outliers_race, align=TRUE, title='Results: Household Income')
avg_med_rent_code <- stargazer(mod_avg_med_rent, mod_avg_med_rent_race, mod_avg_med_rent_no_outliers, mod_avg_med_rent_no_outliers_race, align=TRUE, title='Results: Median Rent')
