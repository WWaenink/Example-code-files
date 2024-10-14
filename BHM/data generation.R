#script for generating data to fit practice model on later
#data is not accurate

#set working directory
setwd("C:/R Projects/Test Hierarch Dataset generation")
#loading packages
if(!require("pacman")){
  install.packages("pacman")
}
p_load(tidyverse, car, boot)

#load in parameters
params <- read_csv("params.csv")

baseChance <- 0.05
nYears <- 30

#create dataframe of bears killed per year per site
nBearsKilled <- matrix(0, nrow = nrow(params), ncol = nYears)

#create column names for the years
years <- vector()
for(i in 1:nYears){
  colname <- paste("Year_", i, sep = "")
  years <- c(years, colname)
}
colnames(nBearsKilled) <- years

#generate data over nYears
for (i in 1:nrow(params)){
  #calculate chance of bear being killed based on params
  chance = inv.logit(logit(baseChance)+params$Country_bear_kill_risk[i]+params$County_level_bear_kill_risk[i])
  #generate data based on chance of bears getting killed and sum it to get amount over nYears
  bearsKilledCountyi = rbinom(nYears,params$bear_pop,chance)
  #add killed amount of bears to vector
  nBearsKilled[i,] <- bearsKilledCountyi
}

#convert bears killed to tibble format
nBearsKilled <- as_tibble(nBearsKilled)


#create dataframe to be exported
bearKillsdf <- params %>% 
  select(Country, County, bear_pop)

bearKillsdf <- bind_cols(bearKillsdf, nBearsKilled)

#write data to csv
write_csv(bearKillsdf, "bearskilled.csv")
