#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# this script shows how I arrived at my national_mu_prior of 0.4922583--------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###


{
library(tidyverse)
library(politicaldata)
}



# getting popular vote margins in every presidential election 1976-2016
historical_2p <- pres_results %>%
  mutate(votes_dem = dem*total_votes,
         votes_rep = rep*total_votes,
         votes_2p = votes_dem + votes_rep) %>%
  select(year, state, votes_dem, votes_rep, total_votes, votes_2p) %>%
  group_by(year) %>%
  summarize(votes_dem = sum(votes_dem),
            votes_rep = sum(votes_rep),
            votes_total = sum(total_votes)) %>%
  summarize(percent_dem = votes_dem/votes_total,
            percent_rep = votes_rep/votes_total,
            margin = percent_dem - percent_rep) %>%
  arrange(desc(margin))


# simple regression to predict Dem% based on the margin
regressdem <- lm(percent_dem ~ margin , data = historical_2p) # could go Bayesian here just as easily with brms


# let's just say Biden wins by 5 points 
# largely arbitrary; a 5-point Biden lead is approximately halfway between Clinton's 2016 result and Biden's final poll average
predict_biden_share <- predict(regressdem, newdata = data.frame(margin = .05)) %>% as.numeric
predict_biden_share