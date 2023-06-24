#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# Setup and startup---------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

### About ###
# This script specifies a statistical model forecasting the 2020 United States presidential election.
# The model follows a multilevel hierarchical Bayesian construction:
# The prior is informed by a blend of state-level presidential approval ratings and down-ballot statewide election results,
# and these estimates are then paired with state-level electoral horserace polls to construct the posterior distribution.
# Above all else, the project's purpose is to emphasize the emerging necessity for trying new predictor variables after 2016 and 2020.

### Data Sources ###
# U.S. Census Bureau: https://www.census.gov/data.html
# MIT Election Lab: https://dataverse.harvard.edu/dataverse/electionscience
# The Economist (very many thanks): https://hdsr.mitpress.mit.edu/pub/nw1dzd02/release/1; https://github.com/TheEconomist/us-potus-model


{
  library(tidyverse)   # install.packages("tidyverse")
  library(lubridate)   # install.packages("lubridate")
  library(rstan)       # install.packages("rstan")
  library(cmdstanr)    # install.packages("cmdstanr")
  library(parallel)    # install.packages("parallel")
  library(boot)        # install.packages("boot")
  library(lqmm)        # install.packages("lqmm")
  library(pbapply)     # install.packages("pbapply")
  library(gridExtra)   # install.packages("gridExtra")
  library(ggrepel)     # install.packages("ggrepel")
  library(urbnmapr)    # install.packages("urbnmapr")
}

options(scipen = 999) # sometimes it puts things in scientific notation. this prevents that.
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

election_day <- as.Date("2020-11-03")
RUN_DATE <- as.Date("2020-11-03") # pretending it's the last day of the campaign 
start_date <- as.Date("2020-03-01") # Keeping all polls after March 1, 2020

options(mc.cores = detectCores())
ncores <-  detectCores() %>% as.numeric
nchains <- ncores




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# Reading in polls and wrangling----------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

# read in raw polling dataset
polls2020_raw <- read.csv("datasets/polls2020wascher.csv")

# extract list of 50 states + DC
state_abb_list <- unique(polls2020_raw$state_abb %>% sort())[-1]

# wrangle polling dataset
polls2020 <- polls2020_raw %>%
  mutate(date_start = as.Date(date_start), # putting dates in the correct format
         date_median = as.Date(date_median),
         date_end = as.Date(date_end)) %>%
  filter(date_median >= start_date) %>%
  mutate(percent_biden = percent_biden / 100, # putting percentages on the correct scale
         percent_trump = percent_trump / 100,
         major_party_sum = percent_biden + percent_trump, # model only wants democrat + republican shares
         n_biden = round(percent_biden*sample_size), # number of poll respondents who supported each candidate
         n_trump = round(percent_trump*sample_size),
         percent_biden = percent_biden / major_party_sum, #two-party percentages
         percent_trump = percent_trump / major_party_sum,
         poll_day = as.numeric(date_median) - as.numeric(min(date_median)) + 1,
         index_state = as.numeric(factor(as.character(state_abb), levels = c('--',state_abb_list))), # Alphabetized beginning with states: 
         index_state = ifelse(index_state == 1, 52, index_state - 1),                                # 1 = AK, 2 = AL, 3 = AR, ..., 8 = DC, ..., 52 = national (--)
         index_time = 1 + as.numeric(date_median) - min(as.numeric(date_median)), # indices for other poll-specific variables of interest
         index_pollster = as.numeric(as.factor(as.character(pollster))),
         index_mode = as.numeric(as.factor(as.character(mode))),
         index_space = as.numeric(as.factor(as.character(sample_space)))) %>%
  arrange(index_state, date_median, index_pollster) %>% 
  select(state_abb, date_median, date_start, date_end, pollster, sample_space, mode, sample_size,
         percent_biden, n_biden, percent_trump, n_trump,
         poll_day, index_state, index_pollster, index_mode, index_space, index_time)


# useful vectors
all_polled_states <- polls2020$state_abb %>% unique %>% sort
all_pollsters <- levels(as.factor(polls2020$pollster))

ndays <- max(polls2020$date_median) - min(polls2020$date_median)
first_day <- min(polls2020$date_median)

state_match_list <- data.frame(state.abb = c(state.abb,"DC"),
                               state.name = c(state.name, "District of Columbia")) %>%
  arrange(state.name)

# major firms that adjust for party
adjusters <- c("ABC News/The Washington Post", "Ipsos", "Pew Research Center", "YouGov", "NBC News/The Wall Street Journal")




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# Reading in 2016 results and wrangling---------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

# but first, (approximately) how much did the adult population of each state change from 2015 to 2019?
adultsbystate <- read.csv("datasets/adult_population_by_state.csv") %>%
  mutate(pct_adult = pop_adult/pop_total) %>%
  select(state.name, pct_adult)

# and how many people live in each state? 
pop <- read.csv("datasets/census_pop_estimates_2010_2019.csv")

popadultgrowth_15_19 <- pop %>% 
  filter(NAME %in% c(state.name, "District of Columbia")) %>% # only want 50 states + DC
  rename(state.name = NAME) %>%
  left_join(adultsbystate) %>% # merge pct_adult by state 
  mutate(popadult2015 = POPESTIMATE2015*pct_adult,  # estimate adult population
         popadult2019 = POPESTIMATE2019*pct_adult,  
         popadultgrowth_15_19 = (popadult2019-popadult2015)/popadult2015) %>% # percent change from 2015-2019
  left_join(state_match_list) %>% # we only want state abbreviations, not full names
  select(state.abb, popadultgrowth_15_19)

# also, how many electoral college votes did each state have in 2020? 
ev <- read.csv("datasets/state_evs.csv") %>% rename(state.abb = state) 


# now that we have those answers, we can read in 2016 election results and wrangle
results2016 <- read.csv("datasets/results2016.csv") %>%
  rename(state.abb = state,
         votes_total = total_votes) %>%
  left_join(popadultgrowth_15_19) %>% # joining with population growth and electoral votes
  left_join(ev) %>%
  mutate(votes_dem = dem*votes_total,
         votes_rep = rep*votes_total,
         percent_dem_2p = votes_dem / (votes_dem + votes_rep),
         percent_dem_2pUSA = sum(votes_dem)/sum(votes_dem + votes_rep),
         state_diff = percent_dem_2p - percent_dem_2pUSA,
         share_vote_national = (votes_total*(1+popadultgrowth_15_19))/sum(votes_total*(1+popadultgrowth_15_19)))
rownames(results2016) <- results2016$state.abb


# some more helpful vectors
prior_state_diff <- results2016$state_diff
names(prior_state_diff) <- all_polled_states[-1]

state_weights <- results2016$share_vote_national / sum(results2016$share_vote_national)
names(state_weights) <- results2016$state.abb

all_states_abb <- results2016$state.abb
all_states_names <- state_match_list %>% arrange(state.abb)  %>% select(state.name) %>% pull
names(all_states_names) <- results2016$state.abb

ev_state <- results2016$ev
names(ev_state) <- results2016$state.abb




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# Defining priors-------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

# my state-by-state prior: 50% Trump disapproval from 11/2/2020 Civiqs poll + 50% trimmed mean of all statewide elections 2014-2019 (complete responses / two-party shares)
approval <- read.csv("datasets/trump_approval_bystate_civiqs_11_02_2020.csv") %>% 
  mutate(disapprove_2p = disapprove/(approve+disapprove)) %>% select(state, disapprove_2p)

benchmarks <- read.csv("datasets/dem_performance_allelextrimmed_twoparty.csv") %>% select(-X)

prior_by_state_wascher <- merge(approval, benchmarks, by = "state") %>%
  mutate(avg = (disapprove_2p + dem_benchmark_2p_2019)/2) %>%
  select(state, avg) %>%
  rbind(data.frame(state = "DC", avg = 0.9569519)) %>% # for DC we just use 2016 results 
  arrange(state) %>%
  pull(avg)
names(prior_by_state_wascher) <- results2016$state.abb

national_mu_prior <- 0.4922583 # predicted dem% using previous popular vote data
mu_b_prior <- logit(prior_by_state_wascher)


# other priors (portions of the code in this section adapted from The Economist)
N_national_polls <- nrow(polls2020 %>% filter(index_state == 52))
N_state_polls <- nrow(polls2020 %>% filter(index_state != 52))

T <- as.integer(round(difftime(election_day, first_day)))
S <- 51
P <- length(unique(polls2020$pollster))
M <- length(unique(polls2020$mode))
Pop <- length(unique(polls2020$sample_space))

state <- polls2020 %>% filter(index_state != 52) %>% pull(index_state)

day_national <- polls2020 %>% filter(index_state == 52) %>% pull(poll_day) 
day_state <- polls2020 %>% filter(index_state != 52) %>% pull(poll_day) 
poll_national <- polls2020 %>% filter(index_state == 52) %>% pull(index_pollster) 
poll_state <- polls2020 %>% filter(index_state != 52) %>% pull(index_pollster) 
poll_mode_national <- polls2020 %>% filter(index_state == 52) %>% pull(index_mode) 
poll_mode_state <- polls2020 %>% filter(index_state != 52) %>% pull(index_mode) 
poll_pop_national <- polls2020 %>% filter(index_state == 52) %>% pull(index_space) 
poll_pop_state <- polls2020 %>% filter(index_state != 52) %>% pull(index_space) 

n_democrat_national <- polls2020 %>% filter(index_state == 52) %>% pull(n_biden)
n_democrat_state <- polls2020 %>% filter(index_state != 52) %>% pull(n_biden)
n_two_share_national <- polls2020 %>% filter(index_state == 52) %>% transmute(n_two_share = n_trump + n_biden) %>% pull(n_two_share)
n_two_share_state <- polls2020 %>% filter(index_state != 52) %>% transmute(n_two_share = n_trump + n_biden) %>% pull(n_two_share)
unadjusted_national <- polls2020 %>% mutate(unadjusted = ifelse(!(pollster %in% adjusters), 1, 0)) %>% filter(index_state == 52) %>% pull(unadjusted)
unadjusted_state <- polls2020 %>% mutate(unadjusted = ifelse(!(pollster %in% adjusters), 1, 0)) %>% filter(index_state != 52) %>% pull(unadjusted)

sigma_measure_noise_national <- 0.04
sigma_measure_noise_state <- 0.04

sigma_c <- 0.06 # pollster
sigma_m <- 0.04 # mode
sigma_pop <- 0.04 # sample space

sigma_e_bias <- 0.02 

# covariance matrices  
state_covariance_0 <- read.csv('datasets/state_covariance_0.csv')[,-1] %>% as.matrix()
state_covariance_mu_b_walk <- read.csv('datasets/state_covariance_mu_b_walk.csv')[,-1] %>% as.matrix()
state_covariance_mu_b_T <- read.csv('datasets/state_covariance_mu_b_T.csv')[,-1] %>% as.matrix()
state_covariance_polling_bias <- read.csv('datasets/state_covariance_polling_bias.csv')[,-1] %>% as.matrix()
state_correlation_polling <- read.csv('datasets/state_correlation_polling.csv')[,-1] %>% as.matrix()
rownames(state_correlation_polling) <- results2016$state.abb

days_til_election <- as.numeric(difftime(election_day,RUN_DATE))
fit_rmse_day_x <- function(x){0.03 +  (10^-6.6)*(x)^2} # fit to error from external script
expected_national_mu_b_T_error <- fit_rmse_day_x(days_til_election)

polling_bias_scale <- 0.013 # probability scale 
polling_bias_scale <- as.numeric(polling_bias_scale) * 4

mu_b_T_scale <- expected_national_mu_b_T_error # probability scale 
mu_b_T_scale <- as.numeric(mu_b_T_scale) * 4

random_walk_scale <- 0.05/sqrt(300) # probability scale 
random_walk_scale <- as.numeric(random_walk_scale) * 4




#### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# Putting it all together and passing to stan----------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

data <- list(
  N_national_polls = N_national_polls, # from polling dataset
  N_state_polls = N_state_polls,       # from polling dataset
  T = T,
  S = S, 
  P = P, 
  M = M, 
  Pop = Pop,
  state = state,
  state_weights = state_weights,       # from results dataset
  day_national = as.integer(day_national),
  day_state = as.integer(day_state),
  poll_national = poll_national,
  poll_state = poll_state,
  poll_mode_national = poll_mode_national, 
  poll_mode_state = poll_mode_state,
  poll_pop_national = poll_pop_national, 
  poll_pop_state = poll_pop_state,
  n_democrat_national = n_democrat_national,
  n_democrat_state = n_democrat_state,
  n_two_share_national = n_two_share_national,
  n_two_share_state = n_two_share_state,
  unadjusted_national = unadjusted_national,
  unadjusted_state = unadjusted_state,
  sigma_measure_noise_national = sigma_measure_noise_national,
  sigma_measure_noise_state = sigma_measure_noise_state,
  mu_b_prior = mu_b_prior,
  sigma_c = sigma_c,
  sigma_m = sigma_m,
  sigma_pop = sigma_pop,
  sigma_e_bias = sigma_e_bias,
  state_covariance_0 = state_covariance_0, # from covariance matrix of state correlations
  polling_bias_scale = polling_bias_scale,
  mu_b_T_scale = mu_b_T_scale,
  random_walk_scale = random_walk_scale
)


# finally: a little bit of Bayes (this takes a  very very very long time)
model <- cmdstanr::cmdstan_model("stan/poll_model_2020.stan",compile=TRUE,force=TRUE)
fit <- model$sample(
  data = data,
  parallel_chains = ncores,
  chains = nchains,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 1000*.1
)


out <- rstan::read_stan_csv(fit$output_files())

write_rds(out, 'stan/stan_model_wascher_run.rds', compress = 'gz')

out <- read_rds('stan/stan_model_wascher_run.rds')

rm(fit)
gc()




#### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# Analyzing output-------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

# function: construct 95% intervals (many portions of the code in this section adapted from The Economist)
mean_low_high <- function(draws, states, id){
  tmp <- draws
  draws_df <- data.frame(mean = inv.logit(apply(tmp, MARGIN = 2, mean)),
                         high = inv.logit(apply(tmp, MARGIN = 2, mean) + 1.96 * apply(tmp, MARGIN = 2, sd)), 
                         low  = inv.logit(apply(tmp, MARGIN = 2, mean) - 1.96 * apply(tmp, MARGIN = 2, sd)),
                         state = states, 
                         type = id)
  return(draws_df) 
}


# draws and plots comparing each prior and posterior

# mu_b_T -- for each state: draw priors, draw posteriors, combine, plot
y <- MASS::mvrnorm(1000, mu_b_prior, Sigma = state_covariance_mu_b_T)
mu_b_T_prior_draws     <- mean_low_high(y, states = colnames(y), id = "prior")

mu_b_T_posterior_draws <- rstan::extract(out, pars = "mu_b")[[1]][,,246]
mu_b_T_posterior_draws <- mean_low_high(mu_b_T_posterior_draws, states = colnames(y), id = "posterior")

mu_b_T <- rbind(mu_b_T_prior_draws, mu_b_T_posterior_draws)

plot_mu_b_T <- mu_b_T %>% arrange(mean) %>%
  ggplot(.) +
  geom_point(aes(y = mean, x = reorder(state, mean), color = type), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = low, ymax = high, x = state, color = type), width = 0, position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw()
plot_mu_b_T


# mu_c -- for each pollster: draw priors, draw posteriors, combine, plot
mu_c_prior_draws <- data.frame(draws = rnorm(P * 1000, 0, sigma_c),
                               index_p = sort(rep(seq(1, P), 1000)), 
                               type = "prior")

mu_c_posterior_draws <- rstan::extract(out, pars = "mu_c")[[1]] 
mu_c_posterior_draws <- data.frame(draws = as.vector(mu_c_posterior_draws),
                                   index_p = sort(rep(seq(1, P), dim(mu_c_posterior_draws)[1])), 
                                   type = "posterior")

mu_c_draws <- rbind(mu_c_posterior_draws, mu_c_prior_draws) %>% rename(index_pollster = index_p)
pollster <- polls2020 %>% select(pollster, index_pollster) %>% distinct()
mu_c_draws <- merge(mu_c_draws, pollster, by = "index_pollster", all.x = TRUE)
mu_c_draws <- mu_c_draws %>%
  group_by(pollster, type) %>%
  summarize(mean = mean(draws), 
            low = mean(draws) - 1.96 * sd(draws),
            high = mean(draws) + 1.96 * sd(draws))

plot_mu_c <- mu_c_draws %>% 
  arrange(mean) %>% 
  filter(pollster %in% (polls2020 %>% group_by(pollster) %>% 
                          summarise(n=n()) %>% filter(n>=5) %>% pull(pollster))) %>%
  ggplot(.) +
  geom_point(aes(y = mean, x = reorder(pollster, mean), color = type), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = low, ymax = high, x = pollster, color = type), 
                width = 0, position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw()
plot_mu_c


# mu_m -- for each polling mode/method: draw priors, draw posteriors, combine, plot
mu_m_prior_draws <- data.frame(draws = rnorm(M * 1000, 0, sigma_m),
                               index_m = sort(rep(seq(1, M), 1000)), 
                               type = "prior")

mu_m_posterior_draws <- rstan::extract(out, pars = "mu_m")[[1]]
mu_m_posterior_draws <- data.frame(draws = as.vector(mu_m_posterior_draws),
                                   index_m = sort(rep(seq(1, M), dim(mu_m_posterior_draws)[1])), 
                                   type = "posterior")

mu_m_draws <- rbind(mu_m_posterior_draws, mu_m_prior_draws) %>% rename(index_mode = index_m)
method <- polls2020 %>% select(mode, index_mode) %>% distinct()
mu_m_draws <- merge(mu_m_draws, method, by = "index_mode", all.x = TRUE)
mu_m_draws <- mu_m_draws %>%
  group_by(mode, type) %>%
  summarize(mean = mean(draws), 
            low = mean(draws) - 1.96 * sd(draws),
            high = mean(draws) + 1.96 * sd(draws))

plot_mu_m <- mu_m_draws %>% arrange(mean) %>%
  ggplot(.) +
  geom_point(aes(y = mean, x = reorder(mode, mean), color = type), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = low, ymax = high, x = mode, color = type), 
                width = 0, position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw()
plot_mu_m


# mu_pop -- for each sample space: draw priors, draw posteriors, combine, plot
mu_pop_prior_draws <- data.frame(draws = rnorm(Pop * 1000, 0, sigma_pop),
                                 index_pop = sort(rep(seq(1, Pop), 1000)), 
                                 type = "prior")

mu_pop_posterior_draws <- rstan::extract(out, pars = "mu_pop")[[1]] 
mu_pop_posterior_draws <- data.frame(draws = as.vector(mu_pop_posterior_draws),
                                     index_pop = sort(rep(seq(1, Pop), dim(mu_pop_posterior_draws)[1])), 
                                     type = "posterior")

mu_pop_draws <- rbind(mu_pop_posterior_draws, mu_pop_prior_draws) %>% rename(index_space = index_pop)
space <- polls2020 %>% select(sample_space, index_space) %>% distinct()
mu_pop_draws <- merge(mu_pop_draws, space, by = "index_space", all.x = TRUE)
mu_pop_draws <- mu_pop_draws %>%
  group_by(sample_space, type) %>%
  summarize(mean = mean(draws), 
            low = mean(draws) - 1.96 * sd(draws),
            high = mean(draws) + 1.96 * sd(draws))

plot_mu_pop <- mu_pop_draws %>% arrange(mean) %>%
  ggplot(.) +
  geom_point(aes(y = mean, x = reorder(sample_space, mean), color = type), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = low, ymax = high, x = sample_space, color = type), 
                width = 0, position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw()
plot_mu_pop


# state error terms -- for each state: draw priors, draw posteriors, combine, plot
y <- MASS::mvrnorm(1000, rep(0, S), Sigma = state_covariance_polling_bias)
polling_bias_prior_draws <- data.frame(draws = as.vector(y),
                                       index_state = sort(rep(seq(1, S), dim(y)[1])), 
                                       type = "prior")

polling_bias_posterior <- rstan::extract(out, pars = "polling_bias")[[1]]
polling_bias_posterior_draws <- data.frame(draws = as.vector(polling_bias_posterior),
                                           index_state = sort(rep(seq(1, S), dim(polling_bias_posterior)[1])), 
                                           type = "posterior")

polling_bias_draws <- rbind(polling_bias_posterior_draws, polling_bias_prior_draws) 
states <- data.frame(index_state = 1:51, states = rownames(state_correlation_polling))
polling_bias_draws <- merge(polling_bias_draws, states, by = "index_state", all.x = TRUE)
polling_bias_draws <- polling_bias_draws %>%
  group_by(states, type) %>%
  summarize(mean = mean(draws), 
            low = mean(draws) - 1.96 * sd(draws),
            high = mean(draws) + 1.96 * sd(draws))

plot_polling_bias <- polling_bias_draws %>%
  ggplot(.) +
  geom_point(aes(y = mean, x = reorder(states, mean), color = type), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = low, ymax = high, x = states, color = type), 
                width = 0, position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw() 
plot_polling_bias


# state correlations: pick disparate states (just to check), and also major battlegrounds
ex_states <- c('IA','FL','OH','WI','MI','PA','AZ','NC','NH','NV','GA','MN')

predicted_score <- rstan::extract(out, pars = "predicted_score")[[1]]
single_draw <- as.data.frame(predicted_score[,dim(predicted_score)[2],])
names(single_draw) <- colnames(state_correlation_polling)

single_draw %>% 
  select(AL,CA,FL,MN,NC,NM,RI,WI) %>%  # also might want to look at NV, OH, IA, IN - especially in previous elections
  cor 


#graphs over time

# Biden's percentages and chances over time: in each state, nationwide, combined
pct_biden_states <- pblapply(1:dim(predicted_score)[3],
                             function(x){
                               # pred is mu_a + mu_b for the past, just mu_b for the future
                               temp <- predicted_score[,,x]
                               # put in tibble
                               tibble(low = apply(temp,2,function(x){(quantile(x,0.025))}),
                                      high = apply(temp,2,function(x){(quantile(x,0.975))}),
                                      mean = apply(temp,2,function(x){(mean(x))}),
                                      prob = apply(temp,2,function(x){(mean(x>0.5))}),
                                      state = x) 
                             }) %>% do.call('bind_rows',.)

pct_biden_states$state = colnames(state_correlation_polling)[pct_biden_states$state]

pct_biden_states <- pct_biden_states %>%
  group_by(state) %>%
  mutate(date = row_number() + min(start_date-1)) %>%
  ungroup()

pct_biden_national <- pblapply(1:dim(predicted_score)[1],
                               function(x){
                                 # each row is a day for a particular draw
                                 temp <- predicted_score[x,,] %>% as.data.frame()
                                 names(temp) <- colnames(state_correlation_polling)
                                 
                                 # for each row, get weighted natl vote
                                 tibble(natl_vote = apply(temp,MARGIN = 1,function(y){weighted.mean(y,state_weights)})) %>%
                                   mutate(date = row_number() + min(start_date-1)) %>%
                                   mutate(draw = x)
                               }) %>% do.call('bind_rows',.)

pct_biden_national <- pct_biden_national %>%
  group_by(date) %>%
  summarise(low = quantile(natl_vote,0.025),
            high = quantile(natl_vote,0.975),
            mean = mean(natl_vote),
            prob = mean(natl_vote > 0.5)) %>%
  mutate(state = '--')

pct_biden_all <- pct_biden_states %>%
  bind_rows(pct_biden_national) %>%
  arrange(desc(mean))


#compare predictions to actual results: data, then graph
results2020_2p <- read.csv("datasets/results2020_2p.csv")
plot_mu_b_t_results <- rbind(mu_b_T_prior_draws, mu_b_T_posterior_draws) %>% 
  bind_rows(results2020_2p) %>%
  arrange(mean) %>%
  filter(state != 'DC') %>%
  ggplot(.) +
  geom_point(aes(y = mean, x = reorder(state, mean), color = type), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = low, ymax = high, x = state, color = type), width = 0, position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw()
plot_mu_b_t_results

map.gg <- urbnmapr::states %>%
  left_join(pct_biden_all %>% filter(date == max(date)) %>%
              select(state_abbv=state,prob)) %>%
  ggplot(aes(x=long,y=lat,group=group,fill=prob)) +
  geom_polygon()  + 
  coord_map("albers",lat0=39, lat1=45) +
  scale_fill_gradient2(high='blue',low='red',mid='white',midpoint=0.5) +
  theme_void()
print(map.gg)

# electoral college by simulation
identifier <- paste0(Sys.Date()," || " , out@model_name)

draws <- pblapply(1:dim(predicted_score)[3],
                  function(x){
                    # pred is mu_a + mu_b for the past, just mu_b for the future
                    pct_biden_all <- predicted_score[,,x]
                    
                    pct_biden_all <- pct_biden_all %>%
                      as.data.frame() %>%
                      mutate(draw = row_number()) %>%
                      gather(date,pct_biden_all,1:(ncol(.)-1)) %>%
                      mutate(date = as.numeric(gsub('V','',date)) + (start_date-1),
                             state = colnames(state_correlation_polling)[x]) 
                  }) %>% do.call('bind_rows',.)


sim_evs <- draws %>%
  left_join(results2016 %>% rename(state = state.abb) %>% select(state, ev),by='state') %>%
  group_by(date, draw) %>%
  summarise(dem_ev = sum(ev * (pct_biden_all > 0.5))) %>%
  group_by(date) %>%
  summarise(mean_dem_ev = mean(dem_ev),
            median_dem_ev = median(dem_ev),
            high_dem_ev = quantile(dem_ev,0.975),
            low_dem_ev = quantile(dem_ev,0.025),
            prob = mean(dem_ev >= 270))


natl_polls.gg <- pct_biden_all %>%
  filter(state == '--') %>%
  left_join(polls2020 %>% rename(state = state_abb, date = date_median) %>% select(state, date, percent_biden, mode)) %>% 
  ggplot(.,aes(x = date)) +
  geom_ribbon(aes(ymin = low, ymax = high) ,col = NA, alpha = 0.2) +
  geom_hline(yintercept = 0.5) +
  geom_hline(yintercept = national_mu_prior, linetype = 2) +
  geom_point(aes(y = percent_biden, shape  = mode), alpha = 0.3) +
  geom_line(aes(y = mean)) +
  facet_wrap(~state) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits = c(ymd('2020-03-01','2020-11-03')), date_breaks = '1 month', date_labels = '%b') +
  scale_y_continuous(breaks = seq(0,1,0.02)) + 
  labs(subtitletitle=sprintf('Biden natl pct | mean = %s | p(win) = %s',
                             round(pct_biden_all[pct_biden_all$state == '--' & pct_biden_all$date == election_day-1,]$mean*100,1),
                             round(pct_biden_all[pct_biden_all$state == '--' & pct_biden_all$date == election_day-1,]$prob,2)))


natl_evs.gg <-  ggplot(sim_evs, aes(x = date)) +
  geom_hline(yintercept = 270) +
  geom_line(aes(y = median_dem_ev)) +
  geom_ribbon(aes(ymin = low_dem_ev, ymax = high_dem_ev), alpha = 0.2) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits = c(ymd('2020-03-01','2020-11-03')), date_breaks = '1 month', date_labels = '%b') +
  labs(subtitletitle=sprintf('clinton evs | median = %s | p(win) = %s',
                             round(sim_evs[sim_evs$date == election_day,]$median_dem_ev),
                             round(sim_evs[sim_evs$date == election_day,]$prob,2)))


state_polls.gg <- pct_biden_all %>%
  filter(state %in% ex_states) %>%
  left_join(polls2020 %>% rename(state = state_abb, date = date_median) %>% select(state, date, percent_biden, mode)) %>% 
  left_join(tibble(state = names(mu_b_prior),
                   prior = inv.logit(mu_b_prior)) ) %>%
  ggplot(.,aes(x = date,col = state)) +
  geom_ribbon(aes(ymin = low,ymax = high),col = NA,alpha = 0.2) +
  geom_hline(yintercept = 0.5) +
  geom_hline(aes(yintercept = prior),linetype = 2) +
  geom_point(aes(y = percent_biden, shape = mode),alpha = 0.3) +
  geom_line(aes(y = mean)) +
  facet_wrap(~state) +
  theme_minimal()  +
  theme(legend.position = 'top') +
  guides(color='none') +
  scale_x_date(limits=c(ymd('2020-03-01','2020-11-03')),date_breaks='1 month',date_labels='%b') +
  labs(subtitle='pct_biden state')


grid.arrange(natl_polls.gg, natl_evs.gg, state_polls.gg, 
             layout_matrix = rbind(c(1,1,3,3,3),
                                   c(2,2,3,3,3)),
             top = identifier)


# diff from national over time?
state_diff.gg <- pct_biden_all[pct_biden_all$state != '--',] %>%
  left_join(pct_biden_all[pct_biden_all$state == '--',] %>% select(date, pct_biden_national = mean), by = 'date') %>%
  mutate(diff = mean - pct_biden_national) %>%
  group_by(state) %>%
  mutate(last_prob = last(prob)) %>%
  filter(state %in% ex_states) %>%
  ggplot(.,aes(x = date, y = diff, col = state)) +
  geom_hline(yintercept = 0.0) +
  geom_line() +
  geom_label_repel(data = . %>% 
                     filter(date == max(date),
                            prob > 0.1 & prob < 0.9),
                   aes(label=state)) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits = c(ymd('2020-03-01','2020-11-03')), date_breaks='1 month', date_labels='%b') +
  scale_y_continuous(breaks = seq(-1,1,0.01)) +
  labs(subtitle = identifier)
state_diff.gg


# actual predictions: this is what we've all been waiting for 

# final electoral college vote distribution
final_evs <- draws %>%
  left_join(ev %>% rename(state = state.abb), by='state') %>%
  filter(date == max(date)) %>%
  group_by(draw) %>%
  summarise(dem_ev = sum(ev* (pct_biden_all > 0.5)))

ev.gg <- ggplot(final_evs,aes(x = dem_ev,
                              fill = ifelse(dem_ev >= 270, 'Democratic', 'Republican'))) +
  geom_vline(xintercept = 270) +
  geom_histogram(binwidth = 1) +
  theme_minimal() +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank()) +
  scale_fill_manual(name = 'Electoral College winner', values = c('Democratic' = '#3A4EB1', 'Republican' = '#E40A04')) +
  labs(x = 'Democratic electoral college votes',
       subtitle = sprintf("p(dem win) = %s", round(mean(final_evs$dem_ev >= 270), 2)))

prob_biden_win_ec <- nrow(final_evs[final_evs$dem_ev > 270, ])/nrow(final_evs) 

print(ev.gg)
prob_biden_win_ec


# Brier score: model performance 
ev_state <- enframe(ev_state)
colnames(ev_state) <- c("state", "ev")
compare <- pct_biden_all %>% 
  filter(date == max(date), state != '--') %>% 
  select(state, biden_win = prob) %>% 
  mutate(biden_win_actual = ifelse(state %in% c('CA','NV','OR','WA','CO','NM','MN','IL','VA','DC','MD','DE','NJ','CT','RI','MA','NH','VT','NY','HI','ME'),1,0),
         diff = (biden_win_actual - biden_win)^2) %>% 
  left_join(ev_state) %>% 
  mutate(ev_weight = ev/(sum(ev))) 

tibble(model = 'Wascher',
       ev_wtd_brier = weighted.mean(compare$diff, compare$ev_weight),
       unwtd_brier = mean(compare$diff),
       states_correct=sum(round(compare$biden_win) == round(compare$biden_win_actual)))