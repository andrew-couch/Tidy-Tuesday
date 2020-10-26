library(tidyverse)
library(lubridate)
library(politicaldata)
library(pbapply)
library(parallel)
library(ggrepel)
library(caret)
library(glmnet)
library(kknn)
library(urbnmapr)
library(data.table)


# setup ------------------- \\
## MASTER VARIABLES
# run date?
RUN_DATE <- as_date(Sys.Date()) 

# this much daily sd in election polls
SD_AT_DAY_300 <- 0.1 
SD_AT_DAY_0 <- 0
DAILY_SD <- (SD_AT_DAY_300 - SD_AT_DAY_0) / 300
DAILY_SD * c(0,100,200,300)

# number of simulations to run
NUM_SIMS <- 100000

# number of cores to use
NUM_CORES <- min(6, parallel::detectCores())

# whether to burn all the models up and start over
REDO_ALL_MODELS <- FALSE

## data
# read in the polls
url<- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQ56fySJKLL18Lipu1_i3ID9JE06voJEz2EXm6JW4Vh11zmndyTwejMavuNntzIWLY0RyhA1UsVEen0/pub?gid=0&single=true&output=csv'

all_polls <- read_csv(url)

# impose a master filter over all modes
all_polls <- all_polls %>%
  filter(grepl('phone|online',tolower(mode)))

todays_polls <- all_polls

# read in state voter weights
states2016 <- read_csv('data/2016.csv') %>%
  mutate(score = clinton_count / (clinton_count + trump_count),
         national_score = sum(clinton_count)/sum(clinton_count + trump_count),
         delta = score - national_score,
         share_national_vote = (total_count*(1+adult_pop_growth_2011_15))
         /sum(total_count*(1+adult_pop_growth_2011_15))) %>%
  arrange(state) 

state_weights <- c(states2016$share_national_vote / sum(states2016$share_national_vote))
names(state_weights) <- states2016$state


# simulate the election as of today ---------------------------------------

#' function to repeat the above for a given date
#' @param RUN_DATE the date we're running on
#' @param all_polls the raw polls read from the google sheet without any wrangling yet
#' @param DAILY_SD the daily standard deviation in the random walk in the polls 

simulation_election_day_x <- function(RUN_DATE, todays_polls, DAILY_SD){
  message(sprintf("Simulating the election for %s....", RUN_DATE))
  
  # days til the election?
  days_til_election <- as.numeric(ymd('2020-11-03') - RUN_DATE)
  start_date <- ymd("2020-01-01") # min(todays_polls$date)
  
  
  # wrangle polls -----------------------------------------------------------
  todays_polls <- todays_polls %>% 
    filter(as_date(as_datetime(todays_polls$entry.date.time..et., format='%m/%d/%Y %H:%M:%S')) <= RUN_DATE) %>%
    mutate(date = mdy(end.date))
  
  head(todays_polls)
  
  # remove any polls if clinton or trump blank
  todays_polls <- todays_polls %>% filter(!is.na(biden),!is.na(trump))#, include == "TRUE")
  
  todays_polls <- todays_polls %>%
    mutate(weight = sqrt(number.of.observations / mean(number.of.observations,na.rm=T)))
  
  # how much should we weight regression by compared to polls?
  # 1 = the weight of an average-sized poll
  regression_weight <-  3 # so 5 is the weight of five polls
  # sqrt((all_polls %>% filter(state != '--') %>% pull(number.of.observations) %>% mean * 0.5) /
  #      (all_polls %>% filter(state != '--') %>% pull(number.of.observations) %>% mean))
  
  
  # get rolling average of national polls -----------------------------------
  
  # average national and state polls
  # should weight according to sample size and recency
  # we could be more complicated about this, but that's not necessary... yet...
  
  # avg overy day
  national_poll_average <- lapply(as_date(start_date:RUN_DATE),
                                  function(RUN_DATE_MOD){
                                    
                                    national_biden_margin_MOD <- todays_polls %>%
                                      mutate(date_entered = as_date(as_datetime(todays_polls$entry.date.time..et., format='%m/%d/%Y %H:%M:%S')) ) %>%
                                      filter(date_entered <= RUN_DATE_MOD) %>%
                                      filter(state == '--') %>%
                                      mutate(decayed_weight = exp( as.numeric(RUN_DATE_MOD - mdy(end.date))*-0.1)) %>%
                                      summarise(mean_biden_margin = weighted.mean(biden-trump,weight*decayed_weight,na.rm=T)) %>%
                                      pull(mean_biden_margin)/100
                                    
                                    tibble(date = RUN_DATE_MOD,
                                           national_biden_margin = national_biden_margin_MOD)
                                    
                                  }) %>% bind_rows
  
  ggplot(national_poll_average, aes(x=date,y=national_biden_margin)) +
    geom_line()  +
    geom_point(data =  todays_polls %>%
                 filter(as_date(as_datetime(todays_polls$entry.date.time..et., format='%m/%d/%Y %H:%M:%S')) <= RUN_DATE) %>%
                 filter(state == '--') %>%
                 mutate(biden_margin = (biden-trump)/100),
               aes(x=date,y=biden_margin),alpha=0.2)
  
  # now filter dates
  todays_polls <- todays_polls %>% filter(mdy(end.date) >= start_date)
  
  # get the last one for later on
  national_biden_margin <- last(national_poll_average$national_biden_margin)
  
  # now trend line adjust the polls
  national_poll_average_deltas <- national_poll_average %>% 
    mutate(national_biden_margin_delta = last( national_biden_margin) - national_biden_margin)
  
  state_averages <- todays_polls %>%
    filter(state != '--') %>%
    # trend line adjust
    left_join(national_poll_average_deltas) %>%
    mutate(biden_margin = (biden-trump) + national_biden_margin_delta) %>%
    # average
    group_by(state) %>%
    mutate(decayed_weight = exp( as.numeric(RUN_DATE - mdy(end.date))*-0.1)) %>%
    summarise(mean_biden_margin = weighted.mean(biden_margin,weight*decayed_weight,na.rm=T)/100,
              num_polls = n(),
              sum_weights = sum(weight,na.rm=T))
  
  # get 2016 results
  results <- politicaldata::pres_results %>% 
    filter(year == 2016) %>%
    mutate(clinton_margin = dem-rep) %>%
    select(state,clinton_margin)
  
  # coefs for a simple model
  coefs <- read_csv('data/state_coefs.csv')
  
  # bind everything together
  # make log pop density
  state <- results %>%
    left_join(state_averages, by = "state") %>%
    mutate(dem_lean_2016 = clinton_margin - 0.021,
           dem_lean_2016_polls = mean_biden_margin - national_biden_margin) %>%
    left_join(coefs, by = "state")
  
  
  # also create a dataset of all the state polls for the model
  state_polls <- todays_polls %>%
    filter(state != '--') %>%
    left_join(results, by = "state") %>%
    mutate(mean_biden_margin = biden_margin/100,
           sum_weights = weight,
           dem_lean_2016 = clinton_margin - 0.021,
           dem_lean_2016_polls = mean_biden_margin - national_biden_margin) %>%
    left_join(coefs, by = "state")
  
  
  # model to fill in polling gaps -------------------------------------------
  
  # simple stepwise linear model with AIC selection
  stepwise_model <- step(lm(mean_biden_margin ~  black_pct  + college_pct + 
                              hisp_other_pct + pct_white_evangel + pop_density + 
                              white_pct + wwc_pct,
                            data = state %>%
                              select(mean_biden_margin,black_pct,college_pct,
                                     hisp_other_pct,median_age,pct_white_evangel,
                                     pop_density,white_pct,wwc_pct,sum_weights) %>%
                              mutate_at(c('black_pct','college_pct',
                                          'hisp_other_pct','median_age','pct_white_evangel',
                                          'pop_density','white_pct','wwc_pct'),
                                        function(x){
                                          (x - mean(x)) / sd(x)
                                        }) %>%
                              na.omit(),
                            weight = sum_weights))
  
  summary(stepwise_model)
  
  # glmnet model fit using caret
  # training is the poll data
  training <- state %>% 
    dplyr::select(state,mean_biden_margin,black_pct,college_pct,
                  hisp_other_pct,median_age,pct_white_evangel,
                  pop_density,white_pct,wwc_pct,sum_weights) %>%
    mutate_at(c('black_pct','college_pct',
                'hisp_other_pct','median_age','pct_white_evangel',
                'pop_density','white_pct','wwc_pct'),
              function(x){
                (x - mean(x)) / sd(x)
              }) %>% 
    na.omit()
  
  # testing is the averaged data
  testing <- state  %>% 
    dplyr::select(state,mean_biden_margin,black_pct,college_pct,
                  hisp_other_pct,median_age,pct_white_evangel,
                  pop_density,white_pct,wwc_pct,sum_weights) %>%
    mutate_at(c('black_pct','college_pct',
                'hisp_other_pct','median_age','pct_white_evangel',
                'pop_density','white_pct','wwc_pct'),
              function(x){
                (x - mean(x)) / sd(x)
              }) 
  
  glmnet_model <- train(mean_biden_margin ~  black_pct + college_pct + 
                          hisp_other_pct + pct_white_evangel + pop_density + 
                          white_pct + wwc_pct,
                        data = training,
                        weights = sum_weights,
                        method = "glmnet",
                        metric = "RMSE",
                        trControl = trainControl(method="LOOCV"),
                        preProcess = c("center", "scale"),
                        tuneLength = 10)
  
  glmnet_model
  
  
  # combine predictions from the two models
  preds <- testing %>%
    mutate(aic_pred = predict(object=stepwise_model,newdata=.),
           glmnet_pred = predict(object=glmnet_model,newdata=testing)) %>%
    mutate(pred = (aic_pred + glmnet_pred)/2) %>%
    pull(pred)
  
  # and average the demographic predictions with the implied margin from partisan lean
  # giving more weight to the partisan lean until we have a ton of polls to shore up the regression
  demo_weight <- min( sum(state$sum_weights,na.rm=T) / (sum(state$sum_weights,na.rm=T) + 100), 0.5)
  partisan_weight <- 1 - demo_weight
  
  preds <- (preds * (demo_weight)) + 
    ( (state$dem_lean_2016 + national_biden_margin) * (partisan_weight) )
  
  # make the projections
  testing$proj_mean_biden_margin <- preds
  
  # check predictions
  ggplot(na.omit(testing), aes(x=mean_biden_margin,y=proj_mean_biden_margin,label=state)) +
    geom_text() + 
    geom_abline() + 
    geom_smooth(method='lm')
  
  mean(abs(testing$proj_mean_biden_margin - testing$mean_biden_margin),na.rm=T)
  
  # average predictions with the polls ------------------------------------
  state <- state %>%
    # append the predictions
    left_join(testing %>% dplyr::select(state,proj_mean_biden_margin), by = "state") %>%
    # make some mutations
    mutate(sum_weights = ifelse(is.na(sum_weights),0,sum_weights),
           mean_biden_margin = ifelse(is.na(mean_biden_margin),999,mean_biden_margin),
           poll_weight = sum_weights / (sum_weights + regression_weight),
           demo_regression_weight = regression_weight / (sum_weights + regression_weight))  %>%
    mutate(mean_biden_margin_hat = #proj_mean_biden_margin
             (mean_biden_margin * poll_weight ) +
             (proj_mean_biden_margin * demo_regression_weight )
    ) %>%
    mutate(mean_biden_margin = ifelse(mean_biden_margin == 999,NA,mean_biden_margin))
  
  
  # adjust state projections to match average of national vote 
  og_national_biden_margin <- last(national_poll_average$national_biden_margin)
  
  implied_national_biden_margin <- weighted.mean(state$mean_biden_margin_hat,state_weights) 
  
  # regress the state predictions back toward the national average
  natl_diff <- function(par, 
                        dat = state, 
                        weights = state_weights,
                        target_natl = og_national_biden_margin,
                        current_natl = implied_national_biden_margin){

    dat$mean_biden_margin_hat_shift <- dat$mean_biden_margin_hat + (target_natl - current_natl)*par
    
    #print(weighted.mean(dat$mean_biden_margin_hat, weights) )
    #print(weighted.mean(dat$mean_biden_margin_hat_shift, weights) )
    
    return( abs( weighted.mean(dat$mean_biden_margin_hat_shift, weights) - target_natl) )
    # return( dat$mean_biden_margin_hat )

  }
  
  natl_diff(par = 1)
  
  multiplier <- optim(par = 1,fn = natl_diff,method = "Brent",upper = 5, lower = -5)$par
  
  state$mean_biden_margin_hat <- state$mean_biden_margin_hat + 
    (og_national_biden_margin - implied_national_biden_margin)*multiplier
  
  
  # save margin for later
  national_biden_margin <- weighted.mean(state$mean_biden_margin_hat,state_weights) 
  
  # plot final prediction against data
  ggplot(na.omit(state), aes(mean_biden_margin, mean_biden_margin_hat, label=state)) +
    geom_text(aes(size=num_polls)) + 
    geom_abline() + 
    geom_smooth(method='lm')
  
  # generate new state lean variable based on adjusted biden national margin
  state$dem_lean_2016 <-  state$mean_biden_margin_hat - national_biden_margin 
  
  state_evs <- read_csv('data/state_evs.csv')
  
  # clean up estimates
  final <- state %>%
    dplyr::select(state,region,clinton_margin,dem_lean_2016,
                  mean_biden_margin = mean_biden_margin_hat,
                  dem_lean_2016_polls,
                  dem_lean_2016, 
                  num_polls,
                  pop_density,
                  wwc_pct) %>%
    mutate(shift = dem_lean_2016 - dem_lean_2016)
  
  final <- final %>%
    left_join(state_evs)
  
  
  # toy simulations ---------------------------------------------------------
  # errors
  national_error <- sqrt((0.025^2) + ((DAILY_SD * days_til_election)^2) ) # national error + drift
  regional_error <- (0.025) 
  state_error <- (0.03) 
  
  sqrt(national_error^2 + regional_error^2 + state_error^2) # this is the total standard deviation on vote margin
  
  # sims
  national_errors <- rnorm(NUM_SIMS, 0, national_error)
  regional_errors <- replicate(NUM_SIMS, rnorm(length(unique(final$region)), 0, regional_error))
  state_errors <- replicate(NUM_SIMS, rnorm(51, 0, state_error))
  
  # actual sims
  
  #' Simulate polling errors.
  #'
  #' @param state_errors Matrix of simulated state polling errors. (num_states x NUM_SIMS)
  #' @param regional_errors Matrix of simulated regional polling errors. (num_regions x NUM_SIMS)
  #' @param national_errors Numeric vector of simulated national polling errors. Length is NUM_SIMS.
  #' @param state_region Data frame of states and their regions.
  #'
  #' @return A data frame of simulated polling errors with the following columns: sim, state, region, state_error,
  #' regional_error, national_error. One row per simulation.
  #'
  simulate_polling_errors <- function(state_errors, regional_errors, national_errors, state_region) {
    states <- unique(state_region$state)
    
    regions <- unique(state_region$region)
    
    state_errors <- state_errors %>%
      t() %>%
      as_tibble(.name_repair = "minimal") %>%
      set_names(~ states) %>%
      mutate(sim = row_number()) %>%
      pivot_longer(-sim, names_to = "state", values_to = "state_error")
    
    regional_errors <- regional_errors %>%
      t() %>%
      as_tibble(.name_repair = "minimal") %>%
      set_names(~ regions) %>%
      mutate(sim = row_number()) %>%
      pivot_longer(-sim, names_to = "region", values_to = "regional_error")
    
    national_errors <-
      tibble(sim = 1:length(national_errors), national_error = national_errors)
    
    state_region %>%
      left_join(state_errors, by = "state") %>%
      left_join(regional_errors, by = c("region", "sim")) %>%
      left_join(national_errors, by = "sim") %>%
      select(sim, state, region, state_error, regional_error, national_error)
  }
  
  state_region <- final %>%
    ungroup() %>%
    select(state, region) %>%
    distinct()
  
  simulated_polling_errors <- simulate_polling_errors(state_errors, regional_errors, national_errors, state_region)
  
  sims <- simulated_polling_errors %>%
    left_join(final %>% select(state, dem_lean_2016), by = "state") %>%
    mutate(proj_biden_margin = dem_lean_2016 + national_biden_margin,
           error = state_error + regional_error + national_error,
           sim_biden_margin = proj_biden_margin + error) %>%
    group_by(state) %>%
    mutate(draw = row_number()) %>%
    left_join(state_evs, by='state') %>%
    left_join(enframe(state_weights, 'state', 'weight'), by = "state") %>%
    group_by(draw) %>%
    mutate(dem_nat_pop_margin = weighted.mean(sim_biden_margin, weight)) %>%
    select(state, sim_biden_margin, draw, ev, weight, dem_nat_pop_margin)
  
  # summarise state data
  state_summary <- sims %>%
    group_by(state) %>%
    summarise(biden_margin_mean = mean(sim_biden_margin),
              biden_margin_high = quantile(sim_biden_margin, 0.975),
              biden_margin_low = quantile(sim_biden_margin, 0.025),
              biden_win_prob = mean(sim_biden_margin >= 0))
  
  # summarise national data
  national_summary  <- sims %>%
    group_by(draw) %>%
    summarise(natl_dem_ev = sum(ev * (sim_biden_margin >= 0)),
              dem_nat_pop_margin = unique(dem_nat_pop_margin)) %>%
    ungroup() %>%
    summarise(
      # votes
      biden_nat_margin_mean = mean(dem_nat_pop_margin),
      biden_nat_margin_high = quantile(dem_nat_pop_margin, 0.975),
      biden_nat_margin_low = quantile(dem_nat_pop_margin, 0.025),
      biden_nat_margin_win_prob = mean(dem_nat_pop_margin >= 0),
      # ec outcomes
      biden_ec_vote_mean = mean(natl_dem_ev),
      biden_ec_vote_mean = median(natl_dem_ev),
      biden_ec_vote_high = quantile(natl_dem_ev, 0.975),
      biden_ec_vote_low = quantile(natl_dem_ev, 0.025),
      biden_ec_vote__win_prob = mean(natl_dem_ev >= 270),
    )
  
  # histogram of EC and pop votes
  sims_summary <- sims %>%
    group_by(draw) %>%
    summarise(natl_dem_ev = sum(ev * (sim_biden_margin >= 0)),
              dem_nat_pop_margin = unique(dem_nat_pop_margin)) %>%
    ungroup()
  
  # return list of this ------
  # return
  list(RUN_DATE = RUN_DATE,
       national_summary = national_summary,
       state_summary = state_summary,
       sims_summary = sims_summary,
       raw_sims = sims,
       og_national_biden_margin = og_national_biden_margin,
       implied_national_biden_margin = implied_national_biden_margin,
       national_biden_margin = national_biden_margin)
  
  
}


# simulate for every day
todays_simulations <- simulation_election_day_x(RUN_DATE, all_polls, DAILY_SD)


# save output, less the big sims
output <- todays_simulations
output$raw_sims <- NULL
write_rds(output,sprintf('output/model_runs/model_run_%s.rds',todays_simulations$RUN_DATE),compress = 'gz')


# calc shift from 2016 to 2020 --------------------------------------------
# first, get the national margins from the model
og_national_biden_margin <- todays_simulations$og_national_biden_margin
implied_national_biden_margin <- todays_simulations$implied_national_biden_margin
national_biden_margin <- todays_simulations$national_biden_margin

# import results data
results <- politicaldata::pres_results %>% 
  filter(year == 2016) %>%
  mutate(clinton_margin = dem-rep) %>%
  select(state,clinton_margin)

coefs <- read_csv('data/state_coefs.csv')

state <- results %>%
  left_join(todays_simulations$state_summary,by='state') %>%
  mutate(national_biden_margin = national_biden_margin,
         dem_lean_2016 = clinton_margin - 0.021,
         dem_lean_2020 = biden_margin_mean - national_biden_margin) %>%
  left_join(coefs, by = "state")


state_evs <- read_csv('data/state_evs.csv')

# clean up estimates
final <- state %>%
  dplyr::select(state,region,clinton_margin,dem_lean_2016,
                biden_margin_mean,
                dem_lean_2020, 
                pop_density,
                wwc_pct) %>%
  mutate(shift = dem_lean_2020 - dem_lean_2016)

final <- final %>%
  left_join(state_evs)


# plot
final %>% 
  filter(abs(clinton_margin) < 0.1) %>% # num_polls > 0
  ggplot(., aes(y=reorder(state,shift),x=shift,
                col = clinton_margin > 0)) + 
  #geom_point() +
  geom_vline(xintercept = 0) + 
  geom_label(aes(label = state,size=ev)) +
  scale_size(range=c(2,6)) + 
  scale_x_continuous(breaks=seq(-1,1,0.01),
                     labels = function(x){round(x*100)}) +
  scale_color_manual(values=c('TRUE'='blue','FALSE'='red')) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(subtitle='Swing toward Democrats in relative presidential vote margin\nSized by electoral votes',
       x='Biden state margin relative to national margin\nminus Clinton state margin relative to national margin')

# as a map
# plot
swing.map.gg <- urbnmapr::states %>%
  left_join(final %>% rename(state_abbv = state)) %>%
  filter(state_abbv != 'DC') %>%
  ggplot(aes(x=long,y=lat,group=group,
             fill = shift*100)) +
  geom_polygon(col='gray40')  + 
  coord_map("albers",lat0=39, lat1=45) +
  scale_fill_gradient2(name='Swing toward Democrats in relative presidential vote margin*',high='#3498DB',low='#E74C3C',mid='gray98',midpoint=0,guide = 'legend') +
  labs(caption='*Biden state margin relative to national margin minus Clinton state margin relative to national margin') +
  guides('fill'=guide_legend(nrow=1,title.position = 'top')) +
  theme_void() + 
  theme(legend.position = 'top',
        plot.caption = element_text(hjust=0.2,margin=margin(30,30,30,30)))


# plot relationswhip between shift in lean with wwc
final %>% 
  filter(state != 'DC') %>%
  ggplot(., aes(x=wwc_pct,y=shift,
                col = clinton_margin > 0,group=NA)) + 
  geom_label(aes(label = state,size=ev)) +
  geom_smooth(method='lm')

# any relationship with urbanicity?
final %>%
  filter(state != 'DC') %>%
  ggplot(.,aes(x=pop_density,y=biden_margin_mean,label=state,
               col = clinton_margin > 0,group=NA)) + 
  geom_text_repel() + 
  geom_smooth(method='lm',col='black',linetype=2) + 
  scale_y_continuous(breaks=seq(-1,1,0.05), labels = function(x){round(x*100)}) +
  scale_color_manual(values=c('TRUE'='blue','FALSE'='red')) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none') +
  labs(subtitle='2020 Biden vote margin*',
       x='Number of people living within 5 miles of each resident (logged)',
       y='',
       caption='*2020 Biden margin is an average of the polls and a regression model\nthat predicts poll margins with state-level demographics')

# tipping point state?
final %>%
  arrange(desc(biden_margin_mean)) %>%
  mutate(cumulative_ev = cumsum(ev)) %>%
  filter(cumulative_ev >= 270) %>% filter(row_number() == 1)  

final %>%
  arrange(desc(biden_margin_mean)) %>%
  mutate(cumulative_ev = cumsum(ev)) %>%
  filter(cumulative_ev >= 270) %>% filter(row_number() == 1)  %>%
  pull(biden_margin_mean) -  national_biden_margin



# overall analysis for today's simulations --------------------------------
sims <- todays_simulations$raw_sims

# calc the avg tipping point
tipping_point <- sims %>%
  group_by(draw) %>%
  mutate(dem_ev = sum(ev * (sim_biden_margin > 0))) %>% 
  arrange(draw,
          ifelse(dem_ev >= 270, desc(sim_biden_margin),sim_biden_margin)) %>%
  ungroup() 

# state-level correlations?
tipping_point %>%
  dplyr::select(draw,state,sim_biden_margin) %>%
  spread(state,sim_biden_margin) %>%
  dplyr::select(-draw) %>%
  cor 

# what is the tipping point
tipping_point %>%
  group_by(draw) %>%
  mutate(cumulative_ev = cumsum(ev)) %>%
  filter(cumulative_ev >= 270) %>%
  filter(row_number() == 1) %>% 
  group_by(state) %>%
  summarise(prop = n()) %>%
  mutate(prop = round(prop / sum(prop)*100,1)) %>%
  arrange(desc(prop)) %>% filter(prop>1) %>% as.data.frame()

tipping_point.kable <- left_join(tipping_point %>%
                                   group_by(draw) %>%
                                   mutate(cumulative_ev = cumsum(ev)) %>%
                                   filter(cumulative_ev >= 270) %>%
                                   filter(row_number() == 1) %>% 
                                   group_by(state) %>%
                                   summarise(prop = n()) %>%
                                   mutate(prop = round(prop / sum(prop)*100,1)) %>%
                                   arrange(desc(prop))  %>% 
                                   head(nrow(.)/2) %>%
                                   mutate(row_number = row_number()),
                                 tipping_point %>%
                                   group_by(draw) %>%
                                   mutate(cumulative_ev = cumsum(ev)) %>%
                                   filter(cumulative_ev >= 270) %>%
                                   filter(row_number() == 1) %>% 
                                   group_by(state) %>%
                                   summarise(prop = n()) %>%
                                   mutate(prop = round(prop / sum(prop)*100,1)) %>%
                                   arrange(desc(prop))  %>% 
                                   tail(nrow(.)-(nrow(.)/2))%>%
                                   mutate(row_number = row_number()),
                                 by='row_number') %>% 
  select(-row_number) %>%
  setNames(.,c('State','Tipping point chance (%)','State','Tipping point chance (%)')) %>%
  knitr::kable(.)

# ev-popvote divide?
ev.popvote.divide <- tipping_point %>%
  group_by(draw) %>%
  mutate(cumulative_ev = cumsum(ev)) %>%
  filter(cumulative_ev >= 270) %>%
  filter(row_number() == 1)  %>%
  mutate(diff =  sim_biden_margin - dem_nat_pop_margin) %>%
  pull(diff) %>% mean

ev.popvote.divide

# what is the pop vote range?
dem_nat_pop_margin <- sims %>%
  group_by(draw) %>%
  summarise(dem_nat_pop_margin = unique(dem_nat_pop_margin)) %>%
  pull(dem_nat_pop_margin)

dem_pop_vote_prob <- mean(dem_nat_pop_margin>0)

# the ec range?
dem_ec_majority_prob <- sims %>%
  group_by(draw) %>%
  summarise(dem_ev = sum(ev * (sim_biden_margin >= 0))) %>%
  pull(dem_ev)

dem_ec_majority_prob <- mean(dem_ec_majority_prob >= 270)

# extract state-level data
state_probs <- sims %>%
  group_by(state_abbv = state) %>%
  summarise(mean_biden_margin = mean(sim_biden_margin,na.rm=T),
            se_biden_margin = sd(sim_biden_margin,na.rm=T),
            ev = unique(ev),
            prob = mean(sim_biden_margin > 0,na.rm=T)) %>%
  ungroup() %>%
  arrange(desc(mean_biden_margin)) %>% 
  mutate(cumulative_ev = cumsum(ev)) 

# graph mean estimates by state
margin_map.gg <- urbnmapr::states %>%
  left_join(state_probs) %>%
  filter(state_abbv != 'DC') %>%
  ggplot(aes(x=long,y=lat,group=group,
             fill = mean_biden_margin*100)) +
  geom_polygon(col='gray40')  + 
  coord_map("albers",lat0=39, lat1=45) +
  scale_fill_gradient2(name='Biden vote margin',high='#3498DB',low='#E74C3C',mid='gray98',midpoint=0,
                       guide = 'legend') +
  guides('fill'=guide_legend(nrow=1,title.position = 'top',title.hjust = 0.5)) +
  theme_void() + 
  theme(legend.position = 'top')

margin_map.gg

# table form  -- close states
sumary_table.close <- state_probs %>%
  arrange(abs(mean_biden_margin)) %>% 
  head(20) %>%
  mutate(upper = round((mean_biden_margin + se_biden_margin*1.96)*100),
         lower = round((mean_biden_margin - se_biden_margin*1.96)*100),
         mean_biden_margin = round(mean_biden_margin*100)) %>%
  arrange(desc(mean_biden_margin)) %>% 
  mutate(txt = sprintf('%s [%s, %s]',mean_biden_margin,lower,upper)) %>%
  dplyr::select(state_abbv,txt) 

margin.kable.close <- left_join(sumary_table.close %>%
                                  head(ceiling(nrow(.)/2)) %>%
                                  mutate(row_number = row_number()),
                                sumary_table.close %>%
                                  tail(floor(nrow(.)-(nrow(.)/2))) %>%
                                  mutate(row_number = row_number()),
                                by='row_number') %>% 
  select(-row_number)

margin.kable.close[is.na(margin.kable.close)] <- ' '

margin.kable.close <- margin.kable.close %>%
  setNames(.,c('State','Biden margin, uncertainty interval (%)','State','Biden margin, ... (%)')) %>%
  knitr::kable(.)

# table form  -- not  states
sumary_table.not_close <- state_probs %>%
  arrange(abs(mean_biden_margin)) %>% 
  tail(31) %>%
  mutate(upper = round((mean_biden_margin + se_biden_margin*1.96)*100),
         lower = round((mean_biden_margin - se_biden_margin*1.96)*100),
         mean_biden_margin = round(mean_biden_margin*100)) %>%
  arrange(desc(mean_biden_margin)) %>% 
  mutate(txt = sprintf('%s [%s, %s]',mean_biden_margin,lower,upper)) %>%
  dplyr::select(state_abbv,txt) 

margin.kable.not_close <- left_join(sumary_table.not_close %>%
                                      head(ceiling(nrow(.)/2)) %>%
                                      mutate(row_number = row_number()),
                                    sumary_table.not_close %>%
                                      tail(floor(nrow(.)-(nrow(.)/2))) %>%
                                      mutate(row_number = row_number()),
                                    by='row_number') %>% 
  select(-row_number)

margin.kable.not_close[is.na(margin.kable.not_close)] <- ' '

margin.kable.not_close <- margin.kable.not_close %>%
  setNames(.,c('State','Biden margin, uncertainty interval (%)','State','Biden margin, ... (%)')) %>%
  knitr::kable(.)


# graph win probabilities
win_probs_map <- urbnmapr::states %>%
  left_join(state_probs) %>%
  ggplot(aes(x=long,y=lat,group=group,fill=prob*100)) +
  geom_polygon(col='gray40')  + 
  coord_map("albers",lat0=39, lat1=45) +
  scale_fill_gradient2(name='Democratic win probability',high='#3498DB',low='#E74C3C',mid='gray98',midpoint=50,
                       limits = c(0,100)) +
  theme_void() + 
  theme(legend.position = 'top')

win_probs_map

# electoral vote histogram
ev.histogram <- sims %>%
  group_by(draw) %>%
  summarise(dem_ev = sum(ev * (sim_biden_margin > 0))) %>%
  ggplot(.,aes(x=dem_ev,fill=dem_ev >= 270)) +
  geom_histogram(binwidth=1) + 
  scale_fill_manual(values=c('TRUE'='blue','FALSE'='red')) +
  scale_y_continuous(labels = function(x){paste0(round(x / max(sims$draw)*100,2),'%')}) +
  scale_x_continuous(breaks = c(0, 130, 270, 400, 538),
                     limits = c(0, 538)) +
  labs(x='Democratic electoral votes',y='Probability') +
  theme_minimal() + 
  theme(legend.position = 'none') +
  labs(subtitle = sprintf('p(Biden win) = %s',
                          sims %>%
                            group_by(draw) %>%
                            summarise(dem_ev = sum(ev * (sim_biden_margin > 0))) %>%
                            ungroup() %>%
                            summarise(dem_ev_majority = round(mean(dem_ev >=270),2)) %>%
                            pull(dem_ev_majority)))

ev.histogram

# scenarios
scenarios.kable <- sims %>%
  group_by(draw) %>%
  summarise(dem_ev = sum(ev * (sim_biden_margin > 0)),
            dem_nat_pop_margin = unique(dem_nat_pop_margin)) %>%
  mutate(scenario = 
           case_when(dem_ev >= 270 & dem_nat_pop_margin > 0 ~ 'Democrats win the popular vote and electoral college',
                     dem_ev >= 270 & dem_nat_pop_margin < 0 ~ 'Republicans win the popular vote, but Democrats win the electoral college',
                     dem_ev <  270 & dem_nat_pop_margin > 0 ~ 'Democrats win the popular vote, but Republicans win the electoral college',
                     dem_ev <  270 & dem_nat_pop_margin < 0 ~ 'Republicans win the popular vote and electoral college',
           )) %>%
  group_by(scenario) %>%
  summarise(chance = n()) %>%
  mutate(chance = round(chance / sum(chance)*100)) %>%
  setNames(.,c('','Chance (%)')) %>%
  knitr::kable()


# tipping point state, maps -----------------------------------------------
# calc the avg tipping point
tipping_point <- sims  %>%
  group_by(draw) %>%
  mutate(dem_ev = sum(ev * (sim_biden_margin >= 0))) %>%
  arrange(draw,ifelse(dem_ev >= 270, desc(sim_biden_margin),sim_biden_margin))


# state-level correlations?
tipping_point %>%
  dplyr::select(draw,state,sim_biden_margin) %>%
  spread(state,sim_biden_margin) %>%
  dplyr::select(-draw) %>%
  cor 

# what is the tipping point
tipping_point %>%
  group_by(draw) %>%
  mutate(cumulative_ev = cumsum(ev)) %>%
  filter(cumulative_ev >= 270) %>%
  filter(row_number() == 1) %>% 
  group_by(state) %>%
  summarise(prop = n()) %>%
  mutate(prop = round(prop / sum(prop)*100,1)) %>%
  arrange(desc(prop)) %>% filter(prop>1) %>% as.data.frame()

tipping_point.kable <- left_join(tipping_point %>%
                                   group_by(draw) %>%
                                   mutate(cumulative_ev = cumsum(ev)) %>%
                                   filter(cumulative_ev >= 270) %>%
                                   filter(row_number() == 1) %>% 
                                   group_by(state) %>%
                                   summarise(prop = n()) %>%
                                   mutate(prop = round(prop / sum(prop)*100,1)) %>%
                                   arrange(desc(prop))  %>% 
                                   head(nrow(.)/2) %>%
                                   mutate(row_number = row_number()),
                                 tipping_point %>%
                                   group_by(draw) %>%
                                   mutate(cumulative_ev = cumsum(ev)) %>%
                                   filter(cumulative_ev >= 270) %>%
                                   filter(row_number() == 1) %>% 
                                   group_by(state) %>%
                                   summarise(prop = n()) %>%
                                   mutate(prop = round(prop / sum(prop)*100,1)) %>%
                                   arrange(desc(prop))  %>% 
                                   tail(nrow(.)-(nrow(.)/2))%>%
                                   mutate(row_number = row_number()),
                                 by='row_number') %>% 
  select(-row_number) %>%
  setNames(.,c('State','Tipping point chance (%)','State','Tipping point chance (%)')) %>%
  knitr::kable(.)

# ec - popular vote gap ---------------------------------------------------

# ev-popvote divide?
ev.popvote.hist <- tipping_point %>%
  group_by(draw) %>%
  mutate(cumulative_ev = cumsum(ev)) %>%
  filter(cumulative_ev >= 270) %>%
  filter(row_number() == 1)  %>%
  mutate(diff =  sim_biden_margin - dem_nat_pop_margin,
         winner = ifelse(dem_ev >= 270,'Democratic','Republican')) %>%
  ggplot(.,aes(x=diff,fill=winner)) +
  geom_vline(xintercept = 0) +
  geom_histogram(binwidth=0.001) +
  scale_x_continuous(breaks=seq(-1,1,0.01),labels=function(x){round(x*100)}) +
  scale_y_continuous(labels=function(x){paste0(round(x/NUM_SIMS*100),'%')}) +
  theme_minimal() + 
  theme(legend.position = 'top',panel.grid.minor = element_blank()) +
  scale_fill_manual(name='Electoral college majority',values=c('Democratic'='blue','Republican'='red')) +
  labs(x="Difference betwen the popular vote and margin in the tipping-point state",
       y='Probability') 

ev.popvote.hist


# look at some interesting differences
target_draw <- tipping_point %>%
  group_by(draw) %>%
  mutate(cumulative_ev = cumsum(ev)) %>%
  filter(cumulative_ev >= 270) %>%
  filter(row_number() == 1)  %>%
  mutate(diff =  sim_biden_margin - dem_nat_pop_margin)  %>%
  filter(diff >= 0.01) %>% 
  ungroup() %>%
  sample_n(size = 16) %>% 
  pull(draw)

tipping_point %>%
  filter(draw %in% target_draw) %>% 
  mutate(cumulative_ev = cumsum(ev)) %>% 
  left_join(politicaldata::pres_results %>% filter(year == 2016)) %>%
  mutate(state_lean_2016 = (dem - rep) - 0.021,
         state_lean_2020 = sim_biden_margin - dem_nat_pop_margin) %>%
  filter(state != "DC") %>%
  ggplot(.,aes(x=state_lean_2016, y=state_lean_2020)) +
  geom_abline() +
  geom_text(aes(label=state)) +
  geom_smooth(method = 'lm') +
  facet_wrap(~draw)

# probability of being the tipping point for each state conditional on gap
bkdown <- tipping_point %>%
  group_by(draw) %>%
  mutate(cumulative_ev = cumsum(ev)) %>%
  filter(cumulative_ev >= 270) %>%
  filter(row_number() == 1)  %>%
  mutate(diff =  sim_biden_margin - dem_nat_pop_margin)  %>%
  select(diff,state) %>%
  group_by(diff = round(diff,2),state) %>%
  summarise(n=n()) %>%
  group_by(diff) %>%
  mutate(tipping_point_chance = n /sum(n))

bkdown <- lapply(unique(bkdown$state),
       function(target_state){
         state_bkdown <- bkdown %>% filter(state == target_state)
         
         tibble(state = target_state,
                diff = seq(-0.08,0.03,0.01)) %>%
           mutate(tipping_point_chance = 
                    predict(glm(tipping_point_chance ~ diff, 
                                data = state_bkdown,
                                family = binomial(link='logit'),
                                weights = n),newdata = .,type = 'response')
                  )
       }) %>% bind_rows

# chart of top states
top_tipping_points <- tipping_point %>%
  group_by(draw) %>%
  mutate(cumulative_ev = cumsum(ev)) %>%
  filter(cumulative_ev >= 270) %>%
  filter(row_number() == 1) %>% 
  group_by(state) %>%
  summarise(prop = n()) %>%
  mutate(prop = round(prop / sum(prop)*100,1)) %>%
  arrange(desc(prop)) %>% filter(prop>1) %>% as.data.frame() %>%
  head(6) %>% pull(state)

bkdown %>%
  filter(state %in% top_tipping_points)  %>%
  ggplot(.,aes(x=diff,y=tipping_point_chance,col=state)) +
  geom_line()


# EC gap ovter time

results <- read_csv('data/potus_historical_results.csv')

# apply the infaltor from multi- to two-party vote
margin_inflator <- all_polls %>%
  filter(state != '--') %>%
  mutate(two_party_margin = (biden / (biden + trump)) - (trump / (biden + trump)),
         biden_margin = (biden - trump)/100) %>%
  summarise(margin_inflator = mean(two_party_margin / biden_margin,na.rm=T)) %>% 
  pull(margin_inflator)

# bind with averages
results <- results %>%
  bind_rows(state_probs %>%
              select(state = state_abbv,
                     dem_two_party_share = mean_biden_margin) %>%
              mutate(year = 2020,
                     dem_two_party_share = dem_two_party_share * margin_inflator,
                     dem_two_party_share = 0.5 + dem_two_party_share/2)) 


# data frame with electoral votes
historical_evs <- read_csv('data/state_evs_historical.csv')


results <- results %>%
  left_join(historical_evs) %>%
  na.omit() %>%
  mutate(# how many evs total?
    total_evs = case_when(year < 1960 ~ 531,
                          year == 1960 ~ 537,
                          year > 1960 ~ 538),
    # how many evs to win?
    evs_to_win = case_when(year < 1960 ~ 266,
                           year == 1960 ~ 269,
                           year > 1960 ~ 270)
    
  )

# add usa
usa <- read_csv('data/nationwide_potus_results.csv') %>% filter(State == 'Nationwide')
usa <- usa %>%
  gather(year,vote,2:ncol(.)) %>% 
  mutate(party = gsub("[^a-zA-Z]","",year),
         year = gsub("[a-zA-Z]","",year)) %>%
  group_by(State,year) %>%
  spread(party,vote) %>%
  mutate(dem_two_party_share_national = Dem / (Dem + Rep)) %>%
  as.data.frame() %>%
  ungroup() %>%
  dplyr::select(state=State,year,dem_two_party_share_national) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::select(-state)

results <- results %>%
  left_join(usa)

results <- results %>%
  group_by(year) %>%
  arrange(year,desc(dem_two_party_share)) %>%
  mutate(cumulative_ev = cumsum(ev)) %>%
  filter(cumulative_ev >= evs_to_win) %>%
  filter(row_number()  == 1) %>%
  mutate(dem_two_party_share_national = ifelse(year == 2020, 
                                               0.5 + (national_biden_margin * margin_inflator)/2,
                                               dem_two_party_share_national)) %>%
  na.omit()  %>%
  ungroup()

results %>%
  filter(year >= 1972) %>%
  mutate(lean = dem_two_party_share - dem_two_party_share_national,
         year = fct_reorder(as.character(year),-year)) %>%
  ggplot(.,aes(x=year,y=lean)) +
  geom_col() + 
  theme_minimal() + 
  coord_flip()

# simulate for every day of the cycle so far ------------------------------
# every day from today going back in time
days_to_simulate <- as_date(na.omit(rev(seq.Date(ymd('2020-03-01'),RUN_DATE,'day'))))

# filter to only days we don't have yet
dates_done_already <- as_date(substr(list.files('output/model_runs/'),11,20))

if(isFALSE(REDO_ALL_MODELS)){
  days_to_simulate <- days_to_simulate[!(days_to_simulate %in% dates_done_already)]
}

# run the simulations for each day. in parllale!!
if(length(days_to_simulate) > 0){
  campaign_simulations <- pblapply(1:length(days_to_simulate),
                                  cl = NUM_CORES,
                                  function(idx){
                                    TODAY_RUN_DATE <- days_to_simulate[idx]
                                    print(TODAY_RUN_DATE)
                                    
                                    output <- simulation_election_day_x(RUN_DATE = TODAY_RUN_DATE, all_polls, DAILY_SD)
                                    output$raw_sims <- NULL
                                    
                                    # save
                                    write_rds(output,sprintf('output/model_runs/model_run_%s.rds',output$RUN_DATE))
                                  })
}


# look at time-series results ---------------------------------------------
# first, read them all in
campaign_simulations <- pblapply(list.files('output/model_runs/',full.names = TRUE),
                                 cl = NUM_CORES,
                                 read_rds)

key_states <- c('AZ','TX','FL','GA','NC','PA','MI','OH','IA','WI','NV','MN')


# glance at first entry
campaign_simulations[[1]]$state_summary %>% filter(state %in% key_states)

# look at overall dem margin over time
biden_national_margin_overtime.gg <- 
  map_df(campaign_simulations,
         function(x){
           x[['national_summary']] %>%
             mutate(date = x[['RUN_DATE']])
         })  %>%
    ggplot(.,aes(x=date,y=biden_nat_margin_mean)) +
    geom_hline(yintercept=0,col='#E74C3C',alpha=0.8) +
    geom_line() +
    geom_point(data = all_polls %>%
                 filter(state == '--') %>%
                 mutate(biden_margin = (biden-trump)/100,
                        date = mdy(end.date)) ,
               aes(x=date,y=biden_margin),alpha=0.2) +
    coord_cartesian(xlim=c(ymd('2020-03-01'),Sys.Date())) +
    scale_x_date(date_breaks='month',date_labels='%b') +
    scale_y_continuous(breaks=seq(-1,1,0.05),labels = function(x){x*100}) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())  +
    labs(x='',
         y='',
         subtitle="Projected Biden margin, alongside national polls")

# overall odds over time
biden_chance_of_winning_overtime.gg <- 
  map_df(campaign_simulations,
         function(x){
           x[['national_summary']] %>%
             mutate(date = x[['RUN_DATE']])
         })  %>%
    ggplot(.,aes(x=date,y=biden_ec_vote__win_prob)) +
    geom_hline(yintercept=0.5,col='#E74C3C',alpha=0.8) +
    geom_line() +
    coord_cartesian(xlim=c(ymd('2020-03-01'),Sys.Date())) +
    scale_x_date(date_breaks='month',date_labels='%b') +
    scale_y_continuous(breaks=seq(0,1,0.1),labels = function(x){x*100},
                       limits=c(0,1)) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    labs(x='',
         y='',
         subtitle="Biden chance of winning the electoral college")


# dem margins in key states over time
base_frame <- expand_grid(state = campaign_simulations[[1]]$state_summary$state,
                          date = as_date(ymd('2020-03-01'):RUN_DATE))


polls_and_trends <- 
  # start with base data on dates
  base_frame %>%
  # add averages
  left_join(
    map_df(campaign_simulations,
           function(x){
             x[['state_summary']] %>%
               mutate(date = x[['RUN_DATE']])
             }),
    by = c("state", "date"))  %>%
  # then add polls
  left_join(
    all_polls %>%
      filter(state != '--') %>%
      mutate(biden_margin = (biden-trump)/100,
             date = mdy(end.date)),
    by=c('date','state')
  )


biden_state_margins_overtime.gg <- 
  polls_and_trends %>%
    filter(state %in% key_states) %>%
    ggplot(.) +
    geom_hline(yintercept=0,col='#E74C3C',alpha=0.8) +
    geom_line(data = . %>% filter(!is.na(biden_margin_mean)),
              aes(x=date,y=biden_margin_mean)) +
    geom_ribbon(data = . %>% filter(!is.na(biden_margin_mean)),
              aes(x=date,ymin=biden_margin_low,ymax=biden_margin_high),
              col=NA,alpha=0.2) +
    geom_point(aes(x=date,y=biden_margin),alpha=0.2) +
    scale_y_continuous(breaks = seq(-1,1,0.05),
                       labels = function(x){round(x*100)}) +
    facet_wrap(~state) +
    coord_cartesian(xlim=c(ymd('2020-03-01'),Sys.Date())) +
    scale_x_date(date_breaks='month',date_labels='%b') +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    labs(x='Date',y='',subtitle='Biden margin and 95% prediction interval in key states')

  
# probability in key states over time
biden_state_chance_of_winning_overtime.gg <- 
  polls_and_trends %>%
    filter(state %in% key_states) %>%
    ggplot(.) +
    geom_hline(yintercept=0.5,col='#E74C3C',alpha=0.8) +
    geom_line(data = . %>% filter(!is.na(biden_margin_mean)),
              aes(x=date,y=biden_win_prob)) +
    scale_y_continuous(breaks = seq(0,1,0.1),
                       labels = function(x){round(x*100)},
                       limits = c(0,1)) +
    facet_wrap(~state) +
    coord_cartesian(xlim=c(ymd('2020-03-01'),Sys.Date())) +
    scale_x_date(date_breaks='month',date_labels='%b') +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())  +
    labs(x='Date',y='',subtitle='Biden win probability in key states')



