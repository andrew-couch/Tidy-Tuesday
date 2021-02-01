library(tidyverse)
library(here)

df <- read_csv(here("Data/fight_data_raw.csv"))

# Clean head-to-head data
df <- df %>% 
  # Deselect fields that will not be used in the model
  select(-UFC_Page, -cards, -fights) %>% 
  # Convert field name to lower case
  rename_all(tolower) %>% 
  # Convert date
  mutate(date = as.Date(date, format = "%B %d, %Y")) %>% 
  arrange(desc(date)) %>% 
  # Convert fields to categorical variables
  mutate(fighter_1_fighter = as.factor(fighter_1_fighter),
         fighter_2_fighter = as.factor(fighter_2_fighter),
         method = as.factor(method),
         fighter_1_res = as.factor(fighter_1_res),
         fighter_2_res = as.factor(fighter_2_res),
         weight_class = as.factor(weight_class)) %>% 
  # Adjust percents 
  mutate(fighter_1_sig_strike_percent = fighter_1_sig_strike_landed / fighter_1_sig_strike_attempts,
         fighter_1_td_percent = fighter_1_td_landed / fighter_1_td_attempts,
         fighter_2_sig_strike_percent = fighter_2_sig_strike_landed / fighter_2_sig_strike_attempts,
         fighter_2_td_percent = fighter_2_td_landed / fighter_2_td_attempts) %>% 
  # If no attempts than percent will be 0 (may be adjusted to a not attempted variable)
  mutate_at(vars(contains("percent")), .funs = ~if_else(is.nan(.x) == TRUE, 0, .x)) %>% 
  # Convert rounds and round_finished
  mutate(time_format = str_extract(time_format, "\\d")) %>% 
  rename("rounds" = time_format) %>% 
  mutate(rounds  = as.factor(rounds),
         round_finished = as.factor(round_finished)) %>% 
  # Convert method
  separate(method, into = c("method"), sep = "-", extra = "drop") %>% 
  mutate(method = str_trim(method),
         method = as.factor(method)) %>% 
  # Convert weight_class
  mutate(gender = str_extract(weight_class, "Women")) %>% 
  mutate(gender = if_else(is.na(gender) == TRUE, "Men", gender)) %>% 
  mutate(weight_class = str_extract(weight_class, "\\w+weight")) %>% 
  mutate(weight_class = if_else(is.na(weight_class == TRUE), "Catchweight", weight_class)) %>% 
  unite("weight_class", c(gender, weight_class)) %>% 
  mutate(weight_class = as.factor(weight_class)) %>% 
  # Convert referee
  mutate(referee = if_else(is.na(referee) == TRUE, "Missing", referee)) %>% 
  mutate(referee = as.factor(referee)) %>% 
  # Convert rounds
  mutate(rounds = if_else(is.na(rounds) == TRUE, round_finished, rounds)) %>% 
  # Create a fight pk for future feature engineering 
  mutate(row_num = row_number()) %>% 
  arrange(desc(row_num)) %>% 
  mutate(fight_pk = row_number()) %>% 
  arrange(row_num) %>% 
  select(-row_num) 

# Fighter 1 data 
fighter_1 <- df %>% 
  select(-fighter_2_fighter, -referee, -fighter_2_res) %>% 
  rename_all(.funs = ~str_replace(.x, "fighter_1_", "")) %>% 
  # Create defensive metrics 
  mutate("sig_strikes_avoided" = fighter_2_sig_strike_attempts - fighter_2_sig_strike_landed,
         "tds_defended" = fighter_2_td_attempts - fighter_2_td_landed,
         "strikes_avoided" = fighter_2_strike_attempts - fighter_2_strike_landed) %>% 
  # Create damage metrics 
  rename("kds_received" = fighter_2_kd,
         "sig_strikes_received" = fighter_2_sig_strike_landed,
         "strikes_received" = fighter_2_strike_landed,
         "tds_received" = fighter_2_td_landed) %>% 
  mutate(strike_percent = strike_landed / strike_attempts,
         sig_reg_mixture = sig_strike_attempts / strike_attempts,
         sig_reg_percent = (sig_strike_landed + strike_landed) / (sig_strike_attempts + strike_attempts)) %>% 
  # Remove unnecessary columns
  select(-fighter_2_sig_strike_attempts, 
         -fighter_2_sig_strike_percent,
         -fighter_2_strike_attempts,
         -fighter_2_td_attempts,
         -fighter_2_td_percent,
         -fighter_2_sub_attempts,
         -fighter_2_pass,
         -fighter_2_rev) %>% 
  # Sort the columns
  select(sort(current_vars())) 

# Fighter 2 data
fighter_2 <- df %>% 
  select(-fighter_1_fighter, -referee, -fighter_1_res) %>% 
  rename_all(.funs = ~str_replace(.x, "fighter_2_", "")) %>% 
  # Create defensive metrics 
  mutate("sig_strikes_avoided" = fighter_1_sig_strike_attempts - fighter_1_sig_strike_landed,
         "tds_defended" = fighter_1_td_attempts - fighter_1_td_landed,
         "strikes_avoided" = fighter_1_strike_attempts - fighter_1_strike_landed) %>% 
  # Create damage metrics 
  rename("kds_received" = fighter_1_kd,
         "sig_strikes_received" = fighter_1_sig_strike_landed,
         "strikes_received" = fighter_1_strike_landed,
         "tds_received" = fighter_1_td_landed) %>% 
  mutate(strike_percent = strike_landed / strike_attempts,
         sig_reg_mixture = sig_strike_attempts / strike_attempts,
         sig_reg_percent = (sig_strike_landed + strike_landed) / (sig_strike_attempts + strike_attempts)) %>% 
  # Remove unnecessary columns
  select(-fighter_1_sig_strike_attempts, 
         -fighter_1_sig_strike_percent,
         -fighter_1_strike_attempts,
         -fighter_1_td_attempts,
         -fighter_1_td_percent,
         -fighter_1_sub_attempts,
         -fighter_1_pass,
         -fighter_1_rev) %>% 
  # Sort the columns 
  select(sort(current_vars()))

# Combine fighter 1 and fighter 2 data 
fight_data <- bind_rows(fighter_1, fighter_2) %>% arrange(desc(fight_pk))

write_csv(fight_data, here("Data/fight_data.csv"))