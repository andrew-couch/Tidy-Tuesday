clean_weight_class <- . %>% 
  select(weight_class) %>% 
  distinct() %>% 
  arrange(weight_class)

clean_fighter <- . %>% 
  select(fighter) %>% 
  distinct() %>% 
  arrange(fighter)

create_elo_data <- function(k){
  temp_df <- elo.run(winner ~ fighter + opponent, k = k, 
                     data = elo_df %>% arrange(fighter , date)) %>% 
    as_tibble() %>% 
    cbind(elo_df %>% arrange(fighter, date) %>% select(match_id)) %>% 
    select(team.A, team.B, elo.A, elo.B, match_id)
  
  rbind(temp_df %>% 
          select_at(vars(contains(".A"), contains("match_id"))) %>% 
          rename_all(.funs = function(x) str_replace(x, ".A", "")),
        temp_df %>% 
          select_at(vars(contains(".B"), contains("match_id"))) %>% 
          rename_all(.funs = function(x) str_replace(x, ".B", ""))) %>% 
    rename("fighter" = "team") %>% 
    left_join(df %>% 
                select(fighter, date, weight_class, match_id),
              by = c("fighter", "match_id")) %>% 
    mutate(date = as.Date(date))
}
elo_df <- read.csv("elo_df.csv")
df <- read.csv("elo.csv")