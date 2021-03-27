library(shiny)
library(DT)
library(plotly)
library(shinydashboard)
library(here)
load(here("Models/kd_model.RDS"))
load(here("Models/sig_strike_model.RDS"))
load(here("Models/strike_model.RDS"))
load(here("Models/sub_model.RDS"))
load(here("Models/td_model.RDS"))
load(here("Models/outcome_model.RDS"))

outcome_ui <- function(id){
  box(plotlyOutput(NS(id, "outcome_plot")))
}
outcome_server <- function(id, df){
  moduleServer(id, function(input, output, session){
    output$outcome_plot <- renderPlotly(
      
      hide_legend(
      ggplotly(
      df %>% 
        filter(res == "W") %>% 
        count(method) %>% 
        mutate(method = fct_reorder(method, n)) %>% 
        ggplot(aes(x = method , y = n, color = method)) + 
        geom_segment(aes(xend = method, yend = 0)) + 
        geom_point(size = 5) + 
        coord_flip() + 
        theme(legend.position = "none")))
    )
  })
}
elo_ui <- function(id){
  box(plotlyOutput(NS(id, "elo_plot")))
}
elo_server <- function(id, df){
  moduleServer(id, function(input, output, session){
    output$elo_plot <- renderPlotly(
      hide_legend(
      ggplotly(
      df %>% 
        mutate(weight_class = str_replace(weight_class, "_", " "),
               weight_class = factor(weight_class, 
                                     levels = c("Women Strawweight", "Women Flyweight", "Women Bantamweight", "Women Featherweight",
                                                "Men Flyweight", "Men Bantamweight", "Men Featherweight", "Men Lightweight",
                                                "Men Welterweight", "Men Middleweight", "Men Heavyweight", "Men Catchweight"))) %>% 
        ggplot(aes(x = weight_class, y = elo, fill = weight_class, labels = fighter)) + 
        geom_point(data = df %>% 
                    group_by(weight_class) %>% filter(elo == max(elo)) %>% 
                    ungroup() %>% mutate(weight_class = str_replace(weight_class, "_", " ")),
                  aes(x = weight_class, y = elo, label = fighter),
                  hjust = -0.1, size = 3) + 
        geom_boxplot(show.legend = FALSE) + 
        coord_flip() + labs(x = "") + ylim(c(1400, 1800))))
    )
  })
}



component_ui <- function(id){
  box(plotOutput(NS(id, "component_plot")))
}
component_server <- function(id, df){
  moduleServer(id, function(input, output, session){
    output$component_plot <- renderPlot(
      df %>% 
        mutate(weight_class = str_replace(weight_class, "_", " "),
               weight_class = factor(weight_class, 
                                     levels = c("Women Strawweight", "Women Flyweight", "Women Bantamweight", "Women Featherweight",
                                                "Men Flyweight", "Men Bantamweight", "Men Featherweight", "Men Lightweight",
                                                "Men Welterweight", "Men Middleweight", "Men Heavyweight", "Men Catchweight"))) %>% 
        select(fight_pk, fighter, weight_class, 
               kd, sig_strike_landed, strike_landed, 
               sub_attempts, td_landed) %>% 
        group_by(fighter, weight_class) %>% 
        filter(fight_pk == max(fight_pk)) %>% 
        ungroup() %>% 
        pivot_longer(kd:td_landed) %>% 
        group_by(name) %>% 
        mutate(value = percent_rank(value)) %>% 
        ungroup() %>% 
        mutate(name = str_replace_all(name, "_", " ") %>% str_to_title()) %>% 
        ggplot(aes(x = value, y = weight_class, fill = weight_class)) + 
        geom_density_ridges(quantile_lines = TRUE, quantiles = c(0.25, 0.975), color = "white",
                            from = 0, to = 1, alpha = 0.8) + 
        scale_x_continuous(labels = scales::label_percent()) + 
        labs(y = "", x = "Percentile")  + 
        theme(legend.position = "none") + 
        facet_wrap(~name, scales = "fixed")
    )
  })
}
best_ui <- function(id){
  box(plotOutput(NS(id, "best_plot")))
}
best_server <- function(id, df){
  moduleServer(id, function(input, output, session){
    output$best_plot <- renderPlot(
      df %>% 
        select(fighter, weight_class, wins = res, kd, sig_strike_landed, strike_landed, sub_attempts, td_landed) %>% 
        mutate(wins = if_else(wins == "W", 1, 0)) %>% 
        group_by(fighter, weight_class) %>% 
        summarise(across(wins:td_landed, sum)) %>% 
        ungroup() %>% 
        pivot_longer(-c(fighter, weight_class)) %>% 
        group_by(name) %>% 
        top_n(value, n = 5) %>% 
        ungroup() %>% 
        mutate(name = paste0("Total ", name) %>% str_replace_all("_", " ") %>% str_to_title(),
               fighter = tidytext::reorder_within(fighter, value, name)) %>% 
        ggplot(aes(x = fighter, y = value, fill = fighter, label = fighter)) + 
        geom_col(show.legend = FALSE) + 
        tidytext::scale_x_reordered() + 
        coord_flip() + 
        facet_wrap(~name, scales = "free")
    )
  })
}
card_prediction_ui <- function(id){
  dataTableOutput(NS(id, "card_predictions_table"))
}
card_prediction_server <- function(id, df){
  moduleServer(id, function(input, output, session){
    output$card_predictions_table <- renderDataTable({
      DT::datatable(df %>% 
                      rename("fighter_name" = fighter,
                             "opponent_name" = opponent) %>% 
                      mutate_all(as.character) %>% 
                      mutate(fight = paste0(fighter_name, " vs. ", opponent_name)) %>% 
                      pivot_longer(-c(card, fight)) %>% 
                      separate(name, c("type", "feature"), sep = "_", extra = "merge") %>% 
                      mutate(feature = str_replace_all(feature, "opp_", "")) %>% 
                      pivot_wider(names_from = feature, values_from = value) %>% 
                      select(-type) %>% 
                      mutate(across(-c(card, fight, name), .fns = ~as.numeric(.x) %>% round(digits = 2))) %>% 
                      rename_all(~str_replace_all(.x , "_", " ") %>% str_to_title()),
                    filter = "top",
                    options = list(autoWidth = TRUE, pageLength = 5))
    })
  })
}


card_probabilities_ui <- function(id){
  plotlyOutput(NS(id, "card_probabilities"))
}
card_probabilities_server <- function(id, df){
  moduleServer(id, function(input, output, session){
    output$card_probabilities <- renderPlotly({
      ggplotly(
        df %>% 
          select(card, fighter, opponent, fighter_lower, fighter_win, fighter_upper, opponent_lower, opponent_win, opponent_upper) %>% 
          mutate(fight = paste0(fighter, " vs ", opponent)) %>% 
          ggplot() + 
          geom_pointrange(aes(x = fight, y = fighter_win, ymin = fighter_lower, ymax = fighter_upper, 
                              label = fighter), color = "blue") + 
          geom_pointrange(aes(x = fight, y = opponent_win, ymin = opponent_lower, ymax = opponent_upper,
                              label = opponent), color = "red") + 
          coord_flip() + 
          scale_y_continuous(labels = scales::label_percent()) + 
          labs(x = "", y = "Win Probability", color = "Type") + 
          theme(legend.position = "top")
      )
    })
  })
}

get_fighter_components <- function(df){
  tibble(
    predict(kd_model, new_data = df) %>% rename("kd" = 1),
    predict(sig_strike_model, new_data = df) %>% rename("sig_strike_landed" = 1),
    predict(strike_model, new_data = df) %>% rename("strike_landed" = 1),
    predict(sub_model, new_data = df) %>% rename("sub_attempts" = 1),
    predict(td_model, new_data = df) %>% rename("td_landed" = 1)
  )
}

get_opponent_components <- function(df){
  tibble(
    predict(kd_model, df %>% 
              rename_all(~paste0("opp_", .x) %>% 
                           str_replace("opp_opp_", "")) %>% 
              rename("fight_pk" = "opp_fight_pk")) %>% rename("opp_kd" = 1),
    predict(sig_strike_model, df %>% 
              rename_all(~paste0("opp_", .x) %>% 
                           str_replace("opp_opp_", "")) %>% 
              rename("fight_pk" = "opp_fight_pk")) %>% rename("opp_sig_strike_landed" = 1),
    predict(strike_model, df %>% 
              rename_all(~paste0("opp_", .x) %>% 
                           str_replace("opp_opp_", "")) %>% 
              rename("fight_pk" = "opp_fight_pk")) %>% rename("opp_strike_landed" = 1),
    predict(sub_model, df %>% 
              rename_all(~paste0("opp_", .x) %>% 
                           str_replace("opp_opp_", "")) %>% 
              rename("fight_pk" = "opp_fight_pk")) %>% rename("opp_sub_attempts" = 1),
    predict(td_model, df %>% 
              rename_all(~paste0("opp_", .x) %>% 
                           str_replace("opp_opp_", "")) %>% 
              rename("fight_pk" = "opp_fight_pk")) %>% rename("opp_td_landed" = 1)
  )
}

get_fight_outcome <- function(model, fighter, opponent){
  bind_rows(
    predict(model, bind_cols(fighter, opponent), type = "prob") %>% 
      rename("L" = 1, "W" = 2),
    predict(model, bind_cols(fighter, opponent) %>% 
              rename_all(~paste0("opp_", .x) %>% str_replace_all("opp_opp_", "")), type = "prob") %>% 
      rename("W" = 1, "L" = 2)
  ) %>% 
    pivot_longer(everything()) %>% 
    group_by(name) %>% 
    summarise(lower = min(value),
              avg = mean(value),
              upper = max(value)) %>% 
    ungroup() 
}