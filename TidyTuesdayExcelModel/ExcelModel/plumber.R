#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(tidyverse)
model <- readRDS("excelmodel.rds")
#* @apiTitle Excel Model

#* Classifies penguins (Adelie, Gentoo, Chinstrap)
#* @param island island penguins lives on (Torgersen, Biscoe, and Dream)
#* @param bill_length_mm penguin bill length
#* @param bill_depth_mm penguin bill depth
#* @param flipper_length_mm penguin flipper length
#* @param body_mass_g penguins body mass 
#* @param sex penguin's sex (male, female)
#* @get /predict
function(island, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, sex) {
  
    model_data <- dplyr::tibble(
      island = as.factor(island), 
      bill_length_mm = as.numeric(bill_length_mm), 
      bill_depth_mm = as.numeric(bill_depth_mm), 
      flipper_length_mm = as.numeric(flipper_length_mm), 
      body_mass_g = as.numeric(body_mass_g), 
      sex = as.factor(sex)
    )
    
    predict(model, model_data) %>% 
      purrr::pluck(1)
      
    
}



