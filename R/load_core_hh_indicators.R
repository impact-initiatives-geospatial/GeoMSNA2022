#' load_core_hh_indicators
#'
#' @param input_df The fetched data frame using the `fetch_msna` function
#' @param country_code The country code
#'
#' @return named list containing variable column names and labels

load_core_hh_indicators <-  function(input_df, country_code="irq"){
  if (country_code=="irq"){
    res <- list(
      `Food Consumption Score (numeric)`= "fcs",
      `Food Consumption Score (categorical)`= "fcs_category",
      `Household Hunger Scale (categorical)` = "household_hunger_scale",
      `main source of food (categorical)` = "food_source",
      `HHH unemployed (categorical)` = "head_seek_work",
      `HH debt (numeric)` = "how_much_debt",
      `debt per member > 90k (categorical)` = "g37",
      `HH income (numeric)`= "inc_employment_pension", ## income from employment and/or pension
      `Medical expense` = "medical_exp",
      `Health issues chronic` = "health_issue_chronic"
    
      
    )
    
  }
  if(country_code=="som"){
    res <- list(
      `Respondent Gender` = "respondent_gender",
      `Respondent Age` = "respondent_age",
      `Region` = "region",
      `District` = "district",
      `Reside in an IDP settlement` = "idp_settlement",
      `Village/settlement/IDP site` = "settlements",
      `Household Size` = "hh_size",
      `Household's total cash income from all income sources` = "total_house_income",
      `If yes, what is yoour household's current total amount of debt in USD` = "total_hh_debt",
      `Drinking` = "drinking_water",
      `Cooking` = "cooking_water",
      `Personal hygiene (washing or bathing)` = "hygiene_water",
      `Other domestic purposes (cleaning house, floor, etc.)` = "domestice_water",
      `FSL shocks Unusually high food prices` = "hh_fsl_shocks/high_food_prices",
      `FSL shocks Drought/irregular rains, prolonged dry spell` = "hh_fsl_shocks/drought",
      `FSL shocks Unusually high level of crop pests and disease` = "hh_fsl_shocks/crop_disease",
      `FSL shocks Disease outbreak in the settlement` = "hh_fsl_shocks/disease_outbreak",
      `FSL shocks Too much rain, flooding` = "hh_fsl_shocks/flooding",
      `FSL shocks Livestock disease outbreak` = "hh_fsl_shocks/livestock_disease",
      `No food to eat of any kind in your house because of lack of resources to get food` = "hh_no_food",
      `How often did this happen in the past [4 weeks/30 days]` = "hh_no_food_freq",
      `Go to sleep at night hungry because there was not enough food` = "hh_hunger",
      `How often did this happen in the past [4 weeks/30 days]` = "hh_hunger_freq",
      `Go a whole day and night without eating anything at all because there was not enough food` = "fs_not_enough_food",
      `Livestock decrease Disease outbreak` = "reason_livestock_decrease/disease_outbreak",
      `Livestock decrease Flooding` = "reason_livestock_decrease/flooding_flooding",
      `Livestock decrease Drought` = "reason_livestock_decrease/drought_drought",
      `Recent displacement Flooding (riverine and flash flood)` = "factors_recent_displacement/flooding",
      `Recent displacement Drought (lack of food, water, livestock loss)` = "factors_recent_displacement/drought",
      `Recent displacement Desert locust invasion` = "factors_recent_displacement/desert_locust",
      `Have access to your current shelter in the next 6 months` = "shelter_access",
      `Latitude` = "Lat",
      `Longitude` = "Lon"
    )
  }
  if(country_code=="nga"){
    res <- list(
      `Flooding Incident` = "nature_safety_incident/flooding",
      `Livestock decrease (rain)` = "hoh_livestock_decrease/drought"
    )
    
  }
  
  # select the given columns on the fetched msna dataset
  df_msna_cols <- input_df |>
    select(any_of(purrr::map_chr(res, ~.x))) |>
    mutate(country_code = country_code)
  
  return(df_msna_cols)
}


