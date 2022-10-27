
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GeoMSNA2022

<!-- badges: start -->
<!-- badges: end -->

The goal of GeoMSNA2022 is to facilitate geospatial and remote sensing
analyses of selected global `MSNA`s.

Prior to the creation of this repository various RS variables have been
extracted for 6 MSNA data sets (SOM, IRQ, COL, NER, NGA, and HTI). The
extraction process is currently performed and documented in the
`surveyGEER` package repository. Now that the variables have been
extracted we aim to explore relationships between
environmental/climatic/geospatial phenomena and household conditions.

It is recommended the country-offices with R-capacity use this
repository to host this stage of analysis.

## Recommendations

-   We recommend starting with Exploratory Data Analysis (EDA) using
    rmarkdown notebooks. You can simply run `usethis::use_vignettes()`
    to automatically create a markdown notebook
-   Once exploratory data analysis has been performed the analyst should
    be sure to use weights for final analysis

## First Steps

You will need to decide on the variables in the HH data set that should
be analyzed though a geospatial/climatic/environmental lens. Once you
have done this you will find a rudimentary function called
`load_core_hh_indicators.R` in the `R/` directory of this project. You
can then add them to the function following the lead the `irq` team has
provided. If you have the same variables as another country make sure
you copy there label to use in your list.

``` r
load_core_hh_indicators <-  function(input_df, country_code="irq"){
  if (country_code=="irq"){
    res <- list(
      # labels on left - column names on right
      `Food Consumption Score (numeric)`= "fcs",
      `Food Consumption Score (categorical)`= "fcs_category",
      `Household Hunger Scale (categorical)` = "household_hunger_scale",
      `main source of food (categorical)` = "food_source",
      `HHH unemployed (categorical)` = "unemployed_seek_work",
      `HH debt (numeric)` = "how_much_debt",
      `debt per member > 90k (categorical)` = "g37"
      
    )
    
  }
  if(country_code=="som"){
    list(
      
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
```
