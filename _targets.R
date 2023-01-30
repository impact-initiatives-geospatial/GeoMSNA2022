# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint
library(tidyverse)
# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(

  # Track Key files ---------------------------------------------------------
  # anything that might change
  tar_target(
    name = nga_msna_filepath,
    command = file.path(input_dir(env_var = "MSNA2022_DIR",country_code = "nga"), "20230130_msna_with_rs_nga.rds"),
    format = "file"
#   format = "feather" # efficient storage of large data frames # nolint
  ),
  tar_target(
    name = nga_tool_filepath,
    command = file.path(input_dir(env_var = "MSNA2022_DIR",country_code = "nga"), "nga_msna_2022_survey_choices.xlsx"),
    format = "file"
#   format = "feather" # efficient storage of large data frames # nolint
  ),
tar_target(nga_msna_data,
           command= read_rds(nga_msna_filepath)
           ),
tar_target(
  name=nga_svy,
  command= load_relevel_svy(data = nga_msna_data,
                            tool_path = nga_tool_filepath,
                            survey_name = "survey",
                            choices_name = "choices",
                            fct_relevel_skip = "ward_of_origin",
                            weights_col ="weight_both"
                            )
  ),
  tar_render(multi_country_overview_report,"vignettes/af-multi_country_overview.Rmd")

# 
#   # Load HH data with questionnaires & Relevel ------------------------------
#   tar_target(
#     name = dats,
#     command = c("nga","col","som") |>
#       purrr::map(
#         ~load_and_relevel(.x)  
#       )
#   ),
#  # produce overiew map?
#  tar_target(
#    name= overview_map,
#    commonad= multicountry_overview_map(dat)
#  ),

)
