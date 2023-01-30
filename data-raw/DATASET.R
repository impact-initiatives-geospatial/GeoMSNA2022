## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)


# pulling a fast one to get the rs indicators in full nga data set.

nga_dat_orig <- read_rds(file.path(input_dir(env_var = "MSNA2022_DIR",country_code = "nga"), "20221107_msna_with_rs_nga.rds")) |> 
  select(-starts_with("rs_"))

nga_just_rs <- read_rds(file.path(input_dir(env_var = "MSNA2022_DIR",country_code = "nga"), 
                                  "20230130_msna_with_rs_nga.rds"))

nga_dat_orig |> 
  left_join(nga_just_rs) |> 
  write_rds(file.path(input_dir(env_var = "MSNA2022_DIR",country_code = "nga"), 
                      "20230130_msna_with_rs_nga.rds"))
