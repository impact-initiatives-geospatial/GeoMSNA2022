#' fetch_msna - helper function to quickly pull msna coordinate files
#'
#' @param country_code \code{character} three letter country code (lower-case)
#' @description return msna data based on suplied country code
#' @return sf data.frame containing msna records
#' @export
#'
#' @examples \dontrun{
#' library(surveyGEER)
#' df <-  fetch_msna("com")
#' }

fetch_msna <- function(country_code="irq"){
  msna_path<- here::here("data")
  msna_files <- list.files(msna_path)
  file_rgx<- glue::glue("{country_code}_clean_.+csv$")
  foi_name<- stringr::str_subset(string = msna_files,pattern = file_rgx)
  assertthat::assert_that(length(foi_name)>0,msg="no file found")
  foi<- glue::glue("{msna_path}/{foi_name}")
  readr::read_csv(foi) |>
    dplyr::mutate(
      country_code=country_code
    )
}

#' load_hh_data
#'
#' @param country_code 
#'
#' @return data.frame containing hh data
#' @export
#'
#' @examples \dontrun{
#' library(GeoMSNAs2022)
#' load_hh_data("nga")
#' }

load_hh_data <-  function(country_code){
  if(country_code=="nga"){
    root_dir <-   Sys.getenv("NGA_MSNA2022")
    dat_fp <- file.path(root_dir, "20221107_msna_with_rs_nga.rds")
    res <- readr::read_rds(dat_fp)  
  }
  if(country_code=="irq"){
    root_dir <-   Sys.getenv("IRQ_MSNA2022")
    dat_fp <- file.path(root_dir, "irq_msna_clean_data_with_rs_indicators.csv")
    res <- readr::read_csv(dat_fp)  
  }
  return(res)
  
}




