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




