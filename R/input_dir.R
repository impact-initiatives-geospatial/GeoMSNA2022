#' input_dir
#'
#' @param env_var 
#' @param country_code \code{character} 3 letter country code (i.e col)
#' @details get input directory path for MSNA 2022 shared data
#' @return
#' @export
#'
#' @examples

input_dir <-  function(env_var= "MSNA2022_DIR" ,country_code){
  # if(country_code=="col"){
    res_fp <- file.path(Sys.getenv(env_var),country_code,"inputs")
  # }
  return(res_fp)
  
}

#' output_dir
#'
#' @param env_var 
#' @param country_code \code{character} 3 letter country code (i.e col)
#' @details get output  directory path for MSNA 2022 shared data
#' @return
#' @export
#'
#' @examples
output_dir <-  function(env_var= "MSNA2022_DIR" ,country_code){
  if(country_code=="col"){
    res_fp <- file.path(Sys.getenv(env_var),country_code,"outputs")
  }
  return(res_fp)
  
}




#' load_hh_data
#' @param env_var 
#' @param country_code 
#'
#' @return data.frame containing hh data
#' @export
#'
#' @examples \dontrun{
#' library(GeoMSNAs2022)
#' load_hh_data("nga")
#' }

load_hh_data <-  function(env_var= "MSNA2022_DIR" ,country_code){
  if(country_code=="nga"){
    dat_fp <- file.path(input_dir(env_var = "MSNA2022_DIR",country_code = country_code), "20221107_msna_with_rs_nga.rds")
    res <- readr::read_rds(dat_fp)  
  }
  if(country_code=="irq"){
    dat_fp <- file.path(input_dir(env_var = "MSNA2022_DIR",country_code = country_code), "irq_msna_clean_data_with_rs_indicators.csv")
    
    res <- readr::read_csv(dat_fp)  
  }
  return(res)
  
}
