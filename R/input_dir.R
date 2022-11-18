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
  if(country_code=="col"){
    res_fp <- file.path(Sys.getenv(env_var),country_code,"inputs")
  }
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