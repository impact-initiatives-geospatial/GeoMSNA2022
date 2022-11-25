weighted_t_test_cus <- function(data,binary_variable,non_binary_variable,strata,survey_weights){
  
  
  data <- data %>% filter(!is.na(data[[binary_variable]]))
  data <- data %>% filter(! is.infinite(data[[binary_variable]]))
  
  
  test_result <- list()
  
  for(i in non_binary_variable){
    
    out_liers_independent_variable <- boxplot.stats(data[[i]])$out
    
    if(length(out_liers_independent_variable) > 0){
      df <- data %>% filter(!data[[i]] %in% out_liers_independent_variable)
    }
    
    if(length(out_liers_independent_variable) == 0){
      df <- data
    }
    
    df <- df %>% filter(!is.na(df[[i]]))
    df <- df %>% filter(! is.infinite(df[[i]]))
    
    dfsvy <- as_survey(df,strata = strata,weights = survey_weights)
    
    
    f_mula <- formula(paste0(i, "~",binary_variable ))
    test <- survey::svyttest(f_mula,design = dfsvy)
    
    
    
    test_result[[i]] <- data.frame(
      binary_variable = binary_variable,
      non_binary_variable = i,
      t_value = test$statistic %>% as.numeric(),
      p_value = test$p.value %>% as.numeric(),
      df = test$parameter %>% as.numeric(),
      difference_in_mean = test$estimate %>% as.numeric(),
      method = test$method
    )
    
    
  }
  
  final_result <- do.call("bind_rows",test_result)
  
  return(final_result)
  
  
}
