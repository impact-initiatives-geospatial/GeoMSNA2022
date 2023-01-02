svy_group_data <- function(df,disag){
  if(length(disag)==1){
    disag_syms<-sym(disag)
    df_grouped<-df %>%
      group_by(!!disag_syms,.drop=FALSE)
  }
  if(length(disag)>1){
    disag_syms <- syms(disag)
    df_grouped<-df %>%
      group_by(!!!disag_syms,.drop=FALSE)
  }
  if(is.null(disag)){
    df_grouped <- df
  }
  return(df_grouped)
}

svy_pct <-  function(df,disag){
  df_grp <-  svy_group_data(df,disag)

    
}


svy_pct_lgl <-  function(df, x,disag, wt,sm_sep){
  x_sym <- sym(x)
  wt_sym <- sym(wt)
  disag_sym <- syms(disag) # for filter w/ across (seems they removed the annoying sym/s warning)
  
  df <- df %>% 
    filter(!is.na(!!x_sym)) %>%
    filter(across(c(!!!disag_sym), ~ !is.na(.))) %>%  # remove NA in independent/s.
    group_data(disag=disag)
  
  svy_counts <- df$variables %>% 
    group_by(!!x_sym,.add = T,.drop=F) %>% 
    summarise(
      n_unweighted=n(),
      .groups="drop"
    ) %>% 
    mutate(
      n_unweighted= replace_na(n_unweighted,replace = 0)
    ) %>% 
    filter(!!x_sym==T) 
  
  
  pct_res <-  df %>% 
    summarise(
      `mean/pct` = weighted.mean(!!x_sym,!!wt_sym),
      # n_unweighted=n(),
      # n_weighted= sum(!!wt_sym),
      
      .groups="drop"
      
    ) 
  if(!is.null(disag)){
    pct_counts_joined <- pct_res %>% 
      left_join(wtd_counts, by = disag)  # join without message
  }
  if(is.null(disag)){
    pct_counts_joined <- cbind(pct_res, wtd_counts)
  }
  res <- pct_counts_joined |> 
    dplyr::mutate(
      variable=str_replace_all(string = x,pattern = "(.+?)(/.*)","\\1"),
      variable_val = str_replace_all(string = x,pattern = "(.+/)(.*)","\\2"),
      
      n_unweighted= if_else(`mean/pct`==0 & is.na(n_unweighted),0,n_unweighted),
      n_weighted= if_else(`mean/pct`==0 & is.na(n_weighted),0,n_weighted)
      
    )

  res
  
}
