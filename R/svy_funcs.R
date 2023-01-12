adjust_sm_vars <- function(df, x_vec,sm_sep){
  sm_parent_child_all<-auto_sm_parent_child(df,sm_sep = sm_sep)
  sm_parent_child_vars<- sm_parent_child_all %>%
    filter(sm_parent %in% x_vec)  
  not_sm<-x_vec[!x_vec %in% sm_parent_child_vars$sm_parent]
  x_vec_adjusted<- c(not_sm, sm_parent_child_vars$sm_child)
  x_vec_adjusted
}


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


svy_collapse_numeric <-  function(df, x,disag){
  x_sym <- sym(x)
  disag_sym <- syms(disag) # for filter w/ across (seems they removed the annoying sym/s warning)
  
  df <- df %>%
    filter(!is.na(!!x_sym)) %>%
    filter(across(c(!!!disag_sym), ~ !is.na(.))) %>%  # remove NA in independent/s.
    svy_group_data(disag=disag)
  
  res <- df %>% 
    summarise(
      mean_pct= srvyr::survey_mean(!!x_sym,vartype = "ci"),
      # `median` = ifelse(all(is.na(!!x_sym)),NA,spatstat.geom::weighted.median(x=!!x_sym,w=!!wt_sym,na.rm=T)),
      `variable` = x,
      # variable_val=unique(!!x_sym,
      n_unweighted= srvyr::unweighted(n()),
      n_weighted =  n(),
      .groups = "drop"
    ) |> 
    dplyr::rename(variable_val=disag)
  return(res)

  
  
}


svy_pct_lgl <-  function(df, x,disag,sm_sep){
  x_sym <- sym(x)
  disag_sym <- syms(disag) # for filter w/ across (seems they removed the annoying sym/s warning)
  
  df <- df %>% 
    filter(!is.na(!!x_sym)) %>%
    filter(across(c(!!!disag_sym), ~ !is.na(.))) %>%  # remove NA in independent/s.
    group_data(disag=disag)
  
  svy_counts <- df %>% 
    group_by(!!x_sym,.add = T,.drop=F) %>% 
    summarise(
      n_unweighted=srvyr::unweighted(n()),
      n_unweighted=n(),
      .groups="drop"
    ) %>% 
    mutate(
      n_unweighted= replace_na(n_unweighted,replace = 0)
    ) %>% 
    filter(!!x_sym==T) 
  
  
  pct_res <-  df %>% 
    summarise(
      `mean_pct` = srvyr::survey_mean(!!x_sym),
      # n_unweighted=n(),
      # n_weighted= sum(!!wt_sym),
      
      .groups="drop"
      
    ) 
  if(!is.null(disag)){
    pct_counts_joined <- pct_res %>% 
      left_join(svy_count, by = disag)  # join without message
  }
  if(is.null(disag)){
    pct_counts_joined <- cbind(pct_res, wtd_counts)
  }

  res
  
}


mutate_key_pair<- function(df, names, values){
  df %>%
    tibble::add_column(!!!set_names(as.list(values),nm=names))
}

svy_collapse_categorical <-  function(df, x,disag,sm_sep="/"){
  x_sym <- sym(x)
  disag_sym <- syms(disag)# for filter w/ across (seems they removed the annoying sym/s warning)
  
  df <- df %>% 
    filter(!is.na(!!x_sym)) %>% 
    filter(if_any(!!!disag_sym, ~ !is.na(.))) %>% # remove NA in independent/s.
    svy_group_data(disag=disag) %>%   
    group_by(!!x_sym,.add = T,.drop=F)
  
  res <- df |>  
    summarise(
      mean_pct= srvyr::survey_mean(na.rm=T),
      n_unweighted=srvyr::unweighted(n()),
      n_weighted=n(),
      .groups = "drop_last"
    ) |> 
    mutate(
      variable= x,
      variable_val=!!x_sym 
    ) 
  
  if(!is.null(disag)){
    res <- make_analysis_col_names(res,disag)
  }
  # res <- res$df %>%
    # mutate(`variable/val`=glue::glue("{variable}{sm_sep}{variable_val}"),.before="subset_1_name") #%>% 
  # # select(-x) %>%
  # select(any_of(c ("variable",
  #                  "variable/val",
  #                  as.character(res$subset_names),
  #                  as.character(res$subset_vals))),
  #        everything())
  return(res)
}


make_analysis_col_names <-  function(df, disag){
  res <- list()
  if(!is.null(disag)){
    subset_names<- glue::glue("subset_{1:length(disag)}_name")
    subset_vals<- glue::glue("subset_{1:length(disag)}_val")
    res<- df %>%
      rename_at(.vars = disag,
                .funs = function(x) glue::glue("subset_{1:length(x)}_val")) %>%
      mutate_key_pair(names =subset_names,values = disag ) %>%
      mutate_at(
        .vars = subset_vals,.funs = function(x)as.character(x)
      ) %>% select(
        any_of(c(
          "variable",
          "variable_val",
          # "variable/val",
          "subset_1_name",
          "subset_1_val",
          "subset_2_name",
          "subset_2_val",
          "subset_3_name",
          "subset_3_val",
          "subset_4_name",
          "subset_4_val",
          
          "subset_5_name",
          "subset_5_val",
          "subset_6_name",
          "subset_6_val",
          "mean_pct",
          "median",
          "n_weighted",
          "n_unweighted"
          
          
        ))
      )
  }
  # if(is.null(disag)){
    # res$subset_names <- "dummy"
    # res$subset_vals <- "dummy"
  # }
  res
  
}




fast_collapse2 <-  function(df, x_vec, disag, wt,sm_sep="/", force_numeric="none",group_name="none",sheet){
  x_vec <- adjust_sm_vars(df, x_vec,sm_sep = sm_sep)
  list_results <- list()
  for(i in 1:length(x_vec)){
    x_temp <- x_vec[i]
    cat(crayon::yellow(x_temp),sep="\n")
    vec_temp <- df[[x_temp]]
    
    if(class(vec_temp) %in% c("numeric","logical","integer")){
      if((!is_binary(vec_temp)|!is.logical(vec_temp))|x_temp %in% force_numeric){
        list_results[[i]] <- fast_collapse_numeric(df = df,x = x_temp,disag = disag,wt = wt) %>% 
          mutate(table_id= paste0(group_name,",",x_temp))
      }
      if((is_binary(vec_temp)|is.logical(vec_temp))& !x_temp %in% force_numeric){ #&
        # (is.null(force_numeric)|!(x_temp%in% force_numeric))){
        df[[x_temp]] <- as.logical(vec_temp)
        list_results[[i]] <- fast_collapse_lgl(df = df,x = x_temp,disag = disag,wt = wt,sm_sep=sm_sep) %>% 
          mutate(table_id= paste0(group_name,",",x_temp))
      }
    }
    if(class(vec_temp)%in% c("character","factor")){
      list_results[[i]] <- fast_collapse_categorical(df = df,x = x_temp,disag = disag,wt = wt,sm_sep = sm_sep) %>% 
        mutate(table_id= paste0(group_name,",",x_temp))
    }
    
  }
  res_df <- bind_rows(list_results) 
  return(res_df)
}



svy_collapse <-  function(df, x_vec, disag,sm_sep){
  x_vec <- adjust_sm_vars(df, x_vec,sm_sep = sm_sep)
  list_results <- list()
  for(i in 1:length(x_vec)){
    x_temp <- x_vec[i]
    cat(crayon::yellow(x_temp),sep="\n")
    vec_temp <- df |> pull(!!sym(x_temp))
    
    if(class(vec_temp) %in% c("numeric","logical","integer")){
      if(all(unique(vec_temp )%in%c(0,1))|is.logical(vec_temp)){ #&
        # (is.null(force_numeric)|!(x_temp%in% force_numeric))){
        df <- df |> 
          mutate(!!x_temp:=as.logical(!!sym(x_temp)))
        list_results[[i]] <- svy_collapse_lgl(df = df,x = x_temp,disag = disag,sm_sep=sm_sep) 
      }else{
        list_results[[i]] <- svy_collapse_numeric(df = df,x = x_temp,disag = disag) 
      }
    }
    if(class(vec_temp)%in% c("character","factor")){
      list_results[[i]] <- svy_collapse_categorical(df = df,x = x_temp,disag = disag,sm_sep = sm_sep)
    }
    
  }
  res_df <- bind_rows(list_results) 
  return(res_df)
}



auto_detect_sm_parents <- function(df, sm_sep="/"){
  sm_parents<-sub(glue::glue('.[^\\{sm_sep}]*$'), '', colnames(df))
  sm_parents<-data.frame(col_names=sm_parents[sm_parents!=""])
  select_multiple_detected<-sm_parents %>%
    group_by(col_names) %>%
    summarise(n=n(), .groups="drop") %>%
    filter(n>1) %>%
    select(col_names)
  return(as.character(select_multiple_detected$col_names))
  
}

auto_sm_parent_child <- function(df, sm_sep="/"){
  sm_parents<-auto_detect_sm_parents(df, sm_sep=sm_sep)
  sm_child<- df %>%
    select(starts_with(glue::glue("{sm_parents}{sm_sep}"))) %>%
    colnames()
  tibble(
    sm_parent=sub(glue::glue('.[^\\{sm_sep}]*$'),'',sm_child),
    sm_child
  )
  
}


