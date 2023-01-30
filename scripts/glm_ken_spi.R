library(tidyverse)
library(broom)
library(tidymodels)
library(sf)
library(janitor)
library(xlsf)
conflicted::conflict_prefer("select",winner = "dplyr")
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)

ks <- readxl::read_xlsx(path = file.path(input_dir(env_var = "MSNA2022_DIR",country_code = "ken"), "REACH_KEN_2207_MSNA_2022_HH_tool.xlsx"),sheet = "survey")

kc <- readxl::read_xlsx(file.path(input_dir(env_var = "MSNA2022_DIR",country_code = "ken"), "REACH_KEN_2207_MSNA_2022_HH_tool.xlsx"),"choices")

tool <- xlsf_load(survey = ks,
          choices = kc,
          label = "label",
          sm_sep = "/")

ken<- st_read(file.path(input_dir(env_var = "MSNA2022_DIR",country_code = "ken"),"New_HH_SPI_VCI.gpkg" ),"New_HH_SPI_VCI"         )


tool |> 
  xlsf_query(pattern = "fcs",.col = "xml_name")



ken$encountered_drought_challenges.reduced_sufficient_food |> tabyl()
ken$encountered_drought_challenges.reduced_nutritious_food |> tabyl()
ken$no_driankable_water |> tabyl()
ken_df$water_challenges.no_enought_water_source |> tabyl()

ken_df <- ken_df |> 
  mutate(
    i.drink_water_tf = ifelse(no_driankable_water %in% c("no","rarely"),"good","bad")
  )


plot_log_pred <-  function(df, x="SPI_1", y,y_res){
  
  y_pred = paste0(".pred_",y_res)
  
  logit_model <- logistic_reg() |> 
    set_engine("glm")
  formula_string <- paste0("as_factor(",y,")~",x)
  logit_fit= logit_model |> 
    fit(formula(formula_string),data=df )
  
  print(logit_fit |> tidy())
  print(logit_fit |> glance())
  plots <- list()
  plots$pred_curve <- logit_fit |> 
    augment(df) |> 
    ggplot(aes(x= !!sym(x), y= !!sym(y_pred)))+
    geom_point()
  plots$boxplot <- df |> 
    ggplot(aes(x=as_factor(!!sym(y)),y=(!!sym(x))))+
    geom_boxplot()
  return(plots)
  
  
  
}
wat <- plot_log_pred(df = ken_df, x = "SPI_1",
              y = "encountered_drought_challenges.reduced_nutritious_food",
              y_res = 1)

wat$boxplot
plot_log_pred(df = ken_df, x = "SPI_1",
              y = "no_food",
              y_res = "yes")
plot_log_pred(df = ken_df, x = "SPI_1",
              y = "water_challenges.no_enought_water_source",
              y_res = "1")
plot_log_pred(df = ken_df, x = "SPI_1",
              y = "i.drink_water_tf",
              y_res = "bad")


plot_log_pred(df = ken_df, x = "SPI_1",
              y = "fetching_water_problem",
              y_res = "yes")



lm(FCS~SPI_1,ken_df) |> plot()

ken |> 
  ggplot(aes(x= SPI_1, y= FCS))+
  geom_point()

logit_model <- logistic_reg() |> 
  set_engine("glm")

ken_df <- ken |> 
  st_drop_geometry() 

ken_df |> select(contains('spi')) |> head()

logit_fit= logit_model |> 
  fit(as_factor(encountered_drought_challenges.reduced_nutritious_food)~SPI_1,data=ken_df)

logit_fit |> 
  tidy()



logit_summary <- logit_fit |> tidy()

ggplot(logit_summary, aes(x= estimate, y= term))+
  geom_point() 

logit_fit |> 
  augment(new_data = ken_df )

pred_data = data.frame(SPI6_mamjja2022 = seq(min(ken_df$SPI6_mamjja2022),max(ken_df$SPI6_mamjja2022), by = 0.1))

logit_pred <-  logit_fit |> predict(new_data= pred_data)

logit_pred = logit_pred |> 
  mutate(
    spi_pred = pred_data$SPI6_mamjja2022
  )
logit_pred |> 
  ggplot(aes(x=spi_pred, y= .pred_class))+
  geom_point()








ken_df |> count(water_copi)
ken_df$encounte10

ken_df |> 
  glimpse()

m_food_spi <- glm(formula = no_food_dummy~SPI6_mamjja2022,family = "binomial",data = ken_df) 
m_food_spi |> 
  broom::tidy(exp=T)

plot.dat <- data.frame(fit=predict(m_food_spi,ken_df),  spi=ken_df$SPI6_mamjja2022,no_food= ken_df$no_food_dummy)

plot.dat$fit_prob = exp(plot.dat$fit)/(1+exp(plot.dat$fit))

plot.dat |> 
  ggplot(aes(x=spi,y=))

# install.packages("epiDisplay")
library(epiDisplay)
logistic.display(m_food_spi)


exp(coef(m_food_spi))

m_food_spi_pred<- data.frame(
  preds=predict(m_food_spi),
  y=ken_df$SPI6_mamjja2022
  
  ) |> 
  mutate(idx= row_number())

m_food_spi_pred |> 
  ggplot(aes(x=idx, y= preds))+
  geom_point()
