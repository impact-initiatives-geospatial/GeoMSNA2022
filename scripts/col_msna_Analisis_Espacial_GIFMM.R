# Librerias
library(data.table)
library(stringr)
library(tidyr)
library(dplyr)
library(readxl)
library(summarytools)
library(JmmiColombia)
library(sf)
library(purrr)
library(vcd)

rm(list = ls())
devtools::load_all()

#devtools::install_github("https://github.com/jhoneder1993/jmmi_col/")
# options para que no salga en notacion cientifica
options(scipen = 100, digits = 4)
## Si se queire restablecer utilizar
#options(scipen = 0)

## Cargar todos los datos
data <- read_excel(
  file.path(input_dir(country_code="col"),"REACH_COL_LSGMSNI_08112022.xlsx"),
  
                   guess_max = min(60000, n_max = NULL), sheet = "Sheet1")

#data[["start"]] <- openxlsx::convertToDateTime(data$start)
#data[["end"]] <- openxlsx::convertToDateTime(data$end)
#data[["today"]] <- openxlsx::convertToDateTime(data$today)
#data[["dia"]] <- openxlsx::convertToDateTime(data$dia)data

# Generar el análisis para GIFMM
data <- data %>% filter(!(pop_group %in% c("población de acogida", "PDI")))


survey <- read_excel(
  file.path(input_dir(country_code="col"),"MSNA-6ta ronda- Formulario final_v7.xlsx"),
  sheet = "survey",
  guess_max = min(60000, n_max = NULL)
  )


choices <- read_excel(
  file.path(input_dir(country_code="col"),country_code,"MSNA-6ta ronda- Formulario final_v7.xlsx")m
  sheet = "choices",
  guess_max = min(60000, n_max = NULL)
)

gee <- readRDS(
  file.path(input_dir(country_code="col"),"20221026_col_rs_indicators_with_uuid.rds")
)

# Ver valores de NA en la base de datos
x <- as.data.frame(colSums(is.na(gee)))
x <- x %>% mutate(obser = 5485) %>% 
  mutate(perc_NA = `colSums(is.na(gee))` / obser * 100)

x %>% arrange(desc(perc_NA))

gee <- gee %>% select(-rs_col_lc_2018.nivel_6, -rs_col_lc_2018.nivel_5, -rs_col_lc_2018.nivel_4, 
                      -rs_NO2_column_number_density_mean, -rs_SO2_column_number_density_15km_mean, 
                      -rs_SO2_column_number_density_amf_mean, -rs_SO2_column_number_density_mean, 
                      -rs_cloud_fraction_mean, -rs_cloud_fraction_mean_1, 
                      -rs_tropospheric_NO2_column_number_density_mean, -rs_Npp_2020, -rs_Npp_2021, 
                      -rs_Npp_Z_2020, -rs_Npp_Z_2021, -rs_Npp_mean_2020, -rs_Npp_mean_2021, 
                      -rs_Npp_median_2020, -rs_Npp_median_2021, -rs_Npp_pct_median_2020, -rs_Npp_pct_median_2021)



shp <- st_read(file.path(input_dir(country_code="col"),"shp/REACH_COL_EncuestasMSNA.shp"))
ame <- st_read(file.path(input_dir(country_code="col"),"shp/REACH_COL_SusceptibleInundacion.shp"))

plot(st_geometry(ame))
plot(st_geometry(shp), pch = 16 ,col = "red", add = TRUE)

# Validar que no hayan errores de geometria en los shp
st_is_valid(ame)
st_is_valid(shp)

# Si hay errores validarlos para que se pueda trabajar con los datos
ame <- st_make_valid(ame)

# Spatial Join
inundacion <- st_join(shp, ame, left = TRUE)

# Join atribute
data2 <- data %>% left_join(inundacion, by = c("uuid" = "F_uuid"))
data2 <- data2 %>% select(-geometry)

data <- data2 %>% left_join(gee, by = "uuid")

data <- data %>% mutate(inundacion = case_when(SUSCEPTIBL == "Inundación" ~ "Amenaza por inundación",
                                               TRUE ~ "No hay amenaza por inundación"))

rm(ame, data2, inundacion, shp, x)
# Cuando no tenia los pesos finales
#data[["pesos"]] <- 1

## Ajustes para los numericos

data$msni_lsg_cate <- as.numeric(data$msni_lsg_cate)

#########################
###ANALISIS ESPACIAL ###
#########################

# Variables numericas del archivo gee o de Zack
sort(names(gee))
gee2 <- gee %>% select(-uuid)
variables <- c(names(gee2), "inundacion", "SUSCEPTIBL");variables

sectores <- c("lsg_score_pro", "proteccion", "lsg_score_sa", "segaliment",
              "lsg_score_shelter", "alojamiento", "lsg_score_wash", "aguasanea",
              "lsg_score_edu", "educacion", "lsg_score_salud", "saludlsg",
              "lsg_score_mdv", "mediosvida", "msni", "msni_lsg_cate")


# Dataframe Categorico
anali_cate <- data.frame("variable" = as.character(),
                         "cnt" = as.character(),
                         "cnt_1" = as.character(),
                         "cnt_2" = as.character(),
                         "cnt_3" = as.character(),
                         "cnt_4" = as.character(),
                         "cnt_5" = as.character(),
                         "percent_1" = as.character(),
                         "percent_2" = as.character(),
                         "percent_3" = as.character(),
                         "percent_4" = as.character(),
                         "percent_5" = as.character())

# Dataframe Numerico
anali_nume_acu <- data.frame("variable" = as.character(),
                             "cnt" = as.character(),
                             "sum" = as.character(),
                             "mean" = as.character(),
                             "sd" = as.character(),
                             "min" = as.character(),
                             "q1" = as.character(),
                             "med" = as.character(),
                             "q3" = as.character(),
                             "max" = as.character())


# Generar los calculos
for (i in variables) {
  # i Variablaes
  print(i)
  ## Para variables categoricas
  if (is.character(data[[i]])) {
    for (j in 1:length(sectores)) {
      # Para los impares haga esto o sea sectores1, sectores3, sectores5...
      if (j %% 2 != 0) {
        # j sectores con LSG y LSG_Categorizado
        print(sectores[[j]])
        
        #table(data$admin2, data$lsg_score_pro)
        
        #Calculo de las frecuencias y estadistica basica
        df <- as.data.frame(data %>% group_by(!!sym(i)) %>% 
                              summarize(cnt = n(),
                                        cnt_1 = sum(!!sym(sectores[[j]]) == 1, na.rm = T),
                                        cnt_2 = sum(!!sym(sectores[[j]]) == 2, na.rm = T),
                                        cnt_3 = sum(!!sym(sectores[[j]]) == 3, na.rm = T),
                                        cnt_4 = sum(!!sym(sectores[[j]]) == 4, na.rm = T),
                                        cnt_5 = sum(!!sym(sectores[[j]]) == 5, na.rm = T),
                                        lsg_1p = sum(pesos[!!sym(sectores[[j]]) == 1 & !is.na(!!sym(i))], na.rm = T),
                                        lsg_2p = sum(pesos[!!sym(sectores[[j]]) == 2 & !is.na(!!sym(i))], na.rm = T),
                                        lsg_3p = sum(pesos[!!sym(sectores[[j]]) == 3 & !is.na(!!sym(i))], na.rm = T),
                                        lsg_4p = sum(pesos[!!sym(sectores[[j]]) == 4 & !is.na(!!sym(i))], na.rm = T),
                                        lsg_5p = sum(pesos[!!sym(sectores[[j]]) == 5 & !is.na(!!sym(i))], na.rm = T)) %>% 
                              mutate(percent_1 = as.character(formattable::percent(lsg_1p / sum(lsg_1p))),
                                     percent_2 = as.character(formattable::percent(lsg_2p / sum(lsg_2p))),
                                     percent_3 = as.character(formattable::percent(lsg_3p / sum(lsg_3p))),
                                     percent_4 = as.character(formattable::percent(lsg_4p / sum(lsg_4p))),
                                     percent_5 = as.character(formattable::percent(lsg_5p / sum(lsg_5p)))) %>% 
                              arrange(desc(cnt))) %>% 
          select(-lsg_1p, -lsg_2p, -lsg_3p, -lsg_4p, -lsg_5p);df
        
        # Cambiar el nombre de la primera columna a Variable
        names(df)[1] <- "variable"
        
        # Print el data frame
        print(df)
        
        # Agregar el Name de la variable
        anali_cate <- anali_cate %>% add_row(variable = i,
                                             cnt = sectores[[j]],
                                             cnt_1 = paste(sectores[[j]], "1", sep = "_"),
                                             cnt_2 = paste(sectores[[j]], "2", sep = "_"),
                                             cnt_3 = paste(sectores[[j]], "3", sep = "_"),
                                             cnt_4 = paste(sectores[[j]], "4", sep = "_"),
                                             cnt_5 = paste(sectores[[j]], "5", sep = "_"),
                                             percent_1 = paste("perct",sectores[[j]], "1", sep = "_"),
                                             percent_2 = paste("perct",sectores[[j]], "2", sep = "_"),
                                             percent_3 = paste("perct",sectores[[j]], "3", sep = "_"),
                                             percent_4 = paste("perct",sectores[[j]], "4", sep = "_"),
                                             percent_5 = paste("perct",sectores[[j]], "5", sep = "_"))
        
        # Agregar los nuevos datos a la tabla anali_cate
        anali_cate <- rbind(anali_cate, df)
        
        # Agregar espacios a la tabla
        anali_cate[nrow(anali_cate) + 2 ,]  <- NA
        
        # Para los pares haga esto o sea sectores2, sectores4, sectores6...
      } else if (j %% 2 == 0) {
        # j sectores con LSG y LSG_Categorizado
        print(sectores[[j]])
        
        #Calculo de las frecuencias y estadistica basica
        # Proteccion
        df <- as.data.frame(data %>% group_by(!!sym(i)) %>% 
                              summarize(cnt = n(),
                                        cnt_1 = sum(!!sym(sectores[[j]]) == 1, na.rm = T),
                                        cnt_2 = sum(!!sym(sectores[[j]]) == 0, na.rm = T),
                                        percent_1 = NA,
                                        percent_2 = NA,
                                        cnt_5 = NA,
                                        lsg_1p = sum(pesos[!!sym(sectores[[j]]) == 1 & !is.na(!!sym(i))], na.rm = T),
                                        lsg_2p = sum(pesos[!!sym(sectores[[j]]) == 0 & !is.na(!!sym(i))], na.rm = T),) %>% 
                              mutate(cnt_3 = as.character(formattable::percent(lsg_1p / sum(lsg_1p))),
                                     cnt_4 = as.character(formattable::percent(lsg_2p / sum(lsg_2p))),
                                     percent_3 = NA,
                                     percent_4 = NA,
                                     percent_5 = NA) %>% 
                              arrange(desc(cnt))) %>% 
          select(-lsg_1p, -lsg_2p);df
        
        # Cambiar el nombre de la primera columna a Variable
        names(df)[1] <- "variable"
        
        # Print el data frame
        print(df)
        
        # Agregar el Name de la variable
        anali_cate <- anali_cate %>% add_row(variable = i,
                                             cnt = sectores[[j]],
                                             cnt_1 = paste(sectores[[j]], "conLSG", sep = "_"),
                                             cnt_2 = paste(sectores[[j]], "SinLSG", sep = "_"),
                                             cnt_3 = paste("perct",sectores[[j]], "conLSG", sep = "_"),
                                             cnt_4 = paste("perct",sectores[[j]], "SinLSG", sep = "_"),)
        
        # Agregar los nuevos datos a la tabla anali_cate
        anali_cate <- rbind(anali_cate, df)
        
        # Agregar espacios a la tabla
        anali_cate[nrow(anali_cate) + 2 ,]  <- NA
        
      }
    }
    ## Para variables numericas
  } else if (is.numeric(data[[i]])) {
    # Generar los calculos
    print(i)
    for (j in sectores) {
      # j sectores con LSG y LSG_Categorizado
      print(j)
      
      #Calculo de las frecuencias y estadistica basica
      df <- as.data.frame(data %>% group_by(!!sym(j)) %>% 
                            summarize(cnt = n(),
                                      sum =sum(!!sym(i), na.rm = TRUE),
                                      mean = mean(!!sym(i), na.rm = TRUE),
                                      sd = sd(!!sym(i), na.rm = TRUE),
                                      min = min(!!sym(i), na.rm = TRUE),
                                      q1 = quantile(!!sym(i), probs = 0.25, na.rm = TRUE),
                                      med = median(!!sym(i), na.rm = TRUE),
                                      q3 = quantile(!!sym(i), probs = 0.95, na.rm = TRUE),
                                      max = max(!!sym(i), na.rm = TRUE)) %>%
                            arrange(!!sym(j))); df
      
      # Cambiar el nombre de la primera columna a Variable
      names(df)[1] <- "variable"
      
      # Print el data frame
      print(df)
      
      # Agregar el Name de la variable
      anali_nume_acu <- anali_nume_acu %>% add_row(variable = j,
                                                   cnt = i,
                                                   sum = "sum",
                                                   mean = "mean",
                                                   sd = "sd",
                                                   min = "min",
                                                   q1 = "q1",
                                                   med = "med",
                                                   q3 = "q3",
                                                   max = "max")
      
      # Agregar los nuevos datos a la tabla anali_nume_acu
      anali_nume_acu <- rbind(anali_nume_acu, df)
      
      # Agregar espacios a la tabla
      anali_nume_acu[nrow(anali_nume_acu) + 2 ,]  <- NA
    }
  }
}

# Exportar los dataframe generados
writexl::write_xlsx(list("Analisis_Categorico" = anali_cate,
                         "Analisis_Numerico" = anali_nume_acu),
                    "Output/REACH_COL_MSNA_Analisis_Espacial_GIFMM_16112022.xlsx")







###########################################################
## Correlaciones para cada uno de los sectores

# Variables de predictoras
gee2 <- gee %>% select(-uuid)
variables <- names(gee2);variables


# Variables de respuesta
sectores <- c("lsg_score_pro", "proteccion", "lsg_score_sa", "segaliment",
              "lsg_score_shelter", "alojamiento", "lsg_score_wash", "aguasanea",
              "lsg_score_edu", "educacion", "lsg_score_salud", "saludlsg",
              "lsg_score_mdv", "mediosvida", "msni", "msni_lsg_cate")


# Dataframe de correlaciones
correla <- data.frame("var_resp" = as.character(),
                      "var_predic1" = as.character(),
                      "var_predic2" = as.character(),
                      "var_predic3" = as.character(),
                      "var_predic4" = as.character(),
                      "var_predic5" = as.character())

#i <- "msni"
#i <- "lsg_score_pro"

for (i in sectores) {
  print(i)
  # Variables de gee
  modelos <- data[,names(data) %in% c(variables, i)]
  modelos<- modelos %>% select_if(is.numeric)
  # Variables de todo el data frame
  #modelos <- data %>% select_if(is.numeric)
  
  #prueba <- cor.test(data$lsg_score_pro, data$rs_rx10d_30d_may);prueba
  #cor.test(lsg_score_pro, y = modelos[[i]])
  #prueba <- cor(data$lsg_score_pro, data$rs_rx10d_30d_may);prueba
  #cor(modelos, y = modelos[[i]], use = "complete.obs")
  #cor(modelos, y = modelos[[i]], use = "na.or.complete")
  #cor(modelos, y = modelos[[i]], use = "pairwise.complete.obs")
  
  #prueba$estimate
  
  best_pred <- modelos %>%
    select(-i) %>%
    map_dbl(cor, y = modelos[[i]], use = "pairwise.complete.obs") %>%
    abs() %>%
    sort(decreasing = TRUE) %>%
    .[1:5]; best_pred
  
  
  # Dataframe de correlaciones preliminares
  correla_pre <- data.frame("var_resp" = as.character(),
                            "var_predic1" = as.character(),
                            "var_predic2" = as.character(),
                            "var_predic3" = as.character(),
                            "var_predic4" = as.character(),
                            "var_predic5" = as.character())
  
  # Agregar el Name de la variable
  correla_pre <- data.frame("var_resp" = "Variable de respuesta",
                            "var_predic1" = names(best_pred[1]),
                            "var_predic2" = names(best_pred[2]),
                            "var_predic3" = names(best_pred[3]),
                            "var_predic4" = names(best_pred[4]),
                            "var_predic5" = names(best_pred[5]))
  
  correla_pre <- correla_pre %>% add_row("var_resp" = i,
                                         "var_predic1" = paste(as.character(round(best_pred[1] * 100, 2)), "%", sep = ""),
                                         "var_predic2" = paste(as.character(round(best_pred[2] * 100, 2)), "%", sep = ""),
                                         "var_predic3" = paste(as.character(round(best_pred[3] * 100, 2)), "%", sep = ""),
                                         "var_predic4" = paste(as.character(round(best_pred[4] * 100, 2)), "%", sep = ""),
                                         "var_predic5" = paste(as.character(round(best_pred[5] * 100, 2)), "%", sep = ""))
  
  # Agregar los nuevos datos a la tabla anali_nume_acu
  correla <- rbind(correla, correla_pre)
  
  # Agregar espacios a la tabla
  correla[nrow(correla) + 2 ,]  <- NA
  
}

# Exportar los dataframe generados
writexl::write_xlsx(list("Correlaciones" = correla),
                    "Output/REACH_COL_MSNA_Analisis_EspacialCorrelaciones_GIFMM_16112022.xlsx")


####################################################
# CORRELACIONES CATEGORICAS
sectores <- c("proteccion",  "segaliment", "alojamiento", "aguasanea",
              "educacion", "saludlsg", "mediosvida", "msni_lsg_cate")

variables <- c("inundacion", "rs_alos_landforms", "rs_srtm_landforms", "rs_col_lc_2018.leyenda", 
               "rs_col_lc_2018.nivel_1", "rs_col_lc_2018.nivel_2", "rs_col_lc_2018.nivel_3", 
               "rs_col_erosion.tipo", "rs_col_erosion.clase", "rs_col_erosion.grado", 
               "rs_col_erosion.base_carto", "rs_col_erosion.zonificaci", 
               "rs_col_lulc_2017.ucvocacion", "rs_col_lulc_2017.vocacion", 
               "rs_col_lulc_2017.uso_princi", "inundacion")



modelos <- data[,names(data) %in% c(variables, sectores)]


for (i in sectores) {
  #print(i)
  for (j in variables) {
    #print(j)
    tabla <- table(modelos[[i]], modelos[[j]]);tabla
    x <- assocstats(x = tabla)
    if (replace_na(x$cramer, 0) > 0.25) {
      print(paste(i, " VS ", j, sep = ""))
      print(x)
      cat("\n")
      cat("\n")
    }
  }
}


mod <- lm(aguasanea ~ rs_col_lulc_2017.uso_princi, data = modelos)
summary(mod)


mod <- lm(msni ~ rs_dist_coast, data = data)
summary(mod)



for (i in sectores) {
  print(i)
  for (j in variables) {
    print(j)
    mod <- lm(modelos[[i]] ~ modelos[[j]], data = modelos)
    mod <- summary(mod)
    r <- mod$adj.r.squared
    p <- mod$coefficients[1,4]
    print(paste("el sector ", i, "vs la variable ", j, "tiene:", sep = ""))
    print(paste("R2: ", r, "    p.value: ", p, sep = ""))
    cat("\n")
  }
}



