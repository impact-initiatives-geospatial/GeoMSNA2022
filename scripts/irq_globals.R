# rm(list = ls())

library(tidyverse)
library(ggpmisc)
library(illuminate)
library(ggpubr)
library(srvyr)
library(survey)
library(oddsratio)

options(scipen = 999)


# source("functions.R")

cols_to_check <- c("fcs","fcs_category","household_hunger_scale","hungry_score","food_source","head_seek_work","how_much_debt",
                   "inc_employment_pension", "medical_exp","health_issue_chronic")


numeric_cols <- c("fcs","rs_VCI_Apr2022","household_hunger_scale","how_much_debt","rs_NDVI_Mar2022","rs_NDVI_Apr2022",
                  "rs_NDVI_May2022","Distance","rs_avg_dist_perm_water_pix_20172020","rs_healthcare_accessbility_walking_only2019",
                  "inc_employment_pension", "medical_exp","rs_city_accessibility2015")




################### reseponse rate ###########################################

response_rate <- get_na_response_rates(data[cols_to_check]) %>% mutate(
  `Number of response (number)` = (12839 - num_non_response),
  `Number of response (%)` = ((12839 - num_non_response)/12839)*100
)



response_rate_plot <- ggplot(response_rate,ylab = "",
                             aes(x =  `Number of response (%)`,
                                 xend = 0,
                                 y = question,
                                 yend = question,
                                 color ="red" )) +
  
  theme(legend.position="none",
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14))+
  geom_segment() +
  geom_point() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))+
  geom_text(aes(label=`Number of response (number)`), vjust=1.5, color="black", size=4.5)+ylab("")

# ,hjust= -.3

################################# Histograms ####################################################

histogram <- list()

for(i in numeric_cols){
  
  mean_i <-  mean(data[[i]],na.rm = T)
  sd_i <- sd(data[[i]],na.rm = T)
  
  normal <- ggplot(data, aes_string(x=names(data)[names(data) %in% i])) + 
    geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
    geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)+
    geom_vline(aes(xintercept = mean_i), color = "#000000", size = 1.25) +
    geom_vline(aes(xintercept =mean_i + sd_i), color = "#000000", size = 1, linetype = "dashed") +
    geom_vline(aes(xintercept = mean_i - sd_i), color = "#000000", size = 1, linetype = "dashed") +
    xlab(paste0(i,"( Normal distribution)")) +  
    theme(legend.position="none",
          panel.background = element_rect(fill = "white",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_blank(),
          axis.text.y = element_blank())+ylab("")
  
  
  data_log <- data
  data_log[[i]] <- log(data_log[[i]])
  
  mean_i_log <-  mean(data_log[[i]],na.rm = T)
  sd_i_log <- sd(data_log[[i]],na.rm = T)
  
  
  log <- ggplot(data_log, aes_string(x=names(data_log)[names(data_log) %in% i])) + 
    geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
    geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)+
    geom_vline(aes(xintercept = mean_i_log), color = "#000000", size = 1.25) +
    geom_vline(aes(xintercept =mean_i_log + sd_i_log), color = "#000000", size = 1, linetype = "dashed") +
    geom_vline(aes(xintercept = mean_i_log - sd_i_log), color = "#000000", size = 1, linetype = "dashed") +
    xlab(paste0(i,"( Log distribution)")) +  
    theme(legend.position="none",
          panel.background = element_rect(fill = "white",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_blank(),
          axis.text.y = element_blank())+ylab("")
  
  
  
  histogram[[i]] <- ggarrange(normal, log)
  
  
}



data <- data %>% mutate(
  ndvi_may_2022_log = log(rs_NDVI_May2022),
  ndvi_apr_2022_log = log(rs_NDVI_Apr2022),
  ndvi_mar_2022_log = log(rs_NDVI_Mar2022),
  Distance_log = log(Distance),
  # Distance_log = Distance,
  rs_healthcare_accessbility_walking_only2019_log = log(rs_healthcare_accessbility_walking_only2019),
  rs_city_accessibility2015_log = log(rs_city_accessibility2015)
)




############################ scatter Plot ###########################################
### distance + fcs
fcs_distance_osm <- ggplot(data, aes(x=Distance, y=fcs)) + 
  geom_point()+
  geom_smooth(method=lm,se=F)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank()) +
  xlab("Distance (OSM)")

fcs_distance_rs <- ggplot(data, aes(x=rs_avg_dist_perm_water_pix_20172020, y= fcs)) + 
  geom_point()+
  geom_smooth(method=lm,se=T)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank()) +
  xlab("Distance (RS)")


fcs_distance_scater_plot <- ggarrange(fcs_distance_osm, fcs_distance_rs)






############ priority_needs.food #################


food_need_distance_t_test <- weighted_t_test_cus(data = data,strata = "strata_and_pop_group",survey_weights = "survey_weight",
                                                 binary_variable = "need_priorities.food",
                                                 non_binary_variable =  c("Distance_log","rs_avg_dist_perm_water_pix_20172020") )




health_need_distance_t_test <- weighted_t_test_cus(data = data,strata = "strata_and_pop_group",survey_weights = "survey_weight",
                                                   binary_variable = "need_priorities.healthcare",
                                                   non_binary_variable =  c("Distance_log","rs_avg_dist_perm_water_pix_20172020") )


# health_issue_distance_t_test <- weighted_t_test_cus(data = data,strata = "strata_and_pop_group",survey_weights = "survey_weight",
#                                                    binary_variable = "need_priorities.healthcare",
#                                                    non_binary_variable =  c("Distance_log","rs_avg_dist_perm_water_pix_20172020") )
# 




##################### CORRELATION CHECK DISTANCE + FSC


fcs_distance_test <- illuminate::weighted_pearson_test(data = data,strata = "strata_and_pop_group",survey_weights = "survey_weight",
                                                       dep_var = "fcs",ind_var = c("Distance_log","rs_avg_dist_perm_water_pix_20172020")  )

##################### CORRELATION CHECK vci + FSC


fcs_vci_test <- illuminate::weighted_pearson_test(data = data,strata = "strata_and_pop_group",survey_weights = "survey_weight",
                                                  dep_var = "fcs",ind_var = "rs_VCI_Apr2022" )




##################### CORRELATION CHECK DISTANCE + medical expense
medical_exp_distance_test <- illuminate::weighted_pearson_test(data = data,strata = "strata_and_pop_group",survey_weights = "survey_weight",
                                                               dep_var = "medical_exp",ind_var = c("Distance_log","rs_avg_dist_perm_water_pix_20172020")  )




##################### CORRELATION CHECK NDVI + FSC


# data_ndvi <- data %>% filter(ndvi_may_2022_log > -3)

fcs_ndvi_test <- illuminate::weighted_pearson_test(data = data,
                                                   strata = "strata_and_pop_group",
                                                   survey_weights = "survey_weight",
                                                   dep_var = "fcs",ind_var = c("ndvi_mar_2022_log",
                                                                               "ndvi_apr_2022_log","ndvi_may_2022_log"))


### vci + fcs

fcs_vci <- ggplot(data, aes(x=rs_VCI_Apr2022, y=fcs)) + 
  geom_point()+
  geom_smooth(method=lm,se=F)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank()) +
  xlab("VCI")




### NDVI + fcs



fcs_NDVI_may <- ggplot((data %>% filter(ndvi_may_2022_log > -3)), aes(x=ndvi_may_2022_log, y=fcs)) + 
  geom_point()+
  geom_smooth(method=lm,se=F)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank()) +
  xlab("NDVI May(log)")


fcs_NDVI_mar <- ggplot(data, aes(x=ndvi_mar_2022_log, y=fcs)) + 
  geom_point()+
  geom_smooth(method=lm,se=F)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank()) +
  xlab("NDVI March(log)")

fcs_NDVI_apr <- ggplot(data, aes(x=ndvi_apr_2022_log, y=fcs)) + 
  geom_point()+
  geom_smooth(method=lm,se=F)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank()) +
  xlab("NDVI April(log)")


fcs_NDVI <- ggarrange(fcs_NDVI_mar,fcs_NDVI_apr, fcs_NDVI_may)




##### medical exp + distance

data_medical_exp <- data %>% filter(medical_exp < 2000000)

medical_exp_distance_osm <- ggplot(data_medical_exp, aes(x=Distance_log, y=medical_exp)) + 
  geom_point()+
  geom_smooth(method=lm,se=F)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank()) +
  xlab("Distance log (OSM)")

medical_exp_distance_other <- ggplot(data_medical_exp, aes(x=rs_avg_dist_perm_water_pix_20172020, y=medical_exp)) + 
  geom_point()+
  geom_smooth(method=lm,se=F)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank()) +
  xlab("Distance(xxx)")




medical_water <- ggarrange(medical_exp_distance_osm,medical_exp_distance_other)



#################### BOX PLOT ###############################


priority_needs_health_distance <- ggplot(data, aes(x=need_priorities.healthcare, y=Distance_log)) + 
  geom_boxplot()+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank())


priority_needs_health_distance2 <- ggplot((data %>% filter(rs_avg_dist_perm_water_pix_20172020 < 400000)),
                                          aes(x=need_priorities.healthcare, y=rs_avg_dist_perm_water_pix_20172020)) + 
  geom_boxplot()+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank())


health_box_plots <- ggarrange(priority_needs_health_distance,priority_needs_health_distance2)





##################################33

priority_needs_food_distance <- ggplot(data, aes(x=need_priorities.food, y=Distance_log)) + 
  geom_boxplot()+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank())


priority_needs_food_distance2 <- ggplot((data %>% filter(rs_avg_dist_perm_water_pix_20172020 < 400000)),
                                        aes(x=need_priorities.food, y=rs_avg_dist_perm_water_pix_20172020)) + 
  geom_boxplot()+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank())


food_box_plots <- ggarrange(priority_needs_food_distance,priority_needs_food_distance2)


################## distance to health and urban center 

## scatter plot


### distance + fcs
accessibility_fcs <- ggplot(data, aes(x=rs_healthcare_accessbility_walking_only2019, y=fcs)) + 
  geom_point()+
  geom_smooth(method=lm,se=F)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank()) +
  xlab("Oxford accessibility")

accessibility_medical_exp <- ggplot(data, aes(x=rs_avg_dist_perm_water_pix_20172020, y= medical_exp)) + 
  geom_point()+
  geom_smooth(method=lm,se=T)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank()) +
  xlab("Oxford accessibility")


accessibility_scater_plot <- ggarrange(accessibility_fcs, accessibility_medical_exp)

### oxford indicator distance to health

data$rs_healthcare_accessibility2019 %>% log()%>% hist()

# 
# oxford_indicaor_test_helth_care_medical_exp <- weighted_pearson_test(data = data,dep_var = "rs_healthcare_accessbility_walking_only2019",
#                                               ind_var = c("medical_exp"),
#                                               strata = "strata_and_pop_group",
#                                               survey_weights = "survey_weight")


oxford_indicaor_test_health_care_accessibility <- weighted_pearson_test(data = data,dep_var = "rs_healthcare_accessbility_walking_only2019",
                                                                        ind_var = c("medical_exp","fcs"),
                                                                        strata = "strata_and_pop_group",
                                                                        survey_weights = "survey_weight")

########################################################################################################################################################
######################################################## logistic regression ########################################################################### 

data$need_priorities.healthcare <-    data$need_priorities.healthcare %>% as.integer()

data_logistic <- data %>% dplyr::select(need_priorities.healthcare,Distance_log,Distance)

# removeing outliers and inf/missing values
outliers <- (boxplot.stats(data_logistic$Distance))$out
outliers_log <- (boxplot.stats(data_logistic$Distance_log))$out

data_logistic <- data_logistic %>% filter(!Distance_log %in% outliers_log)
data_logistic <- data_logistic %>% filter(!Distance %in% outliers)


data_logistic <- data_logistic %>% filter(!is.na(Distance_log)) %>% 
  filter(!is.nan(Distance_log))  %>% filter(!is.infinite(Distance_log)) %>% filter()

data_logistic <- data_logistic %>% filter(!is.na(need_priorities.healthcare)) %>% filter(!is.nan(need_priorities.healthcare))  %>% filter(!is.infinite(need_priorities.healthcare))



###### Regression #####
fit_glm <- glm(need_priorities.healthcare~Distance  , 
               data = data_logistic, family = "binomial") 

logistic_regression_summary <-summary(fit_glm)

#### Calculate Odd ratio ########
odd_ratio <- or_glm(data = data_logistic, model = fit_glm,incr = list(Distance=10))
odd_ratio


### calulate the probability
data_logistic$probabilities <- fit_glm %>% predict(type = "response")


####### Regression plot 

regression_plot<-ggplot(data_logistic,aes(Distance_log, probabilities)) +
  geom_point() +
  geom_smooth(method='glm', method.args = list(family = "binomial"))+
  # stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), sep = "*\", \"*"))) +
  stat_poly_eq(label.y = 0.9) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size =.02, colour = "#D5D5D5"))+
  expand_limits(y=c(0:1))


regression_plot
