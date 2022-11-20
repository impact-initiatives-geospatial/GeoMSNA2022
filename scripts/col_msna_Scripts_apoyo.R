modelos <- data[,names(data) %in% c(variables, "msni", "msni_lsg_cate")]
modelos<- modelos %>% select_if(is.character)

relaciones <- combn(colnames(modelos), 2, simplify = F)


datos <- sapply(relaciones, 
                FUN = function(x){
                  t <- chisq.test(modelos[,x[[1]]], modelos[,x[[2]]])
                  cbind(t$p.value, t$statistic)
                },
                simplify = TRUE
)


chisq.test(modelos$rs_alos_landforms, modelos$rs_col_lc_2018.nivel_2)



set.seed(1)
df <- data.frame(matrix(sample(c("Si","No"), 300, replace = TRUE),ncol=3), stringsAsFactors = FALSE)
df <- as.data.frame(modelos)
head(df)

relaciones <- combn(colnames(df), 2, simplify=FALSE)
relaciones



datos <- sapply(relaciones, 
                FUN = function(x){
                  t <- chisq.test(df[,x[[1]]], df[,x[[2]]])
                  cbind(t$p.value, t$statistic)
                },
                simplify = TRUE
)


resultados <- data.frame(do.call(rbind, relaciones))
resultados$p.value <- datos[1, ]
resultados$statistic <- datos[2, ]

resultados







lista <- list()

for (i in sectores) {
  for(j in variables){
    lista[[length(lista) + 1]] <- c(i, j)
  }
}


datos <- sapply(lista, 
                FUN = function(x){
                  t <- chisq.test(df[,x[[1]]], df[,x[[2]]])
                  cbind(t$p.value, t$statistic)
                },
                simplify = TRUE
)


resultados <- data.frame(do.call(rbind, relaciones))
resultados$p.value <- datos[1, ]
resultados$statistic <- datos[2, ]

resultados


chisq.test(data$lsg_score_pro, data$inundacion)

t <- chisq.test(data[,lista[[1]]][[1]], data[,lista[[1]]][[2]])
t$p.value
t$statistic      


df <- as.data.frame(data)

datos <- sapply(relaciones, 
                FUN = function(x){
                  t <- chisq.test(df[,x[[1]]], df[,x[[2]]])
                  cbind(t$p.value, t$statistic)
                },
                simplify = TRUE
)





t <- chisq.test(df[,lista[[74]]][[1]], df[,lista[[74]]][[2]])
t$p.value
t$statistic

lista[74]

df$proteccion <- as.character(df$proteccion)
df$rs_alos_landforms
# Modelo lineal individual
mod <- lm(proteccion ~ rs_alos_landforms, data = data)
# Ver los resultos
ojo <- summary(mod)
ojo$adj.r.squared
ojo$coefficients[1,4]

ojo$coefficients[1,4] < 0.05


library(vcd)

chisq.test(data$proteccion, data$inundacion)
tabla <- table(data$proteccion, data$inundacion);tabla
assocstats(x = tabla)

class(tabla)







######################################

### Regresiones
data$rs_alos_landforms

lm(msni ~ inundacion, data = data) %>% 
  summary

data2 <- data %>% filter(!is.na(msni))
data2 <- data %>% filter(!is.na(rs_alos_landforms))


plot(data$msni, data$lsg_edu_crit1)
abline(lm.prueba, col = "red")


lm.prueba <- lm(lsg_score_pro ~ rs_rx10d_30d_may, data = data)
summary(lm.prueba)
anova(lm.prueba)
cor(data$msni, data$lsg_edu_crit1)
prueba
prueba <- cor.test(data$msni, data$lsg_edu_crit1)
prueba$estimate

## Regresion Logistica
modelo.sin.dept <- glm(msni ~ alos_landforms,
                       data = data,
                       family = binomial())
summary(modelo.sin.dept)


## Ver las mejores correlaciones para saber con cuales hacer los modelos PAG. 347
gee2 <- gee %>% select(-uuid)
variables <- names(gee2);variables

sectores <- c("lsg_score_pro", "proteccion", "lsg_score_sa", "segaliment",
              "lsg_score_shelter", "alojamiento", "lsg_score_wash", "aguasanea",
              "lsg_score_edu", "educacion", "lsg_score_salud", "saludlsg",
              "lsg_score_mdv", "mediosvida", "msni", "msni_lsg_cate")


modelos <- data[,names(data) %in% c(variables, "msni", "msni_lsg_cate")]
modelos<- modelos %>% select_if(is.numeric)
modelos<- data %>% select_if(is.numeric)

# Ver los 4 mejores modelos
modelos %>%
  select(-msni) %>%
  map_dbl(cor, y = modelos$msni) %>%
  abs() %>% 
  sort(decreasing = TRUE) %>%
  .[1:4]

# Traer los datos de los 4 mejores modelos
best_pred <- modelos %>%
  select(-msni) %>%
  map_dbl(cor, y = modelos$msni) %>%
  abs() %>%
  sort(decreasing = TRUE) %>%
  .[1:4] %>%
  names %>%
  modelos[.]

# Generar el modelo lineal
mod <- lm(modelos$MSNI ~ as.matrix(best_pred))
# Ver los resultos
summary(mod)

# Modelo lineal individual
mod <- lm(MSNI_lsg_cate ~ `precipitation_rollsum10_30 days`, data = modelos)
# Ver los resultos
summary(mod)

##
## Regresion logistica PAG. 436
m <- glm(MSNI_lsg_cate ~ `precipitation_rollsum10_30 days`, family = binomial, data = modelos)
summary(m)


