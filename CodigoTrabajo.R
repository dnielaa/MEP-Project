## TRABAJO DE METODOS ESTADISTICOS ##

# librerias
library(tidyverse)
library(ggplot2)
library(dplyr)
library(purrr)

# leemos los datos
health_df <- read.csv("data/health_C.csv")
head(health_df) #mostrar las primeras filas

## PRIMER BLOQUE ##
# media, mediana y desviación típica de la variable health_score.
media=mean(health_df$health_score) 
mediana=median(health_df$health_score) 
des_tipica=sd(health_df$health_score) 

# histograma de health_score con 20 intervalos.
ggplot(health_df, aes(x = health_score)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "blue") +
  labs(title = "Histograma de indice de salud del paciente",
       x = "Puntaje",
       y = "Indice") +
  theme_minimal()




# distribución de frecuencias relativas de los pacientes por sex. 
 #### MIRAR ESTO BIEN ####
frec_abs=table(health_df$sex)
frec_rel=prop.table(frec_abs)
ptg_rel=frec_rel*100
tabla_frecuencias <- data.frame(
  Frecuencia_Absoluta = as.vector(frec_abs),
  Frecuencia_Relativa = round(as.vector(frec_rel), 4),
  Porcentaje_Porc = round(as.vector(ptg_rel), 2)
)
row.names(tabla_frecuencias) <- names(frec_abs)

# diagrama de cajas de health_score según region.
ggplot(health_df, aes(x = factor(region), y = health_score)) +
  geom_boxplot(fill = "lightblue", color="blue") +
  labs(title = "Indice de salud por región",
       x = "Region",
       y = "Indice de salud") +
  theme_minimal()


# gráfico de dispersión entre bmi y health_score. ¿Qué relación parece observarse? 
ggplot(health_df, aes(x = bmi, y = health_score)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Relación entre índice de salud e índice de masa corporal",
       x = "indice de masa corporal", y = "indice de salud") +
  theme_minimal()
# CUANTO MAS INDICE DE MASA CORPORAL TIENE UNA PERSONA MAS BAJO ES SU INDICE DE SALUD 

# matriz de correlaciones entre las variables numéricas (ej. bmi, activity_hours, sleep_hours, stress, cholesterol, systolic_bp, diastolic_bp).
var_num=health_df[,c("age","bmi","activity_hours","sleep_hours","stress","cholesterol","systolic_bp",
              "diastolic_bp", "alcohol_units","diet_score","med_adherence","comorbidities","income")]
cor_matrix <- cor(var_num)
print(round(cor_matrix, 2))
# TIENE MUCHOS VALORES NA PORQUE LOS DATOS TIENE HUECOS SIN PONER.

# Representa un gráfico de dispersión entre activity_hours y health_score. ¿Hay indicios de una relación no lineal? 
ggplot(health_df, aes(x = activity_hours, y = health_score)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Relación entre horas de actividad e índice de masa corporal",
       x = "horas de actividad", y = "indice de salud") +
  theme_minimal()
# NO HAY INDICIOS PORQUE LA LINEA DE REGRESION ES RECTA, NO CURVA

# boxplot de health_score por fumador (smoker). ¿Existen diferencias claras entre fumadores y no fumadores? 
ggplot(health_df, aes(x = factor(smoker), y = health_score)) +
  geom_boxplot(fill = "lightblue", color="blue") +
  labs(title = "Indice de salud por fumadores",
       x = "fumador (0 no es fumador, 1 sí es fumador)",
       y = "Indice de salud") +
  theme_minimal()
# CON LA LINEA RECTA DEL LAS CAJAS VEMOS QUE EN MEDIA LOS NO FUMADORES TIENEN UN MEJOR INDICE DE SALUD
# LOS BIGOTES SON MUY LARGOS, LO QUE INDICAN QUE A LOS EXTREMOS DE LOS GRUPOS LOS INDICES DE SALUD SON MUY DIFERENTES
# LOS PUNTOS SON CASOS ATIPICOS, LOS NO FUMADORES TIENEN MAS QUE LOS FUMADORES

# valores atípicos en cholesterol y discute si son plausibles en  un contexto clínico. 

## HA DICHO EL PROFE QUE SE PUEDE VER CON UN BOXPLOT

# proporción de pacientes con enfermedad controlada (disease_controlled) en el dataset. 
frec_abs1=table(health_df$disease_controlled)
frec_rel1=prop.table(frec_abs)
ptg_rel1=frec_rel*100
tabla_frecuencias1 <- data.frame(
  Frecuencia_Absoluta1 = as.vector(frec_abs1),
  Frecuencia_Relativa1 = round(as.vector(frec_rel1), 4),
  Porcentaje_Porc1 = round(as.vector(ptg_rel1), 2)
)
row.names(tabla_frecuencias1) <- names(frec_abs1)

## SEGUNDO BLOQUE ##
# variables contienen valores faltantes y cuántos casos faltan en cada una. 
summary(is.na(health_df)) 
 #Los que pone true es el numero de variables que son nulas

# proporción total de valores faltantes sobre el dataset completo. 
mean(is.na(health_df))*100 #en %

# imputación por la media en bmi y calcula de nuevo la media de esta variable. 
health_df_imputado=health_df
media_calculada=mean(health_df$bmi, na.rm = TRUE)
health_df_imputado$bmi[is.na(health_df_imputado$bmi)]=media_calculada
media_imputada=mean(health_df_imputado$bmi)

# histogramas de sleep_hours antes y después de la imputación por media. ¿Observas cambios en la distribución? 

# imputación por la mediana en stress. Compara los resultados con la imputación por la media.

# imputación condicional: sustituye los valores faltantes de cholesterol por la media de pacientes del mismo grupo de sex. 

# discute si es razonable suponer que los valores faltantes son MCAR, MAR o MNAR en este contexto de salud. Justifica tu respuesta. 

# modelo lineal simple (health_score ~ bmi) usando solo casos completos. ¿Cuántas observaciones se pierden? 

# repite el modelo anterior pero con el dataset imputado. ¿Cambian los coeficientes? Explica por qué. 