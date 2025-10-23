###########################################################
#################### Curso R Base #########################
###########################################################

### Limpieza R environment ###

rm(list = ls())


# Módulo 2 - Programar

# Instalar (solo la primera vez):
# install.packages("tidyverse")

library(tidyverse)   # Incluye dplyr, ggplot2, readr, etc.


# Importar la base (ejemplo)
datos <- readRDS(
  "Data/base_personas.rds"
) %>% as.data.frame() # readdRDS es una funcion base de R
# Ver las primeras filas
head(datos[, 1:8], 3)

datos2 <- datos %>% select("id_hogar","id_pers", "edad","sexo", 
                           "etnia","area","ingreso",
                           "pobreza", "anoest") %>% rename(
                             id = id_pers
                           ) %>% as.data.frame()


# Pipes -------------------------------------------------------

ing_sex <- datos2 %>%
  # 1. Nos quedamos con las personas ocupadas
  filter(area == "1") %>%  
  # 2. Agrupamos por sexo
  group_by(sexo) %>%                           
  summarise(
    # Número de personas ocupadas en cada grupo
    n = n(),      
    # Ingreso mensual promedio
    ingreso_promedio = mean(ingreso, na.rm = TRUE)  
  )

ing_sex

top5_pc <- datos2 %>%
  # 1) agrupamos por hogar
  group_by(id_hogar) %>% 
  # 2) sumamos ingreso del hogar
  summarise(ingreso_hogar = sum(ingreso, na.rm = TRUE),
            # 3) contamos miembros
            miembros      = n(),                           
            .groups = "drop") %>%
  # 4) ingreso per cápita
  mutate(ingreso_pc = ingreso_hogar / miembros) %>%  
  # 5) ordenamos
  arrange(desc(ingreso_pc)) %>%      
  # 6) top 5
  slice_head(n = 5)                                        

top5_pc

# Iteración --------------------------------------------

pobre <- unique(datos2$pobreza)
resultado <- data.frame(pobreza = character(), promedio = numeric())

for (p in pobre) {
  promedio <- mean(datos2$ingreso[datos2$pobreza == p], 
                   na.rm = TRUE)
  resultado <- rbind(resultado, 
                     data.frame(pobreza = p, promedio = promedio))
}


resultado

pobrezas <- unique(datos2$pobreza)
sexos <- unique(datos2$sexo)

#Resultado vacío
resultado2 <- data.frame(pobreza=character(), sexo=character(),
                         promedio=numeric(), sd=numeric())

for (p in pobrezas) {
  for (s in sexos) {
    x <- datos2$ingreso[datos2$pobreza == p & datos2$sexo == s]
    resultado2 <- rbind(resultado2, data.frame(
      pobreza = p, sexo = s,promedio= mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE)))}}

resultado2

i <- 1

while (datos2$ingreso[i] <= 15000000 | datos2$sexo[i] != "Mujer") {
  i <- i + 1   # Avanzar a la siguiente persona
}

datos2[i,1:8 ]

i <- 1

while (
  datos2$sexo[i] != "Hombre" |
  datos2$area[i] != "2" |
  is.na(datos2$ingreso[i]) |
  datos2$ingreso[i] < 2000000 |
  datos2$ingreso[i] > 4000000
) {
  i <- i + 1 # Avanzar a la siguiente persona
}

datos2[i,1:8 ]

# Crear Funciones ----------------------------------------------

## nombre_funcion <- function(argumento1, argumento2) {
##   # código que hace algo
##   resultado <- argumento1 + argumento2  # ejemplo
##   return(resultado)  # opcional, pero recomendado
## }

ingreso_pc_por_hogar <- function(base) {
  base %>%
    group_by(id_hogar) %>%
    summarise(
      n_miembros = n(),
      ingreso_hogar = sum(ingreso, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(ingreso_pc = ingreso_hogar / n_miembros)
} 


# Usarla
hogares <- ingreso_pc_por_hogar(datos2)
head(hogares,5)

pobreza_por_hogar <- function(base, linea_pobreza) {
  base %>%
    group_by(id_hogar) %>%
    summarise(
      n_miembros = n(),
      ingreso_hogar= sum(ingreso, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      ingreso_pc = ingreso_hogar / n_miembros,
      pobre = ifelse(ingreso_pc < linea_pobreza, "Pobre", "No pobre")
    )
}


# Usarla
hogares_lp <- pobreza_por_hogar(datos2, linea_pobreza = 480000)
head(hogares_lp, 5)
