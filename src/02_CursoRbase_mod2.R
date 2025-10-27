############################################################
#                    CURSO R BASE – MÓDULO 2               #
#                 Autor: Yury Vanessa Ochoa Montes         #
#                 Descripción: Introducción a tidyverse,   #
#                 pipes, iteraciones y funciones en R.     #
############################################################

## Limpieza del entorno
rm(list = ls())

## Paquetes -------------------------------------------------
# Instalar (solo una vez):
# install.packages("tidyverse")


library(tidyverse)   # Incluye dplyr, ggplot2, readr, etc.


## Datos ----------------------------------------------------
# Importar base de ejemplo (readRDS es función base de R)
ruta_base <- "Data/base_personas.rds"

datos <- readRDS(ruta_base) %>%
  as_tibble()

# Vista rápida
datos %>% select(1:8) %>% slice_head(n = 3)

# Subconjunto y renombre
datos2 <- datos %>%
  select(id_hogar, id_pers, edad, sexo, etnia, area, ingreso, pobreza, anoest) %>%
  rename(id = id_pers) %>%
  as_tibble()

## Pipes ----------------------------------------------------
# Ejemplo 1: Resumen por sexo en área urbana (area == "1")
ing_sex <- datos2 %>%
  filter(area == "1") %>%
  group_by(sexo) %>%
  summarise(
    n = n(),                                           # número de personas
    ingreso_promedio = mean(ingreso, na.rm = TRUE),    # ingreso mensual promedio
    .groups = "drop"
  )

ing_sex

# Ejemplo 2: Top 5 hogares por ingreso per cápita
top5_pc <- datos2 %>%
  group_by(id_hogar) %>%
  summarise(
    ingreso_hogar = sum(ingreso, na.rm = TRUE),
    miembros      = n(),
    .groups = "drop"
  ) %>%
  mutate(ingreso_pc = ingreso_hogar / miembros) %>%
  arrange(desc(ingreso_pc)) %>%
  slice_head(n = 5)

top5_pc

## Iteración ------------------------------------------------
# Promedio de ingreso por condición de pobreza (loop simple)
pobrezas_unicas <- unique(datos2$pobreza)
resultado <- tibble(pobreza = character(), promedio = numeric())

for (p in pobrezas_unicas) {
  promedio <- mean(datos2$ingreso[datos2$pobreza == p], na.rm = TRUE)
  resultado <- bind_rows(resultado, tibble(pobreza = p, promedio = promedio))
}

resultado

# Promedio y desviación estándar de ingreso por pobreza y sexo (doble loop)
pobrezas <- unique(datos2$pobreza)
sexos    <- unique(datos2$sexo)

resultado2 <- tibble(pobreza = character(), sexo = character(),
                     promedio = numeric(), sd = numeric())

for (p in pobrezas) {
  for (s in sexos) {
    x <- datos2$ingreso[datos2$pobreza == p & datos2$sexo == s]
    resultado2 <- bind_rows(
      resultado2,
      tibble(
        pobreza  = p,
        sexo     = s,
        promedio = mean(x, na.rm = TRUE),
        sd       = sd(x, na.rm = TRUE)
      )
    )
  }
}

resultado2

## Búsquedas con while --------------------------------------
# Primera persona con ingreso > 15,000,000 y sexo == "Mujer"
i <- 1
while (datos2$ingreso[i] <= 15000000 | datos2$sexo[i] != "Mujer") {
  i <- i + 1
}
datos2[i, 1:8]

# Primera persona: Hombre, área == "2", ingreso entre [2M, 4M], no NA
i <- 1
while (
  datos2$sexo[i] != "Hombre" |
  datos2$area[i] != "2" |
  is.na(datos2$ingreso[i]) |
  datos2$ingreso[i] < 2000000 |
  datos2$ingreso[i] > 4000000
) {
  i <- i + 1
}
datos2[i, 1:8]

## Funciones ------------------------------------------------
#' Calcula ingreso per cápita por hogar
#' @param base tibble con variables: id_hogar, ingreso
#' @return tibble con n_miembros, ingreso_hogar e ingreso_pc por hogar
ingreso_pc_por_hogar <- function(base) {
  base %>%
    group_by(id_hogar) %>%
    summarise(
      n_miembros   = n(),
      ingreso_hogar = sum(ingreso, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(ingreso_pc = ingreso_hogar / n_miembros)
}

# Uso
hogares <- ingreso_pc_por_hogar(datos2)
hogares %>% slice_head(n = 5)

#' Clasifica pobreza por hogar dada una línea de pobreza
#' @param base tibble con variables: id_hogar, ingreso
#' @param linea_pobreza numérico con umbral de pobreza (per cápita)
#' @return tibble con ingreso_pc y variable pobre (Pobre / No pobre)
#' 
pobreza_por_hogar <- function(base, linea_pobreza) {
  base %>%
    group_by(id_hogar) %>%
    summarise(
      n_miembros    = n(),
      ingreso_hogar = sum(ingreso, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      ingreso_pc = ingreso_hogar / n_miembros,
      pobre      = if_else(ingreso_pc < linea_pobreza, "Pobre", "No pobre")
    )
}

# Uso
hogares_lp <- pobreza_por_hogar(datos2, linea_pobreza = 480000)
hogares_lp %>% slice_head(n = 5)






