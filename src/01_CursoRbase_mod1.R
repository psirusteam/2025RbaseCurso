###########################################################
#################### Curso R Base #########################
###########################################################

### Limpieza R environment ###

rm(list = ls())

# Módulo 1 - Explorar

# Instalar (solo la primera vez):
# install.packages("tidyverse")

library(tidyverse)   # Incluye dplyr, ggplot2, readr, etc.


# Importar la base (ejemplo)
datos <- readRDS(
  "Data/base_personas.rds"
) %>% as.data.frame() # readdRDS es una funcion base de R
# Ver las primeras filas
head(datos[, 1:8], 3)


# R como calculadora ------------------------------------------------

1 + 2
3 * 5
(10 + 5) / 3

# Crear objetos ------------------------------------------------------
x <- 3 * 4
x
resultado <- (59 + 73 + 2) / 3
resultado

# Numeric (numérico)
x_num <- 12.5
class(x_num)

# Tipo de datos -------------------------------------------------------

# Integer (entero)
x_int <- 7
class(x_int)

# Character (texto)
x_char <- "Bogotá"
class(x_char)

# Logical (lógico)
x_log <- TRUE
class(x_log)

# Factor (categorías)
x_fac <- factor(c("Primaria", "Secundaria", "Universitaria"))
class(x_fac)
levels(x_fac)

# Date (fecha)
x_date <- as.Date("2025-10-21")
class(x_date)

# Clases de objetos: Vector ----------------------------------------------

# Vector numérico
edades <- c(25, 30, 28, 40)
edades

# Vector de texto (character)
nombres <- c("Ana", "Luis", "María")
nombres

# Vector lógico (TRUE / FALSE)
es_mayor_edad <- c(TRUE, TRUE, FALSE, TRUE)
es_mayor_edad

# Clases de objetos: Factor ----------------------------------------------

sexo <- factor(c("Mujer", "Hombre", "Mujer", "Hombre"))
levels(sexo)       # Niveles del factor
class(sexo)        # "factor"

# Clases de objetos: Matriz ----------------------------------------------

matriz_ejemplo <- matrix(1:9, nrow = 3, ncol = 3)
matriz_ejemplo

# Clases de objetos: Data Frame ----------------------------------------------
personas <- data.frame(
  nombre = c("Ana", "Luis", "María"),
  edad   = c(23, 30, 28),
  ingreso = c(1200, 1500, 1800)
)

personas
class(personas)  # "data.frame"

# Clases de objetos: Tibble ----------------------------------------------

personas_tibble <- tibble(
  nombre = c("Ana", "Luis", "María"),
  edad   = c(23, 30, 28),
  ingreso = c(1200, 1500, 1800)
)
personas_tibble

# Clases de objetos: Lista ----------------------------------------------

mi_lista <- list(
  numeros = c(1, 2, 3),
  tabla = personas
)

mi_lista

# Funciones ----------------------------------------------

## nombre_funcion(argumento = valor)

# Secuencias y repetición
seq(1, 10, by = 2)     # 1, 3, 5, 7, 9
rep(5, times = 4)      # 5 5 5 5
rep(c("A","B"), each = 3)  # A A A B B B

# Resumen rápido de datos
tail(datos[, 1:8], 3)          # Últimas 3 filas
class(datos)            # Clase del objeto (data.frame, vector, etc.)
names(datos)            # Nombres de columnas

# Resumen rápido de datos

str(datos)              # Estructura del objeto (tipo de dato, columnas, etc.)


# Estadísticas básicas
mean(datos$ingreso, na.rm = TRUE)   # Media
median(datos$ingreso)               # Mediana
sd(datos$ingreso)                   # Desviación estándar
var(datos$ingreso)                  # Varianza


# Transformación de datos ----------------------------------------------

datos2 <- datos %>% select("id_hogar","id_pers", "edad","sexo", 
                           "etnia","area","ingreso",
                           "pobreza", "anoest") %>% rename(
                             id = id_pers
                           ) %>% as.data.frame()

head(datos2,5)

datos_mayores <- datos2 %>%
  filter(area == "1")

head(datos_mayores,4)

datos_ord <- datos2 %>% arrange(desc(ingreso))
head(datos_ord, 5)

# Transformación de datos: Crear variables --------------------------------

# Crear variable: ¿ingreso alto o no?
datos2$ingreso_alto <- ifelse(datos2$ingreso > 1000,
                              "Alto",
                              "No alto")

# Si tiene más de 12 años de estudio = educación superior
datos2$educ_superior <- ifelse(datos2$anoest > 12,
                               "Sí",
                               "No")


# Crear grupos de edad (niñez, juventud, adultez, vejez)
datos2 <- datos2 %>%
  mutate(grupo_edad = case_when(
    edad < 12 ~ "Niñez",
    edad >= 12 & edad < 18 ~ "Adolescencia",
    edad >= 18 & edad < 60 ~ "Adultez",
    edad >= 60 ~ "Adulto mayor",
    TRUE ~ NA_character_
  ))


# Crear grupos de años de educación
datos2 <- datos2 %>%
  mutate(ranoest = case_when(
    anoest == 0  ~ "1", # Sin educacion
    anoest %in% c(1:6) ~ "2",       # 1 - 6
    anoest %in% c(7:12) ~ "3",      # 7 - 12
    anoest > 12 ~ "4",      # mas de 12
    TRUE ~ NA_character_
  ))

# Transformación de datos: Resumir por grupos ----------------------------

resumen1 <- datos2 %>%
  group_by(sexo) %>%
  summarise(
    n = n(),
    ingreso_prom = mean(ingreso, na.rm = TRUE)
  )
resumen1

head(datos2[, 1:8],5)

# Visualización de datos ----------------------------------------------


ggplot(data = datos2) +
  geom_point(mapping = aes(x = ranoest, y = ingreso))

ggplot(data = datos2) +
  geom_point(mapping = aes(x = ranoest, y = ingreso, color = sexo))

grafico <- ggplot(data = datos2) +
  geom_point(mapping = aes(x = ranoest, y = ingreso, color = sexo)) +
  labs(
    title = "Relación entre educación, ingreso y sexo",
    x = "Años de estudio",
    y = "Ingreso",
    color = "Sexo"
  ) 

grafico

box_plot<- ggplot(datos2, aes(x = sexo, y = ingreso, fill = sexo)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(title = "Distribución del ingreso por sexo",
       x = NULL, y = "Ingreso") +
  theme_classic() +
  theme(legend.position = "none")

box_plot

histo <- ggplot(datos2, aes(x = ingreso)) +
  geom_histogram(bins = 40) +
  labs(title = "Histograma del ingreso (toda la muestra)",
       x = "Ingreso", y = "Frecuencia") +
  theme_classic()

histo