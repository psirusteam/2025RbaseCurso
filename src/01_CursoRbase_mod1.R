###############################################################################
#                        CURSO R BASE – MÓDULO 1                              #
#                  Autora: Yury Vanessa Ochoa Montes                          #
#        Descripción: Limpieza de datos, tipos de objetos, estructuras,       #
#      transformación básica y visualización inicial con tidyverse            #
###############################################################################


## 1. Limpiar entorno y cargar paquetes ---------------------------------------
rm(list = ls())

# install.packages("tidyverse")   # (Solo la primera vez)
library(tidyverse)

## 2. Importar datos y vista rápida -------------------------------------------
datos <- readRDS("Data/base_personas.rds") %>% as_tibble()
head(datos[, 1:8], 3)

## 3. R como calculadora y objetos --------------------------------------------
1 + 2; 3 * 5; (10 + 5)/3
x <- 3 * 4; x
resultado <- (59 + 73 + 2)/3

## 4. Tipos de datos básicos ---------------------------------------------------
x_num <- 12.5; class(x_num)
x_int <- 7; class(x_int)
x_char <- "Bogotá"; class(x_char)
x_log <- TRUE; class(x_log)
x_fac <- factor(c("Primaria","Secundaria","Universitaria")); class(x_fac)

## 5. Estructuras de datos -----------------------------------------------------
edades <- c(25, 30, 28, 40)          # Vector numérico  
personas <- data.frame(nombre=c("Ana","Luis"), edad=c(23,30))  # Data frame
personas_tb <- tibble(nombre=c("Ana","Luis"), edad=c(23,30))   # Tibble
mi_lista <- list(numeros=1:3, tabla=personas)                   # Lista

## 6. Funciones rápidas --------------------------------------------------------
seq(1,10,2); rep("A",3)
str(datos); names(datos); tail(datos, 3)

## 7. Estadísticas básicas -----------------------------------------------------
mean(datos$ingreso, na.rm=TRUE)
median(datos$ingreso)
sd(datos$ingreso)

## 8. Transformar datos --------------------------------------------------------
datos2 <- datos %>%
  select(id_hogar, id_pers, edad, sexo, etnia, area, ingreso, pobreza, anoest) %>%
  rename(id = id_pers)

head(datos2, 4)

datos_mayores <- datos2 %>% filter(area == "1")
datos_ord <- datos2 %>% arrange(desc(ingreso))

datos2 <- datos2 %>%
  mutate(
    ingreso_alto = ifelse(ingreso > 1000, "Alto", "No alto"),
    educ_superior = ifelse(anoest > 12, "Sí", "No"),
    grupo_edad = case_when(
      edad < 12 ~ "Niñez",
      edad < 18 ~ "Adolescencia",
      edad < 60 ~ "Adultez",
      TRUE ~ "Adulto mayor"
    )
  )

## 9. Resumen por grupos -------------------------------------------------------
resumen1 <- datos2 %>%
  group_by(sexo) %>%
  summarise(n = n(), ingreso_prom = mean(ingreso, na.rm=TRUE))

resumen1

## 10. Visualización básica ----------------------------------------------------
# Dispersión
ggplot(datos2, aes(x = anoest, y = ingreso, color = sexo)) +
  geom_point() +
  labs(title = "Ingreso vs Años de Estudio", x = "Años de estudio", y = "Ingreso")

# Boxplot
ggplot(datos2, aes(x = sexo, y = ingreso, fill = sexo)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Ingreso por sexo", x = "", y = "Ingreso") +
  theme(legend.position="none")

# Histograma
ggplot(datos2, aes(x = ingreso)) +
  geom_histogram(bins = 40) +
  theme_classic() +
  labs(title = "Distribución del ingreso", x = "Ingreso", y = "Frecuencia")