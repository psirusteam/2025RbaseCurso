###############################################################################
#                ANÁLISIS DE REGRESIÓN LINEAL EN R - DOCUMENTADO
#   Autor: Stalyn Yasid Guerrero Gómez
#   Descripción: Ejemplo completo de modelado lineal, evaluación de errores,
#                y extensión a variables categóricas y modelos no lineales.
###############################################################################

# =============================================================================
# 1. Configuración inicial
# =============================================================================
library(printr)
library(tidyverse)

# =============================================================================
# 2. Exploración inicial de los datos
# =============================================================================

# Cargar base de datos
datos <- readRDS("Data/base_personas_gasto.rds")

# Gráfico de dispersión gasto vs ingreso
ggplot(datos, aes(x = ingreso, y = gasto)) +
  geom_point(color = "#0B4C6B") +
  labs(title = "Dispersión de datos: antes de modelar",
       x = "Ingreso", y = "Gasto") +
  theme_minimal(base_size = 12) +
  xlim(0, 50000) + ylim(0, 50000)

# =============================================================================
# 3. Simulación de múltiples líneas aleatorias (para ilustrar posibles modelos)
# =============================================================================

set.seed(123)
modelos <- tibble(
  a1 = runif(50, 750, 780),
  a2 = runif(50, 0.1, 1)
)

ggplot(datos, aes(x = ingreso, y = gasto)) +
  geom_abline(aes(intercept = a1, slope = a2), data = modelos, alpha = 1/4) +
  geom_point() +
  labs(title = "Simulación de posibles rectas de ajuste",
       x = "Ingreso", y = "Gasto") +
  theme_minimal(base_size = 12) +
  xlim(-1, 50000) + ylim(-1, 50000)

# =============================================================================
# 4. Modelo lineal simple: y = a0 + a1 * x
# =============================================================================

a0 <- 761
a1 <- 0.75

# Predicción y cálculo de errores
datos <- datos %>%
  mutate(
    y_pred = a0 + a1 * ingreso,
    error = gasto - y_pred
  )

# Visualización del error como distancia entre punto observado y modelo
ggplot(datos , aes(x = ingreso, y = gasto)) +
  geom_abline(intercept = a0, slope = a1, colour = "blue", size = 1) +
  geom_point(size = 2, colour = "grey30") +
  geom_segment(aes(xend = ingreso, yend = y_pred),
               colour = "red", linetype = "dashed") +
  labs(
    title = "Distancia (error) entre observación y modelo",
    subtitle = expression(hat(y) == a[0] + a[1]*x),
    x = "Ingreso", y = "Gasto"
  ) +
  theme_minimal(base_size = 12) +
  xlim(30000, 50000) + ylim(5000, 50000)

# =============================================================================
# 5. Funciones auxiliares para evaluar el modelo
# =============================================================================

# Definir modelo lineal
model1 <- function(a, data) {
  a[1] + data$ingreso * a[2]
}

# Aplicar el modelo a los datos
datos$y_pred <- model1(c(762, 0.75), datos)

# Evaluación inicial
datos %>% select(upm, id_hogar, id_pers, gasto, ingreso, y_pred) %>% head()

# Función para calcular la distancia media (RMSE)
measure_distance <- function(mod, data) {
  diff <- data$gasto - model1(mod, data)
  sqrt(mean(diff^2))
}

# Cálculo del RMSE manual y usando summarise
measure_distance(c(a0, a1), datos)
datos %>% summarise(RMSE = sqrt(mean((gasto - y_pred)^2)))

# =============================================================================
# 6. Búsqueda del mejor modelo mediante simulación de parámetros
# =============================================================================

sim1_dist <- function(a1, a2) measure_distance(c(a1, a2), datos)

modelos <- modelos %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist),
         orden = rank(dist))
modelos
# Visualización de los 10 mejores modelos
ggplot(datos, aes(x = ingreso, y = gasto)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(aes(intercept = a1, slope = a2, colour = -dist),
              data = filter(modelos, rank(dist) <= 10)) +
  labs(x = "Ingreso", y = "Gasto") +
  theme_minimal(base_size = 12) +
  xlim(-1, 50000) + ylim(-1, 50000)

# =============================================================================
# 7. Ajuste lineal usando lm()
# =============================================================================

library(broom)
mod1 <- lm(gasto ~ ingreso, data = datos)
tidy(mod1)

# =============================================================================
# 8. Visualización del modelo ajustado
# =============================================================================

a0 <- coef(mod1)[1]
a1 <- coef(mod1)[2]

ggplot(datos, aes(x = ingreso, y = gasto)) +
  geom_point(color = "#0B4C6B", alpha = 0.7) +
  geom_abline(intercept = a0, slope = a1, colour = "red", size = 1) +
  labs(title = "Ajuste lineal estimado", subtitle = "lm(gasto ~ ingreso)",
       x = "Ingreso", y = "Gasto") +
  theme_minimal(base_size = 12) +
  xlim(0, 50000) + ylim(0, 50000)

# =============================================================================
# 9. Predicciones y residuos del modelo
# =============================================================================

library(modelr)
datos <- datos %>% add_predictions(mod1, var = "pred")
datos <- datos %>% add_residuals(mod1, var = "resid")

# =============================================================================
# 10. Análisis gráfico de los residuos
# =============================================================================

# Histograma de residuos
ggplot(datos, aes(resid)) +
  geom_freqpoly(binwidth = 500, colour = "#0B4C6B") +
  labs(title = "Distribución de residuos", x = "Residuo", y = "Frecuencia") +
  theme_minimal(base_size = 14) + xlim(-7000, 7000)

# Residuos vs ingreso
ggplot(datos, aes(x = ingreso, y = resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(color = "#0B4C6B", alpha = 0.8) +
  labs(title = "Residuos vs. Ingreso", x = "Ingreso", y = "Residuo") +
  theme_minimal(base_size = 14) +
  xlim(0, 50000) + ylim(-20000, 20000)

# =============================================================================
# 11. Modelos con variables categóricas
# =============================================================================

library(scales)
library(haven)

datos2 <- datos %>%
  mutate(niveduc = as.character(niveduc_ee),
         niveduc_labels = as_factor(niveduc_ee)) %>%
  filter(!is.na(niveduc)) %>%
  ungroup()

# Visualización por nivel educativo
ggplot(datos2) +
  geom_point(aes(x = niveduc, y = gasto)) +
  labs(title = "Variable categórica", x = "Nivel educativo", y = "Gastos") +
  scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ",")) +
  theme_minimal(base_size = 14)

# =============================================================================
# 12. Modelos múltiples y comparaciones
# =============================================================================

mod2 <- lm(gasto ~ niveduc, data = datos2)
tidy(mod2)

datos2 <- datos2 %>%
  add_predictions(mod2, var = "y_pred_mod2") %>% 
  add_residuals(mod2, var = "residual_mod2") 

ggplot(datos2, aes(niveduc)) +
  geom_point(aes(y = gasto), colour = "#0B4C6B", size = 2) +
  geom_point(aes(y = y_pred_mod2), colour = "red", size = 4) +
  labs(title = "Variable categórica x", x = "Nivel educativo", y = "Gastos") +
  scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ",")) +
  theme_minimal(base_size = 14)

nivedu_colors <- c(
  "1" = "#4E79A7", 
  "2" = "#F28E2B", 
  "3" = "#E15759", 
  "4" = "#76B7B2", 
  "5" = "#59A14F", 
  "6" = "#EDC949", 
  "7" = "#AF7AA1"  
)

ggplot(datos2, aes(x = ingreso, y = gasto)) +
  geom_point(aes(colour = niveduc), size = 2) +
  scale_colour_manual(values = nivedu_colors) +
  labs(x = "Ingreso (x1)", y = "Gastos (y)", 
       colour = "Nivel Edu. (x2)") +
  theme_minimal(base_size = 18) +
  xlim(0 , 50000)+ ylim(0 , 50000)

mod3 <- lm(gasto ~ ingreso + niveduc, data = datos2)
mod4 <- lm(gasto ~ ingreso * niveduc, data = datos2)

tidy(mod3)
tidy(mod4)

datos3 <- datos2 %>% 
  select(gasto, ingreso, niveduc_labels , niveduc) %>%
  gather_predictions(mod3, mod4)

# Gráfico de líneas de predicción
ggplot(datos3,
       aes(
         x = ingreso,
         y = pred,
         colour = niveduc,
         group = interaction(model, niveduc)
       )) +
  geom_point(data = datos3, aes(x = ingreso,
                                y = gasto,
                                colour = niveduc), alpha = 0.6) +
  geom_line(linewidth = 1.2) +
  facet_wrap( ~ model) +
  scale_colour_manual(values = nivedu_colors) +
  labs(x = "Ingreso", y = "Gastos") +
  theme_minimal(base_size = 18) +
  xlim(0, 50000) +
  ylim(0, 50000)

# Gráfico de residuales

datos3 <- datos2 %>% 
  select(gasto, ingreso, niveduc_labels , niveduc) %>%
  gather_residuals(mod3, mod4)


ggplot(datos3, aes(x = ingreso, y = resid, colour = niveduc)) +
  geom_point(alpha = 0.7) +
  facet_grid(model ~ niveduc, scales = "free") +
  scale_colour_manual(values = nivedu_colors) +
  labs(title = "Análisis de residuos por modelo y nivel de x2", 
       x = "Ingreso", y = "Residuo") +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "#A9A9A9",
    linewidth = 0.8
  )


# =============================================================================
# 13. Modelos polinomiales y splines
# =============================================================================

library(splines)
sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>%
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) +
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~model) +
  labs(title = "Ajuste de modelos spline con distintos grados de libertad") +
  theme_minimal(base_size = 18)

###############################################################################
# FIN DEL DOCUMENTO
###############################################################################
