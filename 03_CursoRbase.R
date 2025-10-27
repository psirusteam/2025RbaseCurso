# Configuración inicial: No mostrar mensajes ni warnings
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
library(printr)

library(tidyverse)
datos <- readRDS("../Data/base_personas_gasto.rds")
p <- ggplot(datos, aes(x = ingreso, y = gasto)) +
  geom_point(color = "#0B4C6B") +
  labs(title = "Dispersion de datos: antes de modelar",
       x = "Ingreso", y = "Gasto") +
  theme_minimal(base_size = 12)+
  xlim(0 , 50000)+ ylim(0 , 50000)

p

set.seed(123)
modelos <- tibble(
  a1 = runif(50, 750, 780),
  a2 = runif(50, 0.1, 1)
)

ggplot(datos, aes(x = ingreso, y = gasto)) +
  geom_abline(aes(intercept = a1, slope = a2), data = modelos,
              alpha = 1 / 4) +
  geom_point() +
   labs(title = "Dispersion de datos",
       x = "Ingreso", y = "Gasto") +
  theme_minimal(base_size = 12)+
  xlim(-1 , 50000)+ ylim(-1 , 50000)


a0 <- 761
a1 <- 0.75

datos <- datos %>%
  mutate(
    y_pred = a0 + a1 * ingreso,
    error = gasto - y_pred
  )

ggplot(datos , aes(x = ingreso, y = gasto)) +
  geom_abline(intercept = a0, slope = a1, colour = "blue", size = 1) +
  geom_point(size = 2, colour = "grey30") +
  geom_segment(aes(xend = ingreso, yend = y_pred), colour = "red", linetype = "dashed") +
  labs(
    title = "Distancia (error) entre la observación y el modelo",
    subtitle = expression(hat(y) == a[0] + a[1]*x),
   x = "Ingreso", y = "Gasto") +
  theme_minimal(base_size = 12)+
  xlim(30000 , 50000)+ ylim(5000 , 50000)


model1 <- function(a, data) {
  a[1] + data$ingreso * a[2]
}

datos$y_pred <-  model1(c(762, 0.75), datos)
datos %>% select(upm, id_hogar, id_pers, gasto, 
                 ingreso, y_pred) %>%
  head()


measure_distance <- function(mod, data) {
  diff <- data$gasto - model1(mod, data)
  sqrt(mean(diff^2))
}

measure_distance(c(a0, a1), datos)

datos %>% summarise(RMSE =  sqrt(mean((gasto - y_pred)^2)))


sim1_dist <- function(a1, a2) measure_distance(c(a1, a2), datos)

modelos <- modelos %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist),
         orden = rank(dist)) 
head(modelos)

ggplot(datos, aes(x = ingreso, y = gasto)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist),
    data = filter(modelos, rank(dist) <= 10)
  ) +labs( x = "Ingreso", y = "Gasto") +
  theme_minimal(base_size = 12)+
  xlim(-1 , 50000)+ ylim(-1 , 50000)

library(broom)
mod1 <- lm(gasto ~ ingreso, data = datos)
tidy(mod1)

a0 <- coef(mod1)[1]  # Intercepto
a1 <- coef(mod1)[2]  # Pendiente
ggplot(datos, aes(x = ingreso, y = gasto)) +
  geom_point(color = "#0B4C6B", alpha = 0.7) +
  geom_abline(intercept = a0, slope = a1, colour = "red", size = 1) +
  labs(title = "Ajuste lineal: modelo estimado",
       subtitle = "lm(gasto ~ ingreso)", x = "Ingreso", y = "Gasto") +
  theme_minimal(base_size = 12) +
    xlim(0 , 50000)+ ylim(0 , 50000)

library(modelr)
datos <- datos %>%
  add_predictions(mod1, var = "pred") 

datos %>% select(upm, id_hogar, id_pers, gasto, 
                 ingreso, pred) %>%  head()

ggplot(datos, aes(x = ingreso, y = gasto)) +
  geom_point(color = "#0B4C6B", alpha = 0.7) +
  geom_line(aes(y = pred),  colour = "red", linewidth = 1) +
  labs(
    title = "Predicciones del modelo",
    subtitle = "Recta de regresión ajustada sobre los datos",
    x = "Ingreso",
    y = "Gasto"
  ) +
  theme_minimal(base_size = 14) +
    xlim(0 , 50000)+ ylim(0 , 50000)

datos <- datos %>%
  add_residuals(mod1,var = "resid")

datos %>% select(upm, id_hogar, id_pers, gasto, 
                 ingreso, pred, resid) %>%  head()

ggplot(datos, aes(resid)) +
  geom_freqpoly(binwidth = 500, colour = "#0B4C6B") +
  labs(
    title = "Distribución de los residuos",
    x = "Residuo",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 14) + 
  xlim(-7000,7000)

ggplot(datos, aes(x = ingreso, y = resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(color = "#0B4C6B", alpha = 0.8) +
  labs(
    title = "Residuos vs. Ingreso",
    x = "Ingreso",
    y = "Residuo"
  ) +
  theme_minimal(base_size = 14) +
   xlim(0 , 50000) + ylim(-20000,20000)

model_matrix(datos, gasto ~ sexo) %>% 
  head()

library(scales)
library(haven)
datos2 <- datos %>% 
  mutate(niveduc = as.character(niveduc_ee), 
         niveduc_labels = as_factor(niveduc_ee)) %>% 
  filter(!is.na(niveduc)) %>% ungroup()
datos2 %>% 
  distinct(niveduc_labels,niveduc) %>% arrange(niveduc)

ggplot(datos2) +
  geom_point(aes(x = niveduc , y = gasto)) +
  labs(title = "Variable categórica", x = "Nivel educativo", y = "Gastos") +
  scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ",")) +
  theme_minimal(base_size = 14)

mod2 <- lm(gasto ~ niveduc, data = datos2)

tidy(mod2)

datos2 <- datos2 %>%
  add_predictions(mod2, var = "y_pred")
datos2 %>% distinct(niveduc_labels, 
                  niveduc, y_pred)  %>%  head() 

datos2 %>% add_residuals(mod2, var = "residual") %>%
  distinct(niveduc_labels, niveduc, 
         y_pred, residual) %>%  head()

ggplot(datos2, aes(niveduc)) +
  geom_point(aes(y = gasto), colour = "#0B4C6B", size = 2) +
  geom_point( aes(y = y_pred), colour = "red", size = 4) +
   labs(title = "Variable categórica x", x = "Nivel educativo", y = "Gastos") +
  scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ",")) +
  theme_minimal(base_size = 14)

nivedu_colors <- c(
  "1" = "#4E79A7",  # Azul suave
  "2" = "#F28E2B",  # Naranja pastel
  "3" = "#E15759",  # Rojo suave
  "4" = "#76B7B2",  # Verde-azulado pastel
  "5" = "#59A14F",  # Verde natural
  "6" = "#EDC949",  # Amarillo suave
  "7" = "#AF7AA1"   # Púrpura pastel
)

ggplot(datos2, aes(x = ingreso, y = gasto)) +
  geom_point(aes(colour = niveduc), size = 2) +
  # Usar la paleta de colores institucional
  scale_colour_manual(values = nivedu_colors) +
  labs(x = "Ingreso (x1)", y = "Gastos (y)", 
       colour = "Nivel Edu. (x2)") +
  theme_minimal(base_size = 18) +
  xlim(0 , 50000)+ ylim(0 , 50000)

mod3 <- lm(gasto ~ ingreso + niveduc, data = datos2)
tidy(mod3)

mod4 <- lm(gasto ~ ingreso * niveduc, data = datos2)

tidy(mod4)

datos3 <- datos2 %>% 
  select(gasto, ingreso, niveduc_labels , niveduc) %>%
  gather_predictions(mod3, mod4)

head(datos3, 5)

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


datos3 <- datos2 %>% 
  select(gasto, ingreso, niveduc_labels , niveduc) %>%
  gather_residuals(mod3, mod4)


ggplot(datos3, aes(x = ingreso, y = resid, colour = niveduc)) +
  geom_point(alpha = 0.7) +
  facet_grid(model ~ niveduc, scales = "free") +
  # Usar la paleta de colores institucional
  scale_colour_manual(values = nivedu_colors) +
  labs(title = "Análisis de residuos por modelo y nivel de x2", x = "Ingreso", y = "Residuo") +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  # Línea de referencia en cero, color gris claro
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "#A9A9A9",
    linewidth = 0.8
  )

# lm(log(y) ~ sqrt(x1) + x2)

# lm(y ~ x + I(x^2))

df <- tribble(~y, ~x, 1, 1, 2, 2, 3, 3)
model_matrix(df, y ~ x^2 + x)
model_matrix(df, y ~ I(x^2) + x)

model_matrix(df, y ~ poly(x, 2))

library(splines)
model_matrix(df, y ~ ns(x, 2))

sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
  geom_point() + theme_minimal(base_size = 18)

mod1 <- lm(y ~ splines::ns(x, 1), data = sim5)
mod2 <- lm(y ~ splines::ns(x, 2), data = sim5)
mod3 <- lm(y ~ splines::ns(x, 3), data = sim5)
mod4 <- lm(y ~ splines::ns(x, 4), data = sim5)
mod5 <- lm(y ~ splines::ns(x, 5), data = sim5)

grid <- sim5 %>%
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) +
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~model) +
  labs(title = "Ajuste de modelos spline con distintos grados de libertad") + theme_minimal(base_size = 18)

library(tidyverse)
df <- tribble(  ~x, ~y, 1, 2.2,
  2, NA,  3, 3.5, 4, 8.3, NA, 10
)
df

mod <- lm(y ~ x, data = df, na.action = na.exclude)

nobs(mod)

# glm(y ~ x, family = binomial, data = df)

# library(mgcv)
# gam(y ~ s(x), data = df)

# library(glmnet)
# glmnet(x = as.matrix(df$x), y = df$y)

# library(MASS)
# rlm(y ~ x, data = df)
