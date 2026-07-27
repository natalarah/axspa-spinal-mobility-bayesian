# Best models and their Bayesian version:

library(tidyverse)
library(brms)
library(broom)
library(rstanarm)
library(bayestestR)

# 4 "FLEX_FRONT_CERV_FLEXION":

priors4 <- get_prior(CONTROLADO ~ FLEX_FRONT_CERV_FLEXION,
                     data = df_priors,
                     family = binomial())

data4 <- data.frame(
  'CONTROLADO' = if_else(datos$CONTROLADO == 'SI', 0, 1),
  'FLEX_FRONT_CERV_FLEXION' = datos$FLEX_FRONT_CERV_FLEXION
)

bm4 <- brm(CONTROLADO ~ 1 + FLEX_FRONT_CERV_FLEXION,
           data = data4, 
           family = 'binomial',
           cores = 4,
           prior = priors4)

summary(bm4)

# 1038 

df_priors

priors1038 <- get_prior(CONTROLLED ~ Mass_kg + CERV_ROT_LEF + CERV_ROT_RIG +
                          CERV_FRONT_FLEX + LAT_CERV_FLEX_LEF +
                          INTERMAL_DISTANCE,
                        data = datos,
                        family = binomial())

data1038 <- data.frame(
  'CONTROLLED' = if_else(datos$CONTROLLED == 'SI', 0, 1),
  'Mass_kg' = datos$Mass_kg,
  'CERV_ROT_LEF' = datos$CERV_ROT_LEF,
  'CERV_ROT_RIG' = datos$CERV_ROT_RIG,
  'CERV_FRONT_FLEX' = datos$CERV_FRONT_FLEX,
  'LAT_CERV_FLEX_LEF' = datos$LAT_CERV_FLEX_LEF,
  'INTERMAL_DISTANCE' = datos$INTERMAL_DISTANCE
)

glimpse(data1038)

# Bayes:

bm1038 <- brm(CONTROLLED ~ 1 + Mass_kg + CERV_ROT_LEF + CERV_ROT_RIG +
                CERV_FRONT_FLEX + LAT_CERV_FLEX_LEF +
                INTERMAL_DISTANCE,
              data = data1038, 
              family = 'binomial',
              cores = 4,
              prior = priors1038)

summary(bm1038)

plot(bm1038)

pp_check(bm1038)

# Frequentist:

fm1038 <- glm(CONTROLADO ~ 1 + PESO_kg + ROTA_CERV_IZQUIERDA + ROTA_CERV_DERECHA +
                FLEX_FRONT_CERV_FLEXION + FLEX_LAT_CERV_IZQUIERDA +
                DISTANCIA_INTERMALEOLAR,
              data = data1038, 
              family = 'binomial')

summary(fm1038)


