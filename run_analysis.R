library(tidyverse)

# --- Cargar datos crudos ---
patients <- read_csv('/cloud/project/bayes_asxpa_2026/Data/patients.csv')
patients$NUMERO <- as_factor(patients$NUMERO)
patients$ID <- as_factor(patients$ID)
patients$BIOLOGICO <- as_factor(patients$BIOLOGICO)
patients$TIPO <- as_factor(patients$TIPO)
patients$CONTROLADO <- as_factor(patients$CONTROLADO)

controls <- read_csv('/cloud/project/bayes_asxpa_2026/Data/controls.csv')
controls$NUMERO <- as_factor(controls$NUMERO)
controls$ID <- as_factor(controls$ID)
controls$BIOLOGICO <- as_factor(controls$BIOLOGICO)
controls$TIPO <- as_factor(controls$TIPO)
controls$CONTROLADO <- 'SI'

# --- Combinar y seleccionar variables ópticas ---
datos <- rbind(patients, controls)

datos <- datos %>%
  select(PESO_kg, ROTA_CERV_LAB_IZQUIERDA, ROTA_CERV_LAB_DERECHA,
         FLEX_FRONT_CERV_LAB_GRAD_FLEXION, FLEX_FRONT_CERV_LAB_GRAD_EXTESION,
         FLEX_LAT_CERV_LAB_GRAD_DERECHA, FLEX_LAT_CERV_LAB_GRAD_IZQUIERDA,
         SHOBERT_MODIFICADO_CM_LABORATORIO, FLEX_LUMBAR_LAT_LAB_CM_DERECHA,
         FLEX_LUMBAR_LAT_LAB_CM_IZQUIERDA, DISTANCIA_INTERMALEOLAR_LAB_CM,
         CONTROLADO)

# --- Renombrar a los nombres que usan los scripts de modelado ---
names(datos)[1] <- 'Mass_kg'
names(datos)[2] <- 'CERV_ROT_LEF'
names(datos)[3] <- 'CERV_ROT_RIG'
names(datos)[4] <- 'CERV_FRONT_FLEX'
names(datos)[5] <- 'CERV_FRONT_EXT'
names(datos)[6] <- 'LAT_CERV_FLEX_RIG'
names(datos)[7] <- 'LAT_CERV_FLEX_LEF'
names(datos)[8] <- 'SCHOBERT_MOD'
names(datos)[9] <- 'LAT_LUMBAR_FLEX_RIG'
names(datos)[10] <- 'LAT_LUMBAR_FLEX_LEF'
names(datos)[11] <- 'INTERMAL_DISTANCE'
names(datos)[12] <- 'CONTROLLED'

datos$CONTROLLED <- as_factor(datos$CONTROLLED)
datos <- datos[complete.cases(datos),]

rm(patients, controls)

# --- Verificación ---
message("Filas: ", nrow(datos), " | Columnas: ", ncol(datos))
table(datos$CONTROLLED)

# --- Ahora sí, correr el modelado ---
#source("/cloud/project/bayes_asxpa_2026/best_subset_bayes.R")
