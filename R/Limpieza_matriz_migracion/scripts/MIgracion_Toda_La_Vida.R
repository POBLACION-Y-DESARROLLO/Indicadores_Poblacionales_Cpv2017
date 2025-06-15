
# LIMPIEZA: MATRIZ ORIGEN DESTINO PO DISTRITO  ----------------------------
library(tidyverse)
library(openxlsx)
library(jsonlite)

# 1. Carga de datos -------------------------------------------------------
matriz_tlv <- read.xlsx("../../Salidas/MTLV_Origen_Destino_Distrito.xlsx")
ubigeos <- read.xlsx("../../Salidas/Departamento_Provincia_Distrito.xlsx")


# 2. Limpieza de datos ----------------------------------------------------

matriz_tlv %>%
  pivot_longer(
    cols = -c(UBIGEO.X, DISTRITO_RH.X),
    names_to = "ID",
    values_to = "POBLACION"
  ) %>%
  separate(ID, into = c("UBIGEO.Y", "DISTRITO.Y"), sep = "_", extra = "merge") %>% 
  mutate(
      POBLACION = na_if(POBLACION, "-"),                             # "-" a NA
      POBLACION = as.numeric(POBLACION)                              # convierte a decimal
  ) -> matriz_tlv_clean


# Unir Departamento y Provincia y Latitud y Longitud
matriz_tlv_clean %>% left_join(
  ubigeos %>% 
    select(UBIGEO, REGION, PROVINCIA, DISTRITO, LATITUD_ORIGEN, LONGITUD_ORIGEN),
    by = c("UBIGEO.X"="UBIGEO")) %>%
    left_join(
      ubigeos %>% 
      select(UBIGEO, REGION, PROVINCIA, DISTRITO, LATITUD_ORIGEN, LONGITUD_ORIGEN),
    by = c("UBIGEO.Y"="UBIGEO")
  ) %>%
  transmute(
    UBIGEO.X = UBIGEO.X,
    DEPARTAMENTO.X = REGION.x,
    PROVINCIA.X = PROVINCIA.x,
    DISTRITO.X = DISTRITO.x,
    POBLACION,
    LATITUD.X = LATITUD_ORIGEN.x,
    LONGITUD.X = LONGITUD_ORIGEN.x,
    UBIGEO.Y = UBIGEO.Y,
    DEPARTAMENTO.Y = REGION.y,
    PROVINCIA.Y = PROVINCIA.y,
    DISTRITO.Y = DISTRITO.y,
    LATITUD.Y = LATITUD_ORIGEN.y,
    LONGITUD.Y = LONGITUD_ORIGEN.y
  ) -> matriz_tlv_clean_join

# 3. Creación de indicadores de migración por distrito --------------------

# Residentes habituales por distrito
matriz_tlv_clean_join %>% 
  group_by(UBIGEO.X, DEPARTAMENTO.X, PROVINCIA.X, DISTRITO.X) %>% 
  summarise(
    POB_RESID = sum(POBLACION, na.rm = TRUE)
  ) -> Residentes_habituales

# Residentes 5 años atrás
matriz_tlv_clean_join %>% 
  group_by(UBIGEO.Y) %>% 
  summarise(
    POB_RESID_5AGO = sum(POBLACION, na.rm = TRUE)
  ) -> Residentes_5ago

# No migrantes

matriz_tlv_clean_join %>% 
  filter(UBIGEO.X==UBIGEO.Y) %>% 
  group_by(UBIGEO = UBIGEO.X) %>% 
  summarise(NO_MIGRANTES = sum(POBLACION, na.rm = TRUE)
  ) -> No_migrantes

# Crear la matriz de indicadores de migración por distrito

Residentes_habituales %>% 
  select(
    UBIGEO = UBIGEO.X,
    DEPARTAMENTO = DEPARTAMENTO.X,
    PROVINCIA = PROVINCIA.X,
    DISTRITO = DISTRITO.X,
    POB_RESID = POB_RESID
  ) %>% left_join(
    Residentes_5ago,
    by = c("UBIGEO"="UBIGEO.Y")
  ) %>% left_join(
    No_migrantes,
    by = c("UBIGEO" = "UBIGEO")
  ) %>% 
  mutate(
    INMIGRANTES = POB_RESID - NO_MIGRANTES,
    EMIGRANTES = POB_RESID_5AGO - NO_MIGRANTES,
    MIGRACION_NETA = INMIGRANTES - EMIGRANTES,
    MIGRACION_BRUTA = INMIGRANTES + EMIGRANTES,
    TASA_INMIGRACION = (INMIGRANTES/5)/((POB_RESID + POB_RESID_5AGO)/2)*1000,
    TASA_EMIGRACION = (EMIGRANTES/5)/((POB_RESID + POB_RESID_5AGO)/2)*1000,
    TASA_MIGRACION_NETA = TASA_INMIGRACION - TASA_EMIGRACION,
    INDICE_EFICIENCIA = MIGRACION_NETA/MIGRACION_BRUTA
  ) %>% 
  write_json(path = "../../Salidas/Indicadores_Migracion.json", pretty = TRUE)




































