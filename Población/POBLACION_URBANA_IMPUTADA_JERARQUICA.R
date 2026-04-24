# Cargar paquetes necesarios
library(tidyverse)
library(readxl)
library(lubridate)

# 1. Cargar y preparar los datos
ruta_archivo <- "POBLACION_URBANA_2007-2024.xlsx"
datos <- read_excel(ruta_archivo, sheet = "POBLACION_URBANA") %>%
  mutate(
    UBIGEO = as.character(UBIGEO),
    AÑO = as.numeric(AÑO),
    POBLACION = as.numeric(POBLACION),
    INDICADOR = as.numeric(INDICADOR)
  )

# 2. Identificar nivel jerárquico para cada UBIGEO
datos <- datos %>%
  mutate(nivel = case_when(
    UBIGEO == "XXXXXX" ~ "nacional",
    str_sub(UBIGEO, 3, 6) == "XXXX" ~ "departamento",
    str_sub(UBIGEO, 5, 6) == "XX" ~ "provincia",
    TRUE ~ "distrito"
  )) %>%
  mutate(
    departamento = case_when(
      nivel == "nacional" ~ "XX",
      TRUE ~ str_sub(UBIGEO, 1, 2)
    ),
    provincia = case_when(
      nivel %in% c("nacional", "departamento") ~ "XX",
      TRUE ~ str_sub(UBIGEO, 3, 4)
    )
  )

# 3. Crear un dataframe completo con todos los años y UBIGEOs
años_completos <- 2007:2024
ubigeos_unicos <- unique(datos$UBIGEO)

# Crear grid completo
datos_completos <- expand.grid(
  UBIGEO = ubigeos_unicos,
  AÑO = años_completos,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  left_join(datos %>% select(UBIGEO, AÑO, POBLACION, INDICADOR, nivel, departamento, provincia), 
            by = c("UBIGEO", "AÑO"))

# 4. Extraer tendencias por nivel jerárquico

# 4.1. Tendencia nacional
tendencia_nacional <- datos_completos %>%
  filter(UBIGEO == "XXXXXX", !is.na(INDICADOR)) %>%
  select(AÑO, INDICADOR) %>%
  arrange(AÑO)

# 4.2. Tendencia departamental
tendencias_departamentales <- datos_completos %>%
  filter(nivel == "departamento", !is.na(INDICADOR)) %>%
  select(UBIGEO, AÑO, departamento, INDICADOR) %>%
  arrange(departamento, AÑO)

# 4.3. Tendencia distrital
tendencias_distritales <- datos_completos %>%
  filter(nivel == "distrito", !is.na(INDICADOR)) %>%
  select(UBIGEO, AÑO, INDICADOR) %>%
  arrange(UBIGEO, AÑO)

# 5. Calcular tasas de crecimiento (log odds) para las tendencias
# Usamos transformación logística para mantener proporciones en [0,1]

# 5.1. Tendencia nacional - transformación logística
tendencia_nacional <- tendencia_nacional %>%
  mutate(log_odds = log(INDICADOR / (1 - INDICADOR)))

# Calcular crecimiento anual promedio nacional
if (nrow(tendencia_nacional) >= 2) {
  anos_nacional <- sort(unique(tendencia_nacional$AÑO))
  if (length(anos_nacional) >= 2) {
    crecimiento_nacional <- (tendencia_nacional$log_odds[tendencia_nacional$AÑO == max(anos_nacional)] - 
                               tendencia_nacional$log_odds[tendencia_nacional$AÑO == min(anos_nacional)]) / 
      (max(anos_nacional) - min(anos_nacional))
  } else {
    crecimiento_nacional <- 0
  }
}

# 5.2. Tendencias departamentales
tendencias_departamentales <- tendencias_departamentales %>%
  mutate(log_odds = log(INDICADOR / (1 - INDICADOR)))

departamentos_unicos <- unique(tendencias_departamentales$departamento)
crecimiento_departamental <- tibble(departamento = character(), crecimiento = numeric())

for (depto in departamentos_unicos) {
  datos_depto <- tendencias_departamentales %>% filter(departamento == depto)
  anos_depto <- sort(unique(datos_depto$AÑO))
  
  if (length(anos_depto) >= 2) {
    crecimiento <- (datos_depto$log_odds[datos_depto$AÑO == max(anos_depto)] - 
                      datos_depto$log_odds[datos_depto$AÑO == min(anos_depto)]) / 
      (max(anos_depto) - min(anos_depto))
  } else {
    crecimiento <- 0
  }
  
  crecimiento_departamental <- bind_rows(crecimiento_departamental, 
                                         tibble(departamento = depto, crecimiento = crecimiento))
}

# 5.3. Tendencias distritales
tendencias_distritales <- tendencias_distritales %>%
  mutate(log_odds = log(INDICADOR / (1 - INDICADOR)))

ubigeos_distritos <- unique(tendencias_distritales$UBIGEO)
crecimiento_distrital <- tibble(UBIGEO = character(), crecimiento = numeric())

for (ubigeo in ubigeos_distritos) {
  datos_distrito <- tendencias_distritales %>% filter(UBIGEO == ubigeo)
  anos_distrito <- sort(unique(datos_distrito$AÑO))
  
  if (length(anos_distrito) >= 2) {
    crecimiento <- (datos_distrito$log_odds[datos_distrito$AÑO == max(anos_distrito)] - 
                      datos_distrito$log_odds[datos_distrito$AÑO == min(anos_distrito)]) / 
      (max(anos_distrito) - min(anos_distrito))
  } else {
    crecimiento <- 0
  }
  
  crecimiento_distrital <- bind_rows(crecimiento_distrital, 
                                     tibble(UBIGEO = ubigeo, crecimiento = crecimiento))
}

# 6. Imputar valores usando pooling parcial con transformación logística
for (ubigeo in ubigeos_unicos) {
  # Obtener datos para este UBIGEO
  datos_ubigeo <- datos_completos %>% filter(UBIGEO == ubigeo)
  
  # Obtener nivel y datos jerárquicos
  nivel_ubigeo <- unique(datos_ubigeo$nivel)
  departamento_ubigeo <- unique(datos_ubigeo$departamento)
  
  # Obtener población para ponderación por varianza
  poblacion_ubigeo <- datos_ubigeo$POBLACION[datos_ubigeo$AÑO == 2017]
  if (is.na(poblacion_ubigeo) || poblacion_ubigeo == 0) {
    poblacion_ubigeo <- datos_ubigeo$POBLACION[datos_ubigeo$AÑO == 2007]
  }
  
  # Constante de regularización para evitar división por cero
  k <- 1000
  
  # Ponderación inversamente proporcional a la varianza esperada
  peso_varianza <- ifelse(is.na(poblacion_ubigeo) || poblacion_ubigeo <= 0, 
                          0.5,  # peso por defecto si no hay datos poblacionales
                          poblacion_ubigeo / (poblacion_ubigeo + k))
  
  # Ajustar pesos según nivel jerárquico
  if (nivel_ubigeo == "nacional") {
    peso_nacional <- 1.0
    peso_departamental <- 0.0
    peso_distrital <- 0.0
  } else if (nivel_ubigeo == "departamento") {
    peso_nacional <- 0.5
    peso_departamental <- 0.5
    peso_distrital <- 0.0
  } else if (nivel_ubigeo %in% c("provincia", "distrito")) {
    peso_nacional <- 0.5
    peso_departamental <- 0.35
    peso_distrital <- 0.15
    
    # Si hay datos históricos suficientes para el distrito/provincia, aumentar su peso
    datos_historicos <- datos_ubigeo %>% filter(!is.na(INDICADOR))
    if (nrow(datos_historicos) >= 2) {
      peso_distrital_ajustado <- min(0.3, peso_distrital + 0.15)
      peso_nacional_ajustado <- peso_nacional - (peso_distrital_ajustado - peso_distrital) * 0.6
      peso_departamental_ajustado <- peso_departamental - (peso_distrital_ajustado - peso_distrital) * 0.4
      
      peso_nacional <- peso_nacional_ajustado
      peso_departamental <- peso_departamental_ajustado
      peso_distrital <- peso_distrital_ajustado
    }
  }
  
  # Aplicar ponderación por varianza
  peso_nacional <- peso_nacional + peso_varianza * (1 - peso_nacional)
  peso_departamental <- peso_departamental * (1 - peso_varianza)
  peso_distrital <- peso_distrital * (1 - peso_varianza)
  
  # Normalizar pesos
  suma_pesos <- peso_nacional + peso_departamental + peso_distrital
  peso_nacional <- peso_nacional / suma_pesos
  peso_departamental <- peso_departamental / suma_pesos
  peso_distrital <- peso_distrital / suma_pesos
  
  # Imputar para cada año
  for (año in años_completos) {
    indice <- which(datos_completos$UBIGEO == ubigeo & datos_completos$AÑO == año)
    
    if (length(indice) > 0 && is.na(datos_completos$INDICADOR[indice])) {
      # 1. Tendencia nacional
      log_odds_nacional <- NA
      if (!is.na(tendencia_nacional$log_odds[tendencia_nacional$AÑO == 2017])) {
        # Calcular años desde 2017
        delta_años <- año - 2017
        log_odds_base <- tendencia_nacional$log_odds[tendencia_nacional$AÑO == 2017]
        log_odds_nacional <- log_odds_base + crecimiento_nacional * delta_años
      } else if (!is.na(tendencia_nacional$log_odds[tendencia_nacional$AÑO == 2007])) {
        delta_años <- año - 2007
        log_odds_base <- tendencia_nacional$log_odds[tendencia_nacional$AÑO == 2007]
        log_odds_nacional <- log_odds_base + crecimiento_nacional * delta_años
      }
      
      # 2. Tendencia departamental
      log_odds_departamental <- NA
      if (departamento_ubigeo != "XX") {
        crecimiento_depto <- crecimiento_departamental$crecimiento[crecimiento_departamental$departamento == departamento_ubigeo]
        if (length(crecimiento_depto) > 0 && !is.na(crecimiento_depto)) {
          datos_depto <- tendencias_departamentales %>% 
            filter(departamento == departamento_ubigeo, AÑO %in% c(2007, 2017))
          
          if (nrow(datos_depto) > 0) {
            año_base <- max(datos_depto$AÑO)
            log_odds_base <- datos_depto$log_odds[datos_depto$AÑO == año_base]
            delta_años <- año - año_base
            log_odds_departamental <- log_odds_base + crecimiento_depto * delta_años
          }
        }
      }
      
      # 3. Tendencia distrital
      log_odds_distrital <- NA
      if (nivel_ubigeo %in% c("provincia", "distrito")) {
        crecimiento_dist <- crecimiento_distrital$crecimiento[crecimiento_distrital$UBIGEO == ubigeo]
        if (length(crecimiento_dist) > 0 && !is.na(crecimiento_dist)) {
          datos_dist <- tendencias_distritales %>% 
            filter(UBIGEO == ubigeo, AÑO %in% c(2007, 2017))
          
          if (nrow(datos_dist) > 0) {
            año_base <- max(datos_dist$AÑO)
            log_odds_base <- datos_dist$log_odds[datos_dist$AÑO == año_base]
            delta_años <- año - año_base
            log_odds_distrital <- log_odds_base + crecimiento_dist * delta_años
          }
        }
      }
      
      # Combinar las tres tendencias usando los pesos
      log_odds_combinado <- 0
      peso_total <- 0
      
      if (!is.na(log_odds_nacional)) {
        log_odds_combinado <- log_odds_combinado + peso_nacional * log_odds_nacional
        peso_total <- peso_total + peso_nacional
      }
      
      if (!is.na(log_odds_departamental)) {
        log_odds_combinado <- log_odds_combinado + peso_departamental * log_odds_departamental
        peso_total <- peso_total + peso_departamental
      }
      
      if (!is.na(log_odds_distrital)) {
        log_odds_combinado <- log_odds_combinado + peso_distrital * log_odds_distrital
        peso_total <- peso_total + peso_distrital
      }
      
      # Normalizar si no se usaron todos los pesos
      if (peso_total > 0) {
        log_odds_combinado <- log_odds_combinado / peso_total
        
        # Transformación inversa logística para volver al espacio de proporciones
        proporcion_imputada <- exp(log_odds_combinado) / (1 + exp(log_odds_combinado))
        
        # Restringir a valores lógicos [0.1, 0.9] para evitar valores extremos
        proporcion_imputada <- max(0.1, min(0.9, proporcion_imputada))
        
        # Asignar el valor imputado
        datos_completos$INDICADOR[indice] <- proporcion_imputada
      }
    }
  }
}

# 7. Aplicar suavizado temporal para evitar saltos abruptos
suavizado_datos <- datos_completos %>%
  filter(nivel %in% c("distrito", "provincia", "departamento", "nacional")) %>%
  arrange(UBIGEO, AÑO)

# Crear columna para los valores suavizados
suavizado_datos$INDICADOR_Suavizado <- suavizado_datos$INDICADOR

# Aplicar suavizado por UBIGEO
ubigeos_a_suavizar <- unique(suavizado_datos$UBIGEO)

for (ubigeo in ubigeos_a_suavizar) {
  datos_ubigeo <- suavizado_datos %>% filter(UBIGEO == ubigeo)
  
  if (nrow(datos_ubigeo) > 3) {
    # Identificar años con datos originales (2007 y 2017)
    anos_originales <- c(2007, 2017)
    indices_originales <- which(datos_ubigeo$AÑO %in% anos_originales)
    
    # Aplicar suavizado móvil con ventana de 3 años
    for (i in 2:(nrow(datos_ubigeo) - 1)) {
      año_actual <- datos_ubigeo$AÑO[i]
      
      # No suavizar los años con datos originales
      if (!(año_actual %in% anos_originales)) {
        # Calcular promedio móvil ponderado
        valor_anterior <- datos_ubigeo$INDICADOR[i - 1]
        valor_actual <- datos_ubigeo$INDICADOR[i]
        valor_posterior <- datos_ubigeo$INDICADOR[i + 1]
        
        # Pesos para el suavizado: más peso al año actual
        peso_anterior <- 0.25
        peso_actual <- 0.5
        peso_posterior <- 0.25
        
        valor_suavizado <- peso_anterior * valor_anterior + 
          peso_actual * valor_actual + 
          peso_posterior * valor_posterior
        
        # Limitar el cambio máximo al 10% del valor original para conservar tendencias
        cambio_maximo <- 0.1 * valor_actual
        valor_suavizado <- max(valor_actual - cambio_maximo, min(valor_actual + cambio_maximo, valor_suavizado))
        
        suavizado_datos$INDICADOR_Suavizado[suavizado_datos$UBIGEO == ubigeo & 
                                              suavizado_datos$AÑO == año_actual] <- valor_suavizado
      }
    }
  }
}

# 8. Reemplazar los valores imputados con los suavizados
datos_completos <- datos_completos %>%
  left_join(suavizado_datos %>% select(UBIGEO, AÑO, INDICADOR_Suavizado), 
            by = c("UBIGEO", "AÑO")) %>%
  mutate(INDICADOR = ifelse(!is.na(INDICADOR_Suavizado), INDICADOR_Suavizado, INDICADOR)) %>%
  select(-INDICADOR_Suavizado)

# 9. Verificar resultados y coherencia
# Verificar que no haya valores NA en INDICADOR
valores_na <- datos_completos %>% filter(is.na(INDICADOR))
cat("Número de valores NA restantes:", nrow(valores_na), "\n")

# Verificar que los valores estén en [0,1]
valores_fuera_rango <- datos_completos %>% filter(INDICADOR < 0 | INDICADOR > 1)
cat("Número de valores fuera de rango [0,1]:", nrow(valores_fuera_rango), "\n")

# 10. Guardar resultados
writexl::write_xlsx(datos_completos, "POBLACION_URBANA_IMPUTADA.xlsx")
cat("\nImputación completada y resultados guardados en 'POBLACION_URBANA_IMPUTADA.xlsx'")