# Cargar paquetes necesarios
library(tidyverse)
library(readxl)

# 1. Cargar y preparar los datos
ruta_archivo <- "POBLACION_PROYECTADA_2000_2025.xlsx"
datos <- read_excel(ruta_archivo, sheet = "POBLACION") %>%
  mutate(
    UBIGEO = as.character(UBIGEO),
    AÑO = as.numeric(AÑO),
    POBLACION = as.numeric(POBLACION)
  )

# 2. Identificar niveles jerárquicos
datos <- datos %>%
  mutate(
    nivel = case_when(
      str_sub(UBIGEO, 3, 6) == "0000" ~ "departamento",
      str_sub(UBIGEO, 5, 6) == "00" ~ "provincia",
      TRUE ~ "distrito"
    ),
    departamento = str_sub(UBIGEO, 1, 2),
    provincia = ifelse(nivel == "departamento", "00", str_sub(UBIGEO, 3, 4))
  )

# 3. Crear una secuencia completa de años
años_completos <- 2000:2025

# 4. Identificar todos los UBIGEO únicos
ubigeos_unicos <- unique(datos$UBIGEO)

# 5. Crear un dataframe vacío para almacenar los resultados
datos_completos <- tibble()

# 6. Bucle para completar cada UBIGEO
for (ubigeo in ubigeos_unicos) {
  # Extraer datos para este UBIGEO
  datos_ubigeo <- datos %>% filter(UBIGEO == ubigeo)
  
  # Crear dataframe con todos los años
  años_ubigeo <- tibble(AÑO = años_completos) %>%
    left_join(datos_ubigeo, by = "AÑO")
  
  # Si hay datos faltantes, imputar
  if (any(is.na(años_ubigeo$POBLACION))) {
    # Identificar años con datos
    años_con_datos <- años_ubigeo %>% filter(!is.na(POBLACION)) %>% pull(AÑO) %>% sort()
    
    # Si hay al menos dos años con datos, proceder con interpolación
    if (length(años_con_datos) >= 2) {
      # Identificar años faltantes
      años_faltantes <- años_completos[is.na(años_ubigeo$POBLACION)]
      
      # Imputar cada año faltante
      for (año in años_faltantes) {
        # Encontrar el año anterior con datos
        años_anteriores <- años_con_datos[años_con_datos < año]
        if (length(años_anteriores) > 0) {
          año_anterior <- max(años_anteriores)
          
          # Encontrar el año posterior con datos
          años_posteriores <- años_con_datos[años_con_datos > año]
          if (length(años_posteriores) > 0) {
            año_posterior <- min(años_posteriores)
            
            # Obtener poblaciones
            poblacion_anterior <- años_ubigeo$POBLACION[años_ubigeo$AÑO == año_anterior]
            poblacion_posterior <- años_ubigeo$POBLACION[años_ubigeo$AÑO == año_posterior]
            
            # Calcular tasa de crecimiento exponencial
            if (poblacion_anterior > 0 & poblacion_posterior > 0) {
              tasa <- log(poblacion_posterior / poblacion_anterior) / (año_posterior - año_anterior)
              
              # Calcular población imputada
              poblacion_imputada <- poblacion_anterior * exp(tasa * (año - año_anterior))
              
              # Actualizar el valor
              años_ubigeo$POBLACION[años_ubigeo$AÑO == año] <- poblacion_imputada
            }
          }
        }
      }
    }
  }
  
  # Agregar nivel jerárquico al dataframe temporal
  años_ubigeo <- años_ubigeo %>%
    mutate(
      UBIGEO = ubigeo,
      nivel = unique(datos_ubigeo$nivel),
      departamento = unique(datos_ubigeo$departamento),
      provincia = unique(datos_ubigeo$provincia)
    )
  
  # Agregar al dataframe completo
  datos_completos <- bind_rows(datos_completos, años_ubigeo)
}

# 7. Reordenar columnas para consistencia
datos_completos <- datos_completos %>%
  select(UBIGEO, AÑO, POBLACION, nivel, departamento, provincia)

# 8. Verificar coherencia jerárquica
# Departamentos
deptos <- datos_completos %>% filter(nivel == "departamento")
provs <- datos_completos %>% filter(nivel == "provincia")
distritos <- datos_completos %>% filter(nivel == "distrito")

# 9. Reconciliación jerárquica (top-down)
for (depto in unique(datos_completos$departamento)) {
  depto_ubigeo <- paste0(depto, "0000")
  
  # Ajustar provincias para que sumen a departamentos
  for (año in años_completos) {
    total_dept <- datos_completos$POBLACION[datos_completos$UBIGEO == depto_ubigeo & datos_completos$AÑO == año]
    
    if (!is.na(total_dept) && total_dept > 0) {
      # Obtener provincias en este departamento
      provs_dept <- datos_completos %>%
        filter(departamento == depto, AÑO == año, nivel == "provincia")
      
      if (nrow(provs_dept) > 0) {
        # Calcular proporciones actuales
        proporciones <- provs_dept$POBLACION / sum(provs_dept$POBLACION, na.rm = TRUE)
        
        # Ajustar a total_dept
        ajustes <- total_dept * proporciones
        
        # Actualizar valores
        for (i in 1:nrow(provs_dept)) {
          ubigeo_prov <- provs_dept$UBIGEO[i]
          datos_completos$POBLACION[datos_completos$UBIGEO == ubigeo_prov & datos_completos$AÑO == año] <- ajustes[i]
        }
      }
    }
  }
}

# Ajustar distritos para que sumen a provincias
for (depto in unique(datos_completos$departamento)) {
  for (prov in unique(datos_completos$provincia[datos_completos$departamento == depto])) {
    if (prov != "00") {
      prov_ubigeo <- paste0(depto, prov, "00")
      
      for (año in años_completos) {
        total_prov <- datos_completos$POBLACION[datos_completos$UBIGEO == prov_ubigeo & datos_completos$AÑO == año]
        
        if (!is.na(total_prov) && total_prov > 0) {
          # Obtener distritos en esta provincia
          dists_prov <- datos_completos %>%
            filter(departamento == depto, provincia == prov, AÑO == año, nivel == "distrito")
          
          if (nrow(dists_prov) > 0) {
            # Calcular proporciones actuales
            proporciones <- dists_prov$POBLACION / sum(dists_prov$POBLACION, na.rm = TRUE)
            
            # Ajustar a total_prov
            ajustes <- total_prov * proporciones
            
            # Actualizar valores
            for (i in 1:nrow(dists_prov)) {
              ubigeo_dist <- dists_prov$UBIGEO[i]
              datos_completos$POBLACION[datos_completos$UBIGEO == ubigeo_dist & datos_completos$AÑO == año] <- ajustes[i]
            }
          }
        }
      }
    }
  }
}

# 10. Redondear población a cero decimales
datos_completos$POBLACION <- round(datos_completos$POBLACION, 0)

# Guardar resultados
writexl::write_xlsx(datos_completos, "POBLACION_IMPUTADA_JERARQUICA.xlsx")