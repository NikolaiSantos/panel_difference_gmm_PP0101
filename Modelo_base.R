# Cargar las librerías necesarias
library(plm)    # Para modelos de datos de panel
library(dplyr)  # Para manipulación de datos
library(readxl) # Para cargar el dataset
library(ggplot2) # Para graficar el efecto

# Cargar dataset
datos <- read_excel("Dataset_DISTRITAL.xlsx", sheet = "Hoja1")
View(datos)

############################################
### MODELO 1 - TODAS LAS MUNICIPALIDADES ###
############################################

# Paso 1: Filtrado de datos

# Primer filtrado: Mantener distritos con al menos 4 observaciones
distritos_minimos <- datos %>%
  group_by(ID2) %>%
  summarise(anios_observados = n_distinct(AÑO)) %>%
  filter(anios_observados >= 4)  # Al menos 4 años para GMM

# Aplicar el nuevo filtro
datos_m1 <- datos %>%
  filter(ID2 %in% distritos_minimos$ID2) %>%
  select(ID2, AÑO, CICLO_ELECTORAL, NIVEL_EJECUCION_PP0101)

# Paso 2: Preparación de variables

# Quitar variables innecesarias
datos_m1 <- datos_m1 %>%
  select(ID2, AÑO, CICLO_ELECTORAL, NIVEL_EJECUCION_PP0101)

# Crear variables dummy para el ciclo electoral (usando año 1 como referencia)
datos_m1 <- datos_m1 %>%
  mutate(CICLO_ELECTORAL_2 = ifelse(CICLO_ELECTORAL == 2, 1, 0),
         CICLO_ELECTORAL_3 = ifelse(CICLO_ELECTORAL == 3, 1, 0),
         CICLO_ELECTORAL_4 = ifelse(CICLO_ELECTORAL == 4, 1, 0))

# Paso 3: Convertir a formato de datos de panel
# Ordenar por ID2 y AÑO
panel_m1 <- datos_m1 %>%
  arrange(ID2, AÑO)

# Convertir a panel data
panel_m1 <- pdata.frame(panel_m1, 
                        index = c("ID2", "AÑO"),
                        drop.index = FALSE)

# Paso 4: Estimación del modelo GMM en Niveles y Diferencias (System GMM)

# Primero, verifiquemos que los datos estén ordenados correctamente
panel_m1 <- panel_m1[order(panel_m1$ID2, panel_m1$AÑO), ]

# Estimar el modelo GMM con la sintaxis correcta
modelo_m1 <- pgmm(
  # Especificación del modelo usando la sintaxis correcta para pgmm
  formula = NIVEL_EJECUCION_PP0101 ~ lag(NIVEL_EJECUCION_PP0101, 1) + 
    CICLO_ELECTORAL_2 + CICLO_ELECTORAL_3 + CICLO_ELECTORAL_4 |
    lag(NIVEL_EJECUCION_PP0101, 2:3),
  
  # Datos en formato panel
  data = panel_m1,
  
  # Tipo de modelo GMM
  effect = "individual",  # Efectos fijos individuales
  model = "twosteps",     # Estimación en dos pasos
  transformation = "ld"    # Transformación en niveles y diferencias (System GMM)
)

# Paso 5: Presentación de resultados

# Mostrar resumen del modelo
print("===== RESUMEN DEL MODELO GMM =====")
print(summary(modelo_m1))

# Extraer y mostrar los coeficientes clave
coeficientes_m1 <- coef(summary(modelo_m1))
print("===== COEFICIENTES CLAVE =====")
print(coeficientes_m1[c("CICLO_ELECTORAL_2", "CICLO_ELECTORAL_3", "CICLO_ELECTORAL_4"), ])

# Paso 6: Interpretación de los resultados para las hipótesis

# Calcular los efectos marginales del ciclo electoral
ciclo_2_m1 <- coeficientes_m1["CICLO_ELECTORAL_2", "Estimate"]
ciclo_3_m1 <- coeficientes_m1["CICLO_ELECTORAL_3", "Estimate"]
ciclo_4_m1 <- coeficientes_m1["CICLO_ELECTORAL_4", "Estimate"]

print("===== INTERPRETACIÓN PARA LAS HIPÓTESIS =====")
cat("Efecto del segundo año de gestión (preelectoral-2):", round(ciclo_2_m1, 4), "\n")
cat("Efecto del tercer año de gestión (preelectoral):", round(ciclo_3_m1, 4), "\n")
cat("Efecto del cuarto año de gestión (electoral):", round(ciclo_4_m1, 4), "\n")
cat("Según HE1, se espera que ciclo_3_m1 y ciclo_4_m1 sean positivos y significativamente mayores que 0\n")

# Paso 8: Visualización de los resultados
# Preparar datos para gráfico
coef_data_m1 <- data.frame(
  Ciclo = c("Segundo Año", "Tercer Año (Preelectoral)", "Cuarto Año (Electoral)"),
  Efecto = c(ciclo_2_m1, ciclo_3_m1, ciclo_4_m1),
  Error = c(coeficientes_m1["CICLO_ELECTORAL_2", "Std. Error"],
            coeficientes_m1["CICLO_ELECTORAL_3", "Std. Error"],
            coeficientes_m1["CICLO_ELECTORAL_4", "Std. Error"])
)

# Convertir Ciclo a factor con el orden deseado
coef_data_m1$Ciclo <- factor(coef_data_m1$Ciclo, 
                             levels = c("Segundo Año", 
                                        "Tercer Año (Preelectoral)", 
                                        "Cuarto Año (Electoral)"))

# Crear gráfico de barras con intervalos de confianza
ggplot(coef_data_m1, aes(x = Ciclo, y = Efecto)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Efecto - 1.96*Error, ymax = Efecto + 1.96*Error), 
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Efecto del Ciclo Electoral en el Nivel de Ejecución del PP0101:",
  subtitle = "Todas las muncipalidades del Perú",
       y = "Efecto Marginal",
       x = "Fase del Ciclo Electoral") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

###########################################################################
### MODELO 2 - DIFERENCIANDO MUNICIPALIDADES PROVINCIALES Y DISTRITALES ###
###########################################################################

# Paso 1: Filtrado de datos

# Primer filtrado: Mantener distritos con al menos 4 observaciones
distritos_minimos <- datos %>%
  group_by(ID2) %>%
  summarise(anios_observados = n_distinct(AÑO)) %>%
  filter(anios_observados >= 4)  # Al menos 4 años para GMM

# Aplicar el nuevo filtro
datos_m2 <- datos %>%
  filter(ID2 %in% distritos_minimos$ID2)

# Paso 2: Preparación de variables

# Convertir la variable 'AREA POLITICO-ADMINISTRATIVA' en factor
datos_m2$LIMACALLAO <- ifelse(
  datos_m2$'AREA POLITICO-ADMINISTRATIVA' == "PROVINCIAS", 0, 1)

# Quitar variables innecesarias
datos_m2 <- datos_m2 %>%
  select(ID2, AÑO, UBIGEO, LIMACALLAO, CICLO_ELECTORAL, NIVEL_EJECUCION_PP0101)

# Crear variables dummy para el ciclo electoral (usando año 1 como referencia)
datos_m2 <- datos_m2 %>%
  mutate(CICLO_ELECTORAL_2 = ifelse(CICLO_ELECTORAL == 2, 1, 0),
         CICLO_ELECTORAL_3 = ifelse(CICLO_ELECTORAL == 3, 1, 0),
         CICLO_ELECTORAL_4 = ifelse(CICLO_ELECTORAL == 4, 1, 0))

# Paso 3: Convertir a formato de datos de panel
# Ordenar por ID2 y AÑO
panel_m2 <- datos_m2 %>%
  arrange(ID2, AÑO)

# Convertir a panel data
panel_m2 <- pdata.frame(panel_m2, 
                        index = c("ID2", "AÑO"),
                        drop.index = FALSE)

# Dividir dataframes
panel_m21 <- panel_m2 %>% filter(stringr::str_detect(UBIGEO, "01$"))
panel_m22 <- panel_m2 %>% filter(!stringr::str_detect(UBIGEO, "01$"))

#########################################################
### MODELO 2.1 - MODELO PARA MUNICIPALES PROVINCIALES ###

# Paso 4: Estimación del modelo GMM en Niveles y Diferencias (System GMM)

# Primero, verifiquemos que los datos estén ordenados correctamente
panel_m21 <- panel_m21[order(panel_m21$ID2, panel_m21$AÑO), ]

# Estimar el modelo GMM con la sintaxis correcta
modelo_m21 <- pgmm(
  # Especificación del modelo usando la sintaxis correcta para pgmm
  formula = NIVEL_EJECUCION_PP0101 ~ lag(NIVEL_EJECUCION_PP0101, 1) + 
    CICLO_ELECTORAL_2 + CICLO_ELECTORAL_3 + CICLO_ELECTORAL_4 |
    lag(NIVEL_EJECUCION_PP0101, 2:3),
  
  # Datos en formato panel
  data = panel_m21,
  
  # Tipo de modelo GMM
  effect = "individual",  # Efectos fijos individuales
  model = "twosteps",     # Estimación en dos pasos
  transformation = "ld"    # Transformación en niveles y diferencias (System GMM)
)

# Paso 5: Presentación de resultados

# Mostrar resumen del modelo
print("===== RESUMEN DEL MODELO GMM =====")
print(summary(modelo_m21))

# Extraer y mostrar los coeficientes clave
coeficientes_m21 <- coef(summary(modelo_m21))
print("===== COEFICIENTES CLAVE =====")
print(coeficientes_m21[c("CICLO_ELECTORAL_2", "CICLO_ELECTORAL_3", "CICLO_ELECTORAL_4"), ])

# Paso 6: Interpretación de los resultados para las hipótesis

# Calcular los efectos marginales del ciclo electoral
ciclo_2_m21 <- coeficientes_m21["CICLO_ELECTORAL_2", "Estimate"]
ciclo_3_m21 <- coeficientes_m21["CICLO_ELECTORAL_3", "Estimate"]
ciclo_4_m21 <- coeficientes_m21["CICLO_ELECTORAL_4", "Estimate"]

print("===== INTERPRETACIÓN PARA LAS HIPÓTESIS =====")
cat("Efecto del segundo año de gestión (preelectoral-2):", round(ciclo_2_m21, 4), "\n")
cat("Efecto del tercer año de gestión (preelectoral):", round(ciclo_3_m21, 4), "\n")
cat("Efecto del cuarto año de gestión (electoral):", round(ciclo_4_m21, 4), "\n")
cat("Según HE1, se espera que ciclo_3_m21 y ciclo_4_m21 sean positivos y significativamente mayores que 0\n")

# Paso 8: Visualización de los resultados
# Preparar datos para gráfico
coef_data_m21 <- data.frame(
  Ciclo = c("Segundo Año", "Tercer Año (Preelectoral)", "Cuarto Año (Electoral)"),
  Efecto = c(ciclo_2_m21, ciclo_3_m21, ciclo_4_m21),
  Error = c(coeficientes_m21["CICLO_ELECTORAL_2", "Std. Error"],
            coeficientes_m21["CICLO_ELECTORAL_3", "Std. Error"],
            coeficientes_m21["CICLO_ELECTORAL_4", "Std. Error"])
)

# Convertir Ciclo a factor con el orden deseado
coef_data_m21$Ciclo <- factor(coef_data_m21$Ciclo, 
                             levels = c("Segundo Año", 
                                        "Tercer Año (Preelectoral)", 
                                        "Cuarto Año (Electoral)"))

# Crear gráfico de barras con intervalos de confianza
ggplot(coef_data_m21, aes(x = Ciclo, y = Efecto)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Efecto - 1.96*Error, ymax = Efecto + 1.96*Error), 
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Efecto del Ciclo Electoral en el Nivel de Ejecución del PP0101:",
       subtitle = "Todas las municipalidades provinciales del Perú",
       y = "Efecto Marginal",
       x = "Fase del Ciclo Electoral") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

############################################################
### MODELO 2.2 - MODELO PARA MUNICIPALES DISTRITALES #######

# Paso 4: Estimación del modelo GMM en Niveles y Diferencias (System GMM)

# Primero, verifiquemos que los datos estén ordenados correctamente
panel_m22 <- panel_m22[order(panel_m22$ID2, panel_m22$AÑO), ]

# Estimar el modelo GMM con la sintaxis correcta
modelo_m22 <- pgmm(
  # Especificación del modelo usando la sintaxis correcta para pgmm
  formula = NIVEL_EJECUCION_PP0101 ~ lag(NIVEL_EJECUCION_PP0101, 1) + 
    CICLO_ELECTORAL_2 + CICLO_ELECTORAL_3 + CICLO_ELECTORAL_4 |
    lag(NIVEL_EJECUCION_PP0101, 2:3),
  
  # Datos en formato panel
  data = panel_m22,
  
  # Tipo de modelo GMM
  effect = "individual",  # Efectos fijos individuales
  model = "twosteps",     # Estimación en dos pasos
  transformation = "ld"    # Transformación en niveles y diferencias (System GMM)
)

# Paso 5: Presentación de resultados

# Mostrar resumen del modelo
print("===== RESUMEN DEL MODELO GMM =====")
print(summary(modelo_m22))

# Extraer y mostrar los coeficientes clave
coeficientes_m22 <- coef(summary(modelo_m22))
print("===== COEFICIENTES CLAVE =====")
print(coeficientes_m22[c("CICLO_ELECTORAL_2", "CICLO_ELECTORAL_3", "CICLO_ELECTORAL_4"), ])

# Paso 6: Interpretación de los resultados para las hipótesis

# Calcular los efectos marginales del ciclo electoral
ciclo_2_m22 <- coeficientes_m22["CICLO_ELECTORAL_2", "Estimate"]
ciclo_3_m22 <- coeficientes_m22["CICLO_ELECTORAL_3", "Estimate"]
ciclo_4_m22 <- coeficientes_m22["CICLO_ELECTORAL_4", "Estimate"]

print("===== INTERPRETACIÓN PARA LAS HIPÓTESIS =====")
cat("Efecto del segundo año de gestión (preelectoral-2):", round(ciclo_2_m22, 4), "\n")
cat("Efecto del tercer año de gestión (preelectoral):", round(ciclo_3_m22, 4), "\n")
cat("Efecto del cuarto año de gestión (electoral):", round(ciclo_4_m22, 4), "\n")
cat("Según HE1, se espera que ciclo_3_m22 y ciclo_4_m22 sean positivos y significativamente mayores que 0\n")

# Paso 8: Visualización de los resultados
# Preparar datos para gráfico
coef_data_m22 <- data.frame(
  Ciclo = c("Segundo Año", "Tercer Año (Preelectoral)", "Cuarto Año (Electoral)"),
  Efecto = c(ciclo_2_m22, ciclo_3_m22, ciclo_4_m22),
  Error = c(coeficientes_m22["CICLO_ELECTORAL_2", "Std. Error"],
            coeficientes_m22["CICLO_ELECTORAL_3", "Std. Error"],
            coeficientes_m22["CICLO_ELECTORAL_4", "Std. Error"])
)

# Convertir Ciclo a factor con el orden deseado
coef_data_m22$Ciclo <- factor(coef_data_m22$Ciclo, 
                              levels = c("Segundo Año", 
                                         "Tercer Año (Preelectoral)", 
                                         "Cuarto Año (Electoral)"))

# Crear gráfico de barras con intervalos de confianza
ggplot(coef_data_m22, aes(x = Ciclo, y = Efecto)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Efecto - 1.96*Error, ymax = Efecto + 1.96*Error), 
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Efecto del Ciclo Electoral en el Nivel de Ejecución del PP0101:",
       subtitle = "Todas la municipalidades distritales del Perú",
       y = "Efecto Marginal",
       x = "Fase del Ciclo Electoral") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

###############################################################
### MODELO 3 - DIFERENCIANDO LIMA-CALLAO DEL RESTO DEL PAIS ###
###############################################################

# Paso 1: Filtrado de datos

# Primer filtrado: Mantener distritos con al menos 4 observaciones
distritos_minimos <- datos %>%
  group_by(ID2) %>%
  summarise(anios_observados = n_distinct(AÑO)) %>%
  filter(anios_observados >= 4)  # Al menos 4 años para GMM

# Aplicar el nuevo filtro
datos_m3 <- datos %>%
  filter(ID2 %in% distritos_minimos$ID2)

# Paso 2: Preparación de variables

# Convertir la variable 'AREA POLITICO-ADMINISTRATIVA' en factor
datos_m3$LIMACALLAO <- ifelse(
  datos_m3$'AREA POLITICO-ADMINISTRATIVA' == "PROVINCIAS", 0, 1)

# Quitar variables innecesarias
datos_m3 <- datos_m3 %>%
  select(ID2, AÑO, LIMACALLAO, CICLO_ELECTORAL, NIVEL_EJECUCION_PP0101)

# Crear variables dummy para el ciclo electoral (usando año 1 como referencia)
datos_m3 <- datos_m3 %>%
  mutate(CICLO_ELECTORAL_2 = ifelse(CICLO_ELECTORAL == 2, 1, 0),
         CICLO_ELECTORAL_3 = ifelse(CICLO_ELECTORAL == 3, 1, 0),
         CICLO_ELECTORAL_4 = ifelse(CICLO_ELECTORAL == 4, 1, 0))

# Paso 3: Convertir a formato de datos de panel
# Ordenar por ID2 y AÑO
panel_m3 <- datos_m3 %>%
  arrange(ID2, AÑO)

# Convertir a panel data
panel_m3 <- pdata.frame(panel_m3, 
                        index = c("ID2", "AÑO"),
                        drop.index = FALSE)

# Dividir dataframes
panel_m31 <- panel_m3 %>% filter(LIMACALLAO == 1)
panel_m32 <- panel_m3 %>% filter(LIMACALLAO == 0)

############################################################
### MODELO 3.1 - MODELO PARA LIMA METROPOLITANA Y CALLAO ###

# Paso 4: Estimación del modelo GMM en Niveles y Diferencias (System GMM)

# Primero, verifiquemos que los datos estén ordenados correctamente
panel_m31 <- panel_m31[order(panel_m31$ID2, panel_m31$AÑO), ]

# Estimar el modelo GMM con la sintaxis correcta
modelo_m31 <- pgmm(
  # Especificación del modelo usando la sintaxis correcta para pgmm
  formula = NIVEL_EJECUCION_PP0101 ~ lag(NIVEL_EJECUCION_PP0101, 1) + 
    CICLO_ELECTORAL_2 + CICLO_ELECTORAL_3 + CICLO_ELECTORAL_4 |
    lag(NIVEL_EJECUCION_PP0101, 2:3),
  
  # Datos en formato panel
  data = panel_m31,
  
  # Tipo de modelo GMM
  effect = "individual",  # Efectos fijos individuales
  model = "twosteps",     # Estimación en dos pasos
  transformation = "ld"    # Transformación en niveles y diferencias (System GMM)
)

# Paso 5: Presentación de resultados

# Mostrar resumen del modelo
print("===== RESUMEN DEL MODELO GMM =====")
print(summary(modelo_m31))

# Extraer y mostrar los coeficientes clave
coeficientes_m31 <- coef(summary(modelo_m31))
print("===== COEFICIENTES CLAVE =====")
print(coeficientes_m31[c("CICLO_ELECTORAL_2", "CICLO_ELECTORAL_3", "CICLO_ELECTORAL_4"), ])

# Paso 6: Interpretación de los resultados para las hipótesis

# Calcular los efectos marginales del ciclo electoral
ciclo_2_m31 <- coeficientes_m31["CICLO_ELECTORAL_2", "Estimate"]
ciclo_3_m31 <- coeficientes_m31["CICLO_ELECTORAL_3", "Estimate"]
ciclo_4_m31 <- coeficientes_m31["CICLO_ELECTORAL_4", "Estimate"]

print("===== INTERPRETACIÓN PARA LAS HIPÓTESIS =====")
cat("Efecto del segundo año de gestión (preelectoral-2):", round(ciclo_2_m31, 4), "\n")
cat("Efecto del tercer año de gestión (preelectoral):", round(ciclo_3_m31, 4), "\n")
cat("Efecto del cuarto año de gestión (electoral):", round(ciclo_4_m31, 4), "\n")
cat("Según HE1, se espera que ciclo_3_m31 y ciclo_4_m31 sean positivos y significativamente mayores que 0\n")

# Paso 8: Visualización de los resultados
# Preparar datos para gráfico
coef_data_m31 <- data.frame(
  Ciclo = c("Segundo Año", "Tercer Año (Preelectoral)", "Cuarto Año (Electoral)"),
  Efecto = c(ciclo_2_m31, ciclo_3_m31, ciclo_4_m31),
  Error = c(coeficientes_m31["CICLO_ELECTORAL_2", "Std. Error"],
            coeficientes_m31["CICLO_ELECTORAL_3", "Std. Error"],
            coeficientes_m31["CICLO_ELECTORAL_4", "Std. Error"])
)

# Convertir Ciclo a factor con el orden deseado
coef_data_m31$Ciclo <- factor(coef_data_m31$Ciclo, 
                              levels = c("Segundo Año", 
                                         "Tercer Año (Preelectoral)", 
                                         "Cuarto Año (Electoral)"))

# Crear gráfico de barras con intervalos de confianza
ggplot(coef_data_m31, aes(x = Ciclo, y = Efecto)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Efecto - 1.96*Error, ymax = Efecto + 1.96*Error), 
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Efecto del Ciclo Electoral en el Nivel de Ejecución del PP0101:",
       subtitle = "Todas las municipalidades de Lima Metropolitana y Callao",
       y = "Efecto Marginal",
       x = "Fase del Ciclo Electoral") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

########################################################
### MODELO 3.2 - MODELO PARA DISTRITOS DE PROVINCIAS ###

# Paso 4: Estimación del modelo GMM en Niveles y Diferencias (System GMM)

# Primero, verifiquemos que los datos estén ordenados correctamente
panel_m32 <- panel_m32[order(panel_m32$ID2, panel_m32$AÑO), ]

# Estimar el modelo GMM con la sintaxis correcta
modelo_m32 <- pgmm(
  # Especificación del modelo usando la sintaxis correcta para pgmm
  formula = NIVEL_EJECUCION_PP0101 ~ lag(NIVEL_EJECUCION_PP0101, 1) + 
    CICLO_ELECTORAL_2 + CICLO_ELECTORAL_3 + CICLO_ELECTORAL_4 |
    lag(NIVEL_EJECUCION_PP0101, 2:3),
  
  # Datos en formato panel
  data = panel_m32,
  
  # Tipo de modelo GMM
  effect = "individual",  # Efectos fijos individuales
  model = "twosteps",     # Estimación en dos pasos
  transformation = "ld"    # Transformación en niveles y diferencias (System GMM)
)

# Paso 5: Presentación de resultados

# Mostrar resumen del modelo
print("===== RESUMEN DEL MODELO GMM =====")
print(summary(modelo_m32))

# Extraer y mostrar los coeficientes clave
coeficientes_m32 <- coef(summary(modelo_m32))
print("===== COEFICIENTES CLAVE =====")
print(coeficientes_m32[c("CICLO_ELECTORAL_2", "CICLO_ELECTORAL_3", "CICLO_ELECTORAL_4"), ])

# Paso 6: Interpretación de los resultados para las hipótesis

# Calcular los efectos marginales del ciclo electoral
ciclo_2_m32 <- coeficientes_m32["CICLO_ELECTORAL_2", "Estimate"]
ciclo_3_m32 <- coeficientes_m32["CICLO_ELECTORAL_3", "Estimate"]
ciclo_4_m32 <- coeficientes_m32["CICLO_ELECTORAL_4", "Estimate"]

print("===== INTERPRETACIÓN PARA LAS HIPÓTESIS =====")
cat("Efecto del segundo año de gestión (preelectoral-2):", round(ciclo_2_m32, 4), "\n")
cat("Efecto del tercer año de gestión (preelectoral):", round(ciclo_3_m32, 4), "\n")
cat("Efecto del cuarto año de gestión (electoral):", round(ciclo_4_m32, 4), "\n")
cat("Según HE1, se espera que ciclo_3_m32 y ciclo_4_m32 sean positivos y significativamente mayores que 0\n")

# Paso 8: Visualización de los resultados
# Preparar datos para gráfico
coef_data_m32 <- data.frame(
  Ciclo = c("Segundo Año", "Tercer Año (Preelectoral)", "Cuarto Año (Electoral)"),
  Efecto = c(ciclo_2_m32, ciclo_3_m32, ciclo_4_m32),
  Error = c(coeficientes_m32["CICLO_ELECTORAL_2", "Std. Error"],
            coeficientes_m32["CICLO_ELECTORAL_3", "Std. Error"],
            coeficientes_m32["CICLO_ELECTORAL_4", "Std. Error"])
)

# Convertir Ciclo a factor con el orden deseado
coef_data_m32$Ciclo <- factor(coef_data_m32$Ciclo, 
                              levels = c("Segundo Año", 
                                         "Tercer Año (Preelectoral)", 
                                         "Cuarto Año (Electoral)"))

# Crear gráfico de barras con intervalos de confianza
ggplot(coef_data_m32, aes(x = Ciclo, y = Efecto)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Efecto - 1.96*Error, ymax = Efecto + 1.96*Error), 
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Efecto del Ciclo Electoral en el Nivel de Ejecución del PP0101:",
       subtitle = "Todas las municipalidades fuera de Lima Metropolitana y Callao",
       y = "Efecto Marginal",
       x = "Fase del Ciclo Electoral") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
