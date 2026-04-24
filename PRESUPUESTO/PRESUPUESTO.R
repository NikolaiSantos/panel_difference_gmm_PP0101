library(writexl)
library(dplyr)
library(tidyr)

#PRESUPUESTO <- readRDS("E:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PRESUPUESTO/PRESUPUESTO.rds")
PRESUPUESTO_filtrado <- readRDS("/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PRESUPUESTO/PRESUPUESTO_FILTRADO.rds")
gc()

# ---------------------------
# Limpieza de datos
# ---------------------------
PRESUPUESTO <- PRESUPUESTO %>%
  filter(
    MONTO_DEVENGADO_ANUAL != 0,
    !is.na(MONTO_DEVENGADO_ANUAL))

PRESUPUESTO <- PRESUPUESTO %>%
  mutate(DEPARTAMENTO_EJECUTORA_NOMBRE = ifelse(DEPARTAMENTO_EJECUTORA_NOMBRE == "PROVINCIA CONSTITUCIONAL DEL CALLAO", 
                                                "CALLAO", 
                                                DEPARTAMENTO_EJECUTORA_NOMBRE))
gc()

columnas_a_mantener <- c(
  "ANO_EJE", "DEPARTAMENTO_EJECUTORA_NOMBRE", "PROVINCIA_EJECUTORA_NOMBRE", 
  "DISTRITO_EJECUTORA_NOMBRE", "FUENTE_FINANCIAMIENTO_NOMBRE", "RUBRO_NOMBRE", 
  "MONTO_DEVENGADO_ANUAL"
)

PRESUPUESTO <- PRESUPUESTO %>%
  select(all_of(columnas_a_mantener))
gc()

# ---------------------------
# Exportar
# ---------------------------
#write_xlsx(PRESUPUESTO, "E:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PRESUPUESTO/PRESUPUESTO_FILTRADO.xlsx")

# Opcional: Guardar el resultado en un archivo
saveRDS(PRESUPUESTO, "E:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PRESUPUESTO/PRESUPUESTO_FILTRADO.rds")


# ---------------------------
# Tablas
# ---------------------------
# Crear la tabla con ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE y suma de MONTO_DEVENGADO_ANUAL
PRESUPUESTO_ANUAL_MUNICIPIOS_DEPARTAMENTO <- PRESUPUESTO_filtrado %>%
                                              group_by(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE) %>%
                                              summarise(
                                                SUMA_MONTO_DEVENGADO = sum(MONTO_DEVENGADO_ANUAL, na.rm = TRUE),
                                                .groups = "drop"  # Mantiene el resultado como dataframe regular
                                              ) %>%
                                              arrange(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE)  # Ordenar para mejor visualización

write_xlsx(PRESUPUESTO_ANUAL_MUNICIPIOS_DEPARTAMENTO, "/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PRESUPUESTO/PRESUPUESTO_ANUAL_MUNICIPIOS_DEPARTAMENTO.xlsx")

# Crear el dataframe PRESUPUESTO POR RUBRO
PRESUPUESTO_RUBRO <- PRESUPUESTO_filtrado %>%
  select(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, RUBRO_NOMBRE, MONTO_DEVENGADO_ANUAL) %>%
  pivot_wider(
    names_from = RUBRO_NOMBRE,
    values_from = MONTO_DEVENGADO_ANUAL,
    values_fn = sum,
    values_fill = 0
  )

write_xlsx(PRESUPUESTO_RUBRO, "/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PRESUPUESTO/PRESUPUESTO_RUBRO.xlsx")


