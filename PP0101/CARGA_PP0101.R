# Cargar librerías necesarias
library(dplyr)
library(readr)
library(data.table)
library(writexl)
library(tidyr)

# ---------------------------
# Configurar la ruta y listar archivos
# ---------------------------
ruta_carpeta <- "E:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101/" #"/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101"
archivos_csv <- list.files(
  path = ruta_carpeta,
  pattern = "\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Verificar que se encontraron archivos
if (length(archivos_csv) == 0) {
  stop("No se encontraron archivos .csv en la ruta especificada")
}

# ---------------------------
# Cargar y combinar todos los archivos
# ---------------------------
PP0101 <- rbindlist(lapply(archivos_csv, fread), fill = TRUE)

# ---------------------------
# Verificar el resultado
# ---------------------------
cat("Número de archivos procesados:", length(archivos_csv), "\n")
cat("Número total de filas:", nrow(PP0101), "\n")
cat("Número de columnas:", ncol(PP0101), "\n")

# ---------------------------
# Limpieza de datos
# ---------------------------
PP0101 <- PP0101 %>%
  mutate(DEPARTAMENTO_EJECUTORA_NOMBRE = ifelse(DEPARTAMENTO_EJECUTORA_NOMBRE == "PROVINCIA CONSTITUCIONAL DEL CALLAO", 
                               "CALLAO", 
                               DEPARTAMENTO_EJECUTORA_NOMBRE))
gc()

# ---------------------------
# Dataframe de devengado
# ---------------------------
PP0101_DEVENGADO <- PP0101 %>%
  filter(
#    MONTO_DEVENGADO != 0,
#    !is.na(MONTO_DEVENGADO),
    NIVEL_GOBIERNO_NOMBRE == "GOBIERNOS LOCALES"
    )

columnas_a_mantener <- c(
  "ANO_EJE", "DEPARTAMENTO_EJECUTORA_NOMBRE", 
  "PROVINCIA_EJECUTORA_NOMBRE", "DISTRITO_EJECUTORA_NOMBRE",
  "RUBRO_NOMBRE", "GRUPO_FUNCIONAL_NOMBRE", 
  "MONTO_DEVENGADO"
)

PP0101_DEVENGADO <- PP0101_DEVENGADO %>%
                    select(all_of(columnas_a_mantener))

PP0101_DEVENGADO_TOTAL <- PP0101_DEVENGADO %>%
  select(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, 
         PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE, 
         MONTO_DEVENGADO)%>%
  group_by(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, 
           PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE) %>%
  summarise(
    MONTO_DEVENGADO = sum(MONTO_DEVENGADO, na.rm = TRUE)
  ) %>%
  arrange(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, 
          PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE)  # Ordenar para mejor visualización

#write_xlsx(PP0101_DEVENGADO_TOTAL, "/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101/PP0101_DEVENGADO_TOTAL.xlsx")
write_xlsx(PP0101_DEVENGADO_TOTAL, "E:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101/PP0101_DEVENGADO_TOTAL.xlsx")

PP0101_DEVENGADO_RUBRO <- PP0101_DEVENGADO %>%
  select(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, 
         PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE, 
         RUBRO_NOMBRE, MONTO_DEVENGADO)%>%
  pivot_wider(
    names_from = RUBRO_NOMBRE,
    values_from = MONTO_DEVENGADO,
    values_fn = sum,
    values_fill = 0
  )

#write_xlsx(PP0101_DEVENGADO_RUBRO, "/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101/PP0101_DEVENGADO_RUBRO.xlsx") 
write_xlsx(PP0101_DEVENGADO_RUBRO, "E:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101/PP0101_DEVENGADO_RUBRO.xlsx")

PP0101_DEVENGADO_GF <- PP0101_DEVENGADO %>%
  select(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, 
         PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE, 
         GRUPO_FUNCIONAL_NOMBRE, MONTO_DEVENGADO)%>%
  pivot_wider(
    names_from = GRUPO_FUNCIONAL_NOMBRE,
    values_from = MONTO_DEVENGADO,
    values_fn = sum,
    values_fill = 0
  )

#write_xlsx(PP0101_DEVENGADO_GF, "/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101/PP0101_DEVENGADO_GF.xlsx")
write_xlsx(PP0101_DEVENGADO_GF, "E:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101/PP0101_DEVENGADO_GF.xlsx")

# ---------------------------
# Dataframe de PIM
# ---------------------------
PP0101_PIM <- PP0101 %>%
  filter(
#    MONTO_PIM != 0,
#    !is.na(MONTO_PIM),
    NIVEL_GOBIERNO_NOMBRE == "GOBIERNOS LOCALES"
  )

columnas_a_mantener <- c(
  "ANO_EJE", "DEPARTAMENTO_EJECUTORA_NOMBRE", 
  "PROVINCIA_EJECUTORA_NOMBRE", "DISTRITO_EJECUTORA_NOMBRE", 
  "RUBRO_NOMBRE", "GRUPO_FUNCIONAL_NOMBRE", 
  "MONTO_PIM"
)

PP0101_PIM <- PP0101_PIM %>%
  select(all_of(columnas_a_mantener))

PP0101_PIM_TOTAL <- PP0101_PIM %>%
  select(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, 
         PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE, 
         MONTO_PIM)%>%
  group_by(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, 
           PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE) %>%
  summarise(
    MONTO_PIM = sum(MONTO_PIM, na.rm = TRUE)
  ) %>%
  arrange(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, 
          PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE)  # Ordenar para mejor visualización

#write_xlsx(PP0101_PIM_TOTAL, "/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101/PP0101_PIM_TOTAL.xlsx") 
write_xlsx(PP0101_PIM_TOTAL, "E:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101/PP0101_PIM_TOTAL.xlsx")

PP0101_PIM_RUBRO <- PP0101_PIM %>%
  select(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, 
         PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE, 
         RUBRO_NOMBRE, MONTO_PIM)%>%
  pivot_wider(
    names_from = RUBRO_NOMBRE,
    values_from = MONTO_PIM,
    values_fn = sum,
    values_fill = 0
  )

#write_xlsx(PP0101_PIM_RUBRO, "/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101/PP0101_PIM_RUBRO.xlsx")
write_xlsx(PP0101_PIM_RUBRO, "E:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101/PP0101_PIM_RUBRO.xlsx")

PP0101_PIM_GF <- PP0101_PIM %>%
  select(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, 
         PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE, 
         GRUPO_FUNCIONAL_NOMBRE, MONTO_PIM)%>%
  pivot_wider(
    names_from = GRUPO_FUNCIONAL_NOMBRE,
    values_from = MONTO_PIM,
    values_fn = sum,
    values_fill = 0
  )

#write_xlsx(PP0101_PIM_GF, "/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101/PP0101_PIM_GF.xlsx")
write_xlsx(PP0101_PIM_GF, "E:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PP0101/PP0101_PIM_GF.xlsx")