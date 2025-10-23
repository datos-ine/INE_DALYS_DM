## Limpieza del dataset QUALIDIAB y cálculo peso discapacidad
## Autora: Tamara Ricardo
## Fecha de modificación:
# 2025-10-09 09:08:29

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  epikit,
  janitor,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## QUALIDIAB
datos_raw <- import("raw/fichas_pacientes_QUALIDIAB_solo_ARG_2014.xlsx")

glimpse(datos_raw)

tabyl(datos_raw$sexo)

tabyl(datos_raw$obito)

tabyl(datos_raw$pais)

tabyl(datos_raw$antecedentes_dm2)


# Limpiar datos QUALIDIAB ------------------------------------------------
datos <- datos_raw |>
  # Renombrar columnas
  rename_with(
    .cols = starts_with("comp"),
    .fn = ~ str_remove_all(.x, "complicaciones_")
  ) |>

  # Filtrar casos DM1
  filter(antecedentes_dm2 == 1) |>

  # Cambiar etiquetas sexo
  mutate(sexo = if_else(sexo == 0, "Masculino", "Femenino")) |>

  # Calcular edad y grupo etario
  mutate(edad = year(registro_fecha) - year(fecha_de_nacimiento)) |>

  mutate(grupo_edad = age_categories(edad, lower = 0, upper = 80, by = 5)) |>

  # Filtrar menores de 30 años
  filter(edad >= 35) |>

  # Calcular frecuencia de complicaciones
  rowwise() |>
  group_by(grupo_edad, sexo) |>
  summarise(
    across(
      .cols = c(ceguera:ojos_retinopatia_proliferativa),
      .fns = ~ sum(.x, na.rm = TRUE) / n()
    ),
    .groups = "drop"
  )

# Nombres de columnas
names(datos)

# # Limpiar datos dw -------------------------------------------------------
# dw <- dw_raw |>
#   # Estandarizar nombres de columnas
#   clean_names() |>

#   # Subdividir columna dw
#   separate(col = disability_weight, into = c("dw", "ci"), sep = "\r\n") |>

#   # Separar ci en upper y lower
#   separate(col = ci, into = c("ci_low", "ci_upp"), sep = "-") |>

#   # Convertir a numérico
#   mutate(across(
#     .cols = c(dw, ci_low, ci_upp),
#     .fns = ~ round(parse_number(.x), 3)
#   )) #|>

#   # Seleccionar filas relevantes
#   filter(str_detect( sequela, "diabetes mellitus type 2|claud"))
