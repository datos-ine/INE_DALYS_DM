### Análisis espacial y tendencia de la carga de enfermedad por diabetes mellitus en Argentina,
### período 2005-2018.
# Limpieza de los datasets de mortalidad anual por provincia, sexo y grupo etario (DEIS).
# Limpieza del dataset de esperanza de vida por sexo y grupo etario (WHO) y recalculado de
# indicadores para grupos de edad ampliados.
# Cálculo de los AVP por provincia, sexo y grupo edad ampliado y por región, sexo y grupo de
# edad ampliado.
# Las regiones geográficas utilizadas corresponden a las definidas por la DEIS en el diccionario
# de datos del dataset de mortalidad
### Autora: Tamara Ricardo
### Fecha creación: # 2025-10-22 12:25:39
### Fecha modificación: # 2025-10-27 12:49:07

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  geoAr,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## Provincias
prov <- show_arg_codes() |>
  # Filtrar totales
  filter(between(codprov, "01", "24")) |>

  # Cambiar etiqueta CABA
  mutate(name_iso = if_else(codprov == "01", "CABA", name_iso))


## Esperanza vida
ex_raw <- read_csv2("raw/WHO/argentina_tabla de vida_GHO.csv", skip = 1)


## Mortalidad 2004
def04_raw <- import("raw/DEIS/DE_2004.csv")


## Mortalidad 2005-2018
def05_18_raw <- list.files(
  path = "raw/DEIS/",
  pattern = "^defweb.",
  full.names = TRUE
)


# Limpiar datos defunciones ----------------------------------------------
## Defunciones 2004 ----
def04 <- def04_raw |>
  # Estandarizar nombres de columnas
  clean_names() |>
  rename(
    prov_nombre = jurisdiccion,
    grupo_edad_5 = grupo_de_edad,
    cie10_causa = causa_de_muerte_cie_10
  ) |>

  # Filtrar datos ausentes provincia defunción
  filter(!prov_nombre %in% c("Lugar no especificado", "Otro país")) |>

  # Filtrar datos ausentes sexo
  filter(between(sexo, "Mujer", "Varón")) |>

  # Filtrar menores de edad y datos ausentes
  filter(between(grupo_edad_5, "11.20 a 24", "24.85 y más")) |>

  # Cambiar etiqueta CABA
  mutate(
    prov_nombre = if_else(
      str_detect(prov_nombre, "Ciudad"),
      "CABA",
      prov_nombre
    )
  ) |>

  # Añadir identificador numérico provincias
  mutate(
    prov_id = prov$codprov_censo[match(prov_nombre, prov$name_iso)],
    .before = prov_nombre
  )


## Defunciones 2005-2018 ----
def05_18 <- def05_18_raw |>
  # Crear columna para el año
  set_names(nm = c(2005, 2006, 2008:2010, 2012:2014, 2017:2019)) |>

  # Leer archivos csv
  map(read_csv, locale = locale(encoding = "WINDOWS-1252")) |>

  # Unir datasets
  list_rbind(names_to = "anio") |>

  # Estandarizar nombres de columnas
  clean_names() |>
  rename(
    prov_id = provres,
    grupo_edad_5 = grupedad,
    cie10_causa = causa,
    total = cuenta
  ) |>

  # Filtrar datos ausentes provincia
  filter(between(prov_id, "02", "94")) |>

  # Filtrar datos ausentes sexo
  filter(between(sexo, 1, 2)) |>

  # Filtrar menores de edad y datos ausentes
  filter(between(grupo_edad_5, "05_20 a 24", "17_80 y más")) |>

  # Cambiar niveles sexo
  mutate(sexo = if_else(sexo == 1, "Varón", "Mujer")) |>

  # Añadir identificador categórico provincias
  mutate(
    prov_nombre = prov$name_iso[match(prov_id, prov$codprov_censo)],
    .after = prov_id
  )


## Unir datasets defunciones ----
defun <- bind_rows(def04, def05_18) |>

  # Filtrar muertes por DM
  filter(between(cie10_causa, "E10", "E14")) |>

  # Crear región geográfica DEIS
  mutate(
    region = case_when(
      prov_id %in% c("02", "06", "14", "30", "82") ~ "Centro",
      prov_id %in% c("18", "22", "34", "54") ~ "NEA",
      prov_id %in% c("38", "66", "90") ~ "NOA1",
      prov_id %in% c("10", "86") ~ "NOA2",
      prov_id %in% c("46", "50", "70", "74") ~ "Cuyo",
      prov_id %in% c("42", "58", "62") ~ "Patagonia Norte",
      .default = "Patagonia Sur"
    ),
    .after = prov_nombre
  ) |>

  # Cambiar etiquetas grupo etario
  mutate(grupo_edad_5 = str_sub(grupo_edad_5, 4)) |>

  # Crear grupo edad ampliado
  mutate(
    grupo_edad_amp = case_when(
      between(grupo_edad_5, "20 a 24", "25 a 29") ~ "20 a 29",
      between(grupo_edad_5, "30 a 34", "40 a 44") ~ "30 a 44",
      between(grupo_edad_5, "45 a 49", "55 a 59") ~ "45 a 59",
      between(grupo_edad_5, "60 a 64", "70 a 74") ~ "60 a 74",
      .default = "75+"
    ),
    .after = grupo_edad_5
  ) |>

  # Completar datos faltantes año
  mutate(anio = replace_na(anio, "2004")) |>

  # Añadir año ENFR
  mutate(
    anio_enfr = case_when(
      between(anio, "2004", "2006") ~ "2005",
      between(anio, "2008", "2010") ~ "2009",
      between(anio, "2012", "2014") ~ "2013",
      between(anio, "2017", "2019") ~ "2018"
    ),
    .after = anio
  ) |>

  # Añadir filas faltantes
  complete(
    nesting(anio, anio_enfr),
    nesting(prov_id, prov_nombre, region),
    nesting(grupo_edad_5, grupo_edad_amp),
    sexo,
    fill = list(total = 0)
  ) |>

  # Agrupar datos
  count(
    anio,
    anio_enfr,
    prov_id,
    prov_nombre,
    region,
    grupo_edad_amp,
    sexo,
    wt = total
  )


# Limpiar datos esperanza de vida ----------------------------------------
ex_ge_amp <- ex_raw |>
  # Estandarizar nombres de columnas
  clean_names() |>
  select(
    indicator,
    grupo_edad_5 = age_group,
    "Varón" = male_4,
    "Mujer" = female_5
  ) |>

  # Filtrar menores de edad
  filter(
    between(grupo_edad_5, "20-24 years", "45-49 years") |
      between(grupo_edad_5, "50-54 years", "85+ years")
  ) |>

  # Cambiar niveles indicador
  mutate(indicator = str_extract(indicator, '^[^ ]+')) |>

  # Base long
  pivot_longer(cols = c(Varón, Mujer), names_to = "sexo") |>

  # Base wide
  pivot_wider(names_from = indicator, values_from = value) |>

  # Crear variable para grupo edad ampliado
  mutate(
    # Grupo etario ampliado
    grupo_edad_amp = case_when(
      between(grupo_edad_5, "20-24 years", "25-29 years") ~ "20 a 29",
      between(grupo_edad_5, "30-34 years", "40-44 years") ~ "30 a 44",
      between(grupo_edad_5, "45-49 years", "55-59 years") ~ "45 a 59",
      between(grupo_edad_5, "60-64 years", "70-74 years") ~ "60 a 74",
      .default = "75+"
    )
  ) |>

  # Recalcular indicadores por grupo ampliado
  group_by(sexo, grupo_edad_amp) |>
  summarise(
    lx = first(lx),
    nLx = sum(nLx, na.rm = TRUE),
    ndx = sum(ndx, na.rm = TRUE),
    nMx = sum(nMx * nLx, na.rm = TRUE) / sum(nLx, na.rm = TRUE),
    nqx = sum(nqx * nLx, na.rm = TRUE) / sum(nLx, na.rm = TRUE),
    .groups = "drop"
  ) |>

  # Calcular Tx y ex
  group_by(sexo) |>
  mutate(
    Tx = rev(cumsum(rev(nLx))),
    ex = Tx / lx
  ) |>
  ungroup()


# Calcular AVP -----------------------------------------------------------
## Por provincia, sexo y grupo ampliado edad
AVP_ge_amp <- defun |>
  # Calcular defunciones por trienio
  group_by(
    anio_enfr,
    prov_id,
    prov_nombre,
    region,
    grupo_edad_amp,
    sexo
  ) |>
  summarise(
    defun_n = sum(n, na.rm = TRUE),
    defun_mean = mean(n, na.rm = TRUE),
    .groups = "drop"
  ) |>

  # Añadir datos esperanza de vida
  left_join(ex_ge_amp) |>

  # Calcular AVP por grupo decenal
  mutate(avp_dm = defun_mean * ex)

## Por región, sexo y grupo ampliado edad
AVP_ge_amp_reg <- defun |>
  # Calcular defunciones por trienio
  group_by(
    anio_enfr,
    region,
    grupo_edad_amp,
    sexo
  ) |>
  summarise(
    defun_n = sum(n, na.rm = TRUE),
    defun_mean = mean(n, na.rm = TRUE),
    .groups = "drop"
  ) |>

  # Añadir datos esperanza de vida
  left_join(ex_ge_amp) |>

  # Calcular AVP por grupo decenal
  mutate(avp_dm = defun_mean * ex)


# Exportar datos limpios -------------------------------------------------
# AVP por provincia, sexo y grupos ampliados edad
export(AVP_ge_amp, file = "clean/avp_ge_amp_arg.csv")

# AVP por región, sexo y grupos ampliados edad
export(AVP_ge_amp_reg, file = "clean/avp_ge_amp_reg_arg.csv")

## Limpiar environment
rm(list = ls())
