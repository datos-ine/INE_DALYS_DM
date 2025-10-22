### Carga de DM en Argentina
### Limpieza de los datasets de proyecciones poblacionales por provincia, sexo
### y grupo etario INDEC (2005-2018)
### Autoras: Tamara Ricardo / Micaela Gauto
### Fecha modificación: # 2025-10-22 15:12:09

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  geoAr,
  tabulapdf, # Extraer datos de PDF
  tidyverse,
  readxl
)


# Cargar datos -----------------------------------------------------------
## Provincias
prov <- show_arg_codes() |>
  # Filtrar totales
  filter(between(codprov, "01", "24")) |>

  # Cambiar etiqueta CABA
  mutate(name_iso = if_else(codprov == "01", "CABA", name_iso))


## Proyecciones 2001-2005
proy_01_05_raw <- extract_areas(
  "raw/INDEC/INDEC_proyec 2001-2015.pdf",
  pages = c(22:24, 27:28, 25:26, 29:43, 45, 44)
)

## Proyecciones 2009-2018
# Ruta del archivo de Excel
indec_10 <- "raw/INDEC/c2_proyecciones_prov_2010_2040.xls"

# Cargar/unir hojas
proy_10_18_raw <- excel_sheets(indec_10)[-c(1:2)] |> # Listar hojas por provincia
  # Crear columna para la provincia
  set_names() |>

  # Leer filas para 2010-2015 y unir por provincia
  map(~ read_excel(indec_10, sheet = .x, range = "A3:X28")) |>
  list_rbind(names_to = "prov") |>

  # Unir filas para 2016-2021
  bind_cols(
    excel_sheets(indec_10)[-c(1:2)] |> # Listar hojas por provincia
      # Crear columna para la provincia
      set_names() |>

      # Leer filas para 2016-2021 y unir por provincia
      map(~ read_excel(indec_10, sheet = .x, range = "A31:X56")) |>
      list_rbind(names_to = "prov")
  )


# Limpieza de datos ------------------------------------------------------
## Proyecciones 2001-2005 ----
proy_01_05 <- proy_01_05_raw |>
  # Asignar identificador numérico a cada provincia
  set_names(unique(prov$codprov_censo)) |>

  # Unir tablas de provincias
  list_rbind(names_to = "prov_id") |>

  # Estandarizar nombres de columnas
  clean_names() |>

  # Seleccionar columnas
  select(
    prov_id,
    grupo_edad_5 = x1,
    Varón_2001 = x2001,
    Mujer_2001 = x4,
    Varón_2005 = x2005,
    Mujer_2005 = x7
  ) |>

  # Filtrar menores de edad
  filter(
    between(grupo_edad_5, "20-24", "45-49") |
      between(grupo_edad_5, "50-54", "80 y más")
  ) |>

  # Añadir nombre de provincia
  mutate(
    prov_nombre = prov$name_iso[match(prov_id, prov$codprov_censo)],
    .after = prov_id
  ) |>

  # Recategorizar grupo etario
  mutate(
    grupo_edad_5 = str_replace(grupo_edad_5, "-", " a ") |>
      str_replace(" y más", "+")
  ) |>

  # Base long
  pivot_longer(cols = c(Varón_2001:Mujer_2005), values_to = "proy_pob") |>

  # Separar sexo y año
  separate_wider_delim(name, delim = "_", names = c("sexo", "anio")) |>

  # Población a numérico
  mutate(proy_pob = parse_number(proy_pob, locale = locale(decimal_mark = ",")))


## Proyecciones 2010-2018 ----
proy_10_18 <- proy_10_18_raw |>
  # Estandarizar nombres de columnas
  clean_names() |>

  # Seleccionar columnas relevantes
  select(
    prov_id = prov_1,
    grupo_edad_5 = edad_2,
    Varón_2010 = x4,
    Mujer_2010 = x5,
    Varón_2013 = x16,
    Mujer_2013 = x17,
    Varón_2018 = x37,
    Mujer_2018 = x38
  ) |>

  # Filtrar menores de edad
  filter(
    between(grupo_edad_5, "20-24", "45-49") |
      between(grupo_edad_5, "50-54", "95-99") |
      grupo_edad_5 == "100 y más"
  ) |>

  # Modificar etiquetas provincia
  mutate(prov_id = str_sub(prov_id, 1, 2)) |>

  # Añadir nombre provincia
  mutate(
    prov_nombre = prov$name_iso[match(prov_id, prov$codprov_censo)],
    .after = prov_id
  ) |>

  # Recategorizar mayores 80 años
  mutate(
    grupo_edad_5 = fct_collapse(
      grupo_edad_5,
      "80+" = c("80-84", "85-89", "90-94", "95-99", "100 y más")
    )
  ) |>

  # Cambiar etiquetas grupo etario
  mutate(grupo_edad_5 = str_replace(grupo_edad_5, "-", " a ")) |>

  # Base long
  pivot_longer(cols = c(Varón_2010:Mujer_2018), values_to = "pob") |>

  # Separar sexo y año
  separate_wider_delim(name, delim = "_", names = c("sexo", "anio")) |>

  # Población a numérico
  mutate(pob = parse_number(pob, locale = locale(decimal_mark = ","))) |>

  # Agrupar datos
  count(
    anio,
    prov_id,
    prov_nombre,
    grupo_edad_5,
    sexo,
    wt = pob,
    name = "proy_pob"
  )


## Población estándar 2010 ----
pob_est <- proy_10_18 |>
  filter(anio == 2010) |>
  select(contains("prov"), sexo, contains("grupo"), pob_est = proy_pob)


# Estimar proyección 2009 -------------------------------------------------
# Método lineal
proy_09 <- proy_10_18 |>
  # Filtrar proyecciones 2010
  filter(anio == 2010) |>

  # Unión con población 2001
  bind_rows(
    proy_01_05 |>
      filter(anio == "2001")
  ) |>

  # Formato wide
  pivot_wider(
    names_from = anio,
    values_from = proy_pob,
    names_prefix = "pob_"
  ) |>

  # Interpolación lineal
  mutate(
    anio = "2009",
    tasa_anual = log(pob_2010 / pob_2001) / 9,
    proy_pob = round((pob_2001 * tasa_anual * 8) + pob_2001)
  ) |>

  # Seleccionar columnas
  select(anio, prov_id, prov_nombre, grupo_edad_5, sexo, proy_pob)


# Proyecciones por grupo quinquenal --------------------------------------
proy_ge_5 <- bind_rows(proy_01_05, proy_09, proy_10_18) |>

  # Quitar registros de 2001 y 2010
  filter(!anio %in% c("2001", "2010")) |>

  # Añadir población estándar 2010
  left_join(pob_est) |>

  # Ordenar columnas
  select(anio, everything())


# Proyecciones por grupo decenal -----------------------------------------
proy_ge_10 <- proy_ge_5 |>
  # Crear grupo edad decenal
  mutate(
    grupo_edad_10 = case_when(
      between(grupo_edad_5, "20 a 24", "25 a 29") ~ "20 a 29",
      between(grupo_edad_5, "30 a 34", "35 a 39") ~ "30 a 39",
      between(grupo_edad_5, "40 a 44", "45 a 49") ~ "40 a 49",
      between(grupo_edad_5, "50 a 54", "55 a 59") ~ "50 a 59",
      between(grupo_edad_5, "60 a 64", "65 a 69") ~ "60 a 69",
      between(grupo_edad_5, "70 a 74", "75 a 79") ~ "70 a 79",
      .default = grupo_edad_5
    )
  ) |>

  # Agrupar datos
  group_by(anio, prov_id, prov_nombre, grupo_edad_10, sexo) |>
  summarise(
    proy_pob = sum(proy_pob, na.rm = TRUE),
    pob_est = sum(pob_est, na.rm = TRUE),
    .groups = "drop"
  )


# Proyecciones por grupo ampliado ----------------------------------------
proy_ge_amp <- proy_ge_5 |>
  # Crear grupo edad ampliado
  mutate(
    grupo_edad_amp = case_when(
      between(grupo_edad_5, "20 a 24", "25 a 29") ~ "20 a 29",
      between(grupo_edad_5, "30 a 34", "40 a 44") ~ "30 a 44",
      between(grupo_edad_5, "45 a 49", "55 a 59") ~ "45 a 59",
      between(grupo_edad_5, "60 a 64", "70 a 74") ~ "60 a 74",
      .default = "75+"
    )
  ) |>

  # Agrupar datos
  group_by(anio, prov_id, prov_nombre, grupo_edad_amp, sexo) |>
  summarise(
    proy_pob = sum(proy_pob, na.rm = TRUE),
    pob_est = sum(pob_est, na.rm = TRUE),
    .groups = "drop"
  )


# Guardar datos limpios --------------------------------------------------
## Proyecciones grupo quinquenal
export(proy_ge_5, file = "clean/proy_ge_quin_arg.csv")

## Proyecciones grupo decenal
export(proy_ge_10, file = "clean/proy_ge_dec_arg.csv")

## Proyecciones grupo ampliado
export(proy_ge_amp, file = "clean/proy_ge_amp_arg.csv")
