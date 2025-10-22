### Carga de DM en Argentina
### Cálculo de prevalencias de DM por provincia, sexo y grupo etario según datos
### de las Encuestas Nacionales de Factores de Riesgo (2005, 2009, 2013, 2018)
### Autora: Tamara Ricardo
### Fecha creación: # 2025-10-22 13:12:27
### Fecha modificación: # 2025-10-22 13:53:31

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  srvyr,
  epikit,
  geoAr,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## Provincias
prov <- show_arg_codes() |>
  # Filtrar totales
  filter(between(codprov, "01", "24")) |>

  # Cambiar etiqueta CABA
  mutate(name_iso = if_else(codprov == "01", "CABA", name_iso)) |>

  # Códigos a numérico
  mutate(across(.cols = c(codprov, codprov_censo), .fns = ~ parse_number(.x)))


## ENFR 2005
enfr05_raw <- read_delim("raw/ENFR/ENFR 2005 - Base usuario.txt")


## ENFR 2009
enfr09_raw <- read_delim("raw/ENFR/ENFR 2009 - Base usuario.txt")


## ENFR 2013
enfr13_raw <- import("raw/ENFR/ENFR 2013 - Base usuario.txt")


## ENFR 2018
enfr18_raw <- read_delim("raw/ENFR/ENFR 2018 - Base usuario.txt")

# Réplicas ENFR 2018
enfr18_rep <- read_delim("raw/ENFR/ENFR2018_base_rep_filter.csv")


# Función para limpiar datos ----------------------------------------------
clean_enfr <- function(x) {
  x |>
    # Filtrar menores de 20 años
    filter(edad >= 20) |>

    # Añadir etiquetas provincia
    mutate(prov_nombre = prov$name_iso[match(prov_id, prov$codprov_censo)]) |>

    # Crear grupo de edad quinquenal
    mutate(
      grupo_edad_5 = age_categories(
        edad,
        lower = 20,
        upper = 80,
        by = 5,
        separator = " a "
      )
    ) |>

    # Crear grupo de edad decenal
    mutate(
      grupo_edad_10 = age_categories(
        edad,
        lower = 20,
        upper = 80,
        by = 10,
        separator = " a "
      )
    ) |>

    # Crear grupo de edad ampliado
    mutate(
      grupo_edad_amp = age_categories(
        edad,
        breakers = c(20, 30, 45, 60, 75),
        separator = " a "
      )
    ) |>

    # Cambiar etiquetas sexo
    mutate(sexo = if_else(sexo == 1, "Varón", "Mujer")) |>

    # Crear variable binomial para diabetes por autorreporte
    mutate(dm_auto_bin = if_else(dm_auto == 1, 1, 0)) |>

    # Cambiar etiquetas DM por autorreporte
    mutate(dm_auto = factor(dm_auto, labels = c("Sí", "No", "NS/NC")))
}

# Limpiar datos ----------------------------------------------------------
## ENFR 2005 ----
enfr05 <- enfr05_raw |>
  # Estandarizar nombres columnas
  clean_names() |>

  # Seleccionar columnas
  select(
    prov_id = prov,
    sexo = chch04,
    edad = chch05,
    dm_auto = cidi01,
    ponderacion
  ) |>

  # Aplicar función de limpieza
  clean_enfr()


## ENFR 2009 ----
enfr09 <- enfr09_raw |>
  # Estandarizar nombres columnas
  clean_names() |>

  # Seleccionar columnas
  select(
    id = identifi,
    prov_id = prvnc,
    sexo = bhch04,
    edad = bhch05,
    dm_auto = bidi01,
    ponderacion
  ) |>

  # Aplicar función de limpieza
  clean_enfr()


## ENFR 2013 ----
enfr13 <- enfr13_raw |>
  # Estandarizar nombres columnas
  clean_names() |>

  # Seleccionar columnas
  select(
    id,
    prov_id = cod_provincia,
    sexo = bhch04,
    edad = bhch05,
    dm_auto = bidi01,
    ponderacion
  ) |>

  # Aplicar función de limpieza
  clean_enfr()


## ENFR 2018 ----
enfr18 <- enfr18_raw |>
  # Estandarizar nombres columnas
  clean_names() |>

  # Seleccionar columnas
  select(
    id,
    prov_id = cod_provincia,
    sexo = bhch03,
    edad = bhch04,
    dm_auto = bidi01,
    wf1p
  ) |>

  # Aplicar función de limpieza
  clean_enfr() |>

  # Añadir réplicas
  left_join(enfr18_rep)


# Prevalencias por grupos quinquenales -----------------------------------
## ENFR 2005 ----
enfr05_ge_5 <- enfr05 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_5, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2009 ----
enfr09_ge_5 <- enfr09 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_5, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2013 ----
enfr13_ge_5 <- enfr13 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_5, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2018 (warning) ----
enfr18_ge_5 <- enfr18 |>
  # Crear objeto diseño
  as_survey_rep(
    weights = wf1p,
    repweights = starts_with("wf1p"),
    type = "bootstrap"
  ) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_5, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
    .groups = "drop"
  )


## Unir datasets ----
enfr_ge_5 <- bind_rows(
  enfr05_ge_5,
  enfr09_ge_5,
  enfr13_ge_5,
  enfr18_ge_5,
  .id = "anio_enfr"
) |>

  # Añadir etiquetas año ENFR
  mutate(
    anio_enfr = fct_relabel(anio_enfr, ~ c("2005", "2009", "2013", "2018"))
  ) |>

  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric), .fns = ~ round(.x, 2))) |>

  # Categorizar coeficiente de variación
  mutate(
    dm_prev_cv_cat = cut(
      dm_prev_cv,
      breaks = c(-Inf, .1, .2, .3, Inf),
      labels = c("Baja", "Moderada", "Alta", "Muy alta")
    )
  )

# Prevalencias por grupos decenales --------------------------------------
## ENFR 2005 ----
enfr05_ge_10 <- enfr05 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_10, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2009 ----
enfr09_ge_10 <- enfr09 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_10, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2013 ----
enfr13_ge_10 <- enfr13 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_10, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2018 (warning) ----
enfr18_ge_10 <- enfr18 |>
  # Crear objeto diseño
  as_survey_rep(
    weights = wf1p,
    repweights = starts_with("wf1p"),
    type = "bootstrap"
  ) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_10, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
    .groups = "drop"
  )


## Unir datasets ----
enfr_ge_10 <- bind_rows(
  enfr05_ge_10,
  enfr09_ge_10,
  enfr13_ge_10,
  enfr18_ge_10,
  .id = "anio_enfr"
) |>

  # Añadir etiquetas año ENFR
  mutate(
    anio_enfr = fct_relabel(anio_enfr, ~ c("2005", "2009", "2013", "2018"))
  ) |>

  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric), .fns = ~ round(.x, 2))) |>

  # Categorizar coeficiente de variación
  mutate(
    dm_prev_cv_cat = cut(
      dm_prev_cv,
      breaks = c(-Inf, .1, .2, .3, Inf),
      labels = c("Baja", "Moderada", "Alta", "Muy alta")
    )
  )

# Prevalencias por grupos ampliados --------------------------------------
## ENFR 2005 ----
enfr05_ge_amp <- enfr05 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_amp, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2009 ----
enfr09_ge_amp <- enfr09 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_amp, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2013 ----
enfr13_ge_amp <- enfr13 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_amp, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2018 (warning) ----
enfr18_ge_amp <- enfr18 |>
  # Crear objeto diseño
  as_survey_rep(
    weights = wf1p,
    repweights = starts_with("wf1p"),
    type = "bootstrap"
  ) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_amp, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
    .groups = "drop"
  )


## Unir datasets ----
enfr_ge_amp <- bind_rows(
  enfr05_ge_amp,
  enfr09_ge_amp,
  enfr13_ge_amp,
  enfr18_ge_amp,
  .id = "anio_enfr"
) |>

  # Añadir etiquetas año ENFR
  mutate(
    anio_enfr = fct_relabel(anio_enfr, ~ c("2005", "2009", "2013", "2018"))
  ) |>

  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric), .fns = ~ round(.x, 2))) |>

  # Categorizar coeficiente de variación
  mutate(
    dm_prev_cv_cat = cut(
      dm_prev_cv,
      breaks = c(-Inf, .1, .2, .3, Inf),
      labels = c("Baja", "Moderada", "Alta", "Muy alta")
    )
  )


# Guardar datos limpios --------------------------------------------------
# Prevalencia DM por grupos quinquenales
export(enfr_ge_5, file = "clean/prev_dm_ge_quin_arg.csv")


# Prevalencia DM por grupos decenales
export(enfr_ge_10, file = "clean/prev_dm_ge_dec_arg.csv")


# Prevalencia DM por grupos ampliados
export(enfr_ge_amp, file = "clean/prev_dm_ge_amp_arg.csv")

## Limpiar environment
rm(list = ls())
