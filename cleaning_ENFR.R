### Carga de DM en Argentina
### Cálculo de prevalencias de DM por provincia, sexo y grupo etario según datos
### de las Encuestas Nacionales de Factores de Riesgo (2005, 2009, 2013, 2018)
### Autora: Tamara Ricardo
### Fecha creación: # 2025-10-22 13:12:27
### Fecha modificación: # 2025-10-23 12:06:57

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

    # Cambiar formato id de provincia
    mutate(
      prov_id = if_else(
        prov_id %in% c(2, 6),
        paste0("0", prov_id),
        as.character(prov_id)
      )
    ) |>

    # # Crear grupo de edad quinquenal
    # mutate(
    #   grupo_edad_5 = age_categories(
    #     edad,
    #     lower = 20,
    #     upper = 80,
    #     by = 5,
    #     separator = " a "
    #   )
    # ) |>

    # # Crear grupo de edad decenal
    # mutate(
    #   grupo_edad_10 = age_categories(
    #     edad,
    #     lower = 20,
    #     upper = 80,
    #     by = 10,
    #     separator = " a "
    #   )
    # ) |>

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

    # Convertir dm_auto a binomial
    mutate(dm_auto = if_else(dm_auto == 1, 1, 0))
}

# Limpiar datos ----------------------------------------------------------
## ENFR 2005 ----
enfr05 <- enfr05_raw |>
  # Estandarizar nombres columnas
  clean_names() |>

  # Seleccionar columnas
  select(
    id = identifi,
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


# Prevalencias por grupos ampliados --------------------------------------
## ENFR 2005 ----
enfr05_ge_amp <- enfr05 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, grupo_edad_amp, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2009 ----
enfr09_ge_amp <- enfr09 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, grupo_edad_amp, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2013 ----
enfr13_ge_amp <- enfr13 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, grupo_edad_amp, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto, vartype = c("se", "cv")),
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
  group_by(prov_id, grupo_edad_amp, sexo) |>
  summarise(
    dm_total = survey_total(dm_auto, vartype = c("se", "cv")),
    dm_prev = survey_mean(dm_auto, vartype = c("se", "cv")),
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
# Prevalencia DM por grupos ampliados
export(enfr_ge_amp, file = "clean/prev_dm_ge_amp_arg.csv")

## Limpiar environment
rm(list = ls())
