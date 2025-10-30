# --- 0. Load Necessary Packages ---
library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(readxl)
library(DT)
library(tidyr)
library(stringr)
library(shinyWidgets)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(data.table)
library(lwgeom)
library(tools)
library(htmltools)
library(viridis)
library(leaflet.extras)
library(DBI)
library(odbc)
library(glue)
library(lubridate)
library(plotly)
# library(minpack.lm) # For Arps if needed, currently nls is used

# ==== Gas Plant: constants & helpers ====
`%||%` <- function(x, y) { if (is.null(x) || length(x) == 0) return(y); x }

resolve_existing_dir <- function(candidates, fallback = ".") {
  candidates <- unique(trimws(candidates))
  candidates <- candidates[!is.na(candidates) & candidates != ""]
  for (cand in candidates) {
    candidate_path <- tryCatch(normalizePath(cand, winslash = "/", mustWork = FALSE),
                               error = function(e) cand)
    if (dir.exists(candidate_path)) return(candidate_path)
  }

  tryCatch(normalizePath(fallback, winslash = "/", mustWork = FALSE), error = function(e) fallback)
}

detect_app_directory <- function() {
  script_dir <- tryCatch({
    this_file <- normalizePath("appwithgasplant.R", winslash = "/", mustWork = FALSE)
    dirname(this_file)
  }, error = function(e) NA_character_)
  resolve_existing_dir(c(
    Sys.getenv("HELLO_APP_DIR", ""),
    Sys.getenv("HELLO_BASE_PATH", ""),
    getOption("hello.base_path", ""),
    script_dir,
    normalizePath(".", winslash = "/", mustWork = FALSE),
    "C:/Users/I37643/OneDrive - Wood Mackenzie Limited/Documents/WoodMac/APP"
  ))
}

app_base_dir <- detect_app_directory()

GAS_BASE_DIR <- resolve_existing_dir(c(
  Sys.getenv("HELLO_GAS_BASE_DIR", ""),
  file.path(app_base_dir, "Restart", "HELLO"),
  file.path(app_base_dir, "HELLO"),
  app_base_dir
))
ST50_FILE    <- "st50_gas_plant_master.csv"
MONTHLY_FILE <- "Vol_2025-01-AB.CSV"
AVG_DAYS_PER_MONTH <- 30.4375
MONTHLY_PATTERNS <- c("^Vol_\\d{4}-\\d{2}-AB\\.csv$", "^st13b_\\d{4}_detail\\.csv$")

to_upper_trim <- function(x) toupper(trimws(as.character(x)))
`%nin%` <- function(x, y) !(x %in% y)

map_col <- function(df, candidates, to) {
  nm <- gsub("[ _]+","", tolower(names(df)))
  pick <- function(keys) {
    for (k in keys) {
      i <- match(gsub("[ _]+","", tolower(k)), nm)
      if (!is.na(i)) return(i)
    }
    NA_integer_
  }
  i <- pick(candidates)
  if (!is.na(i)) names(df)[i] <- to
  df
}

discover_latest_monthly_file <- function(base_dir, patterns) {
  if (!dir.exists(base_dir)) return(NA_character_)
  today_csv <- file.path(base_dir, paste0("Vol_", format(Sys.Date(), "%Y-%m"), "-AB.csv"))
  if (file.exists(today_csv)) return(today_csv)
  all <- list.files(base_dir, full.names = TRUE)
  keep <- character(0)
  for (p in patterns) keep <- c(keep, grep(p, basename(all), value = TRUE))
  keep <- unique(file.path(base_dir, keep))
  if (!length(keep)) return(NA_character_)
  keep[which.max(file.info(keep)$mtime)]
}

load_st50_capacity <- function(base_dir, st50_file) {
  p <- file.path(base_dir, st50_file)
  if (!file.exists(p)) { warning("ST50 missing: ", p); return(tibble::tibble()) }
  ext <- tolower(tools::file_ext(p))
  df <- if (ext %in% c("xlsx","xls")) {
    prev <- suppressWarnings(readxl::read_excel(p, col_names = FALSE, n_max = 15))
    hdr <- 3L
    if (nrow(prev) >= 1) {
      for (r in seq_len(min(nrow(prev), 6))) {
        row_norm <- gsub("[^A-Za-z0-9]","", tolower(paste(unlist(prev[r, , drop=TRUE]), collapse=",")))
        if (grepl("facility|reportingfacilityid", row_norm)) { hdr <- r; break }
      }
    }
    readxl::read_excel(p, skip = hdr - 1, .name_repair = "minimal") |> as.data.frame()
  } else {
    readr::read_csv(p, skip = 2, show_col_types = FALSE, guess_max = 200000) |> as.data.frame()
  }
  nm <- gsub("[ _/]+","", tolower(trimws(names(df))))
  pick <- function(keys) { for (k in keys) { i <- match(gsub("[ _/]+","", tolower(k)), nm); if (!is.na(i)) return(i) } ; NA_integer_ }
  id <- pick(c("Reporting Facility ID","ReportingFacilityID","FacilityID","FACILITY_ID"))
  if (is.na(id)) { warning("ST50 has no FacilityID-like column after header detection"); return(tibble::tibble()) }
  out <- tibble::tibble(
    FacilityID   = as.character(df[[id]]),
    FacilityName = if (!is.na(pick(c("Facility Name","FacilityName")))) df[[pick(c("Facility Name","FacilityName"))]] else NA_character_,
    Latitude     = suppressWarnings(as.numeric(if (!is.na(pick(c("Surface Latitude","Location Latitude","Latitude")))) df[[pick(c("Surface Latitude","Location Latitude","Latitude"))]] else NA)),
    Longitude    = suppressWarnings(as.numeric(if (!is.na(pick(c("Location Longitude","Longitude")))) df[[pick(c("Location Longitude","Longitude"))]] else NA)),
    Operator_cap = if (!is.na(pick(c("Operator","Operator Name","OperatorName")))) df[[pick(c("Operator","Operator Name","OperatorName"))]] else NA_character_,
    FacilityType_cap = if (!is.na(pick(c("Facility Subtype","ReportingFacilitySubtypeDesc","FacilitySubtype")))) df[[pick(c("Facility Subtype","ReportingFacilitySubtypeDesc","FacilitySubtype"))]] else NA_character_,
    LicCap_E3m3D = suppressWarnings(as.numeric(if (!is.na(pick(c("Raw Gas E3m3/d","Plant Processes Raw Gas E3m3/d","Licensed Capacity E3m3/d","LicCap_E3m3D")))) df[[pick(c("Raw Gas E3m3/d","Plant Processes Raw Gas E3m3/d","Licensed Capacity E3m3/d","LicCap_E3m3D"))]] else NA)),
    LicCap_E3m3M = suppressWarnings(as.numeric(if (!is.na(pick(c("LicCap_E3m3M","Licensed Capacity E3m3M","Raw Gas Licensed Capacity E3m3M")))) df[[pick(c("LicCap_E3m3M","Licensed Capacity E3m3M","Raw Gas Licensed Capacity E3m3M"))]] else NA))
  ) |>
    dplyr::filter(!is.na(FacilityID) & FacilityID != "") |>
    dplyr::distinct(FacilityID, .keep_all = TRUE)
  out$FacilityID_norm <- to_upper_trim(out$FacilityID)
  out$monthly_capacity_e3m3 <- dplyr::coalesce(out$LicCap_E3m3M, AVG_DAYS_PER_MONTH * out$LicCap_E3m3D)
  out
}

is_gas_plant <- function(type, subtype) {
  t <- to_upper_trim(type)
  !is.na(t) & t == "GP"
}

scale_capacity_radius <- function(cap_daily_e3m3) {
  x <- sqrt(pmax(cap_daily_e3m3, 0))
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || rng[1] == rng[2]) return(rep(6, length(x)))
  6 + 10 * (x - rng[1]) / (rng[2] - rng[1])
}

normalize_operator_label <- function(x) {
  out <- trimws(as.character(x))
  out[is.na(out) | out == ""] <- "(Unknown)"
  out
}

util_palette <- function(x) {
  leaflet::colorBin(
    "viridis",
    domain = x,
    bins = c(0, 0.5, 0.7, 0.85, 1.0, 1.2, Inf),
    right = FALSE,
    na.color = "#cccccc"
  )
}

op_palette <- function(op_levels) {
  leaflet::colorFactor(palette = custom_palette, domain = op_levels, na.color = "#999999")
}

normalize_type_label <- function(x) {
  out <- trimws(as.character(x))
  out[is.na(out) | out == ""] <- "(Unknown)"
  out
}

assign_shape_map <- function(types) {
  clean <- normalize_type_label(types)
  unique_types <- unique(clean)
  base_shapes <- c("circle", "square", "diamond", "triangle", "hexagon")
  shape_map <- stats::setNames(base_shapes[((seq_along(unique_types) - 1) %% length(base_shapes)) + 1], unique_types)
  list(clean = clean, map = shape_map)
}

make_shape_svg <- function(shape, size, fill, stroke = "#2c3e50") {
  size <- max(as.numeric(size), 12)
  half <- size / 2
  inset <- 2
  path <- switch(
    tolower(shape),
    circle  = sprintf('<circle cx="%.1f" cy="%.1f" r="%.1f" fill="%s" stroke="%s" stroke-width="2"/>', half, half, max(half - inset, 2), fill, stroke),
    square  = sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="4" fill="%s" stroke="%s" stroke-width="2"/>', inset, inset, size - inset * 2, size - inset * 2, fill, stroke),
    diamond = sprintf('<polygon points="%.1f,%.1f %.1f,%.1f %.1f,%.1f %.1f,%.1f" fill="%s" stroke="%s" stroke-width="2"/>', half, inset, size - inset, half, half, size - inset, inset, half, fill, stroke),
    triangle = sprintf('<polygon points="%.1f,%.1f %.1f,%.1f %.1f,%.1f" fill="%s" stroke="%s" stroke-width="2"/>', half, inset, size - inset, size - inset, inset, size - inset, fill, stroke),
    hexagon = {
      top <- inset; bottom <- size - inset; left <- inset; right <- size - inset
      mid_top <- top + (bottom - top) * 0.25; mid_bottom <- bottom - (bottom - top) * 0.25
      sprintf('<polygon points="%.1f,%.1f %.1f,%.1f %.1f,%.1f %.1f,%.1f %.1f,%.1f %.1f,%.1f" fill="%s" stroke="%s" stroke-width="2"/>',
              half, top, right, mid_top, right, mid_bottom, half, bottom, left, mid_bottom, left, mid_top, fill, stroke)
    },
    sprintf('<circle cx="%.1f" cy="%.1f" r="%.1f" fill="%s" stroke="%s" stroke-width="2"/>', half, half, max(half - inset, 2), fill, stroke)
  )
  svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">%s</svg>', size, size, size, size, path)
  paste0("data:image/svg+xml;utf8,", utils::URLencode(svg, reserved = TRUE))
}

build_shape_legend <- function(shape_map) {
  if (is.null(shape_map) || !length(shape_map)) return(NULL)
  entries <- lapply(names(shape_map), function(type_label) {
    icon_src <- make_shape_svg(shape_map[[type_label]], 24, "#4a4a4a")
    htmltools::tags$div(
      class = "legend-item",
      htmltools::tags$img(src = icon_src, width = 24, height = 24, alt = paste(type_label, "marker")),
      htmltools::tags$span(type_label)
    )
  })
  htmltools::tags$div(
    class = "shape-legend",
    htmltools::tags$strong("Facility Type"),
    entries
  )
}



# --- Database Connection Details ---
db_user <- "WOODMAC"
db_password <- "c0pp3r"
tns_alias <- "GDC_LINK.geologic.com"
oracle_driver <- "Oracle in instantclient_23_7" # Adjust if your driver name is different

connection_string <- paste0(
  "Driver={", oracle_driver, "};",
  "Dbq=", tns_alias, ";",
  "Uid=", db_user, ";",
  "Pwd=", db_password, ";"
)

con <- NULL

# --- Function to Establish Database Connection ---
connect_to_db <- function() {
  if (!is.null(con) && dbIsValid(con)) {
    # If connection exists and is valid, no need to disconnect and reconnect, just return it.
    # However, if this function is explicitly called to force a new connection,
    # then we might want to disconnect first. For a general "get connection" this is fine.
    return(con)
  }
  local_con <- NULL
  tryCatch({
    message("Attempting to connect to Oracle: ", tns_alias, " as user: ", db_user)
    Sys.sleep(0.1)
    local_con <- dbConnect(
      odbc::odbc(),
      .connection_string = connection_string,
      timeout = 10
    )
    message("SUCCESS: Database connection established.")
    return(local_con)
  }, error = function(e) {
    message("ERROR during dbConnect call: Failed to connect to Oracle.")
    message("Detailed R error: ", e$message)
    return(NULL)
  })
}

# Initial connection attempt (optional; defaults to lazy connection)
if (isTRUE(as.logical(Sys.getenv("HELLO_EAGER_DB_CONNECT", "FALSE")))) {
  con <- connect_to_db()
}

onStop(function() {
  if (!is.null(con) && dbIsValid(con)) {
    dbDisconnect(con)
    message("Disconnected from Oracle database on app stop.")
  }
})

# --- 1. Define File Paths and Constants ---
base_path <- app_base_dir

locate_processed_rds_file <- function(base_dir, default_filename) {
  candidate_env <- trimws(Sys.getenv("HELLO_PROCESSED_RDS_FILE", ""))
  expand_path <- function(path) {
    if (!nzchar(path)) return(NA_character_)
    tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE), error = function(e) path)
  }

  env_path <- expand_path(candidate_env)
  default_path <- expand_path(file.path(base_dir, default_filename))
  working_dir_path <- expand_path(file.path(getwd(), default_filename))
  candidates <- unique(Filter(
    function(p) nzchar(p),
    c(env_path, default_path, working_dir_path)
  ))

  for (cand in candidates) {
    if (nzchar(cand) && file.exists(cand)) {
      if (!identical(cand, default_path)) {
        message("Using processed RDS cache from ", cand)
      }
      return(cand)
    }
  }

  find_latest_processed <- function(search_dir) {
    if (!dir.exists(search_dir)) return(NA_character_)
    files <- list.files(
      search_dir,
      pattern = "processed_app_data.*\\.rds$",
      ignore.case = TRUE,
      full.names = TRUE
    )
    if (!length(files)) return(NA_character_)
    files[which.max(file.info(files)$mtime)]
  }

  fallback <- unique(Filter(
    function(p) nzchar(p) && file.exists(p),
    c(find_latest_processed(base_dir), find_latest_processed(getwd()))
  ))

  if (length(fallback)) {
    message("Processed RDS cache not found at default path; falling back to ", fallback[[1]])
    return(fallback[[1]])
  }

  default_path
}

processed_rds_file <- locate_processed_rds_file(base_path, "processed_app_data_vMERGED_FINAL_v24_DB_sticks_latlen.rds")

woodmack_coverage_file_xlsx <- file.path(base_path, "Woodmack.Coverage.2024.xlsx")
play_subplay_shapefile_dir <- file.path(base_path, "SubplayShapefile")
company_shapefiles_dir <- file.path(base_path, "Shapefile")


# Conversion factors
E3M3_TO_MCF <- 35.3147
M3_TO_BBL <- 6.28981
AVG_DAYS_PER_MONTH <- 30.4375
MCF_PER_BOE <- 6 # Standard conversion for BOE calculations

# Custom Color Palette
custom_palette <- c(
  "#53143F", "#F94355", "#058F96", "#F57D01", "#205B2E", "#A8011E",
  "#5A63E3", "#FFD31A", "#4E207F", "#CC9900", "#A6A6A6", "#9ACEE2",
  "#A9899E", "#FFA3AA", "#92C5C9", "#F9BE96", "#92AC96", "#D6890B",
  "#ABAFF0", "#FFE89C", "#A68EBF", "#D7C68E", "#D1D1D1", "#CBE5EF"
)
if (!all(sapply(custom_palette, function(x) grepl("^#[0-9A-Fa-f]{6}$", x) || x %in% colors()))) {
  warning("Some colors in custom_palette may not be valid R colors or hex codes. Using fallback for invalid ones.")
  custom_palette <- sapply(custom_palette, function(x) {
    if (grepl("^#[0-9A-Fa-f]{6}$", x) || x %in% colors()) return(x)
    if (grepl("^#[0-9A-Fa-f]{5}$", x) && nchar(x) == 6) return(paste0(x, "B"))
    return("grey")
  })
}


# --- 2. Helper Functions ---
standardize_uwi <- function(uwi_vector) {
  uwi_vector <- as.character(uwi_vector)
  uwi_vector <- stringr::str_replace_all(uwi_vector, "[^A-Za-z0-9]", "")
  uwi_vector <- toupper(uwi_vector)
  return(uwi_vector)
}

safe_read_excel <- function(file_path, sheet_name, file_description = file_path) {
  message(paste("Attempting to load Excel sheet:", sheet_name, "from", file_description))
  if (!file.exists(file_path)) {
    warning(paste("Excel file not found:", file_path, "for", file_description))
    return(data.frame())
  }
  tryCatch({
    df <- readxl::read_excel(file_path, sheet = sheet_name, .name_repair = "universal")
    message(paste("Successfully loaded Excel sheet:", sheet_name, "- Rows:", nrow(df), "Cols:", ncol(df)))
    return(as.data.frame(df))
  }, error = function(e) {
    warning(paste("Error loading Excel sheet", sheet_name, "from", file_path, ": ", e$message))
    return(data.frame())
  })
}

clean_df_colnames <- function(df_input, df_name_for_message = "a dataframe") {
  if (is.null(df_input) || (!is.data.frame(df_input) && !is.data.table(df_input)) || ncol(df_input) == 0) {
    return(df_input)
  }
  df <- if (is.data.table(df_input)) data.table::copy(df_input) else df_input
  original_names <- names(df)
  new_names <- make.names(original_names, unique = TRUE)
  new_names <- gsub("\\.+", "_", new_names)
  new_names <- gsub("_+", "_", new_names)
  new_names <- toupper(new_names)
  if (is.data.table(df)) {
    data.table::setnames(df, original_names, new_names)
  } else {
    names(df) <- new_names
  }
  return(df)
}

prepare_filter_choices <- function(column_vector, col_name_for_msg = "column") {
  if (is.null(column_vector) || length(column_vector) == 0) {
    message(paste0("DEBUG (prepare_filter_choices): Input vector for '", col_name_for_msg, "' is NULL or empty."))
    return(character(0))
  }
  column_vector_char <- as.character(column_vector)
  num_na_initial <- sum(is.na(column_vector_char))
  num_empty_initial <- sum(column_vector_char == "", na.rm = TRUE)
  
  choices <- column_vector_char[!is.na(column_vector_char) & column_vector_char != "" & column_vector_char != "NA"]
  choices <- sort(unique(choices))
  
  message(paste0("DEBUG (prepare_filter_choices for '", col_name_for_msg, "'): ",
                 "Original NAs: ", num_na_initial, ", Original empty strings: ", num_empty_initial,
                 ". Final unique choices: ", length(choices)))
  if(length(choices) == 0) return(character(0))
  return(choices)
}

load_process_spatial_layer <- function(shp_path, layer_name, target_crs = 4326, simplify = FALSE, tolerance = NULL, make_valid_geom = FALSE) {
  message(paste0("--- Start Processing Layer: ", layer_name, " ---"))
  message(paste0("Shapefile path: ", shp_path))
  tryCatch({
    if (!file.exists(shp_path)) {
      warning(paste("Shapefile does not exist at path:", shp_path, "for layer", layer_name)); return(NULL)
    }
    data_sf <- sf::st_read(shp_path, quiet = TRUE, stringsAsFactors = FALSE)
    if (nrow(data_sf) == 0) {
      warning(paste("Shapefile for layer", layer_name, "is empty.")); return(NULL)
    }
    message(paste("Layer", layer_name, "read successfully, rows:", nrow(data_sf)))
    current_crs <- sf::st_crs(data_sf)
    if (is.na(current_crs) || (is.list(current_crs) && is.na(current_crs$epsg)) || (is.list(current_crs) && !is.na(current_crs$epsg) && current_crs$epsg != target_crs)) {
      message(paste("Transforming layer", layer_name, "to CRS", target_crs))
      data_sf <- sf::st_transform(data_sf, crs = target_crs)
    }
    if (make_valid_geom) {
      message(paste("Making geometries valid for layer", layer_name))
      data_sf <- sf::st_make_valid(data_sf)
    }
    if (simplify && !is.null(tolerance) && tolerance > 0) {
      message(paste("Simplifying layer", layer_name, "with tolerance", tolerance))
      data_sf <- sf::st_simplify(data_sf, dTolerance = tolerance, preserveTopology = TRUE)
    }
    
    data_sf <- data_sf[!sf::st_is_empty(data_sf), ]
    if(nrow(data_sf) == 0) {
      warning(paste("All geometries became empty for layer", layer_name, "after processing. Skipping."))
      return(NULL)
    }
    
    attrs_df <- sf::st_drop_geometry(data_sf)
    cleaned_attrs_df <- clean_df_colnames(attrs_df, paste("attributes of", layer_name))
    data_sf_final <- sf::st_sf(cleaned_attrs_df, geometry = sf::st_geometry(data_sf))
    if (!"SHP_LAYER_NAME" %in% names(data_sf_final)) data_sf_final$SHP_LAYER_NAME <- layer_name
    message(paste("--- Successfully processed layer:", layer_name, "- Final Features:", nrow(data_sf_final)))
    return(data_sf_final)
  }, error = function(e) {
    message(paste("!!!!!!!! ERROR processing shapefile", shp_path, "for layer", layer_name, ":", e$message, "!!!!!!!!"))
    return(NULL)
  })
}

# Custom geometric mean function
geometric_mean <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  if (any(x < 0) || length(x) == 0) {
    return(NA_real_)
  }
  if (any(x == 0)) {
    return(0)
  }
  exp(mean(log(x)))
}


# --- 3. Load and Pre-process Data ---
final_sf_column_names <- c(
  "UWI", "GSL_UWI", "SurfaceLatitude", "SurfaceLongitude",
  "BH_Latitude", "BH_Longitude", "LateralLength",
  "AbandonmentDate", "WellName", "CurrentStatus", "OperatorCode", "StratUnitID",
  "SpudDate", "FirstProdDate", "FinalTD", "ProvinceState", "Country",
  "UWI_Std", "GSL_UWI_Std", "OperatorName", "Formation", "FieldName",
  "ConfidentialType"
)
empty_wells_df_for_sf <- data.frame(matrix(ncol = length(final_sf_column_names), nrow = 0))
names(empty_wells_df_for_sf) <- final_sf_column_names
empty_wells_df_for_sf$UWI <- character(); empty_wells_df_for_sf$GSL_UWI <- character()
empty_wells_df_for_sf$SurfaceLatitude <- numeric(); empty_wells_df_for_sf$SurfaceLongitude <- numeric()
empty_wells_df_for_sf$BH_Latitude <- numeric(); empty_wells_df_for_sf$BH_Longitude <- numeric()
empty_wells_df_for_sf$LateralLength <- numeric()
empty_wells_df_for_sf$AbandonmentDate <- as.Date(character()); empty_wells_df_for_sf$WellName <- character()
empty_wells_df_for_sf$CurrentStatus <- character(); empty_wells_df_for_sf$OperatorCode <- character()
empty_wells_df_for_sf$StratUnitID <- character(); empty_wells_df_for_sf$SpudDate <- as.Date(character())
empty_wells_df_for_sf$FirstProdDate <- as.Date(character()); empty_wells_df_for_sf$FinalTD <- numeric()
empty_wells_df_for_sf$ProvinceState <- character(); empty_wells_df_for_sf$Country <- character()
empty_wells_df_for_sf$UWI_Std <- character(); empty_wells_df_for_sf$GSL_UWI_Std <- character()
empty_wells_df_for_sf$OperatorName <- character(); empty_wells_df_for_sf$Formation <- character()
empty_wells_df_for_sf$FieldName <- character()
empty_wells_df_for_sf$ConfidentialType <- character()

cached_value_template <- function(template_col) {
  if (inherits(template_col, "Date")) {
    as.Date(NA_character_)
  } else if (inherits(template_col, "POSIXct") || inherits(template_col, "POSIXlt")) {
    as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  } else if (is.numeric(template_col)) {
    as.numeric(NA)
  } else {
    as.character(NA)
  }
}

add_missing_columns_from_template <- function(df, template_df) {
  for (col in names(template_df)) {
    if (!col %in% names(df)) {
      df[[col]] <- rep(cached_value_template(template_df[[col]]), nrow(df))
    }
  }
  df
}

repair_cached_app_data <- function(cache, template_df) {
  fixed <- cache
  if (is.null(fixed) || !is.list(fixed)) fixed <- list()

  wells_obj <- fixed$wells_sf
  fixed$wells_sf <- tryCatch({
    if (inherits(wells_obj, "sf")) {
      wells_clean <- add_missing_columns_from_template(wells_obj, template_df)
      tryCatch(sf::st_transform(wells_clean, 4326), error = function(e) wells_clean)
    } else if (is.data.frame(wells_obj) && nrow(wells_obj) > 0) {
      wells_df <- add_missing_columns_from_template(wells_obj, template_df)
      if (all(c("SurfaceLongitude", "SurfaceLatitude") %in% names(wells_df))) {
        wells_sf <- tryCatch(
          sf::st_as_sf(wells_df, coords = c("SurfaceLongitude", "SurfaceLatitude"), crs = 4269,
                       agr = "constant", remove = FALSE),
          error = function(e) NULL
        )
        if (!is.null(wells_sf)) {
          wells_sf <- add_missing_columns_from_template(wells_sf, template_df)
          tryCatch(sf::st_transform(wells_sf, 4326), error = function(e) wells_sf)
        } else {
          sf::st_sf(template_df, geometry = sf::st_sfc(), crs = 4326)
        }
      } else {
        sf::st_sf(template_df, geometry = sf::st_sfc(), crs = 4326)
      }
    } else {
      sf::st_sf(template_df, geometry = sf::st_sfc(), crs = 4326)
    }
  }, error = function(e) {
    message("Cached wells data invalid: ", e$message)
    sf::st_sf(template_df, geometry = sf::st_sfc(), crs = 4326)
  })

  if (is.null(fixed$play_subplay_layers_list) || !is.list(fixed$play_subplay_layers_list)) {
    fixed$play_subplay_layers_list <- list()
  }
  if (is.null(fixed$company_layers_list) || !is.list(fixed$company_layers_list)) {
    fixed$company_layers_list <- list()
  }
  if (is.null(fixed$operator_lookup) || !is.data.frame(fixed$operator_lookup)) {
    fixed$operator_lookup <- data.table::data.table()
  }

  fixed
}

app_data <- list(
  wells_sf = sf::st_sf(empty_wells_df_for_sf, geometry = sf::st_sfc(), crs = 4326),
  play_subplay_layers_list = list(),
  company_layers_list = list(),
  operator_lookup = data.table::data.table()
)
load_from_db <- TRUE

if (file.exists(processed_rds_file)) {
  message(paste("Attempting to load MERGED pre-processed data from:", processed_rds_file))
  tryCatch({
    loaded_data <- readRDS(processed_rds_file)
    repaired <- repair_cached_app_data(loaded_data, empty_wells_df_for_sf)
    wells_ok <- inherits(repaired$wells_sf, "sf") && nrow(repaired$wells_sf) > 0
    if (!wells_ok) {
      message("Cached wells data is empty after repair; database refresh required.")
    }
    if (!is.list(repaired$play_subplay_layers_list)) {
      message("Cached play/subplay layers not a list; resetting to empty list.")
      repaired$play_subplay_layers_list <- list()
    }
    if (!is.list(repaired$company_layers_list)) {
      message("Cached company layers not a list; resetting to empty list.")
      repaired$company_layers_list <- list()
    }
    app_data <- repaired
    if (wells_ok) {
      message("SUCCESS: Cached wells, shapefiles, and lookup data loaded from RDS. Skipping DB/shapefile refresh.")
      load_from_db <- FALSE
    }
  }, error = function(e) {
    message(paste("ERROR loading RDS file:", processed_rds_file, "-", e$message))
    message("Will proceed to load data from DB and shapefiles.")
    app_data$wells_sf = sf::st_sf(empty_wells_df_for_sf, geometry = sf::st_sfc(), crs = 4326)
    app_data$play_subplay_layers_list <- list()
    app_data$company_layers_list <- list()
    app_data$operator_lookup <- data.table::data.table()
    load_from_db <- TRUE
  })
} else {
  message(paste("Pre-processed RDS file not found at", processed_rds_file, "-- full data refresh required."))
}

if (!isFALSE(load_from_db)) {
  load_from_db <- isTRUE(load_from_db)
}

message(paste0("--- Status before potential DB/Shapefile load: load_from_db = ", load_from_db, " ---"))

if (load_from_db) {
  message("INITIATING DATA LOAD FROM DATABASE AND/OR SHAPEFILES...")
  if(is.null(app_data$wells_sf) || nrow(app_data$wells_sf) == 0 || !all(final_sf_column_names %in% names(app_data$wells_sf))) {
    app_data$wells_sf <- sf::st_sf(empty_wells_df_for_sf, geometry = sf::st_sfc(), crs = 4326)
  }
  app_data$play_subplay_layers_list <- list()
  app_data$company_layers_list <- list()
  
  if (is.null(con) || !dbIsValid(con)) { message("Attempting to (re)connect to database for data loading..."); con <- connect_to_db(); if (is.null(con) || !dbIsValid(con)) { stop("FATAL: Database connection failed. Cannot load primary data.") } }
  
  sql_well_master_base <- paste0(
    "SELECT W.UWI, W.GSL_UWI, W.SURFACE_LATITUDE, W.SURFACE_LONGITUDE, ",
    "W.BOTTOM_HOLE_LATITUDE, W.BOTTOM_HOLE_LONGITUDE, W.GSL_FULL_LATERAL_LENGTH, ",
    "W.ABANDONMENT_DATE, W.WELL_NAME, W.CURRENT_STATUS, W.OPERATOR AS OPERATOR_CODE, W.CONFIDENTIAL_TYPE, ",
    "P.STRAT_UNIT_ID, W.SPUD_DATE, PFS.FIRST_PROD_DATE, W.FINAL_TD, W.PROVINCE_STATE, W.COUNTRY, FL.FIELD_NAME ",
    "FROM WELL W ",
    "LEFT JOIN PDEN P ON W.GSL_UWI = P.GSL_UWI ",
    "LEFT JOIN FIELD FL ON W.ASSIGNED_FIELD = FL.FIELD_ID ",
    "LEFT JOIN PDEN_FIRST_SUM PFS ON W.GSL_UWI = PFS.GSL_UWI ",
    "WHERE W.SURFACE_LATITUDE IS NOT NULL AND W.SURFACE_LONGITUDE IS NOT NULL ",
    "AND (W.ABANDONMENT_DATE IS NULL OR W.ABANDONMENT_DATE > SYSDATE - (365*20))"
  )
  message("Fetching well master data from Oracle..."); wells_master_df_raw <- tryCatch({ dbGetQuery(con, sql_well_master_base) }, error = function(e) { warning(paste("Error fetching well master data from Oracle:", e$message)); data.frame() })
  wells_master_dt <- data.table::data.table()
  if (nrow(wells_master_df_raw) > 0) {
    message(paste("DB Load: Successfully loaded", nrow(wells_master_df_raw), "base well rows from DB.")); wells_master_dt <- data.table::as.data.table(wells_master_df_raw)
    if("UWI" %in% names(wells_master_dt)) wells_master_dt[, UWI_Std := standardize_uwi(UWI)] else wells_master_dt[, UWI_Std := NA_character_]
    if("GSL_UWI" %in% names(wells_master_dt)) wells_master_dt[, GSL_UWI_Std := standardize_uwi(GSL_UWI)] else wells_master_dt[, GSL_UWI_Std := NA_character_]
    if (!"FIELD_NAME" %in% names(wells_master_dt)) { wells_master_dt[, FieldName := NA_character_] } else { setnames(wells_master_dt, "FIELD_NAME", "FieldName") }
    if (!"STRAT_UNIT_ID" %in% names(wells_master_dt)) wells_master_dt[, STRAT_UNIT_ID := NA_character_]; wells_master_dt[, STRAT_UNIT_ID := as.character(STRAT_UNIT_ID)]
    
    if ("CONFIDENTIAL_TYPE" %in% names(wells_master_dt)) {
      wells_master_dt[, CONFIDENTIAL_TYPE := as.character(CONFIDENTIAL_TYPE)]
    } else {
      wells_master_dt[, CONFIDENTIAL_TYPE := NA_character_]
    }
    if (!"BOTTOM_HOLE_LATITUDE" %in% names(wells_master_dt)) wells_master_dt[, BOTTOM_HOLE_LATITUDE := NA_real_]
    if (!"BOTTOM_HOLE_LONGITUDE" %in% names(wells_master_dt)) wells_master_dt[, BOTTOM_HOLE_LONGITUDE := NA_real_]
    wells_master_dt[, BOTTOM_HOLE_LATITUDE := as.numeric(BOTTOM_HOLE_LATITUDE)]
    wells_master_dt[, BOTTOM_HOLE_LONGITUDE := as.numeric(BOTTOM_HOLE_LONGITUDE)]
    if (!"GSL_FULL_LATERAL_LENGTH" %in% names(wells_master_dt)) wells_master_dt[, GSL_FULL_LATERAL_LENGTH := NA_real_]
    wells_master_dt[, GSL_FULL_LATERAL_LENGTH := as.numeric(GSL_FULL_LATERAL_LENGTH)]
    
    
    unique_strat_ids <- unique(wells_master_dt$STRAT_UNIT_ID[!is.na(wells_master_dt$STRAT_UNIT_ID) & wells_master_dt$STRAT_UNIT_ID != ""])
    if (length(unique_strat_ids) > 0) {
      batch_size <- 500; strat_names_list <- list()
      for (i in seq(1, length(unique_strat_ids), by = batch_size)) {
        batch_ids <- unique_strat_ids[i:min(i + batch_size - 1, length(unique_strat_ids))]
        sql_strat_unit_names <- glue::glue_sql("SELECT STRAT_UNIT_ID, SHORT_NAME FROM STRAT_UNIT WHERE STRAT_UNIT_ID IN ({ids*})", ids = batch_ids, .con = con)
        strat_names_batch_df <- tryCatch({ dbGetQuery(con, sql_strat_unit_names) }, error = function(e) { data.frame()}); if(nrow(strat_names_batch_df) > 0) strat_names_list[[length(strat_names_list) + 1]] <- strat_names_batch_df
      }
      if (length(strat_names_list) > 0) {
        strat_names_df <- rbindlist(strat_names_list, use.names = TRUE, fill = TRUE); strat_names_dt <- data.table::as.data.table(strat_names_df)
        setnames(strat_names_dt, "SHORT_NAME", "Formation", skip_absent=TRUE); if (!"Formation" %in% names(strat_names_dt)) strat_names_dt[, Formation := NA_character_]
        if ("STRAT_UNIT_ID" %in% names(strat_names_dt)) strat_names_dt[, STRAT_UNIT_ID := as.character(STRAT_UNIT_ID)] else strat_names_dt[, STRAT_UNIT_ID := NA_character_]
        if("STRAT_UNIT_ID" %in% names(wells_master_dt) && "STRAT_UNIT_ID" %in% names(strat_names_dt)){ wells_master_dt <- merge(wells_master_dt, strat_names_dt[, .(STRAT_UNIT_ID, Formation)], by = "STRAT_UNIT_ID", all.x = TRUE, sort = FALSE)
        } else { wells_master_dt[, Formation := NA_character_] }
      } else { wells_master_dt[, Formation := NA_character_] }
    } else { wells_master_dt[, Formation := NA_character_] }
  } else { message("WARNING (DB Load): No well master data returned from Oracle. wells_master_dt is empty.") }
  operator_codes_df_raw <- safe_read_excel(woodmack_coverage_file_xlsx, sheet_name = "Operator", "Woodmack Coverage (Sheet: Operator)")
  operator_codes_dt <- clean_df_colnames(data.table::as.data.table(operator_codes_df_raw), "Operator Codes from Excel"); operator_codes_final_dt <- data.table()
  if (nrow(operator_codes_dt) > 0 && all(c("OPERATOR", "GSL_PARENT_BA_NAME") %in% names(operator_codes_dt))) {
    operator_codes_final_dt <- operator_codes_dt[, .(WoodmackJoinOperatorCode = as.character(OPERATOR), OperatorNameDisplay = GSL_PARENT_BA_NAME)][!is.na(WoodmackJoinOperatorCode) & WoodmackJoinOperatorCode != "" & !is.na(OperatorNameDisplay) & OperatorNameDisplay != ""]; operator_codes_final_dt <- unique(operator_codes_final_dt, by = "WoodmackJoinOperatorCode")
  }
  app_data$operator_lookup <- data.table::as.data.table(operator_codes_final_dt)
  combined_wells_dt <- data.table()
  if (nrow(wells_master_dt) > 0) {
    if ("OPERATOR_CODE" %in% names(wells_master_dt)) {
      wells_master_dt[, OPERATOR_CODE := as.character(OPERATOR_CODE)]
      if (nrow(operator_codes_final_dt) > 0) { combined_wells_dt <- merge(wells_master_dt, operator_codes_final_dt, by.x = "OPERATOR_CODE", by.y = "WoodmackJoinOperatorCode", all.x = TRUE)
      } else { combined_wells_dt <- wells_master_dt; combined_wells_dt[, OperatorNameDisplay := NA_character_] }
    } else { combined_wells_dt <- wells_master_dt; combined_wells_dt[, OperatorNameDisplay := NA_character_] }
  }
  if (nrow(combined_wells_dt) > 0) {
    db_to_r_names_map <- c(
      "UWI"="UWI", "GSL_UWI"="GSL_UWI", "SURFACE_LATITUDE"="SurfaceLatitude",
      "SURFACE_LONGITUDE"="SurfaceLongitude",
      "BOTTOM_HOLE_LATITUDE"="BH_Latitude", "BOTTOM_HOLE_LONGITUDE"="BH_Longitude",
      "GSL_FULL_LATERAL_LENGTH"="LateralLength", # Updated mapping for LateralLength
      "ABANDONMENT_DATE"="AbandonmentDate", "WELL_NAME"="WellName", "CURRENT_STATUS"="CurrentStatus",
      "OPERATOR_CODE"="OperatorCode", "STRAT_UNIT_ID"="StratUnitID",
      "SPUD_DATE"="SpudDate", "FIRST_PROD_DATE"="FirstProdDate",
      "FINAL_TD"="FinalTD", "PROVINCE_STATE"="ProvinceState", "COUNTRY"="Country",
      "UWI_Std"="UWI_Std", "GSL_UWI_Std"="GSL_UWI_Std",
      "OperatorNameDisplay"="OperatorName", "Formation"="Formation", "FieldName"="FieldName",
      "CONFIDENTIAL_TYPE"="ConfidentialType"
    )
    current_db_names <- names(combined_wells_dt)
    for (db_name in names(db_to_r_names_map)) {
      r_name <- db_to_r_names_map[[db_name]];
      if (db_name %in% current_db_names && db_name != r_name) {
        setnames(combined_wells_dt, db_name, r_name, skip_absent = TRUE)
      } else if (db_name %in% current_db_names && db_name == r_name && !r_name %in% current_db_names[current_db_names != db_name]){
        # Column already has the target R name, do nothing
      } else if (!r_name %in% names(combined_wells_dt) && db_name %in% current_db_names) {
        setnames(combined_wells_dt, db_name, r_name, skip_absent = TRUE)
      }
    }
    
    for(col_sf in final_sf_column_names){
      if(!col_sf %in% names(combined_wells_dt)) {
        col_type <- class(empty_wells_df_for_sf[[col_sf]]);
        if (col_type == "numeric") combined_wells_dt[, (col_sf) := NA_real_]
        else if (col_type == "Date") combined_wells_dt[, (col_sf) := as.Date(NA_character_)]
        else combined_wells_dt[, (col_sf) := NA_character_]
      } else {
        if (col_sf == "ConfidentialType" && !is.character(combined_wells_dt[[col_sf]])) {
          combined_wells_dt[, (col_sf) := as.character(get(col_sf))]
        }
        if (col_sf == "BH_Latitude" && !is.numeric(combined_wells_dt[[col_sf]])) {
          combined_wells_dt[, (col_sf) := as.numeric(get(col_sf))]
        }
        if (col_sf == "BH_Longitude" && !is.numeric(combined_wells_dt[[col_sf]])) {
          combined_wells_dt[, (col_sf) := as.numeric(get(col_sf))]
        }
        if (col_sf == "LateralLength" && !is.numeric(combined_wells_dt[[col_sf]])) {
          combined_wells_dt[, (col_sf) := as.numeric(get(col_sf))]
        }
      }
    }
    
    if("SurfaceLatitude" %in% names(combined_wells_dt) && !is.numeric(combined_wells_dt$SurfaceLatitude)) combined_wells_dt[, SurfaceLatitude := as.numeric(SurfaceLatitude)]
    if("SurfaceLongitude" %in% names(combined_wells_dt) && !is.numeric(combined_wells_dt$SurfaceLongitude)) combined_wells_dt[, SurfaceLongitude := as.numeric(SurfaceLongitude)]
    date_cols_to_convert_pascal <- c("SpudDate", "FirstProdDate", "AbandonmentDate")
    for(dc_pascal in date_cols_to_convert_pascal){ if(dc_pascal %in% names(combined_wells_dt) && !inherits(combined_wells_dt[[dc_pascal]], "Date")){ current_col_values <- combined_wells_dt[[dc_pascal]]; if(inherits(current_col_values, "POSIXct") || inherits(current_col_values, "POSIXlt")) { combined_wells_dt[, (dc_pascal) := as.Date(current_col_values)] } else { combined_wells_dt[, (dc_pascal) := as.Date(as.character(current_col_values), origin = "1970-01-01")] } } }
    combined_wells_for_sf <- combined_wells_dt[!is.na(SurfaceLatitude) & !is.na(SurfaceLongitude)]
    if (nrow(combined_wells_for_sf) > 0) {
      combined_wells_for_sf_df <- as.data.frame(combined_wells_for_sf[, ..final_sf_column_names]); coord_names_to_use <- c("SurfaceLongitude", "SurfaceLatitude")
      if (any(is.na(combined_wells_for_sf_df[[coord_names_to_use[1]]])) || any(is.na(combined_wells_for_sf_df[[coord_names_to_use[2]]]))) { combined_wells_for_sf_df <- combined_wells_for_sf_df[ !is.na(combined_wells_for_sf_df[[coord_names_to_use[1]]]) & !is.na(combined_wells_for_sf_df[[coord_names_to_use[2]]]), ] }
      if(nrow(combined_wells_for_sf_df) > 0) {
        app_data$wells_sf <- sf::st_as_sf(combined_wells_for_sf_df, coords = coord_names_to_use, crs = 4269, agr = "constant", remove = FALSE) %>% sf::st_transform(crs = 4326)
        if(any(!sf::st_is_valid(app_data$wells_sf), na.rm = TRUE)) app_data$wells_sf <- app_data$wells_sf[which(sf::st_is_valid(app_data$wells_sf) %in% TRUE), ]
      } else { message("DB Load: No valid rows after NA filter for coordinates before sf creation.")}
    } else { message("DB Load: No valid rows after filtering for non-NA PascalCase coordinates.") }
  } else { message("DB Load: combined_wells_dt is empty, no wells_sf created from DB.") }
  message(paste("DB Load: app_data$wells_sf object created/updated with", nrow(app_data$wells_sf), "features."))
  
  # --- SHAPEFILE LOADING LOGIC ---
  message("--- Loading Play/Subplay Acreage ---")
  message(paste("Checking directory:", play_subplay_shapefile_dir))
  play_subplay_layers_list_temp <- list()
  if (dir.exists(play_subplay_shapefile_dir)) {
    shp_files <- list.files(play_subplay_shapefile_dir, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
    message(paste("Found", length(shp_files), "potential play/subplay shapefiles in:", play_subplay_shapefile_dir))
    if(length(shp_files) > 0){
      play_subplay_layers_list_temp <- lapply(shp_files, function(p) {
        lname <- tools::file_path_sans_ext(basename(p))
        message(paste("Attempting to load play/subplay layer:", lname, "from:", p))
        ldata <- load_process_spatial_layer(p, lname, simplify = TRUE, tolerance = 100, make_valid_geom = TRUE)
        if (!is.null(ldata) && nrow(ldata) > 0 && inherits(ldata, "sf")) {
          message(paste("SUCCESS: Loaded play/subplay layer:", lname, "with", nrow(ldata), "features."))
          list(name = lname, data = ldata)
        } else {
          message(paste("FAILED or EMPTY: Play/subplay layer:", lname, "from:", p))
          NULL
        }
      })
      app_data$play_subplay_layers_list <- Filter(Negate(is.null), play_subplay_layers_list_temp)
    } else {
      message("No .shp files found in the play/subplay directory.")
    }
  } else { message(paste("Play/Subplay directory NOT FOUND:", play_subplay_shapefile_dir)) }
  message(paste("Loaded", length(app_data$play_subplay_layers_list), "valid play/subplay layers."))
  
  message("--- Loading DISSOLVED Company Acreage ---")
  message(paste("Checking directory:", company_shapefiles_dir))
  company_layers_list_temp <- list()
  if (dir.exists(company_shapefiles_dir)) {
    shp_files <- list.files(company_shapefiles_dir, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE, recursive = FALSE)
    message(paste("Found", length(shp_files), "potential company shapefiles in:", company_shapefiles_dir))
    if(length(shp_files) > 0){
      company_layers_list_temp <- lapply(shp_files, function(p) {
        lname <- tools::file_path_sans_ext(basename(p))
        message(paste("Attempting to load company layer:", lname, "from:", p))
        ldata <- load_process_spatial_layer(p, lname, simplify = FALSE, make_valid_geom = FALSE)
        if (!is.null(ldata) && nrow(ldata) > 0 && inherits(ldata, "sf")) {
          message(paste("SUCCESS: Loaded company layer:", lname, "with", nrow(ldata), "features."))
          list(name = lname, data = ldata)
        } else {
          message(paste("FAILED or EMPTY: Company layer:", lname, "from:", p))
          NULL
        }
      })
      app_data$company_layers_list <- Filter(Negate(is.null), company_layers_list_temp)
    } else {
      message("No .shp files found in the company directory.")
    }
  } else { message(paste("Company shapefile (Dissolved) directory NOT FOUND:", company_shapefiles_dir)) }
  message(paste("Loaded", length(app_data$company_layers_list), "valid company layers."))
  
  message(paste("Saving MERGED processed data (wells and layers) to:", processed_rds_file))
  tryCatch({
    saveRDS(app_data, file = processed_rds_file)
    message("Processed data (wells and layers) saved to RDS.")
  }, error = function(e){
    message(paste("Error saving RDS:", e$message))
  })
  # --- END OF SHAPEFILE LOADING LOGIC ---
  
} else {
  message("Cache satisfied; using cached wells and shapefiles without database refresh.")
}

wells_lookup_dt <- data.table::as.data.table(app_data$operator_lookup)

wells_sf_global <- app_data$wells_sf
if (inherits(wells_sf_global, "sf") && nrow(wells_lookup_dt) > 0) {
  lookup_df <- as.data.frame(wells_lookup_dt)
  if (!"OperatorCode" %in% names(lookup_df) && "WoodmackJoinOperatorCode" %in% names(lookup_df)) {
    lookup_df$OperatorCode <- as.character(lookup_df$WoodmackJoinOperatorCode)
  }
  if (!"OperatorName" %in% names(lookup_df) && "OperatorNameDisplay" %in% names(lookup_df)) {
    lookup_df$OperatorName <- as.character(lookup_df$OperatorNameDisplay)
  }
  if (!"OperatorName" %in% names(wells_sf_global)) {
    wells_sf_global$OperatorName <- rep(NA_character_, nrow(wells_sf_global))
  }
  if ("OperatorCode" %in% names(wells_sf_global) && "OperatorCode" %in% names(lookup_df) && "OperatorName" %in% names(lookup_df)) {
    idx_na <- which(is.na(wells_sf_global$OperatorName) | wells_sf_global$OperatorName == "")
    if (length(idx_na) > 0) {
      joined_names <- lookup_df$OperatorName[match(wells_sf_global$OperatorCode[idx_na], lookup_df$OperatorCode)]
      replace_idx <- which(!is.na(joined_names) & joined_names != "")
      if (length(replace_idx) > 0) {
        wells_sf_global$OperatorName[idx_na[replace_idx]] <- joined_names[replace_idx]
      }
    }
  }
}
play_subplay_layers_list_global <- app_data$play_subplay_layers_list
company_layers_list_global <- app_data$company_layers_list

initial_play_subplay_layer_names <- if (length(play_subplay_layers_list_global) > 0) {
  sort(sapply(play_subplay_layers_list_global, function(x) if(!is.null(x$name)) x$name else NA_character_))
} else { character(0) }
initial_play_subplay_layer_names <- initial_play_subplay_layer_names[!is.na(initial_play_subplay_layer_names)]

initial_company_layer_names <- if (length(company_layers_list_global) > 0) {
  sort(sapply(company_layers_list_global, function(x) if(!is.null(x$name)) x$name else NA_character_))
} else { character(0) }
initial_company_layer_names <- initial_company_layer_names[!is.na(initial_company_layer_names)]

message("--- FINAL CHECK OF wells_sf_global BEFORE SERVER ---")
if (!is.null(wells_sf_global) && inherits(wells_sf_global, "sf")) {
  message(paste("  nrow(wells_sf_global):", nrow(wells_sf_global)))
  if ("ConfidentialType" %in% names(wells_sf_global)) {
    message(paste("  'ConfidentialType' column IS present in wells_sf_global. Sample values:", paste(head(unique(na.omit(wells_sf_global$ConfidentialType))), collapse=", ")))
  } else {
    message("  'ConfidentialType' column IS NOT present in wells_sf_global.")
  }
  if ("BH_Latitude" %in% names(wells_sf_global)) {
    message(paste("  'BH_Latitude' column IS present. Sample:", head(na.omit(wells_sf_global$BH_Latitude), 3)))
  } else {
    message("  'BH_Latitude' column IS NOT present.")
  }
  if ("BH_Longitude" %in% names(wells_sf_global)) {
    message(paste("  'BH_Longitude' column IS present. Sample:", head(na.omit(wells_sf_global$BH_Longitude), 3)))
  } else {
    message("  'BH_Longitude' column IS NOT present.")
  }
  if ("LateralLength" %in% names(wells_sf_global)) { # Check for LateralLength
    message(paste("  'LateralLength' column IS present. Sample:", head(na.omit(wells_sf_global$LateralLength), 3)))
  } else {
    message("  'LateralLength' column IS NOT present.")
  }
  if ("FirstProdDate" %in% names(wells_sf_global) && inherits(wells_sf_global$FirstProdDate, "Date")) {
    message(paste("  'FirstProdDate' column IS present and is a Date. Sample:", head(na.omit(wells_sf_global$FirstProdDate), 3)))
  } else {
    message("  'FirstProdDate' column IS NOT present or not a Date type.")
  }
} else { message("  wells_sf_global is NULL or not an sf object before server starts.") }
message(paste("  Initial Play/Subplay Layers for Picker:", length(initial_play_subplay_layer_names), "Names:", paste(initial_play_subplay_layer_names, collapse=", ")))
message(paste("  Initial Company Layers for Picker:", length(initial_company_layer_names), "Names:", paste(initial_company_layer_names, collapse=", ")))


# --- UI ---
ui <- fluidPage(
  tags$head(tags$style(HTML(".shiny-notification { position:fixed; top: calc(5%); left: calc(50% - 150px); width: 300px; z-index: 2000 !important; }"))),
  titlePanel("Interactive Well and Acreage Map Application (DB V24 - Well Sticks & Province Filter)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Well Selection Criteria"),
      pickerInput("operator_filter", "Operator:", choices = c("Loading..."=""), selected = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, noneSelectedText = "Filter by Operator...", virtualScroll = TRUE, size=10)),
      pickerInput("formation_filter", "Formation:", choices = c("Loading..."=""), selected = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, noneSelectedText = "Filter by Formation...", virtualScroll = TRUE, size=10)),
      pickerInput("field_filter", "Field:", choices = c("Loading..."=""), selected = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, noneSelectedText = "Filter by Field...", virtualScroll = TRUE, size=10)),
      pickerInput("province_filter", "Province/State:", choices = c("Loading..."=""), selected = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, noneSelectedText = "Filter by Province/State...", virtualScroll = TRUE, size=5)),
      dateRangeInput("well_date_filter", "Filter by First Production Date:",
                     start = Sys.Date() - years(10), end = Sys.Date(),
                     min = as.Date("1900-01-01"), max = Sys.Date(),
                     format = "yyyy-mm-dd", startview = "year", width="100%"),
      checkboxInput("gor_include_cnd", "GOR uses Oil + Condensate (recommended)", TRUE),
      sliderInput("gor_range", "GOR filter (MCF/BBL)", min = 0, max = 50000,
                  value = c(0, 50000), step = 50),
      actionButton("update_map", "Apply Filters & Update Map", class = "btn-primary btn-block"),
      actionButton("reset_filters", "Reset All Filters", class = "btn-block"),
      actionButton("reconnect_db_button", "Reconnect to Database", class = "btn-warning btn-block", style="margin-top: 10px;"), # Reconnect Button
      hr(),
      h5(strong(htmlOutput("well_count_display"))),
      hr(),
      h5("Map Layers:"),
      pickerInput("play_subplay_filter", "Play/Subplay Boundaries:", choices = initial_play_subplay_layer_names, selected = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, noneSelectedText = "None", virtualScroll = TRUE, size=5)),
      pickerInput("company_acreage_filter", "Company Acreage:", choices = initial_company_layer_names, selected = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, noneSelectedText = "None", virtualScroll = TRUE, size=5))
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Well Map", leafletOutput("well_map", height = "85vh")),
        tabPanel("Production Analysis",
                 fluidRow(
                   column(12,
                          pickerInput("product_type_filter_analysis", "Filter Analyses by Product Type:",
                                      choices = c("Oil" = "OIL", "Condensate" = "CND", "Gas" = "GAS", "BOE (Oil+Cnd+Gas)" = "BOE"),
                                      selected = c("OIL", "CND", "GAS", "BOE"), # Default to all including BOE
                                      multiple = TRUE,
                                      options = pickerOptions(actionsBox = TRUE, noneSelectedText = "Select Product(s)..."))
                   )
                 ),
                 hr(),
                 tabsetPanel(
                   id = "prod_analysis_tabs",
                   tabPanel("Single Well Analysis",
                            h4("Daily Production Rate"),
                            selectInput("selected_well_for_prod", "Select Well for Production:", choices = c("Apply filters and click a well on map or select here" = "")),
                            uiOutput("production_date_slider_ui"),
                            plotlyOutput("production_plot", height = "45vh"),
                            hr(),
                            h5("Production Data Table (for selected well & date range)"),
                            downloadButton("download_prod_data", "Download Table as CSV"),
                            DT::dataTableOutput("production_table")
                   ),
                   tabPanel("Filtered Group Cumulative",
                            h4("Time Normalized Production by Group (Map-Filtered Wells)"),
                            p("This analysis uses wells currently displayed on the map based on the main filters. Production is normalized by reported lateral length (if available). Assumes lateral length is in feet."),
                            fluidRow(
                              column(6,
                                     selectInput("filtered_group_breakout_by", "Normalize & Group By:",
                                                 choices = c("Operator" = "OperatorName",
                                                             "Formation" = "Formation",
                                                             "Field" = "FieldName",
                                                             "Province/State" = "ProvinceState",
                                                             "First Prod Year" = "FirstProdYear"),
                                                 selected = "OperatorName")
                              ),
                              column(6,
                                     actionButton("calculate_filtered_cumulative", "Calculate Normalized & Cumulative Rates", class = "btn-info btn-block", style="margin-top: 25px;") # Align button
                              )
                            ),
                            hr(),
                            uiOutput("filtered_group_plot_title_normalized"), # Dynamic title
                            plotOutput("filtered_group_cumulative_plot_normalized", height = "45vh"),
                            hr(),
                            uiOutput("filtered_group_plot_title_cumulative_boe"), # Dynamic title for cumulative
                            plotOutput("filtered_group_cumulative_plot_cumulative_boe", height = "45vh"),
                            hr(),
                            # --- NEW CHART UI ---
                            uiOutput("filtered_group_plot_title_calendar_rate"),
                            plotOutput("filtered_group_calendar_rate_plot", height = "45vh"),
                            hr(),
                            # --- END NEW CHART UI ---
                            h5("Filtered Group Production Data Summary (by Selected Group)"),
                            downloadButton("download_filtered_group_prod_data", "Download Summary as CSV"),
                            DT::dataTableOutput("filtered_group_production_table")
                   ),
                   tabPanel("Type Curve Analysis (Arps)",
                            h4("Arps Decline Curve (Peak Normalized - Daily Rates) for Map-Filtered Wells"),
                            p("Generates an Arps type curve based on aligning wells by their peak production month (using average daily rates). Uses wells currently displayed on the map and respects the 'Filter Analyses by Product Type' selection above."),
                            fluidRow(
                              column(6, selectInput("arps_product_type", "Select Product for Arps:", choices = c("Oil/Condensate" = "Oil", "Gas" = "Gas"))),
                              column(6, selectInput("arps_model_type", "Select Arps Model:", choices = c("Hyperbolic" = "hyperbolic", "Exponential" = "exponential", "Harmonic" = "harmonic")))
                            ),
                            actionButton("generate_type_curve", "Generate Type Curve", class = "btn-info btn-block"),
                            hr(),
                            plotOutput("arps_type_curve_plot", height="50vh"),
                            h5("Fitted Arps Parameters, EUR & Decline Summary:"),
                            verbatimTextOutput("arps_parameters_output"),
                            hr(),
                            h5("Aggregated Data Used for Type Curve (Avg Daily Rate vs. Months Since Peak)"),
                            DT::dataTableOutput("arps_data_table")
                   ),
                   tabPanel("GOR",
                            h4("GOR & Gas Weighting (Map-filtered wells)"),
                            p("Uses current main filters, date range, and the Oil+Condensate toggle."),
                            fluidRow(
                              column(6, plotOutput("gor_trend_by_month_plot", height = "45vh")),
                              column(6, plotOutput("gas_weighting_by_vintage_plot", height = "45vh"))
                            ),
                            hr(),
                            downloadButton("download_gor_timeseries_csv", "Download GOR Timeseries (CSV)"),
                            DT::dataTableOutput("gor_timeseries_table")
                   ),
                   tabPanel("Operator Group Cumulative",
                            h4("Operator Group Average Daily Production Rate"),
                            p(strong("Note:"), " This tab shows gross production for selected operators over a specific date range, independent of map filters. For operator comparisons within the main filter context (Formation, Field, Province, Date), please use the 'Filtered Group Cumulative' tab."),
                            pickerInput("group_operator_filter", "Select Operator(s) to Group:",
                                        choices = c("Loading..."=""), selected = NULL, multiple = TRUE,
                                        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, noneSelectedText = "Select Operator(s)...", virtualScroll = TRUE, size=5, maxOptions = 5)),
                            dateRangeInput("group_prod_date_range", "Select Date Range for Grouped Analysis:",
                                           start = Sys.Date() - years(5), end = Sys.Date(),
                                           min = as.Date("1950-01-01"), max = Sys.Date(),
                                           format = "yyyy-mm-dd", startview = "year"),
                            actionButton("update_group_plot", "Update Operator Group Plot", class = "btn-info"),
                            hr(),
                            plotOutput("grouped_cumulative_plot", height = "50vh"),
                            h5("Operator Group Production Data Summary (Monthly, Cumulative, and Avg Daily Rates)"),
                            downloadButton("download_group_prod_data", "Download Operator Group Summary as CSV"),
                            DT::dataTableOutput("grouped_production_table")
                   ),
                   tabPanel("Gas Plants",
                            fluidRow(
                              column(4,
                                     selectInput("gp_month", "Month", choices = c("Loading..." = ""), selected = NULL),
                                     actionButton("gp_reload", "Load Gas Plant Data", class = "btn-primary")
                              ),
                              column(4,
                                     selectInput(
                                       "gp_color_by",
                                       "Color by",
                                       choices = c("Utilization" = "util", "Operator" = "op"),
                                       selected = "util"
                                     ),
                                     helpText("Utilization = Monthly Throughput / Monthly Capacity (from ST50).")
                              ),
                              column(4,
                                     helpText("Decoupled from well filters. Uses monthly volumes + ST50 capacity. Operator from monthly file.")
                              )
                            ),
                            leafletOutput("gp_map", height = "75vh"),
                            br(),
                            DT::DTOutput("gp_table")
                   ),
                   tabPanel("DUCs Over Time",
                            fluidRow(
                              column(
                                width = 4,

                                shinyWidgets::airDatepickerInput(
                                  inputId = "duc_dates",
                                  label   = "Snapshot dates",
                                  value   = c(as.Date("2024-12-31"), as.Date("2025-12-31")),
                                  multiple = TRUE,
                                  clearButton = TRUE
                                ),

                                sliderInput(
                                  "duc_min_hold_days",
                                  "Min days since spud to count as DUC",
                                  min = 0,
                                  max = 180,
                                  value = 30,
                                  step = 10
                                ),

                                sliderInput(
                                  "duc_max_hold_days",
                                  "Max days allowed since spud (exclude long-term zombies)",
                                  min = 30,
                                  max = 2000,
                                  value = 730,
                                  step = 30
                                ),

                                sliderInput(
                                  "duc_spud_recency_months",
                                  "Only include wells spud within last __ months (recency window)",
                                  min = 1,
                                  max = 60,
                                  value = 36,
                                  step = 1
                                ),

                                sliderInput(
                                  "duc_max_months_cap",
                                  "Max months between spud and first production to still call it 'DUC' (ignore wells that have been sitting longer than this many months without first production)",
                                  min = 1,
                                  max = 120,
                                  value = 24,
                                  step = 1
                                ),

                                checkboxInput(
                                  "duc_exclude_conf",
                                  "Exclude wells flagged Confidential",
                                  value = TRUE
                                ),

                                selectInput(
                                  "duc_group_by",
                                  "Group DUC counts by",
                                  choices = c(
                                    "Operator"        = "OperatorName",
                                    "Formation"       = "Formation",
                                    "Field"           = "FieldName",
                                    "Province/State"  = "ProvinceState"
                                  ),
                                  selected = "OperatorName"
                                ),

                                uiOutput("duc_group_filter_ui"),

                                actionButton(
                                  "duc_apply",
                                  "Calculate DUCs",
                                  class = "btn-primary"
                                ),

                                br(),
                                helpText("See below for the detailed DUC definition used in this tool."),
                                wellPanel(
                                  tags$strong("How this tool defines a DUC"),
                                  tags$ul(
                                    tags$li("You pick one or more snapshot dates (these can be month-ends or any reference date)."),
                                    tags$li(
                                      "For each snapshot date D, a well is counted as a DUC if:",
                                      tags$ul(
                                        tags$li("The well has been spud on or before D (SpudDate ≤ D)."),
                                        tags$li("The well has NOT started first production on/before D (FirstProdDate is blank OR FirstProdDate > D)."),
                                        tags$li("The well is not abandoned as of D (AbandonmentDate is blank OR AbandonmentDate > D)."),
                                        tags$li("The well has existed at least [Min days since spud] days by D."),
                                        tags$li("The well has existed no more than [Max days allowed since spud] days by D (drops multi-year zombies / economic suspensions)."),
                                        tags$li("The well was spud within the last [Recency window in months] months as of D (optional high-grading for current programs)."),
                                        tags$li("The well has not exceeded [Max months between spud and first production] months with no first production (prevents including very old inventory)."),
                                        tags$li("If 'Exclude wells flagged Confidential' is checked, wells flagged Confidential are dropped.")
                                      )
                                    ),
                                    tags$li("Counts are grouped by the selected dimension (Operator, Province/State, Formation, Field, etc.)."),
                                    tags$li("The bar chart shows DUC totals for each snapshot date side by side."),
                                    tags$li("The downloadable detail table lists every UWI counted as a DUC at each snapshot date, along with key timestamps.")
                                  ),
                                  tags$small("Counts are grouped by the selected dimension (Operator, Formation, Field, Province). The bar chart shows absolute DUC totals per snapshot date, not just deltas.")
                                )
                              ),
                              column(8,
                                     h4(textOutput("duc_headline")),
                                     plotlyOutput("duc_bar_compare", height = "45vh"),
                                     br(),
                                     downloadButton("duc_download", "Download DUC table (CSV)"),
                                     DTOutput("duc_table"),
                                     hr(),
                                     h4("DUC detail (well-level)"),
                                     div(style = "margin-bottom:8px;",
                                         downloadButton("download_duc_details_csv", "Download DUC detail (CSV)")
                                      ),
                                      DT::DTOutput("duc_detail_table")
                              )
                            )
                  ),
                  tabPanel(
                    "Shut-in Wells",
                    fluidRow(
                      column(
                        width = 3,
                        dateInput(
                          "shutin_snapshot_date",
                          "Snapshot date (as of):",
                          value = Sys.Date(),
                          min = as.Date("2015-01-01"),
                          max = Sys.Date()
                        ),
                        numericInput(
                          "shutin_no_prod_months",
                          "No production for at least (months):",
                          value = 2,
                          min = 1,
                          max = 12,
                          step = 1
                        ),
                        numericInput(
                          "shutin_recent_window_months",
                          "Lookback window for recent activity (months):",
                          value = 12,
                          min = 3,
                          max = 36,
                          step = 1
                        ),
                        actionButton(
                          "calculate_shutin",
                          "Calculate shut-in wells",
                          class = "btn-primary"
                        ),
                        helpText(
                          "Definition of 'shut-in' here:
",
                          "1) Well produced at least once in the last 'recent activity' window (X months before the snapshot).
",
                          "2) The well has zero production for EACH of the most recent 'no production' window (Y months before the snapshot).
",
                          "3) AbandonmentDate is blank or after the snapshot.
",
                          "This excludes dead wells that haven't flowed in years and focuses on wells that were active recently but are currently turned off."
                        )
                      ),
                      column(
                        width = 9,
                        plotOutput("shutin_plot", height = 350),
                        div(
                          style = "margin: 10px 0; display: flex; flex-wrap: wrap; gap: 10px;",
                          downloadButton("shutin_summary_download", "Download shut-in summary (CSV)"),
                          downloadButton("shutin_detail_download", "Download shut-in detail (CSV)")
                        ),
                        DTOutput("shutin_table")
                      )
                    )
                  )
                )
        )
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {

  wells_sf <- wells_sf_global

  # --- Helper: get operator choices with fallbacks & "(Unknown)" ---
  get_operator_choices <- function(sf_obj) {
    if (is.null(sf_obj) || !inherits(sf_obj, "sf") || nrow(sf_obj) == 0) return(character(0))
    op <- trimws(as.character(sf::st_drop_geometry(sf_obj)$OperatorName))
    # fallback to Excel join names if OperatorName mostly NA
    if (all(is.na(op) | op == "")) {
      if ("OperatorNameDisplay" %in% names(sf_obj)) {
        op <- trimws(as.character(sf_obj$OperatorNameDisplay))
      }
    }
    # fallback to any alternative columns already in the file
    if (all(is.na(op) | op == "") && "Operator" %in% names(sf_obj)) {
      op <- trimws(as.character(sf_obj$Operator))
    }
    op <- op[!is.na(op) & op != "" & op != "NA"]
    op <- sort(unique(op))
    if (!length(op)) op <- "(Unknown)"
    op
  }
  play_subplay_layers_list <- play_subplay_layers_list_global
  company_layers_list <- company_layers_list_global
  
  reactive_vals <- reactiveValues(
    wells_to_display = sf::st_sf(geometry = sf::st_sfc(), crs = 4326),
    wells_filtered_base = sf::st_sf(geometry = sf::st_sfc(), crs = 4326),
    map_df_with_gor = sf::st_sf(geometry = sf::st_sfc(), crs = 4326),
    has_map_been_updated_once = FALSE,
    current_selected_gsl_uwi_std = NULL,
    min_prod_date = as.Date("1900-01-01"),
    max_prod_date = Sys.Date(),
    min_first_prod_date_overall = as.Date("1900-01-01"),
    max_first_prod_date_overall = Sys.Date(),
    duc_comp = data.table::data.table(),
    duc_detail = data.table::data.table(),
    duc_groups_available = character(0),
    shutin_summary = data.table::data.table(),
    shutin_detail = data.table::data.table(),
    shutin_snapshot = as.Date(NA),
    shutin_no_prod_months = NA_real_,
    shutin_recent_window_months = NA_real_
  )

  # --- PATCH 1A: safe boolean for "Oil + Condensate" toggle
  use_cnd_reactive <- reactive({ isTRUE(input$gor_include_cnd) })

  fetch_monthly_gor <- function(uwi_vec, date_start, date_end, use_cnd = TRUE) {
    if (length(uwi_vec) == 0) return(data.table::data.table())

    use_cnd <- isTRUE(use_cnd)

    if (is.null(con) || !DBI::dbIsValid(con)) {
      con <<- connect_to_db()
      if (is.null(con) || !DBI::dbIsValid(con)) return(data.table::data.table())
    }

    uwi_batches <- split(uwi_vec, ceiling(seq_along(uwi_vec) / 300))
    prod_list <- vector("list", length(uwi_batches))
    for (i in seq_along(uwi_batches)) {
      sql <- glue::glue_sql(
        "SELECT GSL_UWI, YEAR, PRODUCT_TYPE, \
                JAN_VOLUME, FEB_VOLUME, MAR_VOLUME, APR_VOLUME, MAY_VOLUME, JUN_VOLUME, \
                JUL_VOLUME, AUG_VOLUME, SEP_VOLUME, OCT_VOLUME, NOV_VOLUME, DEC_VOLUME \
         FROM PDEN_VOL_BY_MONTH \
         WHERE GSL_UWI IN ({uwis*}) \
           AND ACTIVITY_TYPE = 'PRODUCTION' \
           AND PRODUCT_TYPE IN ('OIL','CND','GAS')",
        uwis = uwi_batches[[i]], .con = con
      )
      prod_list[[i]] <- tryCatch(
        data.table::as.data.table(DBI::dbGetQuery(con, sql)),
        error = function(e) data.table::data.table()
      )
    }
    raw <- data.table::rbindlist(prod_list, use.names = TRUE, fill = TRUE)
    if (nrow(raw) == 0) return(raw)

    if ("GSL_UWI" %in% names(raw)) raw[, GSL_UWI_Std := standardize_uwi(GSL_UWI)]
    dt <- clean_df_colnames(raw, "PDEN_VOL_BY_MONTH")
    if (!"GSL_UWI_STD" %in% names(dt)) {
      if ("GSL_UWI" %in% names(dt)) dt[, GSL_UWI_STD := standardize_uwi(GSL_UWI)]
    }
    data.table::setDT(dt)

    month_cols <- toupper(paste0(month.abb, "_VOLUME"))
    month_cols <- month_cols[month_cols %in% names(dt)]
    req_cols <- c("GSL_UWI_STD", "YEAR", "PRODUCT_TYPE")
    if (length(month_cols) != 12 || !all(req_cols %in% names(dt))) return(data.table::data.table())

    for (cn in c("YEAR", month_cols)) {
      if (!is.numeric(dt[[cn]])) dt[, (cn) := as.numeric(get(cn))]
    }

    long <- data.table::melt(
      dt,
      id.vars = c("GSL_UWI_STD", "YEAR", "PRODUCT_TYPE"),
      measure.vars = month_cols,
      variable.name = "MonCol",
      value.name = "VOL"
    )
    long[is.na(VOL), VOL := 0]
    long[, Month_Num := match(gsub("_VOLUME", "", MonCol), toupper(month.abb))]
    long[, PROD_DATE := as.Date(paste(YEAR, Month_Num, 1, sep = "-"))]
    long <- long[!is.na(PROD_DATE)]
    long <- long[PROD_DATE >= as.Date(date_start) & PROD_DATE <= as.Date(date_end)]

    long[, PRODUCT_TYPE := toupper(PRODUCT_TYPE)]
    oil <- long[PRODUCT_TYPE == "OIL", .(OilBBL = sum(VOL, na.rm = TRUE)), by = .(GSL_UWI_STD, PROD_DATE)]
    cnd <- long[PRODUCT_TYPE == "CND", .(CndBBL = sum(VOL, na.rm = TRUE)), by = .(GSL_UWI_STD, PROD_DATE)]
    gas <- long[PRODUCT_TYPE == "GAS", .(GasMCF = sum(VOL, na.rm = TRUE)), by = .(GSL_UWI_STD, PROD_DATE)]

    out <- merge(merge(oil, cnd, by = c("GSL_UWI_STD", "PROD_DATE"), all = TRUE),
                 gas, by = c("GSL_UWI_STD", "PROD_DATE"), all = TRUE)
    numeric_cols <- c("OilBBL", "CndBBL", "GasMCF")
    for (col in numeric_cols) {
      if (!col %in% names(out)) out[, (col) := 0]
      if (!is.numeric(out[[col]])) out[, (col) := as.numeric(get(col))]
      out[is.na(get(col)), (col) := 0]
    }
    out[, LiquidsBBL := OilBBL + if (use_cnd) CndBBL else 0]
    out[, GOR_MCF_PER_BBL := data.table::fifelse(LiquidsBBL > 0, GasMCF / LiquidsBBL, NA_real_)]
    out[!is.finite(GOR_MCF_PER_BBL) | GOR_MCF_PER_BBL < 0, GOR_MCF_PER_BBL := NA_real_]
    out[, GasWeighting := data.table::fifelse((GasMCF + LiquidsBBL) > 0, GasMCF / (GasMCF + LiquidsBBL), NA_real_)]

    data.table::setorder(out, GSL_UWI_STD, PROD_DATE)
    latest <- out[, .SD[.N], by = GSL_UWI_STD]
    latest[, `:=`(
      GOR_Latest = GOR_MCF_PER_BBL,
      GOR_Latest_Month = PROD_DATE,
      MonthlyGasMCF = GasMCF,
      MonthlyOilBBL = OilBBL,
      MonthlyCndBBL = CndBBL,
      MonthlyLiquidsBBL = LiquidsBBL,
      GasWeightingLatest = GasWeighting
    )]
    latest[, c("PROD_DATE", "GasMCF", "OilBBL", "CndBBL", "LiquidsBBL", "GOR_MCF_PER_BBL", "GasWeighting") := NULL]
    latest[]
  }

  compute_gor_timeseries_for_wells <- function(uwi_vec, date_start, date_end, use_cnd = TRUE) {
    if (length(uwi_vec) == 0) return(data.table::data.table())

    use_cnd <- isTRUE(use_cnd)

    if (is.null(con) || !DBI::dbIsValid(con)) {
      con <<- connect_to_db()
      if (is.null(con) || !DBI::dbIsValid(con)) return(data.table::data.table())
    }

    date_start <- as.Date(date_start)
    date_end <- as.Date(date_end)
    if (!is.finite(date_start)) date_start <- as.Date("1900-01-01")
    if (!is.finite(date_end)) date_end <- Sys.Date()

    uwi_batches <- split(uwi_vec, ceiling(seq_along(uwi_vec) / 300))
    prod_list <- vector("list", length(uwi_batches))
    for (i in seq_along(uwi_batches)) {
      sql <- glue::glue_sql(
        "SELECT GSL_UWI, YEAR, PRODUCT_TYPE, ",
        "       JAN_VOLUME, FEB_VOLUME, MAR_VOLUME, APR_VOLUME, MAY_VOLUME, JUN_VOLUME, ",
        "       JUL_VOLUME, AUG_VOLUME, SEP_VOLUME, OCT_VOLUME, NOV_VOLUME, DEC_VOLUME ",
        "FROM PDEN_VOL_BY_MONTH ",
        "WHERE GSL_UWI IN ({uwis*}) ",
        "  AND ACTIVITY_TYPE = 'PRODUCTION' ",
        "  AND PRODUCT_TYPE IN ('OIL','CND','GAS')",
        uwis = uwi_batches[[i]], .con = con
      )
      prod_list[[i]] <- tryCatch(
        data.table::as.data.table(DBI::dbGetQuery(con, sql)),
        error = function(e) data.table::data.table()
      )
    }
    raw <- data.table::rbindlist(prod_list, use.names = TRUE, fill = TRUE)
    if (nrow(raw) == 0) return(raw)

    if ("GSL_UWI" %in% names(raw)) raw[, GSL_UWI_Std := standardize_uwi(GSL_UWI)]
    dt <- clean_df_colnames(raw, "PDEN_VOL_BY_MONTH")
    if (!"GSL_UWI_STD" %in% names(dt)) {
      if ("GSL_UWI" %in% names(dt)) dt[, GSL_UWI_STD := standardize_uwi(GSL_UWI)]
    }
    data.table::setDT(dt)

    month_cols <- toupper(paste0(month.abb, "_VOLUME"))
    month_cols <- month_cols[month_cols %in% names(dt)]
    req_cols <- c("GSL_UWI_STD", "YEAR", "PRODUCT_TYPE")
    if (length(month_cols) != 12 || !all(req_cols %in% names(dt))) return(data.table::data.table())

    for (cn in c("YEAR", month_cols)) {
      if (!is.numeric(dt[[cn]])) dt[, (cn) := as.numeric(get(cn))]
    }

    long <- data.table::melt(
      dt,
      id.vars = c("GSL_UWI_STD", "YEAR", "PRODUCT_TYPE"),
      measure.vars = month_cols,
      variable.name = "MonCol",
      value.name = "VOL"
    )
    long[is.na(VOL), VOL := 0]
    long[, Month_Num := match(gsub("_VOLUME", "", MonCol), toupper(month.abb))]
    long[, PROD_DATE := as.Date(paste(YEAR, Month_Num, 1, sep = "-"))]
    long <- long[!is.na(PROD_DATE)]
    long <- long[PROD_DATE >= date_start & PROD_DATE <= date_end]

    long[, PRODUCT_TYPE := toupper(PRODUCT_TYPE)]
    oil <- long[PRODUCT_TYPE == "OIL", .(OilBBL = sum(VOL, na.rm = TRUE)), by = .(GSL_UWI_STD, PROD_DATE)]
    cnd <- long[PRODUCT_TYPE == "CND", .(CndBBL = sum(VOL, na.rm = TRUE)), by = .(GSL_UWI_STD, PROD_DATE)]
    gas <- long[PRODUCT_TYPE == "GAS", .(GasMCF = sum(VOL, na.rm = TRUE)), by = .(GSL_UWI_STD, PROD_DATE)]

    out <- merge(merge(oil, cnd, by = c("GSL_UWI_STD", "PROD_DATE"), all = TRUE),
                 gas, by = c("GSL_UWI_STD", "PROD_DATE"), all = TRUE)
    numeric_cols <- c("OilBBL", "CndBBL", "GasMCF")
    for (col in numeric_cols) {
      if (!col %in% names(out)) out[, (col) := 0]
      if (!is.numeric(out[[col]])) out[, (col) := as.numeric(get(col))]
      out[is.na(get(col)), (col) := 0]
    }
    out[, LiquidsBBL := OilBBL + if (use_cnd) CndBBL else 0]
    out[, GOR_MCF_PER_BBL := data.table::fifelse(LiquidsBBL > 0, GasMCF / LiquidsBBL, NA_real_)]
    out[!is.finite(GOR_MCF_PER_BBL) | GOR_MCF_PER_BBL < 0, GOR_MCF_PER_BBL := NA_real_]
    out[, GasWeighting := data.table::fifelse((GasMCF + LiquidsBBL) > 0, GasMCF / (GasMCF + LiquidsBBL), NA_real_)]

    data.table::setorder(out, GSL_UWI_STD, PROD_DATE)
    out
  }

  # Cap Inf/huge GOR values for plotting only
  cap_gor_for_plot <- function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    finite <- is.finite(x_num)
    if (!any(finite)) return(list(vals = rep(NA_real_, length(x_num)), cap = NA_real_))
    cap <- stats::quantile(x_num[finite], probs = 0.99, na.rm = TRUE)
    if (!is.finite(cap) || is.na(cap) || cap <= 0) cap <- max(x_num[finite], na.rm = TRUE)
    if (!is.finite(cap) || is.na(cap) || cap <= 0) cap <- 1
    x_cap <- x_num
    x_cap[is.infinite(x_cap)] <- cap
    x_cap[finite & x_cap > cap] <- cap
    list(vals = x_cap, cap = cap)
  }

  # --- PATCH 2: robust breaks for GOR
  make_gor_palette <- function(gor_vec, n = 7) {
    gor_vec <- as.numeric(gor_vec)
    gor_vec <- gor_vec[is.finite(gor_vec) & gor_vec >= 0]
    if (length(gor_vec) == 0L) {
      return(leaflet::colorNumeric("viridis", domain = c(0, 1)))
    }
    rng <- range(gor_vec, na.rm = TRUE)
    if (diff(rng) <= .Machine$double.eps) {
      rng[2] <- rng[1] + 1e-9
    }
    br <- unique(as.numeric(stats::quantile(gor_vec, probs = seq(0, 1, length.out = n), na.rm = TRUE)))
    br <- sort(unique(c(rng[1], br, rng[2])))
    if (length(br) < 3L) {
      br <- c(rng[1], rng[2] + 1e-9)
    }
    leaflet::colorBin("viridis", domain = gor_vec, bins = br, right = FALSE, na.color = "#9E9E9E")
  }

  compute_map_with_gor <- function(base_df) {
    if (is.null(base_df) || !inherits(base_df, "sf")) {
      return(sf::st_sf(geometry = sf::st_sfc(), crs = 4326))
    }

    df_out <- base_df
    if (!nrow(df_out)) {
      if (!"GOR_Latest" %in% names(df_out)) {
        df_out$GOR_Latest <- numeric(0)
        df_out$GOR_Latest_Month <- as.Date(character())
        df_out$MonthlyGasMCF <- numeric(0)
        df_out$MonthlyOilBBL <- numeric(0)
        df_out$MonthlyCndBBL <- numeric(0)
      }
      return(df_out)
    }

    if (!"GSL_UWI_Std" %in% names(df_out)) {
      df_out$GOR_Latest <- NA_real_
      df_out$GOR_Latest_Month <- as.Date(NA)
      df_out$MonthlyGasMCF <- NA_real_
      df_out$MonthlyOilBBL <- NA_real_
      df_out$MonthlyCndBBL <- NA_real_
      return(df_out)
    }

    map_uwis <- unique(stats::na.omit(df_out$GSL_UWI_Std))
    date_vals <- input$well_date_filter
    date_start <- if (!is.null(date_vals) && length(date_vals) >= 1) date_vals[1] else Sys.Date() - years(10)
    date_end <- if (!is.null(date_vals) && length(date_vals) >= 2) date_vals[2] else Sys.Date()
    gor_latest <- fetch_monthly_gor(map_uwis, date_start, date_end, use_cnd = use_cnd_reactive())

    df_out$GOR_Latest <- NA_real_
    df_out$GOR_Latest_Month <- as.Date(NA)
    df_out$MonthlyGasMCF <- NA_real_
    df_out$MonthlyOilBBL <- NA_real_
    df_out$MonthlyCndBBL <- NA_real_
    df_out$MonthlyLiquidsBBL <- NA_real_
    df_out$GasWeightingLatest <- NA_real_
    df_out$GOR_Capped_ForColor <- NA_real_

    if (nrow(gor_latest) > 0) {
      match_idx <- match(df_out$GSL_UWI_Std, gor_latest$GSL_UWI_STD)
      df_out$GOR_Latest <- gor_latest$GOR_Latest[match_idx]
      df_out$GOR_Latest_Month <- as.Date(gor_latest$GOR_Latest_Month[match_idx])
      df_out$MonthlyGasMCF <- gor_latest$MonthlyGasMCF[match_idx]
      df_out$MonthlyOilBBL <- gor_latest$MonthlyOilBBL[match_idx]
      df_out$MonthlyCndBBL <- gor_latest$MonthlyCndBBL[match_idx]
      if ("MonthlyLiquidsBBL" %in% names(gor_latest)) {
        df_out$MonthlyLiquidsBBL <- gor_latest$MonthlyLiquidsBBL[match_idx]
      }
      if ("GasWeightingLatest" %in% names(gor_latest)) {
        df_out$GasWeightingLatest <- gor_latest$GasWeightingLatest[match_idx]
      }
    }

    range_vals <- input$gor_range
    capd_filter <- cap_gor_for_plot(df_out$GOR_Latest)
    gor_filter_vals <- capd_filter$vals
    if (!is.null(range_vals) && length(range_vals) == 2) {
      lo <- range_vals[1]
      hi <- range_vals[2]
      keep_idx <- is.na(df_out$GOR_Latest) | (gor_filter_vals >= lo & gor_filter_vals <= hi)
      keep_idx <- as.logical(keep_idx)
      keep_idx[is.na(keep_idx)] <- FALSE
      if (!all(keep_idx)) {
        df_out <- df_out[keep_idx, , drop = FALSE]
        gor_filter_vals <- gor_filter_vals[keep_idx]
      }
    }
    if (length(gor_filter_vals) == nrow(df_out)) {
      df_out$GOR_Capped_ForColor <- gor_filter_vals
    } else {
      df_out$GOR_Capped_ForColor <- rep(NA_real_, nrow(df_out))
    }

    df_out
  }

  update_well_selection_choices <- function(df) {
    well_choices_for_prod <- c("Apply filters or click a well" = "")
    if (!is.null(df) && nrow(df) > 0 && all(c("GSL_UWI_Std", "WellName", "UWI") %in% names(df))) {
      clean_for_display <- function(text_vector) {
        if (is.null(text_vector)) return(rep("[Missing Data]", length(text_vector)))
        text_vector <- as.character(text_vector)
        cleaned_text <- iconv(text_vector, from = "", to = "UTF-8", sub = "?")
        cleaned_text[is.na(cleaned_text) & !is.na(text_vector)] <- "[Encoding Issue]"
        cleaned_text[is.na(cleaned_text)] <- "[Missing]"
        cleaned_text
      }
      well_name_cleaned <- clean_for_display(df$WellName)
      uwi_cleaned <- clean_for_display(df$UWI)
      gsl_uwi_std_values <- df$GSL_UWI_Std
      valid_gsl_uwis <- !is.na(gsl_uwi_std_values) & gsl_uwi_std_values != ""
      if (any(valid_gsl_uwis)) {
        display_names_filtered <- paste(
          str_trunc(well_name_cleaned[valid_gsl_uwis], width = 30, side = "right", ellipsis = "..."),
          "- UWI:",
          str_trunc(uwi_cleaned[valid_gsl_uwis], width = 15, side = "right", ellipsis = "...")
        )
        well_choices_for_prod <- stats::setNames(gsl_uwi_std_values[valid_gsl_uwis], display_names_filtered)
        if (anyDuplicated(names(well_choices_for_prod))) {
          names(well_choices_for_prod) <- make.unique(names(well_choices_for_prod))
        }
        well_choices_for_prod <- c("Select a well from filtered list..." = "", well_choices_for_prod)
      } else {
        well_choices_for_prod <- c("No wells with valid IDs in filter" = "")
      }
    } else {
      well_choices_for_prod <- c("Filtered list empty or key IDs missing" = "")
    }

    current_selection <- isolate(reactive_vals$current_selected_gsl_uwi_std)
    valid_options <- unname(well_choices_for_prod[well_choices_for_prod != ""])
    if (!is.null(current_selection) && current_selection %in% valid_options) {
      updateSelectInput(session, "selected_well_for_prod", choices = well_choices_for_prod, selected = current_selection)
    } else {
      updateSelectInput(session, "selected_well_for_prod", choices = well_choices_for_prod, selected = "")
      if (!is.null(current_selection) && current_selection != "") reactive_vals$current_selected_gsl_uwi_std <- NULL
    }
  }

  # ---- DUC logic helpers ----

  # Returns TRUE/FALSE vector telling whether each row is considered a DUC
  is_duc_at <- function(
    dt,
    snap_date,
    min_hold_days      = 30L,
    max_hold_days      = 730L,
    recency_months     = 36L,
    max_months_cap     = 24L,
    exclude_conf       = TRUE
  ) {
    d <- as.Date(snap_date)

    # Core conditions
    drilled_before_snap <- !is.na(dt$SpudDate) & dt$SpudDate <= d
    not_on_prod_yet     <- (is.na(dt$FirstProdDate) | dt$FirstProdDate > d)
    not_abandoned       <- (is.na(dt$AbandonmentDate) | dt$AbandonmentDate > d)

    # Age since spud at snapshot
    age_days <- as.numeric(d - dt$SpudDate)

    # 1. Minimum hold threshold (exclude wells that are too fresh)
    long_enough <- !is.na(age_days) & (age_days >= as.numeric(min_hold_days))

    # 2. Maximum hold threshold (exclude zombie wells that have sat for years)
    not_too_old <- !is.na(age_days) & (age_days <= as.numeric(max_hold_days))

    # 3. Recency filter: spud must be within the last N months at snapshot
    #    Convert months to ~30.4375 days for a rough but consistent cutoff.
    recency_days <- as.numeric(recency_months) * 30.4375
    recent_enough <- !is.na(age_days) & (age_days <= recency_days)

    # 4. Maximum months between spud and first production cap
    cap_days <- as.numeric(max_months_cap) * 30.4375
    within_month_cap <- !is.na(age_days) & (age_days <= cap_days)

    # Confidential filter
    if (exclude_conf) {
      conf_ok <- (is.na(dt$ConfidentialType) |
                  dt$ConfidentialType == "" |
                  toupper(dt$ConfidentialType) == "NON-CONFIDENTIAL")
    } else {
      conf_ok <- TRUE
    }

    drilled_before_snap &
      not_on_prod_yet &
      not_abandoned &
      long_enough &
      not_too_old &
      recent_enough &
      within_month_cap &
      conf_ok
  }

  duc_detail_for_date <- function(
    wx_dt,
    snap_date,
    min_hold_days,
    max_hold_days,
    recency_months,
    max_months_cap,
    exclude_conf,
    group_col
  ) {
    d <- as.Date(snap_date)

    keep_mask <- is_duc_at(
      dt             = wx_dt,
      snap_date      = d,
      min_hold_days  = min_hold_days,
      max_hold_days  = max_hold_days,
      recency_months = recency_months,
      max_months_cap = max_months_cap,
      exclude_conf   = exclude_conf
    )

    if (!any(keep_mask, na.rm = TRUE)) {
      return(data.table::data.table())
    }

    out <- data.table::copy(wx_dt[keep_mask])

    # audit columns
    out[, SnapshotDate := d]
    out[, DaysSinceSpud := as.numeric(SnapshotDate - SpudDate)]
    out[, MonthsSinceSpud := round(DaysSinceSpud / 30.4375, 1)]
    out[, MinHold_OK := DaysSinceSpud >= as.numeric(min_hold_days)]
    out[, MaxHold_OK := DaysSinceSpud <= as.numeric(max_hold_days)]
    out[, InRecencyWindow := DaysSinceSpud <= as.numeric(recency_months) * 30.4375]
    out[, MaxMonthsCap_OK := DaysSinceSpud <= as.numeric(max_months_cap) * 30.4375]
    out[, NotOnProd := (is.na(FirstProdDate) | FirstProdDate > SnapshotDate)]
    out[, NotAbandoned := (is.na(AbandonmentDate) | AbandonmentDate > SnapshotDate)]
    out[, ConfidentialFlag := ifelse(is.na(ConfidentialType) | ConfidentialType == "", "No", "Yes")]
    out[, HasProducedYet := !is.na(FirstProdDate) & FirstProdDate <= SnapshotDate]
    out[, IsCountedAsDUC := TRUE]

    # grouping label used downstream
    grp <- group_col
    out[, Group := {
      val <- get(grp)
      ifelse(is.na(val) | val == "", "(Unknown)", as.character(val))
    }]

    out[, .(
      UWI,
      Group,
      GSL_UWI_Std,
      OperatorName,
      ProvinceState,
      Formation,
      FieldName,
      SpudDate,
      FirstProdDate,
      AbandonmentDate,
      SnapshotDate,
      DaysSinceSpud,
      MonthsSinceSpud,
      MinHold_OK,
      MaxHold_OK,
      InRecencyWindow,
      MaxMonthsCap_OK,
      NotOnProd,
      NotAbandoned,
      ConfidentialFlag,
      HasProducedYet,
      IsCountedAsDUC
    )]
  }

  fetch_monthly_production_totals <- function(uwi_vec, date_start, date_end) {
    if (length(uwi_vec) == 0) return(data.table::data.table())
    dt <- fetch_monthly_gor(uwi_vec, date_start, date_end, use_cnd = TRUE)
    if (is.null(dt) || !nrow(dt)) return(data.table::data.table())
    if (!"GSL_UWI_STD" %in% names(dt)) return(data.table::data.table())

    prod_dt <- data.table::copy(dt)
    if (!"PROD_DATE" %in% names(prod_dt)) return(data.table::data.table())

    prod_dt[, PROD_MONTH := lubridate::floor_date(PROD_DATE, "month")]
    name_map <- list(
      OilBBL = intersect(c("OilBBL", "OILBBL"), names(prod_dt)),
      CndBBL = intersect(c("CndBBL", "CNDBBL"), names(prod_dt)),
      GasMCF = intersect(c("GasMCF", "GASMCF"), names(prod_dt))
    )
    for (nm in names(name_map)) {
      src <- setdiff(name_map[[nm]], nm)
      if (length(src) > 0) data.table::setnames(prod_dt, old = src[1], new = nm, skip_absent = TRUE)
    }
    for (col in c("OilBBL", "CndBBL", "GasMCF")) {
      if (!col %in% names(prod_dt)) prod_dt[, (col) := 0]
      if (!is.numeric(prod_dt[[col]])) prod_dt[, (col) := as.numeric(get(col))]
      prod_dt[is.na(get(col)), (col) := 0]
    }
    prod_dt[, TotalVolume := OilBBL + CndBBL + GasMCF]
    prod_dt[, .(
      OilBBL = sum(OilBBL, na.rm = TRUE),
      CndBBL = sum(CndBBL, na.rm = TRUE),
      GasMCF = sum(GasMCF, na.rm = TRUE),
      TotalVolume = sum(TotalVolume, na.rm = TRUE)
    ), by = .(GSL_UWI_STD, PROD_MONTH)]
  }
  
  # Initial population of pickers (non-cascading)
  observe({
    req(wells_sf_global, nrow(wells_sf_global) > 0)
    message("SERVER OBSERVE (Initial Picker Population & Date Slider Range): Entered observer.")
    
    op_choices_init <- get_operator_choices(wells_sf_global)
    form_choices_init <- prepare_filter_choices(wells_sf_global$Formation, "Formation (initial)")
    fld_choices_init <- prepare_filter_choices(wells_sf_global$FieldName, "FieldName (initial)")
    prov_choices_init <- prepare_filter_choices(wells_sf_global$ProvinceState, "ProvinceState (initial)")

    if (length(op_choices_init) == 0) {
      showNotification("No values available for this filter under current selections.", type = "warning", duration = 5)
      updatePickerInput(session, "operator_filter", choices = c("(Unknown)" = "(Unknown)"), selected = NULL)
      updatePickerInput(session, "group_operator_filter", choices = c("(Unknown)" = "(Unknown)"), selected = NULL)
    } else {
      updatePickerInput(session, "operator_filter", choices = op_choices_init, selected = NULL)
      updatePickerInput(session, "group_operator_filter", choices = op_choices_init, selected = NULL)
    }

    if (length(form_choices_init) == 0) {
      showNotification("No values available for this filter under current selections.", type = "warning", duration = 5)
      updatePickerInput(session, "formation_filter", choices = c("(Unknown)" = "(Unknown)"), selected = NULL)
    } else {
      updatePickerInput(session, "formation_filter", choices = form_choices_init, selected = NULL)
    }

    if (length(fld_choices_init) == 0) {
      showNotification("No values available for this filter under current selections.", type = "warning", duration = 5)
      updatePickerInput(session, "field_filter", choices = c("(Unknown)" = "(Unknown)"), selected = NULL)
    } else {
      updatePickerInput(session, "field_filter", choices = fld_choices_init, selected = NULL)
    }

    if (length(prov_choices_init) == 0) {
      showNotification("No values available for this filter under current selections.", type = "warning", duration = 5)
      updatePickerInput(session, "province_filter", choices = c("(Unknown)" = "(Unknown)"), selected = NULL)
    } else {
      updatePickerInput(session, "province_filter", choices = prov_choices_init, selected = NULL)
    }
    
    updatePickerInput(session, "play_subplay_filter",
                      choices = if(length(initial_play_subplay_layer_names)>0) initial_play_subplay_layer_names else c("No Layers Loaded" = ""),
                      selected = NULL)
    updatePickerInput(session, "company_acreage_filter",
                      choices = if(length(initial_company_layer_names)>0) initial_company_layer_names else c("No Layers Loaded" = ""),
                      selected = NULL)
    
    if (nrow(wells_sf_global) > 0 && "FirstProdDate" %in% names(wells_sf_global) && inherits(wells_sf_global$FirstProdDate, "Date") && sum(!is.na(wells_sf_global$FirstProdDate)) > 0) {
      min_fp_date <- min(wells_sf_global$FirstProdDate, na.rm = TRUE)
      max_fp_date <- max(wells_sf_global$FirstProdDate, na.rm = TRUE)
      reactive_vals$min_first_prod_date_overall <- min_fp_date
      reactive_vals$max_first_prod_date_overall <- max_fp_date
      default_start_date <- max(min_fp_date, max_fp_date - years(10), na.rm = TRUE)
      updateDateRangeInput(session, "well_date_filter",
                           min = min_fp_date, max = max_fp_date,
                           start = default_start_date, end = max_fp_date)
    } else {
      updateDateRangeInput(session, "well_date_filter",
                           min = as.Date("1900-01-01"), max = Sys.Date(),
                           start = Sys.Date() - years(10), end = Sys.Date())
    }
    message("SERVER: Initial Picker choices and date slider updated (non-cascading).")
    # --- GAS PLANTS SERVER ---
    
    gasplant_data <- eventReactive(input$gp_reload, {
      st50_cap <- tryCatch(load_st50_capacity(GAS_BASE_DIR, ST50_FILE),
                           error=function(e){ message("[GP] ST50 load fail: ", e$message); tibble::tibble() })
      monthly_csv <- file.path(GAS_BASE_DIR, MONTHLY_FILE)
      if (is.na(monthly_csv) || !nzchar(monthly_csv) || !file.exists(monthly_csv)) {
        showNotification("No gas plant monthly CSV found in GAS_BASE_DIR.", type="error", duration=5)
        return(list(plant_monthly=tibble::tibble(), gasplants_joined=tibble::tibble(), st50=st50_cap, months=character(0)))
      }
      monthly_raw <- tryCatch(readr::read_csv(monthly_csv, show_col_types = FALSE, guess_max = 200000),
                              error=function(e){ message("[GP] monthly read fail: ", e$message); NULL })
      if (is.null(monthly_raw) || !nrow(monthly_raw)) {
        showNotification("Monthly gas plant CSV is empty or unreadable.", type="error", duration=5)
        return(list(plant_monthly=tibble::tibble(), gasplants_joined=tibble::tibble(), st50=st50_cap, months=character(0)))
      }
      
      monthly_raw <- monthly_raw |>
        map_col(c("volume","gase3m3","gas_e3m3","gas_vol_e3m3"), "Volume") |>
        map_col(c("product","productid","substance"), "Product") |>
        map_col(c("activityid","activity"), "ActivityID") |>
        map_col(c("reportingfacilityid","reporting facility id","facilityid"), "ReportingFacilityID") |>
        map_col(c("reportingfacilitytype","facilitytype"), "ReportingFacilityType") |>
        map_col(c("reportingfacilitysubtypedesc","facilitysubtype","facility subtype"), "ReportingFacilitySubtypeDesc") |>
        map_col(c("productionmonth","production month","prodmonth","month"), "ProductionMonth") |>
        map_col(c("operatorname","operator","facilityoperatorbaname"), "OperatorName") |>
        map_col(c("tofacilitytype","totype","fromtoidtype","fromtotype"), "ToFacilityType") |>
        map_col(c("provincestate","province"), "ProvinceState")
      
      req <- c("ReportingFacilityID","ReportingFacilityType","ReportingFacilitySubtypeDesc","ActivityID","Product","Volume","ProductionMonth")
      missing <- setdiff(req, names(monthly_raw))
      if (length(missing)) {
        showNotification(paste("Monthly file missing:", paste(missing, collapse=", ")), type="error", duration=6)
        return(list(plant_monthly=tibble::tibble(), gasplants_joined=tibble::tibble(), st50=st50_cap, months=character(0)))
      }
      
      monthly <- tibble::as_tibble(monthly_raw) |>
        dplyr::mutate(
          FacilityID_norm = to_upper_trim(ReportingFacilityID),
          ActivityUpper   = to_upper_trim(ActivityID),
          ProductUpper    = to_upper_trim(Product),
          OperatorName    = if ("OperatorName" %in% names(monthly_raw)) as.character(monthly_raw$OperatorName) else NA_character_,
          ToTypeUpper     = if ("ToFacilityType" %in% names(monthly_raw)) to_upper_trim(monthly_raw$ToFacilityType) else NA_character_,
          ProvinceUpper   = if ("ProvinceState" %in% names(monthly_raw)) to_upper_trim(monthly_raw$ProvinceState) else NA_character_,
          Volume          = suppressWarnings(as.numeric(gsub(",", "", Volume))),
          ProductionMonth_raw = as.character(ProductionMonth)
        )
      monthly$Volume[is.na(monthly$Volume)] <- 0
      pm <- ifelse(grepl("^\\d{4}-\\d{2}$", monthly$ProductionMonth_raw), paste0(monthly$ProductionMonth_raw,"-01"), monthly$ProductionMonth_raw)
      monthly$ProductionMonth <- suppressWarnings(as.Date(pm))
      monthly <- monthly |> dplyr::filter(!is.na(ProductionMonth))
      
      gp <- monthly |>
        dplyr::filter(!is.na(FacilityID_norm) & FacilityID_norm != "") |>
        dplyr::filter(is_gas_plant(ReportingFacilityType, ReportingFacilitySubtypeDesc))
      if ("ProvinceUpper" %in% names(gp)) gp <- gp |> dplyr::filter(is.na(ProvinceUpper) | ProvinceUpper == "AB")
      if (!nrow(gp)) {
        showNotification("No gas plant rows after filters.", type="warning", duration=4)
        return(list(plant_monthly=tibble::tibble(), gasplants_joined=tibble::tibble(), st50=st50_cap, months=character(0)))
      }
      
      base_info <- gp |>
        dplyr::group_by(ProductionMonth, FacilityID_norm) |>
        dplyr::summarise(
          facility_type    = dplyr::first(ReportingFacilityType[!is.na(ReportingFacilityType)]),
          facility_subtype = dplyr::first(ReportingFacilitySubtypeDesc[!is.na(ReportingFacilitySubtypeDesc)]),
          operator_monthly = dplyr::first(OperatorName[!is.na(OperatorName) & OperatorName != ""]),
          .groups = "drop"
        )
      
      receipts <- gp |>
        dplyr::filter(ProductUpper == "GAS", ActivityUpper %in% c("REC","RCPT","RECEIPT")) |>
        dplyr::group_by(ProductionMonth, FacilityID_norm) |>
        dplyr::summarise(receipts_gas_e3m3 = sum(Volume, na.rm = TRUE), .groups = "drop")
      
      disp_all <- gp |> dplyr::filter(ActivityUpper %in% c("DISP","PURDISP"))
      gas_disp <- disp_all |> dplyr::filter(ProductUpper == "GAS")
      gas_disp_filtered <- gas_disp |> dplyr::filter(is.na(ToTypeUpper) | ToTypeUpper %nin% c("GP","GS"))
      
      sumv <- function(df, name) {
        if (!nrow(df)) tibble::tibble(ProductionMonth = as.Date(character()), FacilityID_norm = character(), "{name}" := numeric())
        else df |>
          dplyr::group_by(ProductionMonth, FacilityID_norm) |>
          dplyr::summarise("{name}" := sum(Volume, na.rm = TRUE), .groups = "drop")
      }
      
      disp_total <- sumv(gas_disp_filtered, "dispositions_gas_e3m3")
      sales_codes <- c("PL","PIPE","SLS"); fuel_codes <- c("FUEL"); flare_codes <- c("FLARE","FLR")
      disp_sales <- sumv(gas_disp_filtered |> dplyr::filter(!is.na(ToTypeUpper) & ToTypeUpper %in% sales_codes), "dispositions_sales_gas_e3m3")
      disp_fuel  <- sumv(gas_disp_filtered |> dplyr::filter((!is.na(ToTypeUpper) & ToTypeUpper %in% fuel_codes)  | grepl("FUEL", to_upper_trim(ReportingFacilitySubtypeDesc))), "dispositions_fuel_gas_e3m3")
      disp_flare <- sumv(gas_disp_filtered |> dplyr::filter((!is.na(ToTypeUpper) & ToTypeUpper %in% flare_codes) | grepl("FLAR", to_upper_trim(ReportingFacilitySubtypeDesc))), "dispositions_flare_gas_e3m3")
      ngl_out    <- sumv(disp_all |> dplyr::filter(!is.na(ProductUpper) & ProductUpper != "GAS"), "ngl_out_e3m3")
      
      plant_monthly <- base_info |>
        dplyr::full_join(receipts,   by=c("ProductionMonth","FacilityID_norm")) |>
        dplyr::full_join(disp_total, by=c("ProductionMonth","FacilityID_norm")) |>
        dplyr::full_join(disp_sales, by=c("ProductionMonth","FacilityID_norm")) |>
        dplyr::full_join(disp_fuel,  by=c("ProductionMonth","FacilityID_norm")) |>
        dplyr::full_join(disp_flare, by=c("ProductionMonth","FacilityID_norm")) |>
        dplyr::full_join(ngl_out,    by=c("ProductionMonth","FacilityID_norm")) |>
        dplyr::mutate(
          across(c(receipts_gas_e3m3, dispositions_gas_e3m3, dispositions_sales_gas_e3m3,
                   dispositions_fuel_gas_e3m3, dispositions_flare_gas_e3m3, ngl_out_e3m3),
                 ~ tidyr::replace_na(.x, 0)),
          dispositions_other_gas_e3m3 = pmax(dispositions_gas_e3m3 - (dispositions_sales_gas_e3m3 + dispositions_fuel_gas_e3m3 + dispositions_flare_gas_e3m3), 0),
          throughput_gas_e3m3 = dplyr::if_else(receipts_gas_e3m3 > 0, receipts_gas_e3m3, dispositions_gas_e3m3),
          month = as.Date(ProductionMonth),
          facility_id = FacilityID_norm
        ) |>
        dplyr::filter(!is.na(month)) |>
        dplyr::filter(receipts_gas_e3m3 > 0 | dispositions_gas_e3m3 > 0) |>
        dplyr::arrange(month, facility_id)
      
      if (!"operator_monthly" %in% names(plant_monthly)) {
        plant_monthly$operator_monthly <- NA_character_
      }
      
      gasplants_joined <- plant_monthly |>
        dplyr::left_join(st50_cap |> dplyr::select(FacilityID_norm, FacilityName, Latitude, Longitude, Operator_cap, FacilityType_cap, monthly_capacity_e3m3),
                         by = c("facility_id" = "FacilityID_norm")) |>
        dplyr::mutate(
          Operator = dplyr::coalesce(operator_monthly, Operator_cap, "(Unknown)"),
          Facility = dplyr::coalesce(FacilityName, facility_id),
          FacilityType_display = dplyr::coalesce(FacilityType_cap, facility_subtype, facility_type),
          monthly_throughput_e3m3 = throughput_gas_e3m3,
          utilization = dplyr::if_else(is.finite(monthly_capacity_e3m3) & monthly_capacity_e3m3 > 0,
                                       monthly_throughput_e3m3 / monthly_capacity_e3m3, NA_real_),
          utilization_pct = utilization * 100
        )
      if (!"monthly_capacity_e3m3" %in% names(gasplants_joined)) {
        gasplants_joined$monthly_capacity_e3m3 <- rep(NA_real_, nrow(gasplants_joined))
      }
      if (!"monthly_throughput_e3m3" %in% names(gasplants_joined)) {
        gasplants_joined$monthly_throughput_e3m3 <- rep(0, nrow(gasplants_joined))
      }
      gasplants_joined$monthly_throughput_e3m3[is.na(gasplants_joined$monthly_throughput_e3m3)] <- 0
      if ("utilization" %in% names(gasplants_joined)) {
        gasplants_joined$utilization[!is.finite(gasplants_joined$utilization)] <- NA_real_
      }
      gasplants_joined$utilization_pct <- gasplants_joined$utilization * 100
      gasplants_joined$Operator <- normalize_operator_label(gasplants_joined$Operator)
      gasplants_joined$FacilityType_display <- normalize_type_label(gasplants_joined$FacilityType_display)
      gasplants_joined$Facility[is.na(gasplants_joined$Facility) | gasplants_joined$Facility == ""] <- gasplants_joined$facility_id[is.na(gasplants_joined$Facility) | gasplants_joined$Facility == ""]
      
      months <- sort(unique(format(gasplants_joined$month, "%Y-%m")))
      list(plant_monthly = plant_monthly, gasplants_joined = gasplants_joined, st50 = st50_cap, months = months)
    }, ignoreInit = TRUE)
    
    observeEvent(gasplant_data(), {
      d <- gasplant_data()
      if (length(d$months)) {
        updateSelectInput(session, "gp_month", choices = d$months, selected = tail(d$months, 1))
      } else {
        updateSelectInput(session, "gp_month", choices = c("No months"=""), selected = NULL)
      }
    })
    
    gasplant_month_filtered <- reactive({
      d <- gasplant_data()
      gp <- if (is.null(d)) tibble::tibble() else d$gasplants_joined
      if (is.null(gp) || !nrow(gp)) return(tibble::tibble())
      sel <- input$gp_month
      if (!is.null(sel) && nzchar(sel)) gp <- gp[format(gp$month, "%Y-%m") == sel, , drop = FALSE]
      gp
    })
    
    output$gp_map <- renderLeaflet({
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng=-114, lat=54, zoom=5)
    })
    
    observe({
      gp <- gasplant_month_filtered()
      proxy <- leafletProxy("gp_map") %>% clearMarkers() %>% clearControls()
      if (is.null(gp) || !nrow(gp)) return(invisible(NULL))
      
      gp$Latitude  <- suppressWarnings(as.numeric(gp$Latitude))
      gp$Longitude <- suppressWarnings(as.numeric(gp$Longitude))
      gp <- gp[!is.na(gp$Latitude) & !is.na(gp$Longitude), , drop=FALSE]
      if (!nrow(gp)) return(invisible(NULL))
      
      days_in_mo <- lubridate::days_in_month(gp$month)
      daily_capacity  <- ifelse(days_in_mo > 0, gp$monthly_capacity_e3m3 / days_in_mo, NA_real_)
      daily_through   <- ifelse(days_in_mo > 0, gp$monthly_throughput_e3m3 / days_in_mo, NA_real_)
      radius <- scale_capacity_radius(daily_capacity)
      size_px <- ifelse(is.finite(radius), pmax(16, round(radius * 2)), 16)
      
      gp$Operator <- normalize_operator_label(gp$Operator)
      gp$FacilityType_display <- normalize_type_label(gp$FacilityType_display)

      legend_pal <- NULL
      legend_values <- NULL
      legend_title <- NULL
      if (identical(input$gp_color_by, "util")) {
        pal <- util_palette(gp$utilization)
        gp$color_val <- pal(gp$utilization)
        legend_title <- "Utilization"
        legend_pal <- pal
        legend_values <- gp$utilization
      } else {
        op_source <- if ("operator_monthly" %in% names(gp)) gp$operator_monthly else gp$Operator
        if (is.null(op_source)) op_source <- gp$Operator
        op_vals <- as.character(op_source)
        if (!is.null(op_vals)) {
          blank <- is.na(op_vals) | trimws(op_vals) == ""
          if (any(blank)) {
            op_vals[blank] <- gp$Operator[blank]
          }
        }
        op <- normalize_operator_label(op_vals)
        pal <- op_palette(sort(unique(op)))
        gp$color_val <- pal(op)
        legend_title <- "Operator"
        legend_pal <- pal
        legend_values <- op
      }

      marker_colors <- gp$color_val
      marker_colors[is.na(marker_colors) | marker_colors == ""] <- "#2c3e50"
      
      type_shapes <- assign_shape_map(gp$FacilityType_display)
      shape_assignments <- unname(type_shapes$map[as.character(gp$FacilityType_display)])
      shape_assignments[is.na(shape_assignments)] <- "circle"
      icon_urls <- mapply(
        make_shape_svg,
        shape = shape_assignments,
        size = size_px,
        fill = marker_colors,
        SIMPLIFY = FALSE
      )
      icons <- leaflet::icons(
        iconUrl = unlist(icon_urls),
        iconWidth = size_px,
        iconHeight = size_px,
        iconAnchorX = size_px / 2,
        iconAnchorY = size_px / 2
      )
      
      popup <- sprintf(
        "<b>%s</b><br/>Operator: %s<br/>Type: %s<br/>Capacity: %s (10^3 m³/d)<br/>Throughput: %s (10^3 m³/d)<br/>Utilization: %s%%<br/>Month: %s",
        htmltools::htmlEscape(gp$Facility),
        htmltools::htmlEscape(gp$Operator),
        htmltools::htmlEscape(gp$FacilityType_display),
        scales::comma(round(daily_capacity, 1), accuracy = 0.1),
        scales::comma(round(daily_through,  1), accuracy = 0.1),
        scales::comma(pmin(pmax(gp$utilization_pct, 0), 300), accuracy = 0.1),
        htmltools::htmlEscape(format(gp$month, "%Y-%m"))
      )
      
      proxy <- proxy %>% addMarkers(
        lng = gp$Longitude, lat = gp$Latitude,
        icon = icons,
        popup = lapply(popup, htmltools::HTML),
        options = leaflet::markerOptions(riseOnHover = TRUE)
      )
      
      proxy <- proxy %>%
        {
          if (identical(input$gp_color_by, "util") && !is.null(legend_pal)) {
            leaflet::addLegend(., position = "bottomright", pal = legend_pal, values = legend_values,
                               title = legend_title, opacity = 0.9,
                               labFormat = leaflet::labelFormat(digits = 0, suffix = "x"))
          } else if (!is.null(legend_pal)) {
            leaflet::addLegend(., position = "bottomright", pal = legend_pal, values = legend_values,
                               title = legend_title, opacity = 0.9)
          } else {
            .
          }
        }
      
      shape_legend <- build_shape_legend(type_shapes$map)
      if (!is.null(shape_legend)) {
        proxy <- proxy %>% addControl(shape_legend, position = "bottomleft")
      }
    })
    
    output$gp_table <- DT::renderDT({
      gp <- gasplant_month_filtered()
      if (is.null(gp) || !nrow(gp)) return(DT::datatable(data.frame()))
      days_in_mo <- lubridate::days_in_month(gp$month)
      daily_capacity <- ifelse(days_in_mo > 0, gp$monthly_capacity_e3m3 / days_in_mo, NA_real_)
      daily_through  <- ifelse(days_in_mo > 0, gp$monthly_throughput_e3m3 / days_in_mo, NA_real_)
      df <- tibble::tibble(
        Month = format(gp$month, "%Y-%m"),
        FacilityID = gp$facility_id,
        Facility   = gp$Facility,
        Operator   = gp$Operator,
        Type       = gp$FacilityType_display,
        `Throughput (10^3 m3/d)` = round(daily_through, 1),
        `Capacity (10^3 m3/d)`   = round(daily_capacity, 1),
        `Utilization %`          = round(pmin(pmax(gp$utilization_pct, 0), 300), 1)
      )
      DT::datatable(df, rownames=FALSE, options = list(pageLength = 25, scrollX = TRUE))
    })
    
    })
  
  observeEvent(input$reset_filters, {
    showNotification("Resetting all filters...", type = "message", duration=2, id="resetNotify")

    updatePickerInput(session, "operator_filter", selected = character(0))
    updatePickerInput(session, "formation_filter", selected = character(0))
    updatePickerInput(session, "field_filter", selected = character(0))
    updatePickerInput(session, "province_filter", selected = character(0))
    updatePickerInput(session, "company_acreage_filter", selected = character(0))
    updatePickerInput(session, "play_subplay_filter", selected = character(0))
    updatePickerInput(session, "group_operator_filter", selected = character(0))
    updatePickerInput(session, "product_type_filter_analysis", selected = c("OIL", "CND", "GAS", "BOE"))
    updateCheckboxInput(session, "gor_include_cnd", value = TRUE)
    updateSliderInput(session, "gor_range", value = c(0, 50000))

    default_start_date_reset <- max(reactive_vals$min_first_prod_date_overall, reactive_vals$max_first_prod_date_overall - years(10), na.rm = TRUE)
    if (!is.finite(default_start_date_reset)) default_start_date_reset <- Sys.Date() - years(10)
    updateDateRangeInput(session, "well_date_filter",
                         start = default_start_date_reset,
                         end = reactive_vals$max_first_prod_date_overall)

    reactive_vals$wells_to_display <- sf::st_sf(geometry=sf::st_sfc(), crs=4326)
    reactive_vals$wells_filtered_base <- sf::st_sf(geometry=sf::st_sfc(), crs=4326)
    reactive_vals$map_df_with_gor <- sf::st_sf(geometry=sf::st_sfc(), crs=4326)
    reactive_vals$has_map_been_updated_once <- FALSE
    reactive_vals$current_selected_gsl_uwi_std <- NULL
    updateSelectInput(session, "selected_well_for_prod",
                      choices = c("Apply filters and click a well on map or select here" = ""),
                      selected = "")
    showNotification("Filters reset. Apply filters to display wells.", type = "warning", duration=3, id="resetCompleteNotify")
  })
  
  # Database Reconnect Logic
  observeEvent(input$reconnect_db_button, {
    showNotification("Attempting to reconnect to database...", type="message", id="dbReconnectMsg", duration = NULL)
    if (!is.null(con) && dbIsValid(con)) {
      tryCatch({
        dbDisconnect(con)
        message("Disconnected by user for reconnect.")
      }, error = function(e) {
        message(paste("Error during user-initiated disconnect:", e$message))
      })
    }
    con <<- connect_to_db() # Use <<- to assign to global con
    if (!is.null(con) && dbIsValid(con)) {
      removeNotification("dbReconnectMsg")
      showNotification("Successfully reconnected to the database.", type="message", duration = 3)
    } else {
      removeNotification("dbReconnectMsg")
      showNotification("Failed to reconnect to the database. Please check console for details.", type="error", duration = 5)
    }
  })
  
  observeEvent(input$update_map, {
    req(wells_sf)
    showNotification("Applying filters and updating map...", type = "message", id="mapUpdateMsg", duration = NULL)
    if (is.null(wells_sf) || !inherits(wells_sf, "sf")) {
      reactive_vals$wells_to_display <- sf::st_sf(geometry = sf::st_sfc(), crs = 4326)
      reactive_vals$wells_filtered_base <- sf::st_sf(geometry = sf::st_sfc(), crs = 4326)
      reactive_vals$map_df_with_gor <- sf::st_sf(geometry = sf::st_sfc(), crs = 4326)
      reactive_vals$has_map_been_updated_once <- TRUE
      removeNotification("mapUpdateMsg")
      showNotification("Well data source is not available.", type="error", duration=3)
      return()
    }
    df <- wells_sf_global

    # Operator
    if (!is.null(input$operator_filter) && length(input$operator_filter) > 0 && "OperatorName" %in% names(df)) {
      if ("(Unknown)" %in% input$operator_filter) {
        df <- df %>% dplyr::filter(is.na(OperatorName) | OperatorName == "" | OperatorName %in% setdiff(input$operator_filter, "(Unknown)"))
      } else {
        df <- df %>% dplyr::filter(!is.na(OperatorName) & OperatorName %in% input$operator_filter)
      }
    }

    # Formation
    if (!is.null(input$formation_filter) && length(input$formation_filter) > 0 && "Formation" %in% names(df)) {
      df <- df %>% dplyr::filter(!is.na(Formation) & Formation %in% input$formation_filter)
    }

    # Field
    if (!is.null(input$field_filter) && length(input$field_filter) > 0 && "FieldName" %in% names(df)) {
      df <- df %>% dplyr::filter(!is.na(FieldName) & FieldName %in% input$field_filter)
    }

    # Province/State
    if (!is.null(input$province_filter) && length(input$province_filter) > 0 && "ProvinceState" %in% names(df)) {
      df <- df %>% dplyr::filter(!is.na(ProvinceState) & ProvinceState %in% input$province_filter)
    }

    # Date range (FirstProdDate)
    if (!is.null(input$well_date_filter) && length(input$well_date_filter) == 2 && "FirstProdDate" %in% names(df)) {
      df <- df %>% dplyr::filter(!is.na(FirstProdDate) &
                                  FirstProdDate >= as.Date(input$well_date_filter[1]) &
                                  FirstProdDate <= as.Date(input$well_date_filter[2]))
    }

    if (nrow(df) == 0) {
      reactive_vals$wells_to_display <- df
      reactive_vals$wells_filtered_base <- df
      reactive_vals$map_df_with_gor <- df
      reactive_vals$has_map_been_updated_once <- TRUE
      leafletProxy("well_map") %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        clearShapes() %>%
        clearControls()
      removeNotification("mapUpdateMsg")
      showNotification("No values available for this filter under current selections.", type = "warning", duration = 5)
      update_well_selection_choices(df)
      return(invisible(NULL))
    }

    reactive_vals$wells_filtered_base <- df
    df_with_gor <- compute_map_with_gor(df)
    reactive_vals$wells_to_display <- df_with_gor
    reactive_vals$map_df_with_gor <- df_with_gor
    reactive_vals$has_map_been_updated_once <- TRUE

    update_well_selection_choices(df_with_gor)
    removeNotification("mapUpdateMsg")
    showNotification(paste("Map updated with", format(nrow(df_with_gor), big.mark=","), "wells."), type="message", duration=3, id="mapUpdateSuccessNotify")
  })

  observeEvent(list(input$gor_range, input$gor_include_cnd), {
    if (!reactive_vals$has_map_been_updated_once) return()
    base_df <- reactive_vals$wells_filtered_base
    if (is.null(base_df)) return()
    df_with_gor <- compute_map_with_gor(base_df)
    reactive_vals$wells_to_display <- df_with_gor
    reactive_vals$map_df_with_gor <- df_with_gor
    update_well_selection_choices(df_with_gor)
  }, ignoreNULL = FALSE)
  
  output$well_count_display <- renderUI({
    if (!reactive_vals$has_map_been_updated_once && nrow(reactive_vals$wells_to_display) == 0) {
      return(HTML("Apply filters to see well counts."))
    }
    displayed_wells <- reactive_vals$wells_to_display
    total_count <- nrow(displayed_wells)

    if (total_count == 0) {
      return(HTML("0 wells match the current filters."))
    }
    
    confidential_count <- 0
    if ("ConfidentialType" %in% names(displayed_wells) && total_count > 0) {
      confidential_values <- displayed_wells$ConfidentialType
      confidential_count <- sum(toupper(confidential_values) == "CONFIDENTIAL", na.rm = TRUE)
    }
    
    HTML(paste0("Total Wells Displayed: ", format(total_count, big.mark = ","), "<br/>",
                "Confidential Wells: ", format(confidential_count, big.mark = ",")))
  })
  
  output$well_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Simple Map") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = -106, lat = 55, zoom = 4) %>%
      addLayersControl(
        baseGroups = c("Simple Map", "OpenStreetMap", "Satellite"),
        overlayGroups = c("Wells", "Well Sticks", "Play/Subplay Acreage", "Company Acreage"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addScaleBar(position = "bottomleft") %>%
      leaflet.extras::addFullscreenControl()
  })
  
  observe({
    df_map <- reactive_vals$map_df_with_gor
    selected_co_acreage_names <- input$company_acreage_filter
    selected_ps_acreage_names <- input$play_subplay_filter
    
    proxy <- leafletProxy("well_map", data = df_map) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearShapes() %>%
      clearControls() %>%
      addLayersControl(
        baseGroups = c("Simple Map", "OpenStreetMap", "Satellite"),
        overlayGroups = c("Wells", "Well Sticks", "Play/Subplay Acreage", "Company Acreage"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addScaleBar(position = "bottomleft") %>%
      leaflet.extras::addFullscreenControl(position = "topleft")
    
    current_overlay_groups <- c("Wells", "Well Sticks")
    acreage_legend_labels <- character(0)
    acreage_legend_colors <- character(0)
    
    # Acreage Layers (Polygons)
    if (!is.null(selected_ps_acreage_names) && length(selected_ps_acreage_names) > 0) {
      current_overlay_groups <- c(current_overlay_groups, "Play/Subplay Acreage")
      selected_layers_data_ps <- Filter(function(layer) !is.null(layer$name) && layer$name %in% selected_ps_acreage_names, play_subplay_layers_list)
      if (length(selected_layers_data_ps) > 0) {
        ps_colors_map <- setNames(RColorBrewer::brewer.pal(n = max(3, min(length(selected_layers_data_ps), 9)), name = "Set2"),
                                  sapply(selected_layers_data_ps, function(x) x$name))
        
        for (i in seq_along(selected_layers_data_ps)) {
          layer_info <- selected_layers_data_ps[[i]]
          layer_name_escaped <- htmltools::htmlEscape(layer_info$name)
          layer_color <- ps_colors_map[[layer_info$name]]
          
          if (!is.null(layer_info$data) && inherits(layer_info$data, "sf") && nrow(layer_info$data) > 0) {
            layer_attributes_ps <- sf::st_drop_geometry(layer_info$data)
            popups_for_layer_ps <- paste0("<b>Play/Subplay:</b> ", layer_name_escaped)
            if (nrow(layer_attributes_ps) > 0 && ncol(layer_attributes_ps) > 0) {
              attr_to_display_name_ps <- names(layer_attributes_ps)[1]
              if ("SUBPLAY" %in% toupper(names(layer_attributes_ps))) {
                attr_to_display_name_ps <- names(layer_attributes_ps)[toupper(names(layer_attributes_ps)) == "SUBPLAY"][1]
              } else if ("PLAY" %in% toupper(names(layer_attributes_ps))) {
                attr_to_display_name_ps <- names(layer_attributes_ps)[toupper(names(layer_attributes_ps)) == "PLAY"][1]
              } else if ("NAME" %in% toupper(names(layer_attributes_ps))) {
                attr_to_display_name_ps <- names(layer_attributes_ps)[toupper(names(layer_attributes_ps)) == "NAME"][1]
              }
              popups_for_layer_ps <- paste0("<b>Play/Subplay:</b> ", layer_name_escaped, "<br>",
                                            "<b>", htmltools::htmlEscape(attr_to_display_name_ps), ":</b> ", htmltools::htmlEscape(layer_attributes_ps[[attr_to_display_name_ps]]))
            }
            proxy %>% addPolygons(data = layer_info$data, group = "Play/Subplay Acreage",
                                  layerId = paste0("ps_",make.names(layer_info$name), "_feature_", seq_len(nrow(layer_info$data))),
                                  color = layer_color, weight = 1.5, fillColor = layer_color, fillOpacity = 0.15,
                                  popup = lapply(popups_for_layer_ps, htmltools::HTML),
                                  highlightOptions = highlightOptions(weight = 3, color = "white", bringToFront = TRUE, fillOpacity=0.3))
            acreage_legend_labels <- c(acreage_legend_labels, layer_name_escaped)
            acreage_legend_colors <- c(acreage_legend_colors, layer_color)
          }
        }
      }
    }
    if (!is.null(selected_co_acreage_names) && length(selected_co_acreage_names) > 0) {
      current_overlay_groups <- c(current_overlay_groups, "Company Acreage")
      selected_layers_data_co <- Filter(function(layer) !is.null(layer$name) && layer$name %in% selected_co_acreage_names, company_layers_list)
      if (length(selected_layers_data_co) > 0) {
        co_colors_map <- setNames(RColorBrewer::brewer.pal(n = max(3, min(length(selected_layers_data_co), 8)), name = "Pastel2"),
                                  sapply(selected_layers_data_co, function(x) x$name))
        for (i in seq_along(selected_layers_data_co)) {
          layer_info <- selected_layers_data_co[[i]]
          layer_name_escaped <- htmltools::htmlEscape(layer_info$name)
          layer_color <- co_colors_map[[layer_info$name]]
          
          if (!is.null(layer_info$data) && inherits(layer_info$data, "sf") && nrow(layer_info$data) > 0) {
            layer_attributes_co <- sf::st_drop_geometry(layer_info$data)
            popups_for_layer_co <- paste0("<b>Company Acreage:</b> ", layer_name_escaped)
            if (nrow(layer_attributes_co) > 0 && ncol(layer_attributes_co) > 0) {
              attr_to_display_name_co <- names(layer_attributes_co)[1]
              if ("COMPANY" %in% toupper(names(layer_attributes_co))) {
                attr_to_display_name_co <- names(layer_attributes_co)[toupper(names(layer_attributes_co)) == "COMPANY"][1]
              } else if ("OPERATOR" %in% toupper(names(layer_attributes_co))) {
                attr_to_display_name_co <- names(layer_attributes_co)[toupper(names(layer_attributes_co)) == "OPERATOR"][1]
              } else if ("NAME" %in% toupper(names(layer_attributes_co))) {
                attr_to_display_name_co <- names(layer_attributes_co)[toupper(names(layer_attributes_co)) == "NAME"][1]
              }
              popups_for_layer_co <- paste0("<b>Company Acreage:</b> ", layer_name_escaped, "<br>",
                                            "<b>", htmltools::htmlEscape(attr_to_display_name_co), ":</b> ", htmltools::htmlEscape(layer_attributes_co[[attr_to_display_name_co]]))
            }
            proxy %>% addPolygons(data = layer_info$data, group = "Company Acreage",
                                  layerId = paste0("co_",make.names(layer_info$name), "_feature_", seq_len(nrow(layer_info$data))),
                                  color = "black", weight = 1, fillColor = layer_color, fillOpacity = 0.35,
                                  popup = lapply(popups_for_layer_co, htmltools::HTML),
                                  highlightOptions = highlightOptions(weight = 3, color = "white", bringToFront = TRUE, fillOpacity=0.5))
            acreage_legend_labels <- c(acreage_legend_labels, layer_name_escaped)
            acreage_legend_colors <- c(acreage_legend_colors, layer_color)
          }
        }
      }
    }
    
    # Well Markers (Surface Points) and Well Sticks (Polylines)
    if (!is.null(df_map) && inherits(df_map, "sf") && nrow(df_map) > 0) {

      capd <- cap_gor_for_plot(df_map$GOR_Latest)
      gor_for_color <- capd$vals
      gor_for_color[!is.finite(gor_for_color) | gor_for_color < 0] <- NA_real_
      pal_gor <- make_gor_palette(gor_for_color, n = 7)

      finite_mask <- is.finite(gor_for_color) & gor_for_color >= 0
      color_vec <- rep("#9E9E9E", length(gor_for_color))
      if (any(finite_mask)) {
        color_vec[finite_mask] <- pal_gor(gor_for_color[finite_mask])
      }

      df_map$GOR_Color <- color_vec
      df_map$GOR_Capped_ForColor <- gor_for_color

      has_surface_lon <- "SurfaceLongitude" %in% names(df_map)
      has_surface_lat <- "SurfaceLatitude" %in% names(df_map)
      lon_vals <- if (has_surface_lon) suppressWarnings(as.numeric(df_map$SurfaceLongitude)) else rep(NA_real_, nrow(df_map))
      lat_vals <- if (has_surface_lat) suppressWarnings(as.numeric(df_map$SurfaceLatitude)) else rep(NA_real_, nrow(df_map))

      valid_coords <- !is.na(lon_vals) & !is.na(lat_vals) &
        is.finite(lon_vals) & is.finite(lat_vals)

      if (!any(valid_coords)) {
        message("[MAP] No wells with valid surface coordinates after filtering; map markers skipped.")
      }

      df_map_valid <- df_map[valid_coords, , drop = FALSE]
      lon_valid <- lon_vals[valid_coords]
      lat_valid <- lat_vals[valid_coords]

      well_layer_id_col_name <- if (!"GSL_UWI_Std" %in% names(df_map_valid) || !is.character(df_map_valid$GSL_UWI_Std)) {
        df_map_valid$GSL_UWI_Std_for_map <- paste0("wellmarker_", seq_len(nrow(df_map_valid)))
        "GSL_UWI_Std_for_map"
      } else { "GSL_UWI_Std" }

      base_popup <- paste0(
        "<b>UWI:</b> ", htmltools::htmlEscape(df_map_valid$UWI), "<br>",
        "<b>Well Name:</b> ", htmltools::htmlEscape(df_map_valid$WellName), "<br>",
        "<b>Operator:</b> ", htmltools::htmlEscape(df_map_valid$OperatorName), "<br>",
        "<b>Formation:</b> ", htmltools::htmlEscape(df_map_valid$Formation), "<br>",
        "<b>Field:</b> ", htmltools::htmlEscape(df_map_valid$FieldName), "<br>",
        "<b>Status:</b> ", htmltools::htmlEscape(df_map_valid$CurrentStatus), "<br>",
        "<b>First Prod Date:</b> ", htmltools::htmlEscape(as.character(df_map_valid$FirstProdDate))
      )

      confidential_text_vec <- if ("ConfidentialType" %in% names(df_map_valid)) {
        ifelse(!is.na(df_map_valid$ConfidentialType),
               paste0("<br><b>Confidential:</b> ", htmltools::htmlEscape(df_map_valid$ConfidentialType)),
               "")
      } else { rep("", nrow(df_map_valid)) }

      bh_lat_text_vec <- if ("BH_Latitude" %in% names(df_map_valid)) {
        ifelse(!is.na(df_map_valid$BH_Latitude),
               paste0("<br><b>BH Lat:</b> ", round(df_map_valid$BH_Latitude, 5)),
               "")
      } else { rep("", nrow(df_map_valid)) }

      bh_lon_text_vec <- if ("BH_Longitude" %in% names(df_map_valid)) {
        ifelse(!is.na(df_map_valid$BH_Longitude),
               paste0("<br><b>BH Lon:</b> ", round(df_map_valid$BH_Longitude, 5)),
               "")
      } else { rep("", nrow(df_map_valid)) }

      finite_gor_latest <- is.finite(df_map_valid$GOR_Latest) & df_map_valid$GOR_Latest >= 0
      gor_value_text <- ifelse(finite_gor_latest,
                               paste0(scales::comma(round(df_map_valid$GOR_Latest, 1)), " MCF/BBL"),
                               "NA")
      gor_month_text <- ifelse(!is.na(df_map_valid$GOR_Latest_Month),
                               format(df_map_valid$GOR_Latest_Month, "%Y-%m"),
                               "—")
      gas_text <- ifelse(!is.na(df_map_valid$MonthlyGasMCF),
                         scales::comma(round(df_map_valid$MonthlyGasMCF, 0)),
                         "NA")
      oil_text <- ifelse(!is.na(df_map_valid$MonthlyOilBBL),
                         scales::comma(round(df_map_valid$MonthlyOilBBL, 0)),
                         "NA")
      cnd_text <- ifelse(!is.na(df_map_valid$MonthlyCndBBL),
                         scales::comma(round(df_map_valid$MonthlyCndBBL, 0)),
                         "NA")
      liquids_text <- ifelse(!is.na(df_map_valid$MonthlyLiquidsBBL),
                             scales::comma(round(df_map_valid$MonthlyLiquidsBBL, 0)),
                             "NA")
      gas_weighting_text <- ifelse(!is.na(df_map_valid$GasWeightingLatest),
                                   scales::percent(df_map_valid$GasWeightingLatest, accuracy = 0.1),
                                   "NA")
      cnd_line <- if (isTRUE(input$gor_include_cnd)) paste0("<br><b>Monthly Condensate (BBL):</b> ", cnd_text) else ""
      gor_popup <- paste0(
        "<br><b>GOR:</b> ", gor_value_text,
        "<br><b>GOR Month:</b> ", gor_month_text,
        "<br><b>Monthly Gas (MCF):</b> ", gas_text,
        "<br><b>Monthly Oil (BBL):</b> ", oil_text,
        cnd_line,
        "<br><b>Monthly Liquids (BBL):</b> ", liquids_text,
        "<br><b>Gas Weighting:</b> ", gas_weighting_text
      )

      well_popup_content <- paste0(base_popup, confidential_text_vec, bh_lat_text_vec, bh_lon_text_vec, gor_popup)

      if (nrow(df_map_valid) > 0) {
        proxy %>% addCircleMarkers(
          lng = lon_valid,
          lat = lat_valid,
          radius = 6,
          color = df_map_valid$GOR_Color,
          fillColor = df_map_valid$GOR_Color,
          stroke = FALSE,
          fillOpacity = 0.85,
          popup = lapply(well_popup_content, htmltools::HTML),
          layerId = df_map_valid[[well_layer_id_col_name]],
          group = "Wells",
          clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE, showCoverageOnHover = TRUE, zoomToBoundsOnClick = TRUE)
        )
      }

      bh_lon_vals <- if ("BH_Longitude" %in% names(df_map_valid)) suppressWarnings(as.numeric(df_map_valid$BH_Longitude)) else rep(NA_real_, nrow(df_map_valid))
      bh_lat_vals <- if ("BH_Latitude" %in% names(df_map_valid)) suppressWarnings(as.numeric(df_map_valid$BH_Latitude)) else rep(NA_real_, nrow(df_map_valid))
      wells_with_bh_idx <- which(!is.na(bh_lat_vals) & !is.na(bh_lon_vals) & is.finite(bh_lat_vals) & is.finite(bh_lon_vals))

      if (length(wells_with_bh_idx) > 0) {
        for (idx in wells_with_bh_idx) {
          well_stick_data <- df_map_valid[idx, ]
          stick_color <- if (!is.null(well_stick_data$GOR_Color) && !is.na(well_stick_data$GOR_Color)) well_stick_data$GOR_Color else "#9E9E9E"
          proxy %>% addPolylines(
            lng = c(lon_valid[idx], bh_lon_vals[idx]),
            lat = c(lat_valid[idx], bh_lat_vals[idx]),
            layerId = paste0(well_stick_data[[well_layer_id_col_name]], "_stick"),
            color = stick_color,
            weight = 2,
            opacity = 0.7,
            group = "Well Sticks"
          )
        }
      }

      dom <- gor_for_color[finite_mask & valid_coords]
      if (length(dom) > 0) {
        proxy %>% addLegend(
          position = "bottomright",
          pal = pal_gor,
          values = dom,
          title = htmltools::HTML("GOR (MCF/BBL)"),
          opacity = 0.9,
          layerId = "gor_legend"
        )
      } else {
        message("[GOR] No finite domain for legend; skipping legend.")
      }
    }
    # Add Acreage Legend if any acreage layers are selected
    if (length(acreage_legend_labels) > 0) {
      proxy %>% addLegend(
        position = "bottomleft", # Or another position
        colors = acreage_legend_colors,
        labels = acreage_legend_labels,
        title = "Acreage Layers",
        opacity = 0.7,
        layerId = "acreage_legend"
      )
    }
    
    # Update Layers Control with all potentially visible groups
    # This was already being done correctly.
  })
  
  observeEvent(input$well_map_marker_click, {
    event <- input$well_map_marker_click
    if (is.null(event$id)) return()
    
    cleaned_event_id <- sub("_stick$", "", event$id)
    
    if ("GSL_UWI_Std" %in% names(reactive_vals$wells_to_display) &&
        cleaned_event_id %in% reactive_vals$wells_to_display$GSL_UWI_Std) {
      reactive_vals$current_selected_gsl_uwi_std <- cleaned_event_id
      updateSelectInput(session, "selected_well_for_prod", selected = cleaned_event_id)
      showNotification(paste("Selected well ID:", cleaned_event_id, "for production analysis."), type="message", duration=4, id="wellSelectNotify")
    } else {
      message(paste("Map click ID not directly matched to a GSL_UWI_Std:", event$id))
    }
  })
  
  observeEvent(input$selected_well_for_prod, {
    selected_dropdown_uwi <- input$selected_well_for_prod
    if (!is.null(selected_dropdown_uwi) && selected_dropdown_uwi != "" &&
        (is.null(reactive_vals$current_selected_gsl_uwi_std) || selected_dropdown_uwi != reactive_vals$current_selected_gsl_uwi_std) ) {
      reactive_vals$current_selected_gsl_uwi_std <- selected_dropdown_uwi
    } else if (is.null(selected_dropdown_uwi) || selected_dropdown_uwi == "") {
      if (!is.null(reactive_vals$current_selected_gsl_uwi_std)) {
        reactive_vals$current_selected_gsl_uwi_std <- NULL
      }
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # --- Production Data Fetching and Plotting (Single well, Groups) ---
  fetched_production_data <- reactive({
    req(reactive_vals$current_selected_gsl_uwi_std, input$product_type_filter_analysis, cancelOutput = TRUE)
    selected_uwi_std_for_prod <- reactive_vals$current_selected_gsl_uwi_std
    selected_analysis_products <- input$product_type_filter_analysis
    
    if (is.null(selected_uwi_std_for_prod) || selected_uwi_std_for_prod == "") return(data.table())
    if (is.null(selected_analysis_products) || length(selected_analysis_products) == 0) {
      showNotification("Please select at least one product type for analysis.", type = "warning", duration=5)
      return(data.table())
    }
    
    message(paste0("--- fetched_production_data: Fetching for GSL_UWI_Std: '", selected_uwi_std_for_prod, "' ---"))
    if (is.null(con) || !dbIsValid(con)) {
      message("Attempting to (re)connect for on-demand production...")
      con <<- connect_to_db()
      if (is.null(con) || !dbIsValid(con)) { message("ERROR: DB connection failed for on-demand production."); return(data.table()) }
    }
    sql_single_well_prod <- glue::glue_sql(
      "SELECT GSL_UWI, YEAR, PRODUCT_TYPE, ACTIVITY_TYPE, ",
      "JAN_VOLUME, FEB_VOLUME, MAR_VOLUME, APR_VOLUME, ",
      "MAY_VOLUME, JUN_VOLUME, JUL_VOLUME, AUG_VOLUME, ",
      "SEP_VOLUME, OCT_VOLUME, NOV_VOLUME, DEC_VOLUME ",
      "FROM PDEN_VOL_BY_MONTH WHERE GSL_UWI = {selected_uwi_std_for_prod} ",
      "AND ACTIVITY_TYPE = 'PRODUCTION' AND PRODUCT_TYPE IN ('OIL', 'CND', 'GAS') AND ROWNUM <= 240",
      .con = con
    )
    well_prod_raw <- tryCatch({ data.table::as.data.table(dbGetQuery(con, sql_single_well_prod)) },
                              error = function(e) { warning(paste("Error fetching production for GSL_UWI_Std", selected_uwi_std_for_prod, ":", e$message)); return(data.table()) })
    message(paste("Rows returned from DB for GSL_UWI_Std", selected_uwi_std_for_prod, ":", nrow(well_prod_raw)))
    if (nrow(well_prod_raw) == 0) return(data.table())
    
    if("GSL_UWI" %in% names(well_prod_raw)) { well_prod_raw[, GSL_UWI_Std_from_query := standardize_uwi(GSL_UWI)] } else { well_prod_raw[, GSL_UWI_Std_from_query := selected_uwi_std_for_prod] }
    prod_dt_cleaned <- clean_df_colnames(well_prod_raw, paste("PDEN Volumes for", selected_uwi_std_for_prod))
    if ("GSL_UWI_STD_FROM_QUERY" %in% names(prod_dt_cleaned)) { setnames(prod_dt_cleaned, "GSL_UWI_STD_FROM_QUERY", "GSL_UWI_Std")
    } else if ("GSL_UWI" %in% names(prod_dt_cleaned) && !"GSL_UWI_Std" %in% names(prod_dt_cleaned)) { prod_dt_cleaned[, GSL_UWI_Std := standardize_uwi(GSL_UWI)]; prod_dt_cleaned[, GSL_UWI := NULL]
    } else if (!"GSL_UWI_Std" %in% names(prod_dt_cleaned)) { prod_dt_cleaned[, GSL_UWI_Std := selected_uwi_std_for_prod] }
    year_col_actual <- "YEAR"; product_type_col_actual <- "PRODUCT_TYPE"
    
    oil_prod_val <- "OIL"
    cnd_prod_val <- "CND"
    gas_prod_val <- "GAS"
    
    month_abbrs_upper <- toupper(month.abb); monthly_vol_cols <- character(0)
    prod_dt_colnames_upper <- names(prod_dt_cleaned)
    for(m_abbr in month_abbrs_upper){ potential_col_upper <- paste0(m_abbr, "_VOLUME"); if(potential_col_upper %in% prod_dt_colnames_upper) { monthly_vol_cols <- c(monthly_vol_cols, potential_col_upper) } }
    required_id_cols <- c("GSL_UWI_Std", year_col_actual, product_type_col_actual)
    if(!(length(monthly_vol_cols) == 12 && all(required_id_cols %in% names(prod_dt_cleaned)))) { warning("Not all required columns found for melting production."); return(data.table()) }
    for(col_name in monthly_vol_cols) { if (!is.numeric(prod_dt_cleaned[[col_name]])) prod_dt_cleaned[, (col_name) := as.numeric(get(col_name))] }
    if (!is.numeric(prod_dt_cleaned[[year_col_actual]])) prod_dt_cleaned[, (year_col_actual) := as.numeric(get(year_col_actual))]
    if (!is.character(prod_dt_cleaned[[product_type_col_actual]])) prod_dt_cleaned[, (product_type_col_actual) := as.character(get(product_type_col_actual))]
    
    prod_long <- data.table::melt(prod_dt_cleaned, id.vars = required_id_cols, measure.vars = monthly_vol_cols, variable.name = "MONTH_VOLUME_COL", value.name = "Volume", na.rm = FALSE, verbose = FALSE)
    
    # Convert units first
    prod_long[toupper(get(product_type_col_actual)) %in% c(toupper(oil_prod_val), toupper(cnd_prod_val)), Volume_Converted := Volume * M3_TO_BBL]
    prod_long[toupper(get(product_type_col_actual)) == toupper(gas_prod_val), Volume_Converted := Volume * E3M3_TO_MCF]
    prod_long[is.na(Volume_Converted), Volume_Converted := 0]
    
    prod_long <- prod_long[!is.na(Volume_Converted) & Volume_Converted != 0]
    if(nrow(prod_long) == 0) return(data.table())
    
    prod_long[, Month_Num := match(toupper(substr(MONTH_VOLUME_COL, 1, 3)), month_abbrs_upper)]
    prod_long <- prod_long[!is.na(get(year_col_actual)) & !is.na(Month_Num)]
    prod_long[, PROD_DATE := tryCatch(as.Date(paste(get(year_col_actual), Month_Num, 1, sep="-"), format="%Y-%m-%d"), error = function(e) as.Date(NA))]
    prod_long <- prod_long[!is.na(PROD_DATE)]
    
    prod_long[, MonthlyOilTrueBBL_raw := fifelse(toupper(get(product_type_col_actual)) == toupper(oil_prod_val), Volume_Converted, 0)]
    prod_long[, MonthlyCondensateBBL_raw := fifelse(toupper(get(product_type_col_actual)) == toupper(cnd_prod_val), Volume_Converted, 0)]
    prod_long[, MonthlyGasMCF_raw := fifelse(toupper(get(product_type_col_actual)) == toupper(gas_prod_val), Volume_Converted, 0)]
    
    use_oil_for_boe <- "OIL" %in% selected_analysis_products || "BOE" %in% selected_analysis_products
    use_cnd_for_boe <- "CND" %in% selected_analysis_products || "BOE" %in% selected_analysis_products
    use_gas_for_boe <- "GAS" %in% selected_analysis_products || "BOE" %in% selected_analysis_products
    
    aggregated_prod_for_well <- prod_long[, .(
      MonthlyOilTrueBBL = if("OIL" %in% selected_analysis_products) sum(MonthlyOilTrueBBL_raw, na.rm = TRUE) else 0,
      MonthlyCondensateBBL = if("CND" %in% selected_analysis_products) sum(MonthlyCondensateBBL_raw, na.rm = TRUE) else 0,
      MonthlyGasMCF = if("GAS" %in% selected_analysis_products) sum(MonthlyGasMCF_raw, na.rm = TRUE) else 0,
      MonthlyOilForBOE = if(use_oil_for_boe) sum(MonthlyOilTrueBBL_raw, na.rm = TRUE) else 0,
      MonthlyCondensateForBOE = if(use_cnd_for_boe) sum(MonthlyCondensateBBL_raw, na.rm = TRUE) else 0,
      MonthlyGasForBOE = if(use_gas_for_boe) sum(MonthlyGasMCF_raw, na.rm = TRUE) else 0
    ), by = .(GSL_UWI_Std, PROD_DATE)][order(PROD_DATE)]
    
    return(aggregated_prod_for_well)
  })
  
  production_date_range <- reactive({
    prod_data <- fetched_production_data()
    if (nrow(prod_data) > 0 && "PROD_DATE" %in% names(prod_data)) {
      min_d <- min(prod_data$PROD_DATE, na.rm = TRUE)
      max_d <- max(prod_data$PROD_DATE, na.rm = TRUE)
      if(is.finite(min_d) && is.finite(max_d) && min_d <= max_d) { return(c(min_d, max_d)) }
    }
    return(c(as.Date("1900-01-01"), Sys.Date()))
  })
  output$production_date_slider_ui <- renderUI({
    date_rng <- production_date_range()
    sliderInput("production_date_filter", "Filter Production Dates:", min = date_rng[1], max = date_rng[2], value = date_rng, timeFormat = "%b %Y", width = "100%")
  })
  
  processed_production_for_plotting <- reactive({
    prod_data <- fetched_production_data()
    req(input$production_date_filter)
    if (nrow(prod_data) == 0) return(data.table())
    prod_data_filtered <- prod_data[PROD_DATE >= input$production_date_filter[1] & PROD_DATE <= input$production_date_filter[2]]
    if (nrow(prod_data_filtered) == 0) return(data.table())
    prod_data_filtered <- prod_data_filtered[order(PROD_DATE)]
    
    prod_data_filtered[, CumOilTrueBBL := cumsum(MonthlyOilTrueBBL), by = .(GSL_UWI_Std)]
    prod_data_filtered[, CumCondensateBBL := cumsum(MonthlyCondensateBBL), by = .(GSL_UWI_Std)]
    prod_data_filtered[, CumGasMCF := cumsum(MonthlyGasMCF), by = .(GSL_UWI_Std)]
    
    prod_data_filtered[, DaysInMonth := lubridate::days_in_month(PROD_DATE)]
    prod_data_filtered[, OilTrueRateBBLD := MonthlyOilTrueBBL / DaysInMonth]
    prod_data_filtered[, CondensateRateBBLD := MonthlyCondensateBBL / DaysInMonth]
    prod_data_filtered[, GasRateMCFD := MonthlyGasMCF / DaysInMonth]
    
    prod_data_filtered[, BOERateBBLD := (MonthlyOilForBOE + MonthlyCondensateForBOE + (MonthlyGasForBOE / MCF_PER_BOE)) / DaysInMonth]
    return(prod_data_filtered)
  })
  
  output$production_plot <- renderPlotly({
    plot_data <- processed_production_for_plotting()
    if (is.null(plot_data) || nrow(plot_data) == 0 || !"PROD_DATE" %in% names(plot_data)) {
      p <- ggplot() + labs(title = "No well selected or no production data available for selected product(s).", x=NULL, y=NULL) + theme_void()
      return(ggplotly(p))
    }
    plot_title_detail <- ""
    if (!is.null(reactive_vals$current_selected_gsl_uwi_std) && reactive_vals$current_selected_gsl_uwi_std != "" &&
        !is.null(wells_sf) && nrow(wells_sf) > 0) {
      well_details_list <- wells_sf[wells_sf$GSL_UWI_Std == reactive_vals$current_selected_gsl_uwi_std, ]
      if(nrow(well_details_list) > 0) {
        plot_title_detail <- paste0(well_details_list$WellName[1], " (UWI: ", well_details_list$UWI[1], ")")
      } else { plot_title_detail <- paste0("GSL_UWI: ", reactive_vals$current_selected_gsl_uwi_std) }
    }
    plot_main_title <- paste("Daily Production Rate:", plot_title_detail)
    
    measure_vars_plot <- character()
    if ("OIL" %in% input$product_type_filter_analysis && any(plot_data$OilTrueRateBBLD > 0, na.rm = TRUE)) measure_vars_plot <- c(measure_vars_plot, "OilTrueRateBBLD")
    if ("CND" %in% input$product_type_filter_analysis && any(plot_data$CondensateRateBBLD > 0, na.rm = TRUE)) measure_vars_plot <- c(measure_vars_plot, "CondensateRateBBLD")
    if ("GAS" %in% input$product_type_filter_analysis && any(plot_data$GasRateMCFD > 0, na.rm = TRUE)) measure_vars_plot <- c(measure_vars_plot, "GasRateMCFD")
    if ("BOE" %in% input$product_type_filter_analysis && any(plot_data$BOERateBBLD > 0, na.rm = TRUE)) {
      measure_vars_plot <- c(measure_vars_plot, "BOERateBBLD")
    }
    measure_vars_plot <- unique(measure_vars_plot)
    
    if(length(measure_vars_plot) == 0){
      p <- ggplot() + labs(title = paste(plot_main_title, "\n(No production rates to display for selected product(s))"), x="Date", y="Rate") + theme_minimal()
      return(ggplotly(p))
    }
    
    plot_data_long <- data.table::melt(plot_data, id.vars = c("GSL_UWI_Std", "PROD_DATE"),
                                       measure.vars = measure_vars_plot,
                                       variable.name = "ProductRateType", value.name = "Rate")
    plot_data_long_filtered <- plot_data_long[Rate > 0 & is.finite(Rate)]
    
    if(nrow(plot_data_long_filtered) == 0){
      p <- ggplot() + labs(title = paste(plot_main_title, "\n(No positive production rates to display for selected product(s))"), x="Date", y="Rate") + theme_minimal()
      return(ggplotly(p))
    }
    
    plot_data_long_filtered[, hover_text := paste0(
      "Date: ", format(PROD_DATE, "%b %Y"), "<br>",
      gsub("OilTrueRateBBLD", "Oil Rate (BBL/day)",
           gsub("CondensateRateBBLD", "Cond. Rate (BBL/day)",
                gsub("GasRateMCFD", "Gas Rate (MCF/day)",
                     gsub("BOERateBBLD", "BOE Rate (BBL/day)", ProductRateType)))),
      ": ", round(Rate, 2)
    )]
    
    product_colors_map <- setNames(custom_palette[1:4],
                                   c("OilTrueRateBBLD", "CondensateRateBBLD", "GasRateMCFD", "BOERateBBLD"))
    
    present_product_rate_types <- unique(plot_data_long_filtered$ProductRateType)
    active_colors <- product_colors_map[names(product_colors_map) %in% present_product_rate_types]
    
    all_rate_labels <- c("OilTrueRateBBLD" = "Oil (BBL/day)",
                         "CondensateRateBBLD" = "Cond. (BBL/day)",
                         "GasRateMCFD" = "Gas (MCF/day)",
                         "BOERateBBLD" = "BOE (BBL/day)")
    active_labels <- all_rate_labels[names(all_rate_labels) %in% present_product_rate_types]
    
    if (is.factor(plot_data_long_filtered$ProductRateType)) {
      plot_data_long_filtered[, ProductRateType := factor(ProductRateType, levels = present_product_rate_types)]
    } else {
      plot_data_long_filtered[, ProductRateType := factor(ProductRateType, levels = present_product_rate_types)]
    }
    
    p <- ggplot(plot_data_long_filtered, aes(x = PROD_DATE, y = Rate, color = ProductRateType, group = ProductRateType, text = hover_text)) +
      geom_line(linewidth = 1) + geom_point(size = 1.5) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
      labs(title = plot_main_title, x = "Date", y = "Daily Rate", color = "Product Type") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
      scale_color_manual(values = active_colors, labels = active_labels, name = "Product Type")
    ggplotly(p, tooltip = "text")
  })
  
  dt_rowCallback_js <- JS(
    "function(row, data, displayNum, displayIndex, dataIndex) {",
    "  $('td', row).each(function() {",
    "    $(this).attr('title', $(this).text());",
    "  });",
    "}"
  )
  
  output$production_table <- DT::renderDataTable({
    table_data_for_well <- processed_production_for_plotting()
    if (is.null(table_data_for_well) || nrow(table_data_for_well) == 0) {
      current_uwi <- reactive_vals$current_selected_gsl_uwi_std
      msg <- if (!is.null(current_uwi) && current_uwi != "") paste("No production data for UWI:", current_uwi, "in selected date range or for selected product(s).")
      else "No well selected or no production data for selected product(s)."
      return(DT::datatable(data.frame(Message = msg), options = list(searching = FALSE, paging = FALSE, info=FALSE), rownames=FALSE))
    }
    cols_to_select <- c("PROD_DATE", "MonthlyOilTrueBBL", "MonthlyCondensateBBL", "MonthlyGasMCF",
                        "OilTrueRateBBLD", "CondensateRateBBLD", "GasRateMCFD", "BOERateBBLD",
                        "CumOilTrueBBL", "CumCondensateBBL", "CumGasMCF")
    actual_cols_present <- intersect(cols_to_select, names(table_data_for_well))
    if(length(actual_cols_present) == 0) {
      return(DT::datatable(data.frame(Message = "Required production columns not found."), options = list(searching = FALSE, paging = FALSE, info=FALSE), rownames=FALSE))
    }
    display_data <- table_data_for_well[, ..actual_cols_present]
    setnames(display_data, old = "PROD_DATE", new = "Prod. Month", skip_absent = TRUE)
    setnames(display_data, old = "MonthlyOilTrueBBL", new = "Oil (BBL/Month)", skip_absent = TRUE)
    setnames(display_data, old = "MonthlyCondensateBBL", new = "Cond. (BBL/Month)", skip_absent = TRUE)
    setnames(display_data, old = "MonthlyGasMCF", new = "Gas (MCF/Month)", skip_absent = TRUE)
    setnames(display_data, old = "OilTrueRateBBLD", new = "Oil Rate (BBL/day)", skip_absent = TRUE)
    setnames(display_data, old = "CondensateRateBBLD", new = "Cond. Rate (BBL/day)", skip_absent = TRUE)
    setnames(display_data, old = "GasRateMCFD", new = "Gas Rate (MCF/day)", skip_absent = TRUE)
    setnames(display_data, old = "BOERateBBLD", new = "BOE Rate (BBL/day)", skip_absent = TRUE)
    setnames(display_data, old = "CumOilTrueBBL", new = "Cum. Oil (BBL)", skip_absent = TRUE)
    setnames(display_data, old = "CumCondensateBBL", new = "Cum. Cond. (BBL)", skip_absent = TRUE)
    setnames(display_data, old = "CumGasMCF", new = "Cum. Gas (MCF)", skip_absent = TRUE)
    if ("Prod. Month" %in% names(display_data)) {
      if(inherits(display_data[['Prod. Month']], "Date")){ # Check if it's a Date
        display_data[, `Prod. Month` := format(as.Date(`Prod. Month`), "%Y-%m")]
      } else {
        # Attempt to convert if character
        tryCatch({
          display_data[, `Prod. Month` := format(as.Date(as.character(`Prod. Month`)), "%Y-%m")]
        }, error = function(e) {
          message(paste("Could not format Prod. Month for DT in production_table:", e$message))
        })
      }
    }
    rate_cols_to_format <- c("Oil Rate (BBL/day)", "Cond. Rate (BBL/day)", "Gas Rate (MCF/day)", "BOE Rate (BBL/day)")
    for(rc in rate_cols_to_format){
      if(rc %in% names(display_data)) display_data[[rc]] <- round(display_data[[rc]], 2)
    }
    DT::datatable(
      display_data,
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        order = list(0, 'desc'),
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-right', targets = which(sapply(display_data, is.numeric))-1 )),
        rowCallback = dt_rowCallback_js
      ),
      rownames = FALSE,
      caption = htmltools::tags$caption(style = "caption-side: top; text-align: center; font-weight:bold;", paste("Monthly, Daily Rate, & Cumulative Production Data for GSL_UWI:", reactive_vals$current_selected_gsl_uwi_std))
    )
  })
  
  output$download_prod_data <- downloadHandler(
    filename = function() {
      paste0("production_data_", reactive_vals$current_selected_gsl_uwi_std, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data_to_download <- processed_production_for_plotting()
      if (nrow(data_to_download) > 0) {
        setnames(data_to_download, old = "PROD_DATE", new = "Prod_Month", skip_absent = TRUE)
        setnames(data_to_download, old = "MonthlyOilTrueBBL", new = "Monthly_Oil_BBL", skip_absent = TRUE)
        setnames(data_to_download, old = "MonthlyCondensateBBL", new = "Monthly_Condensate_BBL", skip_absent = TRUE)
        setnames(data_to_download, old = "MonthlyGasMCF", new = "Monthly_Gas_MCF", skip_absent = TRUE)
        setnames(data_to_download, old = "OilTrueRateBBLD", new = "Daily_Oil_Rate_BBLD", skip_absent = TRUE)
        setnames(data_to_download, old = "CondensateRateBBLD", new = "Daily_Condensate_Rate_BBLD", skip_absent = TRUE)
        setnames(data_to_download, old = "GasRateMCFD", new = "Daily_Gas_Rate_MCFD", skip_absent = TRUE)
        setnames(data_to_download, old = "BOERateBBLD", new = "Daily_BOE_Rate_BBLD", skip_absent = TRUE)
        setnames(data_to_download, old = "CumOilTrueBBL", new = "Cumulative_Oil_BBL", skip_absent = TRUE)
        setnames(data_to_download, old = "CumCondensateBBL", new = "Cumulative_Condensate_BBL", skip_absent = TRUE)
        setnames(data_to_download, old = "CumGasMCF", new = "Cumulative_Gas_MCF", skip_absent = TRUE)
        
        cols_for_download <- c("Prod_Month", "Monthly_Oil_BBL", "Monthly_Condensate_BBL", "Monthly_Gas_MCF",
                               "Daily_Oil_Rate_BBLD", "Daily_Condensate_Rate_BBLD", "Daily_Gas_Rate_MCFD", "Daily_BOE_Rate_BBLD",
                               "Cumulative_Oil_BBL", "Cumulative_Condensate_BBL", "Cumulative_Gas_MCF")
        data_to_download_final <- data_to_download[, .SD, .SDcols = intersect(cols_for_download, names(data_to_download))]
        fwrite(data_to_download_final, file)
      } else {
        fwrite(data.table(Message = "No production data available for selected well and date range."), file)
      }
    }
  )
  
  # --- Operator Group Cumulative (Separate from main filters) ---
  operator_group_prod_data <- eventReactive(input$update_group_plot, {
    req(wells_sf_global, input$group_operator_filter, input$group_prod_date_range, input$product_type_filter_analysis)
    selected_operators <- input$group_operator_filter
    selected_operators <- selected_operators[selected_operators != "" & selected_operators != "Loading..." & selected_operators != "No Operators Found" & selected_operators != "No Well Data"]
    if (is.null(selected_operators) || length(selected_operators) == 0) { showNotification("Please select at least one operator for operator group analysis.", type = "warning"); return(NULL) }
    
    selected_analysis_products <- input$product_type_filter_analysis
    if (is.null(selected_analysis_products) || length(selected_analysis_products) == 0) {
      showNotification("Please select at least one product type for analysis.", type = "warning")
      return(NULL)
    }
    
    showNotification("Fetching and processing operator group production data...", type = "message", duration = NULL, id="groupProdMsgOp")
    date_start <- input$group_prod_date_range[1]; date_end <- input$group_prod_date_range[2]
    target_uwis_dt <- as.data.table(wells_sf_global)[OperatorName %in% selected_operators, .(GSL_UWI_Std, OperatorName)]; target_uwis <- unique(target_uwis_dt$GSL_UWI_Std)
    if (length(target_uwis) == 0) { removeNotification("groupProdMsgOp"); showNotification("No wells found for the selected operator(s) in operator group analysis.", type = "warning"); return(NULL) }
    if (is.null(con) || !dbIsValid(con)) { con <<- connect_to_db(); if (is.null(con) || !dbIsValid(con)) { removeNotification("groupProdMsgOp"); return(NULL) } }
    uwi_batches <- split(target_uwis, ceiling(seq_along(target_uwis)/300)); all_prod_data_list <- list()
    for(batch_num in seq_along(uwi_batches)){
      current_batch_uwis <- uwi_batches[[batch_num]]
      sql_group_prod <- glue::glue_sql( "SELECT GSL_UWI, YEAR, PRODUCT_TYPE, ACTIVITY_TYPE, JAN_VOLUME, FEB_VOLUME, MAR_VOLUME, APR_VOLUME, MAY_VOLUME, JUN_VOLUME, JUL_VOLUME, AUG_VOLUME, SEP_VOLUME, OCT_VOLUME, NOV_VOLUME, DEC_VOLUME FROM PDEN_VOL_BY_MONTH WHERE GSL_UWI IN ({uwis*}) AND ACTIVITY_TYPE = 'PRODUCTION' AND PRODUCT_TYPE IN ('OIL', 'CND', 'GAS')", uwis = current_batch_uwis, .con = con)
      batch_prod_raw <- tryCatch({ data.table::as.data.table(dbGetQuery(con, sql_group_prod)) }, error = function(e) { data.table() }); if(nrow(batch_prod_raw) > 0) all_prod_data_list[[length(all_prod_data_list) + 1]] <- batch_prod_raw
    }
    if(length(all_prod_data_list) == 0){ removeNotification("groupProdMsgOp"); showNotification("No production data found for any wells in the selected operator group(s).", type = "warning"); return(NULL) }
    group_prod_raw <- rbindlist(all_prod_data_list, use.names = TRUE, fill = TRUE)
    
    # Filter by selected product types for analysis BEFORE cleaning and merging
    if (!is.null(selected_analysis_products) && length(selected_analysis_products) > 0 && !"BOE" %in% selected_analysis_products) { # If BOE is selected, we need all for BOE calc
      group_prod_raw <- group_prod_raw[toupper(PRODUCT_TYPE) %in% toupper(selected_analysis_products)]
    }
    if(nrow(group_prod_raw) == 0) {
      removeNotification("groupProdMsgOp");
      showNotification("No production data for the selected product type(s) in the operator group.", type = "warning");
      return(NULL)
    }
    
    if("GSL_UWI" %in% names(group_prod_raw)) { group_prod_raw[, GSL_UWI_Std_from_query := standardize_uwi(GSL_UWI)] }
    group_prod_cleaned <- clean_df_colnames(group_prod_raw, "Operator Group PDEN Volumes")
    if ("GSL_UWI_STD_FROM_QUERY" %in% names(group_prod_cleaned)) { setnames(group_prod_cleaned, "GSL_UWI_STD_FROM_QUERY", "GSL_UWI_Std") } else if ("GSL_UWI" %in% names(group_prod_cleaned) && !"GSL_UWI_Std" %in% names(group_prod_cleaned)) { group_prod_cleaned[, GSL_UWI_Std := standardize_uwi(GSL_UWI)]; group_prod_cleaned[, GSL_UWI := NULL] }
    if(!"GSL_UWI_Std" %in% names(target_uwis_dt)) { if("GSL_UWI" %in% names(target_uwis_dt)) { target_uwis_dt[, GSL_UWI_Std := standardize_uwi(GSL_UWI)] } else { removeNotification("groupProdMsgOp"); return(NULL) } }
    group_prod_cleaned <- merge(group_prod_cleaned, unique(target_uwis_dt[,.(GSL_UWI_Std, OperatorName)]), by="GSL_UWI_Std", all.x=TRUE); group_prod_cleaned <- group_prod_cleaned[!is.na(OperatorName)]
    if(nrow(group_prod_cleaned) == 0) { removeNotification("groupProdMsgOp"); return(NULL) }
    year_col_actual <- "YEAR"; product_type_col_actual <- "PRODUCT_TYPE"; oil_prod_val <- "OIL"; cnd_prod_val <- "CND"; gas_prod_val <- "GAS"
    month_abbrs_upper <- toupper(month.abb); monthly_vol_cols <- character(0); prod_dt_colnames_upper <- names(group_prod_cleaned)
    for(m_abbr in month_abbrs_upper){ potential_col_upper <- paste0(m_abbr, "_VOLUME"); if(potential_col_upper %in% prod_dt_colnames_upper) { monthly_vol_cols <- c(monthly_vol_cols, potential_col_upper) } }
    required_id_cols_group <- c("GSL_UWI_Std", "OperatorName", year_col_actual, product_type_col_actual)
    if(!(length(monthly_vol_cols) == 12 && all(required_id_cols_group %in% names(group_prod_cleaned)))) { removeNotification("groupProdMsgOp"); return(NULL) }
    for(col_name in monthly_vol_cols) { if (!is.numeric(group_prod_cleaned[[col_name]])) group_prod_cleaned[, (col_name) := as.numeric(get(col_name))] }
    if(!is.numeric(group_prod_cleaned[[year_col_actual]])) group_prod_cleaned[, (year_col_actual) := as.numeric(get(year_col_actual))]
    if(!is.character(group_prod_cleaned[[product_type_col_actual]])) group_prod_cleaned[, (product_type_col_actual) := as.character(get(product_type_col_actual))]
    group_prod_long <- data.table::melt(group_prod_cleaned, id.vars = required_id_cols_group, measure.vars = monthly_vol_cols, variable.name = "MONTH_VOLUME_COL", value.name = "Volume", na.rm = FALSE, verbose = FALSE)
    
    # Unit conversion based on the *original* PRODUCT_TYPE before aggregation
    group_prod_long[toupper(get(product_type_col_actual)) %in% c(toupper(oil_prod_val), toupper(cnd_prod_val)), Volume_Converted := Volume * M3_TO_BBL]
    group_prod_long[toupper(get(product_type_col_actual)) == toupper(gas_prod_val), Volume_Converted := Volume * E3M3_TO_MCF]
    group_prod_long[is.na(Volume_Converted), Volume_Converted := 0]
    
    group_prod_long <- group_prod_long[!is.na(Volume_Converted) & Volume_Converted != 0]
    if(nrow(group_prod_long) == 0) { removeNotification("groupProdMsgOp"); return(NULL) }
    
    group_prod_long[, Month_Num := match(toupper(substr(MONTH_VOLUME_COL, 1, 3)), month_abbrs_upper)]
    group_prod_long <- group_prod_long[!is.na(get(year_col_actual)) & !is.na(Month_Num)]
    group_prod_long[, PROD_DATE := tryCatch(as.Date(paste(get(year_col_actual), Month_Num, 1, sep="-"), format="%Y-%m-%d"), error = function(e) as.Date(NA))]
    group_prod_long <- group_prod_long[!is.na(PROD_DATE)]; group_prod_long <- group_prod_long[PROD_DATE >= date_start & PROD_DATE <= date_end]
    if(nrow(group_prod_long) == 0) { removeNotification("groupProdMsgOp"); return(NULL) }
    
    # Create specific product columns based on original type, then sum based on filter
    group_prod_long[, MonthlyOilTrueBBL_raw := fifelse(toupper(get(product_type_col_actual)) == toupper(oil_prod_val), Volume_Converted, 0)]
    group_prod_long[, MonthlyCondensateBBL_raw := fifelse(toupper(get(product_type_col_actual)) == toupper(cnd_prod_val), Volume_Converted, 0)]
    group_prod_long[, MonthlyGasMCF_raw := fifelse(toupper(get(product_type_col_actual)) == toupper(gas_prod_val), Volume_Converted, 0)]
    
    agg_by_operator_month <- group_prod_long[, .(
      MonthlyOilBBL = if("OIL" %in% selected_analysis_products || "BOE" %in% selected_analysis_products) sum(MonthlyOilTrueBBL_raw, na.rm = TRUE) else 0,
      MonthlyCondensateBBL = if("CND" %in% selected_analysis_products || "BOE" %in% selected_analysis_products) sum(MonthlyCondensateBBL_raw, na.rm = TRUE) else 0,
      MonthlyGasMCF = if("GAS" %in% selected_analysis_products || "BOE" %in% selected_analysis_products) sum(MonthlyGasMCF_raw, na.rm = TRUE) else 0
    ), by = .(OperatorName, PROD_DATE)][order(OperatorName, PROD_DATE)]
    
    # Ensure all selected products are actually present for BOE calculation
    oil_for_boe <- if("OIL" %in% selected_analysis_products || "BOE" %in% selected_analysis_products) agg_by_operator_month$MonthlyOilBBL else 0
    cnd_for_boe <- if("CND" %in% selected_analysis_products || "BOE" %in% selected_analysis_products) agg_by_operator_month$MonthlyCondensateBBL else 0
    gas_for_boe <- if("GAS" %in% selected_analysis_products || "BOE" %in% selected_analysis_products) agg_by_operator_month$MonthlyGasMCF else 0
    
    agg_by_operator_month[, DaysInMonth := lubridate::days_in_month(PROD_DATE)]
    agg_by_operator_month[, AvgOilRateBBLD := MonthlyOilBBL / DaysInMonth] # This is now sum of selected OIL + CND
    agg_by_operator_month[, AvgGasRateMCFD := MonthlyGasMCF / DaysInMonth]
    agg_by_operator_month[, AvgBOERateBBLD := (oil_for_boe + cnd_for_boe + (gas_for_boe / MCF_PER_BOE)) / DaysInMonth]
    
    agg_by_operator_month[, CumOilBBL := cumsum(MonthlyOilBBL), by = .(OperatorName)] # This is cum of selected OIL+CND
    agg_by_operator_month[, CumGasMCF := cumsum(MonthlyGasMCF), by = .(OperatorName)]
    removeNotification("groupProdMsgOp"); showNotification("Operator group production data processed.", type="message"); return(agg_by_operator_month)
  })
  output$grouped_cumulative_plot <- renderPlot({
    plot_data <- operator_group_prod_data()
    req(plot_data); if(nrow(plot_data) == 0) { return(ggplot() + labs(title = "No data to display for selected operator group(s) and date range.", x=NULL, y=NULL) + theme_void()) }
    
    plot_data_long <- data.table::melt(plot_data, id.vars = c("OperatorName", "PROD_DATE"),
                                       measure.vars = c("AvgOilRateBBLD", "AvgGasRateMCFD", "AvgBOERateBBLD"),
                                       variable.name = "ProductRateType", value.name = "AverageDailyRate")
    plot_data_long_filtered <- plot_data_long[AverageDailyRate > 0 & is.finite(AverageDailyRate)]
    if(nrow(plot_data_long_filtered) == 0){ return(ggplot() + labs(title = "No average daily rates to display for operator group.", x="Date", y="Average Daily Rate") + theme_minimal()) }
    
    # Determine number of unique operators for color palette
    unique_ops_plot <- unique(plot_data_long_filtered$OperatorName)
    operator_colors <- custom_palette[1:min(length(unique_ops_plot), length(custom_palette))]
    if (length(unique_ops_plot) > length(custom_palette)) { # Recycle if more ops than colors
      operator_colors <- rep(custom_palette, length.out = length(unique_ops_plot))
    }
    names(operator_colors) <- unique_ops_plot
    
    
    rate_labels_group <- c("AvgOilRateBBLD" = "Avg Oil/Cond (BBL/day)", "AvgGasRateMCFD" = "Avg Gas (MCF/day)", "AvgBOERateBBLD" = "Avg BOE (BBL/day)")
    
    ggplot(plot_data_long_filtered, aes(x = PROD_DATE, y = AverageDailyRate, color = OperatorName, linetype = ProductRateType, group = interaction(OperatorName, ProductRateType))) +
      geom_line(linewidth = 1.2) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
      labs(title = "Operator Group Average Daily Production Rate", x = "Date", y = "Average Daily Rate", color = "Operator", linetype = "Product Type") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
      scale_color_manual(values = operator_colors) + # Apply custom colors
      scale_linetype_manual(values = c("AvgOilRateBBLD" = "solid", "AvgGasRateMCFD" = "dashed", "AvgBOERateBBLD" = "dotted"),
                            labels = rate_labels_group) +
      guides(color = guide_legend(override.aes = list(linetype = "solid")))
  })
  output$grouped_production_table <- DT::renderDataTable({
    table_data <- operator_group_prod_data()
    if (is.null(table_data) || nrow(table_data) == 0) { return(DT::datatable(data.frame(Message = "No operator group production data to display."), options = list(searching = FALSE, paging = FALSE, info=FALSE), rownames=FALSE)) }
    display_data <- copy(table_data)
    setnames(display_data, old=c("PROD_DATE", "OperatorName", "MonthlyOilBBL", "MonthlyGasMCF", "AvgOilRateBBLD", "AvgGasRateMCFD", "AvgBOERateBBLD", "CumOilBBL", "CumGasMCF"),
             new=c("Prod. Month", "Operator", "Monthly Oil/Cond (BBL)", "Monthly Gas (MCF)", "Avg Oil/Cond Rate (BBL/day)", "Avg Gas Rate (MCF/day)", "Avg BOE Rate (BBL/day)", "Cum. Oil/Cond (BBL)", "Cum. Gas (MCF)"), skip_absent = TRUE)
    if ("Prod. Month" %in% names(display_data) && inherits(display_data[['Prod. Month']], "Date")) {
      display_data[, `Prod. Month` := format(as.Date(`Prod. Month`), "%Y-%m")]
    }
    rate_cols_to_format_group <- c("Avg Oil/Cond Rate (BBL/day)", "Avg Gas Rate (MCF/day)", "Avg BOE Rate (BBL/day)")
    for(rcg in rate_cols_to_format_group){ if(rcg %in% names(display_data)) display_data[[rcg]] <- round(display_data[[rcg]], 2) }
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        order = list(list(1, 'asc'), list(0, 'desc')),
        rowCallback = dt_rowCallback_js
      ),
      rownames = FALSE,
      caption = "Operator Group Monthly, Avg Daily Rate, and Cumulative Production"
    )
  })
  output$download_group_prod_data <- downloadHandler(
    filename = function() { paste0("operator_group_production_summary_", Sys.Date(), ".csv") },
    content = function(file) {
      data_to_download <- operator_group_prod_data()
      if (!is.null(data_to_download) && nrow(data_to_download) > 0) {
        setnames(data_to_download, old=c("PROD_DATE", "OperatorName", "MonthlyOilBBL", "MonthlyGasMCF", "AvgOilRateBBLD", "AvgGasRateMCFD", "AvgBOERateBBLD", "CumOilBBL", "CumGasMCF"),
                 new=c("Prod_Month", "Operator", "Monthly_Oil_Cond_BBL", "Monthly_Gas_MCF", "Avg_Daily_Oil_Cond_Rate_BBLD", "Avg_Daily_Gas_Rate_MCFD", "Avg_Daily_BOE_Rate_BBLD", "Cumulative_Oil_Cond_BBL", "Cumulative_Gas_MCF"), skip_absent = TRUE)
        fwrite(data_to_download, file)
      } else { fwrite(data.table(Message = "No operator group production data available for download."), file) }
    }
  )
  
  gor_data_filtered <- reactive({
    req(input$well_date_filter)
    wells_current <- reactive_vals$wells_to_display
    if (is.null(wells_current) || nrow(wells_current) == 0) return(data.table::data.table())

    wells_dt <- data.table::as.data.table(sf::st_drop_geometry(wells_current))
    if (!"GSL_UWI_Std" %in% names(wells_dt)) return(data.table::data.table())

    uwis <- unique(stats::na.omit(wells_dt$GSL_UWI_Std))
    if (!length(uwis)) return(data.table::data.table())

    date_vals <- input$well_date_filter
    date_start <- if (length(date_vals) >= 1) date_vals[1] else Sys.Date() - years(5)
    date_end <- if (length(date_vals) >= 2) date_vals[2] else Sys.Date()
    ts_dt <- compute_gor_timeseries_for_wells(uwis, date_start, date_end, use_cnd = use_cnd_reactive())
    if (nrow(ts_dt) == 0) return(ts_dt)

    data.table::setorder(ts_dt, GSL_UWI_STD, PROD_DATE)

    meta_cols <- intersect(
      c("GSL_UWI_Std", "WellName", "UWI", "OperatorName", "Formation", "FieldName", "ProvinceState", "FirstProdDate"),
      names(wells_dt)
    )
    wells_meta <- unique(wells_dt[, ..meta_cols])
    if ("FirstProdDate" %in% names(wells_meta) && !inherits(wells_meta$FirstProdDate, "Date")) {
      wells_meta[, FirstProdDate := as.Date(FirstProdDate)]
    }
    char_cols <- setdiff(names(wells_meta), c("GSL_UWI_Std", "FirstProdDate"))
    if (length(char_cols)) {
      wells_meta[, (char_cols) := lapply(.SD, function(x) as.character(x)), .SDcols = char_cols]
    }

    ts_dt <- merge(ts_dt, wells_meta, by.x = "GSL_UWI_STD", by.y = "GSL_UWI_Std", all.x = TRUE)

    ts_dt[, TotalProd := GasMCF + LiquidsBBL]
    ts_dt[, FirstProdMonthSeries := {
      idx <- which(TotalProd > 0)
      if (length(idx)) PROD_DATE[idx[1]] else as.Date(NA)
    }, by = GSL_UWI_STD]
    ts_dt[, MonthOnProduction := {
      idx <- which(TotalProd > 0)
      out <- rep(NA_integer_, .N)
      if (length(idx)) {
        start <- idx[1]
        out[start:.N] <- seq_len(.N - start + 1)
      }
      out
    }, by = GSL_UWI_STD]
    ts_dt[, YearOnProduction := ifelse(
      is.na(FirstProdMonthSeries),
      NA_integer_,
      as.integer(floor(as.numeric(difftime(PROD_DATE, FirstProdMonthSeries, units = "days")) / 365.25)) + 1
    )]

    if ("FirstProdDate" %in% names(ts_dt)) {
      ts_dt[, VintageYear := ifelse(!is.na(FirstProdDate), lubridate::year(FirstProdDate), lubridate::year(FirstProdMonthSeries))]
    } else {
      ts_dt[, VintageYear := lubridate::year(FirstProdMonthSeries)]
    }

    ts_dt[, `:=`(TotalProd = NULL, FirstProdMonthSeries = NULL)]
    ts_dt
  })

  output$gor_trend_by_month_plot <- renderPlot({
    ds <- data.table::copy(gor_data_filtered())
    req(nrow(ds) > 0)
    req(all(c("PROD_DATE", "GOR_MCF_PER_BBL", "MonthOnProduction") %in% names(ds)))

    ds <- ds[is.finite(GOR_MCF_PER_BBL) & GOR_MCF_PER_BBL >= 0]
    req(nrow(ds) > 0)

    capd <- cap_gor_for_plot(ds$GOR_MCF_PER_BBL)
    ds[, GOR_for_plot := capd$vals]

    plot_dt <- ds[!is.na(MonthOnProduction) & MonthOnProduction >= 1]
    req(nrow(plot_dt) > 0)

    if ("Formation" %in% names(plot_dt) && any(!is.na(plot_dt$Formation) & trimws(plot_dt$Formation) != "")) {
      plot_dt[, Group := ifelse(is.na(Formation) | trimws(Formation) == "", "(Unknown)", Formation)]
      color_label <- "Formation"
    } else if ("OperatorName" %in% names(plot_dt) && any(!is.na(plot_dt$OperatorName) & trimws(plot_dt$OperatorName) != "")) {
      plot_dt[, Group := ifelse(is.na(OperatorName) | trimws(OperatorName) == "", "(Unknown)", OperatorName)]
      color_label <- "Operator"
    } else {
      plot_dt[, Group := "All Wells"]
      color_label <- "Group"
    }

    agg <- plot_dt[, .(
      MedianGOR = if (all(is.na(GOR_for_plot))) NA_real_ else stats::median(GOR_for_plot, na.rm = TRUE)
    ), by = .(Group, MonthOnProduction)]
    agg <- agg[is.finite(MedianGOR)]
    req(nrow(agg) > 0)

    unique_groups <- unique(agg$Group)
    group_colors <- custom_palette[1:min(length(unique_groups), length(custom_palette))]
    if (length(unique_groups) > length(custom_palette)) {
      group_colors <- rep(custom_palette, length.out = length(unique_groups))
    }
    names(group_colors) <- unique_groups

    ggplot(agg, aes(x = MonthOnProduction, y = MedianGOR, color = Group, group = Group)) +
      geom_line(linewidth = 1.1) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = group_colors) +
      labs(
        title = "Median GOR by month on production",
        subtitle = "Values capped at p99 for visualization.",
        x = "Month on Production",
        y = "Median GOR (MCF/BBL)",
        color = color_label
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })

  output$gas_weighting_by_vintage_plot <- renderPlot({
    ds <- data.table::copy(gor_data_filtered())
    req(nrow(ds) > 0)
    req("GasWeighting" %in% names(ds))

    dsw <- ds[is.finite(GasWeighting) & GasWeighting >= 0 & GasWeighting <= 1]
    req(nrow(dsw) > 0)

    plot_dt <- dsw[!is.na(VintageYear) & !is.na(YearOnProduction) & YearOnProduction >= 1]
    req(nrow(plot_dt) > 0)

    plot_dt[, VintageYear := as.character(VintageYear)]
    agg <- plot_dt[, .(
      AvgGasWeighting = if (all(is.na(GasWeighting))) NA_real_ else mean(GasWeighting, na.rm = TRUE)
    ), by = .(VintageYear, YearOnProduction)]
    agg <- agg[is.finite(AvgGasWeighting)]
    req(nrow(agg) > 0)

    agg[, VintageYear := factor(VintageYear, levels = sort(unique(VintageYear)))]
    unique_vintages <- levels(agg$VintageYear)
    vintage_colors <- custom_palette[1:min(length(unique_vintages), length(custom_palette))]
    if (length(unique_vintages) > length(custom_palette)) {
      vintage_colors <- rep(custom_palette, length.out = length(unique_vintages))
    }
    names(vintage_colors) <- unique_vintages

    ggplot(agg, aes(x = YearOnProduction, y = AvgGasWeighting, color = VintageYear, group = VintageYear)) +
      geom_line(linewidth = 1.1) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), oob = scales::squish) +
      scale_color_manual(values = vintage_colors) +
      labs(
        title = "Average gas weighting by vintage",
        subtitle = "Only finite gas weighting values included.",
        x = "Year on Production",
        y = "Average Gas Weighting",
        color = "Vintage Year"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })

  output$gor_timeseries_table <- DT::renderDataTable({
    ds <- data.table::copy(gor_data_filtered())
    req(nrow(ds) > 0)

    display <- data.table::copy(ds)
    display[, ProdMonth := format(PROD_DATE, "%Y-%m")]
    display[, WellDisplay := ifelse(!is.na(WellName) & trimws(WellName) != "", WellName, GSL_UWI_STD)]
    display[, `GOR (MCF/BBL)` := ifelse(
      is.finite(GOR_MCF_PER_BBL) & GOR_MCF_PER_BBL >= 0,
      scales::comma(GOR_MCF_PER_BBL, accuracy = 0.1),
      "NA"
    )]
    display[, `Gas weighting (%)` := ifelse(
      is.na(GasWeighting),
      "NA",
      scales::percent(GasWeighting, accuracy = 0.1)
    )]
    display[, MonthOnProduction := as.integer(MonthOnProduction)]
    display[, YearOnProduction := as.integer(YearOnProduction)]
    display[, VintageYear := ifelse(is.na(VintageYear), "", as.character(VintageYear))]

    table_dt <- display[, .(
      Well = WellDisplay,
      `GSL UWI` = GSL_UWI_STD,
      Month = ProdMonth,
      OilBBL = round(OilBBL, 1),
      CndBBL = round(CndBBL, 1),
      GasMCF = round(GasMCF, 1),
      LiquidsBBL = round(LiquidsBBL, 1),
      `GOR (MCF/BBL)`,
      `Gas weighting (%)`,
      MonthOnProduction,
      YearOnProduction,
      VintageYear
    )]

    DT::datatable(
      table_dt,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$download_gor_timeseries_csv <- downloadHandler(
    filename = function() paste0("gor_timeseries_", Sys.Date(), ".csv"),
    content = function(file) {
      gor_dt <- gor_data_filtered()
      if (is.null(gor_dt) || nrow(gor_dt) == 0) {
        data.table::fwrite(data.table::data.table(Message = "No GOR data for current filters."), file)
        return()
      }

      export_dt <- data.table::copy(gor_dt)
      export_dt[, Well := ifelse(!is.na(WellName) & trimws(WellName) != "", WellName, GSL_UWI_STD)]
      export_dt[, Month := format(PROD_DATE, "%Y-%m-%d")]
      export_dt[, MonthOnProduction := as.integer(MonthOnProduction)]
      export_dt[, YearOnProduction := as.integer(YearOnProduction)]
      export_dt[, VintageYear := as.character(VintageYear)]

      cols <- intersect(
        c("Well", "GSL_UWI_STD", "OperatorName", "Formation", "FieldName", "ProvinceState", "Month", "OilBBL", "CndBBL", "GasMCF", "LiquidsBBL", "GOR_MCF_PER_BBL", "GasWeighting", "MonthOnProduction", "YearOnProduction", "VintageYear"),
        names(export_dt)
      )
      data.table::fwrite(export_dt[, ..cols], file)
    }
  )

  observeEvent(input$duc_apply, {
    req(wells_sf_global)
    req(nrow(wells_sf_global) > 0)

    # get user inputs safely
    snap_dates <- sort(unique(as.Date(input$duc_dates)))
    req(length(snap_dates) > 0)

    grp_col <- input$duc_group_by
    if (is.null(grp_col) || !(grp_col %in% c("OperatorName","Formation","FieldName","ProvinceState"))) {
      grp_col <- "OperatorName"
    }

    min_hold_days <- as.numeric(input$duc_min_hold_days %||% 30)
    max_hold_days  <- as.numeric(input$duc_max_hold_days %||% 730)
    recency_months <- as.numeric(input$duc_spud_recency_months %||% 36)
    max_months_cap <- as.numeric(input$duc_max_months_cap %||% 24)
    exclude_conf  <- isTRUE(input$duc_exclude_conf)

    base_sf <- reactive_vals$wells_filtered_base
    if (is.null(base_sf) || !inherits(base_sf, "sf") || nrow(base_sf) == 0) {
      base_sf <- wells_sf_global
    }
    req(inherits(base_sf, "sf"), nrow(base_sf) > 0)

    wx_raw <- data.table::as.data.table(sf::st_drop_geometry(base_sf))
    if ("GSL_UWI_STD" %in% names(wx_raw) && !"GSL_UWI_Std" %in% names(wx_raw)) data.table::setnames(wx_raw, "GSL_UWI_STD", "GSL_UWI_Std")
    if ("GSL_UWI" %in% names(wx_raw) && !"GSL_UWI_Std" %in% names(wx_raw)) wx_raw[, GSL_UWI_Std := standardize_uwi(GSL_UWI)]
    if (!"GSL_UWI_Std" %in% names(wx_raw)) wx_raw[, GSL_UWI_Std := NA_character_]
    if (!"UWI" %in% names(wx_raw)) wx_raw[, UWI := GSL_UWI_Std]
    if (!"ConfidentialType" %in% names(wx_raw)) wx_raw[, ConfidentialType := NA_character_]

    wx <- wx_raw[
      , .(
          UWI               = UWI %||% NA_character_,
          GSL_UWI_Std       = GSL_UWI_Std %||% NA_character_,
          OperatorName      = OperatorName %||% NA_character_,
          Formation         = Formation %||% NA_character_,
          FieldName         = FieldName %||% NA_character_,
          ProvinceState     = ProvinceState %||% NA_character_,
          SpudDate          = as.Date(SpudDate),
          FirstProdDate     = as.Date(FirstProdDate),
          AbandonmentDate   = as.Date(AbandonmentDate),
          ConfidentialType  = ConfidentialType %||% NA_character_
        )
    ]

    detail_list <- lapply(
      snap_dates,
      function(sd) duc_detail_for_date(
        wx_dt           = wx,
        snap_date       = sd,
        min_hold_days   = min_hold_days,
        max_hold_days   = max_hold_days,
        recency_months  = recency_months,
        max_months_cap  = max_months_cap,
        exclude_conf    = exclude_conf,
        group_col       = grp_col
      )
    )
    duc_detail_dt <- data.table::rbindlist(detail_list, use.names = TRUE, fill = TRUE)

    reactive_vals$duc_detail <- duc_detail_dt

    if (nrow(duc_detail_dt)) {
      duc_comp_dt <- duc_detail_dt[, .(DUC_Count = .N), by = .(Group, SnapshotDate)][order(SnapshotDate, -DUC_Count)]
    } else {
      duc_comp_dt <- data.table::data.table(Group = character(0), DUC_Count = integer(0), SnapshotDate = as.Date(character(0)))
    }

    reactive_vals$duc_comp <- duc_comp_dt
    reactive_vals$duc_groups_available <- if (nrow(duc_comp_dt)) sort(unique(duc_comp_dt$Group)) else character(0)
  })

  output$duc_group_filter_ui <- renderUI({
    groups <- reactive_vals$duc_groups_available
    req(!is.null(groups), length(groups) > 0)
    selectizeInput(
      "duc_group_filter",
      label = paste0("Filter ", ifelse(input$duc_group_by == "ProvinceState", "provinces", "groups"), " to display"),
      choices = groups,
      multiple = TRUE,
      selected = groups[1:min(10, length(groups))]
    )
  })

  output$duc_headline <- renderText({
    dt <- reactive_vals$duc_comp
    if (is.null(dt) || !nrow(dt)) {
      return("No DUC results yet. Pick snapshot dates and click Calculate.")
    }
    snaps <- sort(unique(dt$SnapshotDate))
    total_by_snap <- dt[, .(TotalDUCs = sum(DUC_Count, na.rm = TRUE)), by = SnapshotDate]
    paste0(
      "DUC counts for ", length(snaps), " snapshot(s). ",
      paste0(
        format(total_by_snap$SnapshotDate, "%Y-%m-%d"), ": ",
        total_by_snap$TotalDUCs, " wells",
        collapse = " | "
      )
    )
  })

  output$duc_bar_compare <- plotly::renderPlotly({
    dt <- reactive_vals$duc_comp
    req(!is.null(dt), nrow(dt) > 0)

    if (!is.null(input$duc_group_filter) && length(input$duc_group_filter) > 0) {
      dt <- dt[Group %in% input$duc_group_filter]
    }
    req(nrow(dt) > 0)

    if (identical(input$duc_group_by, "ProvinceState")) {
      plot_dt <- dt
    } else {
      topN <- 20
      top_groups <- dt[, .(TotalAllSnaps = sum(DUC_Count, na.rm = TRUE)), by = Group][
        order(-TotalAllSnaps)
      ][1:min(.N, topN)]$Group
      plot_dt <- dt[Group %in% top_groups]
    }
    req(nrow(plot_dt) > 0)

    p <- ggplot2::ggplot(
      plot_dt,
      ggplot2::aes(
        x = Group,
        y = DUC_Count,
        fill = as.factor(SnapshotDate)
      )
    ) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = NULL,
        y = "DUC count",
        fill = "Snapshot"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    plotly::ggplotly(p)
  })

  output$duc_table <- DT::renderDT({
    dt <- reactive_vals$duc_comp
    req(!is.null(dt), nrow(dt) > 0)
    if (!is.null(input$duc_group_filter) && length(input$duc_group_filter) > 0) {
      dt <- dt[Group %in% input$duc_group_filter]
    }
    req(nrow(dt) > 0)
    DT::datatable(
      dt[order(SnapshotDate, -DUC_Count)],
      rownames = FALSE,
      options = list(pageLength = 25, scrollX = TRUE)
    )
  })

  output$duc_download <- downloadHandler(
    filename = function() paste0("duc_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      sum_dt <- reactive_vals$duc_comp
      det_dt <- reactive_vals$duc_detail
      if (is.null(sum_dt) || !nrow(sum_dt)) {
        data.table::fwrite(sum_dt, file)
        return()
      }
      if (!is.null(input$duc_group_filter) && length(input$duc_group_filter) > 0) {
        sum_dt <- sum_dt[Group %in% input$duc_group_filter]
        if (!is.null(det_dt) && nrow(det_dt) > 0) {
          det_dt <- det_dt[Group %in% input$duc_group_filter]
        }
      }
      if (!is.null(det_dt) && nrow(det_dt) > 0) {
        uwis <- det_dt[, .(UWI_List = paste(sort(unique(UWI)), collapse = "|")), by = .(Group, SnapshotDate)]
        out <- merge(sum_dt, uwis, by = c("Group", "SnapshotDate"), all.x = TRUE)
      } else {
        out <- sum_dt
      }
      data.table::fwrite(out[order(SnapshotDate, -DUC_Count)], file)
    }
  )

  output$duc_detail_table <- DT::renderDT({
    dt <- reactive_vals$duc_detail
    req(!is.null(dt), nrow(dt) > 0)
    if (!is.null(input$duc_group_filter) && length(input$duc_group_filter) > 0) {
      dt <- dt[Group %in% input$duc_group_filter]
    }
    req(nrow(dt) > 0)
    dt <- dt[order(SnapshotDate, Group, OperatorName, ProvinceState, Formation, UWI)]
    DT::datatable(
      dt,
      rownames = FALSE,
      options = list(pageLength = 25, scrollX = TRUE)
    )
  })

  output$download_duc_details_csv <- downloadHandler(
    filename = function() paste0("duc_detail_", Sys.Date(), ".csv"),
    content = function(file) {
      dt <- reactive_vals$duc_detail
      if (is.null(dt) || !nrow(dt)) {
        data.table::fwrite(data.table::data.table(), file)
        return()
      }
      if (!is.null(input$duc_group_filter) && length(input$duc_group_filter) > 0) {
        dt <- dt[Group %in% input$duc_group_filter]
      }
      data.table::fwrite(dt[order(SnapshotDate, Group, OperatorName, ProvinceState, Formation, UWI)], file)
    }
  )

  observeEvent(input$calculate_shutin, {
    req(input$shutin_snapshot_date)
    req(input$shutin_no_prod_months)
    req(input$shutin_recent_window_months)

    snapshot_date <- as.Date(input$shutin_snapshot_date)
    no_prod_months <- as.integer(input$shutin_no_prod_months)
    recent_window_mo <- as.integer(input$shutin_recent_window_months)

    reactive_vals$shutin_snapshot <- snapshot_date
    reactive_vals$shutin_no_prod_months <- no_prod_months
    reactive_vals$shutin_recent_window_months <- recent_window_mo

    base_sf <- reactive_vals$wells_filtered_base
    if (is.null(base_sf) || nrow(base_sf) == 0) {
      reactive_vals$shutin_summary <- data.table::data.table()
      reactive_vals$shutin_detail <- data.table::data.table()
      showNotification("No wells available under current filters for shut-in analysis.", type = "warning", duration = 5)
      return(invisible(NULL))
    }

    wells_base <- data.table::as.data.table(sf::st_drop_geometry(base_sf))
    if (!nrow(wells_base)) {
      reactive_vals$shutin_summary <- data.table::data.table()
      reactive_vals$shutin_detail <- data.table::data.table()
      showNotification("Filtered wells data is empty; cannot compute shut-in results.", type = "warning", duration = 5)
      return(invisible(NULL))
    }

    needed_cols <- c("GSL_UWI", "UWI", "OperatorName", "ProvinceState",
                     "SpudDate", "FirstProdDate", "AbandonmentDate", "CurrentStatus")
    for (nc in needed_cols) {
      if (!nc %in% names(wells_base)) wells_base[, (nc) := NA]
    }

    date_cols <- intersect(c("SpudDate", "FirstProdDate", "AbandonmentDate"), names(wells_base))
    for (dc in date_cols) {
      wells_base[, (dc) := as.Date(get(dc))]
    }

    if (!"GSL_UWI" %in% names(wells_base)) {
      wells_base[, GSL_UWI := NA_character_]
    }
    wells_base[, GSL_UWI := trimws(as.character(GSL_UWI))]
    valid_ids <- unique(wells_base$GSL_UWI)
    valid_ids <- valid_ids[!is.na(valid_ids) & valid_ids != ""]

    if (!length(valid_ids)) {
      reactive_vals$shutin_summary <- data.table::data.table()
      reactive_vals$shutin_detail <- data.table::data.table()
      showNotification("No GSL_UWI identifiers available for shut-in analysis.", type = "warning", duration = 5)
      return(invisible(NULL))
    }

    build_month_window <- function(snap_date, n_months) {
      if (is.na(snap_date) || n_months <= 0) return(as.Date(character()))
      snap_month_start <- lubridate::floor_date(snap_date, unit = "month")
      last_full_month_start <- lubridate::floor_date(snap_month_start - lubridate::days(1), unit = "month")
      rev(sapply(seq_len(n_months) - 1, function(i) {
        lubridate::floor_date(last_full_month_start - lubridate::days(30 * i), unit = "month")
      }))
    }

    silent_window_months <- build_month_window(snapshot_date, no_prod_months)
    recent_window_months <- build_month_window(snapshot_date, recent_window_mo)
    all_needed_months <- sort(unique(c(silent_window_months, recent_window_months)))

    if (!length(all_needed_months)) {
      reactive_vals$shutin_summary <- data.table::data.table()
      reactive_vals$shutin_detail <- data.table::data.table()
      showNotification("Unable to derive production windows for shut-in analysis.", type = "warning", duration = 5)
      return(invisible(NULL))
    }

    if (is.null(con) || !DBI::dbIsValid(con)) {
      con <<- connect_to_db()
    }

    if (is.null(con) || !DBI::dbIsValid(con)) {
      reactive_vals$shutin_summary <- data.table::data.table()
      reactive_vals$shutin_detail <- data.table::data.table()
      showNotification("Database connection is unavailable for shut-in analysis.", type = "error", duration = 5)
      return(invisible(NULL))
    }

    prod_sql <- glue::glue_sql(
      "SELECT
        p.GSL_UWI,
        p.PROD_MONTH,
        p.OIL_BBL,
        p.COND_BBL,
        p.GAS_MCF
      FROM PDEN_MONTHLY p
      WHERE p.PROD_MONTH IN ({all_needed_months*})
        AND p.GSL_UWI IN ({uwis*})",
      all_needed_months = all_needed_months,
      uwis = valid_ids,
      .con = con
    )

    prod_raw <- tryCatch(
      DBI::dbGetQuery(con, prod_sql),
      error = function(e) {
        message("ERROR pulling production: ", e$message)
        data.frame()
      }
    )

    prod_dt <- data.table::as.data.table(prod_raw)

    if (nrow(prod_dt) > 0) {
      if (!inherits(prod_dt$PROD_MONTH, "Date")) {
        prod_dt[, PROD_MONTH := as.Date(PROD_MONTH)]
      }
      prod_dt[, GSL_UWI := trimws(as.character(GSL_UWI))]
      prod_dt <- prod_dt[GSL_UWI %in% valid_ids]

      vol_cols <- c("OIL_BBL", "COND_BBL", "GAS_MCF")
      for (vc in vol_cols) {
        if (!vc %in% names(prod_dt)) prod_dt[, (vc) := 0]
        prod_dt[is.na(get(vc)), (vc) := 0]
      }

      prod_dt[, TOTAL_VOL_BOE := (OIL_BBL + COND_BBL) + (GAS_MCF / 6.0)]
    } else {
      prod_dt <- data.table::data.table(
        GSL_UWI = character(),
        PROD_MONTH = as.Date(character()),
        TOTAL_VOL_BOE = numeric()
      )
    }

    sum_over_window <- function(month_vec, wells_scope) {
      if (!length(wells_scope)) {
        return(data.table::data.table(GSL_UWI = character(), WINDOW_VOL_BOE = numeric()))
      }
      if (!length(month_vec)) {
        return(data.table::data.table(GSL_UWI = wells_scope, WINDOW_VOL_BOE = rep(0, length(wells_scope))))
      }
      combo <- data.table::CJ(GSL_UWI = wells_scope, PROD_MONTH = month_vec, unique = TRUE)
      if (nrow(prod_dt)) {
        combo <- merge(
          combo,
          prod_dt[, .(GSL_UWI, PROD_MONTH, TOTAL_VOL_BOE)],
          by = c("GSL_UWI", "PROD_MONTH"),
          all.x = TRUE,
          sort = FALSE
        )
      } else {
        combo[, TOTAL_VOL_BOE := 0]
      }
      combo[is.na(TOTAL_VOL_BOE), TOTAL_VOL_BOE := 0]
      combo[, .(WINDOW_VOL_BOE = sum(TOTAL_VOL_BOE, na.rm = TRUE)), by = GSL_UWI]
    }

    silent_sum_dt <- sum_over_window(silent_window_months, valid_ids)
    data.table::setnames(silent_sum_dt, "WINDOW_VOL_BOE", "SILENT_VOL_BOE")

    recent_sum_dt <- sum_over_window(recent_window_months, valid_ids)
    data.table::setnames(recent_sum_dt, "WINDOW_VOL_BOE", "RECENT_VOL_BOE")

    window_stats <- merge(
      recent_sum_dt,
      silent_sum_dt,
      by = "GSL_UWI",
      all = TRUE
    )
    window_stats[is.na(RECENT_VOL_BOE), RECENT_VOL_BOE := 0]
    window_stats[is.na(SILENT_VOL_BOE), SILENT_VOL_BOE := 0]

    last_prod_by_well <- data.table::data.table(GSL_UWI = character(), LAST_PROD_MONTH = as.Date(character()))
    if (nrow(prod_dt)) {
      last_prod_by_well <- prod_dt[TOTAL_VOL_BOE > 0,
        .(LAST_PROD_MONTH = max(PROD_MONTH, na.rm = TRUE)),
        by = GSL_UWI
      ]
    }

    shutin_candidates <- merge(
      wells_base,
      window_stats,
      by = "GSL_UWI",
      all.x = TRUE,
      sort = FALSE
    )
    shutin_candidates <- merge(
      shutin_candidates,
      last_prod_by_well,
      by = "GSL_UWI",
      all.x = TRUE,
      sort = FALSE
    )

    shutin_candidates[is.na(RECENT_VOL_BOE), RECENT_VOL_BOE := 0]
    shutin_candidates[is.na(SILENT_VOL_BOE), SILENT_VOL_BOE := 0]
    shutin_candidates[, LAST_PROD_MONTH := as.Date(LAST_PROD_MONTH)]
    shutin_candidates[, MonthsSinceLastProd := ifelse(
      is.na(LAST_PROD_MONTH),
      NA_real_,
      as.numeric(difftime(snapshot_date, LAST_PROD_MONTH, units = "days")) / 30.4375
    )]

    shutin_flagged <- shutin_candidates[
      (RECENT_VOL_BOE > 0) &
      (SILENT_VOL_BOE == 0) &
      (is.na(AbandonmentDate) | as.Date(AbandonmentDate) > snapshot_date) &
      !is.na(FirstProdDate) & as.Date(FirstProdDate) <= snapshot_date
    ]

    if (!nrow(shutin_flagged)) {
      reactive_vals$shutin_summary <- data.table::data.table()
      reactive_vals$shutin_detail <- data.table::data.table()
      showNotification("No shut-in wells match the current criteria.", type = "message", duration = 5)
      return(invisible(NULL))
    }

    shutin_flagged[, OperatorName := ifelse(is.na(OperatorName) | OperatorName == "", "(Unknown)", as.character(OperatorName))]

    shutin_summary <- shutin_flagged[
      , .(SHUTIN_WELL_COUNT = .N), by = .(OperatorName)
    ][order(-SHUTIN_WELL_COUNT, OperatorName)]

    shutin_detail <- shutin_flagged[, .(
      SnapshotDate = snapshot_date,
      UWI,
      GSL_UWI,
      OperatorName,
      ProvinceState,
      SpudDate = as.Date(SpudDate),
      FirstProdDate = as.Date(FirstProdDate),
      LAST_PROD_MONTH,
      MonthsSinceLastProd = round(MonthsSinceLastProd, 1),
      RECENT_VOL_BOE,
      SILENT_VOL_BOE,
      AbandonmentDate = as.Date(AbandonmentDate),
      CurrentStatus,
      ShutIn = TRUE
    )]

    reactive_vals$shutin_summary <- shutin_summary
    reactive_vals$shutin_detail <- shutin_detail

    showNotification(
      paste0(
        "Identified ",
        format(nrow(shutin_detail), big.mark = ","),
        " shut-in wells as of ",
        snapshot_date,
        "."
      ),
      type = "message",
      duration = 4
    )
  })
  output$shutin_plot <- renderPlot({
    summary_dt <- reactive_vals$shutin_summary
    snapshot_date <- reactive_vals$shutin_snapshot
    no_prod_months <- reactive_vals$shutin_no_prod_months
    recent_window_mo <- reactive_vals$shutin_recent_window_months
    req(!is.null(summary_dt), !is.na(snapshot_date), !is.na(no_prod_months), !is.na(recent_window_mo))
    validate(need(nrow(summary_dt) > 0, "No shut-in wells match the current criteria."))

    ggplot(summary_dt,
           aes(x = reorder(OperatorName, SHUTIN_WELL_COUNT), y = SHUTIN_WELL_COUNT)) +
      geom_col(fill = "#4a90e2") +
      coord_flip() +
      labs(
        x = "Operator",
        y = "Shut-in well count",
        title = paste0(
          "Shut-in wells as of ",
          format(snapshot_date, "%Y-%m-%d"),
          " (recent window = ",
          recent_window_mo,
          " mo; zero production window = ",
          no_prod_months,
          " mo)"
        )
      ) +
      theme_minimal(base_size = 12)
  })

  output$shutin_table <- DT::renderDT({
    detail_dt <- reactive_vals$shutin_detail
    req(!is.null(detail_dt))
    validate(need(nrow(detail_dt) > 0, "No shut-in wells match the current criteria."))

    detail_dt[order(SnapshotDate, OperatorName, ProvinceState, GSL_UWI)]
  },
  options = list(pageLength = 25, scrollX = TRUE),
  rownames = FALSE)

  output$shutin_summary_download <- downloadHandler(
    filename = function() paste0("shutin_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      dt <- reactive_vals$shutin_summary
      if (is.null(dt) || !nrow(dt)) {
        data.table::fwrite(data.table::data.table(), file)
      } else {
        data.table::fwrite(dt, file)
      }
    }
  )

  output$shutin_detail_download <- downloadHandler(
    filename = function() paste0("shutin_detail_", Sys.Date(), ".csv"),
    content = function(file) {
      dt <- reactive_vals$shutin_detail
      if (is.null(dt) || !nrow(dt)) {
        data.table::fwrite(data.table::data.table(), file)
      } else {
        data.table::fwrite(dt[order(SnapshotDate, OperatorName, ProvinceState, GSL_UWI)], file)
      }
    }
  )


  filtered_group_cumulative_data <- eventReactive(input$calculate_filtered_cumulative, {
    req(wells_sf_global, input$product_type_filter_analysis, input$filtered_group_breakout_by)
    
    selected_analysis_products <- input$product_type_filter_analysis
    if (is.null(selected_analysis_products) || length(selected_analysis_products) == 0) {
      showNotification("Please select at least one product type for analysis (under 'Production Analysis' main tab).", type = "warning", duration=5)
      return(NULL)
    }
    
    showNotification("Calculating normalized rate for filtered wells by operator...", type = "message", duration = NULL, id="filtRateMsg")
    filtered_wells_sf_for_calc <- reactive_vals$wells_to_display
    if (is.null(filtered_wells_sf_for_calc) || nrow(filtered_wells_sf_for_calc) == 0) { removeNotification("filtRateMsg"); showNotification("No wells selected by current map filters.", type = "warning"); return(NULL) }
    
    if (!"LateralLength" %in% names(filtered_wells_sf_for_calc)) {
      removeNotification("filtRateMsg");
      showNotification("LateralLength column not found in well data. Cannot normalize.", type = "error", duration=5);
      return(NULL)
    }
    breakout_cols_needed <- c("GSL_UWI_Std", "OperatorName", "LateralLength", "Formation", "FieldName", "ProvinceState", "FirstProdDate")
    breakout_cols_needed_present <- breakout_cols_needed[breakout_cols_needed %in% names(filtered_wells_sf_for_calc)]
    
    
    target_uwis_with_breakout_cols <- unique(as.data.table(sf::st_drop_geometry(filtered_wells_sf_for_calc))[, ..breakout_cols_needed_present])
    
    if ("FirstProdDate" %in% names(target_uwis_with_breakout_cols) && inherits(target_uwis_with_breakout_cols$FirstProdDate, "Date")) {
      target_uwis_with_breakout_cols[, FirstProdYear := as.character(year(FirstProdDate))]
    } else {
      target_uwis_with_breakout_cols[, FirstProdYear := NA_character_]
    }
    
    
    target_uwis <- target_uwis_with_breakout_cols$GSL_UWI_Std
    target_uwis <- target_uwis[!is.na(target_uwis) & target_uwis != ""]
    
    if (length(target_uwis) == 0) { removeNotification("filtRateMsg"); showNotification("No valid GSL_UWI_Std found for filtered wells.", type = "warning"); return(NULL) }
    if (is.null(con) || !dbIsValid(con)) { con <<- connect_to_db(); if (is.null(con) || !dbIsValid(con)) { removeNotification("filtRateMsg"); return(NULL) } }
    
    uwi_batches <- split(target_uwis, ceiling(seq_along(target_uwis)/300)); all_prod_data_list <- list()
    for(batch_num in seq_along(uwi_batches)){
      current_batch_uwis <- uwi_batches[[batch_num]]
      sql_prod <- glue::glue_sql( "SELECT GSL_UWI, YEAR, PRODUCT_TYPE, ACTIVITY_TYPE, JAN_VOLUME, FEB_VOLUME, MAR_VOLUME, APR_VOLUME, MAY_VOLUME, JUN_VOLUME, JUL_VOLUME, AUG_VOLUME, SEP_VOLUME, OCT_VOLUME, NOV_VOLUME, DEC_VOLUME FROM PDEN_VOL_BY_MONTH WHERE GSL_UWI IN ({uwis*}) AND ACTIVITY_TYPE = 'PRODUCTION' AND PRODUCT_TYPE IN ('OIL', 'CND', 'GAS')", uwis = current_batch_uwis, .con = con)
      batch_prod_raw <- tryCatch({ data.table::as.data.table(dbGetQuery(con, sql_prod)) }, error = function(e) { data.table()}); if(nrow(batch_prod_raw) > 0) all_prod_data_list[[length(all_prod_data_list) + 1]] <- batch_prod_raw
    }
    if(length(all_prod_data_list) == 0){ removeNotification("filtRateMsg"); showNotification("No production data found for filtered wells.", type = "warning"); return(NULL) }
    full_prod_raw <- rbindlist(all_prod_data_list, use.names = TRUE, fill = TRUE)
    
    # Filter by selected product types for analysis
    if (!("BOE" %in% selected_analysis_products) && !is.null(selected_analysis_products) && length(selected_analysis_products) > 0) {
      full_prod_raw <- full_prod_raw[toupper(PRODUCT_TYPE) %in% toupper(selected_analysis_products)]
    }
    if(nrow(full_prod_raw) == 0) {
      removeNotification("filtRateMsg");
      showNotification("No production data for the selected product type(s) in the filtered group.", type = "warning");
      return(NULL)
    }
    
    
    if("GSL_UWI" %in% names(full_prod_raw)) { full_prod_raw[, GSL_UWI_Std_query := standardize_uwi(GSL_UWI)] }
    prod_cleaned <- clean_df_colnames(full_prod_raw, "Filtered Group PDEN Volumes")
    
    if ("GSL_UWI_STD_QUERY" %in% names(prod_cleaned)) {
      setnames(prod_cleaned, "GSL_UWI_STD_QUERY", "GSL_UWI_Std")
    } else if ("GSL_UWI" %in% names(prod_cleaned) && !"GSL_UWI_Std" %in% names(prod_cleaned)) {
      prod_cleaned[, GSL_UWI_Std := standardize_uwi(GSL_UWI)]
    } else if (!"GSL_UWI_Std" %in% names(prod_cleaned)) {
      removeNotification("filtRateMsg"); return(NULL)
    }
    
    prod_cleaned <- merge(prod_cleaned, target_uwis_with_breakout_cols, by = "GSL_UWI_Std", all.x = TRUE, allow.cartesian=TRUE)
    
    breakout_col_name <- input$filtered_group_breakout_by
    if (!breakout_col_name %in% names(prod_cleaned)) {
      removeNotification("filtRateMsg"); showNotification(paste("Breakout column '", breakout_col_name, "' not found after merging production data."), type = "error"); return(NULL)
    }
    prod_cleaned <- prod_cleaned[!is.na(get(breakout_col_name))]
    
    if(nrow(prod_cleaned) == 0) { removeNotification("filtRateMsg"); showNotification(paste("No production data for valid groups based on:", breakout_col_name), type = "warning"); return(NULL) }
    
    year_col <- "YEAR"; product_col <- "PRODUCT_TYPE"
    oil_only_val <- "OIL"; cnd_only_val <- "CND"; gas_val <- "GAS"
    month_cols <- paste0(toupper(month.abb), "_VOLUME");
    
    # OperatorName must be present for calendar cumulative plot, even if not the breakout
    cols_to_check_base <- c(year_col, product_col, breakout_col_name, "LateralLength", "GSL_UWI_Std", "OperatorName")
    if(!all(cols_to_check_base %in% names(prod_cleaned)) || !all(month_cols %in% names(prod_cleaned))) {
      missing_cols <- c(cols_to_check_base[!cols_to_check_base %in% names(prod_cleaned)], month_cols[!month_cols %in% names(prod_cleaned)])
      warning(paste("Filtered Group: Missing required columns after cleaning:", paste(missing_cols, collapse=", ")))
      removeNotification("filtRateMsg"); return(NULL)
    }
    
    for(mc in month_cols) if(!is.numeric(prod_cleaned[[mc]])) prod_cleaned[, (mc) := as.numeric(get(mc))]
    if(!is.numeric(prod_cleaned[[year_col]])) prod_cleaned[, (year_col) := as.numeric(get(year_col))]
    if(!is.character(prod_cleaned[[product_col]])) prod_cleaned[, (product_col) := as.character(get(product_col))]
    
    id_vars_melt <- c("GSL_UWI_Std", year_col, product_col, breakout_col_name, "LateralLength", "OperatorName")
    id_vars_melt <- unique(id_vars_melt)
    prod_long <- melt(prod_cleaned, id.vars = id_vars_melt, measure.vars = month_cols, variable.name = "MONTH_VOL_COL", value.name = "Volume", na.rm = FALSE)
    
    prod_long[toupper(get(product_col)) %in% c(toupper(oil_only_val), toupper(cnd_only_val)), Volume_Converted := Volume * M3_TO_BBL]
    prod_long[toupper(get(product_col)) == toupper(gas_val), Volume_Converted := Volume * E3M3_TO_MCF]
    prod_long[is.na(Volume_Converted), Volume_Converted := 0]
    
    prod_long <- prod_long[!is.na(Volume_Converted) & Volume_Converted != 0]
    if(nrow(prod_long) == 0) { removeNotification("filtRateMsg"); return(NULL) }
    prod_long[, Month_Num := match(toupper(substr(MONTH_VOL_COL, 1, 3)), toupper(month.abb))]
    prod_long <- prod_long[!is.na(get(year_col)) & !is.na(Month_Num)]
    prod_long[, PROD_DATE := as.Date(paste(get(year_col), Month_Num, 1, sep="-"), format="%Y-%m-%d")]; prod_long <- prod_long[!is.na(PROD_DATE)]
    
    prod_long[, MonthlyOilTrueBBL_raw := fifelse(toupper(get(product_col)) == toupper(oil_only_val), Volume_Converted, 0)]
    prod_long[, MonthlyCondensateBBL_raw := fifelse(toupper(get(product_col)) == toupper(cnd_only_val), Volume_Converted, 0)]
    prod_long[, MonthlyGasMCF_raw := fifelse(toupper(get(product_col)) == toupper(gas_val), Volume_Converted, 0)]
    
    prod_long[, DaysInMonth := lubridate::days_in_month(PROD_DATE)]
    prod_long[, DailyBOE_well := (MonthlyOilTrueBBL_raw + MonthlyCondensateBBL_raw + (MonthlyGasMCF_raw / MCF_PER_BOE)) / DaysInMonth]
    prod_long[, DailyBOE_per_1000ft_well := ifelse(LateralLength > 0 & !is.na(LateralLength), DailyBOE_well / (LateralLength / 1000), NA_real_)]
    
    prod_long[, PeakDailyBOE_well := max(DailyBOE_well, na.rm = TRUE), by = GSL_UWI_Std]
    peak_info_well <- prod_long[DailyBOE_well > 0 & DailyBOE_well == PeakDailyBOE_well, .(PeakProdDate_well = min(PROD_DATE)), by = GSL_UWI_Std]
    
    prod_long_from_peak <- merge(prod_long, peak_info_well, by = "GSL_UWI_Std", all.x = TRUE)
    prod_long_from_peak <- prod_long_from_peak[!is.na(PeakProdDate_well) & PROD_DATE >= PeakProdDate_well]
    if(nrow(prod_long_from_peak) == 0) { removeNotification("filtRateMsg"); return(NULL) }
    
    prod_long_from_peak[, MonthOnProd := round(as.numeric(PROD_DATE - PeakProdDate_well) / AVG_DAYS_PER_MONTH) + 1]
    
    grouping_vars_norm <- c(breakout_col_name, "MonthOnProd")
    agg_by_month_group_norm <- prod_long_from_peak[, .(
      AvgNormBOERate_per_1000ft = mean(DailyBOE_per_1000ft_well, na.rm = TRUE),
      WellCount_Norm = uniqueN(GSL_UWI_Std)
    ), by = grouping_vars_norm][order(get(breakout_col_name), MonthOnProd)]
    
    agg_by_month_group_norm[is.na(AvgNormBOERate_per_1000ft), AvgNormBOERate_per_1000ft := 0]
    agg_by_month_group_norm[, CumNormBOE_per_1000ft := cumsum(AvgNormBOERate_per_1000ft * AVG_DAYS_PER_MONTH), by = c(breakout_col_name)]
    
    # --- MODIFIED: Calendar plot data now includes daily rate ---
    agg_by_calendar_month_group <- prod_long[, .(
      TotalMonthlyBOE_Cal = sum(MonthlyOilTrueBBL_raw + MonthlyCondensateBBL_raw + (MonthlyGasMCF_raw / MCF_PER_BOE), na.rm = TRUE)
    ), by = c("PROD_DATE", breakout_col_name)][order(get(breakout_col_name), PROD_DATE)]
    agg_by_calendar_month_group[, DaysInMonth := lubridate::days_in_month(PROD_DATE)]
    agg_by_calendar_month_group[, AvgDailyBOERate_Cal := TotalMonthlyBOE_Cal / DaysInMonth]
    agg_by_calendar_month_group[, CumBOE_Calendar := cumsum(TotalMonthlyBOE_Cal), by = c(breakout_col_name)]
    # --- END MODIFICATION ---
    
    # Table data: Aggregate on calendar basis, grouped by the selected breakout column
    table_data_prep <- prod_long[, .(
      TotalMonthlyOilTrueBBL_sum = sum(MonthlyOilTrueBBL_raw, na.rm = TRUE), # Sum for the group-month
      TotalMonthlyCondensateBBL_sum = sum(MonthlyCondensateBBL_raw, na.rm = TRUE),
      TotalMonthlyGasMCF_sum = sum(MonthlyGasMCF_raw, na.rm = TRUE),
      DailyBOE_per_1000ft_well_avg = mean(DailyBOE_per_1000ft_well, na.rm = TRUE), # Average of per-well normalized rates
      DailyBOE_well_avg = mean(DailyBOE_well, na.rm=TRUE), # Average of per-well BOE rates
      TotalLateralLength_sum = sum(fifelse(DailyBOE_well > 0 & !is.na(LateralLength), LateralLength, 0), na.rm=TRUE) # Sum LL of producing wells in group-month
    ), by = c("PROD_DATE", breakout_col_name)]
    
    table_data_final <- table_data_prep %>%
      .[, DaysInMonth := lubridate::days_in_month(PROD_DATE)] %>%
      .[, TotalMonthlyBOE := (DailyBOE_well_avg * DaysInMonth)] %>% # Recalculate total from avg daily
      .[order(get(breakout_col_name), PROD_DATE)] %>%
      .[, CumTotalMonthlyBOE := cumsum(TotalMonthlyBOE), by = c(breakout_col_name)] %>%
      setnames(old = c("DailyBOE_per_1000ft_well_avg", "DailyBOE_well_avg", "TotalSumLateralLength_sum"),
               new = c("AvgDailyBOE_per_1000ft", "AvgDailyBOE", "TotalSumLateralLength"), skip_absent = TRUE)
    
    
    removeNotification("filtRateMsg"); showNotification("Filtered group normalized rates processed.", type="message");
    return(list(
      normalized_data = agg_by_month_group_norm,
      calendar_cumulative_data = agg_by_calendar_month_group,
      table_data = table_data_final
    ))
  })
  output$filtered_group_plot_title_normalized <- renderUI({
    breakout_choice_map <- c("OperatorName" = "Operator",
                             "Formation" = "Formation",
                             "FieldName" = "Field",
                             "ProvinceState" = "Province/State",
                             "FirstProdYear" = "First Prod Year")
    display_breakout_name <- breakout_choice_map[input$filtered_group_breakout_by]
    if(is.na(display_breakout_name) || is.null(display_breakout_name)) display_breakout_name <- input$filtered_group_breakout_by
    h5(paste("Average Daily BOE Rate per 1000ft Lateral by", display_breakout_name, "(Time Normalized)"))
  })
  output$filtered_group_plot_title_cumulative_boe <- renderUI({
    breakout_choice_map <- c("OperatorName" = "Operator",
                             "Formation" = "Formation",
                             "FieldName" = "Field",
                             "ProvinceState" = "Province/State",
                             "FirstProdYear" = "First Prod Year")
    display_breakout_name <- breakout_choice_map[input$filtered_group_breakout_by]
    if(is.na(display_breakout_name) || is.null(display_breakout_name)) display_breakout_name <- input$filtered_group_breakout_by
    h5(paste("Cumulative BOE Production by", display_breakout_name, "(Calendar Time)"))
  })
  
  output$filtered_group_cumulative_plot_normalized <- renderPlot({
    analysis_data <- filtered_group_cumulative_data()
    req(analysis_data, analysis_data$normalized_data)
    plot_data <- analysis_data$normalized_data
    
    if(is.null(plot_data) || nrow(plot_data) == 0) { return(ggplot() + labs(title = "No normalized rate data for filtered wells.", x=NULL, y=NULL) + theme_void()) }
    
    plot_data_for_ggplot <- plot_data[!is.na(AvgNormBOERate_per_1000ft) & is.finite(AvgNormBOERate_per_1000ft) & AvgNormBOERate_per_1000ft >= 0] # Allow 0 for plotting start
    if(nrow(plot_data_for_ggplot) == 0) { return(ggplot() + labs(title="No positive normalized BOE rates for filtered wells by selected group.", x="Month on Production", y="Normalized BOE Rate (BOE/d per 1000ft)") + theme_minimal()) }
    
    breakout_column_r_name <- input$filtered_group_breakout_by
    
    if (!breakout_column_r_name %in% names(plot_data_for_ggplot)) {
      warning(paste("Breakout column", breakout_column_r_name, "not found in normalized plot data for ggplot."))
      return(ggplot() + labs(title = paste("Error: Breakout column",input$filtered_group_breakout_by,"not found." )) + theme_void())
    }
    unique_groups_plot <- unique(plot_data_for_ggplot[[breakout_column_r_name]])
    
    group_colors_plot <- custom_palette[1:min(length(unique_groups_plot), length(custom_palette))]
    if (length(unique_groups_plot) > length(custom_palette)) {
      group_colors_plot <- rep(custom_palette, length.out = length(unique_groups_plot))
    }
    names(group_colors_plot) <- unique_groups_plot
    
    # Primary y-axis label
    y_lab_primary <- "Avg Daily BOE Rate per 1000m Lateral (Solid)"
    
    p_norm <- ggplot(plot_data_for_ggplot, aes(x = MonthOnProd, y = AvgNormBOERate_per_1000ft, color = .data[[breakout_column_r_name]], group = .data[[breakout_column_r_name]])) +
      geom_line(linewidth = 1.2) +
      scale_x_continuous(name = "Month on Production", breaks = scales::pretty_breaks(n=10)) +
      labs(color = names(which(c("OperatorName" = "Operator", "Formation" = "Formation", "FieldName" = "Field", "ProvinceState" = "Province/State", "FirstProdYear" = "First Prod Year") == breakout_column_r_name))) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
      scale_color_manual(values = group_colors_plot)
    
    # Add secondary axis for cumulative normalized production conditionally
    if ("CumNormBOE_per_1000ft" %in% names(plot_data_for_ggplot) && any(!is.na(plot_data_for_ggplot$CumNormBOE_per_1000ft) & plot_data_for_ggplot$CumNormBOE_per_1000ft > 0)) {
      max_rate <- max(plot_data_for_ggplot$AvgNormBOERate_per_1000ft, na.rm = TRUE)
      max_cum_data <- plot_data_for_ggplot[!is.na(CumNormBOE_per_1000ft)]
      max_cum <- if(nrow(max_cum_data) > 0) max(max_cum_data$CumNormBOE_per_1000ft, na.rm = TRUE) else 0
      
      if (is.finite(max_rate) && max_rate > 0 && is.finite(max_cum) && max_cum > 0) {
        scale_factor <- max_rate / max_cum
        if (is.finite(scale_factor) && scale_factor != 0) {
          p_norm <- p_norm +
            geom_line(data=plot_data_for_ggplot, aes(y = CumNormBOE_per_1000ft * scale_factor), linetype = "dashed", linewidth = 0.8) + # inherit.aes = FALSE removed to use existing color mapping
            scale_y_continuous( # Define both axes here
              name = y_lab_primary,
              labels = scales::comma,
              sec.axis = sec_axis(~ . / scale_factor, name = "Cum. Norm. BOE per 1000m (Dashed)", labels = scales::comma)
            )
        } else {
          p_norm <- p_norm + scale_y_continuous(name = y_lab_primary, labels = scales::comma)
        }
      } else {
        p_norm <- p_norm + scale_y_continuous(name = y_lab_primary, labels = scales::comma)
      }
    } else {
      p_norm <- p_norm + scale_y_continuous(name = y_lab_primary, labels = scales::comma)
    }
    return(p_norm)
  })
  output$filtered_group_cumulative_plot_cumulative_boe <- renderPlot({
    analysis_data <- filtered_group_cumulative_data()
    req(analysis_data, analysis_data$calendar_cumulative_data)
    plot_data <- analysis_data$calendar_cumulative_data # This now contains the breakout column
    
    breakout_column_r_name_cum <- input$filtered_group_breakout_by # Use the same breakout
    
    if(nrow(plot_data) == 0) { return(ggplot() + labs(title = "No cumulative BOE data for filtered wells.", x=NULL, y=NULL) + theme_void()) }
    
    plot_data_for_ggplot <- plot_data[!is.na(CumBOE_Calendar) & is.finite(CumBOE_Calendar)]
    if(nrow(plot_data_for_ggplot) == 0) { return(ggplot() + labs(title=paste("No cumulative BOE data for filtered wells by", breakout_column_r_name_cum), x="Date", y="Cumulative BOE") + theme_minimal()) }
    
    if (!breakout_column_r_name_cum %in% names(plot_data_for_ggplot)) {
      warning(paste("Breakout column", breakout_column_r_name_cum, "not found in calendar cumulative plot data for ggplot."))
      return(ggplot() + labs(title = paste("Error: Breakout column", breakout_column_r_name_cum,"not found." )) + theme_void())
    }
    unique_groups_plot_cum <- unique(plot_data_for_ggplot[[breakout_column_r_name_cum]])
    group_colors_plot_cum <- custom_palette[1:min(length(unique_groups_plot_cum), length(custom_palette))]
    if (length(unique_groups_plot_cum) > length(custom_palette)) {
      group_colors_plot_cum <- rep(custom_palette, length.out = length(unique_groups_plot_cum))
    }
    names(group_colors_plot_cum) <- unique_groups_plot_cum
    
    ggplot(plot_data_for_ggplot, aes(x = PROD_DATE, y = CumBOE_Calendar / 1000, color = .data[[breakout_column_r_name_cum]], group = .data[[breakout_column_r_name_cum]])) +
      geom_line(linewidth = 1.2) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
      labs(title = paste("Cumulative BOE Production by", names(which(c("OperatorName" = "Operator", "Formation" = "Formation", "FieldName" = "Field", "ProvinceState" = "Province/State", "FirstProdYear" = "First Prod Year") == breakout_column_r_name_cum)), "(Calendar Time)"),
           x = "Date", y = "Cumulative BOE (MBOE)", color = names(which(c("OperatorName" = "Operator", "Formation" = "Formation", "FieldName" = "Field", "ProvinceState" = "Province/State", "FirstProdYear" = "First Prod Year") == breakout_column_r_name_cum))) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
      scale_color_manual(values = group_colors_plot_cum)
  })
  
  # --- NEW PLOT TITLE AND RENDER LOGIC ---
  output$filtered_group_plot_title_calendar_rate <- renderUI({
    breakout_choice_map <- c("OperatorName" = "Operator",
                             "Formation" = "Formation",
                             "FieldName" = "Field",
                             "ProvinceState" = "Province/State",
                             "FirstProdYear" = "First Prod Year")
    display_breakout_name <- breakout_choice_map[input$filtered_group_breakout_by]
    if(is.na(display_breakout_name) || is.null(display_breakout_name)) display_breakout_name <- input$filtered_group_breakout_by
    h5(paste("Average Daily BOE Rate by", display_breakout_name, "(Calendar Time)"))
  })
  
  output$filtered_group_calendar_rate_plot <- renderPlot({
    analysis_data <- filtered_group_cumulative_data()
    req(analysis_data, analysis_data$calendar_cumulative_data)
    plot_data <- analysis_data$calendar_cumulative_data
    
    breakout_column_r_name_rate <- input$filtered_group_breakout_by
    
    if(nrow(plot_data) == 0 || !"AvgDailyBOERate_Cal" %in% names(plot_data)) {
      return(ggplot() + labs(title = "No calendar rate data for filtered wells.", x=NULL, y=NULL) + theme_void())
    }
    
    plot_data_for_ggplot <- plot_data[!is.na(AvgDailyBOERate_Cal) & is.finite(AvgDailyBOERate_Cal)]
    if(nrow(plot_data_for_ggplot) == 0) {
      return(ggplot() + labs(title=paste("No daily rate data for filtered wells by", breakout_column_r_name_rate), x="Date", y="Avg Daily BOE Rate") + theme_minimal())
    }
    
    if (!breakout_column_r_name_rate %in% names(plot_data_for_ggplot)) {
      warning(paste("Breakout column", breakout_column_r_name_rate, "not found in calendar rate plot data for ggplot."))
      return(ggplot() + labs(title = paste("Error: Breakout column", breakout_column_r_name_rate,"not found." )) + theme_void())
    }
    
    unique_groups_plot_rate <- unique(plot_data_for_ggplot[[breakout_column_r_name_rate]])
    group_colors_plot_rate <- custom_palette[1:min(length(unique_groups_plot_rate), length(custom_palette))]
    if (length(unique_groups_plot_rate) > length(custom_palette)) {
      group_colors_plot_rate <- rep(custom_palette, length.out = length(unique_groups_plot_rate))
    }
    names(group_colors_plot_rate) <- unique_groups_plot_rate
    
    legend_title <- names(which(c("OperatorName" = "Operator", "Formation" = "Formation", "FieldName" = "Field", "ProvinceState" = "Province/State", "FirstProdYear" = "First Prod Year") == breakout_column_r_name_rate))
    
    ggplot(plot_data_for_ggplot, aes(x = PROD_DATE, y = AvgDailyBOERate_Cal, color = .data[[breakout_column_r_name_rate]], group = .data[[breakout_column_r_name_rate]])) +
      geom_line(linewidth = 1.2) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
      labs(
        x = "Date", y = "Average Daily BOE Rate (boe/d)",
        color = legend_title
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
      scale_color_manual(values = group_colors_plot_rate)
  })
  # --- END NEW PLOT LOGIC ---
  
  output$filtered_group_production_table <- DT::renderDataTable({
    analysis_data <- filtered_group_cumulative_data()
    if(is.null(analysis_data) || is.null(analysis_data$table_data) || nrow(analysis_data$table_data) == 0){
      return(DT::datatable(data.frame(Message = "No data for filtered group table. Check filters or click 'Calculate' button."),
                           options = list(searching=FALSE, paging=FALSE, info=FALSE), rownames=FALSE))
    }
    table_data <- analysis_data$table_data
    
    display_data <- copy(table_data)
    
    breakout_col_r_name <- input$filtered_group_breakout_by
    breakout_col_display_name <- names(which(c("OperatorName" = "Operator", "Formation" = "Formation", "FieldName" = "Field", "ProvinceState" = "Province/State", "FirstProdYear" = "First Prod Year") == breakout_col_r_name))
    if(length(breakout_col_display_name) == 0) breakout_col_display_name <- breakout_col_r_name
    
    # Rename columns for display, ensuring the dynamic breakout column is handled
    # The `table_data_final` should already have the breakout column with its original R name
    current_names <- names(display_data)
    new_names_map <- list(
      "PROD_DATE" = "Prod. Month",
      "TotalMonthlyBOE" = "Total Monthly BOE",
      "AvgDailyBOE" = "Avg Daily BOE",
      "AvgDailyBOE_per_1000ft" = "Avg Daily BOE/1kft",
      "CumTotalMonthlyBOE" = "Cum. BOE",
      "TotalSumLateralLength" = "Sum Prod. LatLen (ft)"
    )
    # Add the dynamic breakout column to the map
    new_names_map[[breakout_col_r_name]] <- breakout_col_display_name
    
    for(old_n in names(new_names_map)){
      if(old_n %in% current_names){
        setnames(display_data, old = old_n, new = new_names_map[[old_n]], skip_absent = TRUE)
      }
    }
    
    if ("Prod. Month" %in% names(display_data)) {
      # Ensure 'Prod. Month' is Date before formatting
      if(!inherits(display_data[['Prod. Month']], "Date")){
        display_data[, `Prod. Month` := tryCatch(as.Date(as.character(`Prod. Month`)), error = function(e) NA_Date_)]
      }
      display_data[!is.na(`Prod. Month`), `Prod. Month` := format(`Prod. Month`, "%Y-%m")]
    }
    
    numeric_cols_to_round <- c("Total Monthly BOE", "Avg Daily BOE", "Avg Daily BOE/1kft", "Cum. BOE", "Sum Prod. LatLen (ft)")
    for(rcfg in numeric_cols_to_round){ if(rcfg %in% names(display_data) && is.numeric(display_data[[rcfg]])) display_data[[rcfg]] <- round(display_data[[rcfg]], 2) }
    
    cols_order <- c(breakout_col_display_name, "Prod. Month")
    other_cols <- setdiff(names(display_data), cols_order)
    actual_cols_for_order <- intersect(c(cols_order, other_cols), names(display_data))
    if(breakout_col_display_name %in% names(display_data)){
      display_data <- display_data[, ..actual_cols_for_order]
    }
    
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        order=list(list(0, 'asc'), list(1,'desc')),
        rowCallback = dt_rowCallback_js
      ),
      rownames=F,
      caption = paste("Aggregated Monthly & Normalized Production by", breakout_col_display_name ,"(Map-Filtered Wells)")
    )
  })
  output$download_filtered_group_prod_data <- downloadHandler(
    filename = function() {
      breakout_col_r_name <- input$filtered_group_breakout_by
      breakout_col_display_name_fn <- names(which(c("OperatorName" = "Operator", "Formation" = "Formation", "FieldName" = "Field", "ProvinceState" = "Province_State", "FirstProdYear" = "First_Prod_Year") == breakout_col_r_name))
      if(length(breakout_col_display_name_fn)==0) breakout_col_display_name_fn <- breakout_col_r_name
      paste0("filtered_group_prod_by_", tolower(gsub("(/| )", "_", breakout_col_display_name_fn)), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      analysis_data <- filtered_group_cumulative_data()
      req(analysis_data, analysis_data$table_data)
      data_dl <- copy(analysis_data$table_data)
      
      if(!is.null(data_dl) && nrow(data_dl) > 0) {
        breakout_col_r_name <- input$filtered_group_breakout_by # Original R name
        breakout_col_dl_name_map <- c("OperatorName" = "Group_By_Operator",
                                      "Formation" = "Group_By_Formation",
                                      "FieldName" = "Group_By_Field",
                                      "ProvinceState" = "Group_By_Province_State",
                                      "FirstProdYear" = "Group_By_First_Prod_Year")
        breakout_col_dl_name <- breakout_col_dl_name_map[breakout_col_r_name]
        if(is.na(breakout_col_dl_name)) breakout_col_dl_name <- breakout_col_r_name
        
        
        old_dl_names <- c("PROD_DATE", breakout_col_r_name, "TotalMonthlyBOE", "AvgDailyBOE", "AvgDailyBOE_per_1000ft", "CumTotalMonthlyBOE", "TotalSumLateralLength")
        new_dl_names <- c("Prod_Month", breakout_col_dl_name, "Total_Monthly_BOE", "Avg_Daily_BOE", "Avg_Daily_BOE_per_1000ft", "Cumulative_BOE", "Sum_Producing_Lateral_Length_ft")
        
        # Ensure only existing columns are attempted to be renamed
        current_dl_names <- names(data_dl)
        valid_old_dl_names <- old_dl_names[old_dl_names %in% current_dl_names]
        valid_new_dl_names <- new_dl_names[match(valid_old_dl_names, old_dl_names)]
        
        if(length(valid_old_dl_names) > 0) {
          setnames(data_dl, old = valid_old_dl_names, new = valid_new_dl_names, skip_absent = TRUE)
        }
        fwrite(data_dl, file)
      } else { fwrite(data.table(Message="No data for download."), file) }
    }
  )
  
  # --- Type Curve Analysis (Arps) ---
  type_curve_analysis_data <- eventReactive(input$generate_type_curve, {
    req(wells_sf_global, input$product_type_filter_analysis)
    
    selected_analysis_products <- input$product_type_filter_analysis
    if (is.null(selected_analysis_products) || length(selected_analysis_products) == 0) {
      showNotification("Please select at least one product type for analysis (under 'Production Analysis' main tab).", type = "warning", duration=5)
      return(list(data=data.table(), fit=NULL, params_text_base="No product types selected for analysis.", eur_25yr = NA_real_, monthly_declines_for_output = data.table()))
    }
    
    showNotification("Generating Arps type curve data (peak normalized, daily rates)...", type = "message", duration = NULL, id="arpsMsg")
    filtered_wells_sf_for_arps <- reactive_vals$wells_to_display
    
    max_wells_for_type_curve <- 1000
    num_selected_wells <- if(!is.null(filtered_wells_sf_for_arps) && nrow(filtered_wells_sf_for_arps) > 0) {
      length(unique(na.omit(filtered_wells_sf_for_arps$GSL_UWI_Std)))
    } else { 0 }
    
    if (num_selected_wells > max_wells_for_type_curve) {
      showNotification(paste("Too many wells selected (", num_selected_wells, ") for type curve analysis. Please refine filters to select fewer than", max_wells_for_type_curve, "wells."), type = "warning", duration = 7, id="tooManyWellsArps")
      removeNotification("arpsMsg")
      return(list(data=data.table(), fit=NULL, params_text_base=paste("Too many wells selected (", num_selected_wells, "). Max allowed:", max_wells_for_type_curve), eur_25yr = NA_real_, monthly_declines_for_output = data.table()))
    }
    
    if (is.null(filtered_wells_sf_for_arps) || nrow(filtered_wells_sf_for_arps) == 0) { removeNotification("arpsMsg"); showNotification("No wells selected by map filters for type curve.", type = "warning"); return(list(data=data.table(), fit=NULL, params_text_base="No wells selected.", eur_25yr = NA_real_, monthly_declines_for_output = data.table())) }
    target_uwis <- unique(filtered_wells_sf_for_arps$GSL_UWI_Std)
    target_uwis <- target_uwis[!is.na(target_uwis) & target_uwis != ""]
    if (length(target_uwis) == 0) { removeNotification("arpsMsg"); showNotification("No valid GSL_UWI_Std for type curve.", type = "warning"); return(list(data=data.table(), fit=NULL, params_text_base="No valid UWIs.", eur_25yr = NA_real_, monthly_declines_for_output = data.table())) }
    if (is.null(con) || !dbIsValid(con)) { con <<- connect_to_db(); if (is.null(con) || !dbIsValid(con)) { removeNotification("arpsMsg"); return(list(data=data.table(), fit=NULL, params_text_base="DB connection failed.", eur_25yr = NA_real_, monthly_declines_for_output = data.table())) } }
    
    uwi_batches <- split(target_uwis, ceiling(seq_along(target_uwis)/300)); all_prod_list_arps <- list()
    for(batch_uwis in uwi_batches) {
      sql_arps <- glue::glue_sql( "SELECT GSL_UWI, YEAR, PRODUCT_TYPE, ACTIVITY_TYPE, JAN_VOLUME, FEB_VOLUME, MAR_VOLUME, APR_VOLUME, MAY_VOLUME, JUN_VOLUME, JUL_VOLUME, AUG_VOLUME, SEP_VOLUME, OCT_VOLUME, NOV_VOLUME, DEC_VOLUME FROM PDEN_VOL_BY_MONTH WHERE GSL_UWI IN ({uwis*}) AND ACTIVITY_TYPE = 'PRODUCTION' AND PRODUCT_TYPE IN ('OIL', 'CND', 'GAS')", uwis = batch_uwis, .con = con)
      batch_data <- tryCatch(as.data.table(dbGetQuery(con, sql_arps)), error = function(e) data.table()); if(nrow(batch_data)>0) all_prod_list_arps[[length(all_prod_list_arps)+1]] <- batch_data
    }
    if(length(all_prod_list_arps) == 0) { removeNotification("arpsMsg"); showNotification("No production data for type curve wells.", type="warning"); return(list(data=data.table(), fit=NULL, params_text_base="No production data.", eur_25yr = NA_real_, monthly_declines_for_output = data.table())) }
    full_prod_arps_raw <- rbindlist(all_prod_list_arps, use.names = TRUE, fill = TRUE)
    
    # Filter by selected product types for analysis (respecting "BOE" selection for Arps)
    effective_selected_analysis_products <- selected_analysis_products
    if ("BOE" %in% selected_analysis_products) {
      effective_selected_analysis_products <- unique(c(effective_selected_analysis_products, "OIL", "CND", "GAS"))
      effective_selected_analysis_products <- effective_selected_analysis_products[effective_selected_analysis_products != "BOE"]
    }
    
    if (!is.null(effective_selected_analysis_products) && length(effective_selected_analysis_products) > 0) {
      full_prod_arps_raw <- full_prod_arps_raw[toupper(PRODUCT_TYPE) %in% toupper(effective_selected_analysis_products)]
    }
    if(nrow(full_prod_arps_raw) == 0) {
      removeNotification("arpsMsg");
      showNotification("No production data for the selected product type(s) for Arps.", type = "warning");
      return(list(data=data.table(), fit=NULL, params_text_base="No production data for selected product(s).", eur_25yr = NA_real_, monthly_declines_for_output = data.table()))
    }
    
    if("GSL_UWI" %in% names(full_prod_arps_raw)) { full_prod_arps_raw[, GSL_UWI_Std_query := standardize_uwi(GSL_UWI)] }
    prod_arps_cleaned <- clean_df_colnames(full_prod_arps_raw, "Arps Type Curve PDEN")
    
    if ("GSL_UWI_STD_QUERY" %in% names(prod_arps_cleaned)) {
      setnames(prod_arps_cleaned, "GSL_UWI_STD_QUERY", "GSL_UWI_Std")
    } else if ("GSL_UWI" %in% names(prod_arps_cleaned) && !"GSL_UWI_Std" %in% names(prod_arps_cleaned)) {
      prod_arps_cleaned[, GSL_UWI_Std := standardize_uwi(GSL_UWI)]
    } else if (!"GSL_UWI_Std" %in% names(prod_arps_cleaned)) {
      warning("Arps: GSL_UWI_Std column not found after cleaning.")
      removeNotification("arpsMsg"); return(list(data=data.table(), fit=NULL, params_text_base="Cannot ID wells.", eur_25yr = NA_real_, monthly_declines_for_output = data.table()))
    }
    
    year_col <- "YEAR"; product_col <- "PRODUCT_TYPE";
    arps_product_choice <- input$arps_product_type # "Oil" or "Gas" (for Arps specific selection)
    
    # Determine which DB product types map to the Arps selection, considering the global analysis filter
    db_products_for_this_arps_run <- character(0)
    if (arps_product_choice == "Oil") { # This means "Oil/Condensate" for Arps
      if ("OIL" %in% effective_selected_analysis_products) db_products_for_this_arps_run <- c(db_products_for_this_arps_run, "OIL")
      if ("CND" %in% effective_selected_analysis_products) db_products_for_this_arps_run <- c(db_products_for_this_arps_run, "CND")
    } else if (arps_product_choice == "Gas") {
      if ("GAS" %in% effective_selected_analysis_products) db_products_for_this_arps_run <- c(db_products_for_this_arps_run, "GAS")
    }
    
    if (length(db_products_for_this_arps_run) == 0) {
      removeNotification("arpsMsg");
      showNotification(paste("No data for Arps. Arps product:", arps_product_choice, " is not among globally selected products:", paste(selected_analysis_products, collapse=", ")), type="warning", duration=7);
      return(list(data=data.table(), fit=NULL, params_text_base="No data for Arps product based on global filter.", eur_25yr = NA_real_, monthly_declines_for_output = data.table()))
    }
    
    month_cols <- paste0(toupper(month.abb), "_VOLUME")
    
    cols_to_check_arps <- c(year_col, product_col, "GSL_UWI_Std")
    if(!all(cols_to_check_arps %in% names(prod_arps_cleaned)) || !all(month_cols %in% names(prod_arps_cleaned))) {
      missing_cols_arps <- c(cols_to_check_arps[!cols_to_check_arps %in% names(prod_arps_cleaned)], month_cols[!month_cols %in% names(prod_arps_cleaned)])
      warning(paste("Arps: Missing required columns after cleaning:", paste(missing_cols_arps, collapse=", ")))
      removeNotification("arpsMsg"); return(list(data=data.table(), fit=NULL, params_text_base="Missing critical columns for Arps.", eur_25yr = NA_real_, monthly_declines_for_output = data.table()))
    }
    
    for(mc in month_cols) if(!is.numeric(prod_arps_cleaned[[mc]])) prod_arps_cleaned[, (mc) := as.numeric(get(mc))]
    prod_arps_cleaned[, (year_col) := as.numeric(get(year_col))]; prod_arps_cleaned[, (product_col) := as.character(get(product_col))]
    prod_arps_long <- melt(prod_arps_cleaned, id.vars = c("GSL_UWI_Std", year_col, product_col), measure.vars = month_cols, variable.name = "MONTH_VOL_COL", value.name = "MonthlyVolume", na.rm = FALSE)
    
    prod_arps_long <- prod_arps_long[toupper(get(product_col)) %in% toupper(db_products_for_this_arps_run)]
    if(nrow(prod_arps_long) == 0) { removeNotification("arpsMsg"); showNotification("No production data for selected Arps product type after global product filter.", type="warning"); return(list(data=data.table(), fit=NULL, params_text_base="No data for Arps product.", eur_25yr = NA_real_, monthly_declines_for_output = data.table())) }
    
    # Apply unit conversions
    prod_arps_long[toupper(get(product_col)) %in% c("OIL", "CND"), MonthlyVolume := MonthlyVolume * M3_TO_BBL]
    prod_arps_long[toupper(get(product_col)) == "GAS", MonthlyVolume := MonthlyVolume * E3M3_TO_MCF]
    
    # Aggregate volumes if "Oil/Condensate" is selected for Arps (summing OIL and CND for each well-month)
    if (arps_product_choice == "Oil" && length(intersect(c("OIL", "CND"), db_products_for_this_arps_run)) > 0) {
      prod_arps_long <- prod_arps_long[, .(MonthlyVolume = sum(MonthlyVolume, na.rm = TRUE)),
                                       by = .(GSL_UWI_Std, YEAR, MONTH_VOL_COL)]
      prod_arps_long[, (product_col) := "Oil_Cond_Combined"]
    }
    
    prod_arps_long <- prod_arps_long[!is.na(MonthlyVolume) & MonthlyVolume > 0]
    if(nrow(prod_arps_long) == 0) { removeNotification("arpsMsg"); showNotification("No positive production after unit conversion for Arps.", type="warning"); return(list(data=data.table(), fit=NULL, params_text_base="No positive production.", eur_25yr = NA_real_, monthly_declines_for_output = data.table())) }
    
    prod_arps_long[, Month_Num := match(toupper(substr(MONTH_VOL_COL, 1, 3)), toupper(month.abb))]
    prod_arps_long <- prod_arps_long[!is.na(get(year_col)) & !is.na(Month_Num)]
    prod_arps_long[, PROD_DATE := as.Date(paste(get(year_col), Month_Num, 1, sep="-"), format="%Y-%m-%d")]; prod_arps_long <- prod_arps_long[!is.na(PROD_DATE)]
    
    prod_arps_long[, DaysInMonth := lubridate::days_in_month(PROD_DATE)]
    prod_arps_long[, DailyRate := MonthlyVolume / DaysInMonth]
    prod_arps_long <- prod_arps_long[!is.na(DailyRate) & is.finite(DailyRate) & DailyRate > 0]
    
    if(nrow(prod_arps_long) == 0) { removeNotification("arpsMsg"); showNotification(paste("No", input$arps_product_type, "production for Arps after all filters."), type="warning"); return(list(data=data.table(), fit=NULL, params_text_base=paste("No", input$arps_product_type, "data."), eur_25yr = NA_real_, monthly_declines_for_output = data.table())) }
    
    if (!"GSL_UWI_Std" %in% names(prod_arps_long)) {
      warning("Arps: GSL_UWI_Std is missing before peak calculation.")
      removeNotification("arpsMsg"); return(list(data=data.table(), fit=NULL, params_text_base="GSL_UWI_Std missing for peak calc.", eur_25yr = NA_real_, monthly_declines_for_output = data.table()))
    }
    prod_arps_long[, PeakDailyRate := max(DailyRate, na.rm = TRUE), by = GSL_UWI_Std]
    peak_info <- prod_arps_long[DailyRate == PeakDailyRate, .(PeakProdDate = min(PROD_DATE)), by = GSL_UWI_Std]
    
    prod_arps_long_from_peak <- merge(prod_arps_long, peak_info, by = "GSL_UWI_Std", all.x = TRUE)
    prod_arps_long_from_peak <- prod_arps_long_from_peak[!is.na(PeakProdDate) & PROD_DATE >= PeakProdDate]
    
    if(nrow(prod_arps_long_from_peak) == 0) { removeNotification("arpsMsg"); showNotification("No production data at or after peak for Arps.", type="warning"); return(list(data=data.table(), fit=NULL, params_text_base="No data at/after peak.", eur_25yr = NA_real_, monthly_declines_for_output = data.table())) }
    
    prod_arps_long_from_peak[, MonthOnProdNormalized := round(as.numeric(PROD_DATE - PeakProdDate) / AVG_DAYS_PER_MONTH) + 1]
    
    type_curve_data <- prod_arps_long_from_peak[, .(
      AvgDailyRate = mean(DailyRate, na.rm = TRUE),
      WellCount = uniqueN(GSL_UWI_Std)
    ), by = MonthOnProdNormalized][order(MonthOnProdNormalized)]
    setnames(type_curve_data, "MonthOnProdNormalized", "MonthOnProd")
    
    type_curve_data <- type_curve_data[WellCount >= 3]
    if(nrow(type_curve_data) < 3) { removeNotification("arpsMsg"); return(list(data=type_curve_data, fit=NULL, params_text_base="Not enough data points for fitting post-peak.", eur_25yr = NA_real_, monthly_declines_for_output = data.table())) }
    
    initial_Qi <- type_curve_data$AvgDailyRate[1]
    if (nrow(type_curve_data) >= 2 && initial_Qi > 0 && is.finite(initial_Qi)) { # Added is.finite
      initial_Di_monthly <- abs( (type_curve_data$AvgDailyRate[2] - initial_Qi) / initial_Qi )
      if(is.na(initial_Di_monthly) || !is.finite(initial_Di_monthly) || initial_Di_monthly == 0) initial_Di_monthly <- 0.05
    } else { initial_Di_monthly <- 0.05 }
    if(is.na(initial_Qi) || !is.finite(initial_Qi) || initial_Qi <=0) initial_Qi <- max(type_curve_data$AvgDailyRate[is.finite(type_curve_data$AvgDailyRate)], na.rm=T); if(is.na(initial_Qi) || !is.finite(initial_Qi) || initial_Qi <=0) initial_Qi <- 100 # Ensure finite max
    
    arps_params_text <- "Fit failed or not attempted."; fit_model <- NULL; eur_25yr <- NA_real_;
    monthly_declines_for_output <- data.table()
    
    tryCatch({
      if(input$arps_model_type == "hyperbolic") { fit_model <- nls(AvgDailyRate ~ Qi / (1 + b * Di * MonthOnProd)^(1/b), data = type_curve_data, start = list(Qi = initial_Qi, Di = initial_Di_monthly, b = 1.0), lower = list(Qi=1e-3, Di=1e-6, b=1e-6), upper = list(Qi=Inf, Di=1, b=2.0), algorithm = "port", control = nls.control(maxiter=200, warnOnly = TRUE, minFactor=1/2048))
      } else if (input$arps_model_type == "exponential") { fit_model <- nls(AvgDailyRate ~ Qi * exp(-Di * MonthOnProd), data = type_curve_data, start = list(Qi = initial_Qi, Di = initial_Di_monthly), lower = list(Qi=1e-3, Di=1e-6), algorithm = "port", control = nls.control(maxiter=100, warnOnly = TRUE))
      } else { fit_model <- nls(AvgDailyRate ~ Qi / (1 + Di * MonthOnProd), data = type_curve_data, start = list(Qi = initial_Qi, Di = initial_Di_monthly), lower = list(Qi=1e-3, Di=1e-6), algorithm = "port", control = nls.control(maxiter=100, warnOnly = TRUE)) }
      
      if(!is.null(fit_model)){
        # EUR Calculation with Dmin
        forecast_months <- 1:(25*12) # 25 years
        predicted_rates_dt <- data.table(MonthOnProd = forecast_months)
        predicted_rates_dt[, Rate_Arps := predict(fit_model, newdata = .SD)]
        predicted_rates_dt[Rate_Arps < 0 | is.na(Rate_Arps), Rate_Arps := 0]
        
        # Determine Dmin (annual effective)
        d_min_annual_eff <- if (input$arps_product_type == "Oil") 0.10 else 0.08 # 10% for Oil/Cnd, 8% for Gas
        # Convert annual effective Dmin to monthly effective Dmin
        d_min_monthly_eff <- 1 - (1 - d_min_annual_eff)^(1/12)
        
        # Apply Dmin from Year 11 (Month 121) onwards
        rate_at_month_120 <- predicted_rates_dt[MonthOnProd == 120, Rate_Arps]
        
        predicted_rates_dt[, FinalRate := Rate_Arps] # Initialize with Arps rate
        
        if (nrow(predicted_rates_dt[MonthOnProd > 120]) > 0 && rate_at_month_120 > 0) { # Check if rate_at_month_120 is positive
          # For months > 120, apply Dmin based on the rate of the PREVIOUS month using Dmin
          # Initialize rate for month 121 using Dmin from rate_at_month_120
          predicted_rates_dt[MonthOnProd == 121, FinalRate := rate_at_month_120 * (1 - d_min_monthly_eff)]
          
          for (m in 122:max(forecast_months)) {
            rate_prev_month_dmin <- predicted_rates_dt[MonthOnProd == (m - 1), FinalRate]
            predicted_rates_dt[MonthOnProd == m, FinalRate := rate_prev_month_dmin * (1 - d_min_monthly_eff)]
          }
        }
        predicted_rates_dt[FinalRate < 0 | is.na(FinalRate), FinalRate := 0] # Ensure no negative rates
        
        predicted_monthly_volumes_eur <- predicted_rates_dt$FinalRate * AVG_DAYS_PER_MONTH
        eur_25yr <- sum(predicted_monthly_volumes_eur, na.rm = TRUE)
        
        # For decline output, use the Arps-fitted curve for the first 10 years
        max_decline_month_output <- 120
        df_rates_for_decline_output <- data.table(MonthOnProd = 0:max_decline_month_output)
        df_rates_for_decline_output$Rate <- predict(fit_model, newdata = df_rates_for_decline_output)
        df_rates_for_decline_output[Rate < 0 | is.na(Rate), Rate := 0]
        df_rates_for_decline_output[, Rate_Prev_Month := shift(Rate, n=1, fill=NA, type="lag")]
        df_rates_for_decline_output <- df_rates_for_decline_output[MonthOnProd > 0]
        df_rates_for_decline_output[, MonthlyEffectiveDecline := ifelse(Rate_Prev_Month > 1e-6, (Rate_Prev_Month - Rate) / Rate_Prev_Month, NA_real_)] # Avoid division by zero/tiny
        monthly_declines_for_output <- df_rates_for_decline_output[, .(MonthOnProd, MonthlyEffectiveDecline)]
        
      }
    }, error = function(e) { arps_params_text <<- paste("Arps fitting error:", e$message); fit_model <<- NULL; eur_25yr <<- NA_real_; monthly_declines_for_output <<- data.table() })
    
    removeNotification("arpsMsg"); showNotification("Type curve data processed.", type="message")
    return(list(data = type_curve_data, fit = fit_model, params_text_base = arps_params_text, eur_25yr = eur_25yr, monthly_declines_for_output = monthly_declines_for_output))
  })
  
  output$arps_type_curve_plot <- renderPlot({
    analysis_results <- type_curve_analysis_data()
    req(analysis_results, analysis_results$data)
    plot_data <- analysis_results$data
    fit <- analysis_results$fit
    if(nrow(plot_data) == 0) { return(ggplot() + labs(title = "No data for Arps type curve.", x="Months Since Peak Production", y="Average Daily Rate") + theme_void()) }
    
    y_axis_label <- paste("Average Daily Rate", ifelse(input$arps_product_type=="Oil", "(BBL/day)", "(MCF/day)"))
    
    g <- ggplot(plot_data, aes(x = MonthOnProd, y = AvgDailyRate)) +
      geom_point(aes(size=WellCount), alpha=0.7, color=custom_palette[1]) +
      geom_line(color=custom_palette[1], alpha=0.5) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
      labs(title = paste("Arps Type Curve (Peak Normalized - Daily Rates) -", input$arps_product_type, "(", input$arps_model_type, "model)"),
           x = "Months Since Peak Production", y = y_axis_label,
           size = "Well Count",
           caption = "Points: Avg Actual Daily Rate. Dashed Red Line: Fitted Arps Curve.") +
      theme_minimal(base_size = 12) + theme(legend.position = "top")
    if (!is.null(fit)) {
      max_month_plot <- max(plot_data$MonthOnProd, na.rm = TRUE)
      predict_months_plot <- seq(1, max_month_plot, length.out = 200)
      if(is.finite(max_month_plot) && max_month_plot > 0){
        predict_df_plot <- data.table(MonthOnProd = predict_months_plot)
        tryCatch({
          predict_df_plot$PredictedRate <- predict(fit, newdata = predict_df_plot)
          predict_df_plot[PredictedRate < 0, PredictedRate := 0]
          g <- g + geom_line(data = predict_df_plot, aes(x=MonthOnProd, y = PredictedRate), color = custom_palette[2], linewidth = 1.1, linetype="dashed")
        }, error = function(e) { message(paste("Error during predict for Arps plot:", e$message)) })
      }
    }
    return(g)
  })
  
  output$arps_parameters_output <- renderText({
    analysis_results <- type_curve_analysis_data()
    req(analysis_results)
    
    params_text_list <- list()
    fit_model <- analysis_results$fit
    type_curve_data_for_calc <- analysis_results$data
    
    if (input$arps_product_type == "Oil") {
      params_text_list[["Product Type Note"]] <- "Oil/Condensate selection includes 'OIL' and 'CND' product types from database (if selected in global product filter)."
    }
    
    if (!is.null(fit_model)) {
      params <- coef(fit_model)
      params_text_list[["Fitted Model"]] <- paste("Model:", input$arps_model_type)
      params_text_list[["Qi (Initial Daily Rate of Fitted Curve)"]] <- paste(signif(params[["Qi"]], 4), ifelse(input$arps_product_type=="Oil", "BBL/day", "MCF/day"))
      
      if (nrow(type_curve_data_for_calc[MonthOnProd == 1]) > 0) {
        actual_ip30_val <- type_curve_data_for_calc[MonthOnProd == 1, AvgDailyRate]
        params_text_list[["Avg. Daily Rate Month 1 (IP30 Approx.)"]] <- paste(round(actual_ip30_val, 1), ifelse(input$arps_product_type=="Oil", "BBL/day", "MCF/day"))
      } else {
        params_text_list[["Avg. Daily Rate Month 1 (IP30 Approx.)"]] <- "N/A (No data at Month 1)"
      }
      
      params_text_list[["Di (Nominal Monthly Decline of Fitted Curve)"]] <- signif(params[["Di"]], 4)
      
      b_val_text <- if ("b" %in% names(params)) {
        signif(params[["b"]], 4)
      } else if (input$arps_model_type == "exponential"){
        "0 (Exponential)"
      } else if (input$arps_model_type == "harmonic") {
        "1 (Harmonic)"
      } else { "N/A" }
      params_text_list[["b (Hyperbolic Exponent of Fitted Curve)"]] <- b_val_text
      
      if ("b" %in% names(params) && params[["b"]] < 0.001 && params[["b"]] > -0.001 && input$arps_model_type == "hyperbolic") {
        params_text_list[["Note on 'b'"]] <- "Fitted 'b' is very small; curve behaves like an Exponential decline. This will result in constant effective decline percentages for periods of the same length."
      }
      
      eur_val <- analysis_results$eur_25yr
      eur_unit <- ifelse(input$arps_product_type=="Oil", "MBBL", "MMCF")
      eur_display_val <- if(!is.na(eur_val)) eur_val / 1000 else NA_real_
      
      if(!is.na(eur_display_val)) {
        params_text_list[["25-Year EUR (Adjusted for Terminal Decline)"]] <- paste(formatC(eur_display_val, format="f", big.mark=",", digits=1), eur_unit)
      } else {
        params_text_list[["25-Year EUR (Adjusted for Terminal Decline)"]] <- "N/A"
      }
      
    } else {
      params_text_list[["Fitted Model"]] <- analysis_results$params_text_base
    }
    
    # --- Decline Rate Percentages based on Excel Logic ---
    decline_output_formatted_lines <- list()
    decline_output_formatted_lines[[1]] <- "Decline Period\tRange Percentage"
    
    if (!is.null(fit_model) && nrow(analysis_results$monthly_declines_for_output) > 0) {
      monthly_eff_declines_dt <- analysis_results$monthly_declines_for_output
      
      get_d_eff <- function(month_num) {
        val <- monthly_eff_declines_dt[MonthOnProd == month_num, MonthlyEffectiveDecline]
        if (length(val) == 0 || is.na(val)) return(NA_real_)
        return(val)
      }
      
      get_rate_at_end_of_month <- function(month_num_on_prod) {
        if (month_num_on_prod == 0) return(coef(fit_model)[["Qi"]])
        val <- predict(fit_model, newdata = data.table(MonthOnProd = month_num_on_prod))
        if (length(val) == 0 || is.na(val) || val < 0) return(0)
        return(val)
      }
      
      decline_periods_monthly_geom <- list(
        "Month 2"   = c(2),
        "Month 3-4"   = c(3, 4),
        "Month 5-6"   = c(5, 6),
        "Month 7-12"  = 7:12,
        "Month 13-18" = 13:18,
        "Month 19-24" = 19:24,
        "Month 25-36" = 25:36
      )
      
      for(period_name in names(decline_periods_monthly_geom)){
        months_in_period <- decline_periods_monthly_geom[[period_name]]
        declines_for_geom_mean <- sapply(months_in_period, get_d_eff)
        valid_declines <- declines_for_geom_mean[!is.na(declines_for_geom_mean) & declines_for_geom_mean > 0]
        
        period_decline_val <- if (length(valid_declines) > 0) {
          if (length(months_in_period) == 1 && period_name == "Month 2") {
            valid_declines[1]
          } else if (length(valid_declines) == length(months_in_period)) {
            geometric_mean(valid_declines, na.rm = TRUE)
          } else {
            NA_real_
          }
        } else {
          NA_real_
        }
        decline_output_formatted_lines[[length(decline_output_formatted_lines) + 1]] <-
          paste0(period_name, "\t", ifelse(is.na(period_decline_val), "N/A", paste0(format(round(period_decline_val * 100, 2), nsmall = 2), "%")))
      }
      decline_output_formatted_lines[[length(decline_output_formatted_lines) + 1]] <- "" # Cell break for Excel
      
      yearly_decline_periods <- list(
        "Year 4" = c(36, 48),
        "Year 5" = c(48, 60)
      )
      for(period_name in names(yearly_decline_periods)){
        m_start <- yearly_decline_periods[[period_name]][1]
        m_end   <- yearly_decline_periods[[period_name]][2]
        
        rate_at_period_start <- get_rate_at_end_of_month(m_start)
        rate_at_period_end   <- get_rate_at_end_of_month(m_end)
        
        period_decline_val <- if (rate_at_period_start > 1e-6) {
          (rate_at_period_start - rate_at_period_end) / rate_at_period_start
        } else { NA_real_ }
        decline_output_formatted_lines[[length(decline_output_formatted_lines) + 1]] <-
          paste0(period_name, "\t", ifelse(is.na(period_decline_val), "N/A", paste0(format(round(period_decline_val * 100, 2), nsmall = 2), "%")))
      }
      
      annual_declines_yr6_10 <- numeric()
      for (year_num in 6:10) {
        m_start_of_year <- (year_num - 1) * 12
        m_end_of_year   <- year_num * 12
        
        rate_at_year_start <- get_rate_at_end_of_month(m_start_of_year)
        rate_at_year_end   <- get_rate_at_end_of_month(m_end_of_year)
        
        annual_decline <- if (rate_at_year_start > 1e-6) {
          (rate_at_year_start - rate_at_year_end) / rate_at_year_start
        } else { NA_real_ }
        annual_declines_yr6_10 <- c(annual_declines_yr6_10, annual_decline)
      }
      valid_annual_declines_yr6_10 <- annual_declines_yr6_10[!is.na(annual_declines_yr6_10) & annual_declines_yr6_10 > 0]
      geom_mean_yr6_10 <- if (length(valid_annual_declines_yr6_10) > 0) {
        geometric_mean(valid_annual_declines_yr6_10, na.rm = TRUE)
      } else { NA_real_ }
      decline_output_formatted_lines[[length(decline_output_formatted_lines) + 1]] <-
        paste0("Year 6-10", "\t", ifelse(is.na(geom_mean_yr6_10), "N/A", paste0(format(round(geom_mean_yr6_10 * 100, 2), nsmall = 2), "%")))
      
      # Year 11+ text based on user request
      year_11_plus_decline_text <- if (input$arps_product_type == "Oil") {
        "Terminal Decline (Dmin): 10.00% (Annual Eff.)"
      } else { # Gas
        "Terminal Decline (Dmin): 8.00% (Annual Eff.)"
      }
      decline_output_formatted_lines[[length(decline_output_formatted_lines) + 1]] <- paste0("Year 11+", "\t", year_11_plus_decline_text)
      
    } else {
      decline_output_formatted_lines[[length(decline_output_formatted_lines) + 1]] <- "Decline Rates: Not calculated (fitting or monthly declines failed)."
    }
    
    main_params_output <- paste(names(params_text_list), params_text_list, sep = ": ", collapse = "\n")
    main_params_output <- gsub("Fitted Model: Model:", "Fitted Model:", main_params_output, fixed=TRUE)
    
    final_output_string <- paste0(
      main_params_output,
      "\n\n--- Decline Rate Percentages ---\n",
      paste(decline_output_formatted_lines, collapse = "\n")
    )
    
    return(final_output_string)
  })
  
  output$arps_data_table <- DT::renderDataTable({
    analysis_results <- type_curve_analysis_data()
    req(analysis_results, analysis_results$data)
    dt_data <- copy(analysis_results$data)
    if("AvgDailyRate" %in% names(dt_data)) setnames(dt_data, "AvgDailyRate", "Avg.Daily.Rate")
    
    if(!is.null(analysis_results$fit)){
      tryCatch({
        dt_data$PredictedDailyRate <- predict(analysis_results$fit, newdata=dt_data)
      }, error = function(e) {
        message(paste("Error during predict for Arps table:", e$message))
        dt_data$PredictedDailyRate <- NA_real_
      })
    }
    numeric_cols <- names(dt_data)[sapply(dt_data, is.numeric)]
    for(col in numeric_cols) {
      if (col %in% c("Avg.Daily.Rate", "PredictedDailyRate")) {
        dt_data[, (col) := round(get(col), 1)]
      } else if (col != "WellCount") {
        if (col %in% names(dt_data)) {
          dt_data[, (col) := signif(get(col), 4)]
        }
      }
    }
    if("MonthOnProd" %in% names(dt_data)) setnames(dt_data, "MonthOnProd", "MonthsSincePeak")
    
    display_cols <- c("MonthsSincePeak", "Avg.Daily.Rate", "WellCount")
    if("PredictedDailyRate" %in% names(dt_data)) display_cols <- c(display_cols, "PredictedDailyRate")
    
    display_cols_exist <- display_cols[display_cols %in% names(dt_data)]
    
    DT::datatable(
      dt_data[, ..display_cols_exist],
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        rowCallback = dt_rowCallback_js
      ),
      rownames = FALSE,
      caption = paste("Aggregated Data for Arps Type Curve (Daily Rates -", ifelse(input$arps_product_type=="Oil","BBL/day","MCF/day"), ")")
    )
  })
  
} # End of server function

# --- 6. Run the Application ---
shinyApp(ui = ui, server = server)

