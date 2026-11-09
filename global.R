# ========================================================================
# CACHE PERSISTENCE
# ========================================================================
cache_dir <- "cache/"
dir.create(cache_dir, showWarnings = FALSE)

# Initialize global cache environments
.gazetteer_cache_env <- new.env(parent = emptyenv())
.gazetteer_hops_cache_env <- new.env(parent = emptyenv())
.gazetteer_blacklist_env <- new.env(parent = emptyenv())
.wm_record_cache_env <- new.env(parent = emptyenv())
.wm_classification_cache_env <- new.env(parent = emptyenv())

# Validate join key columns exist and have matching types before any join
check_join_cols <- function(df1, df2, by, df1_name = "df1", df2_name = "df2") {
  for (key in by) {
    if (!key %in% names(df1)) stop(sprintf("Join key '%s' not found in %s", key, df1_name))
    if (!key %in% names(df2)) stop(sprintf("Join key '%s' not found in %s", key, df2_name))
    t1 <- typeof(df1[[key]])
    t2 <- typeof(df2[[key]])
    if (t1 != t2) {
      warning(sprintf("Join key '%s' type mismatch: %s is %s, %s is %s", key, df1_name, t1, df2_name, t2))
    }
  }
  invisible(TRUE)
}

# Cached wrapper around wm_record to avoid duplicate API calls for the same AphiaID
cached_wm_record <- function(aphia_id) {
  key <- as.character(aphia_id)
  if (exists(key, envir = .wm_record_cache_env)) {
    return(.wm_record_cache_env[[key]])
  }
  rec <- tryCatch(wm_record(aphia_id), error = function(e) NULL)
  assign(key, rec, envir = .wm_record_cache_env)
  rec
}

cached_wm_classification <- function(aphia_id) {
  key <- as.character(aphia_id)
  if (exists(key, envir = .wm_classification_cache_env)) {
    return(.wm_classification_cache_env[[key]])
  }
  cls <- tryCatch(wm_classification(aphia_id), error = function(e) NULL)
  assign(key, cls, envir = .wm_classification_cache_env)
  cls
}

save_classification_cache <- function() {
  saveRDS(as.list(.wm_classification_cache_env), file = file.path(cache_dir, "wm_classification_cache.rds"))
}

save_wm_record_cache <- function() {
  saveRDS(as.list(.wm_record_cache_env), file = file.path(cache_dir, "wm_record_cache.rds"))
}

save_gazetteer_caches <- function() {
  message("Saving gazetteer caches to disk...")
  saveRDS(as.list(.gazetteer_cache_env), file = file.path(cache_dir, "gazetteer_cache.rds"))
  saveRDS(as.list(.gazetteer_hops_cache_env), file = file.path(cache_dir, "gazetteer_hops_cache.rds"))
  saveRDS(as.list(.gazetteer_blacklist_env), file = file.path(cache_dir, "gazetteer_blacklist.rds"))
  saveRDS(as.list(.wm_record_cache_env), file = file.path(cache_dir, "wm_record_cache.rds"))
  saveRDS(as.list(.wm_classification_cache_env), file = file.path(cache_dir, "wm_classification_cache.rds"))
  message("Saving WoRMS caches to disk...")
  # Round 8 (2026-09-09): the OBIS/GBIF presence cache (.evidence_cache_env,
  # defined further down in this file once it exists) used to be in-memory
  # only, unlike every other cache here -- meaning OBIS/GBIF presence
  # results were re-fetched from scratch on every fresh R session even
  # though the WoRMS/gazetteer caches persisted. Saved here too now, at the
  # same onStop() point the others already use (see app.R).
  if (exists(".evidence_cache_env")) {
    saveRDS(as.list(.evidence_cache_env), file = file.path(cache_dir, "evidence_cache.rds"))
    message("Saving OBIS/GBIF evidence cache to disk...")
  }
}

# ------------------------------------------------------------------------
# CHECKPOINTING — Save and load partial results for resilience.
# ------------------------------------------------------------------------
ensure_results_dir <- function() {
  if (!dir.exists("results")) dir.create("results")
}

generate_consistent_filename <- function(project_id, module_id, data_type, extension = "csv") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  sprintf("%s_%s_%s_%s.%s", project_id, module_id, data_type, timestamp, extension)
}

# ------------------------------------------------------------------------
# MODULE RUN LOGS (2026-09-11): one small log file per module run, written
# alongside every module's own CSV/table exports, so a record of what went
# into that module and what came out survives outside the live Shiny
# session (a spreadsheet review, an email attachment, an audit trail) --
# not just the numbers shown in the app's own on-screen Summary tab, which
# disappear once the session ends. Uses the same generate_consistent_filename()
# naming convention as every other export in the app, so log files sort and
# group next to the data exports they describe. Written as BOTH a
# human-readable .txt (for reading or pasting into a report) and a .csv
# (for loading many runs' logs into Excel/R to track metrics over time).
# Each module writes its OWN log file when its own run/analysis completes
# (not one cumulative log per project), since modules can be run
# independently, out of order, or re-run without re-running the whole
# pipeline.
# ------------------------------------------------------------------------
logs_dir <- "logs/"
dir.create(logs_dir, showWarnings = FALSE)

ensure_logs_dir <- function() {
  if (!dir.exists(logs_dir)) dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)
}

#' Write one module's run summary to disk as both .txt and .csv.
#'
#' @param project_id   shared$project_name (or "Project" if not yet set)
#' @param module_id    short module tag matching the app's own convention
#'                     (e.g. "modA".."modG", as used elsewhere by
#'                     generate_consistent_filename())
#' @param module_title human-readable heading for the .txt file, e.g.
#'                      "Module A -- Taxonomic Standardization"
#' @param summary      a named list of metric -> value, IN THE ORDER they
#'                      should appear (not alphabetized) -- e.g.
#'                      list("ASVs in" = 120, "ASVs out" = 98, ...). A
#'                      value that is a vector (e.g. a list of excluded
#'                      column names) is collapsed to one string.
#' @param notes        optional character vector of free-text notes (e.g.
#'                      a module's own accumulated rv$log) appended after
#'                      the metrics in the .txt file only.
#' @return list(txt = <path written>, csv = <path written>)
write_module_log <- function(project_id, module_id, module_title, summary, notes = character()) {
  ensure_logs_dir()
  project_id <- project_id %||% "Project"

  txt_path <- file.path(logs_dir, generate_consistent_filename(project_id, module_id, "log", "txt"))
  csv_path <- file.path(logs_dir, generate_consistent_filename(project_id, module_id, "log", "csv"))

  fmt_value <- function(v) {
    if (is.null(v) || length(v) == 0 || all(is.na(v))) return("(none)")
    if (is.numeric(v) && length(v) == 1) return(format(v, big.mark = ",", scientific = FALSE))
    paste(v, collapse = ", ")
  }

  lines <- c(
    sprintf("=== %s ===", module_title),
    sprintf("Project: %s", project_id),
    sprintf("Timestamp: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    ""
  )
  for (nm in names(summary)) {
    lines <- c(lines, sprintf("%-32s: %s", nm, fmt_value(summary[[nm]])))
  }
  if (length(notes) > 0) {
    lines <- c(lines, "", "--- Notes ---", notes)
  }
  writeLines(lines, txt_path)

  csv_df <- data.frame(
    Metric = names(summary),
    Value = vapply(summary, function(v) {
      if (is.null(v) || length(v) == 0 || all(is.na(v))) return(NA_character_)
      paste(v, collapse = "; ")
    }, character(1)),
    stringsAsFactors = FALSE
  )
  write_csv(csv_df, csv_path)

  list(txt = txt_path, csv = csv_path)
}

save_checkpoint <- function(data, module_id, step_name, project_id) {
  ensure_results_dir()
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  # File format: checkpoint_<project>_<module>_<step>_<timestamp>.rds
  filename <- sprintf("results/checkpoint_%s_%s_%s_%s.rds", project_id, module_id, step_name, timestamp)
  saveRDS(data, filename)
  message(sprintf("Checkpoint saved to %s", filename))
}
if (!dir.exists(cache_dir)) dir.create(cache_dir, showWarnings = FALSE)

load_gazetteer_caches <- function() {
  # Load into existing environments
  if (file.exists(file.path(cache_dir, "gazetteer_cache.rds"))) {
    data <- readRDS(file.path(cache_dir, "gazetteer_cache.rds"))
    list2env(data, envir = .gazetteer_cache_env)
  }
  if (file.exists(file.path(cache_dir, "gazetteer_hops_cache.rds"))) {
    data <- readRDS(file.path(cache_dir, "gazetteer_hops_cache.rds"))
    list2env(data, envir = .gazetteer_hops_cache_env)
  }
  if (file.exists(file.path(cache_dir, "gazetteer_blacklist.rds"))) {
    data <- readRDS(file.path(cache_dir, "gazetteer_blacklist.rds"))
    list2env(data, envir = .gazetteer_blacklist_env)
  }
  if (file.exists(file.path(cache_dir, "wm_record_cache.rds"))) {
    data <- readRDS(file.path(cache_dir, "wm_record_cache.rds"))
    list2env(data, envir = .wm_record_cache_env)
  }
  if (file.exists(file.path(cache_dir, "wm_classification_cache.rds"))) {
    data <- readRDS(file.path(cache_dir, "wm_classification_cache.rds"))
    list2env(data, envir = .wm_classification_cache_env)
  }
  message("Gazetteer caches loaded.")
}

load_gazetteer_caches()

suppressWarnings(suppressMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(ggplot2)
  library(DT)
  library(worrms)
  library(future)
  library(promises)
  library(shinyjs)
  library(httr2)    # Marine Regions gazetteer REST client (relations-based occurrence classification)
  library(jsonlite) # parsing gazetteer JSON responses
  library(parallel) # parallel execution
  library(data.table)
}))

# decontam / microDecon / phyloseq are heavier Bioconductor-style deps.
# We load them defensively so the app still launches if they're missing,
# and only fail (with a clear message) when Module D is actually used.
.pkg_available <- function(pkg) requireNamespace(pkg, quietly = TRUE)
# Helper to get parent AphiaID if the current one is a subspecies
get_parent_aphia_id <- function(aphia_id) {
  if (is.na(aphia_id) || aphia_id == 0) return(aphia_id)
  
  # Try to get the record
  rec <- cached_wm_record(aphia_id)
  if (is.null(rec) || is.null(rec$rank) || rec$rank != "Subspecies") {
    return(aphia_id)
  }
  
  # If it is a subspecies, return the parent ID if it exists and is valid
  if (!is.null(rec$parentNameUsageID) && !is.na(rec$parentNameUsageID) && rec$parentNameUsageID != aphia_id) {
    return(rec$parentNameUsageID)
  }
  
  return(aphia_id)
}

# Round 59: scientific name of a subspecies' parent species (via WoRMS'
# parentNameUsageID, see get_parent_aphia_id()), or NULL when `aphia_id` is not
# a subspecies / has no parent on record. Used so a subspecies with no
# OBIS/GBIF records of its own can borrow its parent species' presence --
# only ever as an extra candidate name tried AFTER the subspecies' own name
# and synonyms came back absent.
get_parent_species_name <- function(aphia_id) {
  if (is.na(aphia_id) || aphia_id == 0) return(NULL)
  pid <- tryCatch(get_parent_aphia_id(aphia_id), error = function(e) aphia_id)
  if (is.na(pid) || identical(as.integer(pid), as.integer(aphia_id))) return(NULL)
  prec <- cached_wm_record(pid)
  if (is.null(prec) || NROW(prec) < 1 || is.null(prec$scientificname) || is.na(prec$scientificname[1])) return(NULL)
  as.character(prec$scientificname[1])
}

# ------------------------------------------------------------------------
# WoRMS synonym cache (2026-09-11)
#
# WoRMS is the app's own taxonomic authority and stays internally
# consistent the moment a species is reclassified (build_species_fallback()
# always re-queries WoRMS for the current, valid AphiaID/name). But every
# EXTERNAL data source this app cross-references -- OBIS, GBIF, and the
# bundled local MarineSpeciesTraits DB (trait_lookup_env) -- is its own,
# separately-indexed dataset that can lag behind a very recent WoRMS
# reclassification, sometimes by months. Confirmed live for AphiaID
# 1872991 (Maea johnstoni, reclassified from Magelona johnstoni in WoRMS on
# 2026-03-09; NOTE, live check 2026-10-05: WoRMS has since reverted -- Magelona
# johnstoni/130269 is accepted again and Maea johnstoni/1872991 is a
# "superseded subsequent combination" with valid_AphiaID 130269, so the
# mechanism described here works in either direction and the code never
# assumes which of the two names is current):
# querying OBIS/GBIF by the then-current name/AphiaID returns ZERO
# records from either, while the superseded combination "Magelona
# johnstoni" / AphiaID 130269 returns 11,060 OBIS records (9,635 within a
# North Sea bounding box) and 10,231 GBIF records for the exact same real
# animal. A name-only or current-AphiaID-only external lookup is a
# systematic FALSE NEGATIVE for every recently-reclassified species, not
# just this one -- silently reporting "not observed" / "no trait data" for
# something that is both observed and characterized, just under a name WoRMS
# itself has moved on from. fetch_worms_synonyms() is the single shared
# fallback: every external, name-or-AphiaID-keyed lookup in this app should
# also try the taxon's known synonym names/AphiaIDs before concluding
# "absent," specifically because a false negative here is worse than the
# extra API calls -- an eDNA species record that looks unobserved/untraited
# because of a taxonomic bookkeeping lag, not because the evidence is
# genuinely missing, quietly corrupts a scientific dataset.
# ------------------------------------------------------------------------
worms_synonym_cache_file <- "cache/worms_synonym_cache.rds"
worms_synonym_cache_env <- new.env(parent = emptyenv())

load_worms_synonym_cache <- function() {
  if (file.exists(worms_synonym_cache_file)) {
    data <- tryCatch(readRDS(worms_synonym_cache_file), error = function(e) NULL)
    if (!is.null(data)) {
      list2env(data, envir = worms_synonym_cache_env)
      message(sprintf("WoRMS synonym cache loaded (%d entries).", length(data)))
    }
  }
}
save_worms_synonym_cache <- function() {
  dir.create(dirname(worms_synonym_cache_file), showWarnings = FALSE)
  saveRDS(as.list(worms_synonym_cache_env), file = worms_synonym_cache_file)
  message(sprintf("WoRMS synonym cache saved (%d entries).", length(as.list(worms_synonym_cache_env))))
}

# Every alternative scientific name (and AphiaID) WoRMS has ever attached to
# this taxon concept -- the older name(s) merged into a current accepted
# AphiaID (the common case here: aphia_id is already the current, valid id,
# and wm_synonyms() returns retired combinations like "Magelona johnstoni"),
# or, if aphia_id itself turns out to be an unaccepted/synonym id, WoRMS's
# own accepted name for it (added explicitly, since wm_synonyms() lists
# *other* synonyms, not necessarily the accepted name itself). Returns
# list(names = character(0+), aphia_ids = integer(0+)), each de-duplicated
# and excluding aphia_id's own current name/id -- callers already have
# those. Never errors: any WoRMS API failure degrades to "no synonyms
# found" (an empty list) rather than blocking the caller, matching every
# other cached_wm_*() wrapper in this file.
fetch_worms_synonyms <- function(aphia_id) {
  empty <- list(names = character(0), aphia_ids = integer(0))
  if (is.na(aphia_id) || aphia_id == 0) return(empty)
  key <- as.character(aphia_id)
  if (exists(key, envir = worms_synonym_cache_env)) {
    return(worms_synonym_cache_env[[key]])
  }

  out_names <- character(0)
  out_ids <- integer(0)

  syns <- tryCatch(wm_synonyms(as.integer(aphia_id)), error = function(e) NULL)
  if (!is.null(syns) && nrow(syns) > 0 && "scientificname" %in% names(syns)) {
    out_names <- c(out_names, as.character(syns$scientificname))
    if ("AphiaID" %in% names(syns)) out_ids <- c(out_ids, suppressWarnings(as.integer(syns$AphiaID)))
  }

  # If aphia_id itself isn't the accepted one, make sure the accepted
  # name/id is offered as an alternative too -- wm_synonyms() lists other
  # synonyms of the accepted taxon, not necessarily the accepted name
  # itself, and this app should never treat a non-accepted starting id as
  # the only name worth trying.
  rec <- cached_wm_record(aphia_id)
  if (!is.null(rec) && !is.null(rec$valid_AphiaID) && !is.na(rec$valid_AphiaID) &&
      rec$valid_AphiaID != aphia_id) {
    if (!is.null(rec$valid_name) && !is.na(rec$valid_name)) out_names <- c(out_names, rec$valid_name)
    out_ids <- c(out_ids, as.integer(rec$valid_AphiaID))
  }

  own_name <- if (!is.null(rec) && !is.null(rec$scientificname)) rec$scientificname else NA_character_
  out_names <- unique(na.omit(out_names))
  out_names <- out_names[is.na(own_name) | out_names != own_name]
  out_ids <- unique(na.omit(out_ids))
  out_ids <- out_ids[out_ids != aphia_id]

  result <- list(names = out_names, aphia_ids = out_ids)
  worms_synonym_cache_env[[key]] <- result
  result
}

load_worms_synonym_cache()

APP_VERSION <- "v1.0.0"
options(shiny.maxRequestSize = 300 * 1024^2)  # 300 MB upload limit

tax_levels <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

# Generic API reachability check — returns TRUE if the endpoint responds 200
# within `timeout_s` seconds. Used to abort analyses when a required API is down.
check_api_reachable <- function(url, timeout_s = 5) {
  tryCatch({
    resp <- request(url) %>% req_timeout(timeout_s) %>% req_perform()
    resp_status(resp) == 200
  }, error = function(e) FALSE)
}

# Named list of (label, test_url) pairs for the APIs this pipeline depends on.
# The test URLs are lightweight endpoints chosen to return quickly.
REQUIRED_APIS <- list(
  WoRMS = list(
    label = "WoRMS",
    url   = "https://www.marinespecies.org/rest/AphiaIDByName/Abra%20alba"
  ),
  MarineRegions = list(
    label = "Marine Regions",
    url   = "https://www.marineregions.org/rest/getGazetteerRecordByMRGID.json/2350/"
  ),
  OBIS = list(
    label = "OBIS",
    url   = "https://api.obis.org/occurrence?scientificname=Gadus%20morhua&size=1"
  ),
  GBIF = list(
    label = "GBIF",
    url   = "https://api.gbif.org/v1/occurrence/search?scientificName=Gadus%20morhua&limit=1"
  )
)

# Many raw input files already contain IDs like "ASV_1", "ASV_42", etc.
# Several parsers below need to guarantee an "ASV_"-prefixed ID for
# consistent joining across modules — but blindly prepending "ASV_" on data
# that already has it produces "ASV_ASV_1". This guards against that.
ensure_asv_prefix <- function(x) {
  x <- as.character(x)
  ifelse(grepl("^ASV_", x), x, paste0("ASV_", x))
}

# Apply proxy on startup from environment (auto-detection)
  auto_detect_proxy <- function() {
    p <- Sys.getenv("https_proxy")
    if (!nzchar(p)) p <- Sys.getenv("http_proxy")
    if (!nzchar(p)) p <- Sys.getenv("ALL_PROXY")
    
    if (nzchar(p)) {
      message(sprintf("Auto-detected proxy from environment: %s", p))
      Sys.setenv(http_proxy = p, https_proxy = p)
      Sys.setenv(HTTP_PROXY = p, HTTPS_PROXY = p)
      return(p)
    }
    message("No proxy detected in environment — direct connection.")
    NULL
  }
  
  auto_detect_proxy()

app_theme <- bs_theme(
  version = 5,
  bg = "#F5F5F0",
  fg = "#222140",
  primary = "#38a3a9",
  secondary = "#6B6358",
  success = "#38a3a9",
  warning = "#B5752C",
  danger = "#A4423A",
  base_font = font_google("Nunito"),
  heading_font = font_google("Nunito", wght = c(700, 800)),
  code_font = font_google("IBM Plex Mono"),
  "font-size-base" = "0.95rem",
  "border-radius" = "0.4rem"
)

# ------------------------------------------------------------------------
# COLORBLIND-SAFE PALETTE — used by every plot in the app.
#
# This is the Okabe-Ito palette (Okabe & Ito, 2008), the standard
# categorical palette for scientific figures because it remains
# distinguishable under all common forms of color vision deficiency
# (deuteranopia, protanopia, tritanopia), not just "looks fine to me."
# Reference: https://jfly.uni-koeln.de/color/
#
# Named slots below map specific *meanings* (agreement level, fallback
# source, occurrence likelihood) to specific colors, so the same concept
# always gets the same color across every plot in the app — e.g. "all
# three databases agree" is always the same green, everywhere.
# ------------------------------------------------------------------------

okabe_ito <- c(
  orange      = "#E69F00",
  sky_blue    = "#56B4E9",
  green       = "#009E73",
  yellow      = "#F0E442",
  blue        = "#0072B2",
  vermillion  = "#D55E00",
  purple      = "#CC79A7",
  black       = "#000000"
)

# Semantic palette for the agreement plot (4 categories, ordered best->worst;
# "Only one database" = a single database made a call, nothing to compare against)
agreement_colors <- c(
  "All agree"         = okabe_ito[["green"]],
  "Two agree"         = okabe_ito[["sky_blue"]],
  "None agree"        = okabe_ito[["vermillion"]],
  "Only one database" = "#999999"
)

# Semantic palette for fallback source (3 possible sources + "None")
fallback_colors <- c(
  "BLAST" = okabe_ito[["blue"]],
  "BOLD"  = okabe_ito[["green"]],
  "MZG"   = okabe_ito[["orange"]],
  "None"  = okabe_ito[["vermillion"]]
)

fallback_color_for <- function(source_labels) {
  cols <- sapply(source_labels, function(s) {
    if (is.na(s)) return(fallback_colors[["None"]])
    if (s == "BLAST") return(fallback_colors[["BLAST"]])
    if (s == "BOLD")  return(fallback_colors[["BOLD"]])
    if (s == "MZG")   return(fallback_colors[["MZG"]])
    fallback_colors[["None"]]
  })
  setNames(cols, source_labels)
}

# Semantic palette for occurrence classification (5 ordered levels,
# likely -> unlikely) — a colorblind-safe sequential ramp via viridis
# rather than another qualitative palette, since this is ordered data.
occurrence_levels_ordered <- c("Very likely", "Likely", "Possible", "Unlikely", "Very unlikely", "Unknown")

# ------------------------------------------------------------------------
# 1. INPUT PARSERS  (from Module A scripts; setwd()/print-cats removed,
#    file paths now passed explicitly, no global CONFIG)
# ------------------------------------------------------------------------

read_BLAST <- function(file) {
  df <- read.delim(file, header = FALSE, stringsAsFactors = FALSE, quote = "")

  colnames(df) <- c(
    "ASV_id", "Accession", "E_value", "Percent_id", "Ref_len", "Query_len",
    "Taxon_id_1", "Taxon_id_2", "Taxon_id_3", "Taxon_id_4",
    "Domain", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"
  )

  df %>%
    arrange(ASV_id, E_value, desc(Percent_id)) %>%
    group_by(ASV_id) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(ASV_id = ensure_asv_prefix(ASV_id)) %>%
    select(ASV_id, Kingdom, Phylum, Class, Order, Family, Genus, Species,
           Percent_id, Query_len, E_value)
}

parse_bold_taxonomy <- function(tax_string) {
  if (is.na(tax_string) || tax_string == "") {
    return(as.data.frame(setNames(as.list(rep(NA, 7)), tax_levels), stringsAsFactors = FALSE))
  }

  first_assignment <- str_split(tax_string, "\\s*\\+\\s*")[[1]][1]
  taxa <- str_extract_all(first_assignment, "[kpcofgs]:[^(]+")[[1]]

  tax_list <- setNames(as.list(rep(NA, 7)), tax_levels)

  for (taxon in taxa) {
    level <- str_extract(taxon, "^[kpcofgs]")
    name <- str_trim(sub("^[kpcofgs]:", "", taxon))

    lvl_name <- dplyr::case_when(
      level == "k" ~ "Kingdom",
      level == "p" ~ "Phylum",
      level == "c" ~ "Class",
      level == "o" ~ "Order",
      level == "f" ~ "Family",
      level == "g" ~ "Genus",
      level == "s" ~ "Species"
    )

    if (!is.na(lvl_name)) tax_list[[lvl_name]] <- name
  }

  as.data.frame(tax_list, stringsAsFactors = FALSE)
}

read_BOLD <- function(file) {
  raw <- read.delim(file, header = FALSE, sep = "\t", stringsAsFactors = FALSE)

  raw %>%
    rename(ASV_id = V1, taxonomy = V2) %>%
    rowwise() %>%
    do(data.frame(ASV_id = .$ASV_id,
                   parse_bold_taxonomy(.$taxonomy),
                   stringsAsFactors = FALSE)) %>%
    ungroup() %>%
    mutate(ASV_id = ensure_asv_prefix(ASV_id))
}

read_MZG <- function(file, log = message) {
  # 1. Read as character to avoid parsing issues, but be careful with the wide data
  df <- read_csv(file, show_col_types = FALSE, col_types = cols(.default = "c"))
  
  # 2. Identify Taxonomy Columns (last 7) - captured by NAME before any
  # column is added/removed below, so this stays correct regardless of what
  # assign_mzg_asv_id() does to the column count.
  tax_start <- ncol(df) - 6
  tax_cols <- names(df)[tax_start:ncol(df)]

  # 3. FIXED: derive ASV_id using the SAME shared function Module C's merge
  # step calls (assign_mzg_asv_id()) on this same source file, instead of
  # this separate hardcoded row-order line. Previously the two read paths
  # each invented their own row-order ASV_id independently; if the two
  # parsers (readr here, base read.delim/read_delim in Module C) ever parsed
  # the file with a different row count or order, the join key silently
  # desynced with no error. Calling the one shared function from both places
  # actually guarantees they agree, rather than merely happening to agree
  # today because both used the same formula.
  id_result <- assign_mzg_asv_id(df)
  df <- id_result$df
  log(id_result$diagnostic)

  # 5. Compute fragment length from sequence column (if present)
  sequence_col <- grep("^(sequence|seq|nucleotides?|nuc|barcode|rep_seq|asv_sequence|otu_sequence)$",
                        names(df), ignore.case = TRUE, value = TRUE)
  if (length(sequence_col) > 0) {
    seq_name <- sequence_col[1]
    df$Sequence_length_bp <- nchar(df[[seq_name]], type = "chars")
    log(sprintf("Computed Sequence_length_bp from column '%s'.", seq_name))
  }
  
  # 6. Extract taxonomy columns
  result_df <- df %>% select(ASV_id, any_of("Sequence_length_bp"), all_of(tax_cols))
  
  # Rename columns to match expected: Kingdom, Phylum, Class, Order, Family, Genus, Species
  # Your file ends in: Kingdom,Phylum,Class,Order,Family,Genus,Species
  colnames(result_df)[(ncol(result_df) - 6):ncol(result_df)] <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  
  log("Parsed MZG file using sequence as backbone.")
  result_df
}

# Both read_MZG() (used in Module A) and read_mzg_auto() (used in Module C
# for the sample-count backbone) read the *same underlying file* but via
# different parsers (readr vs base read.delim). If each one independently
# invents ASV_id from row_number(), the two sequences are only guaranteed
# to line up if both reads parse the exact same number of rows in the exact
# same order — any divergence (trailing blank rows, NA handling, etc.)
# silently desyncs the join key downstream with no error, which is what
# was causing empty MZG_*_AphiaID columns after the Module B agreement join.
#
# Fix: derive ASV_id from the file's own first column whenever it looks like
# a stable per-row identifier (unnamed index column, or an explicit ID/ASV
# column — numeric OR text, either is fine as long as it's stable), so both
# read paths key off the same values instead of two independently-generated
# row-number sequences. Returns both the data and a one-line diagnostic so
# the calling module can show the user exactly which path was taken.
assign_mzg_asv_id <- function(df) {
  # FIXED: this function's own comment above (and the CHANGELOG) documented a
  # fix - prefer a real, stable per-row identifier column when one exists,
  # falling back to row order only as a last resort - that the code below it
  # never actually implemented: it unconditionally forced row-order IDs and
  # DELETED any existing ID column, the opposite of the documented behavior,
  # while always reporting the same "forced row order" diagnostic regardless
  # of what happened. That left the original bug (two independently-invented
  # row-order sequences from two different parsers silently desyncing the
  # join key) live. Now actually implemented: use an existing ID-like column
  # only when it's complete (no NAs) and unique (no duplicates) - otherwise
  # it isn't a safe join key either, so row order remains the correct
  # fallback.
  id_col_idx <- which(grepl("(?i)^(asv_id|asv|otu_id|otu|id)$", names(df)))
  used_existing <- FALSE
  diag <- NULL

  if (length(id_col_idx) > 0) {
    src_name <- names(df)[id_col_idx[1]]
    candidate <- df[[id_col_idx[1]]]
    if (length(candidate) == nrow(df) && !anyNA(candidate) && !any(duplicated(candidate))) {
      # FIXED: an existing ID column (e.g. a bare "ASV" column with values
      # 1, 2, 3...) was used verbatim, with no "ASV_" prefix — while
      # BLAST/BOLD's own ASV_id values (and this same function's row-order
      # fallback below) always carry that prefix. Every MZG row then failed
      # to join against BLAST/BOLD in Module B (mismatched join key), making
      # every MZG-backed ASV look like it had zero cross-database data at
      # all, not just zero agreement. ensure_asv_prefix() normalizes this
      # without double-prefixing a column that already has it.
      candidate <- ensure_asv_prefix(as.character(candidate))
      if (src_name != "ASV_id") df[[src_name]] <- NULL
      df$ASV_id <- candidate
      used_existing <- TRUE
      diag <- sprintf(
        "Derived ASV_id from existing column '%s' (%d unique, non-missing values) instead of row order.",
        src_name, length(candidate)
      )
    }
  }

  if (!used_existing) {
    # No usable existing identifier (missing values, duplicates, or no
    # ID-like column at all) - row order is the correct last resort here.
    if (length(id_col_idx) > 0 && names(df)[id_col_idx[1]] != "ASV_id") {
      df[[id_col_idx[1]]] <- NULL
    }
    df <- df %>% mutate(ASV_id = paste0("ASV_", seq_len(nrow(.))))
    diag <- "No usable existing ID column found (missing values, duplicates, or none present) - forced ASV_id to strict chronological row order (ASV_1, ASV_2, ...)."
  }

  list(df = df %>% relocate(ASV_id, .before = 1), diagnostic = diag)
}

# MZG file used as the ASV/sample-count BACKBONE in Module C — it may be
# tab, comma, or semicolon delimited, and may have raw read-count columns
# in addition to taxonomy.
read_mzg_auto <- function(filepath, log = message) {
  first_lines <- readLines(filepath, n = 2, warn = FALSE)
  header_line <- first_lines[1]

  n_tabs <- length(gregexpr("\t", header_line)[[1]])
  n_commas <- length(gregexpr(",", header_line)[[1]])
  n_semicolons <- length(gregexpr(";", header_line)[[1]])

  if (n_tabs > n_commas && n_tabs > n_semicolons && n_tabs > 5) {
    sep <- "\t"; sep_name <- "tab"
  } else if (n_commas > n_tabs && n_commas > n_semicolons && n_commas > 5) {
    sep <- ","; sep_name <- "comma"
  } else if (n_semicolons > n_tabs && n_semicolons > n_commas && n_semicolons > 5) {
    sep <- ";"; sep_name <- "semicolon"
  } else {
    sep <- "\t"; sep_name <- "tab (default)"
  }
  log(sprintf("Detected %s-delimited file", sep_name))

  # FIXED (2026-09-11): quote = "" disabled ALL quote handling, so a
  # genuinely double-quoted CSV (every field wrapped in "...", a common
  # RFC4180 export style) came through with literal embedded quote
  # characters in both column names ("Sequence" instead of Sequence) and
  # values ("ATTATC... instead of ATTATC...). That silently broke every
  # exact-name match downstream (taxonomy-column stripping, sequence-column
  # detection) for such a file, with no error -- the columns just failed to
  # be recognized and fell through as unexplained extra columns instead.
  # read_MZG() (Module A) never had this problem since it uses readr's
  # quote-aware read_csv(); quote = "\"" brings this reader in line with
  # that for the same file, while unquoted files (the common tab/semicolon
  # MZG export case) are unaffected either way.
  df <- read.delim(filepath, header = TRUE, stringsAsFactors = FALSE,
                    sep = sep, check.names = FALSE, quote = "\"", comment.char = "")

  empty_names <- which(names(df) == "" | is.na(names(df)))
  if (length(empty_names) > 0) {
    for (i in empty_names) names(df)[i] <- paste0("EMPTY_COL_", i)
  }

  if (any(duplicated(names(df)))) {
    names(df) <- make.unique(names(df), sep = "_")
  }

  if (ncol(df) < 5) {
    df_alt <- tryCatch(
      read_delim(filepath, delim = NULL, show_col_types = FALSE,
                 locale = locale(encoding = "UTF-8")),
      error = function(e) NULL
    )
    if (!is.null(df_alt) && ncol(df_alt) > ncol(df)) {
      df <- df_alt
      empty_names_alt <- which(names(df) == "" | is.na(names(df)))
      if (length(empty_names_alt) > 0) {
        for (i in empty_names_alt) names(df)[i] <- paste0("EMPTY_COL_", i)
      }
    }
  }

  df
}

# ------------------------------------------------------------------------
# 2. NAME CLEANING FUNCTIONS (used adaptively in WoRMS fallback lookup)
# ------------------------------------------------------------------------

clean_basic <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_trim() %>%
    str_squish() %>%
    str_remove("(?i)^uncultured\\s+") %>%
    str_remove("(?i)^unclassified\\s+") %>%
    str_remove("(?i)^unknown\\s+") %>%
    str_remove("(?i)^metagenome\\s+") %>%
    str_remove("(?i)^candidatus\\s+")
}

clean_suffix <- function(x) {
  x %>%
    str_remove("(?i) sp\\.?$") %>%
    str_remove("(?i) spp\\.?$") %>%
    str_remove("(?i) cf\\..*$") %>%
    str_remove("(?i) aff\\..*$") %>%
    str_remove("(?i) nr\\..*$")
}

clean_codes <- function(x) {
  x %>%
    str_remove("\\s+[A-Z0-9\\-\\.]+$") %>%
    str_remove("\\s+[0-9]+\\s*$")
}

to_genus <- function(x) word(x, 1)

# ------------------------------------------------------------------------
# 3. WoRMS TAXONOMY RESOLVER
#
# As of Round 53, each dataset's (BLAST/BOLD/MZG's) own row is resolved as a
# unit: resolve_row_anchored() anchors on that row's single most-specific
# resolvable name and fills in all 7 columns (Kingdom..Species) from that
# ONE matched WoRMS record's lineage, rather than each column running its
# own independent ladder-climb. This is implemented ON TOP OF
# resolve_one_column() below, unchanged -- resolve_one_column() still does
# the actual ladder-climb (own rank, then Species -> derived-genus, then
# this row's own higher-rank columns) and is still called directly, exactly
# as before, when `resolve_columns_batch(..., row_anchored = FALSE)` is
# used to roll back to the pre-Round-53 per-column-independent behavior.
#
# Per-column independence was the ORIGINAL design, deliberately preserved
# at the time so Module B's per-level agreement comparison (BLAST_Kingdom_
# AphiaID vs BOLD_Kingdom_AphiaID vs MZG_Kingdom_AphiaID, then separately
# the Species-level trio, etc.) stayed meaningful. Row-anchoring per dataset
# does NOT undo that: BLAST/BOLD/MZG still each anchor independently of one
# another (using their own raw input), so two datasets can still genuinely
# disagree at any rank if their own raw calls point to different organisms
# -- what changes is that WITHIN one dataset's own result, the 7 ranks can
# no longer land on 7 different, mutually inconsistent WoRMS records. See
# the CHANGELOG (Round 53) for why this is safe: with exactly 3 datasets and
# the existing >=2-of-3 agreement rule used elsewhere (build_species_
# fallback()), two ranks can only each get a winner from genuinely
# corroborating evidence, not a chimera -- as long as each dataset's own
# output is internally consistent, which row-anchoring now guarantees.
# Row-anchoring does NOT protect against multiple datasets independently,
# deterministically making the SAME wrong call for the same underlying
# reason (a correlated error, e.g. the Round 52c marine_only bug) -- that's
# a different failure mode, addressed by Round 52c's fixes, not this one.
#
# What resolve_one_column() itself adds, relative to the original
# lookup_one_taxon() it replaced:
#   1. CONFIDENCE TIERS instead of a bare AphiaID. Every column gets one of:
#        exact_species, fuzzy_species, exact_genus, fuzzy_genus,
#        higher_rank_fallback, unresolved
#      "exact/fuzzy_species" and "exact/fuzzy_genus" apply when the column
#      BEING RESOLVED is itself Species or Genus rank and matches at that
#      same rank. Any other column (Kingdom..Family) that resolves
#      successfully is tiered as higher_rank_fallback, since by definition
#      it's above genus.
#   2. FALLBACK CLIMBS TO KINGDOM. If a column's own string returns nothing
#      from WoRMS (after exact + fuzzy attempts, plus the existing
#      basic/suffix/code cleaning), the resolver climbs that SAME ROW's
#      higher-rank columns (e.g. if Family fails, try that row's Order,
#      then Class, ... up to Kingdom) rather than giving up — this is the
#      literal "keep fallback up to Kingdom" requirement.
#   3. HOMONYM CHECK. Some genus/family names are shared across unrelated
#      kingdoms (Morus = mulberry genus in Plantae AND a seabird genus in
#      Animalia; Prunella = a plant genus AND a bird genus). wm_records_name()
#      returns MULTIPLE candidates per query, each with full lineage. When
#      the row has its OWN Kingdom and/or Phylum strings already (even if
#      those are otherwise unreliable upstream classifier output), they're
#      used to prefer the WoRMS candidate whose kingdom/phylum agrees,
#      instead of blindly taking the first result. If no candidate agrees
#      (Round 52c), the match is REFUSED rather than kept anyway — the
#      ladder climbs to the next, coarser rung instead, same as an empty
#      WoRMS result. (Previously the first candidate was kept regardless,
#      flagged only via context_agreed = FALSE — a diagnostic that was
#      never enforced and was also dropped from Module F's final export,
#      so a context-disagreeing match could reach the user's output with no
#      visible trace, e.g. the Sus scrofa -> Ilyella chimera.) Round 54
#      narrows this: an EXACT match returning exactly ONE candidate AT
#      SPECIES RANK skips the context check entirely and is accepted
#      outright — a full species binomial essentially never collides across
#      kingdoms the way a bare genus/family word can, so there is no real
#      disambiguation to do, and trusting the row's OWN other (possibly
#      already-wrong, not just blank) columns to override a clean, exact,
#      unambiguous species-level identity can only make a correct result
#      worse. Fuzzy matches and anything coarser than Species still go
#      through the full check unchanged, so the real homonym protection
#      (Morus, Prunella) is untouched.
#   4. NEVER OVERWRITE WITH BLANK. A column only gets replaced by its
#      resolved value when WoRMS actually returned something for it; an
#      unresolved column keeps its original input string untouched.
#
# Each column's resolution is cached independently, keyed on the cleaned
# string + this row's Kingdom/Phylum context (so identical strings with
# identical context across BLAST/BOLD/MZG share one WoRMS query, but a
# homonym-prone string seen in different contexts is re-evaluated).
#
# NOTE: kept sequential (no furrr/future workers) inside Shiny — parallel
# *processes* don't play well with reactive progress reporting, and the
# dominant cost is network latency anyway, not CPU. worms_delay still
# throttles request rate to be polite to the API.
# ------------------------------------------------------------------------

confidence_levels <- c("exact_species", "fuzzy_species", "exact_genus",
                        "fuzzy_genus", "higher_rank_fallback", "unresolved")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# Empty/placeholder values that should NOT be treated as real taxonomic
# context or a real string to resolve (eDNA pipelines are full of these
# placeholder labels from upstream classifiers, e.g. "Cla_of_environmental
# samples" seen in real BLAST output earlier in this project).
is_blank_taxon <- function(x) {
  is.na(x) | x == "" | grepl("(?i)^(unidentified|unclassified|unknown|NA|environmental sample.*|.*_of_environmental samples)$", x)
}

# Query WoRMS for a single cleaned candidate string, trying exact match
# first and fuzzy match second. Returns NULL if nothing found, or a list
# with $records (the full candidate tibble, possibly >1 row) and $was_fuzzy.
query_worms_candidates <- function(cand) {
  if (is.na(cand) || cand == "" || nchar(cand) < 2) return(NULL)
  # marine_only = FALSE matches every other wm_records_name() call site in
  # this codebase (global.R's other two call sites, mod_e/f/g_enrichment.R).
  # Without it, worrms silently defaults marine_only = TRUE, which can only
  # ever cause harm here: a genuinely non-marine organism (a lab/reagent
  # contaminant like Sus scrofa, Homo sapiens, Gallus gallus) is excluded
  # from the candidate pool by definition, so the exact-match step can never
  # find it, and the fuzzy fallback below is then ALSO marine-restricted and
  # free to return an unrelated marine homonym as the "best available" match
  # (Round 52c: this was the root cause of the Sus scrofa -> Ilyella
  # chimera). Marine-only filtering belongs downstream, on the output of a
  # CORRECT identification (the non-marine override / marine-only export
  # view), never inside identification itself.
  recs <- tryCatch(wm_records_name(cand, fuzzy = FALSE, marine_only = FALSE), error = function(e) NULL)
  if (!is.null(recs) && nrow(recs) > 0) return(list(records = recs, was_fuzzy = FALSE))
  if (nchar(cand) >= 3) {
    recs <- tryCatch(wm_records_name(cand, fuzzy = TRUE, marine_only = FALSE), error = function(e) NULL)
    if (!is.null(recs) && nrow(recs) > 0) return(list(records = recs, was_fuzzy = TRUE))
  }
  NULL
}

# Pick the best candidate record from a multi-row WoRMS result, preferring
# one whose kingdom/phylum agrees with the row's own (possibly unreliable)
# context from its OTHER columns — the homonym disambiguation step.
#
# Round 52c: when context is available but NO candidate agrees with it, this
# now REFUSES the match (returns NULL) instead of silently falling back to
# the top-ranked candidate with context_agreed = FALSE. Previously the
# disagreement was only recorded, never acted on, so a context-disagreeing
# match (e.g. Sus scrofa's row context vs. a WoRMS hit in Platyhelminthes)
# could still be accepted and written out. The caller (resolve_one_column's
# ladder loop) treats a NULL here as "this rung didn't pan out" and moves on
# to the next, coarser rung of the ladder — i.e. refusing continues the same
# fallback-to-a-coarser-name behavior already used for an empty WoRMS result.
#
# Round 54: narrow, explicit exemption for an EXACT match that returned
# exactly ONE candidate AT SPECIES RANK -- a full species binomial is, for
# practical purposes, globally unique across kingdoms (unlike a bare genus
# or family word, where real cross-kingdom collisions exist -- see the
# Morus example above, which this exemption does NOT cover: it still goes
# through the full context check below). Found during Round 53 testing: the
# context check uses the row's OWN other raw columns as its disambiguation
# signal, so when those other columns are already actively wrong (not just
# blank) -- e.g. raw Species correctly resolves to a real, unambiguous
# species, but raw Phylum already, incorrectly, names something else -- the
# check refused the one genuinely correct match available, leaving the row
# unresolved instead of correctly identified. Gated on `!was_fuzzy` so this
# does NOT reopen the Round 52c marine_only-style vulnerability: that bug
# specifically produced a FUZZY single-candidate match (the real exact
# match was excluded from the pool), which still goes through the full
# context check unchanged.
#
# Round 55: a refusal now returns the top-ranked candidate it rejected
# (`rejected = recs[1, ]`) alongside `record = NULL`, instead of a bare
# NULL. The caller (resolve_one_column's ladder loop) still treats
# `record = NULL` exactly as before -- this rung didn't pan out, keep
# climbing. Nothing about what gets ACCEPTED, trusted, or exported changes.
# The rejected candidate's identity is carried along purely so Module B's
# cross-database agreement comparison (check_agreement()) can later show it
# as a disagreeing data point instead of silently treating a refused
# database exactly like one that made no call at all -- see that function's
# Round 55 comments for why.
pick_best_candidate <- function(recs, context_kingdom, context_phylum, was_fuzzy = FALSE) {
  if (!was_fuzzy && nrow(recs) == 1 && !is.na(recs$rank[1]) && recs$rank[1] == "Species") {
    return(list(record = recs[1, ], context_agreed = NA))
  }

  have_context <- !is_blank_taxon(context_kingdom) || !is_blank_taxon(context_phylum)

  if (have_context) {
    agrees <- rep(TRUE, nrow(recs))
    if (!is_blank_taxon(context_kingdom)) {
      agrees <- agrees & !is.na(recs$kingdom) & tolower(recs$kingdom) == tolower(context_kingdom)
    }
    if (!is_blank_taxon(context_phylum)) {
      agrees <- agrees & !is.na(recs$phylum) & tolower(recs$phylum) == tolower(context_phylum)
    }
    if (any(agrees, na.rm = TRUE)) {
      return(list(record = recs[which(agrees)[1], ], context_agreed = TRUE))
    }
    return(list(record = NULL, context_agreed = FALSE, rejected = recs[1, ]))
  }

  list(record = recs[1, ], context_agreed = NA)
}

# Resolve ONE taxonomic column's string for one row, independently.
#   lvl            - which column is being resolved ("Kingdom".."Species")
#   value          - that column's raw string for this row
#   tax_row        - the FULL row (named list, Kingdom..Species) so the
#                    resolver can (a) use other columns as homonym-check
#                    context, and (b) climb to higher-rank columns on this
#                    same row if `value` itself fails to resolve.
# Returns a one-row tibble: original, matched_name, matched_rank, AphiaID,
# Kingdom..Species (the full lineage of whichever record was matched),
# context_agreed, confidence.
resolve_one_column <- function(lvl, value, tax_row, taxon_cache, worms_delay = 0.2) {

  empty_result <- function() {
    tibble(
      original = value, matched_name = NA_character_, matched_rank = NA_character_,
      AphiaID = NA_integer_,
      Kingdom = NA_character_, Phylum = NA_character_, Class = NA_character_,
      Order = NA_character_, Family = NA_character_, Genus = NA_character_, Species = NA_character_,
      context_agreed = NA, confidence = "unresolved",
      rejected_AphiaID = NA_integer_, rejected_name = NA_character_, rejected_rank = NA_character_
    )
  }

  if (is_blank_taxon(value)) return(empty_result())

  # Context for homonym checking = this row's OTHER columns, not the one
  # being resolved (e.g. resolving Genus uses the row's own Kingdom/Phylum
  # strings as context, exactly as originally specified).
  context_kingdom <- if (lvl == "Kingdom") NA_character_ else (tax_row[["Kingdom"]] %||% NA_character_)
  context_phylum  <- if (lvl == "Phylum")  NA_character_ else (tax_row[["Phylum"]]  %||% NA_character_)

  cache_key <- paste(lvl, value, context_kingdom, context_phylum, sep = "\u241F")
  if (exists(cache_key, envir = taxon_cache, inherits = FALSE)) {
    return(get(cache_key, envir = taxon_cache, inherits = FALSE))
  }

  Sys.sleep(worms_delay)

  cleaned <- clean_codes(clean_suffix(clean_basic(value)))

  # Build the fallback ladder for THIS column:
  #   1. the cleaned string itself, at its own rank
  #   2. if this column is Species, also try genus-derived-from-it
  #   3. if still nothing, climb this SAME ROW's higher-rank columns
  #      (Kingdom is the ceiling — "fallback up to Kingdom")
  rank_order <- tax_levels  # Kingdom...Species, low index = higher rank
  lvl_idx <- match(lvl, rank_order)

  ladder <- list(list(string = cleaned, rank = lvl))
  if (lvl == "Species") {
    g <- to_genus(cleaned)
    if (!is_blank_taxon(g)) ladder[[length(ladder) + 1]] <- list(string = g, rank = "Genus")
  }
  if (lvl_idx > 1) {
    for (i in (lvl_idx - 1):1) {
      higher_val <- tax_row[[rank_order[i]]]
      if (!is.null(higher_val) && !is_blank_taxon(higher_val)) {
        ladder[[length(ladder) + 1]] <- list(string = higher_val, rank = rank_order[i])
      }
    }
  }

  # De-duplicate the ladder: the explicit Species->Genus derivation step
  # above and the "climb this row's own higher-rank columns" loop can both
  # produce a Genus-rank candidate (the derived genus word vs. the row's
  # own Genus column) — usually but not always identical strings. Querying
  # WoRMS twice for what's likely the same genus wastes an API call, so
  # keep only the FIRST occurrence of each (rank, lowercased string) pair,
  # preserving ladder order (most-specific attempt first).
  seen <- character(0)
  ladder <- Filter(function(step) {
    key <- paste(step$rank, tolower(step$string))
    if (key %in% seen) return(FALSE)
    seen[[length(seen) + 1]] <<- key
    TRUE
  }, ladder)

  result <- NULL
  # Round 55: remember the FIRST (most-specific ladder rung's) refused
  # candidate encountered while climbing, even if a coarser rung later
  # succeeds -- this is "the best attempt this column could make before it
  # got overruled by context," preserved purely for Module B's agreement
  # comparison (see check_agreement()). It never feeds `result` itself.
  rejected <- NULL
  rejected_step_rank <- NULL
  for (step in ladder) {
    qres <- query_worms_candidates(step$string)
    if (is.null(qres)) next

    picked <- pick_best_candidate(qres$records, context_kingdom, context_phylum, qres$was_fuzzy)
    # Round 52c: a context-disagreeing match is now refused (record = NULL)
    # rather than returned anyway — treat it exactly like an empty WoRMS
    # result and keep climbing the ladder to the next, coarser rung.
    if (is.null(picked$record)) {
      if (is.null(rejected) && !is.null(picked$rejected)) {
        rejected <- picked$rejected
        rejected_step_rank <- step$rank
      }
      next
    }
    rec <- picked$record
    valid_id <- if (!is.na(rec$status) && rec$status != "accepted") rec$valid_AphiaID else rec$AphiaID

    # Round 56: a retired name (status != "accepted") used to swap in only
    # the accepted AphiaID, while the Species string and the lineage columns
    # still came from the retired-name record -- so this column could show
    # the OLD name next to the NEW name's AphiaID until Module B's
    # build_species_fallback() happened to overwrite it. Now the accepted
    # record itself is fetched and used for the name and lineage too, so
    # name, lineage and AphiaID always describe the same, current taxon.
    # `matched_name` below deliberately stays the string the classifier
    # actually reported. Falls back to the retired-name record (the
    # pre-Round-56 behavior) if the accepted record can't be fetched.
    if (!is.na(rec$status) && rec$status != "accepted" &&
        !is.na(valid_id) && !identical(as.integer(valid_id), as.integer(rec$AphiaID))) {
      if (!exists(as.character(valid_id), envir = .wm_record_cache_env, inherits = FALSE)) Sys.sleep(worms_delay)
      accepted_rec <- cached_wm_record(valid_id)
      if (!is.null(accepted_rec) && NROW(accepted_rec) >= 1 && !is.null(accepted_rec$scientificname)) {
        rec <- accepted_rec[1, ]
      }
    }

    # Confidence uses the app's documented six-tier scheme: exact_species,
    # fuzzy_species, exact_genus, fuzzy_genus, higher_rank_fallback, unresolved
    # - there is no exact_family/exact_order/etc. tier. So for Species and
    # Genus specifically, confidence is judged relative to the COLUMN being
    # resolved (e.g. if Species fails but its genus-derivation succeeds,
    # that's exact/fuzzy_genus for the Species column). For every OTHER
    # column (Family, Order, Class, Phylum, Kingdom), any value derived from
    # climbing the ladder - including a same-rank exact match with no
    # climbing at all - is tagged "higher_rank_fallback" by design; the
    # scheme intentionally doesn't distinguish "resolved directly" from
    # "fell back" above Genus.
    confidence <- if (step$rank == lvl && lvl == "Species") {
      if (qres$was_fuzzy) "fuzzy_species" else "exact_species"
    } else if (step$rank == lvl && lvl == "Genus") {
      if (qres$was_fuzzy) "fuzzy_genus" else "exact_genus"
    } else if (step$rank == "Genus" && lvl == "Species") {
      if (qres$was_fuzzy) "fuzzy_genus" else "exact_genus"
    } else {
      "higher_rank_fallback"
    }

    result <- tibble(
      original = value, matched_name = step$string, matched_rank = rec$rank %||% step$rank,
      AphiaID = as.integer(valid_id),
      Kingdom = rec$kingdom %||% NA_character_, Phylum = rec$phylum %||% NA_character_,
      Class = rec$class %||% NA_character_, Order = rec$order %||% NA_character_,
      Family = rec$family %||% NA_character_, Genus = rec$genus %||% NA_character_,
      Species = if (!is.na(rec$rank) && rec$rank == "Species") rec$scientificname else NA_character_,
      context_agreed = picked$context_agreed, confidence = confidence,
      rejected_AphiaID = NA_integer_, rejected_name = NA_character_, rejected_rank = NA_character_
    )
    break
  }

  if (is.null(result)) result <- empty_result()

  # Round 55: attach whatever refusal was encountered during this ladder
  # climb (NA if none), regardless of which branch produced `result` above
  # -- both the success tibble and empty_result() already carry the
  # placeholder columns, so this is a uniform overwrite in every case.
  if (!is.null(rejected)) {
    # Round 56: a refused candidate that is itself a retired name is reported
    # under its accepted AphiaID/name, so Module B's agreement comparison
    # (check_agreement(), which compares accepted AphiaIDs) doesn't see a
    # false disagreement purely because one side is a synonym ID.
    rej_status <- rejected$status %||% NA_character_
    rej_valid  <- rejected$valid_AphiaID %||% NA_integer_
    rej_vname  <- rejected$valid_name %||% NA_character_
    rej_unacc  <- !is.na(rej_status) && rej_status != "accepted" && !is.na(rej_valid)
    result$rejected_AphiaID <- as.integer(if (rej_unacc) rej_valid else rejected$AphiaID)
    result$rejected_name <- if (rej_unacc && !is.na(rej_vname)) rej_vname else rejected$scientificname
    result$rejected_rank <- rejected_step_rank
  }

  assign(cache_key, result, envir = taxon_cache)
  result
}

# Round 53: resolve ALL 7 taxonomic columns for ONE row of ONE classifier's
# taxonomy (BLAST's own row, or BOLD's, or MZG's) by anchoring on the SINGLE
# most-specific name this row actually has, instead of resolving each of
# the 7 columns independently. This is the user's originally-intended
# design: "match the most specific available name once, take the full
# lineage from that one hit, and only fall back to a coarser name if the
# specific one truly doesn't resolve."
#
# Reuses resolve_one_column() UNCHANGED for the actual ladder-climb (own
# rank, then Species -> derived-genus, then this row's own higher-rank
# columns) and everything it already does (Round 52c's marine_only = FALSE
# and context-disagreement refusal) -- this function only picks WHICH
# column to start that ladder from (the row's own most-specific non-blank
# raw value) and fans the single resulting match out to all 7 columns.
#
# Each ancestor rank gets its OWN true AphiaID via cached_wm_classification()
# -- not the matched (leaf) rank's AphiaID copy-pasted into every column --
# specifically so Module B's per-rank, per-classifier agreement comparison
# (BLAST_<rank>_AphiaID vs BOLD_<rank>_AphiaID vs MZG_<rank>_AphiaID) still
# has a genuine, distinct value to compare at every rank. Reusing one leaf
# AphiaID everywhere was the documented reason an earlier row-anchored
# attempt was reverted (CHANGELOG v1.5) -- this mirrors the pattern
# build_species_fallback() already uses downstream for the same reason.
#
# Does NOT fix a chimera caused by multiple classifiers independently,
# deterministically landing on the SAME wrong match (a correlated error,
# e.g. the Round 52c marine_only bug) -- that's a different failure mode,
# addressed by Round 52c's fixes, not by row-anchoring. What this DOES fix:
# a single classifier's own 7 columns landing on genuinely different WoRMS
# records (the literal chimera-row bug report), and the two cases
# build_species_fallback()'s own cross-classifier row-anchoring can't
# reach -- Module A's own per-database output table (viewed before any
# cross-classifier merge happens), and an ASV where only ONE classifier
# produced any data at all (so the >=2-of-3 agreement that triggers
# build_species_fallback()'s re-query never fires).
resolve_row_anchored <- function(tax_row, taxon_cache, worms_delay = 0.2) {
  rank_order <- tax_levels  # Kingdom...Species, low index = higher rank

  empty_for <- function(lvl) {
    tibble(
      original = tax_row[[lvl]] %||% NA_character_, matched_name = NA_character_,
      matched_rank = NA_character_, AphiaID = NA_integer_,
      Kingdom = NA_character_, Phylum = NA_character_, Class = NA_character_,
      Order = NA_character_, Family = NA_character_, Genus = NA_character_, Species = NA_character_,
      context_agreed = NA, confidence = "unresolved",
      rejected_AphiaID = NA_integer_, rejected_name = NA_character_
    )
  }

  empty_all <- function() {
    out <- lapply(rank_order, empty_for)
    names(out) <- rank_order
    out
  }

  # Round 55: whatever resolve_one_column()'s own ladder climb refused (if
  # anything) gets surfaced on the ONE output rank it actually applies to --
  # `result$rejected_rank`, the rung that was rejected -- regardless of
  # whether the row-anchor attempt as a whole ultimately succeeded (at a
  # coarser rung) or failed outright. This is a flat, single-value
  # propagation (not a second classification lookup/lineage fan-out): it
  # tells Module B's agreement comparison "this database attempted X at
  # this rank and had it rejected," nothing more.
  inject_rejected <- function(out, result) {
    rj_rank <- result[["rejected_rank"]]   # [[ ]]: NULL (not a warning) if a result lacks the column
    if (!is.null(rj_rank) && !is.na(rj_rank) && rj_rank %in% names(out)) {
      out[[rj_rank]]$rejected_AphiaID <- result$rejected_AphiaID
      out[[rj_rank]]$rejected_name <- result$rejected_name
    }
    out
  }

  # This row's own most-specific non-blank raw value starts the ladder --
  # Species first, climbing toward Kingdom -- exactly the order the ladder
  # itself climbs in when a column's own rank fails to resolve.
  start_lvl <- NULL
  for (lvl in rev(rank_order)) {
    val <- tax_row[[lvl]]
    if (!is.null(val) && !is_blank_taxon(val)) {
      start_lvl <- lvl
      break
    }
  }
  if (is.null(start_lvl)) return(empty_all())

  result <- resolve_one_column(start_lvl, tax_row[[start_lvl]], tax_row, taxon_cache, worms_delay)
  if (result$confidence == "unresolved") return(inject_rejected(empty_all(), result))

  anchor_rank <- result$matched_rank
  anchor_idx <- match(anchor_rank, rank_order)
  if (is.na(anchor_idx)) return(inject_rejected(empty_all(), result))  # defensive; shouldn't happen

  # Each ancestor's OWN AphiaID, not the matched rank's reused. Skipped
  # entirely when the match IS already Kingdom (nothing senior to look up).
  rank_aphia <- setNames(as.list(rep(NA_integer_, length(rank_order))), rank_order)
  rank_aphia[[anchor_rank]] <- result$AphiaID
  if (anchor_idx > 1) {
    already_cached <- exists(as.character(result$AphiaID), envir = .wm_classification_cache_env, inherits = FALSE)
    if (!already_cached) Sys.sleep(worms_delay)
    cls <- cached_wm_classification(result$AphiaID)
    if (!is.null(cls) && nrow(cls) > 0) {
      for (j in seq_len(nrow(cls))) {
        cl_rank <- cls$rank[j]
        if (!is.na(cl_rank) && cl_rank %in% rank_order) {
          rank_aphia[[cl_rank]] <- as.integer(cls$AphiaID[j])
        }
      }
    }
  }

  out <- list()
  for (lvl in rank_order) {
    lvl_idx <- match(lvl, rank_order)
    # Senior to (or at) the anchor rank, but the matched record's OWN WoRMS
    # lineage has no entry for this particular rank (e.g. many Platyhelminthes
    # genera have no populated Class, confirmed in Round 52b) -- correctly
    # unresolved, not "higher_rank_fallback": there is no value here to have
    # fallen back TO. Without this check, a lineage gap like that one would
    # silently report a confidence tier for a column that in fact carries no
    # information, which is exactly the kind of mismatch this whole round is
    # trying to eliminate.
    if (lvl_idx > anchor_idx || is.na(result[[lvl]]) || result[[lvl]] == "") {
      out[[lvl]] <- empty_for(lvl)
      next
    }
    out[[lvl]] <- tibble(
      original = tax_row[[lvl]] %||% NA_character_, matched_name = result[[lvl]],
      matched_rank = anchor_rank, AphiaID = rank_aphia[[lvl]],
      Kingdom = result$Kingdom, Phylum = result$Phylum, Class = result$Class,
      Order = result$Order, Family = result$Family, Genus = result$Genus, Species = result$Species,
      context_agreed = result$context_agreed,
      confidence = if (lvl == start_lvl) result$confidence else "higher_rank_fallback",
      rejected_AphiaID = NA_integer_, rejected_name = NA_character_
    )
  }
  inject_rejected(out, result)
}

# Batch resolution across an entire data frame: every row, every one of the
# 7 tax_levels columns. As of Round 53, `row_anchored = TRUE` (the default)
# anchors each row on its own single most-specific resolvable name via
# resolve_row_anchored() above; `row_anchored = FALSE` restores the
# pre-Round-53 behavior of resolving each of the 7 columns independently
# via resolve_one_column(), kept as an explicit, instant rollback switch.
# `progress_fn` is called as progress_fn(done, total, label) after every
# column (so total = nrow(data) * 7 roughly, minus blanks skipped without a
# query) regardless of which mode is active, so progress reporting is
# unaffected by this switch.
resolve_columns_batch <- function(data, taxon_cache, worms_delay = 0.2, progress_fn = NULL,
                                   cancel_check = NULL, row_anchored = TRUE) {
  n <- nrow(data)
  done <- 0
  total <- n * length(tax_levels)

  resolved_cols <- list()
  for (lvl in tax_levels) resolved_cols[[lvl]] <- vector("list", n)

  for (i in seq_len(n)) {
    if (!is.null(cancel_check) && isTRUE(cancel_check())) {
      return(NULL)
    }
    tax_row <- as.list(data[i, tax_levels, drop = FALSE])

    if (row_anchored) {
      row_result <- resolve_row_anchored(tax_row, taxon_cache, worms_delay)
      for (lvl in tax_levels) {
        resolved_cols[[lvl]][[i]] <- row_result[[lvl]]
        done <- done + 1
        if (!is.null(progress_fn)) progress_fn(done, total, sprintf("%s: %s", lvl, tax_row[[lvl]] %||% ""))
      }
      next
    }

    for (lvl in tax_levels) {
      if (!is.null(cancel_check) && isTRUE(cancel_check())) {
        return(NULL)
      }
      resolved_cols[[lvl]][[i]] <- resolve_one_column(lvl, tax_row[[lvl]], tax_row, taxon_cache, worms_delay)
      done <- done + 1
      if (!is.null(progress_fn)) progress_fn(done, total, sprintf("%s: %s", lvl, tax_row[[lvl]] %||% ""))
    }
  }

  lapply(resolved_cols, bind_rows)
}

# Seed an environment-based cache from a previously-saved cache data.frame,
# for the "same project" reuse case. Cache schema now matches
# resolve_one_column()'s per-(level, value, context) output.
seed_cache_from_df <- function(cache_df, taxon_cache) {
  if (is.null(cache_df) || nrow(cache_df) == 0) return(invisible(NULL))
  req_cols <- c("level", "original", "AphiaID", "confidence")
  if (!all(req_cols %in% names(cache_df))) {
    stop("Cache file must contain at least the columns: level, original, AphiaID, confidence (plus the lineage columns from a previous run's cache download).")
  }
  context_kingdom <- if ("context_kingdom" %in% names(cache_df)) cache_df$context_kingdom else NA_character_
  context_phylum  <- if ("context_phylum" %in% names(cache_df)) cache_df$context_phylum  else NA_character_
  for (i in seq_len(nrow(cache_df))) {
    row <- cache_df[i, ]
    cache_key <- paste(row$level, row$original, context_kingdom[i] %||% NA_character_,
                        context_phylum[i] %||% NA_character_, sep = "\u241F")
    row <- row %>% select(-level, -any_of(c("context_kingdom", "context_phylum")))
    # Round 63: a cache file saved before Round 55 has no refused-match columns,
    # and one saved later may have them read back as all-NA logical. Give every
    # seeded entry the same three columns, with the right types, that a freshly
    # resolved entry has, so reading them (resolve_row_anchored() ->
    # inject_rejected()) never hits a missing column. A stored refusal is kept;
    # an old entry simply has none.
    row$rejected_AphiaID <- if ("rejected_AphiaID" %in% names(row)) suppressWarnings(as.integer(row$rejected_AphiaID)) else NA_integer_
    row$rejected_name    <- if ("rejected_name"    %in% names(row)) as.character(row$rejected_name)    else NA_character_
    row$rejected_rank    <- if ("rejected_rank"    %in% names(row)) as.character(row$rejected_rank)    else NA_character_
    assign(cache_key, row, envir = taxon_cache)
  }
  invisible(NULL)
}

cache_env_to_df <- function(taxon_cache) {
  keys <- ls(envir = taxon_cache)
  if (length(keys) == 0) {
    return(tibble(
      level = character(), original = character(), matched_name = character(),
      matched_rank = character(), AphiaID = integer(),
      Kingdom = character(), Phylum = character(), Class = character(), Order = character(),
      Family = character(), Genus = character(), Species = character(),
      context_agreed = logical(), confidence = character(),
      context_kingdom = character(), context_phylum = character()
    ))
  }
  bind_rows(lapply(keys, function(k) {
    parts <- strsplit(k, "\u241F", fixed = TRUE)[[1]]
    row <- get(k, envir = taxon_cache, inherits = FALSE)
    row$level <- parts[1]
    row$context_kingdom <- if (length(parts) >= 3) parts[3] else NA_character_
    row$context_phylum  <- if (length(parts) >= 4) parts[4] else NA_character_
    row
  }))
}

# Apply a resolved-columns list (output of resolve_columns_batch, one
# tibble per tax_level) back onto the original data: for each column,
# overwrite with the resolved/WoRMS-derived value ONLY where WoRMS actually
# returned something (never blank out a real original value), add a
# matching `<Level>_AphiaID` column, and attach per-column confidence +
# context-agreement columns for transparency.
#
#
# Round 61: the refused-match information from Round 55 is NOT written into
# this table (an earlier Round 55 version added `<Level>_Rejected_AphiaID` /
# `<Level>_Rejected_Name` columns, which then showed up in every preview and
# download -- removed at the user's request, the output is exactly as it was
# before Round 55). It travels separately: see extract_rejected_matches() below.
apply_resolution <- function(data, resolved_cols) {
  for (lvl in tax_levels) {
    res <- resolved_cols[[lvl]]
    stopifnot(nrow(data) == nrow(res))

    data[[lvl]] <- ifelse(!is.na(res$matched_name) & res$matched_name != "",
                           # Use the matched scientific name at the level's
                           # own rank when available, else keep the input.
                           ifelse(!is.na(res[[lvl]]) & res[[lvl]] != "", res[[lvl]], data[[lvl]]),
                           data[[lvl]])
    data[[paste0(lvl, "_AphiaID")]] <- res$AphiaID
    data[[paste0(lvl, "_confidence")]] <- res$confidence
    data[[paste0(lvl, "_context_agreed")]] <- res$context_agreed
  }
  data
}

# Round 61: internal-only carrier for the Round 55 "refused match" signal.
# Returns a small table (ASV_id + one `<Level>_Rejected_AphiaID` column per
# level, NA except where that database's match at that rank was refused for
# disagreeing with the row's own context). Module A keeps it in memory and in
# shared state for Module B's agreement comparison ONLY; it is never joined
# into an output table, shown, or exported.
extract_rejected_matches <- function(data, resolved_cols) {
  out <- tibble::tibble(ASV_id = as.character(data$ASV_id))
  for (lvl in tax_levels) {
    rj <- resolved_cols[[lvl]]$rejected_AphiaID
    out[[paste0(lvl, "_Rejected_AphiaID")]] <- if (is.null(rj)) rep(NA_integer_, nrow(data)) else as.integer(rj)
  }
  out
}

# Round 61: temporary copy of the combined table with each database's refused
# matches joined on (as `<DB>_<Level>_Rejected_AphiaID`), used only as the input
# to check_agreement() so a refused match counts as that database's disagreeing
# call (Round 55). `combined` itself is never modified, so no such column ever
# reaches Module B's table or anything downstream. `rejected_by_db` is
# list(BLAST = , BOLD = , MZG = ), each an extract_rejected_matches() table or NULL.
agreement_input_with_rejected <- function(combined, rejected_by_db) {
  out <- combined
  for (db in names(rejected_by_db)) {
    rj <- rejected_by_db[[db]]
    if (is.null(rj) || nrow(rj) == 0) next
    rj <- rj[!duplicated(rj$ASV_id), , drop = FALSE]
    names(rj)[names(rj) != "ASV_id"] <- paste0(db, "_", names(rj)[names(rj) != "ASV_id"])
    out <- dplyr::left_join(out, rj, by = "ASV_id")
  }
  out
}

# ------------------------------------------------------------------------
# 4. MODULE B helpers — agreement & species fallback
# ------------------------------------------------------------------------

check_agreement <- function(data, level, log = message) {
  cols <- c(
    paste0("BLAST_", level, "_AphiaID"),
    paste0("MZG_", level, "_AphiaID"),
    paste0("BOLD_", level, "_AphiaID")
  )
  existing_cols <- intersect(cols, names(data))

  if (length(existing_cols) < 2) {
    log(sprintf("Not enough columns for agreement at %s", level))
    return(tibble(Level = level, All_three_agree = NA, Two_agree = NA, None_agree = NA,
                  Only_one_db = NA, Total_with_data = NA, Percent_all_agree = NA,
                  Percent_two_agree = NA, Percent_none_agree = NA, Percent_only_one_db = NA))
  }

  n_dbs <- length(existing_cols)  # 2 or 3 — how many databases are actually present
  df <- data

  # Round 55 (Round 61: the `_Rejected_AphiaID` columns come from
  # agreement_input_with_rejected(), a temporary copy made by Module B just
  # for this call, not from the table itself): before the None/missing
  # collapse below, give every database a chance to contribute a REJECTED
  # match in place of a missing accepted
  # one. Rounds 52c/53/54 made this comparison "too harsh" in the user's
  # words: refusing a context-disagreeing match (correctly keeping it out of
  # the trusted/consensus output) also silently removed that database's
  # data point from THIS agreement comparison, making a database that found
  # something and had it rejected indistinguishable from one that made no
  # call at all -- directly undercutting this function's purpose of
  # comparing each database's own best attempt. Each accepted AphiaID
  # column is coalesced with its `_Rejected_AphiaID` counterpart (NA unless
  # that specific database/rank had a refusal) so a refused match now shows
  # up as a disagreeing value here, while `Species_fallback`,
  # `build_species_fallback()`, and the Consensus_* columns are untouched --
  # they still read only the accepted AphiaID columns, so the refusal still,
  # and only, keeps that value out of the trusted/final output.
  for (col in existing_cols) {
    rej_col <- sub("_AphiaID$", "_Rejected_AphiaID", col)
    if (rej_col %in% names(df)) {
      df[[col]] <- dplyr::coalesce(df[[col]], df[[rej_col]])
    }
  }

  df <- df %>% mutate(across(all_of(existing_cols), ~ replace_na(as.integer(.), 0L)))

  flags <- pmap(df[existing_cols], function(...) {
    vals <- c(...)
    vals <- vals[vals != 0]
    n_unique <- length(unique(vals))
    c(
      # "All agree" means every DB that reported a non-zero call agrees,
      # AND every present DB actually reported one (no missing calls).
      all_agree  = (n_unique == 1 && length(vals) == n_dbs),
      # "Two agree" only means something distinct from "all agree" when
      # there are 3 DBs present (2 of 3 agree, one differs/missing).
      # With only 2 DBs present, "two agree" collapses into "all agree".
      # FIXED: this used to only check length(vals) == 3 (all three DBs
      # reported, two of the three values match) - which structurally cannot
      # match the "one differs/MISSING" case this comment describes: when the
      # third DB has no call at all, length(vals) == 2, and if the other two
      # agree, n_unique == 1, which failed the old n_unique == 2 check. That
      # left a common situation (one of three DBs has no call at this rank,
      # the other two agree) falling through all_agree, two_agree, AND
      # none_agree at once - silently understating real agreement. Now
      # explicitly covers both sub-cases.
      two_agree  = (n_dbs == 3 && n_unique == 1 && length(vals) == 2) ||
                   (n_dbs == 3 && n_unique == 2 && length(vals) == 3),
      none_agree = (n_unique == length(vals) && length(vals) >= 2),
      # Round 62: an ASV where exactly ONE database made a call at this rank
      # has nothing to agree or disagree with, so it fell into none of the
      # three categories above while still being counted in `total` -- the
      # per-level categories then summed to less than "with data" (worst at
      # Species, where databases most often leave gaps). It is now its own
      # category, so All + Two + None + Only-one always equals Total_with_data.
      only_one   = (length(vals) == 1),
      total      = length(vals)
    )
  })

  fl <- bind_rows(flags)

  n_all   <- sum(fl$all_agree,  na.rm = TRUE)
  n_two   <- sum(fl$two_agree,  na.rm = TRUE)
  n_none  <- sum(fl$none_agree, na.rm = TRUE)
  n_one   <- sum(fl$only_one,   na.rm = TRUE)
  n_total <- sum(fl$total > 0)
  pct <- function(k) round(100 * k / max(n_total, 1), 2)

  tibble(
    Level = level,
    All_three_agree = n_all,
    Two_agree = n_two,
    None_agree = n_none,
    Only_one_db = n_one,
    Total_with_data = n_total,
    Percent_all_agree = pct(n_all),
    Percent_two_agree = pct(n_two),
    Percent_none_agree = pct(n_none),
    Percent_only_one_db = pct(n_one)
  )
}

build_species_fallback <- function(combined, log = message, progress = NULL) {

  levels <- c("Species", "Genus", "Family", "Order", "Class", "Phylum", "Kingdom")
  rank_order <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  dbs <- c("BLAST", "BOLD", "MZG")

  combined$Species_fallback <- 0L
  combined$Species_fallback_source <- "None"
  combined$Fallback_rank <- NA_character_
  resolved <- logical(nrow(combined))

  for (level in levels) {
    if (all(resolved)) break

    cols <- intersect(paste0(dbs, "_", level, "_AphiaID"), names(combined))
    if (length(cols) < 2) next

    rows <- which(!resolved)
    if (length(rows) == 0) break

    m <- as.matrix(combined[rows, cols, drop = FALSE])
    m[is.na(m)] <- 0L

    for (i in seq_along(rows)) {
      ri <- rows[i]
      v <- m[i, ]
      nz <- v[v != 0 & !is.na(v)]
      u <- unique(nz)

      for (val in u) {
        if (sum(nz == val, na.rm = TRUE) >= 2) {
          combined$Species_fallback[ri] <- as.integer(val)
          matching_cols <- names(v)[!is.na(v) & v == val]
          # FIXED: derive the agreeing database names directly from the
          # matching column names themselves (e.g. "BLAST_Species_AphiaID" ->
          # "BLAST") rather than indexing the fixed-length `dbs` vector
          # (always length 3) with a logical mask whose length is the number
          # of databases actually present. With only 2 of 3 databases
          # uploaded, `cols` has length 2, and `dbs[cols %in% matching_cols]`
          # silently RECYCLED that length-2 mask against the length-3 `dbs`
          # vector - if both present databases agreed, the recycled mask was
          # all-TRUE and returned all three names, crediting a database that
          # was never even uploaded.
          agreeing <- sub(paste0("_", level, "_AphiaID$"), "", matching_cols)
          # FIXED: previously an unseeded sample(agreeing, 1) picked which
          # single agreeing database to credit, so re-running the identical
          # dataset could report a different source each time even though the
          # consensus AphiaID never changed. Now deterministic and fully
          # informative: every agreeing database, sorted and joined, so
          # nothing is silently discarded.
          combined$Species_fallback_source[ri] <- paste(sort(agreeing), collapse = "+")
          combined$Fallback_rank[ri] <- level
          resolved[ri] <- TRUE
          break
        }
      }
    }
  }

  # Re-query WoRMS for the consensus AphiaID's full classification and
  # overwrite the DB-prefixed columns at and above the fallback rank with
  # WoRMS-verified lineage. This ensures consistent taxonomy even when the
  # original databases had conflicting higher-rank values
  # (e.g. Gastropoda vs Insecta at Class while agreeing at Genus).
  total_to_query <- sum(!is.na(combined$Fallback_rank) & combined$Species_fallback != 0 & !is.na(combined$Species_fallback))
  queried <- 0L
  for (i in seq_len(nrow(combined))) {
    fb_rank <- combined$Fallback_rank[i]
    if (is.na(fb_rank)) next
    fb_aid <- combined$Species_fallback[i]
    if (is.na(fb_aid) || fb_aid == 0) next

    queried <- queried + 1L
    if (is.function(progress)) progress(queried, total_to_query)

    cls <- cached_wm_classification(as.integer(fb_aid))
    if (is.null(cls) || nrow(cls) == 0) next

    fb_idx <- match(fb_rank, rank_order)
    for (j in seq_len(nrow(cls))) {
      cl_rank <- cls$rank[j]
      cl_name <- cls$scientificname[j]
      cl_aid <- cls$AphiaID[j]
      if (is.null(cl_rank) || is.na(cl_rank)) next
      cl_idx <- match(cl_rank, rank_order)
      if (is.na(cl_idx) || is.na(fb_idx) || cl_idx > fb_idx) next
      for (db in dbs) {
        name_col <- paste0(db, "_", cl_rank)
        aphia_col <- paste0(db, "_", cl_rank, "_AphiaID")
        conf_col <- paste0(db, "_", cl_rank, "_confidence")
        ctx_col <- paste0(db, "_", cl_rank, "_context_agreed")
        if (name_col %in% names(combined)) combined[[name_col]][i] <- cl_name
        if (aphia_col %in% names(combined)) combined[[aphia_col]][i] <- as.integer(cl_aid)
        if (conf_col %in% names(combined)) combined[[conf_col]][i] <- "consensus"
        if (ctx_col %in% names(combined)) combined[[ctx_col]][i] <- TRUE
      }
    }
  }

  # Null out DB-prefixed columns below the fallback rank so the row is
  # internally consistent — e.g. if fallback was at Genus, Species columns
  # are set to NA; if at Kingdom, all Phylum..Species are NA.
  for (i in seq_len(nrow(combined))) {
    fb_rank <- combined$Fallback_rank[i]
    if (is.na(fb_rank)) next
    fb_idx <- match(fb_rank, rank_order)
    if (is.na(fb_idx)) next
    lower_ranks <- rank_order[-(1:fb_idx)]
    if (length(lower_ranks) == 0) next
    for (lr in lower_ranks) {
      for (db in dbs) {
        name_col <- paste0(db, "_", lr)
        aphia_col <- paste0(db, "_", lr, "_AphiaID")
        conf_col <- paste0(db, "_", lr, "_confidence")
        ctx_col <- paste0(db, "_", lr, "_context_agreed")
        if (name_col %in% names(combined)) combined[[name_col]][i] <- NA_character_
        if (aphia_col %in% names(combined)) combined[[aphia_col]][i] <- NA_integer_
        if (conf_col %in% names(combined)) combined[[conf_col]][i] <- NA_character_
        if (ctx_col %in% names(combined)) combined[[ctx_col]][i] <- NA
      }
    }
  }

  # Consolidate per-DB taxonomy into single consensus columns and drop the
  # redundant per-DB columns. After WoRMS re-query, all DBs contain identical
  # WoRMS-verified lineage for resolved ASVs, so per-DB columns are redundant.
  # Unresolved ASVs get NA in the consensus columns (no single source of truth).
  # FIXED: this used to call it "consensus" whenever unique(na.omit(x)) had
  # length 1 - which is trivially true when only ONE database has a non-NA
  # value for that rank/row (nothing else to disagree with), not just when
  # 2+ databases genuinely agree. That let an unresolved ASV (Fallback_rank
  # NA, Species_fallback_source "None" - e.g. every row whose only real data
  # came from one orphaned database, per the assign_mzg_asv_id() join-key fix
  # above) still show a fully-populated, seemingly-validated Consensus_*
  # lineage down to Species, contradicting its own Fallback fields. Now
  # requires at least 2 non-NA values before calling anything a consensus,
  # matching the >=2-agree standard used everywhere else in this function
  # and in check_agreement().
  for (lr in rank_order) {
    db_name_cols <- intersect(paste0(dbs, "_", lr), names(combined))
    if (length(db_name_cols) > 0) {
      combined[[paste0("Consensus_", lr)]] <- apply(combined[db_name_cols], 1, function(x) {
        x <- x[!is.na(x)]
        if (length(x) < 2) return(NA_character_)
        vals <- unique(x)
        if (length(vals) == 1) vals[1] else NA_character_
      })
    }
    db_aphia_cols <- intersect(paste0(dbs, "_", lr, "_AphiaID"), names(combined))
    if (length(db_aphia_cols) > 0) {
      combined[[paste0("Consensus_", lr, "_AphiaID")]] <- apply(combined[db_aphia_cols], 1, function(x) {
        x <- as.integer(x)
        x <- x[!is.na(x)]
        if (length(x) < 2) return(NA_integer_)
        vals <- unique(x)
        if (length(vals) == 1) vals[1] else NA_integer_
      })
    }
  }

  # Drop per-DB taxonomy columns (only the known taxonomy patterns, not
  # sample-count columns like MZG_S1 etc.)
  tax_col_pattern <- paste0("^(", paste(dbs, collapse = "|"),
                            ")_(", paste(rank_order, collapse = "|"),
                            ")(_AphiaID|_confidence|_context_agreed|_Rejected_AphiaID|_Rejected_Name)?$")
  # Rounds 60/61: `_Rejected_*` columns no longer exist in Module A/B tables
  # (Round 61 keeps that information out of every output). They stay in this
  # pattern, and in Module E's sample-column exclusion, only so that a table
  # saved during the brief Round 55-60 period that did carry them is cleaned
  # instead of having them mistaken for sample columns.
  combined <- combined %>% select(-matches(tax_col_pattern))

  # Rename MZG_Sequence_length_bp -> Sequence_length_bp and drop BLAST metadata
  if ("MZG_Sequence_length_bp" %in% names(combined)) {
    combined <- combined %>% rename(Sequence_length_bp = MZG_Sequence_length_bp)
  }
  blast_meta_cols <- c("BLAST_Percent_id", "BLAST_Query_len", "BLAST_E_value")
  combined <- combined %>% select(-any_of(blast_meta_cols))

  log(sprintf("Fallback by agreement: %d ASVs resolved (%d genus+, %d family+, %d order+, %d class+, %d phylum+, %d kingdom+), %d unresolved",
              sum(resolved),
              sum(combined$Fallback_rank == "Genus", na.rm = TRUE),
              sum(combined$Fallback_rank == "Family", na.rm = TRUE),
              sum(combined$Fallback_rank == "Order", na.rm = TRUE),
              sum(combined$Fallback_rank == "Class", na.rm = TRUE),
              sum(combined$Fallback_rank == "Phylum", na.rm = TRUE),
              sum(combined$Fallback_rank == "Kingdom", na.rm = TRUE),
              nrow(combined) - sum(resolved)))

  combined
}

# ------------------------------------------------------------------------
# 5. MODULE E helpers — WoRMS enrichment (distribution, traits, vernacular)
# ------------------------------------------------------------------------

# Kept for the human-readable WoRMS_Distribution text column (still shown
# to the user as-is) — this is no longer used to CLASSIFY occurrence (see
# below), only to display what WoRMS actually said.
extract_distribution_text <- function(dist_df) {
  if (is.null(dist_df) || nrow(dist_df) == 0) return(NA_character_)
  text_parts <- c()
  possible_cols <- c("locality", "locationID", "higherGeography", "location", "area")
  for (col in possible_cols) {
    if (col %in% names(dist_df) && any(!is.na(dist_df[[col]]))) {
      text_parts <- c(text_parts, dist_df[[col]][!is.na(dist_df[[col]])])
    }
  }
  if (length(text_parts) == 0) return(NA_character_)
  paste(unique(text_parts), collapse = "; ")
}

# Extract the bare integer MRGID from a locationID field, which WoRMS
# returns as a URI like "http://marineregions.org/mrgid/3293". Returns
# NA_integer_ if the string doesn't contain a recognizable MRGID.
# Handles semicolon-delimited lists of IDs/URLs.
extract_all_mrgids <- function(location_id) {
    if (is.null(location_id) || is.na(location_id) || location_id == "" || location_id == "NA") return(integer(0))
    
    # Split by semicolon
    parts <- unlist(strsplit(as.character(location_id), ";"))
    
    # Extract MRGIDs
    mrgids <- unlist(lapply(parts, function(part) {
        x_char <- trimws(part)
        # 1. Try to find digits following '/mrgid/'
        m <- regmatches(x_char, regexpr("(?<=/mrgid/)[0-9]+", x_char, perl = TRUE))
        # 2. If not found, fallback to finding digits at the end of the string
        if (length(m) == 0 || m == "") {
          m <- regmatches(x_char, regexpr("[0-9]+$", x_char))
        }
        if (length(m) == 0 || m == "") return(NA_integer_)
        suppressWarnings(as.integer(m))
    }))
    
    # Return as vector, removing NAs
    return(mrgids[!is.na(mrgids)])
}

# Legacy wrapper for single-MRGID extraction (picks first one)
extract_mrgid <- function(location_id) {
    res <- extract_all_mrgids(location_id)
    if (length(res) > 0) return(res[1])
    return(NA_integer_)
}


# Classify occurrence based on text-based keyword matching (refined approach)
classify_occurrence_by_text <- function(dist_str, reference_name) {
  if (is.na(dist_str) || !nzchar(dist_str)) return("Unknown")

  text <- tolower(dist_str)
  ref <- tolower(reference_name)

  # 2. Very likely
  # Includes reference name, North Sea, German/British/English coasts, specific
  # sub-regions, and (Round 45, 2026-09-30) Belgium/Netherlands by name --
  # their ENTIRE coastline is on the North Sea, so a bare country mention
  # ("Belgium", "Belgian Exclusive Economic Zone", "Netherlands", "Dutch
  # Exclusive Economic Zone") is as strong a signal as "north sea" itself.
  # Checked BEFORE very_unlikely_keys below (Round 45 reorder, per real-world
  # case Alitta succinea: WoRMS text lists both "Belgian Exclusive Economic
  # Zone" AND "Caribbean Sea" as secondary populations for this wide-ranging
  # species -- the old check order let the incidental Caribbean mention
  # short-circuit to "Very unlikely" before this specific, definitive North
  # Sea match ever got a chance. A species with NO very_likely match still
  # falls through to very_unlikely exactly as before this change -- only the
  # "text matches both lists at once" case is affected.
  very_likely_keys <- c(ref, "north sea", "german bight", "dogger bank", "skagerrak",
                        "kattegat", "helgoland", "wadden sea", "dutch coast",
                        "danish coast", "german coast", "british coast",
                        "uk coast", "english coast",
                        "belgium", "belgian", "netherlands", "dutch")
  if (any(sapply(very_likely_keys, grepl, x = text, fixed = TRUE))) {
    return("Very likely")
  }

  # 1. Very unlikely (Definitively non-European)
  very_unlikely_keys <- c("pacific", "indian ocean", "hawaiian", "caribbean", "antarctic")
  if (any(sapply(very_unlikely_keys, grepl, x = text, fixed = TRUE))) {
    return("Very unlikely")
  }

  # 3. Likely
  # Round 45 (2026-09-30) addition: Germany, Denmark, Norway, Ireland and the
  # UK only partly or indirectly border the North Sea (unlike Belgium/
  # Netherlands above), so a bare country mention for these gets the softer
  # "Likely" tier rather than "Very likely". Real WoRMS_Distribution text
  # very often lists plain country names or "<Country> Exclusive Economic
  # Zone" phrasing rather than a named sea -- none of that was previously
  # recognized by any tier here at all, so these species fell straight
  # through to "Unknown" (or, worse, to "Very unlikely" via the ordering bug
  # above if an incidental far-flung locality was also present).
  likely_keys <- c("northeast atlantic", "eastern atlantic", "celtic sea",
                   "english channel", "irish sea", "norwegian sea",
                   "barents sea", "bay of biscay", "british isles",
                   "germany", "german", "denmark", "danish",
                   "norway", "norwegian", "ireland", "irish", "united kingdom")
  if (any(sapply(likely_keys, grepl, x = text, fixed = TRUE))) {
    return("Likely")
  }

  # 4. Possible
  possible_keys <- c("european waters", "mediterranean", "baltic sea", "black sea", "adriatic")
  if (any(sapply(possible_keys, grepl, x = text, fixed = TRUE))) {
    return("Possible")
  }
  
  # 5. Unlikely (includes general Atlantic references)
  unlikely_keys <- c("atlantic")
  if (any(sapply(unlikely_keys, grepl, x = text, fixed = TRUE))) {
    return("Unlikely")
  }

  # FIXED (2026-09-16): this final fallthrough used to also return
  # "Unlikely" for any non-empty distribution string that didn't match ANY
  # of the keyword lists above -- indistinguishable from a real, confident
  # "Unlikely" match (the "atlantic" case just above). That silently
  # excluded these "we don't recognize this text at all" rows from the
  # OBIS/GBIF empirical gap-filler and run_enrichment_sanity_check()'s text
  # repair in mod_f_enrichment.R, both of which only rescue literal
  # "Unknown" results (see the matching fix in mod_f_enrichment.R's
  # gazetteer-side Occurrence_classification computation). Returning
  # "Unknown" here instead lets real empirical evidence -- or the sanity
  # check's own text recheck -- have the final say for taxa whose
  # distribution text simply isn't covered by this hand-picked keyword
  # list, while a genuine "atlantic"-only match above still stays a real
  # "Unlikely".
  return("Unknown")
}


# ---- Species-specific expert override (Round 46, 2026-09-30) -------------
# Per explicit user decision: some species have a domain-expert-known
# correct classification that the automated pipeline (text keywords +
# OBIS/GBIF) cannot reach on its own -- e.g. Acartia (Acartiura) hudsonica,
# which the user confirmed should be "Very likely" despite showing no North
# Sea evidence in either WoRMS distribution text or live OBIS/GBIF queries
# (most plausibly explained by historical taxonomic conflation with Acartia
# clausi in European monitoring records -- see that entry's own Reason for
# the caveat that this is a proposed explanation, not a verified fact).
# This mechanism lets such corrections be recorded once, in a plain CSV file
# the user maintains directly (no R code required), rather than having them
# silently re-diverge from the algorithm on every run.
#
# File: config/species_overrides.csv, columns:
#   Species               -- matched case-insensitively/trimmed against
#                             whichever species-identity column the calling
#                             app uses (Consensus_Species here; "Species" in
#                             eDNAcheck's own copy of this mechanism)
#   Forced_classification -- must be one of the six standard tiers (Very
#                             likely / Likely / Possible / Unlikely / Very
#                             unlikely / Unknown); anything else is dropped
#                             with a logged warning, never silently applied
#   Reason                -- free text, carried through to the
#                             Occurrence_Override_Reason output column so
#                             the correction stays traceable, not just a
#                             silent number change
#
# A missing file, an empty file, or a file where every row fails validation
# is NOT an error -- this mechanism is opt-in. A row whose species has no
# match in the current run is simply never applied; it does not need to be
# removed from the CSV between runs on different datasets.
#
# Precedence: per explicit user decision, this is checked with the HIGHEST
# priority of any rule in the whole classification chain -- applied last,
# unconditionally, in mod_f_enrichment.R after both classification methods,
# the OBIS/GBIF gap-filler, AND the Round 44b non-marine hard override, so a
# matched species always wins regardless of what any other rule (including
# "nothing overrides this") concluded. This does NOT skip live OBIS/GBIF
# queries for an overridden species -- only the final Occurrence_classification
# value (and the two new Occurrence_Override_* columns) are affected.
SPECIES_OVERRIDES_FILE <- file.path("config", "species_overrides.csv")
SPECIES_OVERRIDE_VALID_TIERS <- c("Very likely", "Likely", "Possible",
                                   "Unlikely", "Very unlikely", "Unknown")

# Round 48 (2026-10-01): normalizes a scientific name for override matching
# by removing a parenthetical subgenus token -- e.g. "Acartia (Acartiura)
# hudsonica" -> "Acartia hudsonica". Found via direct user testing
# ("Acartia (Acartiura) hudsonica" / "Oikopleura (Vexillaria) dioica" --
# species whose accepted WoRMS name includes a subgenus): the Round 46
# species-override mechanism matched Species_key with a plain
# tolower(trimws(...)) exact-string comparison, so the Acartia hudsonica
# override added in Round 46 (Species column: "Acartia hudsonica", no
# subgenus) silently never matched, because this engagement's own earlier
# real run already confirmed WoRMS returns "Acartia (Acartiura) hudsonica"
# as the matched name for that exact species -- the override was dead code
# from the moment it was added. Stripping the parenthetical on BOTH sides of
# the match (here and in apply_species_overrides() below) makes the match
# work regardless of whether either side happens to include subgenus
# notation, instead of requiring them to agree by coincidence.
strip_subgenus_for_match <- function(x) {
  x <- gsub("\\s*\\([^)]*\\)\\s*", " ", x)
  trimws(gsub("\\s+", " ", x))
}

load_species_overrides <- function(logger = message) {
  empty <- data.frame(Species_key = character(0),
                       Forced_classification = character(0),
                       Reason = character(0),
                       stringsAsFactors = FALSE)
  if (!file.exists(SPECIES_OVERRIDES_FILE)) return(empty)

  df <- tryCatch(
    read_csv(SPECIES_OVERRIDES_FILE, show_col_types = FALSE,
             col_types = cols(.default = "c")),
    error = function(e) {
      logger(sprintf("[SpeciesOverride] Failed to read %s: %s -- overrides disabled for this run.",
                      SPECIES_OVERRIDES_FILE, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(df)) return(empty)
  if (!all(c("Species", "Forced_classification") %in% names(df))) {
    logger(sprintf("[SpeciesOverride] %s is missing required column(s) (need at least 'Species' and 'Forced_classification') -- overrides disabled for this run.",
                    SPECIES_OVERRIDES_FILE))
    return(empty)
  }
  if (!"Reason" %in% names(df)) df$Reason <- NA_character_

  df <- df[!is.na(df$Species) & nzchar(trimws(df$Species)), , drop = FALSE]
  if (nrow(df) == 0) return(empty)
  bad_tier <- !df$Forced_classification %in% SPECIES_OVERRIDE_VALID_TIERS
  if (any(bad_tier)) {
    logger(sprintf("[SpeciesOverride] Dropping %d row(s) with an unrecognized Forced_classification (must be one of: %s): %s",
                    sum(bad_tier), paste(SPECIES_OVERRIDE_VALID_TIERS, collapse = ", "),
                    paste(df$Species[bad_tier], collapse = "; ")))
    df <- df[!bad_tier, , drop = FALSE]
  }
  if (nrow(df) == 0) return(empty)

  data.frame(
    Species_key = tolower(strip_subgenus_for_match(df$Species)),
    Forced_classification = df$Forced_classification,
    Reason = ifelse(is.na(df$Reason) | !nzchar(trimws(df$Reason)),
                     "Expert override (no reason given)", df$Reason),
    stringsAsFactors = FALSE
  )
}

apply_species_overrides <- function(df_with_worms, logger = message) {
  df_with_worms$Occurrence_Override_Applied <- FALSE
  df_with_worms$Occurrence_Override_Reason <- NA_character_

  species_col <- if ("Consensus_Species" %in% names(df_with_worms)) "Consensus_Species" else "Species"
  if (!species_col %in% names(df_with_worms)) return(df_with_worms)

  overrides <- load_species_overrides(logger = logger)
  if (nrow(overrides) == 0) return(df_with_worms)

  # Round 48: normalize the incoming name the same way the override's own
  # key was normalized above, so "Genus (Subgenus) species" and
  # "Genus species" match each other regardless of which form either side
  # happens to use.
  key <- tolower(strip_subgenus_for_match(df_with_worms[[species_col]]))
  match_idx <- match(key, overrides$Species_key)
  matched <- !is.na(match_idx)
  if (!any(matched)) return(df_with_worms)

  df_with_worms$Occurrence_classification[matched] <- overrides$Forced_classification[match_idx[matched]]
  df_with_worms$Occurrence_Override_Applied[matched] <- TRUE
  df_with_worms$Occurrence_Override_Reason[matched] <- overrides$Reason[match_idx[matched]]

  logger(sprintf("[SpeciesOverride] Applied %d expert override(s): %s",
                  sum(matched),
                  paste(sprintf("%s -> %s", df_with_worms[[species_col]][matched],
                                 overrides$Forced_classification[match_idx[matched]]),
                        collapse = "; ")))
  df_with_worms
}


# ---- Species-specific trait (functional-group) expert override (Round 51,
# 2026-10-02) -----------------------------------------------------------
# Same rationale and architecture as the occurrence-classification override
# above (config/species_overrides.csv / load_species_overrides() /
# apply_species_overrides()), applied to Group1/Group2/TaxGroup instead:
# some species have a domain-expert-known correct functional group that the
# automated cascade (live WoRMS attributes, the hand-curated
# Phylum/Class/Order lookup table, and the external MarineSpeciesTraits DB,
# in that priority order -- see mod_f_enrichment.R) doesn't reach on its
# own for that one species, even when it reaches the right answer for every
# close relative. Motivating case: *Pisione puzae*, which the user
# confirmed is meroplanktonic (planktonic larva, benthic adult) exactly
# like its sister species *Pisione remota* -- both Pisionidae, both should
# land on Group1="meroplankton"/Group2="benthic invertebrate" via this
# file's own Annelida/Polychaeta lookup entry above, but the user's actual
# exported run shows *Pisione puzae* landing on "holoplankton" via the
# external DB fallback instead, while its sister species lands correctly --
# i.e. whatever is supplying data for this one species specifically (most
# likely the external trait DB's own species-level entry, not a code path
# shared with its sister species) is simply wrong or incomplete for it.
#
# File: config/trait_overrides.csv, columns:
#   Species  -- matched case-insensitively/trimmed, subgenus-stripped, same
#               as the occurrence override above (reuses
#               strip_subgenus_for_match())
#   Group1, Group2 -- forced values; TaxGroup is optional (may be blank to
#               leave whatever the automated cascade already produced)
#   Reason   -- free text, carried through to Trait_Override_Reason
#
# A missing/empty file, or a row missing Species or Group1, is not an error
# -- opt-in, exactly like the occurrence override. Applied with the HIGHEST
# precedence of this cascade -- last, unconditionally, in mod_f_enrichment.R
# after Priority 1/2/2B/3 and the *_Validated flags are all computed, so a
# matched species' Group1/Group2/TaxGroup always ships as the override
# regardless of what the automated cascade concluded. The *_Validated flags
# themselves are left untouched -- they still reflect whether the DB agreed
# with what the AUTOMATED cascade computed before the override, which
# remains separately useful information (it's exactly the DB's answer here
# that prompted this override to exist).
TRAIT_OVERRIDES_FILE <- file.path("config", "trait_overrides.csv")

load_trait_overrides <- function(logger = message) {
  empty <- data.frame(Species_key = character(0), Group1 = character(0),
                       Group2 = character(0), TaxGroup = character(0),
                       Reason = character(0), stringsAsFactors = FALSE)
  if (!file.exists(TRAIT_OVERRIDES_FILE)) return(empty)

  df <- tryCatch(
    read_csv(TRAIT_OVERRIDES_FILE, show_col_types = FALSE,
             col_types = cols(.default = "c")),
    error = function(e) {
      logger(sprintf("[TraitOverride] Failed to read %s: %s -- overrides disabled for this run.",
                      TRAIT_OVERRIDES_FILE, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(df)) return(empty)
  if (!all(c("Species", "Group1") %in% names(df))) {
    logger(sprintf("[TraitOverride] %s is missing required column(s) (need at least 'Species' and 'Group1') -- overrides disabled for this run.",
                    TRAIT_OVERRIDES_FILE))
    return(empty)
  }
  if (!"Group2" %in% names(df)) df$Group2 <- NA_character_
  if (!"TaxGroup" %in% names(df)) df$TaxGroup <- NA_character_
  if (!"Reason" %in% names(df)) df$Reason <- NA_character_

  df <- df[!is.na(df$Species) & nzchar(trimws(df$Species)) &
           !is.na(df$Group1) & nzchar(trimws(df$Group1)), , drop = FALSE]
  if (nrow(df) == 0) return(empty)

  data.frame(
    Species_key = tolower(strip_subgenus_for_match(df$Species)),
    Group1 = df$Group1,
    Group2 = ifelse(is.na(df$Group2) | !nzchar(trimws(df$Group2)), NA_character_, df$Group2),
    TaxGroup = ifelse(is.na(df$TaxGroup) | !nzchar(trimws(df$TaxGroup)), NA_character_, df$TaxGroup),
    Reason = ifelse(is.na(df$Reason) | !nzchar(trimws(df$Reason)),
                     "Expert trait override (no reason given)", df$Reason),
    stringsAsFactors = FALSE
  )
}

apply_trait_overrides <- function(df_with_worms, logger = message) {
  df_with_worms$Trait_Override_Applied <- FALSE
  df_with_worms$Trait_Override_Reason <- NA_character_

  species_col <- if ("Consensus_Species" %in% names(df_with_worms)) "Consensus_Species" else "Species"
  if (!species_col %in% names(df_with_worms)) return(df_with_worms)

  overrides <- load_trait_overrides(logger = logger)
  if (nrow(overrides) == 0) return(df_with_worms)

  key <- tolower(strip_subgenus_for_match(df_with_worms[[species_col]]))
  match_idx <- match(key, overrides$Species_key)
  matched <- !is.na(match_idx)
  if (!any(matched)) return(df_with_worms)

  df_with_worms$Group1[matched] <- overrides$Group1[match_idx[matched]]
  g2 <- overrides$Group2[match_idx[matched]]
  has_g2 <- !is.na(g2)
  df_with_worms$Group2[matched][has_g2] <- g2[has_g2]
  tg <- overrides$TaxGroup[match_idx[matched]]
  has_tg <- !is.na(tg)
  df_with_worms$TaxGroup[matched][has_tg] <- tg[has_tg]
  df_with_worms$Trait_Override_Applied[matched] <- TRUE
  df_with_worms$Trait_Override_Reason[matched] <- overrides$Reason[match_idx[matched]]

  logger(sprintf("[TraitOverride] Applied %d expert trait override(s): %s",
                  sum(matched),
                  paste(sprintf("%s -> Group1=%s", df_with_worms[[species_col]][matched],
                                 overrides$Group1[match_idx[matched]]),
                        collapse = "; ")))
  df_with_worms
}


# Round 40 (2026-09-29) addition, updated Round 41 (2026-09-29) per explicit
# user follow-up ("someone use a different region (e.g. mediterranean)
# should have similar outcome"): a same-continent-vs-different-continent
# read on the WoRMS distribution text, used ONLY to refine the OBIS/GBIF
# double-negative rule in the classification chain (mod_f_enrichment.R) --
# classify_occurrence_by_text() above is unchanged and still drives the
# "Very likely"/"Likely"/"Possible" tiers exactly as before. Keyword-based
# and offline, consistent with the rest of this text classifier (and with
# Round 39's removal of gazetteer's live API-backed hop-distance for being
# too slow to be worth it).
#
# Round 40's first cut hardcoded "same continent" to mean "Europe", because
# reference_name is always North Sea in this app's actual use so far. But
# reference_mrgid/reference_name are picked dynamically from a live
# gazetteer search (see mod_f_enrichment.R around "rv$reference_mrgid <-"),
# so the app genuinely supports any reference region -- and a hardcoded
# Europe-only table would silently misclassify same/different-continent for
# any other one (e.g. it would call a Caribbean mention "different
# continent" even if the chosen reference region were the Gulf of Mexico).
# Round 41 replaces the two flat same/different keyword lists with
# SEA_TO_CONTINENTS, a world sea/ocean/region -> continent(s) table, and
# looks up BOTH reference_name's own continent(s) and the mentioned
# locality's continent(s) in it, so the same logic gives a correct answer
# for any reference region picked via the gazetteer search, not just the
# North Sea. Broad ocean-basin names (bare "atlantic", "pacific", etc.) map
# to the SET of continents that basin borders, since e.g. "Atlantic" alone
# doesn't distinguish European Atlantic from American Atlantic -- the
# same/different comparison below still works off a multi-continent set
# (any continent shared between the two = "same").
#
# This is a best-effort, hand-built table (consistent with the philosophy
# of classify_occurrence_by_text() itself) -- good global coverage of major
# named seas, not an exhaustive gazetteer. A locality this table doesn't
# recognize at all falls back to NA, same as before Round 41.
SEA_TO_CONTINENTS <- list(
  # Europe
  "north sea" = c("Europe"), "german bight" = c("Europe"),
  "baltic sea" = c("Europe"), "wadden sea" = c("Europe"),
  "english channel" = c("Europe"), "irish sea" = c("Europe"),
  "celtic sea" = c("Europe"), "norwegian sea" = c("Europe"),
  "barents sea" = c("Europe"), "skagerrak" = c("Europe"),
  "kattegat" = c("Europe"), "bay of biscay" = c("Europe"),
  "adriatic" = c("Europe"), "ligurian sea" = c("Europe"),
  "tyrrhenian sea" = c("Europe"), "ionian sea" = c("Europe"),
  "white sea" = c("Europe"), "iceland" = c("Europe"), "faroe" = c("Europe"),
  "british isles" = c("Europe"), "dogger bank" = c("Europe"),
  "helgoland" = c("Europe"), "azov" = c("Europe"),
  "northeast atlantic" = c("Europe"), "eastern atlantic" = c("Europe", "Africa"),
  # Europe/Africa/Asia border seas
  "mediterranean" = c("Europe", "Africa", "Asia"),
  "aegean" = c("Europe", "Asia"), "black sea" = c("Europe", "Asia"),
  # Africa
  "red sea" = c("Africa", "Asia"), "gulf of guinea" = c("Africa"),
  "mozambique channel" = c("Africa"), "agulhas" = c("Africa"),
  # Asia
  "persian gulf" = c("Asia"), "arabian sea" = c("Asia"),
  "bay of bengal" = c("Asia"), "south china sea" = c("Asia"),
  "east china sea" = c("Asia"), "yellow sea" = c("Asia"),
  "sea of japan" = c("Asia"), "sea of okhotsk" = c("Asia"),
  "andaman sea" = c("Asia"), "gulf of oman" = c("Asia"),
  "gulf of thailand" = c("Asia"), "philippine sea" = c("Asia"),
  "java sea" = c("Asia", "Oceania"), "coral triangle" = c("Asia", "Oceania"),
  "arafura sea" = c("Asia", "Oceania"),
  "bering sea" = c("Asia", "North America"),
  # North America
  "gulf of mexico" = c("North America"), "caribbean sea" = c("North America"),
  "caribbean" = c("North America"), "chesapeake bay" = c("North America"),
  "labrador sea" = c("North America"), "hudson bay" = c("North America"),
  "gulf of california" = c("North America"), "gulf of st lawrence" = c("North America"),
  "hawaiian" = c("North America", "Oceania"),
  # South America
  "rio de la plata" = c("South America"),
  # Oceania
  "coral sea" = c("Oceania"), "tasman sea" = c("Oceania"),
  "great barrier reef" = c("Oceania"), "australia" = c("Oceania"),
  # Polar
  "antarctic" = c("Antarctica"), "southern ocean" = c("Antarctica"),
  "arctic ocean" = c("Arctic"),
  # Broad, continent-spanning ocean basins (weaker signal, kept last so a
  # more specific match above -- e.g. "gulf of mexico" -- wins first when
  # both appear; text_to_continents() below unions every match anyway, so
  # order doesn't actually change the result, just readability)
  "pacific ocean" = c("Asia", "North America", "South America", "Oceania"),
  "pacific" = c("Asia", "North America", "South America", "Oceania"),
  "indian ocean" = c("Africa", "Asia", "Oceania"),
  "atlantic ocean" = c("Europe", "Africa", "North America", "South America"),
  "atlantic" = c("Europe", "Africa", "North America", "South America"),
  "north pacific" = c("Asia", "North America"),
  "south pacific" = c("South America", "Oceania", "Asia"),
  "western atlantic" = c("North America", "South America"),
  "south america" = c("South America"), "north america" = c("North America"),
  "asia" = c("Asia"), "africa" = c("Africa")
)

# Given already-lowercased text, returns the UNION of continents for every
# SEA_TO_CONTINENTS name found in it (character(0) if none match).
text_to_continents <- function(text_lower) {
  matched <- character(0)
  for (nm in names(SEA_TO_CONTINENTS)) {
    if (grepl(nm, text_lower, fixed = TRUE)) {
      matched <- union(matched, SEA_TO_CONTINENTS[[nm]])
    }
  }
  matched
}

# Returns "same" if the text names a real, non-target-region locality that
# shares at least one continent with reference_name, "different" if it
# names a locality with NO continent in common with reference_name, or NA
# if the text mentions the target region itself, or if either reference_name
# or the text's locality can't be placed in SEA_TO_CONTINENTS at all.
text_locality_continent <- function(dist_str, reference_name) {
  if (is.na(dist_str) || !nzchar(dist_str)) return(NA_character_)
  text <- tolower(dist_str)
  ref <- tolower(reference_name)
  if (grepl(ref, text, fixed = TRUE)) return(NA_character_)

  ref_continents <- text_to_continents(ref)
  loc_continents <- text_to_continents(text)
  if (length(ref_continents) == 0 || length(loc_continents) == 0) return(NA_character_)

  if (length(intersect(ref_continents, loc_continents)) > 0) "same" else "different"
}


# ------------------------------------------------------------------------
# MARINE REGIONS GAZETTEER CLIENT — relations-based occurrence classification
#
# REPLACES the old classify_occurrence() keyword-matching approach. Rather
# than scanning WoRMS's free-text distribution string for hand-picked place
# names (which is only ever as good as the keyword list, and can't express
# real geographic relationships — see the back-and-forth that led here:
# missing North Sea national-waters phrasing, Barents Sea/Atlantic Ocean
# mis-tiered relative to each other, etc.), this asks the Marine Regions
# gazetteer directly: "how many real, named hops separate this distribution
# locality from the North Sea itself?"
#
# Each WoRMS distribution record already carries a locationID — an MRGID
# (Marine Regions Geographic IDentifier) URI identifying that locality in
# the Marine Regions gazetteer. The gazetteer stores actual relationships
# between places (partof, partlypartof, adjacentto, ...) — e.g. the Belgian
# Exclusive Economic Zone is recorded as partof the North Sea AND partof
# Belgium. Walking these relations outward from a distribution locality,
# breadth-first, and checking at each hop whether the North Sea (MRGID
# 2350) has been reached gives a real, data-source-asserted distance
# measure — not a straight-line km figure (which would be biologically
# arbitrary for marine species, since open water connectivity doesn't
# follow straight lines) and not a hand-picked keyword list.
#
# NOTE: no R environment was available to execute/test this against the
# live API while building it — the REST endpoint shapes below were
# confirmed via documentation and third-party client implementations
# (see CHANGELOG), but this needs a real test run before being trusted.
# ------------------------------------------------------------------------

DEFAULT_REFERENCE_MRGID <- 2350L   # IHO Sea Area "North Sea"
DEFAULT_REFERENCE_NAME <- "North Sea"

# Round 65 (2026-10-22): one-click reference-region presets for Module F's
# occurrence classification, so the same species list can be classified
# against two stated North Sea definitions and the boundary cited in a
# methods section:
#   iho_ns   - IHO "Limits of Oceans and Seas" North Sea (Marine Regions MRGID
#              2350; Skagerrak and Kattegat are separate IHO areas) -- the
#              default, unchanged behaviour
#   ices_gns - ICES "Greater North Sea" ecoregion (Marine Regions MRGID 36317,
#              placeType "ICES Ecoregion"; includes Skagerrak and Kattegat, and
#              its outline extends into the English Channel to 5 W / 48 N).
#              Its outline comes through the existing dynamic-geometry pipeline
#              (get_reference_geometry_wkt()), the same path any searched region
#              already takes; MRGID and geometry availability confirmed live by
#              the user on 2026-10-22.
# The free gazetteer search is kept ("custom").
REFERENCE_REGION_PRESETS <- list(
  iho_ns   = list(mrgid = 2350L,  name = "North Sea",
                  label = "IHO North Sea (MRGID 2350)"),
  ices_gns = list(mrgid = 36317L, name = "Greater North Sea",
                  label = "ICES Greater North Sea ecoregion (MRGID 36317)")
)
reference_preset_for_mrgid <- function(mrgid) {
  hit <- names(REFERENCE_REGION_PRESETS)[vapply(REFERENCE_REGION_PRESETS, function(p) identical(p$mrgid, as.integer(mrgid)), logical(1))]
  if (length(hit)) hit[1] else "custom"
}
# Reference regions that contain the German Bight and therefore get the
# Round 43 German Bight sub-region tiering (previously only MRGID 2350).
GERMAN_BIGHT_TIERING_MRGIDS <- c(2350L, 36317L)
MAX_GAZETTEER_HOPS <- 5L  # Increased to 5 to allow slightly deeper paths

marineregions_base_url <- "https://www.marineregions.org/rest"

# Search the gazetteer BY NAME — this is what makes the occurrence
# classification generalizable beyond the North Sea: a user working in the
# Mediterranean, the Gulf of Mexico, or anywhere else can type their own
# region's name here instead of being stuck with a hardcoded reference
# region. Returns a tibble of matching gazetteer records (MRGID,
# preferredGazetteerName, placeType, latitude/longitude, etc.), ranked
# roughly by relevance from the API itself — most specific/well-known
# regions (e.g. "North Sea", "Mediterranean Sea") typically return their
# IHO Sea Area record first, but ambiguous names can return many results,
# so callers should show the user a pick-list rather than blindly taking
# the first result.
#
# `like = TRUE` wraps the search term in SQL LIKE wildcards (partial
# match), `fuzzy = TRUE` additionally allows near-misses (Levenshtein
# distance) — both default TRUE so a slightly-off or partial name (e.g.
# "Mediteranean" or just "Mediterranean") still finds the right record.
gazetteer_search_by_name <- function(name, like = TRUE, fuzzy = TRUE, timeout_s = 8) {
  if (is.na(name) || trimws(name) == "") return(tibble())

  # Spaces in the name need to become "+" before URL-encoding — confirmed
  # necessary from real-world reports of space-encoding bugs with this
  # exact endpoint on some platforms (see CHANGELOG); mirrors how the
  # mregions2 R package's own internals build this URL.
  name_for_url <- utils::URLencode(gsub(" ", "+", trimws(name), fixed = TRUE))
  url <- sprintf("%s/getGazetteerRecordsByName.json/%s/", marineregions_base_url, name_for_url)

  result <- tryCatch({
    resp <- request(url) %>%
      req_url_query(like = tolower(as.character(like)), fuzzy = tolower(as.character(fuzzy))) %>%
      req_timeout(timeout_s) %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()

    if (resp_status(resp) != 200) return(tibble())

    parsed <- resp_body_json(resp, simplifyVector = TRUE)
    if (is.null(parsed) || length(parsed) == 0) return(tibble())
    as_tibble(parsed)
  }, error = function(e) tibble())

  result
}

# Low-level REST call: get all gazetteer records related to `mrgid` in the
# given `direction` ("upper" = parents/containers, "lower" = children/contents,
# "both") and `type` (e.g. "partof", "adjacentto", "all"). Returns a tibble
# of related records (each with its own MRGID, placeType, name, etc.), or
# an empty tibble on any error/empty result — callers should treat "no
# relations found" and "request failed" the same way (just nothing to
# explore further down this branch), not as a hard error.
# New cache for MRGIDs that fail (404s), to skip them in the future
.gazetteer_blacklist_env <- new.env(parent = emptyenv())

gazetteer_relations <- function(mrgid, direction = "both", type = "all", timeout_s = 8) {
  if (is.na(mrgid)) return(tibble())
  
  # Check blacklist first
  blacklist_key <- as.character(mrgid)
  if (exists(blacklist_key, envir = .gazetteer_blacklist_env, inherits = FALSE)) {
    return(tibble())
  }

  url <- sprintf("%s/getGazetteerRelationsByMRGID.json/%d/", marineregions_base_url, as.integer(mrgid))
  
  result <- tibble()
  max_retries <- 3
  
  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      resp <- request(url) %>%
        req_url_query(direction = direction, type = type) %>%
        req_timeout(timeout_s) %>%
        req_error(is_error = function(resp) FALSE) %>%
        req_perform()

      if (resp_status(resp) == 404) {
          assign(blacklist_key, TRUE, envir = .gazetteer_blacklist_env)
          return(tibble())
      }
      
      if (resp_status(resp) != 200) {
        if (attempt < max_retries) {
          Sys.sleep(attempt * 0.5)
          next
        }
        return(tibble())
      }

      parsed <- resp_body_json(resp, simplifyVector = TRUE)
      if (is.null(parsed) || length(parsed) == 0) return(tibble())
      as_tibble(parsed)
    }, error = function(e) {
      if (attempt < max_retries) {
        Sys.sleep(attempt * 0.5)
        return(NULL)
      }
      tibble()
    })
    
    if (!is.null(result)) break
  }
  
  if (is.null(result)) result <- tibble()
  
  Sys.sleep(0.05) # Tiny delay
  result
}

# Cache for gazetteer_relations() calls, keyed on (mrgid, direction, type) —
# each distinct combination is a real network call, and the same MRGIDs
# recur constantly across species (most North Sea species' distributions
# eventually touch the same handful of EEZs/sub-basins), so caching this
# within a session meaningfully cuts down on repeated lookups.
# FIXED (round 8, 2026-09-09): this used to re-run
# ".gazetteer_cache_env <- new.env(parent = emptyenv())" here, AFTER
# load_gazetteer_caches() had already loaded gazetteer_cache.rds into that
# same-named env near the top of this file (line ~93). Since a bare
# "<- new.env()" creates a brand-new, empty environment and rebinds the
# name to it, this silently discarded every previously-cached gazetteer
# relation on every single app start -- the persistence the comment above
# this block describes never actually took effect. Removed; the env
# created (and populated from disk) earlier in this file is reused as-is.
.evidence_cache_env <- new.env(parent = emptyenv())
# OBIS/GBIF presence-check cache: was in-memory only (see save_gazetteer_caches()
# above for the write side). Loaded here, right after the env is created, since
# load_gazetteer_caches() runs earlier in this file, before this env exists.
if (file.exists(file.path(cache_dir, "evidence_cache.rds"))) {
  evidence_cache_data <- tryCatch(readRDS(file.path(cache_dir, "evidence_cache.rds")), error = function(e) NULL)
  if (!is.null(evidence_cache_data)) {
    list2env(evidence_cache_data, envir = .evidence_cache_env)
    message(sprintf("OBIS/GBIF evidence cache loaded (%d entries).", length(evidence_cache_data)))
  }
}

gazetteer_relations_cached <- function(mrgid, direction = "both", type = "all") {
  if (is.na(mrgid)) return(tibble())
  key <- paste(mrgid, direction, type, sep = "\u241F")
  
  # Check blacklist
  if (exists(as.character(mrgid), envir = .gazetteer_blacklist_env, inherits = FALSE)) {
    return(tibble())
  }
  
  if (exists(key, envir = .gazetteer_cache_env, inherits = FALSE)) {
    return(get(key, envir = .gazetteer_cache_env, inherits = FALSE))
  }
  
  res <- gazetteer_relations(mrgid, direction, type)
  
  # If result is empty, blacklist the mrgid to prevent future 404s/empty lookups
  if (nrow(res) == 0) {
      assign(as.character(mrgid), TRUE, envir = .gazetteer_blacklist_env)
  } else {
      assign(key, res, envir = .gazetteer_cache_env)
  }
  res
}

# Breadth-first search outward from `start_mrgid`, looking for
# `target_mrgid` (the North Sea), up to `max_hops` steps. Each hop checks
# BOTH "partof"/"partlypartof" (containment, either direction) and
# "adjacentto" (bordering) relations — these are the relation types that
# correspond to genuine geographic closeness; "similarto",
# "administrativepartof", and "influencedby" are deliberately excluded,
# since they describe naming/administrative/indirect links rather than
# physical proximity.
#
# direction = "both" is used deliberately at every hop (not just "upper"):
# whether the correct path to the North Sea goes via PARENTS or CHILDREN
# depends on whether the starting locality sits above or below the North
# Sea in the gazetteer hierarchy, which isn't known in advance. E.g. a
# species recorded in "Atlantic Ocean" reaches the North Sea by walking
# DOWN (children) the hierarchy; a species recorded at a small named
# sandbank reaches it by walking UP (parents).
#
# SAFETY CAP: searching "lower" (children) from a broad node (like "Atlantic
# Ocean" itself) can branch heavily — the Belgian EEZ alone has 37 related
# records, and ocean-basin-level nodes likely have far more. Without a
# bound, a single broad starting locality could trigger an explosively
# growing number of API calls across hops. max_total_nodes caps the total
# number of distinct MRGIDs explored across the ENTIRE search (not just
# per hop), so one pathologically broad starting point can't make a single
# species' classification take an unbounded amount of time/requests.
#
# Returns the integer hop count at which target_mrgid was found (0 if
# start_mrgid IS the target), or NA_integer_ if not found within max_hops
# OR if the node-exploration cap was hit first.
# Debugging: check gazetteer_hops_to_target behavior
# Max hops back to 4, safety cap 1000 for deeper walking.
# FIXED (round 8, 2026-09-09): this block used to re-run
# ".gazetteer_hops_cache_env <- new.env(parent = emptyenv())" TWICE here
# (an apparent copy-paste duplicate), after load_gazetteer_caches() had
# already loaded gazetteer_hops_cache.rds into that same-named env near the
# top of this file. Same bug as the .gazetteer_cache_env fix above: each
# "<- new.env()" call silently threw away every previously-accumulated hop
# distance, so "persistent across calls within the session" (and across
# sessions, via save_gazetteer_caches()/onStop()) never actually held. Both
# redundant re-declarations removed; the env populated from disk earlier in
# this file is reused as-is.

# Broad MRGIDs that cause traversal timeouts - return NA immediately
.broad_nodes <- c(7130L, 7135L, 8766L, 8765L, 8762L, 8761L, 1905L, 14624L, 26584L, 8940L)

# ------------------------------------------------------------------------
# RESTORED (round 19, 2026-09-21): this app's own diagnosis doc
# (claude/worms_pipeline_app_v8_diagnosis.md, "Round 2", 2026-09-10) records
# that gazetteer_hops_to_target() below was already fixed for exactly this
# failure mode -- generic fan-out-based hub detection (3a) plus a
# deleted-record filter (3b) -- verified live against 5 real North Sea
# species and a second independent reference region, and even ported
# byte-for-byte to the sibling eDNAcheck_v2.0.0 app's helpers_pipeline.R the
# same day. That ported copy is still present and correct in eDNAcheck
# today. But the version of gazetteer_hops_to_target() actually on this
# machine's worms_pipeline_app_v8/global.R -- confirmed by staging the file
# fresh from disk this round -- has NONE of it: no fan-out threshold, no
# deleted-record filter, no cache-version marker, and .broad_nodes only
# ever checked against start_mrgid. This is a regression, not a new bug --
# somewhere between Round 2 and now, this file reverted to (or never
# received) the fix that was already written, tested, and documented.
#
# Rediscovered via a live, reproducible symptom: Euphausia brevis and
# Clausocalanus paululus (Gulf of Mexico species, MRGID 4288) were
# classified "Likely" for the North Sea via a reported 2-hop path, despite
# zero real OBIS/GBIF records there (confirmed live post-Round-17-fix via
# Occurrence_Evidence_Conflict = TRUE). Reading the app's own cached
# relations (cache/gazetteer_cache.rds) directly showed the path: 4288
# (Gulf of Mexico) -> 1912 (North Atlantic Ocean) -> 2350 (North Sea) --
# the SAME hub node (MRGID 1912) Round 2 originally found and fixed
# generically rather than by hardcoding. A full sweep of all 452 localities
# already cached against the North Sea found 85 (~19%) whose hop count
# changes once the restored fix is applied -- nearly all far-flung Gulf of
# Mexico/Caribbean/wider Atlantic localities shortcutting through 1912, not
# a two-species edge case, matching the scale Round 2 already anticipated.
#
# This is a straight restoration of the code Round 2 already wrote (see the
# diagnosis doc for the original derivation), not a new design decision.
#
# IMPORTANT: restoring this code alone does NOT change already-cached
# results on disk. gazetteer_hops_to_target() checks
# cache/gazetteer_hops_cache.rds BEFORE running the BFS, and this restored
# version's cache key includes GAZETTEER_BFS_LOGIC_VERSION specifically so
# it can no longer return a value memoized under the old, un-fixed logic --
# every affected lookup will be recomputed fresh (live API calls) the first
# time it's needed after this change, rather than requiring a manual cache
# wipe.
GAZETTEER_HUB_FANOUT_THRESHOLD <- 60L
GAZETTEER_BFS_LOGIC_VERSION <- "v2"

.gazetteer_fanout <- function(mrgid) {
  rel <- gazetteer_relations_cached(mrgid, direction = "both", type = "all")
  if (nrow(rel) == 0 || !"MRGID" %in% names(rel)) return(0L)
  if ("type" %in% names(rel)) rel <- rel[rel$type %in% c("partof", "partlypartof", "adjacentto"), ]
  if ("status" %in% names(rel)) rel <- rel[is.na(rel$status) | rel$status != "deleted", ]
  length(unique(suppressWarnings(as.integer(rel$MRGID))))
}

gazetteer_hops_to_target <- function(start_mrgid, target_mrgid = DEFAULT_REFERENCE_MRGID,
                                      max_hops = 4L,
                                      max_total_nodes = 1000L,
                                      hub_fanout_threshold = GAZETTEER_HUB_FANOUT_THRESHOLD) {
  if (is.na(start_mrgid)) return(NA_integer_)
  if (start_mrgid %in% .broad_nodes) return(NA_integer_)
  if (start_mrgid == target_mrgid) return(0L)


  # Check cache first. Key includes hub_fanout_threshold and
  # GAZETTEER_BFS_LOGIC_VERSION so a stale hop-distance result computed
  # under the pre-fix logic and persisted to disk from a previous session
  # is never returned as if it still reflected current logic.
  cache_key <- paste(start_mrgid, target_mrgid, max_hops, max_total_nodes, hub_fanout_threshold,
                      GAZETTEER_BFS_LOGIC_VERSION, sep = "\u241F")
  if (exists(cache_key, envir = .gazetteer_hops_cache_env, inherits = FALSE)) {
    return(get(cache_key, envir = .gazetteer_hops_cache_env, inherits = FALSE))
  }

  # NOTE: querying per relation-type (rather than once with type = "all")
  # is deliberate -- the Marine Regions REST API's response for a
  # type = "all" query does not include a per-row relation-type field, so a
  # client-side filter on "type" can never distinguish partof/adjacentto
  # from anything else (confirmed directly from this app's own cached
  # relation tibbles: no "type" column is ever present). Querying with an
  # explicit type value restricts the result server-side instead.
  relation_types <- c("partof", "partlypartof", "adjacentto")
  visited <- c(start_mrgid)
  frontier <- c(start_mrgid)

  for (hop in seq_len(max_hops)) {
    next_frontier <- integer(0)

    for (m in frontier) {
      for (rtype in relation_types) {
        rel <- gazetteer_relations_cached(m, direction = "both", type = rtype)
        if (nrow(rel) == 0 || !"MRGID" %in% names(rel)) next

        # Deleted-record fix (Round 2, 3b): a retired Marine Regions record
        # can be merged, purely by name coincidence, into an unrelated
        # same-named locality thousands of km away (confirmed real case:
        # "St. Andrews Bay", Scotland, status="deleted", merged into "Saint
        # Andrews Bay", Florida). Any relation sourced from a deleted
        # record is excluded entirely.
        if ("status" %in% names(rel)) {
          rel <- rel[is.na(rel$status) | rel$status != "deleted", ]
          if (nrow(rel) == 0) next
        }

        related_ids <- suppressWarnings(as.integer(rel$MRGID))
        related_ids <- related_ids[!is.na(related_ids)]

        # Direct-hit check happens BEFORE any hub filtering below -- a
        # node's own size never blocks a genuine direct adjacency to the
        # target.
        if (target_mrgid %in% related_ids) {
            assign(cache_key, hop, envir = .gazetteer_hops_cache_env)
            return(hop)
        }

        new_ids <- setdiff(related_ids, visited)
        if (length(new_ids) > 0) {
          # Hub-node fix (Round 2, 3a): a whole-ocean-basin node (e.g.
          # MRGID 1912, "North Atlantic Ocean") has far more relations than
          # a real, specific locality ever would, and using it to expand
          # the search bridges otherwise-unrelated regions in just 1-2
          # further hops. Generic by design (fan-out threshold, not another
          # hardcoded MRGID) so it protects against ANY oversized node, not
          # just 1912, and against whatever reference region is configured,
          # not just the North Sea. A node this large can still be
          # *reached* and counted (see the direct-hit check above) -- it
          # just never gets used to jump onward from.
          keep <- vapply(new_ids, function(x) .gazetteer_fanout(x) <= hub_fanout_threshold, logical(1))
          new_ids <- new_ids[keep]
        }
        next_frontier <- c(next_frontier, new_ids)

        if (length(visited) + length(next_frontier) >= max_total_nodes) {
             assign(cache_key, NA_integer_, envir = .gazetteer_hops_cache_env)
             return(NA_integer_)
        }
      }
    }

    next_frontier <- unique(next_frontier)
    if (length(next_frontier) == 0) break

    remaining_budget <- max_total_nodes - length(visited)
    if (remaining_budget <= 0) {
        assign(cache_key, NA_integer_, envir = .gazetteer_hops_cache_env) # Store NA (budget zero)
        return(NA_integer_)
    }
    if (length(next_frontier) > remaining_budget) {
      next_frontier <- next_frontier[seq_len(remaining_budget)]
    }

    visited <- c(visited, next_frontier)
    frontier <- next_frontier
  }

  # Store NA result in cache (not found within max_hops)
  assign(cache_key, NA_integer_, envir = .gazetteer_hops_cache_env)
  NA_integer_
}
# Mapping of hop count to the tier label.
#
# FIXED: the previous version of this function widened "Likely" from 1 hop to
# 1-2 hops (and kept "Possible" at 3-4 hops) purely because the "Unlikely"
# bucket looked too large in one run (see prior comment here, removed) - not
# because anyone checked whether those extra 1-2/3-4-hop paths were actually
# geographically meaningful. That let species several real gazetteer hops away
# from the reference region (e.g. reached only via a broad ocean-basin node)
# clear the "Likely"/"Possible" bar. Restored to the originally documented,
# narrower tier table (README "Occurrence classification (v1.7+)" section):
# 0-1 hop = Very likely, 2 = Likely, 3 = Possible, 4+/no path = Unlikely.
hops_to_occurrence_tier <- function(hops) {
  if (is.na(hops)) return("Unlikely")
  if (hops <= 1) return("Very likely")
  if (hops == 2) return("Likely")
  if (hops == 3) return("Possible")
  "Unlikely"
}

# Check if locality coordinates fall within region bounding box
# Returns TRUE if inside, FALSE otherwise
is_locality_in_region <- function(locality_mrgid, region_mrgid) {
    # 1. Fetch data for both
    # We use relations to get coordinates (it's the fastest way to get metadata)
    # Or we can fetch details directly. Relations table has lat/lon.
    loc_rel <- gazetteer_relations_cached(locality_mrgid, direction = "both", type = "all")
    reg_rel <- gazetteer_relations_cached(region_mrgid, direction = "both", type = "all")
    
    # 2. Extract bounding box for both
    # For a locality, we can treat its lat/lon as a point or check its box.
    # Gazetteer relations table has min/max lat/lon.
    # Let's extract the first row as the definition of the locality
    if (nrow(loc_rel) == 0 || nrow(reg_rel) == 0) return(FALSE)
    
    loc_box <- loc_rel[1, c("minLatitude", "maxLatitude", "minLongitude", "maxLongitude")]
    reg_box <- reg_rel[1, c("minLatitude", "maxLatitude", "minLongitude", "maxLongitude")]
    
    # Simple bounding box overlap (is there any intersection?)
    # Overlap logic: A overlaps B if (A.minLat <= B.maxLat AND A.maxLat >= B.minLat)
    # AND (A.minLon <= B.maxLon AND A.maxLon >= B.minLon)
    is_overlapping <- (loc_box$minLatitude <= reg_box$maxLatitude &&
                       loc_box$maxLatitude >= reg_box$minLatitude &&
                       loc_box$minLongitude <= reg_box$maxLongitude &&
                       loc_box$maxLongitude >= reg_box$minLongitude)
    
    return(!is.na(is_overlapping) && is_overlapping)
}

# Fetch full gazetteer record by MRGID
get_gazetteer_record_by_mrgid <- function(mrgid, timeout_s = 8) {
  if (is.na(mrgid)) return(NULL)
  url <- sprintf("%s/getGazetteerRecordByMRGID.json/%d/", marineregions_base_url, as.integer(mrgid))

  tryCatch({
    resp <- request(url) %>%
      req_timeout(timeout_s) %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()

    # Round 44f (2026-09-30) diagnostic: get_reference_bbox()'s [BboxDebug]
    # log could only report "returned NULL", lumping a non-200 status, an
    # empty/unparseable body, and a thrown exception into one message -- not
    # enough to tell which actually happened for MRGID 3427/29724 in a real
    # run where gazetteer_search_by_name() (a *different* endpoint) worked
    # fine, meaning network access itself isn't the problem. Surfaces the
    # actual HTTP status / parse outcome / exception text so the next run
    # pinpoints the real cause.
    if (resp_status(resp) != 200) {
      message(sprintf("[GazetteerDebug] getGazetteerRecordByMRGID(%s): HTTP %d (url: %s)",
                       as.character(mrgid), resp_status(resp), url))
      return(NULL)
    }

    parsed <- resp_body_json(resp, simplifyVector = TRUE)
    if (is.null(parsed) || length(parsed) == 0) {
      message(sprintf("[GazetteerDebug] getGazetteerRecordByMRGID(%s): HTTP 200 but response body was empty/unparseable (url: %s)",
                       as.character(mrgid), url))
      return(NULL)
    }
    # Round 44g (2026-09-30) ROOT-CAUSE fix, per the [GazetteerDebug] output
    # from a real run: MRGID 3427's record genuinely has minLatitude/
    # minLongitude/maxLatitude/maxLongitude fields, each JSON `null` (it's a
    # point+precision record, not a bounding-box one) -- but
    # resp_body_json(simplifyVector = TRUE) turns a JSON `null` into an R
    # NULL, not NA, and as_tibble() on a list containing an actual NULL
    # element throws ("All columns in a tibble must be vectors") rather than
    # treating it as a missing scalar. That exception was being caught by
    # this function's own tryCatch and silently turned into "get_gazetteer_
    # record_by_mrgid() returned NULL" -- which get_reference_bbox() then
    # couldn't tell apart from a real request failure, and which meant the
    # point+precision fallback a few lines below in get_reference_bbox()
    # (built for exactly this record shape) never even got a chance to run,
    # because there was no record to read latitude/longitude/precision off
    # of in the first place. Fixed by replacing every NULL-valued field with
    # NA before building the tibble, so a partially-null record (bbox nulled
    # out, point fields populated, or vice versa) still produces a usable
    # 1-row tibble instead of throwing.
    parsed <- lapply(parsed, function(x) if (is.null(x)) NA else x)
    as_tibble(parsed)
  }, error = function(e) {
    message(sprintf("[GazetteerDebug] getGazetteerRecordByMRGID(%s): request/parse threw an error: %s (url: %s)",
                     as.character(mrgid), conditionMessage(e), url))
    NULL
  })
}

.mrgid_priority_cache <- new.env(parent = emptyenv())

# Specificity-based priority for MRGIDs (lower number = higher priority/specificity)
get_mrgid_priority <- function(mrgid) {
  if (exists(as.character(mrgid), envir = .mrgid_priority_cache, inherits = FALSE)) {
    return(get(as.character(mrgid), envir = .mrgid_priority_cache, inherits = FALSE))
  }
  
  record <- get_gazetteer_record_by_mrgid(mrgid)
  priority <- 5
  if (!is.null(record) && "placeType" %in% names(record)) {
    pt <- record$placeType
    if (pt %in% c("Harbour", "Municipality", "Beach", "EEZ")) priority <- 1
    else if (pt %in% c("Sea", "IHO Sea Area")) priority <- 2
    else if (pt %in% c("Ocean")) priority <- 3
    else priority <- 4
  }
  
  assign(as.character(mrgid), priority, envir = .mrgid_priority_cache)
  return(priority)
}

# Main entry point, called per-species in place of the old
# classify_occurrence(dist_str). Takes the RAW distribution data frame
# (from wm_distribution()/wm_dist()) so it can read each locality's actual
# locationID/MRGID, rather than a flattened text string.
#
# `reference_mrgid` is the user-chosen target region (North Sea by default,
# but can be ANY gazetteer region — Mediterranean Sea, Gulf of Mexico,
# etc. — looked up via gazetteer_search_by_name()). This is what makes
# occurrence classification usable outside the North Sea specifically.
#
# Returns a list:
#   $classification - one of the five tier labels (or "Unknown")
#   $min_hops        - the fewest hops found across all localities (or NA)
#   $nearest_locality - which locality string produced that minimum
classify_occurrence_by_relations <- function(dist_df, reference_mrgid = DEFAULT_REFERENCE_MRGID) {
  if (is.null(dist_df) || nrow(dist_df) == 0 || !"locationID" %in% names(dist_df)) {
    return(list(classification = "Unknown", min_hops = NA_integer_, nearest_locality = NA_character_))
  }

  # Expand dist_df to handle multiple MRGIDs per row
  # Robustly split each row independently
  expanded_dist <- dist_df %>%
      rowwise() %>%
      mutate(mrgids = list(extract_all_mrgids(locationID))) %>%
      unnest(mrgids) %>%
      filter(!is.na(mrgids)) %>%
      filter(mrgids != 7130L) # Exclude European waters (ERMS scope) to improve precision
      
  if (nrow(expanded_dist) == 0) {
    return(list(classification = "Unknown", min_hops = NA_integer_, nearest_locality = NA_character_))
  }
  
  mrgids <- expanded_dist$mrgids
  localities <- expanded_dist$locality
  
  # Sort by specificity (lower priority value first)
  priorities <- sapply(mrgids, get_mrgid_priority)
  order_idx <- order(priorities)
  mrgids <- mrgids[order_idx]
  localities <- localities[order_idx]

  best_hops <- NA_integer_
  best_locality <- NA_character_

  for (i in seq_along(mrgids)) {
    # 1. FAST PATH: Spatial bounding box check
    if (is_locality_in_region(mrgids[i], reference_mrgid)) {
        best_hops <- 0L
        best_locality <- localities[i]
        break # Found "Very likely" match
    }

    # 2. FALLBACK: Graph traversal
    h <- gazetteer_hops_to_target(mrgids[i], target_mrgid = reference_mrgid)
    message(sprintf("DEBUG: MRGID %d to Target %d hop count: %s", mrgids[i], reference_mrgid, ifelse(is.na(h), "NA", as.character(h))))
    if (!is.na(h) && (is.na(best_hops) || h < best_hops)) {
      best_hops <- h
      best_locality <- localities[i]
    }
    if (!is.na(best_hops) && best_hops == 0) break  # can't do better than "is the reference region itself"
  }

  list(
    classification = if (is.na(best_hops)) "Unlikely" else hops_to_occurrence_tier(best_hops),
    min_hops = best_hops,
    nearest_locality = best_locality
  )
}


# Run independent WoRMS API calls for a batch of species in parallel using httr2
# Records are fetched in a single batch call; dist, attr, vern are fetched per-AphiaID
worms_batch_query <- function(ids, vernacular_language = "English", max_attempts = 500,
                               delay = 0, sub_wave_size = 10, logger = message,
                               cancel_check = function() FALSE, max_backoff = 60) {
  base <- "https://www.marinespecies.org/rest"
  n_ids <- length(ids)

  # results: a list where each element is a list(record, dist, attr, vern) for one AphiaID
  out <- replicate(n_ids, list(record = NULL, dist = NULL, attr = NULL, vern = NULL), simplify = FALSE)
  names(out) <- as.character(ids)
  rate_limit_events <- 0L

  # Interruptible sleep: honors cancel_check() every ~1s instead of blocking
  # through the whole wait, so a long Round 50b backoff (see below) doesn't
  # make the module's Cancel button unresponsive.
  interruptible_sleep <- function(seconds) {
    remaining <- seconds
    while (remaining > 0) {
      if (isTRUE(cancel_check())) return(invisible(TRUE))
      step <- min(1, remaining)
      Sys.sleep(step)
      remaining <- remaining - step
    }
    invisible(FALSE)
  }

  # Round 37 (2026-09-29) fix: this used to fire ALL of a chunk's requests
  # (1 batch-record request + 3 per-species requests -- up to 151 concurrent
  # HTTP calls for the default 50-species chunk) via one req_perform_parallel()
  # call, then unconditionally `break` the moment that call itself didn't
  # error out -- even when individual items inside it came back with a
  # timeout, a connection reset, or a non-200 status. `max_attempts` was
  # therefore only ever useful against a *total* failure (e.g. DNS down);
  # a partial failure (the much more common case when firing 151 requests
  # at once with zero pacing) was never retried, silently leaving those
  # specific species' `record`/`dist`/`attr`/`vern` permanently NULL for
  # this call -- which downstream becomes WoRMS_Rank = NA_character_ and
  # drops the species from every rank-filtered export. Traced via a real
  # 319-species run where clearing every cache (ruling out stale-cache
  # staleness) made the loss much WORSE (255 missing vs. the original 81),
  # confirming this scales with how many fresh queries a run needs, not
  # with cache content -- the signature of an unpaced-burst API-reliability
  # bug, the same category Round 34 found and fixed for GBIF's evidence
  # query, but in this entirely separate core-taxonomy code path.
  #
  # Fixed by making each attempt a real per-item retry: only the AphiaIDs
  # still missing *something* are re-requested on the next attempt (so the
  # burst shrinks as items succeed, instead of blindly re-firing everyone
  # or giving up after one round), and the loop only stops early once
  # nothing is left pending. Individual response keys are now named by
  # AphiaID (`dist_<id>`, `attr_<id>`, `vern_<id>`) rather than positional
  # index, since a retry's request list no longer has a fixed 1:1 shape
  # with the original `ids` vector.
  #
  # Round 50 (2026-10-01): Round 37 above fixed per-item retry granularity,
  # but left every pending item's detail requests (up to 3 x 50 = 150 for a
  # default 50-AphiaID chunk) firing in ONE req_perform_parallel() wave with
  # no rate-limit detection at all -- exactly the failure category Round 34
  # already found and fixed for GBIF in this same file
  # (batch_check_obis_gbif_presence()), but never ported to this, WoRMS's
  # own core-taxonomy path, despite this very comment already flagging the
  # risk. Reported by the user as a reproducible, run-after-run pattern:
  # once a run crosses some number of taxa, EVERY subsequent WoRMS query
  # comes back NA for the rest of that run (confirmed via a real export:
  # AphiaID present and Occurrence_classification still populated from
  # OBIS/GBIF evidence alone, but every column that depends on THIS
  # function -- WoRMS_Rank, WoRMS_URL, Is*, WoRMS_Status,
  # WoRMS_Distribution, WoRMS_attributes, Body_Size_*, the vernacular
  # columns -- all NA from that point on). That pattern is consistent with
  # WoRMS starting to reject an ever-larger unpaced burst of concurrent
  # connections with 429/503 once enough chunks have fired, and the
  # existing retry logic having no way to tell "this was a rate limit, back
  # off properly" from "this was some other transient failure, a
  # half-second nudge is enough" -- so it burns through all 3 attempts in a
  # few seconds and permanently gives up on everything still pending.
  #
  # Fixed the same way as GBIF (Round 34): (1) each attempt's per-ID detail
  # requests are now split into smaller sub-waves (`sub_wave_size`, default
  # 10 IDs = up to 30 requests) instead of one all-at-once burst, directly
  # lowering how many concurrent connections WoRMS ever sees from this
  # client at once; (2) every response -- record-batch included -- is
  # checked for HTTP 429/503 specifically, instead of being lumped in with
  # every other kind of failure; (3) on a detected rate limit, the
  # response's own Retry-After header is honored if present, else an
  # escalating backoff is used, BEFORE the next sub-wave -- not just
  # between whole attempts as before, so a rate limit hit mid-attempt is
  # respected immediately rather than ignored until the attempt loop
  # restarts; (4) `delay` -- previously accepted by
  # safe_worms_batch_query_complete() but silently never passed down to
  # this function, so the "Delay between WoRMS requests" UI control had no
  # effect on this code path at all -- is now applied as a baseline pause
  # between sub-waves even when nothing was rate-limited, so the user's own
  # slider can pre-emptively slow a run down before it ever hits the wall.
  # A `rate_limit_events` count is attached to the return value so callers
  # can surface it in the module log, instead of this happening invisibly.
  #
  # Round 50b (2026-10-01), per explicit user request ("ensure internally
  # that network failures are repeated until network is received"): a
  # network/rate-limit failure is no longer allowed to just exhaust 3
  # attempts and quietly give up, leaving real taxa NA. `max_attempts` is
  # now a high safety ceiling (default 500), not a realistic target -- this
  # loop keeps retrying a genuine network failure (timeout, connection
  # error, 429, 503) for as long as it keeps failing, with backoff capped
  # at `max_backoff` seconds so it never waits unboundedly long between
  # tries. `cancel_check()` (wired to the module's own Cancel button) is
  # the only normal way out of a sustained outage -- checked every attempt
  # and every ~1s of any wait via interruptible_sleep(), so Cancel keeps
  # working even mid-backoff instead of this retrying blindly in the
  # background. This only applies to actual network/availability failures:
  # a genuinely nonexistent AphiaID (the record-batch endpoint responds
  # 200 but simply omits it) is marked record = list() -- confirmed absent,
  # not "never asked" -- the moment that 200 is seen, so it stops being
  # retried immediately rather than being mistaken for a network failure
  # forever (see below).
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    pending_record_ids <- Filter(function(id_str) is.null(out[[id_str]]$record), names(out))
    pending_ids_str <- Filter(function(id_str) {
      is.null(out[[id_str]]$record) || is.null(out[[id_str]]$dist) ||
      is.null(out[[id_str]]$attr)   || is.null(out[[id_str]]$vern)
    }, names(out))

    if (length(pending_ids_str) == 0) break  # everything already fetched successfully

    if (isTRUE(cancel_check())) {
      logger(sprintf("[WoRMS] Cancelled with %d AphiaID(s) still pending.", length(pending_ids_str)))
      break
    }
    if (attempt > max_attempts) {
      logger(sprintf("[WoRMS] Gave up after %d attempts with %d AphiaID(s) still unresolved -- this should be rare; check connectivity to marinespecies.org.",
                     attempt - 1L, length(pending_ids_str)))
      break
    }
    if (attempt %% 10 == 0) {
      logger(sprintf("[WoRMS] Still retrying after %d attempts (%d AphiaID(s) pending) -- network or WoRMS availability issue persisting.",
                     attempt, length(pending_ids_str)))
    }

    attempt_rate_limited <- FALSE
    attempt_retry_after <- NA_real_

    # 1. Record-batch request -- one combined call for all pending IDs,
    # fired by itself (sequentially, via req_perform()) before the per-ID
    # sub-waves below, so it doesn't add to their concurrency burst.
    if (length(pending_record_ids) > 0) {
      r_batch <- tryCatch(
        request(base) %>%
          req_url_path_append("AphiaRecordsByAphiaIDs") %>%
          req_url_query(`aphiaids[]` = pending_record_ids, .multi = "explode") %>%
          req_timeout(10) %>%
          req_error(is_error = function(r) FALSE) %>%
          req_perform(),
        error = function(e) e
      )
      if (inherits(r_batch, "httr2_response")) {
        status <- resp_status(r_batch)
        if (status %in% c(429, 503)) {
          attempt_rate_limited <- TRUE
          rate_limit_events <- rate_limit_events + 1L
          ra <- suppressWarnings(as.numeric(resp_header(r_batch, "Retry-After")))
          if (!is.na(ra)) attempt_retry_after <- max(c(attempt_retry_after, ra), na.rm = TRUE)
          logger(sprintf("[WoRMS] Record batch rate-limited (HTTP %d) on attempt %d.", status, attempt))
        } else if (status == 200) {
          body <- tryCatch(resp_body_string(r_batch), error = function(e) "")
          if (nzchar(body) && body != "NULL") {
            parsed <- tryCatch(fromJSON(body, simplifyVector = FALSE), error = function(e) list())
            for (rec in parsed) {
              if (!is.null(rec$AphiaID)) {
                id_key <- as.character(rec$AphiaID)
                if (id_key %in% names(out)) {
                  out[[id_key]]$record <- rec
                } else {
                  cat(sprintf("DEBUG: AphiaID %s not in names(out) (%s)\n", id_key, paste(names(out), collapse=", ")))
                }
              }
            }
          }
          # Round 50b: a successful (200) response is the server's
          # definitive answer for every ID in THIS request, even the ones
          # it chose not to include (a deleted/invalid AphiaID, typically).
          # Mark those record = list() -- confirmed absent -- rather than
          # leaving them NULL, which would otherwise look identical to "we
          # never got a response" and retry forever under the Round 50b
          # persistent-retry policy above. list() still reads as "no
          # record" to every downstream consumer (is.null(wm$kingdom) is
          # TRUE either way), so this changes only the retry loop's own
          # pending/resolved bookkeeping, not any other behavior.
          for (id_key in pending_record_ids) {
            if (is.null(out[[id_key]]$record)) out[[id_key]]$record <- list()
          }
        }
      }
    }

    if (delay > 0) interruptible_sleep(delay)

    # 2. Per-ID detail requests (dist/attr/vern), split into sub-waves so
    # WoRMS never sees more than ~3 * sub_wave_size concurrent connections
    # from this client at once (Round 50 -- see comment above).
    sub_waves <- split(pending_ids_str, ceiling(seq_along(pending_ids_str) / sub_wave_size))
    for (sw in sub_waves) {
      reqs <- list()
      for (id_str in sw) {
        if (is.null(out[[id_str]]$dist)) reqs[[paste0("dist_", id_str)]] <- request(paste0(base, "/AphiaDistributionsByAphiaID/", id_str)) %>% req_timeout(10) %>% req_error(is_error = function(r) FALSE)
        if (is.null(out[[id_str]]$attr)) reqs[[paste0("attr_", id_str)]] <- request(paste0(base, "/AphiaAttributesByAphiaID/", id_str)) %>% req_url_query(include_inherited = "true") %>% req_timeout(10) %>% req_error(is_error = function(r) FALSE)
        if (is.null(out[[id_str]]$vern)) reqs[[paste0("vern_", id_str)]] <- request(paste0(base, "/AphiaVernacularsByAphiaID/", id_str)) %>% req_timeout(10) %>% req_error(is_error = function(r) FALSE)
      }
      if (length(reqs) == 0) next

      resps <- tryCatch(req_perform_parallel(reqs, on_error = "continue"), error = function(e) NULL)
      if (is.null(resps)) {
        if (delay > 0) interruptible_sleep(delay)
        next
      }
      names(resps) <- names(reqs) # req_perform_parallel() returns positionally; re-attach names for reliable lookup below

      sub_wave_rate_limited <- FALSE
      for (id_str in sw) {
        for (type in c("dist", "attr", "vern")) {
          key <- paste0(type, "_", id_str)
          if (is.null(reqs[[key]])) next # already had this piece from an earlier attempt

          r <- resps[[key]]
          if (inherits(r, "httr2_error") && !is.null(r$resp)) r <- r$resp
          if (inherits(r, "httr2_response")) {
            status <- resp_status(r)
            if (status %in% c(429, 503)) {
              sub_wave_rate_limited <- TRUE
              ra <- suppressWarnings(as.numeric(resp_header(r, "Retry-After")))
              if (!is.na(ra)) attempt_retry_after <- max(c(attempt_retry_after, ra), na.rm = TRUE)
            } else {
              body <- tryCatch(resp_body_string(r), error = function(e) "")
              if (status == 200 && nzchar(body) && body != "NULL") {
                 out[[id_str]][[type]] <- tryCatch(fromJSON(body, simplifyVector = FALSE), error = function(e) list())
              } else if (status == 204 || (status == 200 && !nzchar(body))) {
                 out[[id_str]][[type]] <- list()
              }
            }
          }
        }
      }

      if (sub_wave_rate_limited) {
        attempt_rate_limited <- TRUE
        rate_limit_events <- rate_limit_events + 1L
        wait_s <- min(if (!is.na(attempt_retry_after)) attempt_retry_after else 5, max_backoff)
        logger(sprintf("[WoRMS] Sub-wave rate-limited (HTTP 429/503) on attempt %d -- pausing %.1fs before the next sub-wave.",
                       attempt, wait_s))
        interruptible_sleep(wait_s)
      } else if (delay > 0) {
        interruptible_sleep(delay)
      }
    }

    # Loop continues automatically if anything is still pending after this
    # attempt (checked at the top of the next iteration). A rate-limited
    # attempt gets a real backoff (the server's own Retry-After when known,
    # else an escalating wait) instead of the brief random nudge used for
    # an ordinary, non-rate-limit transient failure.
    if (attempt < max_attempts) {
      if (attempt_rate_limited) {
        wait_s <- min(if (!is.na(attempt_retry_after)) attempt_retry_after else attempt * 5, max_backoff)
        logger(sprintf("[WoRMS] Attempt %d hit a rate limit -- waiting %.1fs before retrying %d still-pending AphiaID(s).",
                       attempt, wait_s, length(pending_ids_str)))
        interruptible_sleep(wait_s)
      } else {
        interruptible_sleep(stats::runif(1, 0.5, 1.5))
      }
    }
  }
  attr(out, "rate_limit_events") <- rate_limit_events
  out
}


# Round 51 (2026-10-02), per explicit user request ("please query synonyms
# during the WoRMS queries, especially for species with NAs"): a single
# one-off WoRMS REST fetch for one AphiaID, used only by the synonym
# fallback inside safe_worms_batch_query_complete() below -- deliberately
# NOT routed through worms_batch_query()'s own batched/paced/retrying
# machinery, since by the time this fires the main run has already
# finished and this only ever runs for the handful of species it left with
# a genuinely empty piece. Mirrors the plain one-off httr2 call pattern the
# pre-existing subspecies fallback just below already uses for distribution
# (wm_distribution(parent_id)), applied here directly against the raw REST
# endpoints instead of the worrms package so it covers attributes and
# vernacular too, not just distribution, with the same request shape
# worms_batch_query() itself uses. Returns list() for "asked, nothing
# there" (a real empty answer) and NULL for "the attempt itself failed" --
# callers only use a synonym's data when non-NULL and non-empty, so a
# failed one-off attempt is treated exactly like no data, never retried.
.worms_fetch_one_rest <- function(aphia_id, endpoint, query = NULL) {
  req <- request(paste0("https://www.marinespecies.org/rest/", endpoint, "/", aphia_id)) %>%
    req_timeout(10) %>%
    req_error(is_error = function(r) FALSE)
  if (!is.null(query)) req <- req %>% req_url_query(!!!query)
  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || !inherits(resp, "httr2_response")) return(NULL)
  if (resp_status(resp) != 200) return(NULL)
  body <- tryCatch(resp_body_string(resp), error = function(e) "")
  if (!nzchar(body) || body == "NULL") return(list())
  tryCatch(fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
}

# Batch enrichment for multiple AphiaIDs
safe_worms_batch_query_complete <- function(ids, delay = 0.2, reference_mrgid = DEFAULT_REFERENCE_MRGID,
                                            vernacular_language = "English", fetch_taxonomy = TRUE,
                                            logger = message,
                                            cancel_check = function() FALSE, max_backoff = 60) {
  # Batch query
  # Round 50 (2026-10-01): `delay` and a logger are now actually passed down
  # to worms_batch_query() -- previously `delay` was accepted here and never
  # forwarded, so the "Delay between WoRMS requests" UI slider had zero
  # effect on this call. See worms_batch_query()'s own Round 50 comment for
  # the rate-limit fix this enables.
  # Round 50b (2026-10-01): `cancel_check` and `max_backoff` are now also
  # forwarded, so the module's own Cancel button can interrupt a long
  # persistent-retry backoff down inside worms_batch_query() instead of that
  # retrying being unreachable from this layer up.
  batch_res <- worms_batch_query(ids, vernacular_language, delay = delay, logger = logger,
                                  cancel_check = cancel_check, max_backoff = max_backoff)

  results <- list()
  
  for (id_str in names(batch_res)) {
    id <- as.integer(id_str)
    pq <- batch_res[[id_str]]
    wm <- pq$record
    
    # Placeholder for species with no record
    empty_res <- tibble(
      AphiaID = id,
      Kingdom = NA_character_, Phylum = NA_character_, Class = NA_character_,
      Order = NA_character_, Family = NA_character_, Genus = NA_character_,
      Species = NA_character_,
      IsMarine = NA, IsBrackish = NA, IsFreshwater = NA, IsTerrestrial = NA,
      WoRMS_Status = NA_character_,
      WoRMS_Distribution = NA_character_,
       WoRMS_attributes = NA_character_,
       Body_Size_Min = NA_real_,
       Body_Size_Mean = NA_real_,
       Body_Size_Max = NA_real_,
       Functional_Group_Parsed = NA_character_,

      WoRMS_FunctionalGroup = NA_character_,
      WoRMS_Rank = NA_character_,
      WoRMS_URL = NA_character_,
      WoRMS_Synonym_Fallback_Used = FALSE,
      WoRMS_Parent_Fallback_Used = FALSE
    )

    # Robust check: ensure wm is a list/data.frame before accessing its elements
    if (fetch_taxonomy && (!is.list(wm) || is.null(wm$kingdom))) {
      results[[id_str]] <- list(worms_data = empty_res, dist_df = NULL)
      next
    }
    
    # Parse distribution
    dist_df <- NULL
    dist_raw <- pq$dist
    
    attr_raw <- pq$attr
    vern_raw <- pq$vern

    # WoRMS synonym fallback (Round 51, 2026-10-02), per explicit user
    # request ("please query synonyms during the WoRMS queries, especially
    # for species with NAs"): the accepted/current AphiaID resolving
    # cleanly (a real `record`, handled above) does not guarantee its own
    # distribution/attribute/vernacular rows were ever migrated off an
    # older, now-synonymized combination -- the exact same
    # taxonomic-bookkeeping-lag pattern fetch_worms_synonyms()'s own header
    # comment documents for OBIS/GBIF (Round 6, see check_obis_presence()/
    # check_gbif_presence() above), but affecting WoRMS's OWN data here,
    # not an external aggregator. Confirmed as a real, reported case: at the
    # time, *Maea johnstoni* (AphiaID 1872991) was the current accepted
    # name/AphiaID (live check 2026-10-05: WoRMS has since reverted to
    # *Magelona johnstoni*/130269 as accepted; the fallback is direction-
    # agnostic), but WoRMS still carried real historical distribution/attribute data
    # under its retired combination *Magelona johnstoni* (AphiaID 130269)
    # that was never considered when only the current AphiaID was queried.
    #
    # When any of dist/attr/vern is still empty for the primary id after
    # everything above, every known synonym AphiaID (fetch_worms_synonyms(),
    # already used elsewhere in this file for exactly this class of problem)
    # is tried in turn for that one still-missing piece, stopping at the
    # first synonym that actually has data. A piece the primary id already
    # answered is never touched or re-queried; this only ever fires for a
    # piece that would otherwise ship as NA. `WoRMS_Synonym_Fallback_Used`
    # records whether this fired for the row, so a rescued value stays
    # traceable rather than looking identical to data the primary AphiaID
    # supplied directly.
    synonym_fallback_used <- FALSE
    if (is.list(wm) && !is.null(wm$kingdom)) {
      need_dist <- is.null(dist_raw) || length(dist_raw) == 0
      need_attr <- is.null(attr_raw) || length(attr_raw) == 0
      need_vern <- is.null(vern_raw) || length(vern_raw) == 0
      if (need_dist || need_attr || need_vern) {
        syn <- fetch_worms_synonyms(id)
        for (syn_id in syn$aphia_ids) {
          if (!need_dist && !need_attr && !need_vern) break
          if (need_dist) {
            Sys.sleep(delay)
            r <- .worms_fetch_one_rest(syn_id, "AphiaDistributionsByAphiaID")
            if (!is.null(r) && length(r) > 0) {
              dist_raw <- r; need_dist <- FALSE; synonym_fallback_used <- TRUE
              message(sprintf("[WoRMS] AphiaID %s: distribution filled from synonym AphiaID %s.", id, syn_id))
            }
          }
          if (need_attr) {
            Sys.sleep(delay)
            r <- .worms_fetch_one_rest(syn_id, "AphiaAttributesByAphiaID", query = list(include_inherited = "true"))
            if (!is.null(r) && length(r) > 0) {
              attr_raw <- r; need_attr <- FALSE; synonym_fallback_used <- TRUE
              message(sprintf("[WoRMS] AphiaID %s: attributes filled from synonym AphiaID %s.", id, syn_id))
            }
          }
          if (need_vern) {
            Sys.sleep(delay)
            r <- .worms_fetch_one_rest(syn_id, "AphiaVernacularsByAphiaID")
            if (!is.null(r) && length(r) > 0) {
              vern_raw <- r; need_vern <- FALSE; synonym_fallback_used <- TRUE
              message(sprintf("[WoRMS] AphiaID %s: vernacular names filled from synonym AphiaID %s.", id, syn_id))
            }
          }
        }
      }
    }

    # Round 59 (2026-10-05), per explicit user decision: a subspecies is
    # KEPT as the subspecies (Module F no longer promotes it to its parent
    # species). For rank "Subspecies" only, any of distribution / attributes
    # / vernacular names that is STILL empty after the subspecies' own fetch
    # and the synonym fallback above is borrowed from its parent species,
    # found through WoRMS' own parent link (get_parent_aphia_id(), i.e.
    # parentNameUsageID) -- not by a name search, which the old
    # distribution-only fallback used and which could land on a different
    # taxon. A piece the subspecies already has is never replaced. Functional
    # group and body size are parsed from `attr_raw` below, so they follow.
    # The row's own AphiaID, name and status are never touched.
    # `WoRMS_Parent_Fallback_Used` records that any piece was borrowed.
    parent_fallback_used <- FALSE
    if (is.list(wm) && !is.null(wm$kingdom) && identical(wm$rank, "Subspecies")) {
      need_dist <- is.null(dist_raw) || length(dist_raw) == 0
      need_attr <- is.null(attr_raw) || length(attr_raw) == 0
      need_vern <- is.null(vern_raw) || length(vern_raw) == 0
      if (need_dist || need_attr || need_vern) {
        parent_id <- tryCatch(get_parent_aphia_id(id), error = function(e) id)
        if (!is.na(parent_id) && !identical(as.integer(parent_id), as.integer(id))) {
          if (need_dist) {
            Sys.sleep(delay)
            r <- .worms_fetch_one_rest(parent_id, "AphiaDistributionsByAphiaID")
            if (!is.null(r) && length(r) > 0) {
              dist_raw <- r; parent_fallback_used <- TRUE
              message(sprintf("[WoRMS] AphiaID %s (subspecies): distribution borrowed from parent species AphiaID %s.", id, parent_id))
            }
          }
          if (need_attr) {
            Sys.sleep(delay)
            r <- .worms_fetch_one_rest(parent_id, "AphiaAttributesByAphiaID", query = list(include_inherited = "true"))
            if (!is.null(r) && length(r) > 0) {
              attr_raw <- r; parent_fallback_used <- TRUE
              message(sprintf("[WoRMS] AphiaID %s (subspecies): attributes borrowed from parent species AphiaID %s.", id, parent_id))
            }
          }
          if (need_vern) {
            Sys.sleep(delay)
            r <- .worms_fetch_one_rest(parent_id, "AphiaVernacularsByAphiaID")
            if (!is.null(r) && length(r) > 0) {
              vern_raw <- r; parent_fallback_used <- TRUE
              message(sprintf("[WoRMS] AphiaID %s (subspecies): vernacular names borrowed from parent species AphiaID %s.", id, parent_id))
            }
          }
        }
      }
    }

    if (!is.null(dist_raw) && length(dist_raw) > 0) {
      locs <- lapply(dist_raw, function(x) {
        fields <- c("locality", "locationID", "higherGeography", "location", "area")
        
        # Extract fields and split
        data_list <- lapply(fields, function(f) {
            val <- x[[f]] %||% NA_character_
            unlist(strsplit(as.character(val), ";"))
        })
        names(data_list) <- fields
        
        # Find max length
        n <- max(sapply(data_list, length))
        
        # Pad all to max length and trim whitespace
        data_list <- lapply(data_list, function(v) {
            v <- if (length(v) < n) c(v, rep(NA_character_, n - length(v))) else v
            trimws(v)
        })
        
        as_tibble(data_list)
      })
      dist_df <- bind_rows(locs)
      dist_df$AphiaID <- id
    }
    
    # Parse attributes
    attr_str <- NA_character_
    body_sizes <- numeric()
    fg_values <- character()
    # "Paraphyletic group" is a separate WoRMS attribute category from
    # "Functional group" (fg_values, above) and "Body size" — flattened the
    # same way fg_values is, into Species_Paraphyletic_Group below (was
    # briefly kept as an unused, unflattened list-column, Species_Traits_List;
    # renamed/flattened 2026-09-11 since nothing ever read the list form and
    # list-columns risk garbling write_csv() output).
    paraphyletic_values <- character()

    # attr_raw was already extracted (and, if needed, synonym-filled) above,
    # before the distribution/attribute/vernacular fallback block -- not
    # re-read from pq$attr here, which would discard a synonym-supplied value.
    if (!is.null(attr_raw) && length(attr_raw) > 0) {
      attr_text <- sapply(attr_raw, function(x) {
        if (!is.null(x$measurementType) && !is.null(x$measurementValue)) {
          paste(x$measurementType, x$measurementValue, sep = "=")
        } else NA_character_
      })
      attr_text <- unique(attr_text[!is.na(attr_text)])
      if (length(attr_text) > 0) {
        attr_str <- paste(attr_text, collapse = " | ")
        
        # Parse body size and functional groups
        for (x in attr_raw) {
          mt <- tolower(x$measurementType %||% "")
          mv <- trimws(x$measurementValue %||% "")
          if (grepl("body size", mt, ignore.case=TRUE)) {
            vals <- as.numeric(unlist(regmatches(mv, gregexpr("[0-9]*\\.?[0-9]+", mv))))
            if (length(vals) > 0) body_sizes <- c(body_sizes, vals)
          } else if (grepl("functional group", mt, ignore.case=TRUE)) {
            fg_values <- c(fg_values, mv)
          } else if (grepl("paraphyletic group", mt, ignore.case=TRUE)) {
            paraphyletic_values <- c(paraphyletic_values, mv)
          }
        }
      }
    }

    
    bs_min <- if(length(body_sizes) > 0) min(body_sizes) else NA_real_

    bs_mean <- if(length(body_sizes) > 0) mean(body_sizes) else NA_real_
    bs_max <- if(length(body_sizes) > 0) max(body_sizes) else NA_real_
    fg_parsed <- if(length(fg_values) > 0) paste(unique(fg_values), collapse = "; ") else NA_character_
    paraphyletic_parsed <- if(length(paraphyletic_values) > 0) paste(unique(paraphyletic_values), collapse = "; ") else NA_character_
    
    # Parse vernacular names
    vern_data <- list()
    # vern_raw was already extracted (and, if needed, synonym-filled) above,
    # before the distribution/attribute/vernacular fallback block -- not
    # re-read from pq$vern here, which would discard a synonym-supplied value.
    if (!is.null(vern_raw) && length(vern_raw) > 0) {
      langs <- trimws(strsplit(vernacular_language, ",")[[1]])
      for (lang in langs) {
        if (!nzchar(lang)) next
        lang_code <- tolower(trimws(lang))
        matches <- character(0)
         for (v in vern_raw) {
           v_lang_code <- tolower(v$language_code %||% "")
           v_lang_name <- tolower(v$language %||% "")
           v_name <- v$vernacular %||% ""
           if ((v_lang_code == lang_code || v_lang_name == lang_code) && nzchar(v_name)) {
             matches <- c(matches, v_name)
           }
         }

        if (length(matches) > 0) {
          # Add new separate columns
          col_name <- paste0("WoRMS_vernacular_", trimws(lang))
          vern_data[[col_name]] <- paste(unique(matches), collapse = "; ")
        }
      }
    }
    
    # Construct base result tibble
    result <- tibble(
      AphiaID = id,
      Kingdom = wm$kingdom %||% NA_character_,
      Phylum = wm$phylum %||% NA_character_,
      Class = wm$class %||% NA_character_,
      Order = wm$order %||% NA_character_,
      Family = wm$family %||% NA_character_,
      Genus = wm$genus %||% NA_character_,
      Species = wm$scientificname %||% NA_character_,
      WoRMS_Rank = wm$rank %||% NA_character_,
      WoRMS_URL = wm$url %||% NA_character_,
      IsMarine = as.logical(wm$isMarine %||% NA),
      IsBrackish = as.logical(wm$isBrackish %||% NA),
      IsFreshwater = as.logical(wm$isFreshwater %||% NA),
      IsTerrestrial = as.logical(wm$isTerrestrial %||% NA),
      WoRMS_Status = as.character(wm$status %||% NA_character_),
      WoRMS_Distribution = extract_distribution_text(dist_df),
      WoRMS_attributes = attr_str,
      Body_Size_Min = bs_min,
      Body_Size_Mean = bs_mean,
      Body_Size_Max = bs_max,
      Functional_Group_Parsed = fg_parsed,
      Species_Paraphyletic_Group = paraphyletic_parsed,
      WoRMS_FunctionalGroup = extract_worms_fg(attr_str),

      # Round 51 (2026-10-02): TRUE when the synonym fallback above actually
      # supplied distribution, attribute, or vernacular data this row's own
      # (accepted) AphiaID didn't have on file -- see the fallback block's
      # own comment above for why this can legitimately happen. Lets a
      # reviewer tell "WoRMS genuinely has nothing for this species" apart
      # from "WoRMS has it, just filed under an older name," which looked
      # identical before this round.
      WoRMS_Synonym_Fallback_Used = synonym_fallback_used,
      WoRMS_Parent_Fallback_Used = parent_fallback_used,

      Occurrence_classification = "Unknown",
      Occurrence_hops_to_reference_region = NA_integer_,
      Occurrence_nearest_locality = NA_character_,
      Parsed_MRGIDs = NA_character_
    )
    
    # Bind vernacular columns
    if (length(vern_data) > 0) {
      result <- bind_cols(result, as_tibble(vern_data))
    }
    
    results[[id_str]] <- list(worms_data = result, dist_df = dist_df)
  }
  # Round 50 (2026-10-01): surface worms_batch_query()'s rate-limit count so
  # the calling module can report it in its run log rather than this
  # happening invisibly.
  attr(results, "rate_limit_events") <- attr(batch_res, "rate_limit_events") %||% 0L
  results
}

# ------------------------------------------------------------------------
# Small utility: safe column existence check + zero-row-safe summaries
# ------------------------------------------------------------------------

pct <- function(n, total) {
  if (is.na(total) || total == 0) return(NA)
  round(100 * n / total, 1)
}

fmt_int <- function(x) format(x, big.mark = ",")

# ------------------------------------------------------------------------
# WoRMS functional group helpers (taxonomic group lookup, trait cache,
# cross-validation) — adapted from eDNAcheck
# ------------------------------------------------------------------------

# Extract WoRMS "Functional group" value from the pipe-delimited attributes string
extract_worms_fg <- function(attr_str) {
  if (is.na(attr_str) || !nzchar(attr_str %||% "")) return(NA_character_)
  parts <- str_split(attr_str, " \\| ")[[1]]
  fg_lines <- grep("^Functional group=", parts, value = TRUE, ignore.case = TRUE)
  if (length(fg_lines) == 0) return(NA_character_)
  vals <- sub("^Functional group=", "", fg_lines, ignore.case = TRUE)
  paste(unique(trimws(vals)), collapse = "; ")
}

taxonomic_group_lookup <- tibble(
  Phylum = character(), Class = character(), Order = character(),
  Group1 = character(), Group2 = character(), TaxGroup = character()
) |>
  # Annelida: Polychaeta — benthic adults, planktonic trochophore larvae
  add_row(Phylum = "Annelida", Class = "Polychaeta", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "polychaete (bristleworm)") |>
  # Arthropoda: Branchiopoda / Onychopoda + Ctenopoda — marine Cladocera are entirely
  # planktonic. Round 17 (2026-09-17): this row used to say Order = "Diplostraca", but
  # WoRMS classifies "Diplostraca" as a SUPERORDER, not the Order rank -- every real marine
  # cladocera record (Podon, Evadne, Pleopis, Pseudevadne = Onychopoda; Penilia = Ctenopoda)
  # reports its Order as one of those two, never "Diplostraca" itself. Since
  # classify_taxonomic_group() requires an exact string match on the Order value actually
  # returned by WoRMS, the old row could never match a real record -- confirmed directly:
  # Podon intermedius (Phylum Arthropoda, Class Branchiopoda, Order Onychopoda per its live
  # WoRMS classification) fell through this entire lookup table with no match, leaving
  # Group1/Group2/TaxGroup all NA whenever Priority 1 (WoRMS live attributes, checkbox-gated)
  # and Priority 2 (external trait DB) also had nothing for it -- which was Podon's actual,
  # current cache state at the time this was found (no entry in worms_fg_cache.rds).
  add_row(Phylum = "Arthropoda", Class = "Branchiopoda", Order = "Onychopoda",
          Group1 = "holoplankton", Group2 = "holoplankton", TaxGroup = "Cladocera") |>
  add_row(Phylum = "Arthropoda", Class = "Branchiopoda", Order = "Ctenopoda",
          Group1 = "holoplankton", Group2 = "holoplankton", TaxGroup = "Cladocera") |>
  # Arthropoda: Copepoda (as class) — primarily holoplanktonic (default).
  #
  # FIXED (2026-09-22): classify_taxonomic_group() does a first-match linear
  # scan through this table (break on the first Phylum+Class+Order match,
  # NA treated as a wildcard), so this class-wide fallback -- appearing
  # BEFORE the Harpacticoida/Siphonostomatoida exceptions below, which are
  # keyed to Class="Hexanauplia" -- unconditionally caught any species
  # whose WoRMS record reports its class as the older "Copepoda" rather
  # than the newer "Hexanauplia" (WoRMS' own taxonomic backbone is
  # inconsistent about which spelling a given copepod record carries,
  # exactly the same class of rank-naming mismatch as the Onychopoda/
  # Ctenopoda fix above), silently mislabeling benthic/meiobenthic
  # Harpacticoida and parasitic Siphonostomatoida records as holoplankton
  # whenever they happened to resolve under the "Copepoda" spelling.
  # Confirmed for Longipedia coronata (a harpacticoid; WoRMS functional
  # group attributes correctly show "benthos; plankton > zooplankton;
  # benthos > meiobenthos", yet the pipeline's own independently-computed
  # Group1 agreed with the external DB's "holoplankton" -- Group1_Validated
  # = TRUE, DB_Group1 = holoplankton in a real pipeline export -- exactly
  # what this first-match ordering produces if its record's class comes
  # back as "Copepoda"). Fixed by mirroring the Hexanauplia block's order
  # exceptions for the "Copepoda" spelling too, ahead of this fallback.
  add_row(Phylum = "Arthropoda", Class = "Copepoda", Order = "Harpacticoida",
          Group1 = "meiobenthos/meiofauna", Group2 = "meiobenthos/meiofauna", TaxGroup = "copepod") |>
  add_row(Phylum = "Arthropoda", Class = "Copepoda", Order = "Siphonostomatoida",
          Group1 = "parasitic", Group2 = "parasitic", TaxGroup = "copepod") |>
  # Arthropoda: Copepoda / Canuelloida — meiobenthic harpacticoid-grade copepods
  # (families Canuellidae, Longipediidae) split out of Harpacticoida into their
  # own order by WoRMS' current backbone (Khodami et al. 2017 revision). FIXED
  # (2026-10-03): Longipedia coronata is literally the documented motivating
  # case for the Harpacticoida exception above (see its comment a few lines up)
  # -- but WoRMS has since moved this species' (and Canuella perplexa's) Order
  # from "Harpacticoida" to "Canuelloida", so the exact same taxon drifted back
  # out from under that fix and was again falling through to the Order=NA
  # class-wide "holoplankton" fallback below. Confirmed live via WoRMS REST
  # (2026-10-03): both species resolve to Class="Copepoda", Order="Canuelloida",
  # accepted. Mirrors the Harpacticoida treatment.
  add_row(Phylum = "Arthropoda", Class = "Copepoda", Order = "Canuelloida",
          Group1 = "meiobenthos/meiofauna", Group2 = "meiobenthos/meiofauna", TaxGroup = "copepod") |>
  add_row(Phylum = "Arthropoda", Class = "Copepoda", Order = NA,
          Group1 = "holoplankton", Group2 = "holoplankton", TaxGroup = "copepod") |>
  # Arthropoda: Hexanauplia / Calanoida — free-living planktonic copepods
  add_row(Phylum = "Arthropoda", Class = "Hexanauplia", Order = "Calanoida",
          Group1 = "holoplankton", Group2 = "holoplankton", TaxGroup = "copepod") |>
  # Arthropoda: Hexanauplia / Cyclopoida — mostly planktonic, some parasitic
  add_row(Phylum = "Arthropoda", Class = "Hexanauplia", Order = "Cyclopoida",
          Group1 = "holoplankton", Group2 = "holoplankton", TaxGroup = "copepod") |>
  # Arthropoda: Hexanauplia / Harpacticoida — primarily meiobenthic
  add_row(Phylum = "Arthropoda", Class = "Hexanauplia", Order = "Harpacticoida",
          Group1 = "meiobenthos/meiofauna", Group2 = "meiobenthos/meiofauna", TaxGroup = "copepod") |>
  # Arthropoda: Hexanauplia / Siphonostomatoida — obligate fish/shellfish parasites
  add_row(Phylum = "Arthropoda", Class = "Hexanauplia", Order = "Siphonostomatoida",
          Group1 = "parasitic", Group2 = "parasitic", TaxGroup = "copepod") |>
  # Arthropoda: Hexanauplia / Canuelloida — see Copepoda/Canuelloida note above
  add_row(Phylum = "Arthropoda", Class = "Hexanauplia", Order = "Canuelloida",
          Group1 = "meiobenthos/meiofauna", Group2 = "meiobenthos/meiofauna", TaxGroup = "copepod") |>
  # Arthropoda: Hexanauplia class-level fallback — for unlisted copepod orders
  add_row(Phylum = "Arthropoda", Class = "Hexanauplia", Order = NA,
          Group1 = "holoplankton", Group2 = "holoplankton", TaxGroup = "copepod") |>
  # Arthropoda: Ostracoda — benthic and planktonic seed shrimps; direct development
  add_row(Phylum = "Arthropoda", Class = "Ostracoda", Order = NA,
          Group1 = "benthic invertebrate", Group2 = "benthic invertebrate", TaxGroup = "ostracod (seed shrimp)") |>
  # Arthropoda: Pycnogonida — sea spiders, benthic, some with planktonic larvae
  add_row(Phylum = "Arthropoda", Class = "Pycnogonida", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "sea spider (pycnogonid)") |>
  # Arthropoda: Malacostraca / Amphipoda — mostly benthic (Gammaridea); Hyperiidea pelagic
  add_row(Phylum = "Arthropoda", Class = "Malacostraca", Order = "Amphipoda",
          Group1 = "benthic invertebrate", Group2 = "benthic invertebrate", TaxGroup = "amphipod") |>
  # Arthropoda: Malacostraca / Decapoda — benthic adults, planktonic zoea larvae
  add_row(Phylum = "Arthropoda", Class = "Malacostraca", Order = "Decapoda",
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "decapod") |>
  # Arthropoda: Malacostraca / Isopoda — mostly parasitic (esp. Cymothoida)
  add_row(Phylum = "Arthropoda", Class = "Malacostraca", Order = "Isopoda",
          Group1 = "parasite", Group2 = "parasite", TaxGroup = "isopod") |>
  # Arthropoda: Malacostraca / Euphausiacea — krill, entirely holoplanktonic
  add_row(Phylum = "Arthropoda", Class = "Malacostraca", Order = "Euphausiacea",
          Group1 = "holoplankton", Group2 = "holoplankton", TaxGroup = "krill (euphausiid)") |>
  # Arthropoda: Malacostraca / Mysida — opossum shrimps, mostly suprabenthic
  add_row(Phylum = "Arthropoda", Class = "Malacostraca", Order = "Mysida",
          Group1 = "benthic invertebrate", Group2 = "benthic invertebrate", TaxGroup = "mysid (opossum shrimp)") |>
  # Arthropoda: Malacostraca / Cumacea — benthic, direct development
  add_row(Phylum = "Arthropoda", Class = "Malacostraca", Order = "Cumacea",
          Group1 = "benthic invertebrate", Group2 = "benthic invertebrate", TaxGroup = "cumacean") |>
  # Arthropoda: Malacostraca / Tanaidacea — benthic, direct development
  add_row(Phylum = "Arthropoda", Class = "Malacostraca", Order = "Tanaidacea",
          Group1 = "benthic invertebrate", Group2 = "benthic invertebrate", TaxGroup = "tanaid") |>
  # Arthropoda: Malacostraca / Stomatopoda — mantis shrimps, benthic with planktonic larvae
  add_row(Phylum = "Arthropoda", Class = "Malacostraca", Order = "Stomatopoda",
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "stomatopod (mantis shrimp)") |>
  # Arthropoda: Thecostraca / Balanomorpha — benthic adults, planktonic nauplius larvae
  add_row(Phylum = "Arthropoda", Class = "Thecostraca", Order = "Balanomorpha",
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "barnacle") |>
  # Arthropoda: Thecostraca / Rhizocephala — parasitic barnacles (sacculinids)
  add_row(Phylum = "Arthropoda", Class = "Thecostraca", Order = "Rhizocephala",
          Group1 = "parasite", Group2 = "parasite", TaxGroup = "barnacle (parasitic)") |>
  # Bryozoa — benthic colonial adults, planktonic cyphonautes larvae
  add_row(Phylum = "Bryozoa", Class = NA, Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "bryozoan") |>
  # Chaetognatha — entirely holoplanktonic arrow worms
  add_row(Phylum = "Chaetognatha", Class = NA, Order = NA,
          Group1 = "holoplankton", Group2 = "holoplankton", TaxGroup = "chaetognath") |>
  # Chordata: fish classes — most have planktonic eggs/larvae
  add_row(Phylum = "Chordata", Class = "Actinopterygii", Order = NA,
          Group1 = "ichthyoplankton / necton", Group2 = "ichthyoplankton / necton", TaxGroup = "fish") |>
  # Teleostei is the WoRMS-standard class for most ray-finned fishes (including
  # Clupea harengus, Gadus morhua, etc.) — WoRMS uses Teleostei rather than
  # Actinopterygii in its modern classification; both should map to the same
  # ecological group since they describe the same animals.
  add_row(Phylum = "Chordata", Class = "Teleostei", Order = NA,
          Group1 = "ichthyoplankton / necton", Group2 = "ichthyoplankton / necton", TaxGroup = "fish") |>
  add_row(Phylum = "Chordata", Class = "Elasmobranchii", Order = NA,
          Group1 = "ichthyoplankton / necton", Group2 = "ichthyoplankton / necton", TaxGroup = "fish") |>
  add_row(Phylum = "Chordata", Class = "Holocephali", Order = NA,
          Group1 = "ichthyoplankton / necton", Group2 = "ichthyoplankton / necton", TaxGroup = "fish") |>
  add_row(Phylum = "Chordata", Class = "Myxini", Order = NA,
          Group1 = "ichthyoplankton / necton", Group2 = "ichthyoplankton / necton", TaxGroup = "fish") |>
  add_row(Phylum = "Chordata", Class = "Petromyzonti", Order = NA,
          Group1 = "ichthyoplankton / necton", Group2 = "ichthyoplankton / necton", TaxGroup = "fish") |>
  # Chordata: Mammalia — marine mammals (cetaceans, pinnipeds); no planktonic stage
  add_row(Phylum = "Chordata", Class = "Mammalia", Order = NA,
          Group1 = NA, Group2 = "marine mammal", TaxGroup = "marine mammal") |>
  # Chordata: Aves — seabirds; no planktonic stage
  add_row(Phylum = "Chordata", Class = "Aves", Order = NA,
          Group1 = NA, Group2 = "seabird", TaxGroup = "seabird") |>
  # Chordata: Reptilia — sea turtles, sea snakes; no planktonic stage
  add_row(Phylum = "Chordata", Class = "Reptilia", Order = NA,
          Group1 = NA, Group2 = "marine reptile", TaxGroup = "marine reptile") |>
  # Chordata: Leptocardii — lancelets (amphioxus), benthic filter feeders, planktonic larvae
  add_row(Phylum = "Chordata", Class = "Leptocardii", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "lancelet (amphioxus)") |>
  # Chordata: Appendicularia (larvaceans) — entirely holoplanktonic
  add_row(Phylum = "Chordata", Class = "Appendicularia", Order = NA,
          Group1 = "holoplankton", Group2 = "holoplankton", TaxGroup = "appendicularian") |>
  # Chordata: Ascidiacea (sea squirts) — benthic adults, planktonic tadpole larvae
  add_row(Phylum = "Chordata", Class = "Ascidiacea", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "tunicate") |>
  # Chordata: Thaliacea (salps, doliolids) — entirely holoplanktonic
  add_row(Phylum = "Chordata", Class = "Thaliacea", Order = NA,
          Group1 = "holoplankton", Group2 = "holoplankton", TaxGroup = "tunicate") |>
  # Cnidaria: Anthozoa / Actiniaria — benthic adults, planktonic planula larvae
  add_row(Phylum = "Cnidaria", Class = "Anthozoa", Order = "Actiniaria",
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "sea anemone") |>
  # Cnidaria: Anthozoa class-level fallback — corals, sea pens, zoanthids; all have planula larvae
  add_row(Phylum = "Cnidaria", Class = "Anthozoa", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "anthozoan (coral/sea anemone)") |>
  # Cnidaria: Hydrozoa — polyp & medusa stages; generally meroplanktonic
  add_row(Phylum = "Cnidaria", Class = "Hydrozoa", Order = NA,
          Group1 = "meroplankton", Group2 = "medusae or hydroid", TaxGroup = "hydrozoan (in: jellyfish / gelatinous zooplankton)") |>
  # Cnidaria: Scyphozoa — "true jellyfish" with benthic scyphistoma + pelagic medusa
  add_row(Phylum = "Cnidaria", Class = "Scyphozoa", Order = NA,
          Group1 = "meroplankton", Group2 = "medusae or polyp", TaxGroup = "true jellyfish (in: jellyfish / gelatinous zooplankton)") |>
  # Cnidaria: Cubozoa — box jellyfish, similar life cycle to Scyphozoa
  add_row(Phylum = "Cnidaria", Class = "Cubozoa", Order = NA,
          Group1 = "meroplankton", Group2 = "medusae or polyp", TaxGroup = "true jellyfish (in: jellyfish / gelatinous zooplankton)") |>
  # Cnidaria: Staurozoa — stalked jellyfish, benthic as adults
  add_row(Phylum = "Cnidaria", Class = "Staurozoa", Order = NA,
          Group1 = "meroplankton", Group2 = "medusae or polyp", TaxGroup = "true jellyfish (in: jellyfish / gelatinous zooplankton)") |>
  # Ctenophora — comb jellies, entirely holoplanktonic
  add_row(Phylum = "Ctenophora", Class = NA, Order = NA,
          Group1 = "holoplankton", Group2 = "holoplankton", TaxGroup = "ctenophore / gelatinous zooplankton") |>
  # Echinodermata: Asteroidea — benthic adults, planktonic bipinnaria larvae
  add_row(Phylum = "Echinodermata", Class = "Asteroidea", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "starfish") |>
  # Echinodermata: Echinoidea — benthic adults, planktonic echinopluteus larvae
  add_row(Phylum = "Echinodermata", Class = "Echinoidea", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "sea urchin") |>
  # Echinodermata: Ophiuroidea — benthic adults, planktonic ophiopluteus larvae
  add_row(Phylum = "Echinodermata", Class = "Ophiuroidea", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "brittlestar") |>
  # Echinodermata: Holothuroidea — sea cucumbers, benthic adults, planktonic auricularia larvae
  add_row(Phylum = "Echinodermata", Class = "Holothuroidea", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "sea cucumber (holothurian)") |>
  # Echinodermata: Crinoidea — feather stars, benthic adults, planktonic larvae
  add_row(Phylum = "Echinodermata", Class = "Crinoidea", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "crinoid (feather star)") |>
  # Mollusca: Bivalvia — benthic adults, planktotrophic veliger larvae
  add_row(Phylum = "Mollusca", Class = "Bivalvia", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "bivalve") |>
  # Mollusca: Cephalopoda — most are nektonic, some with planktonic paralarvae
  add_row(Phylum = "Mollusca", Class = "Cephalopoda", Order = NA,
          Group1 = "meroplankton", Group2 = "cephalopod", TaxGroup = "cephalopod") |>
  # Mollusca: Gastropoda — benthic adults, planktotrophic veliger larvae
  add_row(Phylum = "Mollusca", Class = "Gastropoda", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "gastropod (slugs & snails)") |>
  # Mollusca: Polyplacophora — chitons, benthic with trochophore larvae
  add_row(Phylum = "Mollusca", Class = "Polyplacophora", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "chiton (polyplacophoran)") |>
  # Mollusca: Scaphopoda — tusk shells, benthic with planktonic larvae
  add_row(Phylum = "Mollusca", Class = "Scaphopoda", Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "tusk shell (scaphopod)") |>
  # Annelida: Clitellata — marine oligochaetes, benthic, direct development / cocoons
  add_row(Phylum = "Annelida", Class = "Clitellata", Order = NA,
          Group1 = "benthic invertebrate", Group2 = "benthic invertebrate", TaxGroup = "oligochaete") |>
  # Nemertea — benthic adults, planktonic pilidium larvae
  add_row(Phylum = "Nemertea", Class = NA, Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "nemertines (ribbon worms)") |>
  # Phoronida — benthic tube-dwelling adults, planktonic actinotroch larvae
  add_row(Phylum = "Phoronida", Class = NA, Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "phoronid (horseshoe worm)") |>
  # Rotifera — mostly freshwater, marine forms holoplanktonic
  add_row(Phylum = "Rotifera", Class = NA, Order = NA,
          Group1 = "holoplankton", Group2 = "holoplankton", TaxGroup = "rotifer") |>
  # Nematoda: Chromadorea / Rhabditida — parasitic roundworms
  add_row(Phylum = "Nematoda", Class = "Chromadorea", Order = "Rhabditida",
          Group1 = "parasitic", Group2 = "parasitic", TaxGroup = "nematod (roundworm)") |>
  # Nematoda: Enoplea — mostly free-living, some parasitic
  add_row(Phylum = "Nematoda", Class = "Enoplea", Order = NA,
          Group1 = "parasitic", Group2 = "parasitic", TaxGroup = "nematod (roundworm)") |>
  # Nematoda: general fallback (must come after class/order-specific entries)
  add_row(Phylum = "Nematoda", Class = NA, Order = NA,
          Group1 = "parasitic", Group2 = "parasitic", TaxGroup = "nematod (roundworm)") |>
  # Porifera — sponges, benthic adults, planktonic parenchymula/amphiblastula larvae
  add_row(Phylum = "Porifera", Class = NA, Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "sponge (poriferan)") |>
  # Platyhelminthes — flatworms; free-living (benthic/meiobenthic) and parasitic (Neodermata).
  #
  # FIXED (2026-10-03): this comment always acknowledged both groups exist, but
  # the single rule below unconditionally labeled every flatworm "parasitic" --
  # contradicting itself for the free-living majority. Motivating case:
  # *Microstomum compositum* / *Microstomum inexcultus* (Order Dolichomicrostomida,
  # a classic free-living, meiobenthic/interstitial turbellarian genus -- not
  # parasitic). Confirmed live via WoRMS (2026-10-03): WoRMS tags both species'
  # own "Paraphyletic group" attribute as "Turbellaria" (the traditional
  # free-living grade, used precisely to exclude the parasitic Neodermata), and
  # WoRMS' taxonomic backbone ranks the three parasitic flatworm lineages --
  # Trematoda, Cestoda, Monogenea -- each as its own Class directly under
  # Phylum Platyhelminthes (confirmed via WoRMS REST), with free-living genera
  # like *Microstomum* carrying no populated Class at all (NA) down to Order.
  # Fixed by narrowing "parasitic" to those three Neodermata classes and
  # flipping the general fallback (Class=NA, catching free-living turbellarian
  # genera like Microstomum whose WoRMS record has no Class) to
  # meiobenthos/meiofauna -- correct for most free-living flatworm diversity
  # (tiny, interstitial, direct-developing groups such as Dolichomicrostomida,
  # Macrostomida, Catenulida, Proseriata, Rhabdocoela). Larger macrobenthic
  # free-living groups (e.g. Polycladida, some with a planktonic Müller's
  # larva; Tricladida) aren't broken out separately since no case has
  # surfaced yet -- add an Order-specific exception here if one does, same
  # pattern as the Harpacticoida/Canuelloida copepod exceptions above.
  # TaxGroup labels aligned with eDNAcheck_v2.0.0's helpers_pipeline.R, which
  # turns out to have already fixed this identical bug in an earlier round
  # (not reflected back into this app at the time) with more specific
  # per-class TaxGroup labels -- matched here for cross-app consistency.
  add_row(Phylum = "Platyhelminthes", Class = "Trematoda", Order = NA,
          Group1 = "parasitic", Group2 = "parasitic", TaxGroup = "flatworm (trematode/fluke)") |>
  add_row(Phylum = "Platyhelminthes", Class = "Cestoda", Order = NA,
          Group1 = "parasitic", Group2 = "parasitic", TaxGroup = "flatworm (cestode/tapeworm)") |>
  add_row(Phylum = "Platyhelminthes", Class = "Monogenea", Order = NA,
          Group1 = "parasitic", Group2 = "parasitic", TaxGroup = "flatworm (monogenean)") |>
  add_row(Phylum = "Platyhelminthes", Class = NA, Order = NA,
          Group1 = "meiobenthos/meiofauna", Group2 = "meiobenthos/meiofauna", TaxGroup = "flatworm (free-living)") |>
  # Sipuncula — peanut worms, benthic adults, planktonic trochophore larvae
  add_row(Phylum = "Sipuncula", Class = NA, Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "sipunculan (peanut worm)") |>
  # Echiura — spoon worms, benthic adults, planktonic trochophore larvae
  add_row(Phylum = "Echiura", Class = NA, Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "echiuran (spoon worm)") |>
  # Hemichordata — acorn worms (Enteropneusta), benthic adults, planktonic tornaria larvae
  add_row(Phylum = "Hemichordata", Class = NA, Order = NA,
          Group1 = "meroplankton", Group2 = "benthic invertebrate", TaxGroup = "hemichordate (acorn worm)")

# Classify species into ecological groups based on WoRMS taxonomy
# Returns data.frame with Group1, Group2, TaxGroup, FG_source
# FG_source: "taxonomy" when lookup matched, NA otherwise
classify_taxonomic_group <- function(phylum, class, order) {
  n <- length(phylum)
  g1 <- rep(NA_character_, n)
  g2 <- rep(NA_character_, n)
  tg <- rep(NA_character_, n)
  src <- rep(NA_character_, n)
  lookup <- taxonomic_group_lookup
  for (j in seq_len(n)) {
    if (is.na(phylum[j]) || !nzchar(phylum[j] %||% "")) next
    phy <- tolower(trimws(phylum[j]))
    cls <- if (!is.na(class[j])) tolower(trimws(class[j])) else NA_character_
    ord <- if (!is.na(order[j])) tolower(trimws(order[j])) else NA_character_
    for (i in seq_len(nrow(lookup))) {
      row_phy <- tolower(trimws(lookup$Phylum[i]))
      row_cls <- lookup$Class[i]
      row_ord <- lookup$Order[i]
      phy_match <- !is.na(row_phy) && row_phy == phy
      cls_match <- is.na(row_cls) || (!is.na(cls) && tolower(trimws(row_cls)) == cls)
      ord_match <- is.na(row_ord) || (!is.na(ord) && tolower(trimws(row_ord)) == ord)
      if (phy_match && cls_match && ord_match) {
        g1[j] <- lookup$Group1[i]
        g2[j] <- lookup$Group2[i]
        tg[j] <- lookup$TaxGroup[i]
        src[j] <- "taxonomy"
        break
      }
    }
  }
  data.frame(Group1 = g1, Group2 = g2, TaxGroup = tg, FG_source = src, stringsAsFactors = FALSE)
}

# Cross-validate taxonomy-based assignments against WoRMS "Functional group"
# attribute and update FG_source accordingly.
cross_validate_fg <- function(fg_source, group1, worms_fg) {
  out <- fg_source
  valid <- which(!is.na(fg_source) & !is.na(worms_fg) & nzchar(worms_fg %||% ""))
  for (i in valid) {
    wfg <- tolower(trimws(worms_fg[i]))
    g1 <- tolower(trimws(group1[i]))
    if (grepl("benthos", wfg)) {
      if (!grepl("benthic|meroplankton|meiobenthos|parasit", g1))
        out[i] <- "conflict"
    }
    if (grepl("plankton", wfg)) {
      if (!grepl("holoplankton|meroplankton", g1))
        out[i] <- "conflict"
    }
  }
  unclassed <- is.na(fg_source) & !is.na(worms_fg) & nzchar(worms_fg %||% "")
  out[unclassed] <- "worms_trait"
  out
}

# Build a cache of WoRMS attributes for unique higher taxa (Phylum/Class/Order)
# so we only query each rank once, not per species.
build_worms_trait_cache <- function(phyla, classes, orders, delay = 0.2, logger = message) {
  ranks <- list(
    list(type = "Phylum",  names = unique(phyla[!is.na(phyla) & nzchar(phyla)])),
    list(type = "Class",   names = unique(classes[!is.na(classes) & nzchar(classes)])),
    list(type = "Order",   names = unique(orders[!is.na(orders) & nzchar(orders)]))
  )
  cache <- list()
  for (rk in ranks) {
    type <- rk$type
    rnames <- rk$names
    for (nm in rnames) {
      if (nm %in% names(cache)) next
      Sys.sleep(delay)
      recs <- tryCatch(wm_records_name(nm, marine_only = FALSE), error = function(e) NULL)
      if (is.null(recs) || nrow(recs) == 0) { cache[[nm]] <- NULL; next }
      matched <- NULL
      for (i in seq_len(nrow(recs))) {
        if (tolower(trimws(recs$scientificname[i])) == tolower(trimws(nm)) &&
            tolower(trimws(recs$rank[i])) == tolower(trimws(type))) {
          matched <- recs[i, ]
          break
        }
      }
      if (is.null(matched)) matched <- recs[1, ]
      aid <- matched$AphiaID %||% NA
      if (is.na(aid)) { cache[[nm]] <- NULL; next }
      Sys.sleep(delay)
      attrs <- tryCatch(wm_attr_data(aid, include_inherited = TRUE), error = function(e) NULL)
      if (is.null(attrs) || nrow(attrs) == 0) { cache[[nm]] <- NULL; next }
      traits <- list()
      for (i in seq_len(nrow(attrs))) {
        mt <- attrs$measurementType[i] %||% ""
        mv <- attrs$measurementValue[i] %||% ""
        if (nzchar(mt) && nzchar(mv)) traits[[length(traits) + 1]] <- list(type = mt, value = mv)
      }
      cache[[nm]] <- traits
    }
  }
  cache
}

# Sanity check at the end of enrichment analysis to repair
# cases where data exists but was not correctly classified.
#
# Round 34 (2026-09-28): the old "Classification Repair" step here --
# a raw literal substring match (grepl(tolower(reference_name),
# tolower(WoRMS_Distribution))) applied to any row still "Unknown" after
# the full hybrid rule chain in mod_f_enrichment.R -- has been REMOVED.
# It was an undocumented live duplicate of the exact raw text-match check
# Round 33 believed it had fully retired from the classification logic;
# it just lived in a different function in a different file, so the
# Round 33 grep audit (which searched for the .text_match/text_match
# variable names) never caught it. It ran unconditionally on every
# "Unknown" row with no OBIS/GBIF check and no text-classifier-tier
# check of any kind -- silently overwriting the rule chain's deliberate
# "we don't have corroborating evidence either way" result with a
# single, uncorroborated signal, for precisely the ambiguous cases that
# most needed multi-signal caution. Per explicit instruction, a row the
# rule chain leaves at "Unknown" now stays "Unknown" -- a real, honest
# "we don't know", not a value manufactured by a bypassed check.
run_enrichment_sanity_check <- function(df, reference_name, trait_cache) {
  message("Running final enrichment sanity check...")

  # Trait Repair
  # If attributes have content but trait columns are missing,
  # re-classify using the trait cache.
  #
  # Round 34 (2026-09-28) fix: the trigger condition is OR'd across two
  # fields (WoRMS_FunctionalGroup NA, OR Group1 NA) -- either one alone is
  # enough to enter this branch -- but the repair action used to overwrite
  # Group1/Group2/TaxGroup UNCONDITIONALLY whenever it fired. That meant a
  # row whose Group1 was already correctly resolved by an earlier, more
  # specific stage of the pipeline could get silently clobbered by a
  # coarser Phylum/Class/Order-level trait_cache guess, purely because the
  # unrelated WoRMS_FunctionalGroup field happened to be NA (a field this
  # block never actually fills in the first place -- it's only ever used
  # here as a trigger heuristic). Confirmed reproducible: a
  # correctly-resolved "meroplankton" was overwritten to "benthic
  # invertebrate" in a synthetic test with exactly this shape. Fixed by
  # only filling each target field when IT is actually missing, never
  # overwriting one that already has a value.
  df_list <- split(df, seq(nrow(df)))

  repaired_list <- lapply(df_list, function(row) {
    if (!is.na(row$WoRMS_attributes) && nzchar(row$WoRMS_attributes) &&
        (is.na(row$WoRMS_FunctionalGroup) || is.na(row$Group1))) {

      # Re-classify
      res <- classify_by_trait_cache(row$Phylum, row$Class, row$Order, trait_cache)

      if (!is.null(res)) {
        if (is.na(row$Group1))   row$Group1   <- res$Group1
        if (is.na(row$Group2))   row$Group2   <- res$Group2
        if (is.na(row$TaxGroup)) row$TaxGroup <- res$TaxGroup
      }
    }
    row
  })
  
  bind_rows(repaired_list)
}
classify_by_trait_cache <- function(phylum, class, order, cache, species_traits = NULL) {
  candidates <- list(
    list(rank = "Phylum", name = phylum),
    list(rank = "Class",  name = class),
    list(rank = "Order",  name = order)
  )
  has_benthos <- FALSE
  has_plankton <- FALSE
  has_parasitic <- FALSE
  has_fish <- FALSE
  
  # Helper to check traits
  check_traits <- function(traits) {
    if (is.null(traits) || length(traits) == 0) return(NULL)
    for (tr in traits) {
      tr_type <- tolower(trimws(tr$type))
      tr_val <- tolower(trimws(tr$value))
      if (tr_type == "functional group" && grepl("benthos", tr_val)) has_benthos <<- TRUE
      if (tr_type == "functional group" && grepl("plankton", tr_val)) has_plankton <<- TRUE
      if (tr_type == "ecological interactions" && grepl("parasitic", tr_val)) has_parasitic <<- TRUE
      if (tr_type == "paraphyletic group" && grepl("pisces", tr_val)) has_fish <<- TRUE
    }
  }

  for (cand in candidates) {
    nm <- cand$name
    if (is.na(nm) || !nzchar(nm)) next
    check_traits(cache[[nm]])
  }
  
  # Check species-specific traits
  check_traits(species_traits)
  
  g1 <- NA_character_; g2 <- NA_character_; tg <- NA_character_
  if (has_parasitic) {
    g1 <- "parasitic"; g2 <- "parasitic"
  } else if (has_fish) {
    g1 <- "ichthyoplankton / necton"; g2 <- "ichthyoplankton / necton"; tg <- "fish"
  } else if (has_benthos && has_plankton) {
    # FIXED (2026-09-16): this branch used to set only g1 ("meroplankton"),
    # leaving g2 as NA. Downstream in mod_f_enrichment.R's priority cascade,
    # a column left NA here gets silently filled from the external
    # MarineSpeciesTraits DB instead (Priority 2) -- mixing two different
    # classification sources into what should be one coherent Group1/Group2
    # pair for the same organism, e.g. Group1="meroplankton" (from here,
    # correct) but Group2="holoplankton" (from a mismatched DB record) for a
    # barnacle, which can never be holoplanktonic in either column. Mirrors
    # infer_from_worms_attributes()'s already-correct, complete pairing for
    # this exact case (see Round 5a, worms_pipeline_app_v8_diagnosis.md) --
    # that function was fixed to set both columns together; this one, its
    # sibling and the one actually used by the live "Use WoRMS trait cache"
    # path, was not, until now.
    g1 <- "meroplankton"; g2 <- "benthic invertebrate"
  } else if (has_plankton) {
    g1 <- "holoplankton"; g2 <- "holoplankton"
  } else if (has_benthos) {
    # FIXED (2026-09-16): same issue as above -- used to set only g2, leaving
    # g1 NA to be independently (and possibly inconsistently) filled from the
    # DB or the pipeline lookup.
    g1 <- "benthic invertebrate"; g2 <- "benthic invertebrate"
  } else {
    return(NULL)
  }
  data.frame(Group1 = g1, Group2 = g2, TaxGroup = tg, stringsAsFactors = FALSE)
}

# ------------------------------------------------------------------------
# NOTE (Round 6, 2026-09-11): the "TRAIT VALIDATION (EoL TraitBank
# Integration)" block that used to live here (map_eol_to_internal(),
# fetch_eol_traits()) was removed. It was never wired into anything -- not
# called from any module -- and its "API call" was an unconditional mock
# that always returned Group1="holoplankton"/Group2="planktonic" regardless
# of species, so it carried a false-negative/false-classification risk with
# zero real trait coverage to show for it. Investigated building a real
# integration: EoL's actual TraitBank access (github.com/EOL/eol_website
# doc/api.md, doc/api-access.md) requires a JWT token tied to an individual
# EOL account and queries written in Neo4j Cypher against their graph
# database -- not an open name-lookup REST endpoint like WoRMS/OBIS/GBIF --
# and the exact trait-predicate vocabulary (habitat/benthic/planktonic
# terms) isn't in the public docs; discovering it needs live querying with
# a real token. Given that account/token dependency and the unverifiable
# trait vocabulary, the decision was to remove the stub rather than keep a
# misleading mock in place. If EoL integration becomes worth pursuing later,
# start from https://github.com/EOL/eol_website/blob/master/doc/api.md.
# ------------------------------------------------------------------------

# Fetch bounding box for a reference MRGID
# Hardcoded fallback bboxes for common reference regions (used when the
# Marine Regions API is unreachable, so OBIS/GBIF presence checks still work).
FALLBACK_BBOXES <- list(
  "2350" = c(minLat = 48.5, maxLat = 62.5, minLon = -5.0, maxLon = 10.0)  # North Sea
)

# FIXED (round 20, 2026-09-21): a single rectangular bbox around the North
# Sea -- whether the FALLBACK_BBOXES entry above, or the live-fetched MRGID
# 2350 bounding box from Marine Regions, which is itself just as coarse a
# rectangle -- necessarily spans the full width of the British Isles at
# this latitude range, so it captures the Irish Sea and Scotland's west
# coast right along with the real North Sea. Confirmed with real GBIF
# records for Carinoma armandi: all 5 of its actual occurrences (Firth of
# Clyde, ~55.95N/5.68W; Liverpool Bay, 53.64N/3.01W) are Irish Sea/west-
# coast records, yet Liverpool Bay falls inside the old single rectangle,
# which pushed Occurrence_classification from "Unknown" to "Possible" via
# the OBIS/GBIF gap-filler in mod_f_enrichment.R for a species with zero
# real North Sea evidence.
#
# Fixed with two latitude-banded rectangles instead of one. A single
# tightened western edge can't work: at Liverpool's latitude (~53.6N) the
# real North Sea coast starts around 0 degrees longitude, while at
# Scotland's latitude (~56N) the real North Sea coast (Edinburgh, -3.2W)
# and the Irish Sea (Firth of Clyde, -5.7W) are only ~2.5 degrees of
# longitude apart -- a cutoff safe for one band is wrong for the other.
# Verified against 19 real/known coordinates: the 5 Carinoma armandi
# false-positive points, 8 genuine North Sea/German Bight/Skagerrak points,
# and 6 further edge cases (Shetland, Orkney, the Outer Hebrides, Bristol
# Channel, Dover Strait, the Isle of Man) -- see
# claude/worms_pipeline_app_v8_diagnosis.md, Round 20, for the full test.
#
# This is a North-Sea-specific correction, not a general fix for arbitrary
# reference regions: a user who picks a different region via the region
# search box (Mediterranean, Gulf of Mexico, etc.) still gets the single
# live-fetched-or-fallback rectangle below, which has the same class of
# coastline-adjacency risk for whatever sea is geographically next door to
# THAT region. If the same false-positive pattern is ever demonstrated for
# another region, it would need its own tailored multi-box (or real
# polygon) refinement the same way.
NORTH_SEA_REFINED_BOXES <- list(
  south = c(minLat = 51.0, maxLat = 55.0, minLon = -1.5, maxLon = 12.0),
  north = c(minLat = 55.0, maxLat = 61.5, minLon = -4.3, maxLon = 12.0)
)

# Converts a gazetteer record's center point + precision radius (meters)
# into an approximate bbox, for records that have no min/max bounding box
# at all -- some Marine Regions features (bights, smaller bays, etc., as
# opposed to full IHO Sea Areas) are stored this way: one coordinate plus a
# precision radius, no polygon and no bbox. Round 43 follow-up #4
# (2026-09-29): discovered live for German Bight itself (MRGID 3427,
# placeType "Bight") -- confirmed via the gazetteer's own record page:
# latitude 54.21555556, longitude 8.06805556, precision 150000 (meters), no
# bounding-box field whatsoever. get_reference_bbox() used to just give up
# on a record shaped like this. 1 degree latitude is ~111,320 m everywhere;
# 1 degree longitude is ~111,320*cos(latitude) m (meridians converge toward
# the poles) -- both fine approximations for a rough OBIS/GBIF `geometry`
# query box, not meant to be a precise coastline.
.point_precision_to_bbox <- function(lat, lon, precision_m) {
  if (any(is.na(c(lat, lon, precision_m))) || precision_m <= 0) return(NULL)
  meters_per_deg_lat <- 111320
  delta_lat <- precision_m / meters_per_deg_lat
  meters_per_deg_lon <- 111320 * cos(lat * pi / 180)
  if (is.na(meters_per_deg_lon) || meters_per_deg_lon <= 0) return(NULL)
  delta_lon <- precision_m / meters_per_deg_lon
  c(minLat = lat - delta_lat, maxLat = lat + delta_lat,
    minLon = lon - delta_lon, maxLon = lon + delta_lon)
}

get_reference_bbox <- function(mrgid) {
  # North Sea gets the refined multi-box treatment unconditionally -- see
  # NORTH_SEA_REFINED_BOXES above. Both the live Marine Regions bbox and the
  # FALLBACK_BBOXES entry for this MRGID are just as coarse a single
  # rectangle, so there's nothing to gain by trying either first.
  if (!is.na(mrgid) && as.character(mrgid) == "2350") {
    return(NORTH_SEA_REFINED_BOXES)
  }

  # First try the Marine Regions API to get the actual record
  rec <- get_gazetteer_record_by_mrgid(mrgid)
  if (!is.null(rec) && all(c("minLatitude", "maxLatitude", "minLongitude", "maxLongitude") %in% names(rec))) {
    bbox <- c(minLat = rec$minLatitude[1], maxLat = rec$maxLatitude[1],
              minLon = rec$minLongitude[1], maxLon = rec$maxLongitude[1])
    if (all(!is.na(bbox))) return(bbox)
  }
  # Round 43 follow-up #4: no min/max bbox on file -- try deriving an
  # approximate one from a center-point + precision-radius record instead
  # (see .point_precision_to_bbox() above) before falling back further.
  if (!is.null(rec) && all(c("latitude", "longitude", "precision") %in% names(rec))) {
    pt_bbox <- .point_precision_to_bbox(rec$latitude[1], rec$longitude[1], rec$precision[1])
    if (!is.null(pt_bbox)) {
      message(sprintf("MRGID %s has no bounding box on file -- derived an approximate one from its center point + %.0f m precision radius.",
                       as.character(mrgid), rec$precision[1]))
      return(pt_bbox)
    }
  }
  # Round 44d (2026-09-30) diagnostic: a real run showed BOTH German Bight
  # (3427) and Eastern German Bight (29724) failing to reach even the
  # point+precision fallback above, despite 3427 being confirmed (by direct
  # inspection of its gazetteer page) to be exactly a center-point +
  # 150 km-precision record with no minLatitude/maxLatitude/minLongitude/
  # maxLongitude fields at all -- meaning either get_gazetteer_record_by_mrgid()
  # isn't returning what's expected for THIS endpoint (as opposed to the
  # name-search endpoint this code's field names were checked against), or
  # the request itself is failing. Surfaces exactly what was retrieved so the
  # next run's console output pinpoints the real cause instead of guessing
  # further blind.
  if (is.null(rec)) {
    message(sprintf("[BboxDebug] MRGID %s: get_gazetteer_record_by_mrgid() returned NULL (request failed, non-200 status, or unparseable response).", as.character(mrgid)))
  } else {
    message(sprintf("[BboxDebug] MRGID %s: gazetteer record fetched but has neither a usable min/max bounding box nor a usable latitude/longitude/precision triple. Fields present: %s",
                     as.character(mrgid), paste(names(rec), collapse = ", ")))
    for (fld in intersect(c("minLatitude", "maxLatitude", "minLongitude", "maxLongitude", "latitude", "longitude", "precision"), names(rec))) {
      message(sprintf("[BboxDebug]   %s = %s", fld, paste(rec[[fld]], collapse = "; ")))
    }
  }
  # Fallback to hardcoded bbox if available
  key <- as.character(mrgid)
  if (key %in% names(FALLBACK_BBOXES)) {
    message(sprintf("Marine Regions API unreachable for MRGID %s — using fallback bounding box.", key))
    return(FALLBACK_BBOXES[[key]])
  }
  NULL
}

# Two boxes that share a full edge (e.g. NORTH_SEA_REFINED_BOXES's south/
# north stack, touching at lat 55) cannot be emitted as a MULTIPOLYGON of
# two independent rectangle rings -- OGC/GEOS treats the shared edge as a
# self-intersection and the geometry is invalid (confirmed via shapely:
# "Self-intersection[12 55]"), even though the two rectangles are each
# individually fine. This traces the union of such a stack as a single
# simple polygon ring instead.
#
# Handles the one topology NORTH_SEA_REFINED_BOXES actually needs: two
# boxes stacked on latitude, touching at south$maxLat == north$minLat,
# with their EAST edges aligned (same maxLon) and only the west edge
# stepping in or out. Verified valid, correctly counter-clockwise, and
# passing all 19 North Sea/Irish Sea test points via shapely (see
# claude/worms_pipeline_app_v8_diagnosis.md, Round 20). Any other
# relationship between the two boxes (not touching, touching on longitude
# instead of latitude, or a step on the east edge) returns NULL and falls
# through to the plain MULTIPOLYGON path in .bbox_to_wkt() below, which is
# only guaranteed valid there when the boxes are genuinely disjoint -- if a
# future reference region needs a different stacked-box shape, this
# function should be extended rather than assumed to already cover it.
.merge_stacked_boxes <- function(b1, b2) {
  if (isTRUE(all.equal(unname(b1["maxLat"]), unname(b2["minLat"])))) {
    south <- b1; north <- b2
  } else if (isTRUE(all.equal(unname(b2["maxLat"]), unname(b1["minLat"])))) {
    south <- b2; north <- b1
  } else {
    return(NULL)
  }
  if (!isTRUE(all.equal(unname(south["maxLon"]), unname(north["maxLon"])))) {
    return(NULL)
  }
  midLat  <- unname(south["maxLat"])
  eastLon <- unname(south["maxLon"])
  c(
    sprintf("%f %f", unname(south["minLon"]), unname(south["minLat"])),
    sprintf("%f %f", eastLon,                  unname(south["minLat"])),
    sprintf("%f %f", eastLon,                  unname(north["maxLat"])),
    sprintf("%f %f", unname(north["minLon"]),  unname(north["maxLat"])),
    sprintf("%f %f", unname(north["minLon"]),  midLat),
    sprintf("%f %f", unname(south["minLon"]),  midLat),
    sprintf("%f %f", unname(south["minLon"]),  unname(south["minLat"]))  # close ring
  )
}

# Build a WKT geometry string from one bbox (a single named
# minLat/maxLat/minLon/maxLon vector) or several (a list of such vectors,
# e.g. NORTH_SEA_REFINED_BOXES) -- POLYGON for one box, MULTIPOLYGON for
# more than one (or a merged single POLYGON when exactly two boxes form a
# touching stack -- see .merge_stacked_boxes() above). Both OBIS's and
# GBIF's `geometry` query parameter accept either. Corners are listed
# counter-clockwise (minLon/minLat -> maxLon/minLat -> maxLon/maxLat ->
# minLon/maxLat -> close) -- GBIF specifically requires counter-clockwise
# WKT, or it silently treats the shape as an EXCLUSION zone instead of an
# inclusion one.
.bbox_to_wkt <- function(bbox) {
  bbox_list <- if (is.list(bbox)) bbox else list(bbox)

  if (length(bbox_list) == 2) {
    pts <- .merge_stacked_boxes(bbox_list[[1]], bbox_list[[2]])
    if (!is.null(pts)) {
      return(sprintf("POLYGON((%s))", paste(pts, collapse = ", ")))
    }
  }

  rings <- vapply(bbox_list, function(b) {
    sprintf(
      "((%f %f, %f %f, %f %f, %f %f, %f %f))",
      b["minLon"], b["minLat"],
      b["maxLon"], b["minLat"],
      b["maxLon"], b["maxLat"],
      b["minLon"], b["maxLat"],
      b["minLon"], b["minLat"]
    )
  }, character(1))
  if (length(rings) == 1) sprintf("POLYGON%s", rings[1]) else sprintf("MULTIPOLYGON(%s)", paste(rings, collapse = ", "))
}

# Flattens a bbox (single vector or list of vectors) into one string for use
# in evidence-cache keys, so a species is only ever queried once per
# distinct box shape regardless of whether bbox is a single region or
# NORTH_SEA_REFINED_BOXES's multi-box list.
.bbox_cache_key <- function(bbox) {
  bbox_list <- if (is.list(bbox)) bbox else list(bbox)
  paste(vapply(bbox_list, function(b) paste(b, collapse = "_"), character(1)), collapse = "|")
}

# ------------------------------------------------------------------------
# Real reference-region polygons (Round 20 follow-up, 2026-09-21)
#
# NORTH_SEA_REFINED_BOXES above approximates the North Sea/Irish Sea
# boundary with two hand-drawn rectangles. This section replaces that
# approximation with the ACTUAL Marine Regions coastline polygon for MRGID
# 2350, fetched live from Marine Regions' own `getGazetteerGeometries` REST
# endpoint (https://www.marineregions.org/rest/getGazetteerGeometries.jsonld/2350/,
# the same REST family already used by get_gazetteer_record_by_mrgid()
# above) and simplified offline for use in OBIS/GBIF `geometry` queries.
#
# The live response is enormous (a single POLYGON with 89,696 exterior
# vertices, 1,962 interior-ring holes for small islands, ~3.3 million
# characters of WKT) -- far too large for GBIF, which enforces a practical
# ~1500-character WKT limit. Holes were dropped (irrelevant for a simple
# presence/absence check against the outer sea boundary) and the exterior
# ring simplified with Douglas-Peucker (tolerance 0.7 decimal degrees,
# via Python's shapely: `polygon.simplify(0.7, preserve_topology=True)`)
# down to 64 vertices / 915 characters, then re-oriented counter-clockwise
# (the raw Marine Regions ring is clockwise, which GBIF silently treats as
# an EXCLUSION zone rather than an inclusion one if left as-is -- see the
# .bbox_to_wkt() comment above for the same gotcha).
#
# Verified against the raw, unsimplified 89,696-vertex polygon at 24 test
# points before and after simplification, with zero disagreements: the 5
# Carinoma armandi Irish Sea/west-coast false-positive coordinates that
# motivated NORTH_SEA_REFINED_BOXES, 8 solidly-North-Sea points (German
# Bight, Dutch coast, central North Sea, off Scarborough, off Esbjerg,
# Norwegian coast, Dover Strait, Orkney), and 11 tighter pinch-point checks
# along Scotland's coast, the Wadden Sea border, and the Channel/Irish Sea
# approaches (Firth of Forth, Aberdeen, Mull of Kintyre, Cape Wrath, mid
# Irish Sea, etc.) -- see claude/worms_pipeline_app_v8_diagnosis.md, Round
# 20 follow-up, for the full test and the Python/shapely verification
# script. Not independently verified against a live OBIS/GBIF query (this
# sandbox cannot reach api.obis.org/api.gbif.org).
#
# IMPORTANT, discovered while building this test set: the official Marine
# Regions "North Sea" (2350) polygon is the strict IHO definition -- it
# excludes Skagerrak/Kattegat (their own separate named sea areas) and
# stops short of Shetland's waters. If OSPAR's broader "Greater North Sea"
# definition (which folds in Skagerrak and Kattegat -- the convention used
# in EU marine policy/MSFD/OSPAR QSR work) is what this pipeline actually
# wants for its ecological scope, this polygon would need to be unioned
# with Skagerrak's own Marine Regions polygon. Flagged for a decision, not
# assumed here -- ask before changing.
#
# To add another region (Mediterranean, Gulf of Mexico, etc.): fetch its
# geometry the same way (swap the MRGID in the URL), run it through the
# same fetch -> drop holes -> simplify -> orient -> verify pipeline, and add
# an entry below. Any MRGID not listed here falls back to the
# get_reference_bbox()/.bbox_to_wkt() single-rectangle approach unchanged.
#
# FIXED (Round 34 follow-up, 2026-09-28): the German Bight stretch of this
# ring (originally "...7.911 53.718, 8.884 53.024, 8.667 53.891, 12.006
# 52.875, 9.220 54.346, [~28 more tightly-clustered points], 8.582
# 54.300, 8.123 56.557...") was corrupted -- one vertex (12.006 52.875)
# sits deep inland in Brandenburg, Germany, nowhere near any coastline,
# and the ~30-point cluster right after it is a degenerate, self-
# intersecting loop (confirmed with shapely: removing just the bad vertex
# alone leaves the ring invalid -- Self-intersection at [9.19 54.33]).
# Found while investigating a genuine OBIS/GBIF double-negative result for
# Edwardsia timida during the Round 34 occurrence-classification work:
# confirmed live that its 70 real OBIS + 246 real GBIF records worldwide
# include none inside the app's polygon at all (so that specific case
# wasn't caused by this bug), but testing the polygon directly against
# known real coordinates found it was wrongly excluding genuine German
# Bight coastal/estuary waters regardless -- Cuxhaven (Elbe mouth,
# 8.70E/53.87N) and Wilhelmshaven (Jade Bay, 8.11E/53.53N) both fell
# outside a polygon meant to include the whole North Sea coast.
#
# Fixed by replacing the entire corrupted stretch (everything between the
# East Frisian coast point 7.911/53.718 and the Danish coast point
# 8.123/56.557, both left unchanged) with 6 clean, manually-placed
# waypoints that dip south to properly wrap Jade Bay and the Weser
# estuary before rising north past the Elbe mouth toward the Danish
# coast. Verified with shapely (Python) against 17 real-world reference
# points: Cuxhaven, Wilhelmshaven, Bremerhaven, Borkum, Norderney,
# Helgoland, Sylt, and "off Esbjerg" (Denmark) all now correctly INSIDE;
# Firth of Clyde and Liverpool Bay (Irish Sea -- must stay excluded, same
# false-positive risk NORTH_SEA_REFINED_BOXES above was built to avoid),
# a deep-inland-Germany sanity point, and Skagerrak/Kattegat (excluded by
# this MRGID's strict IHO definition -- see the note above) all correctly
# OUTSIDE; central North Sea, Dutch coast, and off Scarborough (UK) also
# correctly INSIDE. The fixed ring is valid (no self-intersections) and
# actually simpler than before -- 41 vertices / 592 WKT characters, down
# from 64 vertices / 915 characters, since the corrupted detail cluster
# is gone. Not independently verified against a live OBIS/GBIF query
# (this sandbox cannot reach api.obis.org/api.gbif.org) -- the user
# confirmed the Cuxhaven/Wilhelmshaven exclusion live on their own
# machine (check_polygon_bug.R) before this fix was made.
REFERENCE_REGION_POLYGONS <- list(
  "2350" = "POLYGON((5.203 60.832, -0.883 61.000, -4.401 57.918, -4.443 57.483, -1.824 57.613, -3.857 56.114, -3.674 56.012, -3.067 55.945, -2.679 56.063, -2.136 55.916, 0.037 52.904, 1.698 52.722, 0.212 51.483, 0.273 51.455, 0.404 51.443, 0.702 51.472, 0.896 51.336, 1.426 51.394, 1.931 50.995, 4.383 51.080, 5.465 51.887, 4.099 51.984, 5.439 52.516, 4.728 52.960, 7.911 53.718, 7.850 53.450, 8.750 53.450, 8.950 53.950, 8.600 54.900, 8.550 55.500, 8.200 55.650, 8.123 56.557, 9.366 57.024, 8.237 56.701, 5.656 58.558, 6.601 59.559, 5.224 59.527, 7.140 60.494, 6.209 60.469, 5.766 59.979, 5.203 60.832))",

  # Mediterranean Sea (MRGID 1905, IHO Sea Area). Unlike the North Sea,
  # Marine Regions stores this MRGID's geometry as 10 separate constituent
  # sub-basin polygons (Alboran, Western Basin, Ligurian, Tyrrhenian,
  # Ionian, Adriatic, Aegean, Eastern Basin, etc. -- ~13.5 million
  # characters combined, 251,448 exterior vertices once unioned), not one
  # single outline like 2350's. Unlike the North Sea/Skagerrak split, this
  # union is NOT missing any of the Mediterranean's own named seas -- all
  # 10 sub-basins are Marine Regions' own constituent parts of MRGID 1905,
  # confirmed to union into a single seamless polygon (no gaps between
  # them). It DOES exclude the Sea of Marmara (its own separate MRGID,
  # 3369) and the Black Sea (a wholly separate sea) -- confirmed both fall
  # outside during verification, consistent with how this MRGID is scoped.
  #
  # Processed the same way as the North Sea: dropped all interior holes
  # (695+ per sub-basin, mostly small islands -- same rationale as before),
  # unioned the 10 exteriors (shapely.ops.unary_union -- came back as a
  # single valid Polygon, not a MultiPolygon), simplified with
  # Douglas-Peucker (tolerance 0.55 decimal degrees -- coarser tolerances
  # up to 0.7 fit under GBIF's budget with more margin but incorrectly
  # excluded the head of the Adriatic near Venice, caught by testing a
  # point there specifically), and re-oriented counter-clockwise.
  #
  # Verified against the raw, unsimplified 251,448-vertex union at 19 test
  # points (11 solidly-Mediterranean points spanning all sub-basins
  # including the Adriatic's northern end near Venice, 6 solidly-outside
  # points -- Atlantic past Gibraltar, Black Sea, Bay of Biscay, North Sea,
  # Red Sea, Sea of Marmara) plus 9 further pinch-point checks (Sicily
  # Strait, Strait of Gibraltar, Dardanelles approach, etc.) comparing
  # simplified vs. raw containment directly -- zero disagreements. Not
  # independently verified against a live OBIS/GBIF query. See
  # claude/worms_pipeline_app_v8_diagnosis.md for the full test.
  "1905" = "POLYGON((15.630 40.071, 8.746 44.427, 6.160 43.027, 4.736 43.924, 4.611 43.854, 4.596 43.652, 4.609 43.634, 4.653 43.616, 4.659 43.591, 4.717 43.542, 4.732 43.427, 4.838 43.331, 3.195 43.207, 3.179 41.867, 0.484 40.953, -0.718 37.607, -6.033 36.180, -4.682 35.205, -2.009 35.071, 2.895 36.805, 11.036 37.089, 11.128 36.849, 10.467 36.128, 11.167 35.230, 10.184 33.823, 19.056 30.268, 20.070 30.874, 20.089 32.185, 22.145 32.938, 29.020 30.822, 31.923 31.525, 32.343 31.298, 32.302 31.229, 32.314 30.789, 32.343 30.706, 32.270 30.583, 32.357 30.412, 32.303 30.341, 32.573 30.068, 32.576 30.199, 32.563 30.235, 32.305 30.568, 32.327 30.621, 32.346 30.709, 32.310 31.094, 32.359 31.250, 34.332 31.421, 35.990 34.546, 36.017 36.930, 32.803 36.015, 31.037 36.854, 29.683 36.130, 27.363 36.684, 28.331 37.033, 27.264 36.964, 27.253 37.978, 26.232 38.267, 27.170 38.441, 27.171 38.448, 27.159 38.467, 27.027 38.468, 26.949 38.432, 26.722 38.652, 26.942 39.570, 26.063 39.479, 26.823 40.639, 25.114 41.091, 23.730 40.747, 23.986 39.945, 22.589 40.467, 23.352 39.186, 22.532 38.857, 24.087 37.778, 23.013 37.922, 23.202 36.440, 22.483 36.385, 21.126 37.937, 23.224 38.154, 22.404 38.449, 21.101 38.335, 19.288 40.420, 19.463 42.010, 15.951 43.505, 14.559 45.298, 13.905 44.770, 13.490 45.486, 13.630 45.770, 12.234 45.458, 12.374 44.239, 14.190 42.490, 18.518 40.135, 16.941 40.465, 16.487 39.765, 17.172 38.957, 16.063 37.924, 15.630 40.071))"
)

# ------------------------------------------------------------------------
# Dynamic reference-region polygons for ANY MRGID (2026-09-25)
#
# REFERENCE_REGION_POLYGONS above only covers the two regions someone has
# manually fetched, dropped holes from, simplified via Python/shapely, and
# pasted in as a literal string (North Sea 2350, Mediterranean 1905). Every
# other reference region a user picks still falls back to the coarse
# rectangular bbox. This section generalizes the SAME pipeline -- fetch the
# real Marine Regions coastline, drop holes, simplify to fit GBIF's ~1500
# character WKT budget, fix winding order -- to run automatically, in pure
# base R, for any MRGID, the first time it's ever selected, then caches the
# result to disk so that cost is paid once per region ever used, not once
# per pipeline run.
#
# Why no sf/GEOS dependency was needed: the three things the manual Python
# pipeline used shapely for all turn out not to need a geometry library once
# you only need the OUTER boundary, not a topologically valid geometry
# object: dropping holes is "keep the first ring of each POLYGON entry" (a
# text-parsing task); Douglas-Peucker simplification operates on a single
# ring in isolation (a well-known ~20-line recursive algorithm); and
# unioning multi-part regions (like the Mediterranean's 10 sub-basins) is
# unnecessary because WKT MULTIPOLYGON already accepts independently-listed
# parts without merging them first (the same thing NORTH_SEA_REFINED_BOXES'
# two-rectangle case already relies on via .bbox_to_wkt() above). Winding
# order is a shoelace-formula sign check.
#
# Verified offline (this sandbox cannot reach marineregions.org) by running
# this exact pipeline against the raw getGazetteerGeometries responses
# already downloaded for 2350/1905 (the two _sample_geometry_*.json files)
# and confirming point-in-polygon agreement with both the raw, unsimplified
# geometry and the existing hand-verified REFERENCE_REGION_POLYGONS entries
# at every one of the named test points from that hand-verification (German
# Bight, Dutch coast, Scarborough, Orkney, Alboran, Adriatic/Venice, etc. --
# see claude/worms_pipeline_app_v8_diagnosis.md, Round 23). Random-sample
# sweeps across the full bounding box extent show disagreement concentrated
# right along the simplified coastline itself (~3-5% of uniformly random
# points, expected and inherent to any simplification, not a defect --
# nowhere else). Not independently verified against a live OBIS/GBIF query.
# REFERENCE_REGION_POLYGONS' two existing entries are left as pinned,
# hand-verified constants and are NOT regenerated by this pipeline; it only
# runs for MRGIDs not already listed there.

DYNAMIC_GEOMETRY_MAX_WKT_CHARS <- 1450L
DYNAMIC_GEOMETRY_TOL_START     <- 0.05   # decimal degrees; fine-to-coarse search preserves detail
DYNAMIC_GEOMETRY_TOL_GROWTH    <- 1.6
DYNAMIC_GEOMETRY_MAX_ATTEMPTS  <- 20L
DYNAMIC_GEOMETRY_CACHE_VERSION <- "v1"   # bump to invalidate all cached dynamic polygons if this pipeline changes

.strip_crs_prefix <- function(wkt) sub("^\\s*<[^>]+>\\s*", "", wkt)

# Extract ONLY the exterior ring (drops interior-ring holes) from a single WKT
# POLYGON string (CRS prefix already stripped), as a numeric 2-col (lon, lat)
# matrix. Marine Regions' getGazetteerGeometries returns each constituent
# part (1 for most regions, several for a region like the Mediterranean made
# of separate sub-basins) as its own such POLYGON string, with any holes
# embedded as additional rings after the exterior one.
.wkt_polygon_exterior_ring <- function(wkt_poly) {
  body <- trimws(sub("^\\s*POLYGON\\s*", "", wkt_poly, ignore.case = TRUE))
  if (substr(body, 1, 1) != "(" || substr(body, nchar(body), nchar(body)) != ")") return(NULL)
  inner <- substr(body, 2, nchar(body) - 1)
  parts <- strsplit(inner, "\\)\\s*,\\s*\\(")[[1]]
  ring0 <- sub("\\)$", "", sub("^\\(", "", parts[1]))
  pairs <- strsplit(ring0, ",")[[1]]
  mat <- tryCatch(
    do.call(rbind, lapply(pairs, function(p) as.numeric(strsplit(trimws(p), "\\s+")[[1]]))),
    error = function(e) NULL
  )
  if (is.null(mat) || ncol(mat) != 2 || anyNA(mat)) return(NULL)
  colnames(mat) <- c("lon", "lat")
  mat
}

# Iterative Douglas-Peucker (stack-based, vectorized per-segment distance calc
# -- avoids the O(n^2)-ish cost a naive recursive/rbind version hits on a ring
# with tens of thousands of vertices) on an open point sequence. A closed
# ring's first==last point makes this behave correctly on rings too.
.douglas_peucker <- function(pts, tolerance) {
  n <- nrow(pts)
  if (n < 3) return(pts)
  keep <- logical(n); keep[1] <- TRUE; keep[n] <- TRUE
  stack <- list(c(1L, n))
  while (length(stack) > 0) {
    rng <- stack[[length(stack)]]; stack[[length(stack)]] <- NULL
    i1 <- rng[1]; i2 <- rng[2]
    if (i2 - i1 < 2) next
    inner_idx <- (i1 + 1L):(i2 - 1L)
    a <- pts[i1, ]; b <- pts[i2, ]
    px <- pts[inner_idx, 1]; py <- pts[inner_idx, 2]
    if (isTRUE(all.equal(unname(a), unname(b)))) {
      d <- sqrt((px - a[1])^2 + (py - a[2])^2)
    } else {
      num <- abs((b[2] - a[2]) * px - (b[1] - a[1]) * py + b[1] * a[2] - b[2] * a[1])
      den <- sqrt((b[2] - a[2])^2 + (b[1] - a[1])^2)
      d <- num / den
    }
    pos_local <- which.max(d); dmax <- d[pos_local]; pos <- inner_idx[pos_local]
    if (dmax > tolerance) {
      keep[pos] <- TRUE
      stack[[length(stack) + 1L]] <- c(i1, pos)
      stack[[length(stack) + 1L]] <- c(pos, i2)
    }
  }
  pts[keep, , drop = FALSE]
}

# Shoelace signed area (positive = counter-clockwise in lon/lat space, matching
# .bbox_to_wkt()'s convention above) and the CCW-enforcement GBIF's `geometry`
# parameter silently requires (a clockwise ring is treated as an EXCLUSION
# zone instead of an inclusion one).
.ring_signed_area <- function(ring) {
  x <- ring[, 1]; y <- ring[, 2]
  x2 <- c(x[-1], x[1]); y2 <- c(y[-1], y[1])
  0.5 * sum(x * y2 - x2 * y)
}
.ensure_ccw <- function(ring) {
  if (.ring_signed_area(ring) < 0) ring[rev(seq_len(nrow(ring))), , drop = FALSE] else ring
}
.ring_to_wkt_pts <- function(ring) paste(sprintf("%.3f %.3f", ring[, 1], ring[, 2]), collapse = ", ")

# Live fetch of an MRGID's raw geometry from Marine Regions' own
# getGazetteerGeometries REST endpoint -- the same REST family already used
# by get_gazetteer_record_by_mrgid() above. Returns the raw `mr:hasGeometry`
# list (one or more {"gsp:asWKT": "<crs> POLYGON (...)"} entries) or NULL.
fetch_gazetteer_geometry_raw <- function(mrgid, timeout_s = 25) {
  if (is.na(mrgid)) return(NULL)
  url <- sprintf("%s/getGazetteerGeometries.jsonld/%d/", marineregions_base_url, as.integer(mrgid))
  tryCatch({
    resp <- request(url) %>%
      req_timeout(timeout_s) %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
    if (resp_status(resp) != 200) return(NULL)
    parsed <- resp_body_json(resp, simplifyVector = FALSE)
    geoms <- parsed[["mr:hasGeometry"]]
    if (is.null(geoms)) return(NULL)
    if (!is.null(names(geoms)) && "gsp:asWKT" %in% names(geoms)) geoms <- list(geoms)
    geoms
  }, error = function(e) NULL)
}

# Full pipeline for one MRGID: fetch -> drop holes -> simplify (fine-to-coarse
# search for the finest detail that still fits GBIF's WKT budget) -> orient
# CCW -> assemble as POLYGON/MULTIPOLYGON WKT. Returns
# list(status, wkt) -- status is "ok", "fetch_failed" (worth retrying later,
# e.g. a network hiccup), "parse_failed", or "too_complex" (a genuine,
# deterministic property of that MRGID's geometry -- safe to remember).
build_dynamic_region_polygon_wkt <- function(mrgid, logger = message) {
  raw <- fetch_gazetteer_geometry_raw(mrgid)
  if (is.null(raw) || length(raw) == 0) return(list(status = "fetch_failed", wkt = NULL))

  raw_rings <- tryCatch(
    lapply(raw, function(g) .wkt_polygon_exterior_ring(.strip_crs_prefix(g[["gsp:asWKT"]]))),
    error = function(e) NULL
  )
  if (is.null(raw_rings)) return(list(status = "parse_failed", wkt = NULL))
  raw_rings <- Filter(function(r) !is.null(r) && nrow(r) >= 4, raw_rings)
  if (length(raw_rings) == 0) return(list(status = "parse_failed", wkt = NULL))

  tol <- DYNAMIC_GEOMETRY_TOL_START
  for (attempt in seq_len(DYNAMIC_GEOMETRY_MAX_ATTEMPTS)) {
    simp_rings <- tryCatch(lapply(raw_rings, function(r) .ensure_ccw(.douglas_peucker(r, tol))),
                            error = function(e) NULL)
    if (is.null(simp_rings)) return(list(status = "parse_failed", wkt = NULL))
    wkt <- if (length(simp_rings) == 1) {
      sprintf("POLYGON((%s))", .ring_to_wkt_pts(simp_rings[[1]]))
    } else {
      sprintf("MULTIPOLYGON(%s)", paste(vapply(simp_rings, function(r) sprintf("((%s))", .ring_to_wkt_pts(r)), character(1)), collapse = ", "))
    }
    if (nchar(wkt) <= DYNAMIC_GEOMETRY_MAX_WKT_CHARS) {
      logger(sprintf("[DynamicGeometry] MRGID %s: built polygon (%d part%s, %d vertices, %d chars, tolerance %.4f, attempt %d/%d).",
                      mrgid, length(simp_rings), if (length(simp_rings) == 1) "" else "s",
                      sum(vapply(simp_rings, nrow, integer(1))), nchar(wkt), tol, attempt, DYNAMIC_GEOMETRY_MAX_ATTEMPTS))
      return(list(status = "ok", wkt = wkt))
    }
    tol <- tol * DYNAMIC_GEOMETRY_TOL_GROWTH
  }
  logger(sprintf("[DynamicGeometry] MRGID %s: geometry too complex to fit under %d WKT characters even at tolerance %.4f -- using bounding box for this region instead.",
                  mrgid, DYNAMIC_GEOMETRY_MAX_WKT_CHARS, tol))
  list(status = "too_complex", wkt = NULL)
}

.dynamic_geom_cache_env <- new.env(parent = emptyenv())
dynamic_geom_cache_file <- file.path(cache_dir, "dynamic_geometry_cache.rds")
load_dynamic_geom_cache <- function() {
  if (file.exists(dynamic_geom_cache_file)) {
    data <- tryCatch(readRDS(dynamic_geom_cache_file), error = function(e) NULL)
    if (!is.null(data)) list2env(data, envir = .dynamic_geom_cache_env)
  }
}
save_dynamic_geom_cache <- function() {
  dir.create(cache_dir, showWarnings = FALSE)
  saveRDS(as.list(.dynamic_geom_cache_env), file = dynamic_geom_cache_file)
}
load_dynamic_geom_cache()

# Cached wrapper: NULL is a valid, meaningful cached result ("no usable
# dynamic polygon for this MRGID -- use the bbox"), which is why this checks
# `exists()` rather than testing the returned value's nullness. Only "ok"/
# "too_complex"/"parse_failed" are cached to disk (deterministic given the
# same raw geometry); "fetch_failed" is never cached since a network hiccup
# is worth retrying on the next call.
get_dynamic_region_polygon_wkt <- function(mrgid, logger = message) {
  if (is.na(mrgid)) return(NULL)
  key <- paste0(as.integer(mrgid), "_", DYNAMIC_GEOMETRY_CACHE_VERSION)
  if (exists(key, envir = .dynamic_geom_cache_env, inherits = FALSE)) {
    return(get(key, envir = .dynamic_geom_cache_env, inherits = FALSE))
  }
  res <- build_dynamic_region_polygon_wkt(mrgid, logger = logger)
  if (res$status %in% c("ok", "too_complex", "parse_failed")) {
    assign(key, res$wkt, envir = .dynamic_geom_cache_env)
    save_dynamic_geom_cache()
  }
  res$wkt
}

# Resolve the WKT geometry to use for OBIS/GBIF `geometry` queries against a
# reference region, in priority order: (1) the pinned, hand-verified real
# coastline polygon from REFERENCE_REGION_POLYGONS above, if this MRGID has
# one; (2) a dynamically fetched-and-simplified real coastline polygon for
# ANY other MRGID (see the pipeline above), cached after the first fetch;
# (3) the existing bbox-rectangle approach (get_reference_bbox()/
# .bbox_to_wkt()) if neither is available. `mrgid` may be NA (a caller that
# never supplied one) -- always falls back to bbox then.
get_reference_geometry_wkt <- function(mrgid, bbox, logger = message) {
  key <- as.character(mrgid)
  if (!is.na(mrgid) && key %in% names(REFERENCE_REGION_POLYGONS)) {
    return(REFERENCE_REGION_POLYGONS[[key]])
  }
  dyn_wkt <- get_dynamic_region_polygon_wkt(mrgid, logger = logger)
  if (!is.null(dyn_wkt)) return(dyn_wkt)
  .bbox_to_wkt(bbox)
}

# Cache-key counterpart to get_reference_geometry_wkt() -- a real polygon
# (pinned or dynamically fetched) is a fixed constant keyed by MRGID
# (version-suffixed so a future pipeline change busts old cache entries,
# same pattern as GAZETTEER_BFS_LOGIC_VERSION above), never by the numeric
# bbox. Calling get_dynamic_region_polygon_wkt() here is cheap: only the
# very first lookup for a given MRGID ever hits the network, everything
# after that (including this call) is an in-memory/disk cache read.
.reference_geometry_cache_key <- function(mrgid, bbox) {
  key <- as.character(mrgid)
  if (!is.na(mrgid) && key %in% names(REFERENCE_REGION_POLYGONS)) {
    return(paste0("realpoly_", key, "_v1"))
  }
  if (!is.na(mrgid) && !is.null(get_dynamic_region_polygon_wkt(mrgid))) {
    return(paste0("dyngeom_", key, "_", DYNAMIC_GEOMETRY_CACHE_VERSION))
  }
  .bbox_cache_key(bbox)
}

# ------------------------------------------------------------------------
# German Bight sub-region resolution (Round 43, 2026-09-29)
#
# Per explicit user decision: when the reference region is the North Sea
# (MRGID 2350), occurrence classification (mod_f_enrichment.R) distinguishes
# presence within German Bight specifically from presence only in the wider
# North Sea/North Atlantic. German Bight's own MRGID is resolved LIVE via
# gazetteer_search_by_name() above -- the exact same name-search flow the
# app's main reference-region picker already uses -- rather than hardcoded,
# per the user's explicit choice ("look up German Bight's own MRGID/
# geometry") over a simpler fixed bbox. Its real coastline geometry then
# comes for free from the existing get_reference_bbox()/
# get_reference_geometry_wkt() pipeline used for any other MRGID: a live
# fetch-and-simplify the first time (see build_dynamic_region_polygon_wkt()/
# get_dynamic_region_polygon_wkt() above), cached to disk after that exactly
# like any other region. The MRGID lookup itself is cached separately here
# (german_bight_mrgid_cache.rds) since it's a fixed Marine Regions record
# that will never change -- no reason to repeat that specific network call
# every session once it's succeeded once.
GERMAN_BIGHT_SEARCH_NAME <- "German Bight"
german_bight_mrgid_cache_file <- file.path(cache_dir, "german_bight_mrgid_cache.rds")

.german_bight_mrgid_cache_env <- new.env(parent = emptyenv())
load_german_bight_mrgid_cache <- function() {
  if (file.exists(german_bight_mrgid_cache_file)) {
    data <- tryCatch(readRDS(german_bight_mrgid_cache_file), error = function(e) NULL)
    if (!is.null(data)) list2env(data, envir = .german_bight_mrgid_cache_env)
  }
}
save_german_bight_mrgid_cache <- function() {
  dir.create(cache_dir, showWarnings = FALSE)
  saveRDS(as.list(.german_bight_mrgid_cache_env), file = german_bight_mrgid_cache_file)
}
load_german_bight_mrgid_cache()

# Resolves German Bight's MRGID + bounding box together, and caches BOTH to
# disk as a single unit (keyed with a version suffix so a future logic
# change can force a fresh lookup, same pattern as
# DYNAMIC_GEOMETRY_CACHE_VERSION above). Returns list(mrgid = NA_integer_,
# bbox = NULL) if the live gazetteer search fails or finds no USABLE match
# -- callers must treat that the same as "no German Bight sub-region
# available this run" and fall back to flat North Sea tiering, never
# hard-fail the whole enrichment run over it.
#
# FIXED (Round 43 follow-up #3, 2026-09-29): the original version accepted
# whichever candidate matched the NAME first (an exact match, else the
# gazetteer's own top relevance hit) without checking it actually had usable
# geographic data -- confirmed live that "German Bight" resolves an exact
# name match to MRGID 3427, which get_reference_bbox() cannot fetch a
# bounding box for at all (no valid min/max lat/lon on its gazetteer record,
# and no FALLBACK_BBOXES entry). That silently disabled German Bight
# tiering for the entire run (gb_active stayed FALSE, every
# Observed_in_*_GermanBight column stayed FALSE) while still logging a
# MRGID as "resolved" -- and then CACHED that dead MRGID to disk, so every
# future run repeated the exact same silent failure forever. Every
# candidate returned by the name search is now tried in turn (exact
# matches first, in the gazetteer's own relevance order, then the
# remaining fuzzy hits), calling get_reference_bbox() on each one and
# taking the first that actually returns a usable bbox -- not just the
# first that matches the name.
#
# Round 44h (2026-09-30), per direct user question ("MRGID 3427 has no
# bounding box on file ... is this derived each run? can't it be cached?"):
# before this round, ONLY the MRGID number was cached to disk -- the bbox
# itself was recomputed on EVERY run regardless, because
# resolve_german_bight_region() (below) unconditionally called
# get_reference_bbox(mrgid) itself every time it ran, cached MRGID or not.
# Unlike North Sea (MRGID 2350), which get_reference_bbox() special-cases
# with a zero-cost hardcoded return, German Bight (3427) has no bbox on
# file at all and always fell through to a LIVE Marine Regions HTTP fetch
# (get_gazetteer_record_by_mrgid()) followed by a fresh
# .point_precision_to_bbox() recomputation -- every run, forever, even
# though a fixed gazetteer MRGID's bbox is exactly as immutable as the
# MRGID itself. Fixed by caching the bbox alongside the MRGID, as a single
# list(mrgid=, bbox=) unit, under a fresh version-suffixed key
# ("german_bight_resolved_v1", distinct from the old bare-integer
# "german_bight_mrgid_v3" key so a pre-Round-44h disk cache entry -- which
# holds the wrong value SHAPE, not just a stale value -- is automatically
# ignored rather than misread, same self-healing pattern as every earlier
# version bump here). The bbox cached is exactly the cand_bbox that already
# gets computed below to VALIDATE the winning candidate in the first
# place, so nothing new is fetched to populate the cache -- it's simply
# not thrown away afterward anymore. Verified via 4 scenarios (cold
# resolve, warm in-process resolve, a simulated disk-reload resolve, and
# the failure path) confirming the network-shaped calls
# (gazetteer_search_by_name()/get_reference_bbox()) happen exactly once on
# a cold resolve and zero times on every resolve after that.
resolve_german_bight_mrgid <- function(logger = message) {
  key <- "german_bight_resolved_v1"
  if (exists(key, envir = .german_bight_mrgid_cache_env, inherits = FALSE)) {
    return(get(key, envir = .german_bight_mrgid_cache_env, inherits = FALSE))
  }
  hits <- gazetteer_search_by_name(GERMAN_BIGHT_SEARCH_NAME, like = TRUE, fuzzy = TRUE)
  mrgid <- NA_integer_
  bbox <- NULL
  if (nrow(hits) > 0 && "MRGID" %in% names(hits)) {
    name_col <- if ("preferredGazetteerName" %in% names(hits)) hits$preferredGazetteerName else rep(NA_character_, nrow(hits))
    is_exact <- tolower(trimws(name_col %||% "")) == tolower(GERMAN_BIGHT_SEARCH_NAME)
    # Exact-name matches first (in the gazetteer's own relevance order),
    # then every remaining fuzzy hit, in order.
    candidate_order <- c(which(is_exact), which(!is_exact))
    for (i in candidate_order) {
      cand_mrgid <- suppressWarnings(as.integer(hits$MRGID[i]))
      if (is.na(cand_mrgid)) next
      cand_bbox <- get_reference_bbox(cand_mrgid)
      if (!is.null(cand_bbox)) {
        mrgid <- cand_mrgid
        bbox <- cand_bbox
        break
      }
      logger(sprintf("[GermanBight] Candidate MRGID %d ('%s') matched the name but has no fetchable bounding box -- trying the next candidate.",
                      cand_mrgid, name_col[i] %||% "?"))
    }
  }
  result <- list(mrgid = mrgid, bbox = bbox)
  if (!is.na(mrgid)) {
    logger(sprintf("[GermanBight] Resolved MRGID %d via live Marine Regions gazetteer search (bounding box verified and cached).", mrgid))
    # Only a SUCCESSFUL resolution (MRGID + bbox together) is persisted to
    # disk. Round 44e (2026-09-30) fix, still in force: caching a FAILED
    # (NA) resolution to disk exactly like a successful one meant that once
    # resolution failed once, every later run silently short-circuited on
    # the cached failure and never even re-attempted. A failure is memoized
    # in-memory below for the rest of THIS run only (still avoids repeat
    # calls within one session), but every fresh run retries from scratch
    # until it actually succeeds.
    assign(key, result, envir = .german_bight_mrgid_cache_env)
    save_german_bight_mrgid_cache()
  } else {
    logger("[GermanBight] Live gazetteer search found no USABLE match for 'German Bight' (either no name match at all, or none of the matches had a fetchable bounding box) -- German Bight sub-tiering will be unavailable (falls back to flat North Sea tiering).")
    assign(key, result, envir = .german_bight_mrgid_cache_env)
  }
  result
}

# Thin wrapper kept as the stable public entry point (mod_f_enrichment.R,
# the standalone classification script, etc. all call this, never
# resolve_german_bight_mrgid() directly). As of Round 44h,
# resolve_german_bight_mrgid() already resolves and caches the MRGID AND
# its bbox together, so there is no separate get_reference_bbox(mrgid)
# call to make here anymore -- see the fully-commented rationale above
# resolve_german_bight_mrgid().
resolve_german_bight_region <- function(logger = message) {
  resolve_german_bight_mrgid(logger = logger)
}

# Check presence in OBIS within bounding box
#
# NOTE (fixed): OBIS's occurrence search API has no minlat/minlon/maxlat/maxlon
# parameters. Unrecognized query parameters are silently ignored rather than
# rejected, so the previous version of this function ran an UNFILTERED, global
# search for the scientific name and treated any worldwide record as "present in
# the reference region." Confirmed live against api.obis.org: querying
# "Enhydra lutris" (Pacific sea otter, never recorded in the North Sea) with
# minlat/minlon/maxlat/maxlon set to the North Sea bbox returned total=18725
# (a California record); the equivalent query using OBIS's real `geometry` WKT
# polygon parameter correctly returned total=0. Geographic filtering must use
# `geometry`, not a lat/lon box.
# Both check_obis_presence() and check_gbif_presence() return a
# list(present, lat, lon) instead of a bare logical: `present` is the
# presence flag as before, and `lat`/`lon` are the coordinates of one
# occurrence record returned by the query (NA if none / not present), so
# callers can show *where* the validating record was actually seen instead
# of just that a record existed somewhere in the bounding box.
# Single-name, single-database query -- the raw fetch logic shared by
# check_obis_presence()/check_gbif_presence() and their synonym fallback
# loop below. Cached per (name, bbox, db) exactly as before, so a name
# tried as a synonym for one species is reused if it's ever queried again
# (directly, or as another species' synonym).
.obis_query_once <- function(name, bbox) {
  empty <- list(present = FALSE, lat = NA_real_, lon = NA_real_)
  key <- paste(name, .bbox_cache_key(bbox), "obis", sep="_")
  if (exists(key, envir = .evidence_cache_env)) return(get(key, envir = .evidence_cache_env))

  polygon <- .bbox_to_wkt(bbox)

  res <- tryCatch({
    resp <- request("https://api.obis.org/occurrence") %>%
      req_url_query(scientificname = name, geometry = polygon, size = 1) %>%
      req_timeout(5) %>%
      req_perform()
    if (resp_status(resp) == 200) {
       data <- fromJSON(resp_body_string(resp))
       present <- !is.null(data$total) && data$total > 0
       lat <- NA_real_; lon <- NA_real_
       if (present && !is.null(data$results) && length(data$results) > 0) {
         rec <- if (is.data.frame(data$results)) data$results[1, , drop = FALSE] else data$results[[1]]
         lat_v <- suppressWarnings(as.numeric(rec$decimalLatitude))
         lon_v <- suppressWarnings(as.numeric(rec$decimalLongitude))
         if (length(lat_v) > 0 && !is.na(lat_v[1])) lat <- lat_v[1]
         if (length(lon_v) > 0 && !is.na(lon_v[1])) lon <- lon_v[1]
       }
       list(present = present, lat = lat, lon = lon)
    } else empty
  }, error = function(e) empty)

  assign(key, res, envir = .evidence_cache_env)
  res
}

.gbif_query_once <- function(name, bbox) {
  empty <- list(present = FALSE, lat = NA_real_, lon = NA_real_)
  key <- paste(name, .bbox_cache_key(bbox), "gbif", sep="_")
  if (exists(key, envir = .evidence_cache_env)) return(get(key, envir = .evidence_cache_env))

  # FIXED (round 20, 2026-09-21): now uses the same `geometry` WKT parameter
  # OBIS already uses above, instead of a raw decimalLatitude/
  # decimalLongitude range. For a single rectangular bbox this changes
  # nothing (a range query and a rectangular WKT polygon select the exact
  # same points) -- it's required, not cosmetic, for the North Sea's
  # multi-box refinement (NORTH_SEA_REFINED_BOXES above), since two disjoint
  # rectangles excluding the Irish Sea cannot be expressed as one min/max
  # range in either dimension.
  geometry <- .bbox_to_wkt(bbox)

  res <- tryCatch({
    resp <- request("https://api.gbif.org/v1/occurrence/search") %>%
      req_url_query(scientificName = name, geometry = geometry, limit = 1) %>%
      req_timeout(5) %>%
      req_perform()
    if (resp_status(resp) == 200) {
       data <- fromJSON(resp_body_string(resp))
       present <- !is.null(data$count) && data$count > 0
       lat <- NA_real_; lon <- NA_real_
       if (present && !is.null(data$results) && length(data$results) > 0) {
         rec <- if (is.data.frame(data$results)) data$results[1, , drop = FALSE] else data$results[[1]]
         lat_v <- suppressWarnings(as.numeric(rec$decimalLatitude))
         lon_v <- suppressWarnings(as.numeric(rec$decimalLongitude))
         if (length(lat_v) > 0 && !is.na(lat_v[1])) lat <- lat_v[1]
         if (length(lon_v) > 0 && !is.na(lon_v[1])) lon <- lon_v[1]
       }
       list(present = present, lat = lat, lon = lon)
    } else empty
  }, error = function(e) empty)

  assign(key, res, envir = .evidence_cache_env)
  res
}

# FIXED (2026-09-11): both functions below now accept an optional aphia_id.
# When the primary name comes back absent, and an aphia_id is given, every
# known synonym name (fetch_worms_synonyms()) is tried too before
# concluding "not observed" -- see the WoRMS synonym cache comment above
# fetch_worms_synonyms() for why this matters (a species reclassified
# recently enough that OBIS/GBIF haven't re-indexed it yet would otherwise
# read as a false absence). Returns list(present, lat, lon, matched_name):
# matched_name is the exact name (primary or synonym) whose query actually
# returned records, or NA if none did.
check_obis_presence <- function(species_name, bbox, aphia_id = NA) {
  empty <- list(present = FALSE, lat = NA_real_, lon = NA_real_, matched_name = NA_character_)
  if (is.null(bbox)) return(empty)

  primary <- .obis_query_once(species_name, bbox)
  if (isTRUE(primary$present)) return(c(primary, matched_name = species_name))

  if (!is.na(aphia_id)) {
    syn <- fetch_worms_synonyms(aphia_id)
    for (nm in syn$names) {
      r <- .obis_query_once(nm, bbox)
      if (isTRUE(r$present)) return(c(r, matched_name = nm))
    }
  }
  c(primary, matched_name = NA_character_)
}

# Check presence in GBIF within bounding box (see check_obis_presence() above
# for the synonym-fallback rationale and matched_name semantics).
check_gbif_presence <- function(species_name, bbox, aphia_id = NA) {
  empty <- list(present = FALSE, lat = NA_real_, lon = NA_real_, matched_name = NA_character_)
  if (is.null(bbox)) return(empty)

  primary <- .gbif_query_once(species_name, bbox)
  if (isTRUE(primary$present)) return(c(primary, matched_name = species_name))

  if (!is.na(aphia_id)) {
    syn <- fetch_worms_synonyms(aphia_id)
    for (nm in syn$names) {
      r <- .gbif_query_once(nm, bbox)
      if (isTRUE(r$present)) return(c(r, matched_name = nm))
    }
  }
  c(primary, matched_name = NA_character_)
}

# ------------------------------------------------------------------------
# Batched OBIS+GBIF presence check (round 8, 2026-09-09)
#
# Added after benchmarking (benchmark_enrichment.R) showed the original
# mod_f_enrichment.R loop -- calling check_obis_presence()/check_gbif_presence()
# one species at a time, two blocking req_perform() calls per species --
# was the single largest cost of "Run ecological annotation": ~108 sec for
# 40 species (80 sequential HTTP round-trips) vs ~11 sec for the same 80
# calls fired through req_perform_parallel() (the same pattern
# worms_batch_query() already uses above), a ~10x speedup with 0 result
# mismatches against the sequential version across all 40 test species.
#
# Reuses check_obis_presence()'s / check_gbif_presence()'s own
# .evidence_cache_env cache keys: anything already cached (from a prior
# call, a prior run this session, or reloaded from evidence_cache.rds at
# startup) is served straight from cache and never re-fetched, and every
# fresh result this function fetches is written to that same cache so a
# later single-species check_obis_presence()/check_gbif_presence() call
# (or a later call to this function) reuses it too.
#
# Returns a named list keyed by species name, each element
# list(obis = list(present, lat, lon, matched_name),
#      gbif = list(present, lat, lon, matched_name)).
# matched_name is the exact name (the species' own primary name, or a
# synonym) whose OBIS/GBIF query actually returned records -- NA if none
# did, including when bbox/species_names are empty (nothing was ever
# queried, so nothing could have matched).
#
# `species_to_aphia` (2026-09-11, optional but should always be supplied by
# real callers): a named vector/list mapping species name -> AphiaID. Any
# species whose PRIMARY name comes back absent, and whose AphiaID is known,
# is retried against every synonym name WoRMS has on file for that AphiaID
# (fetch_worms_synonyms()) before being reported absent -- see the comment
# above fetch_worms_synonyms() for why (OBIS/GBIF's own taxonomic indexes
# lag behind a recent WoRMS reclassification, so a current-name-only query
# is a false negative for exactly the species that changed most recently).
# Omitting species_to_aphia (or leaving an entry out of it) falls back to
# the old, primary-name-only behavior for that species -- never an error,
# just no synonym fallback for whatever wasn't identified.
# ------------------------------------------------------------------------
EVIDENCE_CACHE_LOGIC_VERSION <- "v3"  # bumped Round 29: parse_resp()/cache entries now
                                       # carry a `checked` flag (query genuinely
                                       # succeeded vs defaulted to FALSE on error) --
                                       # bumping the version means old on-disk cache
                                       # entries (which never distinguished the two)
                                       # are simply never read as if they were, forcing
                                       # a fresh, correctly-flagged re-query instead of
                                       # silently treating a stale error as a confirmed
                                       # absence.
                                       #
                                       # Bumped AGAIN (Round 34 follow-up, 2026-09-28):
                                       # the geometry cache key (.reference_geometry_
                                       # cache_key() above) is keyed by MRGID alone
                                       # ("realpoly_2350_v1") -- it does NOT encode the
                                       # polygon's actual content. When
                                       # REFERENCE_REGION_POLYGONS[["2350"]]'s corrupted
                                       # German Bight stretch was fixed (see that list's
                                       # own comment above), every species already
                                       # queried under the OLD, buggy polygon (e.g. the
                                       # full 319-species reference list, queried before
                                       # this fix) would otherwise keep silently being
                                       # served those stale, pre-fix cache entries
                                       # forever -- the fix would be live in the code but
                                       # invisible in practice for any species already
                                       # cached. This version bump forces one fresh
                                       # re-query for every species/region on first run
                                       # after the fix, exactly like the Round 29 bump
                                       # above.
batch_check_obis_gbif_presence <- function(species_names, bbox, species_to_aphia = NULL, reference_mrgid = NA_integer_) {
  species_names <- unique(species_names)
  # `checked` = TRUE only when the query genuinely completed (got a real,
  # parseable 200 response) -- FALSE covers both "not queried at all" (this
  # `empty` default) and any failure mode below (network error, non-200
  # status, unparseable JSON). Round 29: this distinction lets callers tell
  # "checked and genuinely absent" apart from "we don't actually know",
  # which the old present-only structure collapsed into the same FALSE.
  empty <- list(present = FALSE, checked = FALSE, lat = NA_real_, lon = NA_real_, matched_name = NA_character_)
  results <- vector("list", length(species_names))
  names(results) <- species_names

  if (is.null(bbox) || length(species_names) == 0) {
    for (sp in species_names) results[[sp]] <- list(obis = empty, gbif = empty)
    return(results)
  }

  # FIXED (round 20, 2026-09-21; extended in the same round's follow-up):
  # obis_key()/gbif_key() used to flatten bbox with a bare
  # paste(bbox, collapse="_"), which assumed bbox is always a single
  # minLat/maxLat/minLon/maxLon vector. .reference_geometry_cache_key()
  # handles three shapes now: a real coastline polygon (keyed by MRGID,
  # when reference_mrgid is in REFERENCE_REGION_POLYGONS -- see that list
  # above for which regions have one), NORTH_SEA_REFINED_BOXES' two-
  # rectangle list, and the plain single-rectangle case -- a no-op change
  # for any region with neither a real polygon nor a multi-box refinement.
  geom_key <- .reference_geometry_cache_key(reference_mrgid, bbox)
  obis_key <- function(nm) paste(nm, geom_key, "obis", EVIDENCE_CACHE_LOGIC_VERSION, sep = "_")
  gbif_key <- function(nm) paste(nm, geom_key, "gbif", EVIDENCE_CACHE_LOGIC_VERSION, sep = "_")

  # One parallel wave of fresh OBIS+GBIF requests for whichever (name, db)
  # pairs in `names_for_obis`/`names_for_gbif` aren't already cached.
  # Returns nothing -- writes straight into .evidence_cache_env under each
  # name's own key, exactly like the single-name functions above, so a
  # name checked here (a primary name OR a synonym) is reusable by anyone.
  run_wave <- function(names_for_obis, names_for_gbif) {
    need_obis <- Filter(function(nm) !exists(obis_key(nm), envir = .evidence_cache_env), unique(names_for_obis))
    need_gbif <- Filter(function(nm) !exists(gbif_key(nm), envir = .evidence_cache_env), unique(names_for_gbif))
    if (length(need_obis) == 0 && length(need_gbif) == 0) return(invisible(NULL))

    # FIXED (round 20, 2026-09-21; extended in the same round's follow-up):
    # resolved via get_reference_geometry_wkt() now -- the real coastline
    # polygon from REFERENCE_REGION_POLYGONS above when reference_mrgid has
    # one (currently just the North Sea, 2350), otherwise the bbox-rectangle
    # WKT as before (POLYGON for a single bbox, MULTIPOLYGON/merged-hexagon
    # for NORTH_SEA_REFINED_BOXES' two rectangles). A no-op for every region
    # with neither a real polygon nor a multi-box refinement.
    polygon <- get_reference_geometry_wkt(reference_mrgid, bbox)

    parse_resp <- function(r, count_field) {
      if (inherits(r, "httr2_error") && !is.null(r$resp)) r <- r$resp
      if (!inherits(r, "httr2_response") || resp_status(r) != 200) return(list(present = FALSE, checked = FALSE, lat = NA_real_, lon = NA_real_))
      data <- tryCatch(fromJSON(resp_body_string(r)), error = function(e) NULL)
      if (is.null(data)) return(list(present = FALSE, checked = FALSE, lat = NA_real_, lon = NA_real_))
      present <- !is.null(data[[count_field]]) && data[[count_field]] > 0
      lat <- NA_real_; lon <- NA_real_
      if (present && !is.null(data$results) && length(data$results) > 0) {
        rec <- if (is.data.frame(data$results)) data$results[1, , drop = FALSE] else data$results[[1]]
        lat_v <- suppressWarnings(as.numeric(rec$decimalLatitude))
        lon_v <- suppressWarnings(as.numeric(rec$decimalLongitude))
        if (length(lat_v) > 0 && !is.na(lat_v[1])) lat <- lat_v[1]
        if (length(lon_v) > 0 && !is.na(lon_v[1])) lon <- lon_v[1]
      }
      # A real, successfully-parsed 200 response -- checked = TRUE regardless
      # of whether a record was actually found (present can be TRUE or FALSE).
      list(present = present, checked = TRUE, lat = lat, lon = lon)
    }

    # OBIS: unaffected by rate limiting -- confirmed via gbif_status_
    # diagnostic.R (round 34, 2nd follow-up) that all 319/319 OBIS requests
    # in a single parallel wave succeeded (HTTP 200) with zero rejections.
    # Keep firing OBIS requests all at once, exactly as before.
    if (length(need_obis) > 0) {
      obis_reqs <- lapply(need_obis, function(nm) {
        request("https://api.obis.org/occurrence") %>%
          req_url_query(scientificname = nm, geometry = polygon, size = 1) %>%
          req_timeout(5) %>% req_error(is_error = function(r) FALSE)
      })
      obis_resps <- tryCatch(req_perform_parallel(obis_reqs, on_error = "continue"), error = function(e) NULL)
      if (!is.null(obis_resps)) {
        for (i in seq_along(need_obis)) {
          assign(obis_key(need_obis[i]), parse_resp(obis_resps[[i]], "total"), envir = .evidence_cache_env)
        }
      }
    }

    # GBIF: FIXED (round 34, 2nd follow-up, 2026-09-28) -- firing all GBIF
    # requests in one parallel wave, the same way as OBIS above, used to get
    # ~95% of them rejected with HTTP 429 (Too Many Requests) -- confirmed
    # via gbif_status_diagnostic.R: 304/319 GBIF requests came back 429 in a
    # single-wave test, while the identical-pattern OBIS wave got 319/319
    # clean 200s. This silently produced `checked = FALSE, present = FALSE`
    # for the vast majority of GBIF lookups in any batch run (a 429 response
    # fails the `resp_status(r) != 200` check in parse_resp() above exactly
    # like a genuine absence), which is what gbif_noise_check.R caught: ~19%
    # of species flipping GBIF true/false between two back-to-back runs
    # against the SAME polygon, purely from this.
    #
    # GBIF has asked client authors to keep to roughly 1 request/second (see
    # the rgbif R package's issue tracker, where GBIF requested exactly this
    # of that package's maintainers). httr2's req_retry()/req_throttle()
    # request-level policies were confirmed NOT to take effect inside
    # req_perform_parallel() in this httr2 version -- verified by local
    # testing against a mock server: both were silent no-ops, with every
    # request still firing simultaneously and getting 429'd. So GBIF
    # requests are paced and retried manually here, one at a time via plain
    # req_perform() (confirmed to honor a 429 response's Retry-After header
    # correctly), instead of relying on a request-level policy that doesn't
    # actually apply for this call pattern. This makes GBIF checking much
    # slower (~1 request/second, so a few minutes for a few hundred
    # species) but should eliminate the near-random GBIF true/false flips
    # this diagnosed. req_timeout is bumped from 5s to 15s here too, as
    # cheap defense-in-depth -- though the diagnostic found zero genuine
    # timeouts, only 429s, so this alone would NOT have fixed the problem.
    for (nm in need_gbif) {
      resp <- NULL
      for (attempt in 1:3) {
        resp <- tryCatch(
          request("https://api.gbif.org/v1/occurrence/search") %>%
            req_url_query(scientificName = nm, geometry = polygon, limit = 1) %>%
            req_timeout(15) %>%
            req_error(is_error = function(r) FALSE) %>%
            req_perform(),
          error = function(e) e
        )
        if (inherits(resp, "httr2_response") && !(resp_status(resp) %in% c(429, 503))) break
        retry_after <- if (inherits(resp, "httr2_response")) {
          suppressWarnings(as.numeric(resp_header(resp, "Retry-After")))
        } else NA_real_
        wait_s <- if (!is.na(retry_after)) retry_after else attempt * 2
        Sys.sleep(wait_s)
      }
      assign(gbif_key(nm), parse_resp(resp, "count"), envir = .evidence_cache_env)
      Sys.sleep(1)  # ~1 request/second sustained pacing, per GBIF's own requested rate
    }

    invisible(NULL)
  }

  get_cached <- function(nm, key_fn) {
    k <- key_fn(nm)
    if (exists(k, envir = .evidence_cache_env)) get(k, envir = .evidence_cache_env) else list(present = FALSE, checked = FALSE, lat = NA_real_, lon = NA_real_)
  }

  # --- Wave 1: every species' own primary name ---
  run_wave(species_names, species_names)

  # --- Wave 2: synonyms, but ONLY for species still absent on their primary
  # name (and only for whichever database(s) came back absent) -- this is
  # the whole point of doing it in two waves rather than one: the common
  # case (a species found under its own current name) never pays for a
  # synonym lookup at all. ---
  syn_names_for <- list()  # species -> character vector of synonym names tried
  obis_syn_pool <- character(0)
  gbif_syn_pool <- character(0)
  for (sp in species_names) {
    aid <- if (!is.null(species_to_aphia) && sp %in% names(species_to_aphia)) species_to_aphia[[sp]] else NA
    if (is.na(aid)) next
    primary_obis <- get_cached(sp, obis_key)
    primary_gbif <- get_cached(sp, gbif_key)
    if (isTRUE(primary_obis$present) && isTRUE(primary_gbif$present)) next  # nothing to gain from synonyms
    syn <- fetch_worms_synonyms(aid)
    # Round 59: a subspecies' parent species is the last candidate name,
    # tried only because the subspecies' own name (and its synonyms) came
    # back absent here -- "borrow from the parent only when the subspecies
    # itself has no records". Non-subspecies get no extra name.
    cand_names <- unique(c(syn$names, get_parent_species_name(aid)))
    if (length(cand_names) == 0) next
    syn_names_for[[sp]] <- cand_names
    if (!isTRUE(primary_obis$present)) obis_syn_pool <- c(obis_syn_pool, cand_names)
    if (!isTRUE(primary_gbif$present)) gbif_syn_pool <- c(gbif_syn_pool, cand_names)
  }
  if (length(obis_syn_pool) > 0 || length(gbif_syn_pool) > 0) {
    run_wave(obis_syn_pool, gbif_syn_pool)
  }

  # --- Assemble final per-species results: primary name wins if present;
  # otherwise the first synonym (in WoRMS's own returned order) that is. ---
  for (sp in species_names) {
    obis_r <- get_cached(sp, obis_key)
    gbif_r <- get_cached(sp, gbif_key)
    obis_matched <- if (isTRUE(obis_r$present)) sp else NA_character_
    gbif_matched <- if (isTRUE(gbif_r$present)) sp else NA_character_
    # Round 29: track whether ANY attempted name (primary or a synonym) was
    # genuinely, successfully checked -- independent of whether it was found
    # present -- since obis_r/gbif_r below only get overwritten by a synonym
    # that WAS present, and a species absent under every name it was tried
    # under (the common, correct "checked and genuinely absent" case) would
    # otherwise never have that success recorded.
    obis_checked_any <- isTRUE(obis_r$checked)
    gbif_checked_any <- isTRUE(gbif_r$checked)

    for (nm in syn_names_for[[sp]] %||% character(0)) {
      if (is.na(obis_matched)) {
        r <- get_cached(nm, obis_key)
        if (isTRUE(r$checked)) obis_checked_any <- TRUE
        if (isTRUE(r$present)) { obis_r <- r; obis_matched <- nm }
      }
      if (is.na(gbif_matched)) {
        r <- get_cached(nm, gbif_key)
        if (isTRUE(r$checked)) gbif_checked_any <- TRUE
        if (isTRUE(r$present)) { gbif_r <- r; gbif_matched <- nm }
      }
      if (!is.na(obis_matched) && !is.na(gbif_matched)) break
    }

    results[[sp]] <- list(
      obis = list(present = isTRUE(obis_r$present), checked = obis_checked_any, lat = obis_r$lat %||% NA_real_, lon = obis_r$lon %||% NA_real_, matched_name = obis_matched),
      gbif = list(present = isTRUE(gbif_r$present), checked = gbif_checked_any, lat = gbif_r$lat %||% NA_real_, lon = gbif_r$lon %||% NA_real_, matched_name = gbif_matched)
    )
  }

  results
}

# ------------------------------------------------------------------------
# TRAIT VALIDATION (MarineSpeciesTraits Database)
# ------------------------------------------------------------------------

# Global lookup environment for trait validation
trait_lookup_env <- new.env(parent = emptyenv())

# Persistent larval inference cache — shared across sessions and projects.
# Stores results of has_planktonic_larvae checks per AphiaID so repeated
# lookups (even in different apps) are instant.  Populated on first access
# and saved after each session.
larval_cache_file <- "cache/larval_inference_cache.rds"
larval_cache_env <- new.env(parent = emptyenv())

load_larval_cache <- function() {
  if (file.exists(larval_cache_file)) {
    data <- readRDS(larval_cache_file)
    list2env(data, envir = larval_cache_env)
    message(sprintf("Larval inference cache loaded (%d entries).", length(data)))
  }
}
save_larval_cache <- function() {
  dir.create(dirname(larval_cache_file), showWarnings = FALSE)
  saveRDS(as.list(larval_cache_env), file = larval_cache_file)
  message(sprintf("Larval inference cache saved (%d entries).", length(as.list(larval_cache_env))))
}

# Where the MarineSpeciesTraits database (marineSpeciesTraits.RData, an R object
# `speciesList` with one row per species) is looked up. First existing file wins:
#   1. environment variable MARINE_SPECIES_TRAITS_RDATA (full path to the file)
#   2. R option marineSpeciesTraits.path
#   3. config/marineSpeciesTraits.RData   (inside the app folder)
#   4. data/marineSpeciesTraits.RData     (inside the app folder)
#   5. ~/marineSpeciesTraits.RData        (home folder)
# The database is NOT shipped with the app (obtain it from the source named in the
# README). If none is found the app still starts; trait validation columns
# (DB_Group1, DB_Group2, DB_TaxGroup, *_Validated) stay empty and the fallback from the
# external database is skipped. (Release preparation, Round 66: this used to be one
# fixed absolute path on the author's own computer.)
resolve_trait_db_path <- function() {
  cands <- c(Sys.getenv("MARINE_SPECIES_TRAITS_RDATA", unset = NA_character_),
             as.character(getOption("marineSpeciesTraits.path", default = NA_character_)),
             file.path("config", "marineSpeciesTraits.RData"),
             file.path("data", "marineSpeciesTraits.RData"),
             file.path(Sys.getenv("HOME"), "marineSpeciesTraits.RData"))
  cands <- cands[!is.na(cands) & nzchar(cands)]
  hit <- cands[file.exists(cands)]
  if (length(hit)) normalizePath(hit[1], winslash = "/") else NA_character_
}

message("Loading trait validation database...")
local({
  path <- resolve_trait_db_path()
  if (is.na(path)) {
    message("Trait validation database not found (looked for marineSpeciesTraits.RData; see ",
            "resolve_trait_db_path() in global.R). The app starts without it: DB_* trait columns ",
            "stay empty and the external-database fallback is skipped.")
    return(invisible(NULL))
  }
  tryCatch({
    db_env <- new.env()
    load(path, envir = db_env)
    speciesList <- db_env$speciesList
    for (i in seq_len(nrow(speciesList))) {
      aphia_id <- as.character(speciesList$AphiaID[i])
      assign(aphia_id, speciesList[i, c("feedingType", "morphologySize", "taxGroup", "habitatType", "taxClass", "taxPhylum", "taxOrder", "propaguleDuration")], envir = trait_lookup_env)
    }
    message(sprintf("Trait validation database loaded (%d species) from %s.", nrow(speciesList), path))
  }, error = function(e) message("Trait validation database could not be read from ", path, ": ", conditionMessage(e)))
})
load_larval_cache()

# Pipeline-matching TaxGroup lookup table.
# Mirrors classify_taxonomic_group()'s taxonomic_group_lookup but returns
# only the TaxGroup label so ==-based validation flags match correctly.
# Order matters: more specific (phylum+class+order) rows must precede
# less specific ones for the same phylum.
tg_lookup <- tibble(
  Phylum = character(), Class = character(), Order = character(),
  TaxGroup = character()
) |>
  # Annelida
  add_row(Phylum="Annelida", Class="Polychaeta", Order=NA, TaxGroup="polychaete (bristleworm)") |>
  add_row(Phylum="Annelida", Class="Clitellata", Order=NA, TaxGroup="oligochaete") |>
  # Arthropoda: Branchiopoda/Diplostraca
  add_row(Phylum="Arthropoda", Class="Branchiopoda", Order=NA, TaxGroup="Cladocera") |>
  # Arthropoda: Copepoda / Hexanauplia
  add_row(Phylum="Arthropoda", Class="Copepoda", Order=NA, TaxGroup="copepod") |>
  add_row(Phylum="Arthropoda", Class="Hexanauplia", Order="Calanoida", TaxGroup="copepod") |>
  add_row(Phylum="Arthropoda", Class="Hexanauplia", Order="Cyclopoida", TaxGroup="copepod") |>
  add_row(Phylum="Arthropoda", Class="Hexanauplia", Order="Harpacticoida", TaxGroup="copepod") |>
  add_row(Phylum="Arthropoda", Class="Hexanauplia", Order="Siphonostomatoida", TaxGroup="copepod") |>
  add_row(Phylum="Arthropoda", Class="Hexanauplia", Order=NA, TaxGroup="copepod") |>
  # Arthropoda: Ostracoda
  add_row(Phylum="Arthropoda", Class="Ostracoda", Order=NA, TaxGroup="ostracod (seed shrimp)") |>
  # Arthropoda: Pycnogonida
  add_row(Phylum="Arthropoda", Class="Pycnogonida", Order=NA, TaxGroup="sea spider (pycnogonid)") |>
  # Arthropoda: Malacostraca — order-specific
  add_row(Phylum="Arthropoda", Class="Malacostraca", Order="Amphipoda", TaxGroup="amphipod") |>
  add_row(Phylum="Arthropoda", Class="Malacostraca", Order="Decapoda", TaxGroup="decapod") |>
  add_row(Phylum="Arthropoda", Class="Malacostraca", Order="Isopoda", TaxGroup="isopod") |>
  add_row(Phylum="Arthropoda", Class="Malacostraca", Order="Euphausiacea", TaxGroup="krill (euphausiid)") |>
  add_row(Phylum="Arthropoda", Class="Malacostraca", Order="Mysida", TaxGroup="mysid (opossum shrimp)") |>
  add_row(Phylum="Arthropoda", Class="Malacostraca", Order="Cumacea", TaxGroup="cumacean") |>
  add_row(Phylum="Arthropoda", Class="Malacostraca", Order="Tanaidacea", TaxGroup="tanaid") |>
  add_row(Phylum="Arthropoda", Class="Malacostraca", Order="Stomatopoda", TaxGroup="stomatopod (mantis shrimp)") |>
  # Arthropoda: Thecostraca
  add_row(Phylum="Arthropoda", Class="Thecostraca", Order="Balanomorpha", TaxGroup="barnacle") |>
  add_row(Phylum="Arthropoda", Class="Thecostraca", Order="Rhizocephala", TaxGroup="barnacle (parasitic)") |>
  # Bryozoa
  add_row(Phylum="Bryozoa", Class=NA, Order=NA, TaxGroup="bryozoan") |>
  # Chaetognatha
  add_row(Phylum="Chaetognatha", Class=NA, Order=NA, TaxGroup="chaetognath") |>
  # Chordata: fish classes
  add_row(Phylum="Chordata", Class="Actinopterygii", Order=NA, TaxGroup="fish") |>
  add_row(Phylum="Chordata", Class="Teleostei", Order=NA, TaxGroup="fish") |>
  add_row(Phylum="Chordata", Class="Elasmobranchii", Order=NA, TaxGroup="fish") |>
  add_row(Phylum="Chordata", Class="Holocephali", Order=NA, TaxGroup="fish") |>
  add_row(Phylum="Chordata", Class="Myxini", Order=NA, TaxGroup="fish") |>
  add_row(Phylum="Chordata", Class="Petromyzonti", Order=NA, TaxGroup="fish") |>
  # Chordata: marine mammal, seabird, reptile handled via early return above
  # Chordata: other marine invertebrate chordates
  add_row(Phylum="Chordata", Class="Leptocardii", Order=NA, TaxGroup="lancelet (amphioxus)") |>
  add_row(Phylum="Chordata", Class="Appendicularia", Order=NA, TaxGroup="appendicularian") |>
  add_row(Phylum="Chordata", Class="Ascidiacea", Order=NA, TaxGroup="tunicate") |>
  add_row(Phylum="Chordata", Class="Thaliacea", Order=NA, TaxGroup="tunicate") |>
  # Cnidaria
  add_row(Phylum="Cnidaria", Class="Anthozoa", Order="Actiniaria", TaxGroup="sea anemone") |>
  add_row(Phylum="Cnidaria", Class="Anthozoa", Order=NA, TaxGroup="anthozoan (coral/sea anemone)") |>
  add_row(Phylum="Cnidaria", Class="Hydrozoa", Order=NA, TaxGroup="hydrozoan (in: jellyfish / gelatinous zooplankton)") |>
  add_row(Phylum="Cnidaria", Class="Scyphozoa", Order=NA, TaxGroup="true jellyfish (in: jellyfish / gelatinous zooplankton)") |>
  add_row(Phylum="Cnidaria", Class="Cubozoa", Order=NA, TaxGroup="true jellyfish (in: jellyfish / gelatinous zooplankton)") |>
  add_row(Phylum="Cnidaria", Class="Staurozoa", Order=NA, TaxGroup="true jellyfish (in: jellyfish / gelatinous zooplankton)") |>
  # Ctenophora
  add_row(Phylum="Ctenophora", Class=NA, Order=NA, TaxGroup="ctenophore / gelatinous zooplankton") |>
  # Echinodermata
  add_row(Phylum="Echinodermata", Class="Asteroidea", Order=NA, TaxGroup="starfish") |>
  add_row(Phylum="Echinodermata", Class="Echinoidea", Order=NA, TaxGroup="sea urchin") |>
  add_row(Phylum="Echinodermata", Class="Ophiuroidea", Order=NA, TaxGroup="brittlestar") |>
  add_row(Phylum="Echinodermata", Class="Holothuroidea", Order=NA, TaxGroup="sea cucumber (holothurian)") |>
  add_row(Phylum="Echinodermata", Class="Crinoidea", Order=NA, TaxGroup="crinoid (feather star)") |>
  # Mollusca
  add_row(Phylum="Mollusca", Class="Bivalvia", Order=NA, TaxGroup="bivalve") |>
  add_row(Phylum="Mollusca", Class="Cephalopoda", Order=NA, TaxGroup="cephalopod") |>
  add_row(Phylum="Mollusca", Class="Gastropoda", Order=NA, TaxGroup="gastropod (slugs & snails)") |>
  add_row(Phylum="Mollusca", Class="Polyplacophora", Order=NA, TaxGroup="chiton (polyplacophoran)") |>
  add_row(Phylum="Mollusca", Class="Scaphopoda", Order=NA, TaxGroup="tusk shell (scaphopod)") |>
  # Nemertea
  add_row(Phylum="Nemertea", Class=NA, Order=NA, TaxGroup="nemertines (ribbon worms)") |>
  # Phoronida
  add_row(Phylum="Phoronida", Class=NA, Order=NA, TaxGroup="phoronid (horseshoe worm)") |>
  # Rotifera
  add_row(Phylum="Rotifera", Class=NA, Order=NA, TaxGroup="rotifer") |>
  # Nematoda
  add_row(Phylum="Nematoda", Class="Chromadorea", Order="Rhabditida", TaxGroup="nematod (roundworm)") |>
  add_row(Phylum="Nematoda", Class="Enoplea", Order=NA, TaxGroup="nematod (roundworm)") |>
  add_row(Phylum="Nematoda", Class=NA, Order=NA, TaxGroup="nematod (roundworm)") |>
  # Porifera
  add_row(Phylum="Porifera", Class=NA, Order=NA, TaxGroup="sponge (poriferan)") |>
  # Platyhelminthes — split to match the corrected taxonomic_group_lookup
  # table above (Round 52 Platyhelminthes fix); this mirror is only reached
  # as a last-resort TaxGroup fallback, so was effectively dead for this
  # phylum before (classify_taxonomic_group() always matches Platyhelminthes
  # via its own phylum-wide row first), but keeping the labels in sync avoids
  # ever serving the old, pre-fix generic label if that ever changes.
  add_row(Phylum="Platyhelminthes", Class="Trematoda", Order=NA, TaxGroup="flatworm (trematode/fluke)") |>
  add_row(Phylum="Platyhelminthes", Class="Cestoda", Order=NA, TaxGroup="flatworm (cestode/tapeworm)") |>
  add_row(Phylum="Platyhelminthes", Class="Monogenea", Order=NA, TaxGroup="flatworm (monogenean)") |>
  add_row(Phylum="Platyhelminthes", Class=NA, Order=NA, TaxGroup="flatworm (free-living)") |>
  # Sipuncula
  add_row(Phylum="Sipuncula", Class=NA, Order=NA, TaxGroup="sipunculan (peanut worm)") |>
  # Echiura
  add_row(Phylum="Echiura", Class=NA, Order=NA, TaxGroup="echiuran (spoon worm)") |>
  # Hemichordata
  add_row(Phylum="Hemichordata", Class=NA, Order=NA, TaxGroup="hemichordate (acorn worm)")

# Match a row of DB taxonomy against tg_lookup and return the TaxGroup label.
# Uses the same matching logic as classify_taxonomic_group().
lookup_taxgroup <- function(phylum, class, order) {
  for (i in seq_len(nrow(tg_lookup))) {
    row_phy <- tolower(tg_lookup$Phylum[i])
    row_cls <- tg_lookup$Class[i]
    row_ord <- tg_lookup$Order[i]
    phy_match <- !is.na(row_phy) && tolower(phylum) == row_phy
    cls_match <- is.na(row_cls) || (!is.na(class) && tolower(row_cls) == tolower(class))
    ord_match <- is.na(row_ord) || (!is.na(order) && tolower(row_ord) == tolower(order))
    if (phy_match && cls_match && ord_match) return(tg_lookup$TaxGroup[i])
  }
  NA_character_
}

# Phylum/class-level fallback: taxonomic groups where planktonic larvae are
# near-universal, used when no per-species data is available.
phylum_has_planktonic_larvae <- function(tp, tc) {
  if (is.na(tp)) return(FALSE)
  tp <- tolower(tp); tc <- tolower(tc)
  if (tp %in% c("echinodermata", "bryozoa", "phoronida", "sipuncula", "echiura")) return(TRUE)
  if (tp == "annelida" && !is.na(tc) && tc == "polychaeta") return(TRUE)
  if (tp == "mollusca" && !is.na(tc) && tc != "cephalopoda") return(TRUE)
  if (tp == "nemertea") return(TRUE)
  if (tp == "hemichordata") return(TRUE)
  FALSE
}

# Check whether a species has planktonic larvae, consulting sources in order:
#   1. Persistent cache (shared across projects)
#   2. Local marineSpeciesTraits DB (propaguleDuration > 0 is per-species evidence)
#   3. WoRMS API attributes (online, species-specific, stored as "worms_api")
#   4. Phylum-level taxonomic inference (fallback)
# Returns TRUE/FALSE/NA.  Caches the result so subsequent lookups are instant.
check_planktonic_larvae <- function(aphia_id, tp, tc, db_row = NULL) {
  key <- as.character(aphia_id)

  # 1. Persistent cache hit
  if (exists(key, envir = larval_cache_env)) {
    return(larval_cache_env[[key]])
  }

  result <- FALSE
  source_tag <- "phylum_inference"  # default source tag

  # 2. Local DB: propaguleDuration > 0 is strong per-species evidence of a
  #    planktonic larval stage (734 of 196k species have this field).
  if (!is.null(db_row) && nrow(db_row) == 1) {
    pd <- db_row$propaguleDuration
    if (!is.na(pd) && pd > 0) {
      result <- TRUE
      source_tag <- "db_propaguleDuration"
    }
  }

  # 3. WoRMS API — online species-specific attribute check.
  #    Uses include_inherited=TRUE so that functional-group attributes defined
  #    at higher ranks (e.g. Copepoda → "plankton > zooplankton") are inherited.
  if (!result) {
    attrs <- tryCatch(wm_attr_data(aphia_id, include_inherited = TRUE), error = function(e) NULL)
    if (!is.null(attrs) && nrow(attrs) > 0) {
      # Look for attributes whose measurementType suggests larval/planktonic info
      larval_kw <- "larva|larval|planktonic|plankton|developmental.mode|develop|life.stage"
      hits <- grep(larval_kw, attrs$measurementType, ignore.case = TRUE)
      if (length(hits) > 0) {
        vals <- tolower(attrs$measurementValue[hits])
        # TRUE if any value affirms planktonic larvae
        if (any(grepl("planktonic|pelagic|free.swimming|planktotroph", vals))) {
          result <- TRUE
          source_tag <- paste0("worms_api:", attrs$measurementType[hits[1]])
        } else {
          # Present but not affirming → keep FALSE with API source tag
          source_tag <- paste0("worms_api:", attrs$measurementType[hits[1]])
        }
      }
    }
  }

  # 4. Phylum-level taxonomic inference (fallback)
  if (!result && !is.na(tp)) {
    result <- phylum_has_planktonic_larvae(tp, tc)
    source_tag <- "phylum_inference"
  }

  # Cache result as logical (persisted across sessions)
  larval_cache_env[[key]] <- result
  attr(larval_cache_env[[key]], "source") <- source_tag

  result
}

# ------------------------------------------------------------------------
# WORMS FUNCTIONAL GROUP CACHE — species-level fallback when the local DB
# has no trait data (feedingType / habitatType both NA).  Uses WoRMS
# "Functional group" attributes with include_inherited=TRUE so that
# plankton/benthos designations from higher taxonomy are inherited.
# ------------------------------------------------------------------------
worms_fg_cache_file <- "cache/worms_fg_cache.rds"
worms_fg_cache_env <- new.env(parent = emptyenv())

load_worms_fg_cache <- function() {
  if (file.exists(worms_fg_cache_file)) {
    data <- readRDS(worms_fg_cache_file)
    list2env(data, envir = worms_fg_cache_env)
    message(sprintf("WoRMS FG cache loaded (%d entries).", length(data)))
  }
}
save_worms_fg_cache <- function() {
  dir.create(dirname(worms_fg_cache_file), showWarnings = FALSE)
  saveRDS(as.list(worms_fg_cache_env), file = worms_fg_cache_file)
}

# Query WoRMS API "Functional group" attributes to infer Group1/Group2 when
# the local DB has no trait data.  Returns list(Group1, Group2) or NULL.
infer_from_worms_attributes <- function(aphia_id) {
  key <- as.character(aphia_id)

  # 1. Cache hit
  if (exists(key, envir = worms_fg_cache_env)) {
    return(worms_fg_cache_env[[key]])
  }

  result <- NULL

  # 2. Query WoRMS API with inheritance
  attrs <- tryCatch(wm_attr_data(aphia_id, include_inherited = TRUE), error = function(e) NULL)
  if (!is.null(attrs) && nrow(attrs) > 0) {
    # Extract "Functional group" attributes
    fg_idx <- grep("^Functional group$", attrs$measurementType, ignore.case = TRUE)
    if (length(fg_idx) > 0) {
      fg_vals <- unique(tolower(attrs$measurementValue[fg_idx]))
      has_plankton <- any(grepl("plankton", fg_vals))
      has_benthos  <- any(grepl("benthos", fg_vals))

      # FIXED (2026-09-11, re-applied): this had regressed to checking
      # has_plankton before has_benthos with no combined case -- the exact
      # bug already found and fixed once (see
      # worms_pipeline_app_v8_diagnosis.md, Round 2 §4) but no longer present
      # in this file. A species carrying BOTH WoRMS functional-group
      # attributes (e.g. a benthic adult with a planktonic larval stage --
      # common for meroplanktonic invertebrates, and confirmed live for
      # Maea johnstoni/AphiaID 1872991 (now a superseded combination of
      # Magelona johnstoni/130269 in WoRMS, 2026-10-05), which WoRMS lists as both "benthos"
      # and "plankton > zooplankton") silently lost the benthos signal and
      # was returned as pure holoplankton, mislabeling a benthic Magelonidae
      # polychaete as fully planktonic. Mirrors classify_by_trait_cache()'s
      # already-correct ordering (has_benthos && has_plankton -> meroplankton,
      # checked before the plankton-only case) so the two functions agree.
      if (has_benthos && has_plankton) {
        result <- list(Group1 = "meroplankton", Group2 = "benthic invertebrate")
      } else if (has_plankton) {
        result <- list(Group1 = "holoplankton", Group2 = "holoplankton")
      } else if (has_benthos) {
        result <- list(Group1 = "benthic invertebrate", Group2 = "benthic invertebrate")
      }
    }
  }

  # 3. Cache result (NULL or list)
  worms_fg_cache_env[[key]] <- result
  result
}

load_worms_fg_cache()

# Classify Group1/Group2 directly from the WoRMS "Functional group" attribute
# text already fetched for every row during the main WoRMS query
# (Functional_Group_Parsed) -- no extra per-species API call, unlike
# infer_from_worms_attributes() above, and not gated behind the "Use WoRMS
# trait cache" checkbox. Vectorized over fg_text. Mirrors the exact
# has_plankton && has_benthos -> meroplankton/benthic invertebrate logic
# already used by infer_from_worms_attributes() and map_db_to_internal(),
# just applied to data the pipeline already has in hand instead of requiring
# a fresh, opt-in query.
# (Round 34 follow-up, 2026-09-29, per direct user request: rows whose
# Functional_Group_Parsed shows both a "benthos" and a "plankton"/
# "zooplankton" signal -- e.g. "benthos; plankton > zooplankton; benthos >
# macrobenthos" -- were not being classified as meroplankton/benthic
# invertebrate unless the trait-cache checkbox happened to be on.)
classify_from_fg_text <- function(fg_text) {
  ft <- tolower(fg_text)
  has_plankton <- !is.na(ft) & grepl("plankton", ft)
  has_benthos  <- !is.na(ft) & grepl("benthos", ft)
  g1 <- ifelse(has_benthos & has_plankton, "meroplankton",
         ifelse(has_plankton, "holoplankton",
          ifelse(has_benthos, "benthic invertebrate", NA_character_)))
  g2 <- ifelse(has_benthos & has_plankton, "benthic invertebrate",
         ifelse(has_plankton, "holoplankton",
          ifelse(has_benthos, "benthic invertebrate", NA_character_)))
  list(Group1 = g1, Group2 = g2)
}

# Group1's "meroplankton" and "benthic invertebrate" are not a genuine
# disagreement when compared against each other -- a meroplanktonic
# species' adult IS a benthic invertebrate by definition (Group2 is always
# "benthic invertebrate" whenever Group1="meroplankton", throughout this
# file's classification functions), so "meroplankton" is a strict
# refinement of "benthic invertebrate" ("...and has a planktonic larval
# stage"), not a contradictory answer to the same question the way e.g.
# "holoplankton" vs "benthic invertebrate" genuinely is (an organism cannot
# be both lifelong-pelagic and a benthic adult). One source reporting the
# coarser "benthic invertebrate" while the other reports the more specific
# "meroplankton" reflects a difference in how much each source knows about
# the larval stage, not a real disagreement. Vectorized; used only for the
# Group1_Validated comparison, never for the value that ships in Group1
# itself. (Round 34 follow-up, 2026-09-29, per direct user request.)
group1_values_compatible <- function(a, b) {
  (a == b) | (a %in% c("meroplankton", "benthic invertebrate") &
              b %in% c("meroplankton", "benthic invertebrate"))
}

# Final taxonomic fallback: holoplanktonic groups where all members are fully
# planktonic (no benthic adult stage).  Mirrors the pipeline's Group1="holoplankton"
# assignments for groups not captured by feedingType/habitatType in the local DB.
taxon_is_holoplankton <- function(tp, tc, to) {
  if (is.na(tp)) return(FALSE)
  tp <- tolower(tp)
  tc <- if (is.na(tc)) NA_character_ else tolower(tc)
  to_val <- if (is.na(to)) NA_character_ else tolower(to)

  # Chaetognatha — all holoplanktonic
  if (tp == "chaetognatha") return(TRUE)
  # Ctenophora — all holoplanktonic
  if (tp == "ctenophora") return(TRUE)
  # Rotifera — all holoplanktonic
  if (tp == "rotifera") return(TRUE)
  # Arthropoda: Branchiopoda = Cladocera (water fleas) — mostly holoplanktonic
  if (tp == "arthropoda" && !is.na(tc) && tc == "branchiopoda") return(TRUE)
  # Arthropoda: Copepoda / Hexanauplia — FIXED: this used to return TRUE for the
  # entire class regardless of Order, contradicting this same file's own
  # taxonomic_group_lookup table (used only by classify_taxonomic_group(), which
  # correctly knows Harpacticoida is meiobenthic and Siphonostomatoida is
  # parasitic, not holoplanktonic). Since this function is the fallback that
  # actually determines Group1 for most copepod ASVs (see mod_e_enrichment.R),
  # the class-wide rule was mislabeling benthic/parasitic copepod orders —
  # Harpacticoida especially, which is common in benthic COI eDNA samples — as
  # holoplankton. Now order-aware, mirroring taxonomic_group_lookup: known
  # free-living planktonic orders (and the class-level default for orders not
  # otherwise identified) are holoplanktonic; known benthic/parasitic orders are
  # explicitly excluded.
  if (tp == "arthropoda" && !is.na(tc) && tc %in% c("copepoda", "hexanauplia") &&
      !(!is.na(to_val) && to_val %in% c("harpacticoida", "siphonostomatoida", "canuelloida"))) return(TRUE)
  # Arthropoda: Malacostraca, order Euphausiacea — krill, all holoplanktonic
  if (tp == "arthropoda" && !is.na(tc) && tc == "malacostraca" && !is.na(to_val) && to_val == "euphausiacea") return(TRUE)
  # Chordata: Appendicularia — all holoplanktonic
  if (tp == "chordata" && !is.na(tc) && tc == "appendicularia") return(TRUE)
  # Chordata: Thaliacea — all holoplanktonic
  if (tp == "chordata" && !is.na(tc) && tc == "thaliacea") return(TRUE)

  FALSE
}

# Map database traits to internal schema
# Uses pipeline-compatible vocabulary so that ==-based validation
# correctly corroborates WoRMS-derived Group1/Group2/TaxGroup values.
map_db_to_internal <- function(trait_row, aphia_id = NA) {
  res <- list(Group1=NA_character_, Group2=NA_character_, TaxGroup=NA_character_)
  
  if (is.null(trait_row) || nrow(trait_row) == 0) return(res)
  
  tc <- tolower(trait_row$taxClass)
  tp <- tolower(trait_row$taxPhylum)
  to <- tolower(trait_row$taxOrder)
  ft <- tolower(trait_row$feedingType)
  ht <- tolower(trait_row$habitatType)
  
  # Class-based groupings matching pipeline taxonomic_group_lookup
  # These groups have no planktonic larval stage
  if (!is.na(tc)) {
    if (tc == "aves") {
      res$Group2 <- "seabird"; res$TaxGroup <- "seabird"
      return(res)
    }
    if (tc == "mammalia") {
      res$Group2 <- "marine mammal"; res$TaxGroup <- "marine mammal"
      return(res)
    }
    if (tc == "reptilia") {
      res$Group2 <- "marine reptile"; res$TaxGroup <- "marine reptile"
      return(res)
    }
  }
  
  # Determine planktonic and benthic signals from feedingType + habitatType
  has_plankton <- !is.na(ft) && grepl("plankton", ft)
  has_benth <- (!is.na(ft) && grepl("benth", ft)) ||
               (!is.na(ht) && grepl("benth|endofauna", ht))
  has_pelagic <- !is.na(ht) && grepl("pelagic", ht)
  
  # Map Group1/Group2 using the same vocabulary as classify_taxonomic_group():
  # - plankton + benthos  -> meroplankton (benthic adult, planktonic larvae)
  # - plankton only       -> holoplankton
  # - benthos only        -> benthic invertebrate
  if (has_plankton && has_benth) {
    res$Group1 <- "meroplankton"
    res$Group2 <- "benthic invertebrate"
  } else if (has_plankton || has_pelagic) {
    res$Group1 <- "holoplankton"
    res$Group2 <- "holoplankton"
  } else if (has_benth) {
    # DB only records adult benthic traits.
    # Check multiple sources for larval type (cache → local DB → WoRMS API
    # → phylum inference).  Pass aphia_id + the full row so that
    # propaguleDuration and WoRMS attributes can be consulted.
    has_larvae <- !is.na(aphia_id) &&
                  check_planktonic_larvae(aphia_id, tp, tc, trait_row)
    if (has_larvae) {
      res$Group1 <- "meroplankton"
      res$Group2 <- "benthic invertebrate"
    } else {
      res$Group1 <- "benthic invertebrate"
      res$Group2 <- "benthic invertebrate"
    }
  }
  
  # No local DB traits (all NA) — fall back to WoRMS API "Functional group"
  # attributes (with include_inherited=TRUE so Copepoda→"plankton > zooplankton"
  # is inherited).  Cached in worms_fg_cache_env for cross-project reuse.
  if (is.na(res$Group1) && !is.na(aphia_id)) {
    worms_res <- infer_from_worms_attributes(aphia_id)
    if (!is.null(worms_res)) {
      res$Group1 <- worms_res$Group1
      res$Group2 <- worms_res$Group2
    }
  }
  
  # Final taxonomic fallback: if no data from DB or WoRMS API, check whether
  # the taxonomic group is known to be holoplanktonic (e.g. Copepoda,
  # Chaetognatha, Appendicularia, Thaliacea, Cladocera).
  if (is.na(res$Group1) && !is.na(tp)) {
    if (taxon_is_holoplankton(tp, tc, to)) {
      res$Group1 <- "holoplankton"
      res$Group2 <- "holoplankton"
    }
  }
  
  # Map TaxGroup using fine-grained lookup (phylum+class+order) first,
  # falling back to the coarse taxGroup integer for unmatched taxa.
  if (is.na(res$TaxGroup)) {
    if (!is.na(tp) && !is.na(tc)) {
      tg <- lookup_taxgroup(tp, tc, to)
      if (!is.na(tg)) {
        res$TaxGroup <- tg
      }
    }
  }
  if (is.na(res$TaxGroup)) {
    tg_map <- c(
        "1" = "algae/plant",
        "2" = "crustacean",
        "3" = "mollusc/echinoderm",
        "4" = "cnidarian/sponge",
        "5" = "annelid/bryozoan"
    )
    tg_int <- as.character(trait_row$taxGroup)
    if (!is.na(tg_int) && tg_int %in% names(tg_map)) {
        res$TaxGroup <- tg_map[[tg_int]]
    }
  }
  
  res
}

# Fallback when species is not in the local trait DB: fetch taxonomy from
# WoRMS API and apply taxonomic-group inference (holoplankton, etc.).
fetch_validation_fallback <- function(aphia_id, parent_id = NA) {
  aid <- if (!is.na(parent_id)) parent_id else aphia_id
  key <- as.character(aid)

  # Check WoRMS FG cache first (may have been populated by a prior call)
  worms_res <- infer_from_worms_attributes(aid)
  if (!is.null(worms_res)) {
    return(list(Group1 = worms_res$Group1, Group2 = worms_res$Group2, TaxGroup = NA_character_))
  }

  # Fetch WoRMS record for taxonomy (uses cached_wm_record to avoid duplicate API calls)
  rec <- cached_wm_record(aid)
  if (!is.null(rec)) {
    tp <- if (is.null(rec$phylum)) NA_character_ else rec$phylum
    tc <- if (is.null(rec$class)) NA_character_ else rec$class
    to <- if (is.null(rec$order)) NA_character_ else rec$order
    if (!is.na(tp) && taxon_is_holoplankton(tp, tc, to)) {
      return(list(Group1 = "holoplankton", Group2 = "holoplankton", TaxGroup = NA_character_))
    }
  }
  list(Group1 = NA_character_, Group2 = NA_character_, TaxGroup = NA_character_)
}

# Fetch traits from local validation DB, with parent-species fallback for
# subspecies (e.g. Paracalanus parvus parvus → Paracalanus parvus).
fetch_validation_traits <- function(aphia_id) {
  key <- as.character(aphia_id)
  if (exists(key, envir = trait_lookup_env)) {
      row <- trait_lookup_env[[key]]
      return(map_db_to_internal(row, aphia_id = aphia_id))
  }

  # Not found under this exact AphiaID — try every synonym AphiaID WoRMS
  # has on file before falling through further. FIXED (2026-09-11): the
  # bundled MarineSpeciesTraits DB (trait_lookup_env) is a separate,
  # frozen-in-time dataset just like OBIS/GBIF (see the comment above
  # fetch_worms_synonyms()) — a species reclassified in WoRMS after this
  # local DB was compiled has its real trait row filed under the OLD
  # AphiaID, so an exact-current-AphiaID-only lookup silently misses real,
  # specific trait data and falls through to a much coarser signal (a live
  # WoRMS attribute check, or nothing at all) even though better data is
  # sitting right there in the bundled DB under a name WoRMS has moved on
  # from.
  syn <- fetch_worms_synonyms(aphia_id)
  for (sid in syn$aphia_ids) {
    skey <- as.character(sid)
    if (exists(skey, envir = trait_lookup_env)) {
      row <- trait_lookup_env[[skey]]
      return(map_db_to_internal(row, aphia_id = sid))
    }
  }

  # Still not found — if this is a subspecies, try the parent species (and
  # the parent's own synonyms too, for the same reason).
  parent_id <- get_parent_aphia_id(aphia_id)
  if (!is.na(parent_id) && parent_id != aphia_id) {
    pkey <- as.character(parent_id)
    if (exists(pkey, envir = trait_lookup_env)) {
      row <- trait_lookup_env[[pkey]]
      return(map_db_to_internal(row, aphia_id = parent_id))
    }
    parent_syn <- fetch_worms_synonyms(parent_id)
    for (sid in parent_syn$aphia_ids) {
      skey <- as.character(sid)
      if (exists(skey, envir = trait_lookup_env)) {
        row <- trait_lookup_env[[skey]]
        return(map_db_to_internal(row, aphia_id = sid))
      }
    }
    # Parent (and its synonyms) not in local DB either — try taxonomic fallback via WoRMS API
    return(fetch_validation_fallback(aphia_id, parent_id))
  }
  # Not in DB under any known name/id — try taxonomic fallback via WoRMS API record
  fetch_validation_fallback(aphia_id)
}

# ========================================================================
# Shared taxon-aggregation engine (round 4, 2026-09-09)
# Used by Module G (mod_g_aggregation.R), which now runs right after
# Decontamination and before Enrichment — collapsing ASVs down to unique
# taxa so Module E enriches each one exactly once instead of once per ASV.
#
# Grouping key: each ASV's own resolved consensus identity, i.e.
# `Species_fallback` — the AphiaID Module B settled on for that ASV, at
# WHATEVER rank that ASV's own cross-database consensus actually reached
# (`Fallback_rank`). This is NOT "collapse everything to species" — most
# ASVs never even reach species-level consensus. Two ASVs are merged only if
# they share that same AphiaID, regardless of which rank it sits at; an ASV
# resolved to species stays distinct from another ASV of the same genus that
# only resolved to genus level, and an ASV that only resolved to Family
# merges only with other ASVs that resolved to that same Family AphiaID.
# Rows with no usable consensus AphiaID (NA/blank/0) cannot be safely merged
# with anything else — doing so would lump together organisms that simply
# failed to resolve, which could be anything — so each such row keeps its
# own singleton group.
#
# Reuses the same collation approach the original (pre-round-4) Module G
# used for majority-voting metadata across merged rows (numeric-mode for
# body size, union-of-locations for WoRMS_Distribution text, plain majority
# vote otherwise) — most of that metadata doesn't exist yet at this
# pre-enrichment stage, but the same function handles whatever IS present
# (Species_fallback_source, Fallback_rank, Sequence_length_bp, and any of
# the taxonomy rank columns) without needing a special case. ASV_id is the
# one input column deliberately NOT run through this majority-vote
# collation — see the comment above collate_cols below for why, and for
# the Taxon_id / Merged_ASV_ids columns this function adds instead.
# ========================================================================

# ========================================================================
# find_sequence_col() (2026-09-11, revised)
# Locates the DNA sequence column in a raw MZG data frame: a named column
# (sequence/seq/nucleotides/.../otu_sequence, same patterns Module C's
# own sequence-length step already uses), or -- for a file with no header
# on its leading column, which read_mzg_auto() renames to "EMPTY_COL_N" --
# that column, trusting the same convention this codebase already relies
# on elsewhere (see the "IMPORTANT: derive ASV_id... EMPTY_COL_1" comment
# in Module C): a raw MZG file's unnamed leading column is its sequence.
#
# The content check is a loose sanity check, not a strict gate -- real
# COI/barcode sequences commonly include IUPAC ambiguity codes beyond
# plain ACGTN (R/Y/S/W/K/M/B/D/H/V for ambiguous base calls) and can carry
# stray whitespace or line-ending characters after parsing. An earlier
# version required EVERY sampled value to match a bare [ACGTN]+ pattern,
# which failed real files for exactly these reasons. Now: values are
# trimmed first, the allowed character class covers full IUPAC codes plus
# gap characters, and a supermajority (not all) of sampled values passing
# is enough -- a handful of odd rows shouldn't block matching altogether.
# Returns NULL only if no EMPTY_COL_ column looks like sequence data at
# all.
# ========================================================================
find_sequence_col <- function(df) {
  named <- grep("^(sequence|seq|nucleotides?|nuc|barcode|rep_seq|asv_sequence|otu_sequence)$",
                names(df), ignore.case = TRUE, value = TRUE)
  if (length(named) > 0) return(named[1])

  looks_like_dna <- function(vals) {
    vals <- trimws(vals)
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0) return(FALSE)
    sample_vals <- vals[seq_len(min(50, length(vals)))]
    hits <- grepl("^[ACGTUNRYSWKMBDHVacgtunryswkmbdhv.-]+$", sample_vals)
    mean(hits) >= 0.9  # supermajority, not unanimous
  }

  empty_cols <- grep("^EMPTY_COL_", names(df), value = TRUE)
  # Try EMPTY_COL_1 first (the established leading-column convention),
  # then any other EMPTY_COL_ as a fallback.
  empty_cols <- empty_cols[order(match(empty_cols, "EMPTY_COL_1", nomatch = length(empty_cols) + 1))]
  for (col in empty_cols) {
    if (looks_like_dna(df[[col]])) return(col)
  }
  NULL
}

# ========================================================================
# match_samples_to_metadata() (2026-09-11)
#
# Single source of truth for reconciling a set of ASV-table sample-like
# column names against a sample metadata file's sample-id/sample-type
# columns. Module 4 (the actual decontamination gate) and, for early
# feedback, Module 3's backbone consolidation both call this same
# function -- so a column that would be flagged/dropped at one stage is
# never treated differently at the other, on any project's data.
#
# metadata_df must have a column matching "sample...id" and one matching
# "sample...type" (case-insensitive), the convention used throughout this
# app. sample_type values in blank_values (blank/negative/control/ntc/
# blanc, case-insensitive) are treated as blanks; everything else listed
# is a true sample. Matching against sample_cols is whitespace-insensitive.
#
# Returns list(error=, blanks_matched=, true_samples_matched=,
# unmatched_cols=). `error` is a message string when metadata_df is
# missing the required columns, NULL otherwise.
# ========================================================================
match_samples_to_metadata <- function(sample_cols, metadata_df) {
  sample_id_col <- names(metadata_df)[grepl("sample.*id", names(metadata_df), ignore.case = TRUE)][1]
  sample_type_col <- names(metadata_df)[grepl("sample.*type", names(metadata_df), ignore.case = TRUE)][1]

  if (is.na(sample_id_col) || is.na(sample_type_col)) {
    return(list(
      error = "Metadata file must contain columns matching 'sample...id' and 'sample...type'.",
      blanks_matched = NULL, true_samples_matched = NULL, unmatched_cols = NULL
    ))
  }

  blank_values <- c("blank", "negative", "control", "ntc", "blanc")
  ids   <- trimws(as.character(metadata_df[[sample_id_col]]))
  types <- trimws(as.character(metadata_df[[sample_type_col]]))
  is_blank <- tolower(types) %in% blank_values
  blanks <- ids[is_blank]
  true_samples <- ids[!is_blank]

  blanks_matched <- sample_cols[trimws(sample_cols) %in% trimws(blanks)]
  true_samples_matched <- sample_cols[trimws(sample_cols) %in% trimws(true_samples)]
  unmatched_cols <- setdiff(sample_cols, c(blanks_matched, true_samples_matched))

  list(error = NULL, blanks_matched = blanks_matched,
       true_samples_matched = true_samples_matched, unmatched_cols = unmatched_cols)
}

# ========================================================================
# consolidate_mzg_backbones() (2026-09-11)
#
# Module C (Merge) supports overriding the MZG backbone file it takes its
# sample/read-count columns from — but a plain override silently discards
# whichever backbone loses, with no check that the two files even describe
# the same ASVs.
#
# A second MZG export commonly has no id-like column at all (no "ASV_id",
# "ASV", "OTU", etc.) -- in that case assign_mzg_asv_id() falls back to
# naming rows by raw position (ASV_1, ASV_2, ... in row order). Comparing
# THAT against the original file's real ASV_id values is unsafe: if the
# original file's ids also happen to already be sequential (common —
# that's often exactly what a real id column contains), a same-row-count
# coincidence would make two files with silently reordered, added, or
# dropped rows look like an exact match when they aren't. Row position is
# never a safe join key between two independently-exported files — this is
# the same class of bug this app's ASV_id assignment logic was fixed for
# once already (see assign_mzg_asv_id()'s own header comment).
#
# Two-stage check, per the user's own real-data constraint that identical
# sequences can legitimately repeat (so a pure sequence-content lookup is
# ambiguous the moment a sequence isn't unique):
#
#  Stage 1 -- ASV match: row COUNT must agree, and the override's rows are
#  taken in order to correspond to the original's rows in order (the same
#  row-position convention assign_mzg_asv_id()'s own fallback already
#  uses for a file with no id column). This is what actually assigns the
#  override rows their real ASV_id.
#
#  Stage 2 -- sequence match: a per-position safety check on top of that
#  correspondence -- the sequence at row i must agree between the two
#  files for every i. Duplicate sequences are fine here (no uniqueness
#  needed, since position -- not content -- is doing the matching); what
#  this catches is a silently reordered, inserted, or dropped row, which
#  would show up as a position where the two files disagree.
#
# `original_df` must already have a real ASV_id (from assign_mzg_asv_id()
# on a file that DOES have a usable id column -- see caller); `override_raw`
# is the second file's raw, pre-ID data. Refuses outright on a row-count
# mismatch (stage 1) or ANY position where the sequences disagree (stage 2)
# rather than guessing; only once both stages pass does it hand back the
# override's data carrying the original's real ASV_id at each row.
#
# Returns list(error=, n_matched=, df=): on failure `error` is a message
# string and `n_matched`/`df` are NULL; on success `error` is NULL. This
# function does NOT call showNotification itself -- it's used both from
# the "Assemble" run handler (which shows a toast) and from a live preview
# that fires as soon as the second file is uploaded (which renders inline
# instead), so the decision of how to surface the result is the caller's.
# ========================================================================
consolidate_mzg_backbones <- function(original_df, override_raw, metadata_df = NULL) {
  # --- Stage 1: ASV match (row count / position correspondence) ---
  if (nrow(original_df) != nrow(override_raw)) {
    return(list(error = sprintf(paste0(
      "The original Module 1 MZG backbone (%d rows) and the second MZG backbone (%d rows) don't have the ",
      "same number of ASVs, so their rows can't be matched one-to-one. Consolidation requires both files ",
      "to contain the same ASVs in the same row order."
    ), nrow(original_df), nrow(override_raw)), n_matched = NULL, df = NULL))
  }

  # --- Stage 2: sequence match (per-position verification) ---
  orig_seq_col <- find_sequence_col(original_df)
  over_seq_col <- find_sequence_col(override_raw)

  if (is.null(orig_seq_col) || is.null(over_seq_col)) {
    return(list(
      error = "Could not find a DNA sequence column in one or both MZG backbone files -- cannot verify they describe the same ASVs without one.",
      n_matched = NULL, df = NULL
    ))
  }

  orig_seq <- toupper(trimws(as.character(original_df[[orig_seq_col]])))
  over_seq <- toupper(trimws(as.character(override_raw[[over_seq_col]])))

  # NA-safe position-by-position comparison: a mismatch is either "one
  # side is missing and the other isn't" or "both present but different".
  mismatch <- (is.na(orig_seq) != is.na(over_seq)) |
    (!is.na(orig_seq) & !is.na(over_seq) & orig_seq != over_seq)
  bad_rows <- which(mismatch)

  if (length(bad_rows) > 0) {
    return(list(error = sprintf(paste0(
      "The original Module 1 MZG backbone and the second MZG backbone agree on row count but disagree on ",
      "the sequence at %d row position(s) -- the two files are not row-aligned. Consolidation requires both ",
      "files to contain the same ASVs in the same row order. Example mismatched row(s): %s"
    ), length(bad_rows), paste(head(bad_rows, 5), collapse = ", ")), n_matched = NULL, df = NULL))
  }

  # Both stages passed. The override file contributes read counts only --
  # never taxonomy. Its own Kingdom...Species columns (if present) aren't
  # standardized/verified the way the original Module 1 backbone's are, so
  # they're dropped here explicitly rather than relying on a later,
  # unrelated step to clean them up. Its sequence/unnamed column is also
  # dropped -- it was only needed for the Stage 2 check above, and the
  # original file's own (verified) sequence is what's carried forward
  # elsewhere in the pipeline. Everything else -- every genuine sample /
  # read-count column, blanks and negative controls included -- passes
  # through untouched; this function never reconciles against a metadata
  # file (Module 3 doesn't have one yet at this point in the pipeline),
  # so blanks stay in until Module 4 sorts samples from blanks from
  # orphans using the actual metadata file.
  taxonomy_cols_pattern <- c("^kingdom$", "^phylum$", "^class$", "^order$", "^family$", "^genus$", "^species$")
  override_taxonomy_cols <- names(override_raw)[
    grepl(paste(taxonomy_cols_pattern, collapse = "|"), names(override_raw), ignore.case = TRUE)
  ]
  override_empty_cols <- grep("^EMPTY_COL_", names(override_raw), value = TRUE)
  override_cols_to_drop <- unique(c(override_taxonomy_cols, override_empty_cols))
  override_sample_cols <- setdiff(names(override_raw), override_cols_to_drop)

  # Optional early cross-check against the sample metadata file (the same
  # file, and the same match_samples_to_metadata() logic, Module 4 uses).
  # metadata_df is NULL unless the caller has one available -- Module 3
  # doesn't require a metadata file (it's only uploaded at Module 4 in the
  # normal flow), so this stays fully backward compatible when absent:
  # every genuine sample/blank column from the override file passes
  # through untouched, and Module 4 does the reconciliation later as
  # usual. When metadata_df IS supplied, any override column matching
  # neither a blank nor a true sample is dropped here too -- generically,
  # by whatever names that project's metadata and override file happen to
  # use -- and match_info reports the breakdown for the caller to surface.
  match_info <- NULL
  if (!is.null(metadata_df)) {
    match_info <- match_samples_to_metadata(override_sample_cols, metadata_df)
    if (!is.null(match_info$error)) {
      return(list(error = match_info$error, n_matched = NULL, df = NULL, match_info = NULL))
    }
    if (length(match_info$unmatched_cols) > 0) {
      override_sample_cols <- setdiff(override_sample_cols, match_info$unmatched_cols)
    }
  }

  # Attach the ORIGINAL's real ASV_id to the override rows by the
  # now-verified row position, so override's read counts carry a
  # meaningful, checked identifier instead of an arbitrary row-order
  # label with no verification behind it.
  override_result <- override_raw %>% select(all_of(override_sample_cols))
  override_result$ASV_id <- as.character(original_df$ASV_id)
  override_result <- override_result %>% relocate(ASV_id, .before = 1)

  list(error = NULL, n_matched = nrow(original_df), df = override_result, match_info = match_info)
}

run_taxon_aggregation <- function(raw,
                                   exclude_kingdoms = character(0),
                                   exclude_classes = character(0)) {
  exc_kin <- exclude_kingdoms[nzchar(exclude_kingdoms %||% "")]
  exc_cla <- exclude_classes[nzchar(exclude_classes %||% "")]

  for (i in seq_along(raw)) {
    df <- raw[[i]]
    if (length(exc_kin) > 0) {
      kin_cols <- intersect(c("Kingdom", "Consensus_Kingdom"), names(df))
      if (length(kin_cols) > 0) {
        kin_match <- rowSums(sapply(kin_cols, function(col) tolower(df[[col]]) %in% tolower(exc_kin))) > 0
        df <- df[!kin_match, , drop = FALSE]
      }
    }
    if (length(exc_cla) > 0) {
      cla_cols <- intersect(c("Class", "Consensus_Class"), names(df))
      if (length(cla_cols) > 0) {
        cla_match <- rowSums(sapply(cla_cols, function(col) tolower(df[[col]]) %in% tolower(exc_cla))) > 0
        df <- df[!cla_match, , drop = FALSE]
      }
    }
    raw[[i]] <- df
  }

  all_data <- bind_rows(raw)
  first_names <- names(all_data)

  if (!"Species_fallback" %in% first_names) {
    showNotification(
      "Could not find Species_fallback (Module B's consensus AphiaID column) — cannot aggregate by each ASV's own resolved taxon.",
      type = "error"
    )
    return(NULL)
  }

  tax_levels <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  consensus_tax <- intersect(paste0("Consensus_", tax_levels), first_names)
  bare_tax <- intersect(tax_levels, first_names)

  if (length(consensus_tax) >= 6) {
    tax_map <- setNames(consensus_tax, tax_levels[seq_along(consensus_tax)])
  } else if (length(bare_tax) >= 6) {
    tax_map <- setNames(bare_tax, tax_levels[seq_along(bare_tax)])
  } else {
    showNotification("Could not find sufficient taxonomy columns", type = "error")
    return(NULL)
  }
  tax_present <- unname(tax_map)

  # FIXED (2026-09-11): build_species_fallback() (this file) emits a
  # "Consensus_<Level>_AphiaID" numeric column alongside each
  # "Consensus_<Level>" name column (e.g. Consensus_Kingdom_AphiaID next to
  # Consensus_Kingdom) -- but only the bare name columns were ever added to
  # tax_present above, and "Consensus_<Level>_AphiaID" was never listed in
  # meta_cols below either. Since both exclusions are exact-name matches
  # (intersect()), every one of these _AphiaID columns silently fell through
  # into `samp` (the sample/abundance-column bucket) and got counted and
  # summed as if it were a real sample -- inflating the reported sample
  # count by exactly one per resolved taxonomic rank (7 ranks -> +7 phantom
  # "samples") and corrupting per-sample read totals with AphiaID numbers.
  # This is derived generically from whatever tax_present actually is
  # (consensus-style or bare-style, whichever columns are actually present)
  # rather than hardcoding "Consensus_Kingdom_AphiaID" etc. by name, so it
  # keeps working regardless of which taxonomy columns a given project's
  # upstream modules produced.
  tax_aphia_present <- intersect(paste0(tax_present, "_AphiaID"), first_names)

  # NOTE: exact-name matching (intersect), unlike the grep-based sample-
  # column detectors in mod_e/mod_f — any metadata column added upstream
  # must be listed here by its exact name, or it silently falls into the
  # "sample column" bucket and gets summed as if it were an abundance/read
  # count. Most of these don't exist yet at this pre-enrichment stage;
  # intersect() below degrades gracefully to "nothing to collate" for
  # whichever are absent.
  meta_cols <- c(
    "ASV_id", "ASV", "Sequence", "Sequence_length_bp",
    "AphiaID", "Species_fallback", "Species_fallback_source",
    "Fallback_rank", "WoRMS_Rank", "WoRMS_Status", "IsMarine",
    "WoRMS_Distribution", "Body_Size_Min", "Body_Size_Mean",
    "Body_Size_Max", "Functional_Group_Parsed",
    "Occurrence_classification", "Observed_in_OBIS", "Observed_in_GBIF",
    "OBIS_Occurrence_Lat", "OBIS_Occurrence_Lon",
    "GBIF_Occurrence_Lat", "GBIF_Occurrence_Lon",
    "Group1", "Group2", "TaxGroup",
    "DB_Group1", "DB_Group2", "DB_TaxGroup",
    "Group1_Validated", "Group2_Validated", "TaxGroup_Validated"
  )
  meta_present <- intersect(meta_cols, first_names)
  # Round 60: also never treat the Round 55 per-rank *_Rejected_AphiaID /
  # *_Rejected_Name bookkeeping columns as samples (Module B now drops them;
  # this guards tables saved or loaded from before that fix).
  rejected_bookkeeping <- grep("_Rejected_(AphiaID|Name)$", first_names, value = TRUE)
  samp <- setdiff(first_names, c(tax_present, tax_aphia_present, meta_present, rejected_bookkeeping))

  # Detect blank/control or zero-read sample columns. These should have
  # been removed by the decontamination module (Mod D); flag them so the
  # user does not accidentally aggregate over blanks.
  blank_pattern <- "blank|negative|control|ntc|blanc"
  bl <- samp[grepl(blank_pattern, samp, ignore.case = TRUE)]
  zr <- samp[sapply(samp, function(col) all(is.na(all_data[[col]]) | all_data[[col]] == 0))]
  flagged_blanks <- unique(c(bl, zr))
  if (length(flagged_blanks) > 0) {
    showNotification(paste0("Blank/zero-read sample column(s) detected: ",
                            paste(flagged_blanks, collapse = ", "),
                            ". They may not have been removed by the decontamination module."),
                     type = "warning", duration = 8)
  }

  # Grouping key: each ASV's own resolved consensus AphiaID (see header
  # comment). Unresolved rows each get a unique synthetic key so they are
  # never merged with one another or with anything else.
  fb <- as.character(all_data$Species_fallback)
  unresolved <- is.na(fb) | fb %in% c("", "0", "NA")
  group_key <- fb
  if (any(unresolved)) {
    group_key[unresolved] <- paste0("UNRESOLVED_", seq_len(sum(unresolved)))
  }
  all_data$.agg_key <- group_key

  collate_metadata <- function(rows, cols) {
    if (length(cols) == 0 || nrow(rows) == 0) return(tibble())
    patch <- rows[1, cols, drop = FALSE]
    for (col in cols) {
      vals <- rows[[col]]
      vals <- vals[!is.na(vals)]
      if (col %in% c("Body_Size_Min", "Body_Size_Mean", "Body_Size_Max")) {
        vals <- as.numeric(vals)
        if (length(vals) == 0) {
          patch[[col]] <- NA_real_
        } else {
          key <- round(vals, 6)
          tbl <- sort(table(key), decreasing = TRUE)
          winner <- as.numeric(names(tbl)[1])
          patch[[col]] <- vals[which(key == winner)[1]]
        }
      } else if (col == "WoRMS_Distribution") {
        vals <- iconv(vals, from = "", to = "UTF-8", sub = "")
        all_locs <- unique(trimws(unlist(strsplit(vals, ";"))))
        all_locs <- all_locs[nzchar(all_locs)]
        patch[[col]] <- paste(sort(all_locs), collapse = "; ")
      } else {
        if (length(vals) == 0) {
          patch[[col]] <- NA
        } else {
          tbl <- sort(table(vals, useNA = "no"), decreasing = TRUE)
          winner <- names(tbl)[1]
          patch[[col]] <- vals[which(as.character(vals) == winner)[1]]
        }
      }
    }
    patch
  }

  # Conflict check: within a group (same AphiaID), every taxonomy rank
  # column should already agree by definition — same AphiaID means same
  # organism identity, hence the same full lineage. A mismatch here signals
  # a data inconsistency upstream (e.g. the same AphiaID appearing against
  # two different stated kingdoms), not an expected outcome of a rank
  # choice — there is no rank choice any more; grouping follows each ASV's
  # own resolved identity.
  resolved_groups <- all_data[!all_data$.agg_key %in% group_key[unresolved], , drop = FALSE]
  if (nrow(resolved_groups) > 0) {
    conflict_check <- resolved_groups |>
      group_by(.agg_key) |>
      summarise(across(all_of(tax_present), ~ n_distinct(.x, na.rm = TRUE)), .groups = "drop")
    max_n <- do.call(pmax, c(as.list(conflict_check[tax_present]), list(na.rm = TRUE)))
    conflict_ids <- conflict_check$.agg_key[max_n > 1]
  } else {
    conflict_ids <- character(0)
  }

  # ASV_id is deliberately excluded from the generic majority-vote metadata
  # collation below (2026-09-11 fix). Before this fix, ASV_id was collated
  # the same way as taxonomy/metadata columns: collate_metadata() picks
  # whichever value is most common in the group (ties broken by sort
  # order), which for a column of all-unique ASV identifiers just means
  # "pick one arbitrarily." That single surviving ASV_id then silently
  # stood in for the whole merged taxon row everywhere downstream
  # (Enrichment, Validation, every exported table) — a taxon formed from
  # merging, say, ASV_2/ASV_5/ASV_9 would show up with only "ASV_2" as its
  # identifier and no visible indication it wasn't a single, unmerged ASV.
  # Fixed by giving the aggregated table its own identifier (Taxon_id,
  # below) and preserving the full merge history explicitly
  # (Merged_ASV_ids) instead of losing it behind one arbitrary survivor.
  collate_cols <- setdiff(intersect(c(tax_present, meta_present), first_names), "ASV_id")
  resolved <- all_data |>
    group_by(.agg_key) |>
    group_modify(~ {
      meta_row <- collate_metadata(.x, collate_cols)
      samp_row <- .x |> summarise(across(all_of(samp), ~ sum(.x, na.rm = TRUE)))
      merged_asv_ids <- if ("ASV_id" %in% names(.x)) {
        paste(sort(unique(as.character(.x$ASV_id))), collapse = "; ")
      } else {
        NA_character_
      }
      bind_cols(meta_row, samp_row, Merged_ASV_ids = merged_asv_ids, n_asvs = nrow(.x))
    }) |>
    ungroup()

  resolved$taxonomy_conflict <- resolved$.agg_key %in% conflict_ids
  resolved <- resolved |> select(-.agg_key)

  # Taxon-level identifier for the aggregated table. NOT an ASV id, and
  # deliberately not derived from one — see the comment above collate_cols.
  # Every row from here downstream (Enrichment, Validation, all exported
  # tables) represents a resolved taxon that may be one ASV or many merged
  # ones (see n_asvs / Merged_ASV_ids), so its primary identifier must not
  # look like a single ASV's name.
  resolved$Taxon_id <- sprintf("Taxon_%d", seq_len(nrow(resolved)))
  resolved <- resolved |> relocate(Taxon_id, .before = 1)

  total_reads <- sum(rowSums(resolved[samp], na.rm = TRUE), na.rm = TRUE)

  list(
    resolved = resolved,
    tax_map = tax_map,
    sample_cols = samp,
    total_reads = total_reads,
    flagged_blanks = flagged_blanks
  )
}
