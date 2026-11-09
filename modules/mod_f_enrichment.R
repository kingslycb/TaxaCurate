# ========================================================================
# MODULE F — WoRMS enrichment
# Adds full taxonomy, marine status, distribution, occurrence likelihood
# (via empirical OBIS/GBIF occurrence evidence + WoRMS distribution-text
# keyword matching, as of Round 39 -- generalizable to ANY reference
# region, not just the North Sea), traits, and vernacular names for every
# resolved taxon (one row per taxon post-aggregation — see Module 5; a row
# may represent several merged ASVs, see n_asvs / Merged_ASV_ids). Produces
# 4 output formats: full table,
# marine-only table, traits (single-column), traits (wide format).
# ========================================================================

mod_f_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "step-header",
        span(class = "step-number", "06"),
        div(
          h3("Species ecological enrichment", class = "step-title"),
          p(class = "step-subtitle",
            "Pull full taxonomy, marine status, distribution, traits, and vernacular names for every resolved species.")
        )
    ),

    layout_columns(
      col_widths = c(4, 8),

      card(
        card_header("Inputs & settings"),
        card_body(
          uiOutput(ns("input_status")),
          
          tags$div(class = "api-status-box",
            actionButton(ns("check_api"), "Check Marine Regions API status", class = "btn-sm btn-outline-info w-100"),
            uiOutput(ns("api_status_result"))
          ),
          br(),

          tags$div(class = "example-callout",
            p(class = "example-callout-text", icon("location-dot"),
              " Occurrence likelihood uses empirical OBIS/GBIF occurrence ",
              "records combined with WoRMS distribution-text keyword matching ",
              "(recommended, default), or keyword matching alone, against a ",
              "reference marine region — defaults to the North Sea, but ",
              "you can search for and use ANY region (Mediterranean Sea, ",
              "Gulf of Mexico, etc.)."),
            # Round 65 (2026-10-22): boundary presets (see REFERENCE_REGION_PRESETS in global.R)
            radioButtons(ns("region_preset"), NULL,
                         choices = c(setNames(names(REFERENCE_REGION_PRESETS),
                                              vapply(REFERENCE_REGION_PRESETS, `[[`, "", "label")),
                                     "Other region (search below)" = "custom"),
                         selected = "iho_ns"),
            textInput(ns("region_search"), NULL, value = "North Sea",
                      placeholder = "Search for a reference region, e.g. \"Mediterranean Sea\""),
           actionButton(ns("search_region"), "Search", class = "btn-outline-secondary w-100",
                          icon = icon("magnifying-glass")),
             uiOutput(ns("region_results"))
           ),
           
           # Round 42 (2026-09-29), per explicit user instruction: collapsed
           # from three choices to two. "Gazetteer (Structural)" and "Both
           # (Hybrid)" had become identical since Round 39 removed gazetteer's
           # hop-distance from classification entirely -- both ran the exact
           # same OBIS/GBIF + text evidence chain, so keeping "Gazetteer
           # (Structural)" as a separate, distinctly-labeled choice was
           # actively misleading (it implied a pure geographic/structural
           # method that no longer existed). Replaced with a single,
           # accurately-labeled "evidence" option.
           radioButtons(ns("classification_method"), "Classification method:",
                        choices = c("OBIS/GBIF + Text (recommended)" = "evidence",
                                    "Text-based (Keyword)" = "text"),
                        selected = "evidence"),

           selectInput(ns("vernacular_lang"), "Vernacular language(s):",

                       choices = c("English","Dutch","French","German",
                                   "Italian","Japanese","Korean","Norwegian",
                                   "Polish","Portuguese","Russian","Spanish",
                                   "Swedish","Turkish","Chinese"),
                       selected = "English", multiple = TRUE, width = "100%"),
          numericInput(ns("worms_delay"), "Delay between WoRMS requests (seconds)",
                       value = 0.05, min = 0, max = 2, step = 0.01),
          checkboxInput(ns("worms_use_traits"),
                        "Use WoRMS trait cache for functional group classification (experimental)",
                        value = FALSE),
          tags$details(
            tags$summary("Taxonomic exclusions"),
            br(),
            p(class = "text-muted small",
              "Kingdoms/classes excluded from every output table below (this is a ",
              "deliberate taxonomic filter, unrelated to and applied independently ",
              "of occurrence likelihood / marine status):"),
            textInput(ns("exclude_kingdoms"), "Exclude kingdoms (comma-separated)",
                      value = "Protozoa, Bacteria, Fungi, Plantae, Chromista, Viruses"),
            textInput(ns("exclude_classes"), "Exclude classes (comma-separated)",
                      value = "Arachnida, Hexapoda, Insecta")
          ),
          tags$details(
            tags$summary("Marine-only filter settings"),
            br(),
            p(class = "text-muted small",
              "The marine-only output is filtered to IsMarine = TRUE (WoRMS habitat flag). ",
              "The full table is never filtered this way — both are always produced together.")
          ),
          div(style = "margin-top: 8px; display: flex; gap: 6px;",
              actionButton(ns("run"), "Run ecological annotation", class = "btn-primary flex-fill",
                           icon = icon("play")),
              actionButton(ns("cancel"), "Cancel", class = "btn-outline-danger",
                           icon = icon("stop"))
          )
        )
      ),

      card(
        card_header("Status & results"),
        card_body(
          uiOutput(ns("status_box")),
          tabsetPanel(
            tabPanel("Summary", verbatimTextOutput(ns("summary_text"))),
            tabPanel("Full table", DTOutput(ns("full_table"))),
            tabPanel("Marine-only", DTOutput(ns("marine_table"))),
            tabPanel("Occurrence breakdown", plotOutput(ns("occurrence_plot"), height = 360)),
            tabPanel("Harmonized Taxonomy", DTOutput(ns("harmonized_table")))
          ),
          # NOTE (2026-09-11): a separate "Download harmonized taxonomy" button
          # used to live here. Removed as redundant: Module 7 (Validation) runs
          # on this exact harmonized table automatically (no separate "run" step)
          # and its own "Download Validated Table" export contains the same
          # columns for every row plus validation status — a strict superset.
          # The "Harmonized Taxonomy" tab (on-screen preview) is unchanged; only
          # the duplicate download was removed. The underlying harmonized table
          # itself is unchanged too — Module 7 still consumes it via
          # shared$modF_data below.
          # Round 47 (2026-10-01): "Download species-level"/"Download
          # genus-level" buttons removed from here -- redundant with Module
          # G's single "Download Validated Table" export, which now covers
          # every taxon at every rank from this same full table. See the
          # note at dl_species/dl_genus's old location (removed) below.
          div(style = "margin-top: 12px; display: flex; gap: 8px; flex-wrap: wrap;",
              downloadButton(ns("dl_full"), "Download full table", class = "btn-outline-secondary"),
              downloadButton(ns("dl_marine"), "Download marine-only table", class = "btn-outline-secondary"),
              downloadButton(ns("dl_traits_single"), "Download traits (single column)", class = "btn-outline-secondary"),
              downloadButton(ns("dl_traits_wide"), "Download traits (wide format)", class = "btn-outline-secondary"),
              downloadButton(ns("dl_log_txt"), "Download module log (.txt)", class = "btn-outline-secondary"),
              downloadButton(ns("dl_log_csv"), "Download module log (.csv)", class = "btn-outline-secondary")
          )
        )
      )
    )
  )
}

mod_f_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Marine Regions API Status Check
    observeEvent(input$check_api, {
      output$api_status_result <- renderUI({
        div(class = "text-muted small", "Checking Marine Regions API...")
      })
      url <- "https://www.marineregions.org/rest/getGazetteerRecordsByName.json/North+Sea/"
      res <- tryCatch({
        resp <- request(url) %>% req_timeout(5) %>% req_perform()
        resp_status(resp) == 200
      }, error = function(e) FALSE)
      output$api_status_result <- renderUI({
        if (res) {
          div(class = "alert alert-success mt-2 py-1 px-2", icon("circle-check"), " Marine Regions API is online.")
        } else {
          div(class = "alert alert-danger mt-2 py-1 px-2", icon("circle-xmark"), " Marine Regions API unreachable.")
        }
      })
    })

    rv <- reactiveValues(
      df_with_worms = NULL, df_marine_only = NULL,
      worms_df = NULL, status = NULL, cancel = FALSE,
      reference_mrgid = DEFAULT_REFERENCE_MRGID,
      reference_name = DEFAULT_REFERENCE_NAME,
      reference_bbox = get_reference_bbox(DEFAULT_REFERENCE_MRGID),
      region_candidates = NULL,
      region_search_gen = 0L,
      log_txt_path = NULL,
      log_csv_path = NULL
    )

    .region_pick_observers <- list()

    output$input_status <- renderUI({
      # Round 4 (2026-09-09): Module E (Taxon Aggregation) now runs right
      # after Decontamination and before this module, so Enrichment's input
      # is the aggregated table, not the raw decontaminated ASV table.
      have_input <- !is.null(shared$modE_data)
      row <- function(label, ok) {
        tags$div(class = if (ok) "input-ok" else "input-missing",
                 icon(if (ok) "circle-check" else "circle-minus"), " ", label)
      }
      tagList(
        p(class = "text-muted small", "Input carried over from Module 5 (Aggregation):"),
        row("Aggregated taxon table", have_input),
        if (!have_input) div(class = "alert alert-warning mt-2", "Run Module 5 (Aggregation) first.")
      )
    })

    output$status_box <- renderUI({
      if (is.null(rv$status)) return(NULL)
      cls <- switch(rv$status$type, info = "alert-info", success = "alert-success",
                    error = "alert-danger", "alert-secondary")
      div(class = paste("alert", cls), role = "alert", rv$status$msg)
    })

    # Round 65: reference-region presets. Fires once at start with "iho_ns"
    # (= the defaults above), then on every change; "custom" leaves the
    # current region as it is.
    observeEvent(input$region_preset, {
      p <- REFERENCE_REGION_PRESETS[[input$region_preset]]
      if (!is.null(p)) {
        rv$reference_mrgid <- p$mrgid
        rv$reference_name <- p$name
        rv$reference_bbox <- get_reference_bbox(p$mrgid)
        rv$region_candidates <- NULL
      }
    })

    observeEvent(input$search_region, {
      query <- trimws(input$region_search)
      if (query == "") {
        rv$region_candidates <- NULL
        return(invisible(NULL))
      }
      results <- gazetteer_search_by_name(query)
      rv$region_search_gen <- rv$region_search_gen + 1L
      if (nrow(results) == 0 || !"MRGID" %in% names(results)) {
        rv$region_candidates <- tibble()
        return(invisible(NULL))
      }
      rv$region_candidates <- results %>% slice_head(n = 10)
    })

    output$region_results <- renderUI({
      if (is.null(rv$region_candidates)) {
        return(p(class = "text-muted small", style = "margin-top: 8px;",
                  sprintf("Current reference region: %s (MRGID %d)", rv$reference_name, rv$reference_mrgid)))
      }
      if (nrow(rv$region_candidates) == 0) {
        return(div(class = "alert alert-warning mt-2",
                    "No matching region found in the Marine Regions gazetteer. Try a different spelling or a broader term."))
      }
      gen <- rv$region_search_gen
      tagList(
        p(class = "text-muted small", style = "margin-top: 8px;", "Select a match:"),
        tagList(lapply(seq_len(nrow(rv$region_candidates)), function(i) {
          rec <- rv$region_candidates[i, ]
          actionButton(ns(paste0("pick_region_", gen, "_", i)),
                       sprintf("%s (%s, MRGID %s)", rec$preferredGazetteerName %||% "?",
                                rec$placeType %||% "?", rec$MRGID %||% "?"),
                       class = "btn-outline-secondary w-100 mb-1", style = "text-align: left; font-size: 0.8rem;")
        }))
      )
    })

    observeEvent(rv$region_candidates, {
      for (obs in .region_pick_observers) obs$destroy()
      .region_pick_observers <<- list()

      candidates <- rv$region_candidates
      if (is.null(candidates) || nrow(candidates) == 0) return(invisible(NULL))
      gen <- rv$region_search_gen

      new_observers <- lapply(seq_len(nrow(candidates)), function(i) {
        local({
          idx <- i
          this_gen <- gen
          observeEvent(input[[paste0("pick_region_", this_gen, "_", idx)]], {
            rec <- candidates[idx, ]
            mrgid <- suppressWarnings(as.integer(rec$MRGID))
            if (!is.na(mrgid)) {
              rv$reference_mrgid <- mrgid
              rv$reference_name <- rec$preferredGazetteerName %||% paste("MRGID", mrgid)
              rv$reference_bbox <- get_reference_bbox(mrgid)
              rv$region_candidates <- NULL
              # Round 65: keep the preset buttons in step with a searched pick
              updateRadioButtons(session, "region_preset", selected = reference_preset_for_mrgid(mrgid))
            }
          }, ignoreInit = TRUE, once = TRUE)
        })
      })
      .region_pick_observers <<- new_observers
    }, ignoreNULL = FALSE)

    observeEvent(input$cancel, {
      rv$cancel <- TRUE
      rv$status <- list(type = "warning", msg = "Cancelling WoRMS enrichment... Results will be discarded.")
    })

    # --- Main Execution Loop ---
    observeEvent(input$run, {
      req(!is.null(shared$modE_data))
      shinyjs::disable("run")
      on.exit(shinyjs::enable("run"))
      # Check all required APIs before starting
      failed_apis <- c()
      for (nm in names(REQUIRED_APIS)) {
        if (!check_api_reachable(REQUIRED_APIS[[nm]]$url)) {
          failed_apis <- c(failed_apis, REQUIRED_APIS[[nm]]$label)
        }
      }
      if (length(failed_apis) > 0) {
        proxy_hint <- ""
        if (nzchar(Sys.getenv("http_proxy")) || nzchar(Sys.getenv("https_proxy"))) {
          proxy_hint <- sprintf(" (proxy: %s)", Sys.getenv("http_proxy", Sys.getenv("https_proxy", "")))
        }
        msg <- sprintf("Cannot start enrichment — %s unreachable%s. Check your internet/proxy connection and try again.",
                       paste(failed_apis, collapse = ", "), proxy_hint)
        rv$status <- list(type = "error", msg = msg)
        showNotification(msg, type = "error", duration = 15)
        message("ERROR: ", msg)
        return(invisible(NULL))
      }
      rv$cancel <- FALSE
      rv$status <- list(type = "info", msg = "Initializing WoRMS enrichment...")

      # Round 4 (2026-09-09): Module E (Taxon Aggregation) now runs BEFORE
      # this module, so Enrichment's input is the aggregated table
      # (shared$modE_data), not the raw decontaminated ASV table. Note this
      # module must NOT reset shared$modE_data here — that field is now
      # upstream of Enrichment, not downstream of it, so clearing it here
      # would erase the very input this run just read.
      df_input <- shared$modE_data
      
        withProgress(message = "Ecological Annotation", value = 0, {
        message("Step 1: Starting species resolution...")
        incProgress(0.1, detail = "Resolving species...")

        # Use Species_fallback (the consensus AphiaID from Module B) as the
        # key for WoRMS enrichment. Per-DB taxonomy columns (MZG_Species,
        # BOLD_Species, BLAST_Species) were already dropped by
        # build_species_fallback, so Species is derived from the WoRMS
        # enrichment query + Consensus_Species fallback below.
        if ("Species_fallback" %in% names(df_input)) {
          species_data <- df_input %>%
            select(Taxon_id, any_of("Species"), AphiaID = Species_fallback) %>%
            distinct() %>%
            filter(!is.na(AphiaID) & AphiaID != 0)
        } else if ("AphiaID" %in% names(df_input)) {
          species_data <- df_input %>% select(Taxon_id, any_of("Species"), AphiaID) %>% distinct() %>%
            filter(!is.na(AphiaID) & AphiaID != 0)
        } else {
          # Round 59 (2026-10-05): the former name-based fallback for tables
          # without an AphiaID column is removed. It re-resolved names against
          # WoRMS here (first record, no accepted-name check), which could
          # contradict what Module 1 resolved. It was also unreachable in the
          # app: this module's input is Module 5's output, and Module 5
          # (run_taxon_aggregation() in global.R) refuses any table without
          # Species_fallback. Identities are resolved ONCE, in Module 1.
          rv$status <- list(type = "error", msg = "The aggregated table has no AphiaID / Species_fallback column, so it did not come through Modules 1-5. Re-run Module 1 (Taxonomy), then Modules 2-5, instead of enriching names here.")
          return(invisible(NULL))
        }

        # Round 59: subspecies are KEPT as the subspecies (no promotion to the
        # parent species any more). Whatever the subspecies itself lacks (WoRMS
        # distribution/attributes/vernacular names, OBIS/GBIF presence, traits)
        # is borrowed from the parent species, never replacing what it has --
        # see safe_worms_batch_query_complete(), batch_check_obis_gbif_presence()
        # and fetch_validation_traits() in global.R.

        rv$status <- list(type = "info", msg = sprintf("Step 1/2: Enriching %d unique AphiaIDs via WoRMS...", length(unique(species_data$AphiaID))))
        
        # To avoid re-querying the same AphiaID multiple times:
        unique_aphia_ids <- unique(species_data$AphiaID)
        
        # Chunk into groups of 50 to avoid overloading the API
        chunk_size <- 50
        chunks <- split(unique_aphia_ids, ceiling(seq_along(unique_aphia_ids) / chunk_size))
        
        enrichment_results <- list()
        # Round 50 (2026-10-01): count of HTTP 429/503 rate-limit responses
        # hit during WoRMS enrichment this run, across all chunks -- see
        # worms_batch_query()'s Round 50 comment in global.R for the bug
        # this instruments (WoRMS silently going permanently NA partway
        # through a run). Surfaced in this module's log below so a future
        # recurrence is visible without needing console access.
        worms_rate_limit_events <- 0L

        n_chunks <- length(chunks)
        for (i in seq_along(chunks)) {
          if (isolate(rv$cancel)) {
            rv$status <- list(type = "warning", msg = "Ecological annotation cancelled.")
            return(invisible(NULL))
          }
          Sys.sleep(0.01) # Yield to Shiny event loop so cancel button responds instantly
          chunk <- chunks[[i]]
          rv$status <- list(type = "info", msg = sprintf("Enriching species batch %d/%d (%d species)...", i, n_chunks, length(chunk)))

          # Perform batch query
          # Round 50b (2026-10-01): cancel_check wired through so a persistent
          # WoRMS retry/backoff inside worms_batch_query() (global.R) can
          # still be interrupted promptly by this module's own Cancel button,
          # consistent with the isolate(rv$cancel) check in the outer loop above.
          batch_res <- safe_worms_batch_query_complete(chunk, delay = input$worms_delay, vernacular_language = paste(input$vernacular_lang, collapse = ","), fetch_taxonomy = FALSE, logger = message, cancel_check = function() isolate(rv$cancel))
          worms_rate_limit_events <- worms_rate_limit_events + (attr(batch_res, "rate_limit_events") %||% 0L)
          enrichment_results <- c(enrichment_results, batch_res)

          incProgress(1 / n_chunks * 0.5, detail = sprintf("Batch %d/%d processed", i, n_chunks))
        }
        
        if (length(enrichment_results) == 0) {
           rv$status <- list(type = "error", msg = "Enrichment failed to retrieve any data.")
           return(invisible(NULL))
        }

        # Build lookup table from the results
        worms_lookup <- purrr::map_df(enrichment_results, ~ .x$worms_data)
        worms_lookup_unique <- worms_lookup %>% 
          group_by(AphiaID) %>%
          slice(1) %>%
          ungroup()

        # Round 58: this module keeps the identity (AphiaID + name) that
        # Module 1 resolved and never re-resolves it -- a retired name was
        # already swapped to its accepted AphiaID there (Round 56). The only
        # thing done here is to REPORT, not change, any ID that reached this
        # point whose WoRMS record is not "accepted" (e.g. Module 1 could not
        # reach WoRMS for it), so a disagreement with Module 1 is visible in
        # the log instead of silent.
        n_not_accepted <- sum(!is.na(worms_lookup_unique$WoRMS_Status) &
                              worms_lookup_unique$WoRMS_Status != "accepted")
        if (n_not_accepted > 0) {
          message(sprintf("[WoRMS] %d AphiaID(s) arrived from Module 1 with a non-accepted WoRMS status; kept as resolved there (not re-resolved here). See WoRMS_Status.", n_not_accepted))
        }

        all_dist_df <- purrr::map_df(enrichment_results, ~ .x$dist_df)
        
        rv$worms_df <- worms_lookup_unique
        
        # Build species_mrgids from all_dist_df using AphiaID join
        if (!is.null(all_dist_df) && nrow(all_dist_df) > 0) {
          # Use only unique AphiaIDs to avoid many-to-many warning
          species_mrgids <- {
            check_join_cols(all_dist_df, species_data, "AphiaID", "all_dist_df", "species_data")
            all_dist_df %>%
            left_join(species_data %>% select(AphiaID) %>% distinct(), by = "AphiaID") %>%
            mutate(mrgids = map(locationID, extract_all_mrgids)) %>%
            unnest(mrgids, keep_empty = TRUE) %>%
            rename(mrgid = mrgids) %>%
            filter(!is.na(mrgid)) %>%
            select(AphiaID, mrgid, locality) %>%
            distinct()
          }
          message("DEBUG: species_mrgids rows: ", nrow(species_mrgids))
          if (nrow(species_mrgids) > 0) message("DEBUG: First few MRGIDs: ", paste(head(species_mrgids$mrgid), collapse=", "))
          
        } else {
          species_mrgids <- tibble(AphiaID = integer(), mrgid = integer(), locality = character())
        }
        
        # Join enrichment data back to the input table
        # df_input is Module 5's aggregated (already-collapsed-to-resolved-taxon) table
# Drop bare taxonomy and per-level columns before WoRMS enrichment.
      # Consensus_* columns are kept through enrichment so the coalesce
      # below can fill taxonomy gaps with the Module B fallback.
      # Species_fallback (2026-09-11): dropped from the exported table here —
      # it duplicates the AphiaID column joined in below (identical now that
      # Round 59 no longer promotes subspecies to their parent), and
      # having two AphiaID-shaped columns that agree was confusing to read. Still used
      # internally: species_data/species_map are derived from df_input
      # (not df_input_prepared) above this point, so removing it here has no
      # effect on the join logic itself.
      # Round 55: also drops the new per-rank Rejected_AphiaID/Rejected_Name
      # columns (a refused context-disagreeing match, kept only for Module
      # B's agreement comparison — see check_agreement() in global.R). They
      # were never meant to reach an export; same treatment as confidence/
      # context_agreed above.
      tax_cols_to_drop <- c(
        "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "AphiaID",
        "Species_fallback",
        grep("^(Kingdom|Phylum|Class|Order|Family|Genus|Species)_(AphiaID|confidence|context_agreed|Rejected_AphiaID|Rejected_Name)$",
             names(df_input), value = TRUE)
      )
        df_input_prepared <- df_input %>%
          select(-any_of(tax_cols_to_drop))

        # Join by Taxon_id (to link to table) then AphiaID (to link to enriched
        # taxonomy). Taxon_id, not ASV_id, is the per-row identifier from
        # Module 5 (Aggregation) onward — see run_taxon_aggregation() in
        # global.R.
        species_map <- species_data %>%
          select(Taxon_id, AphiaID) %>%
          group_by(Taxon_id) %>%
          slice(1) %>%
          ungroup()

        check_join_cols(df_input_prepared, species_map, "Taxon_id", "df_input_prepared", "species_map")
        df_with_worms <- df_input_prepared %>%
          mutate(row_idx = row_number()) %>%
          left_join(species_map, by = "Taxon_id")
        check_join_cols(df_with_worms, worms_lookup_unique, "AphiaID", "df_with_worms", "worms_lookup_unique")
        df_with_worms <- df_with_worms %>% left_join(worms_lookup_unique, by = "AphiaID")

        # IsBrackish/IsFreshwater/IsTerrestrial: dropped here from 2026-09-11
        # through round 36 (only IsMarine was ever filtered/read anywhere in
        # the app, so these three WoRMS habitat flags reached the Full Table
        # with no downstream use). Reinstated (round 36 follow-up,
        # 2026-09-29, per direct user request) alongside adding them to the
        # Harmonized-table whitelist below -- they're kept from this point on
        # so they're available for cols_to_keep to pick up.

        # WoRMS enrichment provides bare taxonomy columns (Kingdom–Species)
        # directly from the API at the AphiaID's rank. NAs at ranks above the
        # AphiaID's own rank are preserved — they are NOT filled from Consensus_*
        # fallback. Both column sets are retained for the Full Table; the
        # Harmonized output uses Consensus_* as the canonical taxonomy.

        # Deduplicated OBIS/GBIF presence checks (query each unique species once)
        # OBIS/GBIF presence checks use the canonical Consensus_* taxonomy
        lookup_species_col <- if ("Consensus_Species" %in% names(df_with_worms)) "Consensus_Species" else "Species"
        unique_species <- unique(na.omit(df_with_worms[[lookup_species_col]]))
        # Lookup vectors are now genuinely logical/numeric (previously
        # `character(0)` vectors that a logical TRUE/FALSE got coerced into on
        # assignment - harmless in practice since every downstream comparison
        # happened to coerce consistently, but this keeps Observed_in_OBIS/GBIF
        # correctly typed as logical rather than "TRUE"/"FALSE" strings).
        obis_present <- logical(0); obis_lat <- numeric(0); obis_lon <- numeric(0)
        gbif_present <- logical(0); gbif_lat <- numeric(0); gbif_lon <- numeric(0)
        obis_matched_name <- character(0); gbif_matched_name <- character(0)
        # Round 29: whether the OBIS/GBIF query for this species genuinely
        # succeeded (got a real, parseable response) -- distinct from
        # obis_present/gbif_present, which is FALSE both when genuinely
        # checked-and-absent AND when the query itself failed. Defaults to
        # FALSE (not checked) for every species until proven otherwise below.
        obis_checked <- logical(0); gbif_checked <- logical(0)

        # Round 43 (2026-09-29), per explicit user decision: when the
        # reference region is the North Sea, occurrence classification adds
        # a German Bight sub-region tier -- presence within German Bight
        # specifically (checked via OBIS and/or GBIF, either database alone
        # sufficient) counts as "Very likely" rather than the flat "Likely"
        # a wider-North-Sea-only hit gets. German Bight's own MRGID is
        # resolved LIVE via gazetteer_search_by_name() (global.R), mirroring
        # the app's existing region-search flow, per the user's explicit
        # choice of a real gazetteer lookup over a hardcoded bbox -- and its
        # real coastline geometry then comes for free from the SAME pinned/
        # dynamic-polygon pipeline (get_reference_geometry_wkt()) the main
        # reference region already uses, simplified and cached to disk after
        # the first fetch exactly like any other MRGID. Resolved once per
        # run, not per chunk, since German Bight's MRGID/geometry are fixed
        # across a session. For any OTHER reference region (gb_active stays
        # FALSE), this has zero effect -- .german_bight_true below is never
        # TRUE and the rule chain falls back to its flat OBIS/GBIF tiering.
        gb_region <- if (!is.na(rv$reference_mrgid) && rv$reference_mrgid %in% GERMAN_BIGHT_TIERING_MRGIDS) {
          resolve_german_bight_region(logger = message)
        } else {
          list(mrgid = NA_integer_, bbox = NULL)
        }
        gb_active <- !is.na(gb_region$mrgid) && !is.null(gb_region$bbox)
        if (gb_active) {
          message(sprintf("[GermanBight] Sub-region tiering active this run (MRGID %d).", gb_region$mrgid))
        } else if (!is.na(rv$reference_mrgid) && rv$reference_mrgid %in% GERMAN_BIGHT_TIERING_MRGIDS) {
          message(sprintf("[GermanBight] %s reference region, but German Bight could not be resolved this run -- falling back to flat tiering (no German-Bight 'Very likely' upgrade available).", rv$reference_name))
        }
        gb_obis_present <- logical(0); gb_gbif_present <- logical(0)

        if (length(unique_species) > 0) {
          obis_present <- setNames(logical(length(unique_species)), unique_species)
          obis_lat <- setNames(rep(NA_real_, length(unique_species)), unique_species)
          obis_lon <- setNames(rep(NA_real_, length(unique_species)), unique_species)
          gbif_present <- setNames(logical(length(unique_species)), unique_species)
          gbif_lat <- setNames(rep(NA_real_, length(unique_species)), unique_species)
          gbif_lon <- setNames(rep(NA_real_, length(unique_species)), unique_species)
          gb_obis_present <- setNames(logical(length(unique_species)), unique_species)
          gb_gbif_present <- setNames(logical(length(unique_species)), unique_species)
          obis_matched_name <- setNames(rep(NA_character_, length(unique_species)), unique_species)
          gbif_matched_name <- setNames(rep(NA_character_, length(unique_species)), unique_species)
          obis_checked <- setNames(logical(length(unique_species)), unique_species)
          gbif_checked <- setNames(logical(length(unique_species)), unique_species)

          # Round 6 (2026-09-11): build a species -> AphiaID map so
          # batch_check_obis_gbif_presence() can retry WoRMS synonyms when
          # the primary (current, accepted) name comes back absent from
          # OBIS/GBIF. Both aggregators index their own, separately-frozen
          # taxonomic backbones, which can lag months behind a WoRMS
          # reclassification — verified live for a recently-reclassified
          # species (WoRMS updated 2026-03-09) that OBIS/GBIF still only
          # recognize under the retired name/AphiaID. Without this, a
          # correctly-reclassified species is reported as a FALSE NEGATIVE
          # ("not observed anywhere") purely because the aggregators
          # haven't caught up yet. One AphiaID per species (first non-NA).
          species_to_aphia <- NULL
          if ("AphiaID" %in% names(df_with_worms)) {
            aphia_map_df <- df_with_worms %>%
              filter(!is.na(.data[[lookup_species_col]]), !is.na(AphiaID)) %>%
              distinct(.data[[lookup_species_col]], .keep_all = TRUE)
            if (nrow(aphia_map_df) > 0) {
              species_to_aphia <- setNames(aphia_map_df$AphiaID, aphia_map_df[[lookup_species_col]])
            }
          }

          # Round 8 (2026-09-09): replaced the old one-species-at-a-time
          # check_obis_presence()/check_gbif_presence() loop with
          # batch_check_obis_gbif_presence() (global.R), which fires all
          # OBIS+GBIF requests for a chunk through req_perform_parallel() —
          # the same pattern worms_batch_query() already used above.
          # Benchmarked (benchmark_enrichment.R) at ~10x faster for the
          # same HTTP calls, with 0 result differences from the old
          # sequential loop across 40 test species — this was the dominant
          # cost of "Run ecological annotation", not the WoRMS step.
          # Chunked in groups of 50, same size as the WoRMS batch loop
          # above, purely so the cancel button still responds between
          # chunks on large species lists rather than only between two
          # giant all-at-once request batches.
          chunk_size_og <- 50
          og_chunks <- split(unique_species, ceiling(seq_along(unique_species) / chunk_size_og))
          n_og_chunks <- length(og_chunks)
          for (k in seq_along(og_chunks)) {
            if (isolate(rv$cancel)) break
            Sys.sleep(0.01) # Yield to Shiny event loop so cancel button responds instantly
            chunk_sp <- og_chunks[[k]]
            # reference_mrgid passed through (Round 20 follow-up, 2026-09-21)
            # so batch_check_obis_gbif_presence() can use the real coastline
            # polygon in REFERENCE_REGION_POLYGONS (global.R) instead of the
            # bbox rectangle when one exists for the selected reference
            # region -- currently just the North Sea (MRGID 2350).
            batch_res <- batch_check_obis_gbif_presence(chunk_sp, rv$reference_bbox, species_to_aphia,
                                                         reference_mrgid = rv$reference_mrgid)
            for (sp in chunk_sp) {
              r <- batch_res[[sp]]
              obis_present[sp] <- isTRUE(r$obis$present); obis_lat[sp] <- r$obis$lat; obis_lon[sp] <- r$obis$lon
              gbif_present[sp] <- isTRUE(r$gbif$present); gbif_lat[sp] <- r$gbif$lat; gbif_lon[sp] <- r$gbif$lon
              obis_matched_name[sp] <- r$obis$matched_name %||% NA_character_
              gbif_matched_name[sp] <- r$gbif$matched_name %||% NA_character_
              # Round 29: TRUE only when the OBIS/GBIF query for this species
              # genuinely succeeded (a real, parseable response was received)
              # -- independent of whether it found a record. A checked=FALSE
              # here means the query itself couldn't be run/parsed (no
              # species match, API failure, timeout), which must never be
              # treated as evidence of absence downstream.
              obis_checked[sp] <- isTRUE(r$obis$checked); gbif_checked[sp] <- isTRUE(r$gbif$checked)
            }

            # Round 43 (2026-09-29): a SECOND OBIS/GBIF wave, scoped to German
            # Bight's own geometry instead of the wider North Sea, fired only
            # when this run's reference region is the North Sea and German
            # Bight resolved successfully above. This genuinely doubles the
            # OBIS/GBIF network cost for a North Sea run (GBIF in particular
            # is paced ~1 request/second per Round 34 -- see
            # batch_check_obis_gbif_presence() in global.R), which is the
            # real, known trade-off of the German Bight tier and is called
            # out in USER_GUIDE.md/CHANGELOG.md. Every other reference region
            # is completely unaffected (gb_active is FALSE, this block never
            # runs, zero added cost).
            if (gb_active) {
              gb_batch_res <- batch_check_obis_gbif_presence(chunk_sp, gb_region$bbox, species_to_aphia,
                                                              reference_mrgid = gb_region$mrgid)
              for (sp in chunk_sp) {
                r_gb <- gb_batch_res[[sp]]
                gb_obis_present[sp] <- isTRUE(r_gb$obis$present)
                gb_gbif_present[sp] <- isTRUE(r_gb$gbif$present)
              }
              incProgress(0, detail = sprintf("German Bight OBIS/GBIF batch %d/%d (%d species)", k, n_og_chunks, length(chunk_sp)))
            }
            incProgress(0, detail = sprintf("OBIS/GBIF batch %d/%d (%d species)", k, n_og_chunks, length(chunk_sp)))
          }
        }

        df_with_worms <- df_with_worms %>%
          mutate(
            Observed_in_OBIS = ifelse(!is.na(.data[[lookup_species_col]]), unname(obis_present[.data[[lookup_species_col]]]), FALSE),
            Observed_in_GBIF = ifelse(!is.na(.data[[lookup_species_col]]), unname(gbif_present[.data[[lookup_species_col]]]), FALSE),
            # Round 29: whether the OBIS/GBIF query itself genuinely succeeded
            # for this species -- distinct from Observed_in_OBIS/GBIF, which
            # is FALSE both for a genuine checked-and-absent result AND for a
            # query that simply failed to run. Used below (hybrid method) to
            # gate the new negative-evidence tier rules so a failed check can
            # never be mistaken for confirmed absence.
            Observed_in_OBIS_Checked = ifelse(!is.na(.data[[lookup_species_col]]), unname(obis_checked[.data[[lookup_species_col]]]), FALSE),
            Observed_in_GBIF_Checked = ifelse(!is.na(.data[[lookup_species_col]]), unname(gbif_checked[.data[[lookup_species_col]]]), FALSE),
            # Coordinates of one OBIS/GBIF occurrence record that validated
            # presence — NA when the species wasn't found (or coordinates
            # weren't reported on the returned record).
            OBIS_Occurrence_Lat = ifelse(!is.na(.data[[lookup_species_col]]), unname(obis_lat[.data[[lookup_species_col]]]), NA_real_),
            OBIS_Occurrence_Lon = ifelse(!is.na(.data[[lookup_species_col]]), unname(obis_lon[.data[[lookup_species_col]]]), NA_real_),
            GBIF_Occurrence_Lat = ifelse(!is.na(.data[[lookup_species_col]]), unname(gbif_lat[.data[[lookup_species_col]]]), NA_real_),
            GBIF_Occurrence_Lon = ifelse(!is.na(.data[[lookup_species_col]]), unname(gbif_lon[.data[[lookup_species_col]]]), NA_real_),
            # Name that actually produced the OBIS/GBIF hit: equals the
            # Consensus_Species name itself when found directly, a WoRMS
            # synonym/retired combination when the current name had to
            # fall back, or NA when neither was found anywhere.
            OBIS_Matched_Name = ifelse(!is.na(.data[[lookup_species_col]]), unname(obis_matched_name[.data[[lookup_species_col]]]), NA_character_),
            GBIF_Matched_Name = ifelse(!is.na(.data[[lookup_species_col]]), unname(gbif_matched_name[.data[[lookup_species_col]]]), NA_character_),
            # Round 43 (2026-09-29): German Bight sub-region presence, North
            # Sea reference runs only -- FALSE for every row when gb_active
            # is FALSE (any other reference region, or German Bight couldn't
            # be resolved live this run). Used only to refine
            # Occurrence_classification below (a German-Bight hit upgrades a
            # wider-North-Sea "Likely" to "Very likely"); kept as its own
            # output column so a "Very likely via German Bight specifically"
            # result is traceable rather than indistinguishable from a
            # wider-North-Sea-only "Very likely".
            Observed_in_OBIS_GermanBight = ifelse(!is.na(.data[[lookup_species_col]]), unname(gb_obis_present[.data[[lookup_species_col]]]), FALSE),
            Observed_in_GBIF_GermanBight = ifelse(!is.na(.data[[lookup_species_col]]), unname(gb_gbif_present[.data[[lookup_species_col]]]), FALSE)
          )
        
        # 2. Step 2: Batch Gazetteer Classification
        message("Step 2: Starting occurrence classification...")
        rv$status <- list(type = "info", msg = "Step 2: Starting occurrence classification...")
        incProgress(0.3, detail = "Calculating classification...")
        
        if (input$classification_method == "evidence") {
            # Round 39 (2026-09-29) fix, per explicit user decision ("please
            # remove Gazetteer's hop-distance"): gazetteer's hop-distance
            # signal is removed entirely from occurrence classification.
            # Rationale, confirmed against a real run on 2026-09-29: with
            # the gazetteer caches cleared, a full 763-MRGID warming pass
            # resolved a gazetteer path for 0 of 409 taxa, yet the run's
            # final classification breakdown was still fully populated --
            # proving the OBIS/GBIF + text evidence chain below already
            # carries the real signal on its own, with zero contribution
            # from gazetteer's hop-count. (Round 42: the old "gazetteer" and
            # "hybrid" dropdown choices, which had become identical once
            # gazetteer stopped contributing anything, were collapsed into
            # this single "evidence" choice -- see the radioButtons()
            # definition above.)
            # (The persistent gazetteer_hops_cache.rds / .gazetteer_hops_cache_env
            # caching machinery in global.R, and the Round 38 checkpointing
            # fix that hardened it, are left in place untouched -- simply
            # unused by classification from here on.)
            df_with_worms <- df_with_worms %>%
                mutate(Occurrence_classification = "Unknown",
                       Occurrence_hops_to_reference_region = NA_integer_,
                       Occurrence_nearest_locality = NA_character_,
                       Parsed_MRGIDs = NA_character_,
                       Occurrence_NA_Reason = "Not applicable — gazetteer hop-distance classification removed (Round 39); classification now based on OBIS/GBIF + text evidence only",
                       # Occurrence_Single_DB_Only (Round 46b, 2026-09-30):
                       # default FALSE here; overwritten below for rows that
                       # actually reach the evidence chain's database-only
                       # "Likely" rule. See that mutate's comment for the
                       # full rationale.
                       Occurrence_Single_DB_Only = FALSE)

            # ---- Round 39 (2026-09-29): OBIS/GBIF + text evidence chain,
            # replacing Round 33's gazetteer-anchored 8-rule chain. Per the
            # user's explicit methodological position: "Everything should
            # revolve around OBIS/GBIF because this is the hard evidence."
            # Updated Round 40 (2026-09-29), per explicit user instruction:
            # rule 1 below now distinguishes a same-continent vs a
            # different-continent locality mention rather than collapsing
            # both into "Very unlikely" uniformly.
            # Round 43 follow-up #2 (2026-09-29), per explicit user
            # from-scratch redefinition -- superseding every rule-chain
            # comment from Round 39-43 above this point. classify_occurrence_by_text()
            # (global.R) already grades the distribution text into the SAME
            # 6-value scale used here ("Very likely"/"Likely"/"Possible"/
            # "Unlikely"/"Very unlikely"/"Unknown") via its own keyword tiers
            # (very-likely: reference name/North Sea/German Bight/Wadden Sea/
            # etc.; likely: Northeast Atlantic/Celtic Sea/Irish Sea/Bay of
            # Biscay/etc.; possible: European waters/Mediterranean/Baltic/
            # Black Sea/Adriatic; unlikely: bare "Atlantic"; very unlikely:
            # Pacific/Indian Ocean/Hawaiian/Caribbean/Antarctic) -- this is
            # used DIRECTLY below as an independent evidence path, not
            # collapsed to a boolean the way Round 39-43 did.
            # Ordered rule chain (case_when is first-match-wins):
            #   1. WoRMS explicitly flags the species as neither marine nor
            #      brackish (IsMarine == FALSE AND IsBrackish == FALSE, both
            #      genuinely known -- NA on either flag does NOT trigger this)
            #      -> "Very unlikely", unconditionally. Per explicit user
            #      decision (2026-09-29): OBIS/GBIF's geometry-based presence
            #      queries have no marine-realm filter, so a stray
            #      terrestrial/freshwater occurrence record landing inside the
            #      search polygon can otherwise read as "present" (e.g.
            #      Gnaphalium, a cudweed genus, showing OBIS/GBIF TRUE and
            #      reaching "Likely"). This rule outranks every rule below,
            #      including a positive OBIS/GBIF or German Bight hit -- a
            #      species WoRMS itself calls non-marine/non-brackish can't be
            #      rescued into "Likely"/"Very likely" by such a hit.
            #   2. German Bight sub-region presence (North Sea runs only) --
            #      checked via OBIS and/or GBIF, either database alone
            #      sufficient -> "Very likely". German Bight is a strict
            #      subset of the wider North Sea polygon, so a positive hit
            #      here is real positive evidence and outranks everything
            #      below it, including a same-run wider-zone absence.
            #   3. text classifier tier == "Very likely" (North Sea/German
            #      Bight/Wadden Sea/etc. named in the distribution text) ->
            #      "Very likely", independent of database status -- strong,
            #      on-point text evidence stands on its own. (If OBIS/GBIF
            #      happen to be double-negative at the same time, that
            #      contradiction is still surfaced via
            #      Occurrence_Evidence_Conflict below, not silently hidden.)
            #   4. OBIS TRUE or GBIF TRUE (wider North Sea/reference-region
            #      zone, not already caught by rule 2) -> "Likely". OBIS is
            #      the primary, sufficient signal by itself; GBIF alone still
            #      counts when OBIS didn't confirm. Per explicit user
            #      correction, GBIF agreeing with OBIS no longer upgrades
            #      this to "Very likely" -- that tier is reserved for rules
            #      2-3 only.
            #   5. text classifier tier == "Likely" (Northeast Atlantic/
            #      Celtic Sea/Irish Sea/Norwegian Sea/Bay of Biscay/British
            #      Isles -- i.e. the wider North Atlantic area, named but not
            #      the North Sea itself) -> "Likely", independent of database
            #      status.
            #   6. text classifier tier == "Very unlikely" (Pacific/Indian
            #      Ocean/Hawaiian/Caribbean/Antarctic -- definitively
            #      non-European) -> "Very unlikely".
            #   7. text_locality_continent() (global.R; a broader same/
            #      different-continent read against SEA_TO_CONTINENTS, not
            #      limited to classify_occurrence_by_text()'s narrower
            #      5-keyword "very likely/unlikely" lists) says "different" ->
            #      "Very unlikely". Catches genuinely foreign localities
            #      outside classify_occurrence_by_text()'s own keyword
            #      coverage (e.g. "Gulf of Mexico", "Sea of Japan").
            #   8. text classifier tier %in% c("Possible", "Unlikely") --
            #      "Possible" is literally "Occurrence in the European Seas
            #      except the North Sea" (European waters/Mediterranean/
            #      Baltic/Black Sea/Adriatic); "Unlikely" is a bare, ambiguous
            #      "Atlantic" mention -> both map to "Unlikely".
            #   9. text_locality_continent() says "same" -> "Unlikely".
            #      Broader same-continent safety net beyond rule 8's
            #      narrower keyword coverage (e.g. "Iceland", "Faroe",
            #      "White Sea" aren't in classify_occurrence_by_text()'s own
            #      lists but are in SEA_TO_CONTINENTS).
            #   10. genuine OBIS double-negative alone (GBIF NON-GATING -- see
            #      below), with NO usable text-locality signal at all (rules
            #      3/5/6/7/8/9 all didn't match) -> "Unlikely". A real,
            #      checked OBIS absence is hard evidence on its own even with
            #      zero text signal to pair it with.
            #   11. otherwise (no database signal AND no usable text-locality
            #      signal either) -> stays "Unknown" (falls through to the
            #      OBIS/GBIF gap-filler just below, unchanged since Round 17).
            # GBIF non-gating (unchanged from Round 43 #1): .double_negative
            # requires OBIS's own check to have genuinely succeeded and
            # absent, AND GBIF to NOT positively confirm presence -- GBIF
            # being unchecked/failed, or itself checked-and-absent, no longer
            # blocks rule 9 (previously required GBIF to ALSO be
            # checked-and-absent, which let GBIF's own rate-limiting
            # flakiness, Round 34, silently flip this rule run to run).
            df_with_worms <- df_with_worms %>%
              mutate(
                .not_marine = IsMarine %in% FALSE & IsBrackish %in% FALSE,
                .obis_true = !is.na(Observed_in_OBIS) & Observed_in_OBIS == TRUE,
                .gbif_true = !is.na(Observed_in_GBIF) & Observed_in_GBIF == TRUE,
                .obis_checked_false = Observed_in_OBIS_Checked & !.obis_true,
                .double_negative = .obis_checked_false & !.gbif_true,
                .text_tier = sapply(WoRMS_Distribution, classify_occurrence_by_text, reference_name = rv$reference_name),
                .locality_continent = sapply(WoRMS_Distribution, text_locality_continent, reference_name = rv$reference_name),
                .german_bight_true = (!is.na(Observed_in_OBIS_GermanBight) & Observed_in_OBIS_GermanBight) |
                                      (!is.na(Observed_in_GBIF_GermanBight) & Observed_in_GBIF_GermanBight),
                # Occurrence_Single_DB_Only (Round 46b, 2026-09-30), per
                # explicit user decision: a purely additive review flag,
                # never changes Occurrence_classification itself -- exactly
                # like Occurrence_Evidence_Conflict. Surfaces rows where rule
                # 4's "Likely" rests on EXACTLY ONE of OBIS/GBIF confirming
                # presence (not both), with ZERO WoRMS distribution text to
                # corroborate (.text_tier == "Unknown" -- text either absent
                # or genuinely unrecognized). Found via Oxydromus vittatus:
                # GBIF-only hit, no OBIS confirmation, no distribution text
                # at all -- worth a human spot-check of that specific
                # database record, since a single-source, uncorroborated hit
                # is real but weaker evidence than the other paths to
                # "Likely" (German Bight, both databases agreeing, or text
                # corroboration). Deliberately does NOT touch the tier
                # itself -- the non-marine hard override already covers the
                # "clearly wrong" failure mode (confirmed non-marine/
                # non-brackish); this just keeps a weaker-but-still-real
                # evidence case visible rather than indistinguishable from a
                # stronger one.
                # Gated on !.not_marine & !.german_bight_true & .text_tier !=
                # "Very likely" so this only claims TRUE for rows where rule
                # 4 (database-only "Likely") is genuinely what would fire --
                # not a row a higher-priority rule already resolved.
                .single_db_only_flag = xor(.obis_true, .gbif_true) & .text_tier == "Unknown" &
                                        !.not_marine & !.german_bight_true,
                .primary_tier = case_when(
                  .not_marine ~ "Very unlikely",
                  .german_bight_true ~ "Very likely",
                  .text_tier == "Very likely" ~ "Very likely",
                  .obis_true | .gbif_true ~ "Likely",
                  .text_tier == "Likely" ~ "Likely",
                  .text_tier == "Very unlikely" ~ "Very unlikely",
                  !is.na(.locality_continent) & .locality_continent == "different" ~ "Very unlikely",
                  .text_tier %in% c("Possible", "Unlikely") ~ "Unlikely",
                  !is.na(.locality_continent) & .locality_continent == "same" ~ "Unlikely",
                  .double_negative ~ "Unlikely",
                  TRUE ~ NA_character_
                ),
                Occurrence_classification = ifelse(!is.na(.primary_tier), .primary_tier, Occurrence_classification),
                Occurrence_Single_DB_Only = ifelse(!is.na(.primary_tier) & .primary_tier == "Likely",
                                                    .single_db_only_flag, Occurrence_Single_DB_Only)
              ) %>%
              select(-.not_marine, -.obis_true, -.gbif_true, -.obis_checked_false, -.double_negative, -.text_tier, -.locality_continent, -.german_bight_true, -.primary_tier, -.single_db_only_flag)
        } else {
            # --- Text-based (Keyword) Classification ---
            message("DEBUG: Using text-based classification")
            df_with_worms <- df_with_worms %>%
                mutate(
                    Occurrence_classification = sapply(WoRMS_Distribution, classify_occurrence_by_text, reference_name = rv$reference_name),
                    Occurrence_hops_to_reference_region = NA_integer_,
                    Occurrence_nearest_locality = NA_character_,
                    Parsed_MRGIDs = NA_character_
                )
        }
        # Round 65: which boundary this classification was made against
        if (nrow(df_with_worms) > 0)
          df_with_worms$Occurrence_reference_region <- sprintf("%s (MRGID %d)", rv$reference_name, rv$reference_mrgid)

        # --- Use empirical OBIS/GBIF evidence to resolve cases still "Unknown"
        # after whichever method ran above. Still the primary rescue path for
        # the plain "text" method, which never runs the evidence chain above.
        # For "evidence" this is now effectively a no-op/safety-net: every row
        # where OBIS or GBIF is TRUE was already resolved (to "Likely" or
        # "Very likely") by the block above, so occ_was_unknown & (OBIS|GBIF)
        # can no longer both be true there -- it only still fires here for
        # "text"-method rows.
        occ_was_unknown <- df_with_worms$Occurrence_classification == "Unknown"
        df_with_worms <- df_with_worms %>%
            mutate(
                Occurrence_classification = case_when(
                    occ_was_unknown & Observed_in_OBIS == TRUE & Observed_in_GBIF == TRUE ~ "Likely",
                    occ_was_unknown & (Observed_in_OBIS == TRUE | Observed_in_GBIF == TRUE) ~ "Possible",
                    TRUE ~ Occurrence_classification
                )
            )

        # Non-marine/non-brackish HARD override (Round 44b, 2026-09-30), per
        # explicit user correction ("No, nothing overrides this"): rule 1 in
        # the "evidence" case_when above already puts this first for that one
        # classification method, but that alone doesn't protect the plain
        # "text" method (classify_occurrence_by_text() knows nothing about
        # IsMarine/IsBrackish) or the OBIS/GBIF gap-filler just above (which
        # runs for BOTH methods and could otherwise re-promote a non-marine
        # row from "Unknown" to "Likely"/"Possible" off a stray geometry hit).
        # Applied here, unconditionally, after both classification methods and
        # the gap-filler -- nothing overrides this, regardless of
        # classification_method or what was already assigned above.
        .not_marine_final <- !is.na(df_with_worms$IsMarine) & df_with_worms$IsMarine == FALSE &
                              !is.na(df_with_worms$IsBrackish) & df_with_worms$IsBrackish == FALSE
        df_with_worms$Occurrence_classification[.not_marine_final] <- "Very unlikely"

        # Species-specific expert override (Round 46, 2026-09-30) -- see the
        # fully-commented load_species_overrides()/apply_species_overrides()
        # block in global.R for the full rationale. Applied last,
        # unconditionally, after every rule above including the non-marine
        # hard override just above -- per explicit user decision, nothing
        # outranks a recorded expert correction.
        df_with_worms <- apply_species_overrides(df_with_worms, logger = message)

        # Round 17 (2026-09-17), updated Round 28 (2026-09-28), updated Round 39
        # (2026-09-29): purely additive review flag -- does NOT change
        # Occurrence_classification itself. This surfaces cases worth a human
        # look because the *confident* classification and the actual
        # observational evidence point opposite ways:
        #   - "Unlikely"/"Very unlikely" while OBIS or GBIF actually has a
        #     record within the reference region's geometry, or
        #   - "Likely"/"Very likely" while BOTH OBIS and GBIF came back with
        #     no record at all.
        # (Round 28's separate `evidence_overrode_gazetteer` condition --
        # flagging rows where the OBIS/GBIF+text block overrode gazetteer's
        # own tier -- was removed in Round 39 along with gazetteer's opinion
        # itself: there is nothing left for it to have overridden.)
        # Round 43 (2026-09-29): this flag now also incidentally catches a
        # new possible case for North Sea runs -- a "Very likely" driven by
        # German Bight's independently-queried geometry while the wider-zone
        # Observed_in_OBIS/Observed_in_GBIF (queried separately, against a
        # different polygon) both came back checked-absent. That combination
        # would mean German Bight's own polygon disagrees with the wider
        # North Sea polygon that's supposed to contain it -- worth a human
        # look precisely because it points at a geometry bug (independently
        # fetched/simplified polygons not nesting as expected) rather than a
        # real ecological result. No code change needed here: the existing
        # confident_positive & both-checked-absent condition already covers
        # this automatically.
        obis_confirmed <- !is.na(df_with_worms$Observed_in_OBIS) & df_with_worms$Observed_in_OBIS == TRUE
        gbif_confirmed <- !is.na(df_with_worms$Observed_in_GBIF) & df_with_worms$Observed_in_GBIF == TRUE
        obis_checked_absent <- !is.na(df_with_worms$Observed_in_OBIS) & df_with_worms$Observed_in_OBIS == FALSE
        gbif_checked_absent <- !is.na(df_with_worms$Observed_in_GBIF) & df_with_worms$Observed_in_GBIF == FALSE
        confident_negative <- df_with_worms$Occurrence_classification %in% c("Unlikely", "Very unlikely")
        confident_positive <- df_with_worms$Occurrence_classification %in% c("Likely", "Very likely")
        # Round 39 (2026-09-29) fix, per explicit user decision ("please
        # remove Gazetteer's hop-distance"): gazetteer no longer produces an
        # opinion at all (see Step 2 above -- both "gazetteer" and "hybrid"
        # selections now run the same OBIS/GBIF + text evidence chain), so
        # there is nothing left for this flag to have "overridden". Dropped
        # entirely; Occurrence_Evidence_Conflict keeps its other two,
        # still-meaningful conditions unchanged.
        df_with_worms$Occurrence_Evidence_Conflict <-
            (confident_negative & (obis_confirmed | gbif_confirmed)) |
            (confident_positive & obis_checked_absent & gbif_checked_absent)

        # 3. Post-processing (Exclusions, Marine-only, Traits, Vernacular)
        message("Step 3: Post-processing...")
        incProgress(0.1, detail = "Post-processing...")
        
        # Taxonomic exclusions (Case-insensitive across all taxonomy columns)
        exc_kin <- str_split(input$exclude_kingdoms, ",") %>% map(trimws) %>% unlist()
        exc_kin <- exc_kin[nzchar(exc_kin)] # Remove empty strings
        exc_cla <- str_split(input$exclude_classes, ",") %>% map(trimws) %>% unlist()
        exc_cla <- exc_cla[nzchar(exc_cla)] # Remove empty strings
        
        all_exc <- tolower(c(exc_kin, exc_cla))
        if (length(all_exc) > 0) {
          tax_levels <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
          tax_cols <- intersect(c(tax_levels, paste0("Consensus_", tax_levels)), names(df_with_worms))
          if (length(tax_cols) > 0) {
            match_matrix <- sapply(tax_cols, function(col) tolower(df_with_worms[[col]]) %in% all_exc)
            if (is.matrix(match_matrix)) {
              any_match <- rowSums(match_matrix, na.rm = TRUE) > 0
            } else {
              any_match <- any(match_matrix, na.rm = TRUE)
            }
            df_with_worms <- df_with_worms[!any_match, , drop = FALSE]
          }
        }
        
        shared$excluded_kingdoms <- exc_kin
        shared$excluded_classes <- exc_cla
        
        # 4. Trait validation
        # Priority order for Group1/Group2/TaxGroup. Round 64 (2026-10-08,
        # user-approved after checking 287 species against their life
        # cycles): (1) the pipeline's own hand-curated phylum/class/order
        # lookup table (classify_taxonomic_group()) FIRST; (2) the live WoRMS
        # attribute classification, when "Use WoRMS trait cache" is enabled,
        # fills only the rows the table has no entry for; (3) WoRMS
        # "Functional group" text; (4) the external MarineSpeciesTraits DB,
        # only for rows still blank; (5) config/trait_overrides.csv always
        # wins. Before Round 64 step (2) ran first (Round 21 order) and the
        # curated table only filled what it left blank. That let a pooled
        # Phylum+Class+Order WoRMS attribute guess overrule the curated table:
        # on the 287-species check, the table alone matched the life-cycle
        # class for 262 species, the old order for 164 (cladocerans and many
        # bivalves/gastropods/sea stars/brittle stars came out wrong).
        # Round 21 order, for the record: live WoRMS attributes first, then
        # the lookup table for the blanks, then the external DB.
        #
        # NOTE: this reverses, a second time, the DB-before-pipeline-lookup
        # order that was chosen at the user's request on 2026-09-09 (see
        # worms_pipeline_app_v8_module_review.md) to work around bugs that
        # existed in classify_taxonomic_group() at the time. Those bugs are
        # now fixed (Round 17b: the Onychopoda/Ctenopoda vs. the old, never-
        # matching "Diplostraca" Order; Round 21: the Copepoda-vs-Hexanauplia
        # Class-naming inconsistency that let harpacticoid/siphonostomatoid
        # copepods fall through to a class-wide holoplankton fallback), and
        # Round 17c/21 found concrete, real-data cases where the external DB
        # itself was simply wrong (*Tenuilineus albocinctus*, *Hubrechtella
        # dubia* — both wrongly "holoplankton" in the DB; *Longipedia
        # coronata* — wrongly "holoplankton" via the since-fixed Round 21
        # bug) and, under the DB-first order, silently overrode the
        # pipeline's own correct, order-aware answer before it ever got a
        # chance to run. classify_taxonomic_group() is a small, hand-curated,
        # order-aware table that's now been through several rounds of
        # correctness fixes; the external DB is a much larger but coarser,
        # externally-maintained dataset — treating the DB as the tie-breaker
        # of last resort, rather than the default, is the more defensible
        # default now that the specific bugs motivating the 2026-09-09
        # change are gone. Group1_Validated / Group2_Validated /
        # TaxGroup_Validated below still flag any mismatch between the final
        # (DB-independent) classification and the DB for manual review.
        message("Step 4: Performing trait validation...")
        incProgress(0.1, detail = "Querying trait database...")
        rv$status <- list(type = "info", msg = "Validating traits against MarineSpeciesTraits DB...")

        if (!"Group1" %in% names(df_with_worms)) df_with_worms$Group1 <- NA_character_
        if (!"Group2" %in% names(df_with_worms)) df_with_worms$Group2 <- NA_character_
        if (!"TaxGroup" %in% names(df_with_worms)) df_with_worms$TaxGroup <- NA_character_

        # DB validation (MarineSpeciesTraits) always runs, independent of the
        # priority order below — DB_Group1/DB_Group2/DB_TaxGroup are needed
        # regardless, both as a fallback source and for the *_Validated flags.
        aphia_ids <- df_with_worms$AphiaID
        db_g1 <- character(length(aphia_ids))
        db_g2 <- character(length(aphia_ids))
        db_tg <- character(length(aphia_ids))
        for (j in seq_along(aphia_ids)) {
          if (isolate(rv$cancel)) break
          aid <- aphia_ids[j]
          res <- if (!is.na(aid)) fetch_validation_traits(aid) else list(Group1=NA_character_, Group2=NA_character_, TaxGroup=NA_character_)
          db_g1[j] <- res$Group1 %||% NA_character_
          db_g2[j] <- res$Group2 %||% NA_character_
          db_tg[j] <- res$TaxGroup %||% NA_character_
          incProgress(0, detail = sprintf("Trait DB: %d/%d species", j, length(aphia_ids)))
        }
        df_with_worms$DB_Group1 <- db_g1
        df_with_worms$DB_Group2 <- db_g2
        df_with_worms$DB_TaxGroup <- db_tg

        # FG_source (added 2026-09-22, comprehensive cross-app audit, Round
        # 22f, per explicit user request for parity): tags which priority
        # tier actually supplied each row's Group1/Group2/TaxGroup --
        # "taxonomy" (Priority 1), "worms_trait" (Priority 2; before Round 64
        # these two were the other way round), "external_db" (Priority 3), or
        # "conflict" (set below by
        # cross_validate_fg() when live WoRMS attribute text disagrees).
        # Ported from eDNAcheck_v2.0.0's bg_functions.R, which already had
        # this column -- classify_taxonomic_group() and cross_validate_fg()
        # already existed in this app's own global.R (byte-identical to
        # eDNAcheck's copies) but were never wired into this cascade or
        # exposed as an output column until now.
        if (!"FG_source" %in% names(df_with_worms)) df_with_worms$FG_source <- NA_character_

        # PRIORITY 1 (Round 64, 2026-10-08) — the pipeline's own hand-curated,
        # order-aware phylum/class/order lookup table. It runs first, for every
        # row, because it carries the taxon-specific fixes (Onychopoda/Ctenopoda
        # cladocerans, Harpacticoida/Copepoda naming, Canuelloida, ...) that a
        # pooled higher-taxon WoRMS attribute guess cannot make.
        tax_group_res <- classify_taxonomic_group(df_with_worms$Phylum, df_with_worms$Class, df_with_worms$Order)
        df_with_worms$Group1 <- ifelse(is.na(df_with_worms$Group1), tax_group_res$Group1, df_with_worms$Group1)
        df_with_worms$Group2 <- ifelse(is.na(df_with_worms$Group2), tax_group_res$Group2, df_with_worms$Group2)
        df_with_worms$TaxGroup <- ifelse(is.na(df_with_worms$TaxGroup), tax_group_res$TaxGroup, df_with_worms$TaxGroup)
        df_with_worms$FG_source <- ifelse(is.na(df_with_worms$FG_source) & !is.na(df_with_worms$Group1),
                                            "taxonomy", df_with_worms$FG_source)

        # PRIORITY 2 — live WoRMS attribute-based classification (opt-in via
        # "Use WoRMS trait cache"). Since Round 64 it only fills the rows the
        # lookup table above has no entry for (it used to run first and
        # overrule the table). The trait cache is built only for the
        # phyla/classes/orders of those rows, so it also makes fewer WoRMS
        # calls.
        trait_cache <- NULL
        trait_todo <- which(is.na(df_with_worms$Group1))
        if (isTRUE(input$worms_use_traits) && length(trait_todo) > 0) {
          message(sprintf("Step 5: Running live WoRMS trait-attribute classification (priority 2) for %d row(s) the lookup table did not classify...", length(trait_todo)))
          incProgress(0.1, detail = "Building live WoRMS trait cache...")
          rv$status <- list(type = "info", msg = "Querying live WoRMS attribute data for ecological classification...")
          trait_cache <- build_worms_trait_cache(
            df_with_worms$Phylum[trait_todo], df_with_worms$Class[trait_todo], df_with_worms$Order[trait_todo],
            delay = input$worms_delay
          )
          for (i in trait_todo) {
            if (rv$cancel) break
            res <- classify_by_trait_cache(df_with_worms$Phylum[i], df_with_worms$Class[i], df_with_worms$Order[i], trait_cache)
            if (!is.null(res)) {
              df_with_worms$Group1[i] <- res$Group1
              df_with_worms$Group2[i] <- res$Group2
              df_with_worms$TaxGroup[i] <- res$TaxGroup
              df_with_worms$FG_source[i] <- "worms_trait"
            }
          }
        }

        # PRIORITY 2B (round 34 follow-up, 2026-09-29, per direct user
        # request): fill whatever Priority 1 (the hand-curated
        # taxonomic_group_lookup table) and Priority 2 (opt-in WoRMS
        # attributes) both left blank --
        # i.e. a Phylum/Class/Order combination that table has no entry for
        # at all (e.g. Actiniaria, Verrucomorpha -- see the diagnosis doc's
        # Round 32 finding) -- from the WoRMS "Functional group" attribute
        # text already fetched for every row during the main query
        # (Functional_Group_Parsed), no extra API call needed. Deliberately
        # placed AFTER the curated table (Priority 1), not before it: that table has
        # been through several rounds of deliberate, taxon-specific
        # correctness fixes (e.g. Round 17c: WoRMS/the external DB lacking
        # explicit larval-stage data for known meroplanktonic taxa like
        # Nemertea), so it must still win whenever it has an answer -- this
        # step only ever reaches species that table has no opinion on at
        # all. Placed BEFORE Priority 3 (the external DB) because this is
        # real, already-fetched, species-level WoRMS data (include_inherited
        # = TRUE, so it inherits the order/class-level attribute when the
        # species' own record lacks one), which several rounds of this
        # project's history (17c, and the Edwardsia timida/Metridium
        # senile/Ascidiella scabra cases the "Quality Check output" question
        # surfaced) have shown to be more reliable for larval-stage
        # detection than the external MarineSpeciesTraits DB. See
        # classify_from_fg_text() in global.R.
        if (!"Functional_Group_Parsed" %in% names(df_with_worms)) df_with_worms$Functional_Group_Parsed <- NA_character_
        fg0 <- classify_from_fg_text(df_with_worms$Functional_Group_Parsed)
        fill0 <- is.na(df_with_worms$Group1) & !is.na(fg0$Group1)
        df_with_worms$Group1 <- ifelse(fill0, fg0$Group1, df_with_worms$Group1)
        df_with_worms$Group2 <- ifelse(fill0, fg0$Group2, df_with_worms$Group2)
        df_with_worms$FG_source <- ifelse(fill0, "worms_attr_prefetched", df_with_worms$FG_source)

        # Independent-of-DB classification, captured HERE -- after Priority 1
        # and Priority 2 have both run, but BEFORE the Priority-3 DB fallback
        # below touches anything. This is exactly what will ship in
        # Group1/Group2/TaxGroup for every row the DB fallback doesn't need
        # to touch, and it's used so the *_Validated flags compare the DB
        # against a classification computed INDEPENDENTLY of the DB, rather
        # than against a value the DB fallback, one block later, may have
        # copied directly FROM the DB -- which would make the comparison
        # tautologically TRUE for every row the DB fallback touches. (Round
        # 17, 2026-09-17: this exact tautology previously hid real
        # disagreements between the DB and the pipeline's own taxonomy
        # lookup for *Tenuilineus albocinctus* and *Hubrechtella dubia*.)
        indep_group1 <- df_with_worms$Group1
        indep_group2 <- df_with_worms$Group2
        indep_taxgroup <- df_with_worms$TaxGroup

        # PRIORITY 3 (final fallback, Round 21) — fill whatever's still
        # blank after Priority 1 and Priority 2 -- i.e. a species/order
        # combination classify_taxonomic_group()'s table has no entry for at
        # all -- from the external MarineSpeciesTraits DB. The fill mask is
        # captured before Group1 is overwritten so FG_source can be tagged
        # "external_db" for exactly the rows this step fills (mirroring
        # eDNAcheck_v2.0.0's bg_functions.R, which tags on the Group1 fill
        # mask specifically, not per-column).
        db_fill_g1 <- is.na(df_with_worms$Group1) & !is.na(df_with_worms$DB_Group1)
        df_with_worms$Group1 <- ifelse(db_fill_g1, df_with_worms$DB_Group1, df_with_worms$Group1)
        df_with_worms$Group2 <- ifelse(is.na(df_with_worms$Group2) & !is.na(df_with_worms$DB_Group2), df_with_worms$DB_Group2, df_with_worms$Group2)
        df_with_worms$TaxGroup <- ifelse(is.na(df_with_worms$TaxGroup) & !is.na(df_with_worms$DB_TaxGroup), df_with_worms$DB_TaxGroup, df_with_worms$TaxGroup)
        df_with_worms$FG_source[db_fill_g1] <- "external_db"

        # Cross-validate the taxonomy-based FG_source tags against live WoRMS
        # "Functional group" attribute text, same as eDNAcheck_v2.0.0: flags
        # a row "conflict" if e.g. Group1 says non-benthic but WoRMS' own
        # functional-group attribute says "benthos". Uses the same,
        # byte-identical cross_validate_fg() this file's global.R has always
        # had defined but, until now, never called.
        df_with_worms$FG_source <- cross_validate_fg(
          df_with_worms$FG_source, df_with_worms$Group1, df_with_worms$WoRMS_FunctionalGroup
        )

        # Compute Validated columns -- DB vs. the independent (non-DB) answer.
        # Group1's meroplankton/benthic-invertebrate pairing is treated as a
        # match, not a mismatch, in either direction (round 34 follow-up,
        # 2026-09-29, per direct user request -- see group1_values_compatible()
        # in global.R for why).
        df_with_worms$Group1_Validated <- ifelse(!is.na(indep_group1) & !is.na(df_with_worms$DB_Group1),
                                                   group1_values_compatible(indep_group1, df_with_worms$DB_Group1), NA)
        df_with_worms$Group2_Validated <- ifelse(!is.na(indep_group2) & !is.na(df_with_worms$DB_Group2),
                                                   indep_group2 == df_with_worms$DB_Group2, NA)
        df_with_worms$TaxGroup_Validated <- ifelse(!is.na(indep_taxgroup) & !is.na(df_with_worms$DB_TaxGroup),
                                                     indep_taxgroup == df_with_worms$DB_TaxGroup, NA)

        # Species-specific trait expert override (Round 51, 2026-10-02) --
        # see the fully-commented load_trait_overrides()/apply_trait_overrides()
        # block in global.R for the full rationale. Applied last,
        # unconditionally, after Priority 1/2/2B/3 and the *_Validated flags
        # above have all run -- per the same "nothing outranks a recorded
        # expert correction" precedent as the occurrence override below.
        df_with_worms <- apply_trait_overrides(df_with_worms, logger = message)

        # 6. Final assembly and filters
        message("Step 6: Finalizing...")
        incProgress(0.1, detail = "Finalizing output...")
        
        # 7. Sanity Check (only when trait cache was built)
        message("Step 7: Running sanity check...")
        if (!is.null(trait_cache)) {
            df_with_worms <- run_enrichment_sanity_check(df_with_worms, rv$reference_name, trait_cache)
        }
        
        # Marine-only filter
        df_marine_only <- df_with_worms %>%
          filter(!is.na(IsMarine) & IsMarine == TRUE)
        
        # 8. Update reactive values
        rv$df_with_worms <- df_with_worms
        rv$df_marine_only <- df_marine_only
        
        # Derive sample columns from the aggregated table (blanks already
        # removed upstream in Module 4; ASVs already collapsed to resolved
        # taxa upstream in Module 5). "n_asvs" and "Merged_ASV_ids" are
        # Module 5's own merge-bookkeeping columns, not sample/abundance
        # counts — they must be excluded here or they get treated as sample
        # columns and summed downstream like read counts, which would be
        # meaningless.
        #
        # Round 47 (2026-10-01): use Module E's own canonical sample-column
        # list (shared$modE_sample_cols, set alongside shared$modE_data in
        # mod_e_aggregation.R) instead of re-deriving a guess here via a
        # substring exclude-pattern blacklist. That blacklist previously had
        # to be kept in sync by hand with every metadata column added
        # anywhere upstream, and silently drifted out of date (see the
        # matching note in mod_g_validation.R's sample_cols(), which is the
        # other half of this same bug). Falling back to the old heuristic
        # only covers the edge case of a manual/standalone run that never
        # went through Module E in this session -- it is logged loudly
        # rather than silently trusted, since it's exactly the kind of
        # unchecked guess this round is trying to get rid of.
        if (!is.null(shared$modE_sample_cols)) {
          mod_f_sample_cols <- intersect(shared$modE_sample_cols, names(df_input))
        } else {
          message("WARNING: shared$modE_sample_cols unavailable -- falling back to ",
                   "substring-based sample-column detection for this run. Counts should ",
                   "still be double-checked against Module E's own reported sample count.")
          exclude_patterns <- c("Kingdom", "Phylum", "Class", "Order", "Family",
                                  "Genus", "Species", "Taxon_id", "ASV_id", "n_asvs",
                                  "AphiaID", "Species_fallback", "Fallback_rank",
                                  "WoRMS_Rank", "IsMarine", "WoRMS_Distribution",
                                  "Body_Size", "Functional_Group", "FG_source",
                                  "Occurrence_classification", "Observed_in_OBIS", "Observed_in_GBIF",
                                  "Occurrence_Evidence_Conflict", "Occurrence_reference_region",
                                  "DB_Group", "Group1_Validated", "Group2_Validated", "TaxGroup_Validated",
                                  "taxonomy_conflict", "Sequence_length")
          mod_f_sample_cols <- setdiff(
            names(df_input),
            unique(unlist(lapply(exclude_patterns, function(p) grep(p, names(df_input), ignore.case = TRUE, value = TRUE))))
          )
        }

        # Round 47 (2026-10-01): taxon-count consistency check -- Module F
        # enriches each taxon in place and must never silently gain or lose
        # rows relative to its input (Module E's aggregated table). A
        # mismatch here means a join inside enrichment fanned out
        # one-to-many (duplicated taxa) or dropped unmatched rows -- either
        # way, worth surfacing immediately rather than discovering it later
        # by manually diffing exported CSVs.
        if (nrow(df_with_worms) != nrow(df_input)) {
          warning(sprintf(
            paste0("Module F taxon-count mismatch: %d taxa went in (Module E output), ",
                   "%d came out of enrichment. Every taxon should be enriched in place ",
                   "with no rows added or dropped -- check for a one-to-many join or an ",
                   "unintended filter()."),
            nrow(df_input), nrow(df_with_worms)
          ))
        }

        # Harmonize taxonomy for final output — use Consensus_* as canonical
        # taxonomy. Bare WoRMS taxonomy columns remain in the Full Table for
        # reference but are not included in the simplified Harmonized output.
        #
        # Vernacular names (round 35 follow-up, 2026-09-29, per direct user
        # request): fetched into WoRMS_vernacular_<language> columns (Step 1,
        # per safe_worms_batch_query_complete()'s vern_data handling in
        # global.R) and already visible in Module F's own Full Table, but
        # never added to the cols_to_keep whitelist below -- so they were
        # silently dropped before reaching the Harmonized table, and from
        # there, Module G's Quality Check output. Detected dynamically here
        # (mirroring the existing vern_cols detection used for the Full
        # Table's own summary stats, below in this file) rather than
        # hardcoded to specific language names, since "vernacular_lang"
        # above lets the user pick any of 15 languages, not just whichever
        # ones happen to be selected on a given run.
        vern_cols_to_keep <- grep("^WoRMS_vernacular_", names(df_with_worms), value = TRUE)

        cols_to_keep <- c(
          "Taxon_id", mod_f_sample_cols, "Sequence_length_bp", "n_asvs", "Merged_ASV_ids",
          "AphiaID", "Species_fallback_source", "Fallback_rank",
          "Consensus_Kingdom", "Consensus_Phylum", "Consensus_Class",
          "Consensus_Order", "Consensus_Family", "Consensus_Genus", "Consensus_Species",
          "WoRMS_Rank", "WoRMS_Status",
           # IsBrackish/IsFreshwater/IsTerrestrial (round 36 follow-up,
           # 2026-09-29, per direct user request): reinstated alongside
           # IsMarine -- see the "IsBrackish/IsFreshwater/IsTerrestrial"
           # comment above, where they're kept in df_with_worms in the first
           # place, rather than dropped before this point.
           "IsMarine", "IsBrackish", "IsFreshwater", "IsTerrestrial",
           "WoRMS_Distribution", "Body_Size_Min", "Body_Size_Mean", "Body_Size_Max",
           "Functional_Group_Parsed",
           # WoRMS_attributes/Species_Paraphyletic_Group/WoRMS_FunctionalGroup/
           # WoRMS_URL (round 37, 2026-09-29): identified during the round-36
           # "which other useful columns drop along the way?" review as
           # further gaps but not requested at the time -- added now per
           # direct user request. WoRMS_attributes is the full raw
           # pipe-delimited attribute text (all categories, not just
           # functional group); Species_Paraphyletic_Group is the parsed
           # "Paraphyletic group" attribute (parsed the same way
           # Functional_Group_Parsed is, but previously never carried
           # through); WoRMS_FunctionalGroup is the value cross_validate_fg()
           # actually compares Group1 against to set FG_source = "conflict"
           # -- previously invisible to a reviewer trying to see *why* that
           # flag fired; WoRMS_URL is a direct AphiaID record link.
           "WoRMS_attributes", "Species_Paraphyletic_Group",
           "WoRMS_FunctionalGroup", "WoRMS_URL",
           # WoRMS_Synonym_Fallback_Used (Round 51, 2026-10-02): see
           # safe_worms_batch_query_complete()'s own comment in global.R.
           "WoRMS_Synonym_Fallback_Used",
           # WoRMS_Parent_Fallback_Used (Round 59): subspecies whose WoRMS
           # attributes were borrowed from the parent species.
           "WoRMS_Parent_Fallback_Used",
           vern_cols_to_keep,
           "Occurrence_classification", "Occurrence_Evidence_Conflict",
           # Occurrence_reference_region (Round 65): the boundary the
           # classification was made against, e.g. "North Sea (MRGID 2350)".
           "Occurrence_reference_region",
           # Occurrence_Override_Applied/Occurrence_Override_Reason (Round 46,
           # 2026-09-30): surfaces the species-specific expert override
           # mechanism's effect (if any) in the exported table -- without
           # this, a row's classification could silently differ from what
           # the automated rule chain alone would have produced, with no
           # visible trace of why.
           "Occurrence_Override_Applied", "Occurrence_Override_Reason",
           # Occurrence_Single_DB_Only (Round 46b, 2026-09-30): surfaces a
           # "Likely" that rests on exactly one of OBIS/GBIF confirming
           # presence, with zero corroborating distribution text -- see the
           # fully-commented mutate above for the rationale.
           "Occurrence_Single_DB_Only",
           # Occurrence_hops_to_reference_region/Occurrence_nearest_locality
           # (round 36 follow-up, 2026-09-29, per direct user request): the
           # raw gazetteer BFS hop-count and the specific WoRMS distribution
           # locality it was derived from -- the actual evidence behind the
           # gazetteer signal in Occurrence_classification's rule chain
           # (round 33), previously visible only in Module F's Full Table.
           "Occurrence_hops_to_reference_region", "Occurrence_nearest_locality",
           "Observed_in_OBIS", "Observed_in_GBIF",
           # Observed_in_OBIS_Checked/Observed_in_GBIF_Checked (round 36
           # follow-up, 2026-09-29, per direct user request): the round-29
           # flag distinguishing "genuinely checked and absent" from "query
           # never succeeded" -- without this, a reviewer can't verify
           # whether a double-negative-driven tier (round 33 rules 0/6) was
           # based on a real absence or an unchecked query.
           "Observed_in_OBIS_Checked", "Observed_in_GBIF_Checked",
           "OBIS_Occurrence_Lat", "OBIS_Occurrence_Lon",
           "GBIF_Occurrence_Lat", "GBIF_Occurrence_Lon",
           "OBIS_Matched_Name", "GBIF_Matched_Name",
           "Group1", "Group2", "TaxGroup", "FG_source",
           "DB_Group1", "DB_Group2", "DB_TaxGroup",
           "Group1_Validated", "Group2_Validated", "TaxGroup_Validated",
           # Trait_Override_Applied/Trait_Override_Reason (Round 51,
           # 2026-10-02): surfaces the species-specific trait expert
           # override mechanism's effect (if any), mirroring
           # Occurrence_Override_Applied/Occurrence_Override_Reason above.
           "Trait_Override_Applied", "Trait_Override_Reason"
        )
        
        # Filter for unique columns and ensure they exist in df_with_worms
        # -- rv$harmonized_taxonomy remains a simplified on-screen preview
        # (the "Harmonized Taxonomy" tab below), not what downstream modules
        # receive; see the Round 47 note at shared$modF_data below.
        harmonized_cols <- intersect(unique(cols_to_keep), names(df_with_worms))
        rv$harmonized_taxonomy <- df_with_worms %>% select(all_of(harmonized_cols))

        # Round 47 (2026-10-01): Module F now publishes ONE general output —
        # the full enriched table (every column, every taxon, nothing
        # curated down) — as shared$modF_data, and that single object is
        # also exactly what Module G (Quality Check) reads as its input and
        # re-exports (plus Unvalidated_Reason) as its one "validated_table"
        # download. Previously this line published the trimmed
        # rv$harmonized_taxonomy (a ~60-column whitelist) instead, so any
        # column added to the full table but not also added by hand to that
        # whitelist silently never reached Module G at all -- the root
        # cause of the modF/modG "species_level" exports diverging and of
        # several newer columns (WoRMS_attributes, Occurrence_Override_*,
        # Occurrence_Single_DB_Only, etc.) only reaching Module G by
        # accident, via a different bug in its own sample-column detection
        # (see mod_g_validation.R). One general output removes the
        # whitelist entirely: there is nothing left to fall out of sync.
        shared$modF_data <- df_with_worms %>% select(-row_idx)

        # Round 47 (2026-10-01): recorded independently of shared$modF_data
        # itself, so Module G can cross-check "the taxon count I actually
        # received" against "the taxon count Module F actually produced" as
        # two separately-computed numbers, rather than trusting that the
        # same data object couldn't have been altered in between.
        shared$modF_taxon_count <- nrow(df_with_worms)

        rv$status <- list(type = "success", msg = "WoRMS enrichment complete!")

        wd_log <- rv$worms_df
        occ_tb <- if ("Occurrence_classification" %in% names(df_with_worms)) {
          table(df_with_worms$Occurrence_classification, useNA = "ifany")
        } else NULL
        occ_notes <- if (!is.null(occ_tb) && length(occ_tb) > 0) {
          sprintf("  %-15s: %5d (%.1f%%)", names(occ_tb), as.integer(occ_tb),
                   pct(as.integer(occ_tb), nrow(df_with_worms)))
        } else character(0)

        log_paths <- write_module_log(
          project_id = shared$project_name,
          module_id = "modF",
          module_title = "Module F -- Species Ecological Enrichment (WoRMS)",
          summary = list(
            "Reference region"                = sprintf("%s (MRGID %d)", rv$reference_name, rv$reference_mrgid),
            "Taxa in"                          = nrow(df_input),
            "Taxa in full table"               = nrow(rv$df_with_worms),
            "Taxa in marine-only table"        = nrow(rv$df_marine_only),
            "Taxa in harmonized table"         = nrow(rv$harmonized_taxonomy),
            "Species with distribution data"   = if (!is.null(wd_log)) sum(!is.na(wd_log$WoRMS_Distribution)) else NA,
            "Species with trait data"          = if (!is.null(wd_log)) sum(!is.na(wd_log$WoRMS_attributes)) else NA,
            "WoRMS rate-limit events this run" = worms_rate_limit_events,
            # Round 51 (2026-10-02): counts rows where the WoRMS synonym
            # fallback (global.R, safe_worms_batch_query_complete()) actually
            # rescued distribution/attribute/vernacular data that the row's
            # own accepted AphiaID didn't have on file -- see that function's
            # own comment for the Maea johnstoni case this was built for.
            "WoRMS synonym fallback uses this run" = if ("WoRMS_Synonym_Fallback_Used" %in% names(rv$df_with_worms)) sum(rv$df_with_worms$WoRMS_Synonym_Fallback_Used %in% TRUE) else NA,
            "WoRMS parent-species fallback uses this run" = if ("WoRMS_Parent_Fallback_Used" %in% names(rv$df_with_worms)) sum(rv$df_with_worms$WoRMS_Parent_Fallback_Used %in% TRUE) else NA,
            "Observed in OBIS"                 = if ("Observed_in_OBIS" %in% names(rv$df_with_worms)) sum(rv$df_with_worms$Observed_in_OBIS %in% TRUE) else NA,
            "Observed in GBIF"                 = if ("Observed_in_GBIF" %in% names(rv$df_with_worms)) sum(rv$df_with_worms$Observed_in_GBIF %in% TRUE) else NA,
            "Observed in both OBIS + GBIF"     = if (all(c("Observed_in_OBIS", "Observed_in_GBIF") %in% names(rv$df_with_worms))) sum(rv$df_with_worms$Observed_in_OBIS %in% TRUE & rv$df_with_worms$Observed_in_GBIF %in% TRUE) else NA
          ),
          notes = c("Occurrence classification breakdown:", occ_notes)
        )
        rv$log_txt_path <- log_paths$txt
        rv$log_csv_path <- log_paths$csv

        message("WoRMS enrichment complete.")
      })
    })

    # --- Outputs ---
    output$summary_text <- renderPrint({
      req(rv$df_with_worms)
      cat("=== WoRMS ENRICHMENT SUMMARY ===\n\n")
      cat(sprintf("Reference region: %s (MRGID %d)\n", rv$reference_name, rv$reference_mrgid))
      cat(sprintf("Total taxa in full table:    %d\n", nrow(rv$df_with_worms)))
      cat(sprintf("Taxa in marine-only table:   %d (%.1f%%)\n",
                  nrow(rv$df_marine_only), pct(nrow(rv$df_marine_only), nrow(rv$df_with_worms))))
      cat(sprintf("Taxa in harmonized table:    %d (%.1f%%)\n",
                  nrow(rv$harmonized_taxonomy), pct(nrow(rv$harmonized_taxonomy), nrow(rv$df_with_worms))))
      cat("(Kingdom/Class exclusions above are applied to ALL output formats;\n")
      cat(" the marine-only filter is layered on top of the full table, kept\n")
      cat(" alongside it rather than instead of it.)\n\n")
      cat("\nData coverage (unique species):\n")
      wd <- rv$worms_df
      cat(sprintf("  Distribution data:      %d (%.1f%%)\n", sum(!is.na(wd$WoRMS_Distribution)), pct(sum(!is.na(wd$WoRMS_Distribution)), nrow(wd))))
      cat(sprintf("  Trait data:             %d (%.1f%%)\n", sum(!is.na(wd$WoRMS_attributes)), pct(sum(!is.na(wd$WoRMS_attributes)), nrow(wd))))
      vern_cols <- grep("^WoRMS_vernacular_", names(wd), value = TRUE)
      has_vernacular <- if(length(vern_cols) > 0) rowSums(!is.na(wd[, vern_cols, drop=FALSE])) > 0 else rep(FALSE, nrow(wd))
      cat(sprintf("  Vernacular names:       %d (%.1f%%)\n", sum(has_vernacular), pct(sum(has_vernacular), nrow(wd))))
      cat("\nEmpirical validation (OBIS/GBIF within reference bbox):\n")
      if ("Observed_in_OBIS" %in% names(rv$df_with_worms)) {
        obis_true <- sum(rv$df_with_worms$Observed_in_OBIS %in% TRUE)
        gbif_true <- sum(rv$df_with_worms$Observed_in_GBIF %in% TRUE)
        both_true <- sum(rv$df_with_worms$Observed_in_OBIS %in% TRUE & rv$df_with_worms$Observed_in_GBIF %in% TRUE)
        n_obs <- nrow(rv$df_with_worms)
        cat(sprintf("  Observed in OBIS:       %d (%.1f%%)\n", obis_true, pct(obis_true, n_obs)))
        cat(sprintf("  Observed in GBIF:       %d (%.1f%%)\n", gbif_true, pct(gbif_true, n_obs)))
        cat(sprintf("  Both OBIS + GBIF:       %d (%.1f%%)\n", both_true, pct(both_true, n_obs)))
      }
      method_label <- if (isTRUE(input$classification_method == "evidence")) {
        "OBIS/GBIF + text evidence"
      } else {
        "text keyword matching"
      }
      cat(sprintf("\nOccurrence classification (%s relative to %s):\n", method_label, rv$reference_name))
      tb <- table(rv$df_with_worms$Occurrence_classification, useNA = "ifany")
      for (nm in names(tb)) cat(sprintf("  %-15s: %5d (%.1f%%)\n", nm, tb[nm], pct(tb[nm], nrow(rv$df_with_worms))))
      # Note (Round 39): Marine Regions gazetteer hop-distance no longer
      # contributes to classification at all, so Occurrence_hops_to_reference_region
      # is always NA now -- the old "N/M taxa had a resolvable gazetteer path"
      # line that used to print here was removed as permanently misleading
      # (it would always read "0/M" post-Round-39, wrongly implying gazetteer
      # classification was still being attempted). See CHANGELOG.md v38.
    })

    output$full_table <- renderDT({
      req(rv$df_with_worms)
      datatable(rv$df_with_worms %>% select(-row_idx), options = list(pageLength = 8, scrollX = TRUE))
    })

    output$marine_table <- renderDT({
      req(rv$df_marine_only)
      datatable(rv$df_marine_only %>% select(-row_idx), options = list(pageLength = 8, scrollX = TRUE))
    })
    
    output$harmonized_table <- renderDT({
      req(rv$harmonized_taxonomy)
      datatable(rv$harmonized_taxonomy, options = list(pageLength = 8, scrollX = TRUE))
    })

    output$occurrence_plot <- renderPlot({
      req(rv$df_with_worms)
      tb <- rv$df_with_worms %>%
        count(Occurrence_classification) %>%
        mutate(Occurrence_classification = factor(
          Occurrence_classification,
          levels = rev(occurrence_levels_ordered)
        ))

      ggplot(tb, aes(x = Occurrence_classification, y = n, fill = Occurrence_classification)) +
        geom_col() +
        geom_text(aes(label = n), hjust = -0.2, size = 3.6) +
        coord_flip() +
        scale_fill_viridis_d(option = "viridis", direction = -1, guide = "none") +
        theme_minimal(base_size = 13) +
        labs(title = "Occurrence likelihood — full output table",
             subtitle = sprintf("OBIS/GBIF + text evidence relative to %s; ordered most to least likely", rv$reference_name),
             x = NULL, y = "Number of taxa") +
        expand_limits(y = max(tb$n, 1) * 1.15)
    })

    output$dl_full <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modF", "FULL"),
      content = function(file) { req(rv$df_with_worms); write_csv(rv$df_with_worms %>% select(-row_idx), file) }
    )
    output$dl_marine <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modF", "marine_only"),
      content = function(file) { req(rv$df_marine_only); write_csv(rv$df_marine_only %>% select(-row_idx), file) }
    )
    output$dl_traits_single <- downloadHandler(
      # Keyed on Taxon_id (2026-09-11), not row_idx -- Taxon_id is the join
      # key used everywhere else in the pipeline from Module 5 onward.
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modF", "traits_single_column"),
      content = function(file) { req(rv$df_with_worms); write_csv(rv$df_with_worms %>% select(Taxon_id, TaxGroup) %>% rename(Traits = TaxGroup), file) }
    )
    output$dl_traits_wide <- downloadHandler(
      # FIX (2026-09-11): this used to call pivot_longer() here, which produces
      # a LONG table (two rows per taxon, Trait_Type/Value columns) -- the
      # opposite of what "wide format" means and what the button's own label
      # promises. Wide format is one row per taxon with each trait as its own
      # column, which is just the plain select() below with no pivot at all.
      # Keyed on Taxon_id (2026-09-11), not row_idx -- same reasoning as
      # dl_traits_single above.
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modF", "traits_wide_format"),
      content = function(file) { req(rv$df_with_worms); write_csv(rv$df_with_worms %>% select(Taxon_id, Group1, Group2), file) }
    )
    # Round 47 (2026-10-01): dl_species/dl_genus removed. They were
    # rank-filtered views of this same rv$df_with_worms table under the
    # same "species_level"/"genus_level" names Module G also used for its
    # own, slightly-different-columned filtered exports of the same
    # underlying data -- a redundancy that was also the original source of
    # the "the outputs in terms of number species still do not reconcile
    # well" confusion. Module G's single "Download Validated Table" export
    # is now a strict superset covering every taxon at every rank (see
    # mod_g_validation.R); anyone who wants species-only or genus-only rows
    # can filter that one file on WoRMS_Rank/Fallback_rank.

    output$dl_log_txt <- downloadHandler(
      filename = function() basename(rv$log_txt_path %||% "module_log.txt"),
      content = function(file) { req(rv$log_txt_path); file.copy(rv$log_txt_path, file) }
    )
    output$dl_log_csv <- downloadHandler(
      filename = function() basename(rv$log_csv_path %||% "module_log.csv"),
      content = function(file) { req(rv$log_csv_path); file.copy(rv$log_csv_path, file) }
    )

    invisible(rv)
  })
}
