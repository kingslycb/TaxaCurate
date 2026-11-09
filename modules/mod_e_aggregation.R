
# ========================================================================
# MODULE E — Taxon Aggregation
# Collapses ASVs sharing the same resolved consensus taxon into one row,
# summing sample counts, BEFORE ecological enrichment runs (round 4,
# 2026-09-09: moved ahead of Module E per user request).
#
# Grouping key: each ASV's own resolved consensus identity — Species_fallback,
# the AphiaID Module B settled on for that ASV at whatever rank its own
# cross-database consensus actually reached (Fallback_rank). This is NOT
# "collapse everything to species" — most ASVs never even reach species-level
# consensus. Two ASVs merge only if they share that same AphiaID, regardless
# of which rank it sits at: an ASV resolved to species stays distinct from
# another ASV of the same genus that only resolved to genus level, and an ASV
# that only resolved to Family merges only with other ASVs that resolved to
# that same Family AphiaID. ASVs with no usable consensus AphiaID are never
# merged with anything (see run_taxon_aggregation() in global.R).
#
# Why here, before enrichment: species/genus/family identity is established
# in Modules A/B and survives Decontamination unchanged — Module E doesn't
# create that identity, it only looks it up. So collapsing ASVs that already
# share a resolved taxon loses nothing, and means Module E does one
# WoRMS/OBIS/GBIF lookup per resolved taxon instead of one per ASV.
#
# Round 4 follow-up (2026-09-09): reformatted this module's UI to match the
# layout_columns(4, 8) two-card ("Inputs & settings" / "Status & results")
# + tabsetPanel + input_status/status_box pattern every other module uses —
# previously this was a flat, ad hoc tagList that looked out of place next
# to the rest of the pipeline. No functional changes in this pass.
# ========================================================================

mod_e_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "step-header",
        span(class = "step-number", "05"),
        div(
          h3("Taxon Aggregation", class = "step-title"),
          p(class = "step-subtitle",
            "Collapse ASVs that share the same resolved consensus taxon into one row each, ",
            "summing sample counts, before ecological enrichment runs.")
        )
    ),

    layout_columns(
      col_widths = c(4, 8),

      card(
        card_header("Inputs & settings"),
        card_body(
          uiOutput(ns("input_status")),
          p(class = "text-muted small",
            "Runs directly on this session's Module 4 (Decontamination) output by ",
            "default — no upload needed when working through the pipeline in order. ",
            "You can also (optionally) upload one or more previously-exported clean ",
            "ASV tables (CSV/TSV) instead, e.g. to combine several separate runs. ASVs are ",
            "grouped by their own resolved consensus AphiaID (Species_fallback) — whatever ",
            "rank that ASV's cross-database consensus actually reached, per Module 2's ",
            "Fallback_rank — not by a single rank chosen for the whole table. Two ASVs ",
            "merge only if they share that same AphiaID; taxonomy and metadata are resolved ",
            "across merged ASVs by majority vote. ASVs with no resolved consensus AphiaID ",
            "are never merged with anything, since there is nothing to safely merge on. ",
            "If both an upload and a live Module 4 result are present, the uploaded ",
            "file(s) take priority."
          ),
          fileInput(ns("files"), "Optional: select clean ASV tables (CSV/TSV) instead of using this session's Module 4 output",
                    multiple = TRUE, accept = c(".csv", ".tsv", ".txt")),
          textInput(ns("exclude_kingdoms"), "Exclude kingdoms (comma-separated)",
                    value = "Protozoa, Bacteria, Fungi, Plantae, Chromista, Viruses"),
          textInput(ns("exclude_classes"), "Exclude classes (comma-separated)",
                    value = "Arachnida, Hexapoda, Insecta"),
          div(style = "margin-top: 8px; display: flex; gap: 6px;",
              actionButton(ns("run"), "Aggregate", class = "btn-primary flex-fill",
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
            tabPanel("Summary", verbatimTextOutput(ns("summary"))),
            tabPanel("Aggregated table", DTOutput(ns("table_preview"))),
            tabPanel("Taxonomy conflicts", DTOutput(ns("conflict_table")))
          ),
          div(style = "margin-top: 12px; display: flex; gap: 8px; flex-wrap: wrap;",
              downloadButton(ns("dl_table"), "Download aggregated table", class = "btn-outline-secondary"),
              downloadButton(ns("dl_conflicts"), "Download conflicts table", class = "btn-outline-secondary"),
              downloadButton(ns("dl_log_txt"), "Download module log (.txt)", class = "btn-outline-secondary"),
              downloadButton(ns("dl_log_csv"), "Download module log (.csv)", class = "btn-outline-secondary")
          )
        )
      )
    )
  )
}

mod_e_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(cancel = FALSE, status = NULL)
    agg <- reactiveValues(data = NULL, log_txt_path = NULL, log_csv_path = NULL)

    output$input_status <- renderUI({
      have_clean <- !is.null(shared$modD_clean_no_blanks)
      row <- function(label, ok) {
        tags$div(class = if (ok) "input-ok" else "input-missing",
                 icon(if (ok) "circle-check" else "circle-minus"), " ", label)
      }
      tagList(
        p(class = "text-muted small", "Input carried over from Module 4:"),
        row("Clean ASV table (no blanks/contaminants)", have_clean),
        if (!have_clean) div(class = "alert alert-warning mt-2",
                              "Run Module 4 first, or upload one or more clean ASV tables below.")
      )
    })

    output$status_box <- renderUI({
      if (is.null(rv$status)) return(NULL)
      cls <- switch(rv$status$type, info = "alert-info", success = "alert-success",
                    error = "alert-danger", warning = "alert-warning", "alert-secondary")
      div(class = paste("alert", cls), role = "alert", rv$status$msg)
    })

    observeEvent(input$cancel, { rv$cancel <- TRUE })

    observeEvent(input$run, {
      # Moved ahead of Enrichment per user request (2026-09-09): this module
      # now runs directly on the live Module 4 (Decontamination) output by
      # default, collapsing ASVs that share the same resolved consensus
      # taxon BEFORE enrichment runs (see the module-level comment above). A
      # manual file upload is still supported (e.g. combining several
      # separate exported runs, or offline use outside the live pipeline)
      # and takes priority when both are present.
      shinyjs::disable("run")
      on.exit(shinyjs::enable("run"))

      rv$cancel <- FALSE

      has_upload <- !is.null(input$files) && nrow(input$files) > 0
      if (!has_upload && is.null(shared$modD_clean_no_blanks)) {
        rv$status <- list(type = "error",
                           msg = "Nothing to aggregate: upload one or more clean ASV tables, or run Module 4 (Decontamination) first in this session.")
        showNotification(
          "Nothing to aggregate: upload one or more clean ASV tables, or run Module 4 (Decontamination) first in this session.",
          type = "error", duration = 10
        )
        return(invisible(NULL))
      }

      if (isolate(rv$cancel)) { rv$status <- list(type = "warning", msg = "Cancelled."); return(invisible(NULL)) }
      rv$status <- list(type = "info", msg = "Aggregating...")

      isolate({
        exc_kin <- str_split(input$exclude_kingdoms %||% "", ",") %>% map(trimws) %>% unlist()
        exc_kin <- exc_kin[nzchar(exc_kin)]
        exc_cla <- str_split(input$exclude_classes %||% "", ",") %>% map(trimws) %>% unlist()
        exc_cla <- exc_cla[nzchar(exc_cla)]

        raw <- list()
        if (has_upload) {
          files <- input$files
          nf <- nrow(files)
          for (i in seq_len(nf)) {
            raw[[i]] <- fread(files$datapath[i], sep = "auto",
                        na.strings = c("", "NA", "N/A", "NULL"),
                        strip.white = TRUE, header = TRUE,
                        encoding = "UTF-8", check.names = FALSE) |>
              as_tibble()
          }
        } else {
          # No manual upload — aggregate this session's live Module 4 output.
          raw[[1]] <- shared$modD_clean_no_blanks
        }

        result <- run_taxon_aggregation(raw, exclude_kingdoms = exc_kin, exclude_classes = exc_cla)
        if (is.null(result)) {
          rv$status <- list(type = "error", msg = "Aggregation failed — see notification for details.")
          return(invisible(NULL))
        }

        agg$data <- result$resolved
        agg$tax_map <- result$tax_map
        agg$sample_cols <- result$sample_cols
        agg$total_reads <- result$total_reads
        agg$flagged_blanks <- result$flagged_blanks

        # Invalidate downstream shared state: a fresh Aggregation run means
        # any previously enriched/validated results no longer correspond to
        # the current aggregated table.
        shared$modF_data <- NULL
        shared$modF_taxon_count <- NULL  # Round 47: cross-check value, see mod_f_enrichment.R

        # Publish the aggregated table to shared state so Module F
        # (Enrichment), which now runs AFTER this module, enriches each
        # already-merged taxon once instead of enriching every raw ASV.
        shared$modE_data <- result$resolved

        # Round 47 (2026-10-01): publish the canonical sample-column list
        # alongside the data, so Module F and Module G can both identify
        # "which columns are per-sample abundance data" from this ONE
        # authoritative source instead of each re-deriving its own guess.
        # Before this, mod_f_enrichment.R and mod_g_validation.R each kept
        # their own independent exclude-pattern blacklist to separate
        # sample columns from metadata columns -- and neither blacklist was
        # updated as new metadata columns were added over Rounds 22f-46b
        # (WoRMS_attributes, WoRMS_URL, WoRMS_FunctionalGroup,
        # WoRMS_vernacular_*, Occurrence_Override_Applied/Reason,
        # Occurrence_Single_DB_Only, Occurrence_hops_to_reference_region,
        # Occurrence_nearest_locality, IsBrackish/IsFreshwater/IsTerrestrial
        # all silently fell through Module G's blacklist and got exported
        # as if they were per-sample abundance columns). Reusing this one
        # list everywhere closes that entire class of bug permanently,
        # since there is now only one place to ever update.
        shared$modE_sample_cols <- result$sample_cols

        n_conf <- if ("taxonomy_conflict" %in% names(agg$data)) sum(agg$data$taxonomy_conflict, na.rm = TRUE) else 0
        rv$status <- list(type = "success",
                           msg = sprintf("Done. Aggregated to %d resolved taxa%s.",
                                         nrow(agg$data),
                                         if (n_conf > 0) sprintf(" (%d taxonomy conflict%s flagged)", n_conf, if (n_conf == 1) "" else "s") else ""))

        n_asvs_in <- sum(vapply(raw, nrow, integer(1)))
        merge_note <- if ("n_asvs" %in% names(agg$data)) {
          sprintf("ASVs merged per row: min %d, median %.0f, max %d",
                  min(agg$data$n_asvs), median(agg$data$n_asvs), max(agg$data$n_asvs))
        } else character(0)

        log_paths <- write_module_log(
          project_id = shared$project_name,
          module_id = "modE",
          module_title = "Module E -- Taxon Aggregation",
          summary = list(
            "Files loaded"                     = if (has_upload) nrow(input$files) else 1,
            "ASVs in"                           = n_asvs_in,
            "Resolved taxa out (aggregated rows)" = nrow(agg$data),
            "Taxonomy conflicts flagged"        = n_conf,
            "Samples"                           = length(agg$sample_cols),
            "Total reads"                       = agg$total_reads,
            "Blank/zero-read columns flagged"   = agg$flagged_blanks
          ),
          notes = merge_note
        )
        agg$log_txt_path <- log_paths$txt
        agg$log_csv_path <- log_paths$csv
      })
    })

    output$summary <- renderPrint({
      req(agg$data)
      cat(sprintf("Files loaded: %d\n", nrow(input$files)))
      cat(sprintf("Resolved taxa (rows after aggregation): %d\n", nrow(agg$data)))
      cat(sprintf("Samples: %d\n", length(agg$sample_cols)))
      cat(sprintf("Total reads: %s\n", format(agg$total_reads, big.mark = ",")))
      if ("n_asvs" %in% names(agg$data)) {
        cat(sprintf("ASVs merged per row: min %d, median %.0f, max %d\n",
                    min(agg$data$n_asvs), median(agg$data$n_asvs), max(agg$data$n_asvs)))
      }
      if (length(agg$flagged_blanks) > 0) {
        cat(sprintf("WARNING — blank/zero-read sample column(s) present: %s\n",
                    paste(agg$flagged_blanks, collapse = ", ")))
      } else {
        cat("Blank sample check: no blank/control or zero-read sample columns found\n")
      }
      if (!is.null(agg$tax_map) && "Kingdom" %in% names(agg$tax_map)) {
        kcol <- agg$tax_map[["Kingdom"]]
        if (kcol %in% names(agg$data)) {
          counts <- table(agg$data[[kcol]], useNA = "ifany")
          cat("Kingdoms:", paste(sprintf("%s (%d)", names(counts), counts), collapse = ", "), "\n")
        }
      }
      if (!is.null(agg$tax_map) && "Species" %in% names(agg$tax_map)) {
        scol <- agg$tax_map[["Species"]]
        if (scol %in% names(agg$data)) {
          n_species <- sum(!is.na(agg$data[[scol]]))
          cat(sprintf("Rows resolved to species level: %d / %d\n", n_species, nrow(agg$data)))
        }
      }
      if ("taxonomy_conflict" %in% names(agg$data)) {
        n_conf <- sum(agg$data$taxonomy_conflict, na.rm = TRUE)
        if (n_conf > 0) cat(sprintf("Taxonomy conflicts: %d\n", n_conf))
      }
    })

    output$table_preview <- renderDT({
      req(agg$data)
      datatable(agg$data, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$dl_table <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modE", "aggregated_table"),
      content = function(file) { req(agg$data); write_csv(agg$data, file) }
    )

    output$conflict_table <- renderDT({
      req(agg$data)
      conflicts <- if ("taxonomy_conflict" %in% names(agg$data)) {
        agg$data[agg$data$taxonomy_conflict == TRUE, , drop = FALSE]
      } else {
        agg$data[0, , drop = FALSE]
      }
      datatable(conflicts, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$dl_conflicts <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modE", "taxonomy_conflicts"),
      content = function(file) {
        req(agg$data)
        conflicts <- if ("taxonomy_conflict" %in% names(agg$data)) {
          agg$data[agg$data$taxonomy_conflict == TRUE, , drop = FALSE]
        } else {
          agg$data[0, , drop = FALSE]
        }
        write_csv(conflicts, file)
      }
    )

    output$dl_log_txt <- downloadHandler(
      filename = function() { req(agg$log_txt_path); basename(agg$log_txt_path) },
      content = function(file) { req(agg$log_txt_path); file.copy(agg$log_txt_path, file, overwrite = TRUE) }
    )
    output$dl_log_csv <- downloadHandler(
      filename = function() { req(agg$log_csv_path); basename(agg$log_csv_path) },
      content = function(file) { req(agg$log_csv_path); file.copy(agg$log_csv_path, file, overwrite = TRUE) }
    )
  })
}
