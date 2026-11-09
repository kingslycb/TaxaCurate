# ========================================================================
# MODULE A — Taxonomy Standardization & WoRMS AphiaID Lookup
# Upload raw BLAST / BOLD / MZG files -> parse -> resolve to WoRMS AphiaIDs
# ========================================================================

mod_a_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "step-header",
        span(class = "step-number", "01"),
        div(
          h3("Taxonomic Standardization", class = "step-title"),
          p(class = "step-subtitle",
            "Upload your raw taxonomy outputs and resolve every taxon name to a WoRMS AphiaID.")
        )
    ),

    layout_columns(
      col_widths = c(4, 8),

      card(
        card_header("Inputs"),
        card_body(
          div(class = "example-callout",
              p(class = "example-callout-text",
                icon("flask"), " New to this pipeline? Load a tiny example dataset ",
                "(21 ASVs, mostly North Sea taxa, including 4 realistic lab/reagent contaminants, across BLAST, BOLD, and MZG) ",
                "to see how it runs end-to-end before uploading your real data. ",
                "A matching sample metadata file is available in Module 4 for the decontamination step."),
              actionButton(ns("load_example"), "Load example dataset", class = "btn-outline-secondary w-100",
                           icon = icon("flask"))
          ),
          hr(),
          fileInput(ns("blast_file"), "BLAST 1-hit taxonomy file (.out.taxonomy / .txt)",
                    accept = c(".txt", ".taxonomy", ".tsv", ".out")),
          fileInput(ns("bold_file"), "BOLD taxonomy file (.txt)",
                    accept = c(".txt", ".tsv")),
          fileInput(ns("mzg_file"), "MZG taxonomy file (.csv)",
                    accept = c(".csv")),
          hr(),
          tags$details(
            tags$summary("WoRMS lookup settings"),
            br(),
            actionButton(ns("check_worms_api"), "Check WoRMS API status", class = "btn-sm btn-outline-info w-100"),
            uiOutput(ns("worms_api_status")),
            br(),
            numericInput(ns("worms_delay"), "Delay between WoRMS requests (seconds)",
                         value = 0.2, min = 0, max = 2, step = 0.05),
          ),
          tags$details(
            tags$summary("Reuse a previous WoRMS cache (optional)"),
            br(),
            p(class = "text-muted small",
              "If this is the same project as a previous run, upload its saved cache file ",
              "to skip re-querying taxa you've already resolved."),
            fileInput(ns("cache_seed"), "Previous cache (.csv)", accept = ".csv")
          ),
          div(style = "margin-top: 8px; display: flex; gap: 6px;",
              actionButton(ns("run"), "Run taxonomic standardization", class = "btn-primary flex-fill",
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
            id = ns("results_tabs"),
            tabPanel("BLAST", DTOutput(ns("blast_table"))),
            tabPanel("BOLD", DTOutput(ns("bold_table"))),
            tabPanel("MZG", DTOutput(ns("mzg_table"))),
            tabPanel("Summary", verbatimTextOutput(ns("summary_text")))
          ),
          div(style = "margin-top: 12px; display: flex; gap: 8px; flex-wrap: wrap;",
              downloadButton(ns("dl_blast"), "Download BLAST AphiaID table", class = "btn-outline-secondary"),
              downloadButton(ns("dl_bold"), "Download BOLD AphiaID table", class = "btn-outline-secondary"),
              downloadButton(ns("dl_mzg"), "Download MZG AphiaID table", class = "btn-outline-secondary"),
              downloadButton(ns("dl_cache"), "Download WoRMS cache", class = "btn-outline-secondary"),
              downloadButton(ns("dl_log_txt"), "Download module log (.txt)", class = "btn-outline-secondary"),
              downloadButton(ns("dl_log_csv"), "Download module log (.csv)", class = "btn-outline-secondary")
          )
        )
      )
    )
  )
}

mod_a_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # WoRMS API Status Check
    # Status tracker
    api_status <- reactiveVal(NULL)

    output$worms_api_status <- renderUI({
      req(api_status())
      if (api_status() == "checking") {
          div(class = "text-muted small", "Checking WoRMS API...")
      } else if (api_status() == TRUE) {
          div(class = "alert alert-success mt-2 py-1 px-2", icon("circle-check"), " WoRMS API online.")
      } else {
          div(class = "alert alert-danger mt-2 py-1 px-2", icon("circle-xmark"), " WoRMS API unreachable.")
      }
    })

    observeEvent(input$check_worms_api, {
      api_status("checking")
      
      # Use a simple WoRMS endpoint: AphiaID for "Abra alba"
      url <- "https://www.marinespecies.org/rest/AphiaIDByName/Abra%20alba"
      
      res <- tryCatch({
        resp <- request(url) %>% req_timeout(5) %>% req_perform()
        if (resp_status(resp) == 200) TRUE else FALSE
      }, error = function(e) FALSE)
      
      api_status(res)
    })

    rv <- reactiveValues(
      blast_out = NULL,
      bold_out = NULL,
      mzg_out = NULL,
      # Round 61: refused-match signal per database (internal only, never in a table/export)
      blast_rejected = NULL, bold_rejected = NULL, mzg_rejected = NULL,
      mzg_raw_for_backbone = NULL,
      cache_df = NULL,
      status = NULL,
      log = character(),
      using_example = FALSE,
      cancel = FALSE,
      log_txt_path = NULL,
      log_csv_path = NULL
    )

    add_log <- function(msg) {
      rv$log <- c(rv$log, msg)
    }

    output$status_box <- renderUI({
      if (is.null(rv$status)) return(NULL)
      cls <- switch(rv$status$type, info = "alert-info", success = "alert-success",
                    error = "alert-danger", "alert-secondary")
      div(class = paste("alert", cls), role = "alert", rv$status$msg)
    })

    # Example files are bundled with the app under examples/. They're used
    # by load_example below, and never touched by the normal upload path —
    # input$blast_file etc. are only ever real user uploads.
    example_paths <- list(
      blast = "examples/example_BLAST.out.taxonomy",
      bold  = "examples/example_BOLD.txt",
      mzg   = "examples/example_MZG_tax_id.tsv"
    )

    # Shared core logic for both the real "Run" button and "Load example
    # dataset" button. `paths` is a list with blast/bold/mzg elements, each
    # either NULL or a file path (a real upload's $datapath, or a bundled
    # example path) — the parsing/WoRMS logic below doesn't care which.
    run_taxonomy_assignment <- function(paths, cache_seed_path = NULL, worms_delay = 0.2) {

      rv$cache_df <- NULL  # reset so a failed re-run doesn't show stale success output

      # A1 fix: clear this module's own previous outputs up front so that if
      # this run errors out early (e.g. bad input files) or is cancelled, the
      # UI/downstream code doesn't keep showing results from a prior run as if
      # they belonged to the current one.
      rv$blast_out <- NULL
      rv$bold_out  <- NULL
      rv$mzg_out   <- NULL
      rv$blast_rejected <- NULL; rv$bold_rejected <- NULL; rv$mzg_rejected <- NULL

      # H1 fix: a fresh Module A run invalidates every downstream module's
      # cached output (they were all built from the previous Module A run).
      # Without this reset, re-running Module A while leaving stale
      # shared$modB_combined / modC_agreed / etc. in place lets a user view
      # (or export) downstream results that no longer correspond to the
      # current upload - explicitly force everything back to "not yet run".
      shared$modB_combined         <- NULL
      shared$modC_agreed           <- NULL
      shared$modD_clean_no_blanks  <- NULL
      shared$modE_data             <- NULL  # Module E = Aggregation (round 4 relettering)
      shared$modE_sample_cols      <- NULL  # Round 47: canonical sample-column list, see mod_e_aggregation.R
      shared$modF_data             <- NULL  # Module F = Enrichment (round 4 relettering)
      shared$modF_taxon_count      <- NULL  # Round 47: cross-check value, see mod_f_enrichment.R
      shared$excluded_kingdoms     <- NULL
      shared$excluded_classes      <- NULL

      if (is.null(paths$blast) && is.null(paths$bold) && is.null(paths$mzg)) {
        rv$status <- list(type = "error", msg = "Please upload at least one of: BLAST, BOLD, or MZG file.")
        return(invisible(NULL))
      }

      if (isolate(rv$cancel)) {
        rv$status <- list(type = "warning", msg = "Cancelled.")
        return(invisible(NULL))
      }

      cancel_check <- function() isolate(rv$cancel)

      rv$status <- list(type = "info", msg = "Running taxonomy assignment...")
      rv$log <- character()

      taxon_cache <- new.env(parent = emptyenv())
      
      # Automatically seed from default cache if it exists
      default_cache <- file.path(cache_dir, "worms_cache.csv")
      if (file.exists(default_cache)) {
        seed_df <- tryCatch(read_csv(default_cache, show_col_types = FALSE), error = function(e) NULL)
        if (!is.null(seed_df)) {
            seed_cache_from_df(seed_df, taxon_cache)
            add_log(sprintf("Seeded cache with %d taxa from default file.", nrow(seed_df)))
        }
      }

      if (!is.null(cache_seed_path)) {
        seed_df <- tryCatch(read_csv(cache_seed_path, show_col_types = FALSE),
                             error = function(e) NULL)
        if (!is.null(seed_df)) {
          tryCatch({
            seed_cache_from_df(seed_df, taxon_cache)
            add_log(sprintf("Seeded cache with %d previously resolved taxa.", nrow(seed_df)))
          }, error = function(e) add_log(paste("Could not load cache seed:", e$message)))
        }
      }

      parsed <- list()

      withProgress(message = "Parsing input files", value = 0, {

        if (!is.null(paths$blast)) {
          incProgress(0.05, detail = "Parsing BLAST file")
          parsed$BLAST <- tryCatch(read_BLAST(paths$blast),
                                    error = function(e) {
                                      add_log(paste("BLAST parse error:", e$message)); NULL
                                    })
        }
        if (!is.null(paths$bold)) {
          incProgress(0.05, detail = "Parsing BOLD file")
          parsed$BOLD <- tryCatch(read_BOLD(paths$bold),
                                   error = function(e) {
                                     add_log(paste("BOLD parse error:", e$message)); NULL
                                   })
        }
        if (!is.null(paths$mzg)) {
          incProgress(0.05, detail = "Parsing MZG file")
          parsed$MZG <- tryCatch(read_MZG(paths$mzg, log = add_log),
                                  error = function(e) {
                                    add_log(paste("MZG parse error:", e$message)); NULL
                                  })
          # Keep the raw MZG path around — Module C needs the FULL MZG file
          # (with sample read-count columns) as the ASV backbone, not just
          # the parsed taxonomy-only version. Works the same whether this
          # is a real upload's datapath or the bundled example path.
          rv$mzg_raw_for_backbone <- paths$mzg
        }

        # Row-anchored resolution (Round 53): each dataset (BLAST/BOLD/MZG)
        # gets its own resolve_columns_batch() pass, and within that pass
        # each ROW is resolved as a unit — anchored on that row's own single
        # most-specific resolvable name (full species binomial first, then
        # genus, climbing toward Kingdom exactly as before), with the full 7-
        # column lineage filled in from that ONE matched WoRMS record, rather
        # than each of the 7 columns running its own independent ladder-climb
        # and potentially landing on 7 different records. Each dataset still
        # anchors independently of the others, so Module B's cross-dataset,
        # per-level agreement comparison (BLAST vs BOLD vs MZG at each of
        # Kingdom/Phylum/.../Species) stays meaningful — two datasets can
        # still genuinely disagree at any given rank if their own raw inputs
        # point to different organisms. Confidence tiers and homonym
        # checking (using the row's OTHER raw columns as context) are
        # unchanged, applied once per row at the anchor point instead of
        # once per column. Pass row_anchored = FALSE to resolve_columns_batch()
        # to roll back to the pre-Round-53 per-column-independent behavior.
        # Identical (level, string, context) combinations across datasets
        # still hit one shared taxon_cache entry, so repeated values aren't
        # re-queried.
        total_cells <- sum(sapply(parsed, function(d) if (is.null(d)) 0 else nrow(d) * length(tax_levels)))
        cells_done <- 0

        worms_progress_fn <- function(done, total, label) {
          cells_done <<- cells_done + 1
          incProgress(0.85 * (1 / max(total_cells, 1)),
                      detail = sprintf("Resolving taxonomy %d/%d: %s", cells_done, total_cells,
                                        substr(label, 1, 30)))
        }

        incProgress(0.05, detail = sprintf("Resolving %d taxonomic columns via WoRMS", total_cells))

        # Round 53: row_anchored = TRUE passed explicitly (it's also the
        # function's own default) so this is visible here rather than
        # implicit -- each database's own row is resolved by anchoring on
        # its single most-specific resolvable name and fanning that one
        # match's lineage out to all 7 columns, instead of resolving each
        # column independently. Set to FALSE to roll back to the
        # pre-Round-53 per-column-independent behavior.
        if (!is.null(parsed$BLAST)) {
          resolved <- resolve_columns_batch(parsed$BLAST, taxon_cache, worms_delay, worms_progress_fn, cancel_check, row_anchored = TRUE)
          if (is.null(resolved)) { rv$status <- list(type = "warning", msg = "Taxonomy assignment cancelled."); setProgress(1); return(invisible(NULL)) }
          rv$blast_out <- apply_resolution(parsed$BLAST, resolved)
          rv$blast_rejected <- extract_rejected_matches(parsed$BLAST, resolved)  # Round 61: internal only
        }
        if (isolate(rv$cancel)) { setProgress(1); return(invisible(NULL)) }
        if (!is.null(parsed$BOLD)) {
          resolved <- resolve_columns_batch(parsed$BOLD, taxon_cache, worms_delay, worms_progress_fn, cancel_check, row_anchored = TRUE)
          if (is.null(resolved)) { rv$status <- list(type = "warning", msg = "Taxonomy assignment cancelled."); setProgress(1); return(invisible(NULL)) }
          rv$bold_out <- apply_resolution(parsed$BOLD, resolved)
          rv$bold_rejected <- extract_rejected_matches(parsed$BOLD, resolved)  # Round 61: internal only
        }
        if (isolate(rv$cancel)) { setProgress(1); return(invisible(NULL)) }
        if (!is.null(parsed$MZG)) {
          resolved <- resolve_columns_batch(parsed$MZG, taxon_cache, worms_delay, worms_progress_fn, cancel_check, row_anchored = TRUE)
          if (is.null(resolved)) { rv$status <- list(type = "warning", msg = "Taxonomy assignment cancelled."); setProgress(1); return(invisible(NULL)) }
          rv$mzg_out <- apply_resolution(parsed$MZG, resolved)
          rv$mzg_rejected <- extract_rejected_matches(parsed$MZG, resolved)  # Round 61: internal only
        }

        rv$cache_df <- cache_env_to_df(taxon_cache)
        write_csv(rv$cache_df, file.path(cache_dir, "worms_cache.csv"))
        add_log("Updated WoRMS cache saved.")

        # Eagerly populate the classification cache so Module B's
        # build_species_fallback hits warm cache instead of making N new
        # wm_classification() API calls. Collect unique AphiaIDs across
        # all three resolved databases.
        eager_aphia_ids <- unique(c(
          if (!is.null(rv$blast_out)) {
            blast_aphia <- rv$blast_out %>%
              select(ends_with("_AphiaID")) %>%
              unlist(use.names = FALSE)
            blast_aphia[!is.na(blast_aphia) & blast_aphia != 0]
          },
          if (!is.null(rv$bold_out)) {
            bold_aphia <- rv$bold_out %>%
              select(ends_with("_AphiaID")) %>%
              unlist(use.names = FALSE)
            bold_aphia[!is.na(bold_aphia) & bold_aphia != 0]
          },
          if (!is.null(rv$mzg_out)) {
            mzg_aphia <- rv$mzg_out %>%
              select(ends_with("_AphiaID")) %>%
              unlist(use.names = FALSE)
            mzg_aphia[!is.na(mzg_aphia) & mzg_aphia != 0]
          }
        ))
        n_class <- length(eager_aphia_ids)
        if (n_class > 0) {
          incProgress(0.02, detail = sprintf("Pre-fetching WoRMS classifications (%d AphiaIDs)...", n_class))
          for (k in seq_along(eager_aphia_ids)) {
            if (isolate(rv$cancel)) { setProgress(1); return(invisible(NULL)) }
            cached_wm_classification(eager_aphia_ids[k])
            incProgress(0.02 / n_class, detail = sprintf("WoRMS classification %d/%d", k, n_class))
          }
        }
        save_classification_cache()
      })

      n_taxa <- nrow(rv$cache_df)
      n_resolved <- if (n_taxa > 0) sum(!is.na(rv$cache_df$AphiaID)) else 0
      example_note <- if (isTRUE(rv$using_example)) " (example dataset)" else ""
      conf_summary <- if (n_taxa > 0) {
        tb <- table(factor(rv$cache_df$confidence, levels = confidence_levels))
        paste(sprintf("%s: %d", names(tb)[tb > 0], tb[tb > 0]), collapse = ", ")
      } else ""
      rv$status <- list(
        type = "success",
        msg = sprintf("Done%s. Resolved %d / %d (level, value) lookups to a WoRMS AphiaID (%s%%). %s",
                       example_note, n_resolved, n_taxa, pct(n_resolved, n_taxa), conf_summary)
      )

      # Push outputs into the shared pipeline state for downstream modules
      shared$modA_blast <- rv$blast_out
      shared$modA_bold  <- rv$bold_out
      shared$modA_mzg   <- rv$mzg_out
      # Round 61: refused-match signal for Module B's agreement statistics only
      shared$modA_blast_rejected <- rv$blast_rejected
      shared$modA_bold_rejected  <- rv$bold_rejected
      shared$modA_mzg_rejected   <- rv$mzg_rejected
      shared$modA_mzg_backbone_path <- rv$mzg_raw_for_backbone

      log_paths <- write_module_log(
        project_id = shared$project_name,
        module_id = "modA",
        module_title = "Module A -- Taxonomic Standardization",
        summary = list(
          "Using example dataset"          = isTRUE(rv$using_example),
          "ASVs in (BLAST)"                 = if (!is.null(rv$blast_out)) nrow(rv$blast_out) else NA,
          "ASVs in (BOLD)"                  = if (!is.null(rv$bold_out))  nrow(rv$bold_out)  else NA,
          "ASVs in (MZG)"                   = if (!is.null(rv$mzg_out))   nrow(rv$mzg_out)   else NA,
          "Total (level, value) lookups"    = n_taxa,
          "Resolved to a WoRMS AphiaID"     = n_resolved,
          "Resolution rate"                 = sprintf("%s%%", pct(n_resolved, n_taxa)),
          "Confidence tier breakdown"       = conf_summary
        ),
        notes = rv$log
      )
      rv$log_txt_path <- log_paths$txt
      rv$log_csv_path <- log_paths$csv
    }

    observeEvent(input$cancel, { rv$cancel <- TRUE })

    observeEvent(input$run, {
      shinyjs::disable("run")
      on.exit(shinyjs::enable("run"))
      if (!check_api_reachable(REQUIRED_APIS$WoRMS$url)) {
        rv$status <- list(type = "error", msg = "WoRMS API is unreachable. Taxonomy assignment cannot proceed. Check your internet/proxy connection and try again.")
        showNotification("WoRMS API unreachable — aborting.", type = "error", duration = 10)
        add_log("ERROR: WoRMS API unreachable — taxonomy assignment aborted.")
        return(invisible(NULL))
      }
      rv$using_example <- FALSE
      rv$cancel <- FALSE
      cache_seed_path <- if (!is.null(input$cache_seed)) input$cache_seed$datapath else NULL
      # Extract common prefix from all uploaded files
      files <- list(blast = input$blast_file, bold = input$bold_file, mzg = input$mzg_file)
      valid_files <- files[!sapply(files, is.null)]
      
      # Strip ALL file extensions first
      file_names <- sapply(valid_files, function(f) {
          # Repeatedly remove extensions (everything after the last dot)
          fn <- f$name
          while(grepl("\\.", fn)) {
              fn <- sub("\\.[^.]+$", "", fn)
          }
          fn
      })
      
      # Find the longest common substring across all filenames (min length 3)
      get_longest_common_substring <- function(strs) {
        if (length(strs) == 0) return("")
        # Use the shortest filename as the basis to find commonality
        s1 <- strs[which.min(nchar(strs))]
        len <- nchar(s1)
        for (l in len:3) {
          for (i in 1:(len - l + 1)) {
            sub <- substr(s1, i, i + l - 1)
            if (all(sapply(strs, function(s) grepl(sub, s, fixed = TRUE)))) return(sub)
          }
        }
        return("Project")
      }
      
      project_name <- get_longest_common_substring(file_names)
      
      # Clean up trailing/leading separators
      project_name <- str_remove(project_name, "^[_\\-\\.]+")
      project_name <- str_remove(project_name, "[_\\-\\.]+$")
      
      shared$project_name <- project_name

      run_taxonomy_assignment(
        paths = list(
          blast = if (!is.null(input$blast_file)) input$blast_file$datapath else NULL,
          bold  = if (!is.null(input$bold_file))  input$bold_file$datapath  else NULL,
          mzg   = if (!is.null(input$mzg_file))   input$mzg_file$datapath   else NULL
        ),
        cache_seed_path = cache_seed_path,
        worms_delay = input$worms_delay
      )
    })

    observeEvent(input$load_example, {
      shinyjs::disable("load_example")
      if (!check_api_reachable(REQUIRED_APIS$WoRMS$url)) {
        shinyjs::enable("load_example")
        rv$status <- list(type = "error", msg = "WoRMS API is unreachable. Cannot load example dataset. Check your internet/proxy connection and try again.")
        showNotification("WoRMS API unreachable — aborting.", type = "error", duration = 10)
        add_log("ERROR: WoRMS API unreachable — example dataset load aborted.")
        return(invisible(NULL))
      }
      rv$using_example <- TRUE
      missing_files <- example_paths[!sapply(example_paths, file.exists)]
      if (length(missing_files) > 0) {
        shinyjs::enable("load_example")
        rv$status <- list(type = "error",
                           msg = paste("Example file(s) not found in the app bundle:",
                                       paste(unlist(missing_files), collapse = ", ")))
        return(invisible(NULL))
      }
      run_taxonomy_assignment(
        paths = example_paths,
        cache_seed_path = NULL,
        worms_delay = input$worms_delay
      )
    })

    output$blast_table <- renderDT({ req(rv$blast_out); datatable(rv$blast_out, options = list(pageLength = 8, scrollX = TRUE)) })
    output$bold_table  <- renderDT({ req(rv$bold_out);  datatable(rv$bold_out,  options = list(pageLength = 8, scrollX = TRUE)) })
    output$mzg_table   <- renderDT({ req(rv$mzg_out);   datatable(rv$mzg_out,   options = list(pageLength = 8, scrollX = TRUE)) })

    output$summary_text <- renderPrint({
      req(rv$cache_df)
      cat("=== TAXONOMY ASSIGNMENT SUMMARY ===\n\n")
      cat("Each of the 7 taxonomic columns (Kingdom..Species) is resolved\n")
      cat("INDEPENDENTLY against WoRMS, with its own confidence tier, its own\n")
      cat("fallback chain (climbing this row's own higher-rank columns up to\n")
      cat("Kingdom if needed), and a homonym check against this row's OTHER\n")
      cat("columns (e.g. resolving Genus checks candidates against this row's\n")
      cat("own Kingdom/Phylum, since some genus names are shared across\n")
      cat("unrelated kingdoms — e.g. Morus the mulberry vs. Morus the seabird).\n\n")

      for (nm in c("BLAST", "BOLD", "MZG")) {
        df <- switch(nm, BLAST = rv$blast_out, BOLD = rv$bold_out, MZG = rv$mzg_out)
        if (is.null(df)) next
        n <- nrow(df)
        cat(sprintf("--- %s (%d ASVs) ---\n", nm, n))

        for (lvl in tax_levels) {
          aphia_col <- paste0(lvl, "_AphiaID")
          conf_col <- paste0(lvl, "_confidence")
          agree_col <- paste0(lvl, "_context_agreed")
          rejected_col <- paste0(lvl, "_Rejected_AphiaID")
          rej_tbl <- switch(nm, BLAST = rv$blast_rejected, BOLD = rv$bold_rejected, MZG = rv$mzg_rejected)  # Round 61: internal table
          if (!aphia_col %in% names(df)) next

          n_with_input <- sum(!is_blank_taxon(df[[lvl]]))
          n_match <- sum(!is.na(df[[aphia_col]]))
          if (n_with_input == 0) next

          cat(sprintf("  %-10s: %4d/%4d resolved (%5.1f%%)\n", lvl, n_match, n_with_input,
                      pct(n_match, n_with_input)))

          if (conf_col %in% names(df)) {
            tb <- table(factor(df[[conf_col]], levels = confidence_levels))
            tb <- tb[tb > 0]
            if (length(tb) > 0) {
              cat("              ", paste(sprintf("%s=%d", names(tb), tb), collapse = "  "), "\n")
            }
          }
          # Round 55: context_agreed is never FALSE in the exported result
          # any more — since Round 52c a context-disagreeing match is
          # refused outright rather than accepted-and-flagged, so this count
          # has been silently zero since Round 52c shipped. Rejected_AphiaID
          # is the real signal now: it's populated exactly when a match was
          # found and refused for context disagreement (Round 55), whether
          # or not the ladder went on to resolve this row at a coarser rank.
          if (!is.null(rej_tbl) && rejected_col %in% names(rej_tbl)) {
            n_rejected <- sum(!is.na(rej_tbl[[rejected_col]]))
            if (n_rejected > 0) {
              cat(sprintf("              ⚠ %d row(s) had a match REFUSED here for disagreeing with this row's own context — possible homonym, worth a manual look. Counted in Module B's agreement statistics but excluded from this column's own result (not written to any table).\n", n_rejected))
            }
          } else if (agree_col %in% names(df)) {
            n_disagreed <- sum(df[[agree_col]] %in% FALSE)
            if (n_disagreed > 0) {
              cat(sprintf("              ⚠ %d row(s) where the match disagreed with this row's own context — possible homonym, worth a manual look.\n", n_disagreed))
            }
          }
        }
        cat("\n")
      }
      if (length(rv$log) > 0) {
        cat("--- Notes ---\n")
        cat(paste(rv$log, collapse = "\n"))
      }
    })

    output$dl_blast <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modA", "BLAST_AphiaID"),
      content = function(file) { req(rv$blast_out); write_csv(rv$blast_out, file) }
    )
    output$dl_bold <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modA", "BOLD_AphiaID"),
      content = function(file) { req(rv$bold_out); write_csv(rv$bold_out, file) }
    )
    output$dl_mzg <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modA", "MZG_AphiaID"),
      content = function(file) { req(rv$mzg_out); write_csv(rv$mzg_out, file) }
    )
    output$dl_cache <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modA", "worms_cache"),
      content = function(file) { req(rv$cache_df); write_csv(rv$cache_df, file) }
    )
    output$dl_log_txt <- downloadHandler(
      filename = function() { req(rv$log_txt_path); basename(rv$log_txt_path) },
      content = function(file) { req(rv$log_txt_path); file.copy(rv$log_txt_path, file, overwrite = TRUE) }
    )
    output$dl_log_csv <- downloadHandler(
      filename = function() { req(rv$log_csv_path); basename(rv$log_csv_path) },
      content = function(file) { req(rv$log_csv_path); file.copy(rv$log_csv_path, file, overwrite = TRUE) }
    )

    invisible(rv)
  })
}
