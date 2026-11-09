# ========================================================================
# MODULE D — Contaminant detection & removal
# Uses decontam (prevalence method) and/or microDecon
# Requires a metadata file identifying which samples are blanks/controls
# ========================================================================

mod_d_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "step-header",
        span(class = "step-number", "04"),
        div(
          h3("Contaminant detection & removal", class = "step-title"),
          p(class = "step-subtitle",
            "Flag and remove likely contaminant ASVs using blank/control samples, then drop the blanks themselves.")
        )
    ),

    layout_columns(
      col_widths = c(4, 8),

      card(
        card_header("Inputs & settings"),
        card_body(
          uiOutput(ns("input_status")),
          fileInput(ns("metadata_file"), "Sample metadata file (.txt / .tsv)",
                    accept = c(".txt", ".tsv")),
          p(class = "text-muted small",
            "Metadata must include a sample ID column and a sample type column ",
            "(values like Blank/Control/NTC identify negative controls)."),
          div(class = "example-callout",
              p(class = "example-callout-text",
                icon("flask"), " Using the example dataset from Module 1? Load its matching metadata ",
                "(9 true samples and 4 blanks/controls, including one blank written in a spelling variant ",
                "('blanc') and one zero-read sample) instead of uploading your own."),
              actionButton(ns("use_example_metadata"), "Use example metadata", class = "btn-outline-secondary w-100",
                           icon = icon("flask"))
          ),
          hr(),
          radioButtons(ns("method"), "Detection method",
                       choices = c("decontam only" = "decontam",
                                   "microDecon only" = "microdecon",
                                   "Both — require agreement" = "both_require",
                                   "Both — either flags" = "both_either"),
                       selected = "both_require"),
          numericInput(ns("decontam_threshold"), "decontam prevalence threshold", value = 0.1, min = 0, max = 1, step = 0.05),
          div(style = "margin-top: 8px; display: flex; gap: 6px;",
              actionButton(ns("run"), "Run contaminant detection & removal", class = "btn-primary flex-fill",
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
            tabPanel("Flagged contaminants", DTOutput(ns("contam_table"))),
            tabPanel("Cleaned table (with blanks)", DTOutput(ns("clean_table"))),
            tabPanel("Cleaned table (no blanks)", DTOutput(ns("clean_noblanks_table")))
          ),
          div(style = "margin-top: 12px; display: flex; gap: 8px; flex-wrap: wrap;",
              downloadButton(ns("dl_clean"), "Download cleaned table", class = "btn-outline-secondary"),
              downloadButton(ns("dl_clean_noblanks"), "Download cleaned table (no blanks)", class = "btn-outline-secondary"),
              downloadButton(ns("dl_contaminants"), "Download contaminant list", class = "btn-outline-secondary"),
              downloadButton(ns("dl_log_txt"), "Download module log (.txt)", class = "btn-outline-secondary"),
              downloadButton(ns("dl_log_csv"), "Download module log (.csv)", class = "btn-outline-secondary")
          )
        )
      )
    )
  )
}

mod_d_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      clean = NULL, clean_no_blanks = NULL, contaminants_df = NULL,
      summary = NULL, status = NULL, metadata_path_override = NULL,
      using_example_metadata = FALSE, cancel = FALSE,
      log_txt_path = NULL, log_csv_path = NULL
    )

    example_metadata_path <- "examples/example_metadata.tsv"

    observeEvent(input$use_example_metadata, {
      if (!file.exists(example_metadata_path)) {
        rv$status <- list(type = "error",
                           msg = paste("Example metadata file not found in the app bundle:", example_metadata_path))
        return(invisible(NULL))
      }
      rv$metadata_path_override <- example_metadata_path
      rv$using_example_metadata <- TRUE
      rv$status <- list(type = "info",
                         msg = "Example metadata loaded. Click \"Run contaminant detection\" to continue.")
    })

    # Any real upload takes precedence over a previously-loaded example —
    # if the person uploads their own file after clicking the example
    # button, the real upload should win.
    observeEvent(input$metadata_file, {
      rv$metadata_path_override <- NULL
      rv$using_example_metadata <- FALSE
    })

    output$input_status <- renderUI({
      have_asv <- !is.null(shared$modC_agreed)
      row <- function(label, ok) {
        tags$div(class = if (ok) "input-ok" else "input-missing",
                 icon(if (ok) "circle-check" else "circle-minus"), " ", label)
      }
      tagList(
        p(class = "text-muted small", "Input carried over from Module 3:"),
        row("Final ASV table", have_asv),
        if (!have_asv) div(class = "alert alert-warning mt-2", "Run Module 3 first."),
        if (isTRUE(rv$using_example_metadata))
          div(class = "alert alert-info mt-2", icon("flask"), " Using bundled example metadata.")
      )
    })

    output$status_box <- renderUI({
      if (is.null(rv$status)) return(NULL)
      cls <- switch(rv$status$type, info = "alert-info", success = "alert-success",
                    error = "alert-danger", "alert-secondary")
      div(class = paste("alert", cls), role = "alert", rv$status$msg)
    })

    observeEvent(input$cancel, { rv$cancel <- TRUE })

    observeEvent(input$run, {
      shinyjs::disable("run")
      on.exit(shinyjs::enable("run"))

      rv$clean <- NULL  # reset so a failed re-run doesn't show stale success output
      rv$cancel <- FALSE

      # Invalidate downstream shared state: a fresh Decontamination run means
      # any previously aggregated/enriched/validated results no longer
      # correspond to the current cleaned ASV table. This closes a
      # pre-existing gap in the H1 invalidation pattern (previously only
      # Module A's re-run reset these) that matters more now that Module D
      # feeds directly into Module E (Aggregation), which now runs right
      # after Decontamination and before Module F (Enrichment).
      shared$modE_data <- NULL
      shared$modE_sample_cols <- NULL  # Round 47: canonical sample-column list, see mod_e_aggregation.R
      shared$modF_data <- NULL
      shared$modF_taxon_count <- NULL  # Round 47: cross-check value, see mod_f_enrichment.R

      asv_data <- shared$modC_agreed
      if (is.null(asv_data)) {
        rv$status <- list(type = "error", msg = "No ASV table available — run Module 3 first.")
        return(invisible(NULL))
      }

      metadata_path <- if (!is.null(rv$metadata_path_override)) {
        rv$metadata_path_override
      } else if (!is.null(input$metadata_file)) {
        input$metadata_file$datapath
      } else {
        NULL
      }

      if (is.null(metadata_path)) {
        rv$status <- list(type = "error",
                           msg = "Please upload a sample metadata file, or click \"Use example metadata\".")
        return(invisible(NULL))
      }

      needs_decontam   <- input$method %in% c("decontam", "both_require", "both_either")
      needs_microdecon <- input$method %in% c("microdecon", "both_require", "both_either")

      if (needs_decontam && !(.pkg_available("decontam") && .pkg_available("phyloseq"))) {
        rv$status <- list(type = "error",
                           msg = "The 'decontam' and/or 'phyloseq' R packages are not installed. Install them (Bioconductor) or choose 'microDecon only'.")
        return(invisible(NULL))
      }
      if (needs_microdecon && !.pkg_available("microDecon")) {
        rv$status <- list(type = "error",
                           msg = "The 'microDecon' R package is not installed. Install it or choose 'decontam only'.")
        return(invisible(NULL))
      }

      if (isolate(rv$cancel)) { rv$status <- list(type = "warning", msg = "Cancelled."); return(invisible(NULL)) }
      rv$status <- list(type = "info", msg = "Running contaminant detection...")

      withProgress(message = "Detecting contaminants", value = 0.05, {

        metadata <- tryCatch(read_tsv(metadata_path, show_col_types = FALSE),
                              error = function(e) NULL)
        if (is.null(metadata)) {
          rv$status <- list(type = "error", msg = "Could not parse metadata file as tab-delimited.")
          return(invisible(NULL))
        }

        incProgress(0.1, detail = "Matching samples to ASV table columns")

        exclude_patterns <- c("Kingdom", "Phylum", "Class", "Order", "Family",
                                "Genus", "Species", "ASV_id", "Sequence_length",
                                "AphiaID", "Fallback_rank", "Species_fallback",
                                "Consensus")
        all_cols <- names(asv_data)
        exclude_cols <- unique(unlist(lapply(exclude_patterns, function(p) grep(p, all_cols, ignore.case = TRUE, value = TRUE))))
        sample_cols_in_asv <- setdiff(all_cols, exclude_cols)
        sample_cols_in_asv <- sample_cols_in_asv[sapply(asv_data[sample_cols_in_asv], is.numeric)]

        # Same reconciliation logic Module 3's backbone-consolidation
        # preview uses (match_samples_to_metadata(), global.R) -- a single
        # source of truth so a column treated as unmatched here would also
        # have been flagged there.
        match_result <- match_samples_to_metadata(sample_cols_in_asv, metadata)
        if (!is.null(match_result$error)) {
          rv$status <- list(type = "error", msg = match_result$error)
          return(invisible(NULL))
        }
        blanks_matched <- match_result$blanks_matched
        true_samples_matched <- match_result$true_samples_matched
        recognized_sample_cols <- c(blanks_matched, true_samples_matched)
        unmatched_cols <- match_result$unmatched_cols

        if (length(blanks_matched) == 0) {
          rv$status <- list(type = "error",
                             msg = "No blank/control samples from the metadata file matched numeric columns in the ASV table. Check sample naming consistency.")
          return(invisible(NULL))
        }
        if (length(true_samples_matched) == 0) {
          rv$status <- list(type = "error",
                             msg = "No true (non-blank) samples from the metadata file matched numeric columns in the ASV table. Check sample naming consistency.")
          return(invisible(NULL))
        }

        if (length(unmatched_cols) > 0) {
          showNotification(
            paste0(
              length(unmatched_cols),
              " sample-like column(s) in the ASV table matched neither a blank nor a true sample in the metadata ",
              "file, and are excluded from the decontaminated output used by later modules: ",
              paste(unmatched_cols, collapse = ", "),
              ". They remain in the original uploaded ASV table -- nothing is deleted from your files. If any of ",
              "these should have been included, check their spelling against the sample-id column in your metadata."
            ),
            type = "warning", duration = 15
          )
        }

        final_column_order <- c("ASV_id", blanks_matched, true_samples_matched)
        asv_reorganized <- asv_data %>% select(all_of(final_column_order))

        col_sums <- colSums(asv_reorganized[, -1], na.rm = TRUE)
        zero_read_samples <- names(col_sums)[col_sums == 0]
        
        # 1. Create the 'Working Lists' for decontamination tools (only samples with reads)
        # Note: We must ensure we do not drop samples that have reads just because they aren't in metadata,
        # but we ONLY use metadata-identified blanks for decontamination.
        
        # We need to maintain all samples for decontamination tools to work, 
        # but filter them for the 'is.neg' classification.
        
        blanks_with_reads <- setdiff(blanks_matched, zero_read_samples)
        true_samples_with_reads <- setdiff(true_samples_matched, zero_read_samples)
        
        # Remove zero-read columns from the analysis table
        if (length(zero_read_samples) > 0) {
          asv_reorganized <- asv_reorganized %>% select(-all_of(zero_read_samples))
        }

        asv_sums <- rowSums(asv_reorganized[, -1], na.rm = TRUE)
        zero_read_asvs <- asv_sums == 0
        if (any(zero_read_asvs)) asv_reorganized <- asv_reorganized[!zero_read_asvs, ]

        decontam_contams <- character(0)
        microdecon_contams <- character(0)
        microdecon_error <- NULL

        if (needs_decontam) {
          incProgress(0.25, detail = "Running decontam (prevalence method)")
          counts <- as.matrix(asv_reorganized[, -1])
          rownames(counts) <- asv_reorganized$ASV_id

           sample_data_df <- data.frame(
             Sample_id = colnames(counts),
             is.neg = colnames(counts) %in% blanks_with_reads,
             row.names = colnames(counts)
           )

          ps <- phyloseq::phyloseq(
            phyloseq::otu_table(counts, taxa_are_rows = TRUE),
            phyloseq::sample_data(sample_data_df)
          )

          decontam_results <- tryCatch(
            decontam::isContaminant(
              ps, method = "prevalence", neg = "is.neg",
              threshold = input$decontam_threshold
            ),
            error = function(e) {
              rv$status <- list(type = "error", msg = paste("decontam failed:", e$message))
              NULL
            }
          )
          if (is.null(decontam_results)) return(invisible(NULL))
          decontam_contams <- rownames(decontam_results)[decontam_results$contaminant]
        }

        if (needs_microdecon) {
          incProgress(0.25, detail = "Running microDeCon")
          micro_input <- data.frame(
            ASV_id = as.character(asv_reorganized$ASV_id),
            as.matrix(asv_reorganized[, -1]),
            check.names = FALSE
          )
           micro_res <- tryCatch(
             microDecon::decon(
               data = micro_input,
               numb.blanks = length(blanks_with_reads),
               numb.ind = length(true_samples_with_reads),
               taxa = FALSE
             ),
             error = function(e) { microdecon_error <<- e$message; NULL }
           )
          if (!is.null(micro_res)) {
            microdecon_contams <- setdiff(micro_input$ASV_id, micro_res$decon.table$ASV_id)
          }
        }

        incProgress(0.2, detail = "Combining method results")

        both_n <- length(intersect(decontam_contams, microdecon_contams))

        to_remove <- switch(input$method,
          decontam = decontam_contams,
          microdecon = microdecon_contams,
          both_require = intersect(decontam_contams, microdecon_contams),
          both_either = unique(c(decontam_contams, microdecon_contams))
        )

        # Restrict every exported table's columns to what the app can
        # actually account for: non-sample columns (taxonomy/metadata,
        # via the same exclude_patterns used above) plus only the
        # sample-like columns metadata confirmed as a blank or a true
        # sample. This drops unmatched_cols generically -- whatever they
        # happen to be named, on any dataset -- rather than only ever
        # subtracting the known blanks and letting anything else through.
        non_sample_cols <- setdiff(names(asv_data), sample_cols_in_asv)
        asv_clean <- asv_data %>%
          filter(!ASV_id %in% to_remove) %>%
          select(all_of(non_sample_cols), all_of(recognized_sample_cols))
        asv_clean_no_blanks <- asv_clean %>% select(-any_of(blanks_matched))

        contaminants_df <- tibble(
          ASV_id = to_remove,
          decontam = to_remove %in% decontam_contams,
          microdecon = to_remove %in% microdecon_contams
        )

        rv$clean <- asv_clean
        rv$clean_no_blanks <- asv_clean_no_blanks
        rv$contaminants_df <- contaminants_df
        rv$summary <- list(
          original = nrow(asv_data), cleaned = nrow(asv_clean), removed = length(to_remove),
          n_blanks = length(blanks_matched), n_samples = length(true_samples_matched),
          unmatched_cols = unmatched_cols,
          decontam_flagged = length(decontam_contams), microdecon_flagged = length(microdecon_contams),
          both_agreed = both_n, microdecon_error = microdecon_error,
          method = input$method
        )

        incProgress(0.15, detail = "Done")
      })

      if (!is.null(rv$clean)) {
        rv$status <- list(type = "success",
                           msg = sprintf("Done. Removed %d contaminant ASVs (%s%%). %d ASVs remain.",
                                         rv$summary$removed, pct(rv$summary$removed, rv$summary$original),
                                         rv$summary$cleaned))
        shared$modD_clean_no_blanks <- rv$clean_no_blanks

        s <- rv$summary
        log_paths <- write_module_log(
          project_id = shared$project_name,
          module_id = "modD",
          module_title = "Module D -- Contaminant Detection & Removal",
          summary = list(
            "Method"                     = s$method,
            "ASVs in"                    = s$original,
            "Contaminant ASVs removed"   = s$removed,
            "Removal rate"               = sprintf("%s%%", pct(s$removed, s$original)),
            "ASVs remaining"             = s$cleaned,
            "Blanks/controls used"       = s$n_blanks,
            "True samples used"          = s$n_samples,
            "Unmatched columns excluded" = s$unmatched_cols,
            "decontam flagged"           = s$decontam_flagged,
            "microDecon flagged"         = s$microdecon_flagged,
            "Both methods agreed"        = s$both_agreed
          ),
          notes = if (!is.null(s$microdecon_error)) sprintf("microDecon error: %s", s$microdecon_error) else character(0)
        )
        rv$log_txt_path <- log_paths$txt
        rv$log_csv_path <- log_paths$csv
      }
    })

    output$summary_text <- renderPrint({
      req(rv$summary)
      s <- rv$summary
      cat("=== CONTAMINANT REMOVAL SUMMARY ===\n\n")
      cat(sprintf("Method: %s\n", s$method))
      cat(sprintf("Original ASVs:        %d\n", s$original))
      cat(sprintf("Removed ASVs:         %d (%.1f%%)\n", s$removed, pct(s$removed, s$original)))
      cat(sprintf("Remaining ASVs:       %d\n", s$cleaned))
      
      cat("\n=== METADATA SUMMARY ===\n")
      cat(sprintf("Total samples:        %d\n", s$n_blanks + s$n_samples))
      cat(sprintf("Blanks used:          %d\n", s$n_blanks))
      cat(sprintf("True samples used:    %d\n", s$n_samples))
      if (length(s$unmatched_cols %||% character(0)) > 0) {
        cat(sprintf("Unmatched columns (excluded from output): %d\n", length(s$unmatched_cols)))
        cat(sprintf("  -> %s\n", paste(s$unmatched_cols, collapse = ", ")))
      } else {
        cat("Unmatched columns (excluded from output): 0\n")
      }

      cat("\n=== DETECTION RESULTS ===\n")
      cat(sprintf("decontam flagged:     %d\n", s$decontam_flagged))
      cat(sprintf("microDeCon flagged:   %d\n", s$microdecon_flagged))
      cat(sprintf("Both agreed:          %d\n", s$both_agreed))
      if (!is.null(s$microdecon_error)) {
        cat(sprintf("\nmicroDeCon error: %s\n", s$microdecon_error))
      }
    })

    output$contam_table <- renderDT({
      req(rv$contaminants_df)
      datatable(rv$contaminants_df, options = list(pageLength = 10))
    })
    output$clean_table <- renderDT({
      req(rv$clean)
      datatable(rv$clean, options = list(pageLength = 8, scrollX = TRUE))
    })
    output$clean_noblanks_table <- renderDT({
      req(rv$clean_no_blanks)
      datatable(rv$clean_no_blanks, options = list(pageLength = 8, scrollX = TRUE))
    })

    output$dl_clean <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modD", "decontaminated_clean"),
      content = function(file) { req(rv$clean); write_csv(rv$clean, file) }
    )
    output$dl_clean_noblanks <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modD", "decontaminated_clean_no_blanks"),
      content = function(file) { req(rv$clean_no_blanks); write_csv(rv$clean_no_blanks, file) }
    )
    output$dl_contaminants <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modD", "contaminants_flagged"),
      content = function(file) { req(rv$contaminants_df); write_csv(rv$contaminants_df, file) }
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
