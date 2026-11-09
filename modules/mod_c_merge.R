# ========================================================================
# MODULE C — Final ASV table merge
# Backbone: raw MZG file (sample/read-count columns, taxonomy stripped)
# Adds: Module B's combined + fallback table
# ========================================================================

mod_c_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "step-header",
        span(class = "step-number", "03"),
        div(
          h3("Final ASV table assembly", class = "step-title"),
          p(class = "step-subtitle",
            "Merge sample read counts with standardized taxonomy and the species fallback assignment into one table.")
        )
    ),

    layout_columns(
      col_widths = c(4, 8),

      card(
        card_header("Inputs"),
        card_body(
          uiOutput(ns("input_status")),
          p(class = "text-muted small",
            "This step reuses the MZG file you uploaded in Module 1 as the ASV/sample-count backbone. ",
            "If you have a second MZG backbone file to use instead (e.g. a re-export with corrected read ",
            "counts), upload it here. When both are present, the two are consolidated: the second file's ",
            "read counts are used, but only after confirming both files describe the exact same set of ",
            "ASVs. A mismatch is refused rather than silently combined, since swapping in read counts for ",
            "a different ASV set would desync the pipeline's join key downstream. The second file's own ",
            "taxonomy columns (if any) are always dropped -- taxonomy comes from Module 2's resolved table."),
          fileInput(ns("mzg_override"), "Second MZG backbone file (optional)", accept = c(".csv", ".tsv", ".txt")),
          fileInput(ns("metadata_for_validation"),
                    "Sample metadata file (optional -- only used with a second MZG file, to check its sample/blank columns against your metadata early)",
                    accept = c(".txt", ".tsv")),
          uiOutput(ns("backbone_preview")),
          div(style = "margin-top: 8px; display: flex; gap: 6px;",
              actionButton(ns("run"), "Assemble final ASV table", class = "btn-primary flex-fill",
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
            tabPanel("Agreeing ASVs", DTOutput(ns("merged_table"))),
            tabPanel("All ASVs", DTOutput(ns("all_table"))),
            tabPanel("Disagreeing ASVs", DTOutput(ns("disagree_table"))),
            tabPanel("Summary", verbatimTextOutput(ns("summary_text"))),
            tabPanel("Column selection",
              p(class = "text-muted small", "Deselect any columns to remove them from the table preview and download."),
              uiOutput(ns("column_picker"))
            )
          ),
          div(style = "margin-top: 12px; display: flex; gap: 8px; flex-wrap: wrap;",
              downloadButton(ns("dl_merged"), "Download agreeing ASVs", class = "btn-outline-secondary"),
              downloadButton(ns("dl_all"), "Download all ASVs", class = "btn-outline-secondary"),
              downloadButton(ns("dl_disagree"), "Download disagreeing ASVs", class = "btn-outline-secondary"),
              downloadButton(ns("dl_log_txt"), "Download module log (.txt)", class = "btn-outline-secondary"),
              downloadButton(ns("dl_log_csv"), "Download module log (.csv)", class = "btn-outline-secondary")
          )
        )
      )
    )
  )
}

mod_c_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(merged = NULL, all_df = NULL, disagree_df = NULL, status = NULL, id_diagnostic = NULL, cancel = FALSE,
                         log_txt_path = NULL, log_csv_path = NULL)

    output$input_status <- renderUI({
      have_mzg_backbone <- !is.null(shared$modA_mzg_backbone_path) || !is.null(input$mzg_override)
      have_fallback <- !is.null(shared$modB_combined)

      row <- function(label, ok) {
        tags$div(class = if (ok) "input-ok" else "input-missing",
                 icon(if (ok) "circle-check" else "circle-minus"), " ", label)
      }

      tagList(
        p(class = "text-muted small", "Inputs carried over from previous steps:"),
        row("MZG backbone file (sample/read counts)", have_mzg_backbone),
        row("Combined + fallback table (Module 2)", have_fallback),
        if (!have_fallback)
          div(class = "alert alert-warning mt-2", "Run Module 2 first to generate the fallback table.")
      )
    })

    output$status_box <- renderUI({
      if (is.null(rv$status)) return(NULL)
      cls <- switch(rv$status$type, info = "alert-info", success = "alert-success",
                    error = "alert-danger", "alert-secondary")
      div(class = paste("alert", cls), role = "alert", rv$status$msg)
    })

    # Live consolidation preview (2026-09-11): runs the same two-stage check
    # (ASV count, then per-position sequence) as soon as a second backbone
    # file is uploaded, so a mismatch shows up immediately instead of only
    # after clicking "Assemble final ASV table". Purely informational --
    # does not touch rv$merged/all_df/etc or shared state, and does not
    # fire a toast (consolidate_mzg_backbones() no longer does that itself
    # -- see global.R); it just renders inline below the file input.
    backbone_preview <- reactive({
      req(input$mzg_override)
      original_backbone_path <- shared$modA_mzg_backbone_path
      if (is.null(original_backbone_path)) {
        return(list(type = "info", msg = "No original Module 1 MZG backbone found in this session yet — this file will be used on its own rather than consolidated, unless you run/upload one in Module 1 first."))
      }

      orig_result <- tryCatch(assign_mzg_asv_id(read_mzg_auto(original_backbone_path)), error = function(e) NULL)
      if (is.null(orig_result)) {
        return(list(type = "error", msg = "Could not read the original Module 1 MZG backbone file to check against."))
      }
      override_raw <- tryCatch(read_mzg_auto(input$mzg_override$datapath), error = function(e) NULL)
      if (is.null(override_raw)) {
        return(list(type = "error", msg = "Could not read the second MZG backbone file."))
      }

      metadata_df <- NULL
      if (!is.null(input$metadata_for_validation)) {
        metadata_df <- tryCatch(read_tsv(input$metadata_for_validation$datapath, show_col_types = FALSE), error = function(e) NULL)
        if (is.null(metadata_df)) {
          return(list(type = "error", msg = "Could not parse the metadata file as tab-delimited."))
        }
      }

      consolidated <- consolidate_mzg_backbones(orig_result$df, override_raw, metadata_df = metadata_df)
      if (!is.null(consolidated$error)) {
        return(list(type = "error", msg = consolidated$error))
      }

      msg <- sprintf("%d ASVs matched between the two MZG backbone files (row count + per-position DNA sequence verification).",
                      consolidated$n_matched)
      mi <- consolidated$match_info
      if (is.null(mi)) {
        msg <- paste0(msg, " Ready to assemble.")
        return(list(type = "success", msg = msg))
      }
      msg <- paste0(msg, sprintf(" Against your metadata file: %d true sample column(s) and %d blank/control column(s) matched.",
                                  length(mi$true_samples_matched), length(mi$blanks_matched)))
      if (length(mi$unmatched_cols) > 0) {
        msg <- paste0(msg, sprintf(" %d column(s) in the second file matched neither and will be excluded: %s.",
                                    length(mi$unmatched_cols), paste(mi$unmatched_cols, collapse = ", ")))
        return(list(type = "warning", msg = msg))
      }
      list(type = "success", msg = paste0(msg, " Ready to assemble."))
    })

    output$backbone_preview <- renderUI({
      req(input$mzg_override)
      preview <- backbone_preview()
      cls <- switch(preview$type, info = "alert-info", success = "alert-success",
                    warning = "alert-warning", error = "alert-danger", "alert-secondary")
      div(class = paste("alert", cls), style = "margin-top: 8px; padding: 8px 12px; font-size: 0.9em;",
          role = "alert", preview$msg)
    })

    observeEvent(input$cancel, { rv$cancel <- TRUE })

    observeEvent(input$run, {
      shinyjs::disable("run")
      on.exit(shinyjs::enable("run"))

      rv$merged <- NULL  # reset so a failed re-run doesn't show stale success output
      rv$cancel <- FALSE

      fallback_df <- shared$modB_combined
      if (is.null(fallback_df)) {
        rv$status <- list(type = "error", msg = "Missing the combined + fallback table — run Module 2 first.")
        return(invisible(NULL))
      }

      original_backbone_path <- shared$modA_mzg_backbone_path
      override_backbone_path <- if (!is.null(input$mzg_override)) input$mzg_override$datapath else NULL

      if (is.null(original_backbone_path) && is.null(override_backbone_path)) {
        rv$status <- list(type = "error",
                           msg = "No MZG backbone file available. Upload one here or re-run Module 1 with an MZG file.")
        return(invisible(NULL))
      }

      if (isolate(rv$cancel)) { rv$status <- list(type = "warning", msg = "Cancelled."); return(invisible(NULL)) }
      rv$status <- list(type = "info", msg = "Assembling final ASV table...")

      withProgress(message = "Merging tables", value = 0.1, {

        # IMPORTANT: derive ASV_id from each raw file BEFORE dropping the
        # unnamed/EMPTY_COL_1 leading column below. Module A's read_MZG()
        # applies the same assign_mzg_asv_id() logic to this same source
        # file — using identical logic here keeps the join key consistent
        # between modules. Previously this column was discarded as "empty"
        # and a fresh ASV_id was invented from row order here, which could
        # silently desync from Module A's separately-invented ASV_id if the
        # two reads parsed rows even slightly differently — the root cause
        # of empty MZG_*_AphiaID columns after the Module B agreement join.
        read_and_id <- function(path) {
          raw <- tryCatch(read_mzg_auto(path), error = function(e) NULL)
          if (is.null(raw)) return(NULL)
          assign_mzg_asv_id(raw)
        }

        if (!is.null(original_backbone_path) && !is.null(override_backbone_path)) {
          # Two MZG backbone files present (2026-09-11): consolidate rather
          # than blindly override. The second file commonly has no id-like
          # column at all, so it's intentionally NOT run through
          # assign_mzg_asv_id() here -- instead consolidate_mzg_backbones()
          # (global.R) does a two-stage check: first an ASV match (row
          # count, taking rows in order -- the override's rows inherit the
          # original's real ASV_id by that position), then a sequence
          # match as a safety net on top (verifying the DNA sequence
          # actually agrees at every position). Sequence content ALONE
          # isn't used as the match key, since identical sequences can
          # legitimately repeat, which would make a content-based lookup
          # ambiguous.
          orig_result <- read_and_id(original_backbone_path)
          if (is.null(orig_result)) {
            rv$status <- list(type = "error", msg = "Could not read the original Module 1 MZG backbone file.")
            return(invisible(NULL))
          }
          override_raw <- tryCatch(read_mzg_auto(override_backbone_path), error = function(e) NULL)
          if (is.null(override_raw)) {
            rv$status <- list(type = "error", msg = "Could not read the second MZG backbone file.")
            return(invisible(NULL))
          }

          metadata_df <- NULL
          if (!is.null(input$metadata_for_validation)) {
            metadata_df <- tryCatch(read_tsv(input$metadata_for_validation$datapath, show_col_types = FALSE), error = function(e) NULL)
            if (is.null(metadata_df)) {
              rv$status <- list(type = "error", msg = "Could not parse the metadata file as tab-delimited.")
              return(invisible(NULL))
            }
          }

          incProgress(0.15, detail = "Matching both MZG backbone files (ASV count, then sequence)")
          consolidated <- consolidate_mzg_backbones(orig_result$df, override_raw, metadata_df = metadata_df)
          if (!is.null(consolidated$error)) {
            rv$status <- list(type = "error", msg = consolidated$error)
            showNotification(consolidated$error, type = "error", duration = 20)
            return(invisible(NULL))
          }

          mzg_raw <- consolidated$df
          diag <- sprintf(
            "Consolidated: %d ASVs matched between the two MZG backbone files (row count + per-position DNA sequence verification); using the second file's read counts.",
            consolidated$n_matched
          )
          mi <- consolidated$match_info
          if (!is.null(mi)) {
            diag <- paste0(diag, sprintf(
              " Matched against metadata: %d true sample(s), %d blank/control(s)",
              length(mi$true_samples_matched), length(mi$blanks_matched)
            ))
            if (length(mi$unmatched_cols) > 0) {
              diag <- paste0(diag, sprintf(
                "; excluded %d unmatched column(s): %s",
                length(mi$unmatched_cols), paste(mi$unmatched_cols, collapse = ", ")
              ))
            }
            diag <- paste0(diag, ".")
          }
          rv$id_diagnostic <- diag
        } else {
          backbone_path <- if (!is.null(override_backbone_path)) override_backbone_path else original_backbone_path
          id_result <- read_and_id(backbone_path)
          if (is.null(id_result)) {
            rv$status <- list(type = "error", msg = "Could not read MZG backbone file.")
            return(invisible(NULL))
          }
          mzg_raw <- id_result$df
          rv$id_diagnostic <- id_result$diagnostic
          incProgress(0.15, detail = "Assigning ASV identifiers")
        }
        rv$sequence_col <- NULL

        sequence_col <- grep("^(sequence|seq|nucleotides?|nuc|barcode|rep_seq|asv_sequence|otu_sequence)$",
                              names(mzg_raw), ignore.case = TRUE, value = TRUE)
        if (length(sequence_col) > 0) {
          seq_name <- sequence_col[1]
          rv$sequence_col <- seq_name
          mzg_raw$Sequence_length_bp <- nchar(mzg_raw[[seq_name]], type = "chars")
        }

        incProgress(0.1, detail = "Stripping taxonomy columns from backbone")

        taxonomy_cols_pattern <- c("^kingdom$", "^phylum$", "^class$", "^order$",
                                    "^family$", "^genus$", "^species$")
        taxonomy_cols_to_remove <- names(mzg_raw)[
          grepl(paste(taxonomy_cols_pattern, collapse = "|"), names(mzg_raw), ignore.case = TRUE)
        ]
        empty_cols_to_remove <- grep("^EMPTY_COL_", names(mzg_raw), value = TRUE)
        sequence_cols_to_remove <- if (length(sequence_col) > 0) sequence_col[1] else character(0)
        all_cols_to_remove <- unique(c(taxonomy_cols_to_remove, empty_cols_to_remove, sequence_cols_to_remove))
        asv_cols <- setdiff(names(mzg_raw), all_cols_to_remove)
        asv_cols <- asv_cols[asv_cols != "" & !is.na(asv_cols)]
        # Sequence_length_bp comes from the Module B fallback table
        # (renamed from MZG_Sequence_length_bp) — exclude it here to avoid
        # a column clash on join.
        asv_cols <- setdiff(asv_cols, "Sequence_length_bp")
        asv_cols <- union("ASV_id", asv_cols)  # ASV_id is now guaranteed present — keep it regardless

        mzg_asv <- mzg_raw %>% select(all_of(asv_cols)) %>% mutate(ASV_id = as.character(ASV_id))

        incProgress(0.1, detail = "Building all-ASVs table")

        # FIXED: the case-insensitive rename below must run BEFORE the column
        # is coerced by name - it used to run after a line that already
        # required an exact-case "ASV_id" column to exist (or throw), so it
        # could never actually rescue a differently-cased id column (asv_id,
        # Asv_ID) in the one situation it existed for.
        fallback_all <- fallback_df
        idx <- which(tolower(names(fallback_all)) == "asv_id")
        if (length(idx) > 0 && names(fallback_all)[idx[1]] != "ASV_id") {
          names(fallback_all)[idx[1]] <- "ASV_id"
        }
        fallback_all <- fallback_all %>% mutate(ASV_id = as.character(ASV_id))

        # 1. All ASVs — left join with unfiltered fallback
        check_join_cols(mzg_asv, fallback_all, "ASV_id", "mzg_asv", "fallback_all")
        all_df <- mzg_asv %>% left_join(fallback_all %>% select(ASV_id), by = "ASV_id")
        check_join_cols(all_df, fallback_all, "ASV_id", "all_df", "fallback_all")
        all_df <- all_df %>% left_join(fallback_all, by = "ASV_id", suffix = c("", "_fallback"))

        # 2. Agreeing ASVs — inner join filtered to non-None fallback source
        fallback_agree <- fallback_all %>% filter(Species_fallback_source != "None")
        check_join_cols(mzg_asv, fallback_agree, "ASV_id", "mzg_asv", "fallback_agree")
        agree_df <- mzg_asv %>% inner_join(fallback_agree %>% select(ASV_id), by = "ASV_id")
        check_join_cols(agree_df, fallback_agree, "ASV_id", "agree_df", "fallback_agree")
        agree_df <- agree_df %>% left_join(fallback_agree, by = "ASV_id", suffix = c("", "_fallback"))

        # 3. Disagreeing ASVs — in all but not in agree
        check_join_cols(all_df, agree_df, "ASV_id", "all_df", "agree_df")
        disagree_df <- all_df %>% anti_join(agree_df %>% select(ASV_id), by = "ASV_id")

        incProgress(0.2, detail = "Removing duplicate columns")

        drop_dup_cols <- function(df) {
          dup <- duplicated(names(df))
          if (any(dup)) df <- df[, !dup]
          df
        }
        all_df <- drop_dup_cols(all_df)
        agree_df <- drop_dup_cols(agree_df)
        disagree_df <- drop_dup_cols(disagree_df)

        rv$merged <- agree_df
        rv$all_df <- all_df
        rv$disagree_df <- disagree_df
        rv$all_cols <- names(agree_df)
        # Round 55: the new per-rank _Rejected_AphiaID/_Rejected_Name columns
        # (a refused context-disagreeing match, kept for Module B's agreement
        # comparison — see check_agreement() in global.R) are diagnostic in
        # the same way _context_agreed already is, so they default to hidden
        # here too; still selectable, nothing is actually removed.
        redundant <- grep("_original$|_context_agreed$|_Rejected_AphiaID$|_Rejected_Name$", names(agree_df), value = TRUE)
        rv$redundant_cols <- redundant
        rv$default_keep <- setdiff(rv$all_cols, redundant)
        rv$n_redundant <- length(redundant)
        incProgress(0.1, detail = "Done")
      })

      if (!is.null(rv$merged) && !is.null(rv$all_df) && !is.null(rv$disagree_df)) {
        n_agree <- nrow(rv$merged)
        n_all <- nrow(rv$all_df)
        n_disagree <- nrow(rv$disagree_df)
        rv$status <- list(type = "success",
                           msg = sprintf("Done. All: %d ASVs | Agree: %d | Disagree: %d.",
                                         n_all, n_agree, n_disagree))
        shared$modC_agreed <- rv$merged

        fb_notes <- character(0)
        fb_col <- grep("^Species_fallback_source$", names(rv$merged), value = TRUE)
        if (length(fb_col) > 0) {
          tb <- table(rv$merged[[fb_col[1]]])
          fb_notes <- c("Fallback source breakdown (agreeing set):",
                        sprintf("  %-30s: %5d (%.1f%%)", names(tb), tb, pct(tb, nrow(rv$merged))))
        }

        log_paths <- write_module_log(
          project_id = shared$project_name,
          module_id = "modC",
          module_title = "Module C -- Final ASV Table Assembly",
          summary = list(
            "ASV identifier source" = rv$id_diagnostic %||% "(single MZG backbone file)",
            "All ASVs"              = n_all,
            "Agreeing ASVs"         = n_agree,
            "Disagreeing ASVs"      = n_disagree,
            "Columns (agreeing table)" = ncol(rv$merged)
          ),
          notes = fb_notes
        )
        rv$log_txt_path <- log_paths$txt
        rv$log_csv_path <- log_paths$csv
      }
    })

    output$column_picker <- renderUI({
      req(rv$all_cols, rv$default_keep)
      info <- if (rv$n_redundant > 0)
        div(class = "alert alert-info", style = "padding: 6px 12px;",
            sprintf("Auto-detected %d redundant column(s) and deselected them: %s.\nAll columns are still available — select any to re-include.",
                    rv$n_redundant, paste(rv$redundant_cols, collapse = ", ")))
      tagList(
        info,
        selectInput(ns("keep_cols"), "Columns to keep",
                    choices = rv$all_cols, selected = rv$default_keep,
                    multiple = TRUE, width = "100%",
                    selectize = TRUE)
      )
    })

    filtered_merged <- reactive({
      req(rv$merged)
      if (is.null(input$keep_cols)) return(rv$merged)
      cols <- intersect(input$keep_cols, names(rv$merged))
      if (length(cols) == 0) return(rv$merged)
      rv$merged[, cols, drop = FALSE]
    })

    output$merged_table <- renderDT({
      datatable(filtered_merged(), options = list(pageLength = 8, scrollX = TRUE))
    })
    output$all_table <- renderDT({
      req(rv$all_df)
      datatable(rv$all_df, options = list(pageLength = 8, scrollX = TRUE))
    })
    output$disagree_table <- renderDT({
      req(rv$disagree_df)
      datatable(rv$disagree_df, options = list(pageLength = 8, scrollX = TRUE))
    })

    output$summary_text <- renderPrint({
      req(rv$merged, rv$all_df, rv$disagree_df)
      if (!is.null(rv$id_diagnostic)) {
        cat("ASV identifier source:", rv$id_diagnostic, "\n\n")
      }
      cat(sprintf("All ASVs:        %d\n", nrow(rv$all_df)))
      cat(sprintf("Agreeing ASVs:   %d\n", nrow(rv$merged)))
      cat(sprintf("Disagreeing ASVs:%d\n", nrow(rv$disagree_df)))
      cat(sprintf("Columns:         %d\n", ncol(rv$merged)))
      if (!is.null(rv$sequence_col)) cat(sprintf("Sequence column: %s\n", rv$sequence_col))
      fb_col <- grep("^Species_fallback_source$", names(rv$merged), value = TRUE)
      if (length(fb_col) > 0) {
        cat("\nFallback source breakdown (agreeing set):\n")
        tb <- table(rv$merged[[fb_col[1]]])
        for (nm in names(tb)) {
          cat(sprintf("  %-30s: %5d (%.1f%%)\n", nm, tb[nm], pct(tb[nm], nrow(rv$merged))))
        }
      }
    })

    output$dl_merged <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modC", "agreeing_ASVs"),
      content = function(file) { req(filtered_merged()); write_csv(filtered_merged(), file) }
    )
    output$dl_all <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modC", "all_ASVs"),
      content = function(file) { req(rv$all_df); write_csv(rv$all_df, file) }
    )
    output$dl_disagree <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modC", "disagreeing_ASVs"),
      content = function(file) { req(rv$disagree_df); write_csv(rv$disagree_df, file) }
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
