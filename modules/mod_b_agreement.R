# ========================================================================
# MODULE B — Cross-database agreement & species-level fallback
# Auto-fed from Module A's BLAST / BOLD / MZG AphiaID tables
# ========================================================================

mod_b_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "step-header",
        span(class = "step-number", "02"),
        div(
          h3("Cross-database agreement", class = "step-title"),
          p(class = "step-subtitle",
            "Compare BLAST, BOLD, and MZG taxonomy calls per ASV and build a species-level fallback assignment.")
        )
    ),

    layout_columns(
      col_widths = c(4, 8),

      card(
        card_header("Settings"),
        card_body(
          uiOutput(ns("input_status")),
          div(style = "margin-top: 8px; display: flex; gap: 6px;",
              actionButton(ns("run"), "Run agreement analysis", class = "btn-primary flex-fill",
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
            tabPanel("Agreement plot", plotOutput(ns("agreement_plot"), height = 380)),
            tabPanel("Agreement table", DTOutput(ns("agreement_table"))),
            tabPanel("Species conflicts", DTOutput(ns("conflicts_table"))),
            tabPanel("Fallback source", plotOutput(ns("fallback_plot"), height = 380)),
            tabPanel("Fallback rank", plotOutput(ns("fallback_rank_plot"), height = 380)),
            tabPanel("Combined + fallback", DTOutput(ns("combined_table")))
          ),
          div(style = "margin-top: 12px; display: flex; gap: 8px; flex-wrap: wrap;",
              downloadButton(ns("dl_agreement"), "Download agreement summary", class = "btn-outline-secondary"),
              downloadButton(ns("dl_conflicts"), "Download species conflicts", class = "btn-outline-secondary"),
              downloadButton(ns("dl_combined"), "Download combined + fallback table", class = "btn-outline-secondary"),
              downloadButton(ns("dl_log_txt"), "Download module log (.txt)", class = "btn-outline-secondary"),
              downloadButton(ns("dl_log_csv"), "Download module log (.csv)", class = "btn-outline-secondary")
          )
        )
      )
    )
  )
}

mod_b_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      combined = NULL,
      agreement_summary = NULL,
      species_conflicts = NULL,
      status = NULL,
      cancel = FALSE,
      log_txt_path = NULL,
      log_csv_path = NULL
    )

    output$input_status <- renderUI({
      have_blast <- !is.null(shared$modA_blast)
      have_bold  <- !is.null(shared$modA_bold)
      have_mzg   <- !is.null(shared$modA_mzg)

      row <- function(label, ok) {
        tags$div(class = if (ok) "input-ok" else "input-missing",
                 icon(if (ok) "circle-check" else "circle-minus"), " ", label)
      }

      tagList(
        p(class = "text-muted small", "Inputs carried over from Module A:"),
        row("BLAST AphiaID table", have_blast),
        row("BOLD AphiaID table", have_bold),
        row("MZG AphiaID table", have_mzg),
        if (!have_blast && !have_bold && !have_mzg)
          div(class = "alert alert-warning mt-2",
              "No data found from Module A yet. Run Module A first, or this step has nothing to compare.")
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

      rv$combined <- NULL  # reset so a failed re-run doesn't show stale success output
      rv$cancel <- FALSE

      blast_df <- shared$modA_blast
      bold_df  <- shared$modA_bold
      mzg_df   <- shared$modA_mzg

      if (is.null(blast_df) && is.null(bold_df) && is.null(mzg_df)) {
        rv$status <- list(type = "error", msg = "No inputs available — run Module A first.")
        return(invisible(NULL))
      }

      if (isolate(rv$cancel)) { rv$status <- list(type = "warning", msg = "Cancelled."); return(invisible(NULL)) }
      rv$status <- list(type = "info", msg = "Running agreement analysis...")

      withProgress(message = "Combining and analyzing", value = 0.1, {
        if (isolate(rv$cancel)) { rv$status <- list(type = "warning", msg = "Cancelled."); setProgress(1); return(invisible(NULL)) }

        combined <- NULL
        if (!is.null(bold_df)) {
          combined <- bold_df %>% mutate(ASV_id = as.character(ASV_id)) %>%
            rename_with(~ paste0("BOLD_", .), -ASV_id)
        }
        if (!is.null(blast_df)) {
          bdf <- blast_df %>% mutate(ASV_id = as.character(ASV_id)) %>%
            rename_with(~ paste0("BLAST_", .), -ASV_id)
          if (is.null(combined)) {
            combined <- bdf
          } else {
            check_join_cols(combined, bdf, "ASV_id", "combined (BOLD)", "bdf (BLAST)")
            combined <- full_join(combined, bdf, by = "ASV_id")
          }
        }
        if (!is.null(mzg_df)) {
          mdf <- mzg_df %>% mutate(ASV_id = as.character(ASV_id)) %>%
            rename_with(~ paste0("MZG_", .), -ASV_id)
          if (is.null(combined)) {
            combined <- mdf
          } else {
            check_join_cols(combined, mdf, "ASV_id", "combined", "mdf (MZG)")
            combined <- full_join(combined, mdf, by = "ASV_id")
          }
        }

        incProgress(0.3, detail = "Calculating per-level agreement")

        # Round 61: refused matches (Round 55) are joined onto a TEMPORARY copy
        # used only for these agreement statistics; `combined` and everything
        # downstream never gets a `_Rejected_*` column.
        agreement_input <- agreement_input_with_rejected(
          combined,
          list(BLAST = shared$modA_blast_rejected, BOLD = shared$modA_bold_rejected, MZG = shared$modA_mzg_rejected)
        )
        agreement_summary <- map_df(tax_levels, ~ check_agreement(agreement_input, .x))

        incProgress(0.2, detail = "Identifying species-level conflicts")

        sp_cols <- intersect(c("BLAST_Species_AphiaID", "MZG_Species_AphiaID", "BOLD_Species_AphiaID"), names(combined))

        species_conflicts <- if (length(sp_cols) >= 2) {
          combined %>%
            rowwise() %>%
            filter({
              vals <- c_across(all_of(sp_cols))
              vals <- vals[!is.na(vals) & vals != 0]
              length(vals) >= 2 && max(table(vals)) < 2
            }) %>%
            ungroup() %>%
            select(ASV_id, all_of(sp_cols))
        } else {
          tibble()
        }

        incProgress(0.05, detail = "Checking database columns")

        # Check at least two databases are present BEFORE build_species_fallback
        # (which drops per-DB columns after consolidating them).
        required_cols <- c("BLAST_Species_AphiaID", "MZG_Species_AphiaID", "BOLD_Species_AphiaID")
        present_cols <- intersect(required_cols, names(combined))
        if (length(present_cols) < 2) {
          rv$status <- list(type = "error",
                             msg = "Agreement analysis requires at least two of the following databases: BLAST, BOLD, or MZG.")
          return(invisible(NULL))
        }

        incProgress(0.1, detail = "Applying species fallback logic")

        combined <- build_species_fallback(combined, progress = function(cur, total) {
          incProgress(0.4 / total, detail = sprintf("WoRMS taxonomy re-query (%d/%d)", cur, total))
        })

        rv$species_conflicts_count_pre_filter <- sum(combined$Species_fallback_source == "None")

        rv$combined <- combined
        rv$agreement_summary <- agreement_summary
        rv$species_conflicts <- species_conflicts

        incProgress(0.2, detail = "Done")
      })

        rv$status <- list(type = "success",
                           msg = sprintf("Done. Analysis complete: %d total ASVs (%d with consensus, %d without — filtering will happen in Module 3).",
                                         nrow(rv$combined), nrow(rv$combined) - rv$species_conflicts_count_pre_filter, rv$species_conflicts_count_pre_filter))

      shared$modB_combined <- rv$combined

      agreement_notes <- if (!is.null(rv$agreement_summary)) {
        apply(rv$agreement_summary, 1, function(r) {
          sprintf("  %-8s: All agree=%s, Two agree=%s, None agree=%s, Only one database=%s (of %s with data)",
                  r[["Level"]], r[["All_three_agree"]], r[["Two_agree"]], r[["None_agree"]],
                  r[["Only_one_db"]], r[["Total_with_data"]])
        })
      } else character(0)

      log_paths <- write_module_log(
        project_id = shared$project_name,
        module_id = "modB",
        module_title = "Module B -- Cross-database Agreement",
        summary = list(
          "Total ASVs combined"              = nrow(rv$combined),
          "ASVs with species consensus"      = nrow(rv$combined) - rv$species_conflicts_count_pre_filter,
          "ASVs without consensus (None)"    = rv$species_conflicts_count_pre_filter,
          "Species-level conflicts flagged"  = nrow(rv$species_conflicts)
        ),
        notes = c("Per-level agreement (All/Two/None agree/Only one database):", agreement_notes)
      )
      rv$log_txt_path <- log_paths$txt
      rv$log_csv_path <- log_paths$csv
    })

    output$agreement_table <- renderDT({
      req(rv$agreement_summary)
      datatable(rv$agreement_summary, options = list(pageLength = 10, dom = "t"))
    })

    output$agreement_plot <- renderPlot({
      req(rv$agreement_summary)

      # IMPORTANT: each taxonomic level's counts (All/Two/None agree + Only one
      # database) must be turned into percentages using THAT LEVEL's own total
      # as the denominator, not a grand total summed across all 7 levels. The
      # previous version divided every bar by the sum across ALL levels,
      # which shrank every bar by ~7x and made cross-level comparisons
      # meaningless. Here, percentages are computed per level so each
      # level's segments sum to 100% of the ASVs with data at that level
      # (Round 62: the fourth segment, "Only one database", is what was
      # missing from the three-category version).
      agreement_long <- rv$agreement_summary %>%
        filter(!is.na(All_three_agree)) %>%
        pivot_longer(cols = c(All_three_agree, Two_agree, None_agree, Only_one_db),
                     names_to = "Agreement", values_to = "Count") %>%
        mutate(Level = factor(Level, levels = tax_levels),
               Agreement = factor(Agreement,
                                   levels = c("All_three_agree", "Two_agree", "None_agree", "Only_one_db"),
                                   labels = c("All agree", "Two agree", "None agree", "Only one database"))) %>%
        group_by(Level) %>%
        mutate(Percent = if (sum(Count, na.rm = TRUE) > 0) 100 * Count / sum(Count, na.rm = TRUE) else 0) %>%
        ungroup()

      # 100%-stacked bars: one bar per taxonomic level, segments sum to 100%.
      # This directly shows the expected pattern — agreement is high at
      # Kingdom/Phylum and degrades going down toward Species — in a single
      # glance, which a grouped bar chart (21 separate bars) does not.
      ggplot(agreement_long, aes(x = Level, y = Percent, fill = Agreement)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = ifelse(Percent >= 5, sprintf("%.0f%%", Percent), "")),
                  position = position_stack(vjust = 0.5), color = "white", size = 3.6, fontface = "bold") +
        scale_fill_manual(values = agreement_colors) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
        theme_minimal(base_size = 13) +
        labs(title = "Taxonomic agreement across databases",
             subtitle = "Each bar = 100% of ASVs with data at that level",
             x = NULL, y = "Percent of ASVs", fill = NULL) +
        theme(legend.position = "bottom",
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank())
    })

    output$conflicts_table <- renderDT({
      req(rv$species_conflicts)
      if (nrow(rv$species_conflicts) == 0) return(datatable(tibble(Message = "No species-level conflicts found.")))
      datatable(rv$species_conflicts, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$fallback_plot <- renderPlot({
      req(rv$combined)
      fb <- rv$combined %>% count(Species_fallback_source) %>%
        mutate(Percent = round(100 * n / sum(n), 1))

      ggplot(fb, aes(x = reorder(Species_fallback_source, n), y = n, fill = Species_fallback_source)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = sprintf("%d (%.1f%%)", n, Percent)), hjust = -0.1, size = 3.5) +
        coord_flip() +
        scale_fill_manual(values = fallback_color_for(fb$Species_fallback_source)) +
        theme_minimal(base_size = 13) +
        labs(title = "Species fallback source distribution", x = NULL, y = "Number of ASVs") +
        theme(legend.position = "none") +
        expand_limits(y = max(fb$n) * 1.15)
    })

    output$fallback_rank_plot <- renderPlot({
      req(rv$combined)
      rank_order <- c("Species", "Genus", "Family", "Order", "Class", "Phylum", "Kingdom")
      fb <- rv$combined %>%
        mutate(Fallback_rank = replace_na(Fallback_rank, "Unresolved")) %>%
        count(Fallback_rank) %>%
        mutate(Fallback_rank = factor(Fallback_rank, levels = c(rank_order, "Unresolved")),
               Percent = round(100 * n / sum(n), 1))

      ggplot(fb, aes(x = Fallback_rank, y = n, fill = Fallback_rank)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = sprintf("%d (%.1f%%)", n, Percent)),
                  vjust = -0.3, size = 3.5) +
        scale_fill_viridis_d(direction = -1, na.value = "grey70") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
        theme_minimal(base_size = 13) +
        labs(title = "Fallback rank distribution",
             subtitle = "Finest taxonomic rank at which ≥2 databases agreed",
             x = NULL, y = "Number of ASVs") +
        theme(legend.position = "none",
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank())
    })

    output$combined_table <- renderDT({
      req(rv$combined)
      datatable(rv$combined, options = list(pageLength = 8, scrollX = TRUE))
    })

    output$dl_agreement <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modB", "agreement_summary"),
      content = function(file) { req(rv$agreement_summary); write_csv(rv$agreement_summary, file) }
    )
    output$dl_conflicts <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modB", "species_conflicts"),
      content = function(file) { req(rv$species_conflicts); write_csv(rv$species_conflicts, file) }
    )
    output$dl_combined <- downloadHandler(
      filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modB", "combined_full"),
      content = function(file) { req(rv$combined); write_csv(rv$combined, file) }
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
