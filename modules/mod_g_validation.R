
# ========================================================================
# MODULE G — Occurrence & Trait Audit
# Filters Module 6 (Enrichment) output for conflicts and anomalies
# ========================================================================

mod_g_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "step-header",
        span(class = "step-number", "07"),
        div(
          h3("Data confirmation & validation", class = "step-title"),
          p(class = "step-subtitle", "Validate the enriched, aggregated taxon table from Module 6 against occurrence classification via OBIS/GBIF and trait assignments via the MarineSpeciesTraits database, then review flagged mismatches.")
        )
    ),
    navset_card_tab(
      nav_panel("Summary",
                verbatimTextOutput(ns("summary_text"))
      ),
      nav_panel("Unvalidated", 
                DTOutput(ns("conflict_table")),
                p(class = "text-muted small mt-2", "Filtered: Showing only unvalidated traits.")
      ),
      nav_panel("Validated Table", 
                DTOutput(ns("full_table"))
      ),
    ),
    card(
        card_header("Downloads"),
        card_body(
            p(class = "text-muted small",
              "Three exports, same columns throughout, every row carrying Unvalidated_Reason ",
              "(why it didn't validate, if it didn't) and WoRMS_Consensus_Conflict_Detail (only ",
              "populated when WoRMS's own taxonomy disagrees with the pipeline's Consensus_* ",
              "taxonomy) — Validated Table is every taxon at every rank; Species Table is any ",
              "taxon with a binomial name; Genus Table is any taxon with a resolved genus, ",
              "species included. Use the \"Unvalidated\" tab above to review just the flagged ",
              "rows on-screen."),
            p(class = "text-muted small",
              "Two different conflict columns travel in every export, and they are NOT the ",
              "same check: taxonomy_conflict (from Module 5/Aggregation) is TRUE when multiple ",
              "ASVs merged into one taxon disagreed with EACH OTHER on their input taxonomy, ",
              "before WoRMS was ever consulted. WoRMS_Consensus_Conflict_Detail (added here) is ",
              "populated only when WoRMS's own returned taxonomy for the matched AphiaID ",
              "disagrees with that already-merged input consensus. A taxon can have one, both, ",
              "or neither — they catch different failure points in the pipeline."),
            # Round 47 (2026-10-01): capped at these three exports, all built
            # from the one general table Module F now hands this module (see
            # shared$modF_data) with identical column sets -- previously
            # Module F and Module G each exported their own "species_level"
            # with different columns, which is what made the exported file
            # set look inconsistent ("the outputs in terms of number species
            # still do not reconcile well"). Module F's own species/genus
            # downloads were removed for the same reason; these three here
            # are now the only species-level/genus-level exports in the app.
            div(class = "d-flex gap-2 flex-wrap",
                downloadButton(ns("dl_full"), "Download Validated Table (all taxa, all ranks)", class = "btn-outline-secondary"),
                downloadButton(ns("dl_species_f"), "Download Species Table", class = "btn-outline-secondary"),
                downloadButton(ns("dl_genus_f"), "Download Genus Table (incl. species)", class = "btn-outline-secondary"),
                downloadButton(ns("dl_log_txt"), "Download module log (.txt)", class = "btn-outline-secondary"),
                downloadButton(ns("dl_log_csv"), "Download module log (.csv)", class = "btn-outline-secondary")
            )
        )
    )
  )
}

mod_g_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

    # Log file paths for this module's run -- this module has no explicit
    # Run button (it's a read-only audit view driven by Module F's output),
    # so the log is (re)written whenever input_data() changes, via the
    # observeEvent() below.
    log_rv <- reactiveValues(txt_path = NULL, csv_path = NULL)

    # Round 4 (2026-09-09): Aggregation now runs BEFORE Enrichment (not
    # after), so this module's input is simply Enrichment's output — there
    # is no longer a second, later aggregation step to fall back past.
    input_data <- reactive({
      shared$modF_data
    })

    # Round 47 (2026-10-01): sample columns are now read from Module E's own
    # canonical list (shared$modE_sample_cols, published alongside
    # shared$modE_data in mod_e_aggregation.R) instead of being re-guessed
    # here via a substring exclude-pattern blacklist. That blacklist was the
    # actual root cause of the "species counts don't reconcile" confusion:
    # it was last updated around Round 22f and never extended as newer
    # metadata columns were added (WoRMS_attributes, WoRMS_URL,
    # WoRMS_FunctionalGroup, WoRMS_vernacular_*, Occurrence_Override_Applied/
    # Reason, Occurrence_Single_DB_Only, Occurrence_hops_to_reference_region,
    # Occurrence_nearest_locality, bare Group1/Group2/TaxGroup,
    # IsBrackish/IsFreshwater/IsTerrestrial) -- none of those matched any
    # exclude pattern, so all of them were silently swept into "sample
    # columns" and exported as if they were per-sample abundance data,
    # instead of being deliberately selected as the metadata/QC columns
    # they actually are. Falling back to the old heuristic only covers a
    # session that somehow reaches Module G without shared$modE_sample_cols
    # set, and is logged loudly rather than trusted silently.
    sample_cols <- reactive({
        req(input_data())
        if (!is.null(shared$modE_sample_cols)) {
          intersect(shared$modE_sample_cols, names(input_data()))
        } else {
          message("WARNING: shared$modE_sample_cols unavailable in Module G -- falling back ",
                   "to substring-based sample-column detection for this run.")
          exclude_patterns <- c("Kingdom", "Phylum", "Class", "Order", "Family",
                                  "Genus", "Species", "Taxon_id", "ASV_id", "n_asvs", "taxonomy_conflict",
                                  "AphiaID", "Fallback_rank", "Species_fallback",
                                  "WoRMS_Rank", "IsMarine", "WoRMS_Distribution",
                                  "Body_Size", "Functional_Group", "FG_source",
                                  "Occurrence_classification", "Occurrence_Evidence_Conflict",
                                  "Occurrence_reference_region",   # Round 65: never a sample column
                                  "Observed_in_OBIS", "Observed_in_GBIF",
                                  "Occurrence_Lat", "Occurrence_Lon", "Matched_Name",
                                  "DB_Group", "Group1_Validated", "Group2_Validated", "TaxGroup_Validated",
                                  "Unvalidated_Reason", "Sequence_length")
          all_cols <- names(input_data())
          exclude_cols <- unique(unlist(lapply(exclude_patterns, function(p) grep(p, all_cols, ignore.case = TRUE, value = TRUE))))
          setdiff(all_cols, exclude_cols)
        }
    })

    # Round 49 (2026-10-01): per direct user request after reviewing the real
    # column list ("do we really need all these" / "i found many of the
    # columns redundant"), these columns are dropped from every Module G
    # export. Checked against a real 263-taxon run before cutting anything:
    #   - Occurrence_hops_to_reference_region, Occurrence_nearest_locality,
    #     Parsed_MRGIDs: 0/263 populated -- dead since Round 39 removed the
    #     gazetteer hop-distance mechanism that used to fill them.
    #   - Occurrence_NA_Reason: 263/263 populated, but the EXACT SAME
    #     boilerplate sentence every time ("Not applicable -- gazetteer
    #     hop-distance classification removed...") -- zero information.
    #   - Kingdom/Phylum/Class/Order/Family/Genus/Species (bare WoRMS
    #     taxonomy): agreed with Consensus_Kingdom/.../Consensus_Species in
    #     258-263/263 rows -- redundant with the Consensus_* columns kept
    #     below. The rare disagreement (Order, 5/263 rows in that run) is
    #     now surfaced instead via WoRMS_Consensus_Conflict_Detail (see
    #     add_worms_consensus_conflict_detail() below) rather than by carrying all
    #     7 duplicate columns on every row just in case.
    EXCLUDED_METADATA_COLS <- c(
      "Occurrence_hops_to_reference_region", "Occurrence_nearest_locality", "Parsed_MRGIDs",
      "Occurrence_NA_Reason",
      "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"
    )

    # Round 47 (2026-10-01): Module F now hands Module G ONE general output
    # (every column it produces, nothing curated down -- see
    # shared$modF_data in mod_f_enrichment.R), so "essential columns" no
    # longer means a hand-maintained whitelist that can silently fall behind
    # as new columns are added upstream. It is every column Module F
    # produced MINUS the confirmed-dead/redundant ones in
    # EXCLUDED_METADATA_COLS above, reordered with Taxon_id and the real
    # sample columns first.
    get_essential_cols <- reactive({
        req(input_data())
        all_cols <- setdiff(names(input_data()), EXCLUDED_METADATA_COLS)
        c("Taxon_id", sample_cols(), setdiff(all_cols, c("Taxon_id", sample_cols())))
    })

    get_essential_cols_with_reason <- reactive({
        unique(c(get_essential_cols(), "Unvalidated_Reason", "WoRMS_Consensus_Conflict_Detail"))
    })

    # Round 49 (2026-10-01): replaces the 7 bare taxonomy columns dropped
    # above. Compares each bare WoRMS rank against its Consensus_* sibling
    # (both of which are still present on `df` at this point -- only the
    # bare columns are excluded from the final SELECT, not from the data
    # itself) and reports only the ranks that actually disagree, so a real
    # mismatch stays visible without duplicating all 7 columns on every row.
    # Empty string (not NA) when every available rank pair agrees.
    #
    # NOT the same signal as the pre-existing `taxonomy_conflict` column
    # (boolean, produced in Module E's aggregation -- see global.R's
    # resolved$taxonomy_conflict assignment): that one flags when multiple
    # ASVs merged into one taxon disagreed with EACH OTHER on their input
    # taxonomy, entirely before WoRMS is consulted. This one flags when
    # WoRMS's own returned taxonomy for the matched AphiaID disagrees with
    # that already-merged Consensus_* value. Named WoRMS_Consensus_Conflict
    # (not Taxonomy_Conflict) specifically so it can't be confused with the
    # Module E field when both travel in the same exported row -- raised by
    # direct user request after comparing the real column lists of Module
    # E/F/G output side by side.
    add_worms_consensus_conflict_detail <- function(df) {
      rank_pairs <- list(
        c("Kingdom", "Consensus_Kingdom"), c("Phylum", "Consensus_Phylum"),
        c("Class", "Consensus_Class"), c("Order", "Consensus_Order"),
        c("Family", "Consensus_Family"), c("Genus", "Consensus_Genus"),
        c("Species", "Consensus_Species")
      )
      detail <- rep("", nrow(df))
      for (pair in rank_pairs) {
        bare_col <- pair[1]; cons_col <- pair[2]
        if (!all(c(bare_col, cons_col) %in% names(df))) next
        bare_v <- trimws(as.character(df[[bare_col]]))
        cons_v <- trimws(as.character(df[[cons_col]]))
        # Round 59: Module F now keeps a subspecies as the subspecies, so its
        # bare WoRMS `Species` is the trinomial ("Genus species subspecies")
        # while Consensus_Species -- built from the 7 standard ranks -- is the
        # species-rank binomial. Compare like with like: for a subspecies row,
        # use the species-rank part (first two words) of the WoRMS name, so the
        # subspecies is not reported as a conflict with its own species.
        if (bare_col == "Species" && "WoRMS_Rank" %in% names(df)) {
          is_sub <- !is.na(df$WoRMS_Rank) & df$WoRMS_Rank == "Subspecies" & !is.na(bare_v)
          bare_v[is_sub] <- vapply(strsplit(bare_v[is_sub], "\\s+"), function(w) paste(utils::head(w, 2), collapse = " "), character(1))
        }
        mismatch <- !is.na(bare_v) & !is.na(cons_v) & nzchar(bare_v) & nzchar(cons_v) & bare_v != cons_v
        if (any(mismatch)) {
          detail[mismatch] <- paste0(detail[mismatch],
                                      sprintf("%s: WoRMS=%s vs Consensus=%s; ",
                                              bare_col, bare_v[mismatch], cons_v[mismatch]))
        }
      }
      df$WoRMS_Consensus_Conflict_Detail <- detail
      df
    }

    # Round 47 (2026-10-01): "has a binomial name" is tested directly against
    # the Consensus_Species VALUE (genus + epithet = two whitespace-separated
    # tokens) rather than against the WoRMS_Rank label, per direct user
    # request. This is deliberately more defensive than trusting WoRMS_Rank
    # == "Species"/"Subspecies": this engagement has already hit real cases
    # where a resolved name collapses to a bare genus (e.g. "Spio sp.
    # LK-2011-1" -> genus "Spio" with no species epithet) or to a
    # higher-rank taxon (e.g. "Polychaeta sp." -> class Polychaeta) despite
    # superficially looking like a species-level input -- checking the
    # actual string is a direct, inspectable test that doesn't depend on
    # WoRMS_Rank having been populated correctly for every row.
    is_binomial_name <- function(x) {
      !is.na(x) & grepl("^\\S+\\s+\\S+", trimws(x))
    }

    # Unvalidated_Reason computation, shared by the Unvalidated view/download
    # AND (added per user request) the main Validated Table view/download, so
    # every row's reason travels with the main export instead of only the
    # filtered unvalidated subset. Rows with no mismatch and full DB coverage
    # get an empty string, not NA.
    add_unvalidated_reason <- function(df) {
      df %>%
        mutate(
          Unvalidated_Reason = paste0(
            ifelse(!is.na(Group1_Validated) & Group1_Validated == FALSE, paste0("Group1 mismatch (Pipe:", Group1, ", DB:", DB_Group1, "); "), ""),
            ifelse(!is.na(Group2_Validated) & Group2_Validated == FALSE, paste0("Group2 mismatch (Pipe:", Group2, ", DB:", DB_Group2, "); "), ""),
            ifelse(!is.na(TaxGroup_Validated) & TaxGroup_Validated == FALSE, paste0("TaxGroup mismatch (Pipe:", TaxGroup, ", DB:", DB_TaxGroup, "); "), ""),
            ifelse(is.na(Group1_Validated) | is.na(DB_Group1), "Group1: no DB data; ", ""),
            ifelse(is.na(Group2_Validated) | is.na(DB_Group2), "Group2: no DB data; ", ""),
            ifelse(is.na(TaxGroup_Validated) | is.na(DB_TaxGroup), "TaxGroup: no DB data; ", "")
          )
        )
    }

    # Write the module log whenever the input table (re)appears -- this
    # module has no explicit Run button, so there's no single "run finished"
    # moment to hook the way Modules A-F do; the log is regenerated each
    # time Module F's output changes.
    observeEvent(input_data(), {
      req(input_data())
      df <- input_data() %>% add_unvalidated_reason() %>% add_worms_consensus_conflict_detail()
      n <- nrow(df)
      n_worms_conflict <- sum(nzchar(df$WoRMS_Consensus_Conflict_Detail))
      # Round 49: Module E's own ASV-merge conflict flag, surfaced alongside
      # (not replaced by) the WoRMS-vs-consensus one above -- the two catch
      # different failure points (see add_worms_consensus_conflict_detail()).
      n_asv_merge_conflict <- if ("taxonomy_conflict" %in% names(df)) sum(df$taxonomy_conflict %in% TRUE) else NA

      # Round 47 (2026-10-01): taxon-count consistency check, per direct user
      # request ("always check internally that the number of taxon/species
      # match from one input to another output" / "there should not be any
      # discrepancies or inconsistencies"). shared$modF_taxon_count is
      # recorded independently in mod_f_enrichment.R at the moment Module F
      # finished, so this compares two separately-computed numbers rather
      # than re-deriving the same one twice. Module G no longer filters
      # anything for its main table, so these must always match; a mismatch
      # here means something upstream changed shared$modF_data after Module
      # F wrote it, which should never happen.
      if (!is.null(shared$modF_taxon_count) && n != shared$modF_taxon_count) {
        warning(sprintf(
          paste0("Module G taxon-count mismatch: Module F reported %d taxa, but Module G's ",
                 "Validated Table has %d. These should always be identical -- investigate ",
                 "before trusting any of this run's exports."),
          shared$modF_taxon_count, n
        ))
      }

      # Round 47 (2026-10-01): computed with the EXACT SAME filter
      # expressions as dl_species_f/dl_genus_f below, so this check can never
      # quietly drift out of sync with what the downloads actually do --
      # "always ensure internally that numbers match 100%" means verifying
      # the real nesting (Species Table <= Genus Table <= Validated Table),
      # not just eyeballing that it looks plausible.
      n_species <- if ("Consensus_Species" %in% names(df)) {
        sum(is_binomial_name(df$Consensus_Species))
      } else NA
      n_genus <- if (all(c("Consensus_Genus", "Consensus_Species") %in% names(df))) {
        sum(!is.na(df$Consensus_Genus) | is_binomial_name(df$Consensus_Species))
      } else NA
      if (!is.na(n_species) && !is.na(n_genus) && !(n_species <= n_genus && n_genus <= n)) {
        warning(sprintf(
          paste0("Module G rank-nesting violated: Species Table = %d, Genus Table = %d, ",
                 "Validated Table = %d. Species Table must be <= Genus Table <= Validated ",
                 "Table at all times (every species-resolved taxon also has a genus, and ",
                 "every taxon is in the full table) -- investigate before trusting these exports."),
          n_species, n_genus, n
        ))
      }

      g1_ok  <- if ("Group1_Validated" %in% names(df)) sum(df$Group1_Validated %in% TRUE) else NA
      g1_bad <- if ("Group1_Validated" %in% names(df)) sum(df$Group1_Validated %in% FALSE) else NA
      g2_ok  <- if ("Group2_Validated" %in% names(df)) sum(df$Group2_Validated %in% TRUE) else NA
      g2_bad <- if ("Group2_Validated" %in% names(df)) sum(df$Group2_Validated %in% FALSE) else NA
      tg_ok  <- if ("TaxGroup_Validated" %in% names(df)) sum(df$TaxGroup_Validated %in% TRUE) else NA
      tg_bad <- if ("TaxGroup_Validated" %in% names(df)) sum(df$TaxGroup_Validated %in% FALSE) else NA

      any_bad <- rep(FALSE, n)
      if ("Group1_Validated" %in% names(df)) any_bad <- any_bad | (df$Group1_Validated %in% FALSE)
      if ("Group2_Validated" %in% names(df)) any_bad <- any_bad | (df$Group2_Validated %in% FALSE)
      if ("TaxGroup_Validated" %in% names(df)) any_bad <- any_bad | (df$TaxGroup_Validated %in% FALSE)
      n_bad <- sum(any_bad, na.rm = TRUE)

      no_db <- if (all(c("Group1_Validated", "Group2_Validated", "TaxGroup_Validated") %in% names(df))) {
        sum(is.na(df$Group1_Validated) & is.na(df$Group2_Validated) & is.na(df$TaxGroup_Validated))
      } else NA

      log_paths <- write_module_log(
        project_id = shared$project_name,
        module_id = "modG",
        module_title = "Module G -- Occurrence & Trait Audit (Validation)",
        summary = list(
          "Taxa in (aggregated rows)"       = n,
          "Module F taxon count (cross-check)" = sprintf(
            "%s%s", shared$modF_taxon_count %||% "n/a",
            if (!is.null(shared$modF_taxon_count) && shared$modF_taxon_count == n) " (matches)" else if (!is.null(shared$modF_taxon_count)) " (MISMATCH)" else ""
          ),
          "Validated Table / Genus Table / Species Table" = sprintf("%d / %d / %d", n, n_genus, n_species),
          "Taxa with WoRMS-vs-Consensus taxonomy conflict" = n_worms_conflict,
          "Taxa with ASV-merge taxonomy conflict (Module E)" = n_asv_merge_conflict,
          "DB coverage (Group1)"            = if ("DB_Group1" %in% names(df)) sum(!is.na(df$DB_Group1)) else NA,
          "Group1 validated / mismatched"   = sprintf("%s OK / %s mismatches", g1_ok, g1_bad),
          "Group2 validated / mismatched"   = sprintf("%s OK / %s mismatches", g2_ok, g2_bad),
          "TaxGroup validated / mismatched" = sprintf("%s OK / %s mismatches", tg_ok, tg_bad),
          "Taxa with any mismatch"          = n_bad,
          "Taxa with no trait DB data"      = no_db
        ),
        notes = character(0)
      )
      log_rv$txt_path <- log_paths$txt
      log_rv$csv_path <- log_paths$csv
    })

    # Summary
    output$summary_text <- renderPrint({
      req(input_data())
      df <- input_data()
      n <- nrow(df)

      # Aggregation always runs before this module now, so the table here is
      # always one row per resolved consensus taxon, never raw per-ASV data.
      row_label <- "taxa (aggregated)"
      cat("=== QUALITY CHECK SUMMARY ===\n\n")
      cat(sprintf("Total %s in validated table: %d\n\n", row_label, n))

      # Round 47 (2026-10-01): visible on-screen, not just in the downloadable
      # log, so a mismatch is obvious without opening a file -- "always
      # ensure internally that numbers match 100%" per direct user request.
      cat("--- Output consistency check ---\n")
      n_species_chk <- if ("Consensus_Species" %in% names(df)) sum(is_binomial_name(df$Consensus_Species)) else NA
      n_genus_chk <- if (all(c("Consensus_Genus", "Consensus_Species") %in% names(df))) {
        sum(!is.na(df$Consensus_Genus) | is_binomial_name(df$Consensus_Species))
      } else NA
      cat(sprintf("  Validated Table (all taxa, all ranks): %d\n", n))
      cat(sprintf("  Genus Table (has a genus, incl. species): %d\n", n_genus_chk))
      cat(sprintf("  Species Table (has a binomial name):      %d\n", n_species_chk))
      nesting_ok <- !is.na(n_species_chk) && !is.na(n_genus_chk) &&
        n_species_chk <= n_genus_chk && n_genus_chk <= n
      cat(sprintf("  Nesting (Species <= Genus <= Validated):  %s\n", if (nesting_ok) "OK" else "MISMATCH -- see module log"))
      if (!is.null(shared$modF_taxon_count)) {
        match_label <- if (shared$modF_taxon_count == n) "OK" else "MISMATCH -- see module log"
        cat(sprintf("  Module F -> Module G taxon count (%d -> %d): %s\n", shared$modF_taxon_count, n, match_label))
      }
      n_worms_conflict_chk <- sum(nzchar(add_worms_consensus_conflict_detail(df)$WoRMS_Consensus_Conflict_Detail))
      cat(sprintf("  Taxa with WoRMS-vs-Consensus conflict (see WoRMS_Consensus_Conflict_Detail): %d\n", n_worms_conflict_chk))
      if ("taxonomy_conflict" %in% names(df)) {
        cat(sprintf("  Taxa with ASV-merge conflict (Module E's taxonomy_conflict, a different check): %d\n",
                    sum(df$taxonomy_conflict %in% TRUE)))
      }
      cat("\n")

      cat("--- Trait DB validation (MarineSpeciesTraits) ---\n")
      if ("DB_Group1" %in% names(df)) {
        db_cov <- sum(!is.na(df$DB_Group1))
        g1_ok  <- sum(df$Group1_Validated %in% TRUE)
        g1_bad <- sum(df$Group1_Validated %in% FALSE)
        g2_ok  <- sum(df$Group2_Validated %in% TRUE)
        g2_bad <- sum(df$Group2_Validated %in% FALSE)
        tg_ok  <- sum(df$TaxGroup_Validated %in% TRUE)
        tg_bad <- sum(df$TaxGroup_Validated %in% FALSE)
        cat(sprintf("  DB coverage (Group1):   %d / %d (%.1f%%)\n", db_cov, n, pct(db_cov, n)))
        cat(sprintf("  Group1 validated:       %d OK, %d mismatches\n", g1_ok, g1_bad))
        cat(sprintf("  Group2 validated:       %d OK, %d mismatches\n", g2_ok, g2_bad))
        cat(sprintf("  TaxGroup validated:     %d OK, %d mismatches\n", tg_ok, tg_bad))
      }

      cat(sprintf("\n--- Unvalidated %s ---\n", row_label))
      any_bad <- rep(FALSE, n)
      if ("Group1_Validated" %in% names(df))
        any_bad <- any_bad | (df$Group1_Validated %in% FALSE)
      if ("Group2_Validated" %in% names(df))
        any_bad <- any_bad | (df$Group2_Validated %in% FALSE)
      if ("TaxGroup_Validated" %in% names(df))
        any_bad <- any_bad | (df$TaxGroup_Validated %in% FALSE)
      n_bad <- sum(any_bad, na.rm = TRUE)
      cat(sprintf("  %s with any mismatch: %d (%.1f%%)\n", row_label, n_bad, pct(n_bad, n)))
      if (all(c("Group1_Validated", "Group2_Validated", "TaxGroup_Validated") %in% names(df))) {
        no_db <- is.na(df$Group1_Validated) & is.na(df$Group2_Validated) & is.na(df$TaxGroup_Validated)
        cat(sprintf("  %s with no trait DB data: %d (%.1f%%)\n", row_label, sum(no_db), pct(sum(no_db), n)))
      }
    })

    # Conflict Table
    output$conflict_table <- renderDT({
      req(input_data())
      
      df <- input_data()

      # Audit logic: Filter for conflicts
      df_audit <- df %>%
          add_unvalidated_reason() %>%
          add_worms_consensus_conflict_detail() %>%
          filter(
            (!is.na(Group1_Validated) & Group1_Validated == FALSE) |
            (!is.na(Group2_Validated) & Group2_Validated == FALSE) |
            (!is.na(TaxGroup_Validated) & TaxGroup_Validated == FALSE) |
            (is.na(Group1_Validated) & is.na(Group2_Validated) & is.na(TaxGroup_Validated))
          ) %>%
          select(any_of(get_essential_cols_with_reason()))

      datatable(df_audit, options = list(pageLength = 10, scrollX = TRUE), filter = 'top')
    })

    # Full Validated Table — includes Unvalidated_Reason (added per user
    # request) for every row, not just the filtered Unvalidated subset, so a
    # reviewer working from this one export can see why any row didn't
    # validate without also downloading the separate Unvalidated file.
    output$full_table <- renderDT({
      req(input_data())

      df <- input_data() %>% add_unvalidated_reason() %>% add_worms_consensus_conflict_detail()

      datatable(df %>% select(any_of(get_essential_cols_with_reason())),
                options = list(pageLength = 10, scrollX = TRUE),
                filter = 'top')
    })

    # Download handler for Full Table
    output$dl_full <- downloadHandler(
        filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modG", "validated_table"),
        content = function(file) {
            req(input_data())

            df <- input_data()

            # Ensure essential columns exist, even if empty
            if (!("DB_Group1" %in% names(df))) df$DB_Group1 <- NA_character_
            if (!("DB_Group2" %in% names(df))) df$DB_Group2 <- NA_character_
            if (!("DB_TaxGroup" %in% names(df))) df$DB_TaxGroup <- NA_character_

            df <- df %>% add_unvalidated_reason() %>% add_worms_consensus_conflict_detail()

            write_csv(df %>% select(any_of(get_essential_cols_with_reason())), file)
        }
    )

    # NOTE (2026-09-11): a separate "Download Unvalidated" handler (dl_conflicts)
    # used to live here, exporting the same columns as dl_full above pre-filtered
    # to just the mismatched/no-DB-data rows. Removed as redundant: dl_full above
    # already includes every row (validated and unvalidated together) with
    # Unvalidated_Reason populated for each, so the same subset is one column
    # filter away in the downloaded file (or use the "Unvalidated" tab above to
    # review it on-screen first). The conflict_table/"Unvalidated" tab and its
    # underlying filter logic are unchanged — only the duplicate download was
    # removed.

    # Download handler for Species Table -- Round 47 (2026-10-01): per direct
    # user request, a taxon qualifies by actually HAVING a binomial name
    # (Consensus_Species = "Genus epithet", two tokens), not by its
    # WoRMS_Rank label. Same column set as dl_full.
    output$dl_species_f <- downloadHandler(
        filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modG", "species_level"),
        content = function(file) {
            req(input_data())
            df <- input_data() %>%
              filter(is_binomial_name(Consensus_Species)) %>%
              add_unvalidated_reason() %>%
              add_worms_consensus_conflict_detail()
            write_csv(df %>% select(any_of(get_essential_cols_with_reason())), file)
        }
    )

    # Download handler for Genus Table -- Round 47 (2026-10-01): per direct
    # user request, every taxon with a resolved genus, INCLUDING
    # species-level taxa (a taxon with a binomial name has a genus too) --
    # not "genus rank and coarser only" as it was before. So this is a
    # superset of the Species Table above, not a disjoint rank bucket:
    # Species Table ⊆ Genus Table ⊆ Validated Table (dl_full).
    output$dl_genus_f <- downloadHandler(
        filename = function() generate_consistent_filename(shared$project_name %||% "Project", "modG", "genus_level"),
        content = function(file) {
            req(input_data())
            df <- input_data() %>%
              filter(!is.na(Consensus_Genus) | is_binomial_name(Consensus_Species)) %>%
              add_unvalidated_reason() %>%
              add_worms_consensus_conflict_detail()
            write_csv(df %>% select(any_of(get_essential_cols_with_reason())), file)
        }
    )

    output$dl_log_txt <- downloadHandler(
      filename = function() basename(log_rv$txt_path %||% "module_log.txt"),
      content = function(file) { req(log_rv$txt_path); file.copy(log_rv$txt_path, file) }
    )
    output$dl_log_csv <- downloadHandler(
      filename = function() basename(log_rv$csv_path %||% "module_log.csv"),
      content = function(file) { req(log_rv$csv_path); file.copy(log_rv$csv_path, file) }
    )

  })
}
