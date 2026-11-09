# TaxaCurate — Post-Taxonomic Assignment Species Curation

**Standardized. Reproducible. Transparent.**

**Version 1.0.0**

---


## 1. Introduction

TaxaCurate is a Shiny-based workflow that transforms raw taxonomic assignments from three independent reference databases (NCBI-BLAST, BOLD, MZGdb) into a high-confidence marine species inventory. It performs dual contaminant removal using negative-control samples and enriches the final ASV table with distribution, ecological trait, and vernacular-name data from the World Register of Marine Species (WoRMS).

Seven sequential modules run in order, each presented as a dedicated tab. The module letters used in the source files (A–G) correspond to tabs 1–7, as shown in the table below. Aggregation runs before ecological enrichment, so that enrichment and validation each do one lookup per resolved taxon instead of one per ASV:

| # | Nav pill | Internal module | Description |
|---|---|---|---|
| 1 | Standardization | Module A | Upload & resolve taxon names against WoRMS |
| 2 | Agreement Analysis | Module B | Compare AphiaIDs, resolve conflicts |
| 3 | Merging | Module C | Merge reads + standardized taxonomy |
| 4 | Decontamination | Module D | Flag and remove contaminant ASVs via blank/control samples |
| 5 | Aggregation | Module E | Collapse ASVs sharing the same resolved consensus taxon into one row each, summing sample counts, before enrichment runs |
| 6 | Ecological Annotation | Module F | Full WoRMS profiles, occurrence classification, ecological annotation, for every resolved taxon from Module E |
| 7 | Quality Check | Module G | Validate Module F's taxonomy/occurrence/traits; the pipeline's 3 final downloadable exports (Validated/Species/Genus Table) come from here |

### Contact

kingsly-chuo.beng@thuenen.de

---

## 2. Getting Started

### 2.1 Opening the app

Open `app.R` in RStudio and click **Run App**, or from a terminal:

```r
shiny::runApp("path/to/taxacurate")
```

The app loads with seven navigation tabs arranged sequentially. Tabs 1–6 each have an inputs panel with a run button, a status panel, a results display and a **Cancel** button to stop long-running operations. Tab 7 (Quality Check) has no run button and no Cancel button: it recomputes automatically from the output of tab 6.

### 2.2 App layout

- **Header** — TaxaCurate logo (left), app title & subtitle (center), partner logos CREATE / sustainMare / AWI / HIFMB / Thünen (right). The logo image files are not part of the code release: whenever a logo file is missing from the `www/` folder, the app shows the organisation's name as text in its place and works normally.
- **Navigation pills** — one per module, each with its own colour taken from a colourblind-safe (Okabe–Ito-based) palette.
- **Module content** — inputs, run button, cancel button, progress bar, results (tab 7 has no run or cancel button).
- **Footer** — "Developed by" section (left) with the Thünen Institute logo (or name), a link to the Thünen legal notice and a short institute description; "Funded by" section (right) with the BMFTR and DAM logos (or names).

### 2.3 Example dataset

Module 1 has a **Load example dataset** button, and Module 4 has a matching **Use example metadata** button; the other modules have no example button because they work on the output of the previous module. The bundled example (folder `examples/`) has 21 ASVs: an MZG file with read counts for 13 sample columns (4 blanks and 9 samples), a BLAST file (19 rows) and a BOLD file (19 rows). It consists mostly of North Sea taxa and includes 4 realistic lab/reagent contaminants (*Homo sapiens*, *Sus scrofa*, *Gallus gallus*, *Escherichia coli*). The example metadata file lists the 4 blanks (one of them labelled with the spelling variant "blanc", which the app accepts) and the 9 samples; one sample (`Sample_J`) has no reads at all. Loading the example runs Module 1 on these files, so it needs an internet connection (WoRMS). Use the example to explore the full pipeline before uploading real data.

---

## 3. Requirements & Setup

### 3.1 R packages

**R 4.3 or newer** is recommended (R 4.4.2 was used for the release), together with these packages (`Rscript install_dependencies.R` installs them):

```r
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "stringr", "readr",
                   "purrr", "tibble", "ggplot2", "DT", "worrms", "future", "promises",
                   "shinyjs", "httr2", "jsonlite", "data.table"))

# Module 4 only:
install.packages("BiocManager")
BiocManager::install(c("decontam", "phyloseq"))
# install.packages("remotes"); remotes::install_github("donaldtmcknight/microDecon")
```

Missing decontamination packages don't prevent the app from loading — they produce a clear error only if you try to use Module 4 without them.

### 3.2 Input files

| Source | Format |
|---|---|
| BLAST | Tab-delimited text with **no header row** and 18 columns in this order: ASV_id, Accession, E_value, Percent_id, Ref_len, Query_len, four taxon-id columns, then Domain, Kingdom, Phylum, Class, Order, Family, Genus, Species. If an ASV has several hits, the hit with the lowest E-value (then the highest percent identity) is kept. |
| BOLD | Tab-delimited text with **no header row** and 2 columns: ASV_id and a taxonomy string with rank prefixes (`k:`, `p:`, `c:`, `o:`, `f:`, `g:`, `s:`), for example `k:Animalia(1.00),p:Mollusca(1.00),...`. The confidence values in parentheses are ignored; if several assignments are joined with `+`, the first one is used. |
| MZG | Comma-separated file (.csv) with a header row. The **last seven columns must be the taxonomy columns in the order Kingdom, Phylum, Class, Order, Family, Genus, Species**; the columns before them hold the per-sample read counts, an optional ASV identifier and an optional sequence. An identifier column named `ASV_id`, `ASV`, `OTU_id`, `OTU` or `id` is used when it is complete and unique; otherwise the ASVs are numbered by row (`ASV_1`, `ASV_2`, ...). A column named `Sequence` (also accepted: `seq`, `nucleotide(s)`, `nuc`, `barcode`, `rep_seq`, `asv_sequence`, `otu_sequence`) is used to compute `Sequence_length_bp`. |
| Identifiers | ASV identifiers without the prefix `ASV_` (for example `2`) get that prefix added when the files are read, so that BLAST, BOLD and MZG can be joined on `ASV_id`. |
| Metadata (Module 4) | Tab-delimited text (.txt or .tsv) with a header row, one column whose name contains "sample" and "id" (for example `Sample_ID`) and one whose name contains "sample" and "type" (for example `Sample_Type`). The `Sample_Type` values Blank, Negative, Control, NTC and blanc (in any capitalisation) mark negative controls; every other value marks a true sample. The sample names must match the column names of the ASV table. |

---

## 4. Module 1 — Standardization (Taxonomic Standardization)

### 4.1 What it does

Uploads raw taxonomy files from BLAST, BOLD, and/or MZG. Each dataset's own row is resolved as a unit: the row is anchored on its single most-specific resolvable name, and all 7 ranks (Kingdom through Species) are filled in from that ONE matched WoRMS record's lineage, with adaptive cleaning, fuzzy matching, genus-level fallback, and homonym disambiguation applied once at that anchor point.

### 4.2 WoRMS query cascade

For a row's most-specific non-blank name (Species first; if blank, Genus; and so on up to Kingdom), the resolver first cleans the name and then climbs a ladder of candidates.

**Cleaning** (applied in this order, in one pass):

1. **Basic cleaning** — underscores → spaces, whitespace trimmed, leading *uncultured*, *unclassified*, *unknown*, *metagenome* or *candidatus* removed
2. **Suffix removal** — strip *sp.*, *spp.* and anything from *cf.*, *aff.* or *nr.* onwards
3. **Code stripping** — remove trailing upper-case/numeric codes and trailing numbers

**Ladder** (the first rung that gives an accepted WoRMS match wins; each rung is searched as an exact name first and, only if that returns nothing, as a fuzzy match):

1. **The cleaned name at its own rank**
2. **Genus-only fallback** — the first word of the cleaned name (Species rows only)
3. **Higher-rank fallback** — climb this same row's own higher-rank columns, one rank at a time, up to Kingdom

Once a match is found, its full lineage (Kingdom..Species, each with its own true AphiaID from WoRMS's classification tree) is written into all 7 columns in one pass, rather than each column separately climbing this same ladder from its own raw value. Synonyms are handled automatically: if the matched record's status != `accepted`, `valid_AphiaID` is returned.

**Row-anchored resolution:** anchoring on one name and filling in the rest from its lineage means a single row can never combine ranks from unrelated WoRMS records (for example a Phylum from one organism next to a Species from another), which independent per-column lookups can produce. BLAST, BOLD, and MZG each still anchor independently, so a genuine disagreement *between* datasets at any rank is still caught (see §5). A row with no information at all beyond, say, Genus correctly leaves Species blank rather than fabricating one; a rank the matched record's own WoRMS classification has no entry for (some free-living flatworm genera have no populated Class — see §9.5) is also left blank rather than carrying over a stale value.

**Non-marine contaminants and homonym handling:** every WoRMS query here runs with `marine_only = FALSE` — so a genuinely non-marine organism (a lab/reagent contaminant such as *Sus scrofa*, *Homo sapiens*, *Gallus gallus*, as in the example dataset) resolves to its own correct identity rather than being excluded from matching and falling back to an unrelated marine "best available" guess. Separately, when a WoRMS candidate's kingdom/phylum disagrees with the row's own other columns (a possible homonym — e.g. *Morus* the mulberry genus in Plantae vs. *Morus* the seabird genus in Animalia), that match is refused rather than accepted anyway, and the resolver falls back to a coarser rank exactly as it would for an empty WoRMS result.

**Exact species-level matches are exempt from the disagreement check:** a full species binomial essentially never collides across kingdoms the way a bare genus or family word can, so an exact match that returns exactly one candidate at Species rank is accepted outright, without consulting the row's other columns. This matters when a row's other raw columns are themselves wrong (not just blank) — for example when the raw Species resolves to a real, unambiguous species but the raw Phylum names something else. A fuzzy match, a match at Genus rank or coarser, and an exact match returning multiple candidates still go through the full check, which keeps the homonym protection (*Morus*, *Prunella*) intact.

**Retired names are replaced by the accepted name:** if WoRMS returns a synonym (an older combination), Module 1 fetches the accepted record and writes its name and lineage, together with its AphiaID — so a row never shows an old name beside a new name's ID. The `matched_name` still shows what the classifier originally reported. If the accepted record can't be retrieved, the retired record's name is kept and only the AphiaID is the accepted one.

**Refused matches stay visible for Module 2's comparison:** a context-disagreeing match is kept out of this module's own result for that column, but its AphiaID is kept in memory, separate from every table (it is not a column in any preview or download), and is handed to Module 2 only so it can count it as a disagreeing data point in its agreement statistics (see §5.2). Without this, a database that found something that was refused would look identical to a database that made no call at all. This module's own trusted output is unaffected: the refused value never appears in `<Level>_AphiaID` or anywhere else in its tables.

### 4.3 Confidence tiers

`exact_species`, `fuzzy_species`, `exact_genus`, `fuzzy_genus`, `higher_rank_fallback`, `unresolved`

### 4.4 Outputs

- Three per-database AphiaID tables (BLAST, BOLD, MZG; downloadable CSV) — each database's own row is internally consistent (one matched WoRMS record's lineage), not independently resolved columns that could disagree with each other within the same row. No refused-match columns are written to these tables; that information is kept internally for Module 2's agreement statistics.
- WoRMS cache (CSV, reusable in future sessions through "Reuse a previous WoRMS cache")
- Module log (.txt and .csv)
- Summary: match rates, confidence distribution, and a count of rows where a match was refused for context disagreement at each rank — read from the internal refused-match table (not a column of any output).

---

## 5. Module 2 — Agreement Analysis (Cross-database Agreement)

### 5.1 What it does

Merges the AphiaID tables of the databases that were loaded (at least two are required) on ASV_id and computes per-rank agreement statistics. A rank-by-rank fallback then assigns each ASV one consensus AphiaID (`Species_fallback`): starting at Species and moving up towards Kingdom, the first rank at which at least two databases give the same AphiaID is used (§5.3). All ASVs (including disagreeing ones where databases conflict) are passed downstream — the filtering into "All", "Agreeing", and "Disagreeing" sets happens in Module 3.

### 5.2 Agreement categories

- **All agree** (column `All_three_agree`) — every loaded database made a call and all calls are the same non-zero AphiaID
- **Two agree** — with three databases loaded: two share an AphiaID and the third differs or made no call
- **None agree** — at least two databases made a call and no two of the calls are the same
- **Only one database** — exactly one database made a call at that rank, so there is nothing to compare it with. These are counted separately, so the four categories add up to the number of ASVs with data.

For every level, All + Two + None + Only one database = **Total_with_data** (ASVs where at least one database made a call at that rank). The table also gives each category as a percentage of Total_with_data (`Percent_all_agree`, `Percent_two_agree`, `Percent_none_agree`, `Percent_only_one_db`; they sum to 100 within ±0.02 because each is rounded to two decimals). ASVs where no database made a call at a level are in none of the categories. With only two databases loaded, "Two agree" is always 0.

Visualised as 100% stacked bars (green/blue/red, plus grey for "Only one database").

**Refused matches count here too:** when a database's own best candidate at a rank was refused for disagreeing with that row's other columns (Module 1, §4.2), its identity is included in this comparison as that database's contributed value, exactly as if it had been accepted. A refused value that matches the other databases strengthens agreement instead of making it look as if only two of three databases reported anything; a refused value that genuinely differs counts as a disagreement. This only changes what feeds the comparison above — it has no effect on §5.3's Species fallback or on which AphiaID gets trusted downstream, both of which read only accepted matches.

### 5.3 Species fallback

Walks ranks Species → Kingdom. At each rank, if at least two databases share the same AphiaID, that AphiaID is assigned as `Species_fallback` (despite the name it can be the AphiaID of any rank), the rank is recorded in `Fallback_rank`, and the agreeing databases are recorded in `Species_fallback_source`, sorted alphabetically and joined with "+" (for example `BLAST+MZG`; `None` if no two databases ever agree at any rank). The result is deterministic: running the same data again gives the same source.

Module 2 then re-queries WoRMS for the lineage of each consensus AphiaID, writes it into all ranks at and above the fallback rank, and empties the ranks below it (so an ASV whose databases agree only at Genus has no species name). Finally the per-database taxonomy columns are collapsed into `Consensus_<Rank>` columns (`Consensus_Kingdom` … `Consensus_Species`, plus the matching `_AphiaID` columns), which are filled only where at least two databases gave a value and the values agree.

### 5.4 Outputs

- Agreement summary table (per rank: counts and percentages)
- Species conflicts table (ASVs for which at least two databases gave a Species AphiaID and no two of them agree)
- Combined table with fallback assignments
- Three plots: agreement bars (100% stacked bars per rank), fallback source (bar chart of ASV counts per source combination) and fallback rank (ASV counts per rank)
- Tables downloadable as CSV; module log (.txt and .csv)


---

## 6. Module 3 — Merging (Final ASV Table Assembly)

### 6.1 What it does

Builds the final merged ASV table: sample read counts from the MZG backbone you uploaded in Module 1, plus consensus taxonomy and species fallback from Module 2's combined table. Optionally you can upload a second MZG backbone file (for example a re-export with corrected read counts) and, with it, a metadata file; the second file's read counts are then used, but only after the app has confirmed that both files describe exactly the same set of ASVs (a mismatch is refused). Module 3 also computes `Sequence_length_bp` from the sequence column when there is one.

### 6.2 Three-way split

The merged table is presented in three views:

- **All ASVs** — every ASV from the MZG backbone with taxonomy attached
- **Agreeing ASVs** — only ASVs for which at least two databases agreed on an AphiaID at some rank (`Species_fallback_source != "None"`, see §5.3); this set is passed downstream as `shared$modC_agreed` and is the only set that continues to Module 4
- **Disagreeing ASVs** — ASVs for which no two databases agreed at any rank, so no fallback could be assigned (`Species_fallback_source == "None"`)

Each tab has its own download button ("Download agreeing ASVs", "Download all ASVs", "Download disagreeing ASVs"). A Summary tab and a Column selection tab (columns can be deselected before download; diagnostic columns are deselected by default) complete the module.

### 6.3 Outputs

- Three downloadable CSVs: agreeing, all, and disagreeing ASVs; module log (.txt and .csv)
- Summary: ASV counts per category, column count, sample columns, sequence length stats

---

## 7. Module 4 — Decontamination (Contaminant Detection & Removal)

### 7.1 What it does

Applies two complementary decontamination methods to the ASV table using negative-control samples identified in the metadata file. The input is the **Agreeing ASVs** table from Module 3; a metadata file (§3.2) is uploaded in this module, or the example metadata is used.

### 7.2 Methods

- **decontam (prevalence)** — compares how often an ASV occurs in blanks and in true samples (`decontam::isContaminant`, method "prevalence") and flags ASVs whose contaminant-probability score is below the threshold (default 0.1). Needs the packages `decontam` and `phyloseq`.
- **microDecon** — runs `microDecon::decon()` on the blank and sample columns; the ASVs that microDecon removes from its output table are the ones flagged. Needs the package `microDecon`.

### 7.3 Detection modes

- decontam only
- microDecon only
- Both — require agreement (flag only if both flag; this is the default)
- Both — either flags (union: flag if either method flags)

### 7.4 Outputs

- Cleaned ASV table (with and without blank columns)
- Flagged contaminant list (per-method status)
- Summary: original/removed/remaining counts, per-method stats
- Module log (.txt and .csv)

**How the sample columns are handled.** Sample columns are the numeric columns of the table whose names do not contain any of the words Kingdom, Phylum, Class, Order, Family, Genus, Species, ASV_id, Sequence_length, AphiaID, Fallback_rank, Species_fallback or Consensus (in any capitalisation) — so please avoid these words in sample names. Columns that are not listed in the metadata are excluded from the cleaned tables and a warning lists them (nothing is deleted from your files). Samples with no reads at all, and ASVs with no reads in the remaining samples, are left out of the detection step (the all-zero sample columns stay in the cleaned tables, and Module 5 will warn about them). An error is shown when no blank or no true sample can be matched, or when the packages needed for the chosen method are not installed.


---

## 8. Module E — Aggregation

### 8.1 What it does

Collapses ASVs that share the same resolved consensus taxon into one row each, summing sample read counts. Runs **first** among Modules E/F/G, before ecological enrichment: species/genus/family identity is already established in Modules A/B and survives Decontamination unchanged, so Module E doesn't create that identity, it only looks it up. Aggregating first means Module F and Module G each do one WoRMS/OBIS/GBIF lookup per resolved taxon instead of one per ASV.

### 8.2 Grouping key

Each ASV's own resolved consensus identity — `Species_fallback`, the AphiaID Module B settled on for that ASV at whatever rank its own cross-database consensus actually reached (`Fallback_rank`). This is **not** "collapse everything to one chosen rank" — most ASVs never reach species-level consensus, and there is no rank selector in this module. Two ASVs merge only if they share that same AphiaID, regardless of which rank it sits at: an ASV resolved to species stays distinct from another ASV of the same genus that only resolved to genus level, and a family-level ASV merges only with other ASVs sharing that same Family AphiaID. ASVs with no usable consensus AphiaID are never merged with anything.

### 8.3 Inputs

Runs directly on this session's Module D (Decontamination) output by default — no upload needed when working through the pipeline in order. You can also upload one or more previously-exported clean ASV tables (CSV/TSV) instead, e.g. to combine several separate runs. If both an upload and a live Module D result are present, the uploaded file(s) take priority.

### 8.4 Taxonomic exclusions

Two comma-separated text filters, applied before the ASVs are grouped: exclude kingdoms (default `Protozoa, Bacteria, Fungi, Plantae, Chromista, Viruses`) and exclude classes (default `Arachnida, Hexapoda, Insecta`). A row is removed when its `Consensus_Kingdom` (or `Kingdom`) or its `Consensus_Class` (or `Class`) equals an entry of the respective list, ignoring capitalisation; rows with an empty value are kept. Module F has its own two filters with the same defaults, applied independently (§9.1).

### 8.5 Metadata resolution across merged ASVs

Within a group of ASVs sharing an AphiaID, taxonomy and other metadata are resolved by majority vote (most common value wins; `Body_Size_*` rounds to 6 decimal places before voting; `WoRMS_Distribution` takes the sorted union of all distinct locality entries, joined with "; ", instead of voting, since locality text should accumulate rather than pick one). A **conflict check** then flags any merged group where, among the ASVs of the group (before the voting, blank values ignored), more than one *distinct* value is present on any taxonomy rank column — since every ASV in the group shares the same AphiaID (hence the same real-world identity), any disagreement here signals a genuine data inconsistency upstream, not an expected outcome of the grouping itself. Recorded as the boolean `taxonomy_conflict` column, visible in every downstream export.

This is a **different check** from `WoRMS_Consensus_Conflict_Detail`, added later in Module G (§10.2) — `taxonomy_conflict` here catches ASVs disagreeing with each other, entirely before WoRMS is ever consulted; `WoRMS_Consensus_Conflict_Detail` catches WoRMS's own returned taxonomy disagreeing with this module's already-merged consensus. Both travel in every export from here onward, clearly labeled, and a taxon can have one, both, or neither.

Each merged row gets a `Taxon_id` (`Taxon_1`, `Taxon_2`, ...) plus `Merged_ASV_ids` (every original ASV folded into that row) and `n_asvs` (how many ASVs merged) — so a merged row's provenance stays fully traceable rather than arbitrarily inheriting one surviving ASV's own identifier.

### 8.6 Outputs & Downloads

- **Aggregated table** — one row per resolved taxon at every rank (not a single chosen rank), feeding Module F directly as its input. Needs the column `Species_fallback` (from Module 2); the module reports an error without it.
- **Conflicts table** — the same aggregated table, filtered to `taxonomy_conflict == TRUE` only.
- Summary tab (taxa, samples, total reads, merged ASVs per row, a warning for blank or zero-read sample columns, conflicts).
- Module log (.txt / .csv).

---

## 9. Module F — Ecological Annotation (Species Ecological Enrichment)

### 9.1 What it does

Queries WoRMS once per unique AphiaID for the complete taxonomic record, distribution text, ecological traits, and vernacular names. Subspecies are kept as the subspecies (they are not upgraded to the parent species). Whatever the subspecies itself lacks is borrowed from its parent species, and nothing it already has is replaced: WoRMS distribution, attributes (functional group, body size) and vernacular names (rows flagged in `WoRMS_Parent_Fallback_Used`), OBIS/GBIF presence (the parent's name is tried last, only if the subspecies and its synonyms have no records), and local trait-database values. This module never matches names against WoRMS: it enriches the AphiaIDs Module 1 resolved, and refuses a table that has none. Occurrence likelihood is classified relative to a user-chosen reference marine region using one of two selectable methods — **OBIS/GBIF + Text** (the recommended default, combining empirical occurrence evidence with WoRMS-distribution-text keyword matching) or **Text-based (Keyword)** alone — validated against OBIS and GBIF occurrence data within the reference region's real coastline geometry where available (falling back to a bounding box otherwise). See §9.4 and §9.8.

### 9.2 WoRMS queries per species

- Core record (`AphiaRecordsByAphiaIDs`, one batched request) — taxonomy, status, marine/brackish/freshwater/terrestrial flags
- Distribution (`AphiaDistributionsByAphiaID`) — locality, MRGID, area fields
- Attributes (`AphiaAttributesByAphiaID`, inherited attributes included) — ecological traits, functional group, body size
- Vernacular names (`AphiaVernacularsByAphiaID`) — user-selected languages

Queries run **in parallel, in small sub-waves** (10 AphiaIDs' worth of detail requests at a time) rather than firing every pending species' requests at once, because an unpaced all-at-once burst can trip WoRMS's own rate limiting, after which every subsequent species would come back with every `WoRMS_*` column NA for the rest of the run. Every response (the record batch included) is checked specifically for a rate-limit reply (HTTP 429/503), and the server's own requested wait time is honored when it provides one.

A genuine network or rate-limit failure is retried persistently rather than being given up on after a few tries — it keeps retrying (with each wait capped at a sane maximum, so it never waits unreasonably long between tries) for as long as WoRMS keeps failing, logging a reminder periodically so a long stall is visible rather than silent. The module's **Cancel** button (§12) reaches all the way into this retry loop, so it can still stop a run that's stuck retrying through an outage. A species whose AphiaID genuinely no longer exists in WoRMS (confirmed by a real response that simply omits it, as opposed to no response at all) is recognized immediately and is not retried.

**WoRMS synonym fallback:** when a species' own AphiaID comes back with empty distribution, attribute, or vernacular data, the pipeline retries that specific, still-empty field under the species' WoRMS synonym AphiaIDs (in the order WoRMS lists them) before giving up on it — so a species whose data lives under a synonym record instead of its currently-accepted AphiaID does not silently show NA for that field. The `WoRMS_Synonym_Fallback_Used` column (TRUE/FALSE) marks every row where this fired, and the module's summary tab (§9.9) reports a count for the run. This is separate from, and in addition to, the synonym-following that happens when resolving a species' *identity* (Module A) and when checking OBIS/GBIF presence (§9.6) — this fallback covers the WoRMS data-fetching step itself.

**Refused-match information:** a match refused for context disagreement is never written into a table. Module A keeps it in memory and passes it to Module B, which uses it only to count that database's refused call in the agreement statistics.

**Synonyms are resolved once, in Module A:** a retired name is swapped to its accepted AphiaID, name and lineage in Module A. Every later module keeps that result and does not re-resolve it: Module F fetches by the accepted AphiaID, and the synonym lookups (WoRMS data fallback, OBIS/GBIF retry, trait lookup) only borrow data from a retired name to fill gaps; they never change the row's AphiaID or name. If an AphiaID reaches Module F with a non-accepted WoRMS status, it is kept as is and a line is written to the log.

### 9.3 Vernacular language selection

Choose one or more languages from: English, Dutch, French, German, Italian, Japanese, Korean, Norwegian, Polish, Portuguese, Russian, Spanish, Swedish, Turkish, Chinese. Names appear in separate columns in the output (e.g., `WoRMS_vernacular_English`, `WoRMS_vernacular_German`).

### 9.4 Occurrence classification

Two selectable classification methods are available (radio buttons labelled "Classification method" in this module):

- **OBIS/GBIF + Text** (default, internal value `"evidence"`) — combines empirical OBIS/GBIF occurrence evidence with WoRMS-distribution-text keyword matching, with a specific precedence order; see §9.8 for the full tier table.
- **Text-based (Keyword)** — pure keyword matching against `WoRMS_Distribution` text (`classify_occurrence_by_text()`), with OBIS/GBIF only as a gap-filler for rows it can't classify at all (see the "Unknown" gap-filler note in §9.8).

**No gazetteer method.** The classification does not use the number of hops through the Marine Regions gazetteer: OBIS/GBIF occurrence records are direct empirical evidence, while a hop count is an indirect structural proxy. Marine Regions is still used for the region search and the region outlines. The columns `Occurrence_hops_to_reference_region`, `Occurrence_nearest_locality` and `Parsed_MRGIDs` are always empty (see §15), and the gazetteer cache files (§11) are not used.

### 9.5 Ecological functional groups & body size

Three columns are assigned by a 72-entry lookup table (covering 18 phyla):

- **Group1** — life-history context: holoplankton, meroplankton, benthic invertebrate, parasitic, ichthyoplankton / necton (fish), etc.
- **Group2** — ecological/habitat context: benthic invertebrate, ichthyoplankton / necton (fish), seabird, marine mammal, marine reptile, etc.
- **TaxGroup** — taxonomic label: copepod, polychaete, krill, seabird, fish, etc.

Additionally, WoRMS attributes are parsed into:
- **Body Size (Min/Mean/Max)**: Extracted from numeric attribute values.
- **Functional Group (Parsed)**: Structured hierarchical classification of functional groups.

**Fish classes:** every fish class (Actinopterygii, Teleostei, Elasmobranchii, Holocephali, Myxini, Petromyzonti) is classified with `TaxGroup` carrying the taxonomic label ("fish", matching the pattern every other row in the table follows) and `Group1`/`Group2` carrying the ecological descriptor ("ichthyoplankton / necton" — planktonic eggs/larvae, free-swimming adults), as shown above.

**Canuelloida copepods:** *Canuella perplexa* and *Longipedia coronata* (meiobenthic, harpacticoid-grade copepods, families Canuellidae/Longipediidae) are classified as `Group1 = meiobenthos/meiofauna`. WoRMS' current taxonomic backbone reports their Order as "Canuelloida" rather than the older "Harpacticoida" — a split recognized since Khodami et al. (2017) — so the lookup has a `Canuelloida` exception alongside the existing Harpacticoida/Siphonostomatoida ones.

**Free-living flatworms:** "parasitic" is restricted to the three Neodermata classes (Trematoda, Cestoda, Monogenea); every other Platyhelminthes species defaults to `meiobenthos/meiofauna` (for example the free-living, meiobenthic *Microstomum*, Order Dolichomicrostomida), with a `TaxGroup` that distinguishes the specific parasitic type (`flatworm (trematode/fluke)`, `flatworm (cestode/tapeworm)`, `flatworm (monogenean)`) from free-living flatworms (`flatworm (free-living)`).

### 9.6 Empirical Occurrence Validation

For every species, the pipeline queries OBIS and GBIF for at least one occurrence record (`limit=1`) within the selected reference marine region's real coastline geometry (a hand-verified polygon for the North Sea and Mediterranean, a dynamically-fetched-and-simplified polygon for any other region, or a bounding-box rectangle as the final fallback if geometry can't be resolved).

- **Observed_in_OBIS** / **Observed_in_GBIF** — Boolean (TRUE/FALSE): was a matching occurrence record found.
- **Observed_in_OBIS_Checked** / **Observed_in_GBIF_Checked** — Boolean (TRUE/FALSE): was the query itself genuinely completed (a real, parseable response), independent of whether a record was found. `FALSE` means "we don't actually know" (network error, no species match) — never treated as confirmed absence. This is what makes a genuine double-negative (§9.8) trustworthy.

These columns provide empirical confirmation of species presence in the reference region, complementing the structural classification.

**GBIF rate limiting:** GBIF's public API rejects a large fraction of requests with HTTP 429 (Too Many Requests) when many are fired at once, and a rejected request would otherwise look identical to a genuine absence. GBIF requests are therefore sent one at a time at roughly 1/second with automatic retry (honoring GBIF's `Retry-After` header when present), instead of all at once alongside OBIS. This makes the GBIF portion of this module slower for large species lists but avoids false negatives caused by rate limiting. OBIS is unaffected and remains fast. See §13 for the runtime impact.

### 9.7 Trait Validation (MarineSpeciesTraits Database)

The local MarineSpeciesTraits database (`marineSpeciesTraits.RData`, ~196k species records) is used twice. First, it fills Group1/Group2/TaxGroup for rows that neither the curated lookup nor the live WoRMS trait cache could classify (step 3 of the cascade below). Second, it provides an independent reference for validation: for each species, feedingType, habitatType, and taxGroup from the DB are mapped to the same vocabulary used by the pipeline, then compared with `==` to produce validation flags:

- **DB_Group1 / DB_Group2 / DB_TaxGroup** — the DB-derived reference value for each trait
- **Group1_Validated** — TRUE if final Group1 matches DB-derived value, FALSE if they differ, NA if DB has no data
- **Group2_Validated / TaxGroup_Validated** — same logic for Group2 and TaxGroup

**Mapping logic** — the DB fields are interpreted as follows:

*Group1/Group2 (from feedingType + habitatType)*:
- **feedingType** containing "plankton" + **habitatType** containing "benth"/"endofauna" → `meroplankton`
- **feedingType** containing "plankton" (only) or **habitatType** "pelagic" → `holoplankton`
- **feedingType/habitatType** containing "benth"/"endofauna" (only) → `benthic invertebrate`, *unless* the species is inferred to have planktonic larvae (see below)

**Planktonic larval inference** — many benthic invertebrate phyla have an obligate planktonic larval phase that the adult-centric DB does not capture. When the DB only records adult benthic traits, a multi-source check runs:

1. **Local DB propaguleDuration** — if the DB records a propagule (larval) duration > 0 days, this is taken as species-specific evidence of planktonic larvae (713 species have a value above 0 in version 3 of the database; e.g. *Echinocardium cordatum* = 27 days).
2. **WoRMS API attributes** — species-specific attributes are queried live for any larval/planktonic trait. Future expansions to WoRMS are picked up automatically.
3. **Phylum-level inference** (fallback) — taxa where planktonic larvae are near-universal:
   - Echinodermata, Bryozoa, Phoronida, Sipuncula, Echiura (all classes)
   - Annelida: Polychaeta
   - Mollusca: all except Cephalopoda
   - Nemertea, Hemichordata

When the check affirms planktonic larvae, Group1 is set to `meroplankton` (Group2 stays `benthic invertebrate`). Results are cached to `cache/larval_inference_cache.rds` — a persistent, cross-project cache that grows with each new species lookup.

**WoRMS API fallback** — when the local DB has no feedingType/habitatType data at all, the pipeline queries WoRMS `Functional group` attributes (with `include_inherited=TRUE`, so e.g. Copepoda inherits `plankton > zooplankton`). If only "plankton" is found, Group1/Group2 are set to `holoplankton`; if only "benthos" is found, `benthic invertebrate`; if **both** are found on the same species (this happens — WoRMS sometimes carries both an inherited planktonic-larva signal and a direct benthic-adult signal, occasionally still flagged "unreviewed"), Group1 is set to `meroplankton` rather than losing the benthos signal. Results are cached to `cache/worms_fg_cache.rds` for cross-project reuse.

**Taxonomic holoplankton fallback** — if both the local DB and WoRMS API have no data, the species' phylum/class is checked against known holoplanktonic groups. This catches taxa such as Copepoda, Cladocera, Chaetognatha, Euphausiacea, Appendicularia, Thaliacea, Rotifera, and Ctenophora — all fully planktonic but lacking explicit trait records.

**Parent-species fallback** — subspecies (e.g. *Paracalanus parvus parvus*) that are absent from the local DB are automatically resolved to their parent species for taxonomy and trait lookup.

*Group2/TaxGroup (from taxonomy)*:
- **taxClass = Aves** → Group2 = `seabird`, TaxGroup = `seabird`
- **taxClass = Mammalia** → Group2 = `marine mammal`, TaxGroup = `marine mammal`
- **taxClass = Reptilia** → Group2 = `marine reptile`, TaxGroup = `marine reptile`
- All other species matched against a lookup table keyed on (Phylum, Class, Order) — mirroring the pipeline's own `taxonomic_group_lookup` — so TaxGroup values match exactly (e.g. Copepoda → `copepod`, Echinoidea → `sea urchin`, Ctenophora → `ctenophore / gelatinous zooplankton`, Branchiopoda → `Cladocera`)
- Unmatched taxa fall back to the coarse **taxGroup** integer (1–5) broad labels

These flags appear in this module's full table and in Module G (§10) for conflict review. The DB-derived value fills any gaps when the pipeline (or no-pipeline fallback) has no value, regardless of occurrence classification.

**Trait expert override:** a direct structural mirror of the occurrence-override mechanism below — an opt-in file, `config/trait_overrides.csv` (columns `Species`, `Group1`, `Group2`, `TaxGroup`, `Reason`), lets a recorded domain-expert correction for a species' Group1/Group2/TaxGroup win with the highest precedence of any rule in this section, applied last and unconditionally. Only the columns a matched row actually specifies are overwritten — leaving a cell blank in the CSV leaves that column's existing (automatically-derived) value untouched. Example: *Pisione puzae* is set to match its sister species *Pisione remota* (meroplankton / benthic invertebrate). The columns `Trait_Override_Applied` (TRUE/FALSE) and `Trait_Override_Reason` surface this in the output exactly like `Occurrence_Override_Applied`/`Occurrence_Override_Reason` do for occurrence classification.

**Order of the Group1/Group2/TaxGroup cascade:** (1) the curated Phylum/Class/Order lookup; (2) the live WoRMS trait cache, only for rows the lookup has no entry for (and only if "Use WoRMS traits" is ticked); (2B) the WoRMS "Functional group" text already fetched; (3) the MarineSpeciesTraits DB for anything still blank; (4) `config/trait_overrides.csv`, applied last, always wins. `FG_source` records which step decided each row ("taxonomy", "worms_trait", "worms_attr_prefetched", "external_db", or "conflict" when the WoRMS functional-group text disagrees). Add a species to `config/trait_overrides.csv` whenever the lookup classifies it wrongly for your purpose.

**Where the database file is looked for:** the app searches, in order, the environment variable `MARINE_SPECIES_TRAITS_RDATA`, the R option `marineSpeciesTraits.path`, `config/marineSpeciesTraits.RData`, `data/marineSpeciesTraits.RData` and `~/marineSpeciesTraits.RData`. The file is not shipped with the app; download `marineSpeciesTraits.RData` (version 3) from Assis & Fragkopoulou (2025), https://doi.org/10.6084/m9.figshare.27620676.v3, licence CC BY 4.0 (see README for the file size and MD5). Without it the app starts normally but the `DB_Group1` / `DB_Group2` / `DB_TaxGroup` and `*_Validated` columns are empty and the external-database fallback is skipped; the startup log says "Trait validation database not found".

### 9.8 OBIS/GBIF + Text classification method & empirical-evidence precedence

**Reference-region presets:** above the region search in Module F, three buttons choose the boundary the classification is made against: **IHO North Sea (MRGID 2350)** — the default (IHO *Limits of Oceans and Seas*; the Skagerrak and Kattegat are separate IHO areas); **ICES Greater North Sea ecoregion (MRGID 36317)** — includes the Skagerrak and Kattegat, and its outline extends into the English Channel to 5°W / 48°N; or **Other region (search below)** for the free gazetteer search. The ICES outline is fetched and cached like any searched region, and the German Bight tier applies to it as well. Every output row carries `Occurrence_reference_region` (e.g. "Greater North Sea (MRGID 36317)"). The text classifier gives the same tiers under either preset; differences between the two runs come from the OBIS/GBIF queries inside the different outlines.

**Rule order.** The "OBIS/GBIF + Text" method (§9.4's default) combines empirical OBIS/GBIF occurrence evidence with the independent WoRMS-distribution-text keyword classifier (`classify_occurrence_by_text()`). The rules are applied in this order and the first match wins:

| # | Condition (first match wins) | Result |
|---|---|---|
| 1 | WoRMS flags the species as neither marine nor brackish (both flags known and `FALSE`) | **Very unlikely**, unconditionally (also re-applied as a final override after every later step) |
| 2 | Present in the **German Bight** sub-region in OBIS or GBIF (North Sea runs only; either database is enough) | **Very likely** |
| 3 | The WoRMS distribution text names the North Sea, German Bight, Wadden Sea or the reference region itself | **Very likely** |
| 4 | OBIS **or** GBIF has a record inside the reference region | **Likely** |
| 5 | The text names the wider north-east Atlantic (e.g. Celtic Sea, Irish Sea, Norwegian Sea, Bay of Biscay, British Isles) | **Likely** |
| 6 | The text names only far-away regions (Pacific, Indian Ocean, Hawaii, Caribbean, Antarctic) | **Very unlikely** |
| 7 | The locality named in the text is on a **different continent** from the reference region | **Very unlikely** |
| 8 | The text names only nearby-but-distinct regions (European waters, Mediterranean, Baltic, Black Sea, Adriatic) or a bare "Atlantic" | **Unlikely** |
| 9 | The locality named in the text is on the **same continent** as the reference region | **Unlikely** |
| 10 | Genuine double-negative: the OBIS check completed and found no record, and GBIF does not confirm a record (GBIF itself may have found nothing, or not completed its check) | **Unlikely** |
| – | None of the above | stays **Unknown** |

After the chain: the OBIS/GBIF gap-filler turns a still-`Unknown` row into **Likely** (record in both OBIS and GBIF) or **Possible** (record in one of them) — in practice this only matters for the Text-based method, because the chain above already handles every row that has an OBIS or GBIF record; the non-marine override (rule 1) is applied once more and always wins; the expert overrides in `config/species_overrides.csv` are applied last and outrank everything. The text tiers behind rules 3, 5, 6 and 8 are `Very likely` / `Likely` / `Very unlikely` / `Possible`-`Unlikely` as graded by `classify_occurrence_by_text()`.

**Why OBIS/GBIF is primary:** a spatially-scoped OBIS/GBIF occurrence query inside the reference region's real coastline geometry (§9.6) is direct empirical evidence, not a proxy. The Marine Regions gazetteer plays no part in the classification (§9.4).

**Why a "checked" double-negative is required, not just "not observed":** `Observed_in_OBIS`/`Observed_in_GBIF` being `FALSE` alone doesn't distinguish "genuinely queried and confirmed absent" from "the query failed and we don't actually know." Only a genuine double-negative — a completed OBIS check (`Observed_in_OBIS_Checked` = `TRUE`) with no record, and no GBIF record — can produce the "Unlikely" of rule 10. A failed check never counts as evidence of absence; that row simply falls through unchanged.

**Same-continent vs different-continent:** rules 7 and 9 above use `text_locality_continent()` (`global.R`), which looks up the reference region's own continent(s) and the mentioned locality's continent(s) in a world sea/ocean/region → continent table (`SEA_TO_CONTINENTS`) and compares them — "different" if they share no continent, "same" if they share any, unresolved (falls to the plain double-negative rule) if either can't be placed in the table. This works for any reference region chosen via the region search box, not just the North Sea — e.g. with the reference region set to Mediterranean Sea, a Caribbean Sea mention correctly resolves as "different continent," while a North Sea or Aegean Sea mention correctly resolves as "same continent." The table is a hand-built, best-effort list of major world seas — see the comments above `SEA_TO_CONTINENTS` in `global.R` if you need to add a locality it doesn't recognize.

**`Occurrence_Evidence_Conflict`** flags rows worth a human look: a negative tier (Unlikely or Very unlikely) although OBIS or GBIF has a record in the reference region, or a positive tier (Likely or Very likely) although both OBIS and GBIF report no record.

### 9.9 Enrichment summary

After completion, this module displays a summary tab with:
- Reference region and taxon counts (full table, marine-only table, harmonized table)
- WoRMS data coverage (distribution, traits, vernacular names)
- OBIS/GBIF empirical presence within the reference region (counts observed in OBIS, in GBIF and in both)
- Occurrence classification breakdown


Trait DB validation results appear in **Module G's summary** (§10.5).

**Species-specific expert overrides:** two opt-in CSV files in `config/` let a recorded domain-expert correction win over whatever the automated chain concludes on its own, applied last and unconditionally (highest precedence of any rule): `species_overrides.csv` (columns `Species`, `Forced_classification`, `Reason`) forces `Occurrence_classification`; `trait_overrides.csv` (§9.7) forces `Group1`/`Group2`/`TaxGroup`. Both match on species name, case-insensitively and trimmed, with subgenus notation (e.g. "Acartia (Acartiura) hudsonica") stripped from both sides before comparing so a name with or without its subgenus parenthetical still matches. A missing or empty override file is not an error — the pipeline just runs without overrides. The files shipped with the app contain these entries: *Acartia hudsonica*, *Oncaea waldemari* and *Calanus euxinus* (occurrence); and 16 species in `trait_overrides.csv` — *Pisione puzae*, *Tomopteris (Johnstonella) helgolandica*, *Euterpina acutifrons*, *Hyperoche medusarum*, *Themisto gaudichaudii*, *Clione limacina*, *Alloteuthis media*, *Ctenodrilus serratus*, *Nicolea zostericola*, *Meiodrilus adhaerens*, *Scoloplos armiger*, *Setaphyes dentatus*, *Rissoa membranacea*, *Solenofilomorpha longissima*, *Sacculina carcini* and *Alcyonium digitatum* — each with its `Reason` in the file. These are the authors' own expert decisions for their North Sea work: review them, and delete or replace them if they do not suit your study. Edit these CSVs directly to add, remove, or adjust an override — no code change needed.

**Taxonomy columns:** the full table carries two sets of taxonomy columns. The bare `Kingdom`–`Species` columns come from WoRMS's own record of the AphiaID; ranks finer than the AphiaID's own rank stay empty and are not filled from anywhere else. The `Consensus_Kingdom`–`Consensus_Species` columns come from Module 2; the Harmonized Taxonomy tab uses them as the canonical taxonomy, and the OBIS/GBIF checks query `Consensus_Species` (or `Species` when there is no `Consensus_Species` column). Module 2 has already emptied the consensus ranks below `Fallback_rank` (§5.3), so an ASV whose databases agreed only at Genus has no species name here either.

**Taxonomic exclusions in Module F:** the two text filters (default kingdoms `Protozoa, Bacteria, Fungi, Plantae, Chromista, Viruses`; default classes `Arachnida, Hexapoda, Insecta`) remove a taxon when any entry of either list equals, ignoring capitalisation, the value in any rank column (`Kingdom` to `Species`, and the `Consensus_*` columns). They are applied to every output table of the module and are independent of the filters in Module E (§8.4).


**Re-running:** The "Run ecological annotation" button can be clicked again at any time. This module works on the output of Module 5 (Aggregation); when Aggregation is run again, the results of this module are cleared and it has to be run again. Caches (WoRMS records, OBIS/GBIF evidence, larval inference, functional groups; §11) persist across runs so re-runs are faster.

### 9.10 Outputs (4 formats)

1.  **Full table** — every enriched taxon from Module E, every rank. This exact table (minus one internal bookkeeping column) is also what Module G receives as its entire input — see §10.
2.  **Marine-only** — the full table filtered to `IsMarine = TRUE`; always produced alongside the full table, never instead of it.
3.  **Traits (single column)** — `Taxon_id` and `TaxGroup` (the latter named `Traits`).
4.  **Traits (wide)** — `Taxon_id`, `Group1` and `Group2`.

The "Species-level" and "Genus-level" tables are not produced here: Module G's "Download Species Table" / "Download Genus Table" (§10.4) are the single source for those. The 4 formats above are available directly from this module as a convenience; their contents are also contained in Module G's Validated Table (which has additional columns).

---

## 10. Module G — Validation

### 10.1 What it does

Takes Module F's full enriched table directly as its entire input (no separate upload, no "run" button — it recomputes automatically whenever Module F's output changes) and audits it on two independent axes: occurrence/taxonomy conflicts and trait validation against the MarineSpeciesTraits database. This is the pipeline's final stage — its 3 downloads are the pipeline's actual deliverables.

### 10.2 Two conflict columns, not one

Every row carries two differently-scoped conflict signals, both inherited or computed by this point in the pipeline — they are easy to conflate by name alone, so this guide calls them out explicitly:

| Column | Set in | Flags | Type |
|---|---|---|---|
| `taxonomy_conflict` | Module E (§8.5) | Multiple ASVs merged into one taxon disagreed with EACH OTHER on their input taxonomy, entirely before WoRMS was ever consulted | boolean |
| `WoRMS_Consensus_Conflict_Detail` | Module G | WoRMS's own returned taxonomy for the matched AphiaID disagrees with that already-merged `Consensus_*` value | text, empty unless a real mismatch exists, names which rank(s) disagree and what each side said |

A taxon can have one, both, or neither. Both travel in every one of this module's 3 exports.

### 10.3 Unvalidated trait detection

A trait is flagged when the pipeline's value genuinely contradicts the MarineSpeciesTraits database; the Unvalidated tab (§10.5) additionally lists the rows for which the database has no value for any of the three traits. Every row carries an `Unvalidated_Reason` column (on **every** export, not only the filtered Unvalidated view) with the specific cause:

- **Group1 mismatch** — Pipeline-derived Group1 contradicts DB's feedingType/habitatType (e.g. `Group1 mismatch (Pipe: holoplankton, DB: benthic invertebrate)`)
- **Group2 mismatch** — Pipeline-derived Group2 contradicts DB's habitat type
- **TaxGroup mismatch** — Pipeline-derived TaxGroup contradicts DB's taxGroup
- **No DB data** — reported separately for each trait (`Group1: no DB data; `, `Group2: no DB data; `, `TaxGroup: no DB data; `) whenever the MarineSpeciesTraits DB gives no reference value for that trait. The Summary tab reports both mismatch and no-DB-data counts.

Trait exclusions (Kingdom/Class) were already applied once in Modules E/F — this module does not re-apply them. Uses the same multi-source trait-inference chain (local DB, WoRMS API attributes, phylum-level larval inference, taxonomic holoplankton fallback) described in §9.7.

### 10.4 Outputs & Downloads (maximum 3, by design)

Exactly 3 downloads are offered, all built from the same Module F input with an **identical column set**, so taxon counts reconcile between files and no file disagrees with another on what a column means:

- **Validated Table** (`dl_full`) — every taxon at every rank. The complete audited dataset.
- **Species Table** (`dl_species_f`) — any taxon with a binomial `Consensus_Species` name (tested directly against the name string — two whitespace-separated tokens at minimum — not against the `WoRMS_Rank` label, since a resolved name can collapse to a bare genus or higher rank despite looking species-level on input).
- **Genus Table** (`dl_genus_f`) — any taxon with a resolved `Consensus_Genus`, **including** every species-level taxon (a binomial name has a genus too). A strict superset of the Species Table.

By construction, **Species Table ⊆ Genus Table ⊆ Validated Table** at all times — every species-resolved taxon also has a genus, and every taxon is in the full table. This nesting, plus a taxon-count cross-check against Module F's own recorded output count, is verified on every run and shown both in the module log and on-screen in the Summary tab (§10.6) — a mismatch would mean something altered the data between Module F and here, which should never happen.

Excluded columns: 11 columns present in Module F's full table are dropped from these 3 exports specifically — 4 confirmed-dead columns and the 7 bare WoRMS taxonomy columns (replaced by `WoRMS_Consensus_Conflict_Detail`, §10.2). They remain in the underlying data Module G works with internally (needed to compute the conflict column); only the final exported/displayed column set excludes them.

### 10.5 Views

- **Summary** — the on-screen "Output consistency check" (§10.6), trait DB validation coverage and pass/fail counts for Group1/Group2/TaxGroup, total taxa with any mismatch, and the number with no trait DB data.
- **Unvalidated** — filtered view showing rows with at least one validation flag FALSE, or where the trait DB had no data for all three traits. These rows are NOT removed from the Validated Table — this is a filtered view for review, not a separate dataset.
- **Validated Table** — the complete audited dataset with all validation columns (this tab, not a download button, previews `dl_full`'s exact content).

### 10.6 On-screen consistency checks

Visible in the Summary tab without opening any downloaded file or log:

- Validated / Genus / Species Table row counts, with the nesting check (OK / MISMATCH).
- Module F → Module G taxon count (should always match exactly).
- Taxa with a WoRMS-vs-Consensus conflict (`WoRMS_Consensus_Conflict_Detail` populated) and, separately and clearly labeled, taxa with an ASV-merge conflict (Module E's `taxonomy_conflict`) — see §10.2 for why these are two different numbers, not one.

---

## 11. Caches

The app keeps persistent caches in the `cache/` folder (created automatically in the app folder; it is not part of the release). They speed up repeated runs and are shared by all projects run from the same app folder:

| Cache file | Contents |
|---|---|
| `wm_record_cache.rds`, `wm_classification_cache.rds` | WoRMS records and classifications already fetched |
| `worms_synonym_cache.rds` | WoRMS synonym lookups |
| `evidence_cache.rds` | OBIS/GBIF presence results per species and reference region |
| `dynamic_geometry_cache.rds`, `german_bight_mrgid_cache.rds` | Simplified outlines of searched reference regions; the German Bight record |
| `larval_inference_cache.rds` | Per-species TRUE/FALSE for a planktonic larval stage, with source tag |
| `worms_fg_cache.rds` | WoRMS Functional group results (Group1/Group2) for species not in the local trait DB |
| `worms_cache.csv` | Module 1 name-resolution cache |
| `gazetteer_cache.rds`, `gazetteer_hops_cache.rds`, `gazetteer_blacklist.rds` | Marine Regions gazetteer data — not used for occurrence classification |


Most caches are written when the app is stopped (if R is killed or crashes, the additions of that session can be lost); the region-outline caches and the Module 1 cache are written when they are updated. All are loaded at startup. Module 1 additionally allows uploading a previous WoRMS query cache CSV to skip re-querying already-resolved taxa. Delete a cache file to force fresh queries.

---

## 12. Cancel/Stop

Modules 1–6 each have a red **Cancel** button that stops long-running operations (WoRMS/OBIS/GBIF queries, decontamination, file parsing) at their next checkpoint. Requests that have already been sent to WoRMS, OBIS or GBIF cannot be recalled and may still complete. Module 7 has no Cancel button because it only recomputes views from Module 6's output.

**Module F (Ecological Annotation):** a WoRMS batch enrichment that's stuck retrying through a rate limit or network outage (§9.2) is interrupted within about a second of clicking Cancel, even mid-wait — it does not have to finish its current backoff first.

---

## 13. Performance Tips

| Module | Bottleneck | Typical time |
|---|---|---|
| 1 — Standardization | WoRMS API calls (default pause between requests 0.2 s) | Roughly 0.3–0.5 s per unique name |
| 2 — Agreement Analysis | In-memory joins plus one WoRMS classification lookup per consensus AphiaID (cached) | Seconds for small tables; longer on first use with many new AphiaIDs |
| 3 — Merging | File I/O for the MZG backbone | Usually seconds |
| 4 — Decontamination | decontam/microDecon computation | Usually seconds, longer for very large tables |
| 5 — Aggregation (Module E) | In-memory grouping/majority-vote; file I/O only if uploading instead of using the live Module D output | Instant to a few seconds |
| 6 — Ecological Annotation (Module F) | WoRMS + OBIS/GBIF API calls (4+ per species) | Roughly 1–2 s per species for WoRMS (calls in parallel), **plus ~1 s per species for the GBIF portion of occurrence validation** (paced to avoid GBIF rate-limiting — see §9.6); OBIS stays fast/parallel |
| 7 — Quality Check (Module G) | Read-only validation views, recomputed automatically from Module F's output | Fast |

The times above are rough figures; they depend on the response times of WoRMS, OBIS and GBIF and on your connection.

**Tips:**
- Reduce "Delay between WoRMS requests" if you are not being rate-limited; it adds a baseline pause between WoRMS sub-waves.
- Upload a previous cache file in Module 1 to skip already-resolved taxa.
- Module F (Ecological Annotation) runs WoRMS trait queries in parallel, in sub-waves of 10 AphiaIDs' worth of requests at a time (§9.2) rather than one large all-at-once burst. Occurrence classification does not depend on the Marine Regions gazetteer (§9.4/§9.8), so there is no long cache-warming step. Still monitor system RAM with large species lists, since WoRMS/OBIS/GBIF queries are held in memory during the run. If a run hits a sustained WoRMS outage, it keeps retrying rather than giving up partway through with everything after that point NA (§9.2) — a run that seems to be taking much longer than usual may be doing exactly this; check the module log for "[WoRMS] Still retrying..." messages.
- GBIF occurrence checks are intentionally paced to ~1 request/second to avoid GBIF's rate limiting — for a large species list, budget roughly 1 extra second per species for the GBIF portion of Module F's occurrence validation. The evidence cache (`cache/evidence_cache.rds`) means this cost is paid only once per species/region; reruns against already-cached species are fast.


---

## 14. Troubleshooting

| Symptom | Likely cause |
|---|---|
| No results from Module 1 | File format mismatch; check parser expectations |
| Empty MZG AphiaID columns in Module 2 | ASV IDs out of sync; rerun Module 1 with same MZG file |
| Module 4: the Summary shows a "microDeCon error" line | microDecon could not run on this data (for example too few blank or true-sample columns with reads); use "decontam only" or check the sample matching in the metadata |
| WoRMS enrichment slow | Reduce the delay setting; increase it if HTTP 429 errors appear |
| Occurrence shows "Unknown" | None of the rules in §9.8 applied: the WoRMS distribution text is empty or names no recognisable region, neither OBIS nor GBIF has a record in the reference region, and (with the OBIS/GBIF + Text method) the OBIS check could not be completed, so it does not count as a confirmed absence. Typical for species with little distribution information in WoRMS |
| Cancel button doesn't stop immediately | Requests already sent to WoRMS/OBIS/GBIF complete before the cancellation takes effect |

---

## 15. Output Data Columns



### 15.1 Module E (Aggregation) outputs — Aggregated table / Conflicts table

| Category | Column Names |
| :--- | :--- |
| **Identifiers** | `Taxon_id` (`Taxon_1`, `Taxon_2`, ...) |
| **Provenance** | `Merged_ASV_ids` (every original ASV folded into this row), `n_asvs`, `Sequence_length_bp` (when a sequence column was provided) |
| **Samples** | Original sample columns, unchanged from the MZG input (e.g. `Sample_A`) |
| **Taxonomy** | `Consensus_Kingdom`–`Consensus_Species` (with matching `Consensus_<Rank>_AphiaID` columns), `Species_fallback` (the consensus AphiaID), `Species_fallback_source`, `Fallback_rank` |
| **Conflict** | `taxonomy_conflict` (boolean — see §8.5/§10.2) |

The Conflicts table is the same columns, filtered to `taxonomy_conflict == TRUE`.

### 15.2 Module F (Ecological Annotation) outputs — Full table / Marine-only / Traits (single, wide)

Everything in §15.1 above, carried through unchanged, **plus**:

| Category | Column Names |
| :--- | :--- |
| **Taxonomy (WoRMS)** | `AphiaID`, `Kingdom`, `Phylum`, `Class`, `Order`, `Family`, `Genus`, `Species` (WoRMS's own returned taxonomy for the matched AphiaID — compare against `Consensus_*` above, not necessarily identical), `WoRMS_Rank`, `WoRMS_URL`, `WoRMS_Status`, `WoRMS_Synonym_Fallback_Used`, `WoRMS_Parent_Fallback_Used` |
| **Ecology/Traits** | `IsMarine`, `IsBrackish`, `IsFreshwater`, `IsTerrestrial`, `Body_Size_Min`, `Body_Size_Mean`, `Body_Size_Max`, `Functional_Group_Parsed`, `Species_Paraphyletic_Group`, `WoRMS_FunctionalGroup`, `WoRMS_attributes`, `Group1`, `Group2`, `TaxGroup`, `FG_source`, `Trait_Override_Applied`, `Trait_Override_Reason` |
| **Vernacular** | `WoRMS_vernacular_<Language>` (one column per language selected in §9.3) |
| **Occurrence** | `Occurrence_classification`, `Occurrence_reference_region`, `WoRMS_Distribution`, `Occurrence_Evidence_Conflict`, `Occurrence_Single_DB_Only`, `Occurrence_Override_Applied`, `Occurrence_Override_Reason` (§9.9 species-override mechanism) |
| **Occurrence (reserved, always empty)** | `Occurrence_hops_to_reference_region`, `Occurrence_nearest_locality`, `Parsed_MRGIDs`, `Occurrence_NA_Reason` — **always empty/boilerplate**; they are not computed (§9.4/§9.8). Present in Module F's tables for column-layout compatibility; not included in Module G's 3 exports (§10.4). |
| **Empirical validation** | `Observed_in_OBIS`, `Observed_in_GBIF`, `Observed_in_OBIS_Checked`, `Observed_in_GBIF_Checked`, `Observed_in_OBIS_GermanBight`, `Observed_in_GBIF_GermanBight`, `OBIS_Occurrence_Lat/Lon`, `GBIF_Occurrence_Lat/Lon`, `OBIS_Matched_Name`, `GBIF_Matched_Name` |
| **Trait DB cross-check** | `DB_Group1`, `DB_Group2`, `DB_TaxGroup`, `Group1_Validated`, `Group2_Validated`, `TaxGroup_Validated` |

*Traits (single column) and Traits (wide) contain only `Taxon_id` plus the specific trait column(s) named by their button label — not the full column set above.*

### 15.3 Module G (Validation) outputs — Validated Table / Species Table / Genus Table

All 3 exports share an **identical column set**: everything in §15.1 + §15.2 above, **minus** the 11 columns in `EXCLUDED_METADATA_COLS` (the 4 dead Occurrence_* columns and the 7 bare WoRMS taxonomy columns — both rows marked above), **plus**:

| Category | Column Names |
| :--- | :--- |
| **Trait validation** | `Unvalidated_Reason` — on every row of every export, not only a separate Unvalidated-only file |
| **Taxonomy conflict** | `WoRMS_Consensus_Conflict_Detail` (see §10.2 for how this differs from `taxonomy_conflict` above) |

---

## 16. Quick-Start Checklist

1. Install R packages
2. Open `app.R` → Run App
3. Upload BLAST / BOLD / MZG files (or click "Load example dataset" in Module 1)
4. Click **Run taxonomic standardization** in Module 1 (Standardization)
5. Review results, proceed to Module 2 (Agreement Analysis) → **Run agreement analysis**
6. Module 3 (Merging) → **Assemble final ASV table**
7. Upload metadata (or click "Use example metadata") → Module 4 (Decontamination) → **Run contaminant detection & removal**
8. Module 5/E (Aggregation) → **Aggregate** (it uses Module 4's output automatically; optionally set the exclusion lists first) to collapse ASVs sharing a resolved taxon
9. Module 6/F (Ecological Annotation) → select vernacular language(s) → select reference region → **Run ecological annotation**
10. Module 7/G (Quality Check) → review the on-screen consistency checks and flagged mismatches → **Download** the Validated, Species, or Genus Table (the pipeline's 3 final outputs)

---

---

For further assistance, check the log messages within each module or contact: kingsly-chuo.beng@thuenen.de

---

*Internal note:* Every join between data tables is preceded by an automated column-name and type check. If a join key is missing or has mismatched types, the pipeline stops with a clear error message pinpointing the source tables and column — preventing silent data loss or cartesian joins.
