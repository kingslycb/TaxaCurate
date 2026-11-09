# TaxaCurate — post-taxonomic-assignment curation of COI metabarcoding results (R/Shiny)

TaxaCurate is a click-through R/Shiny app for the CREATE WP2.3 pipeline that takes raw
taxonomic assignments from COI metabarcoding (BLAST, BOLD, MZG) through to a curated,
ecologically annotated species table:

- **A. Taxonomy assignment** — parse BLAST / BOLD / MZG raw outputs and resolve every taxon
  name to a WoRMS AphiaID (hierarchical name-cleaning fallback).
- **B. Cross-database agreement** — compare AphiaIDs across the sources per ASV per taxonomic
  level (All agree / Two agree / None agree / Only one database), flag species-level
  conflicts, and build a deterministic species fallback: the most specific rank at which at
  least two databases share the same AphiaID.
- **C. Final ASV table** — merge sample read counts (MZG backbone) with the standardised
  taxonomy and the fallback assignment.
- **D. Contaminant removal** — flag and remove contaminant ASVs with blank/control samples
  (`decontam`, `microDecon`, or both).
- **E. Aggregation** — aggregate ASVs to taxa.
- **F. WoRMS enrichment** — taxonomy, marine status, distribution, ecological groups
  (Group1/Group2/TaxGroup), body size, vernacular names and North Sea occurrence evidence
  (OBIS/GBIF + WoRMS distribution text, or text only).
- **G. Validation** — review of the enriched table (conflicts between sources).

In the app these seven steps are the tabs *1 · Standardization*, *2 · Agreement Analysis*, *3 · Merging*,
*4 · Decontamination*, *5 · Aggregation*, *6 · Ecological Annotation* and *7 · Quality Check*; the source
files and parts of this README use the letters (Module A = tab 1 … Module G = tab 7), the user guide the numbers.

Each step feeds the next in memory (no re-uploading), and every intermediate result is also
downloadable. See `USER_GUIDE.md` for details and `CHANGELOG.md` for the release note.
**Version:** v1.0.0 (tag `TaxaCurate_v1.0.0`).

## Installation

R 4.3 or newer is recommended (R 4.4.2 was used for the release).

```bash
Rscript install_dependencies.R          # installs the CRAN packages (see the file for optional ones)
Rscript -e "shiny::runApp('.')"         # run from the app folder
```

Module D needs `decontam` together with `phyloseq` (both Bioconductor) and/or `microDecon` (GitHub)
only if you choose a method that uses them (`BiocManager::install(c("decontam", "phyloseq"))`); if they are missing the app still starts and tells you so.

## MarineSpeciesTraits database (optional, not included)

Trait validation (`DB_Group1`, `DB_Group2`, `DB_TaxGroup`, `*_Validated`) and the external-database
fallback use the MarineSpeciesTraits database as an R file `marineSpeciesTraits.RData`
(object `speciesList`, one row per species). **It is not part of this repository.** Download it
from its original source:

> Assis, J. & Fragkopoulou, E. (2025). *marineSpeciesTraits: Global Marine Species Traits Database*
> (version 3). Figshare. https://doi.org/10.6084/m9.figshare.27620676.v3 (https://figshare.com/articles/dataset/marineSpeciesTraits_Global_Marine_Species_Traits_Database/27620676).
> Licence: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/); please cite it when you use it.

Download the file `marineSpeciesTraits.RData` (17,267,764 bytes, MD5 `8a838cade726ad2836fc1704f5281e66`
for version 3; check with `md5sum marineSpeciesTraits.RData`). The app expects this full
file; the smaller `marineSpeciesTraitsReduced.RData` on the same record has not been tested with it.
The licence would allow redistribution with attribution, but the file is deliberately not bundled here
(size, and the record is itself compiled from other sources and may be revised); a different version
may change results, so state the version you used. The app looks for it, in this order, at:

1. the environment variable `MARINE_SPECIES_TRAITS_RDATA` (full path),
2. the R option `marineSpeciesTraits.path`,
3. `config/marineSpeciesTraits.RData`, 4. `data/marineSpeciesTraits.RData`,
5. `~/marineSpeciesTraits.RData`.

If it is not found the app still starts; the trait-validation columns are simply empty.

## Data sources and terms

The app queries WoRMS, OBIS, GBIF and Marine Regions at run time, and optionally uses the
MarineSpeciesTraits database (Assis & Fragkopoulou 2025, CC BY 4.0, see above). This repository contains no data from them; the run-time caches in
`cache/` are rebuilt by the app and are not published. Please consult each provider's terms of use
and cite the sources you use, and give the access date, in your methods.

## Logos

No logos or other images are included in this repository (the institutional and funder logos are the
property of their owners and are not covered by the AGPL). Without them the app runs unchanged and
shows the organisation names as text. To show logos in your own installation, place image files in
`www/` with the file names used in `app.R` (`optional_logo(...)`: `TaxaCurate.png`, `CREATE.svg`,
`sustainMare.svg`, `AWI_Logo_Farbe_RGB.png`, `HIFMB.png`, `THUENEN_LOGO_screen_RGB.png`, `BMFTR.png`,
`DAM_RGB.svg`); `.gitignore` keeps them out of the repository.

## Expert override files (`config/`)

`config/species_overrides.csv` (occurrence classification) and `config/trait_overrides.csv`
(Group1/Group2/TaxGroup) record domain-expert corrections that win over the automated
classification. Both are optional and documented in `USER_GUIDE.md`.

## Checking a result

Before relying on a result, run the whole pipeline on a dataset you know (the bundled example in `examples/` is a quick first check) and compare the output with an independent analysis.

## Licence and citation

GNU Affero General Public License v3 or later — see `LICENSE`, `COPYRIGHT` and `COPYING.txt`.

Authors: Beng, K. C., Laakmann, S., & Sell, A. F.

See also `CITATION.cff` (GitHub's "Cite this repository").

## Example dataset

A bundled example dataset lives in `examples/` — 21 ASVs across BLAST, BOLD,
and MZG formats, plus a matching sample metadata file, covering mostly
North Sea / German Bight species (and four typical lab/reagent contaminants)
and deliberately exercising known edge cases. Click **"Load example dataset"** at the top of Module 1, then
**"Use example metadata"** at the top of Module 4, to run the whole pipeline
end-to-end before uploading your own data.

### Taxonomy coverage (Modules 1–3, 5)

| ASV | Species (MZG call) | What it tests |
|---|---|---|
| ASV_1 | *Ostrea edulis* | Two competing BLAST hits (*Ostrea edulis*, E-value 1e-95, and *Crassostrea gigas*, 1e-80) — checks best-hit selection. Its ID is written with the `ASV_` prefix in the BLAST and BOLD files |
| ASV_2, 3, 6, 9–11, 13, 14 | various | BLAST, BOLD and MZG all give the same species-level name. In the BLAST and BOLD files most IDs are bare numbers (`2`, `3`, …) rather than `ASV_2` — checks that the prefix is added |
| ASV_4 | *Cancer pagurus* | All three files give the same species; like ASV_1, written with the `ASV_` prefix in the BLAST and BOLD files, so those files mix prefixed and bare-number IDs |
| ASV_5 | *Bougainvillia ramosa* | BLAST gives a species-level hit at 88.5 % identity; BOLD stops at family. TaxaCurate applies no identity threshold, so the BLAST species name is used as it stands and agrees with MZG at species level |
| ASV_7 | *Hediste diversicolor* | No BOLD entry — tests agreement with only BLAST + MZG |
| ASV_8 | *Littorina littorea* | No BLAST entry; BOLD resolves only to order — tests agreement with minimal data |
| ASV_12 | *Asterias rubens* | BLAST species-level hit at 84.0 % identity (again, no threshold is applied) |
| ASV_19 | *Clupea harengus* | The same species name as ASV_6 (so Module 5 merges them); BLAST names *Sprattus sprattus*, BOLD and MZG name *Clupea harengus* — a species-level disagreement between databases |
| ASV_20 | *Chaetodon striatus* | A species not expected in the North Sea — exercises the occurrence classification in Module 6 |
| ASV_21 | *Pollachius virens* | Three different species calls (BLAST *Gadus morhua*, BOLD *Merlangius merlangus*, MZG *Pollachius virens*) — a three-way disagreement |
| ASV_15 | *Homo sapiens* | Lab/reagent contaminant. Resolves normally through Modules 1–3 and is removed only in Module 4, on its read-count pattern rather than its identity |
| ASV_16 | *Sus scrofa* | Contaminant; no BLAST entry (BOLD + MZG only) |
| ASV_17 | *Gallus gallus* | Contaminant |
| ASV_18 | *Escherichia coli* | Contaminant; present only in the MZG file (neither BLAST nor BOLD has an entry) |

The BLAST file has 19 rows covering 18 ASVs (ASV_1 has two hits), the BOLD file has 19 rows, and the
MZG file has 21 rows (20 distinct species names, because *Clupea harengus* occurs twice).

The MZG example file is named `.tsv` but is actually comma-delimited (the app reads MZG files as
comma-separated). Its first column is headed `ASV` and holds bare numbers, which the app turns into
`ASV_1`, `ASV_2`, …

### Contaminant removal (Module 4)

`example_metadata.tsv` describes 13 samples: 4 blank/control samples (`Blank_1`–`Blank_4`) and 9 true
samples (`Sample_A`–`Sample_H` and `Sample_J`), matching the columns of the MZG file exactly. Three
rows exercise specific code paths:

| Metadata row | What it tests | What happens |
|---|---|---|
| `Blank_3` / `BLANK` | The label is all capitals | Still recognised as a blank — blank labels are matched case-insensitively |
| `Blank_4` / `blanc` | A spelling variant of "blank" | Recognised as a blank — `blanc` is one of the accepted labels (`blank`, `negative`, `control`, `ntc`, `blanc`) |
| `Sample_J` | Present in both files but has **zero reads** in every ASV | Left out of contaminant detection only; its all-zero column stays in the cleaned tables and is still counted under "True samples used" in the run summary |

The read-count matrix is designed so that ASVs 15–18 (the 4 contaminants) are
present in **all four blanks** at high abundance (59–496 reads), with only
sparse, low-level spillover (14–38 reads) into 1–3 true samples — a
realistic contamination pattern that `decontam`'s prevalence method should
flag clearly. The real species (ASV_1–14 and ASV_19–21) show the opposite pattern:
variable presence across the true samples (26–292 reads where present) and at most
4 reads in any blank.

With the default detection method ("Both — require agreement") the four
contaminants (ASV_15–18) are the ASVs that are meant to be flagged. If your
result is clearly different (the contaminants are not flagged, or several real
species are), please report it. Which ASVs are flagged depends on the installed
versions of `decontam` and `microDecon`.

## File structure

```
taxacurate/
├── app.R                       # entry point — sources everything, defines UI/server
├── global.R                    # shared parsers, cleaning fns, WoRMS lookup, helpers
├── modules/
│   ├── mod_a_taxonomy.R        # A  taxonomy assignment
│   ├── mod_b_agreement.R       # B  cross-database agreement
│   ├── mod_c_merge.R           # C  final ASV table
│   ├── mod_d_contaminants.R    # D  contaminant removal
│   ├── mod_e_aggregation.R     # E  aggregation
│   ├── mod_f_enrichment.R      # F  WoRMS enrichment
│   └── mod_g_validation.R      # G  validation
├── config/                     # optional expert override files
├── examples/                   # bundled example dataset (see "Example dataset")
├── www/                        # styles and user-guide Word file (logos are not distributed, see "Logos")
├── install_dependencies.R
├── CHANGELOG.md  USER_GUIDE.md  README.md
└── LICENSE  COPYRIGHT  COPYING.txt  CITATION.cff
```

Each module is a self-contained Shiny module (`mod_X_ui()` / `mod_X_server()`). A
`shared <- reactiveValues()` object in `app.R`'s server function is passed into every module's
server function and acts as the pipe between steps — e.g. Module A writes `shared$modA_blast`,
and Module B reads it.

## Design notes

- **Name cleaning.** Every taxonomic string goes through a cleaning chain (basic clean, suffix strip,
  code strip); the cleaned string is the first candidate for each column's WoRMS query in
  `resolve_one_column()` (see "Taxonomy resolution" below).
- **BLAST best hit.** The best hit per ASV is selected with `arrange(ASV_id, E_value, desc(Percent_id))`,
  first row per ASV.
- **BOLD multi-assignments.** Only the first `+`-separated taxonomy string is used.
- **Species fallback.** For each ASV the fallback takes the most specific rank (Species, then Genus, …
  up to Kingdom) at which at least two databases share the same AphiaID, and records which databases
  agreed (`Species_fallback_source`, e.g. `BLAST+MZG`) and at which rank (`Fallback_rank`). No %identity
  or length threshold is applied.
- **Occurrence labels.** `Very likely`, `Likely`, `Possible`, `Unlikely`, `Very unlikely`, `Unknown`; see
  "Occurrence classification" below for how they are assigned.
- **No `setwd()` or hardcoded Windows paths.** All file access goes through `input$file$datapath`
  (Shiny's temp upload path) or in-memory reactive values passed between modules.
- **WoRMS lookups run sequentially with a progress bar**, not in parallel worker processes. Parallel
  processes do not report progress back to a Shiny session cleanly, and the bottleneck is network latency
  (the `worms_delay` throttle), not CPU.
- **Module 1 keeps a file-based WoRMS name cache.** On every run it reads `cache/worms_cache.csv` if that
  file exists (taxa already resolved are not queried again) and writes the updated cache back there when
  the run finishes; you can also upload a previous run's cache CSV in Module 1, and every run's cache is
  downloadable. For a brand-new project, delete `cache/worms_cache.csv` (or leave `cache/` empty) and it
  starts fresh.
- **Module B handles 2-database and 3-database runs.** "Two agree" is evaluated over the databases that
  are present, so a BLAST+MZG run without BOLD is compared correctly.
- **Module D's two methods are independently optional**: decontam only, microDecon only, require both to
  agree, or flag-by-either.

## Occurrence classification

`Occurrence_classification` (Module F) grades how likely each species is to occur in a reference region.
The default is the IHO North Sea; Module F also offers the ICES Greater North Sea ecoregion as a preset, and
any other region can be found with the Marine Regions search box. Two methods can be chosen in Module F:

- **OBIS/GBIF + Text** (default) combines empirical OBIS/GBIF occurrence records with a keyword grading of the
  WoRMS distribution text, applying the rules below in order.
- **Text-based (Keyword)** uses the distribution-text grading only; OBIS/GBIF are used only to fill rows that
  remain `Unknown`.

Rules of the default method:

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
| – | None of the above | stays **Unknown** (the OBIS/GBIF gap-filler can still resolve it, see below) |

After the chain, three more steps run in this order: the OBIS/GBIF **gap-filler** (a row still `Unknown` becomes `Likely` if both databases have a record and `Possible` if only one does; it is mainly relevant for the text-only method); the **non-marine override** again (rule 1 always wins); and the **expert overrides** in `config/species_overrides.csv`, which win over everything. Two review flags are added and never change the tier: `Occurrence_Evidence_Conflict` (a confident negative tier although OBIS or GBIF has a record, or a confident positive tier although both databases were checked and are empty) and `Occurrence_Single_DB_Only` (a `Likely` that rests on exactly one of OBIS/GBIF and no distribution text).

OBIS/GBIF queries run inside the real outline of the reference region (a hand-checked polygon for the North Sea, a simplified outline fetched from Marine Regions for any other region). `Observed_in_OBIS_Checked` / `Observed_in_GBIF_Checked` record whether the query itself completed, so a failed request is never mistaken for an absence. GBIF requests are sent about once per second (GBIF rate-limits bursts), so large species lists take a while.

No gazetteer-hop method is used: the columns `Occurrence_hops_to_reference_region`, `Occurrence_nearest_locality` and `Parsed_MRGIDs` are always empty. Marine Regions is used for the region search and the region outlines only.

**Four output formats, produced together:** the **full table** (every taxon), the **marine-only table** (full table filtered to WoRMS `IsMarine = TRUE`), **traits (single column)** (`Taxon_id` and `TaxGroup`, the latter named `Traits`) and **traits (wide format)** (`Taxon_id`, `Group1` and `Group2`). The deliberate Kingdom/Class exclusion applies to all four.

## Taxonomy resolution

Each of the 7 taxonomic columns (Kingdom, Phylum, Class, Order, Family,
Genus, Species) is resolved **independently** against WoRMS. This is deliberate: Module B's agreement
comparison checks each level separately (do BLAST/BOLD/MZG agree at
Kingdom? at Species?), and that only means something if each level really
is its own independent lookup.

In addition:

- **Confidence tiers.** Every resolved column gets one of six tiers:
  `exact_species`, `fuzzy_species`, `exact_genus`, `fuzzy_genus`,
  `higher_rank_fallback`, `unresolved` — visible as new
  `<Level>_confidence` columns (e.g. `Species_confidence`) in every output
  table, and summarized in Module 1's results.
- **Fallback climbs all the way to Kingdom.** If a column's own string
  doesn't resolve (even after the clean/suffix/code-strip
  cleaning), the resolver climbs that *same row's* own higher-rank columns
  — e.g. if `Family` fails, it tries that row's `Order`, then `Class`, up
  to `Kingdom` — rather than giving up after one genus-level attempt.
- **Homonym checking.** Some genus/family names are shared between
  completely unrelated kingdoms (*Morus* is both a mulberry genus in
  Plantae and a seabird genus in Animalia; *Prunella* is both a plant and a
  bird genus). WoRMS often returns multiple candidate records for an
  ambiguous name; the resolver prefers the candidate whose own
  kingdom/phylum agrees with that row's *other* columns. If a row gives a
  kingdom or phylum and **no** candidate agrees with it, the match is
  **refused** and the resolver moves on to the next, coarser name, exactly as
  it does for an empty WoRMS result. (A single exact match at species rank is
  exempt from this check, because a full species name is practically unique
  across kingdoms.) A `<Level>_context_agreed` column records the outcome:
  `TRUE` (a candidate agreeing with the row's context was chosen), `NA` (no
  check applied) or `FALSE` (the candidates disagreed with the row's context
  and were refused).
- **Never overwrites a real value with blank.** A column is only replaced
  by its WoRMS-resolved value when WoRMS actually returned something for
  it; an unresolved column keeps the original input string untouched.

**On non-marine species (e.g. the example dataset's lab contaminants).**
Every WoRMS name query in TaxaCurate is made with `marine_only = FALSE` (the `worrms` default would be
`TRUE`). Purely terrestrial species such as the *Homo sapiens*, *Sus scrofa* and *Gallus gallus*
contaminants of the example dataset therefore resolve to their own correct WoRMS records instead of being
matched to an unrelated marine taxon. Module 6 then relies on WoRMS' own `IsMarine` / `IsBrackish` flags
(rule 1 of the occurrence classification above). This does not affect Module 4's contaminant detection (that is based on read-count
patterns, not on taxonomy).

## Known limitations / things to check on first real run

- **Large files / many unique taxa will take a while.** WoRMS lookups are
  throttled (default 0.2s/request) to be polite to the API — for thousands of
  unique taxa this is genuinely slow. Since each of the 7 taxonomic columns is
  resolved independently (per the "Taxonomy resolution" section above),
  a row with all 7 levels populated can mean up to 7 WoRMS queries instead of
  1, though identical (level, string, context) combinations across BLAST/
  BOLD/MZG still share one cached result. The progress bar will show live
  status, but consider running Module A's largest dataset first and saving
  its cache, then seeding from that cache for related runs.
- **Module D matches the metadata to the ASV table by sample name.** `microDecon::decon()` needs
  `[ASV_id, blanks..., samples...]` with no extra columns; the app builds that layout itself from the
  metadata. A sample-like column of the ASV table that is in neither the blank nor the true-sample list of
  the metadata is left out of the output and a warning names it; if no blank, or no true sample, matches at
  all, the run stops with an error. Check the spelling of your sample IDs against the metadata.
- **The Shiny session holds everything in memory.** For very large ASV tables
  (many tens of thousands of ASVs × hundreds of samples), watch R's memory usage;
  nothing here streams from disk.
- **Validate on your own data.** Run the whole pipeline on a dataset you know and compare the result with an
  independent analysis before relying on it.
