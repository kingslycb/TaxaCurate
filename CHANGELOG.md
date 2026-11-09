# Changelog

## v1.0.0 — First public release

First public release of TaxaCurate, a Shiny application for the curation of species lists after taxonomic
assignment of eDNA metabarcoding data (BLAST, BOLD and MZG assignments).

The release contains the seven steps of the pipeline (tabs 1–7 in the app; Modules A–G in the source):

1. Standardization of the three assignment tables against WoRMS
2. Agreement analysis between the databases
3. Merging, with an agreement-based species fallback
4. Decontamination (decontam and/or microDecon, using sample metadata)
5. Aggregation of ASVs to taxa
6. Ecological annotation (occurrence in a reference region from OBIS/GBIF and WoRMS distribution text, habitat and trait groups)
7. Quality check of the enriched table

Also included: the optional expert override files in `config/`, a small example dataset in `examples/`,
the user guide (`USER_GUIDE.md` and the Word version in `www/guide/`), and the licence and citation files
(AGPL-3.0-or-later; `CITATION.cff`).

Not distributed: research data, organisation logos and images, and the MarineSpeciesTraits database
(see the README for where to obtain it).
