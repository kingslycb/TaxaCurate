# ========================================================================
# APP.R — COI Metabarcoding -> WoRMS Pipeline
# Single-file launcher: sources global helpers + modules, defines UI/server
# ========================================================================

source("global.R")
source("modules/mod_a_taxonomy.R")
source("modules/mod_b_agreement.R")
source("modules/mod_c_merge.R")
source("modules/mod_d_contaminants.R")
# Round 4 (2026-09-09): module letters were reassigned to match pipeline
# execution order (Aggregation now runs before Enrichment) —
# Module E = Aggregation, Module F = Enrichment, Module G = Validation.
source("modules/mod_e_aggregation.R")
source("modules/mod_f_enrichment.R")
source("modules/mod_g_validation.R")

# ------------------------------------------------------------------------
# Optional logos (Rounds 67-68): NO logos or images are distributed with the source code (see README,
# "Logos"); this includes the app's own logo. If a logo file is missing from www/, its name is shown as text instead,
# so the app runs unchanged without them.
# ------------------------------------------------------------------------
optional_logo <- function(src, height, alt) {
  if (file.exists(file.path("www", src))) {
    img(src = src, height = height, alt = alt)
  } else {
    span(style = "color: #6B6358; font-size: 0.8rem; font-weight: 600;", alt)
  }
}


# ------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------

ui <- page_fluid(
  theme = app_theme,
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;700;800&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      body, .bslib-page-fillable, .page-fillable, .container-fluid {
        font-family: 'Nunito', system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif !important;
        color: #222 !important;
        background-color: #F5F5F0 !important;
      }
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Nunito', system-ui, sans-serif !important;
      }
      .app-header-inner {
        display: flex;
        align-items: center;
        justify-content: space-between;
        background-color: #f2f2f2;
    padding: 10px 20px 10px 0;
        border-bottom: 3px solid #38a3a9;
        color: #003366;
      }
      /* Okabe-Ito colorblind-safe step colors */
      .step.step-1 .step-circle { background: #E69F00 !important; border-color: #E69F00 !important; color: white !important; }
      .step.step-2 .step-circle { background: #56B4E9 !important; border-color: #56B4E9 !important; color: white !important; }
      .step.step-3 .step-circle { background: #009E73 !important; border-color: #009E73 !important; color: white !important; }
      .step.step-4 .step-circle { background: #F0E442 !important; border-color: #F0E442 !important; color: #333 !important; }
      .step.step-5 .step-circle { background: #0072B2 !important; border-color: #0072B2 !important; color: white !important; }
      .step.step-6 .step-circle { background: #D55E00 !important; border-color: #D55E00 !important; color: white !important; }
      .step.step-1 .step-label { color: #E69F00 !important; font-weight: 600 !important; }
      .step.step-2 .step-label { color: #56B4E9 !important; font-weight: 600 !important; }
      .step.step-3 .step-label { color: #009E73 !important; font-weight: 600 !important; }
      .step.step-4 .step-label { color: #B8860B !important; font-weight: 600 !important; }
      .step.step-5 .step-label { color: #0072B2 !important; font-weight: 600 !important; }
      .step.step-6 .step-label { color: #D55E00 !important; font-weight: 600 !important; }
      .step.step-7 .step-circle { background: #CC79A7 !important; border-color: #CC79A7 !important; color: white !important; }
      .step.step-7 .step-label { color: #CC79A7 !important; font-weight: 600 !important; }
      /* Nav pill colors match step colors */
      .pipeline-nav .nav-pills .nav-link[data-value='1 · Standardization'].active { background: #E69F00 !important; border-color: #E69F00 !important; }
      .pipeline-nav .nav-pills .nav-link[data-value='2 · Agreement Analysis'].active { background: #56B4E9 !important; border-color: #56B4E9 !important; }
      .pipeline-nav .nav-pills .nav-link[data-value='3 · Merging'].active { background: #009E73 !important; border-color: #009E73 !important; }
      .pipeline-nav .nav-pills .nav-link[data-value='4 · Decontamination'].active { background: #DAA520 !important; border-color: #DAA520 !important; }
      .pipeline-nav .nav-pills .nav-link[data-value='5 · Aggregation'].active { background: #0072B2 !important; border-color: #0072B2 !important; }
      .pipeline-nav .nav-pills .nav-link[data-value='6 · Ecological Annotation'].active { background: #D55E00 !important; border-color: #D55E00 !important; }
      .pipeline-nav .nav-pills .nav-link[data-value='1 · Standardization']:hover { border-color: #E69F00 !important; color: #E69F00 !important; background: #FFF3E0 !important; }
      .pipeline-nav .nav-pills .nav-link[data-value='2 · Agreement Analysis']:hover { border-color: #56B4E9 !important; color: #56B4E9 !important; background: #EBF5FB !important; }
      .pipeline-nav .nav-pills .nav-link[data-value='3 · Merging']:hover { border-color: #009E73 !important; color: #009E73 !important; background: #E0F5EF !important; }
      .pipeline-nav .nav-pills .nav-link[data-value='4 · Decontamination']:hover { border-color: #DAA520 !important; color: #DAA520 !important; background: #FFFDE7 !important; }
      .pipeline-nav .nav-pills .nav-link[data-value='5 · Aggregation']:hover { border-color: #0072B2 !important; color: #0072B2 !important; background: #E3F0F8 !important; }
      .pipeline-nav .nav-pills .nav-link[data-value='6 · Ecological Annotation']:hover { border-color: #D55E00 !important; color: #D55E00 !important; background: #FDF0EB !important; }
      .pipeline-nav .nav-pills .nav-link[data-value='7 · Quality Check'].active { background: #CC79A7 !important; border-color: #CC79A7 !important; }
      .pipeline-nav .nav-pills .nav-link[data-value='7 · Quality Check']:hover { border-color: #CC79A7 !important; color: #CC79A7 !important; background: #F9EAF3 !important; }
      /* Module content colors */
      .mod-step-1 .card-header { color: #E69F00 !important; }
      .mod-step-1 .btn-primary { background-color: #E69F00 !important; border-color: #E69F00 !important; }
      .mod-step-1 .btn-primary:hover { background-color: #C88000 !important; border-color: #C88000 !important; }
      .mod-step-1 .nav-tabs .nav-link.active { color: #E69F00 !important; border-bottom-color: #E69F00 !important; }
      .mod-step-1 .form-check-input:checked { background-color: #E69F00 !important; border-color: #E69F00 !important; }
      .mod-step-1 details summary { color: #C88000 !important; }
      .mod-step-1 .example-callout { border-color: #E69F00 !important; background: #FFF3E0 !important; }
      .mod-step-1 .step-number { -webkit-text-stroke: 1px #E69F00 !important; }
      .mod-step-2 .card-header { color: #56B4E9 !important; }
      .mod-step-2 .btn-primary { background-color: #56B4E9 !important; border-color: #56B4E9 !important; }
      .mod-step-2 .btn-primary:hover { background-color: #3A9FD1 !important; border-color: #3A9FD1 !important; }
      .mod-step-2 .nav-tabs .nav-link.active { color: #56B4E9 !important; border-bottom-color: #56B4E9 !important; }
      .mod-step-2 .form-check-input:checked { background-color: #56B4E9 !important; border-color: #56B4E9 !important; }
      .mod-step-2 details summary { color: #3A9FD1 !important; }
      .mod-step-2 .example-callout { border-color: #56B4E9 !important; background: #EBF5FB !important; }
      .mod-step-2 .step-number { -webkit-text-stroke: 1px #56B4E9 !important; }
      .mod-step-3 .card-header { color: #009E73 !important; }
      .mod-step-3 .btn-primary { background-color: #009E73 !important; border-color: #009E73 !important; }
      .mod-step-3 .btn-primary:hover { background-color: #007A5A !important; border-color: #007A5A !important; }
      .mod-step-3 .nav-tabs .nav-link.active { color: #009E73 !important; border-bottom-color: #009E73 !important; }
      .mod-step-3 .form-check-input:checked { background-color: #009E73 !important; border-color: #009E73 !important; }
      .mod-step-3 details summary { color: #007A5A !important; }
      .mod-step-3 .example-callout { border-color: #009E73 !important; background: #E0F5EF !important; }
      .mod-step-3 .step-number { -webkit-text-stroke: 1px #009E73 !important; }
      .mod-step-4 .card-header { color: #DAA520 !important; }
      .mod-step-4 .btn-primary { background-color: #DAA520 !important; border-color: #DAA520 !important; }
      .mod-step-4 .btn-primary:hover { background-color: #C89200 !important; border-color: #C89200 !important; }
      .mod-step-4 .nav-tabs .nav-link.active { color: #DAA520 !important; border-bottom-color: #DAA520 !important; }
      .mod-step-4 .form-check-input:checked { background-color: #DAA520 !important; border-color: #DAA520 !important; }
      .mod-step-4 details summary { color: #C89200 !important; }
      .mod-step-4 .example-callout { border-color: #DAA520 !important; background: #FFFDE7 !important; }
      .mod-step-4 .step-number { -webkit-text-stroke: 1px #DAA520 !important; }
      .mod-step-5 .card-header { color: #0072B2 !important; }
      .mod-step-5 .btn-primary { background-color: #0072B2 !important; border-color: #0072B2 !important; }
      .mod-step-5 .btn-primary:hover { background-color: #005A8C !important; border-color: #005A8C !important; }
      .mod-step-5 .nav-tabs .nav-link.active { color: #0072B2 !important; border-bottom-color: #0072B2 !important; }
      .mod-step-5 .form-check-input:checked { background-color: #0072B2 !important; border-color: #0072B2 !important; }
      .mod-step-5 details summary { color: #005A8C !important; }
      .mod-step-5 .example-callout { border-color: #0072B2 !important; background: #E3F0F8 !important; }
      .mod-step-5 .step-number { -webkit-text-stroke: 1px #0072B2 !important; }
      .mod-step-6 .card-header { color: #D55E00 !important; }
      .mod-step-6 .btn-primary { background-color: #D55E00 !important; border-color: #D55E00 !important; }
      .mod-step-6 .btn-primary:hover { background-color: #B84E00 !important; border-color: #B84E00 !important; }
      .mod-step-6 .nav-tabs .nav-link.active { color: #D55E00 !important; border-bottom-color: #D55E00 !important; }
      .mod-step-6 .form-check-input:checked { background-color: #D55E00 !important; border-color: #D55E00 !important; }
      .mod-step-6 details summary { color: #B84E00 !important; }
      .mod-step-6 .example-callout { border-color: #D55E00 !important; background: #FDF0EB !important; }
      .mod-step-6 .step-number { -webkit-text-stroke: 1px #D55E00 !important; }
      .mod-step-7 .card-header { color: #CC79A7 !important; }
      .mod-step-7 .btn-primary { background-color: #CC79A7 !important; border-color: #CC79A7 !important; }
      .mod-step-7 .btn-primary:hover { background-color: #A85A8A !important; border-color: #A85A8A !important; }
      .mod-step-7 .nav-tabs .nav-link.active { color: #CC79A7 !important; border-bottom-color: #CC79A7 !important; }
      .mod-step-7 .form-check-input:checked { background-color: #CC79A7 !important; border-color: #CC79A7 !important; }
      .mod-step-7 details summary { color: #A85A8A !important; }
      .mod-step-7 .example-callout { border-color: #CC79A7 !important; background: #F9EAF3 !important; }
      .mod-step-7 .step-number { -webkit-text-stroke: 1px #CC79A7 !important; }
      /* Alert text legibility override — teal success alert needs dark text */
      .alert.alert-success { color: #1a4a4a !important; }
      .alert.alert-warning { color: #5a3a00 !important; }
      .alert.alert-danger { color: #5a1a1a !important; }
    "))
  ),

  div(style = "
    display: flex;
    align-items: center;
    justify-content: space-between;
    background-color: #f2f2f2;
    padding: 10px 30px;
    border-bottom: 3px solid #38a3a9;
    color: #003366;
  ",
    div(style = "display: flex; align-items: center; flex-shrink: 0;",
      optional_logo("TaxaCurate.png", "75", "TaxaCurate")
    ),
    div(style = "text-align: center; flex-grow: 1; margin: 0;",
      h1(style = "font-weight: 800; font-size: 1.5rem; color: #222140; margin: 0; line-height: 1.15; white-space: nowrap;",
        "Post-taxonomic assignment species curation"),
      p(style = "color: #007a7e; font-size: 0.85rem; margin: 4px 0 0 0; line-height: 1.5;",
        "Standardized. Reproducible. Transparent.")
    ),
      div(style = "display: flex; align-items: center; gap: 10px; flex-wrap: nowrap; flex-shrink: 0;",
        optional_logo("CREATE.svg", "75", "CREATE"),
        optional_logo("sustainMare.svg", "50", "sustainMare"),
        optional_logo("AWI_Logo_Farbe_RGB.png", "50", "AWI"),
        optional_logo("HIFMB.png", "50", "HIFMB"),
        optional_logo("THUENEN_LOGO_screen_RGB.png", "75", "Th\u00fcnen Institute")
      )
  ),

  div(class = "pipeline-nav",
      navset_pill(
        id = "main_nav",
        nav_panel("1 · Standardization", div(class = "mod mod-step-1", mod_a_ui("modA"))),
        nav_panel("2 · Agreement Analysis", div(class = "mod mod-step-2", mod_b_ui("modB"))),
        nav_panel("3 · Merging", div(class = "mod mod-step-3", mod_c_ui("modC"))),
        nav_panel("4 · Decontamination", div(class = "mod mod-step-4", mod_d_ui("modD"))),
        nav_panel("5 · Aggregation", div(class = "mod mod-step-5", mod_e_ui("modE"))),
        nav_panel("6 · Ecological Annotation", div(class = "mod mod-step-6", mod_f_ui("modF"))),
        nav_panel("7 · Quality Check", div(class = "mod mod-step-7", mod_g_ui("modG")))
      )
  ),

  div(style = "
    border-top: 1px solid #DDDCD0;
    margin-top: 40px;
    padding: 20px 30px;
    background: #f2f2f2;
  ",
    div(style = "max-width: 1400px; margin: 0 auto; display: flex; justify-content: space-between; align-items: flex-start; flex-wrap: wrap; gap: 24px;",
      div(style = "color: #6B6358; font-size: 0.7rem; line-height: 1.5;",
        p(style = "margin: 0 0 4px 0; font-weight: 600; color: #6B6358;", "Developed by:"),
        p(style = "margin: 0 0 4px 0;",
          optional_logo("THUENEN_LOGO_screen_RGB.png", "75", "Th\u00fcnen Institute")),
        p(style = "margin: 0 0 4px 0;",
          a(href = "https://www.thuenen.de/en/legal-notice-and-data-privacy", target = "_blank",
            "Legal notice / Impressum", style = "color: #005f73;")),
        div(style = "max-width: 520px; padding: 8px 12px; border: 1px solid #DDDCD0; border-radius: 6px; background: #FAFAFA; color: #6B6358; font-size: 0.7rem; line-height: 1.5;",
          "The Johann Heinrich von Th\u00fcnen Institute, Federal Research Institute for Rural ",
          "Areas, Forestry and Fisheries \u2014 Th\u00fcnen Institute in brief \u2014 consists of 15 specialized ",
          "institutes that carry out research and provide policy advice in the fields of economy, ",
          "ecology and technology.")
      ),
      div(style = "display: flex; flex-direction: column; align-items: flex-end; gap: 6px;",
        div(style = "color: #6B6358; font-size: 0.75rem; font-weight: 600;", "Funded by"),
        div(style = "display: flex; align-items: center; gap: 20px;",
          optional_logo("BMFTR.png", "75", "BMFTR"),
          optional_logo("DAM_RGB.svg", "75", "DAM")
        )
      )
    )
  )
)

# ------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------

server <- function(input, output, session) {
  
  onStop(save_gazetteer_caches)
  onStop(save_larval_cache)
  onStop(save_worms_fg_cache)
  onStop(save_worms_synonym_cache)

  # `shared` carries outputs from one module to the next, e.g.
  # shared$modA_blast, shared$modB_combined, shared$modC_agreed, ...
  shared <- reactiveValues()

  mod_a_server("modA", shared)
  mod_b_server("modB", shared)
  mod_c_server("modC", shared)
  mod_d_server("modD", shared)
  mod_e_server("modE", shared)  # Module E = Aggregation
  mod_f_server("modF", shared)  # Module F = Enrichment
  mod_g_server("modG", shared)  # Module G = Validation
}

shinyApp(ui, server)
