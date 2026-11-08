# ui.R
# Layout orchestrator: header, data management and the three tabs.

fluidPage(
  title = APP_TITLE,
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "custom.js")
  ),

  div(class = "app-header",
      div(class = "header-left", tags$img(src = "wpp_media_logo_white.png", height = "50px")),
      div(class = "header-center", h1(APP_TITLE, class = "app-title"), div(APP_SUBTITLE, class = "app-subtitle")),
      div(class = "header-right", tags$img(src = "wpp_media_logo_white.png", height = "50px"))
  ),

  div(class = "section-card",
      h4("Data Management", class = "card-title"),
      data_upload_ui("upload"),
      tags$hr(),
      h5("Project", class = "vof-subsection-title"),
      project_actions_ui("project")
  ),

  div(id = "main_app_content",
      tabsetPanel(id = "main_tabs", type = "pills",
                  tabPanel("Media", media_tab_ui("media")),
                  tabPanel("KPI", kpi_tab_ui("kpi")),
                  tabPanel("External Sources", external_tab_ui("external")),
                  tabPanel("Dummies", dummies_tab_ui("dummies")),
                  tabPanel("VOF QA", qa_tab_ui("qa")))
  )
)
