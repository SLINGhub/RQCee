library(shiny)
library(bslib)
library(shinyjs)
library(rhandsontable)

ui <- page_sidebar(
  title = "RQCee: Evaluation of response curves from preprocessed MS data",
  theme = bs_theme(
    bootswatch = "cosmo",
    base_font = font_google("Inter"),
    navbar_bg = "#005a73"
  ),
  sidebar = sidebar(title = NULL,
    width = "20%",
    h5("Upload Data"),
    NULL,
    position = "left",
    fluidRow(
      column(3, "Format:"),
      column(9,
        radioButtons( "data_type",
                    NULL,
                    choices = c("MRMkit" = "mrmkit", "MH Quant" = "mh_quant"),
                    inline=TRUE))
      ),
      fileInput("datafile_path", NULL, accept = c(".csv", ".tsv")),
    hr(),
    h5("Plot Layout"),
    fluidRow(
      column(width = 6,
        numericInput("n_rows", "Rows", 5, min = 1)),
      column(width = 6,
        numericInput("n_cols", "Columns", 4, min = 1))
    ),
    hr(),
    h5("Save results"),
    downloadButton("download_pdf", "Download Plots as PDF"),
    downloadButton("download_excel", "Download Statistics as Excel")
    ),

      navset_card_tab(
        full_screen = TRUE,
        nav_panel(
          title = "Annotations",
          HTML("Selection (comma-separated)"),
          fluidRow(
            column(width = 6,
              textInput("filter_text", NULL, value = "",  width = "100%")
              #style = "margin-bottom: 20px;"
            ),
            column(width = 3,
              div(
                #style = "margin-top: 20px;",
                actionButton("apply_selection", "Apply Selection",  width = "100%"))),
            column(width = 3,
              div(
                #style = "margin-top: 20px;",
                actionButton("clear_filter", "Unselect all",  width = "100%")))
          ),

          # Show the table
          rHandsontableOutput("table", width = "100%"),

          # Popup window with spinning wheel
          div(id = "popup",
              style = "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 1000; text-align: center; padding-top: 20%;",
              div(style = "background: white; padding: 20px; border-radius: 10px;",
                  p("Processing, please wait...")
              )
          ),

          useShinyjs()  # Initialize shinyjs for showing/hiding the spinner
        ),
        nav_panel(
          title = "Plots",
          fluidRow(
            column(2, actionButton("get_plots", "Retrieve plots")),
            column(2, "Show Page"),
            column(3, selectInput("select_page",
                        NULL,
                        choices = 1:15))
            ),
          plotOutput("plot_rqc", width = "100%")
        ),
        nav_panel(
          title = "Statistics",
          column(2, actionButton("get_stats", "Retrieve statistics")),
          tableOutput("stats_table")
        )



  )
)
