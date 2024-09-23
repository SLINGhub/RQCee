options(shiny.maxRequestSize = 50 * 1024^2)  # Increase file upload size limit
#remotes::install_github("SLINGhub/midar@development")
#shiny::runGitHub("SLINGhub/RQCee", subdir = "app")
library(shiny)
library(rhandsontable)
library(writexl)
library(ggplot2)
library(midar)
library(tidyverse)
library(dplyr)
library(stringr)
library(shinyjs)
library(shinyWidgets)

server <- function(input, output, session) {

    rv <- reactiveValues(mexp = MidarExperiment(),
                       tbl_samples = tibble(),
                       show_filtered = FALSE,
                       plots = NULL,
                       stats_table = tibble())

  observeEvent(input$datafile_path, {

    req(input$datafile_path)

    # Get the file extension
    file_ext <- tools::file_ext(input$datafile_path$name)

    # Check if the file extension matches the selected data type
    valid_ext <- switch(input$data_type,
                        mh_quant = "csv",
                        mrmkit = "tsv")

    if (file_ext != valid_ext) {
      # Show an error message and reset the file input
      showModal(modalDialog(
        title = "Error",
        paste("Please upload a", valid_ext, "file."),
        easyClose = TRUE,
        footer = NULL
      ))

      # Reset file input
      reset("datafile_path")
    } else {
      # Create a MidarExperiment object (S4)
      mexp_temp <- MidarExperiment()

      if (input$data_type == "mh_quant") {
        mexp_temp <- midar::rawdata_import_agilent(mexp_temp, path = input$datafile_path$datapath, file_format = "csv", use_metadata = TRUE)
      } else if (input$data_type == "mrmkit") {
         mexp_temp <- midar::rawdata_import_mrmkit(mexp_temp, path = input$datafile_path$datapath, use_metadata = TRUE)
      }

      #todo: add acquisition_time_stamp and inj_volume
      tbl <- mexp_temp@dataset_orig |>
        select(analysis_id, any_of("sample_name")) |>
        distinct(analysis_id, .keep_all = FALSE) |>
        mutate(is_selected = FALSE,
               rqc_series_id = NA_character_,
               relative_sample_amount = NA_real_)
      rv$mexp <- mexp_temp
      rv$tbl_samples <- tbl
    }
 })

  # Initialize and render the editable rhandsontable
  output$table <- renderRHandsontable({
   rhandsontable(rv$tbl_samples, width = 1000, height = 600) %>%
      hot_cols(columnSorting = TRUE, manualColumnMove = TRUE, manualColumnResize = TRUE)
     })

  # Capture the table edited by the user
  observeEvent(input$table, {
    rv$tbl_samples <- hot_to_r(input$table)
 })

  # Selection logic
  observeEvent(input$apply_selection, {
    # TODO: sep can be replaced by | to generate the regex directly
    filter_terms <- str_split(input$filter_text, ",", simplify = TRUE)[1,]
    filter_terms <- str_trim(filter_terms)  # Trim white space

    if (all(filter_terms != "")) {
      # Update the is_selected column based on filter
      rv$tbl_samples  <- rv$tbl_samples |>
        mutate(is_selected = str_detect(analysis_id, paste(filter_terms, collapse = '|')))
    }
  })

  # Clear the selection
  observeEvent(input$clear_filter, {
    updateTextInput(session, "filter_text", value = "")
    rv$tbl_samples <- rv$tbl_samples |>
      mutate(is_selected = FALSE)
  })

  #add metadata for pdf and excel output ####
  add_metadata <- function() {
    mexp_local <- isolate(rv$mexp)
    annot <- isolate(rv$tbl_samples) |>  filter(is_selected)

    # Check for missing values
    if (any(is.na(annot))) {
      showModal(modalDialog(
        title = "Error",
        "Invalid data: Missing values detected.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    } else {
      annot <- annot |>
      rename(analysis_id = analysis_id) |>
      mutate(relative_sample_amount = relative_sample_amount / 100)

      metadata_responsecurves(mexp_local) <- as_tibble(annot)
      mexp_local
    }
  }
  #finish the function of add metadata for pdf and excel output ####


  # Function to generate the plot and save it to a temporary file
  generate_plots <- function(as_pdf, return_plots) {
    temp_file <- tempfile(fileext = ".pdf")

    print("generate_plots")
    mexp_local <- add_metadata()

    plts <- plot_responsecurves(data = mexp_local,
                        return_plot_list = return_plots,
                        use_filt_data = FALSE,
                        columns_page = input$n_cols,
                        rows_page = input$n_rows,point_size = 4, line_width = 1.6, text_scale_factor = 2,  base_size = 10,
                        output_pdf = as_pdf,
                        path = temp_file)

    #print(str(plts$plt))
    plts$plt
  }

  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("Response_curve_", Sys.Date(), ".pdf", sep = "")
    },

    content = function(file) {
      # Show spinner
      shinyjs::show("popup")

      # Generate the plot and save to a temporary file
      plot_file <- generate_plots(as_pdf = TRUE, return_plots = FALSE)

      # Copy the plot to the final location
      file.copy(plot_file, file, overwrite = TRUE)

      # Hide spinner
      shinyjs::hide("popup")
    }
  )

  output$download_excel <- downloadHandler(
    filename = function() {
      paste("RQC_stats_", Sys.Date(), ".xlsx", sep = "")
    },

    content = function(file) {
      # Show spinner
      shinyjs::show("popup")

      mexp_local <- add_metadata()

      # Write the table to an Excel file
      table_result <- midar::get_response_curve_stats(data = mexp_local,
                                                      with_staturation_stats = FALSE,
                                                      limit_to_rqc = FALSE)

      write_xlsx(table_result, file)
      # Hide spinner
      shinyjs::hide("popup")
    }
  )

  output$plot_rqc <- renderPlot({
    req(rv$plots)
    print("plot_curves")
    print(rv$plots[[as.numeric(input$select_page)]])

  })

  output$stats_table <- renderTable({
    req(rv$stats_table)
    rv$stats_table

  })

  observeEvent(input$get_plots, {
    print("generate_plots")
    #shinyjs::show("popup")
    plts <- generate_plots(as_pdf = TRUE, return_plots = TRUE)
    rv$plots <- plts
    #shinyjs::hide("popup")
  })

  observeEvent(input$get_stats, {
    print("get_stats")

    mexp_local <- add_metadata()

    # Write the table to an Excel file
    table_result <- midar::get_response_curve_stats(data = mexp_local,
                                                    with_staturation_stats = FALSE,
                                                    limit_to_rqc = FALSE)

    rv$stats_table <- table_result
    #shinyjs::hide("popup")
  })

}



