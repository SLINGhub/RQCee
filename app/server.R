options(shiny.maxRequestSize = 50 * 1024^2)  # Increase file upload size limit
#remotes::install_github("SLINGhub/midar@development")
#shiny::runGitHub("SLINGhub/RQCee", subdir = "app")
library(shiny)
library(rhandsontable)
library(ggplot2)
library(midar)
library(tidyverse)
library(dplyr)
library(stringr)
library(shinyjs)
library(shinyWidgets)
library(openxlsx2)
library(DT)


server <- function(input, output, session) {

    rv <- reactiveValues(mexp = MidarExperiment(),
                       tbl_samples = tibble(),
                       show_filtered = FALSE,
                       plots = NULL,
                       stats_table = tibble(
                        feature_id = character(0),
                        r2_rqc_1 = numeric(0),
                        r2_rqc_2 = numeric(0),
                        slopenorm_rqc_1 = numeric(0),
                        slopenorm_rqc_2 = numeric(0),
                        y0norm_rqc_1 = numeric(0),
                        y0norm_rqc_2 = numeric(0)
                      )
                    )

  observeEvent(input$datafile_path, {

    req(input$datafile_path)

    # Get the file extension----
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
        mexp_temp <- midar::data_import_masshunter(mexp_temp, path = input$datafile_path$datapath, file_format = "csv", use_metadata = TRUE)
      } else if (input$data_type == "mrmkit") {
         mexp_temp <- midar::data_import_mrmkit(mexp_temp, path = input$datafile_path$datapath, use_metadata = TRUE)
      }

      #todo: add acquisition_time_stamp and inj_volume
      tbl <- mexp_temp@dataset_orig |>
        select(analysis_id, any_of("sample_name")) |>
        distinct(analysis_id, .keep_all = FALSE) |>
        mutate(is_selected = FALSE,
               curve_id = NA_character_,
               analyzed_amount = NA_real_,
               analyzed_amount_unit = NA_character_)
      rv$mexp <- mexp_temp
      rv$tbl_samples <- tbl
    }
 })

  # Initialize and render the editable rhandsontable
  output$table <- renderRHandsontable({
    if (nrow(rv$tbl_samples) > 0) {
      rhandsontable(
        rv$tbl_samples,
        width = 1000,
        height = 600,
        useTypes = FALSE  # Disable strict type checking for better performance
      ) %>%
        hot_cols(columnSorting = TRUE, manualColumnMove = TRUE, manualColumnResize = TRUE) %>%
        hot_table(stretchH = "all")  # Adjust column width dynamically
    }
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

  #add metadata for pdf and excel output ----
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
       rename(analysis_id = analysis_id)

      metadata_responsecurves(mexp_local) <- as_tibble(annot)
      mexp_local
    }
  }
  #finish the function of add metadata for pdf and excel output ####

  # Function to format numeric columns to 3 digits
  format_numeric <- function(x) {
    if(is.numeric(x)) {
      return(round(x, 3))
    } else {
      return(x)
    }
  }

  # Function to generate the plot and save it to a temporary file
  generate_plots <- function(as_pdf = TRUE, return_plots = FALSE) {
    temp_file <- tempfile(fileext = ".pdf")

    mexp_local <- add_metadata()

    if (is.null(mexp_local)) return(NULL)

    plts <- qc_plot_responsecurves(data = mexp_local,
                        return_plot_list = return_plots,
                        filter_data = FALSE,
                        cols_page = input$n_cols, rows_page = input$n_rows,
                        point_size = 2, line_width = 1.2,
                        scale_factor = 1, font_base_size = 6,
                        save_pdf = as_pdf,
                        path = temp_file )

    if(return_plots)
      plts
    else
      temp_file

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
      if (!is.null(mexp_local)) {
      ResponseCurve <- midar::get_response_curve_stats(data = mexp_local,
                            with_staturation_stats = FALSE,
                            limit_to_rqc = FALSE) |>
                              mutate(across(where(is.numeric), format_numeric))

      # Define expected columns
      expected_columns <- c(
        "feature_id",
        "r2_rqc_1", "slopenorm_rqc_1", "y0norm_rqc_1",
        "r2_rqc_2", "slopenorm_rqc_2", "y0norm_rqc_2"
      )

      # Add missing columns with default values
      for (col in expected_columns) {
        if (!col %in% names(ResponseCurve)) {
          ResponseCurve[[col]] <- NA
        }
      }

      # Ensure column order matches the expected structure
      ResponseCurve <- ResponseCurve[, expected_columns, drop = FALSE]

      # Set a global placeholder for NAs
      options(openxlsx2.na.strings = "")

      #add the color style to the excel table
      wb <- wb_workbook()
      wb$add_dxfs_style(name = "redStyle", font_color = wb_color(hex = "grey0"), bg_fill = wb_color(hex = "pink"))
      wb$add_dxfs_style(name = "brownStyle", font_color = wb_color(hex = "grey0"), bg_fill = wb_color(hex = "#F0E2C4"))

      wb$add_worksheet("ResponseCurve")
      wb$add_data("ResponseCurve", ResponseCurve)


      n_rows <- nrow(mexp_local@annot_features)+1

      wb$add_conditional_formatting(
        sheet = "ResponseCurve",
        dims = wb_dims( rows = 2:n_rows, cols = 2 ),
        rule = "<0.8",
        style = "brownStyle"
      )
      wb$add_conditional_formatting(
        sheet = "ResponseCurve",
        dims = wb_dims( rows = 2:n_rows, cols = 5 ),
        rule = "<0.8",
        style = "brownStyle"
      )


      wb$add_conditional_formatting(
        sheet = "ResponseCurve",
        dims = wb_dims( rows = 2:n_rows, cols = 2 ),
        rule = "<0.7",
        style = "redStyle"
      )
      wb$add_conditional_formatting(
        sheet = "ResponseCurve",
        dims = wb_dims( rows = 2:n_rows, cols = 5 ),
        rule = "<0.7",
        style = "redStyle"
      )

      wb$add_conditional_formatting(
        sheet = "ResponseCurve",
        dims = wb_dims( rows = 2:n_rows, cols = 3 ),
        rule = "<0.75",
        style = "brownStyle"
      )
      wb$add_conditional_formatting(
        sheet = "ResponseCurve",
        dims = wb_dims( rows = 2:n_rows, cols = 6 ),
        rule = "<0.75",
        style = "brownStyle"
      )

      wb$add_conditional_formatting(
        sheet = "ResponseCurve",
        dims = wb_dims( rows = 2:n_rows, cols = 4 ),
        rule = ">0.5",
        style = "brownStyle"
      )
      wb$add_conditional_formatting(
        sheet = "ResponseCurve",
        dims = wb_dims( rows = 2:n_rows, cols = 7 ),
        rule = ">0.5",
        style = "brownStyle"
      )

      #add a sheet with the peak area of the samples
      dataset_wide <- mexp_local@dataset_orig |>
        select(raw_data_filename, acquisition_time_stamp, feature_id, feature_area) |>
        pivot_wider(values_from = feature_area, names_from = feature_id) |>
        arrange(acquisition_time_stamp)


      # Add a worksheet
      wb$add_worksheet("Data")
      wb$add_data("Data", dataset_wide)

      wb_save(wb, file)
      }

      # Hide spinner
      shinyjs::hide("popup")
    }
  )

  output$plot_rqc <- renderPlot({
    req(rv$plots)
    print(rv$plots[[as.numeric(input$select_page)]])

  })

  observeEvent(input$get_stats, {
  shinyjs::show("popup")

  mexp_local <- add_metadata()

  if (!is.null(mexp_local)) {
    # Retrieve stats table and dynamically adjust for missing columns
    table_result <- midar::get_response_curve_stats(data = mexp_local,
                                                    with_staturation_stats = FALSE,
                                                    limit_to_rqc = FALSE) |>
      mutate(across(where(is.numeric), format_numeric))

    # Define expected columns
    expected_columns <- c(
        "feature_id",
        "r2_rqc_1", "slopenorm_rqc_1", "y0norm_rqc_1",
        "r2_rqc_2", "slopenorm_rqc_2", "y0norm_rqc_2"
      )

    # Add missing columns with default values
    for (col in expected_columns) {
      if (!col %in% names(table_result)) {
        table_result[[col]] <- NA
      }
    }

    # Ensure column order matches the expected structure
    rv$stats_table <- table_result[, expected_columns, drop = FALSE]
  }

    shinyjs::hide("popup")
  })


  output$stats_table <- renderDT({
    req(rv$stats_table)

      if (nrow(rv$stats_table) == 0) {
        # Show an empty table with a message
        datatable(
          tibble(message = "No data available. Please click 'Retrieve statistics' to generate the data."),
          options = list(dom = "t", paging = FALSE),
          rownames = FALSE
      )
    } else {
        datatable(rv$stats_table,
                  options = list(
                    pageLength = 10,
                    autoWidth = TRUE,
                    columnDefs = list(
                      list(className = 'dt-center', targets = "_all")
                    )
                  ),
                  escape = FALSE) |>
          formatStyle(
            columns = c("r2_rqc_1", "r2_rqc_2"),
            backgroundColor = styleInterval(c(0.7, 0.8), c("pink", "#F0E2C4", "white"))
          ) |>
          formatStyle(
            columns = c("slopenorm_rqc_1", "slopenorm_rqc_2"),
            backgroundColor = styleInterval(0.75, c("#F0E2C4", "white"))
          ) |>
          formatStyle(
            columns = c("y0norm_rqc_1", "y0norm_rqc_2"),
            backgroundColor = styleInterval(0.5, c("white", "#F0E2C4"))
          )
    }
  })


  observeEvent(input$get_plots, {
    shinyjs::show("popup")
    plts <- generate_plots(as_pdf = TRUE, return_plots = TRUE)
    rv$plots <- plts
    shinyjs::hide("popup")
  })

}
