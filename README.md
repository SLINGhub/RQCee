# RQCee: Evaluation of Response Curves from Preprocessed MS Data

Welcome to **RQCee**, a Shiny application for evaluating response curves from preprocessed mass spectrometry (MS) data. This tool enables users to upload data, visualize plots, and  download analysis results.

## Online Version of the App
The online version of this app can be access via https://slinghub.shinyapps.io/RQCee

## Features

- **Data Upload**: Accepts CSV and TSV files from MRMkit and MH Quant.
- **Plot Layout**: Customizable with user-defined rows and columns.
- **Data Annotation**: Interactive tables for data selection and filtering.
- **Visualization**: Generate and download detailed plots of response curves.
- **Statistics**: Access and export statistical analyses in Excel format.

## Dependencies

The application requires the following R packages:
- **shiny**
- **bslib**
- **shinyjs**
- **rhandsontable**
- **DT**
- **ggplot2**
- **midar**
- **tidyverse**
- **dplyr**
- **stringr**
- **shinyWidgets**
- **openxlsx2**

## How to Use

1. **Select Format**: Choose between MRMkit or MH Quant.
2. **Upload Data**: Upload your MS data files via the sidebar.
3. **Customize Plot Layout**: Set the number of rows and columns for plot arrangement.
4. **Annotate Data**: Use the annotations tab to apply or clear selections.
5. **Retrieve Plots & Statistics**: Navigate the plots and statistics tabs to generate and view results.
6. **Download Results**: Export plots as PDFs and statistics as Excel files.

## Additional Information

- RQCee offers a user-friendly interface for comprehensive MS data analysis, ensuring intuitive data processing and result accessibility.
- For further assistance or inquiries, please contact the development team: Shanshan Ji (lsijish@nus.edu.sg) or Bo Burla (bo.burla@nus.edu.sg).

---

Ensure your data is formatted correctly to fully utilize RQCee's capabilities. Happy analyzing!
