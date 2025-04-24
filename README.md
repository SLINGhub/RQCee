# RQCee: Evaluation of Response Curves from Preprocessed MS Data

Welcome to **RQCee**, a Shiny application for evaluating response curves from preprocessed mass spectrometry (MS) data. This tool enables users to upload data, visualize plots, and  download analysis results.

## Online Version of the App
The online version of this app can be access via https://slinghub.shinyapps.io/RQCee

## Features

-   **Data Upload**: Accepts CSV for MH Quant and TSV files for MRMkit.
-   **Plot Layout**: Customizable with user-defined rows and columns.
-   **Data Annotation**: Interactive tables for data selection and filtering.
-   **Visualization**: Generate and download detailed plots of response curves.
-   **Statistics**: Access and export statistical analyses in Excel format.

## Dependencies

This application manages its R package dependencies using renv. To ensure you have all the necessary packages installed in their correct versions, please follow these steps:

`install.packages("renv")`

`renv::restore()`

## How to Use

1.  **Select Format**: Choose between MRMkit or MH Quant.
2.  **Upload Data**: Upload your MS data files via the sidebar.
3.  **Customize Plot Layout**: Set the number of rows and columns for plot arrangement.
4.  **Annotate Data**: Use the annotations tab to apply or clear selections.
5.  **Retrieve Plots & Statistics**: Navigate the plots and statistics tabs to generate and view results.
6.  **Download Results**: Export plots as PDFs and statistics as Excel files.

## Additional Information

-   For further assistance or inquiries, please contact the development team: Shanshan Ji ([lsijish\@nus.edu.sg](mailto:lsijish@nus.edu.sg){.email}) or Bo Burla ([bo.burla\@nus.edu.sg](mailto:bo.burla@nus.edu.sg){.email}).

------------------------------------------------------------------------

Ensure your data is formatted correctly to fully utilize RQCee's capabilities. Happy analyzing!
