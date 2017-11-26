library(codetools)
library(shiny)
library(rmarkdown)
library(ggplot2)

source("helpers.R")

shinyServer(function(input, output) {
  
  output$info1 <- renderUI(
    if (is.null(input$file) & is.null(in_data())) {
      h1("Please select a database")
    })
  output$info2 <- renderUI(
    if (!is.null(input$file) & is.null(in_data())) {
      h1("Processing")
    })
  
  in_data <- reactive({
    # input$file will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    out <- fit_all(inFile$datapath, input$level)
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(in_data()))
  })
  
  output$tab <- renderDataTable({
    in_data()$out_tab
  })
  
  output$type <- reactive({
    in_data()$type
  })
 
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  outputOptions(output, 'type', suspendWhenHidden = FALSE)
  
  output$export1 <-
    downloadHandler(
      filename =
        function() {
          paste0(gsub("\\.xls.", "", input$file$name),
                 "_LEVEL", input$level, "_TRENDS.docx")
        },
      content = 
        function (file) {
          tempReport <- file.path(tempdir(), "report1.Rmd")
          file.copy("report1.Rmd", tempReport, overwrite = TRUE)
          
          # generate docx
          rmarkdown::render(
            input = "report1.Rmd",
            output_file = file,
            params = list(inFile = input$file[1],
                          level = input$level,
                          out = in_data()),
            envir = new.env(parent = globalenv()))
        }
    )
  
  output$export2 <-
    downloadHandler(
      filename =
        function() {
          paste0(gsub("\\.xls.", "", input$file$name),
                 "_LEVEL", input$level, "_TRENDS.docx")
          },
      content =
        function (file) {
          tempReport <- file.path(tempdir(), "report2.Rmd")
          file.copy("report2.Rmd", tempReport, overwrite = TRUE)
          
          # generate docx
          rmarkdown::render(
            input = "report2.Rmd",
            output_file = file,
            params = list(inFile = input$file[1],
                          level = input$level,
                          out = in_data()),
            envir = new.env(parent = globalenv()))
        }
    )
  
})
