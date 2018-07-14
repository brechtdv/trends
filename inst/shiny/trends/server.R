library(codetools)
library(shiny)
library(rmarkdown)
library(ggplot2)

source("helpers.R")

shinyServer(function(input, output) {
  
  in_data <- reactive({
    ## no file selected
    validate(
      need(input$file != "", "Please select a database")
    )
    
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    ## extract sheet names
    sheets <- excel_sheets(inFile$datapath)
    
    ## define years of analysis
    yrs <- seq(input$yearrange[1], input$yearrange[2])
    
    ## transfer to appropriate function
    if ("Tout Programmation" %in% sheets) {
      output$info1 <-
        renderUI({
          tagList(
            div("Identified dataset: Conformity data",
                class="shiny-output-error-validation"),
            hr()
          )
        })
      fit_discrete(inFile$datapath, yrs, input$level)
        
    } else {
      ## read data
      x <- readxl(inFile$datapath)
      
      ## extract relevant columns
      col_name_fr <- ifelse(input$level == 6, "Mat description", paste("Mat Niveau", input$level))
      cols_fr <- c("Ann\xE9e", col_name_fr, "Param\xE8tre", "Ana.Ech: R\xE9sultat")
      cols_fr <- iconv(cols_fr, "latin1", "UTF-8")
      
      col_name_nl <- ifelse(input$level == 6, "Mat omschrijving", paste("Mat Niveau", input$level))
      cols_nl <- c("Jaar", col_name_nl, "Parameter", "Mon.ana Resultaat")
      
      ## check col names
      validate(
        need(all(make.names(cols_fr) %in% colnames(x)) || all(make.names(cols_nl) %in% colnames(x)),
             paste("Error: the following columns are required:\n",
                   "FR:", paste(sQuote(cols_fr), collapse = ", "), "\n",
                   "NL:", paste(sQuote(cols_nl), collapse = ", "))))
      
      ## detect language
      if ("Jaar" %in% colnames(x)) {
        lang <- "NL"
        
        ## clean data
        x <- x[, make.names(cols_nl)]
        colnames(x) <- c("year", "matrix", "parameter", "result")
        
      } else {
        lang <- "FR"
        
        ## clean data
        x <- x[, make.names(cols_fr)]
        colnames(x) <- c("year", "matrix", "parameter", "result")
      }
      
      ## create info output
      output$info1 <-
        renderUI({
          tagList(
            div("Identified dataset: Continuous data",
                class="shiny-output-error-validation"),
            div(paste("Identified language:", lang),
                class="shiny-output-error-validation"),
            hr()
          )
        })
      
      ## check for non-numeric or zero values
      is_na <- !grepl("[0-9]", x$result)
      is_zero <-
        suppressWarnings(
          as.numeric(
            gsub(",", ".",
                 sub("\\D+$", "",
                     sub("^\\D+", "", x$result)))) == 0)
      is_zero[is.na(is_zero)] <- FALSE

      warning1 <- warning2 <- NULL
      
      if (any(is_na))
        warning1 <-
        div(paste("Removed", sum(is_na), "observations:",
                  paste(sQuote(x$result[is_na]), collapse = ", ")),
            class = "alert alert-warning")
      
      if (any(is_zero))
        warning2 <-
        div(paste("Removed", sum(is_zero), "zero observations."),
            class = "alert alert-warning")
      
      if (!is.null(warning1) | !is.null(warning2)) {
        output$info2 <-
          renderUI({
            tagList(
              warning1,
              warning2,
              hr()
            )
          })
        
      } else {
        output$info2 <- NULL
      }

      ## fit model
      fit_continuous(x, yrs, input$level)
    }
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
                          yearrange = input$yearrange,
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
                          yearrange = input$yearrange,
                          out = in_data()),
            envir = new.env(parent = globalenv()))
        }
    )
  
})
