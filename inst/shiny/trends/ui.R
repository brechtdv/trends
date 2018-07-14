library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Trend Analysis"),
  
  # Sidebar with file upload control
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose Excel File",
                accept = c(".xls", ".xlsx")),
      selectizeInput("level",
                     "Matrix level to be analysed",
                     c("Matrix level 1" = 1,
                       "Matrix level 2" = 2,
                       "Matrix level 3" = 3,
                       "Matrix level 4" = 4,
                       "Matrix level 5" = 5,
                       "Matrix description" = 6),
                     #multiple = TRUE,
                     selected = 4),
      sliderInput("yearrange",
                  "Range of years to be analysed",
                  min = 2008,
                  max = as.numeric(format(Sys.Date(), "%Y")),
                  value = c(2008, 2016),
                  sep = ""),
      conditionalPanel(
        condition = "output.fileUploaded == true && output.type == 'discrete'",
        tags$hr(),
        downloadButton("export1", "Export output")),
      conditionalPanel(
        condition = "output.fileUploaded == true && output.type == 'continuous'",
        tags$hr(),
        downloadButton("export2", "Export output"))
    ),
    
    mainPanel(
      uiOutput("info1"),
      uiOutput("info2"),
      dataTableOutput("tab")
    )
  )
))
