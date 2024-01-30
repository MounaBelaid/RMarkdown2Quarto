suppressPackageStartupMessages({
  library(shiny)
  library(readr)
  library(stringr)
  library(dplyr)
  library(zip)
  library(knitr)
  library(tools)
  library(bslib)
})

ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly", # Use a Bootswatch theme
    primary = "#2b8cbe", # Customize the primary color
    lightness = 40 # Adjust lightness
  ),
  tags$img(src = "rmd_quarto.png", style = "width: 50%; height: auto;"),
  tags$style(".checkclass {color:#31a354}"),
  tags$style(".xmarkclass {color:#f03b20}"),
  titlePanel("Convert your Rmd files to Quarto"),
  sidebarLayout(
    sidebarPanel(
      fileInput("rmdfiles", "Choose your Rmd Files", accept = ".Rmd", multiple = TRUE),
      tableOutput("fileTable")
    ),
    mainPanel(
      fluidPage(
        downloadButton("downloadBtn", "Download Quarto Converted Files")
      )
    )
  )
)

server <- function(input, output, session) {
  files_info <- reactive({
    req(input$rmdfiles)
    # Initialize an empty data frame
    file_data <- data.frame(Filename = character(), stringsAsFactors = FALSE)
    # Loop through each file to validate the extension for each file
    for (i in seq_along(input$rmdfiles$datapath)) {
      # Determine the extension and set the icon accordingly
      extension <- tools::file_ext(input$rmdfiles$datapath[i])
      icon <- ifelse(extension == "Rmd", as.character(icon("check", class = "checkclass")),
        as.character(icon("xmark", class = "xmarkclass"))
      )
      file_data <- bind_rows(file_data, data.frame(
        Filename = input$rmdfiles$name[i],
        File.format.validity = icon,
        stringsAsFactors = FALSE
      ))
    }
    return(file_data)
  })

  output$fileTable <- renderTable(
    files_info(),
    sanitize.text.function = function(x) x
  )

  output$downloadBtn <- downloadHandler(
    filename = function() {
      paste("output", "zip", sep = ".")
    },
    content = function(fname) {
      req(input$rmdfiles)
      fs <- c()
      for (i in seq_along(input$rmdfiles$datapath)) {
        content <- read_lines(input$rmdfiles$datapath[i])
        content <- str_replace_all(content, pattern = "output: pdf_document", replacement = "format: pdf")
        qmd_path <- paste0(file_path_sans_ext(input$rmdfiles$name[i]), ".qmd")
        write_lines(x = content, file = qmd_path)
        convert_chunk_header(qmd_path, output = qmd_path)
        fs <- c(fs, qmd_path)
      }
      zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )
}

shinyApp(ui, server)
