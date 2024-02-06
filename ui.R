suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyFiles)
  library(shinyAce)
})

shinyUI(fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2b8cbe",
    lightness = 40
  ),
  tags$img(src = "rmd_quarto.png", style = "width: 50%; height: auto;"),
  tags$style(".checkclass {color:#31a354}"),
  tags$style(".xmarkclass {color:#f03b20}"),
  titlePanel("Convert your Rmd files to Quarto"),
  sidebarLayout(
    sidebarPanel(
      downloadButton("exportButton", "Export the demo file"),
      br(), br(),
      aceEditor("demo_file_content", value = "", mode = "markdown", height = "250px"),
      br(),
      fileInput("rmdfiles", "Choose your Rmd Files", accept = ".Rmd", multiple = TRUE),
      tableOutput("fileTable"),
      br(),
      h6(icon("circle-exclamation", style = "color: red"),
              "Take advantage of processing all the Rmd files in a selected folder only when the app is running locally."),
      shinyDirButton("directory", "Select a folder", "Please select a folder only when the app is running locally"),
      tableOutput("fileTableFromFolder"),
      br(),
      div(
        style = "text-align:left; font-size: 15px",
        strong("View Code "), a(icon("fab fa-github"), href = "https://github.com/MounaBelaid/RMarkdown2Quarto", target = "_blank")
      ),
    ),
    mainPanel(
      fluidPage(
        column(
          5,
          downloadButton("downloadButtonFiles", "Download the Converted Quarto Files"),
          br(), br(),
          HTML("<p>The conversion process, which occurs behind the scenes, consists of the following steps:</p>
               <ol>
               <li>Locate a list of available RMarkdown files for conversion to Quarto. If you run the application
               locally, you can choose a folder from which only .Rmd files will be selected. If you don't have an Rmd file on hand,
               you can export the demo RMarkdown file and import it to test the application. </li>
               <li>Convert each selected file from the .Rmd format to the .qmd format and
               display the selected .Rmd files in tables.</li>
               <li>Transform the Rmd syntax format to the qmd format in the front matter. The available format syntaxes
               to change are :
              <ul>
                <li>output: pdf_document</li>
                <li>output: word_document</li>
                <li>output: html_document</li>
              </ul> </li>
               <li>Convert the in-header chunk option syntax to the in-body syntax in
                the converted Quarto files.</li>
               </ol>")
        )
      )
    )
  )
))
