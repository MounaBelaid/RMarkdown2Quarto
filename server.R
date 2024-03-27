suppressPackageStartupMessages({
  library(shiny)
  library(readr)
  library(stringr)
  library(dplyr)
  library(zip)
  library(knitr)
  library(tools)
  library(fs)
  library(shinyFiles)
  library(shinyAce)
})

shinyServer(function(input, output, session) {
  observe({
    shinyjs::disable("downloadButtonFiles")
  })

  observeEvent(ignoreInit = TRUE, list(
    input$rmdfiles,
    input$directory
  ), {
    shinyjs::enable("downloadButtonFiles")
  })

  updated_content <- reactiveVal(NULL)
  rmd_content <- read_lines("demo_file.Rmd")
  rmd_content <- paste(rmd_content, collapse = "\n")
  updateAceEditor(session, "demo_file_content", value = rmd_content)
  observeEvent(input$demo_file_content, {
    updated_content(input$demo_file_content)
  })

  output$exportButton <- downloadHandler(
    filename = function() {
      paste0("demo_file", ".Rmd")
    },
    content = function(file) {
      write_lines(updated_content(), file)
    }
  )

  volumes <- c(Home = path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)

  files_info <- reactive({
    rmd_files <- input$rmdfiles
    req(rmd_files)
    process_rmd_files <- function(i) {
      extension <- file_ext(rmd_files$datapath[i])
      icon <- ifelse(tolower(extension) == "rmd", as.character(icon("check", class = "checkclass")),
        as.character(icon("xmark", class = "xmarkclass"))
      )
      dat <- data.frame(
        Filename = rmd_files$name[i],
        File.format.validity = icon
      )

      return(dat)
    }
    df_imported_files <- lapply(seq_along(rmd_files$datapath), process_rmd_files)
    file_data <- bind_rows(df_imported_files)

    return(file_data)
  })

  files_info_from_folder <- reactive({
    req(input$directory)
    files_in_selected_folder <- list.files(parseDirPath(volumes, input$directory), pattern = "\\.Rmd$|\\.rmd$")
    if (length(files_in_selected_folder) > 0) {
      dat_folder <- data.frame(
        Filename = files_in_selected_folder,
        File.format.validity = as.character(icon("check", class = "checkclass"))
      )
    } else {
      # Handle the case when there are no files in the folder
      dat_folder <- data.frame(Filename = character(0), File.format.validity = character(0))
    }

    return(dat_folder)
  })

  output$fileTable <- renderTable(
    files_info(),
    sanitize.text.function = function(x) x
  )

  output$fileTableFromFolder <- renderTable(
    files_info_from_folder(),
    sanitize.text.function = function(x) x
  )

  output$downloadButtonFiles <- downloadHandler(
    filename = function() {
      paste0("Quarto_converted_files_", format(Sys.time(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S"), ".zip")
    },
    content = function(fname) {
      files_path <- c()
      parse_dir <- parseDirPath(volumes, input$directory)
      rmd_files <- input$rmdfiles
      rmd_files <- rmd_files[tolower(file_ext(rmd_files$datapath)) == "rmd", , drop = FALSE]
      files_in_folder <- list.files(parse_dir, pattern = "\\.Rmd$|\\.rmd$", full.names = TRUE)

      if (length(files_in_folder) > 0 & length(rmd_files) > 0) {
        rmd_paths <- c(rmd_files$datapath, files_in_folder)
        files_in_folder <- list.files(parse_dir, pattern = "\\.Rmd$|\\.rmd$")
        rmd_names <- c(file_path_sans_ext(rmd_files$name), file_path_sans_ext(files_in_folder))
        rmd_names <- addNumericSuffix(rmd_names)
      } else if (length(files_in_folder) > 0) {
        files_in_folder <- list.files(parse_dir, pattern = "\\.Rmd$|\\.rmd$", full.names = TRUE)
        rmd_paths <- files_in_folder
        files_in_folder <- list.files(parse_dir, pattern = "\\.Rmd$|\\.rmd$")
        rmd_names <- file_path_sans_ext(files_in_folder)
        rmd_names <- addNumericSuffix(rmd_names)
      } else if (length(rmd_files) > 0) {
        rmd_paths <- rmd_files$datapath
        rmd_names <- file_path_sans_ext(rmd_files$name)
        rmd_names <- addNumericSuffix(rmd_names)
      }

      for (i in seq_along(rmd_paths)) {
        content <- read_lines(rmd_paths[i])
        if ("output: pdf_document" %in% content) {
          content <- str_replace_all(content, pattern = "output: pdf_document", replacement = "format: pdf")
        } else if ("output: word_document" %in% content) {
          content <- str_replace_all(content, pattern = "output: word_document", replacement = "format: word")
        } else if ("output: html_document" %in% content) {
          content <- str_replace_all(content, pattern = "output: html_document", replacement = "format: html")
        }
        qmd_path <- paste0(rmd_names[i], ".qmd")
        write_lines(x = content, file = qmd_path)
        convert_chunk_header(qmd_path, output = qmd_path)
        files_path <- c(files_path, qmd_path)
      }

      zip(zipfile = fname, files = files_path)
    },
    contentType = "application/zip"
  )
})
