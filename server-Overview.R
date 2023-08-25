observe({
  ### TODO:
  # context selection
  # settingsTable:
  # - parameters
  # - test
  # summaryTable
  # - number of hypo/hyper-metyhlated (significant) regions/loci
  # general display options:
  #   - theme
  #   - colors (groups): qual, seq, div
  #   - 
  
  # allCytosineContexts <- inputDataReactive()$allCytosineContexts
  # sampleNames <- inputDataReactive()$sampleNames
  param <- inputDataReactive()$param
  contexts <- inputDataReactive()$contexts
  dataset <- inputDataReactive()$dataset
  factorNames <- inputDataReactive()$factorNames
  factorLevels <- inputDataReactive()$factorLevels
  factors <- inputDataReactive()$factors
  
  
  sampleNames <- dataset["Name"]
  
  
  # fileList <- inputDataReactive()$fileList
  # print(fileList)
  
  if(length(contexts)==3) {
    # print("------- updateRadioGroupButtons -------")
    updateRadioGroupButtons(
      session = getDefaultReactiveDomain(),
      inputId = "selectContext",
      label = "Cytosine context",
      # choices = contexts,
      choices = c("CpG", "CHG", "CHH", "All"),
      # selected = "CpG",
      status = "primary"
    )
  }
  
  # counter <- reactiveVal(0)
  # 
  # bindEvent( # Event
  #   {
  #     output$generalOptions <- renderMenu({
  #       menuItem(
  #         text = "Options",
  #         # tabName = "tab-Contexts",
  #         startExpanded = TRUE,
  #         radioGroupButtons(
  #           inputId = "selectContext",
  #           label = "Cytosine context",
  #           choices = contexts,
  #           # choices = c("CpG", "CHG", "CHH"),
  #           # selected = "CpG",
  #           status = "primary"
  #         ),
  #         radioGroupButtons(
  #           inputId = "selectMetyhlation",
  #           label = "Methylation",
  #           # choices = contexts,
  #           choices = c("Hypo", "Hyper", "All"),
  #           # selected = "Hypo",
  #           status = "primary"
  #         )
  #       )
  #     }) # close renderMenu
  #   # print("executing bindEvent")
  #   
  #   },
  #   {
  #     input$colorPalette
  #   },
  #   
  #   ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
  #   ignoreNULL = F, # default = TRUE
  #   once = TRUE
  # ) # close event
  
  ### samples & grouping table
  samplesDF <- dataset[c("Name", factorNames)]
  
  output$samplesTable <- function() {
    knitr::kable(samplesDF,
                 format = "html",
                 col.names = c("Sample names", factorNames), row.names = F
    ) %>%
      kable_styling(
        bootstrap_options = "striped", full_width = FALSE,
        position = "left"
      )
  }
  
  ### settingsTable
  observeEvent( # Event EmpiricalDistribution
    {
      input$selectContext
      input$selectMetyhlation
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
      # print("------- observeEvent table -------")
      settingsDF <- data.frame("All contexts" = param$allCytosineContexts,
                               "Cytosine context" = input$selectContext,
                               "Methylation" = input$selectMetyhlation,
                               "Reference" = param$refBuild,
                               "Grouping variable" = param$grouping,
                               "Biomart dataset" = param$biomart_selection,
                               # TODO: add cuoff params
                               check.names = FALSE)
                               
      output$settingsTable <- function() {
        knitr::kable(t(settingsDF),
                     format = "html",
                     col.names = "Setting", row.names = TRUE
        ) %>%
          kable_styling(
            bootstrap_options = "striped", full_width = FALSE,
            position = "left", font_size = 13
          ) %>%
          column_spec(1, bold = T)
      }
    }
  )
  
  # settingsDF <- data.frame("Cytosine context" = "CpG",
  #                          "Methylation" = "Hypo"
  # )

  # settingsDF <- data.frame("Context" = "CpG")
  
  # output$settingsTable <- function() {
  #   knitr::kable(t(settingsDF),
  #                format = "html",
  #                col.names = "Setting", row.names = TRUE
  #   ) %>%
  #     kable_styling(
  #       bootstrap_options = "striped", full_width = FALSE,
  #       position = "left"
  #     )
  # }
  
  ### summaryTable
  # summaryDF <- data.frame()
  # output$summaryTable <- function() {
  #   knitr::kable(as.data.frame(settings),
  #                format = "html",
  #                col.names = "Number", row.names = TRUE
  #   ) %>%
  #     kable_styling(
  #       bootstrap_options = "striped", full_width = FALSE,
  #       position = "left"
  #     )
  # }
  # a <- display.brewer.all(n=n_brewerPal, select=rownames(palettesCategorical), exact.n=TRUE)
  # display settings
  n_brewerPal <- 6
  palettesCategorical <- brewer.pal.info[brewer.pal.info$category=="qual" & brewer.pal.info$maxcolors >= n_brewerPal & brewer.pal.info$colorblind==TRUE, ]
  
  palettesDiverging <- brewer.pal.info[brewer.pal.info$category=="div" & brewer.pal.info$maxcolors >= n_brewerPal & brewer.pal.info$colorblind==TRUE, ]
  # display.brewer.all(n=NULL, type="div", select=rownames(palettesDiverging), exact.n=TRUE, colorblindFriendly = TRUE) 
                     
  # n <- n_brewerPal
  output$colorPalettesCategoricalPlot <- renderPlot({
    display.brewer.all(n=n_brewerPal, select=rownames(palettesCategorical), exact.n=TRUE)
  })
  
  output$colorPalettesDivergingPlot <- renderPlot({
    display.brewer.all(n=NULL, type="div", select=rownames(palettesDiverging), exact.n=TRUE, colorblindFriendly = TRUE) 
  })
  
  
  updateRadioGroupButtons(
    inputId = "colorPalette",
    choices = rownames(palettesCategorical),
    selected = rownames(palettesCategorical)[1]
  )
  
  updateRadioGroupButtons(
    inputId = "colorPaletteDiverging",
    choices = rownames(palettesDiverging),
    selected = rownames(palettesDiverging)[1]
  )

  # colorLevels <- c(factorLevels, "Additional color 1", "Additional color 2")
  colorLevels <- c(factorLevels, paste("Additional color", 1:(6-length(factorLevels))))
  
  
  paste("Additional color", 1:3)
  
  # lapply(1:50, function(i) {
  observeEvent( # Event EmpiricalDistribution
    {
      input$colorPalette
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
        lapply(seq_along(colorLevels), function(i) {
          colourpicker::updateColourInput(
            session = session,
            inputId = paste0("GroupColour", i),
            label = colorLevels[i],
            value =  brewer.pal(n_brewerPal, input$colorPalette)[i],
            palette = "limited",
            allowedCols = brewer.pal(n_brewerPal, input$colorPalette),
            closeOnClick = TRUE,
            returnName = TRUE
          )
        })
    }
  )
  
  # output$cytosineContext <- renderPrint({ input$colorPalette })
  
  # updateSelectizeInput( # maybe this is not needed (always give all options)
  #   # session = getDefaultReactiveDomain(),
  #   session = session,
  #   inputId = "selectizecolors",
  #   choices = sampleNames,
  #   selected = sampleNames,
  #   # selected = character(0),
  #   server = TRUE
  #   # server = TRUE
  # )
  
  # output$colorPanel <- renderUI({
  #   lev <- sort(unique(input$selectizeColors)) # sorting so that "things" are unambigious
  #   # lev <- sampleNames
  #   # colorValues <- gg_fill_hue(length(lev))
  #   cols <- brewer.pal(length(sampleNames), input$colorPalette)
  #   # New IDs "colX1" so that it partly coincide with input$selectize...
  #   lapply(seq_along(lev), function(i) {
  #     colorInput(inputId = paste0("col_", lev[i]),
  #                label = paste0("Choose color for ", lev[i]),
  #                value = cols[i]
  #     )
  #   })
  # })
  
  # clickCount <- reactiveVal(0) 
  # observeEvent( # Event 
  #   {
  #     input$colorPalette
  #     # input$manualcolors
  #   },
  #   ignoreInit = T, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
  #   ignoreNULL = T, # default = TRUE
  #   {
  #     
  #     colorValues <- reactive({
  #       # print("up")
  #       # input$actionButtoncolors
  #       # clickCount()
  #       # if (input$actionButtoncolors == 0) {
  #       # if (clickCount() == 0) {
  #       #   c <- brewer.pal(length(sampleNames), input$colorPalette)
  #       #   # print("if")
  #       # } else {
  #       #   c <- paste0("c(", paste0("input$col_", sort(input$selectizecolors), collapse = ", "), ")")
  #       #   c <- eval(parse(text = c))
  #       #   # print("else")
  #       # }
  #       c <- paste0("c(", paste0("input$col_", sort(input$selectizeColors), collapse = ", "), ")")
  #       c <- eval(parse(text = c))
  #       c
  #     })
  #     
  #     colorGroups <- reactive({
  #       input$colorPalette
  #       
  #       cols <- brewer.pal(n = n_brewerPal, input$colorPalette[1])
  #       cols
  #     })
  #   }
  # )
  
  # observeEvent(input$actionButtoncolors, {
  #   # Increment the click count by 1
  #   clickCount(clickCount() + 1)
  # })
  
  
  
}) # close observe
