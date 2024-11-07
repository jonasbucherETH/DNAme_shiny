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
  # param <- inputDataReactive()$param
  contexts <- inputDataReactive()$contexts
  dataset <- inputDataReactive()$dataset
  factorNames <- inputDataReactive()$factorNames
  factorLevels <- inputDataReactive()$factorLevels
  factors <- inputDataReactive()$factors
  sampleNames <- inputDataReactive()$sampleNames
  myPalette <- inputDataReactive()$myPalette
  colourList <- inputDataReactive()$colourList
  testCovariate <- inputDataReactive()$testCovariate
  
  colorLevels <- c(factorLevels, rownames(dataset))
  
  
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
  # samplesDF <- dataset[c("Name", factorNames)]
  samplesDF <- dataset
  
  ## removed that card
  # output$samplesTable <- function() {
  #   knitr::kable(samplesDF,
  #                format = "html",
  #                col.names = c("Sample names", factorNames), row.names = F
  #   ) %>%
  #     kable_styling(
  #       bootstrap_options = "striped", full_width = FALSE,
  #       position = "left"
  #     )
  # }
  
  ### select context and methylation (hypo/hyper)
  observeEvent( # Event select context and methylation
    {
      input$selectContext
      input$selectMetyhlation
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
      # print("------- observeEvent table -------")
      settingsDF <- data.frame(#"All contexts" = param$allCytosineContexts,
                               "Cytosine context" = input$selectContext,
                               "Methylation" = input$selectMetyhlation,
                               #"Reference" = param$refBuild,
                               "Grouping variable" = testCovariate,
                               #"Biomart dataset" = param$biomart_selection,
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
  n_brewerPal <- length(sampleNames)
  palettesCategorical <- brewer.pal.info[brewer.pal.info$category=="qual" & brewer.pal.info$maxcolors >= n_brewerPal, ]
  palettesGroup <- palettesCategorical[c(1,3),] # Set2 and Dark2
  palettesPaired <- palettesCategorical[2,] # Paired
  palettesDiverging <- brewer.pal.info[brewer.pal.info$category=="div" & brewer.pal.info$maxcolors >= n_brewerPal, ]
  # display.brewer.all(n=NULL, type="div", select=rownames(palettesDiverging), exact.n=TRUE, colorblindFriendly = TRUE) 
             
  palettesGroup <- palettesCategorical
          
  # n <- n_brewerPal
  observeEvent( # Event colorblind-friendly switch
    {
      input$colorblind_only
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
      
      # output$color_buttons_diverging <- renderUI({
      #   button_list <- lapply(seq_along(palettesDiverging[,1]), function(i) {
      #     radioGroupButtons(
      #       inputId = paste0("color_button_", i),
      #       label = paste("Sample", i),
      #       choices = "RdPu",
      #       selected = "RdPu",
      #       direction = "horizontal",
      #       individual = TRUE,
      #       status = "default",
      #       size = "normal",
      #       disabled = FALSE # maybe as option to disable it when palette not chosen currently
      #       # individual = TRUE
      #     )
      #   })
      #   fluidRow(do.call(tagList, button_list))
      # })
      
      active_palette_groups <- reactiveVal(NULL)
      # available_palettes_groups <- rownames(palettesGroup[palettesGroup$colorblind == TRUE | palettesGroup$colorblind == input$colorblind_only, ])
      available_palettes_groups <- rownames(palettesGroup)
      
      # output$palette_radio_buttons_groups <- renderUI({
      #   radioGroupButtons(
      #     inputId = "active_palette",
      #     label = NULL,
      #     choices = available_palettes_groups,
      #     individual = TRUE,
      #     direction = "vertical",
      #     status = "radio-class"
      #   )
      # })
      
      output$color_buttons_groups <- renderUI({
        # Create a fluidRow to contain both the radio buttons and the checkboxGroupButtons
        fluidRow(
          # Column for the radioGroupButtons
          # column(
          #   width = 2,
          #   # div(
          #     # style = "display: flex; flex-direction: column; margin-bottom: 30px;", # Ensures vertical stacking of buttons
          #     radioGroupButtons(
          #       inputId = "active_palette_groups",
          #       label = NULL,
          #       # choiceNames = lapply(available_palettes_groups, function(name) {
          #       #   # div(style = "display: block; padding: 10px; border-radius: 5px;",
          #       #   # div(style = "background-color: #0275d8; margin-bottom: 10px; width: 70px; height: 40px; display: block; position: relative;",
          #       #   div(style = "display: block; position: relative; margin-bottom: 10px;",
          #       #       name
          #       #   )
          #       #
          #       # }),
          #       # choiceValues = available_palettes_groups,
          #       choices = available_palettes_groups,
          #       individual = TRUE,
          #       justified = FALSE,
          #       direction = "vertical",
          #       # size = "xs",
          #       status = "radio-class"
          #       # status = "primary"
          #     )
          #   # )
          # ),

          # Column for the checkboxGroupButtons (color palette)
          column(
            width = 12,
            # div(style = "height: 14px;"),
            # Iterate over each palette to create a row of buttons
            do.call(tagList, lapply(available_palettes_groups, function(palette_name) {
              
              colors <- brewer.pal(n = brewer.pal.info[palette_name, 1], name = palette_name)
              
              # tags$label(palette_name)
              
              # Create a checkboxGroupButtons for each color in the palette
              fluidRow(
                column(
                  width = 2,
                  if (input$colorblind_only==TRUE & brewer.pal.info[palette_name, 3]==FALSE) {
                    h5(palette_name, class = "disabled")
                  } else {
                    h5(palette_name)
                  }
                ),
                column(
                  width = 10,
                  checkboxGroupButtons(
                    # inputId = paste0("color_select_", palette_name),
                    inputId = "color_select",
                    label = NULL,  # No label for the palette row
                    # label = palette_name,  # label = palette name, without radiobuttons
                    selected = if (palette_name=="Dark2") colors[1:length(colourList)] else NULL,
                    direction = "horizontal",
                    status = "custom-class",
                    size = "xs",
                    individual = TRUE,
                    justified = FALSE,
                    choiceNames = lapply(colors, function(color) {
                      div(
                        style = sprintf("background-color: %s; width: 40px; height: 40px; display: inline-block; position: relative;", color),
                        tags$i(class = "checkmark fa fa-check-circle")
                      )
                    }),
                    choiceValues = colors,
                    disabled = if (input$colorblind_only==TRUE & brewer.pal.info[palette_name, 3]==FALSE) TRUE else FALSE
                  )
                )
              ) # end fluidRow for color buttons
            }))
          )
        )
      })
      
      # # Monitor the active palette and update enabled/disabled states
      # observe({
      #   # selected_palette <- input$palette_radio
      #   selected_palette_groups <- input$active_palette_groups
      #   active_palette_groups(selected_palette_groups)
      #   
      #   # Loop through all palettes to enable/disable based on the selected radio button
      #   sapply(rownames(palettesGroup), function(palette_name) {
      #     inputId <- paste0("color_select_", palette_name)
      #     updateCheckboxGroupButtons(
      #       session,
      #       inputId = inputId,
      #       disabled = if (is.null(active_palette_groups())) TRUE else palette_name != active_palette_groups()
      #     )
      #   })
      # })

      # Observe all palette inputs to enforce a maximum of 2 selections
      observe({
        # Collect all inputs dynamically based on palette names
        all_selected <- unlist(sapply(names(palettesDiverging), function(palette_name) {
          input[[paste0("color_select_", palette_name)]]
        }))

        # Check if the total number of selections is 2 or more
        if (length(all_selected) >= 2) {
          # Loop through each palette input
          sapply(names(palettesDiverging), function(palette_name) {
            inputId <- paste0("color_select_", palette_name)

            # Disable options that are not selected
            selected_in_palette <- input[[inputId]]
            if (length(selected_in_palette) < 2) {
              updateCheckboxGroupButtons(session, inputId, disabledChoices = setdiff(brewer.pal(n = 11, name = palette_name), selected_in_palette))
            }
          })
        } else {
          # Re-enable all options if fewer than 2 are selected
          sapply(names(palettesDiverging), function(palette_name) {
            inputId <- paste0("color_select_", palette_name)
            updateCheckboxGroupButtons(session, inputId, disabledChoices = character(0))
          })
        }
      })
      
      # output$colorPalettesCategoricalPlot <- renderPlot({
      #   display.brewer.all(n=8, select=rownames(palettesCategorical), colorblindFriendly = input$colorblind_only)
      # })
      # 
      # output$colorPalettesPairedPlot <- renderPlot({
      #   display.brewer.all(n=10, select=rownames(palettesPaired), colorblindFriendly = input$colorblind_only)
      # })
      # 
      # output$colorPalettesDivergingPlot <- renderPlot({
      #   display.brewer.all(n=NULL, type="div", select=rownames(palettesDiverging), colorblindFriendly = input$colorblind_only) 
      # })
      # 
      # updateRadioGroupButtons(
      #   inputId = "colorPalette",
      #   choices = rownames(palettesCategorical),
      #   selected = rownames(palettesCategorical)[1]
      # )
      # 
      # updateRadioGroupButtons(
      #   inputId = "colorPaletteDiverging",
      #   choices = rownames(palettesDiverging[palettesDiverging$colorblind==TRUE | palettesDiverging$colorblind==input$colorblind_only, ]),
      #   selected = rownames(palettesDiverging[palettesDiverging$colorblind==TRUE | palettesDiverging$colorblind==input$colorblind_only, ])[1]
      # )
    }
  )   
  
  observeEvent( # Event assign colours groups
    {
      # input$color_select
      input$confirm_color_selection
      req(input$color_select)
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = F, # default = TRUE
    {
      print("confirm_color_selection")
      print(input$confirm_color_selection)
      # all_selected <- unlist(sapply(names(palettesDiverging), function(palette_name) {
      #   input[[paste0("color_select_", palette_name)]]
      # }))
      # all_selected <- unlist(input[[paste0("color_select_", input$active_palette_groups)]])
      # all_selected <- input[[paste0("color_select_", input$active_palette_groups)]]
      # if(!is.null(input$active_palette_groups)){
      #   all_selected <- input$active_palette_groups
      # } else{
      #   all_selected <- "Paired"
      # }
      # 
      # print("alL_selected")
      # print(input$color_select)
      
      lapply(seq_along(colorLevels), function(i) {
        # if(i<=0.5*length((colorLevels)))
        colourpicker::updateColourInput(
          session = session,
          inputId = paste0("GroupColour", i),
          label = colorLevels[i],
          # value =  brewer.pal(n_brewerPal, input$active_palette_groups)[i],
          value =  input$color_select[i],
          palette = "limited",
          allowedCols = input$color_select,
          # allowedCols = paste0("color_select_", input$active_palette_groups),
          closeOnClick = TRUE,
          returnName = TRUE
        )
      })
    }
  )

  # colorLevels <- c(factorLevels, "Additional color 1", "Additional color 2")
  # colorLevels <- c(factorLevels, paste("Additional color", 1:(6-length(factorLevels))))
  # colorLevels <- factorLevels
  # colorLevels <- c(factorLevels, rownames(dataset))
  # colorLevels <- rownames(dataset)
  
  # observeEvent( # Event colorPalette
  #   {
  #     input$colorPalette
  #   },
  #   ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
  #   ignoreNULL = T, # default = TRUE
  #   {
  #       lapply(seq_along(colorLevels), function(i) {
  #         colourpicker::updateColourInput(
  #           session = session,
  #           inputId = paste0("GroupColour", i),
  #           label = colorLevels[i],
  #           value =  brewer.pal(n_brewerPal, input$colorPalette)[i],
  #           palette = "limited",
  #           allowedCols = brewer.pal(n_brewerPal, input$colorPalette),
  #           closeOnClick = TRUE,
  #           returnName = TRUE
  #         )
  #       })
  #   }
  # )
  
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
