overviewServer <- function(id, dataValues, sharedValues) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns  # Namespace function for IDs
      
      # Access reactive values from dataValues and sharedValues
      dataset <- dataValues$dataset
      factorNames <- dataValues$factorNames
      factorLevels <- dataValues$factorLevels
      factors <- dataValues$factors
      sampleNames <- dataValues$sampleNames
      myPalette <- dataValues$myPalette
      testCovariate <- dataValues$testCovariate
      # se <- dataValues$se
      
      # colourList <- reactive({ sharedValues$colourList })
      myPalette <- sharedValues$myPalette
      
      ### Update Radio Buttons for Cytosine Context ###
      # observeEvent(contexts(), {
      #   if (length(contexts()) == 3) {
      #     updateRadioGroupButtons(
      #       session = session,
      #       inputId = "selectContext",
      #       label = "Cytosine context",
      #       choices = c("CpG", "CHG", "CHH", "All"),
      #       status = "primary"
      #     )
      #   }
      # }, ignoreInit = TRUE)
      
      ### Update Settings Table ###
      # observeEvent({
      #   input$selectContext
      #   input$selectMethylation
      # }, {
      #   settingsDF <- data.frame(
      #     "Cytosine context" = input$selectContext,
      #     "Methylation" = input$selectMethylation,
      #     "Grouping variable" = testCovariate,
      #     check.names = FALSE
      #   )
      #   
      #   output$settingsTable <- renderTable({
      #     t(settingsDF)
      #   }, rownames = TRUE)
      # }, ignoreInit = FALSE)
      

      settingsDF <- data.frame(
        "Cytosine context" = "CpG",
        "Methylation" = "Hyper",
        "Grouping variable" = dataValues$testCovariate,
        check.names = FALSE
      )
      
      output$settingsTable <- renderTable({
        t(settingsDF)
      }, rownames = TRUE)
      
      ### Prepare Color Palettes ###
      n_brewerPal <- length(dataValues$sampleNames)
      palettesCategorical <- RColorBrewer::brewer.pal.info[
        RColorBrewer::brewer.pal.info$category == "qual" &
          RColorBrewer::brewer.pal.info$maxcolors >= n_brewerPal, ]
      palettesGroup <- palettesCategorical
      palettesDiverging <- RColorBrewer::brewer.pal.info[
        RColorBrewer::brewer.pal.info$category == "div" &
          RColorBrewer::brewer.pal.info$maxcolors >= n_brewerPal, ]
      

      # colorLevels <- c(factorLevels, rownames(dataset))
      # print("####### color Levels #######")
      # print(colorLevels[1])
      # print(length(colorLevels))
      selected_colors <- brewer.pal(length(dataValues$colorLevels), "Dark2")
      lapply(seq_along(dataValues$colorLevels), function(i) {
        colourpicker::updateColourInput(
          session = session,
          inputId = paste0("GroupColour", i),
          label = dataValues$colorLevels[i],
          value = selected_colors[i],
          palette = "limited",
          allowedCols = selected_colors,
          closeOnClick = TRUE,
          returnName = TRUE
        )
        sharedValues$colourList[i] <- selected_colors[i]
      })

      ### Update Color Buttons Based on Colorblind-Friendly Switch ###
      observeEvent( # Event EmpiricalDistribution
        {
          input$colorblind_only
        },
        ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
        ignoreNULL = T, # default = TRUE
        {
          available_palettes_groups <- rownames(palettesGroup)
          if (input$colorblind_only) {
            available_palettes_groups <- rownames(palettesGroup[palettesGroup$colorblind == TRUE, ])
          }
          
          output$color_buttons_groups <- renderUI({
            fluidRow(
              column(
                width = 12,
                do.call(tagList, lapply(available_palettes_groups, function(palette_name) {
                  colors <- RColorBrewer::brewer.pal(
                    n = RColorBrewer::brewer.pal.info[palette_name, "maxcolors"],
                    name = palette_name
                  )
                  
                  fluidRow(
                    column(
                      width = 2,
                      if (input$colorblind_only && !RColorBrewer::brewer.pal.info[palette_name, "colorblind"]) {
                        h5(palette_name, class = "disabled")
                      } else {
                        h5(palette_name)
                      }
                    ),
                    column(
                      width = 10,
                      checkboxGroupButtons(
                        inputId = ns(paste0("color_select_", palette_name)),
                        label = NULL,
                        # selected = if (palette_name == "Dark2") colors[1:length(colourList())] else NULL,
                        selected = if (palette_name == "Dark2") colors[1:length(sharedValues$colourList)] else NULL,
                        direction = "horizontal",
                        status = "custom-class",
                        size = "xs",
                        individual = TRUE,
                        justified = FALSE,
                        choiceNames = lapply(colors, function(color) {
                          div(
                            style = sprintf(
                              "background-color: %s; width: 40px; height: 40px; display: inline-block; position: relative;",
                              color
                            ),
                            tags$i(class = "checkmark fa fa-check-circle")
                          )
                        }),
                        choiceValues = colors,
                        disabled = if (input$colorblind_only && !RColorBrewer::brewer.pal.info[palette_name, "colorblind"]) TRUE else FALSE
                      )
                    )
                  )
                }))
              )
            )
          })
      })
      
      ### Assign Colors to Groups ###
      observeEvent( # Event EmpiricalDistribution
        {
          input$confirm_color_selection
        },
        ignoreInit = T, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
        ignoreNULL = F, # default = TRUE
        {
          # Collect selected colors from all palettes
          color_inputs <- grep("^color_select_", names(input), value = TRUE)
          selected_colors <- unlist(lapply(color_inputs, function(inputId) {
            input[[inputId]]
          }))
          req(selected_colors)
          
          # print("####### color Levels #######")
          # print(colorLevels[1])
          lapply(seq_along(dataValues$colorLevels), function(i) {
            colourpicker::updateColourInput(
              session = session,
              inputId = paste0("GroupColour", i),
              label = dataValues$colorLevels[i],
              value = selected_colors[i],
              palette = "limited",
              allowedCols = selected_colors,
              closeOnClick = TRUE,
              returnName = TRUE
            )
            # sharedValues$colourList[i] <- selected_colors[i]
          })
      })
      
      ### Update Shared Color List ###
      observeEvent(input$confirm_color_assignment, {
        # newColourList <- sharedValues$colourList  # Get existing colourList

        lapply(seq_along(dataValues$colorLevels), function(i) {
          color_input_id <- paste0("GroupColour", i)
          color_value <- input[[color_input_id]]
          sharedValues$colourList[[i]] <- paste0(col2hex(color_value), "FF")
          # newColourList[[colorLevels[i]]] <- paste0(col2hex(color_value), "FF")
        })

        # Update the sharedValues$colourList
        # sharedValues$colourList <- newColourList
        print("sharedValues$colourList updated !!!!!!!!!!!")
        print(sharedValues$colourList)
      }, ignoreInit = FALSE)
      
      # print("reached end of overview server")
    }
  )
}
