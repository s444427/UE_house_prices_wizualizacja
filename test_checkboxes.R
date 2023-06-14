  ## Only run examples in interactive R sessions
  if (interactive()) {
    
    # Frontend
    ui <- fluidPage(
      # Checkboxes
      checkboxGroupInput("countries", "Choose countries:",
                         choiceNames = unique(merged_df$geo),
                         choiceValues = unique(merged_df$geo)
      ),
      
      # list of chosen
      textOutput("txt")
    )
    
    # Backend
    server <- function(input, output, session) {
      
      # Printer
      output$txt <- renderText({
        chosen_countries <- paste(input$countries, collapse = ", ")
        paste("You chose", chosen_countries)
      })
    
      }
    
    shinyApp(ui, server)
  }