library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(shinyalert)
library(DT)
# install.packages("DT")

# Frontend
ui <- fluidPage(

sidebarLayout(
  sidebarPanel(
    # Compound interest
    sliderInput("range", "Compound interest:",min = 0, max = 10, value = c(4,8)),textOutput("Compound interest slider"),
    
    
    # Checkboxes
    #tags$head(tags$style(HTML(".checkbox {margin-left:15px}"))),
    checkboxGroupInput("countries", "Chosen countries:",
                       choiceNames = c(map_df$region),
                       choiceValues = c(map_df$geo),
                       selected = c("PL", "CZ", "DE"),
                       inline = TRUE,
                       width = "75%"
    ),
    
    # About
    useShinyalert(),
    actionButton("about", "?"),
    
    
    width=3
  ),
  
  mainPanel(
    h1("Real house prices index"),
    
    tabsetPanel(
      tabPanel("Plot", plotlyOutput("final_plot")),
      tabPanel("Map", leafletOutput("mymap")),
      tabPanel("Table", dataTableOutput('table'))
    ),
   
    width = 9
  ),
  
  fluid = TRUE
)



)

# Backend
server <- function(input, output, session) {

observeEvent(input$about, {
  # Show a modal when the button is pressed
  shinyalert("About project:", 
  "Authors: Paweł Lewicki, Patryk Kaszuba\n
  
  The aim of the project is to present growth of house prices as index of Purchasing Power Parity (PPP) in countries of European Union. 2015 is a benchmark value (100 for each country).
  
  Datasets used:
  - Eurostat Inflation 2022 - prc_hicp_aind_page_linear
  - Eurostat House Price Index - prc_hpi_a__custom_3617733_page_linear
  
  Libraries used:
  shiny, leaflet, ggplot2, dplyr, shinyalert, plotly, dplyr, 
  tidyverse, eurostat, sf, scales, cowplot, ggthemes, RColorBrewer
  
  Subject: Data Visualisation
  Adam Mickiewicz University, 
  Poznan, Poland, June 2023"
  , type = "info")
})

output$table <- DT::renderDataTable({
  datatable(merged_df[, !names(merged_df) %in% c("OBS_VALUE.x", "OBS_VALUE.y", "geometry")], options = list(pageLength = 10))
})


# Plot module
output$final_plot <- renderPlotly({
  final_plot <- ggplot(filter(merged_df, geo %in% input$countries),
                       aes(x = TIME_PERIOD, y = house_prices_wo_hicp, color = geo, 
                           text = paste("Kraj: ", geo, "<br>", "Rok: ", TIME_PERIOD, "<br>", 
                                        "Cena nieruchomości: ", house_prices_wo_hicp))) +
    geom_line(aes(group = geo), linetype = "dotted", size = 1) +
    geom_point() +
    geom_text(data = filter(merged_df, geo %in% input$countries) %>% group_by(geo) %>% slice(n()),
              aes(label = "", hjust = -0.2, size = 4)) +
    stat_function(fun = function(x) 100*(1+input$range[1]/100)^(x-2015), aes(colour = paste0(as.character(input$range[1]), "% Compounding")), inherit.aes = FALSE) +
    stat_function(fun = function(x) 100*(1+input$range[2]/100)^(x-2015), aes(colour = paste0(as.character(input$range[2]), "% Compounding")), inherit.aes = FALSE) +
    scale_x_continuous(breaks = seq(2010, 2024, 2)) +
    labs(x = "Rok", y = "Indeks cen nieruchomości zdyskontowany o wartość inflacji [2015 = 100]",
         color = "Countries") +
    theme(axis.title = element_blank())
  
  plotly_plot <- ggplotly(final_plot, tooltip = "text")
  
  for (i in 1:length(plotly_plot$x$data)) {
    if (plotly_plot$x$data[[i]]$name == paste0(as.character(input$range[1]), "% Compounding") || plotly_plot$x$data[[i]]$name == paste0(as.character(input$range[1]), "% Compounding")) {
      plotly_plot$x$data[[i]]$hoverinfo <- "name+y"
    }
  }
  
  plotly_plot %>% layout(showlegend = TRUE, legend = list(title = list(text = "Countries")))
})





# Map module
output$mymap <- renderLeaflet({
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data=mapdata_new,
                fillOpacity = 0.6, # Przezroczystość
                stroke = TRUE, # Borders visible
                color = "grey", # Border color
                weight = 1,
                
                fillColor = ~qpal(mapdata_new$substr_house_prices_wo_hicp),
                popup = popup_content,
                popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE)
    ) %>% 
    setView( lat = 49, lng = 14, zoom = 4) %>%
    addLegend("bottomright", colors = qpal_colors,
              title = "<span style='white-space: pre-line;'> Real house prices \n index (2022) </span>",
              labels = qpal_labs,
              opacity = 1)
}) # End of map module

}# End of server

# Run
shinyApp(ui, server)
