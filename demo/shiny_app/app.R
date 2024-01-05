library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)
library(shinythemes)

# setwd("~/Documents/GitHub/seroshiny/demo/shiny_app/"); runApp()
# deployApp(account = "kucharski", appName = "www_explore",lint=F) # Deploy

# Load data
test_values <- read.csv("data/data_raw.csv")
model_fits <- read.csv("data/data_fitted.csv")
#model_fits <- model_fits |> rename(titre_obs,me_trans)

# UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # Header
  #headerPanel(title = "Variant titres"),
  
  # Sidebar
  sidebarPanel(
    selectInput("history", "Prior infection:", unique(test_values$inf_history)),
    selectInput("vaccine", "Vaccine status", unique(test_values$vaccinate)),
    selectInput("exposure", "Last exposure:", unique(test_values$last_exposure)),
    checkboxGroupInput("test_type", "Variant tested:", unique(test_values$titre_type),selected=c("Alpha","Delta","BA.1")),
    hr(),
    checkboxInput("show_data", "Show raw data", TRUE)
  ),
  mainPanel(
    h4("Model fits to cohort data"),
    plotOutput("titre_plot")
  )
)

# Server
server <- function(input, output) {
  
  # point_col <- "#3195b7"

  # Plot temporal dynamics
  output$titre_plot <- renderPlot({
    
    reactive_test_data <- subset(test_values,inf_history == input$history & last_exposure == input$exposure & vaccinate == input$vaccine & titre_type %in% input$test_type)
    reactive_model_data <- subset(model_fits,inf_history == input$history & last_exposure == input$exposure & vaccinate == input$vaccine & titre_type %in% input$test_type)
    
    p <- ggplot() +
      geom_line(data = reactive_model_data, aes(x = time, y = me_trans, color = titre_type), linewidth = 1) +
      labs(y= "log2 titre", x = "time since vaccine") +
      geom_ribbon(data = reactive_model_data, 
                  aes(x = time, ymin = lo_95_trans, ymax = hi_95_trans, fill = titre_type),
                  alpha = 0.2, show.legend = FALSE) +
      xlim(0, 200) +
      ylim(0, 8) +
      scale_color_brewer(palette = "Set1",name = "Variant tested") +
      scale_fill_brewer(palette = "Set1") +
      theme_minimal()
    
    # Add data if checkbox is selected
    if(input$show_data){
      p <- p + geom_point(data = reactive_test_data, aes(x = time, y = titre_obs, color = titre_type), alpha = 0.5) 
    }
    
    # Add credible interval if checkbox is selected
    # if(input$show_region){
    #   p <- p + geom_ribbon(data = reactive_model_data, 
    #                        aes(x = time, ymin = lo_95_trans, ymax = hi_95_trans, fill = titre_type),
    #                        alpha = 0.2, show.legend = FALSE)
    # }
    
    return(p)
    
  })
  
  
  
  
}

# Run Shiny App
shinyApp(ui, server)
