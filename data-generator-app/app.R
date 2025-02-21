library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)

source("functions.R")

ui <- page_sidebar(
  title = div(
    h2("Spider-Man Crime Data Generator"),
    p(
      class = "mt-2",
      style = "font-size: 0.9rem; opacity: 0.7;",
      "Simulate and analyze Spider-Man's crime-fighting success rates across New York City. Note that there are some underlying 
      assumptions with data generation: Green Goblin and Doc Ock are harder to defeat, and Queens provides 'home field advantage'
      to Spiderman."
    )
  ),
  sidebar = sidebar(
    title = "Data Generation Controls",
    numericInput("n_rows", "Number of Events:", 5000, min = 1000, max = 100000),
    tooltip(
      sliderInput("weather_effect", 
                  "Weather Impact on Success Rate:", 
                  min = 0, max = 5, value = 0, step = 0.5),
      "How much should bad weather (rain, storm, snow) negatively impact the success rate?",
      htmltools::tags$br(),
      htmltools::tags$br(),
      "0 = No Effect",
      htmltools::tags$br(),
      "5 = High Effect",
      placement = "right"
    ),
    tooltip(
      sliderInput("crew_size_effect", 
                  "Crew Size Impact on Success Rate:", 
                  min = 0, max = 5, value = 0, step = 0.5),
      "How much should a villain's crew size negatively impact the success rate? The larger the crew, the larger the impact.",
      htmltools::tags$br(),
      htmltools::tags$br(),
      "0 = No Effect",
      htmltools::tags$br(),
      "5 = High Effect",
      placement = "right"
    ),
    tooltip(
      sliderInput("hour_effect", 
                  "Night Time Impact on Success Rate:", 
                  min = 0, max = 5, value = 0, step = 0.5),
      "How much should the night hours (22:00 - 4:00) negatively impact the success rate?",
      htmltools::tags$br(),
      htmltools::tags$br(),
      "0 = No Effect",
      htmltools::tags$br(),
      "5 = High Effect",
      placement = "right"
    ),
    actionButton("generate", "Generate Data", class = "btn-primary"),
    br(),
    tooltip(
      checkboxInput("dirty_data", "Make data dirty?", FALSE),
      "Erros include:",
      htmltools::tags$br(),
      "Misspelling of villains, boroughs, weather",
      htmltools::tags$br(),
      "Time values outside of 0-23",
      htmltools::tags$br(),
      "NA values introduced",
      placement = "right"
    ),
    uiOutput("error_freq_ui"),
    downloadButton("download_data", "Download Data (CSV)", class = "btn-secondary")
  ),
  
  card(
    full_screen = TRUE,
    card_header("Data Preview"),
    DTOutput("data_table")
  ),
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Success Rate by Weather"),
      plotOutput("weather_plot")
    ),
    card(
      full_screen = TRUE,
      card_header("Success Rate by Hour"),
      plotOutput("time_plot")
    ),
    card(
      full_screen = TRUE,
      card_header("Success Rate vs Crew Size"),
      plotOutput("crew_plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data generation
  spidey_data <- eventReactive(input$generate, {
    generate_spidey_data(
      input$n_rows,
      input$weather_effect,
      input$crew_size_effect,
      input$hour_effect
    )
  })
  
  # Data table output
  output$data_table <- DT::renderDT({
    spidey_data()
  })
  
  # Modified data based on dirty checkbox
  modified_data <- reactive({
      if (input$dirty_data) {
        d_data <- introduce_character_errors(spidey_data(), 
                                             c("villain", "borough", "weather"), 
                                             misspelling_prob = input$error_freq, missing_prob = input$error_freq)
        d_data <- d_data |> 
          modify_numeric_column("hour", out_of_range_prob = input$error_freq, missing_prob = input$error_freq)
        d_data
      } else {
    spidey_data()
      }
  })
  
  # Reactive UI for error/NA frequency
  output$error_freq_ui <- renderUI({
    if (input$dirty_data) {
      numericInput("error_freq", "Frequency to add dirty values", 
                   value = 0.001, min = 0, max = 1)
    }
  })
  
  # Download Handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("spidey-data-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      data <- modified_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Weather effect plot
  output$weather_plot <- renderPlot({
    data <- spidey_data() %>%
      group_by(weather) %>%
      summarize(success_rate = mean(thwarted))
    
    ggplot(data, aes(x = weather, y = success_rate)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      theme_minimal() +
      labs(y = "Success Rate", x = "Weather Condition") +
      scale_y_continuous(labels = scales::percent)
  })
  
  # Time of day effect plot
  output$time_plot <- renderPlot({
    data <- spidey_data() %>%
      group_by(hour) %>%
      summarize(success_rate = mean(thwarted))
    
    ggplot(data, aes(x = hour, y = success_rate)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(color = "red", size = 3) +
      theme_minimal() +
      labs(x = "Hour of Day", y = "Success Rate") +
      scale_x_continuous(breaks = 0:23, labels = sprintf("%02d:00", 0:23)) +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Crew size effect plot
  output$crew_plot <- renderPlot({
    ggplot(spidey_data(), aes(x = crew_size, y = as.numeric(thwarted))) +
      geom_jitter(alpha = 0.2, height = 0.05, color = "red") +
      geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
      theme_minimal() +
      labs(x = "Villain Crew Size", y = "Probability of Success") +
      scale_y_continuous(labels = scales::percent)
  })
}

shinyApp(ui, server)
