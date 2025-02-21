library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)

# Helper function to generate synthetic data
generate_spidey_data <- function(n_rows,
                                 weather_effect,
                                 crew_size_effect,
                                 hour_effect) {
  
  # Convert 0-5 scale to 0-0.5 effect size
  weather_effect <- weather_effect * 0.1
  crew_size_effect <- crew_size_effect * 0.1
  hour_effect <- hour_effect * 0.1
  
  # Base data generation
  villains <- c("Green Goblin", "Doc Ock", "Vulture", "Mysterio", "Electro", "Sandman")
  boroughs <- c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")
  weather <- c("Clear", "Cloudy", "Rainy", "Stormy", "Snowy")
  
  # Skew Weather to mostly clear
  weather_weights <- c(4, 2, 1.5, 1, 1)
  
  # Skew Crew size to smaller crews
  crew_weights <- c(5, 4, 3, 2, 1, rep(0.5, 15))
  
  data <- data.frame(
    villain = sample(villains, n_rows, replace = TRUE),
    borough = sample(boroughs, n_rows, replace = TRUE),
    weather = sample(weather, n_rows, replace = TRUE, prob = weather_weights),
    hour = sample(0:23, n_rows, replace = TRUE),  # 24-hour format
    crew_size = sample(1:20, n_rows, replace = TRUE, prob = crew_weights)
  )
  
  # Calculate base probability of thwarting crime
  thwart_prob <- 0.75
  
  # Weather effect (harder in bad weather)
  weather_modifier <- ifelse(
    data$weather %in% c("Rainy", "Stormy", "Snowy"),
    -weather_effect,
    0
  )
  
  # Crew size effect (harder with larger crews)
  crew_modifier <- -crew_size_effect * (data$crew_size / 20)
  
  # Time of day effect (harder at night)
  # Define night hours (20:00 - 04:00)
  time_modifier <- ifelse(
    data$hour >= 20 | data$hour <= 4,
    -hour_effect,
    0
  )
  
  # Borough effect (better in Queens - home turf advantage)
  borough_modifier <- ifelse(data$borough == "Queens", 0.1, 0)
  
  # Some villains are harder than others!
  villain_modifier <- ifelse(
    data$villain %in% c("Green Goblin", "Doc Ock"), -0.2, 0
  )
  
  # Calculate final probability
  final_prob <- pmax(0.1, pmin(0.9,  # Keep between 10% and 90%
                               thwart_prob + weather_modifier + crew_modifier + time_modifier + borough_modifier + villain_modifier
  ))
  
  # Generate thwarted outcome
  data$thwarted <- rbinom(n_rows, 1, final_prob) == 1
  
  return(data)
}

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
  
  # Download Handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("spidey-data-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      data <- spidey_data()
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
      geom_line(color = "blue", size = 1) +
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
