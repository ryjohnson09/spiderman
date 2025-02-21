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

# Helper function to misspell a word (you can customize this)
misspell_word <- function(word) {
  # Simple misspelling (remove a random letter)
  if (nchar(word) > 1) {
    pos <- sample(1:nchar(word), 1)
    new_word <- paste0(substr(word, 1, pos - 1), substr(word, pos + 1, nchar(word)))
  } else {
    new_word <- word # Don't misspell short words
  }
  return(new_word)
}

introduce_character_errors <- function(df, columns, misspelling_prob = 0.1, missing_prob = 0.1) {
  
  if (!all(columns %in% names(df))) {
    stop("One or more specified columns not found in the dataframe.")
  }
  
  if (misspelling_prob < 0 || misspelling_prob > 1 || missing_prob < 0 || missing_prob > 1) {
    stop("Probabilities must be between 0 and 1.")
  }
  
  
  for (col in columns) {
    n_rows <- nrow(df)
    n_misspelled <- round(n_rows * misspelling_prob)
    n_missing <- round(n_rows * missing_prob)
    
    # Introduce misspellings
    if (n_misspelled > 0) {
      indices_to_misspell <- sample(1:n_rows, size = n_misspelled, replace = FALSE)
      for (i in indices_to_misspell){
        if (!is.na(df[[col]][i])){ #Don't mispell NAs
          df[[col]][i] <- misspell_word(df[[col]][i])
        }
      }
    }
    
    # Introduce missing values
    if (n_missing > 0) {
      indices_to_missing <- sample(1:n_rows, size = n_missing, replace = FALSE)
      df[[col]][indices_to_missing] <- NA
    }
  }
  
  return(df)
}



modify_numeric_column <- function(df, column_name, out_of_range_prob = 0.1, missing_prob = 0.1) {
  
  if (!(column_name %in% names(df))) {
    stop("Specified column not found in the dataframe.")
  }
  
  if (out_of_range_prob < 0 || out_of_range_prob > 1 || missing_prob < 0 || missing_prob > 1) {
    stop("Probabilities must be between 0 and 1.")
  }
  
  n_rows <- nrow(df)
  n_out_of_range <- round(n_rows * out_of_range_prob)
  n_missing <- round(n_rows * missing_prob)
  
  # Introduce out-of-range values
  if (n_out_of_range > 0) {
    indices_to_out_of_range <- sample(1:n_rows, size = n_out_of_range, replace = FALSE)
    # Replace with random integers between 24 and 47 (example range)
    df[[column_name]][indices_to_out_of_range] <- sample(24:47, size = n_out_of_range, replace = TRUE)
  }
  
  # Introduce missing values
  if (n_missing > 0) {
    indices_to_missing <- sample(1:n_rows, size = n_missing, replace = FALSE)
    df[[column_name]][indices_to_missing] <- NA
  }
  
  return(df)
}
