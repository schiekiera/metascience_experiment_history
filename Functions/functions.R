#libraries
library(report)
library(tidyverse)

# functions
sort_table <- function(x) {
  x <- sort(table(x), decreasing = TRUE)
  return(x)
}


report_apa <- function(x) {
  # Ensure x is a factor
  x <- as.factor(x)
  
  # Get the frequency table and the number of missing values
  freq_table <- sort(table(x), decreasing = TRUE)
  total_entries <- length(x)
  missing_entries <- sum(is.na(x))
  
  # Create the detailed list of all values
  values_summary <- paste(
    paste(names(freq_table), "(n =", freq_table, ")", collapse = "; ")
  )
  
  # Generate the summary sentence
  summary_sentence <- paste(
    "x:", total_entries, "entries,", 
    values_summary, 
    paste0("(", missing_entries, " missing)")
  )
  
  return(summary_sentence)
}

report_apa_percent <- function(x, round_digits = 0) {
  # Ensure x is a factor
  x <- as.factor(x)
  
  # Get the frequency table and convert to percentages
  freq_table <- sort(prop.table(table(x)) * 100, decreasing = TRUE)
  freq_table <- round(freq_table, round_digits)
  
  # Create the detailed list of all values with their percentages
  values_summary <- paste(
    paste(names(freq_table), "(", freq_table, "%)", collapse = "; ")
  )
  
  # Generate the summary sentence
  summary_sentence <- paste(
    "x: Summary by percentage,", 
    values_summary
  )
  
  return(summary_sentence)
}


report_apa_combined <- function(x, round_digits = 0) {
  # Ensure x is a factor
  x <- as.factor(x)
  
  # Get the frequency table
  freq_table <- sort(table(x), decreasing = TRUE)
  
  # Calculate total entries and missing values
  total_entries <- length(x)
  missing_entries <- sum(is.na(x))
  
  # Convert frequencies to percentages
  percent_table <- round(prop.table(freq_table) * 100, round_digits)
  
  # Create the combined summary of all values with their counts and percentages
  values_summary <- paste(
    paste(names(freq_table), ", n =", freq_table, "(", percent_table, "%)", collapse = ";\n\n")
  )
  
  # Generate the final summary sentence
  summary_sentence <- paste(
    values_summary
  )
  
  return(cat(summary_sentence))
}


summarize_by_continent <- function(country_data) {
  # Define a list of continents and their respective countries
  continents <- list(
    "Europe" = c("Austria", "France", "Germany", "Italy", "Netherlands", "Norway", "Poland", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "Other (Europe)"),
    "Northern America" = c("Canada", "United States"),
    "South America/Central America" = c("Brazil", "Other (Americas)"),
    "Asia" = c("India", "Israel", "Other (Asia)"),
    "Oceania" = c("Australia", "Other (Oceania)")
  )
  
  # Initialize a result list to store the summary
  length<-length(country_data)
  country_data<-sort_table(country_data)
  result <- list()
  total_sum <- 0

  for (continent in names(continents)) {
    continent_countries <- continents[[continent]]
    continent_summary <- country_data[names(country_data) %in% continent_countries]
    continent_total <- sum(continent_summary)
    continent_percent <- round(continent_total / length * 100)
    
    result[[continent]] <- paste(
      paste0(continent, ": n = ", continent_total, sep = ", (", continent_percent, "%)")
    )
    
    total_sum <- total_sum + continent_total
  }
  
  # Combine all continents' summaries and add the total sum
  final_summary <- paste(
    unlist(result))
  
  return(cat(paste(final_summary, collapse = "\n\n")))
}

summarize_by_professional_status<- function(professional_status_data) {
  # Define a list of continents and their respective countries
  professional_status <- list(
    "professoral" = c("Professor","Junior Professor"),
    "postdoctoral" = c("Post-Doctoral Researcher", "Other: PhD/Doctorate Degree"),
    "predoctoral" = c("PhD Candidate","Other: No PhD/Doctorate Degree")
  )
  
  # Initialize a result list to store the summary
  length<-length(professional_status_data)
  professional_status_data<-sort_table(professional_status_data)
  result <- list()
  total_sum <- 0
  
  for (status in names(professional_status)) {
    status_professional <- professional_status[[status]]
    status_summary <- professional_status_data[names(professional_status_data) %in% status_professional]
    status_total <- sum(status_summary)
    status_percent <- round(status_total / length * 100)
    
    result[[status]] <- paste(
      paste0(status, ": n = ", status_total, sep = ", (", status_percent, "%)")
    )
    
    total_sum <- total_sum + status_total
  }
  
  # Combine all continents' summaries and add the total sum
  final_summary <- paste(
    unlist(result))
  
  return(cat(paste(final_summary, collapse = "\n\n")))
}

