

#########################################################
#####                                               #####
##### Script 1: Sociodemographic Analyses           #####
#####                                               #####
#####-----------------------------------------------#####
#####                                               #####
##### Content:                                      #####
#####  * 1: Setup and Library Import                #####
#####  * 2: Data Import and Initial Preparation     #####
#####  * 3: Sociodemographics Data Analysis:        #####
#####       Extract and Analyze Demographic Data    #####
#####  * 4: Descriptive Statistics (Table 3):       #####
#####       Analysis of Gender, Age, Position,      #####
#####       and Continent                           #####
#####  * 5: Analysis for Figure 3:                  #####
#####       Political Orientation Analysis          #####
#####  * 6: Historiography and Ideology Analysis:   #####
#####       Ideological Changes and Correlations    #####
#####  * 7: Open Science Framework (OSF) Data:      #####
#####       Analysis of BFI-2-S and Publication     #####
#####       Activity (Tables 1, 2, 3)               #####
#####                                               #####
##### Note:                                         #####
#####   - Various custom functions are sourced      #####
#####     from external scripts.                    #####
#####                                               #####
#########################################################


####################
# IMPORTANT NOTE:  #
####################
# Since the analyses in this script rely on personal data,
# this script is N O T executable.


#####################################
## 1. SETUP AND LIBRARY IMPORT     ##
#####################################

# remove everything
rm(list = ls())

# load required libraries
library(report) # report results
library(jtools) # apa-style themes

#####################################
## 2. DATA IMPORT AND INITIAL      ##
##    PREPARATION                  ##
#####################################
url <- "https://raw.githubusercontent.com/schiekiera/metascience_experiment_history/main/Data/Anonymized_History_Data_Responses.csv"
df <- read.csv(url)
functions_url <- "https://raw.githubusercontent.com/schiekiera/metascience_experiment_history/main/Functions/functions.R"
source(functions_url)

#####################################
## 3. SOCIODEMOGRAPHICS DATA       ##
##    ANALYSIS                     ##
#####################################

# Create a new dataframe "socio_df", which contains only every 17th row
# The original "df" is in long format with each row containing one trial
# Here we need only sociodemographic data, which is repeated 17 times for each
# participant in the "df" dataframe.
socio_df <- df[seq(17, nrow(df), 17),]
colnames(socio_df)



#####################################
## 4. DESCRIPTIVE STATISTICS       ##
##    (TABLE 3)                    ##
#####################################

# Descriptive statistics of the sociodemographic data are shown in Table 3.

# gender
report_apa_combined(socio_df$gender)

# age
report_apa_combined(socio_df$age)

# position
summarize_by_professional_status(socio_df$position)
report_apa_combined(socio_df$position)

# continent
report_apa_combined(socio_df$country)
summarize_by_continent(socio_df$country)




#####################################
## 5. ANALYSIS FOR FIGURE 3        ##
#####################################

# Figure 3 shows the distribution of the political orientation variable.

# Step 1a: refactor and preprocess
socio_df$political_scale_cat <-
  cut(
    socio_df$political_scale,
    breaks = c(0, 3, 4, 8),
    labels = c("progressive", "moderate", "conservative")
  )
socio_df$political_scale_factor <-
  factor(
    socio_df$political_scale,
    levels = 1:7,
    labels = c("1: Left", "2", "3",
               "4: Center", "5",
               "6", "7: Right")
  )

# Step 1b: Calculating the mean and SD of the political_scale_factor variable
mean_value <- mean(socio_df$political_scale, na.rm = TRUE)
sd_value <- sd(socio_df$political_scale, na.rm = TRUE)
median_value <- median(socio_df$political_scale, na.rm = TRUE)

# Step 2: Creating the APA-style bar plot with mean and SD annotations
political_plot <-
  ggplot(socio_df, aes(x = political_scale_factor)) +
  geom_bar(fill = "steelblue",
           color = "black",
           width = 0.9) +
  #geom_text(stat='count', aes(label = paste0("n = ", ..count..)), vjust = -0.5, size = 3.5) +  # Add frequency labels
  labs(x = "Political Orientation: Left-Right Orientation Scale",
       y = "Frequency") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 27)) +
  theme_apa() +
  geom_vline(
    xintercept = mean_value,
    linetype = "solid",
    color = "black",
    size = 1
  ) +  # Mean line
  geom_vline(
    xintercept = median_value,
    linetype = "dashed",
    color = "red",
    size = 1
  ) +  # Median line
  geom_vline(
    xintercept = mean_value + sd_value,
    linetype = "dotted",
    color = "black",
    size = 1
  ) +  # +1 SD line
  geom_vline(
    xintercept = mean_value - sd_value,
    linetype = "dotted",
    color = "black",
    size = 1
  ) +  # -1 SD line
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Rotate x-axis ticks
    panel.grid.major = element_blank(),
    # Optional: Remove major gridlines for a cleaner look
    panel.grid.minor = element_blank(),
    # Optional: Remove minor gridlines
    axis.line = element_line(color = "black")
  )

# Step 3: Display the plot
print(political_plot)

# write to plots
ggsave(
  file.path(
    path_plots,
    "Figure_3_Descriptive_Responses_Political_Scale.png"
  ),
  political_plot,
  width = 8,
  height = 6,
  dpi = 300
)
ggsave(
  file.path(
    path_plots,
    "Figure_3_Descriptive_Responses_Political_Scale.pdf"
  ),
  political_plot,
  width = 8,
  height = 6
)



#####################################
## 6. HISTORIOGRAPHY AND IDEOLOGY  ##
##    ANALYSIS                     ##
#####################################

# ideology change
report_apa_combined(socio_df$ideology_change)

# ideologies correlation
report_apa_combined(socio_df$ideologies_correlation)

# historians ideologies
report_apa_combined(socio_df$historians_ideologies)



#####################################
## 7. OPEN SCIENCE FRAMEWORK       ##
##    (OSF) DATA                   ##
#####################################

##################
## OSF: TABLE 1 ##
##################
# Descriptive Statistics for BFI-2-S Conscientiousness Subscales

# Custom functions for reporting
mean_sd <- function(x, na.rm = TRUE) {
  mean_value <- round(mean(x, na.rm = na.rm), 2)
  sd_value <- round(sd(x, na.rm = na.rm), 2)
  return(cat(paste0(mean_value, " (", sd_value, ")")))
}
report_range <- function(x) {
  min_value <- min(x, na.rm = TRUE)
  max_value <- max(x, na.rm = TRUE)
  return(cat(paste0(min_value, "-", max_value)))
}

# For BFI we have one respondent with missing data, which we have to exclude here
bfi_df <- socio_df[!is.na(socio_df$bfi_2_S_score), ]

# overall conscientiousness
mean_sd(bfi_df$bfi_2_S_score, na.rm = TRUE)
report_range(bfi_df$bfi_2_S_score)
# subscales
## responsibility
mean_sd(bfi_df$bfi_2_S_responsibility, na.rm = TRUE)
report_range(bfi_df$bfi_2_S_responsibility)
## productiveness
mean_sd(bfi_df$bfi_2_S_productiveness, na.rm = TRUE)
report_range(bfi_df$bfi_2_S_productiveness)
## organization
mean_sd(bfi_df$bfi_2_S_organization, na.rm = TRUE)
report_range(bfi_df$bfi_2_S_organization)

##################
## OSF: TABLE 2 ##
##################
# Publication Activity and Attitudes

# reviewer
report_apa_combined(socio_df$reviewer)

# articles total
report_apa_combined(socio_df$articles)

# articles first
report_apa_combined(socio_df$articles_first)

# articles last
report_apa_combined(socio_df$articles_last)

# open science
report_apa_combined(socio_df$open_science)

# publication bias
report_apa_combined(socio_df$publication_bias)

# pressure to publish
report_apa_combined(socio_df$pressure_to_publish)




##################
## OSF: TABLE 3 ##
##################
# Historiographic Research Focus and Attitudes towards Ideologies in Historiography

# research area
report(socio_df$research_area)
socio_df$research_area

# Create a data frame with research_area data
research_area_data <- socio_df$research_area

# Split the string into individual research_areas, remove any leading/trailing whitespace
split_research_areas <- str_split(research_area_data, ",\\s*")

# Unlist to get a single vector of all research_areas
all_research_areas <- unlist(split_research_areas)

# Trim any leading or trailing whitespace from each research_area
all_research_areas <- str_trim(all_research_areas)

# Count the occurrences of each research_area
research_area_counts <- table(all_research_areas)

# Convert to data frame for better readability
research_area_counts_df <-
  as.data.frame(research_area_counts, stringsAsFactors = FALSE)

# Arrange the data frame in descending order of count
research_area_counts_df <- research_area_counts_df %>%
  arrange(desc(Freq))
# percent
research_area_counts_df$percent <-
  round(research_area_counts_df$Freq / 76 * 100)

# n_percent
research_area_counts_df$n_percent <- paste(research_area_counts_df$Freq,
                                           " (",
                                           research_area_counts_df$percent,
                                           "%)",
                                           sep = "")

# select only the columns we want
research_area_counts_df <-
  research_area_counts_df[, c("all_research_areas", "n_percent")]

# Print the result
for (i in 1:nrow(research_area_counts_df)) {
  cat(
    research_area_counts_df$all_research_areas[i],
    ": ",
    research_area_counts_df$n_percent[i],
    "\n"
  )
}


# historiographical_school
report(socio_df$historiographical_school)
socio_df$historiographical_school

# Create a data frame with historiographical_school data
historiographical_school_data <- socio_df$historiographical_school

# Split the string into individual historiographical_schools, remove any leading/trailing whitespace
split_historiographical_schools <-
  str_split(historiographical_school_data, ",\\s*")

# Unlist to get a single vector of all historiographical_schools
all_historiographical_schools <-
  unlist(split_historiographical_schools)

# Trim any leading or trailing whitespace from each historiographical_school
all_historiographical_schools <-
  str_trim(all_historiographical_schools)

# Count the occurrences of each historiographical_school
historiographical_school_counts <-
  table(all_historiographical_schools)

# Convert to data frame for better readability
historiographical_school_counts_df <-
  as.data.frame(historiographical_school_counts, stringsAsFactors = FALSE)

# Arrange the data frame in descending order of count
historiographical_school_counts_df <-
  historiographical_school_counts_df %>%
  arrange(desc(Freq))

# percent
historiographical_school_counts_df$percent <-
  round(historiographical_school_counts_df$Freq / 76 * 100)

# n_percent
historiographical_school_counts_df$n_percent <- paste(
  historiographical_school_counts_df$Freq,
  " (",
  historiographical_school_counts_df$percent,
  "%)",
  sep = ""
)

# select only the columns we want
historiographical_school_counts_df <-
  historiographical_school_counts_df[, c("all_historiographical_schools", "n_percent")]

# Print the result
for (i in 1:nrow(historiographical_school_counts_df)) {
  cat(
    historiographical_school_counts_df$all_historiographical_schools[i],
    ": ",
    historiographical_school_counts_df$n_percent[i],
    "\n"
  )
}
