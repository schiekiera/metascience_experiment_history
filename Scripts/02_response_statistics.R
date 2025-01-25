#########################################################
#####                                               #####
##### Script 2: Descriptive Analyses (Responses)    #####
#####                                               #####
#####-----------------------------------------------#####
#####                                               #####
##### Content:                                      #####
#####  * 1: Setup and Library Import                #####
#####  * 2: Data Import and Initial Preparation     #####
#####  * 3: Study Duration Analysis                 #####
#####  * 4: Preprocessing Response Variables        #####
#####  * 5: Descriptive Statistics (Table 4)        #####
#####  * 6: Analysis for Table 5                    #####
#####  * 7: Data Visualization (Figure 2)           #####
#####                                               #####
#########################################################


####################
# IMPORTANT NOTE:  #
####################
# Since the analyses in this script do N O T rely on personal data,
# this script is F U L L Y executable.


#####################################
## 1. SETUP AND LIBRARY IMPORT     ##
#####################################

# remove everything
rm(list = ls())

# load required libraries
library(report)    # report results
library(tidyverse) # data manipulation
library(jtools)    # apa-style theme
library(viridis)   # plot color
library(gridExtra) # arrange plots
library(patchwork) # arrange plots 2
library(car)


#####################################
## 2. DATA IMPORT AND INITIAL      ##
##    PREPARATION                  ##
#####################################
url <- "https://raw.githubusercontent.com/schiekiera/metascience_experiment_history/main/Data/Anonymized_History_Data_Responses.csv"
df <- read.csv(url)
functions_url <- "https://raw.githubusercontent.com/schiekiera/metascience_experiment_history/main/Functions/functions.R"
source(functions_url)


#####################################
## 3. STUDY DURATION ANALYSIS      ##
#####################################
report(df$study_duration_minutes)




#####################################
## 4. PREPROCESSING RESPONSE       ##
##    VARIABLES                    ##
#####################################

# add 1 to for_resp to match it with the reported 1-7 scale
df$for_resp <- df$for_resp + 1

# define delta response: ΔLoS
df$delta_resp <- df$decision1_resp - df$decision2_resp



#####################################
## 5. DESCRIPTIVE STATISTICS       ##
##    (TABLE 4)                    ##
#####################################


# Table 4 depicts the differences in responses and reaction times between ILoS and CLoS.
# ILoS & CLoS #

# Response #
## ILoS: Response
report(df$decision1_resp)
## CLoS: Response
report(df$decision2_resp)
# t.test paired: ILoS and CLoS - Response
report(
  t.test(
    df$decision1_resp,
    df$decision2_resp,
    paired = TRUE,
    alternative = "two.sided"
  )
)

# Reaction Times #
## ILoS: RT
report(df$decision1_rt / 1000)
## CLoS: RT
report(df$decision2_rt / 1000)
# t.test paired: ILoS and CLoS - RT
report(
  t.test(
    df$decision1_rt / 1000,
    df$decision2_rt / 1000,
    paired = TRUE,
    alternative = "two.sided"
  )
)

#####################################
## 6. ANALYSIS FOR TABLE 5         ##
#####################################

# Table 5 contains the results of t-tests for differences between conservative and progressive abstracts for the various response variables.

## Decision 1
# t.test unpaired: decision1 with respect to the treatment variable
prog_resp <-
  df[df$treatment_string == "progressive",]$decision1_resp
cons_resp <-
  df[df$treatment_string == "conservative",]$decision1_resp
# leveneTest
leveneTest(group = factor(df$treatment_string), df$decision1_resp)
# equal variances therefore t.test
report(t.test(
  prog_resp,
  cons_resp,
  alternative = "two.sided",
  var.equal = TRUE
))


## Decision 2
prog_resp2 <-
  df[df$treatment_string == "progressive",]$decision2_resp
cons_resp2 <-
  df[df$treatment_string == "conservative",]$decision2_resp
# leveneTest
leveneTest(group = factor(df$treatment_string), df$decision2_resp)
# equal variances therefore t.test
report(t.test(
  prog_resp2,
  cons_resp2,
  alternative = "two.sided",
  var.equal = TRUE
))


# Delta
prog_delta <- df[df$treatment_string == "progressive",]$delta_resp
cons_delta <- df[df$treatment_string == "conservative",]$delta_resp
# LeveneTest
leveneTest(group = factor(df$treatment_string), df$delta_resp)
# equal variances therefore t.test
report(t.test(
  prog_delta,
  cons_delta,
  alternative = "two.sided",
  var.equal = TRUE
))


# FOR
prog_for <- df[df$treatment_string == "progressive",]$for_resp
cons_for <- df[df$treatment_string == "conservative",]$for_resp
# LeveneTest
leveneTest(group = factor(df$treatment_string), df$for_resp)
# equal variances therefore t.test
report(t.test(
  prog_for,
  cons_for,
  alternative = "two.sided",
  var.equal = TRUE
))



#####################################
##                                 ##
## 7. DATA VISUALIZATION (PLOTS)   ##
##                                 ##
##         F i g u r e   2         ##
##                                 ##
#####################################


# Figure 2 provides histograms and density plots for reading times and response variables,
# Consists of four subplots

########################
# Plot 1 Reading Times #
########################

# milli seconds to seconds
df$read1_rt_seconds <- df$read1_rt / 1000

# remove reading times < 300 seconds
reading_times <- df %>%
  filter(read1_rt_seconds < 300)

# Create a grouping variable for delta_resp
reading_times$Group2 <- "Reading Time per Abstract"

# Create the density plot with a legend
plot_reading_times <-
  ggplot(reading_times, aes(x = read1_rt_seconds, fill = Group2)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = "darkred", labels = "Reading Time per Abstract") +
  labs(x = "Abstract Reading Duration in Seconds",  # Greek Delta symbol
       y = "Density",
       fill = "Response Type") +
  theme_apa() +
  # add max value for y axis
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )
print(plot_reading_times)


############################################
# Plot 2 Intuitive and Considered Respones #
############################################

# Create the density plot with different shapes
df_long <- df %>%
  pivot_longer(
    cols = c(decision1_resp, decision2_resp),
    names_to = "Decision",
    values_to = "Response"
  )

density_plot <-
  ggplot(df_long, aes(x = Response, fill = Decision)) +
  geom_density(alpha = 0.5, adjust = 1.5) +
  scale_fill_manual(
    values = c("darkgreen", "darkorange"),
    labels = c("Intuitive Responses", "Considered Responses")
  ) +  # Different colors for each decision
  labs(x = "Likelihood of Submitting for Publication",
       y = "Density") +
  theme_apa() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "top",
    # Position the legend at the top
    legend.key = element_blank()  # Set the legend key to be blank
  )

# Plot it
print(density_plot)


#################
## Plot 3 Delta #
#################

# Create a grouping variable for delta_resp
df$Group <- "Delta (Intuitive - Considered)"

# Create the density plot with a legend
delta_plot <- ggplot(df, aes(x = delta_resp, fill = Group)) +
  geom_density(alpha = 0.5, adjust = 1.5) +
  scale_fill_manual(values = "darkblue", labels = "Delta (Intuitive - Considered)") +
  labs(
    x = expression(Delta * " Responses (Intuitive - Considered)"),
    # Greek Delta symbol
    y = "Density",
    fill = "Response Type"  # Title for the legend
  ) +
  theme_apa() +
  # add max value for y axis
  ylim(0, 0.35) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",
    # Position the legend at the top
    
  )
delta_plot

####################
## Plot 4 FOR Plot #
####################

# Step 1: Convert for_resp to a factor with Likert labels
df$for_resp_factor <- factor(
  df$for_resp,
  levels = 1:7,
  labels = c("VU", "U", "RU",
             "NC/UC", "RC",
             "C", "VC")
)
# Step 2: Create the APA-style bar plot
for_plot <- ggplot(df, aes(x = for_resp_factor)) +
  geom_bar(fill = "#66CCCC",
           color = "black",
           width = 0.7) +
  labs(x = "Feeling of Rightness",
       y = "Frequency", ) +
  theme_minimal() +
  theme_apa() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Rotate x-axis ticks
    panel.grid.major = element_blank(),
    # Optional: Remove major gridlines for a cleaner look
    panel.grid.minor = element_blank(),
    # Optional: Remove minor gridlines
    axis.line = element_line(color = "black")
  )

# Step 3: Display the plot
print(for_plot)


# Combine the two plots into a 2x2 grid
combined_plot <-
  plot_reading_times + density_plot + delta_plot + for_plot + plot_layout(ncol = 2)

# Display the combined plot
print(combined_plot)


# write to plots
#ggsave(
#  file.path(path_plots, "Figure_2_Descriptive_Responses_Combined.png"),
#  combined_plot,
#  width = 10,
#  height = 6,
#  dpi = 300
#)
#ggsave(
#  file.path(path_plots, "Figure_2_Descriptive_Responses_Combined.pdf"),
#  combined_plot,
#  width = 10,
#  height = 6
#)