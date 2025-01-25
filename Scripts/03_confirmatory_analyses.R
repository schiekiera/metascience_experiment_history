#########################################################
#####                                               #####
##### Script 3: Confirmatory Analyses               #####
#####           Using Multilevel Models (MLM)       #####
#####                                               #####
#####-----------------------------------------------#####
#####                                               #####
##### Content:                                      #####
#####  * 1: Setup and Library Import                #####
#####  * 2: Data Import and Preparation             #####
#####  * 3: Confirmatory Analyses (MLM)             #####
#####  * 4: Hypothesis 1 (H1) Analysis              #####
#####  * 5: Hypothesis 2 (H2) Analysis              #####
#####  * 6: Hypothesis 3 (H3) Analysis              #####
#####  * 7: Data Visualization (Figure 4)           #####
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
library(report)    # report results
library(tidyverse) # data manipulation
library(jtools)    # apa-style theme
library(lme4)      # linear mixed effects models
library(virdis)    # plot color



#####################################
## 2. DATA IMPORT AND INITIAL      ##
##    PREPARATION                  ##
#####################################
url <- "https://raw.githubusercontent.com/schiekiera/metascience_experiment_history/main/Data/Anonymized_History_Data_Responses.csv"
df <- read.csv(url)
functions_url <- "https://raw.githubusercontent.com/schiekiera/metascience_experiment_history/main/Functions/functions.R"
source(functions_url)


#####################################
## 3. CONFIRMATORY ANALYSES (MLM)  ##
#####################################

# All hypotheses were analyzed using multilevel models (MLM) to account for the nested structure
# of the data within both participants and abstracts. The likelihood of submitting an abstract
# for publication (LoS) is hypothesized to be influenced by characteristics of both participants
# and abstracts.

# Note: The structure of our experimental data is cross-classified, not strictly hierarchical,
# because the same 17 abstracts are presented to all participants, and the abstracts differ
# only according to the political stance manipulation. This results in a counterbalanced design
# with a cross-classified data structure.

# We use cross-classified multilevel modeling to address this (Hox, 2002).


# Model Formula:
# response ~ political stance (abstract) + political orientation (individual) +
#            political stance (abstract) * political orientation (individual) +
#            (political stance (abstract) | participant) + (1 | abstract)

# Explanation of the model:
# The model decomposes the variation in of the response (LoS) for participant j rating abstract i into random and fixed effects.

# Fixed Effects:
# - political stance (abstract): Effect of the abstract's political stance on LoS
# - political orientation (individual): Effect of the participant's political orientation on LoS
# - political stance (abstract) * political orientation (individual): Interaction effect between the abstract's political stance and the participant's political orientation

# Random Effects:
# - (political stance (abstract) | participant):
#   (a) Random slope for participants based on the political stance of an abstract
#   (b) Random intercept at the participant level
# - (1 | abstract): Random intercept at the abstract level

# Coding for political stance:
# - Progressive abstracts are coded as political stance (abstract) = -1
# - Conservative abstracts are coded as political stance (abstract) = 1




#############################
## 4. HYPOTHESIS 1 (H1)     ##
##    ANALYSIS              ##
#############################

# Hypothesis 1 (H1): Intuitive Responses

# The hypothesis (H1) posits that historians are more likely to report a higher intuitive likelihood
# of submitting an abstract for publication (ILoS) when the abstract's political stance aligns with
# their own political orientation.

##Model H1
# Fit a linear mixed-effects model (LMM) using lmer() from the lme4 package
# - decision1_resp: response variable representing the intuitive likelihood of submitting an abstract (Type 1 processing)
# - treatment: fixed effect representing the experimental treatment condition (abstract's political stance)
# - political_scale: fixed effect representing participants' political orientation (1: very progressive to 7: very conservative)
# - treatment * political_scale: interaction effect between treatment and political orientation to examine if the effect of treatment
#   on decision1_resp changes with different political orientations
# - (treatment | id): random slope for treatment within participants (id) to account for individual variability in responses to treatment
# - (1 | abstract): random intercept for abstracts to account for variability between different abstracts
model_H1 <-
  lmer(
    decision1_resp ~ treatment + political_scale + treatment * political_scale +
      (treatment | id) + (1 | abstract),
    data = df
  )
report(model_H1)

# Save the qqplots
## H1
png(
  file.path(path_plots, "qq_plots/qqplot_H1.png"),
  width = 10,
  height = 6,
  units = "in",
  res = 300
)
qqnorm(resid(model_H1), main = "Normal Q-Q Plot")
qqline(resid(model_H1), col = "steelblue", lwd = 2)
dev.off()



#############################
## 5. HYPOTHESIS 2 (H2)     ##
##    ANALYSIS              ##
#############################

# Hypothesis 2 (H2): Response Change from Type 1 to Type 2 Processing

# Hypothesis (H2) posits that abstracts not aligned with researchers' political orientation
# are evaluated less negatively during Type 2 processing compared to Type 1 processing.

# create delta_resp variable
df$delta_resp <- df$decision1_resp - df$decision2_resp

##Model H2
# Fit a linear mixed-effects model (LMM) using lmer() from the lme4 package
# - delta_resp: response variable representing change in decision from Type 1 to Type 2
# - treatment: fixed effect representing the experimental treatment condition (abstract's political stance)
# - political_scale: fixed effect representing participants' political orientation (1: very progressive to 7: very conservative)
# - treatment * political_scale: interaction effect between treatment (abstract's political stance) and political orientation
# - (treatment | id): random slope for treatment within participants (id) to account for individual differences
# - (1 | abstract): random intercept for abstracts to account for differences between abstracts
model_H2 <-
  lmer(
    delta_resp ~ treatment + political_scale + treatment * political_scale +
      (treatment | id) + (1 | abstract),
    data = df
  )
report(model_H2)
# singular fit --> reduce model complexity and withdraw random slope

##Model_H2_random_intercept_only
# Withdraw random slope for treatment
model_H2_random_intercept_only <-
  lmer(
    delta_resp ~ treatment + political_scale + treatment * political_scale +
      (1 | id) + (1 | abstract),
    data = df
  )
report(model_H2_random_intercept_only)

# normality of residuals plot
qqnorm(resid(model_H2_random_intercept_only))
qqline(resid(model_H2_random_intercept_only),
       col = "steelblue",
       lwd = 2)

## H2
png(
  file.path(path_plots, "qq_plots/qqplot_H2.png"),
  width = 10,
  height = 6,
  units = "in",
  res = 300
)
qqnorm(resid(model_H2_random_intercept_only), main = "Normal Q-Q Plot")
qqline(resid(model_H2_random_intercept_only),
       col = "steelblue",
       lwd = 2)
dev.off()




#############################
## 6. HYPOTHESIS 3 (H3)     ##
##    ANALYSIS              ##
#############################

# Hypothesis (H3) posits that historians provide lower Feeling of Rightness (FOR) ratings
# for abstracts with a political stance that conflicts with their political orientations.

## Model H3: Multilevel Model to Test H3
# Fit a linear mixed-effects model (LMM) using lmer() from the lme4 package
# - FOR: response variable representing the Feeling of Rightness (FOR) rating given by participants
# - treatment: fixed effect representing the experimental treatment condition (abstract's political stance)
# - political_scale: fixed effect representing participants' political orientation (1: very progressive to 7: very conservative)
# - treatment * political_scale: interaction effect between treatment and political orientation to examine if the effect of treatment
#   on FOR ratings changes with different political orientations
# - (treatment | id): random slope for treatment within participants (id) to account for individual variability in responses to treatment
# - (1 | abstract): random intercept for abstracts to account for variability between different abstracts
model_H3 <-
  lmer(
    for_resp ~ treatment + political_scale + treatment * political_scale +
      (treatment | id) + (1 |
                            abstract),
    data = df
  )
report(model_H3)
# singular fit --> reduce model complexity and withdraw random slope


# Model_H§_random_intercept_only
# Withdraw random slope for treatment
model_H3_random_intercept_only <-
  lmer(
    for_resp ~ treatment + political_scale + treatment * political_scale +
      (1 | id) + (1 | abstract),
    data = df
  )
report(model_H3_random_intercept_only)

# normality of residuals plot
qqnorm(resid(model_H3_random_intercept_only))
qqline(resid(model_H3_random_intercept_only),
       col = "steelblue",
       lwd = 2)

## H3
png(
  file.path(path_plots, "qq_plots/qqplot_H3.png"),
  width = 10,
  height = 6,
  units = "in",
  res = 300
)
qqnorm(resid(model_H3_random_intercept_only), main = "Normal Q-Q Plot")
qqline(resid(model_H3_random_intercept_only),
       col = "steelblue",
       lwd = 2)
dev.off()




#############################################
## 7. DATA VISUALIZATION (Plot 4)          ##
#############################################

## Plot 4: ILoS as a Function of Abstracts' Political Stance and Historians Political Orientation ##
df_pol_treat <- df %>%
  group_by(political_scale, treatment_string) %>%
  summarise(
    mean_response = mean(decision1_resp),
    se = sd(decision1_resp) / sqrt(n()),
    lower_bound = mean_response - se,
    upper_bound = mean_response + se
  )


# Factorize political_scale
df_pol_treat$political_scale_factor <-
  factor(
    df_pol_treat$political_scale,
    levels = 1:7,
    labels = c("1: Left", "2", "3", "4: Center", "5", "6", "7: Right")
  )

# Create the plot
political_preference_plot <-
  ggplot(
    df_pol_treat,
    aes(
      x = political_scale_factor,
      y = mean_response,
      color = treatment_string,
      group = treatment_string
    )
  ) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                width = 0.2,
                size = 0.7) +  # Add error bars
  labs(x = "Political Orientation: Left-Right Orientation Scale",
       y = "Intuitive Likelihood of Submitting for Publication",
       color = "Treatment") +
  theme_apa() +
  scale_color_viridis_d(
    option = "viridis",
    labels = c("Conservative Abstract", "Progressive Abstract"),
    begin = 0.10,
    end = 0.90
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "bottom",
    # Position the legend below the plot
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis ticks
  )

print(political_preference_plot)

# write to plots
ggsave(
  file.path(path_plots, "Figure_4_ILoS_Main_Results.png"),
  political_preference_plot,
  width = 8,
  height = 6,
  dpi = 300
)
ggsave(
  file.path(path_plots, "Figure_4_ILoS_Main_Results.pdf"),
  political_preference_plot,
  width = 8,
  height = 6
)
