#########################################################
#####                                               #####
##### Script 4: Exploratory Analyses                #####
#####           Using Multilevel Models (MLM)       #####
#####                                               #####
#####-----------------------------------------------#####
#####                                               #####
##### Content:                                      #####
#####  * 1: Setup and Library Import                #####
#####  * 2: Data Import and Preparation             #####
#####  * 3: Political Scale x Treatment Analysis    #####
#####  * 4: Exploratory Model E1 Analysis           #####
#####  * 5: Exploratory Model E2 Analysis           #####
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
library(lme4)      # linear mixed effects models


#####################################
## 2. DATA IMPORT AND INITIAL      ##
##    PREPARATION                  ##
#####################################
url <- "https://raw.githubusercontent.com/schiekiera/metascience_experiment_history/main/Data/Anonymized_History_Data_Responses.csv"
df <- read.csv(url)
functions_url <- "https://raw.githubusercontent.com/schiekiera/metascience_experiment_history/main/Functions/functions.R"
source(functions_url)



##########################################
## 3. POLITICAL SCALE x TREATMENT       ##
##    ANALYSIS                          ##
##########################################

### Mean Evaluations of progressive and conservative abstracts by conservative, moderate and progressive historians

# summarize intuitive judgements by categorical summarization of political orientation
df$political_scale_cat <-
  cut(
    df$political_scale,
    breaks = c(0, 3, 4, 8),
    # corresponds to 1-3 left, 4 moderate, 5-7 right
    labels = c("left-leaning", "moderate", "right-leaning")
  )

# group by treatment and political orientation
tab <- df %>%
  group_by(treatment_string, political_scale_cat) %>%
  summarise(mean = mean(decision1_resp),
            sd = sd(decision1_resp)) # sort by political_scale_cat
colnames(tab) <- c("Abstract", "Political_Scale", "M", "SD")
tab <- tab[order(tab$Political_Scale), ]
# print mean values
print(tab)

# subsets data by political orientation
df_right <- df[df$political_scale_cat == "right-leaning", ]
df_moderate <- df[df$political_scale_cat == "moderate", ]
df_left <- df[df$political_scale_cat == "left-leaning", ]

# t tests by political orientation
report(t.test(df_right$decision1_resp ~ df_right$treatment))
report(t.test(df_moderate$decision1_resp ~ df_moderate$treatment))
report(t.test(df_left$decision1_resp ~ df_left$treatment))



# one sample t-test
overall_mean <- mean(df$decision1_resp)

# left-leaning historians rating conservative abstracts
left_conservative_ratings <-
  subset(df,
         political_scale_cat == "left-leaning" &
           treatment_string == "conservative")$decision1_resp
left_conservative_test <-
  t.test(left_conservative_ratings,
         mu = overall_mean,
         alternative = "less")
report(left_conservative_test)

# right-leaning historians rating conservative abstracts
right_conservative_ratings <-
  subset(df,
         political_scale_cat == "right-leaning" &
           treatment_string == "conservative")$decision1_resp
right_conservative_test <-
  t.test(right_conservative_ratings,
         mu = overall_mean,
         alternative = "greater")
report(right_conservative_test)
right_conservative_test




#############################
## 4. EXPLORATORY MODEL E1  ##
##    ANALYSIS              ##
#############################


# 1. Examining the influence of different geographical regions on IloS
# Create dummy variables for geographical regions: Northern America, South/Central America, Asia, and Oceania
# Use Europe as the reference category in the MLM model
# Include additive terms for the four mentioned geographical regions

# 2. Investigating the influence of professional status group on IloS in MLM
# Convert the categorical variable 'professional status group' (levels: pre-doctoral level, post-doctoral level, professor level)
# into two dichotomous predictors: postdoc and professor
# Use pre-doctoral level as the reference category
# Include additive terms for both status group predictors and the treatment in the MLM

# 3. Investigating the influence of fatigue on researchers’ decisions
# Include a 'trial' variable (numeric; values from 1-17) to index when each abstract was presented
# Add the 'trial' variable as an additive term in the model

# 4. Investigating the influence of reading time of the abstract on decisions
# Include the 'reading_time' variable in the model after normalizing it
# Consider longer reading times as indicators of increased cognitive engagement and deeper processing

# Normalize reading time
df$read1_rt <- scale(df$read1_rt)

###############
##           ##
## M O D E L ##
##           ##
###############
model_E1 <-
  lmer(
    decision1_resp ~  +political_scale + treatment * political_scale +
      northern_america + south_central_america + asia + oceania +
      postdoctoral + professorial +
      trial +
      read1_rt +
      (treatment | id) + (1 | abstract),
    data = df
  )
report(model_E1)

# normality of residuals plot
# Save the qqplots
png(
  file.path(path_plots, "qq_plots/qqplot_exploratory_E1_model.png"),
  width = 10,
  height = 6,
  units = "in",
  res = 300
)
qqnorm(resid(model_E1), main = "Normal Q-Q Plot")
qqline(resid(model_E1), col = "steelblue", lwd = 2)
dev.off()



#############################
## 5. EXPLORATORY MODEL E2  ##
##    ANALYSIS              ##
#############################

# Investigate the influence of conscientiousness on the absolute change in Level of Support (ΔLoS)
# Fit an additional Multilevel Model (MLM) (E2) to examine whether researchers’ tendency
# to revise their rating of an abstract after an initial response is driven by conscientiousness
# rather than the political stance of the abstract

# Hypothesis: Individuals with higher levels of conscientiousness may feel a stronger sense
# of obligation to alter their responses when asked to reconsider and provide a secondary evaluation

# 1. Consider the absolute value of ΔLoS (|ΔLoS|) as the response variable
# We are interested in whether conscientiousness influences the magnitude of the change in answers,
# regardless of the direction (increase or decrease) of the change

# 2. Introduce the BFI-2-S subscale for conscientiousness as a covariate in the model
# Include 'conscientiousness' to examine its influence on the change in ratings

# Integrate these analyses into the lme4 model
# Response variable: |ΔLoS| (absolute change in Level of Support)
# Fixed effects: political stance (abstract), political orientation (individual),
#                interaction term (political stance * political orientation),
#                conscientiousness (BFI-2-S subscale)
# Random effects: political stance (abstract) within participants, and intercepts for abstracts

# Model formula
# ABS(ΔLoS) ~ political stance (abstract) + political orientation (individual) +
#          political stance (abstract) * political orientation (individual) +
#          conscientiousness +
#          (political stance (abstract) | participant) + (1 | abstract)


##########################
## Delta Response Model ##
##########################

# create delta_resp variable
df$delta_resp <- df$decision1_resp - df$decision2_resp

# absolute value of delta_resp
df$abs_delta_resp <- abs(df$delta_resp)

##Model E2
model_E2 <-
  lmer(
    abs_delta_resp ~ treatment + political_scale + treatment * political_scale +
      bfi_2_S_score +
      (treatment | id) + (1 | abstract),
    data = df
  )
report(model_E2)
# singular fit --> reduce model complexity and withdraw random slope

##Model_E2_random_intercept_only
# Withdraw random slope for treatment
model_E2_random_intercept_only <-
  lmer(
    abs_delta_resp ~ treatment + political_scale + treatment * political_scale +
      bfi_2_S_score +
      (1 | id) ,
    data = df
  )
report(model_E2_random_intercept_only)

# normality of residuals plot
# Save the qqplots
png(
  file.path(path_plots, "qq_plots/qqplot_exploratory_E2_model.png"),
  width = 10,
  height = 6,
  units = "in",
  res = 300
)
qqnorm(resid(model_E2_random_intercept_only), main = "Normal Q-Q Plot")
qqline(resid(model_E2_random_intercept_only),
       col = "steelblue",
       lwd = 2)
dev.off()



############################
###                      ###
### Exploratory Analyses ###
###                      ###
############################



###################################
### Political scale x treatment ###
###################################
### Mean Evaluations of progressive and conservative abstracts by conservative, moderate and progressive historians

# summarize intuitive judgements by categorical summarization of political orientation
df$political_scale_cat <-
  cut(
    df$political_scale,
    breaks = c(0, 3, 4, 8),
    # corresponds to 1-3 left, 4 moderate, 5-7 right
    labels = c("left-leaning", "moderate", "right-leaning")
  )

# group by treatment and political orientation
tab <- df %>%
  group_by(treatment_string, political_scale_cat) %>%
  summarise(mean = mean(decision1_resp),
            sd = sd(decision1_resp)) # sort by political_scale_cat
colnames(tab) <- c("Abstract", "Political_Scale", "M", "SD")
tab <- tab[order(tab$Political_Scale), ]
# print mean values
print(tab)

# subsets data by political orientation
df_right <- df[df$political_scale_cat == "right-leaning", ]
df_moderate <- df[df$political_scale_cat == "moderate", ]
df_left <- df[df$political_scale_cat == "left-leaning", ]

# t tests by political orientation
report(t.test(df_right$decision1_resp ~ df_right$treatment))
report(t.test(df_moderate$decision1_resp ~ df_moderate$treatment))
report(t.test(df_left$decision1_resp ~ df_left$treatment))



# one sample t-test
overall_mean <- mean(df$decision1_resp)

# left-leaning historians rating conservative abstracts
left_conservative_ratings <-
  subset(df,
         political_scale_cat == "left-leaning" &
           treatment_string == "conservative")$decision1_resp
left_conservative_test <-
  t.test(left_conservative_ratings,
         mu = overall_mean,
         alternative = "less")
report(left_conservative_test)

# right-leaning historians rating conservative abstracts
right_conservative_ratings <-
  subset(df,
         political_scale_cat == "right-leaning" &
           treatment_string == "conservative")$decision1_resp
right_conservative_test <-
  t.test(right_conservative_ratings,
         mu = overall_mean,
         alternative = "greater")
report(right_conservative_test)
right_conservative_test


#############################
###                       ###
###    E  1  M O D E L    ###
###                       ###
#############################


# 1. Examining the influence of different geographical regions on IloS
# Create dummy variables for geographical regions: Northern America, South/Central America, Asia, and Oceania
# Use Europe as the reference category in the MLM model
# Include additive terms for the four mentioned geographical regions

# 2. Investigating the influence of professional status group on IloS in MLM
# Convert the categorical variable 'professional status group' (levels: pre-doctoral level, post-doctoral level, professor level)
# into two dichotomous predictors: postdoc and professor
# Use pre-doctoral level as the reference category
# Include additive terms for both status group predictors and the treatment in the MLM

# 3. Investigating the influence of fatigue on researchers’ decisions
# Include a 'trial' variable (numeric; values from 1-17) to index when each abstract was presented
# Add the 'trial' variable as an additive term in the model

# 4. Investigating the influence of reading time of the abstract on decisions
# Include the 'reading_time' variable in the model after normalizing it
# Consider longer reading times as indicators of increased cognitive engagement and deeper processing

# Normalize reading time
df$read1_rt <- scale(df$read1_rt)

###############
##           ##
## M O D E L ##
##           ##
###############
model_E1 <-
  lmer(
    decision1_resp ~  +political_scale + treatment * political_scale +
      northern_america + south_central_america + asia + oceania +
      postdoctoral + professorial +
      trial +
      read1_rt +
      (treatment | id) + (1 | abstract),
    data = df
  )
report(model_E1)

# normality of residuals plot
# Save the qqplots
png(
  file.path(path_plots, "qq_plots/qqplot_exploratory_E1_model.png"),
  width = 10,
  height = 6,
  units = "in",
  res = 300
)
qqnorm(resid(model_E1), main = "Normal Q-Q Plot")
qqline(resid(model_E1), col = "steelblue", lwd = 2)
dev.off()



#############################
###                       ###
###    E  2  M O D E L    ###
###                       ###
#############################

# Investigate the influence of conscientiousness on the absolute change in Level of Support (ΔLoS)
# Fit an additional Multilevel Model (MLM) (E2) to examine whether researchers’ tendency
# to revise their rating of an abstract after an initial response is driven by conscientiousness
# rather than the political stance of the abstract

# Hypothesis: Individuals with higher levels of conscientiousness may feel a stronger sense
# of obligation to alter their responses when asked to reconsider and provide a secondary evaluation

# 1. Consider the absolute value of ΔLoS (|ΔLoS|) as the response variable
# We are interested in whether conscientiousness influences the magnitude of the change in answers,
# regardless of the direction (increase or decrease) of the change

# 2. Introduce the BFI-2-S subscale for conscientiousness as a covariate in the model
# Include 'conscientiousness' to examine its influence on the change in ratings

# Integrate these analyses into the lme4 model
# Response variable: |ΔLoS| (absolute change in Level of Support)
# Fixed effects: political stance (abstract), political orientation (individual),
#                interaction term (political stance * political orientation),
#                conscientiousness (BFI-2-S subscale)
# Random effects: political stance (abstract) within participants, and intercepts for abstracts

# Model formula
# ABS(ΔLoS) ~ political stance (abstract) + political orientation (individual) +
#          political stance (abstract) * political orientation (individual) +
#          conscientiousness +
#          (political stance (abstract) | participant) + (1 | abstract)


##########################
## Delta Response Model ##
##########################

# create delta_resp variable
df$delta_resp <- df$decision1_resp - df$decision2_resp

# absolute value of delta_resp
df$abs_delta_resp <- abs(df$delta_resp)

##Model E2
model_E2 <-
  lmer(
    abs_delta_resp ~ treatment + political_scale + treatment * political_scale +
      bfi_2_S_score +
      (treatment | id) + (1 | abstract),
    data = df
  )
report(model_E2)
# singular fit --> reduce model complexity and withdraw random slope

##Model_E2_random_intercept_only
# Withdraw random slope for treatment
model_E2_random_intercept_only <-
  lmer(
    abs_delta_resp ~ treatment + political_scale + treatment * political_scale +
      bfi_2_S_score +
      (1 | id) ,
    data = df
  )
report(model_E2_random_intercept_only)

# normality of residuals plot
# Save the qqplots
png(
  file.path(path_plots, "qq_plots/qqplot_exploratory_E2_model.png"),
  width = 10,
  height = 6,
  units = "in",
  res = 300
)
qqnorm(resid(model_E2_random_intercept_only), main = "Normal Q-Q Plot")
qqline(resid(model_E2_random_intercept_only),
       col = "steelblue",
       lwd = 2)
dev.off()
