
# 
# Script: 00_setup.R 
# Project: Bipolar Disorder and Education: 
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Set up libraries and functions
#
# Author: Vaishnavi K Madhavan
# Start date: 12-04-2025
# 

setwd("N:/durable/projects/limited_C/36199047_Vaish_BipolarEA/")

# 1. Load R Packages ####

library(tidyverse)
library(psych)
library(dplyr)
library(purrr)
library(data.table)
library(pscl)        # R2 calculation for PGS
library(openxlsx)    # for creating worksheets
library(tibble)      # for sample decriptives 
library(knitr)       # for sample decriptives 
library(glue)        # for sample decriptives 
library(sandwich)    # for sandwich clustering
library(lmtest)      # for sandwich clustering
library(survival)    # for cox regression
library(survminer)   # for survival/cumulative plots 
library(ggplot2)     # for bful plots 
library(broom)
library(forcats)
library(stringr)
library(kableExtra)  # for tables
library(performance) # for incremental R2
library(splines)     # check non-linearity
library(patchwork)
library(scales)
library(ranger)
library(ggfortify)
library(splines)     # cox model with non-linear associations


# 2. Functions ####

#* 2.1. Create between and within variables ----

## Documentation:
#  Function to create between and within sibship variables 
#  Arguments:
#         df = name of the dataframe,
#         variable = name of the variable,
#         family_id = variable to use for clustering 

create_between_within <- function(df, variable, family_id) {
  
  df <- df %>%
    group_by(.data[[family_id]]) %>%
    mutate(
      n_valid = sum(!is.na(.data[[variable]])),
      "{variable}_between" := ifelse(n_valid > 1, mean(.data[[variable]], na.rm = TRUE), NA_real_),
      "{variable}_within"  := ifelse(n_valid > 1, .data[[variable]] - mean(.data[[variable]], 
                                                                           na.rm = TRUE), NA_real_)
    ) %>%
    ungroup() %>%
    select(-n_valid)
  
  return(df)
}


#* 2.2. Create Summary ----

## Documentation:
#  Function to obtain descriptive statistics for specified variables
#  
#  Arguments:
#        - data = name of the dataframe,
#        - continuous: list of continuous variable column names,
#        - categorical: list of categorical variable column names,
#        - FID: Family ID variable name,
#        - digits: the number of decimal points required in the statistics
#  

create_summary <- function(data,
                           continuous = NULL,
                           categorical = NULL,
                           FID,
                           digits) {
  
  # force into appropriate formats
  data <- as.data.frame(data)
  
  continuous <- continuous[continuous %in% names(data)]
  categorical <- categorical[categorical %in% names(data)]
  
  
  if(!is.null(continuous)) {
    for(v in continuous) {
      
      data[[v]] <- as.numeric(as.character(data[[v]]))
    }
  }
  
  # start output table
  out <- data.frame(
    Variable = c("Sample Size", "Number of Families"),
    Level = c("", ""),
    Summary = c(
      nrow(data),
      n_distinct(data[[FID]])
    ),
    stringsAsFactors = FALSE)
  
  # continuous variables
  if(!is.null(continuous)) { 
    for(v in continuous) { 
      
      summary <- paste0(
        round(mean(data[[v]], na.rm = TRUE), digits),
        " (",
        round(sd(data[[v]], na.rm = TRUE), digits),
        "), [",
        round(min(data[[v]], na.rm = TRUE), digits),
        ", ",
        round(max(data[[v]], na.rm = TRUE), digits),
        "]"
      )
      
      out <- rbind(
              out, 
              data.frame(
                  Variable = v,
                  Level = "", 
                  Summary = summary, 
                  stringsAsFactors = FALSE
        )
      )
    }}
  
  # categorical variables
  if(!is.null(categorical)) { 
    for(v in categorical) { 
      
      tab <- table(data[[v]], useNA = "no")
      pct <- prop.table(tab) * 100
      
      tmp <- data.frame(
                Variable = c(v, rep("", length(tab) - 1)),
                Level = names(tab),
                Summary = paste0(as.integer(tab), " (", round(pct, 2), "%)"),
                stringsAsFactors = FALSE
        )
      
      out <- rbind(out, tmp)
    }
  }
  
  rownames(out) <- NULL
  out
  
  } 


#* 2.3. Run Linear Regression with Clustered SEs for bipolar PGS - EA ----

## Documentation:
#  Function to run a linear regression model + correct SEs using coeftest
#  
#  Arguments:
#         data = name of the dataframe,
#         outcome, predictor, covariates, cluster_var = variables,
#         Cohort = the cohort/dataset,
#         Model = Population/Sibling,
#         Analysis = Main/Covariate/Orthogonalized/etc.,
#  
#  Note: Cohort, Model, and Analysis are for adding meta data

run_reg_clustered <- function(cohort, model, data, outcome, predictor, covariates, cluster_var,
                              analysis) {
  
  # build model formula
  formula_str <- paste0(outcome, " ~ ", paste(predictor, " + ", covariates, collapse = " + "))
  model_formula <- as.formula(formula_str)
  
  # fit regression model
  reg_model <- lm(model_formula, data = data, x = TRUE)
  
  # robust (clustered) variance-covariance matrix
  cluster <- data[[cluster_var]]
  robust_vcov <- vcovCL(reg_model, cluster = cluster)
  
  # no of observations in the model
  N = nobs(reg_model)
  
  # extract tidy model summary with robust SEs
  tidy_out <- coeftest(reg_model, vcov. = robust_vcov)
  
  # format into data frame
  robust_results <- broom::tidy(tidy_out) %>%
    mutate(
      Cohort = cohort, 
      Model = model,
      Estimate = round(estimate, 3),
      SE = round(std.error, 3),
      CI_lower = round(estimate - 1.96 * SE, 3),
      CI_upper = round(estimate + 1.96 * SE, 3),
      p_value = round(p.value, 4),
      N = N,
      Analysis = analysis
    ) %>%
    select(Cohort, Model, Variable=term, 
           Estimate, SE, CI_lower, CI_upper, p_value, N, Analysis)
  
  return(robust_results)
}


#* 2.4. Run Cox Regression with Clustered SEs for Academic Performance - Bipolar Disorder Diagnosis ----

## Documentation:
#  Function to run a linear regression model + correct SEs using coeftest
#  
#  Arguments:
#         data = name of the dataframe,
#         time = time until observation/diagnosis to right censor outcome 
#         outcome = dichotomous variable 
#         predictor, covariates, cluster_var = variables,
#         cohort = the cohort/dataset,
#         model = Population/Sibling,
#         analysis = Main/Orthogonalized/etc.,
#         is_sibdata = sibdata or not 
#         (the sample needs to be filtered for each grade)
#
#  Note: cohort, grade, test, model, and analysis  are for adding meta_data

run_cox_clustered <- function(cohort, grade, test, model, data, outcome, predictor, 
                              time_var, covariates, cluster_var, analysis, is_sibdata) {
  
  # combine predictors + covariates
  all_terms <- c(predictor, covariates)
  
  # build formula
  formula_str <- paste0("Surv(", time_var, ", ", outcome, ") ~ ",
    paste(all_terms, collapse = " + ")
  )
  model_formula <- as.formula(formula_str)
  
  # variables used in the model
  vars_used <- unique(c(all.vars(model_formula)))
  
  # restrict sample to complete cases
  data_model <- data %>%
    filter(complete.cases(across(all_of(vars_used))))
  
  # if sibdata, filter go families with at least 2 siblings 
  if (is_sibdata) {
    data_model <- data_model %>%
      group_by(.data[[cluster_var]]) %>%
      filter(n() >= 2) %>%
      ungroup()
  }
  
  # fit Cox model with clustering
  cox_model <- coxph(
    model_formula,
    data = data_model
  )
  
  # robust (clustered) variance-covariance matrix
  cluster <- data_model[[cluster_var]]
  robust_vcov <- vcovCL(cox_model, cluster = cluster)
  
  # extract tidy model summary with robust SEs
  tidy_out <- coeftest(cox_model, vcov. = robust_vcov)
  
  # no of observations in the model
  N = cox_model$n
  
  # tidy output
  robust_results <- broom::tidy(tidy_out) %>%
    mutate(
      Cohort = cohort, 
      Grade = grade, 
      Test = test, 
      Model = model,
      Estimate = round(estimate, 3),
      SE = round(std.error, 3),
      HR = round(exp(Estimate), 3),
      CI_lower = round(exp(Estimate - 1.96 * SE), 3),
      CI_upper = round(exp(Estimate + 1.96 * SE), 3),
      p_value = round(p.value, 4),
      N = N,
      Analysis = analysis
    ) %>%
    select(Cohort, Grade, Test, Model, Variable = term, 
           Estimate, SE, HR, CI_lower, CI_upper, p_value, N, Analysis
    )
  
  return(robust_results)
}


#* 2.5. Supplementary - Run Linear Regression with Clustered SEs for Bipolar PGS - Academic Performance ----

## Documentation:
#  Function to run a linear regression model + correct SEs using coeftest
#  (similar to function 2.3. but this is for academic performance, so
#       includes meta data about age and test)
#
#  Arguments:
#         data = name of the dataframe,
#         outcome, predictor, covariates, cluster_var = variables,
#         Cohort = the cohort/dataset,
#         Model = Population/Sibling,
#         Analysis = Main/Covariate/Orthogonalized/etc.,
#  
#  Note: Cohort, Model, and Analysis are for adding meta data

supplementary_run_reg_clustered <- function(cohort, grade, test, model, data, outcome, predictor, covariates, cluster_var,
                              is_sibdata) {
  
  # combine predictors + covariates
  all_terms <- c(predictor, covariates)
  
  # build formula
  formula_str <- paste0(outcome, " ~ ",
    paste(all_terms, collapse = " + ")
  )
  
  model_formula <- as.formula(formula_str)
  
  # variables used in the model
  vars_used <- unique(c(all.vars(model_formula), cluster_var))
  
  # restrict sample to complete cases
  data_model <- data %>%
    filter(complete.cases(across(all_of(vars_used))))
  
  # if sibdata, filter go families with at least 2 siblings 
  if (is_sibdata) {
    data_model <- data_model %>%
      group_by(.data[[cluster_var]]) %>%
      filter(n() >= 2) %>%
      ungroup()
  }
  
  # fit regression model
  reg_model <- lm(model_formula, data = data_model, x = TRUE)
  
  # robust (clustered) variance-covariance matrix
  cluster <- data_model[[cluster_var]]
  robust_vcov <- vcovCL(reg_model, cluster = cluster)
  
  # extract tidy model summary with robust SEs
  tidy_out <- coeftest(reg_model, vcov. = robust_vcov)
  
  # no of observations in the model
  N = nobs(reg_model)
  
  # format into data frame
  robust_results <- broom::tidy(tidy_out) %>%
    mutate(
      Cohort = cohort, 
      Grade = grade, 
      Test = test, 
      Model = model,
      Estimate = round(estimate, 3),
      SE = round(std.error, 3),
      CI_lower = round(estimate - 1.96 * SE, 3),
      CI_upper = round(estimate + 1.96 * SE, 3),
      p_value = round(p.value, 4),
      N = N,
    ) %>%
    select(Cohort, Grade, Test, Model, Variable = term, 
           Estimate, SE, CI_lower, CI_upper, p_value, N)
  
  return(robust_results)
}


#* 2.6. Run Logistic Regression with Clustered SEs for Academic Performance - Bipolar Disorder Diagnosis ----

## Documentation:
#  Function to run a linear regression model + correct SEs using coeftest
#  
#  Arguments:
#         data = name of the dataframe,
#         outcome = dichotomous variable 
#         predictor, covariates, cluster_var = variables,
#         cohort = the cohort/dataset,
#         model = Population/Sibling,
#         analysis = Main/Orthogonalized/etc.,
#         is_sibdata = sibdata or not 
#         (the sample needs to be filtered for each grade)
#
#  Note: cohort, grade, test, model, and analysis  are for adding meta_data

supplementary_run_log_reg <- function(cohort, grade, test, model, data, outcome, predictor, 
                                      covariates, cluster_var, analysis, is_sibdata) {
  
  # combine predictors + covariates
  all_terms <- c(predictor, covariates)
  
  # build model formula
  formula_str <- paste0(outcome, " ~ ", paste(all_terms, collapse = " + "))
  model_formula <- as.formula(formula_str)
  
  # variables used in the model
  vars_used <- unique(c(all.vars(model_formula), cluster_var))
  
  # restrict sample to complete cases
  data_model <- data %>%
    filter(complete.cases(across(all_of(vars_used))))
  
  
  # if sibdata, filter go families with at least 2 siblings 
  if (is_sibdata) {
    data_model <- data_model %>%
      group_by(.data[[cluster_var]]) %>%
      filter(n() >= 2) %>%
      ungroup()
  }
  
  # fit logistic regression model
  logistic_model <- glm(model_formula, data = data_model, family = binomial)
  
  # robust (clustered) variance-covariance matrix
  cluster <- data_model[[cluster_var]]
  robust_vcov <- vcovCL(logistic_model, cluster = cluster)
  
  # extract tidy model summary with robust SEs
  tidy_out <- coeftest(logistic_model, vcov. = robust_vcov)
  
  # no of observations in the model
  N = nobs(logistic_model)  
  
  # tidy output
  robust_results <- broom::tidy(tidy_out) %>%
    mutate(
      Cohort = cohort, 
      Grade = grade, 
      Test = test, 
      Model = model,
      Estimate = round(estimate, 3),
      SE = round(std.error, 3),
      OR = round(exp(Estimate), 3),
      CI_lower = round(exp(Estimate - 1.96 * SE), 3),
      CI_upper = round(exp(Estimate + 1.96 * SE), 3),
      p_value = round(p.value, 4),
      N = N,
      Analysis = analysis
    ) %>%
    select(Cohort, Grade, Test, Model, Variable = term, 
           Estimate, SE, OR, CI_lower, CI_upper, p_value, N, Analysis
    )
  
  return(robust_results)
}


#* 2.7. Non-linearity - Run Spline Cox Regression for Academic Performance - Bipolar Disorder Diagnosis ----

## Documentation:
#  Function to run a linear regression model + correct SEs using coeftest
#  
#  Arguments:
#         data = name of the dataframe,
#         outcome = dichotomous variable 
#         predictor, covariates, cluster_var = variables,
#         cohort = the cohort/dataset,
#         analysis = Main/Orthogonalized/etc.,
#         is_sibdata = sibdata or not 
#         (the sample needs to be filtered for each grade)
#
#  Note: cohort, grade, test, model, and analysis  are for adding meta_data

compare_cox_nonlinearity <- function(cohort, grade, test, data, outcome, predictor, time_var,
                                     covariates, cluster_var, analysis, is_sibdata, spline_df = 4) {
  
  # Build main model formula
  all_terms_main <- c(predictor, covariates)
  
  formula_main_str <- paste0(
    "Surv(", time_var, ", ", outcome, ") ~ ",
    paste(all_terms_main, collapse = " + ")
  )
  
  formula_main <- as.formula(formula_main_str)
  
  # build non-linear model formula
  all_terms_nonlinear <- c(
    paste0("ns(", predictor, ", df = ", spline_df, ")"),
    covariates
  )
  
  formula_nonlinear_str <- paste0(
    "Surv(", time_var, ", ", outcome, ") ~ ",
    paste(all_terms_nonlinear, collapse = " + ")
  )
  
  formula_nonlinear <- as.formula(formula_nonlinear_str)
  
  # variables needed for both models
  vars_used <- unique(c(
    all.vars(formula_main),
    all.vars(formula_nonlinear),
    cluster_var
  ))
  
  # filter to complete cases
  # IMPORTANT: same sample for both models
  data_model <- data %>%
    dplyr::filter(
      complete.cases(dplyr::across(dplyr::all_of(vars_used)))
    )
  
  
  # restrict to sibling families
  if (is_sibdata) {
    data_model <- data_model %>%
      dplyr::group_by(.data[[cluster_var]]) %>%
      dplyr::filter(dplyr::n() >= 2) %>%
      dplyr::ungroup()
  }
  
  # fit main model
  model_main <- survival::coxph(
    formula_main,
    data = data_model
  )
  
  # fit non-linear model
  model_nonlinear <- survival::coxph(
    formula_nonlinear,
    data = data_model
  )
  
  # likelihood ratio test
  comparison <- anova(
    model_main,
    model_nonlinear,
    test = "LRT"
  )
  
  # extract non-linearity test
  result <- data.frame(
    Cohort = cohort,
    Grade = grade,
    Test = test,
    Model_Comparison = "Linear vs non-linear",
    N = nrow(data_model),
    LR = comparison$`Chisq`[2],
    df = comparison$Df[2],
    p_value = comparison$`Pr(>|Chi|)`[2]
  )
  
  return(result)
}

