
#
# Script: 01_Set_Up.R
# Project: Bipolar Disorder and Education:
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Setting up Libraries and Functions
#
# Author: Vaishnavi K Madhavan
# Start date: 28-11-2025
# 


# 1. Set Up ####
setwd("H:/Bipolar-EA/")

library(haven)
library(dplyr)
library(data.table)
library(readr)       # for saving .csv files
library(tidyr)
library(stringr)
library(purrr)
library(psych)
library(lubridate)
library(ggplot2)
library(sandwich)    # for sandwich clustering
library(lmtest)      # for sandwich clustering
library(survival)    # for cox regression
library(forcats)
library(lubridate)
library(splines)
library(patchwork)
library(scales)
library(splines)     # for non-linearity
library(patchwork)   # for non-linearity
library(scales)      # for non-linearity


# 2. Functions ####

#* 2.1. Extract Diagnostic Status - Detailed Datasets ----

## function to obtain case/controls for each disorder (adapted from Perline Demange's code: )
# Documentation:
# The function returns a table with one row per individuals. For every year, 
#     for every diagnoses (both main and secondary, and total), it states True or False 
#     if the individual was diagnosed. 
# Warning: Only works if the all disorders code containing the string code provided are 
# part of the disorder of interest
# Arguments: 
#   - codefile: data frame with two columns, $trait containing the name of the disorder, 
#       $code containing the DSM IV code for the disorder
#   - data_list: data frame with three columns: $year: year of the data, 
#       $main: data with path to the main diagnoses data,
#       $second: data with path to the secondary diagnoses data
#   - gba: column of individual ID 
# Output: data table with one column for the year, and three columns for each disorder
#     (main and secondary diagnoses, and both)

get_diagnoses_2011_2016 <- function(codefile, data_list, gba){
  
  gba_allyears <- NULL 
  trait_list <- codefile$trait #do this outside of the loop will save time 
  code_list <- codefile$code
  year <- data_list$year
  file_list_main <- data_list$main
  file_list_second <- data_list$second
  
  for (y in 1:nrow(data_list)) { # for each year
    gba_year <- gba
    gba_year$year <- data_list$year[y] #get year
    print(year[y])

    
    main <- read_sav(file_list_main[y]) #load data main diagnosis for the correct year
    second <- read_sav(file_list_second[y]) #load data second diagnosis for the correct year
    
    for(x in 1:nrow(codefile)) { # for each trait
      trait <- trait_list[x]
      print(trait)
      code <- code_list[x]
      
      
      # make sure codes are character
      main_codes <- as.character(main$GGZDBCHoofddiagnoseDSMIV)
      second_codes <- as.character(second$GGZDBCnevendiagnoseDSMIV)
      
      main_col <- paste(trait, "main", sep="_")
      second_col <- paste(trait, "second", sep="_")
      combined_col <- paste(trait, sep="_")
      date_col <- paste(trait, "date", sep="_")
      
      
      # filter main and second tables to rows containing the code
      main_hits <- main[!is.na(main_codes) & 
                          grepl(code, main_codes), ] 
      second_hits <- second[!is.na(second_codes) & 
                              grepl(code, second_codes), ] 
      
      # change rinpersoon into numeric
      gba_year$RINPERSOON <- as.numeric(gba_year$RINPERSOON)
      main_hits$RINPERSOON <- as.numeric(main_hits$RINPERSOON)
      second_hits$RINPERSOON <- as.numeric(second_hits$RINPERSOON)
      
      # for the RINPERSOON that are in main_hits (which are the ones with a diagnosis), give them the value "TRUE"
      main_flag <- main_hits %>%
        distinct(RINPERSOON) %>%
        mutate(main_flag = TRUE)
      
      # if main_flag is true, the main_col will be true, if not false. Also remove main_flag in the end
      gba_year <- gba_year %>%
        left_join(main_flag, by = "RINPERSOON") %>%
        mutate(
          ! !main_col := !is.na(main_flag)
        ) %>%
        select(-main_flag)
      
      ## same for secondary diagnoses
      second_flag <- second_hits %>%
        distinct(RINPERSOON) %>%
        mutate(second_flag = TRUE)
      
      gba_year <- gba_year %>%
        left_join(second_flag, by = "RINPERSOON") %>%
        mutate(
          ! !second_col := !is.na(second_flag)
        ) %>%
        select(-second_flag)
      
      gba_year[[combined_col]] <- gba_year[[main_col]] | gba_year[[second_col]]
      gba_year[[date_col]] <- NA

      # main dates (main_hits still has multiple rows per RINPERSOON, so need to aggregate)
      # FUN = min picks the earlier date avialble for each RINPERSOON
      main_dates <- aggregate(
        GGZDBCStartdatumZTR ~ RINPERSOON,
        data = main_hits,
        FUN = min, 
        na.rm = TRUE)
      
      ## assigns dates for main diagnoses back to the gba_year$date_col
      idx <- match(gba_year$RINPERSOON, main_dates$RINPERSOON)
      gba_year[[date_col]][!is.na(idx)] <- main_dates$GGZDBCStartdatumZTR[idx[!is.na(idx)]]
     
      second_dates <- aggregate(
        GGZDBCDiagnosedatum ~ RINPERSOON,
        data = second_hits,
        FUN = min, 
        na.rm = TRUE)
      
      ## whatever date is missing in gba_year$date_col, this is filled by secondary dates if they are not NA
      idx2 <- match(gba_year$RINPERSOON, second_dates$RINPERSOON)
      use <- is.na(gba_year[[date_col]]) & !is.na(idx2)
      gba_year[[date_col]][use] <- second_dates$GGZDBCDiagnosedatum[idx2[use]]
      
    }
    
    gba_allyears <- rbind(gba_allyears, gba_year)
  } 
  
  return(gba_allyears)
  
} 


#* 2.2. Extract Diagnostic Status - Hoofd Datasets ----

get_diagnoses_2015_2021 <- function(codefile, data_list, gba){
  
  gba_allyears <- NULL 
  trait_list <- codefile$trait #do this outside of the loop will save time 
  code_list <- codefile$code
  year <- data_list$year
  file_list_main <- data_list$main
  file_list_second <- data_list$second
  
  for (y in 1:nrow(data_list)) { #for each year
    gba_year <- gba
    gba_year$year <- data_list$year[y] #get year
    print(year[y])
    
    main <- read_sav(file_list_main[y]) #load data main diagnosis for the correct year
    second <- read_sav(file_list_second[y]) #load data second diagnosis for the correct year
    
    for(x in 1:nrow(codefile)){ #for each trait
      trait <- trait_list[x]
      print(trait)
      code <- code_list[x]
      
      # make sure codes are character
      main_codes <- as.character(main$GGZDBCHoofddiagnoseDSMIVHOOFDDIAG)
      second_codes <- as.character(second$GGZDBCnevendiagnoseDSMIVHOOFDDIAG)
      
      main_col <- paste(trait, "main", sep="_")
      second_col <- paste(trait, "second", sep="_")
      combined_col <- paste(trait, sep="_")
      date_col <- paste(trait, "date", sep="_")
      
      # filter main and second tables to rows containing the code
      main_hits <- main[!is.na(main_codes) & 
                          grepl(code, main_codes), ] 
      second_hits <- second[!is.na(second_codes) & 
                              grepl(code, second_codes), ] 
      
      # change rinpersoon into numeric
      gba_year$RINPERSOON <- as.numeric(gba_year$RINPERSOON)
      main_hits$RINPERSOON <- as.numeric(main_hits$RINPERSOON)
      second_hits$RINPERSOON <- as.numeric(second_hits$RINPERSOON)
      
      # for the RINPERSOON that are in main_hits (which are the ones with a diagnosis), give them the value "TRUE"
      main_flag <- main_hits %>%
        distinct(RINPERSOON) %>%
        mutate(main_flag = TRUE)
      
      # if main_flag is true, the main_col will be true, if not false. Also remove main_flag in the end
      gba_year <- gba_year %>%
        left_join(main_flag, by = "RINPERSOON") %>%
        mutate(
          ! !main_col := !is.na(main_flag)
        ) %>%
        select(-main_flag)
      
      ## same for secondary diagnoses
      second_flag <- second_hits %>%
        distinct(RINPERSOON) %>%
        mutate(second_flag = TRUE)
      
      gba_year <- gba_year %>%
        left_join(second_flag, by = "RINPERSOON") %>%
        mutate(
          ! !second_col := !is.na(second_flag)
        ) %>%
        select(-second_flag)
      
      ## I commented this out because it forms multiple rows for each RINPERSOON
      gba_year[[combined_col]] <- gba_year[[main_col]] | gba_year[[second_col]]
      gba_year[[date_col]] <- NA

      # main dates (main_hits still has multiple rows per RINPERSOON, so need to aggregate)
      # FUN = min picks the earlier date avialble for each RINPERSOON
      main_dates <- aggregate(
        GGZDBCStartdatumZTRHOOFDDIAG ~ RINPERSOON,
        data = main_hits,
        FUN = min, 
        na.rm = TRUE)
      
      ## assigns dates for main diagnoses back to the gba_year$date_col
      idx <- match(gba_year$RINPERSOON, main_dates$RINPERSOON)
      gba_year[[date_col]][!is.na(idx)] <- main_dates$GGZDBCStartdatumZTRHOOFDDIAG[idx[!is.na(idx)]]
      
      second_dates <- aggregate(
        GGZDBCDiagnosedatumHOOFDDIAG ~ RINPERSOON,
        data = second_hits,
        FUN = min, 
        na.rm = TRUE)
      
      ## whatever date is missing in gba_year$date_col, this is filled by secondary dates if they are not NA
      idx2 <- match(gba_year$RINPERSOON, second_dates$RINPERSOON)
      use <- is.na(gba_year[[date_col]]) & !is.na(idx2)
      gba_year[[date_col]][use] <- second_dates$GGZDBCDiagnosedatumHOOFDDIAG[idx2[use]]
      
    }
    
    gba_allyears <- rbind(gba_allyears, gba_year)
  } 
  
  return(gba_allyears)
  
} 


#* 2.3. Extract Diagnostic Status - Vektis Datasets ----

get_diagnoses_vektis <- function(codefile, data_list, gba){
  
  gba_allyears <- NULL 
  trait_list <- codefile$trait #do this outside of the loop will save time 
  code_list <- codefile$code
  year <- data_list$year
  file_list_main <- data_list$main
  file_list_second <- data_list$second
  
  for (y in 1:nrow(data_list)) { #for each year
    gba_year <- gba
    gba_year$year <- data_list$year[y] #get year
    print(year[y])
    
    main <- read_sav(file_list_main[y]) #load data main diagnosis for the correct year
    #second <- read_sav(file_list_second[y]) #load data second diagnosis for the correct year
    
    for(x in 1:nrow(codefile)){ #for each trait
      trait <- trait_list[x]
      print(trait)
      code <- code_list[x]
      
      # make sure codes are character
      main_codes <- as.character(main$diagnosecode)
      # second_codes <- as.character(second$GGZDBCnevendiagnoseDSMIVHOOFDDIAG)
      
      main_col <- paste(trait, "main", sep="_")
      #second_col <- paste(trait, "second", sep="_")
      #combined_col <- paste(trait, sep="_")
      date_col <- paste(trait, "date", sep="_")
      
      # filter main and second tables to rows containing the code
      main_hits <- main[!is.na(main_codes) & 
                          grepl(code, main_codes), ] 
      #second_hits <- second[!is.na(second_codes) & 
      #                        grepl(code, second_codes), ] 
      
      # change rinpersoon into numeric
      gba_year$RINPERSOON <- as.numeric(gba_year$RINPERSOON)
      main_hits$RINPERSOON <- as.numeric(main_hits$RINPERSOON)
      #second_hits$RINPERSOON <- as.numeric(second_hits$RINPERSOON)
      
      # for the RINPERSOON that are in main_hits (which are the ones with a diagnosis), give them the value "TRUE"
      main_flag <- main_hits %>%
        distinct(RINPERSOON) %>%
        mutate(main_flag = TRUE)
      
      # if main_flag is true, the main_col will be true, if not false. Also remove main_flag in the end
      gba_year <- gba_year %>%
        left_join(main_flag, by = "RINPERSOON") %>%
        mutate(
          ! !main_col := !is.na(main_flag)
        ) %>%
        select(-main_flag)
      
      #gba_year[[combined_col]] <- gba_year[[main_col]] | gba_year[[second_col]]
      gba_year[[date_col]] <- NA
      
      # main dates (main_hits still has multiple rows per RINPERSOON, so need to aggregate)
      # FUN = min picks the earlier date avialble for each RINPERSOON
      main_dates <- aggregate(
        begindatum_prestatie ~ RINPERSOON,
        data = main_hits,
        FUN = min, 
        na.rm = TRUE 
      )
      
      ## assigns dates for main diagnoses back to the gba_year$date_col
      idx <- match(gba_year$RINPERSOON, main_dates$RINPERSOON)
      gba_year[[date_col]][!is.na(idx)] <- main_dates$begindatum_prestatie[idx[!is.na(idx)]]
      
    }
    
    gba_allyears <- rbind(gba_allyears, gba_year)
  } 
  
  return(gba_allyears)
  
} 


#* 2.4. Diagnosis Status per Person ----

# Documentation
# Returns diagnoses per person for all years pulled together, 1 if they were diagnosed at least once, 0 if not
# Arguments: 
#   - full_diagnosis_data is the dataframe of diagnoses (main and secondary pulled together) for every person for every year
#   - population_name is the name of the subset of the population used 
# Careful: When running summarise, the columns are not logical anymore but just containing 0 or 1 !

get_diagnoses_totalyear_per_person <- function(full_diagnosis_data, population_name){

  diagnoses_totalyears_per_person <- full_diagnosis_data %>%
    group_by(RINPERSOON) %>%
    summarise_if(is.logical, max)
  diagnoses_totalyears_per_person <- as.data.frame(diagnoses_totalyears_per_person)
  return(diagnoses_totalyears_per_person)
}



#* 2.5. Descriptive Table per Year and Total ----

# Documentation 
# Function returns a table countaining the count and prevalence of the diagnoses per year and in total 
# Arguments
#   - full_diagnosis_data is the dataframe of diagnoses (main and secondary pulled together) 
#       for every person for every year
#   - totalyear_diagnosis_data is the output of get_diagnoses_totalyear_per_person, 
#       diagnoses per person for all years pulled together
#   - population_name is the name of the subset of the population used 
#   - trait_list is codefile$trait. Codefile is a data frame with two columns, $trait containing the name of the disorder, 
#       $code containing the DSM IV code for the disorder

get_descriptive_table <- function(full_diagnosis_data, totalyear_diagnosis_data, population_name, trait_list){

  # 1. Count number of persons diagnosed at least once for each disorder for all years
  count_diagnoses_totalyears <- totalyear_diagnosis_data %>%
    summarise_at(vars(-RINPERSOON), sum)
  count_diagnoses_totalyears$year <- 'total'
  
  # 2. Count number of persons diagnosed for each disorder every year
  count_diagnoses_everyyears <- full_diagnosis_data %>%
    group_by(year) %>%
    summarise_if(is.logical, sum)
  count_diagnoses_total <- rbind(count_diagnoses_totalyears, count_diagnoses_everyyears)
  count_diagnoses_total$sample_size <- nrow(totalyear_diagnosis_data)
  count_diagnoses_total$population <- population_name
  count_diagnoses_total$measure <- "count"
  
  # 3. Prevalence calculation
  sample_size <- nrow(totalyear_diagnosis_data)
  prevalence <- count_diagnoses_total[, 1:length(trait_list)]/sample_size #I removed the *100 to make it percent
  prevalence$year <- count_diagnoses_total$year
  prevalence$sample_size <- nrow(totalyear_diagnosis_data)
  prevalence$population <- population_name
  prevalence$measure <- "prevalence"
  
  # Get prevalence CI 
  prevalence_CI <- 1.96*sqrt((prevalence[, 1:length(trait_list)]*(1-prevalence[, 1:length(trait_list)]))/sample_size)
  prevalence_CI$year <- count_diagnoses_total$year
  prevalence_CI$sample_size <- nrow(totalyear_diagnosis_data)
  prevalence_CI$population <- population_name
  prevalence_CI$measure <- "prevalence_CI"
  
  descriptive_table <- rbind(count_diagnoses_total, prevalence, prevalence_CI)
  descriptive_table <- format(descriptive_table, scientific=F)
  return(descriptive_table)
  
}


#* 2.6. Create Between and Within Variables ----

# Documentation 
# Function to form between and within-sibship variables 
# Arguments
#   - df: dataframe
#   - score_var: score variables
#   - family_id: variable used for family clustering

add_between_within <- function(df, score_var, family_id) {
  if (!(score_var %in% names(df))) {
    warning(paste("Variable", score_var, "not found in dataframe. Skipping..."))
    return(df)
  }
  
  df <- df %>%
    group_by(.data[[family_id]]) %>%
    mutate(
      n_valid = sum(!is.na(.data[[score_var]])),
      "{score_var}_between" := ifelse(n_valid > 1, mean(.data[[score_var]], na.rm = TRUE), NA_real_),
      "{score_var}_within"  := ifelse(n_valid > 1, .data[[score_var]] - mean(.data[[score_var]], na.rm = TRUE), NA_real_)
    ) %>%
    ungroup() %>%
    select(-n_valid)
  
  return(df)
}


#* 2.7. Create Descriptive Table ----

## function to obtain descriptive statistics for specified variables
# Documentation:
# The function returns a table with one row per variable + sample size and number of families.  
# Arguments: 
#   - data: name of the dataframe, 
#   - continuous: list of the continuous variable column names 
#   - categorical: list of the categorical variable column names 
#   - FID: Family ID variable name
#   - digits: the number of decimal points required in the statistics

create_summary <- function(data, 
                           continuous = NULL, 
                           categorical = NULL, 
                           FID, 
                           digits) {
  
  # force into appropriate formats
  data <- as.data.frame(data)
  
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
  
  ## continuous variables
  if(!is.null(continuous)) { 
    for(v in continuous) { 
      
      summary <- paste0(
        round(mean(data[[v]], na.rm = TRUE), digits),
        " (",
        round(sd(data[[v]], na.rm = TRUE), digits), 
        ")"
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
    }
  }
  
  
  # categorical variables
  if(!is.null(categorical)) { 
    for(v in categorical) {
      
      tab <- table(data[[v]], useNA = "no")
      pct <- prop.table(tab) * 100
      
      tmp <- data.frame(
        Variable = c(v, rep("", length(tab) - 1)),
        Level = names(tab),
        Summary = paste0(
          as.integer(tab),
          " (",
          round(pct, 2),
          "%)"
        ),
        stringsAsFactors = FALSE
      )
      
      out <- rbind(out, tmp)
      
    } 
  }
  
  rownames(out) <- NULL
  out
}


#* 2.8. Run Cox Regression ----

# Documentation:
## function to run cox regression with clustered SEs
# Arguments: 
#   - cohort: SSB/CBS, 
#   - test: Math/Reading/Language/English,  
#   - data: popdata/sibdata,
#   - cluster_var: FID,
#   - analysis: Main/Adjusted
#   - is_sibdata: to filter to sample of sibs 

run_cox_clustered <- function(cohort, grade, age, test, model, data, outcome, predictor, 
                              time_var, covariates, cluster_var, analysis, is_sibdata) {
  
  # combine predictors + covariates
  all_terms <- c(predictor, covariates)
  
  # build formula
  formula_str <- paste0(
    "Surv(", time_var, ", ", outcome, ") ~",
    paste(all_terms, collapse = " + ")
  )
  model_formula <- as.formula(formula_str)
  
  # variables used in the model 
  vars_used <- unique(c(all.vars(model_formula)))
  
  # restrict sample to complete cases
  data_model <- data %>%
    filter(complete.cases(across(all_of(vars_used))))
  
  # if is_sibdata, filter to families with at least 2 siblings 
  if(is_sibdata) { 
    data_model <- data_model %>%
      group_by(.data[[cluster_var]]) %>%
      filter(n() >= 2) %>%
      ungroup()
  }
  
  
  # fit Cox model with clustering 
  cox_model <- coxph(model_formula,
                     data = data_model)
  
  # robust variance-covariance matrix
  cluster <- data_model[[cluster_var]]
  robust_vcov <- vcovCL(cox_model, cluster = cluster)
  
  # extract model summary with robust SEs
  tidy_out <- coeftest(cox_model, vcov. = robust_vcov)
  
  # no of observations in the model
  N = cox_model$n
  
  # output
  robust_results <- broom::tidy(tidy_out) %>%
    mutate(
      Cohort = cohort,
      Grade = grade, 
      Age = age,
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
    select(Cohort, Grade, Age, Test, Model, Variable = term, 
           Estimate, SE, HR, CI_lower, CI_upper, p_value, N, Analysis)
  
  return(robust_results)

  }


#* 2.9. Run Cox Regression with Weights ----

# Documentation:
## function to run cox regression with clustered SEs
# Arguments: 
#   - cohort: SSB/CBS, 
#   - test: Math/Reading/Language/English,  
#   - data: popdata/sibdata,
#   - cluster_var: FID,
#   - analysis: Main/Adjusted
#   - is_sibdata: to filter to sample of sibs 

## function for cox regression model 
run_cox_clustered_weighted <- function(cohort, grade, age, test, model, data, 
                                       outcome, predictor, time_var, covariates, 
                                       cluster_var, weight_var, analysis, is_sibdata) 
  {
  
  # combine predictors + covariates
  all_terms <- c(predictor, covariates)
  
  # build formula
  formula_str <- paste0(
    "Surv(", time_var, ", ", outcome, ") ~",
    paste(all_terms, collapse = " + ")
  )
  model_formula <- as.formula(formula_str)
  
  # variables used in the model 
  vars_used <- unique(c(all.vars(model_formula)))
  
  # restrict sample to complete cases
  data_model <- data %>%
    filter(complete.cases(across(all_of(vars_used))))
  
  # if is_sibdata, filter to families with at least 2 siblings 
  if(is_sibdata) { 
    data_model <- data_model %>%
      group_by(.data[[cluster_var]]) %>%
      filter(n() >= 2) %>%
      ungroup()
  }
  
  # fit Cox model with clustering 
  diag_weights <- data_model[[weight_var]]
  cox_model <- coxph(model_formula,
                     data = data_model,
                     weights = diag_weights)
  
  # robust variance-covariance matrix
  cluster <- data_model[[cluster_var]]
  robust_vcov <- vcovCL(cox_model, cluster = cluster)
  
  # extract model summary with robust SEs
  tidy_out <- coeftest(cox_model, vcov. = robust_vcov)
  
  # no of observations in the model
  N = cox_model$n
  
  # output
  robust_results <- broom::tidy(tidy_out) %>%
    mutate(
      Cohort = cohort,
      Grade = grade, 
      Age = age,
      Test = test, 
      Model = model, 
      Variable = term,
      Estimate = round(estimate, 3),
      SE = round(std.error, 3),
      HR = round(exp(Estimate), 3),
      CI_lower = round(exp(Estimate - 1.96 * SE), 3),
      CI_upper = round(exp(Estimate + 1.96 * SE), 3),
      p_value = round(p.value, 4), 
      N = N, 
      Analysis = analysis
    ) %>%
    select(Cohort, Grade, Age, Test, Model, Variable, 
           Estimate, SE, HR, CI_lower, CI_upper, p_value, N, Analysis)
  
  return(robust_results)
  
}


#* 2.10. Non-linearity - Run Spline Cox Regression for Academic Performance - Bipolar Disorder Diagnosis ----

## Documentation:
#  Function to run cox regression with clustered SEs using assumptions of non-linearity
#
# Arguments: 
#   - data = name of the dataframe,  
#   - outcome: dichotmous variable,  
#   - cohort = the cohort/dataset,
#   - analysis: Main/Adjusted/etc.,
#   - is_sibdata: to filter to sample of sibs 

compare_cox_nonlinearity <- function(cohort, grade, test, data, outcome, predictor, 
                                     time_var, covariates, cluster_var, analysis, is_sibdata, spline_df = 4) { 
  
  # build main model formula
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
  
  
  # complete cases (same sample for both models)
  data_model <- data %>%
    filter(
      complete.cases(across(all_of(vars_used)))
    )
  
  # restrict to sibling families 
  if(is_sibdata) { 
    data_model <- data_model %>%
      group_by(.data[[cluster_var]]) %>%
      filter(n() >= 2) %>%
      ungroup()
  }
  
  # fit main model
  model_main <- coxph(
    formula_main, 
    data = data_model
  )
  
  # fit non-linear model
  model_nonlinear <- coxph(
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
    LR = comparison$'Chisq'[2],
    df = comparison$Df[2],
    p_value = comparison$'Pr(>|Chi|)'[2]
  )
  
  return(result)
}

