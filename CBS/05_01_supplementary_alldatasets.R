
#
# Script: 05_01_supplementary_alldatasets.R
# Project: Bipolar Disorder and Education:
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Script for the supplementary analysis of the association between academic performance and 
#                   bipolar disorder diagnosis using all datasets
#
# Author: Vaishnavi K Madhavan
# Start date: 13-02-2025
# 


# 1. Set Up ####

cito <- fread("./Data/02_Reference_Data/20260213_cito_scores.csv")
bipolar_diag <- fread("./Data/02_Reference_Data/20260521_bipolar_disorder_diagnosis.csv")
family_info <- fread("./Data/02_Reference_Data/20260521_family_info")


finaldata <- family_info %>%
  full_join(cito, by = "RINPERSOON") %>%
  full_join(bipolar_diag, by = "RINPERSOON")

nrow(finaldata) # 28,336,778
table(finaldata$GBAGEBOORTEJAAR)


# 2. Check and Edit ####

#* 2.1. Filter to those with all available data ----
finaldata <- finaldata %>%
  filter(!is.na(birthorder) & !is.na(GBAGESLACHT) & !is.na(GBAGEBOORTEJAAR)) 

nrow(finaldata) # 4,256,591

finaldata <- finaldata %>%
  filter(!is.na(CitoZScoreTaal) | !is.na(CitoZScoreRekenenWiskunde) | 
           !is.na(CitoZScoreStudievaardigh) | !is.na(std_CitoStandaardScore)) 

nrow(finaldata) # 1,887,597

sum(!is.na(finaldata$alldatasets_time)) # 1,887,597
sum(!is.na(finaldata$FID)) # 1,887,597
sum(!is.na(finaldata$birthorder)) # 1,887,597 (all have FID and birth order)


#* 2.2. Filter out those born after 2006 ----
## we only retain those who are at least 15 years old in December 2021 (which is the last year of diagnostic data available)
## this means those born <= 2006
table(finaldata$GBAGEBOORTEJAAR) # those with available cito scores and diagnosis

check_cases <- finaldata %>%
  filter(alldatasets_bipolar_diag == 1)
hist(check_cases$GBAGEBOORTEJAAR)
table(check_cases$GBAGEBOORTEJAAR) # not many cases after 2004
rm(check_cases)

## remove everyone born before 2004
finaldata <- finaldata %>%
  filter(GBAGEBOORTEJAAR <= 2003)
nrow(finaldata) # 1,434,003


#* 2.3. Changing time into a numeric variable (to avoid issues with cox regression) ----
finaldata$alldatasets_time <- as.numeric(finaldata$alldatasets_time)
finaldata$bipolar_diag <- as.numeric(finaldata$bipolar_diag)


#* 2.4. Check age at diagnosis ----
table(finaldata$alldatasets_age_diag) # around 200 before the age of 15
table(finaldata$alldatasets_time) # same as above

## check those diagnosed before 18
check_18 <- finaldata %>%
  filter(alldatasets_age_diag < 18) # 125 individuals 
table(finaldata$alldatasets_bipolar_diag) # out of 1769 (seems like a lot) 
table(check_18$GBAGEBOORTEJAAR) 

check_15 <- finaldata %>%
  filter(alldatasets_age_diag < 15) # 25 individuals 
table(finaldata$alldatasets_bipolar_diag) # out of 1769   
table(check_15$GBAGEBOORTEJAAR) 
rm(check_18, check_15)

## the date of diagnosis in these datasets are the start of the care trajectory and not the actual date of diagnosis. 
## I was told by the dataset experts that the date of diagnosis can be later than the start of the care trajectory (impossible to know when)
## I retain all since I cannot filter them properly

nrow(finaldata) # 1,434,003
table(finaldata$alldatasets_bipolar_diag) # 1769 cases (0.12%)
finaldata$bipolar_diag_weights <- as.numeric(finaldata$bipolar_diag_weights)
table(finaldata$bipolar_diag_weights) # 0.09 to 1


# 2. Form Population and Sibling Samples ###

## population-level sample
popdata <- finaldata %>%
  filter(!is.na(CitoZScoreTaal) | !is.na(CitoZScoreRekenenWiskunde) | !is.na(CitoZScoreStudievaardigh) |
           !is.na(std_CitoStandaardScore))

nrow(popdata) # 1,434,003

## check birth year after filtering out on available data 
describe(finaldata$GBAGEBOORTEJAAR)

## sibling sample
sibdata <- popdata %>% 
  group_by(FID) %>% filter(n() >= 2) %>% ungroup()

nrow(sibdata) # 856,577

## creating between and within variables 
## looping through all grades
score_vars <- c("CitoZScoreTaal", "CitoZScoreRekenenWiskunde", "CitoZScoreStudievaardigh", "std_CitoStandaardScore")

## apply to popdata and sibdata
for (var in score_vars) {
  sibdata <- add_between_within(sibdata, var, family_id = "FID")
}

write.csv(popdata, "./Data/03_Final_Data/20260717_popdata_alldatasets.csv", row.names=FALSE)
write.csv(sibdata, "./Data/03_Final_Data/20260717_sibdata_alldatasets.csv", row.names=FALSE)


# 3. Descriptive Analyses  ####
# popdata <- fread("./Data/03_Final_Data/20260717_popdata_alldatasets.csv")
# sibdata <- fread("./Data/03_Final_Data/20260717_sibdata_alldatasets.csv")

datasets <- list(
  Population_Total_Sample = popdata,
  Siblings_Total_Sample = sibdata
)

summaries <- lapply(datasets, function(x) { 
  create_summary(
    data = x,
    continuous = c("GBAGEBOORTEJAAR", "Fsize", "birthorder", "TestYear",
                   "CitoScoreTaal", "CitoScoreRekenenWiskunde", "CitoScoreStudieVaardigheden", "CitoStandaardScore",
                   "alldatasets_age_diag"),
    categorical = c("GBAGESLACHT", "alldatasets_bipolar_diag", "bipolar_diag_cutoff_6", "bipolar_diag_cutoff_7", 
                    "bipolar_diag_cutoff_8", "bipolar_diag_cutoff_9"),
    FID = "FID",
    digits = 2
  )})


## edit the table
## NOTE: hard-coding this based on the summary column because no other way 
pop_summary <- summaries[["Population_Total_Sample"]] %>%
  mutate(Variable = case_when(
    Variable == "Sample Size" ~ "Sample Size", 
    Variable == "Number of Families" ~ "Number of Families", 
    Variable == "" & Level == "2" ~ "Sex (Females)",
    Variable == "GBAGESLACHT" & Level == "1" ~ "Sex (Males)",
    Variable == "GBAGEBOORTEJAAR" ~ "Birth Year", 
    Variable == "Fsize" ~ "Family Size", 
    Variable == "birthorder" ~ "Birth Order", 
    Variable == "TestYear" ~ "Year of Test", 
    Variable == "CitoStandaardScore" ~ "Total Score", 
    Variable == "CitoScoreRekenenWiskunde" ~ "Math Score", 
    Variable == "CitoScoreStudieVaardigheden" ~ "Reading Score", 
    Variable == "CitoScoreTaal" ~ "Language Score",
    Variable == "alldatasets_age_diag" ~ "Age at Diagnosis",
    Variable == "alldatasets_bipolar_diag" ~ "Bipolar Disorder Controls - All Datasets, n(%)",
    Summary == "1769 (0.12%)" ~ "Bipolar Disorder Cases - All Datasets, n(%)",
    Variable == "bipolar_diag_cutoff_6" ~ "Bipolar Disorder Controls - Cutoff 0.6, n(%)",
    Summary == "1005 (0.07%)" ~ "Bipolar Disorder Cases - Cutoff 0.6, n(%)",
    Variable == "bipolar_diag_cutoff_7" ~ "Bipolar Disorder Controls - Cutoff 0.7, n(%)",
    Summary == "992 (0.07%)" ~ "Bipolar Disorder Cases - Cutoff 0.7, n(%)",
    Variable == "bipolar_diag_cutoff_8" ~ "Bipolar Disorder Controls - Cutoff 0.8, n(%)",
    Summary == "916 (0.06%)" ~ "Bipolar Disorder Cases - Cutoff 0.8, n(%)",
    Variable == "bipolar_diag_cutoff_9" ~ "Bipolar Disorder Controls - Cutoff 0.9, n(%)",
    Summary == "538 (0.04%)" ~ "Bipolar Disorder Cases - Cutoff 0.9, n(%)"
  )) %>%
  select(-Level) 

colnames(pop_summary) <- c("Variable", "Population_Total_Sample")

sib_summary <- summaries[["Siblings_Total_Sample"]] %>%
  mutate(Variable = case_when(
    Variable == "Sample Size" ~ "Sample Size", 
    Variable == "Number of Families" ~ "Number of Families", 
    Variable == "" & Level == "2" ~ "Sex (Females)",
    Variable == "GBAGESLACHT" & Level == "1" ~ "Sex (Males)",
    Variable == "GBAGEBOORTEJAAR" ~ "Birth Year", 
    Variable == "Fsize" ~ "Family Size", 
    Variable == "birthorder" ~ "Birth Order", 
    Variable == "TestYear" ~ "Year of Test", 
    Variable == "CitoStandaardScore" ~ "Total Score", 
    Variable == "CitoScoreRekenenWiskunde" ~ "Math Score", 
    Variable == "CitoScoreStudieVaardigheden" ~ "Reading Score", 
    Variable == "CitoScoreTaal" ~ "Language Score",
    Variable == "alldatasets_age_diag" ~ "Age at Diagnosis",
    Variable == "alldatasets_bipolar_diag" ~ "Bipolar Disorder Controls - All Datasets, n(%)",
    Summary == "1019 (0.12%)" ~ "Bipolar Disorder Cases - All Datasets, n(%)",
    Variable == "bipolar_diag_cutoff_6" ~ "Bipolar Disorder Controls - Cutoff 0.6, n(%)",
    Summary == "564 (0.07%)" ~ "Bipolar Disorder Cases - Cutoff 0.6, n(%)",
    Variable == "bipolar_diag_cutoff_7" ~ "Bipolar Disorder Controls - Cutoff 0.7, n(%)",
    Summary == "554 (0.06%)" ~ "Bipolar Disorder Cases - Cutoff 0.7, n(%)",
    Variable == "bipolar_diag_cutoff_8" ~ "Bipolar Disorder Controls - Cutoff 0.8, n(%)",
    Summary == "511 (0.06%)" ~ "Bipolar Disorder Cases - Cutoff 0.8, n(%)",
    Variable == "bipolar_diag_cutoff_9" ~ "Bipolar Disorder Controls - Cutoff 0.9, n(%)",
    Summary == "281 (0.03%)" ~ "Bipolar Disorder Cases - Cutoff 0.9, n(%)"
  )) %>%
  select(-Level) 

colnames(sib_summary) <- c("Variable", "Siblings_Total_Sample")
descriptive_table <- pop_summary %>%
  left_join(sib_summary, by = "Variable")

write.csv(descriptive_table, "./Results/Supplementary_Results/AllDatasets_Descriptive_Table.csv", row.names=FALSE)


# 4. Analyses ####
# popdata <- fread("./Data/03_Final_Data/20260521_popdata.csv")
# sibdata <- fread("./Data/03_Final_Data/20260521_sibdata.csv")

#* 4.1. All Datasets ----

## initialize list to store results
all_results <- list()

# define predictor variable 
tests <- c("CitoZScoreTaal", "CitoZScoreRekenenWiskunde", "CitoZScoreStudievaardigh", "std_CitoStandaardScore")

## loop through each grade
for (test in tests) {
  
  test_label <- ifelse(test == "std_CitoStandaardScore", "Total Score",
                       ifelse(test == "CitoZScoreRekenenWiskunde", "Math", 
                              ifelse(test == "CitoZScoreStudievaardigh", "Reading", 
                                     ifelse(test == "CitoZScoreTaal", "Language", NA))))
  
  # define within/between variables
  bw_vars <- c(paste0(test, "_between"), paste0(test, "_within"))
  
  # run population model on popdata
  alldatasets_pop <- run_cox_clustered(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label, 
    model = "Population",
    data = popdata,
    outcome = "alldatasets_bipolar_diag",
    predictor = test,
    time_var = "alldatasets_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    analysis = "Sensitivity - All Datasets",
    is_sibdata = FALSE
  )
  
  # run between-within model on sibdata
  alldatasets_bw <- run_cox_clustered(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label, 
    model = "Sibling",
    data = sibdata,
    outcome = "alldatasets_bipolar_diag",
    predictor = bw_vars,
    time_var = "alldatasets_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    analysis = "Sensitivity - All Datasets",
    is_sibdata = TRUE
  )
  
  # combine all results for this grade
  all_results[[test]] <- bind_rows(alldatasets_pop, alldatasets_bw)
}

## combine across all grades
alldatasets_results <- bind_rows(all_results)


#* 4.2. All Datasets - Weighted ----

## editing the weights variable (the ones who have NA for weights are 1 - since they are controls for sure)
sum(!is.na(popdata$bipolar_diag_weights)) # only 1744 people have weights 

check <- popdata %>%
  filter(is.na(bipolar_diag_weights)) # check those without a weight (n=1,415,160)

table(check$detailed_bipolar_diag)
table(check$alldatasets_bipolar_diag)
## all of them are controls 

popdata$bipolar_diag_weights[is.na(popdata$bipolar_diag_weights)] <- 1
sum(!is.na(popdata$bipolar_diag_weights)) # 1416904
nrow(popdata) # 1416904

sum(!is.na(sibdata$bipolar_diag_weights)) # 1005
sibdata$bipolar_diag_weights[is.na(sibdata$bipolar_diag_weights)] <- 1
sum(!is.na(sibdata$bipolar_diag_weights)) # 846,226
nrow(sibdata) # 846,226

## initialize list to store results
all_results <- list()

# define predictor variable 
tests <- c("CitoZScoreTaal", "CitoZScoreRekenenWiskunde", "CitoZScoreStudievaardigh", "std_CitoStandaardScore")

## loop through each grade
for (test in tests) {
  
  test_label <- ifelse(test == "std_CitoStandaardScore", "Total Score",
                       ifelse(test == "CitoZScoreRekenenWiskunde", "Math", 
                              ifelse(test == "CitoZScoreStudievaardigh", "Reading", 
                                     ifelse(test == "CitoZScoreTaal", "Language", NA))))
  
  # define within/between variables
  bw_vars <- c(paste0(test, "_between"), paste0(test, "_within"))
  
  # run population model on popdata
  alldatasets_weighted_pop <- run_cox_clustered_weighted(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label, 
    model = "Population",
    data = popdata,
    outcome = "alldatasets_bipolar_diag",
    predictor = test,
    time_var = "alldatasets_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    weight_var = "bipolar_diag_weights",
    analysis = "Sensitivity - All Datasets Weighted",
    is_sibdata = FALSE
  )
  
  # run between-within model on sibdata
  alldatasets_weighted_bw <- run_cox_clustered_weighted(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label,
    model = "Sibling",
    data = sibdata,
    outcome = "alldatasets_bipolar_diag",
    predictor = bw_vars,
    time_var = "alldatasets_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    weight_var = "bipolar_diag_weights",
    analysis = "Sensitivity - All Datasets Weighted",
    is_sibdata = TRUE
  )
  
  # combine all results for this grade
  all_results[[test]] <- bind_rows(alldatasets_weighted_pop, alldatasets_weighted_bw)
}

## combine across all grades
alldatasets_results_weighted <- bind_rows(all_results)


#* 4.3. All Datasets - Cutoffs ----

# define predictor variable 
tests <- c( "std_CitoStandaardScore", "CitoZScoreRekenenWiskunde", "CitoZScoreStudievaardigh", "CitoZScoreTaal")


#** 4.3.1. CutOff 0.6 ----

## initialize list to store results
all_results <- list()

## loop through each grade
for (test in tests) {
  
  test_label <- ifelse(test == "std_CitoStandaardScore", "Total Score",
                       ifelse(test == "CitoZScoreRekenenWiskunde", "Math", 
                              ifelse(test == "CitoZScoreStudievaardigh", "Reading", 
                                     ifelse(test == "CitoZScoreTaal", "Language", NA))))
  
  # define within/between variables
  bw_vars <- c(paste0(test, "_between"), paste0(test, "_within"))
  
  # run population model on popdata
  cutoff6_pop <- run_cox_clustered(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label, 
    model = "Population",
    data = popdata,
    outcome = "bipolar_diag_cutoff_6",
    predictor = test,
    time_var = "detailed_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    analysis = "Sensitivity - All Datasets Cutoff 0.6",
    is_sibdata = FALSE
  )
  
  # run between-within model on sibdata
  cutoff6_bw <- run_cox_clustered(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label, 
    model = "Sibling",
    data = sibdata,
    outcome = "bipolar_diag_cutoff_6",
    predictor = bw_vars,
    time_var = "alldatasets_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    analysis = "Sensitivity - All Datasets Cutoff 0.6",
    is_sibdata = TRUE
  )
  
  
  # combine all results for this grade
  all_results[[test]] <- bind_rows(cutoff6_pop, cutoff6_bw)
}

## combine across all grades
alldatasets_results_6 <- bind_rows(all_results)


#** 4.3.2. CutOff 0.7 ----

## initialize list to store results
all_results <- list()

# define predictor variable 
tests <- c("CitoZScoreTaal", "CitoZScoreRekenenWiskunde", "CitoZScoreStudievaardigh", "std_CitoStandaardScore")

## loop through each grade
for (test in tests) {
  
  test_label <- ifelse(test == "std_CitoStandaardScore", "Total Score",
                       ifelse(test == "CitoZScoreRekenenWiskunde", "Math", 
                              ifelse(test == "CitoZScoreStudievaardigh", "Reading", 
                                     ifelse(test == "CitoZScoreTaal", "Language", NA))))
  
  # define within/between variables
  bw_vars <- c(paste0(test, "_between"), paste0(test, "_within"))
  
  # run population model on popdata
  cutoff7_pop <- run_cox_clustered(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label, 
    model = "Population",
    data = popdata,
    outcome = "bipolar_diag_cutoff_7",
    predictor = test,
    time_var = "alldatasets_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    analysis = "Sensitivity - All Datasets Cutoff 0.7",
    is_sibdata = FALSE
  )
  
  # run between-within model on sibdata
  cutoff7_bw <- run_cox_clustered(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label, 
    model = "Sibling",
    data = sibdata,
    outcome = "bipolar_diag_cutoff_7",
    predictor = bw_vars,
    time_var = "alldatasets_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    analysis = "Sensitivity - All Datasets Cutoff 0.7",
    is_sibdata = TRUE
  )
  
  # combine all results for this grade
  all_results[[test]] <- bind_rows(cutoff7_pop, cutoff7_bw)
}

## combine across all grades
alldatasets_results_7 <- bind_rows(all_results)


#** 4.3.3. CutOff 0.8 ----

## initialize list to store results
all_results <- list()

# define predictor variable 
tests <- c("CitoZScoreTaal", "CitoZScoreRekenenWiskunde", "CitoZScoreStudievaardigh", "std_CitoStandaardScore")

## loop through each grade
for (test in tests) {
  
  test_label <- ifelse(test == "std_CitoStandaardScore", "Total Score",
                       ifelse(test == "CitoZScoreRekenenWiskunde", "Math", 
                              ifelse(test == "CitoZScoreStudievaardigh", "Reading", 
                                     ifelse(test == "CitoZScoreTaal", "Language", NA))))
  
  # define within/between variables
  bw_vars <- c(paste0(test, "_between"), paste0(test, "_within"))
  
  # run population model on popdata
  cutoff8_pop <- run_cox_clustered(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label, 
    model = "Population",
    data = popdata,
    outcome = "bipolar_diag_cutoff_8",
    predictor = test,
    time_var = "alldatasets_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    analysis = "Sensitivity - All Datasets Cutoff 0.8",
    is_sibdata = FALSE
  )
  
  # run between-within model on sibdata
  cutoff8_bw <- run_cox_clustered(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label, 
    model = "Sibling",
    data = sibdata,
    outcome = "bipolar_diag_cutoff_8",
    predictor = bw_vars,
    time_var = "alldatasets_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    analysis = "Sensitivity - All Datasets Cutoff 0.8",
    is_sibdata = TRUE
  )
  
  
  # combine all results for this grade
  all_results[[test]] <- bind_rows(cutoff8_pop, cutoff8_bw)
}

## combine across all grades
alldatasets_results_8 <- bind_rows(all_results)


#** 4.3.4. CutOff 0.9 ----

## initialize list to store results
all_results <- list()

# define predictor variable 
tests <- c("CitoZScoreTaal", "CitoZScoreRekenenWiskunde", "CitoZScoreStudievaardigh", "std_CitoStandaardScore")

## loop through each grade
for (test in tests) {
  
  test_label <- ifelse(test == "std_CitoStandaardScore", "Total Score",
                       ifelse(test == "CitoZScoreRekenenWiskunde", "Math", 
                              ifelse(test == "CitoZScoreStudievaardigh", "Reading", 
                                     ifelse(test == "CitoZScoreTaal", "Language", NA))))
  
  # define within/between variables
  bw_vars <- c(paste0(test, "_between"), paste0(test, "_within"))
  
  # run population model on popdata
  cutoff9_pop <- run_cox_clustered(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label, 
    model = "Population",
    data = popdata,
    outcome = "bipolar_diag_cutoff_9",
    predictor = test,
    time_var = "alldatasets_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    analysis = "Sensitivity - All Datasets Cutoff 0.9",
    is_sibdata = FALSE
  )
  
  # run between-within model on sibdata
  cutoff9_bw <- run_cox_clustered(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label, 
    model = "Sibling",
    data = sibdata,
    outcome = "bipolar_diag_cutoff_9",
    predictor = bw_vars,
    time_var = "alldatasets_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    analysis = "Sensitivity - All Datasets Cutoff 0.9",
    is_sibdata = TRUE
  )
  
  # combine all results for this grade
  all_results[[test]] <- bind_rows(cutoff9_pop, cutoff9_bw)
}

## combine across all grades
alldatasets_results_9 <- bind_rows(all_results)


#* 4.4. Combine Results ----

all_sensitivity_results <- rbind(alldatasets_results, alldatasets_results_weighted, 
                            alldatasets_results_6, alldatasets_results_7, 
                            alldatasets_results_8, alldatasets_results_9)

write.csv(all_sensitivity_results, "./Results/Supplementary_Results/AllDatasets_Results.csv", row.names = F)  


# 5. Figures ####
#all_sensitivity_results <- read.csv("./Results/Supplementary_Results/AllDatasets_Results.csv")

plot1 <- all_sensitivity_results %>%
  filter(grepl("^Cito|^std", Variable)) %>%
  
  mutate(
    Test_Label = case_when(
      str_detect(Variable, "^CitoZScoreTaal") ~ "Language",
      str_detect(Variable, "^CitoZScoreRekenenWiskunde") ~ "Math",
      str_detect(Variable, "CitoZScoreStudievaardigh") ~ "Reading",
      str_detect(Variable, "std_CitoStandaardScore") ~ "Total Score",   
  )) %>%
  
  # identify 'between' or 'within' from the variable name
  mutate(
    Estimate_Label = case_when(
      str_detect(Variable, "_between$") ~ "Between-sibship",
      str_detect(Variable, "_within$") ~ "Within-sibship",
      str_detect(Model, "Population") ~ "Population"
    ),
    
    # factor levels for consistent ordering
    Estimate_Label = factor(
      Estimate_Label,
      levels = c(
        "Within-sibship",
        "Between-sibship",
        "Population"
      )
    ),
    
    Test_Label = factor(Test_Label, levels = c("Total Score", 
                                               "Math",
                                               "Reading",
                                               "Language")),
    
    # significance level
    Significance = p_value < 0.05) 


## ggplot 
alldatasets_plot <- ggplot(plot1, aes(
  x = HR,
  y = Estimate_Label,
  xmin = CI_lower,
  xmax = CI_upper,
  shape = Estimate_Label,
  color = Estimate_Label,
  alpha = Significance
)) +
  
  # plot Elements: CI Bars, Points, and Reference Line
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.5) +  # estimate points and CI
  geom_errorbarh(position = position_dodge2(width = 0.5, padding = 0.5), width = 0.2) +  # error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +  # reference line for HR = 1
  
  # faceting by 'Test_Label' 
  facet_grid(rows = vars(Analysis), cols = vars(Test_Label), scales = "free_y", space = "free", switch = "y") +
  
  # alpha mapping: fade non-significant estimates
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5)) +
  guides(alpha = "none") +  # remove alpha legend
  
  # X-axis formatting
  scale_x_continuous(breaks = seq(-0.4, 1.6, by = 0.2)) +
  
  # color mapping: estimate type colors 
  scale_color_manual(
    values = c(
      "Population"       = "royalblue",
      "Between-sibship"     = "#CC79A7",
      "Within-sibship"      = "#009E73"
    ),
    breaks = c(
      "Population Estimate",
      "Between-sibship Estimate",
      "Within-sibship Estimate"
    ),
    name = "Estimate Type"
  ) +
  
  # shape mapping: estimate type symbols
  scale_shape_manual(
    values = c(
      "Population"       = 4,  # diamond
      "Between-sibship"     = 16, # circle
      "Within-sibship"      = 16
    ),
    breaks = c(
      "Population",
      "Between-sibship",
      "Within-sibship"
    ),
    name = "Estimate Type"
  ) +
  
  # axis and title labels
  labs(
    x = "Hazard Ratio (HR)",
    y = "",
    color = "Estimate Type",
    shape = "Estimate Type",
    title = "CITO Test Scores at Age 12 and Bipolar Disorder Diagnosis\n(All Datasets)"
  ) +
  
  # theme and final formatting
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 12, face = "bold", margin = margin(b = 2, t = 2)),
    strip.text.y.left = element_text(size = 9, angle = 0, hjust = 0, margin = margin(0, 10, 0, 10)),
    strip.background = element_rect(color = "black", fill = "white"),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_blank(),           # remove y-axis text
    axis.ticks.y = element_blank(),          # remove y-axis ticks
    axis.text.x = element_text(size = 8),    # format x-axis text
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical"
  )

alldatasets_plot

ggsave("./Results/Figures/AllDatasets_Supplementary_Results.pdf", plot = alldatasets_plot, width = 15, height = 5, dpi = 300)

