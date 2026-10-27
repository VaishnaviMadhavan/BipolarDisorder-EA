
# 
# Script: 06_02_analysis_academicperformance_diagnosis.R 
# Project: Bipolar Disorder and Education: 
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Analysis of academic performance and bipolar disorder diagnosis
#
# Author: Vaishnavi K Madhavan
# Start date: 01-05-2025
# 

## analysis steps
# 1. descriptive analysis
# 2. population-based model with total sample 
# 3. between-within sibship model with sibling sample 
# 4. supplementary Analyses 


# 1. Set Up  ####

#* 1.1. Forming the base datasets ----

pop_sibinfo_covariates <- fread("./Data/Reference Data/pop_sibinfo_covariates.csv") %>%
  select(w19_0634_lnr, mother_lnr, parents, sex, birthyear, birthorder, deathyear, valid_sibs) 

nat_test_scores <- fread("./Data/Reference Data/nat_test_scores.csv") %>%
  select(w19_0634_lnr, 
         score_NPLES05, score_NPREG05, score_NPENG05,
         std_score_NPLES05, std_score_NPREG05, std_score_NPENG05, std_score_agg5,
         score_NPLES08, score_NPREG08, score_NPENG08, 
         std_score_NPLES08, std_score_NPREG08, std_score_NPENG08, std_score_agg8,
         score_NPLES09, score_NPREG09, 
         std_score_NPLES09, std_score_NPREG09, std_score_agg9)

bipolar_diag <- fread("./Data/Reference Data/bipolar_disorder_diagnosis.csv") %>%
  select(w19_0634_lnr, bipolar_diag, age_diag, age_censor, time)

finaldata <- pop_sibinfo_covariates %>%
  left_join(nat_test_scores, by = "w19_0634_lnr") %>%
  left_join(bipolar_diag, by = "w19_0634_lnr")


#* 1.2. Filtering ----

## filtering out those born before 1993 (the first individuals for whom the national test scores are available in 2007) and after 2010 (at least 15 years old at the time of last follow-up) 
describe(as.numeric(finaldata$birthyear)) # 1896 to 2024
nrow(finaldata) # 5,038,126

## first filter based on available sociodemographic info 
finaldata <- finaldata %>%
  filter(!is.na(sex) & 
           !is.na(birthyear) &
           !is.na(birthorder))

nrow(finaldata) #5,038,126

finaldata <- finaldata %>%
  filter(!is.na(std_score_NPLES05) | !is.na(std_score_NPREG05) | !is.na(std_score_NPENG05) | !is.na(std_score_agg5) |
         !is.na(std_score_NPLES08) | !is.na(std_score_NPREG08) | !is.na(std_score_NPENG08) | !is.na(std_score_agg8) |
         !is.na(std_score_NPLES09) | !is.na(std_score_NPREG09) | !is.na(std_score_agg9)) %>%
  filter(!is.na(time))
nrow(finaldata) # 1,256,400

## check and filter based on birthyear
finaldata$birthyear <- as.numeric(finaldata$birthyear)
table(finaldata$birthyear) # 1914 to 2017 
table(finaldata$birthyear, finaldata$bipolar_diag) # 1993 to 2010 with valid diagnosis

finaldata <- finaldata %>%
  filter(birthyear >= 1993 & birthyear <= 2010)
nrow(finaldata) # 1,026,025
table(finaldata$birthyear) # looks good

## check other variables
table(finaldata$bipolar_diag) # 4995  cases

table(finaldata$time) # to remove the individual with 0
check <- finaldata %>% filter(time == 0)
## says they died when they were born, but have test scores. to remove
rm(check)
finaldata <- finaldata %>%
  filter(time > 0)
nrow(finaldata) #1,026,024

## changing time and bipolar_diag into a numeric variable (to avoid issues with cox regression)
finaldata$time <- as.numeric(finaldata$time)
finaldata$bipolar_diag <- as.numeric(finaldata$bipolar_diag)


#* 1.3. Form Population and Sibling Samples ----

## population-level sample
popdata <- finaldata

nrow(popdata) # 1,026,024

check_g5 <- popdata %>%
  filter(!is.na(std_score_NPLES05) | !is.na(std_score_NPREG05) | !is.na(std_score_NPENG05) | !is.na(std_score_agg5))

check_g8 <- popdata %>%
  filter(!is.na(std_score_NPLES08) | !is.na(std_score_NPREG08) | !is.na(std_score_NPENG08) | !is.na(std_score_agg8))

check_g9 <- popdata %>%
  filter(!is.na(std_score_NPLES09) | !is.na(std_score_NPREG09) | !is.na(std_score_agg9))

length(unique(c(check_g5$w19_0634_lnr, check_g8$w19_0634_lnr, check_g9$w19_0634_lnr))) # 1025344

rm(check_g5, check_g8, check_g9)

## sibling sample 
sibdata <- popdata %>% 
  filter(valid_sibs == 1) %>%
  group_by(parents) %>% filter(n() >= 2) %>% ungroup() 

nrow(sibdata) # 499,199 
rm(finaldata)

## each subset needs to have complete set of families
sibdata_main <- sibdata %>%
  select(w19_0634_lnr, parents, mother_lnr, bipolar_diag, 
         time, age_diag, age_censor, sex, birthyear, birthorder)
describe(sibdata_main) # 499,199         

sibdata_g5 <- sibdata %>%
  filter(!is.na(std_score_NPLES05) | !is.na(std_score_NPREG05) | !is.na(std_score_NPENG05) | !is.na(std_score_agg5)) %>%
  filter(!is.na(parents)) %>%
  group_by(parents) %>% filter(n() >= 2) %>% ungroup() %>%
  select(w19_0634_lnr, 
         score_NPLES05, score_NPREG05, score_NPENG05,
         std_score_NPLES05, std_score_NPREG05, std_score_NPENG05, 
         std_score_agg5)
nrow(sibdata_g5) # 369016

sibdata_g8 <- sibdata %>%
  filter(!is.na(std_score_NPLES08) | !is.na(std_score_NPREG08) | !is.na(std_score_NPENG08) | !is.na(std_score_agg8)) %>%
  filter(!is.na(parents)) %>%
  group_by(parents) %>% filter(n() >= 2) %>% ungroup() %>%
  select(w19_0634_lnr, 
         score_NPLES08, score_NPREG08, score_NPENG08,
         std_score_NPLES08, std_score_NPREG08, std_score_NPENG08, std_score_agg8)
nrow(sibdata_g8) # 483156

sibdata_g9 <- sibdata %>%
  filter(!is.na(std_score_NPREG09) | !is.na(std_score_NPLES09) | !is.na(std_score_agg9)) %>%
  filter(!is.na(parents)) %>%
  group_by(parents) %>% filter(n() >= 2) %>% ungroup() %>%
  select(w19_0634_lnr, 
         score_NPREG09, score_NPLES09, 
         std_score_NPREG09, std_score_NPLES09, std_score_agg9)
nrow(sibdata_g9) # 409138

sibdata <- sibdata_g5 %>%
  full_join(sibdata_g8, by = "w19_0634_lnr") %>%
  full_join(sibdata_g9, by = "w19_0634_lnr") %>%
  left_join(sibdata_main, by = "w19_0634_lnr")

nrow(sibdata) # 496969 (this is the final n but each model has only a subset of this)

length(unique(c(sibdata_g5$w19_0634_lnr, sibdata_g8$w19_0634_lnr, 
                sibdata_g9$w19_0634_lnr))) # 496969 


## creating between and within variables 
## looping through all grades
score_vars <- c(
  "std_score_agg5", "std_score_agg8", "std_score_agg9",
  "std_score_NPREG05", "std_score_NPLES05", "std_score_NPENG05",
  "std_score_NPREG08", "std_score_NPLES08", "std_score_NPENG08",
  "std_score_NPREG09", "std_score_NPLES09"
)

## Apply to popdata and sibdata
for (var in score_vars) {
  sibdata <- create_between_within(sibdata, var, family_id = "parents")
}

write.csv(popdata, "./Data/Final Data/academicperformance_diagnosis_popdata.csv", row.names=FALSE)
write.csv(sibdata, "./Data/Final Data/academicperformance_diagnosis_sibdata.csv", row.names=FALSE)


# 2. Descriptive Analysis ####
# popdata <- fread("./Data/Final Data/academicperformance_diagnosis_popdata.csv")
# sibdata <- fread("./Data/Final Data/academicperformance_diagnosis_sibdata.csv")

#* 2.1. Creating datasets for grade 5, 8, and 9 separately ----

## population data 
pop_split_by_grade <- function(data) {
  data_g5 <- data %>%
    filter(!is.na(std_score_agg5) | !is.na(score_NPLES05) | !is.na(score_NPREG05) | !is.na(score_NPENG05)) %>%
    select(w19_0634_lnr, bipolar_diag, age_diag, time,
           score_NPENG05, score_NPREG05, score_NPLES05,
           std_score_NPENG05, std_score_NPREG05, std_score_NPLES05, std_score_agg5,
           sex, birthyear, birthorder, parents) 
  
  data_g8 <- data %>%
    filter(!is.na(std_score_agg8) | !is.na(score_NPLES08) | !is.na(score_NPREG08) | !is.na(score_NPENG08)) %>%
    select(w19_0634_lnr, bipolar_diag, age_diag, time,
           score_NPENG08, score_NPREG08, score_NPLES08,
           std_score_NPENG08, std_score_NPREG08, std_score_NPLES08, std_score_agg8,
           sex, birthyear, birthorder, parents) 
  
  data_g9 <- data %>%
    filter(!is.na(std_score_agg9) | !is.na(score_NPLES09) | !is.na(score_NPREG09)) %>% 
    select(w19_0634_lnr, bipolar_diag, age_diag, time,
           score_NPREG09, score_NPLES09,
           std_score_NPREG09, std_score_NPLES09, std_score_agg9,
           sex, birthyear, birthorder, parents)
  
  list(
    grade5 = data_g5,
    grade8 = data_g8,
    grade9 = data_g9
  )
}

## apply to popdata
popdata_split <- pop_split_by_grade(popdata)
popdata_g5 <- popdata_split$grade5
popdata_g8 <- popdata_split$grade8
popdata_g9 <- popdata_split$grade9

nrow(popdata_g5) # 807240
nrow(popdata_g8) # 1001462
nrow(popdata_g9) # 874986

## sibling data 
sib_split_by_grade <- function(data) {
  data_g5 <- data %>%
    filter(!is.na(std_score_agg5) | !is.na(score_NPLES05) | !is.na(score_NPREG05) | !is.na(score_NPENG05)) %>%
    filter(!is.na(parents)) %>%
    group_by(parents) %>% filter(n() >= 2) %>% ungroup()  %>%
    select(w19_0634_lnr, bipolar_diag, age_diag, time, 
           score_NPENG05, score_NPREG05, score_NPLES05,
           std_score_NPENG05, std_score_NPREG05, std_score_NPLES05, std_score_agg5,
           std_score_agg5_between, std_score_agg5_within,
           sex, birthyear, birthorder, parents)
  
  data_g8 <- data %>%
    filter(!is.na(std_score_agg8) | !is.na(score_NPLES08) | !is.na(score_NPREG08) | !is.na(score_NPENG08)) %>%
    filter(!is.na(parents)) %>%
    group_by(parents) %>% filter(n() >= 2) %>% ungroup() %>%
    select(w19_0634_lnr, bipolar_diag, age_diag, time,
           score_NPENG08, score_NPREG08, score_NPLES08, 
           std_score_NPENG08, std_score_NPREG08, std_score_NPLES08, std_score_agg8, 
           std_score_agg8_between, std_score_agg8_within,
           sex, birthyear, birthorder, parents)  
  
  data_g9 <- data %>%
    filter(!is.na(std_score_agg9) | !is.na(score_NPLES08) | !is.na(score_NPREG08)) %>%
    filter(!is.na(parents)) %>%
    group_by(parents) %>% filter(n() >= 2) %>% ungroup()  %>%
    select(w19_0634_lnr, bipolar_diag, age_diag, time,
           score_NPREG09, score_NPLES09, 
           std_score_NPREG09, std_score_NPLES09, std_score_agg9,
           std_score_agg9_between, std_score_agg9_within,
           sex, birthyear, birthorder, parents)  
  
  list(
    grade5 = data_g5,
    grade8 = data_g8,
    grade9 = data_g9
  )
}

## apply to sibdata
sibdata_split <- sib_split_by_grade(sibdata)

sibdata_g5 <- sibdata_split$grade5 
sibdata_g8 <- sibdata_split$grade8 
sibdata_g9 <- sibdata_split$grade9 

nrow(sibdata_g5) # 369016
nrow(sibdata_g8) # 483156
nrow(sibdata_g9) # 487698

## check manually 
nrow(sibdata_g5) # 369016
check <- popdata %>%
  filter(!is.na(std_score_agg5) | !is.na(score_NPLES05) | !is.na(score_NPREG05) | !is.na(score_NPENG05)) %>%
  filter(!is.na(parents)) %>%
  filter(valid_sibs ==1) %>%
  group_by(parents) %>% filter(n() >= 2) %>% ungroup()
nrow(check) # 369016 (checks out)
rm(check)


#* 2.2. Extracting Sample Descriptive Data ----

datasets <- list(
  Population_Total_Sample = popdata, 
  Siblings_Total_Sample = sibdata,
  Population_Grade5 = popdata_g5, 
  Siblings_Grade5 = sibdata_g5, 
  Population_Grade8  = popdata_g8, 
  Siblings_Grade8 = sibdata_g8,
  Population_Grade9  = popdata_g9, 
  Siblings_Grade9 = sibdata_g9
)

summaries <- lapply(datasets, function(x) { 
  create_summary(
    data = x,
    continuous = c("birthyear", 
                   "score_NPREG05", "score_NPLES05", "score_NPENG05",
                   "score_NPREG08", "score_NPLES08", "score_NPENG08",
                   "score_NPREG09", "score_NPLES09",
                   "age_diag"),
    categorical = c("sex", "bipolar_diag"),
    FID = c("parents"),
    digits = 2
  )
})

## edit the dataset
edit_summary <- function(df, dataset_name) { 
  
  df %>% 
    mutate(
      Variable = case_when(
        Variable == "" & Level == "2" ~ "Sex (Females)",
        Variable == "sex" & Level == "1" ~ "Sex (Males)",
        Variable == "bipolar_diag" & Level == "0" ~ "Bipolar Disorder Controls",
        Variable == "" & Level == "1" ~ "Bipolar Disorder Cases",
        Variable == "age_diag" ~ "Age at Diagnosis",
        Variable == "score_NPREG05" ~ "Age 10 Math Score",
        Variable == "score_NPLES05" ~ "Age 10 Reading Score",
        Variable == "score_NPENG05" ~ "Age 10 English Score",
        Variable == "score_NPREG08" ~ "Age 13 Math Score",
        Variable == "score_NPLES08" ~ "Age 13 Reading Score",
        Variable == "score_NPENG08" ~ "Age 13 English Score",
        Variable == "score_NPREG09" ~ "Age 14 Math Score",
        Variable == "score_NPLES09" ~ "Age 14 Reading Score",
        TRUE ~ Variable
      )) %>%
    select(-Level) %>%
    rename(!!dataset_name := Summary)
}

summary_edited <- Map(
  edit_summary,
  summaries,
  names(summaries)
)

summary_table <- Reduce(function(x,y)
  full_join(x,y, by = "Variable"),
  summary_edited)

View(summary_table)

write.csv(summary_table, "./Results/academicperformance_diagnosis/academicperformance_diagnosis_DescriptiveData.csv", row.names = F)


# 3. Main Analysis ####
# popdata <- fread("./Data/Final Data/academicperformance_diagnosis_popdata.csv")
# sibdata <- fread("./Data/Final Data/academicperformance_diagnosis_sibdata.csv")

#* 3.1. Total Score ----

## looping through all grades
grades <- c("5", "8", "9")

## initialize list to store results
all_results <- list()

## loop through each grade
for (grade in grades) {
  
  # define predictor variable 
  score_var <- paste0("std_score_agg", grade)
  
  # define within/between variables
  bw_vars <- c(paste0(score_var, "_between"), paste0(score_var, "_within"))
  
  # run population model on popdata
  main_pop <- run_cox_clustered(
    cohort = "SSB",
    grade = paste0("Grade ", grade),
    test = "Total Score",
    model = "Population",
    data = popdata,
    outcome = "bipolar_diag",
    predictor = score_var,
    time_var = "time",
    covariates = c("sex", "birthyear", "birthorder"),
    cluster_var = "parents",
    analysis = "Main Model",
    is_sibdata = FALSE
  )
  
  # run between-within model on sibdata
  main_bw <- run_cox_clustered(
    cohort = "SSB",
    grade = paste0("Grade ", grade),
    test = "Total Score",
    model = "Sibling",
    data = sibdata,
    outcome = "bipolar_diag",
    predictor = bw_vars,
    time_var = "time",
    covariates = c("sex", "birthyear", "birthorder"),
    cluster_var = "parents",
    analysis = "Main Model",
    is_sibdata = TRUE
  )
  
  # combine all results for this grade
  all_results[[grade]] <- bind_rows(main_pop, main_bw)
}

## combine across all grades
total_score <- bind_rows(all_results)


#* 3.2. Individual Test Scores ----

## prepping objects to loop over
## list of test variables grouped by grade
test_by_grade <- list(
  "5" = c("std_score_NPREG05", "std_score_NPLES05", "std_score_NPENG05"),
  "8" = c("std_score_NPREG08", "std_score_NPLES08", "std_score_NPENG08"),
  "9" = c("std_score_NPREG09", "std_score_NPLES09")
)

score_vars <- unlist(test_by_grade, use.names = FALSE)

## initialize results list
all_results <- list()

## loop through each grade
for (grade in names(test_by_grade)) {
  
  ## get the test variables for that grade
  test_vars <- test_by_grade[[grade]]
  
  ## loop through each test variable
  for (score_var in test_vars) {
    
    # extract test and grade from score_var
    matches <- stringr::str_match(score_var, "^std_score_([A-Z]+)(\\d+)$")
    test <- matches[, 2]
    grade <- matches[, 3]
    
    # reconstruct base name
    base_name <- paste0("std_score_", test, grade)
    
    # create between and within variable names
    bw_vars <- c(
      paste0(base_name, "_between"),
      paste0(base_name, "_within")
    )
    
    # extract subject name (e.g., "Math", "Reading", "English") from variable
    test_type <- case_when(
      grepl("NPREG", score_var) ~ "Math",
      grepl("NPLES", score_var) ~ "Reading",
      grepl("NPENG", score_var) ~ "English",
      TRUE ~ "Unknown"
    )
    
    # run population model on popdata
    main_pop <- run_cox_clustered(
      cohort = "SSB",
      grade = paste0("Grade ", as.numeric(grade)),
      test = test_type,
      model = "Population",
      data = popdata,
      outcome = "bipolar_diag",
      predictor = score_var,
      time_var = "time",
      covariates = c("sex", "birthyear", "birthorder"),
      cluster_var = "parents",
      analysis = "Main Model",
      is_sibdata = FALSE
      )
    
    # run between-within sibling model
    main_bw <- run_cox_clustered(
      cohort = "SSB",
      grade = paste0("Grade ", as.numeric(grade)),
      test = test_type,
      model = "Sibling",
      data = sibdata,
      outcome = "bipolar_diag",
      predictor = bw_vars,
      time_var = "time",
      covariates = c("sex", "birthyear", "birthorder"),
      cluster_var = "parents",
      analysis = "Main Model",
      is_sibdata = TRUE
       )
    
    # store all models for this score in a list
    key <- paste0(score_var)
    all_results[[key]] <- bind_rows(main_pop, main_bw)
  }
}

## combine results across all test variables and grades
test_scores <- bind_rows(all_results)

## combine all results 
main_results <- bind_rows(total_score, test_scores)

names(main_results)

main_results <- main_results %>%
  mutate(Age = case_when(
    Grade %in% c("Grade 5") ~ "Age 10",
    Grade %in% c("Grade 8") ~ "Age 13",
    Grade %in% c("Grade 9") ~ "Age 14")) %>%
  select(Cohort, Grade, Age, Test, Model, Variable, Estimate, SE, HR, CI_lower, CI_upper, p_value, N, Analysis)


#* 3.3. Plot ----

## edit the results dataframe 
plot1 <- main_results %>%
  filter(grepl("std_score_", Variable)) %>%
  
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
    
    Test = factor(Test, levels = c("Total Score", 
                                   "Math",
                                   "Reading",
                                   "English")),
    
    # significance level
    Significance = p_value < 0.05) %>%
    
    # label for plotting
    arrange(Test, Grade, Estimate_Label) 



## ggplot 
all <- ggplot(plot1, aes(
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
  geom_errorbarh(position = position_dodge2(width = 0.5, padding = 0.5), height = 0.2) +  # error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +  # reference line for HR = 1
  
  # faceting by 'Grade Age' and 'Test'
  facet_grid(rows = vars(Age), cols = vars(Test), scales = "free_y", space = "free", switch = "y") +
  
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
      "Population",
      "Between-sibship",
      "Within-sibship"
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
      "Population Estimate",
      "Between-sibship Estimate",
      "Within-sibship Estimate"
    ),
    name = "Estimate Type"
  ) +
  
  # axis and title labels
  labs(
    x = "Hazard Ratio (HR)",
    y = "",
    color = "Estimate Type",
    shape = "Estimate Type",
    title = "National Test Scores and Bipolar Disorder Diagnosis\n(NOT Corrected for Previous Grade Scores)"
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

all

ggsave("./Results/academicperformance_diagnosis/Figures/academicperformance_diagnosis_MainResults.pdf", plot = all, width = 15, height = 5, dpi = 300)


# 4. Supplementary Analysis - Adjusted for previous grade scores ####
# popdata <- fread("./Data/Final Data/academicperformance_diagnosis_popdata.csv")
# sibdata <- fread("./Data/Final Data/academicperformance_diagnosis_sibdata.csv")

#* 4.1. Total Score ----

## looping through all grades
grades <- c("5", "8", "9")

## initialize list to store results
all_results <- list()

# to add previous grade scores as covariates 
previous_scores <- function(grade) {
  covariates <- switch(
    as.character(grade),
    "5" = character(0),                           # No covariates for grade 5
    "8" = "std_score_agg5",                       # For grade 8, use grade 5 aggregate
    "9" = c("std_score_agg5", "std_score_agg8"),  # For grade 9, use grades 5 and 8
    stop("Grade not supported")
  )
  return(covariates)
}

## loop through each grade
for (grade in grades) {
  
  # define predictor variable 
  score_var <- paste0("std_score_agg", grade)
  
  # define within/between variables
  bw_vars <- c(paste0(score_var, "_between"), paste0(score_var, "_within"))
  
  # run population model on popdata
  main_pop <- run_cox_clustered(
      cohort = "SSB",
      grade = paste0("Grade ", grade),
      test = "Total Score",
      model = "Population",
      data = popdata,
      outcome = "bipolar_diag",
      predictor = score_var,
      time_var = "time",
      covariates = c(previous_scores(grade), "sex", "birthyear", "birthorder"),
      cluster_var = "parents",
      analysis = "Adjusted Model",
      is_sibdata = FALSE
    )
    
    # run between-within model on sibdata
    main_bw <- run_cox_clustered(
      cohort = "SSB",
      grade = paste0("Grade ", grade),
      test = "Total Score",
      model = "Sibling",
      data = sibdata,
      outcome = "bipolar_diag",
      predictor = bw_vars,
      time_var = "time",
      covariates = c(previous_scores(grade), "sex", "birthyear", "birthorder"),
      cluster_var = "parents",
      analysis = "Adjusted Model",
      is_sibdata = TRUE
    )
  
  # combine all results for this grade
  all_results[[grade]] <- bind_rows(main_pop, main_bw)
}

## combine across all grades
total_score_adjusted <- bind_rows(all_results)


#* 4.2. Individual Test Scores ----

## prepping objects to loop over
test_by_grade <- list(
  "5" = c("std_score_NPREG05", "std_score_NPLES05", "std_score_NPENG05"),
  "8" = c("std_score_NPREG08", "std_score_NPLES08", "std_score_NPENG08"),
  "9" = c("std_score_NPREG09", "std_score_NPLES09"))

score_vars <- unlist(test_by_grade, use.names = FALSE)

# function to extract previous grade covariates
previous_scores <- function(score_var) {
  if (score_var == "std_score_NPREG08") return("std_score_NPREG05")
  if (score_var == "std_score_NPLES08") return("std_score_NPLES05")
  if (score_var == "std_score_NPENG08") return("std_score_NPENG05")
  
  if (score_var == "std_score_NPREG09") return(c("std_score_NPREG05", "std_score_NPREG08"))
  if (score_var == "std_score_NPLES09") return(c("std_score_NPLES05", "std_score_NPLES08"))
  
  # Grade 5 or unsupported cases
  return(character(0))
}

## initialize results list
all_results <- list()

## loop through each grade
for (grade in names(test_by_grade)) {
  
  ## get the test variables for that grade
  test_vars <- test_by_grade[[grade]]
  
  ## loop through each test variable
  for (score_var in test_vars) {
    
    # extract test and grade from score_var
    matches <- stringr::str_match(score_var, "^std_score_([A-Z]+)(\\d+)$")
    test <- matches[, 2]
    grade <- matches[, 3]
    
    # reconstruct base name
    base_name <- paste0("std_score_", test, grade)
    
    # create between and within variable names
    bw_vars <- c(
      paste0(base_name, "_between"),
      paste0(base_name, "_within")
    )
    
    # extract subject name (e.g., "Math", "Reading", "English") from variable
    test_type <- case_when(
      grepl("NPREG", score_var) ~ "Math",
      grepl("NPLES", score_var) ~ "Reading",
      grepl("NPENG", score_var) ~ "English",
      TRUE ~ "Unknown"
    )
    
    # run population model on popdata
    main_pop <- run_cox_clustered(
      cohort = "SSB",
      grade = paste0("Grade ", as.numeric(grade)),
      test = test_type,
      model = "Population",
      data = popdata,
      outcome = "bipolar_diag",
      predictor = score_var,
      time_var = "time",
      covariates = c(previous_scores(score_var), "sex", "birthyear", "birthorder"),
      cluster_var = "parents",
      analysis = "Adjusted Model",
      is_sibdata = FALSE
    )
    
    # run between-within sibling model
    main_bw <- run_cox_clustered(
      cohort = "SSB",
      grade = paste0("Grade ", as.numeric(grade)),
      test = test_type,
      model = "Sibling",
      data = sibdata,
      outcome = "bipolar_diag",
      predictor = bw_vars,
      time_var = "time",
      covariates = c(previous_scores(score_var), "sex", "birthyear", "birthorder"),
      cluster_var = "parents",
      analysis = "Adjusted Model",
      is_sibdata = TRUE
      )
    
    # store all models for this score in a list
    key <- paste0(score_var)
    all_results[[key]] <- bind_rows(main_pop, main_bw)
  }
}

## combine results across all test variables and grades
test_scores_adjusted <- bind_rows(all_results)

## combine all results 
supplementary_results_adjusted <- bind_rows(total_score_adjusted, 
                                            test_scores_adjusted)

supplementary_results_adjusted <- supplementary_results_adjusted %>%
  mutate(Age = case_when(
    Grade %in% c("Grade 5") ~ "Age 10",
    Grade %in% c("Grade 8") ~ "Age 13",
    Grade %in% c("Grade 9") ~ "Age 14")) %>%
  select(Cohort, Grade, Age, Test, Model, Variable, Estimate, SE, HR, CI_lower, CI_upper, p_value, N, Analysis)


#* 4.3. Plot ----

## edit the results dataframe 
plot1 <- supplementary_results_adjusted %>%
  filter(grepl("std_score_", Variable)) %>%
  
  filter(
    Grade == "Grade 5" | 
      (Grade == "Grade 8" & grepl("8_", Variable)) | 
      (Grade == "Grade 9" & grepl("9_", Variable)) 
    ) %>%
  
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
    
    Test = factor(Test, levels = c("Total Score", 
                                   "Math",
                                   "Reading",
                                   "English")),
    
    # significance level
    Significance = p_value < 0.05) %>%
  
  arrange(Test, Grade, Estimate_Label) 

## ggplot 
all <- ggplot(plot1, aes(
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
  geom_errorbarh(position = position_dodge2(width = 0.5, padding = 0.5), height = 0.2) +  # error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +  # reference line for HR = 1
  
  # faceting by 'Age' and 'Test'
  facet_grid(rows = vars(Age), cols = vars(Test), scales = "free_y", space = "free", switch = "y") +
  
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
      "Population",
      "Between-sibship",
      "Within-sibship"
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
    title = "National Test Scores and Bipolar Disorder Diagnosis\n(Adjusted for Previous Grade Scores)"
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

all

ggsave("./Results/academicperformance_diagnosis/Figures/academicperformance_diagnosis_Adjusted.pdf",
       plot = all, width = 15, height = 5, dpi = 300)


# 5. Compile All Results and Save ####

all_results <- bind_rows(main_results, 
                         supplementary_results_adjusted)


write.csv(all_results, "./Results/academicperformance_diagnosis/academicperformance_diagnosis_AllResults.csv", row.names = F)

