
# 
# Script: 07_01_supplementary_bipolarPGS_academicperformance.R 
# Project: Bipolar Disorder and Education: 
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Supplementary analysis of Bipolar Disorder PGS and Academic Performance  
#
# Author: Vaishnavi K Madhavan
# Start date: 11-06-2025
# 

## analysis steps
# 1. descriptive analysis
# 2. population-based model with total sample 
# 3. between-within sibship model with sibling sample 


# 1. Set Up #### 

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

bp_pgs <- fread("N:/durable/projects/limited_C/36199047_Vaish_BipolarEA/Data/Reference Data/bipolar_disorder_polygenic_score.csv") %>% 
  select(w19_0634_lnr, 
         bipolar_pgs,
         std_bipolar_pgs)

finaldata <- pop_sibinfo_covariates %>%
  left_join(nat_test_scores, by = "w19_0634_lnr") %>%
  left_join(bp_pgs, by = "w19_0634_lnr") %>%
  left_join(bipolar_diag, by = "w19_0634_lnr") 

nrow(finaldata) # 5,034,008
sum(!is.na(finaldata$bipolar_pgs)) # 198,036

finaldata <- finaldata %>%
               filter(!is.na(std_score_NPLES05) | !is.na(std_score_NPREG05) | 
                        !is.na(std_score_NPENG05) |!is.na(std_score_agg5) | 
                        !is.na(std_score_NPLES08) | !is.na(std_score_NPREG08) | 
                        !is.na(std_score_NPENG08) |!is.na(std_score_agg8) | 
                        !is.na(std_score_NPLES09) | !is.na(std_score_NPREG09) | 
                        !is.na(std_score_agg9)) %>%
  filter(!is.na(bipolar_pgs))

nrow(finaldata) # 74108
sum(!is.na(finaldata$bipolar_pgs)) # 74108

table(finaldata$bipolar_diag) # 153  cases
table(finaldata$birthyear) # 1968 to 2009

check <- finaldata %>%
  filter(birthyear <=1998) # seems fine but I remove it since there is not many people from these cohorts

finaldata <- finaldata %>%
  filter(birthyear >=1999)


## population-level sample
popdata <- finaldata
nrow(popdata) # 74104

## sibling sample 
sibdata <- popdata %>%
  filter(valid_sibs == 1) %>%
  group_by(parents) %>% filter(n() >= 2) %>% ungroup()

nrow(sibdata) # 19,091

## creating between and within variables 
mean_sib_pgs <- 
  group_by(sibdata, parents) %>%
  summarize(std_bipolar_pgs_between = mean(std_bipolar_pgs, na.rm=TRUE))

## merging the two datasets together
sibdata <- merge(sibdata, mean_sib_pgs, by = "parents")

## each individual value of EA_within = one's own EA value - the family average (EA_between)
sibdata$std_bipolar_pgs_within <- sibdata$std_bipolar_pgs - sibdata$std_bipolar_pgs_between

write.csv(popdata, "./Data/Final Data/bipolarPGS_academicperformance_popdata.csv", row.names=FALSE)
write.csv(sibdata, "./Data/Final Data/bipolarPGS_academicperformance_sibdata.csv", row.names=FALSE)


# 2. Descriptive Analysis ####

# popdata <- fread("./Data/Final Data/bipolarPGS_academicperformance_popdata.csv")
# sibdata <- fread("./Data/Final Data/bipolarPGS_academicperformance_sibdata.csv")

#* 2.1. Creating datasets for grade 5, 8, and 9 separately ----

## population data 
pop_split_by_grade <- function(data) {
  data_g5 <- data %>%
    filter(!is.na(std_score_agg5) | !is.na(score_NPLES05) | !is.na(score_NPREG05) | !is.na(score_NPENG05)) %>%
    select(w19_0634_lnr, bipolar_diag, age_diag, bipolar_pgs, std_bipolar_pgs,
           score_NPENG05, score_NPREG05, score_NPLES05,
           std_score_NPENG05, std_score_NPREG05, std_score_NPLES05, std_score_agg5,
           sex, birthyear, birthorder, parents) 
  
  data_g8 <- data %>%
    filter(!is.na(std_score_agg8) | !is.na(score_NPLES08) | !is.na(score_NPREG08) | !is.na(score_NPENG08)) %>%
    select(w19_0634_lnr, bipolar_diag, age_diag, bipolar_pgs, std_bipolar_pgs,
           score_NPENG08, score_NPREG08, score_NPLES08,
           std_score_NPENG08, std_score_NPREG08, std_score_NPLES08, std_score_agg8,
           sex, birthyear, birthorder, parents) 
  
  data_g9 <- data %>%
    filter(!is.na(std_score_agg9) | !is.na(score_NPLES09) | !is.na(score_NPREG09)) %>% 
    select(w19_0634_lnr, bipolar_diag, age_diag, bipolar_pgs, std_bipolar_pgs,
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

nrow(popdata_g5) # 72923
nrow(popdata_g8) # 73136
nrow(popdata_g9) # 71956

## sibling data 
sib_split_by_grade <- function(data) {
  data_g5 <- data %>%
    filter(!is.na(std_score_agg5) | !is.na(score_NPLES05) | !is.na(score_NPREG05) | !is.na(score_NPENG05)) %>%
    filter(!is.na(parents)) %>%
    group_by(parents) %>% filter(n() >= 2) %>% ungroup()  %>%
    select(w19_0634_lnr, bipolar_diag, age_diag, bipolar_pgs, std_bipolar_pgs, 
           score_NPENG05, score_NPREG05, score_NPLES05,
           std_score_NPENG05, std_score_NPREG05, std_score_NPLES05, std_score_agg5,
           sex, birthyear, birthorder, parents)
  
  data_g8 <- data %>%
    filter(!is.na(std_score_agg8) | !is.na(score_NPLES08) | !is.na(score_NPREG08) | !is.na(score_NPENG08)) %>%
    filter(!is.na(parents)) %>%
    group_by(parents) %>% filter(n() >= 2) %>% ungroup() %>%
    select(w19_0634_lnr, bipolar_diag, age_diag, bipolar_pgs, std_bipolar_pgs,
           score_NPENG08, score_NPREG08, score_NPLES08, 
           std_score_NPENG08, std_score_NPREG08, std_score_NPLES08, std_score_agg8, 
           sex, birthyear, birthorder, parents)  
  
  data_g9 <- data %>%
    filter(!is.na(std_score_agg9) | !is.na(score_NPLES08) | !is.na(score_NPREG08)) %>%
    filter(!is.na(parents)) %>%
    group_by(parents) %>% filter(n() >= 2) %>% ungroup()  %>%
    select(w19_0634_lnr, bipolar_diag, age_diag, bipolar_pgs, std_bipolar_pgs,
           score_NPREG09, score_NPLES09, 
           std_score_NPREG09, std_score_NPLES09, std_score_agg9,
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

nrow(sibdata_g5) # 18616
nrow(sibdata_g8) # 18689
nrow(sibdata_g9) # 18845

## check manually 
nrow(sibdata_g5) # 18616
check <- popdata %>%
  filter(!is.na(std_score_agg5) | !is.na(score_NPLES05) | !is.na(score_NPREG05) | !is.na(score_NPENG05)) %>%
  filter(!is.na(parents)) %>%
  group_by(parents) %>% filter(n() >= 2) %>% ungroup()
nrow(check) # 18616 (checks out)
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
    continuous = c("birthyear", "age_diag",
                   "score_NPREG05", "score_NPLES05", "score_NPENG05",
                   "score_NPREG08", "score_NPLES08", "score_NPENG08",
                   "score_NPREG09", "score_NPLES09",
                   "bipolar_pgs"),
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
        Variable == "bipolar_pgs" ~ "Bipolar Disorder PGS",
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

write.csv(summary_table, "./Results/bipolarPGS_academicperformance/bipolarPGS_academicperformance_DescriptiveData.csv", row.names=FALSE)


# 3. Main Analysis #### 
# popdata <- fread("./Data/Final Data/bipolarPGS_academicperformance_popdata.csv")
# sibdata <- fread("./Data/Final Data/bipolarPGS_academicperformance_sibdata.csv")

#* 3.1. Total Scores ----

## looping through all grades
grades <- c("5", "8", "9")

## initialize list to store results
all_results <- list()

## loop through each grade
for (grade in grades) {
  
  # form the outcome variable 
  score_var <- paste0("std_score_agg", grade)
  
  ## run population model on popdata
  main_pop <- supplementary_run_reg_clustered(
      cohort = "MoBa", 
      grade = paste0("Grade ", grade),
      test = "Total Score",
      model = "Population", 
      data = popdata,
      outcome = score_var,
      predictor = "std_bipolar_pgs",
      covariates = c("sex", "birthyear", "birthorder"),
      cluster_var = "parents",
      is_sibdata = FALSE
  )
    
  ## run between-within model on sibdata
  main_bw <- supplementary_run_reg_clustered(
    cohort = "MoBa", 
    grade = paste0("Grade ", grade),
    test = "Total Score",
    model = "Sibling", 
    data = sibdata,
    outcome = score_var,
    predictor = c("std_bipolar_pgs_between", "std_bipolar_pgs_within"),
    covariates = c("sex", "birthyear", "birthorder"),
    cluster_var = "parents",
    is_sibdata = TRUE
  )
  
  ## combine all results for this grade
  all_results[[grade]] <- bind_rows(main_pop, main_bw)
}

## combine across all grades
main_results <- bind_rows(all_results)


#* 3.2. Individual Test Scores ----
# popdata <- fread("./Data/Final Data/bipolarPGS_academicperformance_popdata.csv")
# sibdata <- fread("./Data/Final Data/bipolarPGS_academicperformance_sibdata.csv")

## list of test variables grouped by grade
test_by_grade <- list(
  "5" = c("std_score_NPREG05", "std_score_NPLES05", "std_score_NPENG05"),
  "8" = c("std_score_NPREG08", "std_score_NPLES08", "std_score_NPENG08"),
  "9" = c("std_score_NPREG09", "std_score_NPLES09") 
)

## initialize results list
all_results <- list()

## loop through each grade
for (grade in names(test_by_grade)) {
  
  # get the test variables for that grade
  test_vars <- test_by_grade[[grade]]
  
  # loop through each test variable
  for (score_var in test_vars) {
    
  # extract subject name (e.g., "Math", "Reading", "English") from variable
  test_type <- case_when(
      grepl("NPREG", score_var) ~ "Math",
      grepl("NPLES", score_var) ~ "Reading",
      grepl("NPENG", score_var) ~ "English",
      TRUE ~ "Unknown"
    )
    
  # run population model
  main_pop <- supplementary_run_reg_clustered(
    cohort = "MoBa", 
    grade = paste0("Grade ", grade),
    test = test_type,
    model = "Population", 
    data = popdata,
    outcome = score_var,
    predictor = "std_bipolar_pgs",
    covariates = c("sex", "birthyear", "birthorder"),
    cluster_var = "parents",
    is_sibdata = FALSE
      )
    
  # Run between-within sibling model
  main_bw <- supplementary_run_reg_clustered(
    cohort = "MoBa", 
    grade = paste0("Grade ", grade),
    test = test_type,
    model = "Sibling", 
    data = sibdata,
    outcome = score_var,
    predictor = c("std_bipolar_pgs_between", "std_bipolar_pgs_within"),
    covariates = c("sex", "birthyear", "birthorder"),
    cluster_var = "parents",
    is_sibdata = TRUE
      )
    
  # Store all models for this score in a list
    key <- paste0(score_var)
    all_results[[key]] <- bind_rows(main_pop, main_bw)
  }
}

## Combine results across all test variables and grades
test_results <- bind_rows(all_results)

supplementary_results <- bind_rows(main_results, test_results)

supplementary_results <- supplementary_results %>%
  mutate(Age = case_when(
    Grade %in% c("Grade 5") ~ "Age 10",
    Grade %in% c("Grade 8") ~ "Age 13",
    Grade %in% c("Grade 9") ~ "Age 14")) %>%
  select(Cohort, Grade, Age, Test, Model, Variable, Estimate, SE, CI_lower, CI_upper, p_value, N)

write.csv(supplementary_results, "./Results/bipolarPGS_academicperformance/bipolarPGS_academicperformance_AllResults.csv", row.names = FALSE)


# 4. Figures ####

supplementary_results <- fread("./Results/bipolarPGS_academicperformance/bipolarPGS_academicperformance_AllResults.csv")

## edit the results dataframe 
plot1 <- supplementary_results %>%
  filter(grepl("std_bipolar_", Variable)) %>%
  
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


## forest plot
all <- ggplot(plot1, aes(
  x = Estimate,
  y = Estimate_Label,
  xmin = CI_lower,
  xmax = CI_upper,
  shape = Estimate_Label,
  color = Estimate_Label,
  alpha = Significance)) +
  
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.5) +
  geom_errorbarh(position = position_dodge2(width = 0.5, padding = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  facet_grid(rows = vars(Age), cols = vars(Test), scales = "free_y", space = "free", switch = "y") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5)) +  # fade nonsignificant
  guides(alpha = "none") +                                     # removed legends for significance
  
  # edit labels in x-axis spacing
  scale_x_continuous(
    breaks = seq(-0.08, 0.10, by = 0.02),
    limits = c(-0.08, 0.10)) + 
  
  # color and shape mappings
  scale_color_manual(
    values = c(
      "Population" = "royalblue",
      "Between-sibship" = "#CC79A7",
      "Within-sibship" = "#009E73"
    ),
    breaks = c("Population",
               "Between-sibship", 
               "Within-sibship"),
    name = "Estimate Type"
  ) +
  
  scale_shape_manual(
    values = c(
      "Population" = 4,
      "Between-sibship" = 16,
      "Within-sibship" = 16
    ),
    breaks = c("Population",
               "Between-sibship", 
               "Within-sibship"),
    name = "Estimate Type"
  ) +
  
  # axis and title labels
  labs(
    x = "Estimate (beta)",
    y = "",
    color = "Estimate Type",
    shape = "Estimate Type",
    title = "Bipolar Disorder PGS and Academic Performance") +
  
  # minimal theme with consolidated custom styling
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 12, face = "bold", margin = margin(b = 2, t = 2)),
    strip.text.y.left = element_text(size = 9, angle = 0, hjust = 0, margin = margin(0, 10, 0, 10)),
    strip.background = element_rect(color = "black", fill = "white"),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 6),    # adjust size of x axis labels  
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical")

all

ggsave("./Results/bipolarPGS_academicperformance/Figures/bipolarPGS_academicperformance_AllResults.pdf", plot = all, width = 15, height = 5, dpi = 300)

