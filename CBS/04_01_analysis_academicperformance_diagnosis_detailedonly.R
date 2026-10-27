
#
# Script: 04_01_analysis_academicperformance_diagnosis_detailedonly.R
# Project: Bipolar Disorder and Education:
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Script for the analysis of the association between academic performance and 
#                   bipolar disorder diagnosis - detailed diagnosis only
#
# Author: Vaishnavi K Madhavan
# Start date: 16-02-2025
# 


# 1. Set Up ####

cito <- fread("./Data/02_Reference_Data/20260213_cito_scores.csv")
bipolar_diag <- fread("./Data/02_Reference_Data/20260521_bipolar_disorder_diagnosis.csv")
family_info <- fread("./Data/02_Reference_Data/20260521_family_info.csv")

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


#* 2.2. Filter out those born after 2001 ----
## we only retain those who are at least 15 years old in December 2016 (which is the last year of diagnostic data available)
## this means those born <= 2003
table(finaldata$GBAGEBOORTEJAAR) 

check_cases <- finaldata %>%
  filter(alldatasets_bipolar_diag == 1)
hist(check_cases$GBAGEBOORTEJAAR)
table(check_cases$GBAGEBOORTEJAAR) # not many cases after 2004. 
rm(check_cases)

finaldata <- finaldata %>%
  filter(GBAGEBOORTEJAAR <= 2003)
nrow(finaldata) # 1,144,287


#* 2.3. Changing time into a numeric variable (to avoid issues with cox regression) ----
finaldata$alldatasets_time <- as.numeric(finaldata$alldatasets_time)
finaldata$bipolar_diag <- as.numeric(finaldata$bipolar_diag)


#* 2.4. Check age at diagnosis ----
table(finaldata$alldatasets_age_diag) # around 200 before the age of 15
table(finaldata$alldatasets_time) # same as above

## check those diagnosed before 18
check_18 <- finaldata %>%
  filter(alldatasets_age_diag < 18) # 125 individuals 
table(finaldata$alldatasets_bipolar_diag) # out of 1744 (seems like a lot) 
table(check_18$GBAGEBOORTEJAAR) # from 1993

check_15 <- finaldata %>%
  filter(alldatasets_age_diag < 15) # 25 individuals 
table(finaldata$alldatasets_bipolar_diag) # out of 1744  
table(check_15$GBAGEBOORTEJAAR)
rm(check_18, check_15)

## the date of diagnosis in these datasets are the start of the care trajectory and not the actual date of diagnosis. 
## I was told by the dataset experts that the date of diagnosis can be later than the start of the care trajectory (impossible to know when)
## I retain all since I cannot filter them properly

nrow(finaldata) # 1,144,287
table(finaldata$alldatasets_bipolar_diag) # 1690 cases (0.001%)
finaldata$bipolar_diag_weights <- as.numeric(finaldata$bipolar_diag_weights)

#write.csv(finaldata, "./Data/03_Final_Data/20260716_finaldata_detailedonly.csv", row.names=FALSE)


#* 2.5. Form Population and Sibling Samples ----
# finaldata <- fread("./Data/03_Final_Data/20260716_finaldata_detailedonly.csv")

## population-level sample
nrow(finaldata) # 1,144,287

popdata <- finaldata %>%
  filter(!is.na(CitoZScoreTaal) | !is.na(CitoZScoreRekenenWiskunde) | !is.na(CitoZScoreStudievaardigh) |
           !is.na(std_CitoStandaardScore))

nrow(popdata) # 1,144,287

## check birth year after filtering out on available data 
describe(finaldata$GBAGEBOORTEJAAR) 

## sibling sample
sibdata <- popdata %>% 
  filter(valid_sibs ==1) %>%
  group_by(FID) %>% filter(n() >= 2) %>% ungroup()

nrow(sibdata) # 619,796

## creating between and within variables 
## looping through all grades
score_vars <- c("CitoZScoreTaal", "CitoZScoreRekenenWiskunde", "CitoZScoreStudievaardigh", "std_CitoStandaardScore")

## apply to popdata and sibdata
for (var in score_vars) {
  sibdata <- add_between_within(sibdata, var, family_id = "FID")
}

write.csv(popdata, "./Data/03_Final_Data/20260716_popdata_detailedonly.csv", row.names=FALSE)
write.csv(sibdata, "./Data/03_Final_Data/20260716_sibdata_detailedonly.csv", row.names=FALSE)


# 3. Descriptive Analysis  ####
# popdata <- fread("./Data/03_Final_Data/20260716_popdata_detailedonly.csv")
# sibdata <- fread("./Data/03_Final_Data/20260716_sibdata_detailedonly.csv")

datasets <- list(
  Population_Total_Sample = popdata,
  Siblings_Total_Sample = sibdata
)

summaries <- lapply(datasets, function(x) { 
  create_summary(
    data = x,
    continuous = c("GBAGEBOORTEJAAR", "Fsize", "birthorder", "TestYear",
                   "CitoScoreTaal", "CitoScoreRekenenWiskunde", "CitoScoreStudieVaardigheden", "CitoStandaardScore",
                   "detailed_age_diag"),
    categorical = c("GBAGESLACHT", "detailed_bipolar_diag"),
    FID = "FID",
    digits = 2
  )})


## edit the table
edit_summary <- function(df, dataset_name) { 
  
  df %>% 
    mutate(Variable = case_when(
      Variable == "Sample Size" ~ "Sample Size", 
      Variable == "Number of Families" ~ "Number of Families", 
      Variable == "" & Level == "2" ~ "Sex (Females)",
      Variable == "GBAGESLACHT" & Level == "1" ~ "Sex (Males)",
      Variable == "" & Level == "TRUE" ~ "Bipolar Disorder Cases, n(%)",
      Variable == "detailed_bipolar_diag" & Level == "FALSE" ~ "Bipolar Disorder Controls, n(%)",
      Variable == "GBAGEBOORTEJAAR" ~ "Birth Year", 
      Variable == "Fsize" ~ "Family Size", 
      Variable == "birthorder" ~ "Birth Order", 
      Variable == "TestYear" ~ "Year of Test", 
      Variable == "CitoStandaardScore" ~ "Total Score", 
      Variable == "CitoScoreRekenenWiskunde" ~ "Math Score", 
      Variable == "CitoScoreStudieVaardigheden" ~ "Reading Score", 
      Variable == "CitoScoreTaal" ~ "Language Score",
      Variable == "detailed_age_diag" ~ "Age at Diagnosis",
      Variable == "detailed_bipolar_diag" ~ "Bipolar Disorder Cases, n(%)"
    )) %>%
    select(-Level) %>%
    rename(!!dataset_name := Summary)}

descriptive_table <- Map(
  edit_summary,
  summaries,
  names(summaries)
)

descriptive_table <- Reduce(function(x,y)
  full_join(x,y, by = "Variable"),
  descriptive_table)

write.csv(descriptive_table, "./Results/Main_Results/DetailedOnly_Descriptive_Table.csv", row.names=FALSE)


# 4. Main Analysis ####
# popdata <- fread("./Data/03_Final_Data/20260716_popdata_detailedonly.csv")
# sibdata <- fread("./Data/03_Final_Data/20260716_sibdata_detailedonly.csv")

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
  detailed_pop <- run_cox_clustered(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label, 
    model = "Population",
    data = popdata,
    outcome = "detailed_bipolar_diag",
    predictor = test,
    time_var = "detailed_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    analysis = "Main Model",
    is_sibdata = FALSE
  )
  
  
  # run between-within model on sibdata
  detailed_bw <- run_cox_clustered(
    cohort = "CBS",
    grade = "Grade 7",
    age = "Age 12",
    test = test_label, 
    model = "Sibling",
    data = sibdata,
    outcome = "detailed_bipolar_diag",
    predictor = bw_vars,
    time_var = "detailed_time",
    covariates = c("GBAGESLACHT", "GBAGEBOORTEJAAR", "birthorder"),
    cluster_var = "FID",
    analysis = "Main Model",
    is_sibdata = TRUE
  )
  
  # combine all results for this grade
  all_results[[test]] <- bind_rows(detailed_pop, detailed_bw)
}

## combine across all grades
detailed_results <- bind_rows(all_results)

write_csv(detailed_results, "./Results/Main_Results/DetailedOnly_Results.csv")


# 5. Figures ####
#detailed_results <- fread("./Results/Main_Results/DetailedOnly_Results.csv")

## edit the results dataframe 
plot1 <- detailed_results %>%
  filter(grepl("^Cito|^std", Variable)) %>%
  
  mutate(
    Test_Label = case_when(
      str_detect(Variable, "^CitoZScoreTaal") ~ "Language",
      str_detect(Variable, "^CitoZScoreRekenenWiskunde") ~ "Math",
      str_detect(Variable, "CitoZScoreStudievaardigh") ~ "Reading",
      str_detect(Variable, "^std_CitoStandaardScore") ~ "Total Score",   
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
    
dev.off()


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
  geom_errorbarh(position = position_dodge2(width = 0.5, padding = 0.5), width = 0.2) +  # error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +  # reference line for HR = 1
  
  # faceting by 'Test_Label' 
  facet_grid(cols = vars(Test_Label), scales = "free_y", space = "free", switch = "y") +
  
  # alpha mapping: fade non-significant estimates
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5)) +
  guides(alpha = "none") +  # remove alpha legend
  
  # X-axis formatting
  scale_x_continuous(breaks = seq(-0.4, 3.0, by = 0.2)) +
  
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
      "Population"          = 4,  # diamond
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
    title = "CITO Test Scores at Age 12 and Bipolar Disorder Diagnosis\n(Detailed Dataset - 2011 to 2016)"
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

ggsave("./Results/Figures/DetailedOnly_Results.pdf", plot = all, width = 15, height = 3, dpi = 300)
ggsave("./Results/Figures/DetailedOnly_Results.jpg", plot = all, width = 15, height = 3, dpi = 300)

