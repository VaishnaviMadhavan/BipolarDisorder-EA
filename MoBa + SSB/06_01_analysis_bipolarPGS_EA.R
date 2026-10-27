
# 
# Script: 06_01_analysis_bipolarPGS_EA.R 
# Project: Bipolar Disorder and Education: 
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Analysis of bipolar disorder PGS and EA
#
# Author: Vaishnavi K Madhavan
# Start date: 13-05-2025
# 

## analysis steps
# 1. descriptive analysis
# 2. population-based model with total sample 
# 3. between-within sibship model with sibling sample 
# 4. supplementary analyses 


# 1. Set Up  ####

## prepping datasets 
pop_sibinfo_covariates <- fread("./Data/Reference Data/pop_sibinfo_covariates.csv") %>% 
  select(w19_0634_lnr, mother_lnr, parents, sex, birthyear, birthorder, valid_sibs) 

bp_pgs <- fread("./Data/Reference Data/bipolar_disorder_polygenic_score.csv") %>% 
  select(w19_0634_lnr, bipolar_pgs, std_bipolar_pgs)

ea <- fread("./Data/Reference Data/educational_attainment.csv") %>%
  select(w19_0634_lnr, yrsedu, std_yrsedu, yrsedu_direct, std_yrsedu_direct, age)

bipolar_diag <- fread("./Data/Reference Data/bipolar_disorder_diagnosis.csv") %>%
  select(w19_0634_lnr, bipolar_diag, age_diag)

## combine EA, BP PGS, sibling info and covariates into one dataset 
finaldata <- pop_sibinfo_covariates %>%
  left_join(bp_pgs, by = "w19_0634_lnr") %>%
  left_join(ea, by = "w19_0634_lnr") %>%
  left_join(bipolar_diag, by = "w19_0634_lnr") %>%
  filter(!is.na(yrsedu) & !is.na(bipolar_pgs))
nrow(finaldata) # 122,671

## filter to those born after 1960 (arbitrary cut-off to ensure quality of EA data) 
finaldata$birthyear <- as.numeric(finaldata$birthyear)
describe(finaldata$birthyear) # 1939 to 1992
table(finaldata$birthyear)
finaldata <- finaldata %>%
  filter(birthyear >= 1960)
describe(finaldata$birthyear) # 1960 to 1992
nrow(finaldata) # 121,550


## split into population and sibling samples 

## population-level sample
popdata <- select(finaldata, w19_0634_lnr, mother_lnr, parents,
                  yrsedu, std_yrsedu, 
                  yrsedu_direct, std_yrsedu_direct,
                  bipolar_pgs, std_bipolar_pgs, 
                  bipolar_diag, age_diag,
                  sex, birthyear, birthorder, age,
                  valid_sibs) %>%
  filter(!is.na(std_yrsedu) & !is.na(std_bipolar_pgs) & 
           !is.na(sex) & !is.na(birthyear) &
           !is.na(birthorder))
nrow(popdata) # 121,550

## sibling sample 
sibdata <- popdata %>%
  filter(valid_sibs == 1) %>%
  group_by(parents) %>% filter(n() >= 2) %>% ungroup()

nrow(sibdata) # 16,746


## creating between and within variables using the function
sibdata <- create_between_within(sibdata, variable = "std_bipolar_pgs", family_id = "parents")
names(sibdata)

nrow(sibdata) # 16,746
sum(!is.na(sibdata$std_bipolar_pgs_between))  # 16,746
sum(!is.na(sibdata$std_bipolar_pgs_within)) # 16,746

write.csv(popdata, "./Data/Final Data/bipolarPGS_EA_popdata.csv", row.names=FALSE)
write.csv(sibdata, "./Data/Final Data/bipolarPGS_EA_sibdata.csv", row.names=FALSE)


# 2. Descriptive Analysis   ####
# popdata <- fread("./Data/Final Data/bipolarPGS_EA_popdata.csv")
# sibdata <- fread("./Data/Final Data/bipolarPGS_EA_sibdata.csv")

population_summary <- create_summary(popdata, 
                                     continuous = c("birthyear", "yrsedu", "bipolar_pgs", "age_diag"),
                                     categorical = c("sex", "bipolar_diag"),
                                     FID = c("parents"),
                                     digits = 2) 

names(population_summary) <- c("Variable", "Level", "Population")

population_summary <- population_summary  %>%
  mutate(
    Variable = case_when(
      Variable == "" & Level == "2" ~ "Sex (Females)",
      Variable == "sex" & Level == "1" ~ "Sex (Males)",
      Variable == "bipolar_diag" & Level == "0" ~ "Bipolar Disorder Controls",
      Variable == "" & Level == "1" ~ "Bipolar Disorder Cases",
      Variable == "birthyear" ~ "Birth Year",
      Variable == "yrsedu" ~ "EA (Years of Education)",
      Variable == "bipolar_pgs" ~ "Bipolar Disorder PGS",
      TRUE ~ Variable
    ),
    Level = "") %>%
  select(-Level)

sibling_summary <- create_summary(sibdata, 
                                  continuous = c("birthyear", "yrsedu", "bipolar_pgs", "age_diag"),
                                  categorical = c("sex", "bipolar_diag"),
                                  FID = c("parents"),
                                  digits = 2)

names(sibling_summary) <- c("Variable", "Level", "Sibling")


sibling_summary <- sibling_summary  %>%
  mutate(
    Variable = case_when(
      Variable == "" & Level == "2" ~ "Sex (Females)",
      Variable == "sex" & Level == "1" ~ "Sex (Males)",
      Variable == "bipolar_diag" & Level == "0" ~ "Bipolar Disorder Controls",
      Variable == "" & Level == "1" ~ "Bipolar Disorder Cases",
      Variable == "birthyear" ~ "Birth Year",
      Variable == "yrsedu" ~ "EA (Years of Education)",
      Variable == "bipolar_pgs" ~ "Bipolar Disorder PGS",
      TRUE ~ Variable
    ),
    Level = "") %>%
  select(-Level)


summary_table <- population_summary %>%
  left_join(sibling_summary, by = "Variable")

View(summary_table)

write.csv(summary_table, "./Results/bipolarPGS_EA/bipolarPGS_EA_DescriptiveData.csv", row.names=FALSE)


# 2. Main Analysis   ####
# popdata <- fread("./Data/Final Data/bipolarPGS_EA_popdata.csv")
# sibdata <- fread("./Data/Final Data/bipolarPGS_EA_sibdata.csv")

## run population model on popdata
pgs_ea_pop <- run_reg_clustered(
  cohort = "MoBa", 
  model = "Population", 
  data = popdata,
  outcome = "std_yrsedu",
  predictor = "std_bipolar_pgs",
  covariates = c("sex", "birthyear", "birthorder"),
  cluster_var = "parents",
  analysis = "Main Model"
)

## run between-within model on sibdata
pgs_ea_bw <- run_reg_clustered(
  cohort = "MoBa", 
  model = "Sibling", 
  data = sibdata,
  outcome = "std_yrsedu",
  predictor = c("std_bipolar_pgs_between", "std_bipolar_pgs_within"),
  covariates = c("sex", "birthyear", "birthorder"),
  cluster_var = "parents",
  analysis = "Main Model"
)

pgs_ea_results <- bind_rows(pgs_ea_pop, pgs_ea_bw)


# 3. Supplementary Analysis   ####
# popdata <- fread("./Data/Final Data/bipolarPGS_EA_popdata.csv")
# sibdata <- fread("./Data/Final Data/bipolarPGS_EA_sibdata.csv")

#* 3.1. Bipolar Disorder Diagnosis > EA ----

## in the population, does bipolar disorder have an impact on EA?
bp_ea_pop <- lm(std_yrsedu ~ bipolar_diag + sex + birthyear + birthorder, data = popdata) 
summary(bp_ea_pop)
# this is significant, so we should correct for the diagnostic status in the model 


#* 3.2. Main Model Corrected for Diagnosis Status ----

cov_model_pop <- run_reg_clustered(
  cohort = "MoBa", 
  model = "Population", 
  data = popdata,
  outcome = "std_yrsedu",
  predictor = "std_bipolar_pgs",
  covariates = c("bipolar_diag", "sex", "birthyear", "birthorder"),
  cluster_var = "parents",
  analysis = "Covariate Model"
)

cov_model_bw <- run_reg_clustered(
  cohort = "MoBa", 
  model = "Sibling", 
  data = sibdata,
  outcome = "std_yrsedu",
  predictor = c("std_bipolar_pgs_between", "std_bipolar_pgs_within"),
  covariates = c("bipolar_diag", "sex", "birthyear", "birthorder"),
  cluster_var = "parents",
  analysis = "Covariate Model"
)

cov_model <- bind_rows(cov_model_pop, cov_model_bw)


#* 3.3. Direct conversion from NUS to Years of Education ----

## run population model on popdata
pgs_ea_direct_pop <- run_reg_clustered(
  cohort = "MoBa",
  model = "Population", 
  data = popdata,
  outcome = "std_yrsedu_direct",
  predictor = "std_bipolar_pgs",
  covariates = c("sex", "birthyear", "birthorder"),
  cluster_var = "parents",
  analysis = "Main Model - Direct NUS Conversion"
  )

## run between-within model on sibdata
pgs_ea_direct_bw <- run_reg_clustered(
  cohort = "MoBa", 
  model = "Sibling", 
  data = sibdata,
  outcome = "std_yrsedu_direct",
  predictor = c("std_bipolar_pgs_between", "std_bipolar_pgs_within"),
  covariates = c("sex", "birthyear", "birthorder"),
  cluster_var = "parents",
  analysis = "Main Model - Direct NUS Conversion"
  
)

pgs_ea_direct_results <- bind_rows(pgs_ea_direct_pop, pgs_ea_direct_bw)



#* 3.4.Check Incremental R2 ----

basemodel <- lm(std_yrsedu ~ sex + birthyear + birthorder, data = sibdata)
fullmodel <- lm(std_yrsedu ~ std_bipolar_pgs_between + std_bipolar_pgs_within + sex + birthyear + birthorder, data = sibdata)

compare_performance(basemodel, fullmodel, rank = TRUE)


# 4. Combine Results   ####

## all results in a single dataframe for plotting 
all_results <- bind_rows(pgs_ea_results,
                         cov_model,
                         pgs_ea_direct_results)

write.csv(all_results, "./Results/bipolarPGS_EA/bipolarPGS_EA_AllResults.csv", row.names=FALSE)


# 5. Plots   ####
# all_results <- fread("./Results/bipolarPGS_EA/Analysis_bipolarPGS_EA.csv")

## edit the results dataframe 
plot1 <- all_results %>%
  filter(grepl("_(between|within)$", Variable)) %>%
  
  # identify 'between' or 'within' from the variable name
  mutate(
    Estimate_Label = case_when(
      str_detect(Variable, "_between$") ~ "Between-sibship Estimate",
      str_detect(Variable, "_within$") ~ "Within-sibship Estimate",
      TRUE ~ NA_character_
    ),
    
    # factor levels for consistent ordering
    Estimate_Label = factor(
      Estimate_Label,
      levels = c(
        "Within-sibship Estimate",
        "Between-sibship Estimate"
      )
    ),
    
    # significance level
    Significance = p_value < 0.05,
    
    
    Model = factor(Model, levels = c("Main Model", 
                                     "Covariate Model",
                                     "Main Model - Direct NUS Conversion"))) %>%
  
  # create descriptions for the row labels
  mutate(Analysis_Label = recode(Analysis,
                              "Main Model" = "Main Model",
                              "Covariate Model" = "Covariate Model (+ Bipolar Disorder Diagnosis)",
                              "Main Model - Direct NUS Conversion" = "Main Model - Direct Conversion (NUS > Yrs Edu)"),
         Analysis_Label = factor(Analysis_Label,
                            levels = c("Main Model", 
                                       "Covariate Model (+ Bipolar Disorder Diagnosis)", 
                                       "Main Model - Direct Conversion (NUS > Yrs Edu)")))


## forest plot
all <- ggplot(plot1, aes(
  x = Estimate,
  y = Estimate_Label,
  xmin = CI_lower,
  xmax = CI_upper,
  shape = Estimate_Label,
  color = Estimate_Label,
  alpha = Significance
)) +
  
  # plot Elements: CI Bars, Points, and Reference Line
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.5) +  # estimate points and CI
  geom_errorbarh(position = position_dodge2(width = 0.5, padding = 0.5), height = 0.2) +# error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") + # reference line 
  
  # faceting by 'Analysis_Label' 
  facet_grid(rows = vars(Analysis_Label), scales = "free_y", space = "free", switch = "y") +
  
  # alpha mapping: fade non-significant estimates
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.7)) +  # fade nonsignificant
  guides(alpha = "none") +                                     # remove alpha legend
  
  # X-axis formatting
  scale_x_continuous(
    breaks = seq(-0.08, 0.08, by = 0.02),
    limits = c(-0.08, 0.08)) + 
  
  # color mapping: estimate type colors 
  scale_color_manual(
    values = c(
      "Between-sibship Estimate" = "#CC79A7",
      "Within-sibship Estimate" = "#009E73"),
    breaks = c("Between-sibship Estimate", "Within-sibship Estimate"),
    name = "Estimate Type"
  ) +
  
  # shape mapping: estimate type symbols
  scale_shape_manual(
    values = c(
      "Between-sibship Estimate" = 18,
      "Within-sibship Estimate" = 16),
    breaks = c("Between-sibship Estimate", "Within-sibship Estimate"),
    name = "Estimate Type"
  ) +
  
  # axis and title labels
  labs(
    x = "Estimate (beta)",
    y = "",
    color = "Estimate Type",
    shape = "Estimate Type",
    title = "Bipolar Disorder PGS and Educational Attainment"
  ) +
  
  # theme and final formatting
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 12, face = "bold", margin = margin(b = 2, t = 2)),
    strip.text.y.left = element_text(size = 9, angle = 0, hjust = 0, margin = margin(0, 10, 0, 10)),
    strip.background = element_rect(color = "black", fill = "white"),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_blank(),           # remove y axis labels 
    axis.ticks.y = element_blank(),          
    axis.text.x = element_text(size = 8),    # adjust size of x axis labels  
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical"
  )

dev.off()
all

ggsave("./Results/bipolarPGS_EA/Figures/bipolarPGS_EA_AllResults.pdf", plot = all, width = 10, height = 5, dpi = 300)

