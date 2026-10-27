
#
# Script: 03_02_bipolar_diagnosis_prediction.R
# Project: Bipolar Disorder and Education:
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Script to predict cases in later years using complete diagnosis data 
#
# Author: Vaishnavi K Madhavan
# Start date: 21-04-2026
# 


# 1. Set Up ####
medicine <- fread("G:/GezondheidWelzijn/MEDICIJNTAB/2024/MEDICIJN2024TABV1.csv") 
gba_allyears_full_diagnoses <- fread("./Data/02_Reference_Data/20260212_alldiagnoses_allyears_allcols_cases.csv")
gba <- fread("G:/Bevolking/GBAPERSOONTAB/2024/GBAPERSOON2024TABV3.csv") %>%
  select(RINPERSOON, GBAGEBOORTEJAAR, GBAGESLACHT)


# 2. Clean Medicine Dataset ####
colnames(medicine)
# "RINPERSOONS" "RINPERSOON"  "ATC4"        "DDD"
## DDD has only NAs
medicine <- medicine %>%
  select(-DDD, -RINPERSOONS)

table(medicine$ATC4)
nrow(medicine) # 46,890,764
length(unique(medicine$RINPERSOON)) # 11,819,425 
# so RINPERSOON is repeated if people take more than one medication, one row contains only one medicine code

## ATC-4 codes we are interested in:
# N01A    N01B    N02A    N02B    N02C    N03A    N04
# N04A    N04B    N05A    N05B    N05C    N06A    N06B 
# N06D    N07A    N07B    N07C    N07X 

## retain only these rows 
medicine$ATC4 <- as.character(medicine$ATC4)
medicine_bipolar <- medicine %>%
  filter(startsWith(ATC4, "N"))  
         
table(medicine_bipolar$ATC4)
nrow(medicine_bipolar) # 4,564,496
length(unique(medicine_bipolar$RINPERSOON)) # 3,130,556

medicine_bipolar %>%
  count(RINPERSOON) %>%
  count(n)

## turn this dataset wide
medicine_bipolar_wide <- medicine_bipolar %>%
  mutate(value = 1) %>%
  distinct(RINPERSOON, ATC4, .keep_all = TRUE) %>%
  pivot_wider(
    names_from = ATC4,
    values_from = value, 
    values_fill = 0
  )

colnames(medicine_bipolar_wide)
rm(medicine, medicine_bipolar)


# 3. Prepare Diagnosis Datasets ####

## merge datasets 
df <- gba_allyears_full_diagnoses %>%
  left_join(medicine_bipolar_wide, by = "RINPERSOON") %>%
  left_join(gba, by = "RINPERSOON")

rm(gba, medicine_bipolar_wide)

## replacing NA with 0 in the all the medicine columns 
table(df$detailed_bipolar_allyears)
table(df$hoofd_bipolar_allyears)
table(df$vektis_bipolar_allyears)
table(df$N01A)

df <- df %>%
  mutate(across(-RINPERSOON, ~replace_na(., 0)))

table(df$detailed_bipolar_allyears)
table(df$hoofd_bipolar_allyears)
table(df$vektis_bipolar_allyears)
table(df$N01A)

## check (detailed_bipolar_allyears, detailed_bipolar_and_mood_allyears, hoofd_bipolar_allyears, vektis_bipolar_allyears)
## check medication N03A = antiepileptics, N05A = antipsyhcotics, N06A = antidepressants, N07B = resources for addictions
check <- df %>%
  filter(detailed_bipolar_allyears == 1)
table(check$N03A)
table(check$N05A)
table(check$N06A)
table(check$N07B)

check <- df %>%
  filter(detailed_bipolar_allyears == 0 & detailed_bipolar_and_mood_allyears ==1)
table(check$N03A)
table(check$N05A)
table(check$N06A)
table(check$N07B)

rm(check)

## check correlation between detailed and hoofd bipolar and mood disorder in 2015 - 2016
df$detailed_bipolar_and_mood_overlap <- ifelse(df$bipolar_and_mood_detailed_2015 | df$bipolar_and_mood_detailed_2016 == 1, 1, 0)
df$hoofd_bipolar_and_mood_overlap <- ifelse(df$bipolar_hoofd_2015 | df$bipolar_hoofd_2016 == 1, 1, 0)

check <- df %>%
  filter(detailed_bipolar_and_mood_overlap != hoofd_bipolar_and_mood_overlap)

nrow(df) # 97,605
table(check$detailed_bipolar_and_mood_overlap) # 97% overlap (good to use!)


df_2015_16 <- df %>%
  mutate(hoofd_vektis_bipolar_and_mood_2015_2016 = ifelse(df$bipolar_hoofd_2015 | df$bipolar_hoofd_2016 | 
                                                            df$bipolar_vektis_2015 | df$bipolar_vektis_2016 == 1, 1, 0)) %>%
  filter(hoofd_vektis_bipolar_and_mood_2015_2016 == 1) 


## split into training and validation datasets
set.seed(123)

IDs <- unique(df_2015_16$RINPERSOON)
train_ids <- sample(IDs, size = 0.8*length(IDs))

train <- df_2015_16 %>% filter(RINPERSOON %in% train_ids)
validate <- df_2015_16 %>% filter(!RINPERSOON %in% train_ids)

sum(train$RINPERSOON %in% validate$RINPERSOON)

nrow(train) # 32,427
nrow(validate) # 8,107

table(train$detailed_bipolar_allyears)
table(validate$detailed_bipolar_allyears)


# 4. Training Dataset (2011- 2014) ####

## filtering to those who have a diagnosis in hoofd/vektis dataset in 2015 and 2016

nrow(train) # 32,427
table(train$detailed_bipolar_allyears) 

table(train$GBAGEBOORTEJAAR)
hist(train$GBAGEBOORTEJAAR)

## create outcome
train$Y <- ifelse(train$detailed_bipolar_allyears == 1, 1, 0)

## fit logistic model
model <- glm(
  Y ~ N05A + N02A + N05B + N06A + N01B + N06B + N05C + N07B + N02C + N03A + N02B + N04B + N06D + N07C + N07X + N04A + N01A + N07A + N04,
  data = train,
  family = binomial
)
summary(model)
N <- nobs(model)

reg_coef <- broom::tidy(model) %>%
  mutate(OR = round(exp(estimate), 3), 
         CI_lower = round(exp(estimate - 1.96 * std.error), 3), 
         CI_upper = round(exp(estimate + 1.96 * std.error), 3),
         Variable = term, 
         Estimate = estimate, 
         SE = std.error,
         p_value = p.value,
         N = N
         ) %>%
  select(Variable, Estimate, SE, OR, CI_lower, CI_upper, p_value, N)

  
write.csv(reg_coef, "./Results/Supplementary_Results/AllDatasets_Prediction_RegressionEstimates.csv", row.names=FALSE)


## evaluate model
train$pred <- predict(model, newdata = train, type = "response")

# ROC curve and AUC compared predicted probbilites with the labels in the training dataset
library(pROC)
roc_obj <- roc(train$Y, train$pred)
auc(roc_obj)
# 0.7518


# 5. Valid Dataset (Hoofd and Vektis Overlap) ####

#* 5.1. Validate in Hoofd and Vektis Combined ----

nrow(validate) # 8,107
table(validate$GBAGEBOORTEJAAR) # 1920 to 1998
hist(validate$GBAGEBOORTEJAAR)

table(validate$detailed_bipolar_allyears) # 5768 cases
validate$detailed_bipolar_allyears <- ifelse(validate$detailed_bipolar_allyears == TRUE, 1, 0)
table(validate$detailed_bipolar_allyears) # 5768 cases

## predict
validate$pred_prob <- predict(model, newdata = validate, type = "response")
hist(validate$pred_prob)
table(validate$pred_prob)

## check the rows with 0.561114893351484 
check <- validate %>%
  filter(pred_prob >= 0.5 & pred_prob <=0.6)
table(check$N03A)
table(check$N05A)
table(check$N06A)

check <- validate %>%
  filter(pred_prob >= 0.56 & pred_prob <= 0.57) 
table(check$N03A)
table(check$N05A)
table(check$N06A)

validate$pred_bipolar_6 <- ifelse(validate$pred_prob > 0.6, 1, 0)
validate$pred_bipolar_7 <- ifelse(validate$pred_prob > 0.7, 1, 0)
validate$pred_bipolar_8 <- ifelse(validate$pred_prob > 0.8, 1, 0)
validate$pred_bipolar_9 <- ifelse(validate$pred_prob > 0.9, 1, 0)

table(validate$hoofd_vektis_bipolar_and_mood_2015_2016) 
# 8107 

table(validate$detailed_bipolar_allyears) # 5768 actual bipolar cases
table(validate$pred_bipolar_6) # 4125 cases with 0.6 cutoff 
table(validate$pred_bipolar_7) # 3982 cases with 0.7 cutoff 
table(validate$pred_bipolar_8) # 3203 cases with 0.8 cutoff 
table(validate$pred_bipolar_9) # 1271 cases with 0.9 cutoff 

## check mismatch
validate <- validate %>%
  mutate(mismatch_6 = ifelse(detailed_bipolar_allyears != pred_bipolar_6, 1, 0),
         mismatch_7 = ifelse(detailed_bipolar_allyears != pred_bipolar_7, 1, 0),
         mismatch_8 = ifelse(detailed_bipolar_allyears != pred_bipolar_8, 1, 0),
         mismatch_9 = ifelse(detailed_bipolar_allyears != pred_bipolar_9, 1, 0)
         )

table(validate$mismatch_6)
table(validate$mismatch_7)
table(validate$mismatch_8)
table(validate$mismatch_9)


## auc to report
roc_obj <- roc(validate$detailed_bipolar_allyears, validate$pred_prob)
auc(roc_obj)
# 0.7489

## form result tables
thresholds <- c(0.6, 0.7, 0.8, 0.9)

results <- lapply(thresholds, function(t) {
  
  # classify predicted cases/controls 
  pred_class <- ifelse(validate$pred_prob >=t, 1, 0)
  
  # create 2x2 tables
  tab <- table(
    Predicted = pred_class,
    Actual = validate$detailed_bipolar_allyears
  )
  
  # extract counts
  TP <- tab["1", "1"]
  FP <- tab["1", "0"]
  TN <- tab["0", "0"]
  FN <- tab["0", "1"]
  
  # calculate performance measures 
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  PPV <- TP / (TP + FP)
  NPV <- TN / (TN + FN)
  
  data.frame(
    Threshold = t, 
    TP = TP, 
    FP = FP, 
    TN = TN, 
    FN = FN, 
    Sensitivity = sensitivity, 
    Specificity = specificity,
    PPV = PPV, 
    NPV = NPV
  )
})

results <- do.call(rbind, results)

results

write.csv(results, "./Results/Supplementary_Results/AllDatasets_Prediction_Validation.csv", row.names=FALSE)


# 6. Prediction Dataset (2017 - 2021) ####

df_2017_21 <- df %>%
  mutate(hoofd_vektis_bipolar_and_mood_2017_2021 = ifelse(df$bipolar_hoofd_2017 | df$bipolar_hoofd_2018 | 
                                                            df$bipolar_hoofd_2019 | df$bipolar_hoofd_2020 |
                                                            df$bipolar_hoofd_2021 | 
                                                            df$bipolar_vektis_2017 | df$bipolar_vektis_2018 | 
                                                            df$bipolar_vektis_2019 | df$bipolar_vektis_2020 | 
                                                            df$bipolar_vektis_2021 == 1, 1, 0)) %>%
  filter(hoofd_vektis_bipolar_and_mood_2017_2021 == 1) 

nrow(df_2017_21) # 46,988
table(df_2017_21$detailed_bipolar_allyears)
# FALSE  TRUE 
# 23210 23778 

## filter out those who have a diagnosis in the detailed dataset already
df_2017_21 <- df_2017_21 %>%
  filter(detailed_bipolar_allyears == 0)
nrow(df_2017_21) # 23,210


## predict
df_2017_21$pred_prob <- predict(model, newdata = df_2017_21, type = "response")

df_2017_21$pred_bipolar_6 <- ifelse(df_2017_21$pred_prob > 0.6, 1, 0)
df_2017_21$pred_bipolar_7 <- ifelse(df_2017_21$pred_prob > 0.7, 1, 0)
df_2017_21$pred_bipolar_8 <- ifelse(df_2017_21$pred_prob > 0.8, 1, 0)
df_2017_21$pred_bipolar_9 <- ifelse(df_2017_21$pred_prob > 0.9, 1, 0)

table(df_2017_21$hoofd_vektis_bipolar_and_mood_2017_2021) 
# 23,210  

table(df_2017_21$pred_bipolar_6) # 10027 cases with 0.6 cutoff 
table(df_2017_21$pred_bipolar_7) # 3982 cases with 0.7 cutoff 
table(df_2017_21$pred_bipolar_8) # 3203 cases with 0.8 cutoff 
table(df_2017_21$pred_bipolar_9) # 1271 cases with 0.9 cutoff 


# 7. Make Final Dataset ####

## there are some 'bipolar and other mood disorder' diagnoses in 2015 and 2016. I'm leaving these out since the detailed dataset has these diagnoses
## I am using the final bipolar diag file I created to use in analysis 

#* 7.1. Create weights using predictive probability ----
df_2017_21 <- df_2017_21 %>%
  select(RINPERSOON, pred_prob)

colnames(df_2017_21) <- c("RINPERSOON", "pred_prob_2017_21")

gba_allyears_full_diagnoses <- gba_allyears_full_diagnoses %>%
  left_join(df_2017_21, by = "RINPERSOON")

nrow(gba_allyears_full_diagnoses) # 97605

gba_allyears_full_diagnoses$alldatasets_bipolar_allyears <- ifelse(gba_allyears_full_diagnoses$detailed_bipolar_allyears | 
                                                                   gba_allyears_full_diagnoses$hoofd_bipolar_2017_21 | 
                                                                   gba_allyears_full_diagnoses$vektis_bipolar_2017_21 == 1, 1, 0)

table(gba_allyears_full_diagnoses$alldatasets_bipolar_allyears) # 67,139  with total diagnoses
table(gba_allyears_full_diagnoses$detailed_bipolar_allyears) # 43,929 of them have a sure diagnosis of bipolar disorder 
# these people will have a predictive probability of 1.
# the rest will be assigned the pred_prob

sum(!is.na(gba_allyears_full_diagnoses$pred_prob_2017_21)) # 23,210


## create 'weights' variable
gba_allyears_full_diagnoses <- gba_allyears_full_diagnoses %>%
  mutate(bipolar_diag_weights = case_when(
    detailed_bipolar_allyears == 1 ~ 1,
    !is.na(pred_prob_2017_21) ~ pred_prob_2017_21,
    TRUE ~ NA_real_
  ))

nrow(gba_allyears_full_diagnoses) # 97,605
table(gba_allyears_full_diagnoses$alldatasets_bipolar_allyears) # 67,139  with total diagnoses
sum(!is.na(gba_allyears_full_diagnoses$bipolar_diag_weights)) # 67,139 weights
describe(gba_allyears_full_diagnoses$bipolar_diag_weights) # 67,139 

gba_allyears_full_diagnoses <- gba_allyears_full_diagnoses %>%
  select(-pred_prob_2017_21) 


#* 7.2. Create weights using inverse probability weighting ----

## select required columns 
colnames(gba_allyears_full_diagnoses)
gba_allyears_full_diagnoses <- gba_allyears_full_diagnoses %>%
  select(RINPERSOON, bipolar_diag_weights)
nrow(gba_allyears_full_diagnoses) # 97,605
sum(!is.na(gba_allyears_full_diagnoses$bipolar_diag_weights)) # 67,139

## incorporate into the final bipolar disorder dataset 
#bipolar_diag <- fread("./Data/02_Reference_Data/20260521_bipolar_disorder_diagnosis.csv") 

nrow(bipolar_diag) # 28,336,778

bipolar_diag <- bipolar_diag %>%
  left_join(gba_allyears_full_diagnoses, by = "RINPERSOON")
sum(!is.na(bipolar_diag$bipolar_diag_weights)) # 67139
describe(bipolar_diag$bipolar_diag_weights, na.rm = TRUE)
hist(bipolar_diag$bipolar_diag_weights)


#* 7.3. Create clean bipolar_diag variable with cut-off ----

hist(bipolar_diag$bipolar_diag_weights)
table(bipolar_diag$bipolar_diag_weights)


bipolar_diag <- bipolar_diag %>% 
      mutate(bipolar_diag_cutoff_6 = ifelse(!is.na(bipolar_diag_weights) & bipolar_diag_weights >= 0.6, 1, 0),
             bipolar_diag_cutoff_7 = ifelse(!is.na(bipolar_diag_weights) & bipolar_diag_weights >= 0.7, 1, 0),
             bipolar_diag_cutoff_8 = ifelse(!is.na(bipolar_diag_weights) & bipolar_diag_weights >= 0.8, 1, 0),
             bipolar_diag_cutoff_9 = ifelse(!is.na(bipolar_diag_weights) & bipolar_diag_weights >= 0.9, 1, 0))

table(bipolar_diag$alldatasets_bipolar_diag) # 67139 
table(bipolar_diag$detailed_bipolar_diag) # 43929 
table(bipolar_diag$bipolar_diag_cutoff_6) # 53956 
table(bipolar_diag$bipolar_diag_cutoff_7) # 53462
table(bipolar_diag$bipolar_diag_cutoff_8) # 51138 
table(bipolar_diag$bipolar_diag_cutoff_9) # 46176 


write.csv(bipolar_diag, "./Data/02_Reference_Data/20260521_bipolar_disorder_diagnosis.csv", row.names = F)  
# bipolar_diag <- read.csv("./Data/02_Reference_Data/20260521_bipolar_disorder_diagnosis.csv")

rm(df_2015_16, df_2017_21, df, train, validate, validate_young,
   roc_obj, roc_obj_6, roc_obj_7, roc_obj_8, roc_obj_9)
rm(gba_allyears_full_diagnoses)

