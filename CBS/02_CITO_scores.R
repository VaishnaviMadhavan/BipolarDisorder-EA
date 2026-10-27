
#
# Script: 02_CITO_scores.R
# Project: Bipolar Disorder and Education:
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Script to extract and clean CITO test scores
#
# Author: Vaishnavi K Madhavan
# Start date: 29-10-2025
# 

# 1. Set Up ####
cito_2021 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2021V2.sav")
cito_2019 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2019V3.sav")
cito_2018 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2018V5.sav")
cito_2017 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2017V4.sav")
cito_2016 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2016V3.sav")
cito_2015 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2015V3.sav")
cito_2014 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2014V2.sav")
cito_2013 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2013V2.sav")
cito_2012 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2012V2.sav")
cito_2011 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2011V2.sav")
cito_2010 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2010V2.sav")
cito_2009 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2009V2.sav")
cito_2008 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2008V2.sav")
cito_2007 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2007V2.sav")
cito_2006 <- read_sav("G:/Onderwijs/CITOTAB/CITOTAB2006V2.sav")


colnames(cito_2006)
colnames(cito_2013)
colnames(cito_2015)
colnames(cito_2014)
## Note: not all columns exist in all files

## column names and translation:
# "RINPERSOONS" = Presence in GBA (the file with demographic information). Key: R=present in GBA, G=not in GBA, D=dead at birth, N=Unknown 
# "RINPERSOON" = Individual ID
# "CitoToetsID" = CITO Test ID
# "CitoScoreTaal" = Language Score
# "CitoPercentieltaal" = Language Percentile 
# "CitoZScoreTaal" = Language Z score
# "CitoScoreRekenenWiskunde" = Math Score      
# "CitoPercentielRekenenWiskunde" = Math Percentile 
# "CitoZScoreRekenenWiskunde" = Math Z Score      
# "CitoScoreStudieVaardigheden" = Study Skills Score    
# "CitoPercentielStudievaardigh"  = Study Skills Percentile  
# "CitoZScoreStudievaardigh" = Study Skills Z Score      
# "CitoScoreWereldorientatie" = World Orientation Score      
# "CitoPercentielWereldorientatie" = World Orientation Percentile 
# "CitoScoreTotaalExclWereldo" = Excluding World Orientation Score      
# "CitoPercentielTotalExclWereldo" = Excluding World Orientation Percentile  
# "CitoScoreTotaalInclWereldo" = Including World Orientation Score     
# "CitoPercentielTotalInclWereldo" = Including World Orientation Percentile 
# "CitoStandaardScore" = Standard Total Score             
# "CitoBRIN_crypt" = encrypted BRIN code of the educational institution               
# "CitoLeerlingGewicht" = Student Weight           
# "CitoThuistaal" = Home Language                 
# "CitoAdviesLeerkracht" = Teacher's school type advice           
# "CitoSchooltypeadvies" = School type advice based on standard score           
# "CitoBasisonderwijs" = Funded primary education?              
# "CitoToetsVariant" = CITO test variant


# 2006 - 2012: need to change the following:
# RINPersoons into RINPERSOONS
# RINPersoon to RINPERSOON

# 2013 - 2014: need to change the following:
# RINPersoonS into RINPERSOONS
# RINPersoon to RINPERSOON

# 2015 - 2021 are similar (RINPERSOONS, RINPERSOON)

## add TestYear column and merge all files (by rows)
years <- setdiff(2006:2021, 2020)

cols_to_keep <- c("RINPERSOONS", "RINPERSOON", "CitoToetsID", 
                  "CitoScoreTaal", "CitoPercentieltaal", "CitoZScoreTaal",
                  "CitoScoreRekenenWiskunde", "CitoPercentielRekenenWiskunde", "CitoZScoreRekenenWiskunde",
                  "CitoScoreStudieVaardigheden", "CitoPercentielStudievaardigh", "CitoZScoreStudievaardigh",      
                  "CitoScoreWereldorientatie", "CitoPercentielWereldorientatie", "CitoScoreTotaalExclWereldo",
                  "CitoPercentielTotalExclWereldo", "CitoScoreTotaalInclWereldo", "CitoPercentielTotalInclWereldo",
                  "CitoStandaardScore", "CitoBRIN_crypt", "CitoLeerlingGewicht", "CitoThuistaal")        


data_list <- lapply(years, function(y) {
  df <- get(paste0("cito_", y)) 
  
  # for 2006-2012, rename columns
  if(y>=2006 && y<=2012) {
    df <- df %>%
      rename(
        RINPERSOONS = RINPersoons,
        RINPERSOON = RINPersoon
      )
  }
  
  # for 2013 and 2014, rename columns
  if(y == 2013 | y == 2014) {
    df <- df %>%
      rename(
        RINPERSOONS = RINPersoonS,
        RINPERSOON = RINPersoon
      )
  }
  
  # Add TestYear and select only desired columns
  df <- df %>%
    mutate(TestYear = y) %>%
    select(any_of(c(cols_to_keep, "TestYear")))
  
  return(df)
}) 


cito_all <- bind_rows(data_list)
nrow(cito_all) # 2,047,222

  
# 2. Clean Dataset ####

#* 2.1. Deal with NAs ----
colSums(is.na(cito_all)) 
# NAs present in RINPERSOONS and RINPERSOON
# NAs present in CitoPercentieltaal but not CitoScoreTaal

## deal with NAs in RINPERSOONS and RINPERSOON first 
cito_all <- cito_all[!is.na(cito_all$RINPERSOON) & !is.na(cito_all$RINPERSOONS), ] 
sum(is.na(cito_all$RINPERSOON))
nrow(cito_all) # 2,047,222

## some people have blank RINPERSOON. Check 
blanks <- cito_all %>%
            filter(RINPERSOON == "")
table(blanks$RINPERSOONS) # all G which need to be removed anyway

# key for RINPERSOONS column: R is in GBA, G is not in GBA 
cito_all <- cito_all[which(cito_all$RINPERSOONS == 'R'), ]
nrow(cito_all) # 1,951,180

## check CitoScoreTaal
summary(cito_all$CitoScoreTaal) 
sum(cito_all$CitoScoreTaal == 0) # 2 (are these actually 0s or NA mistaken as 0?)

inspect_zeros <- cito_all[(cito_all$CitoScoreTaal == 0), ]
inspect_zeros
## looks sus. Let's remove these
cito_all <- cito_all[!(cito_all$CitoScoreTaal == 0), ]
nrow(cito_all) # 1,951,178


#* 2.2. Repeated Tests ----
## take the scores from when the individual has taken it the first time 
cito_all %>%
  count(RINPERSOON) %>%
  count(n) # 5332 repeated twice and 10 repeated thrice

repeated_RINPERSOON <- cito_all %>%
  group_by(RINPERSOON) %>%
  filter(n() > 1) %>%
  ungroup()
nrow(repeated_RINPERSOON) # 10,694 (checks out)

repeated_RINPERSOON %>%
  count(RINPERSOON) %>%
  count(n)

cito_all <- cito_all %>%
  arrange(RINPERSOON, TestYear) %>%
  group_by(RINPERSOON) %>%
  slice(1) %>%
  ungroup() 

nrow(cito_all) # 1,945,826
nrow(repeated_RINPERSOON) # 10,694


#* 2.3. Check birthyear of those with CITO Scores Tests ----

gba <- fread("G:/Bevolking/GBAPERSOONTAB/2024/GBAPERSOON2024TABV3.csv") %>%
  select(RINPERSOON, GBAGEBOORTEJAAR)
gba$RINPERSOON <- as.numeric(gba$RINPERSOON)
cito_all$RINPERSOON <- as.numeric(cito_all$RINPERSOON)

cito_all <- gba %>%
  full_join(cito_all, by = "RINPERSOON")
nrow(cito_all) # 28,336,779

cito_all <- cito_all %>%
  filter(!is.na(CitoZScoreTaal) | !is.na(CitoZScoreRekenenWiskunde) | 
           !is.na(CitoZScoreStudievaardigh) | !is.na(CitoStandaardScore)) 

nrow(cito_all) # 1,945,826
table(cito_all$GBAGEBOORTEJAAR)

count_per_testyear <- cito_all %>%
  count(TestYear, GBAGEBOORTEJAAR)

## we only need those who took the test when they were between 11 and 14 years old
cito_all <- cito_all %>%
  mutate(age_at_test = TestYear - GBAGEBOORTEJAAR) %>%
  filter(between(age_at_test, 11, 14))
nrow(cito_all) # 1,944,187 (removed 1639)

count_per_testyear <- cito_all %>%
  count(TestYear, GBAGEBOORTEJAAR)

#write_csv(cito_all, "./Data/02_Reference_Data/20260213_cito_scores.csv")


#* 2.4. make zscores for standaard score ----

## using CitoStandaardScore
hist(cito_all$CitoStandaardScore) # not normally distributed

## check whether standardising per year and standardising overall is better 
check <- cito_all
check$std_standard_overall <- scale(cito_all$CitoStandaardScore)
check <- check %>%
  group_by(TestYear) %>%
  mutate(cito_standard_peryear = scale(CitoStandaardScore)
  ) %>% ungroup()

hist(check$std_standard_overall)
hist(check$cito_standard_peryear)
describe(check$std_standard_overall)
describe(check$cito_standard_peryear)
cor(check$std_standard_overall, check$cito_standard_peryear) # 0.998
rm(check)

# let's do it per year since the z scores of the individuals tests are standardised per year
cito_all <- cito_all %>%
  group_by(TestYear) %>%
  mutate(std_CitoStandaardScore = as.numeric(scale(CitoStandaardScore))) %>%
  ungroup()

ggplot(cito_all, aes(x=CitoStandaardScore)) + 
  geom_histogram() + 
  facet_wrap(~ TestYear)

ggplot(cito_all, aes(x=std_CitoStandaardScore)) + 
  geom_histogram() + 
  facet_wrap(~ TestYear)

write_csv(cito_all, "./Data/02_Reference_Data/20260213_cito_scores.csv")


# 3. Check Descriptives ####
#cito_all <- fread("./Data/02_Reference_Data/20260213_cito_scores.csv")

## required columns:
# "RINPERSOONS" = Presence in GBA
# "RINPERSOON" = Individual ID
# "CitoScoreTaal" = Language Score 
# "CitoZScoreTaal" = Language Z score
# "CitoScoreRekenenWiskunde" = Math Score      
# "CitoZScoreRekenenWiskunde" = Math Z Score      
# "CitoScoreStudieVaardigheden" = Study Skills Score    
# "CitoZScoreStudievaardigh" = Study Skills Z Score      
# "CitoScoreWereldorientatie" = World Orientation Score  (not compulsory for all studenbts. Exclude)    
# "CitoScoreTotaalExclWereldo" = Excluding World Orientation Score      
# "CitoPercentielTotalExclWereldo" = Excluding World Orientation Percentile  
# "CitoScoreTotaalInclWereldo" = Including World Orientation Score (Exclude)    
# "CitoPercentielTotalInclWereldo" = Including World Orientation Percentile (Exclude)
# "CitoStandaardScore" = Standard Total Score   
# TestYear

## do we need these: 
# "CitoBRIN_crypt" = encrypted BRIN code of the educational institution               
# "CitoThuistaal" = Home Language                 
# "CitoToetsID" = CITO Test ID

cito_all <- cito_all %>% 
               select(RINPERSOONS, RINPERSOON, TestYear,
                      CitoScoreTaal, CitoZScoreTaal, CitoPercentieltaal,
                      CitoScoreRekenenWiskunde, CitoZScoreRekenenWiskunde, CitoPercentielRekenenWiskunde,
                      CitoScoreStudieVaardigheden, CitoZScoreStudievaardigh, CitoPercentielStudievaardigh,
                      CitoScoreTotaalExclWereldo, CitoPercentielTotalExclWereldo, 
                      CitoStandaardScore, std_CitoStandaardScore)


#* 3.1. check individual subject scores and distributions ---- 

test_score_cols <- c("CitoScoreTaal", 
                "CitoScoreRekenenWiskunde",
                "CitoScoreStudieVaardigheden",
                "CitoScoreTotaalExclWereldo")

## these are the number of correct answers on the test. 
## number of questions varies from year to year (not comparable across years)
## 0-100 until 2014 and then after that, it varies every year
## CitoScoreTotaalExclWereldo = number of correct answers in language, math and study skills 
## CitoStandaardScore = conversion of the total score into a scale ranging from 501-550 using a formula that is adjusted annually based on the test's difficulty (comparable from year to year)


#** 3.1.1. Histograms (check) ----

## check histograms 
cito_long <- cito_all %>%
               pivot_longer(
                 cols = c(CitoScoreTaal, 
                          CitoScoreRekenenWiskunde,
                          CitoScoreStudieVaardigheden,
                          CitoScoreTotaalExclWereldo,
                          CitoZScoreTaal, 
                          CitoZScoreRekenenWiskunde,
                          CitoZScoreStudievaardigh,
                          CitoStandaardScore),
                 names_to = "TestName", 
                 values_to = "Score")

subjects <- c("CitoScoreTaal", 
              "CitoScoreRekenenWiskunde",
              "CitoScoreStudieVaardigheden",
              "CitoScoreTotaalExclWereldo")
  
save_path <- "H:/Bipolar-EA/Results/"

for(subj in subjects) { 
  p <- cito_long %>%
    filter(TestName == subj) %>%
    ggplot(aes(x = Score)) + 
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) + 
    facet_wrap(~ TestYear, scales = "free") + 
    labs(
      title = paste("CITO Score Distribution -", subj),
      x = "Score", 
      y = "Count"
    ) + 
    theme_minimal()
  
  ggsave(
    filename = paste0(save_path, subj, "_hist", ".png"),
    plot = p,
    width = 10, height = 6, dpi = 300
  )}


ggplot(cito_long, aes(x = Score, fill = factor(TestYear))) + 
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) + 
  facet_wrap(~ TestName, scales = "free") + 
  labs(
    title = "CITO Score Distributions by Test within each Year", 
    x = "Score", 
    y = "Count", 
    fill = "Test Year"
  ) + 
  theme_minimal() 


#** 3.1.2. Filter out invalid scores ----

## in 2018, some scores are given 999. This is invalid and needs to be removed. 
## first inspect those with 999 value 
check_999 <- cito_all %>%
  summarise(
    lang_999 = sum(CitoScoreTaal > 200, na.rm = TRUE),
    math_999 = sum(CitoScoreRekenenWiskunde > 200, na.rm = TRUE),
    study_999 = sum(CitoScoreStudieVaardigheden > 200, na.rm = TRUE),
    total_999 = sum(CitoScoreTotaalExclWereldo > 200, na.rm = TRUE),
  )

check_999


check_999 <- cito_all %>%
  filter(
    (CitoScoreTaal > 200),
    (CitoScoreRekenenWiskunde > 200) | 
    (CitoScoreStudieVaardigheden > 200) | 
    (CitoScoreTotaalExclWereldo > 200)
  )

table(check_999$TestYear)

## replace all of these with NA 
cito_all_no999 <- cito_all %>%
  mutate(
    CitoScoreTaal = ifelse(CitoScoreTaal > 200, NA, CitoScoreTaal),
    CitoScoreRekenenWiskunde = ifelse(CitoScoreRekenenWiskunde > 200, NA, CitoScoreRekenenWiskunde),
    CitoScoreStudieVaardigheden = ifelse(CitoScoreStudieVaardigheden > 200, NA, CitoScoreStudieVaardigheden),
    CitoScoreTotaalExclWereldo = ifelse(CitoScoreTotaalExclWereldo > 200, NA, CitoScoreTotaalExclWereldo),
  )


check_999 <- cito_all_no999 %>%
  summarise(
    lang_999 = sum(CitoScoreTaal > 200, na.rm = TRUE),
    math_999 = sum(CitoScoreRekenenWiskunde > 200, na.rm = TRUE),
    study_999 = sum(CitoScoreStudieVaardigheden > 200, na.rm = TRUE),
    total_999 = sum(CitoScoreTotaalExclWereldo > 200, na.rm = TRUE),
  )

check_999

## check NAs
check_NA <- cito_all_no999 %>%
  summarise(
    lang_999 = sum(is.na(CitoScoreTaal)),
    math_999 = sum(is.na(CitoScoreRekenenWiskunde)),
    study_999 = sum(is.na(CitoScoreStudieVaardigheden)),
    total_999 = sum(is.na(CitoScoreTotaalExclWereldo))
  )

check_999

cito_all <- cito_all_no999
rm(cito_all_no999)


#** 3.1.3. Histograms (final without invalid scores) ----
cito_long_no999 <- cito_all %>%
  pivot_longer(
    cols = c(CitoScoreTaal, 
             CitoScoreRekenenWiskunde,
             CitoScoreStudieVaardigheden,
             CitoScoreTotaalExclWereldo),
    names_to = "TestName", 
    values_to = "Score")

for(subj in subjects) { 
  p2 <- cito_long_no999 %>%
    filter(TestName == subj) %>%
    ggplot(aes(x = Score)) + 
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) + 
    facet_wrap(~ TestYear, scales = "free") + 
    labs(
      title = paste("CITO Score Distribution -", subj),
      x = "Score", 
      y = "Count"
    ) + 
    theme_minimal()
  
  ggsave(
    filename = paste0(save_path, subj, "_hist_no999", ".png"),
    plot = p2,
    width = 10, height = 6, dpi = 300
  )}

## CitoScoreStudieVaardigheden missing for 2015 - 2021 
## check original dataset
summary(cito_2016$CitoScoreStudieVaardigheden)
summary(cito_2018$CitoScoreStudieVaardigheden)
summary(cito_2021$CitoScoreStudieVaardigheden)

# checks out - not available for 2015 - 2021


#** 3.1.4. Descriptive tables ----

## descriptive information for each score within each year
cito_descriptives <- cito_long_no999 %>%
  group_by(TestYear, TestName) %>%
  summarise(
    n = n(),
    min = min(Score, na.rm = TRUE),
    max = max(Score, na.rm = TRUE),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    range = max - min,
    .groups = "drop"
  )

# ignore warning coming from NAs in CitoScoreStudieVaardigheden from 2015

write_csv(cito_descriptives, "./Results/CITO_descriptives_20251201.csv") 


#* 3.2. Check individual subject Z scores and distributions ---- 

## scores standaridised within each year
## available until 2014
## for 2015 - 2017, only filled in for students attending funded primary education  (CitoBasisonderwijs = "1"). 999 for others
## not available from 2018 

#** 3.2.1. histograms  (check) ----
subjects <- c("CitoZScoreTaal", 
              "CitoZScoreRekenenWiskunde",
              "CitoZScoreStudievaardigh",
              "CitoStandaardScore")

save_path <- "H:/Bipolar-EA/Results/"

for(subj in subjects) { 
  p <- cito_long %>%
    filter(TestName == subj) %>%
    ggplot(aes(x = Score)) + 
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) + 
    facet_wrap(~ TestYear, scales = "free") + 
    labs(
      title = paste("CITO Z-Score Distribution -", subj),
      x = "Z-Score", 
      y = "Count"
    ) + 
    theme_minimal()
  
  ggsave(
    filename = paste0(save_path, subj, "_hist", ".png"),
    plot = p,
    width = 10, height = 6, dpi = 300
  )}

ggplot(cito_long, aes(x = Score, fill = factor(TestYear))) + 
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) + 
  facet_wrap(~ TestName, scales = "free") + 
  labs(
    title = "CITO Z-Score Distributions by Test within each Year", 
    x = "Z-Score", 
    y = "Count", 
    fill = "Test Year"
  ) + 
  theme_minimal() 


#** 3.2.2. filter out invalid scores ----

table(cito_all$CitoZScoreRekenenWiskunde)
## in 2018, some scores are given 99.9999 This is invalid and needs to be removed. 
## first inspect those with 99 value 
check_Z_99 <- cito_all %>%
  summarise(
    lang_Z_99 = sum(CitoZScoreTaal > 50, na.rm = TRUE),
    math_Z_99 = sum(CitoZScoreRekenenWiskunde > 50, na.rm = TRUE),
    study_Z_99 = sum(CitoZScoreStudievaardigh > 50, na.rm = TRUE)
  )

check_Z_99
#   lang_99   math_99   
#    4633      4633          

check_99 <- cito_all %>%
  filter(
    (CitoZScoreTaal > 50),
    (CitoZScoreRekenenWiskunde > 50) | 
      (CitoZScoreStudievaardigh > 50)
    )

table(check_99$TestYear)

## replace all of these with NA 
cito_all_no99 <- cito_all %>%
  mutate(
    CitoZScoreTaal = ifelse(CitoZScoreTaal > 50, NA, CitoZScoreTaal),
    CitoZScoreRekenenWiskunde = ifelse(CitoZScoreRekenenWiskunde > 50, NA, CitoZScoreRekenenWiskunde),
    CitoZScoreStudievaardigh = ifelse(CitoZScoreStudievaardigh > 50, NA, CitoZScoreStudievaardigh)
    )

check_99 <- cito_all_no99 %>%
  summarise(
    lang_99 = sum(CitoZScoreTaal > 50, na.rm = TRUE),
    math_99 = sum(CitoZScoreRekenenWiskunde > 50, na.rm = TRUE),
    study_99 = sum(CitoZScoreStudievaardigh > 50, na.rm = TRUE)
  )

check_99

cito_all <- cito_all_no99
rm(cito_all_no99)


#** 3.2.3. histograms (final without invalid scores) ----
cito_long_no99 <- cito_all %>%
  pivot_longer(
    cols = c(CitoZScoreTaal, 
             CitoZScoreRekenenWiskunde,
             CitoZScoreStudievaardigh,
             CitoStandaardScore),
    names_to = "TestName", 
    values_to = "Score")

for(subj in subjects) { 
  p2 <- cito_long_no99 %>%
    filter(TestName == subj) %>%
    ggplot(aes(x = Score)) + 
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) + 
    facet_wrap(~ TestYear, scales = "free") + 
    labs(
      title = paste("CITO Z-Score Distribution -", subj),
      x = "Z-Score", 
      y = "Count"
    ) + 
    theme_minimal()
  
  ggsave(
    filename = paste0(save_path, subj, "_hist_no99", ".png"),
    plot = p2,
    width = 10, height = 6, dpi = 300
  )}


#** 3.2.4. Descriptive tables ----
## descriptive information for each score within each year
cito_descriptives_zscore <- cito_long_no99 %>%
  group_by(TestYear, TestName) %>%
  summarise(
    n = n(),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    .groups = "drop"
  )

# ignore warning coming from NAs in CitoScoreStudieVaardigheden from 2015

write_csv(cito_descriptives_zscore, "./Results/CITO_descriptives_zscores_20251201.csv") 


# 4. Score Vs. Z-Score Comparison ####
## scores - the total number of correct answers (varies by year)
## z-scores - standardised score provided in the dataset 
## aim: to check whether the z-scores I form by standardising within each year is comparable to the z-scores provided 

#cito_all <- fread("./Data/02_Reference_Data/20260213_cito_scores.csv")

#* 4.1. Form z-scores ----
 
## use these columns 
test_score_cols <- c("CitoScoreTaal", 
                     "CitoScoreRekenenWiskunde",
                     "CitoScoreStudieVaardigheden")

# if not numeric, 
# cito_all <- data.frame(lapply(cito_all[ , test_score_cols], as.numeric))

cito_all <- cito_all %>%
  group_by(TestYear) %>%
  mutate(std_CitoScoreTaal = as.numeric(scale(CitoScoreTaal)),
         std_CitoScoreRekenenWiskunde = as.numeric(scale(CitoScoreRekenenWiskunde)),
         std_CitoScoreStudieVaardigheden = as.numeric(scale(CitoScoreStudieVaardigheden)),
         std_CitoScoreTotaalExclWereldo = as.numeric(scale(CitoScoreTotaalExclWereldo)),
         ) %>%
  ungroup()


#* 4.2. Check correlation ---- 

## use these columns 
all_score_cols <- c(
                  "std_CitoScoreTaal",
                  "CitoZScoreTaal",
                  "std_CitoScoreRekenenWiskunde",
                  "CitoZScoreRekenenWiskunde",
                  "std_CitoScoreStudieVaardigheden",
                  "CitoZScoreStudievaardigh")

# if not numeric, 
# cito_all <- data.frame(lapply(cito_all[ , all_score_cols], as.numeric))

cor_matrix <- cor(
  cito_all[ , all_score_cols],
  use = "pairwise.complete.obs"
)

View(cor_matrix) # check correlation between my own z score and their z score
# 0.99 correlation across all tests


# 5. Percentile Score Vs. Z-score (Check Correlations) ####
## percentile scores - the percentile in each year
## z-scores - standardised score provided in the dataset 
## aim: to check whether the z-scores I form using the precentiles is comparable to the z-scores provided 

#cito_all <- fread("./Data/02_Reference_Data/20260213_cito_scores.csv")

## check percentiles
describe(cito_all$CitoPercentieltaal)
describe(cito_all$CitoPercentielRekenenWiskunde)
describe(cito_all$CitoPercentielStudievaardigh)

hist(cito_all$CitoPercentieltaal)

## technically, 0 and 100 shouldn't exist, so let's check these
cito_taal0 <- cito_all %>%
  filter(CitoPercentieltaal == 99)
table(cito_taal0$CitoScoreTaal)


#* 5.1. Form z-scores ---- 

## recode 0 and 100 alone
cito_all <- cito_all %>%
  mutate(CitoPercentieltaal_recode = case_when(
    CitoPercentieltaal == 0 ~ 0.25,
    CitoPercentieltaal == 100 ~ 99.75,
    TRUE ~ CitoPercentieltaal
  ) ) %>%
    mutate(CitoPercentielRekenenWiskunde_recode = case_when(
      CitoPercentielRekenenWiskunde == 0 ~ 0.25,
      CitoPercentielRekenenWiskunde == 100 ~ 99.75,
      TRUE ~ CitoPercentielRekenenWiskunde
    )
  ) %>%
  mutate(CitoPercentielStudievaardigh_recode = case_when(
    CitoPercentielStudievaardigh == 0 ~ 0.25,
    CitoPercentielStudievaardigh == 100 ~ 99.75,
    TRUE ~ CitoPercentielStudievaardigh
  )
  )

cito_all <- cito_all %>%
  mutate(CitoPercentieltaal_Z  = qnorm(CitoPercentieltaal_recode /100),
         CitoPercentielRekenenWiskunde_Z  = qnorm(CitoPercentielRekenenWiskunde_recode /100),
         CitoPercentielStudievaardigh_Z  = qnorm(CitoPercentielStudievaardigh_recode /100))

all_score_cols <- c("CitoZScoreTaal",
                    "CitoPercentieltaal_Z",
                    "CitoZScoreRekenenWiskunde",
                    "CitoPercentielRekenenWiskunde_Z",
                    "CitoZScoreStudievaardigh",
                    "CitoPercentielStudievaardigh_Z")

cor_matrix <- cor(
  cito_all[ , ..all_score_cols],
  use = "pairwise.complete.obs")
cor_matrix
## they are highly correlated (0.97 to 0.98). Not much difference using percentiles versus z-scores. 


# 6. Descriptives ####

#cito_all <- fread("./Data/02_Reference_Data/20260213_cito_scores.csv")

#* 6.1. Entire Population ---- 

cito_long <- cito_all %>%
  select(RINPERSOON, TestYear, 
         CitoScoreTaal, CitoScoreRekenenWiskunde, CitoScoreStudieVaardigheden, CitoScoreTotaalExclWereldo, CitoStandaardScore,
         CitoZScoreTaal, CitoZScoreRekenenWiskunde, CitoZScoreStudievaardigh, std_CitoStandaardScore) %>%
  pivot_longer(
    cols = c(CitoScoreTaal, CitoScoreRekenenWiskunde, CitoScoreStudieVaardigheden, CitoScoreTotaalExclWereldo, CitoStandaardScore, 
             CitoZScoreTaal, CitoZScoreRekenenWiskunde, CitoZScoreStudievaardigh, std_CitoStandaardScore),
    names_to = "TestName", 
    values_to = "Score")


cito_descriptives <- cito_long %>%
  group_by(TestYear, TestName) %>%
  summarise(
    n = n(),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE), 
    se = sd/sqrt(n),
    median = median(Score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sample = "Entire Population"
  )

check <- cito_long %>%
  filter(TestYear == 2015 | 
           TestYear == 2016 | 
           TestYear == 2017) %>%
  filter(TestName == "CitoScoreStudieVaardigheden" | 
           TestName == "CitoPercentielStudievaardigh" | 
           TestName == "CitoZScoreStudievaardigh")

sum(!is.na(check$Score))
# ignore warning coming from NAs in CitoScoreStudieVaardigheden beyond 2015

check <- cito_long %>%
  filter(TestYear > 2017) %>%
  filter(TestName == "CitoZScoreRekenenWiskunde" | 
           TestName == "CitoZScoreTaal")

sum(!is.na(check$Score))

check <- cito_all %>%
  filter(TestYear > 2017)

sum(!is.na(check$CitoZScoreRekenenWiskunde))
sum(!is.na(check$CitoZScoreTaal))
# some z-scores not available after 2017. Ignore NAs. 

rm(check)


#* 6.2. Population with valid data ----

# popdata <- fread("./Data/03_Final_Data/20260716_popdata_detailedonly.csv")

cito_descriptives_pop <- popdata %>%
  select(RINPERSOON) %>%
  left_join(cito_all, by = "RINPERSOON") %>%
  select(RINPERSOON, TestYear, 
         CitoScoreTaal, CitoScoreRekenenWiskunde, CitoScoreStudieVaardigheden, CitoScoreTotaalExclWereldo, CitoStandaardScore,
         CitoZScoreTaal, CitoZScoreRekenenWiskunde, CitoZScoreStudievaardigh, std_CitoStandaardScore) %>%
  pivot_longer(
    cols = c(CitoScoreTaal, CitoScoreRekenenWiskunde, CitoScoreStudieVaardigheden, CitoScoreTotaalExclWereldo, CitoStandaardScore, 
             CitoZScoreTaal, CitoZScoreRekenenWiskunde, CitoZScoreStudievaardigh, std_CitoStandaardScore),
    names_to = "TestName", 
    values_to = "Score")  %>%
  
  group_by(TestYear, TestName) %>%
  summarise(
    n = n(),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE), 
    se = sd/sqrt(n),
    median = median(Score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sample = "Population with Valid Data"
  )


#* 6.3. Population with valid data ---- 

# sibdata <- fread("./Data/03_Final_Data/20260716_sibdata_detailedonly.csv")

cito_descriptives_sib <- sibdata %>%
  select(RINPERSOON) %>%
  left_join(cito_all, by = "RINPERSOON") %>%
  select(RINPERSOON, TestYear, 
         CitoScoreTaal, CitoScoreRekenenWiskunde, CitoScoreStudieVaardigheden, CitoScoreTotaalExclWereldo, CitoStandaardScore,
         CitoZScoreTaal, CitoZScoreRekenenWiskunde, CitoZScoreStudievaardigh, std_CitoStandaardScore) %>%
  pivot_longer(
    cols = c(CitoScoreTaal, CitoScoreRekenenWiskunde, CitoScoreStudieVaardigheden, CitoScoreTotaalExclWereldo, CitoStandaardScore, 
             CitoZScoreTaal, CitoZScoreRekenenWiskunde, CitoZScoreStudievaardigh, std_CitoStandaardScore),
    names_to = "TestName", 
    values_to = "Score")  %>%
  
  group_by(TestYear, TestName) %>%
  summarise(
    n = n(),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE), 
    se = sd/sqrt(n),
    median = median(Score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sample = "Sibling"
  )

cito_descriptives_all <- rbind(cito_descriptives, cito_descriptives_pop, cito_descriptives_sib)

write_csv(cito_descriptives_all, "./Results/Descriptives/CITO_descriptives_peryear.csv") 

