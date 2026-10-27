
# 
# Script: 04_academic_performance.R 
# Project: Bipolar Disorder and Education: 
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Clean standardized national test scores from grades 5, 8, and 9
#
# Author: Vaishnavi K Madhavan
# Start date: 16-04-2025
# 

## script adapted from P.Demange

# 1. Set Up ####

data1 <- data.table::fread("N:/durable/data/registers/SSB/01_data/data_v5.0/EDUCATION_VGS_GRS/csv/EDUCATION_NASJONALE_PROVER.csv",
                          strip.white=TRUE) %>% 
  select(w19_0634_lnr, year = AARGANG, status = DELTATTSTATUS,
         test = PROVE, levelscore=MESTRINGSNIVAA, score = POENG, newscoring=SKALAPOENG)
colnames(data1)

data <- data.table::fread("N:/durable/data/registers/SSB/01_data/data_v6.1/EDUCATION_VGS_GRS/csv/EDUCATION_NASJONALE_PROVER.csv",
                          strip.white=TRUE) %>% 
         select(w19_0634_lnr, year = utd_skoleaar_start, status = deltattstatus,
           test = provekode, levelscore=mestringsnivaa, score = poeng, newscoring=skalapoeng,
           school_code = utd_skolekom)
nrow(data) # 8,422,350(more data!)
nrow(data1) # 7,911,960

## variable name translations
# w19_0634_lnr = the ID number used to identify each individual 
# utd_skoleaar_start = the year in which they took the test (useful for standardising within each year)
# deltattstatus = whether or not they attended the test
# provekode = subject name (NPENG = English, NPLES = Reading, NPREG = Mathematics)
# mestringsnivaa = level of mastery. Based on the score, the students are divided into 3 level for grade 5 and 5 levels for grade 8 and 9
# score = actual score 
# skalapoeng = change in scoring system from 2014. Available for only a subset of the sample


## check Skalapoeng Variable

cor.test(data$score, data$newscoring) # 0.92
sum(!is.na(data$score)) # 8,069,663
sum(!is.na(data$newscoring)) # 5,158,382

# more values are available for poeng (scores) than skalapoeng (newscoring). Drop newscoring 
data <- data %>%
  select(-newscoring)


# 2. Filter and Separate Grades ####

## remove tests which status is not attended
table(data$status)
# D = participated
# F = Exempt 
# I = Not Participated

data <- data %>% 
  filter(status == "D") 
table(is.na(data$score)) 


#* 2.1. Grade 5 ----

table(data$test)
## select only Norwegian tests (drop tests in sami - aka NPLSA, NPSSA, NPNSA)

testname5 <- c("NPENG05",  "NPLES05", "NPREG05") 
grade5 <- data %>% 
  filter(test %in% testname5) 

nrow(grade5) # 3,034,706

## let's look at the scores
table(grade5$test) # not all students attend all 3 tests 
hist(grade5$score) # normally distributed 
table(grade5$score) # 0 means they have not attended the test. To be removed (n=1365)
table(is.na(grade5$score)) # 15 NAs to remove
grade5[is.na(grade5$score),]
table((grade5$score == NULL))
 
## remove missing 
grade5$score[grade5$score == 0] <- NA
table(is.na(grade5$score)) # 1380 NAs 

grade5 <- grade5 %>% 
  drop_na(score)     # NAs removed 

## check
nrow(grade5) # 3,033,326
table(is.na(grade5$score))
table(grade5$levelscore) 

## checking levelscore (denotes the level of mastery and ranges from 0-5)
table(grade5$levelscore) 
grade5[!(grade5$levelscore %in% c(1, 2, 3)), ]  # one NA but score exists, so we can keep
table((grade5$levelscore == NULL))


#* 2.2. Grade 8 ----

testname8 <- c("NPENG08",  "NPLES08", "NPREG08") 
grade8 <- data %>% 
  filter(test %in% testname8) 

nrow(grade8) # 3,178,428

## looking at the scores
hist(grade8$score)
table(grade8$score) # 1623 0s to remove (means they didn't attend the test)
table(is.na(grade8$score)) # 5 NAs to remove
grade8[is.na(grade8$score),]
table((grade8$score == NULL))

## remove missing 
grade8$score[grade8$score == 0] <- NA
table(is.na(grade8$score)) # 5 + 1623 0s = 1628 NAs 

grade8 <- grade8 %>% 
  drop_na(score)     # NAs removed 

## check
nrow(grade8) # 3,176,800
table(is.na(grade8$score))
table(grade8$levelscore) 

## looking at levelscores (indicates level of mastery and ranges from 0 to 5)
table(grade8$levelscore) 
grade8[!(grade8$levelscore %in% c(1, 2, 3, 4, 5)), ] # 2 NA but score exits, so I keep
table((grade8$levelscore == NULL))


#* 2.3. Grade 9 ----

testname9 <- c("NPLES09", "NPREG09")  # no ENG in grade 9
grade9 <- data %>% 
  filter(test %in% testname9) 

nrow(grade9) # 1,760,392 (1mil lesser than grade 8. why?)

## looking at the scores
table(grade9$test) # Math and Reading scores seems to be from different sets of people
hist(grade9$score)
table(grade9$score) # 980 0s (those who didn't attend the test)
table(is.na(grade9$score)) # 2 NAs to remove
grade9[is.na(grade9$score),]
table((grade9$score == NULL))

## remove missing 
grade9$score[grade9$score == 0] <- NA
table(is.na(grade9$score)) # 2 + 980 0s = 982 NAs 
grade9 <- grade9 %>% 
  drop_na(score)      # NAs removed

## check
nrow(grade9) # 1,759,410
table(is.na(grade9$score))
table(grade9$levelscore) 

## looking at the levelscores (indicates mastery level and ranges from 0-5)
table(grade9$levelscore) 
grade9[!(grade9$levelscore %in% c(1, 2, 3, 4, 5)), ] # 2 NA but score exists, so I keep it
table((grade9$levelscore == NULL))


# 3. Standardise Scores per Test per Year ####

## get the average and sd of score per year and per test
hist(grade5$year)
# month is given but there is only one test per year anyway 


#* 3.1 Grade 5 ----

average5 <- grade5 %>%
  group_by(year, test) %>%
  summarise(
    mean = mean(score),
    sd = sd(score))

results5 <- left_join(grade5, average5, by=c("year", "test"))
head(results5)

## standardize the scores per year and test 
results5$std_score <- (results5$score - results5$mean)/results5$sd

# ## clean year column (removing last 2 characters from the string)
# results5$year <- substr(results5$year, 1, nchar(results5$year) - 2)
## Note: I did this for data v5.0 because the year column was in the format YYYYMM 
## but in data V6.1, it is in the format YYYY, so this is not necessary

## check results by plotting 
ggplot(results5, aes(x = score)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(year ~ test)

ggplot(results5, aes(x = std_score)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(year ~ test)
# distribution vary quite a bit by years

hist(results5$score)
table(results5$levelscore, useNA="always") # no zero so everybody took the test
table(is.na(results5$year)) # 3,033,326 
nrow(results5) # 3,033,326 


#* 3.2 Grade 8 ----

## calculates the average for each test and each year 
average8 <- grade8 %>%
  group_by(year, test) %>%
  summarise(
    mean = mean(score),
    sd = sd(score))

results8 <- left_join(grade8, average8, by=c("year", "test"))

## standardize the scores per year and test 
results8$std_score <- (results8$score - results8$mean)/results8$sd

# ## clean year column (removing last 2 characters from the string)
# results8$year <- substr(results8$year, 1, nchar(results8$year) - 2)
## Note: I did this for data v5.0 because the year column was in the format YYYYMM 
## but in data V6.1, it is in the format YYYY, so this is not necessary

## check results by plotting 
ggplot(results8, aes(x = score)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(year ~ test)

ggplot(results8, aes(x = std_score)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(year ~ test)
# distribution vary quite a bit by years

hist(results8$score)
table(results8$levelscore, useNA="always") # no zero so everybody took the test
table(is.na(results8$year)) # 3,176,800
nrow(results8) # 3,176,800


#* 3.3 Grade 9 ----

## calculates the average for each test and each year 
average9 <- grade9 %>%
  group_by(year, test) %>%
  summarise(
    mean = mean(score),
    sd = sd(score))

results9 <- left_join(grade9, average9, by=c("year", "test"))

## standardize the scores per year and test 
results9$std_score <- (results9$score - results9$mean)/results9$sd

# ## clean year column (removing last 2 characters from the string)
# results9$year <- substr(results9$year, 1, nchar(results9$year) - 2)
## Note: I did this for data v5.0 because the year column was in the format YYYYMM 
## but in data V6.1, it is in the format YYYY, so this is not necessary

## check results by plotting 
ggplot(results9, aes(x = score)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(year ~ test)

ggplot(results9, aes(x = std_score)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(year ~ test)
# distribution vary quite a bit by years

hist(results9$score)
table(results9$levelscore, useNA="always") # no zero so everybody took the test
table(is.na(results9$year))
nrow(results9) # 1,759,410

rm(average5, average8, average9)


# 4. Duplicates: Students with 2 Scores for One Year ####

## Edit: Data V5.0 had students with multiple test scores within the same year, so I had this section to filter them 
## These multiples do not exist in Data V6.1, so I have deleted the part where I investigate them
## Because I had w19_0634_lnr numbers mentioned explicitly. 
## For this code, check Perline's code in durable/projects/Perline/2021_Children_siblings_MH/Parental psychopathology and school performance/scripts/02_01_get_school_performance


#* 4.1 Grade 5 ----

## values_ln=length give the number of duplicates when trying to reformat to wide format, duplicates based on same ID, year and status
colnames(results5)

## drop school_code first 
results5 <- results5 %>%
  select(-school_code)

duplicate5 <- pivot_wider(results5, names_from = test, 
                          values_from = c(levelscore, score, mean, sd, std_score), 
                          values_fn=length)
nrow(duplicate5) # 1,057,740
length(unique(duplicate5$w19_0634_lnr)) # 1,057,578
## some duplicate w19_0634_lnr numbers  exist 

## count the number of entries for each person for each subject.
## for example, if person 1 has 2 different math scores, it will say 2 in math_score 
table(duplicate5$levelscore_NPLES05) 
table(duplicate5$levelscore_NPENG05)
table(duplicate5$levelscore_NPREG05)
# no duplicates

duplicate5[!is.na(duplicate5$levelscore_NPENG05) & duplicate5$levelscore_NPENG05 > 1 |
             !is.na(duplicate5$levelscore_NPLES05) & duplicate5$levelscore_NPLES05 > 1 | 
             !is.na(duplicate5$levelscore_NPREG05) & duplicate5$levelscore_NPREG05 > 1 ,
]$w19_0634_lnr # list of ids who are duplicated

## check the IDs that are repeated and see if its in different years
check <- duplicate5 %>%
  group_by(w19_0634_lnr) %>%
  filter(n()>1) %>%
  ungroup()
## some duplicated IDs exist. Seems like they have scores only for different test years. To check

check <- check %>%
  group_by(w19_0634_lnr, year) %>%
  filter(n() > 1) %>%
  ungroup()
## all of them have different test years

results_woduplicated5 <- results5


#* 4.2 Grade 8 ----

## values_ln=length give the number of duplicates when trying to reformat to wide format, duplicates based on same ID, year and status
results8 <- results8 %>%
  select(-school_code)

duplicate8 <- pivot_wider(results8, names_from = test, 
                          values_from = c(levelscore, score, mean, sd, std_score), 
                          values_fn=length)

nrow(duplicate8) # 1,091,041
length(unique(duplicate8$w19_0634_lnr)) # 1,090,746
## some duplicate w19_0634_lnr numbers  exist 

## dealing with those with multiple scores 
## count the number of entries for each person for each subject.
table(duplicate8$levelscore_NPLES08) # no duplicates
table(duplicate8$levelscore_NPENG08)# no duplicate
table(duplicate8$levelscore_NPREG08) # no duplicate

duplicate8[!is.na(duplicate8$levelscore_NPENG08) & duplicate8$levelscore_NPENG08 > 1 |
             !is.na(duplicate8$levelscore_NPLES08) & duplicate8$levelscore_NPLES08 > 1 | 
             !is.na(duplicate8$levelscore_NPREG08) & duplicate8$levelscore_NPREG08 > 1 ,
]$w19_0634_lnr # list of ids who are duplicated
# no duplicates 

## check the IDs that are repeated and see if its in different years
check <- duplicate8 %>%
  group_by(w19_0634_lnr) %>%
  filter(n()>1) %>%
  ungroup()
## some duplicated IDs exist. Seems like they have scores only for different test years. To check

check <- check %>%
  group_by(w19_0634_lnr, year) %>%
  filter(n() > 1) %>%
  ungroup()
## all of them have different test years

results_woduplicated8 <- results8


#* 4.3 Grade 9 ----

## values_ln=length give the number of duplicates when trying to reformat to wide format, duplicates based on same ID, year and status
results9 <- results9 %>%
  select(-school_code)

duplicate9 <- pivot_wider(results9, names_from = test, 
                          values_from = c(levelscore, score, mean, sd, std_score), 
                          values_fn=length)

nrow(duplicate9) # 901,795
length(unique(duplicate9$w19_0634_lnr)) # 901,689
## some duplicate w19_0634_lnr numbers  exist 

## Dealing with those with multiple scores 
## Count the number of entries for each person for each subject.
table(duplicate9$levelscore_NPLES09) # NO duplicates
table(duplicate9$levelscore_NPREG09) # NO duplicates

duplicate9[!is.na(duplicate9$levelscore_NPLES09) & duplicate9$levelscore_NPLES09 > 1 | 
                                    !is.na(duplicate9$levelscore_NPREG09) & duplicate9$levelscore_NPREG09 > 1 ,
]$w19_0634_lnr # list of ids who are duplicated
# no duplicates 

## check the IDs that are repeated and see if its in different years
check <- duplicate9 %>%
  group_by(w19_0634_lnr) %>%
  filter(n()>1) %>%
  ungroup()
## some duplicated IDs exist. Seems like they have scores only for different test years. To check

check <- check %>%
  group_by(w19_0634_lnr, year) %>%
  filter(n() > 1) %>%
  ungroup()
## all of them have different test years

results_woduplicated9 <- results9

rm(results5, results8, results9,
   duplicate5, duplicate8, duplicate9, check)


# 5. Change back to Wide Format ####

## one individual per row
## warning: if some duplicates are left, this will create lists

#* 5.1 Grade 5 ----

results_woduplicated5$levelscore <- as.numeric(results_woduplicated5$levelscore)
wide5 <- pivot_wider(results_woduplicated5,
                    names_from = test,
                    values_from = c(levelscore, score, mean, sd, std_score))
wide <- as.data.frame(wide5)
nrow(wide5) # 1,057,740
length(unique(wide5$w19_0634_lnr)) # 1,057,578
## people who have taken the test in multiple years still exist 


#* 5.2 Grade 8 ----

results_woduplicated8$levelscore <- as.numeric(results_woduplicated8$levelscore)
wide8 <- pivot_wider(results_woduplicated8,
                     names_from = test,
                     values_from = c(levelscore, score, mean, sd, std_score))

wide8 <- as.data.frame(wide8)
nrow(wide8) # 1,091,041  
length(unique(wide8$w19_0634_lnr)) # 1,090,746
## people who have taken the test in multiple years still exist 


#* 5.3 Grade 9 ----

results_woduplicated9$levelscore <- as.numeric(results_woduplicated9$levelscore)
wide9 <- pivot_wider(results_woduplicated9,
                     names_from = test,
                     values_from = c(levelscore, score, mean, sd, std_score))
wide9 <- as.data.frame(wide9)
nrow(wide9) # 901,795  


# 6. Students who did Retakes ####

#* 6.1 Grade 5 ----

retakestudents5 <- wide5[duplicated(wide5$w19_0634_lnr),]$w19_0634_lnr 
# duplicate function only give the second row of the duplicated element

retakestudents5 # there are 162 students who did retakes
retake5 <- wide5[wide5$w19_0634_lnr %in% retakestudents5, ] # so need to extract both rows to check what is happening 

## some students have missing data in the most recent year of the test 
## retaking a school year is not something that is usually done in Norway
## so might be more related to change of schools, or students with special needs
## it looks like most students don't substantially improve in the second year they take the test
## but some have some missing data for the first year

## to only keep the first year results, as this is likely the test they took at the standard test age, and in the school they are the longest attending 

retake5 <- arrange(retake5, w19_0634_lnr, year)

## order by students, and by year of test, chronological, so first year first 
to_remove5 <- retake5[duplicated(retake5$w19_0634_lnr),] 
nrow(to_remove5) # 162 rows to remove
## the duplicated function select the second appearance of the value
## so it select the second time the students passed the tests

grade5_scores <- anti_join(wide5, to_remove5, by = c("w19_0634_lnr", "year"))
nrow(wide5) # 1,057,740
nrow(grade5_scores) # 1,057,578


#* 6.2 Grade 8 ----

retakestudents8 <- wide8[duplicated(wide8$w19_0634_lnr),]$w19_0634_lnr 
retakestudents8 # there are 295 students who did retakes
retake8 <- wide8[wide8$w19_0634_lnr %in% retakestudents8, ] 

retake8 <- arrange(retake8, w19_0634_lnr, year) 
to_remove8 <- retake8[duplicated(retake8$w19_0634_lnr),] 
nrow(to_remove8) # 295

grade8_scores <- anti_join(wide8, to_remove8, by = c("w19_0634_lnr", "year"))
nrow(wide8) # 1,091,041
nrow(grade8_scores) # 1,090,746


#* 6.3 Grade 9 ----

retakestudents9 <- wide9[duplicated(wide9$w19_0634_lnr),]$w19_0634_lnr
retakestudents9 # there are 106 students who did retakes
retake9 <- wide9[wide9$w19_0634_lnr %in% retakestudents9, ] 

retake9 <- arrange(retake9, w19_0634_lnr, year) 
to_remove9 <- retake9[duplicated(retake9$w19_0634_lnr),] 
nrow(to_remove9) # 106

grade9_scores <- anti_join(wide9, to_remove9, by = c("w19_0634_lnr", "year"))
nrow(wide9) # 901,795
nrow(grade9_scores) # 901,689

rm(to_remove5, to_remove8, to_remove9,
   wide5, wide8, wide9, 
   retake5, retake8, retake9,
   results_woduplicated5, results_woduplicated8, results_woduplicated9)


# 7. Aggregating Scores ####

## to form one single aggregate score combining all scores (using only Math and Reading Comprehension)

#* 7.1 Grade 5 ----

nrow(grade5_scores) # 1,057,578

grade5_scores <- grade5_scores %>%
  filter(!is.na(score_NPLES05) | !is.na(score_NPREG05) | !is.na(score_NPENG05))
nrow(grade5_scores) # 1,057,578


grade5_scores$score_agg5 <- ifelse(
  rowSums(!is.na(grade5_scores[, c("score_NPLES05", "score_NPREG05")])) == 2,
  rowMeans(grade5_scores[, c("score_NPLES05", "score_NPREG05")], na.rm = TRUE),
  NA
)

average5 <- grade5_scores %>%
  group_by(year) %>%
  summarise(
    score_agg5_mean = mean(score_agg5, na.rm = TRUE),
    score_agg5_sd = sd(score_agg5, na.rm = TRUE)) 

grade5_scores <- left_join(grade5_scores, average5, by=c("year"))

## standardize the scores per year and test 
grade5_scores$std_score_agg5 <- (grade5_scores$score_agg5 - grade5_scores$score_agg5_mean)/grade5_scores$score_agg5_sd

hist(grade5_scores$std_score_agg5)
describe(grade5_scores$std_score_agg5)
sum(!is.na(grade5_scores$std_score_agg5)) # 1,005,376


#* 7.2 Grade 8 ----

nrow(grade8_scores) # 1,090,746

grade8_scores <- grade8_scores %>%
  filter(!is.na(score_NPLES08) | !is.na(score_NPREG08) | !is.na(score_NPENG08))
nrow(grade8_scores) # 1,090,746

grade8_scores$score_agg8 <- ifelse(
  rowSums(!is.na(grade8_scores[, c("score_NPLES08", "score_NPREG08")])) == 2,
  rowMeans(grade8_scores[, c("score_NPLES08", "score_NPREG08")], na.rm = TRUE),
  NA
)

average8 <- grade8_scores %>%
  group_by(year) %>%
  summarise(
    score_agg8_mean = mean(score_agg8, na.rm = TRUE),
    score_agg8_sd = sd(score_agg8, na.rm = TRUE)) 

grade8_scores <- left_join(grade8_scores, average8, by=c("year"))

## standardize the scores per year and test 
grade8_scores$std_score_agg8 <- (grade8_scores$score_agg8 - grade8_scores$score_agg8_mean)/grade8_scores$score_agg8_sd

hist(grade8_scores$std_score_agg8)
describe(grade8_scores$std_score_agg8)
sum(!is.na(grade8_scores$std_score_agg8)) # 1,032,911


#* 7.3. Grade 9 ----

nrow(grade9_scores) # 901,689

grade9_scores <- grade9_scores %>%
  filter(!is.na(score_NPLES09) | !is.na(score_NPREG09))
nrow(grade8_scores) # 1,090,746

grade9_scores$score_agg9 <- ifelse(
  rowSums(!is.na(grade9_scores[, c("score_NPLES09", "score_NPREG09")])) == 2,
  rowMeans(grade9_scores[, c("score_NPLES09", "score_NPREG09")], na.rm = TRUE),
  NA
)

average9 <- grade9_scores %>%
  group_by(year) %>%
  summarise(
    score_agg9_mean = mean(score_agg9, na.rm = TRUE),
    score_agg9_sd = sd(score_agg9, na.rm = TRUE)) 

grade9_scores <- left_join(grade9_scores, average9, by=c("year"))

## standardize the scores per year and test 
grade9_scores$std_score_agg9 <- (grade9_scores$score_agg9 - grade9_scores$score_agg9_mean)/grade9_scores$score_agg9_sd

hist(grade9_scores$std_score_agg9)
describe(grade9_scores$std_score_agg9)
sum(!is.na(grade9_scores$std_score_agg9)) # 857,537


# 8. Select Only Required Columns ####

grade5_scores <- select(grade5_scores, w19_0634_lnr,
                        score_NPLES05, score_NPREG05, score_NPENG05, 
                        std_score_NPLES05, std_score_NPREG05, std_score_NPENG05, 
                        score_NPLES05, score_NPREG05, score_NPENG05, 
                        std_score_NPLES05, std_score_NPREG05, std_score_NPENG05, 
                        score_agg5, std_score_agg5)

grade8_scores <- select(grade8_scores, w19_0634_lnr,
                        score_NPLES08, score_NPREG08, score_NPENG08, 
                        std_score_NPLES08, std_score_NPREG08, std_score_NPENG08, 
                        score_NPLES08, score_NPREG08, score_NPENG08, 
                        std_score_NPLES08, std_score_NPREG08, std_score_NPENG08, 
                        score_agg8, std_score_agg8)

grade9_scores <- select(grade9_scores, w19_0634_lnr,
                        score_NPLES09, score_NPREG09, 
                        std_score_NPLES09, std_score_NPREG09, 
                        score_NPLES09, score_NPREG09, 
                        std_score_NPLES09, std_score_NPREG09, 
                        score_agg9, std_score_agg9)

nat_test_scores <- full_join(grade5_scores, grade8_scores, by = "w19_0634_lnr") %>%
  full_join(grade9_scores, by = "w19_0634_lnr")
nrow(nat_test_scores) # 1,294,613

write.csv(nat_test_scores, "./Data/Reference Data/nat_test_scores.csv", row.names=FALSE)

