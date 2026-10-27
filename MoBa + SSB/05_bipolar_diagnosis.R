
# 
# Script: 05_bipolar_diagnosis.R 
# Project: Bipolar Disorder and Education: 
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Clean bipolar disorder diagnosis data
#
# Author: Vaishnavi K Madhavan
# Start date: 13-05-2025
# 

# 1. Set Up ####

pop_sibinfo_covariates <- fread("./Data/Reference Data/pop_sibinfo_covariates.csv")

## new file
bipolar_diag_old <- fread("./Data/KUHR_summarized_36199047_Vaish_BipolarEA_2025-04-28.csv")
bipolar_diag <- fread("./Data/KUHR_summarized_36199047_Vaish_Bipolar_EA_update_2026-04-24.csv")

nrow(bipolar_diag_old) # 41122
nrow(bipolar_diag) # 43614 (2492 additional cases)

rm(bipolar_diag_old)

## select required columns
bipolar_diag <- select(bipolar_diag, PASIENTLOPENUMMER, 
                       first_contact, sum_contact, last_contact)
colnames(bipolar_diag) <- c("w19_0634_lnr", "first_contact", "sum_contact", "last_contact") 

bipolar_diag_cov <- pop_sibinfo_covariates %>%
  left_join(bipolar_diag, by = "w19_0634_lnr") 
nrow(bipolar_diag_cov) # 9080090


# 2. Data Cleaning ####

#* 2.1. Creating Dichotomous Variable ----

## changing into a dichotomous variable (keeping 5 number of contacts as a cut-off for having bipolar disorder)
bipolar_diag_cov$bipolar_diag <- ifelse(!is.na(bipolar_diag_cov$sum_contact) & bipolar_diag_cov$sum_contact > 4, 1, 0)
table(bipolar_diag_cov$bipolar_diag) # 43,614 cases


#* 2.2. Checking for Duplicates ----
duplicates <- bipolar_diag_cov[duplicated(bipolar_diag_cov$w19_0634_lnr) | duplicated(bipolar_diag_cov$w19_0634_lnr, fromLast = TRUE), ]
nrow(unique(duplicates)) # no duplicates


#* 2.3. Removing those with a Possible Misdiagnosis ----

#** 2.3.1. Those born after 2010 ----
## check birth year range of the cases
check_cases <- bipolar_diag_cov %>%
  filter(bipolar_diag == 1 )
table(check_cases$birthyear) 
# only 11 people with a diagnosis who were born after 2010. We can change their diagnosis to 0
rm(check_cases)

table(bipolar_diag_cov$bipolar_diag) # 43,614 
bipolar_diag_cov <- bipolar_diag_cov %>%
  mutate(
    bipolar_diag = ifelse(
      birthyear > 2010, 0, bipolar_diag))
table(bipolar_diag_cov$bipolar_diag) # 43,603  (changed the diagnosis of 11 individuals)


#** 2.3.1. Those with a diagnosis before they are 15 ----

## first, for the age at diagnosis, we will use the birthyear and year of last contact 
bipolar_diag_cov$first_contact <- as.numeric(bipolar_diag_cov$first_contact)
bipolar_diag_cov$birthyear <- as.numeric(bipolar_diag_cov$birthyear)

bipolar_diag_cov$age_diag <- with(bipolar_diag_cov,
                                       ifelse(!is.na(first_contact),
                                              first_contact - birthyear,
                                              NA))

## check those who received diagnosis before the age of 15
check <- bipolar_diag_cov %>%
  filter(bipolar_diag == 1) %>%
  filter(age_diag <= 15)
table(check$birthyear)
table(check$sum_contact) # all of them have been diagnosed multiple times. 

## check their last period of contact
table(check$last_contact)
check$period_contact <- check$last_contact - check$first_contact
table(check$period_contact) # 0 - 19 years
check$age_last_contact <- check$last_contact - check$birthyear
table(check$age_last_contact) # to change diag of 56 individuals with last_contact before the age of 15
rm(check)

## change the diagnosis for those whose last contact is before the age of 15
nrow(bipolar_diag_cov) # 9,080,090
bipolar_diag_cov$age_last_contact <- bipolar_diag_cov$last_contact - bipolar_diag_cov$birthyear
table(bipolar_diag_cov$age_last_contact) # 3 to 97

table(bipolar_diag_cov$bipolar_diag) # 43,603 
bipolar_diag_cov <- bipolar_diag_cov %>%
  mutate(
    bipolar_diag = ifelse(
      age_last_contact < 15, 0, bipolar_diag))
table(bipolar_diag_cov$bipolar_diag) # 43,547  

bipolar_diag_cov$bipolar_diag[is.na(bipolar_diag_cov$bipolar_diag)] <- 0
table(bipolar_diag_cov$bipolar_diag) # 43,547 cases 
table(bipolar_diag_cov$birthyear) # 1855 to 2024 (retain this for now)

 
# 3. Creating Age at Last Follow-up Variable ####

## for cox regression, we need the age at last follow-up
## This allows us to censor data and take account of the fact that some people may not have received a diagnosis YET 

## for the individuals without a diagnosis, we will add the age at last follow-up 
## The KUHR data is available until 2025, so we will use this as the year of last follow
## age_consor = age at which the diagnosis was censored 
end_of_followup <- 2026 # end of 2025
bipolar_diag_cov$deathyear <- as.numeric(bipolar_diag_cov$deathyear)

bipolar_diag_cov <- bipolar_diag_cov %>%
  mutate(
    age_censor = if_else(
      !is.na(deathyear),
      deathyear - birthyear,       
      end_of_followup - birthyear             
    ))


## time-to-event or censoring: use age at diagnosis if available, else age at censoring
bipolar_diag_cov$time <- ifelse(!is.na(bipolar_diag_cov$age_diag),
                                        bipolar_diag_cov$age_diag,
                                         bipolar_diag_cov$age_censor)

describe(bipolar_diag_cov$time) # 9080090 (everyone has a time variable)
table(bipolar_diag_cov$time) # 0 to 163
hist(bipolar_diag_cov$time)

## check those with 0 in time variable
check <- bipolar_diag_cov %>%
  filter(time == 0)
table(check$birthyear)
# stillborns? Check if this gets filtered out in the main sample. 
# they shouldn't have a national test score

table(bipolar_diag_cov$bipolar_diag) # 43,547 cases

write.csv(bipolar_diag_cov, "./Data/Reference Data/bipolar_disorder_diagnosis.csv") 
## Note: always save diagnostic data in Limited_C

