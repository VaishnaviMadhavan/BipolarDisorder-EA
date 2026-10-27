
# 
# Script: 03_educational_attainment.R 
# Project: Bipolar Disorder and Education: 
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Clean Educational Attainment (EA) data
#
# Author: Vaishnavi K Madhavan
# Start date: 24-04-2025
# 

## previously used-script by Perline Demange and Rosa Chessman

# 1. Set Up #### 

edu <- fread("N:/durable/data/registers/SSB/01_data/data_v6.1/EDUCATION_ADVANCED/csv/EDUCATION_BU_UTD_IGANG.csv")
nrow(edu) # 7,046,307
describe(edu$BU_2024) # 7,006,497 

edu_old <- fread("N:/durable/data/registers/SSB/01_data/data_v5.0/EDUCATION_ADVANCED/csv/EDUCATION_BU_UTD.csv")
nrow(edu_old) # 6,931,045
colnames(edu_old)
describe(edu$BU_2023) # 6,929,077 (about 70k more individuals in the new version)
rm(edu_old)

## first version looked at highest education reported in 2018. With new data we look at 2023
edu <- edu[, c( "w19_0634_lnr", "BU_2024")]
edu$edu_2024 = as.numeric(str_sub(edu$BU_2024,1,1)) 
# extracting just the first char and turning it into numeric

table(edu$edu_2024)
#    1       2       3       4       5       6       7       8       9 
#  27557 2341771  952799 1473533  183067 1352023  591947   65283   18517 

# 9 = unspecified, so NA
edu$edu_2024[edu$edu_2024 == 9] <- NA 
table(edu$edu_2024)
#     1       2       3       4       5       6       7       8 
#   27557 2341771  952799 1473533  183067 1352023  591947   65283  


# 2. Clean Data ####

## duplicates 
summary(duplicated(edu$w19_0634_lnr))
dupli <- edu[duplicated(edu$w19_0634_lnr),]$w19_0634_lnr #0
dupli 
rm(dupli)

## remove NAs
table(edu$edu_2024)
#     1       2       3       4       5       6       7       8 
#   27557 2341771  952799 1473533  183067 1352023  591947   65283  

# the labels are
# 0 "no edu" 1 "1-7gr" 2 "8-10gr" 3 "some high school" 4 "complete high school" 
# 5 "extra high school" 6 "BA" 7 "MA" 8 "PhD" 9 "unknown" 

nrow(edu) # 7,046,307 
sum(is.na(edu$w19_0634_lnr))
sum(is.na(edu$edu_2024)) # 58,327 to remove
edu <- edu[complete.cases(edu), ]
nrow(edu) # 6,987,980

sum(duplicated(edu$w19_0634_lnr)) # no duplicates


# 3. NUS > ISCED 2011 > Years of Education ####

# the labels are
# 0 "no edu" 1 "primary school" 2 "lower secondary school" 3 "some high school" 4 "complete high school" 
# 5 "extra high school" 6 "BA" 7 "MA" 8 "PhD" 9 "unknown" 

# 3-4 are combined together in ISCED  3
# while 5 is unclear if 4 or 5, here chose 5
x<-edu$edu_2024
edu$isced11<-ifelse(is.na(x), NA,
                            ifelse(!is.na(x) & x == 0, 0, # combines ppl with edu up to age 2 & up to age 5
                            ifelse(!is.na(x) & x == 1, 1, # primary
                            ifelse(!is.na(x) & x == 2, 2, # secondary lower
                            ifelse(!is.na(x) & x == 4 | x == 3, 3, # secondary upper AND post secondary non tertiary shorter than 2 yrs
                            ifelse(!is.na(x) & x == 5, 5,  # postsecondary vocational 0.5-1.5 or 2 years tertiary--not distinguished so dunno if isced 4 vs 5
                            ifelse(!is.na(x) & x == 6, 6, #undergrad
                            ifelse(!is.na(x) & x == 7, 7, #MSc/MA
                            ifelse(!is.na(x) & x == 8, 8, x))))))))) #PhD
table(edu$isced11)
#   1       2       3       5       6       7       8 
# 27557 2341771 2426332  183067 1352023  591947   65283  

## conversion into years of education
# PRE PRIMARY=1 ::0
# PRIMARY=7::1
# LOWER SECONDARY =10 :2
# UPPER SECONDARY 13 : 3
# POST SECONDARY NON TERITARY 15 : 4/5
# FIRST STAGE TERTIARY NOT LEADING TO ADVANCEED RESEARCH 19: ba: 6
# FIRST OR SECOND STAGE CANNOT DISTINGUISH 20 () ma: 7
# SECOND STAGE TERTIARY LEADING TO ADVANCED RESEARCH QUAL 22: 8

x <- edu$isced11
edu$yrsedu<-ifelse(is.na(x), NA,
                             ifelse(!is.na(x) & x == 0, 0, # combines ppl with edu up to age 2 & up to age 5
                             ifelse(!is.na(x) & x == 1, 7, # primary
                             ifelse(!is.na(x) & x == 2, 10, # secondary lower
                             ifelse(!is.na(x) & x == 3, 13,# secondary upper AND post secondary non tertiary shorter than 2 yrs
                             ifelse(!is.na(x) & x == 5, 15,  # postsecondary vocational 0.5-1.5 or 2 years tertiary--not distinguished so dunno if isced 4 vs 5
                             ifelse(!is.na(x) & x == 6, 19, #undergrad
                             ifelse(!is.na(x) & x == 7, 20, #MSc/MA
                             ifelse(!is.na(x) & x == 8, 22, x))))))))) #PhD
table(edu$yrsedu)

#    7      10      13      15      19      20      22 
#  27557 2341771 2426332  183067 1352023  591947   65283

nrow(edu) # 6,987,980
hist(edu$edu_2024)
hist(edu$isced11)
hist(edu$yrsedu)

ggplot(edu, aes(x = factor(yrsedu))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Educational Attainment", y = "Count") +
  theme_minimal()


# 4. NUS > YEARS OF EDUCATION ####

# 0 "no edu" = 0 
# 1 "primary school" = 6 years 
# 2 "lower secondary school" = 9 years 
# 3 "some high school" = 10 years 
# 4 "complete high school" = 12 years
# 5 "extra high school" = 13 years
# 6 "BA" = 16 years
# 7 "MA" = 18 years
# 8 "PhD" = 23 years
# 9 "unknown" = NA

x<-edu$isced11
edu$yrsedu_direct<-ifelse(is.na(x), NA,
                   ifelse(!is.na(x) & x == 0, 0,               
                   ifelse(!is.na(x) & x == 1, 6,               
                   ifelse(!is.na(x) & x == 2, 9,               
                   ifelse(!is.na(x) & x == 3, 10,              
                   ifelse(!is.na(x) & x == 4, 12,              
                   ifelse(!is.na(x) & x == 5, 13,              
                   ifelse(!is.na(x) & x == 6, 16,              
                   ifelse(!is.na(x) & x == 7, 18, 
                   ifelse(!is.na(x) & x == 8, 23,x)))))))))) 
table(edu$yrsedu_direct)
#     6       9      10      13      16      18      23 
#   27557 2341771 2426332  183067 1352023  591947   65283 

nrow(edu) # 6,987,980
hist(edu$edu_2024)


# 5. Age Cut-off ####

pop_sibinfo_covariates <- fread("./Data/Reference Data/pop_sibinfo_covariates.csv")

edu <- edu %>%
  left_join(pop_sibinfo_covariates, by = "w19_0634_lnr")
nrow(edu) # 6,987,980

edu <- edu %>%
  filter(!is.na(birthyear) & !is.na(yrsedu))
nrow(edu) # 3,060,459

edu$birthyear <- as.numeric(edu$birthyear)
describe(edu$birthyear) # 1864 - 2013

edu <- edu %>%
  mutate(
    age = 2024 - birthyear,
    yrsedu = ifelse(age < 30 & yrsedu < 22, NA, yrsedu),
    yrsedu_direct = ifelse(age<30 & yrsedu_direct < 23, NA, yrsedu_direct)
  )

check <- edu %>%
  filter(age < 22)
table(check$yrsedu)
rm(check)

hist(edu$edu_2024)
hist(edu$isced11)
hist(edu$yrsedu)

table(edu$edu_2024)
table(edu$isced11)
table(edu$yrsedu)
table(edu$yrsedu_direct)

sum(!is.na(edu$yrsedu)) # 3,060,459


# 6. Standardising the Variable ####

edu$std_yrsedu <- scale(edu$yrsedu)
edu$std_yrsedu_direct <- scale(edu$yrsedu_direct)

write.csv(edu, "./Data/Reference Data/educational_attainment.csv")

