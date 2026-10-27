
# 
# Script: 01_identify_families.R 
# Project: Bipolar Disorder and Education: 
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Identify siblings in SSB using parental data
#
# Author: Vaishnavi K Madhavan
# Start date: 12-04-2025
# 

## script adapted from P.Demange

# 1. Set Up ####

basic <- read.csv("N:/durable/data/registers/SSB/01_data/data_v6.1/CORE/csv/POPULATION_FASTE_OPPLYSNINGER_reduced.csv")
## this is everyone in the register

head(basic)  
nrow(basic) # there are 9,080,159 individuals

## Meaning of column names 
# w19_0634_lnr = Individual ID
# lopenr_mor = Mother ID
# lopenr_far = Father ID
# foedsels_aar_mnd = Birth year - month
# doeds_aar_mnd = Year of death
# kjoenn = Gender


# 2. Clean Duplicates  ####

## issues with duplicates - SSB confirmed that the duplicates are the same people. 
## the following code obtained from 
## N:/durable/data/registers/SSB/02_scripts/help_scripts/clean_duplicates_faste_opplysninger/01_clean_duplicates_FO
## combines the duplicates and keeps the ones which have conflicting information 

fo_dupl <- basic[duplicated(basic$w19_0634_lnr) | duplicated(basic$w19_0634_lnr, fromLast = TRUE),]
fo_dupl[fo_dupl == ""] <- NA

# loop through duplicates and check if rows can be merged (no conflicting info) - check first if one row mainly has NA
problematic_ids <- NULL
for (id in unique(fo_dupl$w19_0634_lnr)) {
  fo_int <- fo_dupl[fo_dupl$w19_0634_lnr == id,]
  # check if info is equal or NA in the other
  for (v in colnames(fo_int)) {
    if (!is.na(fo_int[1,v] == fo_int[2,v]) & !fo_int[1,v] == fo_int[2,v]) { # meaningful difference
      print(paste0("Difference found for ", id, " in variable ", v))
      
      problematic_ids <- c(problematic_ids, id)
    }
  }
  
  rm(fo_int)
}

problematic_ids <- unique(problematic_ids)

# Function: collapse_nonconflicting
collapse_nonconflicting <- function(df, id_col = "id", conflict_ids = NULL) {
  id_sym <- rlang::sym(id_col)
  other_cols <- setdiff(names(df), id_col)
  
  # Rows to keep unchanged (conflict groups)
  kept_conflicts <- df %>%
    filter(!!id_sym %in% conflict_ids)
  
  # Rows to collapse (safe groups)
  collapsed_safe <- df %>%
    filter(! (!!id_sym %in% conflict_ids)) %>%
    group_by(!!id_sym) %>%
    summarize(
      # for each non-id column, take the first non-NA value (preserving type)
      across(all_of(other_cols), ~ {
        vals <- .x[!is.na(.x)]
        if (length(vals) == 0) {
          # return an NA of the original type
          v <- .x[1]
          v[] <- NA
          v
        } else {
          vals[[1]]
        }
      }),
      .groups = "drop"
    )
  
  # Combine and return (preserve original row order approximately by arranging by id)
  bind_rows(kept_conflicts, collapsed_safe) %>%
    # optional: arrange by id to keep groups together
    arrange(!!id_sym)
}

fo_dupl_clean <- collapse_nonconflicting(fo_dupl, id_col = "w19_0634_lnr", conflict_ids = problematic_ids)

basic <- basic[!basic$w19_0634_lnr %in% fo_dupl$w19_0634_lnr,]
basic <- rbind(basic, fo_dupl_clean)

nrow(basic) # 9,080,097 (62 individuals removed)

rm(fo_dupl, fo_dupl_clean)


# 3. Clean Duplicates with Conflicting Information ####

duplicates <- basic %>%
  group_by(w19_0634_lnr) %>%
  filter(n() > 1)
nrow(duplicates) # 12
length(unique(duplicates$w19_0634_lnr)) # 6
table(duplicates$invkat)

## keep the ones invkat = B 
duplicates_clean <- duplicates %>%
  filter(invkat == "B") %>% 
  group_by(w19_0634_lnr) %>%
  slice(1) %>%
  ungroup()
# 6

## merge with main dataset 
basic_no_duplicates <- basic %>% 
  filter(!w19_0634_lnr %in% duplicates$w19_0634_lnr)
nrow(basic_no_duplicates) # 9,080,085

basic <- bind_rows(basic_no_duplicates, duplicates_clean)
nrow(basic) # 9,080,091 (6 individuals removed)

rm(duplicates, duplicates_clean, basic_no_duplicates)


# 4. Get IDs and Parental Data ####

## prefix function - prefixes the Id numbers of mother and father in a dataframe
## for example, if you run the command prefixlink("new_", df), 
## then all the columns in the dataframe gets added the prefix. 
prefixlink <- function(prefix, df) {
  n<- names(df)
  names(df) <- c("w19_0634_lnr", str_c(prefix, n[2:5]))
  return(df)
}

## get parents data
g0 <- basic %>% 
       select(w19_0634_lnr, mother_lnr = lopenr_mor, father_lnr = lopenr_far, sex=kjoenn, 
         birthdate = foedsels_aar_mnd)
nrow(g0) # 9,080,091
head(g0)
g0 <- as.data.frame(g0)
colnames(g0)

## get family members in the same row, their parents and the parents of their parents.
## mother_lnr="w19_0634_lnr" command looks for mother's data in the dataset g0 
## and adds it to the datatframe using mother_ as the prefix. 
## so now we have data on mother's mother, mother's father. mother's sex, and mother's birthdate in the same row. 
## the same for fathers
kinship <- g0 %>% 
   left_join(prefixlink("mother_", g0), by=c(mother_lnr = "w19_0634_lnr")) %>% 
   left_join(prefixlink("father_", g0), by=c(father_lnr = "w19_0634_lnr")) %>%
  filter(!is.na(sex) & !is.na(birthdate))
head(kinship)
nrow(kinship) # 9,080,090


# 5. Create Family IDs ####

## some empty cells exist. change from empty strings to NA
kinship[kinship == ""] <- NA # 9,080,090
head(kinship)

## the unique combination of motherIDfatherID is the familyID (parents)
kinship$parents <- ifelse(
  !is.na(kinship$mother_lnr) & !is.na(kinship$father_lnr), 
  paste0(kinship$mother_lnr, "_", kinship$father_lnr),
  NA)

## filter to those with valid sociodemographic and parental information
nrow(kinship) # 9,080,090
kinship <- kinship %>%
  filter(!is.na(sex) & !is.na(birthdate) & !is.na(parents))
nrow(kinship) # 5,038,126


# 6. Extracting Siblings ####

## select individual with both parental information 
both_parents <- kinship[is.na(kinship$mother_lnr) == F & 
                          is.na(kinship$father_lnr) == F,] 

nrow(both_parents) # 5,038,126


## identify siblings
shared_parents <- both_parents %>%
  filter(!is.na(parents)) %>%
  add_count(parents, name = "fsize") %>%
  filter(fsize > 1)

nrow(shared_parents) #4,269,661
rm(both_parents)

## filtering to those with valid sociodemographic variables
shared_parents <- shared_parents %>%
  filter(!is.na(sex) & !is.na(birthdate) & !is.na(parents))
nrow(shared_parents) #4,269,661

full_pop_sibs <- shared_parents # to merge later for population sample


# 7. Removing those whose parents had > 1 child with Another Partner ####

## get number of individuals with same father or same mother
nbfather <- as.data.frame(table(shared_parents$father_lnr))
shared_parents$samePa <- nbfather[[2]][match(shared_parents$father_lnr, nbfather[[1]])]
head(shared_parents)
table(shared_parents$fsize)

nbmother <- as.data.frame(table(shared_parents$mother_lnr))
shared_parents$sameMa <- nbfather[[2]][match(shared_parents$mother_lnr, nbmother[[1]])]
head(shared_parents)

## identify individuals who have half siblings 
nrow(shared_parents) # 4269661
shared_parents$halfPa <- shared_parents$fsize < shared_parents$samePa
summary(shared_parents$halfPa)

shared_parents$halfMa <- shared_parents$fsize < shared_parents$sameMa
summary(shared_parents$halfMa)

shared_parents$halfsib <- shared_parents$halfPa | shared_parents$halfMa == T
table(shared_parents$halfsib) # T = 1128269  

## remove those whose parents had more than one child with another partner 
shared_parents <- shared_parents %>%
  filter(halfsib == F)
nrow(shared_parents) # 3101512


# 8. Exclude Homoparental Families ####

## exclude individuals with same sex parents
shared_parents <- shared_parents[shared_parents$mother_sex == 2 &
                                   shared_parents$father_sex == 1,]   

nrow(shared_parents) # 3,099,830


# ## extracting only sibling IDs for KUHR application 
# sib_IDs <- shared_parents %>%
#   ungroup() %>%
#   select(w19_0634_lnr)
# sib_IDS <- select(sib_IDs, w19_0634_lnr)
# 
# write.csv(sib_IDs, "./Data/Reference Data/sibling_IDs_only.csv", row.names=FALSE)


# 9. Check Twins and Multiples ####

## those with the same birthdate 
shared_parents <- shared_parents %>%
  group_by(parents) %>% 
  mutate(multiple = ifelse(duplicated(birthdate) | duplicated(birthdate, fromLast=TRUE), T, F))
summary(shared_parents$multiple) # T=92325  (these number of people belong to multiples)

## get number of multiples within family to investigate potential problems
summary <- shared_parents %>%
  group_by(parents)%>%
  tally(multiple == T)

table(summary$n)
summary[summary$n == 7,]
summary[summary$n==8,] # multiple set of multiples 
sum(is.na(shared_parents$parents))

## land of birth is not available, so we don't know if these are biological septuplets or adopted. 
## We will retain them assuming that all of them are biological siblings born on the same day 
rm(summary)


# 10. Remove those born less than 6 months apart ####

head(shared_parents$birthdate)
shared_parents$birthdate <- as.Date(
  paste0(as.character(shared_parents$birthdate), "01"),
  format = "%Y%m%d")
head(shared_parents$birthdate)

shared_parents <- shared_parents %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  arrange(parents, birthdate) %>%
  group_by(parents) %>%
  mutate(
    next_day = lead(birthdate),
    next_w19_0634_lnr = lead(w19_0634_lnr),
    prev_day = lag(birthdate),
    prev_w19_0634_lnr = lag(w19_0634_lnr),
    
    closer_than_6months =
      (!is.na(next_day) & abs(as.numeric(next_day - birthdate)) <= 213) |
      (!is.na(prev_day) & abs(as.numeric(prev_day - birthdate)) <= 213)
  ) %>%
  ungroup()

table(shared_parents$multiple == T)
table(shared_parents$closer_than_6months)

# 92908 out of which 92133 multiples, so 775 to be removed


## exclude siblings born less than 6 months apart and keep twins
shared_parents$closer_than_6months_not_multiple <-
  ifelse(
    shared_parents$closer_than_6months == TRUE & shared_parents$multiple == FALSE,
    TRUE, FALSE
  )

summary(shared_parents$closer_than_6months_not_multiple)  # 775  
head(shared_parents[shared_parents$closer_than_6months_not_multiple == TRUE, ])

nrow(shared_parents) # 3,099,830
sibling_data <- shared_parents[shared_parents$closer_than_6months_not_multiple == FALSE, ]
nrow(sibling_data)   # 3,099,055 (removed 775 siblings born less than 6 months apart)


## exclude families with only one sib left
count <- as.data.frame(table(sibling_data$parents))  # get number of occurrences of FID

sibling_data$fsize <-
  count[
    match(sibling_data$parents, count[[1]]),
    2
  ]

sibling_data <- sibling_data[sibling_data$fsize > 1, ]
nrow(sibling_data)   # 3,098,789 (removed 266 individuals left without sibs)



# 11. Extracting Covariates (sex, birthyear, and birthorder) ####

## form full dataset
# full_pop_sibs has all the siblings 
# sibling_data has the valid sibs to be used in the sibling analysis 
# singletons has single children

## add identifier to extract siblings later 
sibling_data <- sibling_data %>%
  mutate(valid_sibs = 1) %>%
  select(w19_0634_lnr, valid_sibs)

full_pop_sibs <- full_pop_sibs %>%
  select(w19_0634_lnr, mother_lnr, parents, sex, birthdate, fsize) %>%
  left_join(sibling_data, by = "w19_0634_lnr")

singletons <- kinship %>%
  filter(!is.na(parents)) %>% 
  group_by(parents) %>% filter(n() == 1) %>%
  mutate(valid_sibs = 0) %>%
  mutate(fsize = 1) %>%
  select(w19_0634_lnr, mother_lnr, parents, sex, birthdate, fsize, valid_sibs)

names(full_pop_sibs)
names(singletons)

pop_sibinfo_covariates <- rbind(full_pop_sibs, singletons)

## check valid_sibs columns
nrow(sibling_data) # 3098789  
table(pop_sibinfo_covariates$valid_sibs) # 1 = 3098789 (checks out)

## change birthdate format 
pop_sibinfo_covariates$birthdate <- as.Date(
  paste0(as.character(pop_sibinfo_covariates$birthdate), "01"),
  format = "%Y%m%d"
)


#* 11.1. Birth Order ---- 

pop_sibinfo_covariates <- pop_sibinfo_covariates %>%
  group_by(parents) %>%
  mutate(birthorder = dense_rank(birthdate)) %>%
  ungroup()

nrow(pop_sibinfo_covariates) # 5,038,126
sum(!is.na(pop_sibinfo_covariates$parents)) # 5038126
sum(!is.na(pop_sibinfo_covariates$birthorder)) # 5038126

## check birth order for twins 
twins <- pop_sibinfo_covariates %>%
  group_by(parents) %>% 
  mutate(multiple = ifelse(duplicated(birthdate) | duplicated(birthdate, fromLast=TRUE), T, F))
summary(twins$multiple) # 126,133   (these number of people belong to twins)

twinsonly <- twins[twins$multiple == TRUE, ]
# View(twinsonly) # looks okay
 
rm(twins, twinsonly, add_birthorder)
rm(g0, kinship, shared_parents, singletons, siblings, sibling_data)


#* 11.2. Birth and Death Year ---- 

pop_sibinfo_covariates <- pop_sibinfo_covariates %>%
  left_join(basic %>% 
              select(w19_0634_lnr, deathdate = doeds_aar_mnd), 
            by = "w19_0634_lnr")

pop_sibinfo_covariates$birthyear <- substr(pop_sibinfo_covariates$birthdate, 1, 4)
pop_sibinfo_covariates$deathyear <- substr(pop_sibinfo_covariates$deathdate, 1, 4)

nrow(pop_sibinfo_covariates) # n = 5034008
sum(!is.na(pop_sibinfo_covariates$birthyear)) # 5034008
sum(!is.na(pop_sibinfo_covariates$deathyear)) # 382073


write.csv(pop_sibinfo_covariates, "./Data/Reference Data/pop_sibinfo_covariates.csv", row.names=FALSE)

rm(count, basic, siblings_singletons)


# 12. Check Descriptives ####
# pop_sibinfo_covariates <- read.csv("Bipolar_EA_Vaish/Data/Reference Data/pop_sibinfo_covariates.csv")

## filter to population and sibling data 
popdata <- select(pop_sibinfo_covariates, w19_0634_lnr, sex, birthyear, birthorder)
sibdata <- select(pop_sibinfo_covariates, w19_0634_lnr, parents, sex, birthyear, birthorder)

popdata <- popdata[complete.cases(popdata), ] 
sibdata <- popdata %>% 
  group_by(parents) %>% filter(n() > 1)

nrow(popdata) # 5,035,040 (not the sample size since the final sample size will depend on the data available)
nrow(sibdata) # 4,266,575

## number of siblings in each family 
table(table(sibdata$parents))

#   2      3      4       5      6       7      8      9     10     
# 991161 467983 134348  39175  13269   4820   2037    887    428    

#  11     12     13   14     15     16     17     18 
# 190    126     55   33     13      5      2      2

## check birthyear
popdata$birthyear <- as.numeric(popdata$birthyear)
sibdata$birthyear <- as.numeric(sibdata$birthyear)

describe(popdata$birthyear) # 1896 to 2024
describe(sibdata$birthyear) # 1896 to 2024

rm(popdata, sibdata)

