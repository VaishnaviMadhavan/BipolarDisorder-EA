
#
# Script: 01_identify_families.R
# Project: Bipolar Disorder and Education:
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Script for extracting siblings based on parental information 
#
# Author: Vaishnavi K Madhavan
# Start date: 11-11-2025
# 

## adapted from Scripts by P.Demange: http://github.com/PerlineDemange/CBS-MR


# 1. Clean Data ####

#* 1.1. Open Datasets gbapersoon and Kindouder ----

## open Gbapersoontab (GBAPERSOONKTAB2024V3.csv was saved as .csv from the file G:/Bevolking/GBAPERSOONKTAB/GBAPERSOONKTAB2024V3.sav)
gba <- fread("G:/Bevolking/GBAPERSOONTAB/2024/GBAPERSOON2024TABV3.csv")
head(gba)
nrow(gba) # 28,336,778
colnames(gba)
# "RINPERSOONS"               "RINPERSOON"                "GBAGEBOORTELAND"           "GBAGESLACHT"              
# "GBAGEBOORTELANDMOEDER"     "GBAGEBOORTELANDVADER"      "GBAAANTALOUDERSBUITENLAND" "GBAHERKOMSTGROEPERING"    
# "GBAGENERATIE"              "GBAGEBOORTEJAAR"           "GBAGEBOORTEMAAND"          "GBAGEBOORTEDAG"           
# "GBAGESLACHTMOEDER"         "GBAGESLACHTVADER"          "GBAGEBOORTEJAARMOEDER"     "GBAGEBOORTEMAANDMOEDER"   
# "GBAGEBOORTEDAGMOEDER"      "GBAGEBOORTEJAARVADER"      "GBAGEBOORTEMAANDVADER"     "GBAGEBOORTEDAGVADER"      
# "GBAIMPUTATIECODE"          "GBAHERKOMSTLAND"           "GBAGEBOORTELANDNL"  

## save in personal folder
#write_csv(gba, "./Data/01_Raw_Data/GBAPERSOON2024TABV3.csv")


## open Kindoudertab (KINDOUDER2024TABV1.csv was saved as .csv from the file G:/Bevolking/KINDOUDERTAB/KINDOUDER2024TABV1.sav)
kindouder <- fread("./Data/01_Raw_Data/KINDOUDER2024TABV1.csv")
head(kindouder)
nrow(kindouder) # 18,062,543
colnames(kindouder)
# "RINPERSOONS"   "RINPERSOON"    "RINPERSOONSpa" 
# "RINPERSOONpa"  "RINPERSOONSMa" "RINPERSOONMa"  "XKOPPELNUMMER"

## save in personal folder
write_csv(kindouder, "./Data/01_Raw_Data/KINDOUDER2024TABV1.csv")


#* 1.2. Filter required Cohort ----

## based on the available cito score (2006-2015) and bp diagnosis data (2011-2021) available, I am filtering the birth cohort
summary(gba$GBAGEBOORTEJAAR) 
hist(gba$GBAGEBOORTEJAAR)
gba <- gba[which(gba$GBAGEBOORTEJAAR >= 1990 & gba$GBAGEBOORTEJAAR <= 2010), ]
summary(gba) # 6,726,695
barplot(table(gba$GBAGEBOORTEJAAR))


## 1.2. Remove individual with unknown parents (parents not in GBA) from Kindoudertab ##
# key for RINPERSOON column: R is in GBA, G is not in GBA 
kindpa <- kindouder[which(kindouder$RINPERSOONSpa == 'R' & kindouder$RINPERSOONSMa == 'R'), ]
nrow(kindpa) # 15,123,388


## 1.3. Remove individuals not in GBA (R is in GBA, D is dead at birth and N is unknown) ##
kindclean <- kindpa[which(kindpa$RINPERSOONS == 'R'), ]
head(kindclean)
nrow(kindclean) # 14,880,800
rm(kindpa)
rm(kindouder)


## 1.4. Merge selected gbapersoon and kindouder ##
famdata <- merge(gba, kindclean, by=c('RINPERSOONS', 'RINPERSOON'))
head(famdata)
nrow(famdata) # 4,256,591
barplot(table(famdata$GBAGEBOORTEJAAR))
colnames(famdata)

## checking sex of parents (homoparental families are excluded later)
table(famdata$GBAGESLACHTMOEDER)
table(famdata$GBAGESLACHTVADER) 


# 2. Clean Families ------------

#* 2.1.  Identify individuals with same parents based on RINPERSOON ----
famdata$FID <- paste(famdata$RINPERSOONpa, famdata$RINPERSOONMa, sep="_")
famdata <- famdata[order(famdata$FID),]
head(famdata)

#* 2.2. Get number of full siblings in family: Fsize ----

length(unique(famdata$FID)) # 2,341,176 full families
count <- as.data.frame(table(famdata$FID)) #get number of occurences of FID
head(count)
famdata$Fsize <- count[[2]][match(famdata$FID, count[[1]])] # match data fid with value in count, if true takes value of Freq 
table(famdata$Fsize) 


#* 2.3. Extract Siblings ----
## fam data has also singletons. Now I will filter to only families with atleast 2 sibs. 
data <- famdata
nrow(data) # 4,256,591
data <- data[data$Fsize > 1] 
nrow(data) # 3,324,523


#* 2.4. Remove families whose parents had more than one child with other (identical) partner ----

## get number of individual with same father SamePa or same mother SameMa 
nbfather <- as.data.frame(table(data$RINPERSOONpa))
data$SamePa <- nbfather[[2]][match(data$RINPERSOONpa, nbfather[[1]])] 
head(data)
hist(data$SamePa)
table(data$Fsize)
nbmother <- as.data.frame(table(data$RINPERSOONMa))
data$SameMa <- nbmother[[2]][match(data$RINPERSOONMa, nbmother[[1]])]
hist(data$SameMa)

## identify individuals who have half siblings
data$halfPa <- data$Fsize < data$SamePa # TRUE if the father had children with other mother, halfsibling exist
summary(data$halfPa) # TRUE = 35,242 
head(data[data$halfPa == T])

data$halfMa <- data$Fsize < data$SameMa # TRUE if the mother had children with other father, halfsibling exist
summary(data$halfMa) # TRUE = 16,140 

data$halfsib <- data$halfPa == T | data$halfMa == T
table(data$halfsib) # T= 49,645 
head(data[data$halfsib == T])

## remove families whose parents had more than one child with other (identical) partner
data <- data[data$halfsib == F]
nrow(data) # 3,274,878     


#* 2.5. Exclude homoparental families ----
data$GBAGESLACHTMOEDER  <- as.factor(data$GBAGESLACHTMOEDER)
data$GBAGESLACHTVADER  <- as.factor(data$GBAGESLACHTVADER)
table(data$GBAGESLACHTMOEDER) #sex of the mother: 1 is man and 2 is woman and - missing data
summary(data$GBAGESLACHTVADER) #sex of the father: 1 is man and 2 is woman and - missing data

data <- data[data$GBAGESLACHTMOEDER == 2] #  keep individual whose mother is a woman 
data <- data[data$GBAGESLACHTVADER == 1] # keep individual whose father is a man 

nrow(data) # 3,271,554

rm(kindclean)
rm(nbfather)
rm(nbmother)


#* 2.6. Exclude families with mismatching parental information ----
# Some parents have the same RINDPERSON but appear to have different birthday and country of birth 

#** 2.6.1 Exclude parents within families with different birthday -----
#*** 2.6.1.1 Fathers ----
table(data$GBAGEBOORTEJAARVADER) # 1281 missing data
table(data$GBAGEBOORTEMAANDVADER) # 1281 missing data 
data$GBAGEBOORTEJAARVADER <- as.numeric(data$GBAGEBOORTEJAARVADER)
table(data$GBAGEBOORTEJAARVADER) # doesn't print NA if numeric 
data <- data[data$GBAGEBOORTEJAARVADER > 1870,] 
nrow(data) # 3,270,273
table(data$GBAGEBOORTEMAANDVADER) # we removed all missing data

data$birthdayPa <- paste(data$GBAGEBOORTEJAARVADER, data$GBAGEBOORTEMAANDVADER, data$GBAGEBOORTEDAGVADER, sep ="-")
data_birthdayPa <- data %>% 
  group_by(FID) %>%
  mutate(SameBirthdayPa = ifelse(duplicated(birthdayPa)| duplicated(birthdayPa, fromLast = TRUE), T, F))
data_birthdayPa <- as.data.frame(data_birthdayPa)
summary(data_birthdayPa$SameBirthdayPa) # 767 fathers with a different birthday indicated
tail(data_birthdayPa[data_birthdayPa$SameBirthdayPa == F,]) 

# several are mismatched by only one month, maybe should only sort on year? 
# either i remove all family with a mismatch father or just the mismatch father (keep fam of >2 in the data)
# I decide to do: birthday has to match entirely, only mismatched father is excluded 

## this keeps the families for which at least 2 matching birthdays and removes only the mismatched ones
data_birthdayPa <- data_birthdayPa[data_birthdayPa$SameBirthdayPa == T, ] 
nrow(data_birthdayPa) # 3,269,506


#*** 2.6.1.2 Mothers ----
table(data$GBAGEBOORTEJAARMOEDER) # 151 missing data
table(data$GBAGEBOORTEMAANDMOEDER) # 151 missing data
data$GBAGEBOORTEJAARMOEDER<- as.numeric(data$GBAGEBOORTEJAARMOEDER)
data <- data[which(is.na(data$GBAGEBOORTEJAARMOEDER) == F),] 
nrow(data) # 3,270,122

data$birthdayMa <- paste(data$GBAGEBOORTEJAARMOEDER, data$GBAGEBOORTEMAANDMOEDER, data$GBAGEBOORTEDAGMOEDER, sep ="-")

data_birthdayMa <- data %>% 
  group_by(FID) %>%
  mutate(SameBirthdayMa = ifelse(duplicated(birthdayMa)| duplicated(birthdayMa, fromLast = TRUE), T, F))
data_birthdayMa <- as.data.frame(data_birthdayMa)
summary(data_birthdayMa$SameBirthdayMa) # 723  mothers with a different birthday indicated

data_birthdayMa <- data_birthdayMa[data_birthdayMa$SameBirthdayMa == T, ] 
nrow(data_birthdayMa) # 3,269,399

# Get new number of family size and remove families < 2 (should not happen)
count <- as.data.frame(table(data_birthdayMa$FID)) #get number of occurences of FID
data_birthdayMa$Fsize_final <- count[[2]][match(data_birthdayMa$FID, count[[1]])] # match data fid with value in count, if true takes value of Freq 
data_birthdayMa <- data_birthdayMa[data_birthdayMa$Fsize_final > 1,] 
nrow(data_birthdayMa) # 3,269,399

data <- data_birthdayMa # 3,269,399


#** 2.6.2 Exclude parents with mismatching land of origin within same family  ----
#*** 2.6.2.1 Fathers ---- 
table(data$GBAGEBOORTELANDVADER)
table(is.na(data$GBAGEBOORTELANDVADER)) # no missing data 

data_landPa <- data %>% 
  group_by(FID) %>%
  mutate(SameLandPa = ifelse(duplicated(GBAGEBOORTELANDVADER)| duplicated(GBAGEBOORTELANDVADER, fromLast = TRUE), T, F))
data_landPa <- as.data.frame(data_landPa) 
summary(data_landPa$SameLandPa) # 1975 fathers with different lands of origin 

data_landPa <- data_landPa[data_landPa$SameLandPa == T, ] 
nrow(data_landPa) # 3,267,424

# Get new number of family size and remove families < 2 (should not happen)
count <- as.data.frame(table(data_landPa$FID)) #get number of occurences of FID
data_landPa$Fsize_final <- count[[2]][match(data_landPa$FID, count[[1]])] # match data fid with value in count, if true takes value of Freq 
data_landPa <- data_landPa[data_landPa$Fsize_final > 1,] # 3,267,424

data <- data_landPa # 3,267,424


#*** 2.6.2.2 Mothers ----
table(data$GBAGEBOORTELANDMOEDER)
table(is.na(data$GBAGEBOORTELANDMOEDER)) # no missing data 

data_landMa <- data %>% 
  group_by(FID) %>%
  mutate(SameLandMa = ifelse(duplicated(GBAGEBOORTELANDMOEDER)| duplicated(GBAGEBOORTELANDMOEDER, fromLast = TRUE), T, F))
data_landMa <- as.data.frame(data_landMa) 
summary(data_landMa$SameLandMa) # 534 mothers with different lands of origin 

data_landMa <- data_landMa[data_landMa$SameLandMa == T, ] 
nrow(data_landMa) # 3,266,658

# Get new number of family size and remove families < 2 (should not happen)
count <- as.data.frame(table(data_landMa$FID)) # get number of occurences of FID
data_landMa$Fsize_final <- count[[2]][match(data_landMa$FID, count[[1]])] # match data fid with value in count, if true takes value of Freq 
data_landMa <- data_landMa[data_landMa$Fsize_final > 1,]  # 3,266,658

data <- data_landMa # 3,266,658

rm(data_birthdayPa)
rm(data_birthdayMa)
rm(data_landPa)
rm(data_landMa)


#* 2.7. Exclude siblings born less 6months apart, but keep twins ----
#** 2.7.1. Identify twins (born on the same day) ----
data$birthday <- paste(data$GBAGEBOORTEJAAR, data$GBAGEBOORTEMAAND, data$GBAGEBOORTEDAG, sep ="-")
barplot(table(data$GBAGEBOORTEMAAND))
barplot(table(gba$GBAGEBOORTEDAG)) # Day of birth is set at 1 by security. 

data <- data %>% 
  group_by(FID) %>%
  mutate(multiple = ifelse(duplicated(birthday)| duplicated(birthday, fromLast = TRUE), T, F))
table(data$multiple) # 137,783  multiples 

# Get number of multiples within family to investigate problems
summary <- data %>%
  group_by(FID) %>%
  tally(multiple == T)
table(summary$n)

#I checked families with 8 multiples, nothing looks weird. 


#** 2.7.2 Remove siblings born less 6months apart----
data_twins_6months <- data %>%
  mutate(birthday = as.Date(birthday)) %>%
  arrange(FID, birthday) %>%
  group_by(FID) %>%
  mutate(next_bday = lead(birthday),
         next_RINPERSOON = lead(RINPERSOON),
         prev_bday = lag(birthday),
         prev_RINPERSOON = lag(RINPERSOON),
         
         closer_than_6months = 
           (!is.na(next_bday) & abs(as.numeric(next_bday - birthday)) <= 213) | 
           (!is.na(prev_bday) & abs(as.numeric(prev_bday - birthday)) <= 213)
  ) %>%
  ungroup()

table(data_twins_6months$multiple)
# n=138,352  out of which 137,783 multiples (twins), so 569 to be removed 


#** 2.7.3 Exclude siblings born less than 6 months apart and keep twins ----
data <- data_twins_6months
data$closer_than_6months_not_multiple <- ifelse(data$closer_than_6months == T & data$multiple == F, T, F)
summary(data$closer_than_6months_not_multiple) # 453
head(data[data$closer_than_6months_not_multiple == T,])

data_final <- data[data$closer_than_6months_not_multiple == F, ] # 2,439,279
nrow(data_final) # 3,266,089

## exclude families with only one sib left 
count <- as.data.frame(table(data_final$FID)) #get number of occurences of FID
data_final$Fsize_final <- count[[2]][match(data_final$FID, count[[1]])] # match data fid with value in count, if true takes value of Freq 
data_final <- data_final[data_final$Fsize_final > 1,] # 2,439,217
nrow(data_final) # 3,266,012
rm(data_twins_6months)


# 3. Create Final Dataset #####

## famdata has the entire sample including singletons. 
## data_final has valid siblings. 
## combine singletons from famdata and siblings from data_final 

sibling_data <- data_final %>%
  select(RINPERSOON, GBAGESLACHT, GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, GBAGEBOORTEDAG,
         FID, Fsize) %>%
  filter(!is.na(FID)) %>%
  mutate(valid_sibs = 1) %>%
  group_by(FID) %>%
  filter(n() >1) %>%
  ungroup() %>%
  select(RINPERSOON, valid_sibs)
nrow(sibling_data) # 3,266,012

full_pop_sibs <- famdata %>%
  select(RINPERSOON, GBAGESLACHT, GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, GBAGEBOORTEDAG,
         FID, Fsize) %>%
  filter(!is.na(FID)) %>%
  group_by(FID) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  left_join(sibling_data, by = "RINPERSOON")
nrow(full_pop_sibs) # 3,324,523

singletons <- famdata %>%
  select(RINPERSOON, GBAGESLACHT, GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, GBAGEBOORTEDAG,
         FID, Fsize) %>%
  filter(!is.na(FID)) %>%
  group_by(FID) %>% filter(n() == 1) %>% ungroup() %>%
  mutate(valid_sibs = 0) 
nrow(singletons) # 932,068

sibs_plus_population <- rbind(full_pop_sibs, singletons)

nrow(singletons) # 932,068
nrow(sibling_data) # 3,266,012
nrow(famdata) # 4,256,591 (our initial sample)
nrow(sibs_plus_population) # 4,256,591 (our final sample)

nrow(sibling_data) # 3,266,012
table(sibs_plus_population$valid_sibs) # 3,266,012 (checks out)

sum(!is.na(sibs_plus_population$FID)) # 4,256,591

rm(gba, data_final, singletons, sibling_data)


# 4. Add birth order variable #####
sibs_plus_population$birthday <- paste(sibs_plus_population$GBAGEBOORTEJAAR, sibs_plus_population$GBAGEBOORTEMAAND, sibs_plus_population$GBAGEBOORTEDAG, sep ="-")
sibs_plus_population$birthday <- ymd(sibs_plus_population$birthday)

sibs_plus_population <- sibs_plus_population %>% 
  group_by(FID) %>%
  mutate(birthorder = dense_rank(birthday))


# 5. Save data #####
sibs_plus_population <- as.data.frame(sibs_plus_population)

sibs_plus_population <- sibs_plus_population %>%
  select(RINPERSOON, FID, GBAGESLACHT, GBAGEBOORTEJAAR, birthday, Fsize, birthorder, valid_sibs) 

write.csv(sibs_plus_population, "./Data/02_Reference_Data/20260521_family_info", row.names = F)

