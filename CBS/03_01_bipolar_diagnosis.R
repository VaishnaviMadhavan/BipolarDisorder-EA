
#
# Script: 03_01_bipolar_diagnosis.R
# Project: Bipolar Disorder and Education:
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Script to extract and clean bipolar disorder diagnosis
#
# Author: Vaishnavi K Madhavan
# Start date: 27-11-2025
# 


# 1. Extract Diagnoses ####

#* 1.1. Detailed 2011 - 2016 ----

#** 1.1.1. Create Necessary Files ----

## creating the codefile and the data_list
#  - codefile:  data frame with two columns, $trait containing the name of the disorder, 
#               $code containing the DSM IV code for the disorder
#  - data_list: data frame with three columns: $year: year of the data, 
#               $main: data with path to the main diagnoses data,
#               $second: data with path to the secondary diagnoses data
#  - gba:       column of individual ID 
#  - Output:    data table with one column for the year, and three columns for each disorder
#               (main and secondary diagnoses, and both)

codefile <- data.table(
               trait = c("bipolar", "bipolar_1", "bipolar_2", "bipolar_NOS", "bipolar_and_mood", 
                         "cyclothymia", "mood_dueto_somatic_cond", 
                         "substance_induced_all", "mood_disorder_NOS"),
               
               code = c("as1_6.02.01.01.01|as1_6.02.01.01.02|as1_6.02.01.01.03|as1_6.02.01.01.04|as1_6.02.01.01.05|as1_6.02.01.01.06|as1_6.02.01.01.07|as1_6.02.01.01.08|as1_6.02.01.02|as1_6.02.01.03.01|as1_6.02.01.03.02|as1_6.02.01.03.03|as1_6.02.01.03.04|as1_6.02.01.03.05|as1_6.02.01.03.06|as1_6.02.01.03.07|as1_6.02.01.04|as1_6.02.01.04.01|as1_6.02.01.04.02|as1_6.02.01.04.03|as1_6.02.01.04.04|as1_6.02.01.04.05|as1_6.02.01.04.06|as1_6.02.01.04.07|as1_6.02.01.05|as1_6.02.01.05.01|as1_6.02.01.05.02|as1_6.02.01.05.03|as1_6.02.01.05.04|as1_6.02.01.05.05|as1_6.02.01.05.06|as1_6.02.01.05.07|as1_6.02.01.06|as1_6.02.02|as1_6.02.02.01|as1_6.02.02.02|as1_6.02.04", 
                        "as1_6.02.01.01.01|as1_6.02.01.01.02|as1_6.02.01.01.03|as1_6.02.01.01.04|as1_6.02.01.01.05|as1_6.02.01.01.06|as1_6.02.01.01.07|as1_6.02.01.01.08|as1_6.02.01.02|as1_6.02.01.03.01|as1_6.02.01.03.02|as1_6.02.01.03.03|as1_6.02.01.03.04|as1_6.02.01.03.05|as1_6.02.01.03.06|as1_6.02.01.03.07|as1_6.02.01.04|as1_6.02.01.04.01|as1_6.02.01.04.02|as1_6.02.01.04.03|as1_6.02.01.04.04|as1_6.02.01.04.05|as1_6.02.01.04.06|as1_6.02.01.04.07|as1_6.02.01.05|as1_6.02.01.05.01|as1_6.02.01.05.02|as1_6.02.01.05.03|as1_6.02.01.05.04|as1_6.02.01.05.05|as1_6.02.01.05.06|as1_6.02.01.05.07|as1_6.02.01.06", 
                        "as1_6.02.02|as1_6.02.02.01|as1_6.02.02.02",
                        "as1_6.02.04",
                        "as1_6.02.01.01.01|as1_6.02.01.01.02|as1_6.02.01.01.03|as1_6.02.01.01.04|as1_6.02.01.01.05|as1_6.02.01.01.06|as1_6.02.01.01.07|as1_6.02.01.01.08|as1_6.02.01.02|as1_6.02.01.03.01|as1_6.02.01.03.02|as1_6.02.01.03.03|as1_6.02.01.03.04|as1_6.02.01.03.05|as1_6.02.01.03.06|as1_6.02.01.03.07|as1_6.02.01.04|as1_6.02.01.04.01|as1_6.02.01.04.02|as1_6.02.01.04.03|as1_6.02.01.04.04|as1_6.02.01.04.05|as1_6.02.01.04.06|as1_6.02.01.04.07|as1_6.02.01.05|as1_6.02.01.05.01|as1_6.02.01.05.02|as1_6.02.01.05.03|as1_6.02.01.05.04|as1_6.02.01.05.05|as1_6.02.01.05.06|as1_6.02.01.05.07|as1_6.02.01.06|as1_6.02.02|as1_6.02.02.01|as1_6.02.02.02|as1_6.02.04|as1_6.02.03|as1_6.02.05|as1_6.02.05.01|as1_6.02.05.02|as1_6.02.05.03|as1_6.02.05.04|as1_6.02.06.01|as1_6.02.06.01.01|as1_6.02.06.01.02|as1_6.02.06.01.03|as1_6.02.06.01.04|as1_6.02.06.01.05|as1_6.02.06.02|as1_6.02.06.02.02|as1_6.02.06.02.03|as1_6.02.06.02.04|as1_6.02.06.03|as1_6.02.06.03.01|as1_6.02.06.03.02|as1_6.02.06.03.03|as1_6.02.06.03.05|as1_6.02.06.04.01|as1_6.02.06.04.02|as1_6.02.06.05.02|as1_6.02.06.05.04|as1_6.02.06.06|as1_6.02.06.06.01|as1_6.02.06.06.02|as1_6.02.06.06.05|as1_6.02.06.07|as1_6.02.06.08|as1_6.02.06.08.01|as1_6.02.06.08.02|as1_6.02.06.08.03|as1_6.02.06.08.04|as1_6.02.06.08.05|as1_6.02.06.09|as1_6.02.06.09.01|as1_6.02.06.09.02|as1_6.02.06.09.03|as1_6.02.06.09.04|as1_6.02.06.09.05|as1_6.02.07", 
                        "as1_6.02.03",
                        "as1_6.02.05|as1_6.02.05.01|as1_6.02.05.02|as1_6.02.05.03|as1_6.02.05.04", 
                        "as1_6.02.06.01|as1_6.02.06.01.01|as1_6.02.06.01.02|as1_6.02.06.01.03|as1_6.02.06.01.04|as1_6.02.06.01.05|as1_6.02.06.02|as1_6.02.06.02.02|as1_6.02.06.02.03|as1_6.02.06.02.04|as1_6.02.06.03|as1_6.02.06.03.01|as1_6.02.06.03.02|as1_6.02.06.03.03|as1_6.02.06.03.05|as1_6.02.06.04.01|as1_6.02.06.04.02|as1_6.02.06.05.02|as1_6.02.06.05.04|as1_6.02.06.06|as1_6.02.06.06.01|as1_6.02.06.06.02|as1_6.02.06.06.05|as1_6.02.06.07|as1_6.02.06.08|as1_6.02.06.08.01|as1_6.02.06.08.02|as1_6.02.06.08.03|as1_6.02.06.08.04|as1_6.02.06.08.05|as1_6.02.06.09|as1_6.02.06.09.01|as1_6.02.06.09.02|as1_6.02.06.09.03|as1_6.02.06.09.04|as1_6.02.06.09.05|as1_6.02.07", 
                        "as1_6.02.07")
            )

## main diagnosis
traj_2016 <- "G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2016TABV2.sav"
traj_2015 <- "G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2015TABV2.sav"
traj_2014 <- "G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2014TABV3.sav"
traj_2013 <- "G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2013TABV3.sav"
traj_2012 <- "G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2012TABV3.sav"
traj_2011 <- "G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2011TABV4.sav"

## secondary diagnosis
diag_2016 <- "G:/GezondheidWelzijn/GGZDBCDiagnoseTAB/GGZDBCDiagnose2016TABV1.sav"
diag_2015 <- "G:/GezondheidWelzijn/GGZDBCDiagnoseTAB/GGZDBCDiagnose2015TABV1.sav"
diag_2014 <- "G:/GezondheidWelzijn/GGZDBCDiagnoseTAB/GGZDBCDiagnose2014TABV2.sav"
diag_2013 <- "G:/GezondheidWelzijn/GGZDBCDiagnoseTAB/GGZDBCDiagnose2013TABV1.sav"
diag_2012 <- "G:/GezondheidWelzijn/GGZDBCDiagnoseTAB/GGZDBCDiagnose2012TABV1.sav"
diag_2011 <- "G:/GezondheidWelzijn/GGZDBCDiagnoseTAB/GGZDBCDiagnose2011TABV1.sav"

data_list <- data.table(
                year = c("2011", "2012", "2013", "2014", "2015", "2016"),
                main = c(traj_2011, traj_2012, traj_2013, traj_2014, traj_2015, traj_2016),
                second = c(diag_2011, diag_2012, diag_2013, diag_2014, diag_2015, diag_2016)
             )


# Load gba6585 to get ID of individuals 
gba <- fread("G:/Bevolking/GBAPERSOONTAB/2024/GBAPERSOON2024TABV3.csv")

table(gba$GBAGEBOORTEJAAR) 
nrow(gba) # 28,336,778
gba <- gba[,2 ]


#** 1.1.2. Run function to get disorders status (main and secondary diagnoses) ----
gba_2011_2016 <- get_diagnoses_2011_2016(codefile, data_list, gba)
gba_2011_2016 <- as.data.frame(gba_2011_2016)


#** 1.1.3. Filtering out controls (makes the dataset manageable) ----
gba_2011_2016_full_diagnosis <- gba_2011_2016 %>%
  filter(if_any(c(bipolar, bipolar_and_mood, 
                  bipolar_NOS, 
                  cyclothymia, mood_dueto_somatic_cond, substance_induced_all,
                  mood_disorder_NOS), ~ . == TRUE))

nrow(gba_2011_2016_full_diagnosis) # 182,222 rows


#**  1.1.4. Get full disorder status (main and secondary pulled together), per year ----
trait_list <- codefile$trait 
trait_date_list <- paste0(trait_list, "_date")
trait_main_list <- paste0(trait_list, "_main")
trait_second_list <- paste0(trait_list, "_second")

summary(gba_2011_2016_full_diagnosis$bipolar) 
# 140,257 cases (not the final number since the RINPERSOON IDs are repeated)


#**  1.1.5. Make dataset wide ----

## this helps combine with other datasets
## find the first diagnosis date 
gba_2011_2016_first_diagnosis <- gba_2011_2016_full_diagnosis %>%
  filter(!is.na(bipolar_date)) %>%
  group_by(RINPERSOON) %>%
  slice_min(bipolar_date, n=1, with_ties = FALSE) %>%
  select(RINPERSOON, detailed_diagnosis_date = bipolar_date)

length(unique(gba_2011_2016_full_diagnosis$RINPERSOON))
# final bipolar and mood cases to be expected = 72,208

gba_2011_2016_full_diagnosis_wide <- gba_2011_2016_full_diagnosis %>%
                                          pivot_wider(
                                            id_cols = RINPERSOON,
                                            names_from = year,
                                            values_from = c(all_of(trait_list), all_of(trait_main_list), all_of(trait_second_list)),
                                            names_glue = "{.value}_detailed_{year}",
                                            values_fill =  NA
                                          ) %>%
                                         left_join(gba_2011_2016_first_diagnosis, by = "RINPERSOON")


gba_2011_2016_full_diagnosis_wide$detailed_bipolar_allyears <- ifelse(
  rowSums(gba_2011_2016_full_diagnosis_wide
          [grep("^bipolar_detailed", names(gba_2011_2016_full_diagnosis_wide))] == TRUE, na.rm = TRUE) > 0 , 
  TRUE, 
  FALSE
)

table(gba_2011_2016_full_diagnosis_wide$detailed_bipolar_allyears)
# 43,929 cases 


gba_2011_2016_full_diagnosis_wide$detailed_bipolar_and_mood_allyears <- ifelse(
  rowSums(gba_2011_2016_full_diagnosis_wide
          [grep("^bipolar_and_mood_detailed", names(gba_2011_2016_full_diagnosis_wide))] == TRUE, na.rm = TRUE) > 0 , 
  TRUE, 
  FALSE
)

table(gba_2011_2016_full_diagnosis_wide$detailed_bipolar_and_mood_allyears)
# 72,208 cases 

rm(gba_2011_2016_first_diagnosis)


#* 1.2. 2015 - 2021 ----

#** 1.2.1. Create Necessary Files ----

## creating the codefile and the data_list
#  - codefile:  data frame with two columns, $trait containing the name of the disorder, 
#               $code containing the DSM IV code for the disorder
#  - data_list: data frame with three columns: $year: year of the data, 
#               $main: data with path to the main diagnoses data,
#               $second: data with path to the secondary diagnoses data
#  - gba:       column of individual ID 
#  - Output:    data table with one column for the year, and three columns for each disorder
#               (main and secondary diagnoses, and both)

codefile <- data.table(
  trait = c("bipolar_hoofd"),
  code = c("12|012")
)

## main diagnosis
traj_hoofd_2021 <- "G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2021TABV2.sav"
traj_hoofd_2020 <- "G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2020TABV2.sav"
traj_hoofd_2019 <- "G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2019TABV3.sav"
traj_hoofd_2018 <- "G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2018TABV3.sav"
traj_hoofd_2017 <- "G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2017TABV2.sav"
traj_hoofd_2016 <- "G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2016TABV2.sav"
traj_hoofd_2015 <- "G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2015TABV2.sav"

## secondary diagnosis
diag_hoofd_2021 <- "G:/GezondheidWelzijn/GGZDBCDIAGNOSEHOOFDDIAGTAB/GGZDBCDiagnoseHOOFDDIAG2021TABV2.sav"
diag_hoofd_2020 <- "G:/GezondheidWelzijn/GGZDBCDIAGNOSEHOOFDDIAGTAB/GGZDBCDiagnoseHOOFDDIAG2020TABV2.sav"
diag_hoofd_2019 <- "G:/GezondheidWelzijn/GGZDBCDIAGNOSEHOOFDDIAGTAB/GGZDBCDiagnoseHOOFDDIAG2019TABV3.sav"
diag_hoofd_2018 <- "G:/GezondheidWelzijn/GGZDBCDIAGNOSEHOOFDDIAGTAB/GGZDBCDiagnoseHOOFDDIAG2018TABV3.sav"
diag_hoofd_2017 <- "G:/GezondheidWelzijn/GGZDBCDIAGNOSEHOOFDDIAGTAB/GGZDBCdiagnoseHOOFDDIAG2017TABV2.sav"
diag_hoofd_2016 <- "G:/GezondheidWelzijn/GGZDBCDIAGNOSEHOOFDDIAGTAB/GGZDBCdiagnoseHOOFDDIAG2016TABV1.sav"
diag_hoofd_2015 <- "G:/GezondheidWelzijn/GGZDBCDIAGNOSEHOOFDDIAGTAB/GGZDBCdiagnoseHOOFDDIAG2015TABV1.sav"

data_list <- data.table(
  year = c("2015", "2016", "2017", "2018", "2019", "2020", "2021"),
  main = c(traj_hoofd_2015, traj_hoofd_2016, traj_hoofd_2017, traj_hoofd_2018, traj_hoofd_2019, traj_hoofd_2020, traj_hoofd_2021),
  second = c(diag_hoofd_2015, diag_hoofd_2016, diag_hoofd_2017, diag_hoofd_2018, diag_hoofd_2019, diag_hoofd_2020, diag_hoofd_2021)
)

#** 1.2.2. Run function to get disorders status (main and secondary diagnoses), per year ----
# and save intermediate results 
gba_2015_2021 <- get_diagnoses_2015_2021(codefile, data_list, gba)

gba_2015_2021 <- as.data.frame(gba_2015_2021)


#** 1.2.3. filtering out controls (makes the dataset manageable) ----
gba_2015_2021_hoofd_full_diagnosis <- gba_2015_2021 %>%
  filter(if_any(c(bipolar_hoofd), ~ . == TRUE))

nrow(gba_2015_2021_hoofd_full_diagnosis)
# 155,278 rows (not final since RINPERSOONs are repeated) 


#**  1.2.4. Get full disorder status (main and secondary pulled together), per year ----
trait_list <- codefile$trait 
trait_date_list <- paste0(trait_list, "_date")
trait_main_list <- paste0(trait_list, "_main")
trait_second_list <- paste0(trait_list, "_second")

length(unique(gba_2015_2021_hoofd_full_diagnosis$RINPERSOON))
# 58,348 final cases to be expected


#**  1.2.5. Make dataset wide ----
gba_2015_2021_first_diagnosis <- gba_2015_2021_hoofd_full_diagnosis %>%
  filter(!is.na(bipolar_hoofd_date)) %>%
  group_by(RINPERSOON) %>%
  slice_min(bipolar_hoofd_date, n=1, with_ties = FALSE) %>%
  select(RINPERSOON, hoofd_diagnosis_date = bipolar_hoofd_date)

gba_2015_2021_hoofd_full_diagnosis_wide <- gba_2015_2021_hoofd_full_diagnosis %>%
                                                    pivot_wider(
                                                      id_cols = RINPERSOON,
                                                      names_from =year,
                                                      values_from = c(bipolar_hoofd),
                                                      names_glue = "{.value}_{year}"
                                                      ) %>%
                                                    left_join(gba_2015_2021_first_diagnosis, by="RINPERSOON")
  

gba_2015_2021_hoofd_full_diagnosis_wide$hoofd_bipolar_allyears <- ifelse(
  rowSums(gba_2015_2021_hoofd_full_diagnosis_wide[grep("^bipolar_hoofd", names(gba_2015_2021_hoofd_full_diagnosis_wide))] == TRUE, na.rm = TRUE) > 0 , 
  TRUE, 
  FALSE)

table(gba_2015_2021_hoofd_full_diagnosis_wide$hoofd_bipolar_allyears)
# 58,348 cases 

rm(gba_2015_2021_first_diagnosis)

## I am adding a variable called hoofd_bipolar_2017_2021 to extract the bipolar and mood cases only in this range. 
## the years before that are covered by "detailed" dataset
gba_2015_2021_hoofd_full_diagnosis_wide <- fread("./Data/02_Reference_Data/20260212_hoofd_allyears_cases.csv")  

years <- 2017:2021
cols <- paste0("bipolar_hoofd_", years)

gba_2015_2021_hoofd_full_diagnosis_wide$hoofd_bipolar_2017_21 <- rowSums(
                 gba_2015_2021_hoofd_full_diagnosis_wide[, ..cols] == TRUE, na.rm = TRUE) > 0

table(gba_2015_2021_hoofd_full_diagnosis_wide$hoofd_bipolar_2017_21)
# 39,987 cases


#* 1.3. VEKTIS 2013 - 2021 ----

#** 1.3.1. Create Necessary Files ----

## creating the codefile and the data_list
#  - codefile:  data frame with two columns, $trait containing the name of the disorder, 
#               $code containing the DSM IV code for the disorder
#  - data_list: data frame with three columns: $year: year of the data, 
#               $main: data with path to the main diagnoses data,
#               $second: data with path to the secondary diagnoses data
#  - gba:       column of individual ID 
#  - Output:    data table with one column for the year, and three columns for each disorder
#               (main and secondary diagnoses, and both)

codefile <- data.table(
  trait = c("bipolar_vektis"),
  code = c("12|012")
)

## main diagnosis
vektis_2021 <- "G:/GezondheidWelzijn/GGZDECLVEKTIS/GGZDECLVEKTIS2021V1.sav"
vektis_2020 <- "G:/GezondheidWelzijn/GGZDECLVEKTIS/GGZDECLVEKTIS2020V1.sav"
vektis_2019 <- "G:/GezondheidWelzijn/GGZDECLVEKTIS/GGZDECLVEKTIS2019V1.sav"
vektis_2018 <- "G:/GezondheidWelzijn/GGZDECLVEKTIS/GGZDECLVEKTIS2018V1.sav"
vektis_2017 <- "G:/GezondheidWelzijn/GGZDECLVEKTIS/GGZDECLVEKTIS2017V1.sav"
vektis_2016 <- "G:/GezondheidWelzijn/GGZDECLVEKTIS/GGZDECLVEKTIS2016V1.sav"
vektis_2015 <- "G:/GezondheidWelzijn/GGZDECLVEKTIS/GGZDECLVEKTIS2015V1.sav"
vektis_2014 <- "G:/GezondheidWelzijn/GGZDECLVEKTIS/GGZDECLVEKTIS2014V1.sav"
vektis_2013 <- "G:/GezondheidWelzijn/GGZDECLVEKTIS/GGZDECLVEKTIS2013V1.sav"

data_list <- data.table(
  year = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"),
  main = c(vektis_2013, vektis_2014, vektis_2015, vektis_2016, vektis_2017, vektis_2018, vektis_2019, vektis_2020, vektis_2021)
)


#** 1.3.2. Run function to get disorders status (main and secondary diagnoses), per year ----

gba_vektis <- get_diagnoses_vektis(codefile, data_list, gba)

gba_vektis <- as.data.frame(gba_vektis)
gba_vektis$bipolar_vektis <- gba_vektis$bipolar_vektis_main


#** 1.3.3. filtering out controls (makes the dataset manageable) ----
gba_2013_2021_vektis_full_diagnosis <- gba_vektis %>%
  filter(if_any(c(bipolar_vektis), ~ . == TRUE))

nrow(gba_2013_2021_vektis_full_diagnosis)
# 170,218 (not the final number of cases since RINPERSOON is repeated)


#**  1.3.4. Get full disorder status (main and secondary pulled together), per year ----
trait_list <- codefile$trait 
trait_date_list <- paste0(trait_list, "_date")

head(gba_2013_2021_vektis_full_diagnosis[gba_2013_2021_vektis_full_diagnosis$bipolar_vektis ==T,])
length(unique(gba_2013_2021_vektis_full_diagnosis$RINPERSOON))
# 59,232 cases to be expected

#**  1.3.5. Make dataset wide ----
gba_2013_2021_vektis_first_diagnosis <- gba_2013_2021_vektis_full_diagnosis %>%
filter(!is.na(bipolar_vektis_date)) %>%
  group_by(RINPERSOON) %>%
  slice_min(bipolar_vektis_date, n=1, with_ties = FALSE) %>%
  select(RINPERSOON, vektis_diagnosis_date = bipolar_vektis_date)

gba_2013_2021_vektis_full_diagnosis_wide <- gba_2013_2021_vektis_full_diagnosis %>%
                                                pivot_wider(
                                                  id_cols = RINPERSOON,
                                                  names_from =year,
                                                  values_from = c(bipolar_vektis),
                                                  names_glue = "{.value}_{year}"  
                                                  ) %>%
                                                 left_join(gba_2013_2021_vektis_first_diagnosis, by = "RINPERSOON")


gba_2013_2021_vektis_full_diagnosis_wide$vektis_bipolar_allyears <- ifelse(
  rowSums(gba_2013_2021_vektis_full_diagnosis_wide[grep("^bipolar_vektis_", names(gba_2013_2021_vektis_full_diagnosis_wide))] == TRUE, na.rm = TRUE) > 0 , 
  TRUE, 
  FALSE
)

table(gba_2013_2021_vektis_full_diagnosis_wide$vektis_bipolar_allyears)
# 59,232 cases

rm(gba_2013_2021_vektis_first_diagnosis)


## I am adding a variable called vektis_bipolar_2017_2021 to extract the bipolar and mood cases only in this range. 
## the years before that are covered by "detailed" dataset
gba_2013_2021_vektis_full_diagnosis_wide <- fread("./Data/02_Reference_Data/20260212_vektis_allyears_cases.csv")  
colnames(gba_2013_2021_vektis_full_diagnosis_wide)
years <- 2017:2021
cols <- paste0("bipolar_vektis_", years)

gba_2013_2021_vektis_full_diagnosis_wide$vektis_bipolar_2017_21 <- rowSums(
  gba_2013_2021_vektis_full_diagnosis_wide[, ..cols] == TRUE, na.rm = TRUE) > 0

table(gba_2013_2021_vektis_full_diagnosis_wide$vektis_bipolar_2017_21)
# 36,717  cases


# 2. Combine All Sources of Diagnoses ####
# 1. gba_2011_2016_full_diagnosis_wide
# 2. gba_2015_2021_hoofd_full_diagnosis_wide
# 3. gba_2013_2021_vektis_full_diagnosis_wide

## load datasets
# gba_2011_2016_full_diagnosis_wide <- fread("./Data/02_Reference_Data/20260212_detailed_allyears_cases.csv")  
# gba_2015_2021_hoofd_full_diagnosis_wide <- fread("./Data/02_Reference_Data/20260212_vektis_allyears_cases.csv")  
# gba_2013_2021_vektis_full_diagnosis_wide <- fread("./Data/02_Reference_Data/20260212_hoofd_allyears_cases.csv")  


gba_allyears_full_diagnoses <-  gba_2011_2016_full_diagnosis_wide %>%
                            full_join(gba_2015_2021_hoofd_full_diagnosis_wide, by = "RINPERSOON") %>%
                            full_join(gba_2013_2021_vektis_full_diagnosis_wide, by = "RINPERSOON")

nrow(gba_allyears_full_diagnoses) # 97,605


# 3. Remove Large Datasets and Save Intermediary Files  ####
# these files are anyway converted to the wide format and I don't need them anymore 
rm(gba_2011_2016_full_diagnosis, gba_2013_2021_vektis_full_diagnosis, gba_2015_2021_hoofd_full_diagnosis)
rm(gba_2011_2016_full_diagnosis_wide, gba_2013_2021_vektis_full_diagnosis_wide, gba_2015_2021_hoofd_full_diagnosis_wide)

## I am keeping gba_2011_2016 since I need it to check the level of noise, but these I don't need 
rm(gba_2015_2021)
rm(gba_vektis)
rm(gba)
rm(gba_allyear_full_diagnoses_all)

write.csv(gba_allyears_full_diagnoses, "./Data/02_Reference_Data/20260212_alldiagnoses_allyears_allcols_cases.csv", row.names = F)  
write.csv(gba_2011_2016_full_diagnosis_wide, "./Data/02_Reference_Data/20260212_detailed_allyears_cases.csv", row.names = F)  
write.csv(gba_2013_2021_vektis_full_diagnosis_wide, "./Data/02_Reference_Data/20260212_vektis_allyears_cases.csv", row.names = F)  
write.csv(gba_2015_2021_hoofd_full_diagnosis_wide, "./Data/02_Reference_Data/20260212_hoofd_allyears_cases.csv", row.names = F)  


# 4. Create Age at Diagnosis  ####
gba_allyears_full_diagnoses <- read.csv("./Data/02_Reference_Data/20260212_alldiagnoses_allyears_allcols_cases.csv") 

## first create year variables from date variables
gba_allyears_full_diagnoses <- gba_allyears_full_diagnoses %>%
  mutate(
    detailed_diagnosis_date = ymd(detailed_diagnosis_date),
    hoofd_diagnosis_date = ymd(hoofd_diagnosis_date),
    vektis_diagnosis_date = ymd(vektis_diagnosis_date),
    
    # change to year
    detailed_diagnosis_year = year(detailed_diagnosis_date),
    hoofd_diagnosis_year = year(hoofd_diagnosis_date),
    vektis_diagnosis_year = year(vektis_diagnosis_date)
    
  )


## check
describe(gba_allyears_full_diagnoses$detailed_diagnosis_year) 
describe(gba_allyears_full_diagnoses$hoofd_diagnosis_year)
describe(gba_allyears_full_diagnoses$vektis_diagnosis_year) 
# looks ok

## Combine with GBA to get cases + controls 
gba <- fread("G:/Bevolking/GBAPERSOONTAB/2024/GBAPERSOON2024TABV3.csv") %>%
  select(RINPERSOON, GBAGEBOORTEJAAR)
nrow(gba) # 28,336,778

gba_allyears_full_diagnoses_final <-  gba %>%
  left_join(gba_allyears_full_diagnoses, by = "RINPERSOON") 
nrow(gba_allyears_full_diagnoses) # 97,605
nrow(gba_allyears_full_diagnoses_final) # 28,336,778


#* 4.1.  Detailed 2011 - 2016 ----

## first create age at diagnosis variable 
gba_allyears_full_diagnoses_final <- gba_allyears_full_diagnoses_final %>%
                                       mutate(detailed_age_diag = if_else(
                                       !is.na(detailed_diagnosis_year),
                                       detailed_diagnosis_year - GBAGEBOORTEJAAR,
                                       NA))

## for individuals without a diagnosis, we will add the age at last follow-up 
## the year of last follow-up for the detailed dataset is 2016
## age_censor = age at the last year of follow-up
end_of_followup <- 2016
gba_allyears_full_diagnoses_final <- gba_allyears_full_diagnoses_final %>%
                                   mutate(
                                     detailed_age_censor = if_else(
                                       is.na(detailed_age_diag),
                                       end_of_followup - GBAGEBOORTEJAAR,
                                       NA
                                     )
                                   )


#* 4.2.  2015 - 2021 ----
## first create age at diagnosis variable 
gba_allyears_full_diagnoses_final <- gba_allyears_full_diagnoses_final %>%
                                  mutate(hoofd_age_diag = if_else(
                                  !is.na(hoofd_diagnosis_year),
                                  hoofd_diagnosis_year - GBAGEBOORTEJAAR,
                                  NA))

## for individuals without a diagnosis, we will add the age at last follow-up 
## the year of last follow-up for the detailed dataset is 2016
## age_censor = age at the last year of follow-up
end_of_followup <- 2021
gba_allyears_full_diagnoses_final <- gba_allyears_full_diagnoses_final %>%
                                  mutate(
                                    hoofd_age_censor = if_else(
                                      is.na(hoofd_age_diag),
                                      end_of_followup - GBAGEBOORTEJAAR,
                                      NA
                                      )
                                    )


#* 4.3. Vektis 2013 - 2021 ----
## first create age at diagnosis variable 
gba_allyears_full_diagnoses_final <- gba_allyears_full_diagnoses_final %>%
                                   mutate(vektis_age_diag = if_else(
                                    !is.na(vektis_diagnosis_year),
                                    vektis_diagnosis_year - GBAGEBOORTEJAAR,
                                    NA))

## for individuals without a diagnosis, we will add the age at last follow-up 
## the year of last follow-up for the detailed dataset is 2016
## age_censor = age at the last year of follow-up
end_of_followup <- 2021
gba_allyears_full_diagnoses_final <- gba_allyears_full_diagnoses_final %>%
  mutate(
    vektis_age_censor = if_else(
      is.na(vektis_age_diag),
      end_of_followup - GBAGEBOORTEJAAR,
      NA
    )
  )


# 5. Create Aggregated Variables  ####

#* 5.1. Aggregated diagnosis ----
#**  5.1.1. Entire Dataset ----
gba_allyears_full_diagnoses_final <-  gba_allyears_full_diagnoses_final %>%
  mutate(
    alldatasets_bipolar_allyears = 
      coalesce(detailed_bipolar_allyears, FALSE) | 
      coalesce(hoofd_bipolar_2017_21, FALSE) |
      coalesce(vektis_bipolar_2017_21, FALSE)
  )

table(gba_allyears_full_diagnoses_final$alldatasets_bipolar_allyears)
#  FALSE      TRUE 
# 28269639    67139 

table(gba_allyears_full_diagnoses_final$detailed_bipolar_allyears) # 43,929 cases
table(gba_allyears_full_diagnoses_final$hoofd_bipolar_2017_21) # 39,987  cases
table(gba_allyears_full_diagnoses_final$vektis_bipolar_2017_21) # 36,717 cases


##** 5.1.2. Only detailed dataset ######
table(gba_allyears_full_diagnoses_final$detailed_bipolar_allyears)
gba_allyears_full_diagnoses_final$detailed_bipolar_allyears[is.na(gba_allyears_full_diagnoses_final$detailed_bipolar_allyears)] <- FALSE
table(gba_allyears_full_diagnoses_final$detailed_bipolar_allyears) # F = 28,292,849


#* 5.2. Aggregated diagnosis date ----

#**  5.2.1. Entire Dataset ----
gba_allyears_full_diagnoses_final <- gba_allyears_full_diagnoses_final %>%
  mutate(
    alldatasets_diagnosis_date = pmin(detailed_diagnosis_date, 
                                      hoofd_diagnosis_date, 
                                      vektis_diagnosis_date, 
                                      na.rm = TRUE),
    alldatasets_diagnosis_year = year(alldatasets_diagnosis_date))

## check
describe(gba_allyears_full_diagnoses_final$alldatasets_diagnosis_year) # 83,455 (some without a diagnosis have a date - need to filter out later)
sum(gba_allyears_full_diagnoses_final$alldatasets_bipolar_allyears == 1) # 67,139


## check those who do not have a date 
check <- gba_allyears_full_diagnoses_final %>%
  select(RINPERSOON, alldatasets_bipolar_allyears, detailed_bipolar_allyears, hoofd_bipolar_allyears, hoofd_bipolar_2017_21,
         vektis_bipolar_allyears, vektis_bipolar_2017_21, alldatasets_diagnosis_date, alldatasets_diagnosis_year) %>%
  filter(alldatasets_bipolar_allyears == 1) %>%
  filter(is.na(alldatasets_diagnosis_year) | is.na(alldatasets_diagnosis_date))

# from the hoofd dataset, but no diagnosis date available. They will get filtered out later anyway 
rm(check)


#**  5.2.2. Only Detailed Dataset ----
table(gba_allyears_full_diagnoses_final$detailed_bipolar_allyears) # 43929 cases
describe(gba_allyears_full_diagnoses_final$detailed_diagnosis_year) # 43929 individuals (checks out)


#* 5.3. Aggregated age at diagnosis variable ----
gba_allyears_full_diagnoses_final <- gba_allyears_full_diagnoses_final %>%
mutate(
  alldatasets_age_diag = pmin(detailed_age_diag, 
                                    hoofd_age_diag, 
                                    vektis_age_diag, 
                                    na.rm = TRUE),
  alldatasets_age_censor = pmax(detailed_age_censor,
                                hoofd_age_censor,
                                vektis_age_diag,
                                na.rm = TRUE)
  )

## check
describe(gba_allyears_full_diagnoses_final$alldatasets_age_diag) 
describe(gba_allyears_full_diagnoses_final$alldatasets_age_censor) 
# some negatives, but the ones who were born after 2021 will anyway be filtered out later)

sum(gba_allyears_full_diagnoses_final$alldatasets_bipolar_allyears == 1) # 67139 (expected number for all variables)
sum(!is.na(gba_allyears_full_diagnoses_final$alldatasets_diagnosis_date)) # 83455 
sum(!is.na(gba_allyears_full_diagnoses_final$alldatasets_diagnosis_year)) # 83455 
sum(!is.na(gba_allyears_full_diagnoses_final$alldatasets_age_diag)) # 83455 

## remove the diagnosis dates and years for those who do not have a diagnosis 
gba_allyears_full_diagnoses_final$alldatasets_diagnosis_date[gba_allyears_full_diagnoses_final$alldatasets_bipolar_allyears == FALSE] <- NA
gba_allyears_full_diagnoses_final$alldatasets_diagnosis_year[gba_allyears_full_diagnoses_final$alldatasets_bipolar_allyears == FALSE] <- NA
gba_allyears_full_diagnoses_final$alldatasets_age_diag[gba_allyears_full_diagnoses_final$alldatasets_bipolar_allyears == FALSE] <- NA

sum(gba_allyears_full_diagnoses_final$alldatasets_bipolar_allyears == 1) # 67139 (expected number for all variables)
sum(!is.na(gba_allyears_full_diagnoses_final$alldatasets_diagnosis_date)) # 67125 
sum(!is.na(gba_allyears_full_diagnoses_final$alldatasets_diagnosis_year)) # 67125
sum(!is.na(gba_allyears_full_diagnoses_final$alldatasets_age_diag)) # 67125


#* 5.4. Aggregated time variable ----

#**  5.4.1. Entire Dataset ----
## form final time variable
## if age_diag is available, use that, if not, then use age_censor
gba_allyears_full_diagnoses_final$alldatasets_time <- with(gba_allyears_full_diagnoses_final,
                                                    ifelse(!is.na(alldatasets_age_diag),
                                                           alldatasets_age_diag,
                                                           alldatasets_age_censor))

describe(gba_allyears_full_diagnoses_final$alldatasets_time) 
# retain values less than 1 for now. They will get filtered out later


#**  5.4.2. Only Detailed Dataset ----
## form final time variable
## if age_diag is available, use that, if not, then use age_censor
gba_allyears_full_diagnoses_final$detailed_time <- with(gba_allyears_full_diagnoses_final,
                                                        ifelse(!is.na(detailed_age_diag),
                                                               detailed_age_diag,
                                                               detailed_age_censor))

describe(gba_allyears_full_diagnoses_final$detailed_time) # some negatives

## check this variable for the sample that I might use 
check_sample <- gba_allyears_full_diagnoses_final %>%
                                        filter(GBAGEBOORTEJAAR >= 1994 & GBAGEBOORTEJAAR <= 2005)

describe(check_sample$alldatasets_time) # I follow some people up to the age of 27
describe(check_sample$detailed_time) # until 23 
rm(check_sample)

gba_allyears_full_diagnoses_final$alldatasets_bipolar_diag <- gba_allyears_full_diagnoses_final$alldatasets_bipolar_allyears
gba_allyears_full_diagnoses_final$detailed_bipolar_diag <- gba_allyears_full_diagnoses_final$detailed_bipolar_allyears

## make the dataset manageable (remove unnecessary columns)
gba_allyears_full_diagnoses_final <- gba_allyears_full_diagnoses_final %>%
  select(RINPERSOON,
         alldatasets_bipolar_diag, detailed_bipolar_diag, 
         alldatasets_age_diag, alldatasets_age_censor, alldatasets_time,
         detailed_age_diag, detailed_age_censor, detailed_time)

write.csv(gba_allyears_full_diagnoses_final, "./Data/02_Reference_Data/20260521_bipolar_disorder_diagnosis.csv", row.names = F)  

rm(gba_allyears_full_diagnoses_final)


# 6. Descriptives ####
gba_allyears_full_diagnoses <- fread("./Data/02_Reference_Data/20260212_alldiagnoses_allyears_allcols_cases.csv")  

#**  6.1. Diagnosis counts within and across years ----
extract_year <- function(x) as.numeric(stringr::str_extract(x, "\\d{4}$"))

count_years_long <- function(df, cols, dataset_name, subtype_name) { 
  tibble(
    dataset = dataset_name,
    subtype = subtype_name,
    year = as.character(extract_year(cols)),
    n_cases = purrr::map_int(cols, ~sum(df[[.x]] == TRUE, na.rm = TRUE))
  )
}

count_across_years <- function(df, cols, dataset_name, subtype_name, year_label) { 
  total <- df %>%
    mutate(any_year = rowSums(select(., all_of(cols)), na.rm = TRUE) > 0) %>%
    summarise(n_cases = sum(any_year)) %>%
    pull(n_cases)
  
  tibble(
    dataset = dataset_name, 
    subtype = subtype_name,
    year = as.character(year_label), 
    n_cases = total
  )
}


## Identify columns 
vars <- names(gba_allyears_full_diagnoses)

get_cols <- function(prefix) {
  vars[grepl(paste0("^", prefix, "\\d{4}$"), vars)]
}

get_cols_bp12 <- function(prefix) {
  vars[grepl(prefix, vars)]
}

# base datasets 
detailed_cols <- get_cols("^bipolar_detailed_") 
hoofd_cols <- get_cols("^bipolar_hoofd_")
vektis_cols <- get_cols("^bipolar_vektis_")

# subtypes
bipolar1_cols <- get_cols_bp12("^bipolar_1_detailed") 
bipolar2_cols <- get_cols_bp12("^bipolar_2_detailed") 
bipolar_mood_cols <- get_cols("^bipolar_and_mood_detailed_") 
cyclothymia_cols <- get_cols("^cyclothymia_detailed_") 
mood_dueto_somatic_cond_cols <- get_cols("^mood_dueto_somatic_cond_detailed_") 
substance_induced_all_cols <- get_cols("^substance_induced_all_detailed_") 


#*** 6.1.1. Whole Dataset ---- 
results <- bind_rows(
  
  # yearly counts 
  count_years_long(gba_allyears_full_diagnoses, detailed_cols, "detailed", "bipolar"), 
  count_years_long(gba_allyears_full_diagnoses, hoofd_cols, "hoofd", "bipolar_and_mood"), 
  count_years_long(gba_allyears_full_diagnoses, vektis_cols, "vektis", "bipolar_and_mood"), 
  
  count_years_long(gba_allyears_full_diagnoses, bipolar1_cols, "detailed", "bipolar1"), 
  count_years_long(gba_allyears_full_diagnoses, bipolar2_cols, "detailed", "bipolar2"), 
  count_years_long(gba_allyears_full_diagnoses, bipolar_mood_cols, "detailed", "bipolar_and_mood"), 
  count_years_long(gba_allyears_full_diagnoses, cyclothymia_cols, "detailed", "cyclothymia"), 
  count_years_long(gba_allyears_full_diagnoses, mood_dueto_somatic_cond_cols, "detailed", "mood_dueto_somatic_cond"), 
  count_years_long(gba_allyears_full_diagnoses, substance_induced_all_cols, "detailed", "substance_induced_all"), 
  
  # across all years 
  count_across_years(gba_allyears_full_diagnoses, detailed_cols, "detailed", "bipolar", "all_years"), 
  count_across_years(gba_allyears_full_diagnoses, hoofd_cols, "hoofd", "bipolar_and_mood", "all_years"), 
  count_across_years(gba_allyears_full_diagnoses, vektis_cols, "vektis", "bipolar_and_mood", "all_years"), 
  
  count_across_years(gba_allyears_full_diagnoses, bipolar1_cols, "detailed", "bipolar1", "all_years"), 
  count_across_years(gba_allyears_full_diagnoses, bipolar2_cols, "detailed", "bipolar2", "all_years"), 
  count_across_years(gba_allyears_full_diagnoses, bipolar_mood_cols, "detailed", "bipolar_and_mood", "all_years"), 
  count_across_years(gba_allyears_full_diagnoses, cyclothymia_cols, "detailed", "cyclothymia", "all_years"), 
  count_across_years(gba_allyears_full_diagnoses, mood_dueto_somatic_cond_cols, "detailed", "mood_dueto_somatic_cond", "all_years"), 
  count_across_years(gba_allyears_full_diagnoses, substance_induced_all_cols, "detailed", "substance_induced_all", "all_years") 
  
)

results <- results %>%
  arrange(dataset, subtype, year)

results_entirepopdata <- results %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = n_cases
  ) %>%
  relocate('all_years', .after = last_col()) %>%
  mutate(sample = "Entire Population")


#*** 6.1.2. Population Data ---- 
popdata <- fread("./Data/03_Final_Data/20260716_popdata_detailedonly.csv")
popdata <- popdata %>%
  left_join(gba_allyears_full_diagnoses, by = "RINPERSOON")

results <- bind_rows(
  
  # yearly counts 
  count_years_long(popdata, detailed_cols, "detailed", "bipolar"), 
  count_years_long(popdata, hoofd_cols, "hoofd", "bipolar_and_mood"), 
  count_years_long(popdata, vektis_cols, "vektis", "bipolar_and_mood"), 
  
  count_years_long(popdata, bipolar1_cols, "detailed", "bipolar1"), 
  count_years_long(popdata, bipolar2_cols, "detailed", "bipolar2"), 
  count_years_long(popdata, bipolar_mood_cols, "detailed", "bipolar_and_mood"), 
  count_years_long(popdata, cyclothymia_cols, "detailed", "cyclothymia"), 
  count_years_long(popdata, mood_dueto_somatic_cond_cols, "detailed", "mood_dueto_somatic_cond"), 
  count_years_long(popdata, substance_induced_all_cols, "detailed", "substance_induced_all"), 
  
  # across all years 
  count_across_years(popdata, detailed_cols, "detailed", "bipolar", "all_years"), 
  count_across_years(popdata, hoofd_cols, "hoofd", "bipolar_and_mood", "all_years"), 
  count_across_years(popdata, vektis_cols, "vektis", "bipolar_and_mood", "all_years"), 
  
  count_across_years(popdata, bipolar1_cols, "detailed", "bipolar1", "all_years"), 
  count_across_years(popdata, bipolar2_cols, "detailed", "bipolar2", "all_years"), 
  count_across_years(popdata, bipolar_mood_cols, "detailed", "bipolar_and_mood", "all_years"), 
  count_across_years(popdata, cyclothymia_cols, "detailed", "cyclothymia", "all_years"), 
  count_across_years(popdata, mood_dueto_somatic_cond_cols, "detailed", "mood_dueto_somatic_cond", "all_years"), 
  count_across_years(popdata, substance_induced_all_cols, "detailed", "substance_induced_all", "all_years") 
  
)

results <- results %>%
  arrange(dataset, subtype, year)

results_popdata_wide <- results %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = n_cases
  ) %>%
  relocate('all_years', .after = last_col()) %>%
  mutate(sample = "Population with Valid Data")


#*** 6.1.3. Sibling Data ---- 

sibdata <- fread("./Data/03_Final_Data/20260716_sibdata_detailedonly.csv")

sibdata <- sibdata %>%
  left_join(gba_allyears_full_diagnoses, by = "RINPERSOON")

results <- bind_rows(
  
  # yearly counts 
  count_years_long(sibdata, detailed_cols, "detailed", "bipolar"), 
  count_years_long(sibdata, hoofd_cols, "hoofd", "bipolar_and_mood"), 
  count_years_long(sibdata, vektis_cols, "vektis", "bipolar_and_mood"), 
  
  count_years_long(sibdata, bipolar1_cols, "detailed", "bipolar1"), 
  count_years_long(sibdata, bipolar2_cols, "detailed", "bipolar2"), 
  count_years_long(sibdata, bipolar_mood_cols, "detailed", "bipolar_and_mood"), 
  count_years_long(sibdata, cyclothymia_cols, "detailed", "cyclothymia"), 
  count_years_long(sibdata, mood_dueto_somatic_cond_cols, "detailed", "mood_dueto_somatic_cond"), 
  count_years_long(sibdata, substance_induced_all_cols, "detailed", "substance_induced_all"), 
  
  # across all years 
  count_across_years(sibdata, detailed_cols, "detailed", "bipolar", "all_years"), 
  count_across_years(sibdata, hoofd_cols, "hoofd", "bipolar_and_mood", "all_years"), 
  count_across_years(sibdata, vektis_cols, "vektis", "bipolar_and_mood", "all_years"), 
  
  count_across_years(sibdata, bipolar1_cols, "detailed", "bipolar1", "all_years"), 
  count_across_years(sibdata, bipolar2_cols, "detailed", "bipolar2", "all_years"), 
  count_across_years(sibdata, bipolar_mood_cols, "detailed", "bipolar_and_mood", "all_years"), 
  count_across_years(sibdata, cyclothymia_cols, "detailed", "cyclothymia", "all_years"), 
  count_across_years(sibdata, mood_dueto_somatic_cond_cols, "detailed", "mood_dueto_somatic_cond", "all_years"), 
  count_across_years(sibdata, substance_induced_all_cols, "detailed", "substance_induced_all", "all_years") 
  
)

results_sibdata <- results %>%
  arrange(dataset, subtype, year)

results_sibdata_wide <- results_sibdata %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = n_cases
  ) %>%
  relocate('all_years', .after = last_col()) %>%
  mutate(sample = "Sibling")

results_wide <- rbind(results_entirepopdata, results_popdata_wide, results_sibdata_wide)

## prepare dataset for exporting (some cells have <10 individuals, especially in sibling sample. need to remove this)
## I will only include 'bipolar', 'bipolar and mood'.

results_wide_export <- results_wide %>% 
  filter(subtype == "bipolar" | subtype == "bipolar_and_mood")

write.csv(results_wide_export, "./Results/Descriptives/bipolar_diagnosis_counts.csv", row.names = F) 


#**  6.2. Check Noise in the Detailed 2011 - 2016 Dataset ----

count_any <- function(cols) { 
  gba_allyears_full_diagnoses %>%
    mutate(any = rowSums(select(., all_of(cols)), na.rm = TRUE) > 0) %>%
    summarise(n_cases = sum(any)) %>%
    pull(n_cases)
}

# total counts over all years
n_bipolar_all <- count_any(detailed_cols)
n_bipolar1_all <- count_any(bipolar1_cols)
n_bipolar2_all <- count_any(bipolar2_cols)
n_cyclothymia_all <- count_any(cyclothymia_cols)
n_mood_dueto_somatic_cond_all <- count_any(mood_dueto_somatic_cond_cols)
n_substance_induced_all <- count_any(substance_induced_all_cols)
n_mood_all <- count_any(bipolar_mood_cols)


# build final table
percentage_table <- tibble(
  subtype = c("bipolar", "bipolar1", "bipolar2", "cyclothymia", "mood_dueto_somatic_cond", "substance_induced"), 
  
  year_range = c("all_years", "all_years", "all_years", "all_years", "all_years", "all_years"),
  
  cases = c(n_bipolar_all, n_bipolar1_all, n_bipolar2_all, n_cyclothymia_all, n_mood_dueto_somatic_cond_all, n_substance_induced_all),
  
  bipolar_and_mood_cases = c(n_mood_all, n_mood_all, n_mood_all, n_mood_all, n_mood_all, n_mood_all),
  
  percent_ind_diag_by_total = round(c(
    n_bipolar_all / n_mood_all * 100,
    n_bipolar1_all / n_mood_all * 100,
    n_bipolar2_all / n_mood_all * 100,
    n_cyclothymia_all / n_mood_all * 100,
    n_mood_dueto_somatic_cond_all / n_mood_all * 100,
    n_substance_induced_all / n_mood_all * 100
  ), 2)
)

write.csv(percentage_table, "./Results/Descriptives/detailed_dataset_noise.csv", row.names = F)  
write.csv(percentage_table, "./Results/Export/detailed_dataset_noise.csv", row.names = F)  


# 7. Sensitivity Checks ####

## the 2015 - 2021 + the vektis dataset contains only diagnosis at the 'hoofd' level 
## which means we have included those under 'Bipolar and other mood disorders' as cases

gba_allyears_full_diagnoses <- fread("./Data/02_Reference_Data/20260212_alldiagnoses_allyears_allcols_cases.csv") 


#* 7.1. Check overlap and mismatch ----
names(gba_allyears_full_diagnoses)
table(gba_allyears_full_diagnoses$detailed_bipolar_allyears) # 43,929 cases
table(gba_allyears_full_diagnoses$hoofd_bipolar_allyears) # 58,348  cases
table(gba_allyears_full_diagnoses$vektis_bipolar_allyears) # 59,232 cases

table(gba_allyears_full_diagnoses$alldatasets_bipolar_allyears) # 75,202 cases

sum(gba_allyears_full_diagnoses$hoofd_bipolar_allyears == 1 &
      gba_allyears_full_diagnoses$detailed_bipolar_allyears == 1, na.rm = TRUE)
## out of 52663 in hoofd dataset, 27260 also have a diagnosis in detailed dataset

sum(gba_allyears_full_diagnoses$vektis_bipolar_allyears == 1 &
      gba_allyears_full_diagnoses$detailed_bipolar_allyears == 1, na.rm = TRUE)
## out of 53322 in vektis dataset, 27172 also have a diagnosis in detailed dataset

sum(gba_allyears_full_diagnoses$detailed_bipolar_allyears == 1 &
      gba_allyears_full_diagnoses$hoofd_bipolar_allyears == 1 &
      gba_allyears_full_diagnoses$vektis_bipolar_allyears == 1, na.rm = TRUE)
## 23397 have a diagnosis in all 3 datasets (out of 75,202 in total)

sum(gba_allyears_full_diagnoses$hoofd_bipolar_allyears == 1 &
      gba_allyears_full_diagnoses$vektis_bipolar_allyears == 1, na.rm = TRUE)
## 39402 have a diagnosis in both hoofd and vektis


#* 7.2. Check noise (within year and across years) ----

## I no longer have the dataset - I couldn't save it since it was too big. 
## I can rerun the first part of this script to get the dataset 

# check percentage of noise 
check_peryear_test <- gba_2011_2016 %>% 
  group_by(year) %>%
  summarise (
    
    # bipolar counts
    bipolar_primary = sum(bipolar_main == TRUE, na.rm = TRUE),
    bipolar_second = sum(bipolar_second == TRUE, na.rm = TRUE),
    bipolar_both = sum(bipolar == TRUE, na.rm = TRUE),
    
    # denominators
    bpmood_primary = sum(bipolar_and_mood_main == TRUE, na.rm = TRUE),
    bpmood_second = sum(bipolar_and_mood_second == TRUE, na.rm = TRUE),
    bpmood_both = sum(bipolar_and_mood == TRUE, na.rm = TRUE)
  ) %>%
  
  # convert to long format
  pivot_longer(
    cols = c(bipolar_primary, bipolar_second, bipolar_both,
             bpmood_primary, bpmood_second, bpmood_both),
    names_to = c(".value", "dataset"),
    names_pattern = "(bipolar|bpmood)_(primary|second|both)"
  ) %>%
  
  # clean names
  mutate( 
    dataset = recode(dataset,
                     "primary" = "primary",
                     "second"= "second",
                     "both" = "both"),
    
    
    pct_bipolar = 
      if_else(bpmood > 0, bipolar/bpmood * 100, NA_real_)
  ) %>%
  
  # final column names
  rename(
    n_bipolar = bipolar,
    n_bipolar_mood = bpmood) %>%
  
  select(year, dataset, n_bipolar, n_bipolar_mood, pct_bipolar)

# check across years table
check_allyears_test <- gba_2011_2016 %>%
  group_by(RINPERSOON) %>%
  summarise (
    
    # counts
    across_primary = any(bipolar_main == TRUE, na.rm = TRUE),
    across_second = any(bipolar_second == TRUE, na.rm = TRUE),
    across_both = any(bipolar == TRUE, na.rm = TRUE),
    
    # denominators
    across_bpmood_primary = any(bipolar_and_mood_main == TRUE, na.rm = TRUE),
    across_bpmood_second = any(bipolar_and_mood_second == TRUE, na.rm = TRUE),
    across_bpmood_both = any(bipolar_and_mood == TRUE, na.rm = TRUE),
    .group = "drop"
  ) %>%
  
  summarise(
    
    # counts
    bipolar_primary = sum(across_primary),
    bipolar_second = sum(across_second),
    bipolar_both = sum(across_both),
    
    # denominators
    bpmood_primary = sum(across_bpmood_primary),
    bpmood_second = sum(across_bpmood_second),
    bpmood_both = sum(across_bpmood_both)
  ) %>%
  
  # convert to long format
  pivot_longer(
    cols = c(bipolar_primary, bipolar_second, bipolar_both,
             bpmood_primary, bpmood_second, bpmood_both),
    names_to = c(".value", "dataset"),
    names_pattern = "(bipolar|bpmood)_(primary|second|both)"
  ) %>%
  
  # clean names
  mutate( 
    dataset = recode(dataset,
                     "primary" = "primary",
                     "second"= "second",
                     "both" = "both"),
    
    pct_bipolar = 
      if_else(bpmood > 0, bipolar/bpmood * 100, NA_real_),
    year = "allyears"
  ) %>%
  
  # final column names
  rename(
    n_bipolar = bipolar,
    n_bipolar_mood = bpmood) %>%
  
  select(year, dataset, n_bipolar, n_bipolar_mood, pct_bipolar)

# combine datasets
check_perdataset_test <- bind_rows(check_peryear_test, check_allyears_test) %>%
  arrange(year,dataset)


write_csv(check_perdataset, "./Results/bipolar_disorder_diagnosis_noise_20251211.csv")
check_perdataset <- fread("./Results/bipolar_disorder_diagnosis_noise_20251211.csv")


#* 7.3. Check number of times an individual received a diagnoses ----

gba_2011_2016_full_diagnosis_wide <- fread("./Data/02_Reference_Data/20260212_detailed_allyears_cases.csv")  

#** 7.3.1. Bipolar Disorder Diagnoses ----
## primary
gba_2011_2016_full_diagnosis_wide  <- gba_2011_2016_full_diagnosis_wide %>%
  mutate(
    n_bipolar_main_detailed = rowSums(
      across(starts_with("bipolar_main_detailed_20"), ~.x ==TRUE),
      na.rm = TRUE
    ), 
    n_bipolar_second_detailed = rowSums(
      across(starts_with("bipolar_second_detailed_20"), ~.x ==TRUE),
      na.rm = TRUE
    ),
    n_bipolar_both_detailed = rowSums(
      across(starts_with("bipolar_detailed_20"), ~.x ==TRUE),
      na.rm = TRUE
    )
  )

table(gba_2011_2016_full_diagnosis_wide$detailed_bipolar_allyears) # 43929 cases and 28729 controls 
table(gba_2011_2016_full_diagnosis_wide$n_bipolar_main_detailed)
#    0     1     2     3     4     5     6 
# 32216 10190  6591  6203  5012  5648  6348 

table(gba_2011_2016_full_diagnosis_wide$n_bipolar_second_detailed)
#    0     1     2     3     4     5     6 
# 55420  9912  3795  1620   687   519   255 

table(gba_2011_2016_full_diagnosis_wide$n_bipolar_both_detailed)
#  0     1     2     3     4     5     6 
# 28279 11378  7233  6758  5520  6181  6859 


#** 7.3.2. Bipolar and Other Mood Disorders Diagnoses ----

## main
gba_2011_2016_full_diagnosis_wide  <- gba_2011_2016_full_diagnosis_wide %>%
  mutate(
    n_bipolar_and_mood_main_detailed = rowSums(
      across(starts_with("bipolar_and_mood_main_detailed_20"), ~.x ==TRUE),
      na.rm = TRUE
    ),
    n_bipolar_and_mood_second_detailed = rowSums(
      across(starts_with("bipolar_and_mood_second_detailed_20"), ~.x ==TRUE),
      na.rm = TRUE
    ),
    n_bipolar_and_mood_both_detailed = rowSums(
      across(starts_with("bipolar_and_mood_detailed_20"), ~.x ==TRUE),
      na.rm = TRUE
    )
  )

table(gba_2011_2016_full_diagnosis_wide$n_bipolar_and_mood_main_detailed)
#     0     1     2     3     4     5     6 
#   11887 25421  9891  7199  5404  5862  6544 

table(gba_2011_2016_full_diagnosis_wide$n_bipolar_and_mood_second_detailed)
#     0     1     2     3     4     5     6 
#   42006 19534  6245  2432  1017   672   302

table(gba_2011_2016_full_diagnosis_wide$n_bipolar_and_mood_both_detailed)
#     1     2     3     4     5     6 
#   31444 12277  8516  6288  6574  7109 


#** 7.3.3. Compile in a table ----

gba_2011_2016_full_diagnosis_wide <- gba_2011_2016_full_diagnosis_wide %>%
  mutate(
    detailed_bipolar_main_allyears  = as.integer(
      rowSums(across(starts_with("bipolar_main_")), na.rm = TRUE) > 0),
    detailed_bipolar_second_allyears  = as.integer(
      rowSums(across(starts_with("bipolar_second_")), na.rm = TRUE) > 0),
    detailed_bipolar_and_mood_main_allyears  = as.integer(
      rowSums(across(starts_with("bipolar_and_mood_main_")), na.rm = TRUE) > 0),
    detailed_bipolar_and_mood_second_allyears  = as.integer(
      rowSums(across(starts_with("bipolar_and_mood_second_")), na.rm = TRUE) > 0)
    )

  
case_counts <- gba_2011_2016_full_diagnosis_wide %>%
  summarise(
    n_bipolar = sum(detailed_bipolar_allyears == 1, na.rm = TRUE),
    n_bipolar_and_mood = sum(detailed_bipolar_and_mood_allyears),
    n_bipolar_main = sum(detailed_bipolar_main_allyears),
    n_bipolar_second = sum(detailed_bipolar_second_allyears),
    n_bipolar_and_mood_main = sum(detailed_bipolar_and_mood_main_allyears),
    n_bipolar_and_mood_second = sum(detailed_bipolar_and_mood_second_allyears)
  )

diagnosis_counts <- gba_2011_2016_full_diagnosis_wide %>%
  select(starts_with("n_")) %>%
  pivot_longer(
    everything(),
    names_to = "diagnosis_group",
    values_to = "count"
  ) %>%
  mutate( 
    n_diagnoses = factor(count, levels = 1:7)
  ) %>%
  count(diagnosis_group, count) %>%
  pivot_wider(
    names_from = count,
    values_from = n,
    values_fill = 0
  ) %>%
  select("diagnosis_group", "1", "2", "3", "4", "5", "6") %>%
  mutate(
    n_cases = case_when(
      grepl("^n_bipolar_and_mood_both", diagnosis_group) ~ case_counts$n_bipolar_and_mood, 
      grepl("^n_bipolar_and_mood_main", diagnosis_group) ~ case_counts$n_bipolar_and_mood_main, 
      grepl("^n_bipolar_and_mood_second", diagnosis_group) ~ case_counts$n_bipolar_and_mood_second, 
      grepl("^n_bipolar_both", diagnosis_group) ~ case_counts$n_bipolar, 
      grepl("^n_bipolar_main", diagnosis_group) ~ case_counts$n_bipolar_main, 
      grepl("^n_bipolar_second", diagnosis_group) ~ case_counts$n_bipolar_second, 
      TRUE ~NA_real_
            )
    ) %>%
      relocate(n_cases, .after = diagnosis_group) 

diagnosis_counts$n_cases <- as.numeric(diagnosis_counts$n_cases)

colnames(diagnosis_counts)
  
diagnosis_counts_pct <- diagnosis_counts %>%
  rename(
    once = 1,
    twice = 2, 
    thrice = 3,
    fource = 4,
    fivece = 5,
    sixce = 6) %>%
  mutate(
    across(
      .cols = -c("n_cases", "diagnosis_group"),
      .fns = ~.x / n_cases * 100,
      .names = "pct_{.col}"
    )) %>%
  mutate(
    more_than_once = twice + thrice + fource + fivece + sixce,
    more_than_once_pct = more_than_once / n_cases * 100 ) 

write.csv(diagnosis_counts_pct, "./Results/bipolar_diagnosis_counts_20251218.csv", row.names = F)  
diagnosis_counts <- read.csv("./Results/bipolar_diagnosis_counts_20251218.csv")

## recurring diagnoses is more common for bipolar than bipolar and mood disorder. That's why the average across years is lower for bipolar than bipolar and mood disorders, even though the proportion within each year is relatively similar. 


#** 7.3.4. Check Graphs ----

diagnosis_counts_plot <- diagnosis_counts_pct %>%
  pivot_longer(
    cols = pct_once:pct_sixce,
    names_to = "frequency", 
    values_to = "count"
  )

diagnosis_counts_plot$frequency <- factor(
  diagnosis_counts_plot$frequency,
  levels = c("pct_once", "pct_twice", "pct_thrice", "pct_fource", "pct_fivece", "pct_sixce"),
  labels = c("pct_once", "pct_twice", "pct_thrice", "pct_fource", "pct_fivece", "pct_sixce")
)

counts_plot <- ggplot(diagnosis_counts_plot, aes(x=frequency, y=count)) + 
  geom_col(fill = "steelblue") + 
  facet_wrap(~ diagnosis_group, scale = "free_y") + 
  labs(
    x = "Frequency", 
    y = "Count", 
    title = "Distribuition of Occurances"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

counts_plot

ggsave("./Results/bipolar_diagnosis_counts_20260202.jpg", plot = counts_plot, width = 10, height = 5, dpi = 300)

