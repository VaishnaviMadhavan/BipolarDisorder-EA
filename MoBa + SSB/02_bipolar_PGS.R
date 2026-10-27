
# 
# Script: 02_bipolar_PGS.R 
# Project: Bipolar Disorder and Education: 
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Clean Bipolar Disorder PGS 
#
# Author: Vaishnavi K Madhavan
# Start date: 25-04-2025
# 

# 1. Set Up #### 

pgs <- read.table("./Data/Raw Data/Bipolar_Eur_ExclMoBa_Oconnell2025_LDpred2a.txt", header = TRUE, sep = "", stringsAsFactors = FALSE) %>%
                select(FID, IID, bipolar_pgs=Bipolar_Eur_ExclMoBa_Oconnell2025_LDpred2a)

bipolar_diag <- fread("./data/KUHR_summarized_36199047_Vaish_Bipolar_EA_update_2026-04-24.csv") %>%
                  select(PASIENTLOPENUMMER, sum_contact)

linkage <- fread("N:/durable/data/moba/linkage/merged_IDs/MoBa_SSB_IDs_20250317.csv") %>%
             select(w19_0634_lnr, SENTRIX_ID)

pop_sibinfo_covariates <- fread("./Data/Reference Data/pop_sibinfo_covariates.csv") %>%
             select(w19_0634_lnr, sex, birthyear, birthorder, FID = parents)


# 2. Clean Files  ####

## joining linkage with bipolar_pgs
nrow(pgs) # 207,283

nrow(linkage) # 311,324
sum(!is.na(linkage$SENTRIX_ID)) # 227,949

## remove those with NA for 
colnames(linkage) <- c("w19_0634_lnr", "IID") 
linkage <- linkage %>%
  filter(!is.na(linkage$IID))
nrow(linkage) # 227,949

## remove duplicates
check <- linkage[duplicated(linkage$IID) | duplicated(linkage$IID, fromLast = TRUE),]

fo_dupl <- linkage[duplicated(linkage$IID) | duplicated(linkage$IID, fromLast = TRUE),]
fo_dupl[fo_dupl == ""] <- NA
fo_dupl <- as.data.frame(fo_dupl)

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

# function: collapse_nonconflicting
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
  
  # combine and return (preserve original row order approximately by arranging by id)
  bind_rows(kept_conflicts, collapsed_safe) %>%
    # optional: arrange by id to keep groups together
    arrange(!!id_sym)
}

fo_dupl_clean <- collapse_nonconflicting(fo_dupl, id_col = "w19_0634_lnr", conflict_ids = problematic_ids)

linkage <- linkage[!linkage$w19_0634_lnr %in% fo_dupl$w19_0634_lnr,]
linkage <- rbind(linkage, fo_dupl_clean)

nrow(linkage) # 203,895


# 3. Merging Data  ####

nrow(pgs) # 207,283

pgs_w19 <- pgs %>%
  left_join(linkage, by = "IID")

nrow(pgs) # 207,283
nrow(pgs_w19) # 203,895

pgs_w19 <- pgs_w19 %>%
  filter(!is.na(w19_0634_lnr))
nrow(pgs_w19) # 203,876 (removed 19)

## merge diagnostic data onto the pgs data 
bp_pgs <- pgs_w19 %>%
  left_join(pop_sibinfo_covariates, by = "w19_0634_lnr") %>%
  left_join(bipolar_diag %>% rename(w19_0634_lnr = PASIENTLOPENUMMER), by = "w19_0634_lnr") 
nrow(bp_pgs) # 203,876

## changing into a dichotomous variable (keeping 5 number of contacts as a cut-off for having bipolar disorder)
bp_pgs$bipolar_diag <- ifelse(!is.na(bp_pgs$sum_contact) & bp_pgs$sum_contact > 4, 1, 0)
table(bp_pgs$bipolar_diag) # 1548 cases


# 4. Check Incremental R2 ####

## base model (covariates only)
base_model <- glm(bipolar_diag ~ sex + birthyear + birthorder,
                  data = bp_pgs, family = binomial)

## full model (covariates + PGS)
full_model <- glm(bipolar_diag ~ sex + birthyear + birthorder + bipolar_pgs,
                  data = bp_pgs, family = binomial)


#* 4.1. McFadden's pseudo-R2 ----
mcf_r2_base <- pR2(base_model)["McFadden"]
mcf_r2_full <- pR2(full_model)["McFadden"]

mcfd_r2 <- mcf_r2_full - mcf_r2_base
mcfd_r2 # 0.01823 (1.76%)


#* 4.2. McFadden's pseudo-R2 - Manual Method ----
mc_fadden_r2 <- 1 - (logLik(full_model) / logLik(base_model))
print(mc_fadden_r2) # 0.01892 (1.81%)


#* 4.3. Nagelkerke's R2 ----
install.packages("fmsb")
library(fmsb)

nagel_base <- NagelkerkeR2(base_model)
nagel_full <- NagelkerkeR2(full_model)

nagelkerke_r2 <- nagel_full$R2 - nagel_base$R2
nagelkerke_r2 # 0.0189 (1.84%)


# 5. Cleaning the Variable ####

table(is.na(bp_pgs$bipolar_pgs)) # everyone has a pgs
nrow(bp_pgs) # 203876

## remove duplicates
duplicates <- bp_pgs[duplicated(bp_pgs$w19_0634_lnr) | duplicated(bp_pgs$w19_0634_lnr, fromLast = TRUE), ]
nrow(unique(duplicates)) # 0

describe(bp_pgs$bipolar_pgs)
hist(bp_pgs$bipolar_pgs)

## scaling the variable
bp_pgs$std_bipolar_pgs <- scale(bp_pgs$bipolar_pgs)
hist(bp_pgs$std_bipolar_pgs)

write.csv(bp_pgs, "./Data/Reference Data/bipolar_disorder_polygenic_score.csv", row.names=FALSE)

