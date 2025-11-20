#############################################
# artemis
# Author: Alisson Licona
# Description: Connect to an OMOP-CDM database
#              and use artemis for 
#              oncology regimen identification.
#############################################

# ---- Install required packages if not already installed ----
# install.packages("reticulate")
library(reticulate)
use_virtualenv("~/.virtualenvs/r-reticulate", required = TRUE)
# reticulate::py_install("numpy")
# reticulate::py_install("pandas")
# devtools::install_github("OHDSI/ARTEMIS")
# devtools::install_github("OHDSI/CohortGenerator")
# devtools::install_github("OHDSI/CirceR")

# ---- Connection to DB ----
library(DatabaseConnector)

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms     = "postgresql",
  user     = "postgres",
  password = "",
  server   = "localhost/oncoregimenfinder",
  port     = 5432,
  pathToDriver = "c:/temp/jdbcDrivers"
)

conn <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(conn), add = TRUE)



# ---- User script ----
library(ARTEMIS)
library(dplyr)
library(CDMConnector)


##### INPUT #####

# ---- Cohort definition ----
cohortSet <- CDMConnector::readCohortSet(path = here::here("C:/Users/pc/Desktop/to_share_vm/BreastCancer/myCohort/"))
# verify str(cohortSet)


name <- "breastCancer"

# ---- Create valid drugs if needed ----
# If valid drugs exists already, just load it
#validdrugs <- loadDrugs()
#sqlValidDrugs <- DatabaseConnector::querySql(conn, "
#  SELECT DISTINCT
#    de.drug_concept_id AS valid_concept_id,
#    c.concept_name
#  FROM public.drug_exposure de
#  JOIN public.concept c
#    ON de.drug_concept_id = c.concept_id
#  WHERE c.standard_concept = 'S'
#    AND c.domain_id = 'Drug'
#    AND LOWER(c.concept_class_id) = 'ingredient';
#")

sqlValidDrugs <- DatabaseConnector::querySql(conn,"
                                            WITH selected_atc AS (
  SELECT concept_id_atc, concept_code_atc
  FROM public.stg_atc_rxnorm
  WHERE concept_code_atc IN (
    'L01FD01',  -- trastuzumab
    'L01FD02',  -- pertuzumab
    'L01FD03',  -- ado-trastuzumab emtansine
    'L01FD04',  -- trastuzumab deruxtecan
    'L01EH03',  -- tucatinib
    'L01EH02',  -- neratinib
    'L01EH01',  -- lapatinib
    'L01FD06',  -- margetuximab
    'L01CD01',  -- paclitaxel
    'L01CD02',  -- docetaxel
    'L01DB01',  -- doxorubicin
    'L01AA01',  -- cyclophosphamide
    'L01BC06',  -- capecitabine
    'L02BA01',  -- tamoxifen
    'L02BG04',  -- letrozole
    'L02BG03',  -- anastrozole
    'L02BG06',  -- exemestane
    'L02BA03',  -- fulvestrant
    'L01EM03',  -- alpelisib
    'L01EF01',  -- palbociclib
    'L01EF02',  -- ribociclib
    'L01EF03'   -- abemaciclib
  )
), rxnorm_ingredients AS (
  SELECT DISTINCT
    s.concept_id_atc,
    s.concept_code_atc,
    r.concept_id_rxnorm AS concept_id_rxnorm
  FROM selected_atc s
  JOIN public.stg_atc_rxnorm r
    ON s.concept_id_atc = r.concept_id_atc
), rxnorm_to_hemonc AS (
  SELECT DISTINCT
    r.concept_id_rxnorm,
    c_rx.concept_name AS rxnorm_name,
    cr.concept_id_2 AS concept_id_hemonc,
    c_hemonc.concept_name AS hemonc_name,
    c_hemonc.domain_id,
    c_hemonc.concept_class_id
  FROM rxnorm_ingredients r
  JOIN public.concept c_rx
    ON r.concept_id_rxnorm = c_rx.concept_id
  JOIN public.concept_relationship cr
    ON r.concept_id_rxnorm = cr.concept_id_1
  JOIN public.concept c_hemonc
    ON cr.concept_id_2 = c_hemonc.concept_id
  WHERE c_hemonc.vocabulary_id = 'HemOnc'
    AND c_hemonc.invalid_reason IS NULL
    AND c_hemonc.domain_id = 'Drug'
    AND c_hemonc.concept_class_id = 'Component'
)
SELECT DISTINCT
    COALESCE(h.hemonc_name, c_rx.concept_name) AS name,
    COALESCE(h.concept_id_hemonc, r.concept_id_rxnorm) AS concept_id,
    COALESCE(h.concept_id_hemonc, r.concept_id_rxnorm) AS Manual,
    c_rx.concept_name AS concept_me,
    r.concept_id_rxnorm AS valid_concept_id,
    COALESCE(h.domain_id, c_rx.domain_id) AS domain_id,
    COALESCE(h.concept_class_id, c_rx.concept_class_id) AS concept_class_id,
    NULL AS Manual_Req,
    r.concept_code_atc
FROM rxnorm_ingredients r
JOIN public.concept c_rx ON r.concept_id_rxnorm = c_rx.concept_id
LEFT JOIN rxnorm_to_hemonc h   ON r.concept_id_rxnorm = h.concept_id_rxnorm

")

## Check if all drugs needed were added. If not, add manually:
new_drug <- data.frame(
  name = "ado-trastuzumab emtansine",
  concept_id = 35802866,                # HemOnc component ID
  manual = 35802866,                    # same as concept_id (manual tracking)
  concept_me = "ado-trastuzumab emtansine",
  valid_concept_id = 43525787,          # RxNorm precise ingredient ID
  domain_id = "Drug",
  concept_class_id = "Component",       # consistent with HemOnc vocab
  manual_req = NA,
  concept_code_atc = "L01FD03"
)
new_drug2 <- data.frame(
  name = "trastuzumab deruxtecan",
  concept_id = 42542260,                # HemOnc component ID
  manual = 42542260,                    # same as concept_id (manual tracking)
  concept_me = "trastuzumab deruxtecan",
  valid_concept_id = 36118944,          # RxNorm precise ingredient ID
  domain_id = "Drug",
  concept_class_id = "Component",       # consistent with HemOnc vocab
  manual_req = NA,
  concept_code_atc = "L01FD04"
)
validDrugs <- rbind(validDrugs, new_drug)
validDrugs <- rbind(validDrugs, new_drug2)

#write.csv(validDrugs, here::here("C:/Users/pc/Desktop/to_share_vm/BreastCancer/myCohort/validDrugs.csv"), row.names = FALSE)

# ---- Load valid drugs ----
validDrugs <- read.csv(here::here("C:/Users/pc/Desktop/to_share_vm/BreastCancer/myCohort/validDrugs.csv"))

# ---- Regimen definition ----
# Regimens by ARTEMIS
#regimens <- loadRegimens(condition = "all")
# Regimens by ETL_HemOnc_regimens
#load("C:/Users/pc/Desktop/to_share_vm/BreastCancer/HemOnc_output/regimens.rda")

# Create our own regimens
# 1. Filter our valid drugs first
con_df2 <- con_df[con_df$ancestor_concept_id %in% validDrugs$valid_concept_id,]
# 2. Second filter of drugs (excipients)
con_df2 <- con_df2[!con_df2$concept_name %in% c("sodium chloride", 
                                                "hyaluronidase", 
                                                "mannitol"), ]
# 3. Create regimen combinations based on our pts
regimens_emp <- con_df2 %>%
  mutate(drug_date = as.Date(drug_exposure_start_date)) %>%
  arrange(person_id, drug_date) %>%
  group_by(person_id) %>%
  mutate(
    interval = as.numeric(drug_date - lag(drug_date, default = first(drug_date))),
    block = cumsum(ifelse(is.na(interval) | interval > 30, 1, 0))
  ) %>%
  group_by(person_id, block) %>%
  summarise(
    drug_combo = paste(
      unique(tolower(gsub(" ", "", concept_name))),   #  temporal order
      collapse = "+"
    ),
    mean_days = mean(interval[interval <= 30], na.rm = TRUE),
    span_days = as.numeric(max(drug_date) - min(drug_date)),  # total span of that block
    .groups = "drop"
  ) %>%
  # Keep only regimens where all drugs are within 30 days
  filter(span_days <= 30)
# 4. Create list of unique regimen combinations
library(stringr)
regimens_empirical <- regimens_emp2 %>%
  # Create a canonical order-insensitive key (for naming)
  mutate(
    combo_key = sapply(strsplit(drug_combo, "\\+"), function(x) {
      paste(sort(unique(x)), collapse = "+")
    })
  ) %>%
  # Assign shared Emp_ names based on combo_key
  group_by(combo_key) %>%
  mutate(
    regName = paste0("Emp_", cur_group_id()),
    avg_days = round(mean(mean_days, na.rm = TRUE)),
    count = n()
  ) %>%
  ungroup() %>%
  distinct(drug_combo, regName, avg_days, count, .keep_all = TRUE) %>%
  rowwise() %>%
  mutate({
    drugs <- str_split(drug_combo, "\\+")[[1]]
    n <- length(drugs)
    
    # Raw cumulative spacing (0, avg, 2*avg, etc.)
    raw_increments <- cumsum(c(0, rep(avg_days, n - 1)))[1:n]
    
    # Scale so the maximum increment ≤ 30, round to integers
    if (max(raw_increments) > 0) {
      scaled_increments <- round((raw_increments / max(raw_increments)) * 30)
    } else {
      scaled_increments <- raw_increments
    }
    
    shortString <- paste0(paste0(scaled_increments, ".", drugs, ";"), collapse = "")
    tibble(shortString = shortString)
  }) %>%
  ungroup() %>%
  select(regName, shortString, count, avg_days) %>%
  arrange(regName)
## Add repetitions to get multi regimens with same drugs
single_drug_regs <- regimens_empirical %>%
  mutate(
    # Extract drug names after the digits and dot
    drugs = str_extract_all(shortString, "[0-9]+\\.([a-z0-9]+)"),
    # Clean numeric prefixes (remove digits and dots)
    drugs = lapply(drugs, function(x) gsub("^[0-9]+\\.", "", x)),
    n_drugs = sapply(drugs, function(x) length(unique(x)))
  ) %>%
  filter(n_drugs == 1)

replicated_single_drug_regs <- single_drug_regs %>%
  rowwise() %>%
  mutate(
    drug = gsub("^[0-9]+\\.", "", str_extract(shortString, "[0-9]+\\.[a-z0-9]+")),
    avg = avg_days
  ) %>%
  do({
    tibble(
      regName = .$regName,
      count = .$count,
      avg_days = .$avg_days,
      shortString = c(
        # 1x
        paste0("0.", .$drug, ";"),
        # 2x
        paste0("0.", .$drug, ";1.", .$drug, ";"),
        # 3x
        paste0("0.", .$drug, ";1.", .$drug, ";2.", .$drug, ";")
      )
    )
  }) %>%
  ungroup()
# Merge first version + repetitions
regimens_empirical <- bind_rows(regimens_empirical, replicated_single_drug_regs) %>%
  distinct(shortString, .keep_all = TRUE)
# Add cycle length data
regimens_final <- regimens_empirical %>%
  mutate(cycleLength = avg_days) %>%
  select(regName, shortString, cycleLength, count, avg_days)

# 5. Prepare regGroups
#Load groups
regimens_list <- read.csv(
  here::here("C:/Users/pc/Desktop/to_share_vm/BreastCancer/myCohort/regimens_list.csv"),
  sep = ";"
)

#Compare regimens_list and regimens_empirical2
library(purrr)
regimens_list_clean <- regimens_list %>%
  mutate(
    # extract all drug names after numbers and dots, before semicolons
    key = str_extract_all(shortString, "(?<=\\.)[a-z0-9_]+(?=;)") %>%
      map(~ sort(tolower(.x))) %>%              # sort alphabetically
      map_chr(~ paste(.x, collapse = "+"))      # join into single key
  )

regimens_empirical_clean <- regimens_final %>%
  mutate(
    key = str_extract_all(shortString, "(?<=\\.)[a-z0-9_]+(?=;)") %>%
      map(~ sort(tolower(.x))) %>%
      map_chr(~ paste(.x, collapse = "+"))
  )

regimens_total_groups <- regimens_empirical_clean %>%
  left_join(
    regimens_list_clean %>% select(key, Groups, Groups2),
    by = "key"
  ) %>%
  group_by(regName) %>%
  mutate(
    # Fill missing group labels within the same regimen name
    Groups = first(na.omit(Groups)),
    Groups2 = first(na.omit(Groups2))
  ) %>%
  ungroup() %>%
  select(regName, shortString, count, avg_days, Groups, Groups2, key)

missing_groups <- regimens_total_groups %>%
  filter(is.na(Groups) | trimws(Groups) == "") %>%
  
  distinct(regName, shortString, key) %>%
  
  rowwise() %>%
  mutate(
    n_unique_drugs = length(unique(str_split(key, "\\+")[[1]]))
  ) %>%
  ungroup() %>%
  select(-n_unique_drugs)

regimens_total_groups_extended <- regimens_total_groups %>%
  group_by(regName) %>%
  mutate(
    Groups = ifelse(is.na(Groups),
                    first(na.omit(Groups)),
                    Groups),
    Groups2 = ifelse(is.na(Groups2),
                     first(na.omit(Groups2)),
                     Groups2)
  ) %>%
  ungroup()

regGroups <- regimens_total_groups_extended %>%
  select(regName, Groups) %>%
  distinct() %>%        # keep unique combinations only
  arrange(regName) 

# ---- Schema definition ----
cdmSchema <- "public"
writeSchema <- "artemis"


##### MAIN #####
# Modified version of getConDF (use of ingredients, no branded drugs)
# Retrieves drug exposure data for all pts under cohort conditions
#source("getConDF_noAncestor.R")
con_df <- getConDF_noAncestor(connectionDetails = connectionDetails, json = cohortSet, name = name, cdmSchema = cdmSchema, writeSchema = writeSchema)
# Creates string of drug combinations for each patient, based on valid drugs
stringDF <- stringDF_from_cdm(con_df = con_df2, writeOut = F, validDrugs = validDrugs)

# Split single vs multi regimen
regimens_single <- regimens_final %>% filter(str_count(shortString, ";") == 1)
regimens_multi  <- regimens_final %>% filter(str_count(shortString, ";") > 1)

# Fix stringDF for those who have only one drug
library(stringr)
library(tidyr)
library(purrr)
stringDF_fixed <- stringDF %>%
  rowwise() %>%
  mutate(
    # Store matches as list-columns
    drugs = list(str_extract_all(seq, "(?<=\\.)[A-Za-z0-9_]+")[[1]]),
    days  = list(as.numeric(str_extract_all(seq, "[0-9]+(?=\\.)")[[1]])),
    
    # Count unique drugs
    n_unique = length(unique(drugs)),
    
    # Only add duplicate if there's exactly ONE unique drug
    seq = if (n_unique == 1 && length(days) > 0) {
      max_day <- max(unlist(days), na.rm = TRUE)
      drug <- unique(unlist(drugs))
      paste0(seq, (max_day + 1), ".", drug, ";")
    } else {
      seq
    }
  ) %>%
  ungroup() %>%
  select(person_id, seq)


# Generates alingments between pt drug combinations and regimens table
# Parameters can be changed. Now, they allow the max possible alignments
# Alingment for multi drug regimens
output_own <- own_generate_alignments(stringDF_fixed,regimens_multi)
# Check if any patient did not have any alignment
missing_ids <- setdiff(stringDF$person_id, output_own$personID)
stringDF_single <- stringDF[stringDF$person_id %in% missing_ids, ]

# Clean output
output_own2 <- output_own %>%
  mutate(
    across(c(regimen_Start, regimen_End, drugRec_Start, drugRec_End),
           ~ replace_na(as.numeric(.), 0))
  ) %>%
  mutate(
    drugRec_End = ifelse(drugRec_End < drugRec_Start, drugRec_Start, drugRec_End),
    regimen_End = ifelse(regimen_End < regimen_Start, regimen_Start, regimen_End)
  )

# Process alignments for drug regimens
processed_own <- postprocess_alignments(output_own, regimenCombine = 30)
# Process eras
processedEras_combined <- processed_own %>% calculateEras_local(discontinuationTime = 30)
# Generate basic stats for eras
regStats <- processedEras_combined %>% generateRegimenStats()


### Explore data
plotFrequency(processed_own)
#plotScoreDistribution(regimen1 = "Letrozole", regimen2 = "Tamoxifen", processedAll = processedAll_combined)
#plotRegimenLengthDistribution(regimen1 = "Aspirin and Dexamethasone", regimen2 = "Cyclophosphamide and Doxorubicin (AC)", processedAll = processedAll)



##### OUTPUT #####
# plotSankey_local(processedEras, regGroups, saveLocation = NA, fileName = "Network")
writeOutputs(output_all_combined, processedAll = processedAll_combined, processedEras = processedEras_combined,
             connectionDetails = connectionDetails, cdmSchema = cdmSchema, regGroups = regGroups,
             regStats = regStats, stringDF = stringDF, con_df = con_df)



#### Disconnet from db
DatabaseConnector::disconnect(connection)
