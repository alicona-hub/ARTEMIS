#############################################
# Treatment extraction/alignment for PMM
#############################################

# ---- Install required packages if not already installed ----
library(reticulate)
#use_virtualenv("~/.virtualenvs/r-reticulate", required = TRUE)

# ---- Connection to DB ----
library(DatabaseConnector)

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms     = "postgresql",
  user     = "postgres",
  password = "Traverse123&",
  server   = "localhost/ARTEMIS_PMM",
  port     = 5432,
  pathToDriver = "c:/temp/jdbcDrivers"
)

conn <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
DatabaseConnector::disconnect(conn)


# ---- User script ----
library(ARTEMIS)
library(dplyr)
library(CDMConnector)
library(CirceR)


##### INPUT #####

# ---- Cohort definition ----
#Test cohort
library(CirceR)
jsonPath <- "C:/Users/pc/Desktop/to_share_vm/ARTEMIS/definitions/PMMDefinitions/PMM_cohort.json"
expr <- CirceR::cohortExpressionFromJson(readChar(jsonPath, file.info(jsonPath)$size))
sql <- CirceR::buildCohortQuery(
  expression = expr,
  options = CirceR::createGenerateOptions()
)

cohortSet <- CDMConnector::readCohortSet(path = here::here("C:/Users/pc/Desktop/to_share_vm/ARTEMIS/definitions/PMMDefinitions"))
# verify str(cohortSet)


name <- "MM"

# ---- Create valid drugs if needed ----
# If valid drugs exists already, just load it
#validdrugs <- loadDrugs()
validDrugs <- DatabaseConnector::querySql(conn, "
WITH selected_atc AS (
  SELECT
      concept_id AS concept_id_atc,
      concept_code AS concept_code_atc,
      concept_name
  FROM cdm.concept
  WHERE vocabulary_id = 'ATC'
  AND concept_code IN (
    'H02AB02',
    'H02AB07',
    'L01AA01',
    'L01AA03',
    'L04AX02',
    'L04AX04',
    'L04AX06',
    'L01XG01',
    'L01XG02',
    'L01XG03',
    'L01FC01',
    'L01FC02',
    'L01XL05',
    'L01XL07',
    'L01XX52',
    'L01XX66'
  )
),

atc_to_hemonc AS (
  SELECT DISTINCT
      a.concept_id_atc,
      cr.concept_id_2 AS concept_id_hemonc,
      c_hemonc.concept_name AS hemonc_name,
      c_hemonc.domain_id,
      c_hemonc.concept_class_id
  FROM selected_atc a
  LEFT JOIN cdm.concept_relationship cr
      ON a.concept_id_atc = cr.concept_id_1
  LEFT JOIN cdm.concept c_hemonc
      ON cr.concept_id_2 = c_hemonc.concept_id
  WHERE c_hemonc.vocabulary_id = 'HemOnc'
    AND c_hemonc.invalid_reason IS NULL
    AND c_hemonc.domain_id = 'Drug'
    AND c_hemonc.concept_class_id = 'Component'
)

SELECT DISTINCT
    COALESCE(h.hemonc_name, a.concept_name) AS name,
    COALESCE(h.concept_id_hemonc, a.concept_id_atc) AS concept_id,
    COALESCE(h.concept_id_hemonc, a.concept_id_atc) AS Manual,
    a.concept_name AS concept_me,
    a.concept_id_atc AS valid_concept_id,
    COALESCE(h.domain_id, 'Drug') AS domain_id,
    COALESCE(h.concept_class_id, 'ATC') AS concept_class_id,
    NULL AS Manual_Req,
    a.concept_code_atc
FROM selected_atc a
LEFT JOIN atc_to_hemonc h
  ON a.concept_id_atc = h.concept_id_atc
")



#write.csv(validDrugs, here::here("C:/Users/pc/Desktop/to_share_vm/ARTEMIS/definitions/PMMDefinitions/validDrugs.csv"), row.names = FALSE)

# ---- Load valid drugs ----
validDrugs <- read.csv(here::here("C:/Users/pc/Desktop/to_share_vm/ARTEMIS/definitions/PMMDefinitions/validDrugs.csv"))

# ---- Regimen definition ----
# Regimens by ARTEMIS
regimens <- loadRegimens(condition = "all")
# Filter multiple myeloma
regimens <- regimens %>%
  filter(regimens$condition == "Multiple myeloma")
# Regimens by ETL_HemOnc_regimens
#load("C:/Users/pc/Desktop/to_share_vm/BreastCancer/HemOnc_output/regimens.rda")

# Create our own regimens
# 1. Filter our valid drugs first
#con_df2 <- con_df[con_df$ancestor_concept_id %in% validDrugs$valid_concept_id,]
# No needed for PMM since we filtered in con_df

# 3. Create regimen combinations based on our pts
regimens_emp_11mar <- con_df %>%
  mutate(drug_date = as.Date(drug_exposure_start_date)) %>%
  arrange(person_id, drug_date) %>%
  group_by(person_id) %>%
  mutate(
    # Create regimen blocks anchored on the first drug
    block = {
      anchor <- drug_date[1]
      b <- 1
      out <- numeric(length(drug_date))

      for(i in seq_along(drug_date)) {
        if(as.numeric(drug_date[i] - anchor) > 90) {
          b <- b + 1
          anchor <- drug_date[i]
        }
        out[i] <- b
      }
      out
    }
  ) %>%
  group_by(person_id, block) %>%
  summarise(
    # Build regimen combination in temporal order
    drug_combo = paste(
      unique(tolower(gsub(" ", "", concept_name))),
      collapse = "+"
    ),

    # Mean spacing between drugs inside regimen
    mean_days = mean(as.numeric(diff(drug_date)), na.rm = TRUE),

    # Total time span of regimen
    span_days = as.numeric(max(drug_date) - min(drug_date)),

    .groups = "drop"
  )


# 4. Create list of unique regimen combinations
library(stringr)
regimens_empirical_11mar <- regimens_emp_11mar %>%
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
      scaled_increments <- round((raw_increments / max(raw_increments)) * 56)
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
single_drug_regs_11mar <- regimens_empirical_11mar %>%
  mutate(
    # Extract drug names after the digits and dot
    drugs = str_extract_all(shortString, "[0-9]+\\.([a-z0-9]+)"),
    # Clean numeric prefixes (remove digits and dots)
    drugs = lapply(drugs, function(x) gsub("^[0-9]+\\.", "", x)),
    n_drugs = sapply(drugs, function(x) length(unique(x)))
  ) %>%
  filter(n_drugs == 1)

replicated_single_drug_regs_11mar <- single_drug_regs_11mar %>%
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
regimens_empirical_v2 <- bind_rows(regimens_empirical_11mar, replicated_single_drug_regs_11mar) %>%
  distinct(shortString, .keep_all = TRUE)
# Add cycle length data
regimens_final_v2 <- regimens_empirical_v2 %>%
  mutate(cycleLength = avg_days) %>%
  select(regName, shortString, cycleLength, count, avg_days)


# Check if we are missing groups
library(purrr)
regimens_list_clean <- regimen_groups %>%
  mutate(
    # extract all drug names after numbers and dots, before semicolons
    key = str_extract_all(shortString, "(?<=\\.)[a-z0-9_]+(?=;)") %>%
      map(~ sort(tolower(.x))) %>%              # sort alphabetically
      map_chr(~ paste(.x, collapse = "+"))      # join into single key
  )

new_groups_clean <- new_groups %>%
  mutate(
    key = str_extract_all(shortString, "(?<=\\.)[a-z0-9_]+(?=;)") %>%
      map(~ sort(tolower(.x))) %>%
      map_chr(~ paste(.x, collapse = "+"))
  )

regimens_list_clean <- bind_rows(
  regimens_list_clean,
  new_groups_clean
)

regimens_empirical_clean <- regimens_final_v2 %>%
  mutate(
    key = str_extract_all(shortString, "(?<=\\.)[a-z0-9_]+(?=;)") %>%
      map(~ sort(tolower(.x))) %>%
      map_chr(~ paste(.x, collapse = "+"))
  )

group_lookup <- regimens_list_clean %>%
  select(key, Group) %>%
  distinct(key, .keep_all = TRUE)

regimens_total_groups <- regimens_empirical_clean %>%
  left_join(group_lookup, by = "key") %>%
  group_by(regName) %>%
  mutate(Group = first(na.omit(Group))) %>%
  ungroup() %>%
  select(regName, shortString, count, avg_days, Group, key)

# Check if any null group
regimens_total_groups %>%
  filter(is.na(Group))

missing_groups <- regimens_total_groups %>%
  filter(is.na(Group) | trimws(Group) == "") %>%

  distinct(regName, shortString, key) %>%

  rowwise() %>%
  mutate(
    n_unique_drugs = length(unique(str_split(key, "\\+")[[1]]))
  ) %>%
  ungroup() %>%
  select(-n_unique_drugs)

# Download CSV
write.csv(missing_groups, "missing_groups_11mar.csv", row.names = FALSE)
write.csv(regimens_total_groups, "regimens_total_11mar.csv", row.names = FALSE)

new_groups <- read_excel("C:/Users/pc/Desktop/to_share_vm/ARTEMIS/definitions/PMMDefinitions/regimens_total_11mar.xlsx")


# 5. Prepare regGroups
#Load groups
library(readxl)
regimen_groups <- read_excel("C:/Users/pc/Desktop/to_share_vm/ARTEMIS/definitions/PMMDefinitions/regimens_new.xlsx", skip = 1)

regGroups <- new_groups %>%
  select(regName, Group) %>%
  distinct() %>%        # keep unique combinations only
  arrange(regName)

# ---- Schema definition ----
cdmSchema <- "cdm"
writeSchema <- "artemis"


##### MAIN #####
# Modified version of getConDF (use of ingredients, no branded drugs)
# Retrieves drug exposure data for all pts under cohort conditions
#source("getConDF_noAncestor.R")
con_df <- getConDF_noAncestor(connectionDetails = connectionDetails, json = cohortSet, name = name, cdmSchema = cdmSchema, writeSchema = writeSchema)
# Creates string of drug combinations for each patient, based on valid drugs
stringDF <- stringDF_from_cdm(con_df = con_df, writeOut = F, validDrugs = validDrugs)

# only for testing original pipeline
#output_all <- stringDF %>% generateRawAlignments(regimens = regimens_total_groups,
#                                                 g = 0.4,
#                                                 Tfac = 0.5,
#                                                 verbose = 0,
#                                                 mem = -1,
#                                                 removeOverlap = 1,
#                                                 method = "PropDiff")


# Split single vs multi regimen
#regimens_single <- regimens_final %>% filter(str_count(shortString, ";") == 1)
#regimens_multi  <- regimens_final %>% filter(str_count(shortString, ";") > 1)

# Fix stringDF for those who have only one drug
library(stringr)
library(tidyr)
library(purrr)
stringDF_fixed <- stringDF %>%
  mutate(
    drugs = str_extract_all(seq, "(?<=\\.)[a-zA-Z0-9_]+"),
    days  = str_extract_all(seq, "[0-9]+(?=\\.)")
  ) %>%
  mutate(
    n_unique = sapply(drugs, function(x) length(unique(x))),
    days = lapply(days, as.numeric)
  ) %>%
  mutate(
    seq = mapply(function(s, d, dr, n) {
      if (n == 1 && length(d) > 0) {
        paste0(s, max(d) + 1, ".", unique(dr), ";")
      } else {
        s
      }
    }, seq, days, drugs, n_unique)
  ) %>%
  select(person_id, seq)


# Generates alingments between pt drug combinations and regimens table
# Parameters can be changed. Now, they allow the max possible alignments
# Alingment for multi drug regimens
output_own <- own_generate_alignments(stringDF_fixed,regimens_final_v2)

#Check if any patient did not have alignments
output_own %>%
  filter(is.na(regName)) %>%
  count(DrugRecord)

# Check if any patient did not have any alignment
#missing_ids <- setdiff(stringDF_fixed$person_id, output_own$personID)
#stringDF_single <- stringDF[stringDF$person_id %in% missing_ids, ]

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
processed_own <- postprocess_alignments(output_own, regimenCombine = 56)
# Process eras
processedEras_combined <- processed_own %>% calculateEras_local(
  same_regimen_gap = 146,   # 56 + 90
  diff_regimen_gap = 112     # 56 + 56
)
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
