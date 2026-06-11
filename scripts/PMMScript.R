############################################################
# ARTEMIS-PMM Treatment Pathway Extraction and Alignment
#
# Purpose:
#   End-to-end workflow for extracting, processing, and
#   aligning treatment regimens using the ARTEMIS framework
#   with PMM-specific adaptations and custom extensions.
#
# Description:
#   This script executes the complete treatment pathway
#   analysis pipeline, including cohort preparation,
#   regimen generation, alignment, and output generation.
#   Several components have been modified from the original
#   ARTEMIS implementation to support project-specific
#   requirements and methodological improvements.
#
# Inputs:
#   - OMOP/CDM-derived treatment data
#   - Cohort definitions
#   - ARTEMIS configuration files
#
# Outputs:
#   - Treatment regimen datasets
#   - Alignment results
#   - Summary statistics and visualizations
#
# Author: Alisson Licona
# Last updated: 2026-06-11
############################################################



############################################################
# 1. Load Libraries and Configuration
############################################################

# ---- Install required packages if not already installed ----
source("R/setUp.R")

# ---- Connect to IRIS following the R/connectToIRIS.R file ----

# ---- Required libraries ----
library(reticulate)
library(ARTEMIS)
library(dplyr)
library(CDMConnector)
library(CirceR)

############################################################
# 2. Define Cohort and Analysis Parameters
############################################################

# ------------------------------------------------------------------
# Input Cohort Definition
#
# Load the PMM cohort definition used to identify the study
# population from the OMOP Common Data Model (CDM).
#
# The cohort is defined using an OHDSI CohortDefinition JSON
# specification, which is converted into SQL and executed
# against the target database.
#
# Outputs:
#   - expr      : cohort expression object
#   - sql       : SQL query generated from the cohort definition
#   - cohortSet : cohort metadata used during cohort generation
# ------------------------------------------------------------------

# Path to the PMM cohort definition
jsonPath <- "definitions/PMMDefinitions/PMM_cohort.json"

# Parse cohort definition JSON into an OHDSI cohort expression
expr <- CirceR::cohortExpressionFromJson(
  readChar(jsonPath, file.info(jsonPath)$size)
)

# Generate SQL corresponding to the cohort definition
sql <- CirceR::buildCohortQuery(
  expression = expr,
  options = CirceR::createGenerateOptions()
)

# Load cohort metadata and settings
cohortSet <- CDMConnector::readCohortSet(
  path = here::here(
    "definitions/PMMDefinitions"
  )
)

# ------------------------------------------------------------------
# Define Database Schemas
#
# cdmSchema:
#   Schema containing the OMOP Common Data Model tables.
#
# writeSchema:
#   Schema used by ARTEMIS for temporary tables and
#   intermediate processing outputs.
# ------------------------------------------------------------------

cdmSchema <- "cdm"
writeSchema <- "artemistest"
# Analysis identifier
name <- "MM"

# ------------------------------------------------------------------
# Create Valid Drug Definitions
#
# ARTEMIS requires a validDrugs table containing the treatment
# concepts that will be considered when constructing treatment
# regimens.
#
# For the MM analysis, candidate therapies are identified using
# a curated list of ATC codes representing treatments of
# interest. Where possible, ATC concepts are mapped to HemOnc
# drug components to improve treatment standardization and
# regimen construction.
#
# This step only needs to be executed when creating or updating
# the treatment definition file. For routine analyses, the
# pre-generated validDrugs.csv file should be loaded instead.
#
# Output:
#   validDrugs
#     - Drug name
#     - Concept identifier
#     - Vocabulary mapping information
#     - Metadata required by ARTEMIS
# ------------------------------------------------------------------

# Retrieve selected treatment concepts from the OMOP vocabulary.
# The query:
#   1. Selects predefined ATC drug classes.
#   2. Maps ATC concepts to HemOnc drug components.
#   3. Creates the validDrugs structure required by ARTEMIS.

validDrugs <- DatabaseConnector::querySql(conn, "
WITH selected_atc AS (
  SELECT
      concept_id AS concept_id_atc,
      concept_code AS concept_code_atc,
      concept_name
  FROM vocabularies.concept
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
  LEFT JOIN vocabularies.concept_relationship cr
      ON a.concept_id_atc = cr.concept_id_1
  LEFT JOIN vocabularies.concept c_hemonc
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

# ------------------------------------------------------------------
# Persist Treatment Definitions
#
# Save the generated validDrugs table for reproducibility and
# future analyses. Once validated, this file serves as the
# canonical treatment definition set for the MM workflow.
# ------------------------------------------------------------------

write.csv(
  validDrugs,
  here::here(
    "definitions",
    "PMMDefinitions",
    "validDrugs.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------------
# Load Curated Treatment Definitions
#
# For routine execution, load the validated treatment
# definitions rather than regenerating them from the
# vocabulary tables.
# ------------------------------------------------------------------

validDrugs <- read.csv(
  here::here(
    "definitions",
    "PMMDefinitions",
    "validDrugs.csv"
  )
)

# ------------------------------------------------------------------
# Retrieve Treatment Exposure Data
#
# Extract all drug exposure records for patients included in
# the study cohort.
#
# This customized version of getConDF():
#   - Uses ingredient-level concepts rather than branded drugs.
#   - Restricts extraction to therapies included in the cohort.
#   - Produces a standardized treatment exposure dataset for
#     downstream regimen construction.
#
# Output:
#   con_df
#     One row per treatment exposure event containing patient,
#     drug, and timing information.
# ------------------------------------------------------------------

source("getConDF_noAncestor.R")
con_df <- getConDF_noAncestor(
  connectionDetails = connection_details,
  json = cohortSet,
  name = name,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema
)

# Quality control checks
nrow(con_df)
length(unique(con_df$person_id))

# ------------------------------------------------------------------
# Construct Patient Treatment Sequences
#
# Convert individual drug exposure records into ARTEMIS
# treatment strings using the curated validDrugs definition.
#
# These treatment strings provide a standardized
# representation of each patient's treatment history and
# serve as the primary input for regimen identification and
# alignment.
#
# Output:
#   stringDF
#     Patient-level treatment sequences encoded in ARTEMIS
#     format.
# ------------------------------------------------------------------

stringDF <- stringDF_from_cdm(
  con_df = con_df,
  writeOut = FALSE,
  validDrugs = validDrugs
)

# Quality control checks
nrow(stringDF)
length(unique(stringDF$person_id))

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


############################################################
# 3. Extract Treatment Regimens
#
# ARTEMIS provides a library of predefined oncology regimens.
# For the PMM analysis, we additionally derive empirical
# regimens directly from observed treatment patterns in the
# study population.
#
# This section:
#   1. Loads standard ARTEMIS regimens.
#   2. Identifies observed drug combinations from patient data.
#   3. Constructs empirical regimen definitions.
#   4. Assigns regimen groups for downstream alignment.
############################################################

# ------------------------------------------------------------------
# Load Standard ARTEMIS Regimens
#
# Load the curated regimen library distributed with ARTEMIS and
# retain only regimens relevant to Multiple Myeloma.
#
# These regimens provide a reference framework for treatment
# alignment and comparison with empirically observed regimens.
# ------------------------------------------------------------------

# Disclaimer: currently, ARTEMIS regimens list is not complete and
# lacks important regimen combinations required for PMM
regimens <- loadRegimens(condition = "all")

regimens <- regimens %>%
  filter(condition == "Multiple myeloma")

# Regimens by ETL_HemOnc_regimens
#load("C:/Users/pc/Desktop/to_share_vm/BreastCancer/HemOnc_output/regimens.rda")

# ------------------------------------------------------------------
# Derive Empirical Regimens from Observed Drug Exposure Data
#
# In addition to predefined regimens, treatment combinations
# are derived directly from patient-level drug exposures.
#
# Regimen episodes are constructed by grouping drugs occurring
# within a 90-day treatment window. Drugs administered within
# the same episode are considered part of the same regimen.
#
# Output (last version):
#   regimens_emp_11mar
#     One row per patient regimen episode.
# ------------------------------------------------------------------

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

# ------------------------------------------------------------------
# Create Unique Regimen Definitions
#
# Convert patient-specific regimen episodes into a set of
# unique regimen definitions.
#
# Drug combinations are normalized to a canonical ordering so
# that equivalent regimens (e.g. A+B and B+A) are treated as
# the same regimen.
#
# Regimens are assigned:
#   - a unique empirical identifier (Emp_x)
#   - an average administration interval
#   - an occurrence count
#
# ARTEMIS represents regimens using a compact shortString
# notation:
#
#   0.drugA;28.drugB;
#
# where numbers indicate relative administration timing.
#
# Relative drug timing is estimated from the observed average
# spacing between treatments and scaled to a standardized
# treatment cycle.
# ------------------------------------------------------------------

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

# ------------------------------------------------------------------
# Expand Single-Agent Regimens
#
# Single-agent therapies may be administered repeatedly over
# multiple cycles. To improve matching during alignment,
# additional representations containing repeated exposures
# are generated.
#
# Example:
#
#   Original:
#     0.lenalidomide;
#
#   Expanded:
#     0.lenalidomide;
#     0.lenalidomide;1.lenalidomide;
#     0.lenalidomide;1.lenalidomide;2.lenalidomide;
#
# These alternative encodings facilitate alignment of
# repeated monotherapy patterns.
# ------------------------------------------------------------------

# Get single-agent regimens list
single_drug_regs_11mar <- regimens_empirical_11mar %>%
  mutate(
    # Extract drug names after the digits and dot
    drugs = str_extract_all(shortString, "[0-9]+\\.([a-z0-9]+)"),
    # Clean numeric prefixes (remove digits and dots)
    drugs = lapply(drugs, function(x) gsub("^[0-9]+\\.", "", x)),
    n_drugs = sapply(drugs, function(x) length(unique(x)))
  ) %>%
  filter(n_drugs == 1)

# Expand Single-Agent Regimens
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

# Unify both regimen lists
# Merge first version + repetitions
regimens_empirical_v2 <- bind_rows(regimens_empirical_11mar, replicated_single_drug_regs_11mar) %>%
  distinct(shortString, .keep_all = TRUE)

# Add cycle length data
regimens_final_v2 <- regimens_empirical_v2 %>%
  mutate(cycleLength = avg_days) %>%
  select(regName, shortString, cycleLength, count, avg_days)

# ------------------------------------------------------------------
# Assign Regimen Groups
#
# Regimens are mapped to clinically meaningful treatment
# groups based on drug composition.
#
# Drug combinations are converted into an order-independent
# key and matched against existing group definitions.
#
# Group assignments are used during downstream treatment
# alignment and visualization.
# ------------------------------------------------------------------

# Create list of regimens for clinical review
#
# Clinicians should assign a treatment group to each regimen
# by completing the Group column in the exported file.

review_list <- regimens_final_v2 %>%
  select(regName, shortString, count, avg_days) %>%
  mutate(Group = NA_character_)

# Export review file
write.csv(
  review_list,
  "definitions/PMMDefinitions/regimens_for_clinical_review.csv",
  row.names = FALSE
)

# Read complete excel
library(readxl)
regimen_groups <- read_excel(
  "definitions/PMMDefinitions/regimens_total_11mar.xlsx"
)

# Generate matching key
library(purrr)
create_regimen_key <- function(shortString){

  str_extract_all(
    shortString,
    "(?<=\\.)[a-z0-9_]+(?=;)"
  ) %>%
    map(~ sort(tolower(.x))) %>%
    map_chr(~ paste(.x, collapse = "+"))
}

regimen_groups <- regimen_groups %>%
  mutate(key = create_regimen_key(shortString))

group_lookup <- regimen_groups %>%
  select(key, Group) %>%
  distinct()

regimens_final_v2 <- regimens_final_v2 %>%
  mutate(key = create_regimen_key(shortString))

# Assign groups
regimens_total_groups <- regimens_final_v2 %>%
  left_join(
    group_lookup %>% select(key, Group),
    by = "key"
  )

# Validate
missing_groups <- regimens_total_groups %>%
  filter(is.na(Group) | Group == "")

nrow(missing_groups)

# If nrow(missing_groups) > 0 export:
# write.csv(
#   missing_groups,
#   "missing_groups.csv",
#   row.names = FALSE
# )

# Load regGroups
regGroups <- regimens_total_groups %>%
  select(regName, Group) %>%
  distinct() %>%        # keep unique combinations only
  arrange(regName)

############################################################
# 4. Perform Regimen Alignment
############################################################

# ------------------------------------------------------------------
# Align Patient Treatment Histories to Reference Regimens
#
# Patient treatment sequences are compared against the
# empirical regimen library to identify the best matching
# treatment regimen(s).
#
# Alignment allows heterogeneous drug exposure records to be
# translated into standardized regimen definitions suitable
# for downstream treatment pathway analysis.
#
# The current alignment settings are intentionally permissive
# to maximize the identification of potential regimen matches.
#
# Output:
#   output_own
#     Raw alignment results containing matched regimens and
#     alignment coordinates for each patient treatment record.
# ------------------------------------------------------------------

source('own_generate_alignments.R')
output_own <- own_generate_alignments(
  stringDF_fixed,
  regimens_final_v2
)

# ------------------------------------------------------------------
# Quality Control: Unmatched Treatment Records
#
# Identify treatment records that could not be aligned to any
# regimen definition.
#
# These records may indicate:
#   - Missing regimen definitions
#   - Incomplete treatment records
#   - Novel treatment combinations requiring review
# ------------------------------------------------------------------

output_own %>%
  filter(is.na(regName)) %>%
  count(DrugRecord)

# ------------------------------------------------------------------
# Standardize Alignment Coordinates
#
# Alignment boundaries are cleaned to ensure valid interval
# definitions and to prevent negative or missing alignment
# spans from propagating into downstream analyses.
# ------------------------------------------------------------------

output_own2 <- output_own %>%
  mutate(
    across(c(regimen_Start, regimen_End, drugRec_Start, drugRec_End),
           ~ replace_na(as.numeric(.), 0))
  ) %>%
  mutate(
    drugRec_End = ifelse(drugRec_End < drugRec_Start, drugRec_Start, drugRec_End),
    regimen_End = ifelse(regimen_End < regimen_Start, regimen_Start, regimen_End)
  )

# ------------------------------------------------------------------
# Post-process Regimen Alignments
#
# Raw alignment results are transformed into a standardized
# regimen representation suitable for treatment pathway
# reconstruction.
#
# regimenCombine:
#   Maximum interval (days) used when combining regimen
#   components into a single treatment episode.
# ------------------------------------------------------------------

source('postprocess_alignments.R')
processed_own <- postprocess_alignments(
  output_own,
  regimenCombine = 56
)

# ------------------------------------------------------------------
# Construct Treatment Eras
#
# Consecutive regimen exposures are consolidated into
# treatment eras representing continuous periods of therapy.
#
# same_regimen_gap:
#   Maximum allowable gap between occurrences of the same
#   regimen before a new era is initiated.
#
# diff_regimen_gap:
#   Maximum allowable gap between different regimens when
#   defining treatment transitions.
#
# Current settings:
#
#   same_regimen_gap = 146 days
#     = 56-day regimen cycle + 90-day tolerance window
#
#   diff_regimen_gap = 112 days
#     = 2 regimen cycles (56 + 56)
#
# These parameters were selected to accommodate delays,
# treatment interruptions, and real-world variation in
# regimen administration patterns.
# ------------------------------------------------------------------

source('calculateEras_local.R')
processedEras_combined <- processed_own %>%
  calculateEras_local(
    same_regimen_gap = 146,   # 56 + 90
    diff_regimen_gap = 112     # 56 + 56
  )

# Number of aligned treatment records
nrow(output_own)

# Number of unmatched records
sum(is.na(output_own$regName))

# Number of treatment eras generated
nrow(processedEras_combined)

# Number of patients represented
dplyr::n_distinct(processedEras_combined$personID)

############################################################
# 5. Generate Outputs and Visualizations
############################################################

# ------------------------------------------------------------------
# Generate Regimen Summary Statistics
#
# Calculate descriptive statistics for the treatment eras
# identified during the alignment process.
#
# Statistics include:
#   - Regimen frequency
#   - Number of patients per regimen
#   - Treatment duration
#   - Other summary measures used to characterize treatment
#     patterns within the study population.
#
# Output:
#   regStats
#     Summary table describing the observed treatment
#     regimens and eras.
# ------------------------------------------------------------------

regStats <- processedEras_combined %>%
  generateRegimenStats()

# ------------------------------------------------------------------
# Exploratory Data Analysis
#
# Visual inspection of the generated treatment pathways and
# regimen distributions.
#
# These plots serve as a quality-control step to verify that
# regimen frequencies and treatment patterns are clinically
# plausible before downstream analyses.
# ------------------------------------------------------------------

# Visualize the frequency of aligned regimens across the
# study population.
plotFrequency(processed_own)

# ------------------------------------------------------------------
# Quality Control Checks
# ------------------------------------------------------------------

# Number of patients included
dplyr::n_distinct(processedEras_combined$personID)

# Number of treatment eras identified
nrow(processedEras_combined)

# Number of unique regimens observed
dplyr::n_distinct(processedEras_combined$component)

# ------------------------------------------------------------------
# Generate Treatment Pathway Sankey Diagram
#
# Visualize transitions between treatment regimens across
# successive lines of therapy.
#
# Regimens are aggregated according to the clinician-defined
# treatment groups (regGroups) to improve interpretability
# and reduce visual complexity.
#
# Inputs:
#   processedEras
#     Patient-level treatment eras generated from aligned
#     regimen sequences.
#
#   regGroups
#     Clinician-curated mapping of regimens to treatment
#     groups.
#
# Output:
#   Sankey diagram of treatment pathways.
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# 1. Extract Regimens by Line of Therapy
# ------------------------------------------------------------------

firstLine <- processedEras_combined[processedEras_combined$First_Line==1,]
firstLine_Tab <- as.data.frame(table(firstLine$component))

secondLine <- processedEras_combined[processedEras_combined$Second_Line==1,]
secondLine_Tab <- as.data.frame(table(secondLine$component))

thirdLine <- processedEras_combined[processedEras_combined$Third_Line==1,]
thirdLine_Tab <- as.data.frame(table(thirdLine$component))

fourthLine <- processedEras_combined[processedEras_combined$Fourth_Line==1,]
fourthLine_Tab <- as.data.frame(table(fourthLine$component))

fifthLine <- processedEras_combined[processedEras_combined$Fifth_Line==1,]
fifthLine_Tab <- as.data.frame(table(fifthLine$component))

sixthLine <- processedEras_combined[processedEras_combined$Sixth_Line==1,]
sixthLine_Tab <- as.data.frame(table(sixthLine$component))

seventhLine <- processedEras_combined[processedEras_combined$Seventh_Line==1,]
seventhLine_Tab <- as.data.frame(table(seventhLine$component))

eigthLine <- processedEras_combined[processedEras_combined$Other==1,]
eigthLine_Tab <- as.data.frame(table(eigthLine$component))

sankey_first <- firstLine[,c(3,1)]
sankey_sec <- secondLine[,c(3,1)]
sankey_third <- thirdLine[,c(3,1)]
sankey_fourth <- fourthLine[,c(3,1)]
sankey_fifth <- fifthLine[,c(3,1)]
sankey_sixth <- sixthLine[,c(3,1)]
sankey_seventh <- seventhLine[,c(3,1)]
sankey_eigth <- eigthLine[,c(3,1)]

colnames(sankey_first) <- c("personID","Var1")
colnames(sankey_sec) <- c("personID","Var1")
colnames(sankey_third) <- c("personID","Var1")
colnames(sankey_fourth) <- c("personID","Var1")
colnames(sankey_fifth) <- c("personID","Var1")
colnames(sankey_sixth) <- c("personID","Var1")
colnames(sankey_seventh) <- c("personID","Var1")
colnames(sankey_eigth) <- c("personID","Var1")

colnames(regGroups) <- c("Var1","regGroup")

# ------------------------------------------------------------------
# 2. Map Regimens to Treatment Groups
# ------------------------------------------------------------------

sankey_first <- merge(sankey_first,regGroups,by="Var1")[,c(2,3)]
sankey_sec <- merge(sankey_sec,regGroups,by="Var1")[,c(2,3)]
sankey_third <- merge(sankey_third,regGroups,by="Var1")[,c(2,3)]
sankey_fourth <- merge(sankey_fourth,regGroups,by="Var1")[,c(2,3)]
sankey_fifth <- merge(sankey_fifth,regGroups,by="Var1")[,c(2,3)]
sankey_sixth <- merge(sankey_sixth,regGroups,by="Var1")[,c(2,3)]
sankey_seventh <- merge(sankey_seventh,regGroups,by="Var1")[,c(2,3)]
sankey_eigth <- merge(sankey_eigth,regGroups,by="Var1")[,c(2,3)]

colnames(sankey_first) <- c("personID","First Line")
colnames(sankey_sec) <- c("personID","Second Line")
colnames(sankey_third) <- c("personID","Third Line")
colnames(sankey_fourth) <- c("personID","Fourth Line")
colnames(sankey_fifth) <- c("personID","Fifth Line")
colnames(sankey_sixth) <- c("personID","Sixth Line")
colnames(sankey_seventh) <- c("personID","Seventh Line")
colnames(sankey_eigth) <- c("personID","Subsequent Lines")

# ------------------------------------------------------------------
# 3. Construct Patient-Level Treatment Pathways
# ------------------------------------------------------------------

sankey_list <- list(
  sankey_first,
  sankey_sec,
  sankey_third,
  sankey_fourth,
  sankey_fifth,
  sankey_sixth,
  sankey_seventh,
  sankey_eigth
)

sankey_all <- Reduce(function(x, y) merge(x, y, by = "personID", all = TRUE), sankey_list)
sankey_all <- sankey_all[!duplicated(sankey_all$personID),]

sankey_all <- sankey_all %>%
  mutate(across(ends_with("Line"), ~coalesce(., "")))

sankey_all[is.na(sankey_all$`Subsequent Lines`),]$`Subsequent Lines` <- ""






# Additional artemis-code
#
# Compare alignment scores between specific regimens.
# Useful for investigating potentially ambiguous treatment
# classifications or validating alignment performance.
#
# plotScoreDistribution(
#  regimen1 = "Letrozole",
#  regimen2 = "Tamoxifen",
#  processedAll = processedAll_combined)
#
# Examine treatment duration distributions for selected
# regimens.
#
# # plotRegimenLengthDistribution(
#     regimen1 = "Aspirin and Dexamethasone",
#     regimen2 = "Cyclophosphamide and Doxorubicin (AC)",
#     processedAll = processedAll)
#
# Save artemis outputs
# writeOutputs(output_all_combined, processedAll = processedAll_combined, processedEras = processedEras_combined,
#             connectionDetails = connectionDetails, cdmSchema = cdmSchema, regGroups = regGroups,
#             regStats = regStats, stringDF = stringDF, con_df = con_df)
