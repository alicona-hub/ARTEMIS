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
  password = "Traverse123&",
  server   = "localhost/oncoregimenfinder",
  port     = 5432,
  pathToDriver = "c:/temp/jdbcDrivers"
)

conn <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(conn), add = TRUE)


# ---- User script ----
library(ARTEMIS)
library(dplyr)
library(stringr)
library(CDMConnector)


##### INPUT #####

# ---- Cohort definition ----
cohortSet <- CDMConnector::readCohortSet(path = here::here("C:/Users/pc/Desktop/to_share_vm/BreastCancer/myCohort/"))
# verify str(cohortSet)

name <- "breastCancer"

# ---- Create valid drugs if needed ----
# If valid drugs exists already, just load it
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

write.csv(validDrugs, here::here("C:/Users/pc/Desktop/to_share_vm/BreastCancer/myCohort/validDrugs.csv"), row.names = FALSE)


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
  mutate(
    drug_date = as.Date(drug_exposure_start_date)
  ) %>%
  arrange(person_id, drug_date) %>%
  group_by(person_id) %>%
  mutate(
    interval = as.numeric(drug_date - lag(drug_date, default = first(drug_date))),
    block = cumsum(ifelse(is.na(interval) | interval > 28, 1, 0))
  ) %>%
  group_by(person_id, block) %>%
  summarise(
    drug_combo = paste(
      sort(unique(
        tolower(gsub(" ", "", concept_name))  #removes all spaces
      )),
      collapse = "+"
    )
  ) %>%
  ungroup()

# 4. Create list of unique regimen combinations
regimens_empirical <- regimens_emp %>%
  count(drug_combo, sort = TRUE) %>%
  mutate(
    regName = paste0("Emp_", row_number()),
    shortString = paste0("0.", gsub("\\+", ";0.", drug_combo), ";")
  ) %>%
  select(regName, shortString)


# ---- Schema definition ----
cdmSchema <- "public"
writeSchema <- "artemis"


##### MAIN #####
# Modified version of getConDF (use of ingredients, no branded drugs)
# Retrieves drug exposure data for all pts under cohort conditions
con_df <- getConDF(connectionDetails = connectionDetails, json = cohortSet, name = name, cdmSchema = cdmSchema, writeSchema = writeSchema)

# Creates string of drug combinations for each patient, based on valid drugs
stringDF <- stringDF_from_cdm(con_df = con_df, writeOut = F, validDrugs = validDrugs)

# Split single vs multi regimen
regimens_single <- regimens_empirical %>% filter(str_count(shortString, ";") == 1)
regimens_multi  <- regimens_empirical %>% filter(str_count(shortString, ";") > 1)

# Generates alingments between pt drug combinations and regimens table
# Parameters can be changed. Now, they allow the max possible alignments
# Alingment for multi drug regimens
output_multi <- generateRawAlignments(
  stringDF = stringDF,
  regimens = regimens_multi,
  g = 0.1,
  Tfac = 0.1,
  verbose = 1,
  mem = -1,
  removeOverlap = 1,
  method = "PropDiff",
  writeOut = TRUE,
  outputName = "Output"
)

# Synthetic alignments for single-drug regimens
output_single <- make_single_drug_alignments(stringDF, regimens_single)

# Clean output (sometimes the start and end points are inverted)
output_multi2 <- output_multi %>%
  mutate(
    drugRec_Start = as.numeric(drugRec_Start),
    drugRec_End   = as.numeric(drugRec_End),
    drugRec_End   = ifelse(drugRec_End < drugRec_Start, drugRec_Start, drugRec_End)
  )

# Process alignments for multi-drug regimens
processedAll_multi <- processAlignments(output_multi2,regimenCombine = 2, regimens = regimens_multi)

# Process alignments for single-drug regimens
processed_single <- make_processed_single(output_single, stringDF)

# Process combined version
processedAll_combined <- bind_rows(processedAll_multi, processed_single  %>%
                                     mutate (personID = as.character(personID))) %>%
  mutate(
    t_start = as.numeric(t_start),
    t_end = as.numeric(t_end),
    adjustedS = as.numeric(adjustedS),
    timeToNextRegimen = as.numeric(timeToNextRegimen),
    timeToEOD = as.numeric(timeToEOD),
    regLength = as.numeric(regLength)
  )

# Process eras
processedEras_combined <- processedAll_combined %>% calculateEras(discontinuationTime = 30)

# Generate basic stats for eras
regStats <- processedEras_combined %>% generateRegimenStats()


### Explore data
plotFrequency(processedAll_combined)
#plotScoreDistribution(regimen1 = "Aspirin and Dexamethasone", regimen2 = "Cyclophosphamide and Doxorubicin (AC)", processedAll = processedAll)
#plotRegimenLengthDistribution(regimen1 = "Aspirin and Dexamethasone", regimen2 = "Cyclophosphamide and Doxorubicin (AC)", processedAll = processedAll)


##### OUTPUT #####
# plotSankey_local(processedEras, regGroups, saveLocation = NA, fileName = "Network")
writeOutputs(output_all, processedAll = processedAll, processedEras = processedEras,
                   connectionDetails = connectionDetails, cdmSchema = cdmSchema, regGroups = regGroups,
                   regStats = regStats, stringDF = stringDF, con_df = con_df)



#### Disconnet from db
DatabaseConnector::disconnect(connection)
