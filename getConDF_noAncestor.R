#' Generate a con_df dataframe (without concept_ancestor join)
#' @param connectionDetails A set of DatabaseConnector connection details
#' @param json A loaded cohort from loadJSON() or CDMConnector::readCohortSet()
#' @param name A cohort-specific name for written tables
#' @param cdmSchema Schema containing a valid OMOP CDM
#' @param writeSchema Schema where the user has write access
#' @return A con_df dataframe (person_id, drug_exposure_start_date, concept info)
#' @export
getConDF_noAncestor <- function(connectionDetails, json, name, cdmSchema, writeSchema) {
  message("Connecting using IRIS driver")
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  # ---- 1. Create empty cohort definition set
  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()

  # ---- 2. Convert JSON to CirceR expression
  cohortExpression <- CirceR::cohortExpressionFromJson(json$json[[1]])
  cohortSql <- CirceR::buildCohortQuery(
    cohortExpression,
    options = CirceR::createGenerateOptions(generateStats = FALSE)
  )

  cohortsToCreate <- rbind(
    cohortsToCreate,
    data.frame(
      cohortId = 1,
      cohortName = name,
      sql = cohortSql,
      stringsAsFactors = FALSE
    )
  )

  # ---- 3. Create cohort tables in write schema
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = name)
  CohortGenerator::createCohortTables(
    connection = connection,
    cohortDatabaseSchema = writeSchema,
    cohortTableNames = cohortTableNames
  )

  # ---- 4. Generate cohort
  CohortGenerator::generateCohortSet(
    connection = connection,
    cdmDatabaseSchema = cdmSchema,
    cohortDatabaseSchema = writeSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortsToCreate
  )

  # ---- 5. Retrieve subject IDs
  subject_ids <- DatabaseConnector::dbGetQuery(
    conn = connection,
    statement = paste0("SELECT subject_id FROM ", writeSchema, ".", name)
  )

  if (nrow(subject_ids) == 0) {
    warning("No subjects found in cohort: ", name)
    return(data.frame())
  }

  subject_ids_vec <- paste(subject_ids$subject_id, collapse = ",")

  # ---- 6. Simplified query: direct join to concept only (no concept_ancestor)
  sql_template <- glue::glue("
SELECT DISTINCT
    de.person_id,
    de.drug_exposure_start_date,
    de.drug_concept_id AS ancestor_concept_id,

    CASE
        WHEN POSITION(';' IN c.concept_name) > 0
        THEN SUBSTRING(c.concept_name, 1,
                       POSITION(';' IN c.concept_name) - 1)
        ELSE c.concept_name
    END AS concept_name

FROM cdm.drug_exposure de
JOIN vocabularies.concept c
    ON de.drug_concept_id = c.concept_id

WHERE de.drug_source_value IN (
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
")

  # ---- 7. Execute query
  con_df <- DatabaseConnector::dbGetQuery(conn = connection, statement = sql_template)

  message("✅ con_df successfully generated with ", nrow(con_df), " rows.")
  return(as.data.frame(con_df))
}
