make_single_drug_alignments <- function(stringDF, regimens_single) {

  # Clean and normalize regimen info
  regimens_single <- regimens_single %>%
    mutate(
      drug = str_remove_all(shortString, "^0\\.|;$"),
      drug_lower = tolower(drug)
    )
  
  # Extract patient-level sequence order to compute start/end positions
  # Each drug exposure (in order of appearance) gets a sequence index
  patient_positions <- stringDF %>%
    mutate(drug_list = str_extract_all(seq, "(?<=\\.)[A-Za-z0-9_]+")) %>%
    rowwise() %>%
    mutate(drug_df = list(tibble(
      drug = tolower(drug_list),
      position = seq_along(drug_list)
    ))) %>%
    unnest(drug_df) %>%
    select(person_id, drug, position) %>%
    ungroup()
  
  # For each regimen, find patients who have that drug and assign start/end
  output_list <- lapply(seq_len(nrow(regimens_single)), function(i) {
    reg <- regimens_single[i, ]
    
    matches <- patient_positions %>%
      filter(drug == reg$drug_lower) %>%
      distinct(person_id, drug, position) 
    
    if (nrow(matches) == 0) {
        return(tibble())
    }
      
    matches <- matches  %>%
      left_join(stringDF, by = "person_id") %>%
      mutate(
        # Synthetic 2-point sequence (fixes createDrugDF compatibility)
        DrugRecord = paste0("0.", reg$drug, ";1.", reg$drug),
        Score = 1,
        adjustedS = 1,
        regimen_Start = 0,
        regimen_End = 1,
        drugRec_Start = position,
        drugRec_End = position + 1,
        Aligned_Seq_len = 2,
        totAlign = 2,
        regName = reg$regName,
        shortString = reg$shortString
      ) %>%
      select(
        DrugRecord, Score, regimen_Start, regimen_End,
        drugRec_Start, drugRec_End, Aligned_Seq_len, totAlign,
        adjustedS, personID = person_id, shortString, regName
      )
    
    return(matches)
  })
  
  out <- bind_rows(output_list) %>%
    mutate(across(
      c(Score, adjustedS, regimen_Start, regimen_End,
        drugRec_Start, drugRec_End, Aligned_Seq_len, totAlign),
      as.numeric
    )) %>%
    filter(!is.na(personID), !is.na(DrugRecord))
  
  return(out)
}