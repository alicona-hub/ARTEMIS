library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

own_generate_alignments <- function(stringDF,
                                    regimens,
                                    max_gap = 30,
                                    writeOut = FALSE,
                                    outputName = "Output_simplified") {
  # --- Input checks ---
  if (!all(c("person_id", "seq") %in% names(stringDF))) {
    stop("stringDF must contain 'person_id' and 'seq' columns.")
  }
  if (!all(c("regName", "shortString") %in% names(regimens))) {
    stop("regimens must contain 'regName' and 'shortString' columns.")
  }
  
  # --- Parse regimen drug composition ---
  regimens_clean <- regimens %>%
    mutate(
      drugs = map(shortString, ~ {
        str_extract_all(.x, "(?<=\\.)[A-Za-z0-9_]+")[[1]] %>%
          tolower() %>%
          sort() %>%
          unique()
      }),
      key = map_chr(drugs, ~ paste(.x, collapse = "+"))
    ) %>%
    group_by(key) %>%
    summarise(regName = first(regName), .groups = "drop")
  
  # --- Expand patient strings (convert relative → cumulative days) ---
  patient_expanded <- stringDF %>%
    mutate(drug_entries = str_split(seq, ";")) %>%
    unnest(drug_entries) %>%
    filter(drug_entries != "") %>%
    mutate(
      rel_day = as.numeric(str_extract(drug_entries, "^[0-9]+")),
      drug = tolower(str_extract(drug_entries, "(?<=\\.)[A-Za-z0-9_]+"))
    ) %>%
    group_by(person_id) %>%
    mutate(
      day = cumsum(pmax(rel_day, 0)),  # ensure positive increments only
      day = ifelse(row_number() == 1, 0, day)  # first drug always starts at 0
    ) %>%  # convert to absolute timeline
    arrange(person_id, day)%>%
    mutate(day = cummax(day))
  
  # --- Split sequences by > max_gap days ---
  patient_blocks <- patient_expanded %>%
    group_by(person_id) %>%
    mutate(
      gap = day - lag(day, default = first(day)),
      new_block = if_else(row_number() == 1 | gap > max_gap, 1, 0),
      block = cumsum(new_block)
    ) %>%
    group_by(person_id, block) %>%
    summarise(
      seq = paste0(paste0(day, ".", drug, ";"), collapse = ""),
      drugs = list(sort(unique(drug))),
      key = paste(sort(unique(drug)), collapse = "+"),
      regimen_Start = min(day, na.rm = TRUE),
      regimen_End = max(day, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- Duplicate single-drug sequences (so all have ≥2 entries) ---
  patient_blocks <- patient_blocks %>%
    rowwise() %>%
    mutate(
      n_drugs = length(unique(unlist(drugs))),
      seq = if (n_drugs == 1) {
        # Extract numeric and drug
        day0 <- as.numeric(str_extract(seq, "^[0-9]+"))
        drug0 <- str_extract(seq, "(?<=\\.)[A-Za-z0-9_]+")
        paste0(seq, (day0 + 1), ".", drug0, ";")  # add +1 day duplicate
      } else {
        seq
      }
    ) %>%
    ungroup()
  
  # --- Match to known regimens ---
  patient_blocks <- patient_blocks %>%
    left_join(regimens_clean, by = "key")
  
  # --- Create ARTEMIS-style output ---
  output <- patient_blocks %>%
    mutate(
      Regimen = regName,
      DrugRecord = seq,
      Score = 1,
      drugRec_Start = regimen_Start,
      drugRec_End = regimen_End,
      Aligned_Seq_len = str_count(seq, ";"),
      totAlign = Aligned_Seq_len,
      adjustedS = 1,
      personID = person_id,
      shortString = seq
    ) %>%
    select(
      regName, Regimen, DrugRecord, Score, regimen_Start, regimen_End,
      drugRec_Start, drugRec_End, Aligned_Seq_len, totAlign, adjustedS,
      personID, shortString
    )
  
  # Optional write-out
  if (writeOut) {
    write.csv(output, outputName, row.names = FALSE)
  }
  
  return(output)
}
